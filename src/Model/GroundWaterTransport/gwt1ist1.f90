module GwtIstModule

  use KindModule,             only: DP, I4B
  use ConstantsModule,        only: DONE, DZERO, LENFTYPE, LENPACKAGENAME,     &
                                    LENBUDTXT, DHNOFLO
  use BndModule,              only: BndType
  use BudgetModule,           only: BudgetType
  use GwtFmiModule,           only: GwtFmiType
  use GwtMstModule,           only: GwtMstType, get_zero_order_decay
  use OutputControlData,      only: OutputControlDataType
  !
  implicit none
  !
  private
  public :: ist_create
  !
  character(len=LENFTYPE)       :: ftype = 'IST'
  character(len=LENPACKAGENAME) :: text  = ' IMMOBILE DOMAIN'
  integer(I4B), parameter :: NBDITEMS = 5
  character(len=LENBUDTXT), dimension(NBDITEMS) :: budtxt
  data budtxt / ' STORAGE-AQUEOUS', '  STORAGE-SORBED', &
                '   DECAY-AQUEOUS', '    DECAY-SORBED', &
                '   MOBILE-DOMAIN' /
  !
  type, extends(BndType) :: GwtIstType
    
    type(GwtFmiType), pointer                        :: fmi => null()           !< pointer to fmi object
    type(GwtMstType), pointer                        :: mst => null()           !< pointer to mst object
    
    integer(I4B), pointer                            :: icimout => null()       !< unit number for binary cim output
    integer(I4B), pointer                            :: idcy => null()          !< order of decay rate (0:none, 1:first, 2:zero)
    integer(I4B), pointer                            :: isrb => null()          !< sorption active flag (0:off, 1:on)
    integer(I4B), pointer                            :: kiter => null()         !< picard iteration counter
    real(DP), dimension(:), pointer, contiguous      :: cim => null()           !< concentration for immobile domain
    real(DP), dimension(:), pointer, contiguous      :: cimnew => null()        !< immobile concentration at end of current time step
    real(DP), dimension(:), pointer, contiguous      :: cimold => null()        !< immobile concentration at end of last time step
    real(DP), dimension(:), pointer, contiguous      :: zetaim => null()        !< mass transfer rate to immobile domain
    real(DP), dimension(:), pointer, contiguous      :: thetaim => null()       !< porosity of the immobile domain
    real(DP), dimension(:), pointer, contiguous      :: bulk_density => null()  !< bulk density
    real(DP), dimension(:), pointer, contiguous      :: distcoef => null()      !< distribution coefficient
    real(DP), dimension(:), pointer, contiguous      :: decay => null()         !< first or zero order rate constant for liquid
    real(DP), dimension(:), pointer, contiguous      :: decaylast => null()     !< decay rate used for last iteration (needed for zero order decay)
    real(DP), dimension(:), pointer, contiguous      :: decayslast => null()    !< sorbed decay rate used for last iteration (needed for zero order decay)
    real(DP), dimension(:), pointer, contiguous      :: decay_sorbed => null()  !< first or zero order rate constant for sorbed mass
    real(DP), dimension(:), pointer, contiguous      :: strg => null()          !< mass transfer rate
    real(DP), dimension(2, NBDITEMS)                 :: budterm                 !< immmobile domain mass summaries
    
    type(BudgetType), pointer                        :: budget => null()        !< budget object
    type(OutputControlDataType), pointer             :: ocd => null()           !< output control object for cim
    
  contains
  
    procedure :: bnd_ar => ist_ar
    procedure :: bnd_rp => ist_rp
    procedure :: bnd_ad => ist_ad
    procedure :: bnd_fc => ist_fc
    procedure :: bnd_cq => ist_cq
    procedure :: bnd_bd => ist_bd
    procedure :: bnd_ot_model_flows => ist_ot_model_flows
    procedure :: bnd_ot_dv => ist_ot_dv
    procedure :: bnd_ot_bdsummary => ist_ot_bdsummary
    procedure :: bnd_da => ist_da
    procedure :: allocate_scalars
    procedure :: read_dimensions => ist_read_dimensions
    procedure :: read_options
    procedure, private :: ist_allocate_arrays
    procedure, private :: read_data
    !procedure, private :: calcddbud
    !procedure, private :: calccim
    
  end type GwtIstType
  
  contains
  
  subroutine ist_create(packobj, id, ibcnum, inunit, iout, namemodel, pakname, &
                        fmi, mst)
! ******************************************************************************
! ist_create -- Create a New Immobile Domain Package
! Subroutine: (1) create new-style package
!             (2) point packobj to the new package
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(BndType), pointer :: packobj
    integer(I4B),intent(in) :: id
    integer(I4B),intent(in) :: ibcnum
    integer(I4B),intent(in) :: inunit
    integer(I4B),intent(in) :: iout
    character(len=*), intent(in) :: namemodel
    character(len=*), intent(in) :: pakname
    ! -- local
    type(GwtIstType), pointer :: istobj
    type(GwtFmiType), pointer :: fmi
    type(GwtMstType), pointer :: mst
! ------------------------------------------------------------------------------
    !
    ! -- allocate the object and assign values to object variables
    allocate(istobj)
    packobj => istobj
    !
    ! -- create name and memory path
    call packobj%set_names(ibcnum, namemodel, pakname, ftype)
    packobj%text = text
    !
    ! -- allocate scalars
    call packobj%allocate_scalars()
    !
    ! -- initialize package
    call packobj%pack_initialize()
    !
    ! -- store values
    packobj%inunit = inunit
    packobj%iout = iout
    packobj%id = id
    packobj%ibcnum = ibcnum
    packobj%ncolbnd = 1
    packobj%iscloc = 1
    !
    ! -- Point IST specific variables
    istobj%fmi => fmi
    istobj%mst => mst
    !
    ! -- return
    return
  end subroutine ist_create

  subroutine ist_ar(this)
! ******************************************************************************
! ist_ar -- Allocate and Read
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use SimModule, only: ustop, store_error, count_errors
    use BudgetModule, only: budget_cr
    ! -- dummy
    class(GwtIstType), intent(inout) :: this
    ! -- local
    integer(I4B) :: n
    ! -- formats
    character(len=*), parameter :: fmtist =                                    &
      "(1x,/1x,'IST -- IMMOBILE DOMAIN STORAGE AND TRANSFER PACKAGE, ',        &
      &'VERSION 1, 12/24/2018 INPUT READ FROM UNIT ', i0, //)"
! ------------------------------------------------------------------------------
    !
    ! --print a message identifying the immobile domain package.
    write(this%iout, fmtist) this%inunit
    !
    ! -- Read immobile domain options
    call this%read_options()
    !
    ! -- Allocate arrays
    call this%ist_allocate_arrays()
    !
    ! -- Now that arrays are allocated, check in the cimnew array to 
    !    the output control manager for subsequent printing/saving
    call this%ocd%init_dbl('CIM', this%cimnew, this%dis, 'PRINT LAST ',        &
                           'COLUMNS 10 WIDTH 11 DIGITS 4 GENERAL ',            &
                            this%iout, DHNOFLO)
    !
    ! -- read the data block
    call this%read_data()
    !
    ! -- set cimnew to the cim start values read from input
    do n = 1, this%dis%nodes
      this%cimnew(n) = this%cim(n)
    end do
    !
    ! -- add thetaim to the prsity2 accumulator in mst package
    call this%mst%addto_prsity2(this%thetaim)
    !
    ! -- setup the immobile domain budget
    call budget_cr(this%budget, this%memoryPath)
    call this%budget%budget_df(NBDITEMS, 'MASS', 'M', bdzone=this%packName)
    !
    ! -- Perform a check to ensure that sorption and decay are set 
    !    consistently between the MST and IST packages.
    if (this%idcy /= this%mst%idcy) then
      call store_error('DECAY MUST BE ACTIVATED CONSISTENTLY BETWEEN THE &
        &MST AND IST PACKAGES.  TURN DECAY ON OR OFF FOR BOTH PACKAGES.')
    endif
    if (this%isrb /= this%mst%isrb) then
      call store_error('SORPTION MUST BE ACTIVATED CONSISTENTLY BETWEEN THE &
        &MST AND IST PACKAGES.  TURN SORPTION ON OR OFF FOR BOTH PACKAGES.')
    endif
    if (count_errors() > 0) then
      call this%parser%StoreErrorUnit()
      call ustop()
    end if
    !
    ! -- Return
    return
  end subroutine ist_ar
  
  subroutine ist_rp(this)
! ******************************************************************************
! ist_rp -- override in order to skip reading for ist package
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(GwtIstType),intent(inout) :: this
    ! -- local
    ! -- format
! ------------------------------------------------------------------------------
    !
    ! -- return
    return
  end subroutine ist_rp

  !> @ brief Advance the ist package
  !!
  !!  Advance the IST Package and handle the adaptive time stepping
  !!  feature by copying from new to old or old to new accordingly
  !!
  !<
  subroutine ist_ad(this)
    ! -- modules
    use SimVariablesModule, only: iFailedStepRetry
    ! -- dummy variables
    class(GwtIstType) :: this  !< BndType object
    ! -- local variables
    integer(I4B) :: n
    !
    ! -- Call parent advance
    call this%BndType%bnd_ad()
    !
    ! -- set independent kiter counter to zero
    this%kiter = 0
    !
    ! -- copy cimnew into cimold or vice versa if this is a repeat of
    !    a failed time step
    if (iFailedStepRetry == 0) then
      do n = 1, this%dis%nodes
        this%cimold(n) = this%cimnew(n)
      end do
    else
      do n = 1, this%dis%nodes
        this%cimnew(n) = this%cimold(n)
      end do
    end if
    !
    return
  end subroutine ist_ad

  subroutine ist_fc(this, rhs, ia, idxglo, amatsln)
! ******************************************************************************
! ist_fc -- Calculate coefficients and fill amat and rhs
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use TdisModule, only: delt
    ! -- dummy
    class(GwtIstType) :: this
    real(DP), dimension(:), intent(inout) :: rhs
    integer(I4B), dimension(:), intent(in) :: ia
    integer(I4B), dimension(:), intent(in) :: idxglo
    real(DP), dimension(:), intent(inout) :: amatsln
    ! -- local
    integer(I4B) :: n, idiag
    real(DP) :: tled
    real(DP) :: hhcof, rrhs
    real(DP) :: swt, swtpdt
    real(DP) :: vcell
    real(DP) :: thetaim
    real(DP) :: zetaim
    real(DP) :: thetamfrac
    real(DP) :: thetaimfrac
    real(DP) :: kd
    real(DP) :: rhob
    real(DP) :: lambda1im
    real(DP) :: lambda2im
    real(DP) :: gamma1im
    real(DP) :: gamma2im
    real(DP) :: cimold
    real(DP) :: f
    real(DP) :: cimsrbold
    real(DP) :: cimsrbnew
    real(DP), dimension(9) :: ddterm
! ------------------------------------------------------------------------------
    !
    ! -- set variables
    tled = DONE / delt
    this%kiter = this%kiter + 1
    !
    ! -- loop through and calculate immobile domain contribution to hcof and rhs
    do n = 1, this%dis%nodes
      !
      ! -- skip if transport inactive
      if(this%ibound(n) <= 0) cycle
      !
      ! -- calculate new and old water volumes
      vcell = this%dis%area(n) * (this%dis%top(n) - this%dis%bot(n))
      swtpdt = this%fmi%gwfsat(n)
      swt = this%fmi%gwfsatold(n, delt)
      thetaim = this%thetaim(n)
      idiag = ia(n)
      !
      ! -- set exchange coefficient      
      zetaim = this%zetaim(n)
      !
      ! -- Set thetamfrac and thetaimfrac
      thetamfrac = this%mst%get_thetamfrac(n)
      thetaimfrac = this%mst%get_thetaimfrac(n, this%thetaim(n))
      !
      ! -- Add dual domain mass transfer contributions to rhs and hcof
      kd = DZERO
      rhob = DZERO
      lambda1im = DZERO
      lambda2im = DZERO
      gamma1im = DZERO
      gamma2im = DZERO
      !
      ! -- setup decay variables
      if (this%idcy == 1) lambda1im = this%decay(n)
      if (this%idcy == 2) then
        gamma1im = get_zero_order_decay(this%decay(n), this%decaylast(n),      &
                                        this%kiter, this%cimold(n),            &
                                        this%cimnew(n), delt)
        this%decaylast(n) = gamma1im
      end if
      !
      ! -- setup sorption variables
      if (this%isrb > 0) then
        kd = this%distcoef(n)
        rhob = this%bulk_density(n)
        if (this%idcy == 1) lambda2im = this%decay_sorbed(n)
        if (this%idcy == 2) then
          cimsrbold = this%cimold(n) * kd
          cimsrbnew = this%cimnew(n) * kd
          gamma2im = get_zero_order_decay(this%decay_sorbed(n),                &
                                          this%decayslast(n),                  &
                                          this%kiter, cimsrbold,               &
                                          cimsrbnew, delt)
          this%decayslast(n) = gamma2im
        end if
      end if
      !
      ! -- calculate the terms and then get the hcof and rhs contributions
      call get_ddterm(thetaim, vcell, delt, swtpdt,                            &
                      thetaimfrac, rhob, kd, lambda1im, lambda2im,             &
                      gamma1im, gamma2im, zetaim, ddterm, f)
      cimold = this%cimold(n)
      call get_hcofrhs(ddterm, f, cimold, hhcof, rrhs)
      !
      ! -- update solution accumulators
      amatsln(idxglo(idiag)) = amatsln(idxglo(idiag)) + hhcof
      rhs(n) = rhs(n) + rrhs
      !
    enddo
    !
    ! -- Return
    return
  end subroutine ist_fc
  
  subroutine ist_cq(this, x, flowja, iadv)
! ******************************************************************************
! ist_cq -- Calculate flows
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use TdisModule, only: delt
    use ConstantsModule, only: DZERO
    ! -- dummy
    class(GwtIstType), intent(inout) :: this
    real(DP), dimension(:), intent(in) :: x
    real(DP), dimension(:), contiguous, intent(inout) :: flowja
    integer(I4B), optional, intent(in) :: iadv
    ! -- local
    integer(I4B) :: idiag
    integer(I4B) :: n
    real(DP) :: rate
    real(DP) :: swt, swtpdt
    real(DP) :: hhcof, rrhs
    real(DP) :: vcell
    real(DP) :: thetaim
    real(DP) :: zetaim
    real(DP) :: thetamfrac
    real(DP) :: thetaimfrac
    real(DP) :: kd
    real(DP) :: rhob
    real(DP) :: lambda1im
    real(DP) :: lambda2im
    real(DP) :: gamma1im
    real(DP) :: gamma2im
    real(DP) :: cimnew
    real(DP) :: cimold
    real(DP) :: f
    real(DP) :: cimsrbold
    real(DP) :: cimsrbnew
    real(DP), dimension(9) :: ddterm
    ! -- formats
! ------------------------------------------------------------------------------
    !
    ! -- initialize
    this%budterm(:, :) = DZERO    
    !
    ! -- Calculate immobile domain transfer rate
    do n = 1, this%dis%nodes
      !
      ! -- skip if transport inactive
      rate = DZERO
      cimnew = DZERO
      if(this%ibound(n) > 0) then
        !
        ! -- calculate new and old water volumes
        vcell = this%dis%area(n) * (this%dis%top(n) - this%dis%bot(n))
        swtpdt = this%fmi%gwfsat(n)
        swt = this%fmi%gwfsatold(n, delt)
        thetaim = this%thetaim(n)
        !
        ! -- set exchange coefficient      
        zetaim = this%zetaim(n)
        !
        ! -- Set thetamfrac and thetaimfrac
        thetamfrac = this%mst%get_thetamfrac(n)
        thetaimfrac = this%mst%get_thetaimfrac(n, this%thetaim(n))
        !
        ! -- Calculate exchange with immobile domain
        rate = DZERO
        hhcof = DZERO
        rrhs = DZERO
        kd = DZERO
        rhob = DZERO
        lambda1im = DZERO
        lambda2im = DZERO
        gamma1im = DZERO
        gamma2im = DZERO
        if (this%idcy == 1) lambda1im = this%decay(n)
        if (this%idcy == 2) then
          gamma1im = get_zero_order_decay(this%decay(n), this%decaylast(n), 0, &
                                          this%cimold(n), this%cimnew(n), delt)
        end if
        if (this%isrb > 0) then
          kd = this%distcoef(n)
          rhob = this%bulk_density(n)
          if (this%idcy == 1) lambda2im = this%decay_sorbed(n)
          if (this%idcy == 2) then
            cimsrbold = this%cimold(n) * kd
            cimsrbnew = this%cimnew(n) * kd
            gamma2im = get_zero_order_decay(this%decay_sorbed(n),                &
                                            this%decayslast(n),                  &
                                            0, cimsrbold,                        &
                                            cimsrbnew, delt)
          end if
        end if
        !
        ! -- calculate the terms and then get the hcof and rhs contributions
        call get_ddterm(thetaim, vcell, delt, swtpdt,                          &
                        thetaimfrac, rhob, kd, lambda1im, lambda2im,           &
                        gamma1im, gamma2im, zetaim, ddterm, f)
        cimold = this%cimold(n)
        call get_hcofrhs(ddterm, f, cimold, hhcof, rrhs)
        !
        ! -- calculate rate from hcof and rhs
        rate = hhcof * x(n) - rrhs
        !
        ! -- calculate immobile domain concentration
        cimnew = get_ddconc(ddterm, f, cimold, x(n))
        !
        ! -- accumulate the budget terms
        call accumulate_budterm(this%budterm, ddterm, cimnew, cimold, x(n),    &
                                this%idcy)
      end if
      !
      ! -- store rate and add to flowja
      this%strg(n) = rate
      idiag = this%dis%con%ia(n)
      flowja(idiag) = flowja(idiag) + rate
      !
      ! -- store immobile domain concentration
      this%cimnew(n) = cimnew
      !
    enddo
    return
  end subroutine ist_cq

  subroutine ist_bd(this, model_budget)
    ! -- add package ratin/ratout to model budget
    use TdisModule, only: delt
    use BudgetModule, only: BudgetType, rate_accumulator
    class(GwtIstType) :: this
    type(BudgetType), intent(inout) :: model_budget
    real(DP) :: ratin
    real(DP) :: ratout
    integer(I4B) :: isuppress_output
    isuppress_output = 0
    call rate_accumulator(this%strg(:), ratin, ratout)
    call model_budget%addentry(ratin, ratout, delt, this%text,                 &
                               isuppress_output, this%packName)
    
  end subroutine ist_bd

  subroutine ist_ot_model_flows(this, icbcfl, ibudfl, icbcun, imap)
! ******************************************************************************
! ist_ot_model_flows -- write flows to binary file and/or print flows to budget
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use ConstantsModule, only: DZERO
    ! -- dummy
    class(GwtIstType) :: this
    integer(I4B), intent(in) :: icbcfl
    integer(I4B), intent(in) :: ibudfl
    integer(I4B), intent(in) :: icbcun
    integer(I4B), dimension(:), optional, intent(in) :: imap
    ! -- loca
    integer(I4B) :: n
    integer(I4B) :: ibinun
    integer(I4B) :: nbound
    integer(I4B) :: naux
    real(DP) :: rate
    !
    ! -- Set unit number for binary output
    if(this%ipakcb < 0) then
      ibinun = icbcun
    elseif(this%ipakcb == 0) then
      ibinun = 0
    else
      ibinun = this%ipakcb
    endif
    if(icbcfl == 0) ibinun = 0
    !
    ! -- Record the storage rate if requested
    !
    ! -- If cell-by-cell flows will be saved as a list, write header.
    if(ibinun /= 0) then
      nbound = this%dis%nodes
      naux = 0
      call this%dis%record_srcdst_list_header(this%text, this%name_model,      &
                  this%name_model, this%name_model, this%packName, naux,       &
                  this%auxname, ibinun, nbound, this%iout)
    endif
    !
    ! -- Calculate immobile domain rhs and hcof
    do n = 1, this%dis%nodes
      !
      ! -- skip if transport inactive
      rate = DZERO
      if(this%ibound(n) > 0) then
        !
        ! -- set rate from this%strg
        rate = this%strg(n)
      end if
      !
      ! -- If saving cell-by-cell flows in list, write flow
      if (ibinun /= 0) then
        call this%dis%record_mf6_list_entry(ibinun, n, n, rate,                &
                                            naux, this%auxvar(:,n),            &
                                            olconv=.TRUE.,                     &
                                            olconv2=.TRUE.)
      end if
      !
    enddo
    !
    ! -- Return
    return
  end subroutine ist_ot_model_flows
    
  subroutine ist_ot_dv(this, idvsave, idvprint)
    ! -- modules
    use TdisModule, only: kstp, kper, nstp
    class(GwtIstType) :: this
    integer(I4B), intent(in) :: idvsave
    integer(I4B), intent(in) :: idvprint
    ! -- local
    integer(I4B) :: ipflg
    integer(I4B) :: ibinun
    !
    ! -- Save cim to a binary file. ibinun is a flag where 1 indicates that
    !    cim should be written to a binary file if a binary file is open
    !    for it.
    ipflg = 0
    ibinun = 1
    if(idvsave == 0) ibinun = 0
    if (ibinun /= 0) then
      call this%ocd%ocd_ot(ipflg, kstp, nstp(kper), this%iout,                 &
                           iprint_opt=0, isav_opt=ibinun)
    endif
    !
    ! -- Print immobile domain concentrations to listing file
    if (idvprint /= 0) then
      call this%ocd%ocd_ot(ipflg, kstp, nstp(kper), this%iout,                      &
                           iprint_opt=idvprint, isav_opt=0)
    endif
  end subroutine ist_ot_dv
  
  subroutine ist_ot_bdsummary(this, kstp, kper, iout)
    use TdisModule, only: delt
    class(GwtIstType) :: this
    integer(I4B), intent(in) :: kstp
    integer(I4B), intent(in) :: kper
    integer(I4B), intent(in) :: iout
    integer(I4B) :: isuppress_output = 0
    !
    ! -- Write budget to list file
    call this%budget%reset()
    call this%budget%addentry(this%budterm, delt, budtxt, isuppress_output)
    call this%budget%budget_ot(kstp, kper, iout)
    
  end subroutine ist_ot_bdsummary
  
  subroutine ist_da(this)
! ******************************************************************************
! mst_da -- Deallocate variables
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryManagerModule, only: mem_deallocate
    ! -- dummy
    class(GwtIstType) :: this
! ------------------------------------------------------------------------------
    !
    ! -- Deallocate arrays if package was active
    if(this%inunit > 0) then
      call mem_deallocate(this%icimout)
      call mem_deallocate(this%idcy)
      call mem_deallocate(this%isrb)
      call mem_deallocate(this%kiter)
      call mem_deallocate(this%cim)
      call mem_deallocate(this%cimnew)
      call mem_deallocate(this%cimold)
      call mem_deallocate(this%zetaim)
      call mem_deallocate(this%thetaim)
      call mem_deallocate(this%bulk_density)
      call mem_deallocate(this%distcoef)
      call mem_deallocate(this%decay)
      call mem_deallocate(this%decaylast)
      call mem_deallocate(this%decayslast)
      call mem_deallocate(this%decay_sorbed)
      call mem_deallocate(this%strg)
      this%fmi => null()
      this%mst => null()
    endif
    !
    ! -- Scalars
    !
    ! -- Objects
    call this%budget%budget_da()
    deallocate(this%budget)
    call this%ocd%ocd_da()
    deallocate(this%ocd)
    !
    ! -- deallocate parent
    call this%BndType%bnd_da()
    !
    ! -- Return
    return
  end subroutine ist_da

  subroutine allocate_scalars(this)
! ******************************************************************************
! allocate_scalars
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryManagerModule, only: mem_allocate, mem_setptr
    use OutputControlData, only: ocd_cr
    ! -- dummy
    class(GwtIstType) :: this
    ! -- local
! ------------------------------------------------------------------------------
    !
    ! -- call standard BndType allocate scalars
    call this%BndType%allocate_scalars()
    !
    ! -- Allocate
    call mem_allocate(this%icimout, 'ICIMOUT', this%memoryPath)
    call mem_allocate(this%isrb, 'ISRB', this%memoryPath)
    call mem_allocate(this%idcy, 'IDCY', this%memoryPath)
    call mem_allocate(this%kiter, 'KITER', this%memoryPath)
    !
    ! -- Initialize
    this%icimout = 0
    this%isrb = 0
    this%idcy = 0
    this%kiter = 0
    !
    ! -- Create the ocd object, which is used to manage printing and saving
    !    of the immobile domain concentrations
    call ocd_cr(this%ocd)
    !
    ! -- Return
    return
  end subroutine allocate_scalars

  subroutine ist_allocate_arrays(this)
! ******************************************************************************
! ist_allocate_arrays -- allocate arraya
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryManagerModule, only: mem_allocate
    ! -- dummy
    class(GwtIstType),   intent(inout) :: this
    ! -- local
    integer(I4B) :: n
! ------------------------------------------------------------------------------
    !
    ! -- call standard BndType allocate scalars
    !    nbound and maxbound are 0 in order to keep memory footprint low
    call this%BndType%allocate_arrays()
    !
    ! -- allocate ist arrays of size nodes
    call mem_allocate(this%strg, this%dis%nodes, 'STRG', this%memoryPath)
    call mem_allocate(this%cim, this%dis%nodes, 'CIM', this%memoryPath)
    call mem_allocate(this%cimnew, this%dis%nodes, 'CIMNEW', this%memoryPath)
    call mem_allocate(this%cimold, this%dis%nodes, 'CIMOLD', this%memoryPath)
    call mem_allocate(this%zetaim, this%dis%nodes, 'ZETAIM', this%memoryPath)
    call mem_allocate(this%thetaim, this%dis%nodes, 'THETAIM', this%memoryPath)
    if (this%isrb == 0) then
      call mem_allocate(this%bulk_density, 1, 'BULK_DENSITY', this%memoryPath)
      call mem_allocate(this%distcoef,  1, 'DISTCOEF', this%memoryPath)
    else
      call mem_allocate(this%bulk_density, this%dis%nodes, 'BULK_DENSITY',     &
                        this%memoryPath)
      call mem_allocate(this%distcoef,  this%dis%nodes, 'DISTCOEF', this%memoryPath)
    endif
    if (this%idcy == 0) then
      call mem_allocate(this%decay, 1, 'DECAY', this%memoryPath)
      call mem_allocate(this%decaylast, 1, 'DECAYLAST', this%memoryPath)
    else
      call mem_allocate(this%decay, this%dis%nodes, 'DECAY', this%memoryPath)
      call mem_allocate(this%decaylast, this%dis%nodes, 'DECAYLAST', this%memoryPath)
    endif
    if (this%isrb == 0 .and. this%idcy == 0) then
      call mem_allocate(this%decayslast, 1, 'DECAYSLAST', this%memoryPath)
    else
      call mem_allocate(this%decayslast, this%dis%nodes, 'DECAYSLAST', this%memoryPath)
    end if
    call mem_allocate(this%decay_sorbed, 1, 'DECAY_SORBED', this%memoryPath)
    !
    ! -- initialize
    do n = 1, this%dis%nodes
      this%strg(n) = DZERO
      this%cim(n) = DZERO
      this%cimnew(n) = DZERO
      this%cimold(n) = DZERO
      this%zetaim(n) = DZERO
      this%thetaim(n) = DZERO
    enddo
    do n = 1, size(this%decay)
      this%decay(n) = DZERO
      this%decaylast(n) = DZERO
    enddo
    do n = 1, size(this%decayslast)
      this%decayslast(n) = DZERO
    enddo
    !
    ! -- Set pointers
    this%ocd%dis => this%dis
    !
    ! -- return
    return
  end subroutine ist_allocate_arrays

  subroutine read_options(this)
! ******************************************************************************
! read_options -- Read options
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use ConstantsModule,   only: LINELENGTH
    use SimModule,         only: ustop, store_error
    ! -- dummy
    class(GwtIstType), intent(inout) :: this
    ! -- local
    character(len=LINELENGTH) :: errmsg, keyword
    character(len=:), allocatable :: keyword2
    integer(I4B) :: ierr
    logical :: isfound, endOfBlock
    ! -- formats
    character(len=*), parameter :: fmtisvflow =                                &
      "(4x,'CELL-BY-CELL FLOW INFORMATION WILL BE SAVED TO BINARY FILE " //    &
      "WHENEVER ICBCFL IS NOT ZERO.')"
    character(len=*), parameter :: fmtisrb =                                   &
      "(4x,'LINEAR SORPTION IS SELECTED. ')"
    character(len=*), parameter :: fmtidcy1 =                               &
      "(4x,'FIRST-ORDER DECAY IS ACTIVE. ')"
    character(len=*), parameter :: fmtidcy2 =                               &
      "(4x,'ZERO-ORDER DECAY IS ACTIVE. ')"
! ------------------------------------------------------------------------------
    !
    ! -- get options block
    call this%parser%GetBlock('OPTIONS', isfound, ierr, blockRequired=.false., &
                              supportOpenClose=.true.)
    !
    ! -- parse options block if detected
    if (isfound) then
      write(this%iout,'(1x,a)') 'PROCESSING IMMOBILE STORAGE AND TRANSFER &
                                &OPTIONS'
      do
        call this%parser%GetNextLine(endOfBlock)
        if (endOfBlock) exit
        call this%parser%GetStringCaps(keyword)
        select case (keyword)
          case ('SAVE_FLOWS')
            this%ipakcb = -1
            write(this%iout, fmtisvflow)
          case('CIM')
            call this%parser%GetRemainingLine(keyword2)
            call this%ocd%set_option(keyword2, this%inunit, this%iout)
          case ('SORBTION', 'SORPTION')
            this%isrb = 1
            write(this%iout, fmtisrb)
          case ('FIRST_ORDER_DECAY')
            this%idcy = 1
            write(this%iout, fmtidcy1)
          case ('ZERO_ORDER_DECAY')
            this%idcy = 2
            write(this%iout, fmtidcy2)
          case default
            write(errmsg,'(4x,a,a)')'****ERROR. UNKNOWN IST OPTION: ',         &
                                     trim(keyword)
            call store_error(errmsg)
            call this%parser%StoreErrorUnit()
            call ustop()
        end select
      end do
      write(this%iout,'(1x,a)') 'END OF IMMOBILE STORAGE AND TRANSFER &
                                &OPTIONS'
    end if
    !
    ! -- Return
    return
  end subroutine read_options

  subroutine ist_read_dimensions(this)
! ******************************************************************************
! ist_read_dimensions -- override in order to skip dimensions
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(GwtIstType),intent(inout) :: this
    ! -- local
    ! -- format
! ------------------------------------------------------------------------------
    !
    ! -- return
    return
  end subroutine ist_read_dimensions

  subroutine read_data(this)
! ******************************************************************************
! read_data -- read the immodbile domain (griddata) block
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use ConstantsModule,   only: LINELENGTH
    use SimModule,         only: ustop, store_error, count_errors
    use MemoryManagerModule, only: mem_reallocate, mem_reassignptr
    ! -- dummy
    class(GwtIstType) :: this
    ! -- local
    character(len=LINELENGTH) :: errmsg, keyword
    character(len=:), allocatable :: line
    integer(I4B) :: istart, istop, lloc, ierr
    logical :: isfound, endOfBlock
    logical, dimension(7) :: lname
    character(len=24), dimension(7) :: aname
    ! -- formats
    ! -- data
    data aname(1) /'            BULK DENSITY'/
    data aname(2) /'DISTRIBUTION COEFFICIENT'/
    data aname(3) /'              DECAY RATE'/
    data aname(4) /'       DECAY SORBED RATE'/
    data aname(5) /'   INITIAL IMMOBILE CONC'/
    data aname(6) /'  FIRST ORDER TRANS RATE'/
    data aname(7) /'IMMOBILE DOMAIN POROSITY'/
! ------------------------------------------------------------------------------
    !
    ! -- initialize
    isfound = .false.
    lname(:) = .false.
    !
    ! -- get griddata block
    call this%parser%GetBlock('GRIDDATA', isfound, ierr)
    if(isfound) then
      write(this%iout,'(1x,a)')'PROCESSING GRIDDATA'
      do
        call this%parser%GetNextLine(endOfBlock)
        if (endOfBlock) exit
        call this%parser%GetStringCaps(keyword)
        call this%parser%GetRemainingLine(line)
        lloc = 1
        select case (keyword)
          case ('BULK_DENSITY')
            if (this%isrb == 0) &
              call mem_reallocate(this%bulk_density, this%dis%nodes,           &
                                  'BULK_DENSITY', trim(this%memoryPath))
            call this%dis%read_grid_array(line, lloc, istart, istop, this%iout,&
                                         this%parser%iuactive,                 &
                                         this%bulk_density, aname(1))
            lname(1) = .true.
          case ('DISTCOEF')
            if (this%isrb == 0) &
              call mem_reallocate(this%distcoef, this%dis%nodes, 'DISTCOEF',   &
                                trim(this%memoryPath))
            call this%dis%read_grid_array(line, lloc, istart, istop, this%iout,&
                                         this%parser%iuactive, this%distcoef,  &
                                         aname(2))
            lname(2) = .true.
          case ('DECAY')
            if (this%idcy == 0) &
              call mem_reallocate(this%decay, this%dis%nodes, 'DECAY',         &
                                  trim(this%memoryPath))
            call this%dis%read_grid_array(line, lloc, istart, istop, this%iout,&
                                         this%parser%iuactive, this%decay,     &
                                         aname(3))
            lname(3) = .true.
          case ('DECAY_SORBED')
            call mem_reallocate(this%decay_sorbed, this%dis%nodes,             &
                                'DECAY_SORBED', trim(this%memoryPath))
            call this%dis%read_grid_array(line, lloc, istart, istop, this%iout,&
                                         this%parser%iuactive,                 &
                                         this%decay_sorbed, aname(4))
            lname(4) = .true.
          case ('CIM')
            call this%dis%read_grid_array(line, lloc, istart, istop, this%iout,&
                                         this%parser%iuactive, this%cim,    &
                                         aname(5))
            lname(5) = .true.
          case ('ZETAIM')
            call this%dis%read_grid_array(line, lloc, istart, istop, this%iout,&
                                         this%parser%iuactive, this%zetaim,    &
                                         aname(6))
            lname(6) = .true.
          case ('THETAIM')
            call this%dis%read_grid_array(line, lloc, istart, istop, this%iout,&
                                         this%parser%iuactive, this%thetaim,    &
                                         aname(7))
            lname(7) = .true.
          case default
            write(errmsg,'(4x,a,a)')'ERROR. UNKNOWN GRIDDATA TAG: ',            &
                                     trim(keyword)
            call store_error(errmsg)
            call this%parser%StoreErrorUnit()
            call ustop()
        end select
      end do
      write(this%iout,'(1x,a)')'END PROCESSING GRIDDATA'
    else
      write(errmsg,'(1x,a)')'ERROR.  REQUIRED GRIDDATA BLOCK NOT FOUND.'
      call store_error(errmsg)
      call this%parser%StoreErrorUnit()
      call ustop()
    end if
    !
    ! -- Check for required sorption variables
    if (this%isrb > 0) then
      if (.not. lname(1)) then
        write(errmsg, '(1x,a)') 'ERROR.  SORPTION IS ACTIVE BUT BULK_DENSITY &
          &NOT SPECIFIED.  BULK_DENSITY MUST BE SPECIFIED IN GRIDDATA BLOCK.'
        call store_error(errmsg)
      endif
      if (.not. lname(2)) then
        write(errmsg, '(1x,a)') 'ERROR.  SORPTION IS ACTIVE BUT DISTRIBUTION &
          &COEFFICIENT NOT SPECIFIED.  DISTCOEF MUST BE SPECIFIED IN &
          &GRIDDATA BLOCK.'
        call store_error(errmsg)
      endif
    else
      if (lname(1)) then
        write(this%iout, '(1x,a)') 'WARNING.  SORPTION IS NOT ACTIVE BUT &
          &BULK_DENSITY WAS SPECIFIED.  BULK_DENSITY WILL HAVE NO AFFECT ON &
          &SIMULATION RESULTS.'
      endif
      if (lname(2)) then
        write(this%iout, '(1x,a)') 'WARNING.  SORPTION IS NOT ACTIVE BUT &
          &DISTRIBUTION COEFFICIENT WAS SPECIFIED.  DISTCOEF WILL HAVE &
          &NO AFFECT ON SIMULATION RESULTS.'
      endif
    endif
    !
    ! -- Check for required decay/production rate coefficients
    if (this%idcy > 0) then
      if (.not. lname(3)) then
        write(errmsg, '(1x,a)') 'ERROR.  FIRST OR ZERO ORDER DECAY IS &
          &ACTIVE BUT THE FIRST RATE COEFFICIENT IS NOT SPECIFIED.  &
          &DECAY MUST BE SPECIFIED IN GRIDDATA BLOCK.'
        call store_error(errmsg)
      endif
      if (.not. lname(4)) then
        !
        ! -- If DECAY_SORBED not specified and sorption is active, then set
        !    decay_sorbed equal to decay
        if (this%isrb > 0) then
          write(errmsg, '(a)') 'DECAY_SORBED not provided in GRIDDATA &
            &block but decay and sorption are active.  Specify DECAY_SORBED &
            &in GRIDDATA block.'
          call store_error(errmsg)
        endif
      endif
    else
      if (lname(3)) then
        write(this%iout, '(1x,a)') 'WARNING.  FIRST OR ZERO ORER DECAY &
          &IS NOT ACTIVE BUT DECAY WAS SPECIFIED.  DECAY WILL &
          &HAVE NO AFFECT ON SIMULATION RESULTS.'
      endif
      if (lname(4)) then
        write(this%iout, '(1x,a)') 'WARNING.  FIRST OR ZERO ORER DECAY &
          &IS NOT ACTIVE BUT DECAY_SORBED MUST  WAS SPECIFIED.  &
          &DECAY_SORBED MUST  WILL HAVE NO AFFECT ON SIMULATION &
          &RESULTS.'
      endif
    endif
    !
    ! -- Check for required dual domain arrays or warn if they are specified
    !    but won't be used.
    if (.not. lname(5)) then
      write(this%iout, '(1x,a)') 'WARNING.  DUAL DOMAIN IS ACTIVE BUT &
        &INITIAL IMMOBILE DOMAIN CONCENTRATION WAS NOT SPECIFIED.  &
        &SETTING CIM TO ZERO.'
    endif
    if (.not. lname(6)) then
      write(errmsg, '(1x,a)') 'ERROR.  DUAL DOMAIN IS ACTIVE BUT DUAL &
        &DOMAIN MASS TRANSFER RATE (ZETAIM) WAS NOT SPECIFIED.  ZETAIM &
        &MUST BE SPECIFIED IN GRIDDATA BLOCK.'
      call store_error(errmsg)
    endif
    if (.not. lname(7)) then
      write(errmsg, '(1x,a)') 'ERROR.  DUAL DOMAIN IS ACTIVE BUT &
        &IMMOBILE DOMAIN POROSITY (THETAIM) WAS NOT SPECIFIED.  THETAIM &
        &MUST BE SPECIFIED IN GRIDDATA BLOCK.'
      call store_error(errmsg)
    endif
    !
    ! -- terminate if errors
    if(count_errors() > 0) then
      call this%parser%StoreErrorUnit()
      call ustop()
    endif
    !
    ! -- Return
    return
  end subroutine read_data

!  subroutine calcddhcofrhs(thetaim, vcell, delt, swtpdt, swt, thetamfrac,      &
!                           thetaimfrac, rhob, kd, lambda1im, lambda2im,        &
!                           gamma1im, gamma2im, zetaim, cimt, hcof, rhs)
!! ******************************************************************************
!! calcddhcofrhs -- calculate the hcof and rhs contributions for dual domain
!!   mass transfer
!! ******************************************************************************
!!
!!    SPECIFICATIONS:
!! ------------------------------------------------------------------------------
!    ! -- dummy
!    real(DP), intent(in) :: thetaim
!    real(DP), intent(in) :: vcell
!    real(DP), intent(in) :: delt
!    real(DP), intent(in) :: swtpdt
!    real(DP), intent(in) :: swt
!    real(DP), intent(in) :: thetamfrac
!    real(DP), intent(in) :: thetaimfrac
!    real(DP), intent(in) :: rhob
!    real(DP), intent(in) :: kd
!    real(DP), intent(in) :: lambda1im
!    real(DP), intent(in) :: lambda2im
!    real(DP), intent(in) :: gamma1im
!    real(DP), intent(in) :: gamma2im
!    real(DP), intent(in) :: zetaim
!    real(DP), intent(in) :: cimt
!    real(DP), intent(inout) :: hcof
!    real(DP), intent(inout) :: rhs    
!    ! -- local
!    real(DP), dimension(9) :: ddterm
!    real(DP) :: f
!! ------------------------------------------------------------------------------
!    !
!    ! -- calculate the ddterms
!    call calcddterms(thetaim, vcell, delt, swtpdt, swt, thetamfrac,            &
!                     thetaimfrac, rhob, kd, lambda1im, lambda2im,              &
!                     gamma1im, gamma2im, zetaim, cimt, ddterm, f)
!    !
!    ! -- calculate hcof
!    hcof = ddterm(9) ** 2 / f - ddterm(9)
!    !
!    ! -- calculate rhs, and switch the sign because this term needs to
!    !    be moved to the left hand side
!    rhs = (ddterm(2) + ddterm(4)) * cimt - ddterm(7) - ddterm(8)
!    rhs = rhs * ddterm(9) / f
!    rhs = -rhs
!    !
!    ! -- Return
!    return
!  end subroutine calcddhcofrhs

!  subroutine calcddbud(this, budterm, cnew)
!! ******************************************************************************
!! calcddbud -- calculate the individual budget terms for the immobile domain
!! ******************************************************************************
!!
!!    SPECIFICATIONS:
!! ------------------------------------------------------------------------------
!    ! -- modules
!    use TdisModule, only: delt
!    ! -- dummy
!    class(GwtIstType) :: this
!    real(DP), dimension(:, :), intent(inout) :: budterm
!    real(DP), dimension(:), intent(in) :: cnew
!    ! -- local
!    integer(I4B) :: n, i
!    real(DP) :: vcell
!    real(DP) :: swt
!    real(DP) :: swtpdt
!    real(DP) :: thetamfrac
!    real(DP) :: thetaimfrac
!    real(DP) :: kd
!    real(DP) :: lambda1im
!    real(DP) :: lambda2im
!    real(DP) :: gamma1im
!    real(DP) :: gamma2im
!    real(DP) :: ddterm(9)
!    real(DP) :: f
!    real(DP) :: cimt
!    real(DP) :: cimtpdt
!    real(DP) :: rate
!    real(DP) :: rhob
!    real(DP) :: ctmp
!! ------------------------------------------------------------------------------
!    !
!    ! -- Calculate cim
!    do n = 1, this%dis%nodes
!      if (this%ibound(n) <= 0) cycle
!      vcell = this%dis%area(n) * (this%dis%top(n) - this%dis%bot(n))
!      swt = this%fmi%gwfsatold(n, delt)
!      swtpdt = this%fmi%gwfsat(n)
!      thetamfrac = this%mst%get_thetamfrac(n)
!      thetaimfrac = this%mst%get_thetaimfrac(n, this%thetaim(n))
!      kd = DZERO
!      rhob = DZERO
!      lambda1im = DZERO
!      lambda2im = DZERO
!      gamma1im = DZERO
!      gamma2im = DZERO
!      if (this%idcy == 1) lambda1im = this%decay(n)
!      if (this%idcy == 2) then
!        !gamma1im = this%decay(n)
!        ctmp = max(cnew(n), DZERO)
!        cimtpdt = calcddconc(this%thetaim(n), vcell, delt, swtpdt, swt,        &
!                             thetamfrac, thetaimfrac, rhob, kd,                &
!                             lambda1im, lambda2im, gamma1im, gamma2im,         &
!                             this%zetaim(n), cimt, ctmp)
!        gamma1im = get_zero_order_decay(this%decay(n), this%decaylast(n), 0,   &
!                        this%cimold(n), cimtpdt, delt)
!      end if
!      if (this%isrb > 0) then
!        kd = this%distcoef(n)
!        rhob = this%bulk_density(n)
!        if (this%idcy == 1) lambda2im = this%decay_sorbed(n)
!        if (this%idcy == 2) gamma2im = this%decay_sorbed(n)
!      end if
!      !
!      ! -- calculate the ddterms
!      cimt = this%cimold(n)
!      call calcddterms(this%thetaim(n), vcell, delt, swtpdt, swt, thetamfrac,  &
!                       thetaimfrac, rhob, kd, lambda1im,                       &
!                       lambda2im, gamma1im, gamma2im, this%zetaim(n), cimt,    &
!                       ddterm, f)
!      ctmp = max(cnew(n), DZERO)
!      cimtpdt = calcddconc(this%thetaim(n), vcell, delt, swtpdt, swt,          &
!                           thetamfrac, thetaimfrac, rhob, kd,                  &
!                           lambda1im, lambda2im, gamma1im, gamma2im,           &
!                           this%zetaim(n), cimt, ctmp)
!      !
!      ! -- calculate STORAGE-AQUEOUS
!      i = 1
!      rate = - ddterm(1) * cimtpdt + ddterm(2) * cimt
!      if (rate > DZERO) then
!        budterm(1, i) = budterm(1, i) + rate
!      else
!        budterm(2, i) = budterm(2, i) - rate
!      endif
!      !
!      ! -- calculate STORAGE-SORBED
!      i = 2
!      rate = - ddterm(3) * cimtpdt + ddterm(4) * cimt
!      if (rate > DZERO) then
!        budterm(1, i) = budterm(1, i) + rate
!      else
!        budterm(2, i) = budterm(2, i) - rate
!      endif
!      !
!      ! -- calculate DECAY-AQUEOUS
!      i = 3
!      rate = DZERO
!      if (this%idcy == 1) then
!        rate = - ddterm(5) * cimtpdt
!      else if (this%idcy == 2) then
!        rate = - ddterm(7)
!      else
!        rate = DZERO
!      endif
!      if (rate > DZERO) then
!        budterm(1, i) = budterm(1, i) + rate
!      else
!        budterm(2, i) = budterm(2, i) - rate
!      endif
!      !
!      ! -- calculate DECAY-SORBED
!      i = 4
!      if (this%idcy == 1) then
!        rate = - ddterm(6) * cimtpdt
!      else if (this%idcy == 2) then
!        rate = - ddterm(8)
!      else
!        rate = DZERO
!      endif
!      if (rate > DZERO) then
!        budterm(1, i) = budterm(1, i) + rate
!      else
!        budterm(2, i) = budterm(2, i) - rate
!      endif
!      !
!      ! -- calculate MOBILE-DOMAIN
!      i = 5
!      rate = ddterm(9) * cnew(n) - ddterm(9) * cimtpdt
!      if (rate > DZERO) then
!        budterm(1, i) = budterm(1, i) + rate
!      else
!        budterm(2, i) = budterm(2, i) - rate
!      endif
!      !
!    enddo
!    !
!    ! -- Return
!    return
!  end subroutine calcddbud

!  subroutine calccim(this, cimnew, cnew, cimold)
!! ******************************************************************************
!! calccim -- if dual domain mass transfer, then calculate immobile domain
!!   concentration using cnew (concentration solution for this time step)
!! ******************************************************************************
!!
!!    SPECIFICATIONS:
!! ------------------------------------------------------------------------------
!    ! -- modules
!    use TdisModule, only: delt
!    ! -- dummy
!    class(GwtIstType) :: this
!    real(DP), dimension(:), intent(inout) :: cimnew
!    real(DP), dimension(:), intent(in) :: cnew
!    real(DP), dimension(:), intent(in) :: cimold
!    ! -- local
!    integer(I4B) :: n
!    real(DP) :: vcell
!    real(DP) :: swt
!    real(DP) :: swtpdt
!    real(DP) :: thetamfrac
!    real(DP) :: thetaimfrac
!    real(DP) :: kd
!    real(DP) :: lambda1im
!    real(DP) :: lambda2im
!    real(DP) :: gamma1im
!    real(DP) :: gamma2im
!    real(DP) :: ctmp
!    real(DP) :: rhob
!    real(DP) :: cimtpdt
!! ------------------------------------------------------------------------------
!    !
!    ! -- Calculate cim
!    do n = 1, this%dis%nodes
!      if(this%ibound(n) <= 0) cycle
!      vcell = this%dis%area(n) * (this%dis%top(n) - this%dis%bot(n))
!      swt = this%fmi%gwfsatold(n, delt)
!      swtpdt = this%fmi%gwfsat(n)
!      thetamfrac = this%mst%get_thetamfrac(n)
!      thetaimfrac = this%mst%get_thetaimfrac(n, this%thetaim(n))
!      kd = DZERO
!      rhob = DZERO
!      lambda1im = DZERO
!      lambda2im = DZERO
!      gamma1im = DZERO
!      gamma2im = DZERO
!      if (this%idcy == 1) lambda1im = this%decay(n)
!      if (this%idcy == 2) then
!        !gamma1im = this%decay(n)
!        ctmp = max(cnew(n), DZERO)
!        cimtpdt = calcddconc(this%thetaim(n), vcell, delt, swtpdt, swt,        &
!                             thetamfrac, thetaimfrac, rhob, kd,                &
!                             lambda1im, lambda2im, gamma1im, gamma2im,         &
!                             this%zetaim(n), this%cimold(n), ctmp)
!        gamma1im = get_zero_order_decay(this%decay(n), this%decaylast(n), 0,   &
!                        this%cimold(n), cimtpdt, delt)
!      end if
!      if (this%isrb > 0) then
!        kd = this%distcoef(n)
!        rhob = this%bulk_density(n)
!        if (this%idcy == 1) lambda2im = this%decay_sorbed(n)
!        if (this%idcy == 2) gamma2im = this%decay_sorbed(n)
!      end if
!      !
!      ! -- calculate cimnew using cnew
!      ctmp = calcddconc(this%thetaim(n), vcell, delt, swtpdt, swt,           &
!                        thetamfrac, thetaimfrac, rhob, kd,                   &
!                        lambda1im, lambda2im, gamma1im, gamma2im,            &
!                        this%zetaim(n), cimold(n), cnew(n))
!      cimnew(n) = ctmp
!    enddo
!    !
!    ! -- Return
!    return
!  end subroutine calccim
!
!  function calcddconc(thetaim, vcell, delt, swtpdt, swt, thetamfrac,           &
!                      thetaimfrac, rhob, kd, lambda1im, lambda2im, gamma1im,   &
!                      gamma2im, zetaim, cimt, ctpdt) result (ddconc)
!! ******************************************************************************
!! calcddconc -- Calculate and return the concentration of the immobile domain
!!   for a single cell.
!! ******************************************************************************
!!
!!    SPECIFICATIONS:
!! ------------------------------------------------------------------------------
!    ! -- dummy
!    real(DP), intent(in) :: thetaim
!    real(DP), intent(in) :: vcell
!    real(DP), intent(in) :: delt
!    real(DP), intent(in) :: swtpdt
!    real(DP), intent(in) :: swt
!    real(DP), intent(in) :: thetamfrac
!    real(DP), intent(in) :: thetaimfrac
!    real(DP), intent(in) :: rhob
!    real(DP), intent(in) :: kd
!    real(DP), intent(in) :: lambda1im
!    real(DP), intent(in) :: lambda2im
!    real(DP), intent(in) :: gamma1im
!    real(DP), intent(in) :: gamma2im
!    real(DP), intent(in) :: zetaim
!    real(DP), intent(in) :: cimt
!    real(DP), intent(in) :: ctpdt
!    ! -- result
!    real(DP) :: ddconc
!    ! -- local
!    real(DP), dimension(9) :: ddterm
!    real(DP) :: f
!! ------------------------------------------------------------------------------
!    !
!    ! -- initialize
!    ddconc = DZERO
!    !
!    ! -- calculate the ddterms
!    call calcddterms(thetaim, vcell, delt, swtpdt, swt, thetamfrac,            &
!                     thetaimfrac, rhob, kd, lambda1im, lambda2im,              &
!                     gamma1im, gamma2im, zetaim, cimt, ddterm, f)
!    !
!    ! -- calculate ddconc
!    ddconc = (ddterm(2) + ddterm(4)) * cimt + ddterm(9) * ctpdt - ddterm(7)    &
!             - ddterm(8)
!    ddconc = ddconc / f
!    !
!    ! -- Return
!    return
!  end function calcddconc
                      
!  subroutine calcddterms(thetaim, vcell, delt, swtpdt, swt, thetamfrac,        &
!                         thetaimfrac, rhob, kd, lambda1im, lambda2im,          &
!                         gamma1im, gamma2im, zetaim, cimt, ddterm, f)
!! ******************************************************************************
!! calcddterms -- Calculate the terms for the immobile domain mass balance
!!   equation.
!! ******************************************************************************
!!
!!    SPECIFICATIONS:
!! ------------------------------------------------------------------------------
!    ! -- dummy
!    real(DP), intent(in) :: thetaim
!    real(DP), intent(in) :: vcell
!    real(DP), intent(in) :: delt
!    real(DP), intent(in) :: swtpdt
!    real(DP), intent(in) :: swt
!    real(DP), intent(in) :: thetamfrac
!    real(DP), intent(in) :: thetaimfrac
!    real(DP), intent(in) :: rhob
!    real(DP), intent(in) :: kd
!    real(DP), intent(in) :: lambda1im
!    real(DP), intent(in) :: lambda2im
!    real(DP), intent(in) :: gamma1im
!    real(DP), intent(in) :: gamma2im
!    real(DP), intent(in) :: zetaim
!    real(DP), intent(in) :: cimt
!    real(DP), dimension(:), intent(inout) :: ddterm
!    real(DP), intent(inout) :: f
!    ! -- local
!    real(DP) :: tled
!! ------------------------------------------------------------------------------
!    !
!    ! -- initialize
!    tled = DONE / delt
!    !
!    ! -- Calculate terms.  These terms correspond to the concentration 
!    !    coefficients in equation 7-4 of the GWT model report
!    ddterm(1) = thetaim * vcell * tled
!    ddterm(2) = thetaim * vcell * tled
!    ddterm(3) = thetaimfrac * rhob * vcell * kd * tled
!    ddterm(4) = thetaimfrac * rhob * vcell * kd * tled
!    ddterm(5) = thetaim * lambda1im * vcell
!    ddterm(6) = thetaimfrac * lambda2im * rhob * kd * vcell
!    ddterm(7) = thetaim * gamma1im * vcell
!    ddterm(8) = thetaimfrac * gamma2im * rhob * vcell
!    ddterm(9) = vcell * swtpdt * zetaim
!    !
!    ! -- calculate denominator term, f
!    f = ddterm(1) + ddterm(3) + ddterm(5) + ddterm(6) + ddterm(9)
!    !
!    ! -- Return
!    return
!  end subroutine calcddterms

                         
  subroutine get_ddterm(thetaim, vcell, delt, swtpdt,                          &
                        thetaimfrac, rhob, kd, lambda1im, lambda2im,           &
                        gamma1im, gamma2im, zetaim, ddterm, f)
! ******************************************************************************
! calcddterms -- Calculate the terms for the immobile domain mass balance
!   equation.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    real(DP), intent(in) :: thetaim
    real(DP), intent(in) :: vcell
    real(DP), intent(in) :: delt
    real(DP), intent(in) :: swtpdt
    real(DP), intent(in) :: thetaimfrac
    real(DP), intent(in) :: rhob
    real(DP), intent(in) :: kd
    real(DP), intent(in) :: lambda1im
    real(DP), intent(in) :: lambda2im
    real(DP), intent(in) :: gamma1im
    real(DP), intent(in) :: gamma2im
    real(DP), intent(in) :: zetaim
    real(DP), dimension(:), intent(inout) :: ddterm
    real(DP), intent(inout) :: f
    ! -- local
    real(DP) :: tled
! ------------------------------------------------------------------------------
    !
    ! -- initialize
    tled = DONE / delt
    !
    ! -- Calculate terms.  These terms correspond to the concentration 
    !    coefficients in equation 7-4 of the GWT model report
    ddterm(1) = thetaim * vcell * tled
    ddterm(2) = thetaim * vcell * tled
    ddterm(3) = thetaimfrac * rhob * vcell * kd * tled
    ddterm(4) = thetaimfrac * rhob * vcell * kd * tled
    ddterm(5) = thetaim * lambda1im * vcell
    ddterm(6) = thetaimfrac * lambda2im * rhob * kd * vcell
    ddterm(7) = thetaim * gamma1im * vcell
    ddterm(8) = thetaimfrac * gamma2im * rhob * vcell
    ddterm(9) = vcell * swtpdt * zetaim
    !
    ! -- calculate denominator term, f
    f = ddterm(1) + ddterm(3) + ddterm(5) + ddterm(6) + ddterm(9)
    !
    ! -- Return
    return
  end subroutine get_ddterm
                         
  subroutine get_hcofrhs(ddterm, f, cimold, hcof, rhs)
! ******************************************************************************
! get_hcofrhs -- calculate the hcof and rhs contributions for dual domain
!   mass transfer
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    real(DP), dimension(:), intent(in) :: ddterm
    real(DP), intent(in) :: f
    real(DP), intent(in) :: cimold
    real(DP), intent(inout) :: hcof
    real(DP), intent(inout) :: rhs    
! ------------------------------------------------------------------------------
    !
    ! -- calculate hcof
    hcof = ddterm(9) ** 2 / f - ddterm(9)
    !
    ! -- calculate rhs, and switch the sign because this term needs to
    !    be moved to the left hand side
    rhs = (ddterm(2) + ddterm(4)) * cimold - ddterm(7) - ddterm(8)
    rhs = rhs * ddterm(9) / f
    rhs = -rhs
    !
    ! -- Return
    return
  end subroutine get_hcofrhs

  function get_ddconc(ddterm, f, cimold, cnew) result (cimnew)
! ******************************************************************************
! calcddconc -- Calculate and return the concentration of the immobile domain
!   for a single cell.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    real(DP), dimension(:), intent(in) :: ddterm
    real(DP), intent(in) :: f
    real(DP), intent(in) :: cimold
    real(DP), intent(in) :: cnew
    ! -- result
    real(DP) :: cimnew
    ! -- local
! ------------------------------------------------------------------------------
    !
    ! -- calculate ddconc
    cimnew = (ddterm(2) + ddterm(4)) * cimold + ddterm(9) * cnew - ddterm(7)    &
             - ddterm(8)
    cimnew = cimnew / f
    !
    ! -- Return
    return
  end function get_ddconc
  
  subroutine accumulate_budterm(budterm, ddterm, cimnew, cimold, cnew, idcy)
! ******************************************************************************
! accumulate_budterm -- calculate the individual budget terms for the immobile domain
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    real(DP), dimension(:, :), intent(inout) :: budterm
    real(DP), dimension(:), intent(in) :: ddterm
    real(DP), intent(in) :: cimnew
    real(DP), intent(in) :: cimold
    real(DP), intent(in) :: cnew
    integer(I4B), intent(in) :: idcy
    ! -- local
    real(DP) :: rate
    integer(I4B) :: i
! ------------------------------------------------------------------------------
    !
    ! -- calculate STORAGE-AQUEOUS
    i = 1
    rate = - ddterm(1) * cimnew + ddterm(2) * cimold
    if (rate > DZERO) then
      budterm(1, i) = budterm(1, i) + rate
    else
      budterm(2, i) = budterm(2, i) - rate
    endif
    !
    ! -- calculate STORAGE-SORBED
    i = 2
    rate = - ddterm(3) * cimnew + ddterm(4) * cimold
    if (rate > DZERO) then
      budterm(1, i) = budterm(1, i) + rate
    else
      budterm(2, i) = budterm(2, i) - rate
    endif
    !
    ! -- calculate DECAY-AQUEOUS
    i = 3
    rate = DZERO
    if (idcy == 1) then
      rate = - ddterm(5) * cimnew
    else if (idcy == 2) then
      rate = - ddterm(7)
    else
      rate = DZERO
    endif
    if (rate > DZERO) then
      budterm(1, i) = budterm(1, i) + rate
    else
      budterm(2, i) = budterm(2, i) - rate
    endif
    !
    ! -- calculate DECAY-SORBED
    i = 4
    if (idcy == 1) then
      rate = - ddterm(6) * cimnew
    else if (idcy == 2) then
      rate = - ddterm(8)
    else
      rate = DZERO
    endif
    if (rate > DZERO) then
      budterm(1, i) = budterm(1, i) + rate
    else
      budterm(2, i) = budterm(2, i) - rate
    endif
    !
    ! -- calculate MOBILE-DOMAIN
    i = 5
    rate = ddterm(9) * cnew - ddterm(9) * cimnew
    if (rate > DZERO) then
      budterm(1, i) = budterm(1, i) + rate
    else
      budterm(2, i) = budterm(2, i) - rate
    endif
    !
    !
    ! -- Return
    return
  end subroutine accumulate_budterm

end module GwtIstModule