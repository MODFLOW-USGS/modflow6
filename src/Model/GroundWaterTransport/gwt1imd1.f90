module GwtImdModule

  use KindModule,             only: DP, I4B
  use ConstantsModule,        only: DONE, DZERO, LENFTYPE, LENPACKAGENAME,     &
                                    LENBUDTXT
  use BndModule,              only: BndType
  use BudgetModule,           only: BudgetType
  use GwtFmiModule,           only: GwtFmiType
  use GwtStoModule,           only: GwtStoType
  !
  implicit none
  !
  private
  public :: imd_create
  !
  character(len=LENFTYPE)       :: ftype = 'IMD'
  character(len=LENPACKAGENAME) :: text  = ' IMMOBILE DOMAIN'
  integer(I4B), parameter :: NBDITEMS = 5
  character(len=LENBUDTXT), dimension(NBDITEMS) :: budtxt
  data budtxt / ' STORAGE-AQUEOUS', '  STORAGE-SORBED', &
                '   DECAY-AQUEOUS', '    DECAY-SORBED', &
                '   MOBILE-DOMAIN' /
  !
  type, extends(BndType) :: GwtImdType
    
    type(GwtFmiType), pointer                        :: fmi => null()           ! pointer to fmi object
    type(GwtStoType), pointer                        :: sto => null()           ! pointer to sto object
    
    integer(I4B), pointer                            :: irorder => null()       ! order of reaction rate (0:none, 1:first, 2:zero)
    integer(I4B), pointer                            :: isrb => null()          ! sorbtion active flag (0:off, 1:on)
    real(DP), dimension(:), pointer, contiguous      :: cim => null()           ! concentration for immobile domain
    real(DP), dimension(:), pointer, contiguous      :: zetaim => null()        ! mass transfer rate to immobile domain
    real(DP), dimension(:), pointer, contiguous      :: thetaim => null()       ! porosity of the immobile domain
    real(DP), dimension(:), pointer, contiguous      :: rhob => null()          ! bulk density
    real(DP), dimension(:), pointer, contiguous      :: distcoef => null()      ! distribution coefficient
    real(DP), dimension(:), pointer, contiguous      :: rc1 => null()           ! first or zero order rate constant for liquid
    real(DP), dimension(:), pointer, contiguous      :: rc2 => null()           ! first or zero order rate constant for sorbed mass
    real(DP), dimension(:), pointer, contiguous      :: strg => null()          ! mass transfer rate
    
    type(BudgetType), pointer                        :: budget => null()        ! budget object
    
  contains
  
    procedure :: bnd_ar => imd_ar
    procedure :: bnd_rp => imd_rp
    procedure :: bnd_fc => imd_fc
    procedure :: bnd_bd => imd_bd
    procedure :: bnd_ot => imd_ot
    procedure :: allocate_scalars
    procedure :: read_dimensions => imd_read_dimensions
    procedure :: read_options
    procedure, private :: imd_allocate_arrays
    procedure, private :: read_data
    procedure, private :: calcddbud
    procedure, private :: calccim
    
  end type GwtImdType
  
  contains
  
  subroutine imd_create(packobj, id, ibcnum, inunit, iout, namemodel, pakname, &
                        fmi, sto)
! ******************************************************************************
! imd_create -- Create a New Immobile Domain Package
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
    type(GwtImdType), pointer :: imdobj
    type(GwtFmiType), pointer :: fmi
    type(GwtStoType), pointer :: sto
! ------------------------------------------------------------------------------
    !
    ! -- allocate the object and assign values to object variables
    allocate(imdobj)
    packobj => imdobj
    !
    ! -- create name and origin
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
    ! -- Point IMD specific variables
    imdobj%fmi => fmi
    imdobj%sto => sto
    !
    ! -- return
    return
  end subroutine imd_create

  subroutine imd_ar(this)
! ******************************************************************************
! imd_ar -- Allocate and Read
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use BudgetModule, only: budget_cr
    ! -- dummy
    class(GwtImdType), intent(inout) :: this
    ! -- local
    ! -- formats
    character(len=*), parameter :: fmtimd =                                    &
      "(1x,/1x,'IMD -- IMMODBILE DOMAIN PACKAGE, VERSION 1, 12/24/2018',       &
      &' INPUT READ FROM UNIT ', i0, //)"
! ------------------------------------------------------------------------------
    !
    ! --print a message identifying the immobile domain package.
    write(this%iout, fmtimd) this%inunit
    !
    ! -- Read immobile domain options
    call this%read_options()
    !
    ! -- Allocate arrays
    call this%imd_allocate_arrays()
    !
    ! -- read the data block
    call this%read_data()
    !
    ! -- add thetaim to the prsity2 accumulator in sto package
    call this%sto%addto_prsity2(this%thetaim)
    !
    ! -- setup the immobile domain budget
    call budget_cr(this%budget, this%origin)
    call this%budget%budget_df(NBDITEMS, 'MASS', 'M', bdzone=this%name)
    !
    ! -- Return
    return
  end subroutine imd_ar
  
  subroutine imd_read_dimensions(this)
! ******************************************************************************
! imd_read_dimensions -- override in order to skip dimensions
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(GwtImdType),intent(inout) :: this
    ! -- local
    ! -- format
! ------------------------------------------------------------------------------
    !
    ! -- return
    return
  end subroutine imd_read_dimensions

  subroutine imd_rp(this)
! ******************************************************************************
! imd_rp -- override in order to skip reading for imd package
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(GwtImdType),intent(inout) :: this
    ! -- local
    ! -- format
! ------------------------------------------------------------------------------
    !
    ! -- return
    return
  end subroutine imd_rp

  subroutine imd_fc(this, rhs, ia, idxglo, amatsln)
! ******************************************************************************
! imd_fc -- Calculate coefficients and fill amat and rhs
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use TdisModule, only: delt
    ! -- dummy
    class(GwtImdType) :: this
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
    real(DP) :: thetamfrac
    real(DP) :: thetaimfrac
    real(DP) :: kd
    real(DP) :: lambda1im
    real(DP) :: lambda2im
    real(DP) :: gamma1im
    real(DP) :: gamma2im
! ------------------------------------------------------------------------------
    !
    ! -- set variables
    tled = DONE / delt
    !
    ! -- loop through and calculate sorption contribution to hcof and rhs
    do n = 1, this%dis%nodes
      !
      ! -- skip if transport inactive
      if(this%ibound(n) <= 0) cycle
      !
      ! -- calculate new and old water volumes
      vcell = this%dis%area(n) * (this%dis%top(n) - this%dis%bot(n))
      swtpdt = this%fmi%gwfsat(n)
      swt = this%fmi%gwfsatold(n, delt)
      idiag = ia(n)
      !
      ! -- Set thetamfrac and thetaimfrac
      thetamfrac = this%sto%get_thetamfrac(n)
      thetaimfrac = this%sto%get_thetaimfrac(n, this%thetaim(n))
      !
      ! -- Add dual domain mass transfer contributions to rhs and hcof
      kd = DZERO
      lambda1im = DZERO
      lambda2im = DZERO
      gamma1im = DZERO
      gamma2im = DZERO
      if (this%isrb > 0) kd = this%distcoef(n)
      if (this%irorder == 1) lambda1im = this%rc1(n)
      if (this%irorder == 2) gamma1im = this%rc1(n)
      if (this%irorder == 1) lambda2im = this%rc2(n)
      if (this%irorder == 2) gamma2im = this%rc2(n)
      call calcddhcofrhs(this%thetaim(n), vcell, delt, swtpdt, swt,          &
                          thetamfrac, thetaimfrac, this%rhob(n), kd,         &
                          lambda1im, lambda2im, gamma1im, gamma2im,          &
                          this%zetaim(n), this%cim(n), hhcof, rrhs)
      amatsln(idxglo(idiag)) = amatsln(idxglo(idiag)) + hhcof
      rhs(n) = rhs(n) + rrhs
      !
    enddo
    !
    ! -- Return
    return
  end subroutine imd_fc
  
  subroutine imd_bd(this, x, idvfl, icbcfl, ibudfl, icbcun, iprobs,            &
                    isuppress_output, model_budget, imap, iadv)
! ******************************************************************************
! imd_bd -- Calculate Budget
! Note that the compact budget will always be used.
! Subroutine: (1) Process each package entry
!             (2) Write output
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use TdisModule, only: delt
    use ConstantsModule, only: LENBOUNDNAME, DZERO
    use BudgetModule, only: BudgetType
    ! -- dummy
    class(GwtImdType) :: this
    real(DP),dimension(:),intent(in) :: x
    integer(I4B), intent(in) :: idvfl
    integer(I4B), intent(in) :: icbcfl
    integer(I4B), intent(in) :: ibudfl
    integer(I4B), intent(in) :: icbcun
    integer(I4B), intent(in) :: iprobs
    integer(I4B), intent(in) :: isuppress_output
    type(BudgetType), intent(inout) :: model_budget
    integer(I4B), dimension(:), optional, intent(in) :: imap
    integer(I4B), optional, intent(in) :: iadv
    ! -- local
    integer(I4B) :: ibinun
    real(DP) :: ratin, ratout
    integer(I4B) :: n
    real(DP) :: rate
    real(DP) :: swt, swtpdt
    real(DP) :: hhcof, rrhs
    real(DP) :: vcell
    real(DP) :: thetamfrac
    real(DP) :: thetaimfrac
    real(DP) :: kd
    real(DP) :: lambda1im
    real(DP) :: lambda2im
    real(DP) :: gamma1im
    real(DP) :: gamma2im
    real(DP), dimension(2, NBDITEMS) :: budterm
    ! -- formats
! ------------------------------------------------------------------------------
    !
    ! -- Clear accumulators and set flags
    ratin = DZERO
    ratout = DZERO
    !
    ! -- Set unit number for binary output
    if (this%ipakcb < 0) then
      ibinun = icbcun
    else if (this%ipakcb == 0) then
      ibinun = 0
    else
      ibinun = this%ipakcb
    end if
    if (icbcfl == 0) ibinun = 0
    if (isuppress_output /= 0) ibinun = 0
    !
    ! -- Reset budget object for this immobile domain package
    call this%budget%reset()
    !
    ! -- Calculate sorption change
    do n = 1, this%dis%nodes
      !
      ! -- skip if transport inactive
      if(this%ibound(n) <= 0) cycle
      !
      ! -- calculate new and old water volumes
      vcell = this%dis%area(n) * (this%dis%top(n) - this%dis%bot(n))
      swtpdt = this%fmi%gwfsat(n)
      swt = this%fmi%gwfsatold(n, delt)
      !
      ! -- Set thetamfrac and thetaimfrac
      thetamfrac = this%sto%get_thetamfrac(n)
      thetaimfrac = this%sto%get_thetaimfrac(n, this%thetaim(n))
      !
      ! -- Calculate exchange with immobile domain
      rate = DZERO
      hhcof = DZERO
      rrhs = DZERO
      kd = DZERO
      lambda1im = DZERO
      lambda2im = DZERO
      gamma1im = DZERO
      gamma2im = DZERO
      if (this%isrb > 0) kd = this%distcoef(n)
      if (this%irorder == 1) lambda1im = this%rc1(n)
      if (this%irorder == 2) gamma1im = this%rc1(n)
      if (this%irorder == 1) lambda2im = this%rc2(n)
      if (this%irorder == 2) gamma2im = this%rc2(n)
      call calcddhcofrhs(this%thetaim(n), vcell, delt, swtpdt, swt,          &
                          thetamfrac, thetaimfrac, this%rhob(n), kd,         &
                          lambda1im, lambda2im, gamma1im, gamma2im,          &
                          this%zetaim(n), this%cim(n), hhcof, rrhs)
      rate = hhcof * x(n) - rrhs
      if (rate < DZERO) then
        ratout = ratout - rate
      else
        ratin = ratin + rate
      endif
      !
    enddo
    !
    ! -- Store the rates for the GWT model budget, which is the transfer
    !    from the immobile domain to the mobile domain.
    call model_budget%addentry(ratin, ratout, delt, this%text,                 &
                               isuppress_output, this%name)
    !
    ! -- Calculate and store the rates for the immobile domain
    budterm(:, :) = DZERO
    call this%calcddbud(budterm, x)
    call this%budget%addentry(budterm, delt, budtxt, isuppress_output)
    !
    ! -- Save the simulated values to the ObserveType objects
    if (iprobs /= 0 .and. this%obs%npakobs > 0) then
      call this%bnd_bd_obs()
    endif
    !
    ! -- update cim
    call this%calccim(this%cim, x)
    !
    ! -- return
    return
  end subroutine imd_bd

  subroutine imd_ot(this, kstp, kper, iout, ihedfl, ibudfl)
! ******************************************************************************
! imd_ot -- Output package budget
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(GwtImdType) :: this
    integer(I4B),intent(in) :: kstp
    integer(I4B),intent(in) :: kper
    integer(I4B),intent(in) :: iout
    integer(I4B),intent(in) :: ihedfl
    integer(I4B),intent(in) :: ibudfl
    ! -- format
! ------------------------------------------------------------------------------
    !
    ! -- Write budget to list file
    call this%budget%budget_ot(kstp, kper, iout)
    !
    ! -- return
    return
  end subroutine imd_ot

  subroutine allocate_scalars(this)
! ******************************************************************************
! allocate_scalars
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryManagerModule, only: mem_allocate, mem_setptr
    ! -- dummy
    class(GwtImdType) :: this
    ! -- local
! ------------------------------------------------------------------------------
    !
    ! -- call standard BndType allocate scalars
    call this%BndType%allocate_scalars()
    !
    ! -- Allocate
    call mem_allocate(this%isrb, 'ISRB', this%origin)
    call mem_allocate(this%irorder, 'IRORDER', this%origin)
    !call mem_allocate(this%bditems, 'BDITEMS', this%origin)
    !call mem_allocate(this%nbdtxt, 'NBDTXT', this%origin)
    !
    ! -- Initialize
    this%isrb = 0
    this%irorder = 0
    !this%bditems = 5
    !this%nbdtxt = 7
    !
    ! -- Return
    return
  end subroutine allocate_scalars

  subroutine imd_allocate_arrays(this)
! ******************************************************************************
! imd_allocate_arrays -- allocate arraya
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryManagerModule, only: mem_allocate
    ! -- dummy
    class(GwtImdType),   intent(inout) :: this
    ! -- local
    integer(I4B) :: n
! ------------------------------------------------------------------------------
    !
    ! -- call standard BndType allocate scalars
    call this%BndType%allocate_arrays()
    !
    ! -- allocate imd arrays of size nodes
    call mem_allocate(this%strg, this%dis%nodes, 'STRG', this%origin)
    call mem_allocate(this%cim, this%dis%nodes, 'CIM', this%origin)
    call mem_allocate(this%zetaim, this%dis%nodes, 'ZETAIM', this%origin)
    call mem_allocate(this%thetaim, this%dis%nodes, 'THETAIM', this%origin)
    call mem_allocate(this%rhob, this%dis%nodes, 'RHOB', this%origin)
    call mem_allocate(this%distcoef,  this%dis%nodes, 'DISTCOEF', this%origin)
    call mem_allocate(this%rc1, this%dis%nodes, 'RC1', this%origin)
    call mem_allocate(this%rc2, this%dis%nodes, 'RC2', this%origin)
    !
    ! -- initialize
    do n = 1, this%dis%nodes
      this%strg(n) = DZERO
      this%cim(n) = DZERO
      this%zetaim(n) = DZERO
      this%thetaim(n) = DZERO
      this%rhob(n) = DZERO
      this%distcoef(n) = DZERO
      this%rc1(n) = DZERO
      this%rc2(n) = DZERO
    enddo
    !
    ! -- allocate and initialize character array for budget text
    !allocate(this%bdtxt(this%nbdtxt))
    !this%bdtxt(1) = '  STORAGE-LIQUID'
    !this%bdtxt(2) = '  STORAGE-SORBED'
    !this%bdtxt(3) = '1ST-DECAY-LIQUID'
    !this%bdtxt(4) = '1ST-DECAY-SORBED'
    !this%bdtxt(5) = '0TH-DECAY-LIQUID'
    !this%bdtxt(6) = '0TH-DECAY-SORBED'
    !this%bdtxt(7) = '   MOBILE-DOMAIN'
    !
    ! -- return
    return
  end subroutine imd_allocate_arrays

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
    class(GwtImdType), intent(inout) :: this
    ! -- local
    character(len=LINELENGTH) :: errmsg, keyword, keyword2
    integer(I4B) :: ierr
    logical :: isfound, endOfBlock
    ! -- formats
    character(len=*), parameter :: fmtisvflow =                                &
      "(4x,'CELL-BY-CELL FLOW INFORMATION WILL BE SAVED TO BINARY FILE " //    &
      "WHENEVER ICBCFL IS NOT ZERO.')"
    character(len=*), parameter :: fmtisrb =                                   &
      "(4x,'LINEAR SORBTION IS SELECTED. ')"
    character(len=*), parameter :: fmtirorder1 =                               &
      "(4x,'FIRST ORDER REACTION IS ACTIVE. ')"
    character(len=*), parameter :: fmtirorder2 =                               &
      "(4x,'ZERO ORDER REACTION IS ACTIVE. ')"
! ------------------------------------------------------------------------------
    !
    ! -- get options block
    call this%parser%GetBlock('OPTIONS', isfound, ierr, blockRequired=.false.)
    !
    ! -- parse options block if detected
    if (isfound) then
      write(this%iout,'(1x,a)')'PROCESSING IMMODBILE DOMAIN OPTIONS'
      do
        call this%parser%GetNextLine(endOfBlock)
        if (endOfBlock) exit
        call this%parser%GetStringCaps(keyword)
        select case (keyword)
          case ('SAVE_FLOWS')
            this%ipakcb = -1
            write(this%iout, fmtisvflow)
          case ('SORBTION')
            this%isrb = 1
            write(this%iout, fmtisrb)
          case ('DECAYORDER')
            this%isrb = 1
            call this%parser%GetStringCaps(keyword2)
            select case(keyword2)
              case('ONE')
                this%irorder = 1
                write(this%iout, fmtirorder1)
              case('ZERO')
                this%irorder = 2
                write(this%iout, fmtirorder2)
              case default
                write(errmsg,'(4x,a,a)')'****ERROR. UNKNOWN DECAYORDER OPTION: ',         &
                                         trim(keyword2)
                call store_error(errmsg)
                write(errmsg,'(4x,a)')'VALID OPTIONS FOR DECAYORDER ARE "ZERO" AND "ONE"'
                call store_error(errmsg)
                call this%parser%StoreErrorUnit()
                call ustop()
            end select
          case default
            write(errmsg,'(4x,a,a)')'****ERROR. UNKNOWN IMD OPTION: ',         &
                                     trim(keyword)
            call store_error(errmsg)
            call this%parser%StoreErrorUnit()
            call ustop()
        end select
      end do
      write(this%iout,'(1x,a)')'END OF IMMOBILE DOMAIN OPTIONS'
    end if
    !
    ! -- Return
    return
  end subroutine read_options

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
    use MemoryManagerModule, only: mem_reallocate
    ! -- dummy
    class(GwtImdType) :: this
    ! -- local
    character(len=LINELENGTH) :: line, errmsg, keyword
    integer(I4B) :: istart, istop, lloc, ierr
    logical :: isfound, endOfBlock
    logical, dimension(7) :: lname
    character(len=24), dimension(7) :: aname
    ! -- formats
    ! -- data
    data aname(1) /'            BULK DENSITY'/
    data aname(2) /'DISTRIBUTION COEFFICIENT'/
    data aname(3) /'  FIRST RATE COEFFICIENT'/
    data aname(4) /' SECOND RATE COEFFICIENT'/
    data aname(5) /'   INITIAL IMMOBILE CONC'/
    data aname(6) /'  DUAL DOMAIN TRANS RATE'/
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
          case ('RHOB')
            !call mem_reallocate(this%rhob, this%dis%nodes, 'RHOB',             &
            !                  trim(this%origin))
            call this%dis%read_grid_array(line, lloc, istart, istop, this%iout,&
                                         this%parser%iuactive, this%rhob,      &
                                         aname(1))
            lname(1) = .true.
          case ('DISTCOEF')
            !call mem_reallocate(this%distcoef, this%dis%nodes, 'DISTCOEF',     &
            !                  trim(this%origin))
            call this%dis%read_grid_array(line, lloc, istart, istop, this%iout,&
                                         this%parser%iuactive, this%distcoef,  &
                                         aname(2))
            lname(2) = .true.
          case ('RC1')
            !call mem_reallocate(this%rc1, this%dis%nodes, 'RC1',               &
            !                  trim(this%origin))
            call this%dis%read_grid_array(line, lloc, istart, istop, this%iout,&
                                         this%parser%iuactive, this%rc1,       &
                                         aname(3))
            lname(3) = .true.
          case ('RC2')
            !call mem_reallocate(this%rc2, this%dis%nodes, 'RC2',               &
            !                  trim(this%origin))
            call this%dis%read_grid_array(line, lloc, istart, istop, this%iout,&
                                         this%parser%iuactive, this%rc2,       &
                                         aname(4))
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
        write(errmsg, '(1x,a)') 'ERROR.  SORPTION IS ACTIVE BUT RHOB NOT &
          &SPECIFIED.  RHOB MUST BE SPECIFIED IN GRIDDATA BLOCK.'
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
        write(this%iout, '(1x,a)') 'WARNING.  SORPTION IS NOT ACTIVE BUT RHOB &
          &WAS SPECIFIED.  RHOB WILL HAVE NO AFFECT ON SIMULATION RESULTS.'
      endif
      if (lname(2)) then
        write(this%iout, '(1x,a)') 'WARNING.  SORPTION IS NOT ACTIVE BUT &
          &DISTRIBUTION COEFFICIENT WAS SPECIFIED.  DISTCOEF WILL HAVE &
          &NO AFFECT ON SIMULATION RESULTS.'
      endif
    endif
    !
    ! -- Check for required decay/production rate coefficients
    if (this%irorder > 0) then
      if (.not. lname(3)) then
        write(errmsg, '(1x,a)') 'ERROR.  FIRST OR ZERO ORDER REACTIONS ARE &
          &ACTIVE BUT THE FIRST RATE COEFFICIENT IS NOT SPECIFIED.  RC1 MUST &
          &BE SPECIFIED IN GRIDDATA BLOCK.'
        call store_error(errmsg)
      endif
      if (.not. lname(4)) then
        write(errmsg, '(1x,a)') 'ERROR.  FIRST OR ZERO ORDER REACTIONS ARE &
          &ACTIVE BUT THE SECOND RATE COEFFICIENT IS NOT SPECIFIED.  RC2 MUST &
          &BE SPECIFIED IN GRIDDATA BLOCK.'
        call store_error(errmsg)
      endif
    else
      if (lname(3)) then
        write(this%iout, '(1x,a)') 'WARNING.  FIRST OR ZERO ORER REACTIONS &
          &ARE NOT ACTIVE BUT RC1 WAS SPECIFIED.  RC1 WILL HAVE NO AFFECT &
          &ON SIMULATION RESULTS.'
      endif
      if (lname(4)) then
        write(this%iout, '(1x,a)') 'WARNING.  FIRST OR ZERO ORER REACTIONS &
          &ARE NOT ACTIVE BUT RC2 WAS SPECIFIED.  RC2 WILL HAVE NO AFFECT &
          &ON SIMULATION RESULTS.'
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

  subroutine calcddhcofrhs(thetaim, vcell, delt, swtpdt, swt, thetamfrac,      &
                           thetaimfrac, rhob, kd, lambda1im, lambda2im,        &
                           gamma1im, gamma2im, zetaim, cimt, hcof, rhs)
! ******************************************************************************
! calcddhcofrhs -- calculate the hcof and rhs contributions for dual domain
!   mass transfer
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    real(DP), intent(in) :: thetaim
    real(DP), intent(in) :: vcell
    real(DP), intent(in) :: delt
    real(DP), intent(in) :: swtpdt
    real(DP), intent(in) :: swt
    real(DP), intent(in) :: thetamfrac
    real(DP), intent(in) :: thetaimfrac
    real(DP), intent(in) :: rhob
    real(DP), intent(in) :: kd
    real(DP), intent(in) :: lambda1im
    real(DP), intent(in) :: lambda2im
    real(DP), intent(in) :: gamma1im
    real(DP), intent(in) :: gamma2im
    real(DP), intent(in) :: zetaim
    real(DP), intent(in) :: cimt
    real(DP), intent(inout) :: hcof
    real(DP), intent(inout) :: rhs    
    ! -- local
    real(DP), dimension(9) :: ddterm
    real(DP) :: f
! ------------------------------------------------------------------------------
    !
    ! -- calculate the ddterms
    call calcddterms(thetaim, vcell, delt, swtpdt, swt, thetamfrac,            &
                     thetaimfrac, rhob, kd, lambda1im, lambda2im,              &
                     gamma1im, gamma2im, zetaim, cimt, ddterm, f)
    !
    ! -- calculate hcof
    hcof = ddterm(9) ** 2 / f - ddterm(9)
    !
    ! -- calculate rhs, and switch the sign because this term needs to
    !    be moved to the left hand side
    rhs = ddterm(9) * (ddterm(2) + ddterm(4)) / f * cimt - ddterm(9)           &
          * ddterm(7) / f - ddterm(9) * ddterm(8) / f
    rhs = -rhs
    !
    ! -- Return
    return
  end subroutine calcddhcofrhs

  subroutine calcddbud(this, budterm, cnew)
! ******************************************************************************
! calcddbud -- calculate the individual budget terms for the immobile domain
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use TdisModule, only: delt
    ! -- dummy
    class(GwtImdType) :: this
    real(DP), dimension(:, :), intent(inout) :: budterm
    real(DP), dimension(:), intent(in) :: cnew
    ! -- local
    integer(I4B) :: n, i
    real(DP) :: vcell
    real(DP) :: swt
    real(DP) :: swtpdt
    real(DP) :: thetamfrac
    real(DP) :: thetaimfrac
    real(DP) :: kd
    real(DP) :: lambda1im
    real(DP) :: lambda2im
    real(DP) :: gamma1im
    real(DP) :: gamma2im
    real(DP) :: ddterm(9)
    real(DP) :: f
    real(DP) :: cimt
    real(DP) :: cimtpdt
    real(DP) :: rate
! ------------------------------------------------------------------------------
    !
    ! -- Calculate cim
    do n = 1, this%dis%nodes
      if (this%ibound(n) <= 0) cycle
      vcell = this%dis%area(n) * (this%dis%top(n) - this%dis%bot(n))
      swt = this%fmi%gwfsatold(n, delt)
      swtpdt = this%fmi%gwfsat(n)
      thetamfrac = this%sto%get_thetamfrac(n)
      thetaimfrac = this%sto%get_thetaimfrac(n, this%thetaim(n))
      kd = DZERO
      lambda1im = DZERO
      lambda2im = DZERO
      gamma1im = DZERO
      gamma2im = DZERO
      if (this%isrb > 0) kd = this%distcoef(n)
      if (this%irorder == 1) lambda1im = this%rc1(n)
      if (this%irorder == 2) gamma1im = this%rc1(n)
      if (this%irorder == 1) lambda2im = this%rc2(n)
      if (this%irorder == 2) gamma2im = this%rc2(n)
      !
      ! -- calculate the ddterms
      cimt = this%cim(n)
      call calcddterms(this%thetaim(n), vcell, delt, swtpdt, swt, thetamfrac,  &
                       thetaimfrac, this%rhob(n), kd, lambda1im, lambda2im,    &
                       gamma1im, gamma2im, this%zetaim(n), cimt, ddterm, f)
      cimtpdt = calcddconc(this%thetaim(n), vcell, delt, swtpdt, swt,          &
                           thetamfrac, thetaimfrac, this%rhob(n), kd,          &
                           lambda1im, lambda2im, gamma1im, gamma2im,           &
                           this%zetaim(n), this%cim(n), cnew(n))
      !
      ! -- calculate STORAGE-AQUEOUS
      i = 1
      rate = - ddterm(1) * cimtpdt + ddterm(2) * cimt
      if (rate > DZERO) then
        budterm(1, i) = budterm(1, i) + rate
      else
        budterm(2, i) = budterm(2, i) - rate
      endif
      !
      ! -- calculate STORAGE-SORBED
      i = 2
      rate = - ddterm(3) * cimtpdt + ddterm(4) * cimt
      if (rate > DZERO) then
        budterm(1, i) = budterm(1, i) + rate
      else
        budterm(2, i) = budterm(2, i) - rate
      endif
      !
      ! -- calculate DECAY-AQUEOUS
      i = 3
      rate = DZERO
      if (this%irorder == 1) then
        rate = - ddterm(5) * cimtpdt
      else if (this%irorder == 2) then
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
      if (this%irorder == 1) then
        rate = - ddterm(6) * cimtpdt
      else if (this%irorder == 2) then
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
      rate = ddterm(9) * cnew(n) - ddterm(9) * cimtpdt
      if (rate > DZERO) then
        budterm(1, i) = budterm(1, i) + rate
      else
        budterm(2, i) = budterm(2, i) - rate
      endif
      !
    enddo
    !
    ! -- Return
    return
  end subroutine calcddbud

  subroutine calccim(this, cim, cnew)
! ******************************************************************************
! calccimt -- if dual domain mass transfer, then calculate immobile domain
!   concentration using cnew (concentration solution for last time step)
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use TdisModule, only: delt
    ! -- dummy
    class(GwtImdType) :: this
    real(DP), dimension(:), intent(inout) :: cim
    real(DP), dimension(:), intent(in) :: cnew
    ! -- local
    integer(I4B) :: n
    real(DP) :: vcell
    real(DP) :: swt
    real(DP) :: swtpdt
    real(DP) :: thetamfrac
    real(DP) :: thetaimfrac
    real(DP) :: kd
    real(DP) :: lambda1im
    real(DP) :: lambda2im
    real(DP) :: gamma1im
    real(DP) :: gamma2im
    real(DP) :: ctmp
! ------------------------------------------------------------------------------
    !
    ! -- Calculate cim
    do n = 1, this%dis%nodes
      if(this%ibound(n) <= 0) cycle
      vcell = this%dis%area(n) * (this%dis%top(n) - this%dis%bot(n))
      swt = this%fmi%gwfsatold(n, delt)
      swtpdt = this%fmi%gwfsat(n)
      thetamfrac = this%sto%get_thetamfrac(n)
      thetaimfrac = this%sto%get_thetaimfrac(n, this%thetaim(n))
      kd = DZERO
      lambda1im = DZERO
      lambda2im = DZERO
      gamma1im = DZERO
      gamma2im = DZERO
      if (this%isrb > 0) kd = this%distcoef(n)
      if (this%irorder == 1) lambda1im = this%rc1(n)
      if (this%irorder == 2) gamma1im = this%rc1(n)
      if (this%irorder == 1) lambda2im = this%rc2(n)
      if (this%irorder == 2) gamma2im = this%rc2(n)
      ctmp = this%cim(n)
      ctmp = calcddconc(this%thetaim(n), vcell, delt, swtpdt, swt,           &
                        thetamfrac, thetaimfrac, this%rhob(n), kd,           &
                        lambda1im, lambda2im, gamma1im, gamma2im,            &
                        this%zetaim(n), ctmp, cnew(n))
      cim(n) = ctmp
    enddo
    !
    ! -- Return
    return
  end subroutine calccim

  function calcddconc(thetaim, vcell, delt, swtpdt, swt, thetamfrac,           &
                      thetaimfrac, rhob, kd, lambda1im, lambda2im, gamma1im,   &
                      gamma2im, zetaim, cimt, ctpdt) result (ddconc)
! ******************************************************************************
! calcddconc -- Calculate and return the concentration of the immobile domain
!   for a single cell.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    real(DP), intent(in) :: thetaim
    real(DP), intent(in) :: vcell
    real(DP), intent(in) :: delt
    real(DP), intent(in) :: swtpdt
    real(DP), intent(in) :: swt
    real(DP), intent(in) :: thetamfrac
    real(DP), intent(in) :: thetaimfrac
    real(DP), intent(in) :: rhob
    real(DP), intent(in) :: kd
    real(DP), intent(in) :: lambda1im
    real(DP), intent(in) :: lambda2im
    real(DP), intent(in) :: gamma1im
    real(DP), intent(in) :: gamma2im
    real(DP), intent(in) :: zetaim
    real(DP), intent(in) :: cimt
    real(DP), intent(in) :: ctpdt
    ! -- result
    real(DP) :: ddconc
    ! -- local
    real(DP), dimension(9) :: ddterm
    real(DP) :: f
! ------------------------------------------------------------------------------
    !
    ! -- initialize
    ddconc = DZERO
    !
    ! -- calculate the ddterms
    call calcddterms(thetaim, vcell, delt, swtpdt, swt, thetamfrac,            &
                     thetaimfrac, rhob, kd, lambda1im, lambda2im,              &
                     gamma1im, gamma2im, zetaim, cimt, ddterm, f)
    !
    ! -- calculate ddconc
    ddconc = (ddterm(2) + ddterm(4)) * cimt + ddterm(9) * ctpdt - ddterm(7)    &
             - ddterm(8)
    ddconc = ddconc / f
    !
    ! -- Return
    return
  end function calcddconc
                      
  subroutine calcddterms(thetaim, vcell, delt, swtpdt, swt, thetamfrac,        &
                         thetaimfrac, rhob, kd, lambda1im, lambda2im,          &
                         gamma1im, gamma2im, zetaim, cimt, ddterm, f)
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
    real(DP), intent(in) :: swt
    real(DP), intent(in) :: thetamfrac
    real(DP), intent(in) :: thetaimfrac
    real(DP), intent(in) :: rhob
    real(DP), intent(in) :: kd
    real(DP), intent(in) :: lambda1im
    real(DP), intent(in) :: lambda2im
    real(DP), intent(in) :: gamma1im
    real(DP), intent(in) :: gamma2im
    real(DP), intent(in) :: zetaim
    real(DP), intent(in) :: cimt
    real(DP), dimension(:), intent(inout) :: ddterm
    real(DP), intent(inout) :: f
    ! -- local
    real(DP) :: tled
! ------------------------------------------------------------------------------
    !
    ! -- initialize
    tled = DONE / delt
    !
    ! -- calculate terms
    ddterm(1) = thetaim * vcell * tled * swtpdt
    ddterm(2) = thetaim * vcell * tled * swt
    ddterm(3) = thetaimfrac * rhob * vcell * kd * swtpdt * tled
    ddterm(4) = thetaimfrac * rhob * vcell * kd * swt * tled
    ddterm(5) = thetaim * lambda1im * vcell * swtpdt
    ddterm(6) = thetaimfrac * lambda2im * rhob * kd * vcell
    ddterm(7) = thetaim * gamma1im * vcell * swtpdt
    ddterm(8) = thetaimfrac * gamma2im * rhob * vcell
    ddterm(9) = vcell * swtpdt * zetaim
    !
    ! -- calculate denometer term, f
    f = ddterm(1) + ddterm(3) + ddterm(5) + ddterm(6) + ddterm(9)
    !
    ! -- Return
    return
  end subroutine calcddterms

end module GwtImdModule