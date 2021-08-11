! -- todo: need observations for SSM terms
  
module GwtSsmModule
  
  use KindModule,             only: DP, I4B, LGP
  use ConstantsModule,        only: DONE, DZERO, LENAUXNAME, LENFTYPE,         &
                                    LENPACKAGENAME, LINELENGTH,                &
                                    TABLEFT, TABCENTER
  use SimModule,              only: store_error, count_errors
  use SimVariablesModule,     only: errmsg
  use NumericalPackageModule, only: NumericalPackageType
  use BaseDisModule,          only: DisBaseType
  use GwtFmiModule,           only: GwtFmiType
  use TableModule,            only: TableType, table_cr
  use GwtSsmInputModule,      only: GwtSsmInputType
  
  implicit none
  public :: GwtSsmType
  public :: ssm_cr

  character(len=LENFTYPE)       :: ftype = 'SSM'
  character(len=LENPACKAGENAME) :: text  = ' SOURCE-SINK MIX'

  type, extends(NumericalPackageType) :: GwtSsmType
    
    integer(I4B), pointer                              :: nbound                !< number of flow boundaries in this time step
    integer(I4B), dimension(:), pointer, contiguous    :: isrctype => null()    !< source type 0 is unspecified, 1 is aux, 2 is auxmixed, 3 is ssmi, 4 is ssmimixed
    integer(I4B), dimension(:), pointer, contiguous    :: iauxpak => null()     !< aux col for concentration
    integer(I4B), dimension(:), pointer, contiguous    :: ibound => null()      !< pointer to model ibound
    real(DP), dimension(:), pointer, contiguous        :: cnew => null()        !< pointer to gwt%x
    type(GwtFmiType), pointer                          :: fmi => null()         !< pointer to fmi object
    type(TableType), pointer                           :: outputtab => null()   !< output table object
    type(GwtSsmInputType), dimension(:), pointer       :: ssmivec => null()     !< array of ssm input objects
    
  contains
  
    procedure :: ssm_df
    procedure :: ssm_ar
    procedure :: ssm_rp
    procedure :: ssm_ad
    procedure :: ssm_fc
    procedure :: ssm_cq
    procedure :: ssm_bd
    procedure :: ssm_ot_flow
    procedure :: ssm_da
    procedure :: allocate_scalars
    procedure, private :: ssm_term
    procedure, private :: allocate_arrays
    procedure, private :: read_options
    procedure, private :: read_data
    procedure, private :: pak_setup_outputtab
    procedure, private :: set_iauxpak
    procedure, private :: set_ssmivec
    procedure, private :: get_ssm_conc
  
  end type GwtSsmType
  
  contains
  
  subroutine ssm_cr(ssmobj, name_model, inunit, iout, fmi)
! ******************************************************************************
! ssm_cr -- Create a new SSM object
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    type(GwtSsmType), pointer :: ssmobj
    character(len=*), intent(in) :: name_model
    integer(I4B), intent(in) :: inunit
    integer(I4B), intent(in) :: iout
    type(GwtFmiType), intent(in), target :: fmi
! ------------------------------------------------------------------------------
    !
    ! -- Create the object
    allocate(ssmobj)
    !
    ! -- create name and memory path
    call ssmobj%set_names(1, name_model, 'SSM', 'SSM')
    !
    ! -- Allocate scalars
    call ssmobj%allocate_scalars()
    !
    ! -- Set variables
    ssmobj%inunit = inunit
    ssmobj%iout = iout
    ssmobj%fmi => fmi
    !
    ! -- Initialize block parser
    call ssmobj%parser%Initialize(ssmobj%inunit, ssmobj%iout)
    !
    ! -- Return
    return
  end subroutine ssm_cr

  subroutine ssm_df(this)
! ******************************************************************************
! ssm_df -- Allocate and Read
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryManagerModule, only: mem_setptr
    ! -- dummy
    class(GwtSsmType) :: this
    ! -- local
    ! -- formats
! ------------------------------------------------------------------------------
    !
    ! -- Return
    return
  end subroutine ssm_df

  subroutine ssm_ar(this, dis, ibound, cnew)
! ******************************************************************************
! ssm_ar -- Allocate and Read
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryManagerModule, only: mem_setptr
    ! -- dummy
    class(GwtSsmType) :: this
    class(DisBaseType), pointer, intent(in) :: dis
    integer(I4B), dimension(:), pointer, contiguous :: ibound
    real(DP), dimension(:), pointer, contiguous :: cnew
    ! -- local
    ! -- formats
    character(len=*), parameter :: fmtssm =                                    &
      "(1x,/1x,'SSM -- SOURCE-SINK MIXING PACKAGE, VERSION 1, 8/25/2017',      &
      &' INPUT READ FROM UNIT ', i0, //)"
! ------------------------------------------------------------------------------
    !
    ! --print a message identifying the storage package.
    write(this%iout, fmtssm) this%inunit
    !
    ! -- store pointers to arguments that were passed in
    this%dis     => dis
    this%ibound  => ibound
    this%cnew    => cnew
    !
    ! -- Check to make sure that there are flow packages
    if (this%fmi%nflowpack == 0) then
      write(errmsg, '(a)') 'SSM PACKAGE DOES NOT HAVE &
                            &BOUNDARY FLOWS.  ACTIVATE GWF-GWT EXCHANGE &
                            &OR TURN ON FMI AND PROVIDE A BUDGET FILE &
                            &THAT CONTAINS BOUNDARY FLOWS.'
      call store_error(errmsg)
      call this%parser%StoreErrorUnit()
    endif
    !
    ! -- Allocate arrays
    call this%allocate_arrays()
    !
    ! -- Read ssm options
    call this%read_options()
    !
    ! -- read the data block
    call this%read_data()
    !
    ! -- setup the output table
    call this%pak_setup_outputtab()
    !
    ! -- Return
    return
  end subroutine ssm_ar

  subroutine ssm_rp(this)
! ******************************************************************************
! ssm_rp -- Read and prepare
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(GwtSsmType) :: this
    ! -- local
    integer(I4B) :: ip
    type(GwtSsmInputType), pointer :: ssmiptr
    ! -- formats
! ------------------------------------------------------------------------------
    !
    ! -- Call the rp method on any ssm input files
    do ip = 1, this%fmi%nflowpack
      if (this%fmi%iatp(ip) /= 0) cycle
      if (this%isrctype(ip) == 3 .or. this%isrctype(ip) == 4) then
        ssmiptr => this%ssmivec(ip)
        call ssmiptr%ssmi_rp()
      end if
    end do
    !
    ! -- Return
    return
  end subroutine ssm_rp

  subroutine ssm_ad(this)
! ******************************************************************************
! ssm_ad -- Calculate number of flow boundaries
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(GwtSsmType) :: this
    ! -- local
    integer(I4B) :: ip
! ------------------------------------------------------------------------------
    !
    ! -- Calculate total number of flow boundaries
    this%nbound = 0
    do ip = 1, this%fmi%nflowpack
      if (this%fmi%iatp(ip) /= 0) cycle 
      this%nbound = this%nbound + this%fmi%gwfpackages(ip)%nbound
    end do
    !
    ! -- Return
    return
  end subroutine ssm_ad
  
  subroutine ssm_term(this, ipackage, ientry, rrate, rhsval, hcofval,          &
                      cssm, qssm)
! ******************************************************************************
! ssm_term
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(GwtSsmType) :: this                     !< GwtSsmType
    integer(I4B), intent(in) :: ipackage          !< package number
    integer(I4B), intent(in) :: ientry            !< bound number
    real(DP), intent(out), optional :: rrate      !< calculated mass flow rate
    real(DP), intent(out), optional :: rhsval     !< calculated rhs value
    real(DP), intent(out), optional :: hcofval    !< calculated hcof value
    real(DP), intent(out), optional :: cssm       !< calculated source concentration depending on flow direction
    real(DP), intent(out), optional :: qssm       !< water flow rate into model cell from boundary package
    ! -- local
    logical(LGP) :: lauxmixed
    integer(I4B) :: n
    real(DP) :: qbnd
    real(DP) :: ctmp
    real(DP) :: omega
    real(DP) :: hcoftmp
    real(DP) :: rhstmp
! ------------------------------------------------------------------------------
    !
    ! -- retrieve node number, qbnd and iauxpos
    hcoftmp = DZERO
    rhstmp = DZERO
    ctmp = DZERO
    qbnd = DZERO
    n = this%fmi%gwfpackages(ipackage)%nodelist(ientry)
    !
    ! -- If cell is active (ibound > 0) then calculate values
    if (this%ibound(n) > 0) then
      !
      ! -- retrieve qbnd and iauxpos
      qbnd = this%fmi%gwfpackages(ipackage)%get_flow(ientry)
      call this%get_ssm_conc(ipackage, ientry, ctmp, lauxmixed)
      !
      ! -- assign values for hcoftmp, rhstmp, and ctmp for subsequent assigment
      !    of hcof, rhs, and rate    
      if(.not. lauxmixed) then
        !
        ! -- If qbnd is positive, then concentration represents the inflow 
        !    concentration.  If qbnd is negative, then the outflow concentration
        !    is set equal to the simulated cell concentration
        if (qbnd >= DZERO) then
          omega = DZERO  ! rhs
        else
          ctmp = this%cnew(n)
          omega = DONE  ! lhs
        end if
      else
        !
        ! -- lauxmixed value indicates that this is a mixed sink type where 
        !    the concentration value represents the injected concentration if 
        !    qbnd is positive. If qbnd is negative, then the withdrawn water 
        !    is equal to the minimum of the aux concentration and the cell 
        !    concentration.
        if (qbnd >= DZERO) then
          omega = DZERO  ! rhs (ctmp is aux value)
        else
          if (ctmp < this%cnew(n)) then
            omega = DZERO  ! rhs (ctmp is aux value)
          else
            omega = DONE ! lhs (ctmp is cell concentration)
            ctmp = this%cnew(n)
          end if
        end if
      endif
      !
      ! -- Add terms based on qbnd sign
      if(qbnd <= DZERO) then
        hcoftmp = qbnd * omega
      else
        rhstmp = -qbnd * ctmp * (DONE - omega)
      endif
      !
      ! -- end of active ibound
    end if
    !
    ! -- set requested values
    if (present(hcofval)) hcofval = hcoftmp
    if (present(rhsval)) rhsval = rhstmp
    if (present(rrate)) rrate = hcoftmp * ctmp - rhstmp
    if (present(cssm)) cssm = ctmp
    if (present(qssm)) qssm = qbnd
    !
    ! -- return
    return
  end subroutine ssm_term
  
  subroutine get_ssm_conc(this, ipackage, ientry, conc, lauxmixed)
    ! -- dummy
    class(GwtSsmType) :: this
    integer(I4B), intent(in) :: ipackage
    integer(I4B), intent(in) :: ientry
    real(DP), intent(out) :: conc
    logical(LGP), intent(out) :: lauxmixed
    ! -- local
    integer(I4B) :: isrctype
    integer(I4B) :: iauxpos
    
    conc = DZERO
    lauxmixed = .false.
    isrctype = this%isrctype(ipackage)
    
    select case(isrctype)
    case(1, 2)
      iauxpos = this%iauxpak(ipackage)
      conc = this%fmi%gwfpackages(ipackage)%auxvar(iauxpos, ientry)
      if (isrctype == 2) lauxmixed = .true.
    case(3, 4)
      conc = this%ssmivec(ipackage)%get_value(ientry)
      if (isrctype == 4) lauxmixed = .true.
    end select
    
    return
  end subroutine get_ssm_conc
  
  subroutine ssm_fc(this, amatsln, idxglo, rhs)
! ******************************************************************************
! ssm_fc -- Calculate coefficients and fill amat and rhs
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(GwtSsmType) :: this
    real(DP), dimension(:), intent(inout) :: amatsln
    integer(I4B), intent(in), dimension(:) :: idxglo
    real(DP), intent(inout), dimension(:) :: rhs
    ! -- local
    integer(I4B) :: ip
    integer(I4B) :: i
    integer(I4B) :: n
    integer(I4B) :: idiag
    integer(I4B) :: nflowpack
    integer(I4B) :: nbound
    real(DP) :: hcofval
    real(DP) :: rhsval
! ------------------------------------------------------------------------------
    !
    ! -- do for each flow package
    nflowpack = this%fmi%nflowpack
    do ip = 1, nflowpack
      if (this%fmi%iatp(ip) /= 0) cycle
      !
      ! -- do for each entry in package (ip)
      nbound = this%fmi%gwfpackages(ip)%nbound
      do i = 1, nbound
        n = this%fmi%gwfpackages(ip)%nodelist(i)
        call this%ssm_term(ip, i, rhsval=rhsval, hcofval=hcofval)
        idiag = idxglo(this%dis%con%ia(n))
        amatsln(idiag) = amatsln(idiag) + hcofval
        rhs(n) = rhs(n) + rhsval
        !
      enddo
      !
    enddo
    !
    ! -- Return
    return
  end subroutine ssm_fc
  
  subroutine ssm_cq(this, flowja)
! ******************************************************************************
! ssm_cq -- Calculate the flows
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(GwtSsmType) :: this
    real(DP), dimension(:), contiguous, intent(inout) :: flowja
    ! -- local
    integer(I4B) :: ip
    integer(I4B) :: i
    integer(I4B) :: n
    integer(I4B) :: idiag
    real(DP) :: rate
! ------------------------------------------------------------------------------
    !
    ! -- do for each flow package
    do ip = 1, this%fmi%nflowpack
      !
      ! -- cycle if package is being managed as an advanced package
      if (this%fmi%iatp(ip) /= 0) cycle
      !
      ! -- do for each boundary
      do i = 1, this%fmi%gwfpackages(ip)%nbound
        n = this%fmi%gwfpackages(ip)%nodelist(i)
        call this%ssm_term(ip, i, rrate=rate)
        idiag = this%dis%con%ia(n)
        flowja(idiag) = flowja(idiag) + rate
        !
      enddo
      !
    enddo
    !
    ! -- Return
    return
  end subroutine ssm_cq

  subroutine ssm_bd(this, isuppress_output, model_budget)
! ******************************************************************************
! ssm_bd -- Calculate budget terms
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use TdisModule, only: delt
    use BudgetModule, only: BudgetType
    ! -- dummy
    class(GwtSsmType) :: this
    integer(I4B), intent(in) :: isuppress_output
    type(BudgetType), intent(inout) :: model_budget
    ! -- local
    character(len=LENPACKAGENAME) :: rowlabel  = 'SSM'
    integer(I4B) :: ip
    integer(I4B) :: i
    real(DP) :: rate
    real(DP) :: rin
    real(DP) :: rout
! ------------------------------------------------------------------------------
    !
    ! -- do for each flow package, unless it is being handled by an advanced
    !    transport package
    do ip = 1, this%fmi%nflowpack
      !
      ! -- cycle if package is being managed as an advanced package
      if (this%fmi%iatp(ip) /= 0) cycle
      !
      ! -- Initialize the rate accumulators
      rin = DZERO
      rout = DZERO
      !
      ! -- do for each boundary
      do i = 1, this%fmi%gwfpackages(ip)%nbound
        call this%ssm_term(ip, i, rrate=rate)
        if(rate < DZERO) then
          rout = rout - rate
        else
          rin = rin + rate
        endif
        !
      enddo
      !
      call model_budget%addentry(rin, rout, delt,                              &
                                 this%fmi%flowpacknamearray(ip),               &
                                 isuppress_output, rowlabel=rowlabel)
    enddo
    !
    ! -- Return
    return
  end subroutine ssm_bd

  subroutine ssm_ot_flow(this, icbcfl, ibudfl, icbcun)
! ******************************************************************************
! ssm_ot_flow -- Print and save the ssm flows
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use TdisModule, only: kstp, kper
    use ConstantsModule, only: LENPACKAGENAME, LENBOUNDNAME, LENAUXNAME, DZERO
    use BudgetModule, only: BudgetType
    ! -- dummy
    class(GwtSsmType) :: this
    integer(I4B), intent(in) :: icbcfl
    integer(I4B), intent(in) :: ibudfl
    integer(I4B), intent(in) :: icbcun
    ! -- local
    character (len=LINELENGTH) :: title
    integer(I4B) :: node, nodeu
    character(len=20) :: nodestr
    integer(I4B) :: maxrows
    integer(I4B) :: ip
    integer(I4B) :: i, n2, ibinun
    real(DP) :: rrate
    real(DP) :: qssm
    real(DP) :: cssm
    integer(I4B) :: naux
    real(DP), dimension(0,0) :: auxvar
    character(len=LENAUXNAME), dimension(0) :: auxname
    ! -- for observations
    character(len=LENBOUNDNAME) :: bname
    ! -- formats
    character(len=*), parameter :: fmttkk = &
      "(1X,/1X,A,'   PERIOD ',I0,'   STEP ',I0)"
! ------------------------------------------------------------------------------
    !
    ! -- set maxrows
    maxrows = 0
    if (ibudfl /= 0 .and. this%iprflow /= 0) then
      call this%outputtab%set_kstpkper(kstp, kper)
      do ip = 1, this%fmi%nflowpack
        if (this%fmi%iatp(ip) /= 0) cycle
        !
        ! -- do for each boundary
        do i = 1, this%fmi%gwfpackages(ip)%nbound
          node = this%fmi%gwfpackages(ip)%nodelist(i)
          if (node > 0) then
            maxrows = maxrows + 1
          end if
        end do
      end do
      if (maxrows > 0) then
        call this%outputtab%set_maxbound(maxrows)
      end if
      title = 'SSM PACKAGE (' // trim(this%packName) //     &
              ') FLOW RATES'
      call this%outputtab%set_title(title)
    end if
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
    !
    ! -- If cell-by-cell flows will be saved as a list, write header.
    if(ibinun /= 0) then
      naux = 0
      call this%dis%record_srcdst_list_header(text, this%name_model,           &
                  this%name_model, this%name_model, this%packName, naux,       &
                  auxname, ibinun, this%nbound, this%iout)
    endif
    !
    ! -- If no boundaries, skip flow calculations.
    if(this%nbound > 0) then
      !
      ! -- Loop through each boundary calculating flow.
      do ip = 1, this%fmi%nflowpack
        if (this%fmi%iatp(ip) /= 0) cycle
        !
        ! -- do for each boundary
        do i = 1, this%fmi%gwfpackages(ip)%nbound
          !
          ! -- Calculate rate for this entry
          node = this%fmi%gwfpackages(ip)%nodelist(i)
          call this%ssm_term(ip, i, rrate=rrate, qssm=qssm, cssm=cssm)
          !
          ! -- Print the individual rates if the budget is being printed
          !    and PRINT_FLOWS was specified (this%iprflow<0)
          if (ibudfl /= 0) then
            if (this%iprflow /= 0) then
              !
              ! -- set nodestr and write outputtab table
              nodeu = this%dis%get_nodeuser(node)
              call this%dis%nodeu_to_string(node, nodestr)
              bname = this%fmi%gwfpackages(ip)%name
              call this%outputtab%add_term(i)
              call this%outputtab%add_term(trim(adjustl(nodestr)))
              call this%outputtab%add_term(qssm)
              call this%outputtab%add_term(cssm)
              call this%outputtab%add_term(rrate)
              call this%outputtab%add_term(bname)
              !                                     qssm, cssm, rrate, bname)
            end if
          end if
          !
          ! -- If saving cell-by-cell flows in list, write flow
          if (ibinun /= 0) then
            n2 = i
            call this%dis%record_mf6_list_entry(ibinun, node, n2, rrate,       &
                                                naux, auxvar(:,i),             &
                                                olconv2=.FALSE.)
          end if
          !
        enddo
        !
      enddo
    endif
    if (ibudfl /= 0) then
      if (this%iprflow /= 0) then
          write(this%iout,'(1x)')
      end if
    end if
    !
    ! -- return
    return
  end subroutine ssm_ot_flow

  subroutine ssm_da(this)
! ******************************************************************************
! ssm_da -- Deallocate variables
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryManagerModule, only: mem_deallocate
    ! -- dummy
    class(GwtSsmType) :: this
! ------------------------------------------------------------------------------
    !
    ! -- Deallocate arrays if package was active
    if(this%inunit > 0) then
      call mem_deallocate(this%iauxpak)
      call mem_deallocate(this%isrctype)
      this%ibound => null()
      this%fmi => null()
    endif
    !
    ! -- output table object
    if (associated(this%outputtab)) then
      call this%outputtab%table_da()
      deallocate(this%outputtab)
      nullify(this%outputtab)
    end if
    !
    ! -- Scalars
    call mem_deallocate(this%nbound)
    !
    ! -- deallocate parent
    call this%NumericalPackageType%da()
    !
    ! -- Return
    return
  end subroutine ssm_da

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
    class(GwtSsmType) :: this
    ! -- local
! ------------------------------------------------------------------------------
    !
    ! -- allocate scalars in NumericalPackageType
    call this%NumericalPackageType%allocate_scalars()
    !
    ! -- Allocate
    call mem_allocate(this%nbound, 'NBOUND', this%memoryPath)
    !
    ! -- Initialize
    this%nbound = 0
    !
    ! -- Return
    return
  end subroutine allocate_scalars

  subroutine allocate_arrays(this)
! ******************************************************************************
! allocate_scalars
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryManagerModule, only: mem_allocate, mem_setptr
    ! -- dummy
    class(GwtSsmType) :: this
    ! -- local
    integer(I4B) :: nflowpack
    integer(I4B) :: i
! ------------------------------------------------------------------------------
    !    
    ! -- Allocate
    nflowpack = this%fmi%nflowpack
    call mem_allocate(this%iauxpak, nflowpack, 'IAUXPAK', this%memoryPath)
    call mem_allocate(this%isrctype, nflowpack, 'ISRCTYPE', this%memoryPath)
    !
    ! -- Initialize
    do i = 1, nflowpack
      this%iauxpak(i) = 0
      this%isrctype(i) = 0
    end do
    !
    ! -- Allocate the ssmivec array
    allocate(this%ssmivec(nflowpack))
    !
    ! -- Return
    return
  end subroutine allocate_arrays
  
  subroutine read_options(this)
! ******************************************************************************
! read_options -- read package options
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(GwtSSMType) :: this
    ! -- local
    character(len=LINELENGTH) :: keyword
    integer(I4B) :: ierr
    logical :: isfound, endOfBlock
    ! -- formats
    character(len=*), parameter :: fmtiprflow =                                &
      "(4x,'SSM FLOW INFORMATION WILL BE PRINTED TO LISTING FILE " // &
      "WHENEVER ICBCFL IS NOT ZERO.')"
    character(len=*), parameter :: fmtisvflow =                                &
      "(4x,'CELL-BY-CELL FLOW INFORMATION WILL BE SAVED TO BINARY FILE " //    &
      "WHENEVER ICBCFL IS NOT ZERO.')"
! ------------------------------------------------------------------------------
    !
    ! -- get options block
    call this%parser%GetBlock('OPTIONS', isfound, ierr, blockRequired=.false., &
                              supportOpenClose=.true.)
    !
    ! -- parse options block if detected
    if (isfound) then
      write(this%iout,'(1x,a)')'PROCESSING SSM OPTIONS'
      do
        call this%parser%GetNextLine(endOfBlock)
        if (endOfBlock) exit
        call this%parser%GetStringCaps(keyword)
        select case (keyword)
          case ('PRINT_FLOWS')
            this%iprflow = 1
            write(this%iout, fmtiprflow)
          case ('SAVE_FLOWS')
            this%ipakcb = -1
            write(this%iout, fmtisvflow)
          case default
            write(errmsg,'(4x,a,a)') 'UNKNOWN SSM OPTION: ', trim(keyword)
            call store_error(errmsg)
            call this%parser%StoreErrorUnit()
        end select
      end do
      write(this%iout,'(1x,a)')'END OF SSM OPTIONS'
    end if
    !
    ! -- Return
    return
  end subroutine read_options

  subroutine read_data(this)
! ******************************************************************************
! read_data -- read source data
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(GwtSsmtype) :: this
    ! -- local
    character(len=LINELENGTH) :: keyword
    character(len=20) :: srctype
    integer(I4B) :: ierr
    integer(I4B) :: ip
    integer(I4B) :: nflowpack
    integer(I4B) :: isrctype
    logical :: isfound, endOfBlock
    logical :: pakfound
    logical :: lauxmixed
    ! -- formats
    ! -- data
! ------------------------------------------------------------------------------
    !
    ! -- initialize
    isfound = .false.
    lauxmixed = .false.
    nflowpack = this%fmi%nflowpack
    !
    ! -- get sources block
    call this%parser%GetBlock('SOURCES', isfound, ierr, supportOpenClose=.true.)
    if(isfound) then
      write(this%iout,'(1x,a)')'PROCESSING SOURCES'
      do
        call this%parser%GetNextLine(endOfBlock)
        if (endOfBlock) exit
        !
        ! -- read package name and make sure it can be found
        call this%parser%GetStringCaps(keyword)
        pakfound = .false.
        do ip = 1, nflowpack
          if (trim(adjustl(this%fmi%gwfpackages(ip)%name)) == keyword) then
            pakfound = .true.
            exit
          endif
        enddo
        if (.not. pakfound) then
          write(errmsg,'(1x, a, a)') 'FLOW PACKAGE CANNOT BE FOUND: ',      &
                                      trim(keyword)
          call store_error(errmsg)
          call this%parser%StoreErrorUnit()
        endif
        !
        ! -- read the source type
        call this%parser%GetStringCaps(srctype)
        select case(srctype)
        case('AUX')
          write(this%iout,'(1x,a)') 'AUX SOURCE DETECTED.'
          isrctype = 1
        case('AUXMIXED')
          write(this%iout,'(1x,a)') 'AUXMIXED SOURCE DETECTED.'
          lauxmixed = .true.
          isrctype = 2
        case('SSMI')
          write(this%iout,'(1x,a)') 'SSMI SOURCE DETECTED.'
          isrctype = 3
        case('SSMIMIXED')
          write(this%iout,'(1x,a)') 'SSMIMIXED SOURCE DETECTED.'
          isrctype = 4
        case default
          write(errmsg,'(1x, a, a)')                                          &
            'SRCTYPE MUST BE AUX, AUXMIXED, SSMI, or SSMIMIXED.  FOUND: ',    &
            trim(srctype)
          call store_error(errmsg)
          call this%parser%StoreErrorUnit()
        end select
        !
        ! -- Ensure package was not specified more than once in SOURCES block
        if (this%isrctype(ip) == 0) then
          this%isrctype(ip) = isrctype
        else
          write(errmsg,'(1x, a, a)')                                          &
            'A PACKAGE CANNOT BE SPECIFIED MORE THAN ONCE IN THE SSM SOURCES &
            &BLOCK.  THE FOLLOWING PACKAGE WAS SPECIFIED MORE THAN ONCE: ', &
            trim(keyword)
          call store_error(errmsg)
          call this%parser%StoreErrorUnit()
        end if
        
        select case(isrctype)
        case(1, 2)
          call this%set_iauxpak(ip, trim(keyword))
        case(3, 4)
          call this%set_ssmivec(ip, trim(keyword))
        end select

      end do
      write(this%iout,'(1x,a)')'END PROCESSING SOURCES'
    else
      write(errmsg,'(1x,a)')'ERROR.  REQUIRED SOURCES BLOCK NOT FOUND.'
      call store_error(errmsg)
      call this%parser%StoreErrorUnit()
    end if
    !
    ! -- terminate if errors
    if(count_errors() > 0) then
      call this%parser%StoreErrorUnit()
    endif
    !
    ! -- Return
    return
  end subroutine read_data
  
  !> @ brief Set iauxpak array value for package ip
  !!
  !!  The next call to parser will return the auxiliary name for
  !!  package ip in the SSM SOURCES block.  The routine searches
  !!  through the auxiliary names in package ip and sets iauxpak
  !!  to the column number corresponding to the correct auxiliary
  !!  column.
  !!
  !<
  subroutine set_iauxpak(this, ip, packname)
    ! -- dummy
    class(GwtSsmtype),intent(inout) :: this  !< GwtSsmtype
    integer(I4B), intent(in) :: ip           !< package number
    character(len=*), intent(in) :: packname !< name of package
    ! -- local
    character(len=LENAUXNAME) :: auxname    
    logical :: auxfound
    integer(I4B) :: iaux
    !
    ! -- read name of auxiliary column
    call this%parser%GetStringCaps(auxname)
    auxfound = .false.
    do iaux = 1, this%fmi%gwfpackages(ip)%naux
      if (trim(this%fmi%gwfpackages(ip)%auxname(iaux)) ==                  &
          trim(auxname)) then
        auxfound = .true.
        exit
      endif
    enddo
    if (.not. auxfound) then
      write(errmsg,'(1x, a, a)')                                           &
        'AUXILIARY NAME CANNOT BE FOUND: ', trim(auxname)
      call store_error(errmsg)
      call this%parser%StoreErrorUnit()
    endif
    !
    ! -- set iauxpak and write message
    this%iauxpak(ip) = iaux
    write(this%iout, '(4x, a, i0, a, a)') 'USING AUX COLUMN ',      &
      iaux, ' IN PACKAGE ', trim(packname)
    !
    ! -- return
    return
  end subroutine set_iauxpak
  
  !> @ brief Set ssmivec array value for package ip
  !!
  !!  The next call to parser will return the input file name for
  !!  package ip in the SSM SOURCES block.  The routine then
  !!  initializes the ssmi input file.
  !!
  !<
  subroutine set_ssmivec(this, ip, packname)
    ! -- module
    use InputOutputModule,   only: openfile, getunit
    ! -- dummy
    class(GwtSsmtype),intent(inout) :: this  !< GwtSsmtype
    integer(I4B), intent(in) :: ip           !< package number
    character(len=*), intent(in) :: packname !< name of package
    ! -- local
    character(len=LINELENGTH) :: filename
    type(GwtSsmInputType), pointer :: ssmiptr
    integer(I4B) :: inunit
    !
    ! -- read file name
    call this%parser%GetString(filename)
    inunit = getunit()
    call openfile(inunit, this%iout, filename, 'SSMI', filstat_opt='OLD')
    
    ! -- Create the ssmi file object
    ssmiptr => this%ssmivec(ip)
    call ssmiptr%initialize(ip, inunit, this%iout, this%name_model)
    
    write(this%iout, '(4x, a, a, a, a)') 'USING SSMI INPUT FILE ',             &
      trim(filename), ' TO SET CONCENTRATIONS FOR PACKAGE ', trim(packname)
    !
    ! -- return
    return
  end subroutine set_ssmivec
  
  subroutine pak_setup_outputtab(this)
! ******************************************************************************
! bnd_options -- set options for a class derived from BndType
! This subroutine can be overridden by specific packages to set custom options
! that are not part of the package superclass.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(GwtSsmtype),intent(inout) :: this
    ! -- local
    character(len=LINELENGTH) :: title
    character(len=LINELENGTH) :: text
    integer(I4B) :: ntabcol
! ------------------------------------------------------------------------------
    !
    ! -- allocate and initialize the output table
    if (this%iprflow /= 0) then
      !
      ! -- dimension table
      ntabcol = 6
      !if (this%inamedbound > 0) then
      !  ntabcol = ntabcol + 1
      !end if
      !
      ! -- initialize the output table object
      title = 'SSM PACKAGE (' // trim(this%packName) //     &
              ') FLOW RATES'
      call table_cr(this%outputtab, this%packName, title)
      call this%outputtab%table_df(1, ntabcol, this%iout, transient=.TRUE.)
      text = 'NUMBER'
      call this%outputtab%initialize_column(text, 10, alignment=TABCENTER)
      text = 'CELLID'
      call this%outputtab%initialize_column(text, 20, alignment=TABLEFT)
      text = 'BOUND Q'
      call this%outputtab%initialize_column(text, 15, alignment=TABCENTER)
      text = 'SSM CONC'
      call this%outputtab%initialize_column(text, 15, alignment=TABCENTER)
      text = 'RATE'
      call this%outputtab%initialize_column(text, 15, alignment=TABCENTER)
      text = 'PACKAGE NAME'
      call this%outputtab%initialize_column(text, 16, alignment=TABCENTER)
      !if (this%inamedbound > 0) then
      !  text = 'NAME'
      !  call this%outputtab%initialize_column(text, 20, alignment=TABLEFT)
      !end if
    end if
    !
    ! -- return
    return
  end subroutine pak_setup_outputtab

end module GwtSsmModule