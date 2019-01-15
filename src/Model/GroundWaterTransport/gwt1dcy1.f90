module GwtDcyModule
  
  use KindModule,             only: DP, I4B
  use ConstantsModule,        only: DONE, DZERO
  use NumericalPackageModule, only: NumericalPackageType
  use BaseDisModule,          only: DisBaseType
  use GwtFmiModule,           only: GwtFmiType
  use GwtStoModule,           only: GwtStoType
  use BudgetModule,           only: BudgetType
  
  implicit none
  public :: GwtDcyType
  public :: dcy_cr

  type, extends(NumericalPackageType) :: GwtDcyType
    
    real(DP), dimension(:), pointer, contiguous      :: porosity => null()      ! pointer to storage package porosity
    integer(I4B), dimension(:), pointer, contiguous  :: ibound => null()        ! pointer to model ibound
    type(GwtFmiType), pointer                        :: fmi => null()           ! pointer to fmi object
    type(GwtStoType), pointer                        :: sto => null()           ! pointer to sto object
    
    integer(I4B), pointer                            :: irorder => null()       ! order of reaction rate (1:first, 0:zero)
    real(DP), dimension(:), pointer, contiguous      :: rc => null()            ! first or zero order rate constant
    real(DP), dimension(:), pointer, contiguous      :: rate => null()          ! mass decay rate for each cell
    
  contains
  
    procedure :: dcy_ar
    procedure :: dcy_ad
    procedure :: dcy_fc
    procedure :: dcy_bdcalc
    procedure :: dcy_bdsav
    procedure :: dcy_da
    procedure :: allocate_scalars
    procedure, private :: allocate_arrays
    procedure, private :: read_options
    procedure, private :: read_data

  end type GwtDcyType
  
  contains
  
  subroutine dcy_cr(dcyobj, name_model, inunit, iout, fmi)
! ******************************************************************************
! dcy_cr -- Create a new RCT object
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    type(GwtDcyType), pointer :: dcyobj
    character(len=*), intent(in) :: name_model
    integer(I4B), intent(in) :: inunit
    integer(I4B), intent(in) :: iout
    type(GwtFmiType), intent(in), target :: fmi
! ------------------------------------------------------------------------------
    !
    ! -- Create the object
    allocate(dcyobj)
    !
    ! -- create name and origin
    call dcyobj%set_names(1, name_model, 'DCY', 'DCY')
    !
    ! -- Allocate scalars
    call dcyobj%allocate_scalars()
    !
    ! -- Set variables
    dcyobj%inunit = inunit
    dcyobj%iout = iout
    dcyobj%fmi => fmi
    !
    ! -- Initialize block parser
    call dcyobj%parser%Initialize(dcyobj%inunit, dcyobj%iout)
    !
    ! -- Return
    return
  end subroutine dcy_cr

  subroutine dcy_ar(this, dis, sto, ibound)
! ******************************************************************************
! dcy_ar -- Allocate and Read
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(GwtDcyType) :: this
    class(DisBaseType), pointer, intent(in) :: dis
    type(GwtStoType), pointer, intent(in) :: sto
    integer(I4B), dimension(:), pointer, contiguous :: ibound
    ! -- local
    ! -- formats
    character(len=*), parameter :: fmtsrb =                                    &
      "(1x,/1x,'SRB -- DECAY PACKAGE, VERSION 1, 01/15/2019',                  &
      &' INPUT READ FROM UNIT ', i0, //)"
! ------------------------------------------------------------------------------
    !
    ! --print a message identifying the sorption package.
    write(this%iout, fmtsrb) this%inunit
    !
    ! -- store pointers to arguments that were passed in
    this%dis => dis
    this%sto => sto
    this%ibound => ibound
    this%porosity => sto%porosity
    !
    ! -- Allocate arrays
    call this%allocate_arrays()
    !
    ! -- Read sorption options
    call this%read_options()
    !
    ! -- read the data block
    call this%read_data()
    !
    ! -- Return
    return
  end subroutine dcy_ar
  
  subroutine dcy_ad(this)
! ******************************************************************************
! dcy_ad -- advance
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(GwtDcyType) :: this
    ! -- local
    ! -- formats
! ------------------------------------------------------------------------------
    !
    ! -- Nothing to do 
    !
    ! -- Return
    return
  end subroutine dcy_ad
  
  subroutine dcy_fc(this, nodes, cold, nja, njasln, amatsln, idxglo, rhs)
! ******************************************************************************
! dcy_fc -- Calculate coefficients and fill amat and rhs
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(GwtDcyType) :: this
    integer, intent(in) :: nodes
    real(DP), intent(in), dimension(nodes) :: cold
    integer(I4B), intent(in) :: nja
    integer(I4B), intent(in) :: njasln
    real(DP), dimension(njasln), intent(inout) :: amatsln
    integer(I4B), intent(in), dimension(nja) :: idxglo
    real(DP), intent(inout), dimension(nodes) :: rhs
    ! -- local
    integer(I4B) :: n, idiag
    real(DP) :: hhcof, rrhs
    real(DP) :: swtpdt
    real(DP) :: vcell
! ------------------------------------------------------------------------------
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
      !
      ! -- add reaction rate terms to accumulators
      if (this%irorder == 1) then
        !
        ! -- first order reaction rate is a function of concentration, so add
        !    to left hand side
        hhcof = -this%rc(n) * vcell * swtpdt * this%porosity(n)
        idiag = this%dis%con%ia(n)
        amatsln(idxglo(idiag)) = amatsln(idxglo(idiag)) + hhcof
      elseif (this%irorder == 0) then
        !
        ! -- zero-order reaction rate is not a function of concentration, so add
        !    to right hand side
        rrhs = this%rc(n) * vcell * swtpdt
        rhs(n) = rhs(n) + rrhs
      endif
      !
    enddo
    !
    ! -- Return
    return
  end subroutine dcy_fc
  
  subroutine dcy_bdcalc(this, nodes, cnew, cold, isuppress_output, model_budget)
! ******************************************************************************
! dcy_bdcalc -- Calculate budget terms
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use TdisModule,        only: delt
    use BudgetModule, only: BudgetType
    ! -- dummy
    class(GwtDcyType) :: this
    integer(I4B), intent(in) :: nodes
    real(DP), intent(in), dimension(nodes) :: cnew
    real(DP), intent(in), dimension(nodes) :: cold
    integer(I4B), intent(in) :: isuppress_output
    type(BudgetType), intent(inout) :: model_budget
    ! -- local
    integer(I4B) :: n
    integer(I4B) :: idiag
    real(DP) :: rate
    real(DP) :: swtpdt
    real(DP) :: rdcyin, rdcyout
    real(DP) :: hhcof, rrhs
    real(DP) :: vcell
! ------------------------------------------------------------------------------
    !
    ! -- initialize 
    rdcyin = DZERO
    rdcyout = DZERO
    !
    ! -- Calculate decay change
    do n = 1, nodes
      !
      ! -- skip if transport inactive
      this%rate(n) = DZERO
      if(this%ibound(n) <= 0) cycle
      !
      ! -- calculate new and old water volumes
      vcell = this%dis%area(n) * (this%dis%top(n) - this%dis%bot(n))
      swtpdt = this%fmi%gwfsat(n)
      idiag = this%dis%con%ia(n)
      !
      ! -- calculate reaction gains and losses
      rate = DZERO
      hhcof = DZERO
      rrhs = DZERO
      if (this%irorder == 1) then
        hhcof = -this%rc(n) * vcell * swtpdt * this%porosity(n)
      elseif (this%irorder == 0) then
        rrhs = this%rc(n) * vcell * swtpdt
      endif
      rate = hhcof * cnew(n) - rrhs
      this%rate(n) = rate
      if (rate < DZERO) then
        rdcyout = rdcyout - rate
      else
        rdcyin = rdcyin + rate
      endif
      !
    enddo
    !
    ! -- Add reaction contributions to model budget
    call model_budget%addentry(rdcyin, rdcyout, delt, '           DECAY',      &
                                isuppress_output)
    !
    ! -- Return
    return
  end subroutine dcy_bdcalc

  subroutine dcy_bdsav(this, icbcfl, icbcun)
! ******************************************************************************
! dcy_bdsav -- Save budget terms
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(GwtDcyType) :: this
    integer(I4B), intent(in) :: icbcfl
    integer(I4B), intent(in) :: icbcun
    ! -- local
    integer(I4B) :: ibinun
    !character(len=16), dimension(2) :: aname
    integer(I4B) :: iprint, nvaluesp, nwidthp
    character(len=1) :: cdatafmp=' ', editdesc=' '
    real(DP) :: dinact
! ------------------------------------------------------------------------------
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
    ! -- Record the decay rates if requested
    if(ibinun /= 0) then
      iprint = 0
      dinact = DZERO
      call this%dis%record_array(this%rate, this%iout, iprint, -ibinun,        &
                                 '           DECAY', cdatafmp, nvaluesp,       &
                                 nwidthp, editdesc, dinact)
    endif
    !
    ! -- Return
    return
  end subroutine dcy_bdsav

  subroutine dcy_da(this)
! ******************************************************************************
! dcy_da -- Deallocate variables
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryManagerModule, only: mem_deallocate
    ! -- dummy
    class(GwtDcyType) :: this
! ------------------------------------------------------------------------------
    !
    ! -- Deallocate arrays if package was active
    if(this%inunit > 0) then
      call mem_deallocate(this%rate)
      call mem_deallocate(this%rc)
      call mem_deallocate(this%irorder)
      this%ibound => null()
      this%porosity => null()
      this%fmi => null()
    endif
    !
    ! -- Scalars
    !
    ! -- deallocate parent
    call this%NumericalPackageType%da()
    !
    ! -- Return
    return
  end subroutine dcy_da

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
    class(GwtDcyType) :: this
    ! -- local
! ------------------------------------------------------------------------------
    !
    ! -- allocate scalars in NumericalPackageType
    call this%NumericalPackageType%allocate_scalars()
    !
    ! -- Allocate
    call mem_allocate(this%irorder, 'IRORDER', this%origin)
    !
    ! -- Initialize
    this%irorder = 1
    !
    ! -- Return
    return
  end subroutine allocate_scalars

  subroutine allocate_arrays(this)
! ******************************************************************************
! allocate_arrays
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use MemoryManagerModule, only: mem_allocate
    !modules
    use ConstantsModule, only: DZERO
    ! -- dummy
    class(GwtDcyType) :: this
    ! -- local
! ------------------------------------------------------------------------------
    !
    ! -- Allocate
    call mem_allocate(this%rate, this%dis%nodes, 'RATE', this%origin)
    call mem_allocate(this%rc, this%dis%nodes, 'RC', this%origin)
    !
    ! -- Initialize
    !
    ! -- Return
    return
  end subroutine allocate_arrays

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
    class(GwtDcyType) :: this
    ! -- local
    character(len=LINELENGTH) :: errmsg, keyword
    integer(I4B) :: ierr
    logical :: isfound, endOfBlock
    ! -- formats
    character(len=*), parameter :: fmtisvflow =                                &
      "(4x,'CELL-BY-CELL FLOW INFORMATION WILL BE SAVED TO BINARY FILE " //    &
      "WHENEVER ICBCFL IS NOT ZERO.')"
    character(len=*), parameter :: fmtirorder1 =                               &
      "(4x,'FIRST ORDER REACTION IS ACTIVE. ')"
    character(len=*), parameter :: fmtirorder0 =                               &
      "(4x,'ZERO ORDER REACTION IS ACTIVE. ')"
! ------------------------------------------------------------------------------
    !
    ! -- get options block
    call this%parser%GetBlock('OPTIONS', isfound, ierr, blockRequired=.false.)
    !
    ! -- parse options block if detected
    if (isfound) then
      write(this%iout,'(1x,a)')'PROCESSING DECAY OPTIONS'
      do
        call this%parser%GetNextLine(endOfBlock)
        if (endOfBlock) exit
        call this%parser%GetStringCaps(keyword)
        select case (keyword)
          case ('SAVE_FLOWS')
            this%ipakcb = -1
            write(this%iout, fmtisvflow)
          case ('ZERO_ORDER')
            this%irorder = 0
            write(this%iout, fmtirorder0)
          case default
            write(errmsg,'(4x,a,a)')'****ERROR. UNKNOWN DCY OPTION: ',         &
                                     trim(keyword)
            call store_error(errmsg)
            call this%parser%StoreErrorUnit()
            call ustop()
        end select
      end do
      if (this%irorder == 1) write(this%iout, fmtirorder1)
      write(this%iout,'(1x,a)')'END OF DECAY OPTIONS'
    end if
    !
    ! -- Return
    return
  end subroutine read_options

  subroutine read_data(this)
! ******************************************************************************
! read_data -- read the sorption data (griddata) block
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use ConstantsModule,   only: LINELENGTH
    use SimModule,         only: ustop, store_error, count_errors
    use MemoryManagerModule, only: mem_reallocate
    ! -- dummy
    class(GwtDcyType) :: this
    ! -- local
    character(len=LINELENGTH) :: line, errmsg, keyword
    integer(I4B) :: istart, istop, lloc, ierr
    logical :: isfound, endOfBlock
    logical, dimension(1) :: lname
    character(len=24), dimension(1) :: aname
    ! -- formats
    ! -- data
    data aname(1) /'        RATE COEFFICIENT'/
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
          case ('RC')
            call this%dis%read_grid_array(line, lloc, istart, istop, this%iout,&
                                         this%parser%iuactive, this%rc,       &
                                         aname(1))
            lname(1) = .true.
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
    ! -- Check for required decay/production rate coefficients
    if (.not. lname(1)) then
      write(errmsg, '(1x,a)') 'ERROR.  FIRST OR ZERO ORDER DECAY IS &
        &ACTIVE BUT THE RATE COEFFICIENT IS NOT SPECIFIED.  RC MUST &
        &BE SPECIFIED IN GRIDDATA BLOCK.'
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
    
end module GwtDcyModule
