module GwtFmiModule
  
  use KindModule,             only: DP, I4B
  use ConstantsModule,        only: DONE, DZERO, DHALF, LINELENGTH
  use SimModule,              only: store_error, store_error_unit, ustop
  use NumericalPackageModule, only: NumericalPackageType
  use BaseDisModule,          only: DisBaseType
  use ListModule,             only: ListType
  use BudgetFileReaderModule, only: BudgetFileReaderType
  use HeadFileReaderModule,   only: HeadFileReaderType
  use PackageBudgetModule,    only: PackageBudgetType

  implicit none
  private
  public :: GwtFmiType
  public :: fmi_cr

  type, extends(NumericalPackageType) :: GwtFmiType
    
    logical, pointer                                :: flows_from_file => null() ! if .false., then there is no water flow
    integer(I4B), dimension(:), pointer, contiguous :: iatp => null()           ! advanced transport package applied to gwfpackages
    type(ListType), pointer                         :: gwfbndlist => null()     ! list of gwf stress packages
    integer(I4B), pointer                           :: iflowerr => null()       ! add the flow error correction
    real(DP), dimension(:), pointer, contiguous     :: flowerr => null()        ! residual error of the flow solution
    integer(I4B), dimension(:), pointer, contiguous :: ibound => null()         ! pointer to GWT ibound
    real(DP), dimension(:), pointer, contiguous     :: gwfflowja => null()      ! pointer to the GWF flowja array
    real(DP), dimension(:, :), pointer, contiguous  :: gwfspdis  => null()      ! pointer to npf specific discharge array
    real(DP), dimension(:), pointer, contiguous     :: gwfhead   => null()      ! pointer to the GWF head array
    real(DP), dimension(:), pointer, contiguous     :: gwfsat    => null()      ! pointer to the GWF saturation array
    integer(I4B), dimension(:), pointer, contiguous :: gwfibound => null()      ! pointer to the GWF ibound array
    real(DP), dimension(:), pointer, contiguous     :: gwfthksat => null()      ! calculated saturated thickness
    real(DP), dimension(:), pointer, contiguous     :: gwfstrgss => null()      ! pointer to flow model QSTOSS
    real(DP), dimension(:), pointer, contiguous     :: gwfstrgsy => null()      ! pointer to flow model QSTOSY
    integer(I4B), pointer                           :: igwfstrgss => null()     ! indicates if gwfstrgss is available
    integer(I4B), pointer                           :: igwfstrgsy => null()     ! indicates if gwfstrgsy is available
    integer(I4B), dimension(:), pointer, contiguous :: gwficelltype => null()   ! pointer to the GWF icelltype array
    integer(I4B), pointer                           :: igwfinwtup => null()     ! NR indicator
    integer(I4B), pointer                           :: iubud => null()          ! unit number GWF budget file
    integer(I4B), pointer                           :: iuhds => null()          ! unit number GWF head file
    integer(I4B), pointer                           :: nflowpack => null()
    type(BudgetFileReaderType)                      :: bfr
    type(HeadFileReaderType)                        :: hfr
    type(PackageBudgetType), dimension(:), allocatable :: gwfpackages
  contains
  
    procedure :: fmi_ar
    procedure :: fmi_ad
    procedure :: fmi_fc
    procedure :: fmi_bdcalc
    procedure :: fmi_da
    procedure :: allocate_scalars
    procedure :: allocate_arrays
    procedure :: gwfsatold
    procedure :: read_options
    procedure :: initialize_bfr
    procedure :: advance_bfr
    procedure :: finalize_bfr
    procedure :: initialize_hfr
    procedure :: advance_hfr
    procedure :: finalize_hfr
    procedure :: allocate_gwfpackages
  
  end type GwtFmiType

  contains
  
  subroutine fmi_cr(fmiobj, name_model, inunit, iout)
! ******************************************************************************
! fmi_cr -- Create a new FMI object
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    type(GwtFmiType), pointer :: fmiobj
    character(len=*), intent(in) :: name_model
    integer(I4B), intent(inout) :: inunit
    integer(I4B), intent(in) :: iout
! ------------------------------------------------------------------------------
    !
    ! -- Create the object
    allocate(fmiobj)
    !
    ! -- create name and origin
    call fmiobj%set_names(1, name_model, 'FMI', 'FMI')
    !
    ! -- Allocate scalars
    call fmiobj%allocate_scalars()
    !
    ! -- if inunit == 0, then there is no file to read, but it still needs
    !    to be active in order to manage pointers to gwf model
    !if (inunit == 0) inunit = 1
    !
    ! -- Set variables
    fmiobj%inunit = inunit
    fmiobj%iout = iout
    !
    ! -- Initialize block parser
    call fmiobj%parser%Initialize(fmiobj%inunit, fmiobj%iout)
    !
    ! -- Return
    return
  end subroutine fmi_cr

  subroutine fmi_ar(this, dis, ibound, inssm)
! ******************************************************************************
! fmi_ar -- Allocate and Read
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use SimModule,           only: ustop, store_error
    ! -- dummy
    class(GwtFmiType) :: this
    class(DisBaseType), pointer, intent(in) :: dis
    integer(I4B), dimension(:), pointer, contiguous :: ibound
    integer(I4B), intent(in) :: inssm
    ! -- local
    ! -- formats
    character(len=*), parameter :: fmtfmi =                                    &
      "(1x,/1x,'FMI -- FLOW MODEL INTERFACE, VERSION 1, 8/29/2017',            &
      &' INPUT READ FROM UNIT ', i0, //)"
    character(len=*), parameter :: fmtfmi0 =                                   &
      "(1x,/1x,'FMI -- FLOW MODEL INTERFACE, VERSION 1, 8/29/2017')"
! ------------------------------------------------------------------------------
    !
    ! --print a message identifying the FMI package.
    if (this%inunit /= 0) then
      write(this%iout, fmtfmi) this%inunit
    else
      write(this%iout, fmtfmi0)
      if (.not. this%flows_from_file) then
        write(this%iout, '(a)') '  FLOWS PROVIDED BY A GWF MODEL IN THIS &
          &SIMULATION'
      else
        write(this%iout, '(a)') '  FLOWS ARE ASSUMED TO BE ZERO.'
      endif 
    endif
    !
    ! -- Add a check to see if GWF-GWT Exchange is on and the FMI 
    !    package is specified by the user.  Program should return with an
    !    error in this case.
    !if (.not. this%flows_from_file .and. this%inunit /= 0) then
    !  call store_error('ERROR: A GWF-GWT EXCHANGE IS MAKING GWF FLOWS&
    !    & AVAILABLE FOR THIS TRANSPORT MODEL AND AN FMI PACKAGE HAS ALSO&
    !    & BEEN SPECIFIED BY THE USER.  REMOVE THE FMI PACKAGE FROM THE&
    !    & GWT NAME FILE OR TURN OFF THE GWF-GWT EXCHANGE IN MFSIM.NAM')
    !end if
    !
    ! -- store pointers to arguments that were passed in
    this%dis     => dis
    this%ibound  => ibound
    !
    ! -- Allocate arrays
    call this%allocate_arrays(dis%nodes)
    !
    ! -- Read fmi options
    if (this%inunit /= 0) then
      call this%read_options()
    end if
    !
    ! -- Initialize the budget file reader
    if (this%iubud /= 0) then
      call this%initialize_bfr()
    endif
    !
    ! -- Initialize the head file reader
    if (this%iuhds /= 0) then
      call this%initialize_hfr()
    endif
    !
    ! -- Make sure that ssm is on if there are any boundary packages
    if (inssm == 0) then
      if (this%nflowpack > 0) then
        call store_error('ERROR: FLOW MODEL HAS BOUNDARY PACKAGES, BUT THERE &
          &IS NO SSM PACKAGE.  THE SSM PACKAGE MUST BE ACTIVATED.')
        call ustop()
      endif
    endif
    !
    ! -- read the data block
    !call this%read_data()
    !
    ! -- Return
    return
  end subroutine fmi_ar
  
  subroutine fmi_ad(this, cnew)
! ******************************************************************************
! fmi_ad -- advance
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use ConstantsModule, only: DHDRY
    ! -- dummy
    class(GwtFmiType) :: this
    real(DP), intent(inout), dimension(:) :: cnew
    ! -- local
    integer(I4B) :: n
    integer(I4B) :: m
    integer(I4B) :: ipos
    real(DP) :: crewet, tflow, flownm
    character (len=15) :: nodestr
    character(len=*), parameter :: fmtdry = &
     &"(/1X,'WARNING: DRY CELL ENCOUNTERED AT ',a,';  RESET AS INACTIVE')"
    character(len=*), parameter :: fmtrewet = &
     &"(/1X,'DRY CELL REACTIVATED AT ', a,&
     &' WITH STARTING CONCENTRATION =',G13.5)"
! ------------------------------------------------------------------------------
    !
    ! -- If reading flows from a budget file, read the next set of records
    if (this%iubud /= 0) then
      call this%advance_bfr()
    endif
    !
    ! -- If reading heads from a head file, read the next set of records
    if (this%iuhds /= 0) then
      call this%advance_hfr()
    endif
    !
    ! -- if flow cell is dry, then set gwt%ibound = 0 and conc to dry
    do n = 1, this%dis%nodes
      !
      ! -- Check if active transport cell is inactive for flow
      if (this%ibound(n) > 0) then
        if (this%gwfhead(n) == DHDRY) then
          ! -- transport cell should be made inactive
          this%ibound(n) = 0
          cnew(n) = DHDRY
          call this%dis%noder_to_string(n, nodestr)
          write(this%iout, fmtdry) trim(nodestr)
        endif
      endif
      !
      ! -- Convert dry transport cell to active if flow has rewet
      if (cnew(n) == DHDRY) then
        if (this%gwfibound(n) > 0) then
          !
          ! -- obtain weighted concentration
          crewet = DZERO
          tflow = DZERO
          do ipos = this%dis%con%ia(n) + 1, this%dis%con%ia(n + 1) - 1
            m = this%dis%con%ja(ipos)
            flownm = this%gwfflowja(ipos)
            if (flownm > 0) then
              if (this%ibound(m) /= 0) then
                crewet = crewet + cnew(m) * flownm
                tflow = tflow + this%gwfflowja(ipos)
              endif
            endif
          enddo
          if (tflow > DZERO) then
            crewet = crewet / tflow
          else
            crewet = DZERO
          endif
          !
          ! -- cell is now wet
          this%ibound(n) = 1
          cnew(n) = crewet
          call this%dis%noder_to_string(n, nodestr)
          write(this%iout, fmtrewet) trim(nodestr), crewet
        endif
      endif
    enddo
    !
    ! -- Return
    return
  end subroutine fmi_ad
  
  subroutine fmi_fc(this, nodes, cold, nja, njasln, amatsln, idxglo, rhs)
! ******************************************************************************
! fmi_fc -- Calculate coefficients and fill amat and rhs
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    !use BndModule,              only: BndType, GetBndFromList
    ! -- dummy
    class(GwtFmiType) :: this
    integer, intent(in) :: nodes
    real(DP), intent(in), dimension(nodes) :: cold
    integer(I4B), intent(in) :: nja
    integer(I4B), intent(in) :: njasln
    real(DP), dimension(njasln), intent(inout) :: amatsln
    integer(I4B), intent(in), dimension(nja) :: idxglo
    real(DP), intent(inout), dimension(nodes) :: rhs
    ! -- local
    !class(BndType), pointer :: packobj
    integer(I4B) :: n, ipos, idiag
    integer(I4B) :: ip, i
    real(DP) :: qbnd
! ------------------------------------------------------------------------------
    !
    ! -- If not adding flow error correction, return
    if (this%iflowerr == 0) return
    !
    ! -- Loop through and calculate flow residual for face flows and storage
    do n = 1, nodes
      this%flowerr(n) = DZERO
      if (this%gwfibound(n) <= 0) cycle
      do ipos = this%dis%con%ia(n) + 1, this%dis%con%ia(n + 1) - 1
        this%flowerr(n) = this%flowerr(n) + this%gwfflowja(ipos)
      enddo
      if (this%igwfstrgss /= 0) &
        this%flowerr(n) = this%flowerr(n) + this%gwfstrgss(n)
      if (this%igwfstrgsy /= 0) &
        this%flowerr(n) = this%flowerr(n) + this%gwfstrgsy(n)
    enddo
    !
    ! -- Add package flow terms
    do ip = 1, this%nflowpack
      do i = 1, this%gwfpackages(ip)%nbound
        n = this%gwfpackages(ip)%nodelist(i)
        if (this%gwfibound(n) <= 0) cycle
        qbnd = this%gwfpackages(ip)%get_flow(i)
        this%flowerr(n) = this%flowerr(n) + qbnd
      enddo
    enddo
    !
    ! -- Correct the transport solution for the flow imbalance by adding
    !    the flow residual to the diagonal
    do n = 1, nodes
      idiag = idxglo(this%dis%con%ia(n))
      amatsln(idiag) = amatsln(idiag) - this%flowerr(n)
    enddo
    !
    ! -- Return
    return
  end subroutine fmi_fc
  
  subroutine fmi_bdcalc(this, cnew, isuppress_output, model_budget)
! ******************************************************************************
! fmi_fc -- Calculate coefficients and fill amat and rhs
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use TdisModule, only: delt
    use BudgetModule, only: BudgetType
    ! -- dummy
    class(GwtFmiType) :: this
    real(DP), intent(in), dimension(:) :: cnew
    integer(I4B), intent(in) :: isuppress_output
    type(BudgetType), intent(inout) :: model_budget
    ! -- local
    integer(I4B) :: n, nodes
    real(DP) :: rate, rin, rout
! ------------------------------------------------------------------------------
    !
    ! -- If not adding flow error correction, return
    if (this%iflowerr == 0) return
    !
    ! -- initialize 
    rin = DZERO
    rout = DZERO
    nodes = this%dis%nodes
    !
    ! -- Accumulate the flow error term
    do n = 1, nodes
      if (this%ibound(n) <= 0) cycle
      rate = this%flowerr(n) * cnew(n)
      if (rate < 0) then
        rout = rout - rate
      else
        rin = rin + rate
      endif
    enddo
    !
    ! -- Add contributions to model budget
    call model_budget%addentry(rin, rout, delt, '      FLOW-ERROR',            &
                               isuppress_output)
    !
    ! -- Return
    return
  end subroutine fmi_bdcalc
  
  subroutine fmi_da(this)
! ******************************************************************************
! fmi_da -- Deallocate variables
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryManagerModule, only: mem_deallocate
    ! -- dummy
    class(GwtFmiType) :: this
! ------------------------------------------------------------------------------
    ! -- todo: finalize hfr and bfr either here or in a finalize routine
    !
    ! -- nullify pointers
    ! -- todo: memdeallocate these if flows_from_file
    this%gwfflowja => null()
    this%gwfspdis  => null()
    this%gwfhead   => null() 
    this%gwfsat    => null() 
    this%gwfibound => null() 
    this%gwfstrgss => null()    
    this%gwfstrgsy => null()    
    this%gwfbndlist => null()
    this%gwficelltype => null()
    this%igwfinwtup => null()
    !
    ! -- deallocate fmi arrays
    call mem_deallocate(this%gwfthksat)
    call mem_deallocate(this%flowerr)
    !
    ! -- deallocate scalars
    call mem_deallocate(this%iflowerr)
    call mem_deallocate(this%igwfstrgss)
    call mem_deallocate(this%igwfstrgsy)
    !
    ! -- deallocate parent
    call this%NumericalPackageType%da()
    !
    ! -- Return
    return
  end subroutine fmi_da
  
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
    class(GwtFmiType) :: this
    ! -- local
! ------------------------------------------------------------------------------
    !
    ! -- allocate scalars in NumericalPackageType
    call this%NumericalPackageType%allocate_scalars()
    !
    ! -- Allocate
    call mem_allocate(this%flows_from_file, 'FLOWS_FROM_FILE', this%origin)
    call mem_allocate(this%iflowerr, 'IFLOWERR', this%origin)
    call mem_allocate(this%igwfstrgss, 'IGWFSTRGSS', this%origin)
    call mem_allocate(this%igwfstrgsy, 'IGWFSTRGSY', this%origin)
    call mem_allocate(this%iubud, 'IUBUD', this%origin)
    call mem_allocate(this%iuhds, 'IUHDS', this%origin)
    call mem_allocate(this%nflowpack, 'NFLOWPACK', this%origin)
    !
    ! -- Initialize
    this%flows_from_file = .true.
    this%iflowerr = 1
    this%igwfstrgss = 0
    this%igwfstrgsy = 0
    this%iubud = 0
    this%iuhds = 0
    this%nflowpack = 0
    !
    ! -- Return
    return
  end subroutine allocate_scalars

  subroutine allocate_arrays(this, nodes)
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
    class(GwtFmiType) :: this
    integer(I4B), intent(in) :: nodes
    ! -- local
    integer(I4B) :: n
! ------------------------------------------------------------------------------
    !
    ! -- Allocate variables needed for all cases
    call mem_allocate(this%gwfthksat, nodes, 'THKSAT', this%origin)
    call mem_allocate(this%flowerr, nodes, 'FLOWERR', this%origin)
    call mem_allocate(this%iatp, this%nflowpack, 'IATP', this%origin)
    !
    ! -- Initialize
    do n = 1, nodes
      this%gwfthksat(n) = DZERO
      this%flowerr(n) = DZERO
    enddo
    do n = 1, this%nflowpack
      this%iatp(n) = 0
    end do
    !
    ! -- Allocate variables needed when there isn't a GWF model running 
    !    concurrently.  In that case, these variables are pointed directly
    !    to the corresponding GWF variables.
    if (this%flows_from_file) then
      call mem_allocate(this%igwfinwtup, 'IGWFINWTUP', this%origin)
      call mem_allocate(this%gwfflowja, this%dis%con%nja, 'GWFFLOWJA', this%origin)
      call mem_allocate(this%gwfsat, nodes, 'GWFSAT', this%origin)
      call mem_allocate(this%gwfhead, nodes, 'GWFHEAD', this%origin)
      !call mem_allocate(this%gwfstrgss, nodes, 'GWFSTRGSS', this%origin)
      !call mem_allocate(this%gwfstrgsy, nodes, 'GWFSTRGSY', this%origin)
      call mem_allocate(this%gwfspdis, 3, nodes, 'GWFSPDIS', this%origin)
      call mem_allocate(this%gwfibound, nodes, 'GWFIBOUND', this%origin)
      call mem_allocate(this%gwficelltype, nodes, 'GWFICELLTYPE', this%origin)
      this%igwfinwtup = 0
      !this%igwfstrgss = 1
      !this%igwfstrgsy = 1
      do n = 1, nodes
        this%gwfsat(n) = DONE
        this%gwfhead(n) = DZERO
        !this%gwfstrgss(n) = DZERO
        !this%gwfstrgsy(n) = DZERO
        this%gwfspdis(:, n) = DZERO
        this%gwfibound(n) = 1
        this%gwficelltype(n) = 0
      end do
      do n = 1, size(this%gwfflowja)
        this%gwfflowja(n) = DZERO
      end do
    end if
    !
    ! -- todo: need to handle  allocation of gwfstrgss and gwfstrgsy
    !    for transient flow when fmi reading from file
    !
    ! -- Return
    return
  end subroutine allocate_arrays
  
  function gwfsatold(this, n, delt) result(satold)
! ******************************************************************************
! gwfsatold -- calculate the groundwater cell head saturation for the end of
!   the last time step
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(GwtFmiType) :: this
    integer(I4B), intent(in) :: n
    real(DP), intent(in) :: delt
    ! -- result
    real(DP) :: satold
    ! -- local
    real(DP) :: vcell
    real(DP) :: vnew
    real(DP) :: vold
! ------------------------------------------------------------------------------
    !
    ! -- calculate the value
    vcell = this%dis%area(n) * (this%dis%top(n) - this%dis%bot(n))
    vnew = vcell * this%gwfsat(n)
    vold = vnew
    if (this%igwfstrgss /= 0) vold = vold + this%gwfstrgss(n) * delt
    if (this%igwfstrgsy /= 0) vold = vold + this%gwfstrgsy(n) * delt
    satold = vold / vcell
    !
    ! -- Return
    return
  end function gwfsatold
  
  subroutine read_options(this)
! ******************************************************************************
! read_options -- Read Options
! Subroutine: (1) read options from input file
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use OpenSpecModule, only: ACCESS, FORM
    use ConstantsModule, only: LINELENGTH, DEM6
    use InputOutputModule, only: getunit, openfile, urdaux
    use SimModule, only: store_error, store_error_unit, ustop
    ! -- dummy
    class(GwtFmiType) :: this
    ! -- local
    character(len=LINELENGTH) :: errmsg, keyword, fname
    integer(I4B) :: ierr
    integer(I4B) :: inunit
    logical :: isfound, endOfBlock
! ------------------------------------------------------------------------------
    !
    ! -- get options block
    call this%parser%GetBlock('OPTIONS', isfound, ierr, blockRequired=.false.)
    !
    ! -- parse options block if detected
    if (isfound) then
      write(this%iout,'(1x,a)')'PROCESSING FMI OPTIONS'
      do
        call this%parser%GetNextLine(endOfBlock)
        if (endOfBlock) exit
        call this%parser%GetStringCaps(keyword)
        select case (keyword)
          case ('GWFBUDGET')
            call this%parser%GetStringCaps(keyword)
            if(keyword /= 'FILEIN') then
              call store_error('GWFBUDGET KEYWORD MUST BE FOLLOWED BY ' //     &
                '"FILEIN" then by filename.')
              call this%parser%StoreErrorUnit()
              call ustop()
            endif
            call this%parser%GetString(fname)
            inunit = getunit()
            call openfile(inunit, this%iout, fname, 'DATA(BINARY)', FORM, ACCESS)
            this%iubud = inunit
          case ('GWFHEAD')
            call this%parser%GetStringCaps(keyword)
            if(keyword /= 'FILEIN') then
              call store_error('GWFHEAD KEYWORD MUST BE FOLLOWED BY ' //     &
                '"FILEIN" then by filename.')
              call this%parser%StoreErrorUnit()
              call ustop()
            endif
            call this%parser%GetString(fname)
            inunit = getunit()
            call openfile(inunit, this%iout, fname, 'DATA(BINARY)', FORM, ACCESS)
            this%iuhds = inunit
          case default
            write(errmsg,'(4x,a,a)')'***ERROR. UNKNOWN FMI OPTION: ', &
                                     trim(keyword)
            call store_error(errmsg)
            call this%parser%StoreErrorUnit()
            call ustop()
        end select
      end do
      write(this%iout,'(1x,a)') 'END OF FMI OPTIONS'
    end if
    !
    ! -- return
    return
  end subroutine read_options

  subroutine initialize_bfr(this)
! ******************************************************************************
! initialize_bfr -- initalize the budget file reader and figure out how many
!   different terms and packages are contained within the file
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryManagerModule, only: mem_reallocate
    ! -- dummy
    class(GwtFmiType) :: this
    integer(I4B) :: ncrbud
    integer(I4B) :: nflowpack
    integer(I4B) :: i, ip
    integer(I4B) :: naux
    logical :: found_flowja
    logical :: found_dataspdis
    logical :: found_stoss
    logical :: found_stosy
    integer(I4B), dimension(:), allocatable :: imap
! ------------------------------------------------------------------------------
    !
    ! -- initialize variables
    found_flowja = .false.
    found_dataspdis = .false.
    found_stoss = .false.
    found_stosy = .false.
    !
    ! -- Initialize the budget file reader
    call this%bfr%initialize(this%iubud, this%iout, ncrbud)
    !
    ! -- Calculate the number of gwf flow packages
    allocate(imap(this%bfr%nbudterms))
    imap(:) = 0
    nflowpack = 0
    do i = 1, this%bfr%nbudterms
      select case(trim(adjustl(this%bfr%budtxtarray(i))))
      case ('FLOW-JA-FACE')
        found_flowja = .true.
      case ('DATA-SPDIS')
        found_dataspdis = .true.
      case ('STO-SS')
        found_stoss = .true.
      case ('STO-SY')
        found_stosy = .true.
      case default
        nflowpack = nflowpack + 1
        imap(i) = 1
      end select
    end do
    !
    ! -- allocate gwfpackages and set the name
    call this%allocate_gwfpackages(nflowpack)
    ip = 1
    do i = 1, this%bfr%nbudterms
      if (imap(i) == 0) cycle
      call this%gwfpackages(ip)%set_name(this%bfr%dstpackagenamearray(i))
      naux = this%bfr%nauxarray(i)
      call this%gwfpackages(ip)%set_auxname(this%bfr%auxtxtarray(1:naux, i))
      ip = ip + 1
    end do
    !
    ! -- Now that nflowpack is known, need to reallocate the advanced
    !    package indicator array
    call mem_reallocate(this%iatp, nflowpack, 'IATP', this%origin)
    this%iatp(:) = 0
    !
    ! -- todo: error if flowja and qxqyqz not found
    
  end subroutine initialize_bfr
  
  subroutine advance_bfr(this)
! ******************************************************************************
! advance_bfr -- advance the budget file reader by reading the next chunk
!   of information for the current time step and stress period
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use TdisModule, only: kstp, kper
    ! -- dummy
    class(GwtFmiType) :: this
    ! -- local
    character(len=LINELENGTH) :: errmsg
    logical :: success
    integer(I4B) :: n
    integer(I4B) :: iposu, iposr
    integer(I4B) :: nu, nr
    integer(I4B) :: ip, i
    logical :: readnext
    ! -- format
    character(len=*), parameter :: fmtkstpkper =                               &
      "(1x,/1x,'FMI READING BUDGET TERMS FOR KSTP ', i0, ' KPER ', i0)"
    character(len=*), parameter :: fmtbudkstpkper = &
      "(1x,/1x, 'FMI SETTING BUDGET TERMS FOR KSTP ', i0, ' AND KPER ',        &
      &i0, ' TO BUDGET FILE TERMS FROM KSTP ', i0, ' AND KPER ', i0)"
! ------------------------------------------------------------------------------
    !
    ! -- Do not read the budget if the budget is at end of file or if the next
    !    record in the budget file is the first timestep of the next stress
    !    period.
    readnext = .true.
    if (kstp * kper > 1) then
      if (this%bfr%endoffile) then
        readnext = .false.
      else
        if (this%bfr%kpernext == kper + 1 .and. this%bfr%kstpnext == 1) &
          readnext = .false.
      endif
    endif
    !
    ! -- Read the next record
    if (readnext) then
      !
      ! -- Write the current time step and stress period
      write(this%iout, fmtkstpkper) kstp, kper
      !
      ! -- loop through the budget terms for this stress period
      !    i is the counter for gwf flow packages
      ip = 1
      do n = 1, this%bfr%nbudterms
        call this%bfr%read_record(success, this%iout)
        if (.not. success) then
          write(errmsg,'(4x,a)') '***ERROR.  GWF BUDGET READ NOT SUCCESSFUL'
          call store_error(errmsg)
          call store_error_unit(this%iubud)
          call ustop()
        endif
        !
        ! -- Ensure kper is same between model and budget file
        if (kper /= this%bfr%kper) then
          write(errmsg,'(4x,a)') '***ERROR.  PERIOD NUMBER IN BUDGET FILE &
            &DOES NOT MATCH PERIOD NUMBER IN TRANSPORT MODEL.'
          call store_error(errmsg)
          call store_error_unit(this%iubud)
          call ustop()
        endif
        !
        ! -- if budget file kstp > 1, then kstp must match
        if (this%bfr%kstp > 1 .and. (kstp /= this%bfr%kstp)) then
          write(errmsg,'(4x,a)') '***ERROR.  IF THERE IS MORE THAN ONE TIME &
            &STEP IN THE BUDGET FILE, THEN BUDGET FILE TIME STEPS MUST MATCH &
            &GWT MODEL TIME STEPS ONE-FOR-ONE.'
          call store_error(errmsg)
          call store_error_unit(this%iubud)
          call ustop()
        endif
        !
        ! -- parse based on the type of data, and compress all user node
        !    numbers into reduced node numbers
        select case(trim(adjustl(this%bfr%budtxt)))
        case('FLOW-JA-FACE')
            iposr = 0
            do iposu = 1, size(this%bfr%flowja)
              nu = this%dis%con%jausr(iposu)
              nr = this%dis%get_nodenumber(nu, 0)
              if (nr <= 0) cycle
              iposr = iposr + 1
              this%gwfflowja(iposr) = this%bfr%flowja(iposu)
            end do
          case('DATA-SPDIS')
            do nu = 1, this%dis%nodesuser
              nr = this%dis%get_nodenumber(nu, 0)
              if (nr <= 0) cycle
              this%gwfspdis(1, nr) = this%bfr%auxvar(1, nu)
              this%gwfspdis(2, nr) = this%bfr%auxvar(2, nu)
              this%gwfspdis(3, nr) = this%bfr%auxvar(3, nu)
            end do
          case('STO-SS')
            do nu = 1, this%dis%nodesuser
              nr = this%dis%get_nodenumber(nu, 0)
              if (nr <= 0) cycle
              this%gwfstrgss(nr) = this%bfr%flow(nu)
            end do
          case('STO-SY')
            do nu = 1, this%dis%nodesuser
              nr = this%dis%get_nodenumber(nu, 0)
              if (nr <= 0) cycle
              this%gwfstrgsy(nr) = this%bfr%flow(nu)
            end do
          case default
            call this%gwfpackages(ip)%copy_values( &
                                                 this%bfr%dstpackagename, &
                                                 this%bfr%auxtxt, &
                                                 this%bfr%nlist, &
                                                 this%bfr%naux, &
                                                 this%bfr%nodesrc, &
                                                 this%bfr%flow, &
                                                 this%bfr%auxvar)
            do i = 1, this%gwfpackages(ip)%nbound
              nu = this%gwfpackages(ip)%nodelist(i)
              nr = this%dis%get_nodenumber(nu, 0)
              this%gwfpackages(ip)%nodelist(i) = nr
            end do
            ip = ip + 1
        end select
      end do
    else
      write(this%iout, fmtbudkstpkper) kstp, kper, this%bfr%kstp, this%bfr%kper
    endif
  end subroutine advance_bfr
  
  subroutine finalize_bfr(this)
! ******************************************************************************
! finalize_bfr -- finalize the budget file reader
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    class(GwtFmiType) :: this
    ! -- dummy
! ------------------------------------------------------------------------------
    !
    ! -- Finalize the budget file reader
    call this%bfr%finalize()
    !
  end subroutine finalize_bfr
  
  subroutine initialize_hfr(this)
! ******************************************************************************
! initialize_hfr -- initalize the head file reader
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    class(GwtFmiType) :: this
    ! -- dummy
! ------------------------------------------------------------------------------
    !
    ! -- Initialize the budget file reader
    call this%hfr%initialize(this%iuhds, this%iout)
    !
    ! -- todo: need to run through the head terms
    !    and do some checking
  end subroutine initialize_hfr
  
  subroutine advance_hfr(this)
! ******************************************************************************
! advance_hfr -- advance the head file reader
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use TdisModule, only: kstp, kper
    class(GwtFmiType) :: this
    character(len=LINELENGTH) :: errmsg
    integer(I4B) :: nu, nr, i, ilay
    integer(I4B) :: ncpl
    real(DP) :: val
    logical :: readnext
    logical :: success
    character(len=*), parameter :: fmtkstpkper =                               &
      "(1x,/1x,'FMI READING HEAD FOR KSTP ', i0, ' KPER ', i0)"
    character(len=*), parameter :: fmthdskstpkper = &
      "(1x,/1x, 'FMI SETTING HEAD FOR KSTP ', i0, ' AND KPER ',        &
      &i0, ' TO BINARY FILE HEADS FROM KSTP ', i0, ' AND KPER ', i0)"
! ------------------------------------------------------------------------------
    !
    ! -- Do not read heads if the head is at end of file or if the next
    !    record in the head file is the first timestep of the next stress
    !    period.
    readnext = .true.
    if (kstp * kper > 1) then
      if (this%hfr%endoffile) then
        readnext = .false.
      else
        if (this%hfr%kpernext == kper + 1 .and. this%hfr%kstpnext == 1) &
          readnext = .false.
      endif
    endif
    !
    ! -- Read the next record
    if (readnext) then
      !
      ! -- write to list file that heads are being read
      write(this%iout, fmtkstpkper) kstp, kper
      !
      ! -- loop through the layered heads for this time step
      do ilay = 1, this%hfr%nlay
        !
        ! -- read next head chunk
        call this%hfr%read_record(success, this%iout)
        if (.not. success) then
          write(errmsg,'(4x,a)') '***ERROR.  GWF HEAD READ NOT SUCCESSFUL'
          call store_error(errmsg)
          call store_error_unit(this%iuhds)
          call ustop()
        endif
        !
        ! -- Ensure kper is same between model and head file
        if (kper /= this%hfr%kper) then
          write(errmsg,'(4x,a)') '***ERROR.  PERIOD NUMBER IN HEAD FILE &
            &DOES NOT MATCH PERIOD NUMBER IN TRANSPORT MODEL.'
          call store_error(errmsg)
          call store_error_unit(this%iuhds)
          call ustop()
        endif
        !
        ! -- if head file kstp > 1, then kstp must match
        if (this%hfr%kstp > 1 .and. (kstp /= this%hfr%kstp)) then
          write(errmsg,'(4x,a)') '***ERROR.  IF THERE IS MORE THAN ONE TIME &
            &STEP IN THE HEAD FILE, THEN HEAD FILE TIME STEPS MUST MATCH &
            &GWT MODEL TIME STEPS ONE-FOR-ONE.'
          call store_error(errmsg)
          call store_error_unit(this%iuhds)
          call ustop()
        endif
        !
        ! -- fill the head array for this layer and
        !    compress into reduced form
        ncpl = size(this%hfr%head)
        do i = 1, ncpl
          nu = (ilay - 1) * ncpl + i
          nr = this%dis%get_nodenumber(nu, 0)
          val = this%hfr%head(i)
          if (nr > 0) this%gwfhead(nr) = val
        enddo
      end do
    else
      write(this%iout, fmthdskstpkper) kstp, kper, this%hfr%kstp, this%hfr%kper
    endif
  end subroutine advance_hfr
  
  subroutine finalize_hfr(this)
! ******************************************************************************
! finalize_hfr -- finalize the head file reader
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    class(GwtFmiType) :: this
    ! -- dummy
! ------------------------------------------------------------------------------
    !
    ! -- Finalize the head file reader
    close(this%iuhds)
    !
  end subroutine finalize_hfr
  
  subroutine allocate_gwfpackages(this, nflowpack)
! ******************************************************************************
! allocate_gwfpackages -- allocate the gwfpackages array
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(GwtFmiType) :: this
    integer(I4B), intent(in) :: nflowpack
! ------------------------------------------------------------------------------
    !
    ! -- allocate
    allocate(this%gwfpackages(nflowpack))
    this%nflowpack = nflowpack
    !
  end subroutine allocate_gwfpackages
  
end module GwtFmiModule
