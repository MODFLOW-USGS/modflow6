module GwtFmiModule
  
  use KindModule,             only: DP, I4B
  use ConstantsModule,        only: DONE, DZERO, DHALF
  use NumericalPackageModule, only: NumericalPackageType
  use BaseDisModule,          only: DisBaseType
  use ListModule,             only: ListType

  implicit none
  private
  public :: GwtFmiType
  public :: fmi_cr

  type, extends(NumericalPackageType) :: GwtFmiType
    
    integer(I4B),                   pointer :: iflowerr => null()               ! add the flow error correction
    real(DP), dimension(:),         pointer :: flowerr => null()                ! residual error of the flow solution
    integer(I4B), dimension(:),     pointer :: ibound => null()                 ! pointer to GWT ibound
    real(DP), dimension(:),         pointer :: gwfflowja => null()              ! pointer to the GWF flowja array
    real(DP), dimension(:, :),      pointer :: gwfspdis  => null()              ! pointer to npf specific discharge array
    real(DP), dimension(:),         pointer :: gwfhead   => null()              ! pointer to the GWF head array
    real(DP), dimension(:),         pointer :: gwfsat    => null()              ! pointer to the GWF saturation array
    integer(I4B), dimension(:),     pointer :: gwfibound => null()              ! pointer to the GWF ibound array
    real(DP), dimension(:),         pointer :: gwfthksat => null()              ! calculated saturated thickness
    real(DP), dimension(:),         pointer :: gwfstrgss => null()              ! pointer to flow model QSTOSS
    real(DP), dimension(:),         pointer :: gwfstrgsy => null()              ! pointer to flow model QSTOSY
    integer(I4B), pointer                   :: igwfstrgss => null()             ! indicates if gwfstrgss is available
    integer(I4B), pointer                   :: igwfstrgsy => null()             ! indicates if gwfstrgsy is available
    type(ListType),                 pointer :: gwfbndlist => null()             ! list of gwf stress packages

  contains
  
    procedure :: fmi_ar
    procedure :: fmi_ad
    procedure :: fmi_fc
    procedure :: fmi_bdcalc
    procedure :: fmi_da
    procedure :: allocate_scalars
    procedure :: allocate_arrays
  
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
    if (inunit == 0) inunit = 1
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

  subroutine fmi_ar(this, dis, ibound)
! ******************************************************************************
! fmi_ar -- Allocate and Read
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryManagerModule, only: mem_setptr
    ! -- dummy
    class(GwtFmiType)                       :: this
    class(DisBaseType), pointer, intent(in) :: dis
    integer(I4B), dimension(:), pointer          :: ibound
    ! -- local
    ! -- formats
    character(len=*), parameter :: fmtfmi =                                    &
      "(1x,/1x,'FMI -- FLOW MODEL INTERFACE, VERSION 1, 8/29/2017',            &
      &' INPUT READ FROM UNIT ', i0, //)"
! ------------------------------------------------------------------------------
    !
    ! --print a message identifying the FMI package.
    write(this%iout, fmtfmi) this%inunit
    !
    ! -- store pointers to arguments that were passed in
    this%dis     => dis
    this%ibound  => ibound
    !
    ! -- Allocate arrays
    call this%allocate_arrays(dis%nodes)
    !
    ! -- Read storage options
    !call this%read_options()
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
    integer(I4B) :: icomp
    real(DP) :: crewet, tflow, flownm
    character (len=15) :: nodestr
    character(len=*), parameter :: fmtdry = &
     &"(/1X,'WARNING: DRY CELL ENCOUNTERED AT ',a,';  RESET AS INACTIVE')"
    character(len=*), parameter :: fmtrewet = &
     &"(/1X,'DRY CELL REACTIVATED AT ', a,&
     &' FOR SPECIES ',i0,' WITH STARTING CONCENTRATION =',G13.5)"
! ------------------------------------------------------------------------------
    !
    ! -- initialize
    icomp = 1
    !
    ! -- if flow cell is dry, then set gwt%ibound = 0 and conc to dry
    do n = 1, size(this%ibound)
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
          write(this%iout, fmtrewet) trim(nodestr), icomp, crewet
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
    use BndModule,              only: BndType, GetBndFromList
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
    class(BndType), pointer :: packobj
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
    do ip = 1, this%gwfbndlist%Count()
      packobj => GetBndFromList(this%gwfbndlist, ip)
      do i = 1, packobj%nbound
        n = packobj%nodelist(i)
        if (this%gwfibound(n) <= 0) cycle
        qbnd = packobj%hcof(i) * packobj%xnew(n) - packobj%rhs(i)
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
  
  subroutine fmi_bdcalc(this, icomp, cnew, isuppress_output, model_budget)
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
    integer, intent(in) :: icomp
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
    nodes = size(cnew)
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
    !
    ! -- nullify pointers
    this%gwfflowja => null()
    this%gwfspdis  => null()
    this%gwfhead   => null() 
    this%gwfsat    => null() 
    this%gwfibound => null() 
    this%gwfstrgss => null()    
    this%gwfstrgsy => null()    
    this%gwfbndlist => null()
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
    call mem_allocate(this%iflowerr, 'IFLOWERR', this%origin)
    call mem_allocate(this%igwfstrgss, 'IGWFSTRGSS', this%origin)
    call mem_allocate(this%igwfstrgsy, 'IGWFSTRGSY', this%origin)
    !
    ! -- Initialize
    this%iflowerr = 1
    this%igwfstrgss = 0
    this%igwfstrgsy = 0
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
    ! -- Allocate
    call mem_allocate(this%gwfthksat, nodes, 'THKSAT', this%origin)
    call mem_allocate(this%flowerr, nodes, 'FLOWERR', this%origin)
    !
    ! -- Initialize
    do n = 1, nodes
      this%gwfthksat(n) = DZERO
      this%flowerr(n) = DZERO
    enddo
    !
    ! -- Return
    return
  end subroutine allocate_arrays

end module GwtFmiModule
