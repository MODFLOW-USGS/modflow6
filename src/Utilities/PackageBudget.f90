module PackageBudgetModule
  
  use KindModule
  use ConstantsModule, only: LENPACKAGENAME, LENAUXNAME, LENMEMPATH
  use MemoryManagerModule, only: mem_allocate, mem_reassignptr,                &
                                 mem_reallocate, mem_deallocate

  implicit none
  
  private
  public :: PackageBudgetType
  
  type :: PackageBudgetType
    
    character(len=LENMEMPATH) :: memoryPath = ''                                 !< the location in the memory manager where the variables are stored
    character(len=LENPACKAGENAME), pointer :: name => null()                     !< name of the package
    character(len=LENPACKAGENAME), pointer :: budtxt => null()                   !< type of flow (CHD, RCH, RCHA, ...)
    character(len=LENAUXNAME), dimension(:), pointer,                           &
                                 contiguous :: auxname => null()                 !< vector of auxname
    integer(I4B), pointer :: naux => null()                                      !< number of auxiliary variables
    integer(I4B), pointer :: nbound => null()                                    !< number of boundaries for current stress period
    integer(I4B), dimension(:), pointer, contiguous :: nodelist => null()        !< vector of reduced node numbers
    real(DP), dimension(:), pointer, contiguous :: flow => null()                !< calculated flow
    real(DP), dimension(:,:), pointer, contiguous :: auxvar => null()            !< auxiliary variable array
    
  contains
  
    procedure :: initialize
    procedure :: set_name
    procedure :: set_auxname
    procedure :: set_pointers
    procedure :: copy_values
    procedure :: get_flow
    procedure :: da
    
  end type PackageBudgetType  
  
  contains
  
  subroutine initialize(this, mempath)
    class(PackageBudgetType) :: this
    character(len=*), intent(in) :: mempath
    this%memoryPath = mempath
    !
    ! -- allocate scalars
    call mem_allocate(this%name, LENPACKAGENAME, 'NAME', mempath)
    call mem_allocate(this%budtxt, LENPACKAGENAME, 'BUDTXT', mempath)
    call mem_allocate(this%naux, 'NAUX', mempath)
    call mem_allocate(this%auxname, LENAUXNAME, 0, 'AUXNAME', this%memoryPath)
    call mem_allocate(this%nbound, 'NBOUND', mempath)
    call mem_allocate(this%nodelist, 0, 'NODELIST', mempath)
    call mem_allocate(this%flow, 0, 'FLOW', mempath)
    call mem_allocate(this%auxvar, 0, 0, 'AUXVAR', mempath)
    !
    ! -- initialize
    this%name = ''
    this%budtxt = ''
    this%naux = 0
    this%nbound = 0
    return
  end subroutine initialize
  
  subroutine set_name(this, name)
    class(PackageBudgetType) :: this
    character(len=LENPACKAGENAME) :: name
    this%name = name
    return
  end subroutine set_name
  
  subroutine set_auxname(this, naux, auxname)
    class(PackageBudgetType) :: this
    integer(I4B), intent(in) :: naux
    character(len=LENAUXNAME), contiguous, dimension(:), intent(in) :: auxname
    this%naux = naux
    call mem_reallocate(this%auxname, LENAUXNAME, naux, 'AUXNAME', this%memoryPath)
    this%auxname(:) = auxname(:)
    return
  end subroutine set_auxname
  
  subroutine set_pointers(this, budtxt, flowvarname, mem_path_target)
    class(PackageBudgetType) :: this
    character(len=*), intent(in) :: budtxt            !< name of budget term (CHD, RCH, EVT, etc.)
    character(len=*), intent(in) :: flowvarname       !< name of variable storing flow (SIMVALS, SIMTOMVR)
    character(len=*), intent(in) :: mem_path_target   !< path where target variable is stored
    !
    ! -- Reassign pointers to variables in the flow model
    this%budtxt = budtxt
    call mem_reassignptr(this%nbound, 'NBOUND', this%memoryPath, &
                         'NBOUND', mem_path_target)
    call mem_reassignptr(this%nodelist, 'NODELIST', this%memoryPath, &
                         'NODELIST', mem_path_target)
    call mem_reassignptr(this%flow, 'FLOW', this%memoryPath, &
                         flowvarname, mem_path_target)
    call mem_reassignptr(this%auxvar, 'AUXVAR', this%memoryPath, &
                         'AUXVAR', mem_path_target)
    return    
  end subroutine set_pointers

  subroutine copy_values(this, name, budtxt, auxname, nbound, naux, &
                         nodelist, flow, auxvar)
    class(PackageBudgetType) :: this
    character(len=LENPACKAGENAME), intent(in) :: name
    character(len=LENPACKAGENAME), intent(in) :: budtxt
    character(len=LENAUXNAME), contiguous, dimension(:), intent(in) :: auxname
    integer(I4B), intent(in) :: nbound
    integer(I4B), intent(in) :: naux
    integer(I4B), dimension(:), contiguous, intent(in) :: nodelist
    real(DP), dimension(:), contiguous, intent(in) :: flow
    real(DP), dimension(:,:), contiguous, intent(in) :: auxvar
    integer(I4B) :: i
    !
    ! -- Assign variables
    this%name = name
    this%budtxt = budtxt
    this%naux = naux
    this%nbound = nbound
    !
    ! -- Lists are not large enough (maxbound is not known), so need to
    !    reallocate based on size in binary budget file.
    if (size(this%nodelist) < nbound) then
      call mem_reallocate(this%nodelist, nbound, 'NODELIST', this%memoryPath)
      call mem_reallocate(this%flow, nbound, 'FLOW', this%memoryPath)
      call mem_reallocate(this%auxvar, naux, nbound, 'AUXVAR', this%memoryPath)
    endif
    !
    ! -- Copy values into member variables
    do i = 1, nbound
      this%nodelist(i) = nodelist(i)
      this%flow(i) = flow(i)
      this%auxvar(:, i) = auxvar(:, i)
    end do
  end subroutine copy_values
  
  function get_flow(this, i) result(flow)
    class(PackageBudgetType) :: this
    integer(I4B), intent(in) :: i
    real(DP) :: flow
    flow = this%flow(i)
    return
  end function get_flow
  
  subroutine da(this)
    class(PackageBudgetType) :: this
    call mem_deallocate(this%name, 'NAME', this%memoryPath)
    call mem_deallocate(this%budtxt, 'BUDTXT', this%memoryPath)
    call mem_deallocate(this%naux)
    call mem_deallocate(this%auxname, 'AUXNAME', this%memoryPath)
    call mem_deallocate(this%nbound)
    call mem_deallocate(this%nodelist, 'NODELIST', this%memoryPath)
    call mem_deallocate(this%flow, 'FLOW', this%memoryPath)
    call mem_deallocate(this%auxvar, 'AUXVAR', this%memoryPath)
    return
  end subroutine da
  
  
  
end module PackageBudgetModule