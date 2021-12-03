module PackageBudgetModule
  
  use KindModule
  use ConstantsModule, only: LENPACKAGENAME, LENAUXNAME, LENMEMPATH
  use MemoryManagerModule, only: mem_allocate

  implicit none
  
  private
  public :: PackageBudgetType
  
  type :: PackageBudgetType
    
    character(len=LENMEMPATH) :: memoryPath = ''                                 !< the location in the memory manager where the variables are stored
    character(len=LENPACKAGENAME) :: name = ''                                   !< name of the package
    character(len=LENPACKAGENAME) :: budtxt = ''                                 !< type of flow (CHD, RCH, RCHA, ...)
    character(len=LENAUXNAME), allocatable, dimension(:) :: auxname              !< auxiliary variable names
    integer(I4B) :: naux                                                         !< number of auxiliary variables
    integer(I4B), pointer :: nbound => null()                                    !< number of boundaries for current stress period
    integer(I4B), dimension(:), pointer, contiguous :: nodelist => null()        !< vector of reduced node numbers
    real(DP), dimension(:,:), pointer, contiguous :: bound => null()             !< array of package specific boundary numbers
    real(DP), dimension(:,:), pointer, contiguous :: auxvar => null()            !< auxiliary variable array
    real(DP), dimension(:), pointer, contiguous :: flow => null()                !< calculated flow
    
  contains
  
    procedure :: initialize
    procedure :: set_name
    procedure :: set_auxname
    procedure :: set_pointers
    procedure :: copy_values
    procedure :: get_flow
    
  end type PackageBudgetType  
  
  contains
  
  subroutine initialize(this, mempath)
    class(PackageBudgetType) :: this
    character(len=*), intent(in) :: mempath
    this%memoryPath = mempath
    !
    ! -- allocate scalars
    !call mem_allocate(this%name, LENPACKAGENAME, 'NAME', mempath)
    !call mem_allocate(this%budtxt, LENPACKAGENAME, 'BUDTXT', mempath)
    !call mem_allocate(this%naux, 'NAUX', mempath)
    !call mem_allocate(this%nbound, 'NBOUND', mempath)
    !
    ! -- initialize
    !this%name = ''
    !this%budtxt = ''
    !this%naux = 0
    !this%nbound = 0
    return
  end subroutine initialize
  
  subroutine set_name(this, name)
    class(PackageBudgetType) :: this
    character(len=LENPACKAGENAME) :: name
    this%name = name
  end subroutine set_name
  
  subroutine set_auxname(this, naux, auxname)
    class(PackageBudgetType) :: this
    integer(I4B), intent(in) :: naux
    character(len=LENAUXNAME), contiguous, dimension(:), intent(in) :: auxname
    this%naux = naux
    if (naux > 0) then
      if (.not. allocated(this%auxname)) then
        allocate(this%auxname(this%naux))
      end if
      this%auxname(:) = auxname(:)
    end if
  end subroutine set_auxname
  
  subroutine set_pointers(this, name, budtxt, auxname, nbound, naux, nodelist, &
                           flow, auxvar)
    class(PackageBudgetType) :: this
    character(len=LENPACKAGENAME) :: name
    character(len=LENPACKAGENAME) :: budtxt
    character(len=LENAUXNAME), contiguous, dimension(:), intent(in) :: auxname 
    integer(I4B), target, intent(in) :: nbound
    integer(I4B), target, intent(in) :: naux
    integer(I4B), dimension(:), target, contiguous, intent(in) :: nodelist
    real(DP), dimension(:), target, contiguous, intent(in):: flow
    real(DP), dimension(:,:), target, contiguous, intent(in) :: auxvar
    this%name = name
    this%budtxt = budtxt
    this%naux = naux
    if (naux > 0) then
      if (.not. allocated(this%auxname)) then
        allocate(this%auxname(naux))
      end if
      this%auxname(:) = auxname(:)
    end if
    this%nbound => nbound
    this%nodelist => nodelist
    this%flow => flow
    this%auxvar => auxvar
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
    this%name = name
    this%budtxt = budtxt
    if (.not. allocated(this%auxname)) allocate(this%auxname(naux))
    this%auxname(:) = auxname(:)
    this%naux = naux
    if (.not. associated(this%nbound)) allocate(this%nbound)
    this%nbound = nbound
    if (associated(this%nodelist)) then
      if (size(this%nodelist) < nbound) then
        deallocate(this%nodelist)
        deallocate(this%flow)
        deallocate(this%auxvar)
        allocate(this%nodelist(nbound))
        allocate(this%flow(nbound))
        allocate(this%auxvar(naux, nbound))
      endif
    else
        allocate(this%nodelist(nbound))
        allocate(this%flow(nbound))
        allocate(this%auxvar(naux, nbound))
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
  
end module PackageBudgetModule