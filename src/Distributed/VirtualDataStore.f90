module VirtualDataStoreModule  
  use KindModule, only: I4B
  use VirtualSolutionModule
  use VirtualDataContainerModule
  use RouterBaseModule
  use RouterFactoryModule, only: create_router  
  implicit none
  private

  type, public :: VirtualDataStoreType
    integer(I4B) :: nr_solutions
    integer(I4B), dimension(:), allocatable :: solution_ids
    class(VirtualSolutionType), dimension(:), pointer :: virtual_solutions
    class(RouterBaseType), pointer :: router
  contains
    procedure :: init => vds_init
    procedure :: synchronize => vds_synchronize
    procedure :: destroy

    ! private
    procedure, private :: prepare_solution
    procedure, private :: sync_solution
    procedure, private :: link
    procedure, private :: count_nr_solutions
  end type VirtualDataStoreType

contains

  subroutine vds_init(this, sim_mode)
    class(VirtualDataStoreType) :: this
    character(len=*) :: sim_mode
    ! local    
    integer(I4B) :: nr_sol

    nr_sol = this%count_nr_solutions()
    allocate(this%virtual_solutions(nr_sol))
    allocate(this%solution_ids(nr_sol))
    
    this%router => create_router(sim_mode)
    
  end subroutine vds_init

  subroutine vds_synchronize(this, stage)
    class(VirtualDataStoreType) :: this
    integer(I4B) :: stage
    ! local
    integer(I4B) :: i
    
    do i = 1, this%nr_solutions
      call this%prepare_solution(this%virtual_solutions(i), stage)
      call this%sync_solution(this%virtual_solutions(i), stage)
    end do

  end subroutine vds_synchronize

  subroutine prepare_solution(this, virtual_sol, stage)
    class(VirtualDataStoreType) :: this
    type(VirtualSolutionType) :: virtual_sol
    integer(I4B) :: stage
    ! local
    integer(I4B) :: i
    class(*), pointer :: vdc

    do i = 1, virtual_sol%models%Count()
      vdc => virtual_sol%models%GetItem(i)
      select type (vdc)
      class is (VirtualDataContainerType)
        call vdc%prepare_stage(stage)
      end select
    end do

  end subroutine prepare_solution

  subroutine sync_solution(this, virtual_sol, stage)
    class(VirtualDataStoreType) :: this
    type(VirtualSolutionType) :: virtual_sol
    integer(I4B) :: stage

    ! local objects are linked
    call this%link(virtual_sol, stage)

    ! remote objects are routed
    call this%router%route(virtual_sol, stage)

  end subroutine sync_solution

  subroutine link(this, virtual_sol, stage)
    class(VirtualDataStoreType) :: this
    type(VirtualSolutionType) :: virtual_sol
    integer(I4B) :: stage

  end subroutine link

  function count_nr_solutions(this) result(count)
    use ListsModule, only: basesolutionlist
    use NumericalSolutionModule, only: NumericalSolutionType   
    class(VirtualDataStoreType) :: this
    integer(I4B) :: count
    ! local    
    integer(I4B) :: isol
    class(*), pointer :: sol

    ! count nr. of numerical solutions
    count = 0
    do isol = 1, basesolutionlist%Count()
      sol => basesolutionlist%GetItem(isol)
      select type (sol)
        class is (NumericalSolutionType)
        count = count + 1
      end select
    end do

  end function count_nr_solutions

  subroutine destroy(this)
    class(VirtualDataStoreType) :: this

    deallocate(this%virtual_solutions)

  end subroutine destroy

end module VirtualDataStoreModule