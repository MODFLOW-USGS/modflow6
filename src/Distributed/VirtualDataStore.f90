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
  end type VirtualDataStoreType

contains

  subroutine vds_init(this, nr_sol, routing_mode)
    class(VirtualDataStoreType) :: this
    integer(I4B) :: nr_sol
    character(len=*) :: routing_mode

    allocate(this%virtual_solutions(nr_sol))
    this%router => create_router(routing_mode)
    
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

  subroutine destroy(this)
    class(VirtualDataStoreType) :: this

    deallocate(this%virtual_solutions)

  end subroutine destroy

end module VirtualDataStoreModule