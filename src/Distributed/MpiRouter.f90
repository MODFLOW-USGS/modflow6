module MpiRouterModule
  use RouterBaseModule
  use KindModule, only: I4B
  use SimVariablesModule, only: MF6_COMM_WORLD, proc_id
  use SimStagesModule, only: STG_TO_STR
  use VirtualDataListsModule, only: virtual_model_list, &
                                    virtual_exchange_list
  use VirtualDataContainerModule, only: VirtualDataContainerType, &
                                        get_vdc_from_list
  use VirtualSolutionModule
  use mpi
  implicit none
  private

  public :: create_mpi_router

  type, public, extends(RouterBaseType) :: MpiRouterType
    integer(I4B), dimension(:), pointer :: model_proc_ids
    integer(I4B), dimension(:), pointer :: exg_proc_ids
  contains
    procedure :: initialize => mr_initialize
    procedure :: route_all => mr_route_all
    procedure :: route_sln => mr_route_sln
    procedure :: destroy => mr_destroy
  end type MpiRouterType

contains

  !> Factory method to create MPI router
  !<
  function create_mpi_router() result(router)
    class(RouterBaseType), pointer :: router
    ! local
    class(MpiRouterType), pointer :: mpi_router

    allocate (mpi_router)
    router => mpi_router

  end function create_mpi_router

  subroutine mr_initialize(this)
    class(MpiRouterType) :: this
    ! local
    integer(I4B) :: i, ierr, nr_models
    class(VirtualDataContainerType), pointer :: vdc    

    nr_models = virtual_model_list%Count()
    allocate (this%model_proc_ids(nr_models))


    do i = 1, nr_models
      vdc => get_vdc_from_list(virtual_model_list, i)
      if (vdc%is_remote) then
        this%model_proc_ids(i) = 0
      else
        this%model_proc_ids(i) = proc_id
      end if
    end do

    call mpi_allreduce(MPI_IN_PLACE, this%model_proc_ids, nr_models, &
                       MPI_INTEGER, MPI_SUM, MF6_COMM_WORLD, ierr)

    

  end subroutine mr_initialize

  subroutine mr_route_all(this, stage)
    class(MpiRouterType) :: this
    integer(I4B) :: stage

    !write(*,*) 'mpi route all for stage ', STG_TO_STR(stage)

  end subroutine mr_route_all

  subroutine mr_route_sln(this, virtual_sol, stage)
    class(MpiRouterType) :: this
    type(VirtualSolutionType) :: virtual_sol
    integer(I4B) :: stage

    !write(*,*) 'mpi route solution ', virtual_sol%solution_id, ' for stage ', STG_TO_STR(stage)

    ! snd+rcv header

    ! snd+rcv maps

    ! snd+rcv data

    ! async. wait

  end subroutine mr_route_sln

  subroutine mr_destroy(this)
    class(MpiRouterType) :: this

    deallocate (this%model_proc_ids)

  end subroutine mr_destroy

end module MpiRouterModule