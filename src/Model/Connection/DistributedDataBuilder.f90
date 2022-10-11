module DistDataBuilderModule
  use KindModule, only: I4B
  use VectorIntModule
  use BaseSolutionModule, only: BaseSolutionType
  use NumericalSolutionModule, only: NumericalSolutionType
  use SpatialModelConnectionModule, only: SpatialModelConnectionType, &
                                          GetSpatialModelConnectionFromList
  use DistributedModelModule, only: DistributedModelType, get_dist_model
  use DistributedExchangeModule, only: DistributedExchangeType, get_dist_exg
  use ListModule
  implicit none
  private

  public :: DistDataBuilderType

  type :: DistDataBuilderType
    contains
    procedure :: process_solution
    procedure, private :: process_exchanges
  end type

  contains

  subroutine process_solution(this, sol)
    class(DistDataBuilderType) :: this
    class(BaseSolutionType), pointer :: sol

    select type(sol)
    class is (NumericalSolutionType)
      call this%process_exchanges(sol%id, sol%exchangelist)
    end select

  end subroutine process_solution

  subroutine process_exchanges(this, sol_id, exchanges)
    class(DistDataBuilderType) :: this
    integer(I4B) :: sol_id
    type(ListType), pointer :: exchanges
    ! local
    integer(I4B) :: ix, im, ihx
    integer(I4B) :: model_id, exg_id
    class(SpatialModelConnectionType), pointer :: mod_conn
    type(VectorInt) :: halo_models_ns !< halo models for numerical solution
    type(VectorInt) :: halo_exg_ns !< halo exchanges for numerical solution
    class(DistributedModelType), pointer :: dist_model
    class(DistributedExchangeType), pointer :: dist_exg
    
    ! aggregate solution halo
    do ix = 1, exchanges%Count()
      mod_conn => GetSpatialModelConnectionFromList(exchanges, ix)
      if (.not. associated(mod_conn)) cycle

      if (ix == 1) then
        halo_models_ns = mod_conn%haloModels
        halo_exg_ns = mod_conn%haloExchanges
      else
        ! extend
        do im = 1, mod_conn%haloModels%size
          model_id = mod_conn%haloModels%at(im)
          if (.not. halo_models_ns%contains(model_id)) then
            call halo_models_ns%push_back(model_id)
          end if                    
        end do
        do ihx = 1, mod_conn%haloExchanges%size
          exg_id = mod_conn%haloExchanges%at(ihx)
          if (.not. halo_exg_ns%contains(exg_id)) then
            call halo_exg_ns%push_back(exg_id)
          end if
        end do
      end if
    end do

    ! prepare distributed models/exchanges
    do im = 1, halo_models_ns%size
      model_id = halo_models_ns%at(im)
      dist_model => get_dist_model(model_id)
      call dist_model%init_connectivity()
    end do
    do ihx = 1, halo_exg_ns%size
      exg_id = halo_exg_ns%at(ihx)
      dist_exg => get_dist_exg(exg_id)
      call dist_exg%init_connectivity()
    end do

    ! aggregate distributed data from halo models and exchanges
    ! (i.e. model_conn%haloModels, model_conn%haloExchanges)
    ! for initialization we need (full*) coverage of:
    ! = model=
    ! nodes*
    ! con%ia*,ja*,jas,ihc,hwva,cl1,cl2,anglex
    ! dis%xc,dis%yc,dis%xorigin,dis%yorigin,dis%angrot
    !
    ! = exg =
    ! nexg*      
    ! nodem1*,nodem2*
    ! naux,auxname_cst,auxvar
    ! ihc,cl1,cl2,hwva

    ! add interface model variables
    !model_conn%interfaceMap

  end subroutine process_exchanges

end module DistDataBuilderModule