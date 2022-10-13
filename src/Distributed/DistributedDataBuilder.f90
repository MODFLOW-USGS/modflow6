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
  public :: connect_halo

  type :: DistDataBuilderType
    contains
    procedure :: connect_halo
  end type

  contains

  subroutine connect_halo(this, num_sol)
    class(DistDataBuilderType) :: this
    class(NumericalSolutionType), pointer :: num_sol
    ! local
    integer(I4B) :: ix, im, ihx
    integer(I4B) :: model_id, exg_id
    class(SpatialModelConnectionType), pointer :: mod_conn
    type(VectorInt) :: halo_models_ns !< halo models for numerical solution
    type(VectorInt) :: halo_exg_ns !< halo exchanges for numerical solution
    class(DistributedModelType), pointer :: dist_model
    class(DistributedExchangeType), pointer :: dist_exg
    
    ! aggregate solution halo
    call halo_models_ns%init()
    call halo_exg_ns%init()

    do ix = 1, num_sol%exchangelist%Count()
      mod_conn => GetSpatialModelConnectionFromList(num_sol%exchangelist, ix)
      if (.not. associated(mod_conn)) cycle

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

    call halo_models_ns%destroy()
    call halo_exg_ns%destroy()

  end subroutine connect_halo

end module DistDataBuilderModule