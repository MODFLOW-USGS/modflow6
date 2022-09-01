module InterfaceMapModule
  use KindModule, only: I4B
  use ConstantsModule, only: LENMODELNAME, LENEXCHANGENAME

  implicit none
  private

  type, public :: IndexMapType
    integer(I4B), dimension(:), pointer :: src_idx
    integer(I4B), dimension(:), pointer :: tgt_idx
  end type IndexMapType

  type, public :: IndexMapSgnType
    integer(I4B), dimension(:), pointer :: src_idx
    integer(I4B), dimension(:), pointer :: tgt_idx
    integer(I4B), dimension(:), pointer :: sign
  end type IndexMapSgnType

  type, public :: InterfaceMapType
    integer(I4B) :: nr_models
    character(len=LENMODELNAME), dimension(:), pointer :: model_names
    integer(I4B) :: nr_exchanges
    integer(I4B) :: prim_exg_idx
    character(len=LENEXCHANGENAME), dimension(:), pointer :: exchange_names
    type(IndexMapType), dimension(:), pointer :: node_map
    type(IndexMapType), dimension(:), pointer :: connection_map
    type(IndexMapSgnType), dimension(:), pointer :: exchange_map
  end type InterfaceMapType

end module InterfaceMapModule
