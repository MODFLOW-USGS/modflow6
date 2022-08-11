module InterfaceMapModule
  use KindModule, only: I4B

  implicit none
  private

  type, public :: IndexMap
    integer(I4B), dimension(:), pointer :: src_idx
    integer(I4B), dimension(:), pointer :: tgt_idx
  end type IndexMap

  type, public :: InterfaceMap
    integer(I4B) :: nr_models
    integer(I4B), dimension(:), pointer :: model_ids
    type(IndexMap), dimension(:), pointer :: node_map
    type(IndexMap), dimension(:), pointer :: connection_map
  end type InterfaceMap

end module InterfaceMapModule