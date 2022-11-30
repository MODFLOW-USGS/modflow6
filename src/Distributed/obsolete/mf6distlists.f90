module DistListsModule
  use ListModule, only: ListType
  implicit none
  private

  public :: distmodellist, distexchangelist

  ! -- list of all distributed models
  type(ListType) :: distmodellist

  ! -- list of all distributed exchanges
  type(ListType) :: distexchangelist

end module