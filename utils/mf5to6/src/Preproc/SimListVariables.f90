module SimListVariablesModule
  use ListModule, only: ListType

  implicit none
  
  private
  public :: ModelPacks, SimMovers

  type(ListType), pointer :: SimMovers => null()
  type(ListType) :: ModelPacks

end module SimListVariablesModule
