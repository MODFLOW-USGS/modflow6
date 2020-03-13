module SimVariablesModule

  use KindModule, only: I4B
  use ConstantsModule, only: MAXCHARLEN, IUSTART
  use GLOBAL, only: IOUT
  use ListModule, only: ListType

  private
  public :: ModelPacks, SimMovers, iout, iunext

  integer(I4B) :: iunext = IUSTART
  type(ListType), pointer :: SimMovers => null()
  type(ListType) :: ModelPacks

end module SimVariablesModule
