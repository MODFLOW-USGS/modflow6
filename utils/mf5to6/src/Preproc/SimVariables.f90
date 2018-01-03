module SimVariablesModule

  use ConstantsModule, only: MAXCHARLEN
  use GLOBAL, only: IOUT
  use ListModule, only: ListType

  private
  public :: ModelPacks, SimMovers, iout

  type(ListType), pointer :: SimMovers
  type(ListType) :: ModelPacks

end module SimVariablesModule
