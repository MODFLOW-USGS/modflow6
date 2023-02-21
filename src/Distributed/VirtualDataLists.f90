module VirtualDataListsModule
  use ListModule, only: ListType
  implicit none
  private

  public :: virtual_model_list
  public :: virtual_exchange_list

  type(ListType) :: virtual_model_list

  type(ListType) :: virtual_exchange_list

end module VirtualDataListsModule
