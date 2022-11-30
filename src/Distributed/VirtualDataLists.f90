module VirtualDataListsModule
  use KindModule, only: I4B
  use ListModule, only: ListType
  use VirtualDataContainerModule, only: VirtualDataContainerType
  implicit none
  private

  public :: virtual_model_list
  public :: virtual_exchange_list
  public :: get_virtual_model
  public :: get_virtual_exchange

  type(ListType) :: virtual_model_list

  type(ListType) :: virtual_exchange_list

contains

  function get_virtual_model(model_id) result(virtual_model)
    integer(I4B) :: model_id
    class(VirtualDataContainerType), pointer :: virtual_model
    ! local
    integer(I4B) :: i
    class(*), pointer :: vm

    virtual_model => null()
    do i = 1, virtual_model_list%Count()
      vm => virtual_model_list%GetItem(i)
      select type (vm)
        class is (VirtualDataContainerType)
          if (vm%id == model_id) then
            virtual_model => vm
            return
          end if
      end select
    end do

  end function get_virtual_model

  function get_virtual_exchange(exg_id) result(virtual_exg)
    integer(I4B) :: exg_id
    class(VirtualDataContainerType), pointer :: virtual_exg
    ! local
    integer(I4B) :: i
    class(*), pointer :: ve

    virtual_exg => null()
    do i = 1, virtual_exchange_list%Count()
      ve => virtual_exchange_list%GetItem(i)
      select type (ve)
        class is (VirtualDataContainerType)
          if (ve%id == exg_id) then
            virtual_exg => ve
            return
          end if
      end select
    end do

  end function get_virtual_exchange

end module VirtualDataListsModule