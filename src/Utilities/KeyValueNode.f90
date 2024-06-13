module KeyValueNodeModule
  use ConstantsModule, only: LENMEMADDRESS
  implicit none
  private

  public :: KeyValueNodeType

  !> @brief A key-value pair node
  !!
  !<
  type KeyValueNodeType
    type(KeyValueNodeType), pointer :: next => null() !< the next node
    class(*), pointer :: value => null() !< the value
    ! Due to a bug in the gfortran compiler we can't use a deferred length character variable
    ! https://gcc.gnu.org/bugzilla/show_bug.cgi?id=106317
    character(len=LENMEMADDRESS), pointer :: key => null() !< the key
  end type KeyValueNodeType
end module
