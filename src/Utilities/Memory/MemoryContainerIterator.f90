module MemoryContainerIteratorModule
  use KindModule, only: I4B, LGP
  use MemoryTypeModule, only: MemoryType
  use IteratorModule, only: IteratorType

  implicit none
  private

  public :: MemoryContainerIteratorType

  !> @brief An iterator used to iterate through a MemoryContainer
  !!
  !<
  type :: MemoryContainerIteratorType
    private
    class(IteratorType), allocatable :: container_iterator !< the current iterator to the underlying container
  contains
    procedure :: has_next
    procedure :: next
    procedure :: value
  end type

  interface MemoryContainerIteratorType
    module procedure constructor
  end interface MemoryContainerIteratorType

contains
  !> @brief Constructor to create a MemoryContainerIterator
  !!
  !<
  function constructor(container_iterator) result(iterator)
    class(IteratorType), allocatable :: container_iterator
    type(MemoryContainerIteratorType) :: iterator

    call move_alloc(container_iterator, iterator%container_iterator)

  end function constructor

  !> @brief Indicates if there is a next node in the iteration chain
  !!
  !<
  function has_next(this) result(res)
    class(MemoryContainerIteratorType) :: this
    logical(LGP) :: res

    res = this%container_iterator%has_next()
  end function

  !> @brief Increment the iterator to the next node
  !!
  !<
  subroutine next(this)
    class(MemoryContainerIteratorType) :: this

    call this%container_iterator%next()
  end subroutine

  !> @brief Get the value the iterator is pointing to
  !!
  !<
  function value(this) result(res)
    class(MemoryContainerIteratorType), target :: this
    type(MemoryType), pointer :: res
    ! -- local
    class(*), pointer :: obj !< void pointer to MemoryType

    obj => this%container_iterator%value()

    select type (obj)
    type is (MemoryType)
      res => obj
    class default
      res => null()
    end select

  end function

end module MemoryContainerIteratorModule
