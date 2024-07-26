module PtrHashTableIteratorModule
  use KeyValueListModule, only: KeyValueListType
  use KindModule, only: I4B
  use IteratorModule, only: IteratorType

  implicit none
  private

  public :: PtrHashTableIteratorType

  !> @brief An iterator used to iterate through a PtrHashTable
  !!
  !<
  type, extends(IteratorType) :: PtrHashTableIteratorType
    type(KeyValueListType), pointer :: buckets(:) => null() !< the buckets of the PtrHashTable to iterate through
    class(IteratorType), allocatable :: current_bucket_iterator !< the iterator of the bucket to which the current iterator belongs
    integer(I4B) :: curent_bucket_index = 1 !< the bucket in which the current iterator belongs
  contains
    procedure :: has_next
    procedure :: next
    procedure :: value
  end type

  interface PtrHashTableIteratorType
    module procedure Constructor
  end interface PtrHashTableIteratorType

contains
  !> @brief Constructor to create a PtrHashTableIterator
  !!
  !<
  function constructor(buckets) Result(iterator)
    type(KeyValueListType), target, dimension(:), intent(in) :: buckets
    type(PtrHashTableIteratorType) :: iterator
    ! -- local
    type(KeyValueListType), pointer :: first_bucket

    iterator%buckets => buckets

    first_bucket => iterator%buckets(1)
    allocate (iterator%current_bucket_iterator, source=first_bucket%iterator())

  end function constructor

  !> @brief Indicates if there is a next node in the iteration chain
  !!
  !<
  function has_next(this) result(res)
    class(PtrHashTableIteratorType) :: this
    type(logical) :: res
    ! -- local
    type(KeyValueListType), pointer :: bucket
    integer(I4B) :: bucket_index

    !
    ! -- check if there are more values in the current bucket
    if (this%current_bucket_iterator%has_next()) then
      res = .true.
      return
    end if

    !
    ! -- check if there is a next bucket which has values.
    ! -- if so then there is more to iterate through.
    do bucket_index = this%curent_bucket_index + 1, size(this%buckets)
      bucket => this%buckets(bucket_index)
      if (bucket%count() > 0) then
        res = .true.
        return
      end if
    end do

    res = .false.

  end function

  !> @brief Increment the iterator to the next node
  !!
  !<
  subroutine next(this)
    class(PtrHashTableIteratorType) :: this
    ! -- local
    type(KeyValueListType), pointer :: bucket !< a bucket
    integer(I4B) :: bucket_index

    !
    ! -- If the current bucket doesn't have anymore values continue to the next
    if (.not. this%current_bucket_iterator%has_next()) then
      do bucket_index = this%curent_bucket_index + 1, size(this%buckets)
        bucket => this%buckets(bucket_index)
        if (bucket%count() > 0) then
          deallocate (this%current_bucket_iterator)
          allocate (this%current_bucket_iterator, source=bucket%iterator())
          this%curent_bucket_index = bucket_index
          exit
        end if
      end do
    end if

    call this%current_bucket_iterator%next()

  end subroutine

  !> @brief Get the value the iterator is pointing to
  !!
  !<
  function value(this) result(res)
    class(PtrHashTableIteratorType) :: this
    class(*), pointer :: res

    res => this%current_bucket_iterator%value()

  end function

end module PtrHashTableIteratorModule
