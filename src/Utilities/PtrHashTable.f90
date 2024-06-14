module PtrHashTableModule
  use KeyValueListModule, only: KeyValueListType
  use KindModule, only: DP, I4B, LGP
  use IteratorModule, only: IteratorType
  use PtrHashTableIteratorModule, only: PtrHashTableIteratorType
  use SimModule, only: store_warning

  implicit none
  private

  public :: PtrHashTableType

  integer(I4B), parameter, private :: HASH_SIZE = 4993
  integer(I4B), parameter, private :: MULTIPLIER = 31
  integer(I4B), parameter, public :: BUCKET_SIZE = 1000

  !> @brief HashTable that stores void pointer items.
  !!
  !<
  type PtrHashTableType
    private
    type(KeyValueListType) :: buckets(BUCKET_SIZE) !< the HashTable buckets
    integer(I4B) :: cnt = 0 !< the number of items in the HashTable
  contains
    procedure :: iterator
    procedure :: add
    procedure :: get
    procedure :: contains
    procedure :: count
    procedure :: clear
  end type PtrHashTableType

contains

  !> @brief An iterator used to iterate through the HashTable
  !!
  !<
  function iterator(this) result(itr)
    class(PtrHashTableType), target :: this
    class(IteratorType), allocatable :: itr

    itr = PtrHashTableIteratorType(this%buckets)
  end function

  !> @brief Add a void pointer to the HashTable
  !!
  !<
  subroutine add(this, key, val)
    class(PtrHashTableType), target :: this
    character(len=*), intent(in) :: key !< key of the item to be added
    class(*), pointer, intent(in) :: val !< value to be added
    ! -- local
    type(KeyValueListType), pointer :: bucket !< bucket the key points to
    integer(I4B) :: hash !< hashed value of the key

    if (this%contains(key)) then
      call store_warning( &
        "Already existing variable being added to the HashTable -"//key)
    end if

    hash = compute_hash(key)

    bucket => this%buckets(hash)
    call bucket%add(key, val)
    this%cnt = this%cnt + 1
  end subroutine add

  !> @brief Get a void pointer from the HashTable using a key
  !!
  !<
  function get(this, key) result(val)
    class(PtrHashTableType), target :: this
    character(len=*), intent(in) :: key !< key of the item to retrieve
    class(*), pointer :: val !< item associated with the key
    ! -- local
    type(KeyValueListType), pointer :: bucket !< bucket the key points to
    integer(I4B) :: hash !< hashed value of the key

    hash = compute_hash(key)

    bucket => this%buckets(hash)
    val => bucket%get(key)

  end function

  !> @brief Boolean indicating if an item exists in the hashtable
  !!
  !<
  function contains(this, key) result(res)
    class(PtrHashTableType), target :: this
    character(len=*), intent(in) :: key !< key of the item to retrieve
    logical :: res !< item found

    ! -- local
    res = associated(this%get(key))

  end function

  !> @brief The nummer of items in the HashTable
  !!
  !<
  function count(this) result(val)
    class(PtrHashTableType) :: this
    integer(I4B) :: val

    val = this%cnt
  end function

  !> @brief Clears the HashTable
  !!
  !! Loops over all the buckets and clears them.
  !<
  subroutine clear(this)
    class(PtrHashTableType), target :: this
    ! -- local
    type(KeyValueListType), pointer :: bucket
    integer(I4B) :: bucket_index

    do bucket_index = 1, BUCKET_SIZE
      bucket => this%buckets(bucket_index)
      call bucket%clear()
    end do

    this%cnt = 0
  end subroutine

  !> @brief Compute the hash of a key
  !!
  !! The hash produced will be in the interval 1 - BUCKET_SIZE
  !<
  function compute_hash(key) result(hash)
    character(len=*), intent(in) :: key !< the key
    integer(I4B) :: hash !< the hash
    ! -- local
    integer(I4B) :: i !< character index of the key

    hash = 0
    do i = 1, len_trim(key)
      hash = modulo(MULTIPLIER * hash + ichar(key(i:i)), HASH_SIZE)
    end do
    hash = 1 + modulo(hash, BUCKET_SIZE)

  end function compute_hash

end module PtrHashTableModule
