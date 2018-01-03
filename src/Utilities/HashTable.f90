module HashTableModule
  ! HashTableType implements a hash table for storing integers,
  ! for use as an index for an array that could contain 
  ! any data type.
  
  use KindModule, only: DP, I4B

  private
  public :: HashTableType

  integer(I4B), parameter :: ARRAYDIMDEFAULT = 100

  type HashTableNodeType
    ! -- Private members
    integer(I4B),                          private :: indx
    character(len=:), allocatable,    private :: key
    type(HashTableNodeType), pointer, private :: NextNode => null()
  contains
    ! -- Private procedures
    procedure, private :: put_node
    procedure, private :: get_node
    procedure, private :: free_node
  end type HashTableNodeType

  type HashTableType
    ! -- Private members
    integer(I4B), private :: lenarray = 0
    logical, private :: is_init = .false.
    type(HashTableNodeType), allocatable, dimension(:), private :: array
  contains
    ! -- Public procedures
    procedure, public :: InitHash
    procedure, public :: PutHash
    procedure, public :: GetHash
    procedure, public :: FreeHash
  end type HashTableType

contains

  ! Private type-bound procedures for HashTableNodeType

  recursive subroutine put_node(this, key, indx)
    implicit none
    ! -- dummy
    class(HashTableNodeType), intent(inout) :: this
    character(len=*),         intent(in)    :: key
    integer(I4B),                  intent(in)    :: indx
    !
    if (allocated(this%key)) then
      if (this%key /= key) then
        if (.not. associated(this%NextNode)) then
          allocate(this%NextNode)
        endif
        call put_node(this%NextNode, key, indx)
      else
        this%indx = indx
      end if
    else
      this%key = key
      this%indx = indx
    end if
    !
    return
  end subroutine put_node

  recursive subroutine get_node(this, key, indx)
    implicit none
    ! -- dummy
    class(HashTableNodeType), intent(in)  :: this
    character(len=*),         intent(in)  :: key
    integer(I4B),                  intent(out) :: indx
    !
    if (allocated(this%key) .and. (this%key == key)) then
      indx = this%indx
    else if(associated(this%NextNode)) then
      call get_node(this%NextNode, key, indx)
    else
      return
    end if
    return
    !
  end subroutine get_node

  recursive subroutine free_node(this)
    implicit none
    ! -- dummy
    class(HashTableNodeType), intent(inout) :: this
    !
    if (associated(this%NextNode)) then
       call free_node(this%NextNode)
       deallocate(this%NextNode)
    end if
    this%NextNode => null()
    if (allocated(this%key)) deallocate(this%key)
    return
    !
  end subroutine free_node
  
  ! Public type-bound procedures for HashTableType
  
  subroutine InitHash(this, lenarr)
    implicit none
    ! -- dummy
    class(HashTableType), intent(inout) :: this
    integer(I4B),    optional, intent(in)    :: lenarr
    !
    if (allocated(this%array)) deallocate(this%array)
    if (present(lenarr)) then
      allocate(this%array(0:lenarr-1))
      this%lenarray = lenarr
    else
      allocate(this%array(0:ARRAYDIMDEFAULT-1))
      this%lenarray = ARRAYDIMDEFAULT
    end if
    this%is_init = .true.
    return
    !
  end subroutine InitHash

  subroutine PutHash(this, key, indx)
    implicit none
    ! -- dummy
    class(HashTableType), intent(inout) :: this
    character(len=*),     intent(in)    :: key
    integer(I4B),              intent(in)    :: indx
    ! -- local
    integer(I4B)                       :: ihash
    !
    ihash = mod(sum_ichar(key), this%lenarray)
    call this%array(ihash)%put_node(key, indx)
    !
    return
  end subroutine PutHash

  subroutine GetHash(this, key, indx)
    implicit none
    ! -- dummy
    class(HashTableType), intent(in)  :: this
    character(len=*),     intent(in)  :: key
    integer(I4B),              intent(out) :: indx
    ! -- local
    integer(I4B) :: ihash
    !
    if (this%lenarray > 0) then
      ihash = mod(sum_ichar(key), this%lenarray)
      call this%array(ihash)%get_node(key, indx)
    else
      indx = 0
    endif
    !
    return
  end subroutine GetHash

  subroutine FreeHash(this)
    implicit none
    ! -- dummy
    class(HashTableType), intent(inout) :: this
    integer(I4B) :: i, low, ihigh
    !
    if (allocated(this%array)) then
      low  = lbound(this%array,dim=1)
      ihigh = ubound(this%array,dim=1) 
      do i=low,ihigh
        call this%array(i)%free_node()
      end do
      deallocate(this%array)
    end if
    this%is_init = .false.
    !
    return
  end subroutine FreeHash
  
  ! Non-type-bound procedure

  elemental function sum_ichar(str)
    implicit none
    ! -- return
    integer(I4B) :: sum_ichar
    ! -- dummy
    character(len=*), intent(in)   :: str
    ! -- local
    character, dimension(len(str)) :: tmpch
    integer(I4B) :: i
    !
    forall (i=1:len(str))
      tmpch(i) = str(i:i)
    end forall
    sum_ichar = sum(ichar(tmpch))
    !
    return
  end function sum_ichar
  
end module HashTableModule
