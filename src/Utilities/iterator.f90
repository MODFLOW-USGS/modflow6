! Iterator for stepping through each entry position in a compressed
! sparse row format.  This was developed so that connections could
! masked and skipped over in a hidden manner. It can be used in
! the following way
!
!  type(CSRIteratorType) :: iterator
!  call iterator%set_iaja(ia, ja)
!  call iterator%set_masked_array(masked_array)
!  call iterator%reset_next_offdiagonal(upper_triangle=.true.)
!  do while (iterator%next_offdiagonal)
!    n = iterator%n
!    m = iterator%m
!    ipos = iterator%japos
!    print *, n, m, ipos
!  end do
    
module IteratorModule
  
  use KindModule, only: DP, I4B
  
  type :: CSRIteratorType
    
    integer(I4B), dimension(:), contiguous, pointer :: ia => null()
    integer(I4B), dimension(:), contiguous, pointer :: ja => null()
    integer(I4B), dimension(:), contiguous, pointer :: masked_array => null()
    integer(I4B) :: n
    integer(I4B) :: m
    integer(I4B) :: japos
    integer(I4B) :: iapos
    integer(I4B) :: jasize
    logical :: upper_triangle
    logical :: finished
    
  contains
    
    procedure :: set_iaja
    procedure :: set_masked_array
    procedure :: reset
    procedure :: reset_next_offdiagonal
    procedure :: next
    procedure :: next_unmasked
    procedure :: next_offdiagonal
    
  end type CSRIteratorType
    
  contains
  
  subroutine set_iaja(this, ia, ja)
    class(CSRIteratorType) :: this
    integer(I4B), dimension(:), contiguous, target, intent(in) :: ia
    integer(I4B), dimension(:), contiguous, target, intent(in) :: ja
    this%ia => ia
    this%ja => ja
    this%jasize = size(ja)
  end subroutine set_iaja
  
  subroutine set_masked_array(this, masked_array)
    class(CSRIteratorType) :: this
    integer(I4B), dimension(:), contiguous, target, intent(in) :: masked_array
    this%masked_array => masked_array
  end subroutine set_masked_array
  
  subroutine reset(this)
    class(CSRIteratorType) :: this
    this%japos = 0
    this%iapos = 1
    this%finished = .false.
    this%upper_triangle = .false.
  end subroutine reset
  
  subroutine reset_next_offdiagonal(this, upper_triangle)
    class(CSRIteratorType) :: this
    logical, optional, intent(in) :: upper_triangle
    this%japos = 0
    this%iapos = 1
    this%finished = .false.
    if (present(upper_triangle)) then
      this%upper_triangle = upper_triangle
    else
      this%upper_triangle = .false.
    end if
  end subroutine reset_next_offdiagonal
  
  function next(this) result(res)
    class(CSRIteratorType) :: this
    logical :: res
    integer(I4B) :: imask
    do
      call this%next_unmasked()
      if (this%finished) then
        exit
      else if (.not. associated(this%masked_array)) then
        imask = 0
      else
        imask = this%masked_array(this%japos)
      end if
      if (imask == 0) exit
    end do
    res = (.not. this%finished)
  end function next
    
  subroutine next_unmasked(this)
    class(CSRIteratorType) :: this
    this%japos = this%japos + 1
    if (this%japos > this%jasize) then
      this%finished = .true.
      this%n = 0
      this%m = 0
    else
      if (this%japos == this%ia(this%iapos)) then
        this%n = this%ja(this%japos)
        this%iapos = this%iapos + 1
      end if
      this%m = this%ja(this%japos)
    end if
  end subroutine next_unmasked

  function next_offdiagonal(this) result(res)
    class(CSRIteratorType) :: this
    logical :: res
    logical :: ltmp
    do
      ltmp = this%next()
      if (this%finished) exit
      if (this%upper_triangle) then
        if (this%m > this%n) exit
      else
        if (this%n /= this%m) exit
      end if
    end do
    res = (.not. this%finished)
  end function next_offdiagonal
  
end module IteratorModule