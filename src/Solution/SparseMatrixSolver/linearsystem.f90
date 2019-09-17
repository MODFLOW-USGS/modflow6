module LinearSystemMatrixModule
  
  use KindModule, only: DP, I4B
  
  implicit none
  
  private
  public :: LinearSystemMatrixType
  
  type LinearSystemMatrixType
    
    integer(I4B), dimension(:), contiguous, pointer :: ma => null()
    real(DP), dimension(:), contiguous, pointer :: amat => null()
    
  contains
    
    procedure :: set_matrix_pointer
    procedure :: add_to_matrix
    procedure :: replace_in_matrix
    procedure :: get_term
    
  end type LinearSystemMatrixType

  contains
  
    subroutine set_matrix_pointer(this, amat)
      class(LinearSystemMatrixType) :: this
      real(DP), dimension(:), contiguous, target, intent(in) :: amat
      this%amat => amat
    end subroutine set_matrix_pointer
    
    subroutine add_to_matrix(this, ipos, value)
      class(LinearSystemMatrixType) :: this
      integer(I4B), intent(in) :: ipos
      real(DP), intent(in) :: value
      this%amat(ipos) = this%amat(ipos) + value
    end subroutine add_to_matrix
    
    subroutine replace_in_matrix(this, ipos, value)
      class(LinearSystemMatrixType) :: this
      integer(I4B), intent(in) :: ipos
      real(DP), intent(in) :: value
      this%amat(ipos) = value
    end subroutine replace_in_matrix
    
    function get_term(this, ipos) result(value)
      class(LinearSystemMatrixType) :: this
      integer(I4B), intent(in) :: ipos
      real(DP) :: value
      value = this%amat(ipos)
    end function get_term
    
end module LinearSystemMatrixModule