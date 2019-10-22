module CsrUtilsModule

  implicit none
  private
  
  public :: getCSRIndex
  
contains
  
  ! return index for element i,j in CSR storage, and -1 when not there
  function getCSRIndex(i, j, ia, ja) result(csrIndex)
    use KindModule, only: I4B
    integer(I4B) :: i, j                          ! the element to get the index for
    integer(I4B), dimension(:), intent(in) :: ia  ! csr ia
    integer(I4B), dimension(:), intent(in) :: ja  ! csr ja
    integer(I4B) :: csrIndex                 ! the resulting index
    ! local
    integer(I4B) :: idx
    
    csrIndex = -1
    do idx = ia(i), ia(i+1)-1
      if (ja(idx) == j) then
        csrIndex = idx
        return
      end if
    end do
    
  end function  

end module