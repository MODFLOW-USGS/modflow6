module CsrUtilsModule

  implicit none
  private

  public :: getCSRIndex

contains

  !> @brief Return index for element i,j in CSR storage,
  !< returns -1 when not there
  function getCSRIndex(i, j, ia, ja) result(csrIndex)
    use KindModule, only: I4B
    integer(I4B), intent(in) :: i !< the row index
    integer(I4B), intent(in) :: j !< the column index
    integer(I4B), dimension(:), intent(in) :: ia !< CSR ia array
    integer(I4B), dimension(:), intent(in) :: ja !< CSR ja array
    integer(I4B) :: csrIndex !< the CSR ndex of element i,j
    ! local
    integer(I4B) :: idx

    csrIndex = -1
    do idx = ia(i), ia(i + 1) - 1
      if (ja(idx) == j) then
        csrIndex = idx
        return
      end if
    end do

  end function

end module
