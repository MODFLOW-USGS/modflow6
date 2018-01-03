module globalPHMF
  
  implicit none
  
  private
  public :: iout, iprecision, ndigits, outfile
  
  ! scalars
  integer :: iout = 0
  integer :: iprecision = 2  ! 2 = double precision
  integer :: ndigits = 7
  character(len=30) :: outfile = 'PreHeadsMF_output.txt'

end module globalPHMF
