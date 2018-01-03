module SimVariablesModule
  use KindModule, only: DP, I4B
  public
  integer(I4B) :: iout              ! -- unit number for simulation output
  integer(I4B) :: isimcnvg          ! -- 1 if all objects have converged, 0 otherwise
  integer(I4B) :: isimcontinue = 0  ! -- 1 to continue if isimcnvg = 0, 0 to terminate
  integer(I4B) :: isimcheck = 1     ! -- 1 to check input, 0 to ignore checks
  integer(I4B) :: numnoconverge = 0 ! -- number of times there were convergence problems
end module SimVariablesModule
