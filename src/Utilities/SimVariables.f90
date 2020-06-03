module SimVariablesModule
  use, intrinsic :: iso_fortran_env, only: output_unit
  use KindModule, only: DP, I4B
  use ConstantsModule, only: LINELENGTH, MAXCHARLEN, IUSTART, VALL, MNORMAL
  public
  character(len=LINELENGTH) :: simfile    = 'mfsim.nam'
  character(len=LINELENGTH) :: simlstfile = 'mfsim.lst'
  character(len=LINELENGTH) :: simstdout  = 'mfsim.stdout'
  character(len=MAXCHARLEN) :: errmsg
  character(len=MAXCHARLEN) :: warnmsg
  integer(I4B) :: istdout = output_unit                                          ! -- unit number for stdout
  integer(I4B) :: isim_level = VALL                                              ! -- simulation output level
  integer(I4B) :: isim_mode = MNORMAL                                            ! -- simulation mode
  integer(I4B) :: iout                                                           ! -- unit number for simulation output
  integer(I4B) :: isimcnvg                                                       ! -- 1 if all objects have converged, 0 otherwise
  integer(I4B) :: isimcontinue = 0                                               ! -- 1 to continue if isimcnvg = 0, 0 to terminate
  integer(I4B) :: isimcheck = 1                                                  ! -- 1 to check input, 0 to ignore checks
  integer(I4B) :: numnoconverge = 0                                              ! -- number of times there were convergence problems
  integer(I4B) :: ireturnerr = 0                                                 ! -- return code for program (0 successful, 1 non-convergence, 2 error)
  integer(I4B) :: iforcestop = 1                                                 ! -- 1 forces a call to ustop(..) when the simulation has ended, 0 doesn't
  integer(I4B) :: iunext = iustart
end module SimVariablesModule
