!> @brief This module contains simulation variables
!!
!! This module contains simulation variables that are available to all
!! other modules. This variables in this module are defined at run time.
!! The module does not have any dependencies on models, exchanges, or
!! solutions in a simulation.
!!
!<
module SimVariablesModule
  use, intrinsic :: iso_fortran_env, only: output_unit
  use KindModule, only: DP, I4B
  use ConstantsModule, only: LINELENGTH, MAXCHARLEN, IUSTART, VALL, MNORMAL
  public
  character(len=LINELENGTH) :: simfile    = 'mfsim.nam'                          !< simulation name file
  character(len=LINELENGTH) :: simlstfile = 'mfsim.lst'                          !< simulation listing file name
  character(len=LINELENGTH) :: simstdout  = 'mfsim.stdout'                       !< name of standard out file if screen output is piped to a file
  character(len=MAXCHARLEN) :: errmsg                                            !< error message string
  character(len=MAXCHARLEN) :: warnmsg                                           !< warning message string
  integer(I4B) :: istdout = output_unit                                          !< unit number for stdout
  integer(I4B) :: isim_level = VALL                                              !< simulation output level
  integer(I4B) :: isim_mode = MNORMAL                                            !< simulation mode
  integer(I4B) :: iout                                                           !< file unit number for simulation output
  integer(I4B) :: isimcnvg                                                       !< simulation convergence flag (1) if all objects have converged, (0) otherwise
  integer(I4B) :: isimcontinue = 0                                               !< simulation continue flag (1) to continue if isimcnvg = 0, (0) to terminate
  integer(I4B) :: isimcheck = 1                                                  !< simulation input check flag (1) to check input, (0) to ignore checks
  integer(I4B) :: numnoconverge = 0                                              !< number of times the simulation did not converge
  integer(I4B) :: ireturnerr = 0                                                 !< return code for program (0) successful, (1) non-convergence, (2) error
  integer(I4B) :: iforcestop = 1                                                 !< forced stop flag (1) forces a call to ustop(..) when the simulation has ended, (0) doesn't
  integer(I4B) :: iunext = IUSTART                                               !< next file unit number to assign
  integer(I4B) :: lastStepFailed = 0                                             !< flag indicating if the last step failed (1) if last step failed; (0) otherwise (set in converge_check)
  integer(I4B) :: iFailedStepRetry = 0                                           !< current retry for this time step
end module SimVariablesModule
