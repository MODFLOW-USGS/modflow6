module GlobalVariablesPHMFModule
  use ConstantsPHMFModule, only: HUGEDBL

  implicit none

  private
  public :: prognamPHMF, vnam, verbose
  
  character(len=60) :: prognamPHMF
  character(len=40) :: vnam
  parameter (prognamPHMF='PreHeadsMF')
  parameter (vnam='version 0.9.0 - 4/21/2017')
  logical :: verbose = .false.

end module GlobalVariablesPHMFModule
