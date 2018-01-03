module ConverterCommonModule
  private
  public :: SupportPreproc
  
  ! Make SupportPreproc TRUE to allow converter to read a file of mf5to6 
  ! options, use PreHeadsMF, and prepare input for PostObsMF.
  logical :: SupportPreproc = .false. 
  
end module ConverterCommonModule
