module SimVariablesModule
  use, intrinsic :: iso_fortran_env, only: output_unit
  use KindModule, only: I4B
  use ConstantsModule, only: MAXCHARLEN, IUSTART
  use ListModule, only: ListType

  implicit none
  
  private
  public :: ModelPacks, SimMovers, iout, istdout, iunext

  integer(I4B) :: iout
  integer(I4B) :: istdout = output_unit
  integer(I4B) :: iunext = IUSTART
  type(ListType), pointer :: SimMovers => null()
  type(ListType) :: ModelPacks

end module SimVariablesModule
