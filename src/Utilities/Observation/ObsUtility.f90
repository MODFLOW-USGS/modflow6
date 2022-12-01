! This module contains subroutines for writing simulated values stored
! in objects of ObserveType to output files.  The subroutines handle
! continuous observations, and can write values to either formatted or
! unformatted files.
!-----------------------------------------------------------------------
module ObsUtilityModule

  use KindModule, only: DP, I4B
  use ConstantsModule, only: LENOBSNAME, LENBIGLINE
  use ObserveModule, only: ObserveType
  use ObsOutputListModule, only: ObsOutputListType
  use ObsOutputModule, only: ObsOutputType
  use TdisModule, only: totim

  implicit none

  private
  public :: write_fmtd_cont, write_unfmtd_cont

contains

  subroutine write_fmtd_cont(fmtc, obsrv, obsOutputList, value)
! **************************************************************************
! For a continuous observation, store a simulated value for the end of a
! time step into the lineout member of a specified ObserveType object
! for later writing.  If the simulation time has not been written to
! the lineout member for the current time step, totim is written.  The
! simulated value is written in the format specified in the fmtc argument.
! **************************************************************************
!
!    SPECIFICATIONS:
! --------------------------------------------------------------------------
    implicit none
    ! -- dummy
    character(len=*), intent(in) :: fmtc
    type(ObserveType), intent(inout) :: obsrv
    type(ObsOutputListType), pointer, intent(inout) :: obsOutputList
    real(DP), intent(in) :: value
    ! -- local
    integer(I4B) :: indx
    integer(I4B) :: nunit
    character(len=50) :: cval
    character(len=LENOBSNAME), pointer :: linout => null()
    type(ObsOutputType), pointer :: ObsOutput => null()
    !---------------------------------------------------------------------------
    ! -- format for totim
10  format(G20.13)
    ! -- output unit
    nunit = obsrv%UnitNumber
    !
    indx = obsrv%indxObsOutput
    ObsOutput => obsOutputList%Get(indx)
    linout => obsOutput%lineout
    if (linout == '') then
      write (linout, 10) totim
      write (cval, 10) totim
      write (nunit, '(a)', advance='NO') trim(adjustl(cval))
    end if
    ! -- append value to output line
    write (cval, fmtc) value
    write (nunit, '(a,a)', advance='NO') ',', trim(adjustl(cval))
    !
    ! -- return
    return
  end subroutine write_fmtd_cont

  subroutine write_unfmtd_cont(obsrv, iprec, obsOutputList, value)
! **************************************************************************
! For a continuous observation, write a simulated value for the end of a
! time step to the output unit number stored in the specified
! ObserveType object.  If the simulation time has not been written to
! the output file for the current time step, totim is written.  Totim and
! the simulated value are written unformatted using the precision specified
! in the iprec argument.
! **************************************************************************
!
!    SPECIFICATIONS:
! --------------------------------------------------------------------------
!   real32 specifies 32-bit real = 4 bytes = single precision.
!   real64 specifies 64-bit real = 8 bytes = double precision.
    use iso_fortran_env, only: real32, real64
    implicit none
    ! -- dummy
    type(ObserveType), intent(inout) :: obsrv
    integer(I4B), intent(in) :: iprec
    type(ObsOutputListType), pointer, intent(inout) :: obsOutputList
    real(DP), intent(in) :: value
    ! -- local
    integer(I4B) :: indx, nunit
    character(len=LENOBSNAME), pointer :: linout => null()
    real(real32) :: totimsngl, valsngl
    real(real64) :: totimdbl, valdbl
    type(ObsOutputType), pointer :: obsOutput => null()
    !---------------------------------------------------------------------------
    ! -- formats
10  format(G20.13)
    ! -- output unit
    nunit = obsrv%UnitNumber
    ! -- continuous observation
    indx = obsrv%indxObsOutput
    obsOutput => obsOutputList%Get(indx)
    linout => obsOutput%lineout
    if (linout == '') then
      write (linout, 10) totim
      if (iprec == 1) then
        totimsngl = real(totim, real32)
        write (nunit) totimsngl
      elseif (iprec == 2) then
        totimdbl = totim
        write (nunit) totimdbl
      end if
    end if
    ! -- write value to unformatted output
    if (iprec == 1) then
      valsngl = real(value, real32)
      write (nunit) valsngl
    elseif (iprec == 2) then
      valdbl = value
      write (nunit) valdbl
    end if
    !
    ! -- return
    return
  end subroutine write_unfmtd_cont

end module ObsUtilityModule
