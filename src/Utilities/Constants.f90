module ConstantsModule
  use, intrinsic :: iso_fortran_env, only: output_unit
  use KindModule
  public
  ! -- constants
  integer(I4B), parameter :: ISTDOUT = output_unit
  integer(I4B), parameter :: IUSERFORMATSTRIP = -99
  integer(I4B), parameter :: IUSERFORMATWRAP = 99
  integer(I4B), parameter :: LENBIGLINE = 5000
  integer(I4B), parameter :: LENHUGELINE = 50000
  integer(I4B), parameter :: LENVARNAME = 16
  integer(I4B), parameter :: LENMODELNAME = 16
  integer(I4B), parameter :: LENSOLUTIONNAME = 16
  integer(I4B), parameter :: LENAUXNAME = 16
  integer(I4B), parameter :: LENBOUNDNAME = 40
  integer(I4B), parameter :: LENBUDTXT = 16
  integer(I4B), parameter :: LENPACKAGENAME = 16
  integer(I4B), parameter :: LENPACKAGETYPE = 7
  integer(I4B), parameter :: LENORIGIN = LENMODELNAME + LENPACKAGENAME + 1
  integer(I4B), parameter :: LENFTYPE = 5
  integer(I4B), parameter :: LENOBSNAME = 40
  integer(I4B), parameter :: LENOBSTYPE = 20
  integer(I4B), parameter :: LENTIMESERIESNAME = LENOBSNAME
  integer(I4B), parameter :: LENTIMESERIESTEXT = 12
  integer(I4B), parameter :: LENDATETIME = 30
  integer(I4B), parameter :: LINELENGTH = 300
  integer(I4B), parameter :: MAXCHARLEN = 1000
  integer(I4B), parameter :: MAXOBSTYPES = 100
  integer(I4B), parameter :: NAMEDBOUNDFLAG = -9
  integer(I4B), parameter :: IZERO = 0

  real(DP), parameter :: DZERO = 0.0_DP
  real(DP), parameter :: DONETHIRD = 1.0_DP / 3.0_DP
  real(DP), parameter :: DHALF = 0.5_DP
  real(DP), parameter :: DP6 = 0.6_DP
  real(DP), parameter :: DTWOTHIRDS = 2.0_DP / 3.0_DP
  real(DP), parameter :: DP7 = 0.7_DP
  real(DP), parameter :: DP9 = 0.9_DP
  real(DP), parameter :: DP99 = 0.99_DP
  real(DP), parameter :: DP999 = 0.999_DP

  real(DP), parameter :: DONE = 1.0_DP
  real(DP), parameter :: D1P1 = 1.1_DP
  real(DP), parameter :: DFIVETHIRDS = 5.0_DP / 3.0_DP
  real(DP), parameter :: DTWO = 2.0_DP
  real(DP), parameter :: DTHREE = 3.0_DP
  real(DP), parameter :: DFOUR = 4.0_DP
  real(DP), parameter :: DSIX = 6.0_DP
  real(DP), parameter :: DEIGHT = 8.0_DP
  real(DP), parameter :: DTEN = 1.0e1_DP
  real(DP), parameter :: DHUNDRED = 1.0e2_DP

  real(DP), parameter :: DEP6 = 1.0e6_DP
  real(DP), parameter :: DEP20 = 1.0e20_DP

  real(DP), parameter :: DHNOFLO = 1.e30_DP
  real(DP), parameter :: DHDRY = -1.e30_DP
  real(DP), parameter :: DNODATA = 3.0e30_DP

  real(DP), parameter :: DEM1  = 1.0e-1_DP
  real(DP), parameter :: D5EM2 = 5.0e-2_DP
  real(DP), parameter :: DEM2  = 1.0e-2_DP
  real(DP), parameter :: DEM3  = 1.0e-3_DP
  real(DP), parameter :: DEM4  = 1.0e-4_DP
  real(DP), parameter :: DEM5  = 1.0e-5_DP
  real(DP), parameter :: DEM6  = 1.0e-6_DP
  real(DP), parameter :: DEM7  = 1.0e-7_DP
  real(DP), parameter :: DEM8  = 1.0e-8_DP
  real(DP), parameter :: DEM9  = 1.0e-9_DP
  real(DP), parameter :: DEM10 = 1.0e-10_DP
  real(DP), parameter :: DEM12 = 1.0e-12_DP
  real(DP), parameter :: DEM14 = 1.0e-14_DP
  real(DP), parameter :: DEM15 = 1.0e-15_DP
  real(DP), parameter :: DEM20 = 1.0e-20_DP
  real(DP), parameter :: DEM30 = 1.0e-30_DP

  real(DP), parameter :: DPREC = EPSILON(1.0_DP)
  real(DP), parameter :: DSAME = DHUNDRED * DPREC

  real(DP), parameter :: DLNLOW = 0.995_DP
  real(DP), parameter :: DLNHIGH = 1.005_DP

  real(DP), parameter :: DPI = DFOUR * ATAN(DONE)
  real(DP), parameter :: DTWOPI = DTWO * DFOUR * ATAN(DONE)
  real(DP), parameter :: DPIO180 = datan(DONE)/4.5d1

  real(DP), parameter :: DGRAVITY = 9.80665_DP
  real(DP), parameter :: DCD = 0.61_DP

  character(len=10), dimension(3, 3), parameter :: cidxnames = reshape (       &
    [ '      NODE', '          ', '          ',                                &
      '     LAYER', '    CELL2D', '          ',                                &
      '     LAYER', '       ROW', '       COL'], [3,3])

  ! -- Enumerators used with TimeSeriesType
  ENUM, BIND(C)
    ! Sets UNDEFINED=0, STEPWISE=1, LINEAR=2, LINEAREND=3
    ENUMERATOR :: UNDEFINED, STEPWISE, LINEAR, LINEAREND
  END ENUM


end module ConstantsModule
