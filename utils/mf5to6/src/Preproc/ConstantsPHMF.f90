module ConstantsPHMFModule
  
  public
  
  integer, parameter :: LENCTYPE = 30
  integer, parameter :: LENOBSNAMENEW = 40

  double precision, parameter :: BIGHKVALUE = 1.0d20
  double precision, parameter :: HUGEDBL = huge(1.0d0)
  double precision, parameter :: HNOFLODEFAULT = 1.d30
  double precision, parameter :: HDRYDEFAULT = 2.d30

    ! File codes
  integer, parameter :: FCUNKNOWN  = 0  ! default--not to be listed in a name file
  ! Input
  integer, parameter :: FCINPUT    = 1  ! ascii MF6 input file (named type)
  integer, parameter :: FCDATAIN   = 2  ! ascii DATA file for MF6 input
  integer, parameter :: FCDATABIN  = 3  ! DATA(BINARY) file for MF6 input
  ! Output
  integer, parameter :: FCOUTPUT   = 4  ! MF6 output file (named type)
  integer, parameter :: FCDATAOUT  = 5  ! ascii DATA file for MF6 output
  integer, parameter :: FCDATABOUT = 6  ! DATA(BINARY) file for MF6 output
  
  ! Obs file formats
  integer, parameter :: SINGLE     = 1
  integer, parameter :: CONTINUOUS = 2

  ! Trig
  double precision, parameter :: PI = 4.D0*ATAN(1.D0)
  double precision, parameter :: DEGREES_TO_RADIANS = PI/180.0d0

end module ConstantsPHMFModule
