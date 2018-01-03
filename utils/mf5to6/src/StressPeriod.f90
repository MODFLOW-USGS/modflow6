module StressPeriodModule

  private
  public StressPeriodType

  type :: StressPeriodType
    double precision :: perlen
    integer          :: nstp
    double precision :: tsmult
    character(len=2) :: sstr
  end type StressPeriodType

end module StressPeriodModule
