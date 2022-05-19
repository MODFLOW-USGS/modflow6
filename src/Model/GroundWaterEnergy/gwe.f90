! Groundwater Energy Transport (GWE) Model

module GweModule

  use KindModule,                  only: DP, I4B
  use InputOutputModule,           only: ParseLine, upcase
  use ConstantsModule,             only: LENFTYPE, DZERO, LENPAKLOC
  use VersionModule,               only: write_listfile_header
  use NumericalModelModule,        only: NumericalModelType  
  use TransportModelModule,        only: TransportModelType
  use BaseModelModule,             only: BaseModelType

  implicit none
  
  private

  public :: GweModelType
  
  type, extends(TransportModelType) :: GweModelType
      
  contains
  
  
  end type GweModelType
  
  ! -- Module variables constant for simulation
  integer(I4B), parameter :: NIUNIT=100
  
end module GweModule