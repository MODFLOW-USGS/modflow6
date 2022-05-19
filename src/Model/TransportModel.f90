! Generalized Transport Base Class
! Base class for solute (mass) and energy (thermal) transport
!   (The following copied from gwt1.f90)
!   * Add check that discretization is the same between both models 
!   * Program GWT-GWT exchange transport (awaiting implementation of interface model)
!   * Consider implementation of steady-state transport (affects MST, IST)
!   * Check and handle pore space discrepancy between flow and transport (porosity vs specific yield)
!   * UZT may not have the required porosity term

module TransportModelModule
  use KindModule,                  only: DP, I4B
  use ConstantsModule,             only: LENFTYPE
  use SimVariablesModule,          only: errmsg
  use NumericalModelModule,        only: NumericalModelType
  
  implicit none
  
  private
  
  public :: TransportModelType
  
  type, extends(NumericalModelType) :: TransportModelType

  contains
  
  end type TransportModelType
  

  
end module TransportModelModule  
  