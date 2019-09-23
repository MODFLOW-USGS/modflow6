module TestNumericalExchangeHelperModule
  use KindModule, only:I4B
  use NumericalExchangeModule
  use NumericalModelModule
  implicit none 
  
contains
  
  function getNumericalExchange(model1, model2, nexg) result(numEx)
    class(NumericalExchangeType), pointer :: numEx
    class(NumericalModelType), pointer :: model1, model2
    integer(I4B) :: nexg
    
    allocate(numEx)
    call numEx%allocate_scalars()
    numEx%nexg = nexg
    
    allocate(numEx%nodem1(nexg), numEx%nodem2(nexg))
    numEx%m1 => model1
    numEx%m2 => model2  
  end function
  
end module TestNumericalExchangeHelperModule