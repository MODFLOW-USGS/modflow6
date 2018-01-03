module KindModule
  
  integer, parameter :: DP = KIND(1.0D0)              ! Precision of all real variables
  integer, parameter :: I4B = SELECTED_INT_KIND(8)    ! Integer kind
  integer, parameter :: I8B = SELECTED_INT_KIND(18)   ! Long integer kind

end module KindModule
