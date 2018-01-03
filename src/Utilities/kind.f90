module KindModule
  
  integer, parameter :: DP = KIND(1.0D0)              !Precision of all real variables
  integer, parameter :: I4B = SELECTED_INT_KIND(8)    !Integer kind

end module KindModule
