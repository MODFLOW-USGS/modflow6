module SimStagesModule
  use KindModule, only: I4B
  implicit none

  ! stages for synchronization
  integer(I4B), public, parameter :: STG_NEVER = 0
  integer(I4B), public, parameter :: STG_BEFORE_INIT = 1
  integer(I4B), public, parameter :: STG_BEFORE_DF = 2
  integer(I4B), public, parameter :: STG_AFTER_DF = 3
  integer(I4B), public, parameter :: STG_BEFORE_AC = 4
  integer(I4B), public, parameter :: STG_BEFORE_AR = 5
  integer(I4B), public, parameter :: STG_AFTER_AR = 6
  integer(I4B), public, parameter :: STG_BEFORE_AD = 7
  integer(I4B), public, parameter :: STG_BEFORE_CF = 8
  integer(I4B), public, parameter :: STG_BEFORE_FC = 9

end module SimStagesModule