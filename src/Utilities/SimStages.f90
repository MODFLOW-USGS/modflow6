module SimStagesModule
  use KindModule, only: I4B
  implicit none
  private

  public :: STG_TO_STR

  ! stages for synchronization
  integer(I4B), public, parameter :: STG_NEVER = 0
  integer(I4B), public, parameter :: STG_INIT = 1
  integer(I4B), public, parameter :: STG_AFTER_MDL_DF = 2
  integer(I4B), public, parameter :: STG_AFTER_EXG_DF = 3
  integer(I4B), public, parameter :: STG_AFTER_CON_CR = 4
  integer(I4B), public, parameter :: STG_BEFORE_CON_DF = 5
  integer(I4B), public, parameter :: STG_AFTER_CON_DF = 6
  integer(I4B), public, parameter :: STG_BEFORE_AC = 7
  integer(I4B), public, parameter :: STG_BEFORE_AR = 8
  integer(I4B), public, parameter :: STG_AFTER_AR = 9
  integer(I4B), public, parameter :: STG_BEFORE_AD = 10
  integer(I4B), public, parameter :: STG_BEFORE_CF = 11
  integer(I4B), public, parameter :: STG_BEFORE_FC = 12

contains

  !> @brief Converts a stage to its string representation
  !<
  function STG_TO_STR(stage) result(stg_str)
    integer(I4B) :: stage
    character(len=24) :: stg_str

    if (stage == STG_NEVER) then; stg_str = "STG_NEVER"
    else if (stage == STG_INIT) then; stg_str = "STG_INIT"
    else if (stage == STG_AFTER_MDL_DF) then; stg_str = "STG_AFTER_MDL_DF"
    else if (stage == STG_AFTER_EXG_DF) then; stg_str = "STG_AFTER_EXG_DF"
    else if (stage == STG_AFTER_CON_CR) then; stg_str = "STG_AFTER_CON_CR"
    else if (stage == STG_BEFORE_CON_DF) then; stg_str = "STG_BEFORE_CON_DF"
    else if (stage == STG_AFTER_CON_DF) then; stg_str = "STG_AFTER_CON_DF"
    else if (stage == STG_BEFORE_AC) then; stg_str = "STG_BEFORE_AC"
    else if (stage == STG_BEFORE_AR) then; stg_str = "STG_BEFORE_AR"
    else if (stage == STG_AFTER_AR) then; stg_str = "STG_AFTER_AR"
    else if (stage == STG_BEFORE_AD) then; stg_str = "STG_BEFORE_AD"
    else if (stage == STG_BEFORE_CF) then; stg_str = "STG_BEFORE_CF"
    else if (stage == STG_BEFORE_FC) then; stg_str = "STG_BEFORE_FC"
    else; stg_str = "UNKNOWN"
    end if

  end function STG_TO_STR

end module SimStagesModule
