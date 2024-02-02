module SimStagesModule
  use KindModule, only: I4B
  implicit none
  private

  public :: STG_TO_STR

  ! stages for synchronization
  integer(I4B), public, parameter :: STG_NEVER = 0 !< never
  integer(I4B), public, parameter :: STG_BFR_MDL_DF = 1 !< before model define
  integer(I4B), public, parameter :: STG_AFT_MDL_DF = 2 !< after model define
  integer(I4B), public, parameter :: STG_AFT_EXG_DF = 3 !< after exchange define
  integer(I4B), public, parameter :: STG_AFT_CON_CR = 4 !< after connection create
  integer(I4B), public, parameter :: STG_BFR_CON_DF = 5 !< before connection define
  integer(I4B), public, parameter :: STG_AFT_CON_DF = 6 !< after connection define
  integer(I4B), public, parameter :: STG_BFR_EXG_AC = 7 !< before exchange add connections (per solution)
  integer(I4B), public, parameter :: STG_BFR_CON_AR = 8 !< before connection allocate read
  integer(I4B), public, parameter :: STG_AFT_CON_AR = 9 !< afterr connection allocate read
  integer(I4B), public, parameter :: STG_BFR_EXG_RP = 10 !< before exchange read prepare
  integer(I4B), public, parameter :: STG_AFT_CON_RP = 11 !< after connection read prepare
  integer(I4B), public, parameter :: STG_BFR_EXG_AD = 12 !< before exchange advance (per solution)
  integer(I4B), public, parameter :: STG_BFR_EXG_CF = 13 !< before exchange calculate (per solution)
  integer(I4B), public, parameter :: STG_BFR_EXG_FC = 14 !< before exchange formulate (per solution)
  integer(I4B), public, parameter :: NR_SIM_STAGES = 14 !< before exchange formulate (per solution)

contains

  !> @brief Converts a stage to its string representation
  !<
  function STG_TO_STR(stage) result(stg_str)
    integer(I4B) :: stage
    character(len=24) :: stg_str

    if (stage == STG_NEVER) then; stg_str = "STG_NEVER"
    else if (stage == STG_BFR_MDL_DF) then; stg_str = "STG_BFR_MDL_DF"
    else if (stage == STG_AFT_MDL_DF) then; stg_str = "STG_AFT_MDL_DF"
    else if (stage == STG_AFT_EXG_DF) then; stg_str = "STG_AFT_EXG_DF"
    else if (stage == STG_AFT_CON_CR) then; stg_str = "STG_AFT_CON_CR"
    else if (stage == STG_BFR_CON_DF) then; stg_str = "STG_BFR_CON_DF"
    else if (stage == STG_AFT_CON_DF) then; stg_str = "STG_AFT_CON_DF"
    else if (stage == STG_BFR_EXG_AC) then; stg_str = "STG_BFR_EXG_AC"
    else if (stage == STG_BFR_CON_AR) then; stg_str = "STG_BFR_CON_AR"
    else if (stage == STG_AFT_CON_AR) then; stg_str = "STG_AFT_CON_AR"
    else if (stage == STG_BFR_EXG_RP) then; stg_str = "STG_BFR_EXG_RP"
    else if (stage == STG_AFT_CON_RP) then; stg_str = "STG_AFT_CON_RP"
    else if (stage == STG_BFR_EXG_AD) then; stg_str = "STG_BFR_EXG_AD"
    else if (stage == STG_BFR_EXG_CF) then; stg_str = "STG_BFR_EXG_CF"
    else if (stage == STG_BFR_EXG_FC) then; stg_str = "STG_BFR_EXG_FC"
    else; stg_str = "UNKNOWN"
    end if

  end function STG_TO_STR

end module SimStagesModule
