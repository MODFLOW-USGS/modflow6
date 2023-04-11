module DistVariableModule
  use ConstantsModule, only: LENCOMPONENTNAME, LENVARNAME
  use KindModule, only: I4B
  use ListModule, only: ListType
  use InterfaceMapModule

  implicit none
  private

  public :: GetDistVarFromList

  ! types of variables
  integer(I4B), public, parameter :: SYNC_SCALAR = 0
  integer(I4B), public, parameter :: SYNC_NODES = 1
  integer(I4B), public, parameter :: SYNC_NODES_NOREDUCE = 4
  integer(I4B), public, parameter :: SYNC_CONNECTIONS = 2
  integer(I4B), public, parameter :: SYNC_EXCHANGES = 3

  type, public :: DistVarType
    character(len=LENVARNAME) :: var_name !< name of variable, e.g. "K11"
    character(len=LENCOMPONENTNAME) :: subcomp_name !< subcomponent, e.g. "NPF"
    character(len=LENCOMPONENTNAME) :: comp_name !< component, e.g. the model or exchange name
    integer(I4B) :: map_type !< can be 0 = scalar, 1 = node based, 2 = connection based,
                             !! 3 = exchange based (connections crossing model boundaries)
    character(len=LENVARNAME) :: exg_var_name !< needed for exchange variables, e.g. SIMVALS
    integer(I4B), dimension(:), allocatable :: sync_stages !< when to sync, e.g. (/ BEFORE_AD, BEFORE_CF /)
  end type DistVarType

contains

  function GetDistVarFromList(list, idx) result(res)
    implicit none
    type(ListType), intent(inout) :: list
    integer(I4B), intent(in) :: idx
    class(DistVarType), pointer :: res
    ! local
    class(*), pointer :: obj

    obj => list%GetItem(idx)
    res => CastAsDistVar(obj)
    return

  end function GetDistVarFromList

  function CastAsDistVar(obj) result(res)
    implicit none
    class(*), pointer, intent(inout) :: obj
    class(DistVarType), pointer :: res

    res => null()
    if (.not. associated(obj)) return

    select type (obj)
    class is (DistVarType)
      res => obj
    end select
    return
  end function CastAsDistVar

end module DistVariableModule
