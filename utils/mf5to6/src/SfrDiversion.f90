module SfrDiversionModule
  
  use ListModule, only: ListType
  
  private
  public :: SfrDiversionType, ConstructSfrDiversion, &
            AddDiversionToList, GetDiversionFromList
  
  type :: SfrDiversionType
    integer :: SegnumUs  ! number of upstream segment from which flow is diverted
    integer :: SegnumDs  ! number of downstream segment that receives diversion
    integer :: Rno       ! reach number (of all reaches in package)
    integer :: Iconr     ! number of downstream reach to receive diversion
    integer :: Iprior    ! prioritization system flag
    real    :: DivFlow   ! volumetric flow rate to be diverted
    real    :: Ustrf     ! fraction of upstream flow to be diverted to reach Rno
    !
    ! Is this diversion to be kept and written, considering idomain 
    ! of parent is modified by child grids?
    logical :: KeepDiversionActive
  end type SfrDiversionType

contains

  subroutine ConstructSfrDiversion(newDiversion)
    implicit none
    type(SfrDiversionType), pointer, intent(out) :: newDiversion
    !
    allocate(newDiversion)
    !
    ! Initialize members
    newDiversion%SegnumUs = 0
    newDiversion%SegnumDs = 0
    newDiversion%Rno = 0
    newDiversion%Iconr = 0
    newDiversion%Iprior = -1   ! -1 is "priority" algorithm of STR1
    newDiversion%DivFlow = 0.0
    newDiversion%Ustrf = 0.0
    newDiversion%KeepDiversionActive = .true.
    !
    return
  end subroutine ConstructSfrDiversion

  function CastAsSfrDiversionType(obj) result (res)
    implicit none
    class(*), pointer, intent(inout) :: obj
    type(SfrDiversionType), pointer :: res
    !
    res => null()
    if (.not. associated(obj)) return
    !
    select type (obj)
    type is (SfrDiversionType)
      res => obj
    end select
    return
  end function CastAsSfrDiversionType
  
  subroutine AddDiversionToList(DiversionList, diversion)
    implicit none
    ! dummy
    type(ListType), pointer :: DiversionList
    type(SfrDiversionType), pointer :: diversion
    ! local
    class(*), pointer :: obj => null()
    !
    obj => diversion
    call DiversionList%Add(obj)
    !
    return
  end subroutine AddDiversionToList
  
  function GetDiversionFromList(DiversionList, idx) result(res)
    implicit none
    ! dummy
    type(ListType), pointer :: DiversionList
    integer, intent(in) :: idx
    type(SfrDiversionType), pointer :: res
    ! local
    class(*), pointer :: obj => null()
    !
    res => null()
    obj => DiversionList%GetItem(idx)
    if (associated(obj)) then
      select type (obj)
      type is (SfrDiversionType)
        res => obj
      end select
    endif
    !
    return
  end function GetDiversionFromList

end module SfrDiversionModule


