module TimeSeriesRecordModule
  
  use KindModule, only: DP, I4B
  use ListModule, only: ListType

  private
  public :: TimeSeriesRecordType, ConstructTimeSeriesRecord, &
            CastAsTimeSeriesRecordType, AddTimeSeriesRecordToList

  type :: TimeSeriesRecordType
    ! -- Public members
    real(DP), public :: tsrTime
    real(DP), public :: tsrValue
  end type TimeSeriesRecordType

contains

  subroutine ConstructTimeSeriesRecord(newTsRecord, time, value)
    ! Allocate and assign members of a new TimeSeriesRecordType object
    implicit none
    type(TimeSeriesRecordType), pointer, intent(out) :: newTsRecord
    real(DP), intent(in) :: time, value
    !
    allocate(newTsRecord)
    newTsRecord%tsrTime = time
    newTsRecord%tsrValue = value
    return
  end subroutine ConstructTimeSeriesRecord

  function CastAsTimeSeriesRecordType(obj) result(res)
    ! Cast an unlimited polymorphic object as TimeSeriesRecordType
    implicit none
    class(*), pointer, intent(inout) :: obj
    type(TimeSeriesRecordType), pointer :: res
    !
    res => null()
    if (.not. associated(obj)) return
    !
    select type (obj)
    type is (TimeSeriesRecordType)
      res => obj
    end select
    return
  end function CastAsTimeSeriesRecordType

  subroutine AddTimeSeriesRecordToList(list, tsrecord)
    implicit none
    ! -- dummy
    type(ListType),             intent(inout) :: list
    type(TimeSeriesRecordType), pointer, intent(inout) :: tsrecord
    ! -- local
    class(*), pointer :: obj => null()
    !
    obj => tsrecord
    call list%Add(obj)
    !
    return
  end subroutine AddTimeSeriesRecordToList

end module TimeSeriesRecordModule
