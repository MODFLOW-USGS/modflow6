module TimeArraySeriesModule

  use ArrayReadersModule, only: ReadArray
  use BlockParserModule,  only: BlockParserType
  use ConstantsModule,    only: LINELENGTH, UNDEFINED, STEPWISE, LINEAR,        &
                                LENTIMESERIESNAME, DZERO, DONE
  use GenericUtilitiesModule,   only: is_same
  use InputOutputModule,  only: GetUnit, openfile
  use KindModule,         only: DP, I4B
  use ListModule,         only: ListType, ListNodeType
  use SimVariablesModule, only: errmsg
  use SimModule,          only: count_errors, store_error, store_error_unit
  use TimeArrayModule,    only: TimeArrayType, ConstructTimeArray, &
                                AddTimeArrayToList, CastAsTimeArrayType, &
                                GetTimeArrayFromList
  use BaseDisModule,      only: DisBaseType
  use, intrinsic :: iso_fortran_env, only: IOSTAT_END

  implicit none
  private
  public  :: TimeArraySeriesType, ConstructTimeArraySeries, &
             CastAsTimeArraySeriesType, GetTimeArraySeriesFromList

  type TimeArraySeriesType
    ! -- Public members
    character(len=LENTIMESERIESNAME), public :: Name = ''
    ! -- Private members
    integer(I4B), private :: inunit = 0
    integer(I4B), private :: iout = 0
    integer(I4B), private :: iMethod = UNDEFINED
    real(DP), private :: sfac = DONE
    character(len=LINELENGTH), private :: dataFile = ''
    logical, private :: autoDeallocate = .true.
    type(ListType), pointer, private :: list => null()
    class(DisBaseType), pointer, private :: dis => null()
    type(BlockParserType), private :: parser
  contains
    ! -- Public procedures
    procedure, public :: tas_init
    procedure, public :: GetAverageValues
    procedure, public :: GetInunit
    procedure, public :: da => tas_da
    ! -- Private procedures
    procedure, private :: get_integrated_values
    procedure, private :: get_latest_preceding_node
    procedure, private :: get_values_at_time
    procedure, private :: get_surrounding_records
    procedure, private :: read_next_array
    procedure, private :: DeallocateBackward
  end type TimeArraySeriesType

contains

  ! -- Constructor for TimeArraySeriesType

  subroutine ConstructTimeArraySeries(newTas, filename)
! ******************************************************************************
! ConstructTimeArraySeries -- Allocate a new TimeArraySeriesType object.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    type(TimeArraySeriesType), pointer, intent(out) :: newTas
    character(len=*), intent(in) :: filename
    ! -- local
    logical :: lex
! ------------------------------------------------------------------------------
    ! formats
    10 format('Error: Time-array-series file "',a,'" does not exist.')
    !
    ! -- Allocate a new object of type TimeArraySeriesType
    allocate(newTas)
    allocate(newTas%list)
    !
    ! -- Ensure that input file exists
    inquire(file=filename,exist=lex)
    if (.not. lex) then
      write(errmsg,10)trim(filename)
      call store_error(errmsg, terminate=.TRUE.)
    endif
    newTas%datafile = filename
    !
    return
  end subroutine ConstructTimeArraySeries

  ! -- Public procedures

  subroutine tas_init(this, fname, dis, iout, tasname, autoDeallocate)
! ******************************************************************************
! tas_init -- initialize the time array series
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(TimeArraySeriesType), intent(inout) :: this
    character(len=*), intent(in) :: fname
    class(DisBaseType), pointer, intent(inout) :: dis
    integer(I4B), intent(in) :: iout
    character(len=*), intent(inout) :: tasname
    logical, optional,          intent(in)    :: autoDeallocate
    ! -- local
    integer(I4B) :: istatus
    integer(I4B) :: ierr
    integer(I4B) :: inunit
    character(len=40) :: keyword, keyvalue
    logical :: found, continueread, endOfBlock
! ------------------------------------------------------------------------------
    !
    ! -- initialize some variables
    if (present(autoDeallocate)) this%autoDeallocate = autoDeallocate
    this%dataFile = fname
    allocate(this%list)
    !
    ! -- assign members
    this%dis => dis
    this%iout = iout
    !
    ! -- open time-array series input file
    inunit = GetUnit()
    this%inunit = inunit
    call openfile(inunit, 0, fname, 'TAS6')
    !
    ! -- initialize block parser
    call this%parser%Initialize(this%inunit, this%iout)
    !
    ! -- read ATTRIBUTES block
    continueread = .false.
    ierr = 0
    !
    ! -- get BEGIN line of ATTRIBUTES block
    call this%parser%GetBlock('ATTRIBUTES', found, ierr, &
      supportOpenClose=.true.)
    if (.not. found) then
      errmsg = 'Error: Attributes block not found in file: ' // &
              trim(fname)
      call store_error(errmsg)
      call this%parser%StoreErrorUnit()
    endif
    !
    ! -- parse ATTRIBUTES entries
    do
      ! -- read line from input
      call this%parser%GetNextLine(endOfBlock)
      if (endOfBlock) exit
      !
      ! -- get the keyword
      call this%parser%GetStringCaps(keyword)
      !
      ! -- get the word following the keyword (the key value)
      call this%parser%GetStringCaps(keyvalue)
      select case (keyword)
      case ('NAME')
        this%Name = keyvalue
        tasname = keyvalue
      case ('METHOD')
        select case (keyvalue)
        case ('STEPWISE')
          this%iMethod = STEPWISE
        case ('LINEAR')
          this%iMethod = LINEAR
        case default
          errmsg = 'Unknown interpolation method: "' // trim(keyvalue) // '"'
          call store_error(errmsg)
          call this%parser%StoreErrorUnit()
        end select
      case ('AUTODEALLOCATE')
        this%autoDeallocate = (keyvalue == 'TRUE')
      case ('SFAC')
        read(keyvalue,*,iostat=istatus)this%sfac
        if (istatus /= 0) then
          errmsg = 'Error reading numeric SFAC value from "' // trim(keyvalue) &
                  // '"'
          call store_error(errmsg)
          call this%parser%StoreErrorUnit()
        endif
      case default
        errmsg = 'Unknown option found in ATTRIBUTES block: "' // &
                trim(keyword) // '"'
        call store_error(errmsg)
        call this%parser%StoreErrorUnit()
      end select
    enddo
    !
    ! -- ensure that NAME and METHOD have been specified
    if (this%Name == '') then
      errmsg = 'Name not specified for time array series in file: ' // &
               trim(this%dataFile)
      call store_error(errmsg)
      call this%parser%StoreErrorUnit()
    endif
    if (this%iMethod == UNDEFINED) then
      errmsg = 'Interpolation method not specified for time' // &
               ' array series in file: ' // trim(this%dataFile)
      call store_error(errmsg)
      call this%parser%StoreErrorUnit()
    endif
    !
    ! -- handle any errors encountered so far
    if (count_errors()>0) then
      errmsg = 'Error(s) encountered initializing time array series from file: ' // &
               trim(this%dataFile)
      call store_error(errmsg)
      call this%parser%StoreErrorUnit()
    endif
    !
    ! -- try to read first time array into linked list
    if (.not. this%read_next_array()) then
      errmsg = 'Error encountered reading time-array data from file: ' // &
               trim(this%dataFile)
      call store_error(errmsg)
      call this%parser%StoreErrorUnit()
    endif
    !
    return
  end subroutine tas_init

  subroutine GetAverageValues(this, nvals, values, time0, time1)
! ******************************************************************************
! GetAverageValues -- populate an array time-weighted average value for a 
!   specified time span.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(TimeArraySeriesType), intent(inout) :: this
    integer(I4B),                    intent(in)    :: nvals
    real(DP), dimension(nvals), intent(inout) :: values
    real(DP),           intent(in)    :: time0
    real(DP),           intent(in)    :: time1
    ! -- local
    integer(I4B) :: i
    real(DP) :: timediff
! ------------------------------------------------------------------------------
    !
    timediff = time1 - time0
    if (timediff > 0) then
      call this%get_integrated_values(nvals, values, time0, time1)
      do i=1,nvals
        values(i) = values(i) / timediff
      enddo
    else
      ! -- time0 and time1 are the same, so skip the integration step.
      call this%get_values_at_time(nvals, values, time0)
    endif
    !
    return
  end subroutine GetAverageValues

  function GetInunit(this)
! ******************************************************************************
! GetInunit -- return unit number
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- return
    integer(I4B) :: GetInunit
    ! -- dummy
    class(TimeArraySeriesType) :: this
! ------------------------------------------------------------------------------
    !
    GetInunit = this%inunit
    !
    return
  end function GetInunit

  ! -- Private procedures

  subroutine get_surrounding_records(this, time, taEarlier, taLater)
! ******************************************************************************
! get_surrounding_records -- get_surrounding_records
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(TimeArraySeriesType), intent(inout) :: this
    real(DP),      intent(in)    :: time
    type(TimeArrayType), pointer, intent(inout) :: taEarlier
    type(TimeArrayType), pointer, intent(inout) :: taLater
    ! -- local
    real(DP) :: time0, time1
    type(ListNodeType), pointer :: currNode => null()
    type(ListNodeType), pointer :: node0 => null()
    type(ListNodeType), pointer :: node1 => null()
    type(TimeArrayType), pointer :: ta => null(), ta0 => null(), ta1 => null()
    class(*), pointer :: obj
! ------------------------------------------------------------------------------
    !
    taEarlier => null()
    taLater => null()
    !
    if (associated(this%list%firstNode)) then
      currNode => this%list%firstNode
    endif
    !
    ! -- If the next node is earlier than time of interest, advance along
    !    linked list until the next node is later than time of interest.
    do
      if (associated(currNode)) then
        if (associated(currNode%nextNode)) then
          obj => currNode%nextNode%GetItem()
          ta => CastAsTimeArrayType(obj)
          if (ta%taTime <= time) then
            currNode => currNode%nextNode
          else
            exit
          endif
        else
          ! -- read another array
          if (.not. this%read_next_array()) exit
        endif
      else
        exit
      endif
    enddo
    !
    if (associated(currNode)) then
      !
      ! -- find earlier record
      node0 => currNode
      obj => node0%GetItem()
      ta0 => CastAsTimeArrayType(obj)
      time0 = ta0%taTime
      do while (time0 > time)
        if (associated(node0%prevNode)) then
          node0 => node0%prevNode
          obj => node0%GetItem()
          ta0 => CastAsTimeArrayType(obj)
          time0 = ta0%taTime
        else
          exit
        endif
      enddo
      !
      ! -- find later record
      node1 => currNode
      obj => node1%GetItem()
      ta1 => CastAsTimeArrayType(obj)
      time1 = ta1%taTime
      do while (time1 < time)
        if (associated(node1%nextNode)) then
          node1 => node1%nextNode
          obj => node1%GetItem()
          ta1 => CastAsTimeArrayType(obj)
          time1 = ta1%taTime
        else
          ! -- get next array
          if (.not. this%read_next_array()) then
            ! -- end of file reached, so exit loop
            exit
          endif
        endif
      enddo
      !
    endif
    !
    if (time0 <= time) taEarlier => ta0
    if (time1 >= time) taLater => ta1
    !
    return
  end subroutine get_surrounding_records

  logical function read_next_array(this)
! ******************************************************************************
! read_next_array -- Read next time array from input file and append to list.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(TimeArraySeriesType), intent(inout) :: this
    ! -- local
    integer(I4B) :: i, ierr, istart, istat, istop, lloc, nrow, ncol, nodesperlayer
    logical :: lopen, isFound
    type(TimeArrayType), pointer  :: ta => null()
! ------------------------------------------------------------------------------
    !
    istart = 1
    istat = 0
    istop = 1
    lloc = 1
    ! Get dimensions for supported discretization type
    if (this%dis%supports_layers()) then
      nodesperlayer = this%dis%get_ncpl()
      if(size(this%dis%mshape) == 3) then
        nrow = this%dis%mshape(2)
        ncol = this%dis%mshape(3)
      else
        nrow = 1
        ncol = this%dis%mshape(2)
      endif
    else
      errmsg = 'Time array series is not supported for selected discretization type.'
      call store_error(errmsg)
      call this%parser%StoreErrorUnit()
    endif
    !
    read_next_array = .false.
    inquire(unit=this%inunit,opened=lopen)
    if (lopen) then
      call ConstructTimeArray(ta, this%dis)
      ! -- read a time and an array from the input file
      ! -- Get a TIME block and read the time
      call this%parser%GetBlock('TIME', isFound, ierr, &
        supportOpenClose=.false.)
      if (isFound) then
        ta%taTime = this%parser%GetDouble()
        ! -- Read the array
        call ReadArray(this%parser%iuactive, ta%taArray, this%Name, &
                        this%dis%ndim, ncol, nrow, 1, nodesperlayer, &
                        this%iout, 0, 0)
        !
        ! -- multiply values by sfac
        do i = 1, nodesperlayer
          ta%taArray(i) = ta%taArray(i) * this%sfac
        enddo
        !
        ! -- append the new time array to the list
        call AddTimeArrayToList(this%list, ta)
        read_next_array = .true.
        !
        ! -- make sure block is closed
        call this%parser%terminateblock()
      endif
    endif
    return ! Normal return
    !
    return
  end function read_next_array

  subroutine get_values_at_time(this, nvals, values, time)
! ******************************************************************************
! get_values_at_time -- Return an array of values for a specified time, same 
!   units as time-series values.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(TimeArraySeriesType), intent(inout) :: this
    integer(I4B), intent(in) :: nvals
    real(DP), dimension(nvals), intent(inout) :: values
    real(DP), intent(in) :: time ! time of interest
    ! -- local
    integer(I4B) :: i, ierr
    real(DP) :: ratio, time0, time1, timediff, timediffi, val0, val1, &
                        valdiff
    type(TimeArrayType), pointer :: taEarlier => null()
    type(TimeArrayType), pointer :: taLater => null()
    ! formats
    10 format('Error getting array at time ',g10.3, &
              ' for time-array series "',a,'"')
! ------------------------------------------------------------------------------
    !
    ierr = 0
    call this%get_surrounding_records(time,taEarlier,taLater)
    if (associated(taEarlier)) then
      if (associated(taLater)) then
        ! -- values are available for both earlier and later times
        if (this%iMethod == STEPWISE) then
          ! -- Just populate values from elements of earlier time array
          values =  taEarlier%taArray
        elseif (this%iMethod == LINEAR) then
          ! -- perform linear interpolation
          time0 = taEarlier%taTime
          time1 = taLater%tatime
          timediff = time1 - time0
          timediffi = time - time0
          if (timediff>0) then
            ratio = timediffi/timediff
          else
            ! -- should not happen if TS does not contain duplicate times
            ratio = 0.5d0
          endif
          ! -- Iterate through all elements and perform interpolation.
          do i=1,nvals
            val0 = taEarlier%taArray(i)
            val1 = taLater%taArray(i)
            valdiff = val1 - val0
            values(i) = val0 + (ratio*valdiff)
          enddo
        else
          ierr = 1
        endif
      else
        if (is_same(taEarlier%taTime, time)) then
          values = taEarlier%taArray
        else
          ! -- Only earlier time is available, and it is not time of interest;
          !    however, if method is STEPWISE, use value for earlier time.
          if (this%iMethod == STEPWISE) then
            values =  taEarlier%taArray
          else
            ierr = 1
          endif
        endif
      endif
    else
      if (associated(taLater)) then
        if (is_same(taLater%taTime, time)) then
          values = taLater%taArray
        else
          ! -- only later time is available, and it is not time of interest
          ierr = 1
        endif
      else
        ! -- Neither earlier nor later time is available.
        !    This should never happen!
        ierr = 1
      endif
    endif
    !
    if (ierr > 0) then
      write(errmsg,10)time,trim(this%Name)
      call store_error(errmsg)
      call store_error_unit(this%inunit)
    endif
    !
    return
  end subroutine get_values_at_time

  subroutine get_integrated_values(this, nvals, values, time0, time1)
! ******************************************************************************
! get_integrated_values -- Populates an array with integrated values for a 
!    specified time span.  Units: (ts-value-unit)*time
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(TimeArraySeriesType), intent(inout) :: this
    integer(I4B),                    intent(in)    :: nvals
    real(DP), dimension(nvals), intent(inout) :: values
    real(DP),           intent(in)    :: time0
    real(DP),           intent(in)    :: time1
    ! -- local
    integer(I4B) :: i
    real(DP) :: area, currTime, nextTime, ratio0, ratio1, t0, &
                        t01, t1, timediff, value, value0, value1, valuediff
    logical :: ldone
    type(ListNodeType), pointer :: precNode => null()
    type(ListNodeType), pointer :: currNode => null(), nextNode => null()
    type(TimeArrayType), pointer :: currRecord => null(), nextRecord => null()
    class(*), pointer :: currObj => null(), nextObj => null()
    ! -- formats
10  format('Error encountered while performing integration', &
        ' for time-array series "',a,'" for time interval: ', &
        g12.5,' to ',g12.5)
! ------------------------------------------------------------------------------
    !
    values = DZERO
    value = DZERO
    ldone = .false.
    t1 = -DONE
    call this%get_latest_preceding_node(time0, precNode)
    if (associated(precNode)) then
      currNode => precNode
      do while (.not. ldone)
        currObj => currNode%GetItem()
        currRecord => CastAsTimeArrayType(currObj)
        currTime = currRecord%taTime
        if (currTime < time1) then
          if (.not. associated(currNode%nextNode)) then
            ! -- try to read the next array
            if (.not. this%read_next_array()) then
              write(errmsg,10)trim(this%Name),time0,time1
              call store_error(errmsg)
              call store_error_unit(this%inunit)
            endif
          endif
          if (associated(currNode%nextNode)) then
            nextNode => currNode%nextNode
            nextObj => nextNode%GetItem()
            nextRecord => CastAsTimeArrayType(nextObj)
            nextTime = nextRecord%taTime
            ! -- determine lower and upper limits of time span of interest
            !    within current interval
            if (currTime >= time0) then
              t0 = currTime
            else
              t0 = time0
            endif
            if (nextTime <= time1) then
              t1 = nextTime
            else
              t1 = time1
            endif
            ! -- For each element, find area of rectangle
            !    or trapezoid delimited by t0 and t1.
            t01 = t1 - t0
            select case (this%iMethod)
            case (STEPWISE)
              do i=1,nvals
                ! -- compute area of a rectangle
                value0 = currRecord%taArray(i)
                area = value0 * t01
                ! -- add area to integrated value
                values(i) = values(i) + area
              enddo
            case (LINEAR)
              do i=1,nvals
                ! -- compute area of a trapezoid
                timediff = nextTime - currTime
                ratio0 = (t0 - currTime) / timediff
                ratio1 = (t1 - currTime) / timediff
                valuediff = nextRecord%taArray(i) - currRecord%taArray(i)
                value0 = currRecord%taArray(i) + ratio0 * valuediff
                value1 = currRecord%taArray(i) + ratio1 * valuediff
                area = 0.5d0 * t01 * (value0 + value1)
                ! -- add area to integrated value
                values(i) = values(i) + area
              enddo
            end select
          else
            write(errmsg,10)trim(this%Name),time0,time1
            call store_error(errmsg)
            call store_error('(Probable programming error)', terminate=.TRUE.)
          endif
        else
          ! Current node time = time1 so should be done
          ldone = .true.
        endif
        !
        ! -- Are we done yet?
        if (t1 >= time1) then
          ldone = .true.
        else
          if (.not. associated(currNode%nextNode)) then
            ! -- try to read the next array
            if (.not. this%read_next_array()) then
              write(errmsg,10)trim(this%Name),time0,time1
              call store_error(errmsg)
              call this%parser%StoreErrorUnit()
            endif
          endif
          if (associated(currNode%nextNode)) then
            currNode => currNode%nextNode
          else
            write(errmsg,10)trim(this%Name),time0,time1
            call store_error(errmsg)
            call store_error('(Probable programming error)', terminate=.TRUE.)
          endif
        endif
      enddo
    endif
    !
    if (this%autoDeallocate) then
      if (associated(precNode)) then
        if (associated(precNode%prevNode))then
          call this%DeallocateBackward(precNode%prevNode)
        endif
      endif
    endif
    !
    return
  end subroutine get_integrated_values

  subroutine DeallocateBackward(this, fromNode)
! ******************************************************************************
! DeallocateBackward -- Deallocate fromNode and all previous nodes in list; 
!   reassign firstNode.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(TimeArraySeriesType),  intent(inout) :: this
    type(ListNodeType), pointer, intent(inout) :: fromNode
    !
    ! -- local
    type(ListNodeType),  pointer :: current => null()
    type(ListNodeType),  pointer :: prev => null()
    type(TimeArrayType), pointer :: ta => null()
    class(*),            pointer :: obj => null()
! ------------------------------------------------------------------------------
    !
    if (associated(fromNode)) then
      ! -- reassign firstNode
      if (associated(fromNode%nextNode)) then
        this%list%firstNode => fromNode%nextNode
      else
        this%list%firstNode => null()
      endif
      ! -- deallocate fromNode and all previous nodes
      current => fromNode
      do while (associated(current))
        prev => current%prevNode
        obj => current%GetItem()
        ta => CastAsTimeArrayType(obj)
        ! -- Deallocate the contents of this time array,
        !    then remove it from the list
        call ta%da()
        call this%list%RemoveNode(current, .true.)
        current => prev
      enddo
      fromNode => null()
    endif
    !
    return
  end subroutine DeallocateBackward

  subroutine get_latest_preceding_node(this, time, tslNode)
! ******************************************************************************
! get_latest_preceding_node -- Return pointer to ListNodeType object for the 
!    node representing the latest preceding time in the time series
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(TimeArraySeriesType),  intent(inout) :: this
    real(DP),            intent(in)    :: time
    type(ListNodeType), pointer, intent(inout) :: tslNode
    ! -- local
    real(DP) :: time0
    type(ListNodeType),  pointer :: currNode => null()
    type(ListNodeType),  pointer :: node0 => null()
    type(TimeArrayType), pointer :: ta => null()
    type(TimeArrayType), pointer :: ta0 => null()
    class(*),            pointer :: obj => null()
! ------------------------------------------------------------------------------
    !
    tslNode => null()
    if (associated(this%list%firstNode)) then
      currNode => this%list%firstNode
    else
      call store_error('probable programming error in get_latest_preceding_node', &
                       terminate=.TRUE.)
    endif
    !
    continue
    ! -- If the next node is earlier than time of interest, advance along
    !    linked list until the next node is later than time of interest.
    do
      if (associated(currNode)) then
        if (associated(currNode%nextNode)) then
          obj => currNode%nextNode%GetItem()
          ta => CastAsTimeArrayType(obj)
          if (ta%taTime < time  .or. is_same(ta%taTime, time)) then
            currNode => currNode%nextNode
          else
            exit
          endif
        else
          ! -- read another record
          if (.not. this%read_next_array()) exit
        endif
      else
        exit
      endif
    enddo
    !
    if (associated(currNode)) then
      !
      ! -- find earlier record
      node0 => currNode
      obj => node0%GetItem()
      ta0 => CastAsTimeArrayType(obj)
      time0 = ta0%taTime
      do while (time0 > time)
        if (associated(node0%prevNode)) then
          node0 => node0%prevNode
          obj => node0%GetItem()
          ta0 => CastAsTimeArrayType(obj)
          time0 = ta0%taTime
        else
          exit
        endif
      enddo
    endif
    !
    if (time0 <= time) tslNode => node0
    !
    return
  end subroutine get_latest_preceding_node

  subroutine tas_da(this)
! ******************************************************************************
! tas_da -- deallocate
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(TimeArraySeriesType), intent(inout) :: this
    ! -- local
    integer :: i, n
    type(TimeArrayType), pointer :: ta => null()
! ------------------------------------------------------------------------------
    !
    ! -- Deallocate contents of each time array in list
    n = this%list%Count()
    do i=1,n
      ta => GetTimeArrayFromList(this%list, i)
      call ta%da()
    enddo
    !
    ! -- Deallocate the list of time arrays
    call this%list%Clear(.true.)
    deallocate(this%list)
    !
    return
  end subroutine tas_da

  ! -- Procedures not type-bound

  function CastAsTimeArraySeriesType(obj) result (res)
! ******************************************************************************
! CastAsTimeArraySeriesType -- Cast an unlimited polymorphic object as 
!   class(TimeArraySeriesType)
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(*),   pointer, intent(inout) :: obj
    type(TimeArraySeriesType), pointer :: res
! ------------------------------------------------------------------------------
    !
    res => null()
    if (.not. associated(obj)) return
    !
    select type (obj)
    type is (TimeArraySeriesType)
      res => obj
    end select
    !
    return
  end function CastAsTimeArraySeriesType

  function GetTimeArraySeriesFromList(list, indx) result (res)
! ******************************************************************************
! GetTimeArraySeriesFromList -- get time array from list
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    type(ListType),          intent(inout) :: list
    integer,                 intent(in)    :: indx
    type(TimeArraySeriesType), pointer :: res
    ! -- local
    class(*), pointer :: obj
! ------------------------------------------------------------------------------
    !
    obj => list%GetItem(indx)
    res => CastAsTimeArraySeriesType(obj)
    !
    return
  end function GetTimeArraySeriesFromList

end module TimeArraySeriesModule
