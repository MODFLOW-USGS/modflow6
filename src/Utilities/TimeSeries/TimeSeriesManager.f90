module TimeSeriesManagerModule

  use KindModule,                only: DP, I4B
  use ConstantsModule,           only: DZERO, LENPACKAGENAME, MAXCHARLEN, &
                                       LINELENGTH, LENTIMESERIESNAME
  use HashTableModule,           only: HashTableType
  use InputOutputModule,         only: same_word, UPCASE
  use ListModule,                only: ListType
  use SimModule,                 only: store_error, store_error_unit, ustop
  use TdisModule,                only: delt, kper, kstp, totim, totimc, &
                                       totimsav
  use TimeSeriesFileListModule, only: TimeSeriesFileListType
  use TimeSeriesLinkModule,      only: TimeSeriesLinkType, &
                                       ConstructTimeSeriesLink, &
                                       GetTimeSeriesLinkFromList, &
                                       AddTimeSeriesLinkToList
  use TimeSeriesModule,          only: TimeSeriesContainerType, &
                                       TimeSeriesFileType, &
                                       TimeSeriesType

  implicit none

  private
  public :: TimeSeriesManagerType, read_value_or_time_series, &
            read_single_value_or_time_series, tsmanager_cr

  type TimeSeriesManagerType
    integer(I4B), public :: iout = 0                                             ! output unit number
    type(TimeSeriesFileListType), pointer, public :: tsfileList => null()        ! list of ts files objs
    type(ListType), pointer, public :: boundTsLinks => null()                    ! links to bound and aux
    integer(I4B) :: numtsfiles = 0       ! number of ts files
    character(len=MAXCHARLEN), allocatable, dimension(:) :: tsfiles              ! list of ts files
    type(ListType), pointer, private :: auxvarTsLinks => null()                  ! list of aux links
    type(HashTableType), private :: BndTsHashTable                               ! hash of ts to tsobj
    type(TimeSeriesContainerType), allocatable, dimension(:),                   &
                                   private :: TsContainers
  contains
    ! -- Public procedures
    procedure, public :: tsmanager_df
    procedure, public :: ad => tsmgr_ad
    procedure, public :: da => tsmgr_da
    procedure, public :: add_tsfile
    procedure, public :: CountLinks
    procedure, public :: GetLink
    procedure, public :: Reset
    procedure, public :: HashBndTimeSeries
    ! -- Private procedures
    procedure, private :: get_time_series
    procedure, private :: make_link
  end type TimeSeriesManagerType

  contains
  
  subroutine tsmanager_cr(this, iout)
! ******************************************************************************
! tsmanager_cr -- create the tsmanager
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    type(TimeSeriesManagerType) :: this
    integer(I4B), intent(in) :: iout
! ------------------------------------------------------------------------------
    !
    this%iout = iout
    allocate(this%boundTsLinks)
    allocate(this%auxvarTsLinks)
    allocate(this%tsfileList)
    allocate(this%tsfiles(1000))
    !
    return
  end subroutine tsmanager_cr
  
  subroutine tsmanager_df(this)
! ******************************************************************************
! tsmanager_df -- define
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(TimeSeriesManagerType) :: this
! ------------------------------------------------------------------------------
    !
    if (this%numtsfiles > 0) then
      call this%HashBndTimeSeries(this%numtsfiles)
    endif
    !
    ! -- return
    return
  end subroutine tsmanager_df
                                              
  subroutine add_tsfile(this, fname, inunit)
! ******************************************************************************
! add_tsfile -- add a time series file to this manager
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use SimModule, only: store_error, store_error_unit, ustop
    use ArrayHandlersModule, only: ExpandArray
    ! -- dummy
    class(TimeSeriesManagerType) :: this
    character(len=*), intent(in) :: fname
    integer(I4B), intent(in) :: inunit
    ! -- local
    integer(I4B) :: isize
    integer(I4B) :: i
    class(TimeSeriesFileType), pointer :: tsfile => null()
! ------------------------------------------------------------------------------
    !
    ! -- Check for fname duplicates
    if (this%numtsfiles > 0) then
      do i = 1, this%numtsfiles
        if (this%tsfiles(i) == fname) then
          call store_error('Found duplicate time-series file name: ' // trim(fname))
          call store_error_unit(inunit)
          call ustop()
        endif
      enddo
    endif
    !
    ! -- Save fname
    this%numtsfiles = this%numtsfiles + 1
    isize = size(this%tsfiles)
    if (this%numtsfiles > isize) then
      call ExpandArray(this%tsfiles, 1000)
    endif
    this%tsfiles(this%numtsfiles) = fname
    !
    ! -- 
    call this%tsfileList%Add(fname, this%iout, tsfile)
    !
    return
  end subroutine add_tsfile
  
  subroutine tsmgr_ad(this)
! ******************************************************************************
! tsmgr_ad -- time step (or subtime step) advance. Call this each time step or 
!   subtime step.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(TimeSeriesManagerType) :: this
    ! -- local
    type(TimeSeriesLinkType), pointer :: tsLink => null()
    type(TimeSeriesType), pointer :: timeseries => null()
    integer(I4B) :: i, nlinks, nauxlinks
    real(DP) :: begintime, endtime
    character(len=LENPACKAGENAME+2) :: pkgID
    ! formats
    character(len=*),parameter :: fmt5 =                                       &
      &"(/,'Time-series controlled values in stress period: ', i0,             &
        &', time step ', i0, ':')"
    10  format(a,' package: Boundary ',i0,', entry ',i0, ' value from time series "',a,'" = ',g12.5)
    15  format(a,' package: Boundary ',i0,', entry ',i0,' value from time series "',a,'" = ',g12.5,' (',a,')')
    20  format(a,' package: Boundary ',i0,', ',a,' value from time series "',a,'" = ',g12.5)
    25  format(a,' package: Boundary ',i0,', ',a,' value from time series "',a,'" = ',g12.5,' (',a,')')
! ------------------------------------------------------------------------------
    !
    ! -- Initialize time variables
    begintime = totimc
    endtime = begintime + delt
    !
    ! -- Iterate through boundtslinks and replace specified
    !    elements of bound with average value obtained from
    !    appropriate time series. (For list-type packages)
    nlinks = this%boundtslinks%Count()
    nauxlinks = this%auxvartslinks%Count()
    do i = 1, nlinks
      tsLink => GetTimeSeriesLinkFromList(this%boundTsLinks, i)
      if (i == 1) then
        if (tsLink%Iprpak == 1) then
          write(this%iout, fmt5) kper, kstp
        endif
      endif
      ! this part needs to be different for MAW because MAW does not use
      ! bound array for well rate (although rate is stored in
      ! this%bound(4,ibnd)), it uses this%mawwells(n)%rate%value
      if (tsLink%UseDefaultProc) then
        timeseries => tsLink%timeSeries
        tsLink%BndElement = timeseries%GetValue(begintime, endtime)
!        ! -- If multiplier is active and it applies to this element, do the multiplication
        if (associated(tsLink%RMultiplier)) then
          tsLink%BndElement = tsLink%BndElement * tsLink%RMultiplier
        endif
!        ! Ned TODO: Need a flag to control output of values generated from time series
!        ! Also need to format as a table? Otherwise, just remove this if block.
        if (tsLink%Iprpak == 1) then
          pkgID = '"' // trim(tsLink%PackageName) // '"'
          if (tsLink%Text == '') then
            if (tsLink%BndName == '') then
              write(this%iout,10)trim(pkgID), tsLink%IRow, tsLink%JCol, &
                                 trim(tsLink%timeSeries%Name), &
                                 tsLink%BndElement
            else
              write(this%iout,15)trim(pkgID), tsLink%IRow, tsLink%JCol, &
                                 trim(tsLink%timeSeries%Name), &
                                 tsLink%BndElement, trim(tsLink%BndName)
            endif
          else
            if (tsLink%BndName == '') then
              write(this%iout,20)trim(pkgID), tsLink%IRow, trim(tsLink%Text), &
                                 trim(tsLink%timeSeries%Name), &
                                 tsLink%BndElement
            else
              write(this%iout,25)trim(pkgID), tsLink%IRow, trim(tsLink%Text), &
                                 trim(tsLink%timeSeries%Name), &
                                 tsLink%BndElement, trim(tsLink%BndName)
            endif
          endif
        endif
        !
        ! -- If conversion from flux to flow is required, multiply by cell area
        if (tsLink%ConvertFlux) then
          tsLink%BndElement = tsLink%BndElement * tsLink%CellArea
        endif
      endif
      !if (i==nlinks) then
      !  write(this%iout,'()')
      !endif
    enddo
    !
    ! -- Iterate through auxvartslinks and replace specified
    !    elements of auxvar with average value obtained from
    !    appropriate time series.
    do i=1,nauxlinks
      tsLink => GetTimeSeriesLinkFromList(this%auxvarTsLinks, i)
      timeseries => tsLink%timeSeries
      if (i==1 .and. nlinks==0) then
        if (tsLink%Iprpak == 1) then
          write(this%iout, fmt5) kper, kstp
        endif
      endif
      tsLink%BndElement = timeseries%GetValue(begintime, endtime)
!     ! Ned TODO: Need a flag to control output of values generated from time series
!     ! Also need to format as a table? Otherwise, just remove this if block.
      if (tsLink%Iprpak == 1) then
        pkgID = '"' // trim(tsLink%PackageName) // '"'
        if (tsLink%Text == '') then
          if (tsLink%BndName == '') then
            write(this%iout,10)trim(pkgID), tsLink%IRow, tsLink%JCol, &
                                trim(tsLink%timeSeries%Name), &
                                tsLink%BndElement
          else
            write(this%iout,15)trim(pkgID), tsLink%IRow, tsLink%JCol, &
                                trim(tsLink%timeSeries%Name), &
                                tsLink%BndElement, trim(tsLink%BndName)
          endif
        else
          if (tsLink%BndName == '') then
            write(this%iout,20)trim(pkgID), tsLink%IRow, trim(tsLink%Text), &
                                trim(tsLink%timeSeries%Name), &
                                tsLink%BndElement
          else
            write(this%iout,25)trim(pkgID), tsLink%IRow, trim(tsLink%Text), &
                                trim(tsLink%timeSeries%Name), &
                                tsLink%BndElement, trim(tsLink%BndName)
          endif
        endif
      endif
    enddo
    if (nlinks + nauxlinks > 0) then
      if (tsLink%Iprpak == 1) then
        write(this%iout,'()')
      endif
    end if
    !
    return
  end subroutine tsmgr_ad

  subroutine tsmgr_da(this)
! ******************************************************************************
! tsmgr_da -- deallocate
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(TimeSeriesManagerType) :: this
    ! -- local
! ------------------------------------------------------------------------------
    !
    ! -- Deallocate time-series links in boundTsLinks
    call this%boundTsLinks%Clear(.true.)
    deallocate(this%boundTsLinks)
    !
    ! -- Deallocate time-series links in auxvarTsLinks
    call this%auxvarTsLinks%Clear(.true.)
    deallocate(this%auxvarTsLinks)
    !
    ! -- Deallocate tsfileList
    call this%tsfileList%da()
    deallocate(this%tsfileList)
    !
    ! -- Deallocate the hash table
    call this%BndTsHashTable%FreeHash()
    !
    deallocate(this%tsfiles)
    !
    return
  end subroutine tsmgr_da

  subroutine Reset(this, pkgName)
! ******************************************************************************
! reset -- Call this when a new BEGIN PERIOD block is read for a new stress
!    period.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(TimeSeriesManagerType) :: this
    character(len=*), intent(in) :: pkgName
    ! -- local
    integer(I4B)                      :: i, nlinks
    type(TimeSeriesLinkType), pointer :: tslink
! ------------------------------------------------------------------------------
    ! Zero out values for time-series controlled stresses.
    ! Also deallocate all tslinks too.
    ! Then when time series are
    ! specified in this or another stress period,
    ! a new tslink would be set up.
    !
    ! Reassign all linked elements to zero
    nlinks = this%boundTsLinks%Count()
    do i=1,nlinks
      tslink => GetTimeSeriesLinkFromList(this%boundTsLinks, i)
      if (associated(tslink)) then
        if (tslink%PackageName == pkgName) then
          tslink%BndElement = DZERO
        endif
      endif
    enddo
    !
    ! Remove links belonging to calling package
    nlinks = this%boundTsLinks%Count()
    do i=nlinks,1,-1
      tslink => GetTimeSeriesLinkFromList(this%boundTsLinks, i)
      if (associated(tslink)) then
        if (tslink%PackageName == pkgName) then
          call this%boundTsLinks%RemoveNode(i, .true.)
        endif
      endif
    enddo
    nlinks = this%auxvarTsLinks%Count()
    do i=nlinks,1,-1
      tslink => GetTimeSeriesLinkFromList(this%auxvarTsLinks,i)
      if (associated(tslink)) then
        if (tslink%PackageName == pkgName) then
          call this%auxvarTsLinks%RemoveNode(i, .true.)
        endif
      endif
    enddo
    !
    return
  end subroutine Reset

  subroutine make_link(this, timeSeries, pkgName, auxOrBnd, bndElem, &
                       irow, jcol, iprpak, tsLink, text, bndName)
! ******************************************************************************
! make_link -- 
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(TimeSeriesManagerType),      intent(inout) :: this
    type(TimeSeriesType),     pointer, intent(inout) :: timeSeries
    character(len=*),                  intent(in)    :: pkgName
    character(len=3),                  intent(in)    :: auxOrBnd
    real(DP),                 pointer, intent(inout) :: bndElem
    integer(I4B),                      intent(in)    :: irow, jcol
    integer(I4B),                      intent(in)    :: iprpak
    type(TimeSeriesLinkType), pointer, intent(inout) :: tsLink
    character(len=*),                  intent(in)    :: text
    character(len=*),                  intent(in)    :: bndName
    ! -- local
! ------------------------------------------------------------------------------
    !
    tsLink => null()
    call ConstructTimeSeriesLink(tsLink, timeSeries, pkgName, &
                                 auxOrBnd, bndElem, irow, jcol, iprpak)
    if (associated(tsLink)) then
      if (auxOrBnd == 'BND') then
        call AddTimeSeriesLinkToList(this%boundTsLinks, tsLink)
      elseif (auxOrBnd == 'AUX') then
        call AddTimeSeriesLinkToList(this%auxvarTsLinks, tsLink)
      else
        call ustop('programmer error in make_link')
      endif
      tsLink%Text = text
      tsLink%BndName = bndName
    endif
    !
    return
  end subroutine make_link

  function GetLink(this, auxOrBnd, indx) result(tsLink)
! ******************************************************************************
! GetLink -- 
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(TimeSeriesManagerType) :: this
    character(len=3), intent(in) :: auxOrBnd
    integer(I4B),          intent(in) :: indx
    type(TimeSeriesLinkType), pointer :: tsLink
    ! -- local
    type(ListType), pointer :: list
! ------------------------------------------------------------------------------
    !
    list => null()
    tsLink => null()
    !
    select case (auxOrBnd)
    case ('AUX')
      list => this%auxvarTsLinks
    case ('BND')
      list => this%boundTsLinks
    end select
    !
    if (associated(list)) then
      tsLink => GetTimeSeriesLinkFromList(list, indx)
    endif
    !
    return
  end function GetLink

  function CountLinks(this, auxOrBnd)
! ******************************************************************************
! CountLinks -- 
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- return
    integer(I4B) :: CountLinks
    ! -- dummy
    class(TimeSeriesManagerType) :: this
    character(len=3), intent(in) :: auxOrBnd
! ------------------------------------------------------------------------------
    !
    CountLinks = 0
    if (auxOrBnd == 'BND') then
      CountLinks = this%boundTsLinks%Count()
    elseif (auxOrBnd == 'AUX') then
      CountLinks = this%auxvarTsLinks%count()
    endif
    !
    return
  end function CountLinks

  function get_time_series(this, name) result (res)
! ******************************************************************************
! get_time_series -- 
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(TimeSeriesManagerType)  :: this
    character(len=*), intent(in)  :: name
    ! -- result
    type(TimeSeriesType), pointer :: res
    ! -- local
    integer(I4B) :: indx
! ------------------------------------------------------------------------------
    !
    ! Get index from hash table, get time series from TsContainers,
    !     and assign result to time series contained in link.
    res => null()
    call this%BndTsHashTable%GetHash(name, indx)
    if (indx > 0) then
      res => this%TsContainers(indx)%timeSeries
    endif
    !
    return
  end function get_time_series

  subroutine HashBndTimeSeries(this, ivecsize)
! ******************************************************************************
! HashBndTimeSeries -- 
!   Store all boundary (stress) time series links in
!   TsContainers and construct hash table BndTsHashTable.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class (TimeSeriesManagerType), intent(inout) :: this
    integer(I4B), intent(in) :: ivecsize
    ! -- local
    integer(I4B) :: i, j, k, numtsfiles, numts
    character(len=LENTIMESERIESNAME) :: name
    type(TimeSeriesFileType), pointer :: tsfile => null()
! ------------------------------------------------------------------------------
    !
    ! Initialize the hash table
    call this%BndTsHashTable%InitHash(ivecsize)
    !
    ! Allocate the TsContainers array to accommodate all time-series links.
    numts = this%tsfileList%CountTimeSeries()
    allocate(this%TsContainers(numts))
    !
    ! Store a pointer to each time series in the TsContainers array
    ! and put its key (time-series name) and index in the hash table.
    numtsfiles = this%tsfileList%Counttsfiles()
    k = 0
    do i = 1, numtsfiles
      tsfile => this%tsfileList%Gettsfile(i)
      numts = tsfile%Count()
      do j=1,numts
        k = k + 1
        this%TsContainers(k)%timeSeries => tsfile%GetTimeSeries(j)
        if (associated(this%TsContainers(k)%timeSeries)) then
          name = this%TsContainers(k)%timeSeries%Name
          call this%BndTsHashTable%PutHash(name, k)
        endif
      enddo
    enddo
    !
    return
  end subroutine HashBndTimeSeries

  ! -- Non-type-bound procedures

  subroutine read_value_or_time_series(textInput, ii, jj, bndElem, &
                 pkgName, auxOrBnd, tsManager, iprpak, tsLink)
! ******************************************************************************
! read_value_or_time_series -- 
!   Call this subroutine if the time-series link is available or needed.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    character(len=*),                  intent(in)    :: textInput
    integer(I4B),                      intent(in)    :: ii
    integer(I4B),                      intent(in)    :: jj
    real(DP), pointer,                 intent(inout) :: bndElem
    character(len=*),                  intent(in)    :: pkgName
    character(len=3),                  intent(in)    :: auxOrBnd
    type(TimeSeriesManagerType),       intent(inout) :: tsManager
    integer(I4B),                      intent(in)    :: iprpak
    type(TimeSeriesLinkType), pointer, intent(inout) :: tsLink
    ! -- local
    type(TimeSeriesType),     pointer :: timeseries => null()
    type(TimeSeriesLinkType), pointer :: tslTemp => null()
    integer(I4B)              :: i, istat, nlinks
    real(DP)                  :: r
    character(len=LINELENGTH) :: ermsg
    character(len=LENTIMESERIESNAME) :: tsNameTemp
    logical :: found
! ------------------------------------------------------------------------------
    !
    read (textInput,*,iostat=istat) r
    if (istat == 0) then
      bndElem = r
    else
      tsNameTemp = textInput
      call UPCASE(tsNameTemp)
      ! -- If text is a time-series name, get value
      !    from time series.
      timeseries => tsManager%get_time_series(tsNameTemp)
      ! -- Create a time series link and add it to the package
      !    list of time series links used by the array.
      if (associated(timeseries)) then
        ! -- Assign value from time series to current
        !    array element
        r = timeseries%GetValue(totimsav, totim)
        bndElem = r
        ! Look to see if this array element already has a time series
        ! linked to it.  If not, make a link to it.
        nlinks = tsManager%CountLinks(auxOrBnd)
        found = .false.
        searchlinks: do i=1,nlinks
          tslTemp => tsManager%GetLink(auxOrBnd, i)
          if (tslTemp%PackageName == pkgName) then
              ! -- Check ii, jj against iRow, jCol stored in link
              if (tslTemp%IRow==ii .and. tslTemp%JCol==jj) then
                ! -- This array element is already linked to a time series.
                tsLink => tslTemp
                found = .true.
                exit searchlinks
              endif
          endif
        enddo searchlinks
        if (.not. found) then
          ! -- Link was not found. Make one and add it to the list.
          call tsManager%make_link(timeseries, pkgName, auxOrBnd, bndElem, &
                                   ii, jj, iprpak, tsLink, '', '')
        endif
      else
        ermsg = 'Error in list input. Expected numeric value or ' // &
                  'time-series name, but found: ' // trim(textInput)
        call store_error(ermsg)
      endif
    endif
  end subroutine read_value_or_time_series

  subroutine read_single_value_or_time_series(textInput, bndElem, name, endtim,&
                                              pkgName, auxOrBnd, tsManager, &
                                              iprpak, ii, jj, linkText, &
                                              bndName, inunit)
! ******************************************************************************
! read_single_value_or_time_series -- 
!    Call this subroutine if the time-series link is NOT available or
!    needed and if you need to select the link by its Text member.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    character(len=*),            intent(in)    :: textInput
    real(DP), pointer,           intent(inout) :: bndElem
    character (len=*),           intent(inout) :: name
    real(DP),                    intent(in)    :: endtim
    character(len=*),            intent(in)    :: pkgName
    character(len=3),            intent(in)    :: auxOrBnd
    type(TimeSeriesManagerType), intent(inout) :: tsManager
    integer(I4B),                intent(in)    :: iprpak
    integer(I4B),                intent(in)    :: ii
    integer(I4B),                intent(in)    :: jj
    character(len=*),            intent(in)    :: linkText
    character(len=*),            intent(in)    :: bndName
    integer(I4B),                intent(in)    :: inunit
    ! -- local
    integer(I4B) :: i, istat, nlinks
    real(DP) :: v
    character(len=LINELENGTH) :: ermsg
    character(len=LENTIMESERIESNAME) :: tsNameTemp
    logical :: found
    integer(I4B) :: removeLink
    type(TimeSeriesType),     pointer :: timeseries => null()
    type(TimeSeriesLinkType), pointer :: tslTemp => null()
    type(TimeSeriesLinkType), pointer :: tsLink => null()
! ------------------------------------------------------------------------------
    !
    name = ''
    read (textInput, *, iostat=istat) v
    if (istat == 0) then
      ! Numeric value was successfully read.
      bndElem = v
      ! Look to see if this array element already has a time series
      ! linked to it.  If so, remove the link.
      nlinks = tsManager%CountLinks(auxOrBnd)
      found = .false.
      removeLink = -1
      csearchlinks: do i=1,nlinks
        tslTemp => tsManager%GetLink(auxOrBnd, i)
        if (tslTemp%PackageName == pkgName) then
          ! -- Check ii against iRow, linkText against Text member of link
          if (tslTemp%IRow==ii .and. same_word(tslTemp%Text,linkText)) then
            ! -- This array element is already linked to a time series.
            found = .true.
            removeLink = i
            exit csearchlinks
          endif
        endif
      enddo csearchlinks
      if (found) then
        if (removeLink > 0) then
          if (auxOrBnd == 'BND') then
            call tsManager%boundTsLinks%RemoveNode(removeLink, .true.)
          else if (auxOrBnd == 'AUX') then
            call tsManager%auxvarTsLinks%RemoveNode(removeLink, .true.)
          end if
        end if
      end if
    else
      ! Attempt to read numeric value from textInput failed.
      ! Text should be a time-series name.
      tsNameTemp = textInput
      call UPCASE(tsNameTemp)
      ! -- If textInput is a time-series name, get average value
      !    from time series.
      timeseries => tsManager%get_time_series(tsNameTemp)
      ! -- Create a time series link and add it to the package
      !    list of time series links used by the array.
      if (associated(timeseries)) then
        ! -- Assign average value from time series to current
        !    array element
        v = timeseries%GetValue(totim, endtim)
        bndElem = v
        name = tsNameTemp
        ! Look to see if this array element already has a time series
        ! linked to it.  If not, make a link to it.
        nlinks = tsManager%CountLinks(auxOrBnd)
        found = .false.
        removeLink = -1
        searchlinks: do i=1,nlinks
          tslTemp => tsManager%GetLink(auxOrBnd, i)
          if (tslTemp%PackageName == pkgName) then
            ! -- Check ii against iRow, linkText against Text member of link
            if (tslTemp%IRow==ii .and. same_word(tslTemp%Text,linkText)) then
              if (tslTemp%timeseries%name==tsNameTemp) then
                ! -- This array element is already linked to a time series.
                found = .true.
                exit searchlinks
              else
                if (tslTemp%auxOrBnd == auxOrBnd) then
                  removeLink = i
                end if
              end if
            endif
          endif
        enddo searchlinks
        if (.not. found) then
          if (removeLink > 0) then
            if (auxOrBnd == 'BND') then
              call tsManager%boundTsLinks%RemoveNode(removeLink, .true.)
            else if (auxOrBnd == 'AUX') then
              call tsManager%auxvarTsLinks%RemoveNode(removeLink, .true.)
            end if
          end if
          ! -- Link was not found. Make one and add it to the list.
          call tsManager%make_link(timeseries, pkgName, auxOrBnd, bndElem, &
                                   ii, jj, iprpak, tsLink, linkText, bndName)
          !! -- update array element
          !v = timeseries%GetValue(totim, endtim)
          !bndElem = v
        endif
      else
        ermsg = 'Error in list input. Expected numeric value or ' // &
                  'time-series name, but found: ' // trim(textInput)
        call store_error(ermsg)
        call store_error_unit(inunit)
        call ustop()
      end if
    end if
    return
  end subroutine read_single_value_or_time_series

end module TimeSeriesManagerModule
