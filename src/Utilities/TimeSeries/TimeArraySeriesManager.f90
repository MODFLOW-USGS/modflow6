module TimeArraySeriesManagerModule

  use KindModule,                only: DP, I4B
  use SimVariablesModule,        only: errmsg
  use ConstantsModule,           only: DZERO, LENTIMESERIESNAME, LINELENGTH, &
                                       LENHUGELINE
  use ListModule,                only: ListType
  use SimModule,                 only: store_error, store_error_unit
  use TdisModule,                only: delt, totimc, kper, kstp
  use TimeArraySeriesLinkModule, only: TimeArraySeriesLinkType, &
                                       ConstructTimeArraySeriesLink, &
                                       GetTimeArraySeriesLinkFromList, &
                                       AddTimeArraySeriesLinkToList
  use TimeArraySeriesModule,     only: TimeArraySeriesType
  use BaseDisModule,             only: DisBaseType

  implicit none

  private
  public :: TimeArraySeriesManagerType, tasmanager_cr

  type TimeArraySeriesManagerType
    ! -- Public members
    integer(I4B), public :: iout = 0                                             ! output unit num
    class(DisBaseType), pointer, public :: dis => null()                         ! pointer to dis
    ! -- Private members
    type(ListType), pointer, private :: boundTasLinks => null()                  ! list of TAS links
    character(len=LINELENGTH), allocatable, dimension(:)  :: tasfiles            ! list of TA file names
    type(TimeArraySeriesType), dimension(:), pointer, contiguous :: taslist      ! array of TA pointers
    character(len=LENTIMESERIESNAME), allocatable, dimension(:) :: tasnames      ! array of TA names
  contains
    ! -- Public procedures
    procedure, public :: tasmanager_df
    procedure, public :: ad => tasmgr_ad
    procedure, public :: da => tasmgr_da
    procedure, public :: add_tasfile
    procedure, public :: CountLinks
    procedure, public :: GetLink
    procedure, public :: MakeTasLink
    procedure, public :: Reset
    ! -- Private procedures
    procedure, private :: tasmgr_add_link
    procedure, private :: tasmgr_convert_flux
  end type TimeArraySeriesManagerType

contains

! -- Type-bound procedures of TimeArraySeriesManagerType

  ! -- Public procedures

  subroutine tasmanager_cr(this, dis, iout)
! ******************************************************************************
! tasmanager_cr -- create the tasmanager
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    type(TimeArraySeriesManagerType) :: this
    class(DisBaseType), pointer :: dis
    integer(I4B), intent(in) :: iout
! ------------------------------------------------------------------------------
    !
    this%iout = iout
    this%dis => dis
    allocate(this%boundTasLinks)
    allocate(this%tasfiles(0))
    !
    return
  end subroutine tasmanager_cr
  
  subroutine tasmanager_df(this)
! ******************************************************************************
! tasmanager_df -- define
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(TimeArraySeriesManagerType) :: this
    ! -- local
    type(TimeArraySeriesType), pointer :: tasptr => null()
    integer(I4B) :: nfiles
    integer(I4B) :: i
! ------------------------------------------------------------------------------
    !
    ! -- determine how many tasfiles.  This is the number of time array series
    !    so allocate arrays to store them
    nfiles = size(this%tasfiles)
    allocate(this%taslist(nfiles))
    allocate(this%tasnames(nfiles))
    !
    ! -- Setup a time array series for each file specified
    do i = 1, nfiles
      tasptr => this%taslist(i)
      call tasptr%tas_init(this%tasfiles(i), this%dis,           &
        this%iout, this%tasnames(i))
    enddo
    !
    return
  end subroutine tasmanager_df
  
  subroutine tasmgr_ad(this)
! ******************************************************************************
! tasmgr_ad -- time step (or subtime step) advance.
!   Call this each time step or subtime step.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(TimeArraySeriesManagerType) :: this
    ! -- local
    type(TimeArraySeriesLinkType), pointer :: tasLink => null()
    type(TimeArraySeriesType), pointer :: timearrayseries => null()
    integer(I4B) :: i, j, nlinks, nvals, isize1, isize2, inunit
    real(DP) :: begintime, endtime
    ! formats
    character(len=*),parameter :: fmt5 =                                       &
      "(/,'Time-array-series controlled arrays in stress period ',             &
      &i0, ', time step ', i0, ':')"
10  format('"',a, '" package: ',a,' array obtained from time-array series "',a,'"')
! ------------------------------------------------------------------------------
    !
    ! -- Initialize time variables
    begintime = totimc
    endtime = begintime + delt
    !
    ! -- Iterate through boundtaslinks and update specified
    !    array with array of average values obtained from
    !    appropriate time series.
    if (associated(this%boundTasLinks)) then
      nlinks = this%boundTasLinks%Count()
      do i = 1, nlinks
        tasLink => GetTimeArraySeriesLinkFromList(this%boundTasLinks, i)
        if (tasLink%Iprpak == 1 .and. i==1) then
          write(this%iout, fmt5) kper, kstp
        endif
        if (tasLink%UseDefaultProc) then
          timearrayseries => tasLink%timeArraySeries
          nvals = size(tasLink%BndArray)
          !
          ! -- Fill the package array with integrated values
          call timearrayseries%GetAverageValues(nvals, tasLink%BndArray, &
                                                begintime, endtime)
          !
          ! -- If conversion from flux to flow is required, multiply by cell area
          if (tasLink%ConvertFlux) then
            call this%tasmgr_convert_flux(tasLink)
          endif
          !
          ! -- If PRINT_INPUT is specified, write information
          !    regarding source of time-array series data
          if (tasLink%Iprpak == 1) then
            write(this%iout,10) trim(tasLink%PackageName),                  &
                                   trim(tasLink%Text),                         &
                                   trim(tasLink%timeArraySeries%Name)
          endif
        endif
        if (i == nlinks) then
          write(this%iout, '()')
        endif
      enddo
      !
      ! -- Now that all array values have been substituted, can now multiply
      !    an array by a multiplier array
      do i = 1, nlinks
        tasLink => GetTimeArraySeriesLinkFromList(this%boundTasLinks, i)
        if (tasLink%UseDefaultProc) then
          if (associated(tasLink%RMultArray)) then
            isize1 = size(tasLink%BndArray)
            isize2 = size(tasLink%RMultArray)
            if (isize1 == isize2 .and. isize1 == nvals) then
              do j = 1, nvals
                tasLink%BndArray(j) = tasLink%BndArray(j) * tasLink%RMultArray(j)
              enddo
            else
              errmsg = 'Size mismatch between boundary and multiplier arrays' // &
                       ' using time-array series: ' // &
                       trim(tasLink%TimeArraySeries%Name)
              call store_error(errmsg)
              inunit = tasLink%TimeArraySeries%GetInunit()
              call store_error_unit(inunit)
            endif
          endif
        endif
      enddo
    endif
    !
    return
  end subroutine tasmgr_ad

  subroutine tasmgr_da(this)
! ******************************************************************************
! tasmgr_da -- deallocate
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(TimeArraySeriesManagerType) :: this
    ! -- local
    integer :: i, n
    type(TimeArraySeriesLinkType), pointer :: tasLink => null()
! ------------------------------------------------------------------------------
    !
    ! -- Deallocate contents of each TimeArraySeriesType object in list
    !    of time-array series links.
    n = this%boundTasLinks%Count()
    do i=1,n
      tasLink => GetTimeArraySeriesLinkFromList(this%boundTasLinks, i)
      call tasLink%da()
    enddo
    !
    ! -- Go through and deallocate individual time array series
    do i = 1, size(this%taslist)
      call this%taslist(i)%da()
    end do
    !
    ! -- Deallocate the list of time-array series links.
    call this%boundTasLinks%Clear(.true.)
    deallocate(this%boundTasLinks)
    deallocate(this%tasfiles)
    !
    ! -- Deallocate the time array series
    deallocate(this%taslist)
    deallocate(this%tasnames)
    !
    ! -- nullify pointers
    this%dis => null()
    this%boundTasLinks => null()
    !
    return
  end subroutine tasmgr_da

  subroutine add_tasfile(this, fname)
! ******************************************************************************
! add_tasfile -- add a tas file
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use ArrayHandlersModule, only: ExpandArray
    ! -- dummy
    class(TimeArraySeriesManagerType) :: this
    character(len=*), intent(in) :: fname
    ! -- local
    integer(I4B) :: indx
! ------------------------------------------------------------------------------
    !
    call ExpandArray(this%tasfiles, 1)
    indx = size(this%tasfiles)
    this%tasfiles(indx) = fname
    !
    return
  end subroutine add_tasfile

  subroutine Reset(this, pkgName)
! ******************************************************************************
! Reset -- zero out arrays that are represented with time series.
!   Delete all existing links from time array series to package arrays as they
!   will need to be created with a new BEGIN PERIOD block.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(TimeArraySeriesManagerType) :: this
    character(len=*), intent(in) :: pkgName
    ! -- local
    integer(I4B) :: i, j, nlinks
    type(TimeArraySeriesLinkType), pointer :: taslink
! ------------------------------------------------------------------------------
    !
    ! -- Reassign all linked elements to zero
    nlinks = this%boundTasLinks%Count()
    do i = 1, nlinks
      taslink => GetTimeArraySeriesLinkFromList(this%boundTasLinks, i)
      if (associated(taslink)) then
        do j = 1, size(taslink%BndArray)
          taslink%BndArray(j) = DZERO
        enddo
      endif
    enddo
    !
    ! -- Delete all existing time array links
    if (associated(this%boundTasLinks)) then
      ! Deallocate and remove all links belonging to package
      nlinks = this%boundTasLinks%Count()
      do i = nlinks, 1, -1
        taslink => GetTimeArraySeriesLinkFromList(this%boundTasLinks, i)
        if (associated(taslink)) then
          call taslink%da()
          call this%boundTasLinks%RemoveNode(i, .true.)
        endif
      enddo
    endif
    !
    return
  end subroutine Reset

  subroutine MakeTasLink(this, pkgName, bndArray, iprpak,                      &
                         tasName, text, convertFlux, nodelist, inunit)
! ******************************************************************************
! MakeTasLink -- Make link from TAS to package array
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(TimeArraySeriesManagerType) :: this
    character(len=*), intent(in) :: pkgName
    real(DP), dimension(:), pointer :: bndArray
    integer(I4B), intent(in) :: iprpak
    character(len=*), intent(in) :: tasName
    character(len=*), intent(in) :: text
    logical, intent(in) :: convertFlux
    integer(I4B), dimension(:), pointer, contiguous, intent(in) :: nodelist
    integer(I4B), intent(in) :: inunit
    ! -- local
    integer(I4B) :: i, nfiles, iloc
    character(LINELENGTH) :: ermsg
    type(TimeArraySeriesLinkType), pointer :: newTasLink
    type(TimeArraySeriesType),     pointer :: tasptr => null()
! ------------------------------------------------------------------------------
    !
    ! -- Find the time array series
    nfiles = size(this%tasnames)
    iloc = 0
    do i = 1, nfiles
      if (this%tasnames(i) == tasname) then
        iloc = i
        exit
      endif
    end do
    if (iloc == 0) then
      ermsg = 'Error: Time-array series "' // trim(tasName) // '" not found.'
      call store_error(ermsg)
      call store_error_unit(inunit)
    endif
    tasptr => this%taslist(iloc)
    !
    ! -- Construct a time-array series link
    newTasLink => null()
    call ConstructTimeArraySeriesLink(newTasLink, tasptr, &
                                      pkgName, bndArray, iprpak, &
                                      text)
    newTasLink%ConvertFlux = convertFlux
    newTasLink%nodelist => nodelist
    !
    ! -- Add link to list of links
    call this%tasmgr_add_link(newTasLink)
    !
    return
  end subroutine MakeTasLink

  function GetLink(this, indx) result(tasLink)
! ******************************************************************************
! GetLink -- get link from the boundtaslinks list
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(TimeArraySeriesManagerType) :: this
    integer(I4B), intent(in) :: indx
    type(TimeArraySeriesLinkType), pointer :: tasLink
    ! -- local
! ------------------------------------------------------------------------------
    !
    tasLink => null()
    !
    if (associated(this%boundTasLinks)) then
      tasLink => GetTimeArraySeriesLinkFromList(this%boundTasLinks, indx)
    endif
    !
    return
  end function GetLink

  function CountLinks(this)
! ******************************************************************************
! CountLinks -- count number of links in the boundtaslinks list
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- return
    integer(I4B) :: CountLinks
    ! -- dummy
    class(TimeArraySeriesManagerType) :: this
! ------------------------------------------------------------------------------
    !
    if (associated(this%boundtaslinks)) then
      CountLinks = this%boundTasLinks%Count()
    else
      CountLinks = 0
    endif
    !
    return
  end function CountLinks

  ! -- Private procedures

  subroutine tasmgr_convert_flux(this, tasLink)
! ******************************************************************************
! tasmgr_convert_flux -- convert the array from a flux to a flow rate by
!   multiplying by the cell area
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(TimeArraySeriesManagerType) :: this
    type(TimeArraySeriesLinkType), pointer, intent(inout) :: tasLink
    ! -- local
    integer(I4B) :: i, n, noder
    real(DP) :: area
! ------------------------------------------------------------------------------
    !
    n = size(tasLink%BndArray)
    do i=1,n
      noder = tasLink%nodelist(i)
      if (noder > 0) then
        area = this%dis%get_area(noder)
        tasLink%BndArray(i) = tasLink%BndArray(i) * area
      endif
    enddo
    !
    return
  end subroutine tasmgr_convert_flux

  subroutine tasmgr_add_link(this, tasLink)
! ******************************************************************************
! tasmgr_add_link -- add a time arrays series link
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(TimeArraySeriesManagerType)      :: this
    type(TimeArraySeriesLinkType), pointer :: tasLink
    ! -- local
! ------------------------------------------------------------------------------
    !
    call AddTimeArraySeriesLinkToList(this%boundTasLinks, tasLink)
    !
    return
  end subroutine tasmgr_add_link

end module TimeArraySeriesManagerModule

