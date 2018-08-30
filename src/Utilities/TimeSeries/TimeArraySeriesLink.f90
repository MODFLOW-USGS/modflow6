module TimeArraySeriesLinkModule

  use KindModule, only: DP, I4B
  use ConstantsModule,       only: LENPACKAGENAME, LENTIMESERIESTEXT
  use InputOutputModule,     only: UPCASE
  use ListModule,            only: ListType
  use TimeArraySeriesModule, only: TimeArraySeriesType

  implicit none

  private
  public :: TimeArraySeriesLinkType, ConstructTimeArraySeriesLink, &
            AddTimeArraySeriesLinkToList, GetTimeArraySeriesLinkFromList
  private :: CastAsTimeArraySeriesLinkType

  type :: TimeArraySeriesLinkType
    ! -- Public members
    character(len=LENPACKAGENAME), public :: PackageName = ''
    character(len=LENTIMESERIESTEXT), public :: Text = ''
    integer(I4B), public :: Iprpak = 1
    logical, public :: UseDefaultProc = .true.
    logical, public :: ConvertFlux = .false.
    integer(I4B), dimension(:), pointer, contiguous, public :: nodelist => null()
    ! BndArray can point to an array in either the bound or auxval
    ! array of BndType, or any other double precision variable or array
    ! element that contains a value that could be controlled by a time series.
    real(DP), dimension(:), pointer, public :: BndArray => null()
    real(DP), dimension(:), pointer, public :: RMultArray => null()
    type(TimeArraySeriesType), pointer, public :: TimeArraySeries => null()
  contains
    procedure, public :: da => tasl_da
  end type TimeArraySeriesLinkType

contains

  subroutine tasl_da(this)
! ******************************************************************************
! tasl_da -- deallocate
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(TimeArraySeriesLinkType), intent(inout) :: this
! ------------------------------------------------------------------------------
    !
    this%nodelist => null()
    this%bndarray => null()
    this%rmultarray => null()
    this%TimeArraySeries => null()
    !
    return
  end subroutine tasl_da

  subroutine ConstructTimeArraySeriesLink(newTasLink, timeArraySeries, &
                                     pkgName, bndArray, iprpak, text)
! ******************************************************************************
! ConstructTimeArraySeriesLink -- construct
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    type(TimeArraySeriesLinkType), pointer,  intent(out) :: newTasLink
    type(TimeArraySeriesType), pointer,      intent(in)  :: timeArraySeries
    character(len=*),                        intent(in)  :: pkgName
    real(DP), dimension(:), pointer,         intent(in)  :: bndArray
    integer(I4B),                            intent(in)  :: iprpak
    character(len=*),                        intent(in)  :: text
    ! -- local
    character(len=LENPACKAGENAME) :: pkgNameTemp
! ------------------------------------------------------------------------------
    !
    allocate(newTasLink)
    ! Store package name as all caps
    pkgNameTemp = pkgName
    call UPCASE(pkgNameTemp)
    newTasLink%PackageName = pkgNameTemp
    newTasLink%timeArraySeries => timeArraySeries
    newTasLink%BndArray => bndArray
    newTasLink%Iprpak = iprpak
    newTasLink%Text = text
    !
    return
  end subroutine ConstructTimeArraySeriesLink

  function CastAsTimeArraySeriesLinkType(obj) result(res)
! ******************************************************************************
! CastAsTimeArraySeriesLinkType -- Cast an unlimited polymorphic object as 
!   TimeArraySeriesLinkType
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    class(*), pointer, intent(inout) :: obj
    type(TimeArraySeriesLinkType), pointer :: res
! ------------------------------------------------------------------------------
    !
    res => null()
    if (.not. associated(obj)) return
    !
    select type (obj)
    type is (TimeArraySeriesLinkType)
      res => obj
    end select
    !
    return
  end function CastAsTimeArraySeriesLinkType

  subroutine AddTimeArraySeriesLinkToList(list, tasLink)
! ******************************************************************************
! AddTimeArraySeriesLinkToList -- add to list
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    type(ListType),                         intent(inout) :: list
    type(TimeArraySeriesLinkType), pointer, intent(inout) :: tasLink
    ! -- local
    class(*), pointer :: obj
! ------------------------------------------------------------------------------
    !
    obj => tasLink
    call list%Add(obj)
    !
    return
  end subroutine AddTimeArraySeriesLinkToList

  function GetTimeArraySeriesLinkFromList(list, idx) result (res)
! ******************************************************************************
! GetTimeArraySeriesLinkFromList -- get from list
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    type(ListType),             intent(inout) :: list
    integer(I4B),                    intent(in)    :: idx
    type(TimeArraySeriesLinkType), pointer    :: res
    ! -- local
    class(*), pointer :: obj
! ------------------------------------------------------------------------------
    !
    obj => list%GetItem(idx)
    res => CastAsTimeArraySeriesLinkType(obj)
    !
    return
  end function GetTimeArraySeriesLinkFromList

end module TimeArraySeriesLinkModule

