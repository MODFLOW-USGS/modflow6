module RectangularChGeometryModule
  use KindModule, only: DP, I4B
  use BaseGeometryModule, only: BaseGeometryType
  use ConstantsModule, only: DZERO, DEM5
  use SmoothingModule,  only: sCubicSaturation
  implicit none
  private
  public :: RectangularChGeometryType
  
  type, extends(BaseGeometryType) :: RectangularChGeometryType
    real(DP) :: width  = DZERO
    real(DP) :: length = DZERO
  contains
    procedure :: surface_area
    procedure :: top_width_wet
    procedure :: surface_area_wet
    procedure :: area_wet
    procedure :: perimeter_wet
    procedure :: set_attribute
    procedure :: init
    procedure :: print_attributes
  end type RectangularChGeometryType
  
  contains
  
  function surface_area(this)
! ******************************************************************************
! area_wet -- return surface area
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use ConstantsModule, only: DTWO, DPI, DZERO
    ! -- return
    real(DP) :: surface_area
    ! -- dummy
    class(RectangularChGeometryType) :: this
! ------------------------------------------------------------------------------
    !
    ! -- Calculate surface area
    surface_area = this%width * this%length
    !
    ! -- Return
    return
  end function surface_area
  
  function top_width_wet(this, depth)
! ******************************************************************************
! area_wet -- return wetted surface area
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use ConstantsModule, only: DTWO, DPI, DZERO
    ! -- return
    real(DP) :: top_width_wet
    ! -- dummy
    class(RectangularChGeometryType) :: this
    real(DP), intent(in) :: depth
    ! -- local
    real(DP) :: sat
! ------------------------------------------------------------------------------
    !
    ! -- Calculate surface area
    sat = sCubicSaturation(DEM5, DZERO, depth, DEM5)
    top_width_wet = this%width * sat
    !
    ! -- Return
    return
  end function top_width_wet
  
  function surface_area_wet(this, depth)
! ******************************************************************************
! area_wet -- return wetted surface area
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use ConstantsModule, only: DTWO, DPI, DZERO
    ! -- return
    real(DP) :: surface_area_wet
    ! -- dummy
    class(RectangularChGeometryType) :: this
    real(DP), intent(in) :: depth
    ! -- local
    real(DP) :: top_width
! ------------------------------------------------------------------------------
    !
    ! -- Calculate surface area
    top_width = this%top_width_wet(depth)
    surface_area_wet = top_width * this%length
    !
    ! -- Return
    return
  end function surface_area_wet
  
  function area_wet(this, depth)
! ******************************************************************************
! area_wet -- return wetted area
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use ConstantsModule, only: DTWO, DPI, DZERO
    ! -- return
    real(DP) :: area_wet
    ! -- dummy
    class(RectangularChGeometryType) :: this
    real(DP), intent(in) :: depth
! ------------------------------------------------------------------------------
    !
    ! -- Calculate area
    area_wet = depth * this%width
    !
    ! -- Return
    return
  end function area_wet
  
  function perimeter_wet(this, depth)
! ******************************************************************************
! perimeter_wet -- return wetted perimeter
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use ConstantsModule, only: DTWO, DPI
    ! -- return
    real(DP) :: perimeter_wet
    ! -- dummy
    class(RectangularChGeometryType) :: this
    real(DP), intent(in) :: depth
! ------------------------------------------------------------------------------
    !
    ! -- Calculate wetted perimeter
    !perimeter_wet = DTWO * depth + this%width
    perimeter_wet = this%width
    !
    ! -- return
    return
  end function perimeter_wet
  
  subroutine set_attribute(this, line)
! ******************************************************************************
! set_attribute -- set a parameter for this rectangular channel object
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- module
    use InputOutputModule, only: urword
    use ConstantsModule, only: LINELENGTH
    use SimModule, only: ustop, store_error, count_errors
    ! -- dummy
    class(RectangularChGeometryType) :: this
    character(len=LINELENGTH) :: errmsg
    character(len=*), intent(inout) :: line
    ! -- local
    integer(I4B) :: lloc, istart, istop, ival
    real(DP) :: rval
! ------------------------------------------------------------------------------
    !    
    ! -- should change this and set id if uninitialized or store it
    lloc=1
    call urword(line, lloc, istart, istop, 2, ival, rval, 0, 0)
    this%id = ival
    
    ! -- Parse the attribute
    call urword(line, lloc, istart, istop, 1, ival, rval, 0, 0)
    select case(line(istart:istop))
    case('NAME')
      call urword(line, lloc, istart, istop, 1, ival, rval, 0, 0)
      this%name = line(istart:istop)
    case('WIDTH')
      call urword(line, lloc, istart, istop, 3, ival, rval, 0, 0)
      this%width = rval      
    case('LENGTH')
      call urword(line, lloc, istart, istop, 3, ival, rval, 0, 0)
      this%length = rval      
    case default
      write(errmsg,'(4x,a,a)') &
        '****ERROR. UNKNOWN RECTANGULAR CHANNEL GEOMETRY ATTRIBUTE: ', &
                               line(istart:istop)
      call store_error(errmsg)
      call ustop()
    end select
    !
    ! -- return
    return
  end subroutine set_attribute

  
  subroutine init(this, id, name, width, length)
! ******************************************************************************
! init -- initialize this rectangular channel object
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(RectangularChGeometryType) :: this
    integer(I4B), intent(in) :: id
    character (len=*), intent(in) :: name
    real(DP), intent(in) :: width
    real(DP), intent(in) :: length
! ------------------------------------------------------------------------------
    !
    ! -- initialize rectangular channel object using passed variables
    this%geo_type = 'RECTANGULAR CH'
    this%id = id
    this%name = name
    this%width = width
    this%length = length
    !
    ! -- return
    return
  end subroutine init
  
  subroutine print_attributes(this, iout)
! ******************************************************************************
! print_attributes -- print the attributes for this object
! *****************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(RectangularChGeometryType) :: this
    ! -- local
    integer(I4B), intent(in) :: iout
    ! -- formats
    character(len=*), parameter :: fmtnm = "(4x,a,a)"
    character(len=*), parameter :: fmttd = "(4x,a,1(1PG15.6))"
    character(len=*), parameter :: fmtline = "(4x,36('-'))"
    character(len=*), parameter :: fmtend = "(4x)"
! ------------------------------------------------------------------------------
    !
    ! -- call parent to print parent attributes
    call this%BaseGeometryType%print_attributes(iout)
    !
    ! -- Print specifics of this geometry type
    write(iout, fmttd) 'WIDTH = ', this%width
    write(iout, fmttd) 'LENGTH = ', this%length
    write(iout, fmtline)
    write(iout, fmtend)
    !
    ! -- return
    return
  end subroutine print_attributes
  
end module RectangularChGeometryModule