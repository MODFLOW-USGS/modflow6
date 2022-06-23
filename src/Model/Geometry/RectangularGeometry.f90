module RectangularGeometryModule
  use KindModule, only: DP, I4B
  use BaseGeometryModule, only: BaseGeometryType
  use ConstantsModule, only: DZERO
  implicit none
  private
  public :: RectangularGeometryType

  type, extends(BaseGeometryType) :: RectangularGeometryType
    real(DP) :: height = DZERO
    real(DP) :: width = DZERO
  contains
    procedure :: area_sat
    procedure :: perimeter_sat
    procedure :: area_wet
    procedure :: perimeter_wet
    procedure :: set_attribute
    procedure :: print_attributes
  end type RectangularGeometryType

contains

  function area_sat(this)
! ******************************************************************************
! area_sat -- return saturated area
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use ConstantsModule, only: DTWO, DPI
    ! -- return
    real(DP) :: area_sat
    ! -- dummy
    class(RectangularGeometryType) :: this
! ------------------------------------------------------------------------------
    !
    ! -- Calculate area
    area_sat = this%height * this%width
    !
    ! -- Return
    return
  end function area_sat

  function perimeter_sat(this)
! ******************************************************************************
! perimeter_sat -- return saturated perimeter
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use ConstantsModule, only: DTWO, DPI
    ! -- return
    real(DP) :: perimeter_sat
    ! -- dummy
    class(RectangularGeometryType) :: this
! ------------------------------------------------------------------------------
    !
    ! -- Calculate area
    perimeter_sat = DTWO * (this%height + this%width)
    !
    ! -- return
    return
  end function perimeter_sat

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
    class(RectangularGeometryType) :: this
    real(DP), intent(in) :: depth
! ------------------------------------------------------------------------------
    !
    ! -- Calculate area
    if (depth <= DZERO) then
      area_wet = DZERO
    elseif (depth <= this%height) then
      area_wet = depth * this%width
    else
      area_wet = this%width * this%height
    end if
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
    class(RectangularGeometryType) :: this
    real(DP), intent(in) :: depth
! ------------------------------------------------------------------------------
    !
    ! -- Calculate area
    if (depth <= DZERO) then
      perimeter_wet = DZERO
    elseif (depth <= this%height) then
      perimeter_wet = DTWO * (depth + this%width)
    else
      perimeter_wet = DTWO * (this%height + this%width)
    end if
    !
    ! -- return
    return
  end function perimeter_wet

  subroutine set_attribute(this, line)
! ******************************************************************************
! set_attribute -- set a parameter for this rectangular object
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- module
    use InputOutputModule, only: urword
    use ConstantsModule, only: LINELENGTH
    use SimModule, only: store_error, count_errors
    ! -- dummy
    class(RectangularGeometryType) :: this
    character(len=LINELENGTH) :: errmsg
    character(len=*), intent(inout) :: line
    ! -- local
    integer(I4B) :: lloc, istart, istop, ival
    real(DP) :: rval
! ------------------------------------------------------------------------------
    !
    ! -- should change this and set id if uninitialized or store it
    lloc = 1
    call urword(line, lloc, istart, istop, 2, ival, rval, 0, 0)
    this%id = ival

    ! -- Parse the attribute
    call urword(line, lloc, istart, istop, 1, ival, rval, 0, 0)
    select case (line(istart:istop))
    case ('NAME')
      call urword(line, lloc, istart, istop, 1, ival, rval, 0, 0)
      this%name = line(istart:istop)
    case ('HEIGHT')
      call urword(line, lloc, istart, istop, 3, ival, rval, 0, 0)
      this%height = rval
    case ('WIDTH')
      call urword(line, lloc, istart, istop, 3, ival, rval, 0, 0)
      this%width = rval
    case default
      write (errmsg, '(4x,a,a)') &
        'Unknown rectangular geometry attribute: ', line(istart:istop)
      call store_error(errmsg, terminate=.TRUE.)
    end select
    !
    ! -- return
    return
  end subroutine set_attribute

  subroutine print_attributes(this, iout)
! ******************************************************************************
! print_attributes -- print the attributes for this object
! *****************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(RectangularGeometryType) :: this
    ! -- local
    integer(I4B), intent(in) :: iout
    ! -- formats
    character(len=*), parameter :: fmtnm = "(4x,a,a)"
    character(len=*), parameter :: fmttd = "(4x,a,1(1PG15.6))"
! ------------------------------------------------------------------------------
    !
    ! -- call parent to print parent attributes
    call this%BaseGeometryType%print_attributes(iout)
    !
    ! -- Print specifics of this geometry type
    write (iout, fmttd) 'HEIGHT = ', this%height
    write (iout, fmttd) 'WIDTH = ', this%width
    write (iout, fmttd) 'SATURATED AREA = ', this%area_sat()
    write (iout, fmttd) 'SATURATED WETTED PERIMETER = ', this%perimeter_sat()
    !
    ! -- return
    return
  end subroutine print_attributes

end module RectangularGeometryModule
