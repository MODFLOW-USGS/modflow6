module CircularGeometryModule
  use KindModule, only: DP, I4B
  use BaseGeometryModule, only: GeometryBaseType
  use ConstantsModule, only: DZERO, DTWO, DPI,                                   &
                             LINELENGTH, LENORIGIN, LENFTYPE, LENPACKAGENAME

  use SimModule,                    only: count_errors, store_error, ustop,    &
                                          store_error_unit
  implicit none

  private
  public :: CircularGeometryType, cgeo_cr

  character(len=LENFTYPE)       :: ftype = 'CGEO'
  character(len=LENPACKAGENAME) :: text  = '            CGEO'
  
  type, extends(GeometryBaseType) :: CircularGeometryType
    real(DP), dimension(:), pointer :: radius => null()
    real(DP), dimension(:), pointer :: diameter => null()
  contains
    procedure :: geo_df => cgeo_df
    procedure :: geo_da => cgeo_da
    procedure :: area_sat
    procedure :: perimeter_sat
    procedure :: area_wet
    procedure :: perimeter_wet
    ! -- private
    procedure, private :: read_options
    procedure, private :: read_dimensions
  end type CircularGeometryType
  
  contains

  subroutine cgeo_cr(geo, name_model, inunit, iout)
! ******************************************************************************
! cgeo_cr -- Create a new circular geometry  object
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    class(GeometryBaseType), pointer :: geo
    character(len=*), intent(in) :: name_model
    integer(I4B), intent(in) :: inunit
    integer(I4B), intent(in) :: iout
    type(CircularGeometryType), pointer :: geonew
    ! -- local
! ------------------------------------------------------------------------------
    allocate(geonew)
    geo => geonew
    !
    !
    call geonew%allocate_scalars(name_model, text)
    !
    ! -- set
    geo%inunit = inunit
    geo%iout = iout
    geo%filtyp = ftype
    !
    ! -- Initialize block parser
    call geo%parser%Initialize(geo%inunit, geo%iout)
    !
    ! -- Return
    return
  end subroutine cgeo_cr  
  
  subroutine cgeo_df(this)
    ! -- dummy
    class(CircularGeometryType) :: this
! ------------------------------------------------------------------------------
    !
    ! -- read data from file
    if (this%inunit /= 0) then
      !
      ! -- Identify package
      write(this%iout, 1) this%inunit
  1   format(1X,/1X,'CGEO -- CIRCULAR GEOMETRY PACKAGE,',                        &
                    ' VERSION 1 : 02/28/2020 - INPUT READ FROM UNIT ',I0,//)
      !
      ! -- Read options
      call this%read_options()
      !
      ! -- Read dimensions block
      call this%read_dimensions()
      !
      ! -- read package data
    end if
    ! -- return
    return
  end subroutine cgeo_df

  subroutine cgeo_da(this)
    use MemoryManagerModule, only: mem_deallocate
    ! -- dummy
    class(CircularGeometryType) :: this
! ------------------------------------------------------------------------------
    !
    ! -- Allocate parent scalars
    call this%GeometryBaseType%geo_da()    
    !
    ! -- deallocate local arrays
    call mem_deallocate(this%radius)
    call mem_deallocate(this%diameter)
    !
    ! -- return
    return
  end subroutine cgeo_da

  subroutine read_options(this)
! ******************************************************************************
! read_options -- Read options
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use ConstantsModule, only: LINELENGTH
    ! -- dummy
    class(CircularGeometryType) :: this
    ! -- locals
    character(len=LINELENGTH) :: errmsg, keyword
    integer(I4B) :: ierr
    logical :: isfound, endOfBlock
! ------------------------------------------------------------------------------
    !
    ! -- get options block
    call this%parser%GetBlock('OPTIONS', isfound, ierr,                          &
                              supportOpenClose=.true., blockRequired=.false.)
    !
    ! -- parse options block if detected
    if (isfound) then
      write(this%iout,'(/,1x,a)')'PROCESSING CIRCULAR GEOMETRY OPTIONS'
      do
        call this%parser%GetNextLine(endOfBlock)
        if (endOfBlock) exit
        call this%parser%GetStringCaps(keyword)
        select case (keyword)
          case('PRINT_INPUT')
            this%iprpak = 1
            write(this%iout,'(4x,a,2(1x,a))')                                    &
              'LISTS OF', trim(adjustl(this%text)), 'CELLS WILL BE PRINTED.'
          case default
            write(errmsg,'(4x,a,3(1x,a))')                                       &
              '****ERROR. UNKNOWN', trim(adjustl(this%text)), 'OPTION: ',        &
              trim(keyword)
            call store_error(errmsg)
            call this%parser%StoreErrorUnit()
            call ustop()
        end select
      end do
    else
      write(this%iout,'(1x,a,2(1x,a))')                                          &
        'NO', trim(adjustl(ftype)), 'OPTION BLOCK DETECTED.'
    end if
    if(isfound) then
      write(this%iout,'(1x,a)')'END OF DISCRETIZATION OPTIONS'
    endif
    !
    ! -- Return
    return
  end subroutine read_options

  subroutine read_dimensions(this)
! ******************************************************************************
! read_dimensions -- Read dimensions
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use ConstantsModule,  only: LINELENGTH
    ! -- dummy
    class(CircularGeometryType) :: this
    ! -- locals
    character(len=LINELENGTH) :: errmsg, keyword
    integer(I4B) :: ierr
    logical :: isfound, endOfBlock
! ------------------------------------------------------------------------------
    !
    ! -- get dimensions block
    call this%parser%GetBlock('DIMENSIONS', isfound, ierr, &
                              supportOpenClose=.true.)
    !
    ! -- parse dimensions block if detected
    if (isfound) then
      write(this%iout,'(/,1x,a)')'PROCESSING CIRCULAR GEOMETRY DIMENSIONS'
      do
        call this%parser%GetNextLine(endOfBlock)
        if (endOfBlock) exit
        call this%parser%GetStringCaps(keyword)
        select case (keyword)
          case ('NGEO')
            this%ngeo = this%parser%GetInteger()
            write(this%iout,'(3x,a,i0)')'NGEO = ', this%ngeo
          case default
            write(errmsg,'(4x,a,3(1x,a))')                                       &
              '****ERROR. UNKNOWN', trim(adjustl(this%text)), 'OPTION: ',        &
              trim(keyword)
            call store_error(errmsg)
            call this%parser%StoreErrorUnit()
            call ustop()
        end select
      end do
    else
      call store_error('ERROR.  REQUIRED DIMENSIONS BLOCK NOT FOUND.')
    end if
    !
    ! -- verify dimensions were set
    if(this%ngeo < 1) then
      call store_error( &
          'ERROR.  NGEO WAS NOT SPECIFIED OR WAS SPECIFIED INCORRECTLY.')
    endif
    write(this%iout,'(1x,a)')'END OF CIRCULAR GEOMETRY DIMENSIONS'
    !
    ! -- check for errors
    if (count_errors() > 0) then
      call store_error_unit(this%inunit)
      call ustop()
    endif
    !
    ! -- Return
    return
  end subroutine read_dimensions
  
  subroutine allocate_scalars(this, name_model, ftype)
    ! -- modules
    use MemoryManagerModule, only: mem_allocate
    ! -- dummy
    class(CircularGeometryType) :: this
    character(len=*), intent(in) :: name_model
    character(len=*), intent(in) :: ftype
    ! -- local 
! ------------------------------------------------------------------------------
    !
    ! -- Allocate parent scalars
    call this%GeometryBaseType%allocate_scalars(name_model, ftype)    
    !
    ! -- return
    return
  end subroutine allocate_scalars

  subroutine allocate_arrays(this)
    ! -- modules
    use MemoryManagerModule, only: mem_allocate
    ! -- dummy
    class(CircularGeometryType) :: this
    ! -- local
! ------------------------------------------------------------------------------
    !
    ! -- Allocate parent arrays
    call this%GeometryBaseType%allocate_arrays()    
    !
    ! -- allocate local arrays
    call mem_allocate(this%radius, this%ngeo, 'RADIUS', this%origin)
    call mem_allocate(this%diameter, this%ngeo, 'DIAMETER', this%origin)
    !
    ! -- return
    return
  end subroutine allocate_arrays
  
  function area_sat(this, n)
! ******************************************************************************
! area_sat -- return area as if geometry is fully saturated
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- return
    real(DP) :: area_sat
    ! -- dummy
    class(CircularGeometryType) :: this
    integer(I4B), intent(in) :: n
! ------------------------------------------------------------------------------
    !
    ! -- Calculate area
    area_sat = DPI * this%radius(n) ** DTWO
    !
    ! -- Return
    return
  end function area_sat
  
  function perimeter_sat(this, n)
! ******************************************************************************
! perimeter_sat -- return perimeter as if geometry is fully saturated
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- return
    real(DP) :: perimeter_sat
    ! -- dummy
    class(CircularGeometryType) :: this
    integer(I4B), intent(in) :: n
    ! -- local
! ------------------------------------------------------------------------------
    !
    ! -- Calculate area
    perimeter_sat = DTWO * DPI * this%radius(n)
    !
    ! -- return
    return
  end function perimeter_sat
  
  function area_wet(this, n, depth)
! ******************************************************************************
! area_wet -- return wetted area
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- return
    real(DP) :: area_wet
    ! -- dummy
    class(CircularGeometryType) :: this
    integer(I4B), intent(in) :: n
    real(DP), intent(in) :: depth
    ! -- local 
    real(DP) :: radius
    real(DP) :: diameter
! ------------------------------------------------------------------------------
    !
    ! -- initialize local variables
    radius = this%radius(n)
    diameter = this%diameter(n)
    !
    ! -- Calculate area
    if(depth <= DZERO) then
      area_wet = DZERO      
    elseif(depth <= radius) then
      area_wet = radius * radius *                                               &
                 acos((radius - depth) / radius) -                               &
                 (radius - depth) * sqrt(radius * radius -                       &
                 (radius - depth) ** DTWO)
    elseif(depth <= diameter) then
      area_wet = radius * radius * (DPI - acos((depth - radius) / radius)) -     &
                 (radius - depth) * sqrt(radius *  radius -                      &
                 (radius - depth) ** DTWO)
    else
      area_wet = DPI * radius * radius
    endif
    !
    ! -- Return
    return
  end function area_wet
  
  function perimeter_wet(this, n, depth)
! ******************************************************************************
! perimeter_wet -- return wetted perimeter
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- return
    real(DP) :: perimeter_wet
    ! -- dummy
    class(CircularGeometryType) :: this
    integer(I4B), intent(in) :: n
    real(DP), intent(in) :: depth
    ! -- local
    real(DP) :: radius
    real(DP) :: diameter
! ------------------------------------------------------------------------------
    !
    ! -- initialize local variables
    radius = this%radius(n)
    diameter = this%diameter(n)
    !
    ! -- Calculate area
    if(depth <= DZERO) then
      perimeter_wet = DZERO      
    elseif(depth <= radius) then
      perimeter_wet = diameter * acos((radius - depth) / radius)
    elseif(depth <= diameter) then
      perimeter_wet = diameter * (DPI - acos((depth - radius) / radius))
    else
      perimeter_wet = DPI * diameter
    endif
    !
    ! -- return
    return
  end function perimeter_wet
  
end module CircularGeometryModule