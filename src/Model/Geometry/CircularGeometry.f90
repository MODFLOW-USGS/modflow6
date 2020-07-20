module CircularGeometryModule
  use KindModule, only: DP, I4B
  use BaseGeometryModule, only: GeometryBaseType
  use ConstantsModule, only: DZERO, DTWO, DPI,                                   &
                             LINELENGTH, LENORIGIN, LENFTYPE, LENPACKAGENAME,    &
                             TABLEFT, TABCENTER
  use SimModule,                    only: count_errors, store_error, ustop,    &
                                          store_error_unit
  use TableModule, only: table_cr

  implicit none

  private
  public :: CircularGeometryType, cgeo_cr

  character(len=LENFTYPE)       :: ftype = 'CGEO'
  character(len=LENPACKAGENAME) :: text  = '            CGEO'
  
  type, extends(GeometryBaseType) :: CircularGeometryType
    real(DP), dimension(:), pointer, contiguous :: radius => null()
    real(DP), dimension(:), pointer, contiguous :: diameter => null()
  contains
    procedure :: geo_ar => cgeo_ar
    procedure :: geo_da => cgeo_da
    procedure :: area_sat
    procedure :: perimeter_sat
    procedure :: area_wet
    procedure :: perimeter_wet
    ! -- private
    procedure, private :: cgeo_allocate_scalars
    procedure, private :: cgeo_allocate_arrays
    procedure, private :: read_options
    procedure, private :: read_dimensions
    procedure, private :: read_packagedata
  end type CircularGeometryType
  
  contains

  subroutine cgeo_cr(geo, name_model, inunit, iout)
! ******************************************************************************
! cgeo_cr -- Create a new circular geometry object
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
    call geonew%cgeo_allocate_scalars(name_model, text)
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
  
  subroutine cgeo_ar(this)
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
      ! -- allocate arrays
      call this%cgeo_allocate_arrays()
      !
      ! -- read package data
      call this%read_packagedata()
    end if
    ! -- return
    return
  end subroutine cgeo_ar

  subroutine cgeo_da(this)
    use MemoryManagerModule, only: mem_deallocate
    ! -- dummy
    class(CircularGeometryType) :: this
! ------------------------------------------------------------------------------
    !
    ! -- deallocate local variables
    call mem_deallocate(this%radius)
    call mem_deallocate(this%diameter)
    !
    ! -- deallocate parent variables
    call this%GeometryBaseType%geo_da()    
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
      write(this%iout,'(1x,a)')'END OF CIRCULAR GEOMETRY OPTIONS'
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

  subroutine read_packagedata(this)
  ! ******************************************************************************
  ! read_packagedata -- read package data
  ! ******************************************************************************
  !
  !    SPECIFICATIONS:
  ! ------------------------------------------------------------------------------
    use ConstantsModule, only: LINELENGTH, DTWO
    use SimModule, only: ustop, store_error, count_errors
    ! -- dummy
    class(CircularGeometryType),intent(inout) :: this
    ! -- local
    character (len=LINELENGTH) :: title
    character (len=LINELENGTH) :: text
    character (len=LINELENGTH) :: errmsg
    logical :: isfound, endOfBlock
    integer(I4B) :: ntabcol
    integer(I4B) :: ntabrow
    integer(I4B) :: ierr
    integer(I4B) :: idx
    integer(I4B) :: n
    ! -- format
  ! ------------------------------------------------------------------------------
    !
    ! -- initialize local variables
    idx = 0
    !
    ! -- read package data
    call this%parser%GetBlock('PACKAGEDATA', isfound, ierr, &
                              supportOpenClose=.true.)
    !
    ! -- parse reaches block if detected
    if (isfound) then
      write(this%iout,'(/1x,a)')'PROCESSING '//trim(adjustl(this%text))// &
        ' PACKAGEDATA'
      do
        call this%parser%GetNextLine(endOfBlock)
        if (endOfBlock) then
          exit
        end if
        !
        ! -- increment counter
        idx = idx + 1
        !
        ! -- check if idx exceeds ngeo dimension
        if (idx > this%ngeo) then
          cycle
        end if
        ! -- read cell number
        n = this%parser%GetInteger()
        !
        ! -- convert cell number to cellid
        ! use disl to convert
        this%nodelist(idx) = n
        ! -- get radius
        this%radius(idx) = this%parser%GetDouble()
        this%csheight(idx) = this%radius(idx)
      end do
      write(this%iout,'(1x,a)')'END OF '//trim(adjustl(this%text))//' PACKAGEDATA'
    else
      call store_error('ERROR.  REQUIRED PACKAGEDATA BLOCK NOT FOUND.')
    end if
    !
    ! -- print input data
    if (this%iprpak /= 0) then
      !
      ! -- reset the input table object
      title = trim(adjustl(this%text)) // ' PACKAGE ' //                         &
              'CIRCULAR GEOMETRY DATA'
      ntabcol = 3
      ntabrow = min(idx, this%ngeo)
      call table_cr(this%inputtab, this%text, title)
      call this%inputtab%table_df(ntabrow, ntabcol, this%iout)
      text = 'NUMBER'
      call this%inputtab%initialize_column(text, 10, alignment=TABCENTER)
      text = 'CELLID'
      call this%inputtab%initialize_column(text, 20, alignment=TABLEFT)
      text = 'RADIUS'
      call this%inputtab%initialize_column(text, 15, alignment=TABCENTER)
      do n = 1, ntabrow
        call this%inputtab%add_term(n)
        call this%inputtab%add_term(this%nodelist(n))
        call this%inputtab%add_term(this%radius(n))
      end do
    end if
    
    !
    ! -- check if the number of entries is not equal to
    if (idx /= this%ngeo) then
      write(errmsg,'(4x,a,i0,a,i0,a)')                                           &
        '****ERROR. NUMBER OF CIRCULAR GEOMETRY ENTRIES (', idx,                 &
        ') IS NOT EQUAL TO NGEO (', this%ngeo, ')'
      call store_error(errmsg)
    end if
    !
    ! -- write summary of error messages for block
    if (count_errors() > 0) then
      call this%parser%StoreErrorUnit()
      call ustop()
    end if
    !
    ! -- return
    return
  end subroutine read_packagedata  

  subroutine cgeo_allocate_scalars(this, name_model, ftype)
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
  end subroutine cgeo_allocate_scalars

  subroutine cgeo_allocate_arrays(this)
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
  end subroutine cgeo_allocate_arrays
  
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