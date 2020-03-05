module RectangularGeometryModule
  use KindModule, only: DP, I4B
  use BaseGeometryModule, only: GeometryBaseType
  use ConstantsModule, only: DZERO, DONE, DTWO,                                  &
                             LINELENGTH, LENORIGIN, LENFTYPE, LENPACKAGENAME,    &
                             TABLEFT, TABCENTER
  use SimModule,                    only: count_errors, store_error, ustop,      &
                                          store_error_unit
  use SmoothingModule,  only: sCubicSaturation
  use TableModule, only: table_cr

  implicit none
  
  private
  public :: RectangularGeometryType, rgeo_cr

  character(len=LENFTYPE)       :: ftype = 'RGEO'
  character(len=LENPACKAGENAME) :: text  = '            RGEO'
  
  type, extends(GeometryBaseType) :: RectangularGeometryType
    real(DP), dimension(:), pointer, contiguous :: width => null()
    real(DP), dimension(:), pointer, contiguous :: height => null()
  contains
    procedure :: geo_ar => rgeo_ar
    procedure :: geo_da => rgeo_da
    procedure :: area_sat
    procedure :: perimeter_sat
    procedure :: area_wet
    procedure :: perimeter_wet
    ! -- private
    procedure, private :: rgeo_allocate_scalars
    procedure, private :: rgeo_allocate_arrays
    procedure, private :: read_options
    procedure, private :: read_dimensions
    procedure, private :: read_packagedata
  end type RectangularGeometryType
  
  contains

  subroutine rgeo_cr(geo, name_model, inunit, iout)
! ******************************************************************************
! rgeo_cr -- Create a new rectangular geometry object
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    class(GeometryBaseType), pointer :: geo
    character(len=*), intent(in) :: name_model
    integer(I4B), intent(in) :: inunit
    integer(I4B), intent(in) :: iout
    type(RectangularGeometryType), pointer :: geonew
    ! -- local
! ------------------------------------------------------------------------------
    allocate(geonew)
    geo => geonew
    !
    !
    call geonew%rgeo_allocate_scalars(name_model, text)
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
  end subroutine rgeo_cr  
  
  subroutine rgeo_ar(this)
    ! -- dummy
    class(RectangularGeometryType) :: this
! ------------------------------------------------------------------------------
    !
    ! -- read data from file
    if (this%inunit /= 0) then
      !
      ! -- Identify package
      write(this%iout, 1) this%inunit
  1   format(1X,/1X,'CGEO -- RECTANGULAR GEOMETRY PACKAGE,',                     &
                    ' VERSION 1 : 02/28/2020 - INPUT READ FROM UNIT ',I0,//)
      !
      ! -- Read options
      call this%read_options()
      !
      ! -- Read dimensions block
      call this%read_dimensions()
      !
      ! -- allocate arrays
      call this%rgeo_allocate_arrays()
      !
      ! -- read package data
      call this%read_packagedata()
    end if
    ! -- return
    return
  end subroutine rgeo_ar

  subroutine rgeo_da(this)
    use MemoryManagerModule, only: mem_deallocate
    ! -- dummy
    class(RectangularGeometryType) :: this
! ------------------------------------------------------------------------------
    !
    ! -- deallocate local variables
    call mem_deallocate(this%width)
    call mem_deallocate(this%height)
    !
    ! -- deallocate parent variables
    call this%GeometryBaseType%geo_da()    
    !
    ! -- return
    return
  end subroutine rgeo_da

  subroutine read_options(this)
! ******************************************************************************
! read_options -- Read options
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use ConstantsModule, only: LINELENGTH
    ! -- dummy
    class(RectangularGeometryType) :: this
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
      write(this%iout,'(/,1x,a)')'PROCESSING RECTANGULAR GEOMETRY OPTIONS'
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
      write(this%iout,'(1x,a)')'END OF RECTANGULAR GEOMETRY OPTIONS'
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
    class(RectangularGeometryType) :: this
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
      write(this%iout,'(/,1x,a)')'PROCESSING RECTANGULAR GEOMETRY DIMENSIONS'
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
    write(this%iout,'(1x,a)')'END OF RECTANGULAR GEOMETRY DIMENSIONS'
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
    use ConstantsModule, only: LINELENGTH
    use SimModule, only: ustop, store_error, count_errors
    ! -- dummy
    class(RectangularGeometryType),intent(inout) :: this
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
        ! -- get width
        this%width(idx) = this%parser%GetDouble()
        ! -- get width
        this%height(idx) = this%parser%GetDouble()
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
              'RECTANGULAR GEOMETRY DATA'
      ntabcol = 4
      ntabrow = min(idx, this%ngeo)
      call table_cr(this%inputtab, this%text, title)
      call this%inputtab%table_df(ntabrow, ntabcol, this%iout)
      text = 'NUMBER'
      call this%inputtab%initialize_column(text, 10, alignment=TABCENTER)
      text = 'CELLID'
      call this%inputtab%initialize_column(text, 20, alignment=TABLEFT)
      text = 'WIDTH'
      call this%inputtab%initialize_column(text, 15, alignment=TABCENTER)
      text = 'HEIGHT'
      call this%inputtab%initialize_column(text, 15, alignment=TABCENTER)
      do n = 1, ntabrow
        call this%inputtab%add_term(n)
        call this%inputtab%add_term(this%nodelist(n))
        call this%inputtab%add_term(this%width(n))
        call this%inputtab%add_term(this%height(n))
      end do
    end if
    
    !
    ! -- check if the number of entries is not equal to
    if (idx /= this%ngeo) then
      write(errmsg,'(4x,a,i0,a,i0,a)')                                           &
        '****ERROR. NUMBER OF RECTANGULAR GEOMETRY ENTRIES (', idx,              &
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
  
  subroutine rgeo_allocate_scalars(this, name_model, ftype)
    ! -- modules
    use MemoryManagerModule, only: mem_allocate
    ! -- dummy
    class(RectangularGeometryType) :: this
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
  end subroutine rgeo_allocate_scalars

  subroutine rgeo_allocate_arrays(this)
    ! -- modules
    use MemoryManagerModule, only: mem_allocate
    ! -- dummy
    class(RectangularGeometryType) :: this
    ! -- local
! ------------------------------------------------------------------------------
    !
    ! -- Allocate parent arrays
    call this%GeometryBaseType%allocate_arrays()    
    !
    ! -- allocate local arrays
    call mem_allocate(this%width, this%ngeo, 'WIDTH', this%origin)
    call mem_allocate(this%height, this%ngeo, 'HEIGHT', this%origin)
    !
    ! -- return
    return
  end subroutine rgeo_allocate_arrays
  
  function area_sat(this, n)
! ******************************************************************************
! area_sat -- return saturated area
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- return
    real(DP) :: area_sat
    ! -- dummy
    class(RectangularGeometryType) :: this
    integer(I4B), intent(in) :: n
    ! -- local
    real(DP) :: width
    real(DP) :: height
! ------------------------------------------------------------------------------
    !
    ! -- initialize width and height
    width = this%width(n)
    height = this%height(n)
    !
    ! -- Calculate area
    area_sat = height * width
    !
    ! -- Return
    return
  end function area_sat
  
  function perimeter_sat(this, n)
! ******************************************************************************
! perimeter_sat -- return saturated perimeter
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- return
    real(DP) :: perimeter_sat
    ! -- dummy
    class(RectangularGeometryType) :: this
    integer(I4B), intent(in) :: n
    ! -- local
    real(DP) :: width
    real(DP) :: height
! ------------------------------------------------------------------------------
    !
    ! -- initialize width and height
    width = this%width(n)
    height = this%height(n)
    !
    ! -- Calculate area
    perimeter_sat = DTWO * (height + width)
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
    class(RectangularGeometryType) :: this
    integer(I4B), intent(in) :: n
    real(DP), intent(in) :: depth
    ! -- local
    real(DP) :: width
    real(DP) :: height
! ------------------------------------------------------------------------------
    !
    ! -- initialize width and height
    width = this%width(n)
    height = this%height(n)
    !
    ! -- Calculate area
    if(depth <= DZERO) then
      area_wet = DZERO      
    elseif(depth <= height) then
      area_wet = depth * width
    else
      area_wet = width * height
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
    class(RectangularGeometryType) :: this
    integer(I4B), intent(in) :: n
    real(DP), intent(in) :: depth
    ! -- local
    real(DP) :: width
    real(DP) :: height
    real(DP) :: sat
! ------------------------------------------------------------------------------
    !
    ! -- initialize width and height
    width = this%width(n)
    height = this%height(n)
    !
    ! -- Calculate area
    if(depth <= DZERO) then
      perimeter_wet = DZERO      
    elseif(depth <= height) then
      sat = DONE !sCubicSaturation(DEM5, DZERO, depth, DEM5)
      perimeter_wet = DTWO * (depth + width) * sat
    else
      perimeter_wet = DTWO * (height + width)
    endif
    !
    ! -- return
    return
  end function perimeter_wet
  
end module RectangularGeometryModule