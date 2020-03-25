module NpointGeometryModule
  use KindModule, only: DP, I4B
  use BaseGeometryModule, only: GeometryBaseType
  use ConstantsModule, only: DZERO, DONE, DTWO, DNODATA,                         &
                             LINELENGTH, LENORIGIN, LENFTYPE, LENPACKAGENAME,    &
                             TABLEFT, TABCENTER
  use SimModule, only: count_errors, store_error, ustop, store_error_unit
  use GenericUtilitiesModule, only: is_same
  use SmoothingModule,  only: sCubicSaturation
  use TableModule, only: table_cr

  implicit none
  
  private
  public :: NpointGeometryType, ngeo_cr

  character(len=LENFTYPE)       :: ftype = 'NGEO'
  character(len=LENPACKAGENAME) :: text  = '            NGEO'
  
  type, extends(GeometryBaseType) :: NpointGeometryType
    integer(I4B), dimension(:), pointer, contiguous :: isclosed => null()
    integer(I4B), dimension(:), pointer, contiguous :: ia => null()
    real(DP), dimension(:), pointer, contiguous :: xcoords => null()
    real(DP), dimension(:), pointer, contiguous :: zcoords => null()
    real(DP), dimension(:), pointer, contiguous :: zmin => null()
    real(DP), dimension(:), pointer, contiguous :: zmax => null()
  contains
    procedure :: geo_ar => ngeo_ar
    procedure :: geo_da => ngeo_da
    procedure :: area_sat
    procedure :: perimeter_sat
    procedure :: area_wet
    procedure :: perimeter_wet
    ! -- private
    procedure, private :: ngeo_allocate_scalars
    procedure, private :: ngeo_allocate_arrays
    procedure, private :: read_options
    procedure, private :: read_dimensions
    procedure, private :: read_packagedata
  end type NpointGeometryType
  
  contains

  subroutine ngeo_cr(geo, name_model, inunit, iout)
! ******************************************************************************
! ngeo_cr -- Create a new Npoint geometry object
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    class(GeometryBaseType), pointer :: geo
    character(len=*), intent(in) :: name_model
    integer(I4B), intent(in) :: inunit
    integer(I4B), intent(in) :: iout
    type(NpointGeometryType), pointer :: geonew
    ! -- local
! ------------------------------------------------------------------------------
    allocate(geonew)
    geo => geonew
    !
    !
    call geonew%ngeo_allocate_scalars(name_model, text)
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
  end subroutine ngeo_cr  
  
  subroutine ngeo_ar(this)
    ! -- dummy
    class(NpointGeometryType) :: this
! ------------------------------------------------------------------------------
    !
    ! -- read data from file
    if (this%inunit /= 0) then
      !
      ! -- Identify package
      write(this%iout, 1) this%inunit
  1   format(1X,/1X,'NGEO -- N-POINT GEOMETRY PACKAGE,',                         &
                    ' VERSION 1 : 02/28/2020 - INPUT READ FROM UNIT ',I0,//)
      !
      ! -- Read options
      call this%read_options()
      !
      ! -- Read dimensions block
      call this%read_dimensions()
      !
      ! -- allocate arrays
      call this%ngeo_allocate_arrays()
      !
      ! -- read package data
      call this%read_packagedata()
    end if
    ! -- return
    return
  end subroutine ngeo_ar

  subroutine ngeo_da(this)
    use MemoryManagerModule, only: mem_deallocate
    ! -- dummy
    class(NpointGeometryType) :: this
! ------------------------------------------------------------------------------
    !
    ! -- deallocate local variables
    call mem_deallocate(this%isclosed)
    call mem_deallocate(this%ia)
    call mem_deallocate(this%xcoords)
    call mem_deallocate(this%zcoords)
    call mem_deallocate(this%zmin)
    call mem_deallocate(this%zmax)
    !
    ! -- deallocate parent variables
    call this%GeometryBaseType%geo_da()    
    !
    ! -- return
    return
  end subroutine ngeo_da

  subroutine read_options(this)
! ******************************************************************************
! read_options -- Read options
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use ConstantsModule, only: LINELENGTH
    ! -- dummy
    class(NpointGeometryType) :: this
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
      write(this%iout,'(/,1x,a)')'PROCESSING N-POINT GEOMETRY OPTIONS'
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
      write(this%iout,'(1x,a)')'END OF N-POINT GEOMETRY OPTIONS'
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
    class(NpointGeometryType) :: this
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
      write(this%iout,'(/,1x,a)')'PROCESSING N-PPOINT GEOMETRY DIMENSIONS'
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
    write(this%iout,'(1x,a)')'END OF N-POINT GEOMETRY DIMENSIONS'
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
    use MemoryManagerModule, only: mem_reallocate
    ! -- dummy
    class(NpointGeometryType),intent(inout) :: this
    ! -- local
    character (len=LINELENGTH) :: title
    character (len=LINELENGTH) :: text
    character (len=LINELENGTH) :: errmsg
    logical :: isfound, endOfBlock
    integer(I4B) :: ntabcol
    integer(I4B) :: ntabrow
    integer(I4B) :: ierr
    integer(I4B) :: idx
    integer(I4B) :: jdx
    integer(I4B) :: npoint
    integer(I4B) :: npos
    integer(I4B) :: nsize
    integer(I4B) :: i0
    integer(I4B) :: i1
    integer(I4B) :: n
    real(DP) :: x0
    real(DP) :: x1
    real(DP) :: z0
    real(DP) :: z1
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
        !
        ! -- get npoint
        npoint = this%parser%GetInteger()
        npos = this%ia(idx) + npoint
        this%ia(idx+1) = npos
        nsize = npos - 1
        !
        ! -- reallocate xcoords and zcoords, if necessary
        if (nsize > size(this%xcoords)) then
          call mem_reallocate(this%xcoords, nsize, 'XCOORDS', this%origin)
          call mem_reallocate(this%zcoords, nsize, 'ZCOORDS', this%origin)
        end if
        !
        ! -- initialize range, zmin, and zmax
        i0 = this%ia(idx)
        i1 = this%ia(idx+1) - 1
        !
        ! -- get xcoords
        do jdx = i0, i1
          this%xcoords(jdx) = this%parser%GetDouble()
        end do
        !
        ! -- get zcoords
        do jdx = i0, i1
          z0 = this%parser%GetDouble()
          this%zcoords(jdx) = z0
          if (z0 < this%zmin(idx)) then
            this%zmin(idx) = z0
          end if
          if (z0 > this%zmax(idx)) then
            this%zmax(idx) = z0
          end if
        end do
      end do
      write(this%iout,'(1x,a)')'END OF '//trim(adjustl(this%text))//' PACKAGEDATA'
    else
      call store_error('ERROR.  REQUIRED PACKAGEDATA BLOCK NOT FOUND.')
    end if
    !
    ! -- determine if cross-section is closed
    do n = 1, this%ngeo
      i0 = this%ia(n)
      i1 = this%ia(n+1) - 1
      x0 = this%xcoords(i0)
      x1 = this%xcoords(i1)
      z0 = this%zcoords(i0)
      z1 = this%zcoords(i1)
      if (is_same(x0, x1) .and. is_same(z0, z1)) then
        this%isclosed(n) = 1
      end if
    end do
    !
    ! -- print input data
    if (this%iprpak /= 0) then
      !
      ! -- reset the input table object
      title = trim(adjustl(this%text)) // ' PACKAGE ' //                         &
              'N-POINT GEOMETRY DATA'
      ntabcol = 5
      ntabrow = this%ia(this%ngeo+1) - 1
      call table_cr(this%inputtab, this%text, title)
      call this%inputtab%table_df(ntabrow, ntabcol, this%iout)
      text = 'NUMBER'
      call this%inputtab%initialize_column(text, 10, alignment=TABCENTER)
      text = 'CELLID'
      call this%inputtab%initialize_column(text, 20, alignment=TABLEFT)
      text = 'XCOORDS'
      call this%inputtab%initialize_column(text, 15, alignment=TABCENTER)
      text = 'ZCOORDS'
      call this%inputtab%initialize_column(text, 15, alignment=TABCENTER)
      text = 'CLOSED CROSS-SECTION'
      call this%inputtab%initialize_column(text, 15, alignment=TABCENTER)
      do n = 1, this%ngeo
        i0 = this%ia(n)
        i1 = this%ia(n+1) - 1
        do jdx = i0, i1
          if (jdx == i0) then
            call this%inputtab%add_term(n)
            call this%inputtab%add_term(this%nodelist(n))
          else
            call this%inputtab%add_term(' ')
            call this%inputtab%add_term(' ')
          end if
          call this%inputtab%add_term(this%xcoords(jdx))
          call this%inputtab%add_term(this%zcoords(jdx))
            text = ' '
          if (jdx == i0) then
            if (this%isclosed(n) /= 0) then
              text = 'TRUE'
            end if
          end if
          call this%inputtab%add_term(text)
        end do
      end do
    end if
    
    !
    ! -- check if the number of entries is not equal to
    if (idx /= this%ngeo) then
      write(errmsg,'(4x,a,i0,a,i0,a)')                                           &
        '****ERROR. NUMBER OF N-POINT GEOMETRY ENTRIES (', idx,                  &
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
  
  subroutine ngeo_allocate_scalars(this, name_model, ftype)
    ! -- modules
    use MemoryManagerModule, only: mem_allocate
    ! -- dummy
    class(NpointGeometryType) :: this
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
  end subroutine ngeo_allocate_scalars

  subroutine ngeo_allocate_arrays(this)
    ! -- modules
    use MemoryManagerModule, only: mem_allocate
    ! -- dummy
    class(NpointGeometryType) :: this
    ! -- local
    integer(I4B) :: n
! ------------------------------------------------------------------------------
    !
    ! -- Allocate parent arrays
    call this%GeometryBaseType%allocate_arrays()    
    !
    ! -- allocate local arrays
    call mem_allocate(this%isclosed, this%ngeo, 'ISCLOSED', this%origin)
    call mem_allocate(this%ia, this%ngeo+1, 'IA', this%origin)
    call mem_allocate(this%xcoords, this%ngeo, 'XCOORDS', this%origin)
    call mem_allocate(this%zcoords, this%ngeo, 'ZCOORDS', this%origin)
    call mem_allocate(this%zmin, this%ngeo, 'ZMIN', this%origin)
    call mem_allocate(this%zmax, this%ngeo, 'ZMAX', this%origin)
    !
    ! -- initialize arrays
    this%ia(1) = 1
    do n = 1, this%ngeo
      this%isclosed(n) = 0
      this%zmin(n) = DNODATA
      this%zmax(n) = DZERO
    end do
    !
    ! -- return
    return
  end subroutine ngeo_allocate_arrays
  
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
    class(NpointGeometryType) :: this
    integer(I4B), intent(in) :: n
    ! -- local
    integer(I4B) :: i0
    integer(I4B) :: i1
    integer(I4B) :: i
    real(DP) :: zmin
    real(DP) :: zmax
    real(DP) :: x0
    real(DP) :: x1
    real(DP) :: z0
    real(DP) :: z1
! ------------------------------------------------------------------------------
    !
    ! -- initialize range, zmin, and zmax
    i0 = this%ia(n)
    i1 = this%ia(n+1) - 1
    zmin = this%zmin(n)
    zmax = this%zmax(n)
    !
    ! -- Calculate saturated area
    area_sat = DZERO
    do i = i0+1, i1
      x0 = this%xcoords(i-1)
      x1 = this%xcoords(i)
      z0 = this%zcoords(i-1)
      z1 = this%zcoords(i)
      area_sat = area_sat
    end do
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
    class(NpointGeometryType) :: this
    integer(I4B), intent(in) :: n
    ! -- local
    integer(I4B) :: i0
    integer(I4B) :: i1
    integer(I4B) :: i
    real(DP) :: zmin
    real(DP) :: zmax
    real(DP) :: x0
    real(DP) :: x1
    real(DP) :: z0
    real(DP) :: z1
! ------------------------------------------------------------------------------
    !
    ! -- initialize range, zmin, and zmax
    i0 = this%ia(n)
    i1 = this%ia(n+1) - 1
    zmin = this%zmin(n)
    zmax = this%zmax(n)
    !
    ! -- Calculate saturated perimeter
    perimeter_sat = DZERO
    do i = i0+1, i1
      x0 = this%xcoords(i-1)
      x1 = this%xcoords(i)
      z0 = this%zcoords(i-1)
      z1 = this%zcoords(i)
      perimeter_sat = perimeter_sat
    end do
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
    class(NpointGeometryType) :: this
    integer(I4B), intent(in) :: n
    real(DP), intent(in) :: depth
    ! -- local
    integer(I4B) :: i0
    integer(I4B) :: i1
    integer(I4B) :: i
    real(DP) :: zmin
    real(DP) :: zmax
    real(DP) :: x0
    real(DP) :: x1
    real(DP) :: z0
    real(DP) :: z1
! ------------------------------------------------------------------------------
    !
    ! -- initialize range, zmin, and zmax
    i0 = this%ia(n)
    i1 = this%ia(n+1) - 1
    zmin = this%zmin(n)
    zmax = this%zmax(n)
    !
    ! -- Calculate saturated area
    area_wet = DZERO
    if(depth > DZERO) then
      do i = i0+1, i1
        x0 = this%xcoords(i-1)
        x1 = this%xcoords(i)
        z0 = this%zcoords(i-1)
        z1 = this%zcoords(i)
      end do
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
    class(NpointGeometryType) :: this
    integer(I4B), intent(in) :: n
    real(DP), intent(in) :: depth
    ! -- local
    integer(I4B) :: i0
    integer(I4B) :: i1
    integer(I4B) :: i
    real(DP) :: zmin
    real(DP) :: zmax
    real(DP) :: x0
    real(DP) :: x1
    real(DP) :: z0
    real(DP) :: z1
! ------------------------------------------------------------------------------
    !
    ! -- initialize range, zmin, and zmax
    i0 = this%ia(n)
    i1 = this%ia(n+1) - 1
    zmin = this%zmin(n)
    zmax = this%zmax(n)
    !
    ! -- Calculate saturated area
    perimeter_wet = DZERO
    if(depth > DZERO) then
      do i = i0+1, i1
        x0 = this%xcoords(i-1)
        x1 = this%xcoords(i)
        z0 = this%zcoords(i-1)
        z1 = this%zcoords(i)
      end do
    endif
    !
    ! -- return
    return
  end function perimeter_wet
  
end module NpointGeometryModule