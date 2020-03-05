module BaseGeometryModule
  
  use KindModule, only: DP, I4B
  use ConstantsModule, only: DZERO,                                              &
                             LENORIGIN, LENMODELNAME, LENFTYPE, LENPACKAGENAME
  use SimModule, only: store_error, ustop  
  use BlockParserModule, only: BlockParserType
  use TableModule, only: TableType

  implicit none
  
  private
  public GeometryBaseType
  
  type :: GeometryBaseType
    character(len=LENORIGIN), pointer               :: origin     => null()      !origin name for mem allocation
    character(len=LENMODELNAME), pointer            :: name_model => null()      !name of the model
    character(len=LENFTYPE), pointer                :: filtyp     => null()      !file type (CHD, DRN, RIV, etc.)
    character(len=LENPACKAGENAME), pointer          :: text       => null()
    integer(I4B), pointer                           :: inunit     => null()      !unit number for input file
    integer(I4B), pointer                           :: iout       => null()      !unit number for output file
    integer(I4B), pointer                           :: iprpak     => null()      !integer flag to echo input
    integer(I4B), pointer                           :: ngeo       => null()
    integer(I4B), dimension(:), pointer, contiguous :: nodelist   => null()      !vector of reduced node numbers
    type(BlockParserType)                           :: parser                    !object to read blocks
    !
    ! -- table objects
    type(TableType), pointer :: inputtab => null()
  contains
    procedure :: geo_ar
    procedure :: geo_da
    procedure :: area_sat
    procedure :: perimeter_sat
    procedure :: area_wet
    procedure :: perimeter_wet
    procedure :: allocate_scalars
    procedure :: allocate_arrays
  end type GeometryBaseType

  contains
  
  subroutine geo_ar(this)
    ! -- dummy
    class(GeometryBaseType) :: this
! ------------------------------------------------------------------------------
    ! -- code
    call store_error('Program error: GeometryBaseType method geo_df not' //      &
                     'implemented.')
    call ustop()
    !
    ! -- return
    return
  end subroutine geo_ar

  subroutine geo_da(this)
    use MemoryManagerModule, only: mem_deallocate
    ! -- dummy
    class(GeometryBaseType) :: this
! ------------------------------------------------------------------------------
    ! -- code
    deallocate(this%origin)
    deallocate(this%name_model)
    deallocate(this%filtyp)
    deallocate(this%text)
    !
    ! -- deallocate arrays
    call mem_deallocate(this%nodelist)
    !
    ! -- input table object
    if (associated(this%inputtab)) then
      call this%inputtab%table_da()
      deallocate(this%inputtab)
      nullify(this%inputtab)
    end if
    !
    ! -- deallocate scalars
    call mem_deallocate(this%inunit)
    call mem_deallocate(this%iout)
    call mem_deallocate(this%iprpak)
    call mem_deallocate(this%ngeo)
    !
    ! -- return
    return
  end subroutine geo_da

  subroutine allocate_scalars(this, name_model, text)
    ! -- modules
    use MemoryManagerModule, only: mem_allocate, mem_setptr
    ! -- dummy
    class(GeometryBaseType) :: this
    character(len=*), intent(in) :: name_model
    character(len=*), intent(in) :: text
    ! -- local
    character(len=LENORIGIN) :: origin
    integer(I4B), pointer :: imodelprpak => NULL()
! ------------------------------------------------------------------------------
    !
    ! -- Assign origin name
    origin = trim(adjustl(name_model)) // ' ' // trim(adjustl(text))
    !
    ! -- allocate character scalars
    allocate(this%origin)
    allocate(this%name_model)
    allocate(this%filtyp)
    allocate(this%text)
    !
    ! -- initialize character scalars
    this%origin = origin
    this%name_model = name_model
    this%text = text
    !
    ! -- allocate remaining scalars
    call mem_allocate(this%inunit, 'INUNIT', origin)
    call mem_allocate(this%iout, 'IOUT', origin)
    call mem_allocate(this%iprpak, 'IPRPAK', origin)
    call mem_allocate(this%ngeo, 'NGEO', origin)
    !
    ! -- Set pointer to model iprpak, iprflow, and ipakcb variables
    call mem_setptr(imodelprpak, 'IPRPAK', trim(name_model))
    !
    ! -- initialize scalars
    this%iprpak = imodelprpak
    this%ngeo = 0
    !
    ! -- nullify unneeded pointers
    imodelprpak  => NULL()
    !
    ! -- return
    return
  end subroutine allocate_scalars

  subroutine allocate_arrays(this)
    ! -- modules
    use MemoryManagerModule, only: mem_allocate
    ! -- dummy
    class(GeometryBaseType) :: this
! ------------------------------------------------------------------------------
    !
    ! -- Allocate arrays
    call mem_allocate(this%nodelist, this%ngeo, 'NODELIST', this%origin)
    !
    ! -- return
    return
  end subroutine allocate_arrays
  
  function area_sat(this, n)
    ! -- return
    real(DP) :: area_sat
    ! -- dummy
    class(GeometryBaseType) :: this
    integer(I4B), intent(in) :: n
! ------------------------------------------------------------------------------
    ! -- code
    area_sat = DZERO
    ! -- return
    return
 end function area_sat
  
 function perimeter_sat(this, n)
    ! -- return
    real(DP) :: perimeter_sat
    ! -- dummy
    class(GeometryBaseType) :: this
    integer(I4B), intent(in) :: n
! ------------------------------------------------------------------------------
    ! -- code
    perimeter_sat = DZERO
    ! -- return
    return
  end function perimeter_sat
  
  function area_wet(this, n, depth)
    ! -- return
    real(DP) :: area_wet
    ! -- dummy
    class(GeometryBaseType) :: this
    integer(I4B), intent(in) :: n
    real(DP), intent(in) :: depth
! ------------------------------------------------------------------------------
    ! -- code
    area_wet = DZERO
    ! -- return
    return
  end function area_wet
  
  function perimeter_wet(this, n, depth)
    ! -- return
    real(DP) :: perimeter_wet
    ! -- dummy
    class(GeometryBaseType) :: this
    integer(I4B), intent(in) :: n
    real(DP), intent(in) :: depth    
! ------------------------------------------------------------------------------
    ! -- code
    perimeter_wet = DZERO
    ! -- return
    return
  end function perimeter_wet
  
  
end module BaseGeometryModule