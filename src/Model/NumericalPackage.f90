module NumericalPackageModule
  ! -- modules
  use KindModule, only: DP, I4B
  use ConstantsModule,              only: LENPACKAGENAME, LENMODELNAME,        &
                                          LENORIGIN, LENFTYPE, LINELENGTH
  use SimModule,                    only: store_error, ustop
  use BlockParserModule,            only: BlockParserType
  use BaseDisModule,                only: DisBaseType
  
  implicit none
  private
  public NumericalPackageType

  type :: NumericalPackageType

    ! -- strings
    character(len=LENPACKAGENAME)                      :: name        = ''       !name of the package
    character(len=LENMODELNAME)                        :: name_model  = ''       !name of model to which package belongs
    character(len=LENORIGIN)                           :: origin      = ''       !name of model // name of package
    character(len=LENFTYPE)                            :: filtyp      = ''       !file type (CHD, DRN, RIV, etc.)
    !
    ! -- integers
    integer(I4B), pointer                              :: id          => null()  !consecutive package number in model
    integer(I4B), pointer                              :: inunit      => null()  !unit number for input file
    integer(I4B), pointer                              :: iout        => null()  !unit number for writing package output
    integer(I4B), pointer                              :: inewton     => null()  !newton flag
    integer(I4B), pointer                              :: iasym       => null()  !package causes matrix asymmetry
    integer(I4B), pointer                              :: iprpak      => null()  !integer flag to echo input
    integer(I4B), pointer                              :: iprflow     => null()  !flag to print simulated flows
    integer(I4B), pointer                              :: ipakcb      => null()  !output flows (-1, 0, 1) - save_flows
    integer(I4B), pointer                              :: ionper      => null()  !stress period for next data
    integer(I4B), pointer                              :: lastonper   => null()  !last value of ionper (for checking)
    !
    ! -- derived types
    type(BlockParserType)                              :: parser                 !parser object for reading blocks of information
    class(DisBaseType), pointer                        :: dis => null()
    
  contains
    procedure :: set_names
    procedure :: allocate_scalars
    procedure :: da
    procedure :: read_check_ionper
  end type NumericalPackageType
  !
  contains
  !
  subroutine set_names(this, ibcnum, name_model, pakname, ftype)
! ******************************************************************************
! set_names -- Assign strings to some character attributes
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(NumericalPackageType),intent(inout) :: this
    integer(I4B), intent(in) :: ibcnum
    character(len=*), intent(in) :: name_model
    character(len=*), intent(in) :: pakname
    character(len=*), intent(in) :: ftype
    ! -- locals
    character(len=LINELENGTH) :: errmsg
! ------------------------------------------------------------------------------
    this%name_model = name_model
    this%filtyp = ftype
    if(pakname == '') then
      write(this%name,'(a, i0)') trim(ftype) // '-', ibcnum
    else
      !
      ! -- Ensure pakname has no spaces
      if(index(trim(pakname), ' ') > 0) then
        errmsg = 'Package name contains spaces: ' // trim(pakname)
        call store_error(errmsg)
        errmsg = 'Remove spaces from name.'
        call store_error(errmsg)
        call ustop()
      endif
      !
      this%name = pakname
    endif
    this%origin = trim(this%name_model) // ' ' // trim(this%name)
    !
    ! -- Return
    return
  end subroutine set_names
  
  subroutine allocate_scalars(this)
! ******************************************************************************
! allocate_scalars -- allocate the scalars
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryManagerModule, only: mem_allocate, mem_setptr
    ! -- dummy
    class(NumericalPackageType) :: this
    ! -- local
    integer(I4B), pointer :: imodelnewton => NULL()
    integer(I4B), pointer :: imodelprpak => NULL()
    integer(I4B), pointer :: imodelprflow => NULL()
    integer(I4B), pointer :: imodelpakcb => NULL()
! ------------------------------------------------------------------------------
    !
    ! -- allocate
    call mem_allocate(this%id, 'ID', this%origin)
    call mem_allocate(this%inunit, 'INUNIT', this%origin)
    call mem_allocate(this%iout, 'IOUT', this%origin)
    call mem_allocate(this%inewton, 'INEWTON', this%origin)
    call mem_allocate(this%iasym, 'IASYM', this%origin)
    call mem_allocate(this%iprpak, 'IPRPAK', this%origin)
    call mem_allocate(this%iprflow, 'IPRFLOW', this%origin)
    call mem_allocate(this%ipakcb, 'IPAKCB', this%origin)
    !
    ! -- set pointer to model inewton variable
    call mem_setptr(imodelnewton, 'INEWTON', trim(this%name_model))
    !
    ! -- Set pointer to model iprpak, iprflow, and ipakcb variables
    call mem_setptr(imodelprpak, 'IPRPAK', trim(this%name_model))
    call mem_setptr(imodelprflow, 'IPRFLOW', trim(this%name_model))
    call mem_setptr(imodelpakcb, 'IPAKCB', trim(this%name_model))
    call mem_allocate(this%ionper, 'IONPER', this%origin)
    call mem_allocate(this%lastonper, 'LASTONPER', this%origin)
    !
    ! -- initialize
    this%id = 0
    this%inunit = 0
    this%iout = 0
    this%inewton = imodelnewton
    this%iasym = 0
    this%iprpak = imodelprpak
    this%iprflow = imodelprflow
    this%ipakcb = imodelpakcb
    this%ionper = 0
    this%lastonper = 0
    !
    ! -- nullify unneeded pointers
    imodelnewton => NULL()
    imodelprpak  => NULL()
    imodelprflow => NULL()
    imodelpakcb  => NULL()
    !
    ! -- Return
    return
  end subroutine allocate_scalars

  subroutine da(this)
! ******************************************************************************
! deallocate -- deallocate
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryManagerModule, only: mem_deallocate
    ! -- dummy
    class(NumericalPackageType) :: this
    ! -- local
! ------------------------------------------------------------------------------
    !
    ! -- allocate
    call mem_deallocate(this%id)
    call mem_deallocate(this%inunit)
    call mem_deallocate(this%iout)
    call mem_deallocate(this%inewton)
    call mem_deallocate(this%iasym)
    call mem_deallocate(this%iprpak)
    call mem_deallocate(this%iprflow)
    call mem_deallocate(this%ipakcb)
    call mem_deallocate(this%ionper)
    call mem_deallocate(this%lastonper)
    !
    ! -- Return
    return
  end subroutine da
  !
  subroutine read_check_ionper(this)
! ******************************************************************************
! read_check_ionper -- Read ionper and check to make sure periods are increasing
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use TdisModule, only: kper
    ! -- dummy
    class(NumericalPackageType),intent(inout) :: this
    ! -- local
    character(len=LINELENGTH) :: errmsg
! ------------------------------------------------------------------------------
    !
    ! -- save last value and read period number
    this%lastonper = this%ionper
    this%ionper = this%parser%GetInteger()
    !
    ! -- make check
    if (this%ionper <= this%lastonper) then
      write(errmsg, '(a, i0)') &
        'ERROR IN STRESS PERIOD ', kper
      call store_error(errmsg)
      write(errmsg, '(a, i0)') &
        'PERIOD NUMBERS NOT INCREASING.  FOUND ', this%ionper
      call store_error(errmsg)
      write(errmsg, '(a, i0)') &
        'BUT LAST PERIOD BLOCK WAS ASSIGNED ', this%lastonper
      call store_error(errmsg)
      call this%parser%StoreErrorUnit()
      call ustop()
    endif
    !
    ! -- return
    return
  end subroutine read_check_ionper

end module NumericalPackageModule
  
