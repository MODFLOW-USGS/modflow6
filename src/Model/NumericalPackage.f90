module NumericalPackageModule
  ! -- modules
  use KindModule, only: DP, I4B
  use ConstantsModule,              only: LENPACKAGENAME, LENMODELNAME,        &
                                          LENMEMPATH, LENFTYPE, LINELENGTH,    &
                                          LENVARNAME
  use SimModule,                    only: store_error, ustop
  use BlockParserModule,            only: BlockParserType
  use BaseDisModule,                only: DisBaseType
  use MemoryHelperModule,           only: create_mem_path

  implicit none
  private
  public NumericalPackageType

  type :: NumericalPackageType

    ! -- strings
    character(len=LENMODELNAME)                        :: name_model      = ''       !< the name of the model that contains this package
    character(len=LENPACKAGENAME)                      :: packName        = ''       !< name of the package
    character(len=LENMEMPATH)                          :: memoryPath      = ''       !< the location in the memory manager where the variables are stored
    character(len=LENMEMPATH)                          :: memoryPathModel = ''       !< the location in the memory manager where the variables
                                                                                     ! of the parent model are stored
    character(len=LENFTYPE)                            :: filtyp          = ''       !< file type (CHD, DRN, RIV, etc.)
 
    ! -- integers
    integer(I4B), pointer                              :: id          => null()  !< consecutive package number in model
    integer(I4B), pointer                              :: inunit      => null()  !< unit number for input file
    integer(I4B), pointer                              :: iout        => null()  !< unit number for writing package output
    integer(I4B), pointer                              :: inewton     => null()  !< newton flag
    integer(I4B), pointer                              :: iasym       => null()  !< package causes matrix asymmetry
    integer(I4B), pointer                              :: iprpak      => null()  !< integer flag to echo input
    integer(I4B), pointer                              :: iprflow     => null()  !< flag to print simulated flows
    integer(I4B), pointer                              :: ipakcb      => null()  !< output flows (-1, 0, 1) - save_flows
    integer(I4B), pointer                              :: ionper      => null()  !< stress period for next data
    integer(I4B), pointer                              :: lastonper   => null()  !< last value of ionper (for checking)
    !
    ! -- derived types
    type(BlockParserType)                              :: parser                 !< parser object for reading blocks of information
    class(DisBaseType), pointer                        :: dis => null()
    
  contains
    procedure :: set_names
    procedure :: allocate_scalars
    procedure :: da
    procedure :: read_check_ionper
    procedure :: get_block_data
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
    this%filtyp = ftype
    this%name_model = name_model
    if(pakname == '') then
      write(this%packName,'(a, i0)') trim(ftype) // '-', ibcnum
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
      this%packName = pakname
    endif
    this%memoryPath = create_mem_path(name_model, this%packName)
    this%memoryPathModel = create_mem_path(name_model)
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
    call mem_allocate(this%id, 'ID', this%memoryPath)
    call mem_allocate(this%inunit, 'INUNIT', this%memoryPath)
    call mem_allocate(this%iout, 'IOUT', this%memoryPath)
    call mem_allocate(this%inewton, 'INEWTON', this%memoryPath)
    call mem_allocate(this%iasym, 'IASYM', this%memoryPath)
    call mem_allocate(this%iprpak, 'IPRPAK', this%memoryPath)
    call mem_allocate(this%iprflow, 'IPRFLOW', this%memoryPath)
    call mem_allocate(this%ipakcb, 'IPAKCB', this%memoryPath)
    !
    call mem_allocate(this%ionper, 'IONPER', this%memoryPath)
    call mem_allocate(this%lastonper, 'LASTONPER', this%memoryPath)
    !
    ! -- set pointer to model variables
    call mem_setptr(imodelnewton, 'INEWTON', this%memoryPathModel)
    call mem_setptr(imodelprpak, 'IPRPAK', this%memoryPathModel)
    call mem_setptr(imodelprflow, 'IPRFLOW', this%memoryPathModel)
    call mem_setptr(imodelpakcb, 'IPAKCB', this%memoryPathModel)
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

  subroutine get_block_data(this, tags, lfound, varinames)
! ******************************************************************************
! get_block_data -- Read griddata block for a package 
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryManagerModule, only: mem_setptr
    ! -- dummy
    class(NumericalPackageType) :: this
    character(len=24), dimension(:), intent(in)           :: tags
    logical, dimension(:), intent(inout)                  :: lfound
    character(len=24), dimension(:), intent(in), optional :: varinames
    ! -- local
    logical :: lkeyword
    logical :: endOfBlock
    integer(I4B) :: nsize
    integer(I4B) :: j
    character(len=24)         :: tmpvar
    character(len=LENVARNAME) :: varname
    character(len=LINELENGTH) :: errmsg, keyword
    character(len=:), allocatable :: line
    integer(I4B) :: istart, istop, lloc
    integer(I4B), dimension(:), pointer, contiguous :: aint
    real(DP), dimension(:), pointer, contiguous     :: adbl
! ------------------------------------------------------------------------------
    ! -- initialize nsize
    nsize = size(tags)
    do
      call this%parser%GetNextLine(endOfBlock)
      if (endOfBlock) exit
      call this%parser%GetStringCaps(keyword)
      call this%parser%GetRemainingLine(line)
      lkeyword = .false.
      lloc = 1
      tag_iter: do j = 1, nsize
        if (trim(adjustl(keyword)) == trim(adjustl(tags(j)))) then
          lkeyword = .true.
          lfound(j) = .true.
          if (present(varinames)) then
            tmpvar = adjustl(varinames(j))
          else
            tmpvar = adjustl(tags(j))
          end if
          varname = tmpvar(1:LENVARNAME)
          if (keyword(1:1) == 'I') then
            call mem_setptr(aint, trim(varname), trim(this%memoryPath))
            call this%dis%read_grid_array(line, lloc, istart, istop, this%iout,  &
                                this%parser%iuactive, aint, tags(j))
          else
            call mem_setptr(adbl, trim(varname), trim(this%memoryPath))
            call this%dis%read_grid_array(line, lloc, istart, istop, this%iout,  &
                                this%parser%iuactive, adbl, tags(j))
          end if
          exit tag_iter
        end if
      end do tag_iter
      if (.not.lkeyword) then
        write(errmsg,'(4x,a,a)')'ERROR. UNKNOWN GRIDDATA TAG: ',           &
                                  trim(keyword)
        call store_error(errmsg)
        call this%parser%StoreErrorUnit()
        call ustop()
      end if
    end do
    !
    ! -- return
    return
  end subroutine get_block_data

end module NumericalPackageModule
  
