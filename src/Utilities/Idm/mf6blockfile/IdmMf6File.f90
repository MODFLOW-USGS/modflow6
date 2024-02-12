!> @brief This module contains the IdmMf6FileModule
!!
!! This module contains high-level routines for loading
!! MODFLOW 6 ASCII source input. This module implements the
!! loader types that the IdmLoadModule creates and invokes.
!! It also creates and manages dynamic ASCII input loaders
!! for all supported types of MODFLOW 6 ASCII dynamic input.
!!
!<
module IdmMf6FileModule

  use KindModule, only: DP, I4B, LGP
  use SimVariablesModule, only: errmsg
  use ConstantsModule, only: LINELENGTH
  use SimModule, only: store_error, store_error_filename
  use BlockParserModule, only: BlockParserType
  use ModflowInputModule, only: ModflowInputType, getModflowInput
  use InputLoadTypeModule, only: StaticPkgLoadBaseType, DynamicPkgLoadBaseType
  use AsciiInputLoadTypeModule, only: AsciiDynamicPkgLoadBaseType

  implicit none
  private
  public :: input_load
  public :: Mf6FileStaticPkgLoadType, Mf6FileDynamicPkgLoadType
  public :: open_mf6file

  !> @brief MF6File static loader type
  !<
  type, extends(StaticPkgLoadBaseType) :: Mf6FileStaticPkgLoadType
  contains
    procedure :: init => static_init
    procedure :: load => static_load
    procedure :: destroy => static_destroy
  end type Mf6FileStaticPkgLoadType

  !> @brief MF6File dynamic loader type
  !<
  type, extends(DynamicPkgLoadBaseType) :: Mf6FileDynamicPkgLoadType
    type(BlockParserType), pointer :: parser !< parser for MF6File period blocks
    integer(I4B), pointer :: iper
    integer(I4B), pointer :: ionper
    class(AsciiDynamicPkgLoadBaseType), pointer :: rp_loader
  contains
    procedure :: init => dynamic_init
    procedure :: df => dynamic_df
    procedure :: ad => dynamic_ad
    procedure :: rp => dynamic_rp
    procedure :: read_ionper => dynamic_read_ionper
    procedure :: create_loader => dynamic_create_loader
    procedure :: destroy => dynamic_destroy
  end type Mf6FileDynamicPkgLoadType

contains

  !> @brief input load for traditional mf6 simulation static input file
  !<
  subroutine input_load(filename, mf6_input, component_filename, iout)
    use LoadMf6FileModule, only: LoadMf6FileType
    character(len=*), intent(in) :: filename
    type(ModflowInputType), intent(in) :: mf6_input
    character(len=*), intent(in) :: component_filename !< component (e.g. model) filename
    integer(I4B), intent(in) :: iout !< unit number for output
    type(BlockParserType), allocatable, target :: parser !< block parser
    type(LoadMf6FileType) :: loader
    integer(I4B) :: inunit
    !
    ! -- open input file
    inunit = open_mf6file(mf6_input%pkgtype, filename, component_filename, iout)
    !
    ! -- allocate and initialize parser
    allocate (parser)
    call parser%Initialize(inunit, iout)
    !
    ! -- invoke the selected load routine
    call loader%load(parser, mf6_input, filename, iout)
    !
    ! -- clear parser file handles
    call parser%clear()
    !
    ! -- cleanup
    deallocate (parser)
    !
    ! -- return
    return
  end subroutine input_load

  !> @brief static loader init
  !<
  subroutine static_init(this, mf6_input, component_name, component_input_name, &
                         input_name)
    class(Mf6FileStaticPkgLoadType), intent(inout) :: this
    type(ModflowInputType), intent(in) :: mf6_input
    character(len=*), intent(in) :: component_name
    character(len=*), intent(in) :: component_input_name
    character(len=*), intent(in) :: input_name
    !
    ! -- initialize base type
    call this%StaticPkgLoadType%init(mf6_input, component_name, &
                                     component_input_name, input_name)
    !
  end subroutine static_init

  !> @brief load routine for static loader
  !<
  function static_load(this, iout) result(rp_loader)
    class(Mf6FileStaticPkgLoadType), intent(inout) :: this
    integer(I4B), intent(in) :: iout
    class(DynamicPkgLoadBaseType), pointer :: rp_loader
    class(Mf6FileDynamicPkgLoadType), pointer :: mf6_loader
    !
    ! -- initialize return pointer
    nullify (rp_loader)
    !
    ! -- load model package to input context
    if (this%iperblock > 0) then
      !
      ! -- allocate dynamic loader
      allocate (mf6_loader)
      !
      ! -- initialize dynamic loader
      call mf6_loader%init(this%mf6_input, this%component_name, &
                           this%component_input_name, this%input_name, &
                           this%iperblock, iout)
      !
      ! -- set return pointer to base dynamic loader
      rp_loader => mf6_loader
      !
    else
      !
      ! -- load static input
      call input_load(this%input_name, this%mf6_input, &
                      this%component_input_name, iout)
    end if
    !
    ! -- return
    return
  end function static_load

  !> @brief static loader destroy
  !<
  subroutine static_destroy(this)
    class(Mf6FileStaticPkgLoadType), intent(inout) :: this
    !
    ! -- deallocate base type
    call this%StaticPkgLoadType%destroy()
    !
  end subroutine static_destroy

  !> @brief dynamic loader init
  !<
  subroutine dynamic_init(this, mf6_input, component_name, component_input_name, &
                          input_name, iperblock, iout)
    use InputDefinitionModule, only: InputParamDefinitionType
    use DefinitionSelectModule, only: get_param_definition_type
    use MemoryManagerModule, only: mem_allocate
    class(Mf6FileDynamicPkgLoadType), intent(inout) :: this
    type(ModflowInputType), intent(in) :: mf6_input
    character(len=*), intent(in) :: component_name
    character(len=*), intent(in) :: component_input_name
    character(len=*), intent(in) :: input_name
    integer(I4B), intent(in) :: iperblock
    integer(I4B), intent(in) :: iout
    integer(I4B) :: inunit
    !
    ! -- initialize base loader
    call this%DynamicPkgLoadType%init(mf6_input, component_name, &
                                      component_input_name, input_name, &
                                      iperblock, iout)
    !
    ! -- allocate scalars
    call mem_allocate(this%iper, 'IPER', this%mf6_input%mempath)
    call mem_allocate(this%ionper, 'IONPER', this%mf6_input%mempath)
    !
    ! -- initialize package
    nullify (this%rp_loader)
    this%iper = 0
    this%ionper = 0
    !
    ! -- open input file
    inunit = open_mf6file(mf6_input%pkgtype, input_name, &
                          component_input_name, iout)
    !
    ! -- allocate and initialize parser
    allocate (this%parser)
    call this%parser%Initialize(inunit, iout)
    !
    ! -- allocate and initialize loader
    call this%create_loader()
    !
    ! -- return
    return
  end subroutine dynamic_init

  !> @brief define routine for dynamic loader
  !<
  subroutine dynamic_df(this)
    class(Mf6FileDynamicPkgLoadType), intent(inout) :: this
    !
    ! -- invoke loader define
    call this%rp_loader%df()
    !
    ! -- read first ionper
    call this%read_ionper()
    !
    ! -- return
    return
  end subroutine dynamic_df

  !> @brief advance routine for dynamic loader
  !<
  subroutine dynamic_ad(this)
    class(Mf6FileDynamicPkgLoadType), intent(inout) :: this
    !
    ! -- invoke loader advance
    call this%rp_loader%ad()
    !
    ! -- return
    return
  end subroutine dynamic_ad

  !> @brief read and prepare routine for dynamic loader
  !<
  subroutine dynamic_rp(this)
    ! -- modules
    use TdisModule, only: kper, nper
    ! -- dummy
    class(Mf6FileDynamicPkgLoadType), intent(inout) :: this
    ! -- local
    !
    ! -- check if ready to load
    if (this%ionper /= kper) return
    !
    ! -- dynamic load
    call this%rp_loader%rp(this%parser)
    !
    ! -- update loaded iper
    this%iper = kper
    !
    ! -- read next iper
    if (kper < nper) then
      call this%read_ionper()
    else
      this%ionper = nper + 1
    end if
    !
    ! -- return
    return
  end subroutine dynamic_rp

  !> @brief dynamic loader read ionper of next period block
  !<
  subroutine dynamic_read_ionper(this)
    ! -- modules
    use TdisModule, only: kper, nper
    ! -- dummy
    class(Mf6FileDynamicPkgLoadType), intent(inout) :: this
    ! -- local
    character(len=LINELENGTH) :: line
    logical(LGP) :: isblockfound
    integer(I4B) :: ierr
    character(len=*), parameter :: fmtblkerr = &
      &"('Looking for BEGIN PERIOD iper.  Found ', a, ' instead.')"
    !
    call this%parser%GetBlock('PERIOD', isblockfound, ierr, &
                              supportOpenClose=.true., &
                              blockRequired=.false.)
    !
    ! -- set first period block IPER
    if (isblockfound) then
      !
      this%ionper = this%parser%GetInteger()
      !
      if (this%ionper <= this%iper) then
        write (errmsg, '(a, i0, a, i0, a, i0, a)') &
          'Error in stress period ', kper, &
          '. Period numbers not increasing.  Found ', this%ionper, &
          ' but last period block was assigned ', this%iper, '.'
        call store_error(errmsg)
        call this%parser%StoreErrorUnit()
      end if
      !
    else
      !
      ! -- PERIOD block not found
      if (ierr < 0) then
        ! -- End of file found; data applies for remainder of simulation.
        this%ionper = nper + 1
      else
        ! -- Found invalid block
        call this%parser%GetCurrentLine(line)
        write (errmsg, fmtblkerr) adjustl(trim(line))
        call store_error(errmsg)
        call this%parser%StoreErrorUnit()
      end if
    end if
    !
    ! -- return
    return
  end subroutine dynamic_read_ionper

  !> @brief allocate a dynamic loader based on load context
  !<
  subroutine dynamic_create_loader(this)
    use Mf6FileGridInputModule, only: BoundGridInputType
    use Mf6FileListInputModule, only: BoundListInputType
    ! -- dummy
    class(Mf6FileDynamicPkgLoadType), intent(inout) :: this
    class(BoundListInputType), pointer :: bndlist_loader
    class(BoundGridInputType), pointer :: bndgrid_loader
    !
    ! -- allocate and set loader
    if (this%readasarrays) then
      allocate (bndgrid_loader)
      this%rp_loader => bndgrid_loader
    else
      allocate (bndlist_loader)
      this%rp_loader => bndlist_loader
    end if
    !
    ! -- initialize loader
    call this%rp_loader%ainit(this%mf6_input, &
                              this%component_name, &
                              this%component_input_name, &
                              this%input_name, &
                              this%iperblock, &
                              this%parser, &
                              this%iout)
    !
    ! -- return
    return
  end subroutine dynamic_create_loader

  !> @brief dynamic loader destroy
  !<
  subroutine dynamic_destroy(this)
    use MemoryManagerModule, only: mem_deallocate
    class(Mf6FileDynamicPkgLoadType), intent(inout) :: this
    !
    ! -- deallocate loader
    call this%rp_loader%destroy()
    deallocate (this%rp_loader)
    !
    ! -- deallocate scalars
    call mem_deallocate(this%iper)
    call mem_deallocate(this%ionper)
    !
    ! -- deallocate parser
    call this%parser%clear()
    deallocate (this%parser)
    !
    ! -- deallocate input context
    call this%DynamicPkgLoadType%destroy()
    !
    ! -- return
    return
  end subroutine dynamic_destroy

  !> @brief open a model package files
  !<
  function open_mf6file(filetype, filename, component_fname, iout) result(inunit)
    ! -- modules
    use InputOutputModule, only: openfile, getunit
    ! -- dummy
    character(len=*), intent(in) :: filetype
    character(len=*), intent(in) :: filename
    character(len=*), intent(in) :: component_fname
    integer(I4B), intent(in) :: iout
    ! -- return
    integer(I4B) :: inunit
    ! -- local
    !
    ! -- initialize
    inunit = 0
    !
    if (filename /= '') then
      !
      ! -- get unit number, update object and open file
      inunit = getunit()
      call openfile(inunit, iout, trim(adjustl(filename)), filetype, &
                    'FORMATTED', 'SEQUENTIAL', 'OLD')
    else
      write (errmsg, '(a,a,a)') &
        'File unspecified, cannot load model or package &
        &type "', trim(filetype), '".'
      call store_error(errmsg)
      call store_error_filename(component_fname)
    end if
    !
    ! -- return
    return
  end function open_mf6file

end module IdmMf6FileModule
