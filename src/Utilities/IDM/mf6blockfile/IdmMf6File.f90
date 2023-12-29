!> @brief This module contains the IdmMf6FileModule
!!
!! This module contains high-level routines for loading
!! MODFLOW 6 ASCII source input.
!!
!<
module IdmMf6FileModule

  use KindModule, only: DP, I4B, LGP
  use SimVariablesModule, only: errmsg
  use ConstantsModule, only: LINELENGTH, LENMEMPATH, LENMODELNAME, &
                             LENPACKAGENAME, LENFTYPE, LENPACKAGETYPE, &
                             LENAUXNAME, LENBOUNDNAME, LENTIMESERIESNAME, &
                             LENLISTLABEL, LENVARNAME, DNODATA, &
                             DZERO, IZERO
  use SimModule, only: store_error, store_error_filename
  use InputOutputModule, only: openfile, getunit
  use BlockParserModule, only: BlockParserType
  use ModflowInputModule, only: ModflowInputType, getModflowInput
  use CharacterStringModule, only: CharacterStringType
  use InputLoadTypeModule, only: StaticPkgLoadBaseType, DynamicPkgLoadBaseType
  use AsciiInputLoadTypeModule, only: AsciiDynamicPkgLoadBaseType

  implicit none
  private
  public :: input_load
  public :: Mf6FileStaticPkgLoadType, Mf6FileDynamicPkgLoadType
  public :: open_mf6file

  !> @brief derived type for storing package loader
  !!
  !! This derived type is used to store a pointer to a
  !! package load procedure.  This could be used to write
  !! a custom package loader as a way to override the
  !! generic_mf6_load routine.
  !!
  !<
  type :: PackageLoad
    procedure(IPackageLoad), nopass, pointer, public :: load_package => null() !< procedure pointer to the load routine
  end type PackageLoad

  abstract interface
    subroutine IPackageLoad(parser, mf6_input, iout)
      use KindModule, only: DP, I4B
      use BlockParserModule, only: BlockParserType
      use ModflowInputModule, only: ModflowInputType
      type(BlockParserType), intent(inout) :: parser !< block parser
      type(ModflowInputType), intent(in) :: mf6_input !< description of input
      integer(I4B), intent(in) :: iout !< unit number for output
    end subroutine IPackageLoad
  end interface

  !> @brief MF6File static loader derived type
  !<
  type, extends(StaticPkgLoadBaseType) :: Mf6FileStaticPkgLoadType
  contains
    procedure :: init => static_init
    procedure :: load => static_load
    procedure :: destroy => static_destroy
  end type Mf6FileStaticPkgLoadType

  !> @brief MF6File dynamic loader derived type
  !<
  type, extends(DynamicPkgLoadBaseType) :: Mf6FileDynamicPkgLoadType
    type(BlockParserType), pointer :: parser !< parser for MF6File period blocks
    integer(I4B), pointer :: iper => null()
    integer(I4B), pointer :: ionper => null()
    class(AsciiDynamicPkgLoadBaseType), pointer :: block_loader => null()
  contains
    procedure :: init => dynamic_init
    procedure :: df => dynamic_df
    procedure :: ad => dynamic_ad
    procedure :: set => dynamic_set
    procedure :: rp => dynamic_rp
    procedure :: read_ionper => dynamic_read_ionper
    procedure :: create_loader => dynamic_create_loader
    procedure :: destroy => dynamic_destroy
  end type Mf6FileDynamicPkgLoadType

contains

  !> @brief generic procedure to MODFLOW 6 load routine
  !<
  subroutine generic_mf6_load(parser, mf6_input, iout)
    use LoadMf6FileModule, only: idm_load
    type(BlockParserType), intent(inout) :: parser !< block parser
    type(ModflowInputType), intent(in) :: mf6_input !< description of input
    integer(I4B), intent(in) :: iout !< unit number for output

    call idm_load(parser, mf6_input, iout)

  end subroutine generic_mf6_load

  !> @brief input load for traditional mf6 simulation input file
  !<
  subroutine input_load(filename, mf6_input, component_filename, iout, &
                        mf6_parser)
    character(len=*), intent(in) :: filename
    type(ModflowInputType), intent(in) :: mf6_input
    character(len=*), intent(in) :: component_filename !< component (e.g. model) filename
    integer(I4B), intent(in) :: iout !< unit number for output
    type(BlockParserType), pointer, optional, intent(inout) :: mf6_parser
    type(BlockParserType), allocatable, target :: parser !< block parser
    type(PackageLoad) :: pkgloader
    integer(I4B) :: inunit
    !
    ! -- set parser based package loader by file type
    select case (mf6_input%pkgtype)
    case default
      !
      ! -- open input file
      inunit = open_mf6file(mf6_input%pkgtype, filename, component_filename, iout)
      !
      ! -- allocate and initialize parser
      allocate (parser)
      call parser%Initialize(inunit, iout)
      !
      ! -- set load interface
      pkgloader%load_package => generic_mf6_load
      !
    end select
    !
    ! -- invoke the selected load routine
    call pkgloader%load_package(parser, mf6_input, iout)
    !
    ! -- generate a dynamic loader parser if requested
    if (present(mf6_parser)) then
      !
      ! -- create dynamic parser
      allocate (mf6_parser, source=parser)
    else
      !
      ! -- clear parser file handles
      call parser%clear()
    end if
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
    call this%StaticPkgLoadType%init(mf6_input, component_name, &
                                     component_input_name, input_name)
    !
  end subroutine static_init

  !> @brief load routine for static loader
  !<
  function static_load(this, iout) result(period_loader)
    class(Mf6FileStaticPkgLoadType), intent(inout) :: this
    integer(I4B), intent(in) :: iout
    class(DynamicPkgLoadBaseType), pointer :: period_loader
    class(Mf6FileDynamicPkgLoadType), pointer :: mf6_loader => null()
    type(BlockParserType), pointer :: parser => null()
    !
    ! -- initialize
    nullify (period_loader)
    !
    ! -- load model package to input context
    if (this%iperblock > 0) then
      !
      ! -- package is dynamic, allocate loader
      allocate (mf6_loader)
      !
      ! -- load static input
      call input_load(this%input_name, this%mf6_input, &
                      this%component_input_name, iout, parser)
      !
      ! -- initialize dynamic loader
      call mf6_loader%init(this%mf6_input, this%component_name, &
                           this%component_input_name, this%input_name, &
                           this%iperblock, iout)
      !
      ! -- set parser
      call mf6_loader%set(parser)
      !
      ! -- set return pointer to base dynamic loader
      period_loader => mf6_loader
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
    call this%StaticPkgLoadType%destroy()
    !
  end subroutine static_destroy

  !> @brief dynamic loader init
  !<
  subroutine dynamic_init(this, mf6_input, modelname, modelfname, source, &
                          iperblock, iout)
    use InputDefinitionModule, only: InputParamDefinitionType
    use DefinitionSelectModule, only: get_param_definition_type
    use MemoryManagerModule, only: mem_allocate
    class(Mf6FileDynamicPkgLoadType), intent(inout) :: this
    type(ModflowInputType), intent(in) :: mf6_input
    character(len=*), intent(in) :: modelname
    character(len=*), intent(in) :: modelfname
    character(len=*), intent(in) :: source
    integer(I4B), intent(in) :: iperblock
    integer(I4B), intent(in) :: iout
    !
    call this%DynamicPkgLoadType%init(mf6_input, modelname, modelfname, &
                                      source, iperblock, iout)
    !
    call mem_allocate(this%iper, 'IPER', this%mf6_input%mempath)
    call mem_allocate(this%ionper, 'IONPER', this%mf6_input%mempath)
    !
    this%iper = 0
    this%ionper = 0
    !
    ! -- allocate and initialize loader
    call this%create_loader()
    !
    ! -- return
    return
  end subroutine dynamic_init

  !> @brief dynamic loader set parser object
  !<
  subroutine dynamic_set(this, parser)
    use InputDefinitionModule, only: InputParamDefinitionType
    use DefinitionSelectModule, only: get_param_definition_type
    class(Mf6FileDynamicPkgLoadType), intent(inout) :: this
    type(BlockParserType), pointer, intent(inout) :: parser
    !
    ! -- set the parser
    this%parser => parser
    !
    ! -- return
    return
  end subroutine dynamic_set

  !> @brief define routine for dynamic loader
  !<
  subroutine dynamic_df(this)
    class(Mf6FileDynamicPkgLoadType), intent(inout) :: this
    !
    ! -- read first iper
    call this%read_ionper()
    !
    call this%block_loader%df()
    !
    ! -- return
    return
  end subroutine dynamic_df

  !> @brief advance routine for dynamic loader
  !<
  subroutine dynamic_ad(this)
    class(Mf6FileDynamicPkgLoadType), intent(inout) :: this
    !
    call this%block_loader%ad()
    !
    ! -- return
    return
  end subroutine dynamic_ad

  !> @brief read and prepare routine for dynamic loader
  !<
  subroutine dynamic_rp(this)
    ! -- modules
    use TdisModule, only: kper, nper
    use MemoryManagerModule, only: mem_setptr
    ! -- dummy
    class(Mf6FileDynamicPkgLoadType), intent(inout) :: this
    ! -- locals
    !
    ! -- check if ready to load
    if (this%ionper /= kper) return
    !
    ! -- dynamic load
    call this%block_loader%rp(this%parser)
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
    ! -- locals
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
    use StressListInputModule, only: StressListInputType
    use StressGridInputModule, only: StressGridInputType
    ! -- dummy
    class(Mf6FileDynamicPkgLoadType), intent(inout) :: this
    class(StressListInputType), pointer :: list_loader
    class(StressGridInputType), pointer :: grid_loader
    !
    ! -- allocate and set loader
    if (this%readasarrays) then
      allocate (grid_loader)
      this%block_loader => grid_loader
    else
      allocate (list_loader)
      this%block_loader => list_loader
    end if
    !
    ! -- initialize loader
    call this%block_loader%init(this%mf6_input, &
                                this%modelname, &
                                this%modelfname, &
                                this%sourcename, &
                                this%iperblock, &
                                this%iout)
    !
    ! -- return
    return
  end subroutine dynamic_create_loader

  !> @brief dynamic loader destroy
  !<
  subroutine dynamic_destroy(this)
    class(Mf6FileDynamicPkgLoadType), intent(inout) :: this
    !
    ! -- deallocate input context
    !call this%DynamicPkgLoadType%destroy()
    !
    ! -- deallocate loader
    call this%block_loader%destroy()
    deallocate (this%block_loader)
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
    ! -- dummy
    character(len=*), intent(in) :: filetype
    character(len=*), intent(in) :: filename
    character(len=*), intent(in) :: component_fname
    integer(I4B), intent(in) :: iout
    ! -- return
    integer(I4B) :: inunit
    ! -- locals
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
