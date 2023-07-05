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
                             LENLISTLABEL
  use SimModule, only: store_error, store_error_filename
  use InputOutputModule, only: openfile, getunit
  use BlockParserModule, only: BlockParserType
  use ModflowInputModule, only: ModflowInputType, getModflowInput
  use CharacterStringModule, only: CharacterStringType
  use InputLoadTypeModule, only: StaticPkgLoadBaseType, DynamicPkgLoadBaseType

  implicit none
  private
  public :: input_load, model_load
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
  contains
    procedure :: init => dynamic_init
    procedure :: set => dynamic_set
    procedure :: period_load => dynamic_load
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

    call idm_load(parser, mf6_input%pkgtype, &
                  mf6_input%component_type, mf6_input%subcomponent_type, &
                  mf6_input%component_name, mf6_input%subcomponent_name, &
                  iout)

  end subroutine generic_mf6_load

  function create_dynamic_parser(mf6_input, mf6_parser, static_parser) &
    result(created)
    type(ModflowInputType), intent(in) :: mf6_input
    type(BlockParserType), pointer, intent(inout) :: mf6_parser
    type(BlockParserType), allocatable, target, intent(inout) :: static_parser
    logical(LGP) :: created
    integer(I4B) :: iblock
    !
    ! -- initialize
    nullify (mf6_parser)
    created = .false.
    !
    ! -- check if package has dynamic input
    do iblock = 1, size(mf6_input%block_dfns)
      !
      if (mf6_input%block_dfns(iblock)%blockname == 'PERIOD') then
        !
        ! -- dynamic package, allocate parser
        allocate (mf6_parser, source=static_parser)
        created = .true.
        !
        exit
        !
      end if
    end do
    !
    ! -- return
    return
  end function

  !> @brief input load for traditional mf6 simulation input file
  !<
  subroutine input_load(filename, pkgtype, &
                        component_type, subcomponent_type, &
                        component_name, subcomponent_name, &
                        component_filename, iout, mf6_parser)
    character(len=*), intent(in) :: filename
    character(len=*), intent(in) :: pkgtype !< pkgtype to load, such as DIS6, DISV6, NPF6
    character(len=*), intent(in) :: component_type !< component type, such as GWF or GWT
    character(len=*), intent(in) :: subcomponent_type !< subcomponent type, such as DIS or NPF
    character(len=*), intent(in) :: component_name !< component name, such as MYGWFMODEL
    character(len=*), intent(in) :: subcomponent_name !< subcomponent name, such as MYWELLPACKAGE
    character(len=*), intent(in) :: component_filename !< component (e.g. model) filename
    integer(I4B), intent(in) :: iout !< unit number for output
    type(BlockParserType), pointer, optional, intent(inout) :: mf6_parser
    type(BlockParserType), allocatable, target :: parser !< block parser
    type(ModflowInputType) :: mf6_input
    type(PackageLoad) :: pkgloader
    integer(I4B) :: inunit
    logical(LGP) :: created = .false.
    !
    ! -- create description of input
    mf6_input = getModflowInput(pkgtype, component_type, &
                                subcomponent_type, component_name, &
                                subcomponent_name)
    !
    ! -- set parser based package loader by file type
    select case (pkgtype)
    case default
      !
      ! -- open input file
      inunit = open_mf6file(pkgtype, filename, component_filename, iout)
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
    ! -- generate a dynamic loader parser if requested and relevant
    if (present(mf6_parser)) then
      !
      ! -- create dynamic parser
      created = create_dynamic_parser(mf6_input, mf6_parser, parser)
    end if
    !
    ! -- deallocate static load parser
    if (allocated(parser)) then
      !
      if (.not. created) call parser%clear()
      deallocate (parser)
      !
    end if
    !
    ! -- return
    return
  end subroutine input_load

  subroutine static_init(this, mf6_input, modelname, modelfname, source)
    class(Mf6FileStaticPkgLoadType), intent(inout) :: this
    type(ModflowInputType), intent(in) :: mf6_input
    character(len=*), intent(in) :: modelname
    character(len=*), intent(in) :: modelfname
    character(len=*), intent(in) :: source
    !
    call this%StaticPkgLoadType%init(mf6_input, modelname, modelfname, source)
    !
  end subroutine static_init

  function static_load(this, iout) result(period_loader)
    class(Mf6FileStaticPkgLoadType), intent(inout) :: this
    integer(I4B), intent(in) :: iout
    class(DynamicPkgLoadBaseType), pointer :: period_loader
    class(Mf6FileDynamicPkgLoadType), pointer :: mf6file_period_loader => null()
    type(BlockParserType), pointer :: parser => null()
    !
    ! -- initialize
    nullify (period_loader)
    !
    ! -- load model package to input context
    call input_load(this%sourcename, this%mf6_input%pkgtype, &
                    this%mf6_input%component_type, &
                    this%mf6_input%subcomponent_type, &
                    this%mf6_input%component_name, &
                    this%mf6_input%subcomponent_name, &
                    this%modelfname, iout, parser)
    !
    if (associated(parser)) then
      !
      ! -- package is dynamic, allocate loader
      allocate (mf6file_period_loader)
      !
      ! -- initialize dynamic loader
      call mf6file_period_loader%init(this%mf6_input, this%modelname, &
                                      this%modelfname, this%sourcename, iout)
      !
      ! -- set parser
      call mf6file_period_loader%set(parser, iout)
      !
      ! -- set return pointer to base dynamic loader
      period_loader => mf6file_period_loader
      !
    end if
    !
  end function static_load

  subroutine static_destroy(this)
    class(Mf6FileStaticPkgLoadType), intent(inout) :: this
    !
    call this%StaticPkgLoadType%destroy()
    !
  end subroutine static_destroy

  subroutine dynamic_init(this, mf6_input, modelname, modelfname, source, iout)
    class(Mf6FileDynamicPkgLoadType), intent(inout) :: this
    type(ModflowInputType), intent(in) :: mf6_input
    character(len=*), intent(in) :: modelname
    character(len=*), intent(in) :: modelfname
    character(len=*), intent(in) :: source
    integer(I4B), intent(in) :: iout
    !
    call this%DynamicPkgLoadType%init(mf6_input, modelname, modelfname, &
                                      source, iout)
    !
    ! -- return
    return
  end subroutine dynamic_init

  subroutine dynamic_set(this, parser, iout)
    class(Mf6FileDynamicPkgLoadType), intent(inout) :: this
    type(BlockParserType), pointer, intent(inout) :: parser
    integer(I4B), intent(in) :: iout
    !
    ! -- Not currently implemented
    !
    ! -- return
    return
  end subroutine dynamic_set

  subroutine dynamic_load(this, iout)
    ! -- modules
    ! -- dummy
    class(Mf6FileDynamicPkgLoadType), intent(inout) :: this
    integer(I4B), intent(in) :: iout
    ! -- locals
    !
    ! -- Not currently implemented
    write (errmsg, '(a)') &
      'MODFLOW 6 internal error: input context dynamic load &
      &not implemented'
    call store_error(errmsg)
    call store_error_filename(this%modelfname)
    !
    ! -- return
    return
  end subroutine dynamic_load

  subroutine dynamic_destroy(this)
    class(Mf6FileDynamicPkgLoadType), intent(inout) :: this
    !
    call this%parser%clear()
    deallocate (this%parser)
    !
    call this%DynamicPkgLoadType%destroy()
    !
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

  !> @brief input load a single model namfile and model package files
  !<
  subroutine model_load(component, mtype, mfname, mname, iout)
    ! -- modules
    use SimVariablesModule, only: simfile
    ! -- dummy
    character(len=LENFTYPE), intent(in) :: component
    character(len=*), intent(in) :: mtype
    character(len=*), intent(in) :: mfname
    character(len=*), intent(in) :: mname
    integer(I4B), intent(in) :: iout
    ! -- locals
    !
    ! -- load model namfile to the input context
    call input_load(mfname, mtype, component, 'NAM', mname, 'NAM', simfile, iout)
    !
    ! -- return
    return
  end subroutine model_load

end module IdmMf6FileModule
