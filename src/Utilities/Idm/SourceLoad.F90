!> @brief This module contains the SourceLoadModule
!!
!! This module contains the routines needed to generate
!! a loader object for an input source and routines
!! that distribute processing to a particular source.
!!
!<
module SourceLoadModule

  use KindModule, only: DP, I4B, LGP
  use SimVariablesModule, only: errmsg
  use ConstantsModule, only: LINELENGTH, LENMEMPATH, LENMODELNAME, LENFTYPE, &
                             LENPACKAGETYPE, LENPACKAGENAME
  use SimModule, only: store_error, store_error_filename
  use ModflowInputModule, only: ModflowInputType, getModflowInput

  implicit none
  private
  public :: create_input_loader
  public :: open_source_file
  public :: load_modelnam, load_simnam
  public :: remote_model_ndim

contains

  !> @brief factory function to create and setup model package static loader
  !<
  function create_input_loader(component_type, subcomponent_type, &
                               component_name, subcomponent_name, input_type, &
                               input_fname, component_fname) result(loader)
    use SourceCommonModule, only: package_source_type, idm_subcomponent_name
    use InputLoadTypeModule, only: StaticPkgLoadBaseType
    character(len=*), intent(in) :: component_type
    character(len=*), intent(in) :: subcomponent_type
    character(len=*), intent(in) :: component_name
    character(len=*), intent(in) :: subcomponent_name
    character(len=*), intent(in) :: input_type
    character(len=*), intent(in) :: input_fname
    character(len=*), intent(in) :: component_fname
    class(StaticPkgLoadBaseType), pointer :: loader
    type(ModflowInputType) :: mf6_input
    character(len=LENPACKAGENAME) :: source_type
    character(len=LENPACKAGENAME) :: sc_name
    !
    ! -- set subcomponent name
    sc_name = idm_subcomponent_name(component_type, subcomponent_type, &
                                    subcomponent_name)
    !
    ! -- create description of input
    mf6_input = getModflowInput(input_type, component_type, subcomponent_type, &
                                component_name, sc_name, input_fname)
    !
    ! -- set package source
    source_type = package_source_type(input_fname)
    !
    ! -- set source loader for model package
    loader => package_loader(source_type)
    !
    ! -- initialize loader
    call loader%init(mf6_input, component_name, component_fname, input_fname)
    !
    ! -- return
    return
  end function create_input_loader

  !> @brief allocate source model package static loader
  !<
  function package_loader(source_type) result(loader)
    use InputLoadTypeModule, only: StaticPkgLoadBaseType
    use IdmMf6FileModule, only: Mf6FileStaticPkgLoadType
    character(len=*), intent(inout) :: source_type
    class(Mf6FileStaticPkgLoadType), pointer :: mf6file_loader
    class(StaticPkgLoadBaseType), pointer :: loader
    !
    ! -- initialize
    nullify (loader)
    !
    ! -- allocate derived object
    select case (source_type)
    case ('MF6FILE')
      allocate (mf6file_loader)
      loader => mf6file_loader
    case default
      write (errmsg, '(a)') &
        'Simulation package input source type "'//trim(source_type)// &
        '" not currently supported.'
      call store_error(errmsg, .true.)
    end select
    !
    ! -- return
    return
  end function package_loader

  function open_source_file(pkgtype, filename, modelfname, iout) result(fd)
    use SourceCommonModule, only: package_source_type
    use IdmMf6FileModule, only: open_mf6file
    character(len=*), intent(in) :: pkgtype
    character(len=*), intent(in) :: filename
    character(len=*), intent(in) :: modelfname
    integer(I4B), intent(in) :: iout
    integer(I4B) :: fd
    character(len=LENPACKAGENAME) :: source_type
    !
    ! -- initialize
    fd = 0
    !
    ! -- set source type
    source_type = package_source_type(filename)
    !
    select case (source_type)
    case ('MF6FILE')
      fd = open_mf6file(pkgtype, filename, modelfname, iout)
    case default
    end select
    !
    ! -- return
    return
  end function open_source_file

  subroutine load_modelnam(mtype, mfname, mname, iout)
    use SimVariablesModule, only: simfile
    use SourceCommonModule, only: package_source_type, idm_component_type
    use IdmMf6FileModule, only: input_load
    character(len=*), intent(in) :: mtype
    character(len=*), intent(in) :: mfname
    character(len=*), intent(in) :: mname
    integer(I4B), intent(in) :: iout
    type(ModflowInputType) :: mf6_input
    character(len=LENPACKAGENAME) :: source_type
    !
    ! -- set source type
    source_type = package_source_type(mfname)
    !
    ! -- create description of input
    mf6_input = getModflowInput(mtype, idm_component_type(mtype), 'NAM', &
                                mname, 'NAM', mfname)
    !
    select case (source_type)
    case ('MF6FILE')
      call input_load(mfname, mf6_input, simfile, iout)
    case default
    end select
    !
    ! -- return
    return
  end subroutine load_modelnam

  subroutine load_simnam()
    use SimVariablesModule, only: simfile, iout
    use MessageModule, only: write_message
    use IdmMf6FileModule, only: input_load
    type(ModflowInputType) :: mf6_input
    character(len=LINELENGTH) :: line
    logical :: lexist
    !
    ! -- load mfsim.nam if it exists
    inquire (file=trim(adjustl(simfile)), exist=lexist)
    !
    if (lexist) then
      !
      ! -- write name of namfile to stdout
      write (line, '(2(1x,a))') 'Using Simulation name file:', &
        trim(adjustl(simfile))
      call write_message(line, skipafter=1)
      !
      ! -- create description of input
      mf6_input = getModflowInput('NAM6', 'SIM', 'NAM', 'SIM', 'NAM', simfile)
      !
      ! -- open namfile and load to input context
      call input_load(simfile, mf6_input, simfile, iout)
    end if
    !
    ! -- return
    return
  end subroutine load_simnam

  function remote_model_ndim(mtype, mfname) result(ncelldim)
    use SourceCommonModule, only: package_source_type
    use ConstantsModule, only: LINELENGTH
    use InputOutputModule, only: openfile, getunit
    use BlockParserModule, only: BlockParserType
    character(len=*), intent(in) :: mtype
    character(len=*), intent(in) :: mfname
    integer(I4B) :: ncelldim
    character(len=LENPACKAGENAME) :: source_type
    type(BlockParserType) :: parser
    integer(I4B) :: ierr, inunit
    logical(LGP) :: isfound, endOfBlock
    character(len=LINELENGTH) :: ptype
    !
    ! -- initialize
    ncelldim = 0
    !
    ! -- set source type
    source_type = package_source_type(mfname)
    !
    select case (source_type)
    case ('MF6FILE')
      !
      ! -- open name file
      inunit = getunit()
      call openfile(inunit, 0, trim(adjustl(mfname)), mtype, &
                    'FORMATTED', 'SEQUENTIAL', 'OLD')
      !
      ! -- initialize parser
      call parser%Initialize(inunit, 0)
      !
      ! -- get options block
      call parser%GetBlock('OPTIONS', isfound, ierr, &
                           supportOpenClose=.true., blockRequired=.false.)
      ! -- iterate through options
      if (isfound) then
        do
          call parser%GetNextLine(endOfBlock)
          if (endOfBlock) exit
        end do
      end if
      !
      ! -- get packages block
      call parser%GetBlock('PACKAGES', isfound, ierr, &
                           supportOpenClose=.true., blockRequired=.true.)
      if (isfound) then
        ! -- read through packages
        do
          call parser%GetNextLine(endOfBlock)
          if (endOfBlock) exit
          !
          call parser%GetStringCaps(ptype)
          !
          select case (ptype)
          case ('DIS6')
            ncelldim = 3
            exit
          case ('DISV6')
            ncelldim = 2
            exit
          case ('DISU6')
            ncelldim = 1
            exit
          case default
          end select
        end do
      end if
      !
      call parser%clear()
      !
    case default
    end select
    !
    ! -- return
    return
  end function remote_model_ndim

end module SourceLoadModule
