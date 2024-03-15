!> @brief This module contains the ModflowInputModule
!!
!! This module contains a helper object and function
!! for accessing the ModflowInput, which is a
!! description of the structure of a modflow input
!! file.
!!
!<
module ModflowInputModule

  use KindModule, only: I4B, LGP
  use ConstantsModule, only: LENMEMPATH, LENCOMPONENTNAME, &
                             LENPACKAGETYPE, LENPACKAGENAME
  use MemoryHelperModule, only: create_mem_path
  use InputDefinitionModule, only: InputParamDefinitionType, &
                                   InputBlockDefinitionType
  use IdmDfnSelectorModule, only: block_definitions, &
                                  aggregate_definitions, &
                                  param_definitions
  use SimVariablesModule, only: idm_context

  implicit none
  private
  public :: ModflowInputType, getModflowInput

  !> @brief type for storing a description of a modflow input
  !!
  !! This type contains the information needed to read a
  !! specific modflow input file, including block definitions,
  !! aggregate definitions (structarrays), and individual
  !! parameter definitions.
  !!
  !<
  type ModflowInputType
    character(len=LENCOMPONENTNAME) :: pkgtype
    character(len=LENCOMPONENTNAME) :: component_type
    character(len=LENCOMPONENTNAME) :: subcomponent_type
    character(len=LENCOMPONENTNAME) :: component_name
    character(len=LENCOMPONENTNAME) :: subcomponent_name
    character(len=LENMEMPATH) :: mempath
    character(len=LENMEMPATH) :: component_mempath
    type(InputBlockDefinitionType), dimension(:), pointer :: block_dfns
    type(InputParamDefinitionType), dimension(:), pointer :: aggregate_dfns
    type(InputParamDefinitionType), dimension(:), pointer :: param_dfns
  end type ModflowInputType

contains

  !> @brief function to return ModflowInputType
  !<
  function getModflowInput(pkgtype, component_type, subcomponent_type, &
                           component_name, subcomponent_name, filename) &
    result(mf6_input)
    character(len=*), intent(in) :: pkgtype !< package type to load, such as DIS6, DISV6, NPF6
    character(len=*), intent(in) :: component_type !< component type, such as GWF or GWT
    character(len=*), intent(in) :: subcomponent_type !< subcomponent type, such as DIS or NPF
    character(len=*), intent(in) :: component_name !< component name, such as MYGWFMODEL
    character(len=*), intent(in) :: subcomponent_name !< subcomponent name, such as MYWELLPACKAGE
    character(len=*), optional, intent(in) :: filename !< optional name of package input file
    type(ModflowInputType) :: mf6_input
    character(len=LENPACKAGETYPE) :: dfn_subcomponent_type

    ! -- set subcomponent type
    if (present(filename)) then
      dfn_subcomponent_type = update_sc_type(pkgtype, filename, component_type, &
                                             subcomponent_type, component_name, &
                                             subcomponent_name)
    else
      dfn_subcomponent_type = trim(subcomponent_type)
    end if

    ! -- set input attributes
    mf6_input%pkgtype = trim(pkgtype)
    mf6_input%component_type = trim(component_type)
    mf6_input%subcomponent_type = trim(dfn_subcomponent_type)
    mf6_input%component_name = trim(component_name)
    mf6_input%subcomponent_name = trim(subcomponent_name)

    ! -- set mempaths
    mf6_input%mempath = create_mem_path(component_name, subcomponent_name, &
                                        idm_context)
    mf6_input%component_mempath = create_mem_path(component=component_name, &
                                                  context=idm_context)

    ! -- set input definitions
    mf6_input%block_dfns => block_definitions(mf6_input%component_type, &
                                              mf6_input%subcomponent_type)
    mf6_input%aggregate_dfns => aggregate_definitions(mf6_input%component_type, &
                                                      mf6_input%subcomponent_type)
    mf6_input%param_dfns => param_definitions(mf6_input%component_type, &
                                              mf6_input%subcomponent_type)
  end function getModflowInput

  function update_sc_type(filetype, filename, component_type, subcomponent_type, &
                          component_name, subcomponent_name) &
    result(sc_type)
    use SourceCommonModule, only: package_source_type
    use InputModelContextModule, only: GetModelNCContext
    use NCModelInputsModule, only: NCModelInputsType, NCModelPackageInputType
    character(len=*), intent(in) :: filetype
    character(len=*), intent(in) :: filename
    character(len=*), intent(in) :: component_type
    character(len=*), intent(in) :: subcomponent_type
    character(len=*), intent(in) :: component_name
    character(len=*), intent(in) :: subcomponent_name
    ! -- result
    character(len=LENPACKAGETYPE) :: sc_type
    ! -- local
    character(len=LENPACKAGENAME) :: source_type
    type(NCModelInputsType), pointer :: nc_context
    type(NCModelPackageInputType), pointer :: ncpkg
    !
    sc_type = subcomponent_type
    source_type = package_source_type(filename)
    !
    if (source_type == 'MF6FILE') then
      select case (subcomponent_type)
      case ('RCH', 'EVT', 'SCP')
        sc_type = read_as_arrays(filetype, filename, component_type, &
                                 subcomponent_type)
      case default
      end select
    else if (source_type == 'NETCDF4') then
      select case (subcomponent_type)
      case ('RCH', 'EVT', 'SCP')
        nc_context => GetModelNCContext(component_name)
        ncpkg => nc_context%get_package(component_name, &
                                        subcomponent_name)
        sc_type = trim(ncpkg%subcomponent_type)
      case default
      end select
    end if
    !
    ! -- return
    return
  end function update_sc_type

  function read_as_arrays(filetype, filename, component_type, subcomponent_type) &
    result(sc_type)
    use ConstantsModule, only: LINELENGTH
    use InputOutputModule, only: openfile, getunit
    use BlockParserModule, only: BlockParserType
    character(len=*), intent(in) :: component_type
    character(len=*), intent(in) :: subcomponent_type
    character(len=*), intent(in) :: filetype
    character(len=*), intent(in) :: filename
    ! -- result
    character(len=LENPACKAGETYPE) :: sc_type
    type(BlockParserType) :: parser
    integer(I4B) :: ierr, inunit
    logical(LGP) :: isfound
    logical(LGP) :: endOfBlock
    character(len=LINELENGTH) :: keyword
    !
    sc_type = subcomponent_type
    !
    inunit = getunit()
    !
    call openfile(inunit, 0, trim(adjustl(filename)), filetype, &
                  'FORMATTED', 'SEQUENTIAL', 'OLD')
    !
    call parser%Initialize(inunit, 0)
    !
    ! -- get options block
    call parser%GetBlock('OPTIONS', isfound, ierr, &
                         supportOpenClose=.true., blockRequired=.false.)
    !
    ! -- parse options block if detected
    if (isfound) then
      do
        call parser%GetNextLine(endOfBlock)
        !
        if (endOfBlock) exit
        !
        call parser%GetStringCaps(keyword)
        !
        if (keyword == 'READASARRAYS') then
          write (sc_type, '(a)') trim(subcomponent_type)//'A'
          exit
        end if
      end do
    end if
    !
    call parser%clear()
    !
    ! -- return
    return
  end function read_as_arrays

end module ModflowInputModule
