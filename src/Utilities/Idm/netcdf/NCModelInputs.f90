!> @brief This module contains the NCModelInputsModule
!!
!! These data structures organize model package input information
!! associated with a single model netcdf input file.  It contains
!! an ordered package load list.  Each package contains a blocklist,
!! ordered according to the definition, which contains variable
!! descriptions.
!!
!<
module NCModelInputsModule

  use KindModule, only: DP, I4B, LGP
  use ConstantsModule, only: LINELENGTH, LENCOMPONENTNAME, LENPACKAGETYPE
  use SimModule, only: store_error, store_error_filename
  use SimVariablesModule, only: errmsg
  use ListModule, only: ListType
  use ArrayHandlersModule, only: expandarray
  use InputDefinitionModule, only: InputParamDefinitionType, &
                                   InputBlockDefinitionType
  use IdmDfnSelectorModule, only: param_definitions, &
                                  block_definitions

  implicit none
  private
  public :: NCModelPackageInputType
  public :: NCModelInputsType
  public :: NCBlockVariablesType

  !> @brief NetCDF model input package block variables
  !<
  type :: NCBlockVariablesType
    character(len=LINELENGTH), dimension(:), allocatable :: tagnames !< variable tagname
    integer(I4B), dimension(:), allocatable :: varids !< netcdf file variable id
    character(len=LENCOMPONENTNAME) :: blockname !< block name
    logical(LGP) :: aggregate !< is block aggregate
    integer(I4B) :: varnum !< number of package variables in file
  contains
    procedure :: create => blockvars_create
    procedure :: add => blockvars_add
    procedure :: destroy => blockvars_destroy
  end type NCBlockVariablesType

  !> @brief NetCDF model input package description
  !<
  type :: NCModelPackageInputType
    character(len=LENCOMPONENTNAME) :: component_type !< component type
    character(len=LENCOMPONENTNAME) :: subcomponent_type !< subcomponent type
    character(len=LENCOMPONENTNAME) :: component_name !< component name
    character(len=LENCOMPONENTNAME) :: subcomponent_name !< subcomponent name
    type(NCBlockVariablesType), dimension(:), allocatable :: blocklist !< dfn ordered blocklist
    integer(I4B), dimension(:), allocatable :: ipers !< netcdf file package ipers array
    integer(I4B) :: iperblock !< index of period block in blocklist
    logical(LGP) :: ts_active !< is package timeseries active
  contains
    procedure :: init => ncpkg_init
    procedure :: reset => ncpkg_reset
    procedure :: destroy => ncpkg_destroy
    procedure :: add => ncpkg_add
  end type NCModelPackageInputType

  !> @brief NetCDF model input description and ordered package list
  !<
  type :: NCModelInputsType
    character(len=LENPACKAGETYPE) :: modeltype !< model type, e.g. GWF6
    character(len=LENCOMPONENTNAME) :: component_type !< component type, e.g. GWF
    character(len=LENCOMPONENTNAME) :: modelname !< model name, e.g. EVT02
    character(len=LINELENGTH) :: modelfname !< model input file name
    integer(I4B) :: ncid !< id of open netcdf file
    type(ListType) :: pkglist !< list of packages
  contains
    procedure :: init => ncmodelinputs_init
    procedure :: add => ncmodelinputs_add
    procedure :: get => ncmodelinputs_get
    procedure :: get_package => ncmodelinputs_get_package
    procedure :: has_package => ncmodelinputs_has_package
    procedure :: destroy => ncmodelinputs_destroy
  end type NCModelInputsType

contains

  !> @brief create a new package type
  !<
  subroutine blockvars_create(this, blockname, aggregate)
    ! -- modules
    ! -- dummy
    class(NCBlockVariablesType) :: this
    character(len=*), intent(in) :: blockname
    logical(LGP), intent(in) :: aggregate
    ! -- local
    !
    ! -- initialize
    this%blockname = blockname
    this%aggregate = aggregate
    this%varnum = 0
    !
    ! -- allocate arrays
    allocate (this%tagnames(0))
    allocate (this%varids(0))
    !
    ! -- return
    return
  end subroutine blockvars_create

  !> @brief add a new package instance to this package type
  !<
  subroutine blockvars_add(this, varname, varid)
    ! -- modules
    ! -- dummy
    class(NCBlockVariablesType) :: this
    character(len=*), intent(in) :: varname
    integer(I4B), intent(in) :: varid
    ! -- local
    !
    ! -- reallocate
    call expandarray(this%tagnames)
    call expandarray(this%varids)
    !
    ! -- add new package instance
    this%varnum = this%varnum + 1
    this%tagnames(this%varnum) = varname
    this%varids(this%varnum) = varid
    !
    ! -- return
    return
  end subroutine blockvars_add

  !> @brief deallocate object
  !<
  subroutine blockvars_destroy(this)
    ! -- modules
    ! -- dummy
    class(NCBlockVariablesType) :: this
    ! -- local
    !
    ! -- deallocate dynamic arrays
    if (allocated(this%tagnames)) deallocate (this%tagnames)
    if (allocated(this%varids)) deallocate (this%varids)
    !
    ! -- return
    return
  end subroutine blockvars_destroy

  !> @brief initialize model package inputs object
  !<
  subroutine ncpkg_init(this, pkgtype, component_type, subcomponent_type, &
                        component_name, subcomponent_name, filename)
    ! -- modules
    ! -- dummy
    class(NCModelPackageInputType) :: this
    character(len=*) :: pkgtype
    character(len=*) :: component_type
    character(len=*) :: subcomponent_type
    character(len=*) :: component_name
    character(len=*) :: subcomponent_name
    character(len=*) :: filename
    ! -- local
    character(len=LINELENGTH) :: blockname
    type(InputBlockDefinitionType), dimension(:), &
      pointer :: block_dfns
    integer(I4B) :: iblock
    !
    ! -- initialize object
    this%component_type = component_type
    this%subcomponent_type = subcomponent_type
    this%component_name = component_name
    this%subcomponent_name = subcomponent_name
    this%iperblock = 0
    this%ts_active = .false.
    !
    ! -- set pointer to block definition list
    block_dfns => block_definitions(component_type, subcomponent_type)
    !
    ! allocate blocklist
    allocate (this%blocklist(size(block_dfns)))
    !
    ! -- create blocklist types
    do iblock = 1, size(block_dfns)
      blockname = block_dfns(iblock)%blockname
      if (blockname == 'PERIOD') this%iperblock = iblock
      call this%blocklist(iblock)%create(blockname, &
                                         block_dfns(iblock)%aggregate)
    end do
    !
    ! -- return
    return
  end subroutine ncpkg_init

  !> @brief reset model nc package blocklist
  !!
  !! This routine resets the blocklist.  Needed for array based inputs
  !! because the model namefile packages block will specify RCH6, for
  !! example, in reference to either the array or list based definition
  !! set. This routine is called when the definition set should be array
  !! but was initialized as list. It should only be called once per
  !! package when the first input variable is read.
  !!
  !<
  subroutine ncpkg_reset(this, pkgtype, subcomponent_type)
    ! -- modules
    ! -- dummy
    class(NCModelPackageInputType) :: this
    character(len=*) :: pkgtype
    character(len=*) :: subcomponent_type
    ! -- local
    character(len=LINELENGTH) :: blockname
    type(InputBlockDefinitionType), dimension(:), &
      pointer :: block_dfns
    integer(I4B) :: n, iblock
    !
    ! -- destroy blocklist
    do n = 1, size(this%blocklist)
      call this%blocklist(n)%destroy()
    end do
    !
    if (allocated(this%blocklist)) deallocate (this%blocklist)
    !
    ! -- reset subcomponent_type
    this%subcomponent_type = subcomponent_type
    !
    ! -- set pointer to block definition list
    block_dfns => block_definitions(this%component_type, this%subcomponent_type)
    !
    ! allocate blocklist
    allocate (this%blocklist(size(block_dfns)))
    !
    ! -- create blocklist types
    do iblock = 1, size(block_dfns)
      blockname = block_dfns(iblock)%blockname
      if (blockname == 'PERIOD') this%iperblock = iblock
      call this%blocklist(iblock)%create(blockname, &
                                         block_dfns(iblock)%aggregate)
    end do
    !
    ! -- return
    return
  end subroutine ncpkg_reset

  !> @brief add package variable info to package type list
  !<
  subroutine ncpkg_add(this, blockname, varname, varid)
    ! -- modules
    use InputOutputModule, only: lowcase, upcase
    ! -- dummy
    class(NCModelPackageInputType) :: this
    character(len=*), intent(inout) :: blockname
    character(len=*), intent(inout) :: varname
    integer(I4B) :: varid
    ! -- local
    integer(I4B) :: n
    !
    ! -- set to upper case
    call upcase(blockname)
    call upcase(varname)
    !
    ! -- locate index of block in blocklist
    do n = 1, size(this%blocklist)
      if (this%blocklist(n)%blockname == blockname) then
        call this%blocklist(n)%add(varname, varid)
        exit
      end if
    end do
    !
    ! -- return
    return
  end subroutine ncpkg_add

  !> @brief deallocate object
  !<
  subroutine ncpkg_destroy(this)
    ! -- modules
    ! -- dummy
    class(NCModelPackageInputType) :: this
    ! -- local
    integer(I4B) :: n
    !
    ! --
    do n = 1, size(this%blocklist)
      call this%blocklist(n)%destroy()
    end do
    !
    if (allocated(this%ipers)) deallocate (this%ipers)
    if (allocated(this%blocklist)) deallocate (this%blocklist)
    !
    ! -- return
    return
  end subroutine ncpkg_destroy

  !> @brief init object
  !<
  subroutine ncmodelinputs_init(this, modeltype, component_type, &
                                modelname, modelfname, ncid)
    ! -- modules
    ! -- dummy
    class(NCModelInputsType) :: this
    character(len=*) :: modeltype
    character(len=*) :: component_type
    character(len=*) :: modelname
    character(len=*) :: modelfname
    integer(I4B) :: ncid
    ! -- local
    !
    ! -- initialize object
    this%modeltype = modeltype
    this%component_type = component_type
    this%modelname = modelname
    this%modelfname = modelfname
    this%ncid = ncid
    !
    ! -- return
    return
  end subroutine ncmodelinputs_init

  !> @brief add to package list
  !<
  subroutine ncmodelinputs_add(this, pkgtype, sc_type, pkgname)
    ! -- dummy variables
    class(NCModelInputsType) :: this
    character(len=*) :: pkgtype
    character(len=*) :: sc_type
    character(len=*) :: pkgname
    class(NCModelPackageInputType), pointer :: ncpkg
    ! -- local variables
    class(*), pointer :: obj
    !
    allocate (ncpkg)
    call ncpkg%init(pkgtype, this%component_type, &
                    sc_type, this%modelname, &
                    pkgname, this%modelfname)
    !
    obj => ncpkg
    call this%pkglist%Add(obj)
    !
    ! -- return
    return
  end subroutine ncmodelinputs_add

  !> @brief get package list
  !<
  function ncmodelinputs_get(this, idx) result(ncpkg)
    ! -- dummy variables
    class(NCModelInputsType) :: this
    integer(I4B), intent(in) :: idx
    ! -- return
    class(NCModelPackageInputType), pointer :: ncpkg
    ! -- local variables
    class(*), pointer :: obj
    !
    ! -- initialize
    ncpkg => null()
    !
    obj => this%pkglist%GetItem(idx)
    if (associated(obj)) then
      select type (obj)
      class is (NCModelPackageInputType)
        ncpkg => obj
      end select
    end if
    !
    ! -- return
    return
  end function ncmodelinputs_get

  !> @brief get package list
  !<
  subroutine ncmodelinputs_destroy(this)
    ! -- dummy variables
    class(NCModelInputsType) :: this
    ! -- local variables
    class(NCModelPackageInputType), pointer :: ncpkg
    integer(I4B) :: n
    !
    ! -- initialize
    ncpkg => null()
    !
    do n = 1, this%pkglist%count()
      ncpkg => this%get(n)
      call ncpkg%destroy()
      deallocate (ncpkg)
      nullify (ncpkg)
    end do
    !
    call this%pkglist%clear()
    !
    ! -- return
    return
  end subroutine ncmodelinputs_destroy

  !> @brief get package list
  !<
  function ncmodelinputs_get_package(this, component_name, subcomponent_name) &
    result(ncpkg)
    ! -- dummy variables
    class(NCModelInputsType) :: this
    character(len=*), intent(in) :: component_name
    character(len=*), intent(in) :: subcomponent_name
    ! -- return
    class(NCModelPackageInputType), pointer :: ncpkg
    ! -- local variables
    class(*), pointer :: obj
    class(NCModelPackageInputType), pointer :: pkg
    integer(I4B) :: n
    !
    ! -- initialize
    ncpkg => null()
    !
    do n = 1, this%pkglist%count()
      obj => this%pkglist%GetItem(n)
      if (associated(obj)) then
        select type (obj)
        class is (NCModelPackageInputType)
          pkg => obj
          if (component_name == pkg%component_name .and. &
              subcomponent_name == pkg%subcomponent_name) then
            ncpkg => obj
            exit
          end if
        end select
      end if
    end do
    !
    ! -- set error if not found
    if (.not. associated(ncpkg)) then
      errmsg = 'NC Model package not found. Model='// &
               trim(component_name)//', package='// &
               trim(subcomponent_name)//'.'
      call store_error(errmsg)
      call store_error_filename(this%modelfname)
    end if
    !
    ! -- return
    return
  end function ncmodelinputs_get_package

  !> @brief get package list
  !<
  function ncmodelinputs_has_package(this, component_name, subcomponent_name) &
    result(has)
    ! -- dummy variables
    class(NCModelInputsType) :: this
    character(len=*), intent(in) :: component_name
    character(len=*), intent(in) :: subcomponent_name
    ! -- return
    logical(LGP) :: has
    ! -- local variables
    class(*), pointer :: obj
    class(NCModelPackageInputType), pointer :: pkg
    integer(I4B) :: n
    !
    ! -- initialize
    has = .false.
    !
    do n = 1, this%pkglist%count()
      obj => this%pkglist%GetItem(n)
      if (associated(obj)) then
        select type (obj)
        class is (NCModelPackageInputType)
          pkg => obj
          if (component_name == pkg%component_name .and. &
              subcomponent_name == pkg%subcomponent_name) then
            has = .true.
            exit
          end if
        end select
      end if
    end do
    !
    ! -- return
    return
  end function ncmodelinputs_has_package

end module NCModelInputsModule
