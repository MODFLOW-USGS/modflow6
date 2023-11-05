!> @brief This module contains the NC4ModelInputsModule
!!
!!
!<
module NC4ModelInputsModule

  use KindModule, only: DP, I4B, LGP
  use ConstantsModule, only: LINELENGTH, LENCOMPONENTNAME, &
                             LENVARNAME, LENPACKAGETYPE, &
                             LENPACKAGENAME
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
  public :: NC4ModelPackageInputType
  public :: NC4ModelInputsType
  public :: NC4BlockVariablesType

  type :: NC4BlockVariablesType
    ! -- block
    character(len=LENCOMPONENTNAME) :: blockname
    integer(I4B) :: idfn
    logical(LGP) :: aggregate
    ! -- input tagname; variable tags can exceed LENVARNAME
    character(len=LINELENGTH), dimension(:), allocatable :: varnames
    ! -- model nc4 dataset variable id
    integer(I4B), dimension(:), allocatable :: varids
    ! -- number of variables
    integer(I4B) :: varnum
  contains
    procedure :: create => blockvars_create
    procedure :: add => blockvars_add
    procedure :: destroy => blockvars_destroy
  end type NC4BlockVariablesType

  type :: NC4ModelPackageInputType
    type(NC4BlockVariablesType), dimension(:), allocatable :: blocklist
    integer(I4B), dimension(:), allocatable :: ipers
    character(len=LENCOMPONENTNAME) :: component_type
    character(len=LENCOMPONENTNAME) :: subcomponent_type
    character(len=LENCOMPONENTNAME) :: component_name
    character(len=LENCOMPONENTNAME) :: subcomponent_name
  contains
    procedure :: init => nc4pkg_init
    procedure :: reset => nc4pkg_reset
    procedure :: destroy => nc4pkg_destroy
    procedure :: add => nc4pkg_add
  end type NC4ModelPackageInputType

  type :: NC4ModelInputsType
    character(len=LENPACKAGETYPE) :: modeltype
    character(len=LENCOMPONENTNAME) :: component_type
    character(len=LENCOMPONENTNAME) :: modelname
    character(len=LINELENGTH) :: modelfname
    integer(I4B) :: ncid
    type(ListType) :: pkglist
  contains
    procedure :: init => nc4modelinputs_init
    procedure :: add => nc4modelinputs_add
    procedure :: get => nc4modelinputs_get
    procedure :: get_package => nc4modelinputs_get_package
    procedure :: destroy => nc4modelinputs_destroy
  end type NC4ModelInputsType

contains

  !> @brief create a new package type
  !<
  subroutine blockvars_create(this, blockname, idfn, aggregate)
    ! -- modules
    ! -- dummy
    class(NC4BlockVariablesType) :: this
    character(len=*), intent(in) :: blockname
    integer(I4B), intent(in) :: idfn
    logical(LGP), intent(in) :: aggregate
    ! -- local
    !
    ! -- initialize
    this%blockname = blockname
    this%idfn = idfn
    this%aggregate = aggregate
    this%varnum = 0
    !
    ! -- allocate arrays
    allocate (this%varnames(0))
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
    class(NC4BlockVariablesType) :: this
    character(len=*), intent(in) :: varname
    integer(I4B), intent(in) :: varid
    ! -- local
    !
    ! -- reallocate
    call expandarray(this%varnames)
    call expandarray(this%varids)
    !
    ! -- add new package instance
    this%varnum = this%varnum + 1
    this%varnames(this%varnum) = varname
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
    class(NC4BlockVariablesType) :: this
    ! -- local
    !
    ! -- deallocate dynamic arrays
    if (allocated(this%varnames)) deallocate (this%varnames)
    if (allocated(this%varids)) deallocate (this%varids)
    !
    ! -- return
    return
  end subroutine blockvars_destroy

  !> @brief initialize model package inputs object
  !<
  subroutine nc4pkg_init(this, pkgtype, component_type, subcomponent_type, &
                         component_name, subcomponent_name, filename)
    ! -- modules
    ! -- dummy
    class(NC4ModelPackageInputType) :: this
    character(len=*) :: pkgtype
    character(len=*) :: component_type
    character(len=*) :: subcomponent_type
    character(len=*) :: component_name
    character(len=*) :: subcomponent_name
    character(len=*) :: filename
    ! -- local
    character(len=100) :: blockname
    type(InputBlockDefinitionType), dimension(:), &
      pointer :: block_dfns
    integer(I4B) :: iblock
    !
    ! -- initialize object
    this%component_type = component_type
    this%subcomponent_type = subcomponent_type
    this%component_name = component_name
    this%subcomponent_name = subcomponent_name
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
      call this%blocklist(iblock)%create(blockname, iblock, &
                                         block_dfns(iblock)%aggregate)
    end do
    !
    ! -- return
    return
  end subroutine nc4pkg_init

  !> @brief reset model nc4 package blocklist
  !!
  !! This routine resets the blocklist.  Needed for array based inputs
  !! because the model namefile packages block will specify RCH6, for
  !! example, in reference to either the array or list based definition
  !! set. This routine is called when the definition set should be array
  !! but was initialized as list. It should only be called once per
  !! package when the first input variable is read.
  !!
  !<
  subroutine nc4pkg_reset(this, pkgtype, subcomponent_type)
    ! -- modules
    ! -- dummy
    class(NC4ModelPackageInputType) :: this
    character(len=*) :: pkgtype
    character(len=*) :: subcomponent_type
    ! -- local
    character(len=100) :: blockname
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
      call this%blocklist(iblock)%create(blockname, iblock, &
                                         block_dfns(iblock)%aggregate)
    end do
    !
    ! -- return
    return
  end subroutine nc4pkg_reset

  !> @brief add package variable info to package type list
  !<
  subroutine nc4pkg_add(this, blockname, varname, varid)
    ! -- modules
    use InputOutputModule, only: lowcase, upcase
    ! -- dummy
    class(NC4ModelPackageInputType) :: this
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
    ! -- locate index of pkgtype in pkglist
    do n = 1, size(this%blocklist)
      if (this%blocklist(n)%blockname == blockname) then
        call this%blocklist(n)%add(varname, varid)
        exit
      end if
    end do
    !
    ! -- return
    return
  end subroutine nc4pkg_add

  !> @brief deallocate object
  !<
  subroutine nc4pkg_destroy(this)
    ! -- modules
    ! -- dummy
    class(NC4ModelPackageInputType) :: this
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
  end subroutine nc4pkg_destroy

  !> @brief init object
  !<
  subroutine nc4modelinputs_init(this, modeltype, component_type, &
                                 modelname, modelfname, ncid)
    ! -- modules
    ! -- dummy
    class(NC4ModelInputsType) :: this
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
  end subroutine nc4modelinputs_init

  !> @brief add to package list
  !<
  subroutine nc4modelinputs_add(this, pkgtype, pkgname, sc_type)
    ! -- dummy variables
    class(NC4ModelInputsType) :: this
    character(len=*) :: pkgtype
    character(len=*) :: pkgname
    character(len=*) :: sc_type
    class(NC4ModelPackageInputType), pointer :: nc4pkg
    ! -- local variables
    class(*), pointer :: obj
    !
    ! --
    allocate (nc4pkg)
    call nc4pkg%init(pkgtype, this%component_type, &
                     sc_type, this%modelname, &
                     pkgname, this%modelfname)
    !
    obj => nc4pkg
    call this%pkglist%Add(obj)
    !
    ! -- return
    return
  end subroutine nc4modelinputs_add

  !> @brief get package list
  !<
  function nc4modelinputs_get(this, idx) result(nc4pkg)
    ! -- dummy variables
    class(NC4ModelInputsType) :: this
    integer(I4B), intent(in) :: idx
    ! -- return
    class(NC4ModelPackageInputType), pointer :: nc4pkg
    ! -- local variables
    class(*), pointer :: obj
    !
    ! -- initialize
    nc4pkg => null()
    !
    obj => this%pkglist%GetItem(idx)
    if (associated(obj)) then
      select type (obj)
      class is (NC4ModelPackageInputType)
        nc4pkg => obj
      end select
    end if
    !
    ! -- return
    return
  end function nc4modelinputs_get

  !> @brief get package list
  !<
  subroutine nc4modelinputs_destroy(this)
    ! -- dummy variables
    class(NC4ModelInputsType) :: this
    ! -- local variables
    class(NC4ModelPackageInputType), pointer :: nc4pkg
    integer(I4B) :: n
    !
    ! -- initialize
    nc4pkg => null()
    !
    do n = 1, this%pkglist%count()
      nc4pkg => this%get(n)
      call nc4pkg%destroy()
      deallocate (nc4pkg)
      nullify (nc4pkg)
    end do
    !
    call this%pkglist%clear()
    !
    ! -- return
    return
  end subroutine nc4modelinputs_destroy

  !> @brief get package list
  !<
  function nc4modelinputs_get_package(this, component_name, subcomponent_name) &
    result(nc4_pkg)
    ! -- dummy variables
    class(NC4ModelInputsType) :: this
    character(len=*), intent(in) :: component_name
    character(len=*), intent(in) :: subcomponent_name
    ! -- return
    class(NC4ModelPackageInputType), pointer :: nc4_pkg
    ! -- local variables
    class(*), pointer :: obj
    class(NC4ModelPackageInputType), pointer :: pkg
    integer(I4B) :: n
    !
    ! -- initialize
    nc4_pkg => null()
    !
    do n = 1, this%pkglist%count()
      obj => this%pkglist%GetItem(n)
      if (associated(obj)) then
        select type (obj)
        class is (NC4ModelPackageInputType)
          pkg => obj
          if (component_name == pkg%component_name .and. &
              subcomponent_name == pkg%subcomponent_name) then
            nc4_pkg => obj
            exit
          end if
        end select
      end if
    end do
    !
    ! -- set error if not found
    if (.not. associated(nc4_pkg)) then
      errmsg = 'Programming error. NC4 Model package not found. &
               &Model='//trim(component_name)//', package='// &
               trim(subcomponent_name)//'.'
      call store_error(errmsg)
      call store_error_filename(this%modelfname)
    end if
    !
    ! -- return
    return
  end function nc4modelinputs_get_package

end module NC4ModelInputsModule
