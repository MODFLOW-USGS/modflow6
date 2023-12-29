!> @brief This module contains the InputLoadTypeModule
!!
!! This module defines types that support generic IDP
!! static and dynamic input loading.
!!
!<
module InputLoadTypeModule

  use KindModule, only: DP, I4B, LGP
  use ConstantsModule, only: LINELENGTH, LENCOMPONENTNAME, LENMODELNAME
  use ModflowInputModule, only: ModflowInputType
  use ListModule, only: ListType
  use InputDefinitionModule, only: InputParamDefinitionType

  implicit none
  private
  public :: StaticPkgLoadBaseType
  public :: DynamicPkgLoadBaseType
  public :: ModelDynamicPkgsType
  public :: AddDynamicModelToList, GetDynamicModelFromList
  public :: StaticPkgLoadType, DynamicPkgLoadType

  !> @brief derived type for source static load
  !!
  !! This derived type is a base concrete type for a model
  !! package static load
  !!
  !<
  type StaticPkgLoadType
    type(ModflowInputType) :: mf6_input !< description of modflow6 input
    character(len=LENCOMPONENTNAME) :: component_name !< name of component
    character(len=LINELENGTH) :: component_input_name !< name of component input name, e.g. filename
    character(len=LINELENGTH) :: input_name !< source name, e.g. name of input file
    integer(I4B) :: iperblock
  contains
    procedure :: init => static_init
    procedure :: destroy => static_destroy
  end type StaticPkgLoadType

  !> @brief base abstract type for source static load
  !!
  !! IDM sources should extend and implement this type
  !!
  !<
  type, abstract, extends(StaticPkgLoadType) :: StaticPkgLoadBaseType
  contains
    procedure(load_if), deferred :: load
  end type StaticPkgLoadBaseType

  !> @brief derived type for source dynamic load
  !!
  !! This derived type is a base concrete type for a model
  !! package dynamic (period) load
  !!
  !<
  type :: DynamicPkgLoadType
    type(ModflowInputType) :: mf6_input !< description of modflow6 input
    character(len=LENMODELNAME) :: modelname !< name of model
    character(len=LINELENGTH) :: modelfname !< name of model input file
    character(len=LINELENGTH) :: sourcename !< source name, e.g. name of file
    logical(LGP) :: readasarrays
    integer(I4B) :: iperblock
    integer(I4B) :: iout
  contains
    procedure :: init => dynamic_init
    procedure :: df => dynamic_df
    procedure :: ad => dynamic_ad
    procedure :: destroy => dynamic_destroy
  end type DynamicPkgLoadType

  !> @brief base abstract type for source dynamic load
  !!
  !! IDM sources should extend and implement this type
  !!
  !<
  type, abstract, extends(DynamicPkgLoadType) :: DynamicPkgLoadBaseType
  contains
    procedure(period_load_if), deferred :: rp
  end type DynamicPkgLoadBaseType

  !> @brief load interfaces for source static and dynamic types
  !<
  abstract interface
    function load_if(this, iout) result(dynamic_loader)
      import StaticPkgLoadBaseType, DynamicPkgLoadBaseType, I4B
      class(StaticPkgLoadBaseType), intent(inout) :: this
      integer(I4B), intent(in) :: iout
      class(DynamicPkgLoadBaseType), pointer :: dynamic_loader
    end function load_if
    subroutine period_load_if(this)
      import DynamicPkgLoadBaseType, I4B
      class(DynamicPkgLoadBaseType), intent(inout) :: this
    end subroutine
  end interface

  !> @brief derived type for storing a dynamic package load list
  !!
  !! This derived type is used to store a list of package
  !! dynamic load types for a model
  !!
  !<
  type :: ModelDynamicPkgsType
    character(len=LENMODELNAME) :: modelname !< name of model
    character(len=LINELENGTH) :: modelfname !< name of model input file
    type(ListType) :: pkglist !< list of pointers to model dynamic package loaders
    integer(I4B) :: iout
  contains
    procedure :: init => dynamicpkgs_init
    procedure :: add => dynamicpkgs_add
    procedure :: get => dynamicpkgs_get
    procedure :: rp => dynamicpkgs_rp
    procedure :: df => dynamicpkgs_df
    procedure :: ad => dynamicpkgs_ad
    procedure :: size => dynamicpkgs_size
    procedure :: destroy => dynamicpkgs_destroy
  end type ModelDynamicPkgsType

contains

  !> @brief initialize static package loader
  !!
  !<
  subroutine static_init(this, mf6_input, component_name, component_input_name, &
                         input_name)
    class(StaticPkgLoadType), intent(inout) :: this
    type(ModflowInputType), intent(in) :: mf6_input
    character(len=*), intent(in) :: component_name
    character(len=*), intent(in) :: component_input_name
    character(len=*), intent(in) :: input_name
    integer(I4B) :: iblock
    !
    this%mf6_input = mf6_input
    this%component_name = component_name
    this%component_input_name = component_input_name
    this%input_name = input_name
    this%iperblock = 0
    !
    ! -- identify period block definition
    do iblock = 1, size(mf6_input%block_dfns)
      !
      if (mf6_input%block_dfns(iblock)%blockname == 'PERIOD') then
        this%iperblock = iblock
        exit
      end if
    end do
    !
    return
  end subroutine static_init

  subroutine static_destroy(this)
    class(StaticPkgLoadType), intent(inout) :: this
    !
    return
  end subroutine static_destroy

  !> @brief initialize dynamic package loader
  !!
  !! Any managed memory pointed to from model/package context
  !! must be allocated when derived dynamic loader is initialized.
  !!
  !<
  subroutine dynamic_init(this, mf6_input, modelname, modelfname, source, &
                          iperblock, iout)
    use SimVariablesModule, only: errmsg
    use SimModule, only: store_error, store_error_filename
    class(DynamicPkgLoadType), intent(inout) :: this
    type(ModflowInputType), intent(in) :: mf6_input
    character(len=*), intent(in) :: modelname
    character(len=*), intent(in) :: modelfname
    character(len=*), intent(in) :: source
    integer(I4B), intent(in) :: iperblock
    integer(I4B), intent(in) :: iout
    !
    this%mf6_input = mf6_input
    this%modelname = modelname
    this%modelfname = modelfname
    this%sourcename = source
    this%iperblock = iperblock
    this%iout = iout
    !
    ! -- throw error and exit if not found
    if (this%iperblock == 0) then
      write (errmsg, '(a,a)') &
        'Programming error. (IDM) PERIOD block not found in '&
        &'dynamic package input block dfns: ', &
        trim(mf6_input%subcomponent_name)
      call store_error(errmsg)
      call store_error_filename(this%sourcename)
    else
      !
      this%readasarrays = (.not. mf6_input%block_dfns(iperblock)%aggregate)
    end if
    !
    ! -- return
    return
  end subroutine dynamic_init

  !> @brief dynamic package loader define
  !!
  !<
  subroutine dynamic_df(this)
    class(DynamicPkgLoadType), intent(inout) :: this
    !
    ! override in derived type
    !
    return
  end subroutine dynamic_df

  !> @brief dynamic package loader advance
  !!
  !<
  subroutine dynamic_ad(this)
    class(DynamicPkgLoadType), intent(inout) :: this
    !
    ! override in derived type
    !
    return
  end subroutine dynamic_ad

  !> @brief dynamic package loader destroy
  !!
  !<
  subroutine dynamic_destroy(this)
    use MemoryManagerExtModule, only: memorylist_remove
    use SimVariablesModule, only: idm_context
    class(DynamicPkgLoadType), intent(inout) :: this
    !
    ! -- deallocate package static and dynamic input context
    call memorylist_remove(this%mf6_input%component_name, &
                           this%mf6_input%subcomponent_name, &
                           idm_context)
    !
    return
  end subroutine dynamic_destroy

  !> @brief model dynamic packages init
  !!
  !<
  subroutine dynamicpkgs_init(this, modelname, modelfname, iout)
    class(ModelDynamicPkgsType), intent(inout) :: this
    character(len=*), intent(in) :: modelname
    character(len=*), intent(in) :: modelfname
    integer(I4B), intent(in) :: iout
    !
    this%modelname = modelname
    this%modelfname = modelfname
    this%iout = iout
    !
    return
  end subroutine dynamicpkgs_init

  !> @brief add package to model dynamic packages list
  !!
  !<
  subroutine dynamicpkgs_add(this, dynamic_pkg)
    class(ModelDynamicPkgsType), intent(inout) :: this
    class(DynamicPkgLoadBaseType), pointer, intent(inout) :: dynamic_pkg
    class(*), pointer :: obj
    !
    obj => dynamic_pkg
    call this%pkglist%add(obj)
    !
    return
  end subroutine dynamicpkgs_add

  !> @brief retrieve package from model dynamic packages list
  !!
  !<
  function dynamicpkgs_get(this, idx) result(res)
    class(ModelDynamicPkgsType), intent(inout) :: this
    integer(I4B), intent(in) :: idx
    class(DynamicPkgLoadBaseType), pointer :: res
    class(*), pointer :: obj
    !
    nullify (res)
    obj => this%pkglist%GetItem(idx)
    !
    if (associated(obj)) then
      select type (obj)
      class is (DynamicPkgLoadBaseType)
        res => obj
      end select
    end if
    !
    return
  end function dynamicpkgs_get

  !> @brief read and prepare model dynamic packages
  !!
  !<
  subroutine dynamicpkgs_rp(this)
    use IdmLoggerModule, only: idm_log_period_header, idm_log_period_close
    class(ModelDynamicPkgsType), intent(inout) :: this
    class(DynamicPkgLoadBaseType), pointer :: dynamic_pkg
    integer(I4B) :: n
    !
    call idm_log_period_header(this%modelname, this%iout)
    !
    do n = 1, this%pkglist%Count()
      dynamic_pkg => this%get(n)
      call dynamic_pkg%rp()
    end do
    !
    call idm_log_period_close(this%iout)
    !
    return
  end subroutine dynamicpkgs_rp

  !> @brief define model dynamic packages
  !!
  !<
  subroutine dynamicpkgs_df(this)
    class(ModelDynamicPkgsType), intent(inout) :: this
    class(DynamicPkgLoadBaseType), pointer :: dynamic_pkg
    integer(I4B) :: n
    !
    do n = 1, this%pkglist%Count()
      dynamic_pkg => this%get(n)
      call dynamic_pkg%df()
    end do
    !
    return
  end subroutine dynamicpkgs_df

  !> @brief advance model dynamic packages
  !!
  !<
  subroutine dynamicpkgs_ad(this)
    class(ModelDynamicPkgsType), intent(inout) :: this
    class(DynamicPkgLoadBaseType), pointer :: dynamic_pkg
    integer(I4B) :: n
    !
    do n = 1, this%pkglist%Count()
      dynamic_pkg => this%get(n)
      call dynamic_pkg%ad()
    end do
    !
    return
  end subroutine dynamicpkgs_ad

  !> @brief get size of model dynamic packages list
  !!
  !<
  function dynamicpkgs_size(this) result(size)
    class(ModelDynamicPkgsType), intent(inout) :: this
    integer(I4B) :: size
    !
    size = this%pkglist%Count()
    !
    return
  end function dynamicpkgs_size

  !> @brief destroy model dynamic packages object
  !!
  !<
  subroutine dynamicpkgs_destroy(this)
    class(ModelDynamicPkgsType), intent(inout) :: this
    class(DynamicPkgLoadBaseType), pointer :: dynamic_pkg
    integer(I4B) :: n
    !
    do n = 1, this%pkglist%Count()
      dynamic_pkg => this%get(n)
      call dynamic_pkg%destroy()
      deallocate (dynamic_pkg)
      nullify (dynamic_pkg)
    end do
    !
    call this%pkglist%Clear()
    !
    return
  end subroutine dynamicpkgs_destroy

  !> @brief add model dynamic packages object to list
  !!
  !<
  subroutine AddDynamicModelToList(list, model_dynamic)
    ! -- dummy variables
    type(ListType), intent(inout) :: list !< package list
    class(ModelDynamicPkgsType), pointer, intent(inout) :: model_dynamic
    ! -- local variables
    class(*), pointer :: obj
    !
    obj => model_dynamic
    call list%Add(obj)
    !
    ! -- return
    return
  end subroutine AddDynamicModelToList

  !> @brief get model dynamic packages object from list
  !!
  !<
  function GetDynamicModelFromList(list, idx) result(res)
    ! -- dummy variables
    type(ListType), intent(inout) :: list !< spd list
    integer(I4B), intent(in) :: idx !< package number
    class(ModelDynamicPkgsType), pointer :: res
    ! -- local variables
    class(*), pointer :: obj
    !
    ! -- initialize res
    res => null()
    !
    ! -- get the object from the list
    obj => list%GetItem(idx)
    if (associated(obj)) then
      select type (obj)
      class is (ModelDynamicPkgsType)
        res => obj
      end select
    end if
    !
    ! -- return
    return
  end function GetDynamicModelFromList

end module InputLoadTypeModule
