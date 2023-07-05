!> @brief This module contains the InputLoadTypeModule
!!
!! This module defines types that support generic IDP
!! static and dynamic input loading.
!!
!<
module InputLoadTypeModule

  use KindModule, only: DP, I4B, LGP
  use ConstantsModule, only: LINELENGTH, LENMODELNAME
  use ModflowInputModule, only: ModflowInputType
  use ListModule, only: ListType
  use StressPkgInputModule, only: StressPkgInputType

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
    character(len=LENMODELNAME) :: modelname !< name of model
    character(len=LINELENGTH) :: modelfname !< name of model input file
    character(len=LINELENGTH) :: sourcename !< source name, e.g. name of file
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
    type(StressPkgInputType), pointer :: stresspkg !< stress pkg input context
  contains
    procedure :: init => dynamic_init
    procedure :: destroy => dynamic_destroy
  end type DynamicPkgLoadType

  !> @brief base abstract type for source dynamic load
  !!
  !! IDM sources should extend and implement this type
  !!
  !<
  type, abstract, extends(DynamicPkgLoadType) :: DynamicPkgLoadBaseType
  contains
    procedure(period_load_if), deferred :: period_load
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
    subroutine period_load_if(this, iout)
      import DynamicPkgLoadBaseType, I4B
      class(DynamicPkgLoadBaseType), intent(inout) :: this
      integer(I4B), intent(in) :: iout
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
  contains
    procedure :: init => dynamicpkgs_init
    procedure :: add => dynamicpkgs_add
    procedure :: get => dynamicpkgs_get
    procedure :: period_load => dynamicpkgs_period_load
    procedure :: size => dynamicpkgs_size
    procedure :: destroy => dynamicpkgs_destroy
  end type ModelDynamicPkgsType

contains

  subroutine static_init(this, mf6_input, modelname, modelfname, source)
    class(StaticPkgLoadType), intent(inout) :: this
    type(ModflowInputType), intent(in) :: mf6_input
    character(len=*), intent(in) :: modelname
    character(len=*), intent(in) :: modelfname
    character(len=*), intent(in) :: source
    !
    this%mf6_input = mf6_input
    this%modelname = modelname
    this%modelfname = modelfname
    this%sourcename = source
    !
    return
  end subroutine static_init

  subroutine static_destroy(this)
    class(StaticPkgLoadType), intent(inout) :: this
    !
    return
  end subroutine static_destroy

  subroutine dynamic_init(this, mf6_input, modelname, modelfname, source, iout)
    class(DynamicPkgLoadType), intent(inout) :: this
    type(ModflowInputType), intent(in) :: mf6_input
    character(len=*), intent(in) :: modelname
    character(len=*), intent(in) :: modelfname
    character(len=*), intent(in) :: source
    integer(I4B), intent(in) :: iout
    !
    this%mf6_input = mf6_input
    this%modelname = modelname
    this%modelfname = modelfname
    this%sourcename = source
    !
    allocate (this%stresspkg)
    call this%stresspkg%init(this%mf6_input, iout)
    !
    return
  end subroutine dynamic_init

  subroutine dynamic_destroy(this)
    class(DynamicPkgLoadType), intent(inout) :: this
    !
    call this%stresspkg%destroy()
    deallocate (this%stresspkg)
    !
    return
  end subroutine dynamic_destroy

  subroutine dynamicpkgs_init(this, modelname, modelfname)
    class(ModelDynamicPkgsType), intent(inout) :: this
    character(len=*), intent(in) :: modelname
    character(len=*), intent(in) :: modelfname
    !
    this%modelname = modelname
    this%modelfname = modelfname
    !
    return
  end subroutine dynamicpkgs_init

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

  subroutine dynamicpkgs_period_load(this, iout)
    class(ModelDynamicPkgsType), intent(inout) :: this
    integer(I4B), intent(in) :: iout
    class(DynamicPkgLoadBaseType), pointer :: dynamic_pkg
    integer(I4B) :: n
    !
    do n = 1, this%pkglist%Count()
      dynamic_pkg => this%get(n)
      call dynamic_pkg%period_load(iout)
    end do
    !
    return
  end subroutine dynamicpkgs_period_load

  function dynamicpkgs_size(this) result(size)
    class(ModelDynamicPkgsType), intent(inout) :: this
    integer(I4B) :: size
    !
    size = this%pkglist%Count()
    !
    return
  end function dynamicpkgs_size

  subroutine dynamicpkgs_destroy(this, iout)
    class(ModelDynamicPkgsType), intent(inout) :: this
    integer(I4B), intent(in) :: iout
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
    ! -- get the package from the list
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
