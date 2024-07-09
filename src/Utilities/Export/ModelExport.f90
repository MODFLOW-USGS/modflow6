!> @brief This module contains the ModelExportModule
!!
!! This modeule defines the local list of model exports.
!! It is not dependent on netcdf or other external
!! libraries.
!!
!<
module ModelExportModule

  use KindModule, only: DP, I4B, LGP
  use ConstantsModule, only: LINELENGTH, LENMODELNAME, LENCOMPONENTNAME, &
                             LENMEMPATH
  use ListModule, only: ListType
  use NCModelExportModule, only: NCBaseModelExportType

  implicit none
  private
  public :: modelexports_create
  public :: modelexports_post_step
  public :: modelexports_destroy
  public :: nc_export_active
  public :: export_models
  public :: get_export_model
  public :: ExportModelType

  type(ListType) :: export_models

  !> @brief export model type
  !!
  !!  This is a container variable which holds
  !!  model export objects.
  !!
  !<
  type :: ExportModelType
    character(len=LENMODELNAME) :: modelname !< name of model
    character(len=LENCOMPONENTNAME) :: modeltype !< type of model
    character(len=LINELENGTH) :: modelfname !< name of model input file
    class(NCBaseModelExportType), pointer :: nc_export => null() !< netcdf export object pointer
    integer(I4B) :: nctype !< type of netcdf export
    integer(I4B) :: disenum !< type of discretization
    integer(I4B) :: iout !< lst file descriptor
  contains
    procedure :: init
    procedure :: post_step
    procedure :: destroy
  end type ExportModelType

contains

  !> @brief is netcdf export configured for any model
  !!
  function nc_export_active() result(active)
    use NCModelExportModule, only: NETCDF_UNDEF
    logical(LGP) :: active
    integer(I4B) :: n
    type(ExportModelType), pointer :: export_model
    active = .false.
    !
    do n = 1, export_models%Count()
      export_model => get_export_model(n)
      if (export_model%nctype /= NETCDF_UNDEF) then
        active = .true.
        exit
      end if
    end do
  end function nc_export_active

  !> @brief create export container variable for all local models
  !!
  subroutine modelexports_create(iout)
    use InputLoadTypeModule, only: ModelDynamicPkgsType
    use InputLoadTypeModule, only: model_dynamic_pkgs
    use MemoryManagerModule, only: mem_setptr
    use MemoryManagerExtModule, only: mem_set_value
    use MemoryHelperModule, only: create_mem_path
    use InputLoadTypeModule, only: GetDynamicModelFromList
    use SimVariablesModule, only: idm_context
    use NCModelExportModule, only: NETCDF_UGRID, NETCDF_STRUCTURED
    integer(I4B), intent(in) :: iout
    type(ModelDynamicPkgsType), pointer :: model_dynamic_input
    type(ExportModelType), pointer :: export_model
    character(len=LENMEMPATH) :: modelnam_mempath, model_mempath
    integer(I4B), pointer :: disenum
    character(len=LINELENGTH) :: exportstr
    integer(I4B) :: n
    logical(LGP) :: found
    !
    do n = 1, model_dynamic_pkgs%Count()
      !
      ! -- allocate and initialize
      allocate (export_model)
      !
      ! -- set pointer to dynamic input model instance
      model_dynamic_input => GetDynamicModelFromList(model_dynamic_pkgs, n)
      !
      ! --set input mempaths
      modelnam_mempath = &
        create_mem_path(component=model_dynamic_input%modelname, &
                        subcomponent='NAM', context=idm_context)
      model_mempath = create_mem_path(component=model_dynamic_input%modelname, &
                                      context=idm_context)
      ! -- set pointer to dis enum type
      call mem_setptr(disenum, 'DISENUM', model_mempath)
      !
      ! --  initialize model
      call export_model%init(model_dynamic_input%modelname, &
                             model_dynamic_input%modeltype, &
                             model_dynamic_input%modelfname, disenum, iout)
      !
      ! -- update EXPORT_NETCDF string if provided
      call mem_set_value(exportstr, 'EXPORT_NETCDF', modelnam_mempath, found)
      if (found) then
        if (exportstr == 'STRUCTURED') then
          export_model%nctype = NETCDF_STRUCTURED
        else
          export_model%nctype = NETCDF_UGRID
        end if
      end if
      !
      ! -- add model to list
      call add_export_model(export_model)
    end do
  end subroutine modelexports_create

  !> @brief export model list post step
  !!
  subroutine modelexports_post_step()
    ! -- local variables
    class(*), pointer :: obj
    class(ExportModelType), pointer :: export_model
    integer(I4B) :: n
    !
    do n = 1, export_models%Count()
      obj => export_models%GetItem(n)
      if (associated(obj)) then
        select type (obj)
        class is (ExportModelType)
          export_model => obj
          call export_model%post_step()
        end select
      end if
    end do
  end subroutine modelexports_post_step

  !> @brief destroy export model list
  !!
  subroutine modelexports_destroy()
    ! -- local variables
    class(*), pointer :: obj
    class(ExportModelType), pointer :: export_model
    integer(I4B) :: n
    !
    do n = 1, export_models%Count()
      obj => export_models%GetItem(n)
      if (associated(obj)) then
        select type (obj)
        class is (ExportModelType)
          export_model => obj
          call export_model%destroy()
          deallocate (export_model)
          nullify (export_model)
        end select
      end if
    end do
    !
    call export_models%clear()
  end subroutine modelexports_destroy

  !> @brief initialize model export container variable
  !!
  !<
  subroutine init(this, modelname, modeltype, modelfname, disenum, iout)
    use NCModelExportModule, only: NETCDF_UNDEF
    class(ExportModelType), intent(inout) :: this
    character(len=*), intent(in) :: modelname
    character(len=*), intent(in) :: modeltype
    character(len=*), intent(in) :: modelfname
    integer(I4B), intent(in) :: disenum
    integer(I4B), intent(in) :: iout
    !
    this%modelname = modelname
    this%modeltype = modeltype
    this%modelfname = modelfname
    this%nctype = NETCDF_UNDEF
    this%disenum = disenum
    this%iout = iout
    !
    nullify (this%nc_export)
  end subroutine init

  !> @brief model export container post step actions
  !!
  !<
  subroutine post_step(this)
    class(ExportModelType), intent(inout) :: this
    !
    if (associated(this%nc_export)) then
      call this%nc_export%step()
    end if
  end subroutine post_step

  !> @brief destroy model export container
  !!
  !<
  subroutine destroy(this)
    class(ExportModelType), intent(inout) :: this
    !
    if (associated(this%nc_export)) then
      call this%nc_export%destroy()
      deallocate (this%nc_export)
      nullify (this%nc_export)
    end if
  end subroutine destroy

  !> @brief add model export object to list
  !!
  !<
  subroutine add_export_model(export_model)
    ! -- dummy variables
    type(ExportModelType), pointer, intent(inout) :: export_model
    ! -- local variables
    class(*), pointer :: obj
    !
    obj => export_model
    call export_models%Add(obj)
  end subroutine add_export_model

  !> @brief get model export object by index
  !!
  !<
  function get_export_model(idx) result(res)
    ! -- dummy variables
    integer(I4B), intent(in) :: idx !< package number
    ! -- local variables
    class(ExportModelType), pointer :: res
    class(*), pointer :: obj
    !
    ! -- initialize res
    nullify (res)
    !
    ! -- get the object from the list
    obj => export_models%GetItem(idx)
    if (associated(obj)) then
      select type (obj)
      class is (ExportModelType)
        res => obj
      end select
    end if
  end function get_export_model

end module ModelExportModule
