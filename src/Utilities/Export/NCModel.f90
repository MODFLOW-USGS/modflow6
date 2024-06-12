!> @brief This module contains the NCModelExportModule
!!
!! This module defines a model export and base type for
!! supported netcdf files. It is not not dependent on
!! netcdf libraries.
!!
!<
module NCModelExportModule

  use KindModule, only: DP, I4B, LGP
  use ConstantsModule, only: LINELENGTH, LENCOMPONENTNAME, LENMODELNAME, &
                             LENMEMPATH, LENBIGLINE, DIS, DISU, DISV
  use SimVariablesModule, only: errmsg
  use SimModule, only: store_error, store_error_filename

  implicit none
  private
  public :: NCBaseModelExportType
  public :: NCExportAnnotation
  public :: NETCDF_UNDEF, NETCDF_STRUCTURED, NETCDF_UGRID

  !> @brief netcdf export types enumerator
  !<
  ENUM, BIND(C)
    ENUMERATOR :: NETCDF_UNDEF = 0 !< undefined netcdf export type
    ENUMERATOR :: NETCDF_STRUCTURED = 1 !< netcdf structrured export
    ENUMERATOR :: NETCDF_UGRID = 2 !< netcdf ugrid export
  END ENUM

  !> @brief netcdf export attribute annotations
  !<
  type :: NCExportAnnotation
    character(len=LINELENGTH) :: title !< file scoped title attribute
    character(len=LINELENGTH) :: model !< file scoped model attribute
    character(len=LINELENGTH) :: history !< file scoped history attribute
    character(len=LINELENGTH) :: source !< file scoped source attribute
    character(len=LINELENGTH) :: conventions !< file scoped conventions attribute
    character(len=LINELENGTH) :: stdname !< dependent variable standard name
    character(len=LINELENGTH) :: longname !< dependent variable long name
  contains
    procedure :: set
  end type NCExportAnnotation

  !> @brief base class for an export model
  !<
  type :: NCModelExportType
    character(len=LENMODELNAME) :: modelname !< name of model
    character(len=LENCOMPONENTNAME) :: modeltype
    character(len=LINELENGTH) :: modelfname !< name of model input file
    character(len=LINELENGTH) :: nc_filename
    character(len=LINELENGTH) :: gridmap_name = 'projection'
    character(len=LINELENGTH) :: mesh_name = 'mesh'
    character(len=LENMEMPATH) :: dis_mempath
    character(len=LENMEMPATH) :: ncf_mempath
    character(len=LENBIGLINE) :: ogc_wkt
    character(len=LINELENGTH) :: datetime
    character(len=LINELENGTH) :: xname
    type(NCExportAnnotation) :: annotation
    real(DP), dimension(:), pointer, contiguous :: x
    integer(I4B) :: disenum
    integer(I4B) :: ncid
    integer(I4B) :: stepcnt
    integer(I4B) :: totnstp
    integer(I4B) :: iout
  contains
    procedure :: init => export_init
    procedure :: destroy => export_destroy
  end type NCModelExportType

  !> @brief abstract type for model netcdf export type
  !<
  type, abstract, extends(NCModelExportType) :: NCBaseModelExportType
  contains
    procedure(model_define_if), deferred :: df
    procedure(model_step_if), deferred :: step
  end type NCBaseModelExportType

  !> @brief abstract interfaces for model netcdf export type
  !<
  abstract interface
    subroutine model_define_if(this)
      import NCBaseModelExportType
      class(NCBaseModelExportType), intent(inout) :: this
    end subroutine
    subroutine model_step_if(this)
      import NCBaseModelExportType
      class(NCBaseModelExportType), intent(inout) :: this
    end subroutine
  end interface

contains

  !> @brief initialization of netcdf file scoped attributes
  !<
  subroutine set(this, modelname, modeltype, modelfname, nctype)
    use VersionModule, only: VERSION
    class(NCExportAnnotation), intent(inout) :: this
    character(len=*), intent(in) :: modelname
    character(len=*), intent(in) :: modeltype
    character(len=*), intent(in) :: modelfname
    integer(I4B), intent(in) :: nctype
    character(len=LINELENGTH) :: fullname
    integer :: values(8)
    !
    this%title = ''
    this%model = ''
    this%history = ''
    this%source = ''
    this%conventions = ''
    this%stdname = ''
    this%longname = ''
    !
    ! -- set file conventions
    this%conventions = 'CF-1.11'
    if (nctype == NETCDF_UGRID) this%conventions = &
      trim(this%conventions)//' UGRID-1.0'
    !
    ! -- set model specific attributes
    select case (modeltype)
    case ('GWF')
      fullname = 'Groundwater Flow'
      this%title = trim(modelname)//' hydraulic head'
      ! -- https://csdms.colorado.edu/wiki/CSN_Searchable_List-Names
      !this%stdname = 'soil_water__pressure_head'
      this%longname = 'head'
    case ('GWT')
      fullname = 'Groundwater Transport'
      this%title = trim(modelname)//' concentration'
      ! -- https://csdms.colorado.edu/wiki/CSN_Searchable_List-Names
      ! -- https://en.wikipedia.org/wiki/Diffusivity
      !this%stdname = 'soil_water__diffusivity'
      this%longname = 'concentration'
    case ('GWE')
      fullname = 'Groundwater Energy'
      this%title = trim(modelname)//' temperature'
      ! -- https://csdms.colorado.edu/wiki/CSN_Searchable_List-Names
      ! -- https://en.wikipedia.org/wiki/Diffusivity
      !this%stdname = 'soil_water__diffusivity'
      this%longname = 'temperature'
    case default
      errmsg = trim(modeltype)//' models not supported for NetCDF export.'
      call store_error(errmsg)
      call store_error_filename(modelfname)
    end select
    !
    ! -- model attribute string
    this%model = trim(modelname)//': MODFLOW 6 '//trim(fullname)// &
                 ' ('//trim(modeltype)//') model'
    !
    ! -- modflow6 version string
    this%source = 'MODFLOW 6 '//trim(adjustl(VERSION))
    !
    ! -- create timestamp
    call date_and_time(values=values)
    write (this%history, '(a,i0,a,i0,a,i0,a,i0,a,i0,a,i0,a,i0)') &
      'first created ', values(1), '/', values(2), '/', values(3), ' ', &
      values(5), ':', values(6), ':', values(7), '.', values(8)
  end subroutine set

  !> @brief initialization of model netcdf export
  !<
  subroutine export_init(this, modelname, modeltype, modelfname, disenum, &
                         nctype, iout)
    use SimVariablesModule, only: idm_context
    use TdisModule, only: datetime0, nstp
    use MemoryManagerModule, only: mem_setptr
    use MemoryHelperModule, only: create_mem_path
    use MemoryManagerExtModule, only: mem_set_value
    use InputOutputModule, only: lowcase
    use UtlNcfInputModule, only: UtlNcfParamFoundType
    class(NCModelExportType), intent(inout) :: this
    character(len=*), intent(in) :: modelname
    character(len=*), intent(in) :: modeltype
    character(len=*), intent(in) :: modelfname
    integer(I4B), intent(in) :: disenum
    integer(I4B), intent(in) :: nctype
    integer(I4B), intent(in) :: iout
    character(len=LENMEMPATH) :: model_mempath
    type(UtlNcfParamFoundType) :: found
    logical(LGP) :: found_mempath
    !
    ! -- initialize
    this%modelname = modelname
    this%modeltype = modeltype
    this%modelfname = modelfname
    this%nc_filename = trim(modelname)//'.nc'
    call lowcase(this%nc_filename)
    this%ncf_mempath = ''
    this%ogc_wkt = ''
    this%datetime = ''
    this%xname = ''
    this%disenum = disenum
    this%ncid = 0
    this%stepcnt = 0
    this%totnstp = 0
    this%iout = iout
    !
    ! -- set file scoped attributes
    call this%annotation%set(modelname, modeltype, modelfname, nctype)
    !
    ! -- set dependent variable basename
    select case (modeltype)
    case ('GWF')
      this%xname = 'head'
    case ('GWT')
      this%xname = 'concentration'
    case ('GWE')
      this%xname = 'temperature'
    case default
      errmsg = trim(modeltype)//' models not supported for NetCDF export.'
      call store_error(errmsg)
      call store_error_filename(modelfname)
    end select
    !
    ! -- set discretization input mempath
    if (disenum == DIS) then
      this%dis_mempath = create_mem_path(modelname, 'DIS', idm_context)
    else if (disenum == DISU) then
      this%dis_mempath = create_mem_path(modelname, 'DISU', idm_context)
    else if (disenum == DISV) then
      this%dis_mempath = create_mem_path(modelname, 'DISV', idm_context)
    end if
    !
    ! -- set dependent variable pointer
    model_mempath = create_mem_path(component=modelname)
    call mem_setptr(this%x, 'X', model_mempath)
    !
    ! --set ncf_mempath and wkt string if provided
    call mem_set_value(this%ncf_mempath, 'NCF6_MEMPATH', this%dis_mempath, &
                       found_mempath)
    if (found_mempath) then
      call mem_set_value(this%ogc_wkt, 'OGC_WKT', this%ncf_mempath, &
                         found%ogc_wkt)
    end if
    !
    ! -- set datetime string
    if (datetime0 == '') then
      errmsg = 'TDIS parameter START_DATE_TIME required for NetCDF export.'
      call store_error(errmsg)
      call store_error_filename(modelfname)
    else
      this%datetime = 'days since '//trim(datetime0)
    end if
    !
    ! -- set total nstp
    this%totnstp = sum(nstp)
  end subroutine export_init

  !> @brief destroy model netcdf export object
  !<
  subroutine export_destroy(this)
    class(NCModelExportType), intent(inout) :: this
    ! -- override in derived class
  end subroutine export_destroy

end module NCModelExportModule
