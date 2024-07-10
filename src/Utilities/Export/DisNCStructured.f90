!> @brief This module contains the DisNCStructuredModule
!!
!! This module defines a STRUCTURED (non-ugrid) netcdf
!! export type for DIS models. It is dependent on netcdf
!! libraries.
!!
!<
module DisNCStructuredModule

  use KindModule, only: DP, I4B, LGP
  use ConstantsModule, only: LINELENGTH, LENBIGLINE, LENCOMPONENTNAME, &
                             LENMEMPATH
  use SimVariablesModule, only: errmsg
  use SimModule, only: store_error, store_error_filename
  use MemoryManagerModule, only: mem_setptr
  use InputDefinitionModule, only: InputParamDefinitionType
  use CharacterStringModule, only: CharacterStringType
  use NCModelExportModule, only: NCBaseModelExportType
  use DisModule, only: DisType
  use NetCDFCommonModule, only: nf_verify
  use netcdf

  implicit none
  private
  public :: DisNCStructuredType

  type :: StructuredNCDimIdType
    integer(I4B) :: x
    integer(I4B) :: y
    integer(I4B) :: z
    integer(I4B) :: time
    integer(I4B) :: bnd
  contains
  end type StructuredNCDimIdType

  type :: StructuredNCVarIdType
    integer(I4B) :: x
    integer(I4B) :: y
    integer(I4B) :: z
    integer(I4B) :: time
    integer(I4B) :: dependent
    integer(I4B) :: x_bnds
    integer(I4B) :: y_bnds
    integer(I4B) :: z_bnds
    integer(I4B) :: lat
    integer(I4B) :: lon
  contains
  end type StructuredNCVarIdType

  type, extends(NCBaseModelExportType) :: DisNCStructuredType
    type(StructuredNCDimIdType) :: dim_ids
    type(StructuredNCVarIdType) :: var_ids
    class(DisType), pointer :: dis => null() !< pointer to model dis package
    integer(I4B) :: nlay !< number of layers
    real(DP), dimension(:), pointer, contiguous :: lat => null()
    real(DP), dimension(:), pointer, contiguous :: lon => null()
    integer(I4B), pointer :: chunk_z !< chunking parameter for z dimension
    integer(I4B), pointer :: chunk_y !< chunking parameter for y dimension
    integer(I4B), pointer :: chunk_x !< chunking parameter for x dimension
    integer(I4B), dimension(:), allocatable :: layers
    logical(LGP) :: latlon
  contains
    procedure :: init => dis_export_init
    procedure :: destroy => dis_export_destroy
    procedure :: df
    procedure :: step
    procedure :: export_input_array
    procedure :: export_input_arrays
    procedure :: add_pkg_data
    procedure :: add_global_att
    procedure :: define_dim
    procedure :: define_dependent
    procedure :: define_gridmap
    procedure :: define_projection
    procedure :: add_proj_data
    procedure :: add_grid_data
  end type DisNCStructuredType

  interface nc_export_array
    module procedure nc_export_int1d, nc_export_int2d, &
      nc_export_int3d, nc_export_dbl1d, &
      nc_export_dbl2d, nc_export_dbl3d
  end interface nc_export_array

contains

  !> @brief netcdf export dis init
  !<
  subroutine dis_export_init(this, modelname, modeltype, modelfname, disenum, &
                             nctype, iout)
    use MemoryManagerModule, only: get_isize
    use MemoryManagerExtModule, only: mem_set_value
    class(DisNCStructuredType), intent(inout) :: this
    character(len=*), intent(in) :: modelname
    character(len=*), intent(in) :: modeltype
    character(len=*), intent(in) :: modelfname
    integer(I4B), intent(in) :: disenum
    integer(I4B), intent(in) :: nctype
    integer(I4B), intent(in) :: iout
    integer(I4B) :: k, latsz, lonsz
    logical(LGP) :: found
    !
    ! -- set nlay
    this%nlay = this%dis%nlay
    !
    ! -- allocate
    allocate (this%chunk_z)
    allocate (this%chunk_y)
    allocate (this%chunk_x)
    allocate (this%layers(this%nlay))
    !
    ! -- initialize
    this%chunk_z = -1
    this%chunk_y = -1
    this%chunk_x = -1
    do k = 1, this%nlay
      this%layers(k) = k
    end do
    !
    this%latlon = .false.
    !
    ! -- initialize base class
    call this%NCModelExportType%init(modelname, modeltype, modelfname, disenum, &
                                     nctype, iout)
    ! -- update values from input context
    if (this%ncf_mempath /= '') then
      call mem_set_value(this%chunk_z, 'CHUNK_Z', this%ncf_mempath, found)
      call mem_set_value(this%chunk_y, 'CHUNK_Y', this%ncf_mempath, found)
      call mem_set_value(this%chunk_x, 'CHUNK_X', this%ncf_mempath, found)
      !
      if (this%chunk_time > 0 .and. this%chunk_z > 0 .and. &
          this%chunk_y > 0 .and. this%chunk_x > 0) then
        this%chunking_active = .true.
      end if
      !
      call get_isize('LAT', this%ncf_mempath, latsz)
      call get_isize('LON', this%ncf_mempath, lonsz)
      !
      if (latsz > 0 .and. lonsz > 0) then
        this%latlon = .true.
        call mem_setptr(this%lat, 'LAT', this%ncf_mempath)
        call mem_setptr(this%lon, 'LON', this%ncf_mempath)
      end if
    end if
    !
    ! -- create the netcdf file
    call nf_verify(nf90_create(this%nc_fname, &
                               IOR(NF90_CLOBBER, NF90_NETCDF4), this%ncid), &
                   this%nc_fname)
  end subroutine dis_export_init

  !> @brief netcdf export dis destroy
  !<
  subroutine dis_export_destroy(this)
    use SimVariablesModule, only: idm_context
    use MemoryManagerExtModule, only: memorystore_remove
    class(DisNCStructuredType), intent(inout) :: this
    call nf_verify(nf90_close(this%ncid), this%nc_fname)
    !
    ! -- destroy base class
    call this%NCModelExportType%destroy()
    !
    ! -- Deallocate idm memory
    if (this%ncf_mempath /= '') then
      call memorystore_remove(this%modelname, 'NCF', idm_context)
    end if
  end subroutine dis_export_destroy

  !> @brief netcdf export define
  !<
  subroutine df(this)
    use ConstantsModule, only: MVALIDATE
    use SimVariablesModule, only: isim_mode
    class(DisNCStructuredType), intent(inout) :: this
    ! -- put root group file scope attributes
    call this%add_global_att()
    ! -- define root group dimensions and coordinate variables
    call this%define_dim()
    ! -- define grid projection variables
    call this%define_projection()
    if (isim_mode /= MVALIDATE) then
      ! -- define the dependent variable
      call this%define_dependent()
    end if
    ! -- exit define mode
    call nf_verify(nf90_enddef(this%ncid), this%nc_fname)
    ! -- add data locations
    call this%add_grid_data()
    ! -- add projection data
    call this%add_proj_data()
    ! -- define and set package input griddata
    call this%add_pkg_data()
    ! -- define and set gridmap variable
    call this%define_gridmap()
    ! -- synchronize file
    call nf_verify(nf90_sync(this%ncid), this%nc_fname)
  end subroutine df

  !> @brief netcdf export step
  !<
  subroutine step(this)
    use TdisModule, only: totim
    class(DisNCStructuredType), intent(inout) :: this
    real(DP), dimension(:), pointer, contiguous :: dbl1d
    integer(I4B) :: n
    !
    this%stepcnt = this%stepcnt + 1
    !
    if (size(this%dis%nodeuser) < &
        size(this%dis%nodereduced)) then
      allocate (dbl1d(size(this%dis%nodereduced)))
      dbl1d = NF90_FILL_DOUBLE
      do n = 1, size(this%dis%nodereduced)
        if (this%dis%nodereduced(n) > 0) then
          dbl1d(n) = this%x(this%dis%nodereduced(n))
        end if
      end do
      ! -- write step data to dependent variable
      call nf_verify(nf90_put_var(this%ncid, &
                                  this%var_ids%dependent, dbl1d, &
                                  start=(/1, 1, 1, this%stepcnt/), &
                                  count=(/this%dis%ncol, &
                                          this%dis%nrow, &
                                          this%dis%nlay, 1/)), &
                     this%nc_fname)
      deallocate (dbl1d)
    else
      ! -- write step data to dependent variable
      call nf_verify(nf90_put_var(this%ncid, &
                                  this%var_ids%dependent, this%x, &
                                  start=(/1, 1, 1, this%stepcnt/), &
                                  count=(/this%dis%ncol, &
                                          this%dis%nrow, &
                                          this%dis%nlay, 1/)), &
                     this%nc_fname)
    end if
    !
    ! -- write to time coordinate variable
    call nf_verify(nf90_put_var(this%ncid, this%var_ids%time, &
                                totim, start=(/this%stepcnt/)), &
                   this%nc_fname)
    ! -- synchronize file
    call nf_verify(nf90_sync(this%ncid), this%nc_fname)
  end subroutine step

  !> @brief netcdf export an input array
  !<
  subroutine export_input_array(this, pkgtype, pkgname, mempath, idt)
    use InputOutputModule, only: lowcase
    use MemoryHelperModule, only: memPathSeparator
    class(DisNCStructuredType), intent(inout) :: this
    character(len=*), intent(in) :: pkgtype
    character(len=*), intent(in) :: pkgname
    character(len=*), intent(in) :: mempath
    type(InputParamDefinitionType), pointer, intent(in) :: idt
    integer(I4B), dimension(:), pointer, contiguous :: int1d
    integer(I4B), dimension(:, :), pointer, contiguous :: int2d
    integer(I4B), dimension(:, :, :), pointer, contiguous :: int3d
    real(DP), dimension(:), pointer, contiguous :: dbl1d
    real(DP), dimension(:, :), pointer, contiguous :: dbl2d
    real(DP), dimension(:, :, :), pointer, contiguous :: dbl3d
    character(len=LINELENGTH) :: pname, vname, nc_varname, gridmap, input_attr
    !
    ! -- set variable name written to file
    pname = pkgname
    vname = idt%tagname
    call lowcase(pname)
    call lowcase(vname)
    nc_varname = trim(pname)//'_'//trim(vname)
    if (this%ogc_wkt /= '') then
      gridmap = this%gridmap_name
    else
      gridmap = ''
    end if
    !
    if (this%input_attr > 0) then
      input_attr = trim(this%modelname)//memPathSeparator//trim(pkgname)// &
                   memPathSeparator//trim(idt%mf6varname)
    else
      input_attr = ''
    end if
    !
    select case (idt%datatype)
    case ('INTEGER1D')
      call mem_setptr(int1d, idt%mf6varname, mempath)
      call nc_export_array(this%ncid, this%dim_ids, this%var_ids, this%dis, &
                           int1d, nc_varname, pkgname, idt%tagname, &
                           idt%shape, idt%longname, input_attr, gridmap, &
                           this%latlon, this%deflate, this%shuffle, &
                           this%chunk_z, this%chunk_y, this%chunk_x, &
                           this%nc_fname)
    case ('INTEGER2D')
      call mem_setptr(int2d, idt%mf6varname, mempath)
      call nc_export_array(this%ncid, this%dim_ids, this%var_ids, this%dis, &
                           int2d, nc_varname, pkgname, idt%tagname, &
                           idt%shape, idt%longname, input_attr, gridmap, &
                           this%latlon, this%deflate, this%shuffle, &
                           this%chunk_z, this%chunk_y, this%chunk_x, &
                           this%nc_fname)
    case ('INTEGER3D')
      call mem_setptr(int3d, idt%mf6varname, mempath)
      call nc_export_array(this%ncid, this%dim_ids, this%var_ids, this%dis, &
                           int3d, nc_varname, pkgname, idt%tagname, &
                           idt%shape, idt%longname, input_attr, gridmap, &
                           this%latlon, this%deflate, this%shuffle, &
                           this%chunk_z, this%chunk_y, this%chunk_x, &
                           this%nc_fname)
    case ('DOUBLE1D')
      call mem_setptr(dbl1d, idt%mf6varname, mempath)
      call nc_export_array(this%ncid, this%dim_ids, this%var_ids, this%dis, &
                           dbl1d, nc_varname, pkgname, idt%tagname, &
                           idt%shape, idt%longname, input_attr, gridmap, &
                           this%latlon, this%deflate, this%shuffle, &
                           this%chunk_z, this%chunk_y, this%chunk_x, &
                           this%nc_fname)
    case ('DOUBLE2D')
      call mem_setptr(dbl2d, idt%mf6varname, mempath)
      call nc_export_array(this%ncid, this%dim_ids, this%var_ids, this%dis, &
                           dbl2d, nc_varname, pkgname, idt%tagname, &
                           idt%shape, idt%longname, input_attr, gridmap, &
                           this%latlon, this%deflate, this%shuffle, &
                           this%chunk_z, this%chunk_y, this%chunk_x, &
                           this%nc_fname)
    case ('DOUBLE3D')
      call mem_setptr(dbl3d, idt%mf6varname, mempath)
      call nc_export_array(this%ncid, this%dim_ids, this%var_ids, this%dis, &
                           dbl3d, nc_varname, pkgname, idt%tagname, &
                           idt%shape, idt%longname, input_attr, gridmap, &
                           this%latlon, this%deflate, this%shuffle, &
                           this%chunk_z, this%chunk_y, this%chunk_x, &
                           this%nc_fname)
    case default
      ! -- no-op, no other datatypes exported
    end select
  end subroutine export_input_array

  !> @brief write package gridded input data
  !<
  subroutine export_input_arrays(this, pkgtype, pkgname, mempath, param_dfns)
    use MemoryManagerModule, only: get_isize
    class(DisNCStructuredType), intent(inout) :: this
    character(len=*), intent(in) :: pkgtype
    character(len=*), intent(in) :: pkgname
    character(len=*), intent(in) :: mempath
    type(InputParamDefinitionType), dimension(:), pointer, &
      intent(in) :: param_dfns
    type(InputParamDefinitionType), pointer :: idt
    integer(I4B) :: iparam, isize
    !
    do iparam = 1, size(param_dfns)
      ! -- assign param definition pointer
      idt => param_dfns(iparam)
      ! -- for now
      if (idt%blockname == 'GRIDDATA') then
        ! -- check if variable is already allocated
        call get_isize(idt%mf6varname, mempath, isize)
        !
        if (isize > 0) then
          call this%export_input_array(pkgtype, pkgname, mempath, idt)
        end if
      end if
    end do
  end subroutine export_input_arrays

  !> @brief determine packages to write gridded input
  !<
  subroutine add_pkg_data(this)
    use SimVariablesModule, only: idm_context
    use MemoryManagerExtModule, only: mem_set_value
    use MemoryHelperModule, only: create_mem_path
    use IdmDfnSelectorModule, only: param_definitions
    use SourceCommonModule, only: idm_subcomponent_type
    class(DisNCStructuredType), intent(inout) :: this
    character(LENCOMPONENTNAME) :: ptype, pname, pkgtype
    type(CharacterStringType), dimension(:), contiguous, &
      pointer :: pkgtypes => null()
    type(CharacterStringType), dimension(:), contiguous, &
      pointer :: pkgnames => null()
    type(CharacterStringType), dimension(:), contiguous, &
      pointer :: mempaths => null()
    type(InputParamDefinitionType), dimension(:), pointer :: param_dfns
    character(len=LENMEMPATH) :: input_mempath, mempath
    integer(I4B) :: n
    integer(I4B), pointer :: export_arrays
    logical(LGP) :: found
    !
    input_mempath = create_mem_path(component=this%modelname, context=idm_context)
    !
    ! -- set pointers to model path package info
    call mem_setptr(pkgtypes, 'PKGTYPES', input_mempath)
    call mem_setptr(pkgnames, 'PKGNAMES', input_mempath)
    call mem_setptr(mempaths, 'MEMPATHS', input_mempath)
    !
    do n = 1, size(mempaths)
      !
      ! -- allocate export_arrays
      allocate (export_arrays)
      export_arrays = 0
      !
      ! -- set package attributes
      mempath = mempaths(n)
      pname = pkgnames(n)
      ptype = pkgtypes(n)
      !
      ! -- export input arrays
      if (mempath /= '') then
        ! -- update export
        call mem_set_value(export_arrays, 'EXPORT_NC', mempath, found)
        !
        if (export_arrays > 0) then
          pkgtype = idm_subcomponent_type(this%modeltype, ptype)
          param_dfns => param_definitions(this%modeltype, pkgtype)
          call this%export_input_arrays(ptype, pname, mempath, param_dfns)
        end if
      end if
      !
      ! -- cleanup
      deallocate (export_arrays)
    end do
  end subroutine add_pkg_data

  !> @brief create file (group) attributes
  !<
  subroutine add_global_att(this)
    class(DisNCStructuredType), intent(inout) :: this
    ! -- file scoped title
    call nf_verify(nf90_put_att(this%ncid, NF90_GLOBAL, 'title', &
                                this%annotation%title), this%nc_fname)
    ! -- source (MODFLOW 6)
    call nf_verify(nf90_put_att(this%ncid, NF90_GLOBAL, 'source', &
                                this%annotation%source), this%nc_fname)
    ! -- export type (MODFLOW 6)
    call nf_verify(nf90_put_att(this%ncid, NF90_GLOBAL, 'modflow6_grid', &
                                this%annotation%grid), this%nc_fname)
    ! -- MODFLOW 6 model type
    call nf_verify(nf90_put_att(this%ncid, NF90_GLOBAL, 'modflow6_model', &
                                this%annotation%model), this%nc_fname)
    ! -- generation datetime
    call nf_verify(nf90_put_att(this%ncid, NF90_GLOBAL, 'history', &
                                this%annotation%history), this%nc_fname)
    ! -- supported conventions
    call nf_verify(nf90_put_att(this%ncid, NF90_GLOBAL, 'Conventions', &
                                this%annotation%conventions), &
                   this%nc_fname)
  end subroutine add_global_att

  !> @brief netcdf export define dimensions
  !<
  subroutine define_dim(this)
    use ConstantsModule, only: MVALIDATE
    use SimVariablesModule, only: isim_mode
    class(DisNCStructuredType), intent(inout) :: this

    ! bound dim
    call nf_verify(nf90_def_dim(this%ncid, 'bnd', 2, this%dim_ids%bnd), &
                   this%nc_fname)
    !
    ! -- Time
    if (isim_mode /= MVALIDATE) then
      call nf_verify(nf90_def_dim(this%ncid, 'time', this%totnstp, &
                                  this%dim_ids%time), this%nc_fname)
      call nf_verify(nf90_def_var(this%ncid, 'time', NF90_DOUBLE, &
                                  this%dim_ids%time, this%var_ids%time), &
                     this%nc_fname)
      call nf_verify(nf90_put_att(this%ncid, this%var_ids%time, 'calendar', &
                                  'standard'), this%nc_fname)
      call nf_verify(nf90_put_att(this%ncid, this%var_ids%time, 'units', &
                                  this%datetime), this%nc_fname)
      call nf_verify(nf90_put_att(this%ncid, this%var_ids%time, 'axis', 'T'), &
                     this%nc_fname)
      !call nf_verify(nf90_put_att(ncid, var_ids%time, 'bounds', 'time_bnds'), this%nc_fname)
      call nf_verify(nf90_put_att(this%ncid, this%var_ids%time, 'standard_name', &
                                  'time'), this%nc_fname)
      call nf_verify(nf90_put_att(this%ncid, this%var_ids%time, 'long_name', &
                                  'time'), this%nc_fname)
    end if
    !
    ! -- Z dimension
    call nf_verify(nf90_def_dim(this%ncid, 'z', this%dis%nlay, this%dim_ids%z), &
                   this%nc_fname)
    call nf_verify(nf90_def_var(this%ncid, 'z', NF90_DOUBLE, this%dim_ids%z, &
                                this%var_ids%z), this%nc_fname)
    call nf_verify(nf90_put_att(this%ncid, this%var_ids%z, 'units', 'layer'), &
                   this%nc_fname)
    call nf_verify(nf90_put_att(this%ncid, this%var_ids%z, 'long_name', &
                                'layer number'), this%nc_fname)
    !call nf_verify(nf90_put_att(this%ncid, this%var_ids%z, 'bounds', 'z_bnds'), &
    !               this%nc_fname)
    !call nf_verify(nf90_def_var(this%ncid, 'z_bnds', NF90_DOUBLE, &
    !                            (/this%dim_ids%bnd, this%dim_ids%z/), &
    !                            this%var_ids%z_bnds), this%nc_fname)
    !call nf_verify(nf90_put_var(this%ncid, this%var_ids%z_bnds, &
    !                            this%elev_bnds), this%nc_fname)
    !
    ! -- Y dimension
    call nf_verify(nf90_def_dim(this%ncid, 'y', this%dis%nrow, this%dim_ids%y), &
                   this%nc_fname)
    call nf_verify(nf90_def_var(this%ncid, 'y', NF90_DOUBLE, this%dim_ids%y, &
                                this%var_ids%y), this%nc_fname)
    call nf_verify(nf90_put_att(this%ncid, this%var_ids%y, 'units', 'm'), &
                   this%nc_fname)
    call nf_verify(nf90_put_att(this%ncid, this%var_ids%y, 'axis', 'Y'), &
                   this%nc_fname)
    call nf_verify(nf90_put_att(this%ncid, this%var_ids%y, 'standard_name', &
                                'projection_y_coordinate'), this%nc_fname)
    call nf_verify(nf90_put_att(this%ncid, this%var_ids%y, 'long_name', &
                                'Northing'), this%nc_fname)
    if (this%ogc_wkt /= '') then
      call nf_verify(nf90_put_att(this%ncid, this%var_ids%y, 'grid_mapping', &
                                  this%gridmap_name), this%nc_fname)
    end if
    call nf_verify(nf90_put_att(this%ncid, this%var_ids%y, 'bounds', 'y_bnds'), &
                   this%nc_fname)
    call nf_verify(nf90_def_var(this%ncid, 'y_bnds', NF90_DOUBLE, &
                                (/this%dim_ids%bnd, this%dim_ids%y/), &
                                this%var_ids%y_bnds), this%nc_fname)
    !
    ! -- X dimension
    call nf_verify(nf90_def_dim(this%ncid, 'x', this%dis%ncol, this%dim_ids%x), &
                   this%nc_fname)
    call nf_verify(nf90_def_var(this%ncid, 'x', NF90_DOUBLE, this%dim_ids%x, &
                                this%var_ids%x), this%nc_fname)
    call nf_verify(nf90_put_att(this%ncid, this%var_ids%x, 'units', 'm'), &
                   this%nc_fname)
    call nf_verify(nf90_put_att(this%ncid, this%var_ids%x, 'axis', 'X'), &
                   this%nc_fname)
    call nf_verify(nf90_put_att(this%ncid, this%var_ids%x, 'standard_name', &
                                'projection_x_coordinate'), this%nc_fname)
    call nf_verify(nf90_put_att(this%ncid, this%var_ids%x, 'long_name', &
                                'Easting'), this%nc_fname)
    if (this%ogc_wkt /= '') then
      call nf_verify(nf90_put_att(this%ncid, this%var_ids%x, 'grid_mapping', &
                                  this%gridmap_name), this%nc_fname)
    end if
    call nf_verify(nf90_put_att(this%ncid, this%var_ids%x, 'bounds', 'x_bnds'), &
                   this%nc_fname)
    call nf_verify(nf90_def_var(this%ncid, 'x_bnds', NF90_DOUBLE, &
                                (/this%dim_ids%bnd, this%dim_ids%x/), &
                                this%var_ids%x_bnds), this%nc_fname)
  end subroutine define_dim

  !> @brief create the model layer dependent variables
  !<
  subroutine define_dependent(this)
    class(DisNCStructuredType), intent(inout) :: this
    !
    call nf_verify(nf90_def_var(this%ncid, this%xname, NF90_DOUBLE, &
                                (/this%dim_ids%x, this%dim_ids%y, &
                                  this%dim_ids%z, this%dim_ids%time/), &
                                this%var_ids%dependent), &
                   this%nc_fname)
    ! -- apply chunking parameters
    if (this%chunking_active) then
      call nf_verify(nf90_def_var_chunking(this%ncid, &
                                           this%var_ids%dependent, &
                                           NF90_CHUNKED, &
                                           (/this%chunk_x, this%chunk_y, &
                                             this%chunk_z, this%chunk_time/)), &
                     this%nc_fname)
    end if
    ! -- deflate and shuffle
    if (this%deflate >= 0) then
      call nf_verify(nf90_def_var_deflate(this%ncid, &
                                          this%var_ids%dependent, &
                                          shuffle=this%shuffle, deflate=1, &
                                          deflate_level=this%deflate), &
                     this%nc_fname)
    end if
    !
    call nf_verify(nf90_put_att(this%ncid, this%var_ids%dependent, &
                                'units', 'm'), this%nc_fname)
    call nf_verify(nf90_put_att(this%ncid, this%var_ids%dependent, &
                                'standard_name', this%annotation%stdname), &
                   this%nc_fname)
    call nf_verify(nf90_put_att(this%ncid, this%var_ids%dependent, 'long_name', &
                                this%annotation%longname), this%nc_fname)
    call nf_verify(nf90_put_att(this%ncid, this%var_ids%dependent, '_FillValue', &
                                (/NF90_FILL_DOUBLE/)), this%nc_fname)
    if (this%ogc_wkt /= '') then
      if (this%latlon) then
        call nf_verify(nf90_put_att(this%ncid, this%var_ids%dependent, &
                                    'coordinates', 'lon lat'), &
                       this%nc_fname)
      else
        call nf_verify(nf90_put_att(this%ncid, this%var_ids%dependent, &
                                    'coordinates', 'x y'), this%nc_fname)
      end if
      call nf_verify(nf90_put_att(this%ncid, this%var_ids%dependent, &
                                  'grid_mapping', this%gridmap_name), &
                     this%nc_fname)
    end if
  end subroutine define_dependent

  !> @brief create the file grid mapping container variable
  !<
  subroutine define_gridmap(this)
    class(DisNCStructuredType), intent(inout) :: this
    integer(I4B) :: var_id
    if (this%ogc_wkt /= '') then
      call nf_verify(nf90_redef(this%ncid), this%nc_fname)
      call nf_verify(nf90_def_var(this%ncid, this%gridmap_name, NF90_INT, &
                                  var_id), this%nc_fname)
      call nf_verify(nf90_put_att(this%ncid, var_id, 'crs_wkt', this%ogc_wkt), &
                     this%nc_fname)
      call nf_verify(nf90_enddef(this%ncid), this%nc_fname)
      call nf_verify(nf90_put_var(this%ncid, var_id, 1), &
                     this%nc_fname)
    end if
  end subroutine define_gridmap

  !> @brief define grid projection variables
  !<
  subroutine define_projection(this)
    class(DisNCStructuredType), intent(inout) :: this
    if (this%latlon .and. this%ogc_wkt /= '') then
      ! -- lat
      call nf_verify(nf90_def_var(this%ncid, 'lat', NF90_DOUBLE, &
                                  (/this%dim_ids%x, this%dim_ids%y/), &
                                  this%var_ids%lat), this%nc_fname)
      call nf_verify(nf90_put_att(this%ncid, this%var_ids%lat, 'units', &
                                  'degrees_north'), this%nc_fname)
      call nf_verify(nf90_put_att(this%ncid, this%var_ids%lat, 'standard_name', &
                                  'latitude'), this%nc_fname)
      call nf_verify(nf90_put_att(this%ncid, this%var_ids%lat, 'long_name', &
                                  'latitude'), this%nc_fname)
      !
      ! -- lon
      call nf_verify(nf90_def_var(this%ncid, 'lon', NF90_DOUBLE, &
                                  (/this%dim_ids%x, this%dim_ids%y/), &
                                  this%var_ids%lon), this%nc_fname)
      call nf_verify(nf90_put_att(this%ncid, this%var_ids%lon, 'units', &
                                  'degrees_east'), this%nc_fname)
      call nf_verify(nf90_put_att(this%ncid, this%var_ids%lon, 'standard_name', &
                                  'longitude'), this%nc_fname)
      call nf_verify(nf90_put_att(this%ncid, this%var_ids%lon, 'long_name', &
                                  'longitude'), this%nc_fname)
    end if
  end subroutine define_projection

  !> @brief add grid projection data
  !<
  subroutine add_proj_data(this)
    class(DisNCStructuredType), intent(inout) :: this
    if (this%latlon .and. this%ogc_wkt /= '') then
      ! -- lat
      !
      call nf_verify(nf90_put_var(this%ncid, this%var_ids%lat, &
                                  this%lat, start=(/1, 1/), &
                                  count=(/this%dis%ncol, this%dis%nrow/)), &
                     this%nc_fname)
      ! -- lon
      !
      call nf_verify(nf90_put_var(this%ncid, this%var_ids%lon, &
                                  this%lon, start=(/1, 1/), &
                                  count=(/this%dis%ncol, this%dis%nrow/)), &
                     this%nc_fname)
    end if
  end subroutine add_proj_data

  !> @brief add grid coordinates
  !<
  subroutine add_grid_data(this)
    class(DisNCStructuredType), intent(inout) :: this
    integer(I4B) :: ibnd, n !, k, i, j
    real(DP), dimension(:, :), pointer, contiguous :: dbl2d
    real(DP), dimension(:), allocatable :: x, y
    !
    allocate (x(size(this%dis%cellx)))
    allocate (y(size(this%dis%celly)))

    do n = 1, size(this%dis%cellx)
      x(n) = this%dis%cellx(n) + this%dis%xorigin
    end do

    do n = 1, size(this%dis%celly)
      y(n) = this%dis%celly(n) + this%dis%yorigin
    end do

    call nf_verify(nf90_put_var(this%ncid, this%var_ids%x, x), &
                   this%nc_fname)
    call nf_verify(nf90_put_var(this%ncid, this%var_ids%y, y), &
                   this%nc_fname)
    ! -- TODO see cf-conventions 4.3.3. Parametric Vertical Coordinate
    call nf_verify(nf90_put_var(this%ncid, this%var_ids%z, this%layers), &
                   this%nc_fname)

    deallocate (x)
    deallocate (y)

    !
    ! -- bounds x
    allocate (dbl2d(2, size(this%dis%cellx)))
    ibnd = 1
    do n = 1, size(this%dis%cellx)
      if (ibnd == 1) then
        dbl2d(1, ibnd) = this%dis%xorigin
        dbl2d(2, ibnd) = this%dis%xorigin + this%dis%delr(ibnd)
      else
        dbl2d(1, ibnd) = dbl2d(1, ibnd - 1) + this%dis%delr(ibnd)
        dbl2d(2, ibnd) = dbl2d(2, ibnd - 1) + this%dis%delr(ibnd)
      end if
      ibnd = ibnd + 1
    end do
    call nf_verify(nf90_put_var(this%ncid, this%var_ids%x_bnds, dbl2d), &
                   this%nc_fname)
    deallocate (dbl2d)

    ! -- bounds y
    allocate (dbl2d(2, size(this%dis%celly)))
    ibnd = 1
    do n = size(this%dis%celly), 1, -1
      if (ibnd == 1) then
        dbl2d(1, ibnd) = this%dis%yorigin + sum(this%dis%delc) - this%dis%delc(n)
        dbl2d(2, ibnd) = this%dis%yorigin + sum(this%dis%delc)
      else
        dbl2d(1, ibnd) = dbl2d(1, ibnd - 1) - this%dis%delc(n)
        dbl2d(2, ibnd) = dbl2d(2, ibnd - 1) - this%dis%delc(n)
      end if
      ibnd = ibnd + 1
    end do
    call nf_verify(nf90_put_var(this%ncid, this%var_ids%y_bnds, dbl2d), &
                   this%nc_fname)
    deallocate (dbl2d)
  end subroutine add_grid_data

  !> @brief netcdf export 1D integer
  !<
  subroutine nc_export_int1d(ncid, dim_ids, var_ids, dis, p_mem, nc_varname, &
                             pkgname, tagname, shapestr, longname, nc_tag, &
                             gridmap_name, latlon, deflate, shuffle, chunk_z, &
                             chunk_y, chunk_x, nc_fname)
    use InputOutputModule, only: lowcase
    integer(I4B), intent(in) :: ncid
    type(StructuredNCDimIdType), intent(inout) :: dim_ids
    type(StructuredNCVarIdType), intent(inout) :: var_ids
    type(DisType), pointer, intent(in) :: dis
    integer(I4B), dimension(:), pointer, contiguous, intent(in) :: p_mem
    character(len=*), intent(in) :: nc_varname
    character(len=*), intent(in) :: pkgname
    character(len=*), intent(in) :: tagname
    character(len=*), intent(in) :: shapestr
    character(len=*), intent(in) :: longname
    character(len=*), intent(in) :: nc_tag
    character(len=*), intent(in) :: gridmap_name
    logical(LGP), intent(in) :: latlon
    integer(I4B), intent(in) :: deflate
    integer(I4B), intent(in) :: shuffle
    integer(I4B), intent(in) :: chunk_z
    integer(I4B), intent(in) :: chunk_y
    integer(I4B), intent(in) :: chunk_x
    character(len=*), intent(in) :: nc_fname
    ! -- local
    integer(I4B) :: var_id, axis_sz
    !
    if (shapestr == 'NROW' .or. &
        shapestr == 'NCOL') then
      if (shapestr == 'NROW') then
        axis_sz = dim_ids%y
      else
        axis_sz = dim_ids%x
      end if
      !
      ! -- reenter define mode and create variable
      call nf_verify(nf90_redef(ncid), nc_fname)
      call nf_verify(nf90_def_var(ncid, nc_varname, NF90_INT, &
                                  (/axis_sz/), var_id), &
                     nc_fname)
      !
      ! -- NROW/NCOL shapes use default chunking
      if (deflate >= 0) then
        call nf_verify(nf90_def_var_deflate(ncid, var_id, shuffle=shuffle, &
                                            deflate=1, deflate_level=deflate), &
                       nc_fname)
      end if
      !
      call nf_verify(nf90_put_att(ncid, var_id, '_FillValue', &
                                  (/NF90_FILL_INT/)), nc_fname)
      call nf_verify(nf90_put_att(ncid, var_id, 'long_name', &
                                  longname), nc_fname)
      !
      if (nc_tag /= '') then
        call nf_verify(nf90_put_att(ncid, var_id, 'modflow6_input', &
                                    nc_tag), nc_fname)
      end if
      !
      ! -- exit define mode and write data
      call nf_verify(nf90_enddef(ncid), nc_fname)
      call nf_verify(nf90_put_var(ncid, var_id, p_mem), &
                     nc_fname)

    else
      !
      ! -- reenter define mode and create variable
      call nf_verify(nf90_redef(ncid), nc_fname)
      call nf_verify(nf90_def_var(ncid, nc_varname, NF90_INT, &
                                  (/dim_ids%x, dim_ids%y, dim_ids%z/), var_id), &
                     nc_fname)
      !
      ! -- apply chunking parameters
      if (chunk_z > 0 .and. chunk_y > 0 .and. chunk_x > 0) then
        call nf_verify(nf90_def_var_chunking(ncid, var_id, NF90_CHUNKED, &
                                             (/chunk_x, chunk_y, chunk_z/)), &
                       nc_fname)
      end if
      ! -- deflate and shuffle
      if (deflate >= 0) then
        call nf_verify(nf90_def_var_deflate(ncid, var_id, shuffle=shuffle, &
                                            deflate=1, deflate_level=deflate), &
                       nc_fname)
      end if
      !
      call nf_verify(nf90_put_att(ncid, var_id, '_FillValue', &
                                  (/NF90_FILL_INT/)), nc_fname)
      call nf_verify(nf90_put_att(ncid, var_id, 'long_name', &
                                  longname), nc_fname)
      if (gridmap_name /= '') then
        if (latlon) then
          call nf_verify(nf90_put_att(ncid, var_id, 'coordinates', 'lon lat'), &
                         nc_fname)
        else
          call nf_verify(nf90_put_att(ncid, var_id, 'coordinates', 'x y'), &
                         nc_fname)
        end if
        call nf_verify(nf90_put_att(ncid, var_id, 'grid_mapping', gridmap_name), &
                       nc_fname)
      end if
      !
      if (nc_tag /= '') then
        call nf_verify(nf90_put_att(ncid, var_id, 'modflow6_input', &
                                    nc_tag), nc_fname)
      end if
      !
      ! -- exit define mode and write data
      call nf_verify(nf90_enddef(ncid), nc_fname)
      call nf_verify(nf90_put_var(ncid, var_id, p_mem, start=(/1, 1, 1/), &
                                  count=(/dis%ncol, dis%nrow, dis%nlay/)), &
                     nc_fname)
    end if
  end subroutine nc_export_int1d

  !> @brief netcdf export 2D integer
  !<
  subroutine nc_export_int2d(ncid, dim_ids, var_ids, dis, p_mem, nc_varname, &
                             pkgname, tagname, shapestr, longname, nc_tag, &
                             gridmap_name, latlon, deflate, shuffle, chunk_z, &
                             chunk_y, chunk_x, nc_fname)
    integer(I4B), intent(in) :: ncid
    type(StructuredNCDimIdType), intent(inout) :: dim_ids
    type(StructuredNCVarIdType), intent(inout) :: var_ids
    type(DisType), pointer, intent(in) :: dis
    integer(I4B), dimension(:, :), pointer, contiguous, intent(in) :: p_mem
    character(len=*), intent(in) :: nc_varname
    character(len=*), intent(in) :: pkgname
    character(len=*), intent(in) :: tagname
    character(len=*), intent(in) :: shapestr
    character(len=*), intent(in) :: longname
    character(len=*), intent(in) :: nc_tag
    character(len=*), intent(in) :: gridmap_name
    logical(LGP), intent(in) :: latlon
    integer(I4B), intent(in) :: deflate
    integer(I4B), intent(in) :: shuffle
    integer(I4B), intent(in) :: chunk_z
    integer(I4B), intent(in) :: chunk_y
    integer(I4B), intent(in) :: chunk_x
    character(len=*), intent(in) :: nc_fname
    ! -- local
    integer(I4B) :: var_id
    !
    ! -- reenter define mode and create variable
    call nf_verify(nf90_redef(ncid), nc_fname)
    call nf_verify(nf90_def_var(ncid, nc_varname, NF90_INT, &
                                (/dim_ids%x, dim_ids%y/), var_id), &
                   nc_fname)
    !
    ! -- apply chunking parameters
    if (chunk_y > 0 .and. chunk_x > 0) then
      call nf_verify(nf90_def_var_chunking(ncid, var_id, NF90_CHUNKED, &
                                           (/chunk_x, chunk_y/)), nc_fname)
    end if
    ! -- deflate and shuffle
    if (deflate >= 0) then
      call nf_verify(nf90_def_var_deflate(ncid, var_id, shuffle=shuffle, &
                                          deflate=1, deflate_level=deflate), &
                     nc_fname)
    end if
    !
    call nf_verify(nf90_put_att(ncid, var_id, '_FillValue', &
                                (/NF90_FILL_INT/)), nc_fname)
    call nf_verify(nf90_put_att(ncid, var_id, 'long_name', &
                                longname), nc_fname)
    if (gridmap_name /= '') then
      if (latlon) then
        call nf_verify(nf90_put_att(ncid, var_id, 'coordinates', 'lon lat'), &
                       nc_fname)
      else
        call nf_verify(nf90_put_att(ncid, var_id, 'coordinates', 'x y'), &
                       nc_fname)
      end if
      call nf_verify(nf90_put_att(ncid, var_id, 'grid_mapping', gridmap_name), &
                     nc_fname)
    end if
    !
    if (nc_tag /= '') then
      call nf_verify(nf90_put_att(ncid, var_id, 'modflow6_input', &
                                  nc_tag), nc_fname)
    end if
    !
    ! -- exit define mode and write data
    call nf_verify(nf90_enddef(ncid), nc_fname)
    call nf_verify(nf90_put_var(ncid, var_id, p_mem, start=(/1, 1/), &
                                count=(/dis%ncol, dis%nrow/)), &
                   nc_fname)
  end subroutine nc_export_int2d

  !> @brief netcdf export 3D integer
  !<
  subroutine nc_export_int3d(ncid, dim_ids, var_ids, dis, p_mem, nc_varname, &
                             pkgname, tagname, shapestr, longname, nc_tag, &
                             gridmap_name, latlon, deflate, shuffle, chunk_z, &
                             chunk_y, chunk_x, nc_fname)
    use InputOutputModule, only: lowcase
    integer(I4B), intent(in) :: ncid
    type(StructuredNCDimIdType), intent(inout) :: dim_ids
    type(StructuredNCVarIdType), intent(inout) :: var_ids
    type(DisType), pointer, intent(in) :: dis
    integer(I4B), dimension(:, :, :), pointer, contiguous, intent(in) :: p_mem
    character(len=*), intent(in) :: nc_varname
    character(len=*), intent(in) :: pkgname
    character(len=*), intent(in) :: tagname
    character(len=*), intent(in) :: shapestr
    character(len=*), intent(in) :: longname
    character(len=*), intent(in) :: nc_tag
    character(len=*), intent(in) :: gridmap_name
    logical(LGP), intent(in) :: latlon
    integer(I4B), intent(in) :: deflate
    integer(I4B), intent(in) :: shuffle
    integer(I4B), intent(in) :: chunk_z
    integer(I4B), intent(in) :: chunk_y
    integer(I4B), intent(in) :: chunk_x
    character(len=*), intent(in) :: nc_fname
    ! -- local
    integer(I4B) :: var_id
    !
    ! -- reenter define mode and create variable
    call nf_verify(nf90_redef(ncid), nc_fname)
    call nf_verify(nf90_def_var(ncid, nc_varname, NF90_INT, &
                                (/dim_ids%x, dim_ids%y, dim_ids%z/), var_id), &
                   nc_fname)
    !
    ! -- apply chunking parameters
    if (chunk_z > 0 .and. chunk_y > 0 .and. chunk_x > 0) then
      call nf_verify(nf90_def_var_chunking(ncid, var_id, NF90_CHUNKED, &
                                           (/chunk_x, chunk_y, chunk_z/)), &
                     nc_fname)
    end if
    ! -- deflate and shuffle
    if (deflate >= 0) then
      call nf_verify(nf90_def_var_deflate(ncid, var_id, shuffle=shuffle, &
                                          deflate=1, deflate_level=deflate), &
                     nc_fname)
    end if
    !
    call nf_verify(nf90_put_att(ncid, var_id, '_FillValue', &
                                (/NF90_FILL_INT/)), nc_fname)
    call nf_verify(nf90_put_att(ncid, var_id, 'long_name', &
                                longname), nc_fname)
    if (gridmap_name /= '') then
      if (latlon) then
        call nf_verify(nf90_put_att(ncid, var_id, 'coordinates', 'lon lat'), &
                       nc_fname)
      else
        call nf_verify(nf90_put_att(ncid, var_id, 'coordinates', 'x y'), &
                       nc_fname)
      end if
      call nf_verify(nf90_put_att(ncid, var_id, 'grid_mapping', gridmap_name), &
                     nc_fname)
    end if
    !
    if (nc_tag /= '') then
      call nf_verify(nf90_put_att(ncid, var_id, 'modflow6_input', &
                                  nc_tag), nc_fname)
    end if
    !
    ! -- exit define mode and write data
    call nf_verify(nf90_enddef(ncid), nc_fname)
    call nf_verify(nf90_put_var(ncid, var_id, p_mem, start=(/1, 1, 1/), &
                                count=(/dis%ncol, dis%nrow, dis%nlay/)), &
                   nc_fname)
  end subroutine nc_export_int3d

  !> @brief netcdf export 1D double
  !<
  subroutine nc_export_dbl1d(ncid, dim_ids, var_ids, dis, p_mem, nc_varname, &
                             pkgname, tagname, shapestr, longname, nc_tag, &
                             gridmap_name, latlon, deflate, shuffle, chunk_z, &
                             chunk_y, chunk_x, nc_fname)
    use InputOutputModule, only: lowcase
    integer(I4B), intent(in) :: ncid
    type(StructuredNCDimIdType), intent(inout) :: dim_ids
    type(StructuredNCVarIdType), intent(inout) :: var_ids
    type(DisType), pointer, intent(in) :: dis
    real(DP), dimension(:), pointer, contiguous, intent(in) :: p_mem
    character(len=*), intent(in) :: nc_varname
    character(len=*), intent(in) :: pkgname
    character(len=*), intent(in) :: tagname
    character(len=*), intent(in) :: shapestr
    character(len=*), intent(in) :: longname
    character(len=*), intent(in) :: nc_tag
    character(len=*), intent(in) :: gridmap_name
    logical(LGP), intent(in) :: latlon
    integer(I4B), intent(in) :: deflate
    integer(I4B), intent(in) :: shuffle
    integer(I4B), intent(in) :: chunk_z
    integer(I4B), intent(in) :: chunk_y
    integer(I4B), intent(in) :: chunk_x
    character(len=*), intent(in) :: nc_fname
    ! -- local
    integer(I4B) :: var_id, axis_sz
    !
    if (shapestr == 'NROW' .or. &
        shapestr == 'NCOL') then
      if (shapestr == 'NROW') then
        axis_sz = dim_ids%y
      else
        axis_sz = dim_ids%x
      end if
      !
      ! -- reenter define mode and create variable
      call nf_verify(nf90_redef(ncid), nc_fname)
      call nf_verify(nf90_def_var(ncid, nc_varname, NF90_DOUBLE, &
                                  (/axis_sz/), var_id), &
                     nc_fname)
      !
      ! -- NROW/NCOL shapes use default chunking
      if (deflate >= 0) then
        call nf_verify(nf90_def_var_deflate(ncid, var_id, shuffle=shuffle, &
                                            deflate=1, deflate_level=deflate), &
                       nc_fname)
      end if
      !
      call nf_verify(nf90_put_att(ncid, var_id, '_FillValue', &
                                  (/NF90_FILL_DOUBLE/)), nc_fname)
      call nf_verify(nf90_put_att(ncid, var_id, 'long_name', &
                                  longname), nc_fname)
      !
      if (nc_tag /= '') then
        call nf_verify(nf90_put_att(ncid, var_id, 'modflow6_input', &
                                    nc_tag), nc_fname)
      end if
      !
      ! -- exit define mode and write data
      call nf_verify(nf90_enddef(ncid), nc_fname)
      call nf_verify(nf90_put_var(ncid, var_id, p_mem), &
                     nc_fname)

    else
      !
      ! -- reenter define mode and create variable
      call nf_verify(nf90_redef(ncid), nc_fname)
      call nf_verify(nf90_def_var(ncid, nc_varname, NF90_DOUBLE, &
                                  (/dim_ids%x, dim_ids%y, dim_ids%z/), var_id), &
                     nc_fname)
      !
      ! -- apply chunking parameters
      if (chunk_z > 0 .and. chunk_y > 0 .and. chunk_x > 0) then
        call nf_verify(nf90_def_var_chunking(ncid, var_id, NF90_CHUNKED, &
                                             (/chunk_x, chunk_y, chunk_z/)), &
                       nc_fname)
      end if
      ! -- deflate and shuffle
      if (deflate >= 0) then
        call nf_verify(nf90_def_var_deflate(ncid, var_id, shuffle=shuffle, &
                                            deflate=1, deflate_level=deflate), &
                       nc_fname)
      end if
      !
      call nf_verify(nf90_put_att(ncid, var_id, '_FillValue', &
                                  (/NF90_FILL_DOUBLE/)), nc_fname)
      call nf_verify(nf90_put_att(ncid, var_id, 'long_name', &
                                  longname), nc_fname)
      if (gridmap_name /= '') then
        if (latlon) then
          call nf_verify(nf90_put_att(ncid, var_id, 'coordinates', 'lon lat'), &
                         nc_fname)
        else
          call nf_verify(nf90_put_att(ncid, var_id, 'coordinates', 'x y'), &
                         nc_fname)
        end if
        call nf_verify(nf90_put_att(ncid, var_id, 'grid_mapping', gridmap_name), &
                       nc_fname)
      end if
      !
      if (nc_tag /= '') then
        call nf_verify(nf90_put_att(ncid, var_id, 'modflow6_input', &
                                    nc_tag), nc_fname)
      end if
      !
      ! -- exit define mode and write data
      call nf_verify(nf90_enddef(ncid), nc_fname)
      call nf_verify(nf90_put_var(ncid, var_id, p_mem, start=(/1, 1, 1/), &
                                  count=(/dis%ncol, dis%nrow, dis%nlay/)), &
                     nc_fname)
    end if
  end subroutine nc_export_dbl1d

  !> @brief netcdf export 2D double
  !<
  subroutine nc_export_dbl2d(ncid, dim_ids, var_ids, dis, p_mem, nc_varname, &
                             pkgname, tagname, shapestr, longname, nc_tag, &
                             gridmap_name, latlon, deflate, shuffle, chunk_z, &
                             chunk_y, chunk_x, nc_fname)
    integer(I4B), intent(in) :: ncid
    type(StructuredNCDimIdType), intent(inout) :: dim_ids
    type(StructuredNCVarIdType), intent(inout) :: var_ids
    type(DisType), pointer, intent(in) :: dis
    real(DP), dimension(:, :), pointer, contiguous, intent(in) :: p_mem
    character(len=*), intent(in) :: nc_varname
    character(len=*), intent(in) :: pkgname
    character(len=*), intent(in) :: tagname
    character(len=*), intent(in) :: shapestr
    character(len=*), intent(in) :: longname
    character(len=*), intent(in) :: nc_tag
    character(len=*), intent(in) :: gridmap_name
    logical(LGP), intent(in) :: latlon
    integer(I4B), intent(in) :: deflate
    integer(I4B), intent(in) :: shuffle
    integer(I4B), intent(in) :: chunk_z
    integer(I4B), intent(in) :: chunk_y
    integer(I4B), intent(in) :: chunk_x
    character(len=*), intent(in) :: nc_fname
    ! -- local
    integer(I4B) :: var_id
    !
    ! -- reenter define mode and create variable
    call nf_verify(nf90_redef(ncid), nc_fname)
    call nf_verify(nf90_def_var(ncid, nc_varname, NF90_DOUBLE, &
                                (/dim_ids%x, dim_ids%y/), var_id), &
                   nc_fname)
    !
    ! -- apply chunking parameters
    if (chunk_y > 0 .and. chunk_x > 0) then
      call nf_verify(nf90_def_var_chunking(ncid, var_id, NF90_CHUNKED, &
                                           (/chunk_x, chunk_y/)), nc_fname)
    end if
    ! -- deflate and shuffle
    if (deflate >= 0) then
      call nf_verify(nf90_def_var_deflate(ncid, var_id, shuffle=shuffle, &
                                          deflate=1, deflate_level=deflate), &
                     nc_fname)
    end if
    !
    call nf_verify(nf90_put_att(ncid, var_id, '_FillValue', &
                                (/NF90_FILL_DOUBLE/)), nc_fname)
    call nf_verify(nf90_put_att(ncid, var_id, 'long_name', &
                                longname), nc_fname)
    if (gridmap_name /= '') then
      if (latlon) then
        call nf_verify(nf90_put_att(ncid, var_id, 'coordinates', 'lon lat'), &
                       nc_fname)
      else
        call nf_verify(nf90_put_att(ncid, var_id, 'coordinates', 'x y'), &
                       nc_fname)
      end if
      call nf_verify(nf90_put_att(ncid, var_id, 'grid_mapping', gridmap_name), &
                     nc_fname)
    end if
    !
    if (nc_tag /= '') then
      call nf_verify(nf90_put_att(ncid, var_id, 'modflow6_input', &
                                  nc_tag), nc_fname)
    end if
    !
    ! -- exit define mode and write data
    call nf_verify(nf90_enddef(ncid), nc_fname)
    call nf_verify(nf90_put_var(ncid, var_id, p_mem, start=(/1, 1/), &
                                count=(/dis%ncol, dis%nrow/)), &
                   nc_fname)
  end subroutine nc_export_dbl2d

  !> @brief netcdf export 3D double
  !<
  subroutine nc_export_dbl3d(ncid, dim_ids, var_ids, dis, p_mem, nc_varname, &
                             pkgname, tagname, shapestr, longname, nc_tag, &
                             gridmap_name, latlon, deflate, shuffle, chunk_z, &
                             chunk_y, chunk_x, nc_fname)
    use InputOutputModule, only: lowcase
    integer(I4B), intent(in) :: ncid
    type(StructuredNCDimIdType), intent(inout) :: dim_ids
    type(StructuredNCVarIdType), intent(inout) :: var_ids
    type(DisType), pointer, intent(in) :: dis
    real(DP), dimension(:, :, :), pointer, contiguous, intent(in) :: p_mem
    character(len=*), intent(in) :: nc_varname
    character(len=*), intent(in) :: pkgname
    character(len=*), intent(in) :: tagname
    character(len=*), intent(in) :: shapestr
    character(len=*), intent(in) :: longname
    character(len=*), intent(in) :: nc_tag
    character(len=*), intent(in) :: gridmap_name
    logical(LGP), intent(in) :: latlon
    integer(I4B), intent(in) :: deflate
    integer(I4B), intent(in) :: shuffle
    integer(I4B), intent(in) :: chunk_z
    integer(I4B), intent(in) :: chunk_y
    integer(I4B), intent(in) :: chunk_x
    character(len=*), intent(in) :: nc_fname
    ! -- local
    integer(I4B) :: var_id
    !
    ! -- reenter define mode and create variable
    call nf_verify(nf90_redef(ncid), nc_fname)
    call nf_verify(nf90_def_var(ncid, nc_varname, NF90_DOUBLE, &
                                (/dim_ids%x, dim_ids%y, dim_ids%z/), var_id), &
                   nc_fname)
    !
    ! -- apply chunking parameters
    if (chunk_z > 0 .and. chunk_y > 0 .and. chunk_x > 0) then
      call nf_verify(nf90_def_var_chunking(ncid, var_id, NF90_CHUNKED, &
                                           (/chunk_x, chunk_y, chunk_z/)), &
                     nc_fname)
    end if
    ! -- deflate and shuffle
    if (deflate >= 0) then
      call nf_verify(nf90_def_var_deflate(ncid, var_id, shuffle=shuffle, &
                                          deflate=1, deflate_level=deflate), &
                     nc_fname)
    end if
    !
    call nf_verify(nf90_put_att(ncid, var_id, '_FillValue', &
                                (/NF90_FILL_DOUBLE/)), nc_fname)
    call nf_verify(nf90_put_att(ncid, var_id, 'long_name', &
                                longname), nc_fname)
    if (gridmap_name /= '') then
      if (latlon) then
        call nf_verify(nf90_put_att(ncid, var_id, 'coordinates', 'lon lat'), &
                       nc_fname)
      else
        call nf_verify(nf90_put_att(ncid, var_id, 'coordinates', 'x y'), &
                       nc_fname)
      end if
      call nf_verify(nf90_put_att(ncid, var_id, 'grid_mapping', gridmap_name), &
                     nc_fname)
    end if
    !
    if (nc_tag /= '') then
      call nf_verify(nf90_put_att(ncid, var_id, 'modflow6_input', &
                                  nc_tag), nc_fname)
    end if
    !
    ! -- exit define mode and write data
    call nf_verify(nf90_enddef(ncid), nc_fname)
    call nf_verify(nf90_put_var(ncid, var_id, p_mem, start=(/1, 1, 1/), &
                                count=(/dis%ncol, dis%nrow, dis%nlay/)), &
                   nc_fname)
  end subroutine nc_export_dbl3d

end module DisNCStructuredModule
