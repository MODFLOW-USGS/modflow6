!> @brief This module contains the MeshDisvModelModule
!!
!! This module defines UGRID layered mesh compliant netcdf
!! export type for DISV models. It is dependent on netcdf
!! libraries.
!!
!<
module MeshDisvModelModule

  use KindModule, only: DP, I4B, LGP
  use ConstantsModule, only: LINELENGTH, LENBIGLINE, LENCOMPONENTNAME, &
                             LENMEMPATH
  use MemoryManagerModule, only: mem_setptr
  use InputDefinitionModule, only: InputParamDefinitionType
  use CharacterStringModule, only: CharacterStringType
  use MeshModelModule, only: nf_verify, Mesh2dModelType, &
                             MeshNCDimIdType, MeshNCVarIdType
  use DisvModule, only: DisvType
  use netcdf

  implicit none
  private
  public :: Mesh2dDisvExportType

  ! -- UGRID layered mesh (ULM) DISV
  type, extends(Mesh2dModelType) :: Mesh2dDisvExportType
    class(DisvType), pointer :: disv => null() !discretization object
  contains
    procedure :: init => disv_export_init
    procedure :: destroy => disv_export_destroy
    procedure :: df
    procedure :: step
    procedure :: export_input_array
    procedure :: define_dim
    procedure :: add_mesh_data
  end type Mesh2dDisvExportType

contains

  !> @brief netcdf export dis model init
  !<
  subroutine disv_export_init(this, modelname, modeltype, modelfname, disenum, &
                              nctype, iout)
    use ArrayHandlersModule, only: expandarray
    class(Mesh2dDisvExportType), intent(inout) :: this
    character(len=*), intent(in) :: modelname
    character(len=*), intent(in) :: modeltype
    character(len=*), intent(in) :: modelfname
    integer(I4B), intent(in) :: disenum
    integer(I4B), intent(in) :: nctype
    integer(I4B), intent(in) :: iout
    !
    ! -- initialize base class
    call this%NCModelExportType%init(modelname, modeltype, modelfname, disenum, &
                                     nctype, iout)
    !
    ! allocate var_id arrays
    allocate (this%var_ids%dependent(this%disv%nlay))
    !
    ! -- set nlay
    this%nlay = this%disv%nlay
    !
    ! -- create the netcdf file
    call nf_verify(nf90_create(this%nc_filename, &
                               IOR(NF90_CLOBBER, NF90_NETCDF4), this%ncid), &
                   this%ncid, this%iout)
  end subroutine disv_export_init

  subroutine disv_export_destroy(this)
    use SimVariablesModule, only: idm_context
    use MemoryManagerExtModule, only: memorylist_remove
    class(Mesh2dDisvExportType), intent(inout) :: this
    call nf_verify(nf90_close(this%ncid), this%ncid, this%iout)
    !
    ! -- Deallocate idm memory
    if (this%ncf_mempath /= '') then
      call memorylist_remove(this%modelname, 'NCF', idm_context)
    end if
  end subroutine disv_export_destroy

  subroutine df(this)
    class(Mesh2dDisvExportType), intent(inout) :: this
    ! -- put root group file scope attributes
    call this%add_global_att()
    ! -- define root group dimensions and coordinate variables
    call this%define_dim()
    ! -- define mesh variables
    call this%create_mesh()
    ! -- define the dependent variable
    call this%define_dependent()
    ! -- exit define mode
    call nf_verify(nf90_enddef(this%ncid), this%ncid, this%iout)
    ! -- set mesh variables
    call this%add_mesh_data()
    ! -- define and set package input griddata
    call this%add_pkg_data()
    ! -- define and set gridmap variable
    call this%define_gridmap()
    ! -- synchronize file
    call nf_verify(nf90_sync(this%ncid), this%ncid, this%iout)
  end subroutine df

  subroutine step(this)
    use ConstantsModule, only: DHNOFLO
    use TdisModule, only: totim
    class(Mesh2dDisvExportType), intent(inout) :: this
    real(DP), dimension(:), pointer, contiguous :: dbl1d
    integer(I4B) :: n, k
    integer(I4B), dimension(2) :: dis_shape
    real(DP), dimension(:, :), pointer, contiguous :: dbl2d
    !
    ! -- increment step
    this%stepcnt = this%stepcnt + 1
    !
    dis_shape(1) = this%disv%ncpl
    dis_shape(2) = this%disv%nlay
    allocate (dbl2d(dis_shape(1), dis_shape(2)))
    !
    ! -- add data to dependent variable
    if (size(this%disv%nodeuser) < &
        size(this%disv%nodereduced)) then
      !
      ! -- allocate nodereduced size 1d array
      allocate (dbl1d(size(this%disv%nodereduced)))
      !
      ! -- initialize DHNOFLO for non-active cells
      dbl1d = DHNOFLO
      !
      ! -- update active cells
      do n = 1, size(this%disv%nodereduced)
        if (this%disv%nodereduced(n) > 0) then
          dbl1d(n) = this%x(this%disv%nodereduced(n))
        end if
      end do
      !
      dbl2d = reshape(dbl1d, dis_shape)
      !
      deallocate (dbl1d)
    else
      !
      dbl2d = reshape(this%x, dis_shape)
      !
    end if
    !
    do k = 1, this%disv%nlay
      ! -- extend array with step data
      call nf_verify(nf90_put_var(this%ncid, &
                                  this%var_ids%dependent(k), dbl2d(:, k), &
                                  start=(/1, this%stepcnt/), &
                                  count=(/this%disv%ncpl, 1/)), &
                     this%ncid, this%iout)
    end do
    !
    ! -- write to time coordinate variable
    call nf_verify(nf90_put_var(this%ncid, this%var_ids%time, &
                                totim, start=(/this%stepcnt/)), &
                   this%ncid, this%iout)
    !
    ! -- update file
    call nf_verify(nf90_sync(this%ncid), this%ncid, this%iout)
    !
    ! -- cleanup
    deallocate (dbl2d)
  end subroutine step

  subroutine export_input_array(this, pkgname, mempath, idt)
    use InputOutputModule, only: lowcase
    class(Mesh2dDisvExportType), intent(inout) :: this
    character(len=*), intent(in) :: pkgname
    character(len=*), intent(in) :: mempath
    type(InputParamDefinitionType), pointer, intent(in) :: idt
    integer(I4B), dimension(:), pointer, contiguous :: int1d
    integer(I4B), dimension(:, :), pointer, contiguous :: int2d
    real(DP), dimension(:), pointer, contiguous :: dbl1d
    real(DP), dimension(:, :), pointer, contiguous :: dbl2d
    character(len=LINELENGTH) :: pname, vname, nc_varname, gridmap
    !
    ! -- set package name
    pname = pkgname
    call lowcase(pname)
    ! -- set variable name
    vname = idt%tagname
    call lowcase(vname)
    ! -- set variable name written to file
    nc_varname = trim(pname)//'_'//trim(vname)
    !
    if (this%ogc_wkt /= '') then
      ! -- set gridmap variable name
      gridmap = this%gridmap_name
    else
      gridmap = ''
    end if
    !
    select case (idt%datatype)
    case ('INTEGER1D')
      call mem_setptr(int1d, idt%mf6varname, mempath)
      call nc_export_int1d(this%ncid, this%dim_ids, this%var_ids, this%disv, &
                           int1d, nc_varname, pkgname, idt%tagname, gridmap, &
                           idt%shape, idt%longname, this%iout)
    case ('INTEGER2D')
      call mem_setptr(int2d, idt%mf6varname, mempath)
      call nc_export_int2d(this%ncid, this%dim_ids, this%var_ids, this%disv, &
                           int2d, nc_varname, pkgname, idt%tagname, gridmap, &
                           idt%shape, idt%longname, this%iout)
    case ('DOUBLE1D')
      call mem_setptr(dbl1d, idt%mf6varname, mempath)
      call nc_export_dbl1d(this%ncid, this%dim_ids, this%var_ids, this%disv, &
                           dbl1d, nc_varname, pkgname, idt%tagname, gridmap, &
                           idt%shape, idt%longname, this%iout)
    case ('DOUBLE2D')
      call mem_setptr(dbl2d, idt%mf6varname, mempath)
      call nc_export_dbl2d(this%ncid, this%dim_ids, this%var_ids, this%disv, &
                           dbl2d, nc_varname, pkgname, idt%tagname, gridmap, &
                           idt%shape, idt%longname, this%iout)
    case default
      ! -- no-op, no other datatypes exported
    end select
  end subroutine export_input_array

  subroutine define_dim(this)
    class(Mesh2dDisvExportType), intent(inout) :: this
    integer(I4B), dimension(:), contiguous, pointer :: ncvert
    integer(I4B) :: ncpl_dim
    !
    ! -- set pointers to input context
    call mem_setptr(ncvert, 'NCVERT', this%dis_mempath)
    !
    ! -- time
    call nf_verify(nf90_def_dim(this%ncid, 'time', this%totnstp, &
                                this%dim_ids%time), this%ncid, this%iout)
    call nf_verify(nf90_def_var(this%ncid, 'time', NF90_DOUBLE, &
                                this%dim_ids%time, this%var_ids%time), &
                   this%ncid, this%iout)
    call nf_verify(nf90_put_att(this%ncid, this%var_ids%time, 'calendar', &
                                'standard'), this%ncid, this%iout)
    call nf_verify(nf90_put_att(this%ncid, this%var_ids%time, 'units', &
                                this%datetime), this%ncid, this%iout)
    call nf_verify(nf90_put_att(this%ncid, this%var_ids%time, 'axis', 'T'), &
                   this%ncid, this%iout)
    call nf_verify(nf90_put_att(this%ncid, this%var_ids%time, 'standard_name', &
                                'time'), this%ncid, this%iout)
    call nf_verify(nf90_put_att(this%ncid, this%var_ids%time, 'long_name', &
                                'time'), this%ncid, this%iout)
    !
    ! -- mesh
    call nf_verify(nf90_def_dim(this%ncid, 'nmesh_node', this%disv%nvert, &
                                this%dim_ids%nmesh_node), this%ncid, this%iout)
    call nf_verify(nf90_def_dim(this%ncid, 'nmesh_face', this%disv%ncpl, &
                                this%dim_ids%nmesh_face), this%ncid, this%iout)
    call nf_verify(nf90_def_dim(this%ncid, 'max_nmesh_face_nodes', &
                                maxval(ncvert), &
                                this%dim_ids%max_nmesh_face_nodes), &
                   this%ncid, this%iout)
    !
    ! -- ncpl, nlay
    call nf_verify(nf90_def_dim(this%ncid, 'nlay', this%disv%nlay, &
                                this%dim_ids%nlay), this%ncid, this%iout)
    call nf_verify(nf90_def_dim(this%ncid, 'ncpl', this%disv%ncpl, &
                                ncpl_dim), this%ncid, this%iout)
  end subroutine define_dim

  subroutine add_mesh_data(this)
    class(Mesh2dDisvExportType), intent(inout) :: this
    integer(I4B), dimension(:), contiguous, pointer :: icell2d => null()
    integer(I4B), dimension(:), contiguous, pointer :: ncvert => null()
    integer(I4B), dimension(:), contiguous, pointer :: icvert => null()
    real(DP), dimension(:), contiguous, pointer :: cell_x => null()
    real(DP), dimension(:), contiguous, pointer :: cell_y => null()
    real(DP), dimension(:), contiguous, pointer :: vert_x => null()
    real(DP), dimension(:), contiguous, pointer :: vert_y => null()
    integer(I4B) :: n, m, idx, cnt, iv, maxvert
    integer(I4B), dimension(:), allocatable :: verts
    real(DP), dimension(:), allocatable :: bnds
    integer(I4B) :: istop
    !
    ! -- set pointers to input context
    ! -- TODO: should the data come from the disv object
    ! -- TODO: does this data ignore xorigin/yorigin?
    call mem_setptr(icell2d, 'ICELL2D', this%dis_mempath)
    call mem_setptr(ncvert, 'NCVERT', this%dis_mempath)
    call mem_setptr(icvert, 'ICVERT', this%dis_mempath)
    call mem_setptr(cell_x, 'XC', this%dis_mempath)
    call mem_setptr(cell_y, 'YC', this%dis_mempath)
    call mem_setptr(vert_x, 'XV', this%dis_mempath)
    call mem_setptr(vert_y, 'YV', this%dis_mempath)
    !
    ! -- initialize max vertices required to define cell
    maxvert = maxval(ncvert)
    !
    ! -- set mesh container variable value to 1
    call nf_verify(nf90_put_var(this%ncid, this%var_ids%mesh, 1), &
                   this%ncid, this%iout)
    !
    ! -- allocate temporary arrays
    allocate (verts(maxvert))
    allocate (bnds(maxvert))
    !
    ! -- write node_x and node_y arrays to netcdf file
    call nf_verify(nf90_put_var(this%ncid, this%var_ids%mesh_node_x, &
                                vert_x + this%disv%xorigin), this%ncid, this%iout)
    call nf_verify(nf90_put_var(this%ncid, this%var_ids%mesh_node_y, &
                                vert_y + this%disv%yorigin), this%ncid, this%iout)
    !
    ! -- write face_x and face_y arrays to netcdf file
    call nf_verify(nf90_put_var(this%ncid, this%var_ids%mesh_face_x, &
                                cell_x + this%disv%xorigin), this%ncid, this%iout)
    call nf_verify(nf90_put_var(this%ncid, this%var_ids%mesh_face_y, &
                                cell_y + this%disv%yorigin), this%ncid, this%iout)
    !
    ! -- set face nodes array
    cnt = 0
    do n = 1, size(ncvert)
      verts = NF90_FILL_INT
      idx = cnt + ncvert(n)
      iv = 0
      istop = cnt + 1
      do m = idx, istop, -1
        cnt = cnt + 1
        iv = iv + 1
        verts(iv) = icvert(m)
      end do
      !
      ! -- write face nodes array to netcdf file
      call nf_verify(nf90_put_var(this%ncid, this%var_ids%mesh_face_nodes, &
                                  verts, start=(/1, n/), &
                                  count=(/maxvert, 1/)), &
                     this%ncid, this%iout)
      !
      ! -- set face y bounds array
      bnds = NF90_FILL_DOUBLE
      do m = 1, size(bnds)
        if (verts(m) /= NF90_FILL_INT) then
          bnds(m) = vert_y(verts(m))
        end if
        ! -- write face y bounds array to netcdf file
        call nf_verify(nf90_put_var(this%ncid, this%var_ids%mesh_face_ybnds, &
                                    bnds, start=(/1, n/), &
                                    count=(/maxvert, 1/)), &
                       this%ncid, this%iout)
      end do
      !
      ! -- set face x bounds array
      bnds = NF90_FILL_DOUBLE
      do m = 1, size(bnds)
        if (verts(m) /= NF90_FILL_INT) then
          bnds(m) = vert_x(verts(m))
        end if
        ! -- write face x bounds array to netcdf file
        call nf_verify(nf90_put_var(this%ncid, this%var_ids%mesh_face_xbnds, &
                                    bnds, start=(/1, n/), &
                                    count=(/maxvert, 1/)), &
                       this%ncid, this%iout)
      end do
    end do
    !
    ! -- cleanup
    deallocate (bnds)
    deallocate (verts)
  end subroutine add_mesh_data

  subroutine nc_export_int1d(ncid, dim_ids, var_ids, dis, p_mem, nc_varname, &
                             pkgname, tagname, gridmap_name, shapestr, longname, &
                             iout)
    use InputOutputModule, only: lowcase
    integer(I4B), intent(in) :: ncid
    type(MeshNCDimIdType), intent(inout) :: dim_ids
    type(MeshNCVarIdType), intent(inout) :: var_ids
    type(DisvType), pointer, intent(in) :: dis
    integer(I4B), dimension(:), pointer, contiguous, intent(in) :: p_mem
    character(len=*), intent(in) :: nc_varname
    character(len=*), intent(in) :: pkgname
    character(len=*), intent(in) :: tagname
    character(len=*), intent(in) :: gridmap_name
    character(len=*), intent(in) :: shapestr
    character(len=*), intent(in) :: longname
    integer(I4B), intent(in) :: iout
    ! -- local
    integer(I4B), dimension(2) :: dis_shape
    integer(I4B), dimension(:, :), pointer, contiguous :: int2d
    integer(I4B) :: axis_sz, k
    integer(I4B), dimension(:), allocatable :: var_id
    character(len=LINELENGTH) :: longname_l, varname_l
    !
    if (shapestr == 'NCPL') then
      ! -- set long_name attribute
      if (longname /= '') then
        longname_l = trim(longname)
      else
        longname_l = trim(pkgname)//' '//trim(tagname)
      end if
      !
      allocate (var_id(1))
      axis_sz = dim_ids%nmesh_face
      !
      ! -- reenter define mode and create variable
      call nf_verify(nf90_redef(ncid), ncid, iout)
      call nf_verify(nf90_def_var(ncid, nc_varname, NF90_INT, &
                                  (/axis_sz/), var_id(1)), &
                     ncid, iout)
      call nf_verify(nf90_put_att(ncid, var_id(1), '_FillValue', &
                                  (/NF90_FILL_INT/)), ncid, iout)
      call nf_verify(nf90_put_att(ncid, var_id(1), 'long_name', &
                                  longname_l), ncid, iout)
      if (gridmap_name /= '') then
        call nf_verify(nf90_put_att(ncid, var_id(1), 'coordinates', &
                                    'mesh_face_x mesh_face_y'), ncid, iout)
        call nf_verify(nf90_put_att(ncid, var_id(1), 'grid_mapping', &
                                    gridmap_name), ncid, iout)
      end if
      !
      ! -- exit define mode and write data
      call nf_verify(nf90_enddef(ncid), ncid, iout)
      call nf_verify(nf90_put_var(ncid, var_id(1), p_mem), &
                     ncid, iout)

    else
      allocate (var_id(dis%nlay))
      !
      ! -- reenter define mode and create variable
      call nf_verify(nf90_redef(ncid), ncid, iout)
      do k = 1, dis%nlay
        write (varname_l, '(a,i0)') trim(nc_varname)//'_l', k
        if (longname /= '') then
          write (longname_l, '(a,i0,a)') trim(longname)//' (layer ', k, ')'
        else
          write (longname_l, '(a,i0,a)') trim(pkgname)//' '//trim(tagname)// &
            ' (layer ', k, ')'
        end if
        call nf_verify(nf90_def_var(ncid, varname_l, NF90_INT, &
                                    (/dim_ids%nmesh_face/), var_id(k)), &
                       ncid, iout)
        call nf_verify(nf90_put_att(ncid, var_id(k), '_FillValue', &
                                    (/NF90_FILL_INT/)), ncid, iout)
        call nf_verify(nf90_put_att(ncid, var_id(k), 'long_name', &
                                    longname_l), ncid, iout)
        if (gridmap_name /= '') then
          call nf_verify(nf90_put_att(ncid, var_id(k), 'coordinates', &
                                      'mesh_face_x mesh_face_y'), ncid, iout)
          call nf_verify(nf90_put_att(ncid, var_id(k), 'grid_mapping', &
                                      gridmap_name), ncid, iout)
        end if
      end do
      !
      ! -- allocate temporary 3d and reshape input
      dis_shape(1) = dis%ncpl
      dis_shape(2) = dis%nlay
      allocate (int2d(dis_shape(1), dis_shape(2)))
      int2d = reshape(p_mem, dis_shape)
      !
      ! -- exit define mode and write data
      call nf_verify(nf90_enddef(ncid), ncid, iout)
      do k = 1, dis%nlay
        call nf_verify(nf90_put_var(ncid, var_id(k), int2d(:, k)), ncid, iout)
      end do
      !
      ! -- cleanup
      deallocate (int2d)
      deallocate (var_id)
    end if
  end subroutine nc_export_int1d

  !> @brief Create export file int2d
  !<
  subroutine nc_export_int2d(ncid, dim_ids, var_ids, disv, p_mem, nc_varname, &
                             pkgname, tagname, gridmap_name, shapestr, longname, &
                             iout)
    integer(I4B), intent(in) :: ncid
    type(MeshNCDimIdType), intent(inout) :: dim_ids
    type(MeshNCVarIdType), intent(inout) :: var_ids
    type(DisvType), pointer, intent(in) :: disv
    integer(I4B), dimension(:, :), pointer, contiguous, intent(in) :: p_mem
    character(len=*), intent(in) :: nc_varname
    character(len=*), intent(in) :: pkgname
    character(len=*), intent(in) :: tagname
    character(len=*), intent(in) :: gridmap_name
    character(len=*), intent(in) :: shapestr
    character(len=*), intent(in) :: longname
    integer(I4B), intent(in) :: iout
    ! -- local
    integer(I4B), dimension(:), allocatable :: var_id
    character(len=LINELENGTH) :: longname_l, varname_l
    integer(I4B) :: k
    !
    allocate (var_id(disv%nlay))
    !
    ! -- reenter define mode and create variable
    call nf_verify(nf90_redef(ncid), ncid, iout)
    do k = 1, disv%nlay
      write (varname_l, '(a,i0)') trim(nc_varname)//'_l', k
      if (longname /= '') then
        write (longname_l, '(a,i0,a)') trim(longname)//' (layer ', k, ')'
      else
        write (longname_l, '(a,i0,a)') trim(pkgname)//' '//trim(tagname)// &
          ' (layer ', k, ')'
      end if
      call nf_verify(nf90_def_var(ncid, varname_l, NF90_INT, &
                                  (/dim_ids%nmesh_face/), var_id(k)), &
                     ncid, iout)
      call nf_verify(nf90_put_att(ncid, var_id(k), '_FillValue', &
                                  (/NF90_FILL_INT/)), ncid, iout)
      call nf_verify(nf90_put_att(ncid, var_id(k), 'long_name', &
                                  longname_l), ncid, iout)
      if (gridmap_name /= '') then
        call nf_verify(nf90_put_att(ncid, var_id(k), 'coordinates', &
                                    'mesh_face_x mesh_face_y'), ncid, iout)
        call nf_verify(nf90_put_att(ncid, var_id(k), 'grid_mapping', &
                                    gridmap_name), ncid, iout)
      end if
    end do
    !
    ! -- exit define mode and write data
    call nf_verify(nf90_enddef(ncid), ncid, iout)
    do k = 1, disv%nlay
      call nf_verify(nf90_put_var(ncid, var_id(k), p_mem(:, k)), ncid, iout)
    end do
    !
    deallocate (var_id)
  end subroutine nc_export_int2d

  subroutine nc_export_dbl1d(ncid, dim_ids, var_ids, dis, p_mem, nc_varname, &
                             pkgname, tagname, gridmap_name, shapestr, longname, &
                             iout)
    use InputOutputModule, only: lowcase
    integer(I4B), intent(in) :: ncid
    type(MeshNCDimIdType), intent(inout) :: dim_ids
    type(MeshNCVarIdType), intent(inout) :: var_ids
    type(DisvType), pointer, intent(in) :: dis
    real(DP), dimension(:), pointer, contiguous, intent(in) :: p_mem
    character(len=*), intent(in) :: nc_varname
    character(len=*), intent(in) :: pkgname
    character(len=*), intent(in) :: tagname
    character(len=*), intent(in) :: gridmap_name
    character(len=*), intent(in) :: shapestr
    character(len=*), intent(in) :: longname
    integer(I4B), intent(in) :: iout
    ! -- local
    integer(I4B), dimension(2) :: dis_shape
    real(DP), dimension(:, :), pointer, contiguous :: dbl2d
    integer(I4B) :: axis_sz, k
    integer(I4B), dimension(:), allocatable :: var_id
    character(len=LINELENGTH) :: longname_l, varname_l
    !
    if (shapestr == 'NCPL') then
      ! -- set long_name attribute
      if (longname /= '') then
        longname_l = trim(longname)
      else
        longname_l = trim(pkgname)//' '//trim(tagname)
      end if
      !
      allocate (var_id(1))
      axis_sz = dim_ids%nmesh_face
      !
      ! -- reenter define mode and create variable
      call nf_verify(nf90_redef(ncid), ncid, iout)
      call nf_verify(nf90_def_var(ncid, nc_varname, NF90_DOUBLE, &
                                  (/axis_sz/), var_id(1)), &
                     ncid, iout)
      call nf_verify(nf90_put_att(ncid, var_id(1), '_FillValue', &
                                  (/NF90_FILL_DOUBLE/)), ncid, iout)
      call nf_verify(nf90_put_att(ncid, var_id(1), 'long_name', &
                                  longname_l), ncid, iout)
      if (gridmap_name /= '') then
        call nf_verify(nf90_put_att(ncid, var_id(1), 'coordinates', &
                                    'mesh_face_x mesh_face_y'), ncid, iout)
        call nf_verify(nf90_put_att(ncid, var_id(1), 'grid_mapping', &
                                    gridmap_name), ncid, iout)
      end if
      !
      ! -- exit define mode and write data
      call nf_verify(nf90_enddef(ncid), ncid, iout)
      call nf_verify(nf90_put_var(ncid, var_id(1), p_mem), &
                     ncid, iout)

    else
      allocate (var_id(dis%nlay))
      !
      ! -- reenter define mode and create variable
      call nf_verify(nf90_redef(ncid), ncid, iout)
      do k = 1, dis%nlay
        write (varname_l, '(a,i0)') trim(nc_varname)//'_l', k
        if (longname /= '') then
          write (longname_l, '(a,i0,a)') trim(longname)//' (layer ', k, ')'
        else
          write (longname_l, '(a,i0,a)') trim(pkgname)//' '//trim(tagname)// &
            ' (layer ', k, ')'
        end if
        call nf_verify(nf90_def_var(ncid, varname_l, NF90_DOUBLE, &
                                    (/dim_ids%nmesh_face/), var_id(k)), &
                       ncid, iout)
        call nf_verify(nf90_put_att(ncid, var_id(k), '_FillValue', &
                                    (/NF90_FILL_DOUBLE/)), ncid, iout)
        call nf_verify(nf90_put_att(ncid, var_id(k), 'long_name', &
                                    longname_l), ncid, iout)
        if (gridmap_name /= '') then
          call nf_verify(nf90_put_att(ncid, var_id(k), 'coordinates', &
                                      'mesh_face_x mesh_face_y'), ncid, iout)
          call nf_verify(nf90_put_att(ncid, var_id(k), 'grid_mapping', &
                                      gridmap_name), ncid, iout)
        end if
      end do
      !
      ! -- allocate temporary 3d and reshape input
      dis_shape(1) = dis%ncpl
      dis_shape(2) = dis%nlay
      allocate (dbl2d(dis_shape(1), dis_shape(2)))
      dbl2d = reshape(p_mem, dis_shape)
      !
      ! -- exit define mode and write data
      call nf_verify(nf90_enddef(ncid), ncid, iout)
      do k = 1, dis%nlay
        call nf_verify(nf90_put_var(ncid, var_id(k), dbl2d(:, k)), ncid, iout)
      end do
      !
      ! -- cleanup
      deallocate (dbl2d)
      deallocate (var_id)
    end if
  end subroutine nc_export_dbl1d

  !> @brief Create export file dbl2d
  !<
  subroutine nc_export_dbl2d(ncid, dim_ids, var_ids, disv, p_mem, nc_varname, &
                             pkgname, tagname, gridmap_name, shapestr, longname, &
                             iout)
    integer(I4B), intent(in) :: ncid
    type(MeshNCDimIdType), intent(inout) :: dim_ids
    type(MeshNCVarIdType), intent(inout) :: var_ids
    type(DisvType), pointer, intent(in) :: disv
    real(DP), dimension(:, :), pointer, contiguous, intent(in) :: p_mem
    character(len=*), intent(in) :: nc_varname
    character(len=*), intent(in) :: pkgname
    character(len=*), intent(in) :: tagname
    character(len=*), intent(in) :: gridmap_name
    character(len=*), intent(in) :: shapestr
    character(len=*), intent(in) :: longname
    integer(I4B), intent(in) :: iout
    ! -- local
    integer(I4B), dimension(:), allocatable :: var_id
    character(len=LINELENGTH) :: longname_l, varname_l
    integer(I4B) :: k
    !
    allocate (var_id(disv%nlay))
    !
    ! -- reenter define mode and create variable
    call nf_verify(nf90_redef(ncid), ncid, iout)
    do k = 1, disv%nlay
      write (varname_l, '(a,i0)') trim(nc_varname)//'_l', k
      if (longname /= '') then
        write (longname_l, '(a,i0,a)') trim(longname)//' (layer ', k, ')'
      else
        write (longname_l, '(a,i0)') trim(pkgname)//' '//trim(tagname)// &
          ' (layer ', k, ')'
      end if
      call nf_verify(nf90_def_var(ncid, varname_l, NF90_DOUBLE, &
                                  (/dim_ids%nmesh_face/), var_id(k)), &
                     ncid, iout)
      call nf_verify(nf90_put_att(ncid, var_id(k), '_FillValue', &
                                  (/NF90_FILL_DOUBLE/)), ncid, iout)
      call nf_verify(nf90_put_att(ncid, var_id(k), 'long_name', &
                                  longname_l), ncid, iout)
      if (gridmap_name /= '') then
        call nf_verify(nf90_put_att(ncid, var_id(k), 'coordinates', &
                                    'mesh_face_x mesh_face_y'), ncid, iout)
        call nf_verify(nf90_put_att(ncid, var_id(k), 'grid_mapping', &
                                    gridmap_name), ncid, iout)
      end if
    end do
    !
    ! -- exit define mode and write data
    call nf_verify(nf90_enddef(ncid), ncid, iout)
    do k = 1, disv%nlay
      call nf_verify(nf90_put_var(ncid, var_id(k), p_mem(:, k)), ncid, iout)
    end do
    !
    deallocate (var_id)
  end subroutine nc_export_dbl2d

end module MeshDisvModelModule
