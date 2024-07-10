!> @brief This module contains the MeshDisModelModule
!!
!! This module defines UGRID layered mesh compliant netcdf
!! export type for DIS models. It is dependent on netcdf
!! libraries.
!!
!<
module MeshDisModelModule

  use KindModule, only: DP, I4B, LGP
  use ConstantsModule, only: LINELENGTH, LENBIGLINE, LENCOMPONENTNAME, &
                             LENMEMPATH
  use MemoryManagerModule, only: mem_setptr
  use InputDefinitionModule, only: InputParamDefinitionType
  use CharacterStringModule, only: CharacterStringType
  use MeshModelModule, only: Mesh2dModelType, MeshNCDimIdType, MeshNCVarIdType
  use DisModule, only: DisType
  use NetCDFCommonModule, only: nf_verify
  use netcdf

  implicit none
  private
  public :: Mesh2dDisExportType

  ! -- UGRID layered mesh (ULM) DIS
  type, extends(Mesh2dModelType) :: Mesh2dDisExportType
    class(DisType), pointer :: dis => null() !< pointer to model dis package
    integer(I4B) :: x_dim !< ncol dimension id
    integer(I4B) :: y_dim !< nrow dimension id
  contains
    procedure :: init => dis_export_init
    procedure :: destroy => dis_export_destroy
    procedure :: df
    procedure :: step
    procedure :: export_input_array
    procedure :: define_dim
    procedure :: add_mesh_data
  end type Mesh2dDisExportType

contains

  !> @brief netcdf export dis init
  !<
  subroutine dis_export_init(this, modelname, modeltype, modelfname, disenum, &
                             nctype, iout)
    use ArrayHandlersModule, only: expandarray
    class(Mesh2dDisExportType), intent(inout) :: this
    character(len=*), intent(in) :: modelname
    character(len=*), intent(in) :: modeltype
    character(len=*), intent(in) :: modelfname
    integer(I4B), intent(in) :: disenum
    integer(I4B), intent(in) :: nctype
    integer(I4B), intent(in) :: iout
    !
    ! -- set nlay
    this%nlay = this%dis%nlay
    !
    ! allocate var_id arrays
    allocate (this%var_ids%dependent(this%nlay))
    !
    ! -- initialize base class
    call this%mesh_init(modelname, modeltype, modelfname, disenum, nctype, iout)
  end subroutine dis_export_init

  !> @brief netcdf export dis destroy
  !<
  subroutine dis_export_destroy(this)
    use SimVariablesModule, only: idm_context
    use MemoryManagerExtModule, only: memorystore_remove
    class(Mesh2dDisExportType), intent(inout) :: this
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
    class(Mesh2dDisExportType), intent(inout) :: this
    ! -- put root group file scope attributes
    call this%add_global_att()
    ! -- define root group dimensions and coordinate variables
    call this%define_dim()
    ! -- define mesh variables
    call this%create_mesh()
    if (isim_mode /= MVALIDATE) then
      ! -- define the dependent variable
      call this%define_dependent()
    end if
    ! -- exit define mode
    call nf_verify(nf90_enddef(this%ncid), this%nc_fname)
    ! -- create mesh
    call this%add_mesh_data()
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
    use ConstantsModule, only: DHNOFLO
    use TdisModule, only: totim
    class(Mesh2dDisExportType), intent(inout) :: this
    real(DP), dimension(:), pointer, contiguous :: dbl1d
    integer(I4B) :: n, k
    integer(I4B), dimension(2) :: dis_shape
    real(DP), dimension(:, :), pointer, contiguous :: dbl2d
    !
    ! -- increment step
    this%stepcnt = this%stepcnt + 1
    !
    dis_shape(1) = this%dis%ncol * this%dis%nrow
    dis_shape(2) = this%dis%nlay
    allocate (dbl2d(dis_shape(1), dis_shape(2)))
    !
    ! -- add data to dependent variable
    if (size(this%dis%nodeuser) < &
        size(this%dis%nodereduced)) then
      !
      ! -- allocate nodereduced size 1d array
      allocate (dbl1d(size(this%dis%nodereduced)))
      !
      ! -- initialize DHNOFLO for non-active cells
      dbl1d = DHNOFLO
      !
      ! -- update active cells
      do n = 1, size(this%dis%nodereduced)
        if (this%dis%nodereduced(n) > 0) then
          dbl1d(n) = this%x(this%dis%nodereduced(n))
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
    do k = 1, this%dis%nlay
      ! -- extend array with step data
      call nf_verify(nf90_put_var(this%ncid, &
                                  this%var_ids%dependent(k), dbl2d(:, k), &
                                  start=(/1, this%stepcnt/), &
                                  count=(/(this%dis%ncol * this%dis%nrow), 1/)), &
                     this%nc_fname)
    end do
    !
    ! -- write to time coordinate variable
    call nf_verify(nf90_put_var(this%ncid, this%var_ids%time, &
                                totim, start=(/this%stepcnt/)), &
                   this%nc_fname)
    !
    ! -- update file
    call nf_verify(nf90_sync(this%ncid), this%nc_fname)
    !
    ! --cleanup
    deallocate (dbl2d)
  end subroutine step

  !> @brief netcdf export an input array
  !<
  subroutine export_input_array(this, pkgtype, pkgname, mempath, idt)
    use InputOutputModule, only: lowcase
    use MemoryHelperModule, only: memPathSeparator
    class(Mesh2dDisExportType), intent(inout) :: this
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
      call nc_export_int1d(this%ncid, this%dim_ids, this%x_dim, this%y_dim, &
                           this%var_ids, this%dis, int1d, nc_varname, pkgname, &
                           idt%tagname, gridmap, idt%shape, idt%longname, &
                           input_attr, this%deflate, this%shuffle, &
                           this%chunk_face, this%nc_fname)
    case ('INTEGER2D')
      call mem_setptr(int2d, idt%mf6varname, mempath)
      call nc_export_int2d(this%ncid, this%dim_ids, this%var_ids, this%dis, &
                           int2d, nc_varname, pkgname, idt%tagname, gridmap, &
                           idt%shape, idt%longname, input_attr, this%deflate, &
                           this%shuffle, this%chunk_face, this%nc_fname)
    case ('INTEGER3D')
      call mem_setptr(int3d, idt%mf6varname, mempath)
      call nc_export_int3d(this%ncid, this%dim_ids, this%var_ids, this%dis, &
                           int3d, nc_varname, pkgname, idt%tagname, gridmap, &
                           idt%shape, idt%longname, input_attr, this%deflate, &
                           this%shuffle, this%chunk_face, this%nc_fname)
    case ('DOUBLE1D')
      call mem_setptr(dbl1d, idt%mf6varname, mempath)
      call nc_export_dbl1d(this%ncid, this%dim_ids, this%x_dim, this%y_dim, &
                           this%var_ids, this%dis, dbl1d, nc_varname, pkgname, &
                           idt%tagname, gridmap, idt%shape, idt%longname, &
                           input_attr, this%deflate, this%shuffle, &
                           this%chunk_face, this%nc_fname)
    case ('DOUBLE2D')
      call mem_setptr(dbl2d, idt%mf6varname, mempath)
      call nc_export_dbl2d(this%ncid, this%dim_ids, this%var_ids, this%dis, &
                           dbl2d, nc_varname, pkgname, idt%tagname, gridmap, &
                           idt%shape, idt%longname, input_attr, this%deflate, &
                           this%shuffle, this%chunk_face, this%nc_fname)
    case ('DOUBLE3D')
      call mem_setptr(dbl3d, idt%mf6varname, mempath)
      call nc_export_dbl3d(this%ncid, this%dim_ids, this%var_ids, this%dis, &
                           dbl3d, nc_varname, pkgname, idt%tagname, gridmap, &
                           idt%shape, idt%longname, input_attr, this%deflate, &
                           this%shuffle, this%chunk_face, this%nc_fname)
    case default
      ! -- no-op, no other datatypes exported
    end select
  end subroutine export_input_array

  !> @brief netcdf export define dimensions
  !<
  subroutine define_dim(this)
    use ConstantsModule, only: MVALIDATE
    use SimVariablesModule, only: isim_mode
    class(Mesh2dDisExportType), intent(inout) :: this
    !
    ! -- time
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
      call nf_verify(nf90_put_att(this%ncid, this%var_ids%time, 'standard_name', &
                                  'time'), this%nc_fname)
      call nf_verify(nf90_put_att(this%ncid, this%var_ids%time, 'long_name', &
                                  'time'), this%nc_fname)
    end if
    !
    ! -- mesh
    call nf_verify(nf90_def_dim(this%ncid, 'nmesh_node', &
                                ((this%dis%ncol + 1) * (this%dis%nrow + 1)), &
                                this%dim_ids%nmesh_node), this%nc_fname)
    call nf_verify(nf90_def_dim(this%ncid, 'nmesh_face', &
                                (this%dis%ncol * this%dis%nrow), &
                                this%dim_ids%nmesh_face), this%nc_fname)
    call nf_verify(nf90_def_dim(this%ncid, 'max_nmesh_face_nodes', 4, &
                                this%dim_ids%max_nmesh_face_nodes), &
                   this%nc_fname)
    !
    ! -- x, y, nlay
    call nf_verify(nf90_def_dim(this%ncid, 'nlay', this%dis%nlay, &
                                this%dim_ids%nlay), this%nc_fname)
    call nf_verify(nf90_def_dim(this%ncid, 'x', this%dis%ncol, &
                                this%x_dim), this%nc_fname)
    call nf_verify(nf90_def_dim(this%ncid, 'y', this%dis%nrow, &
                                this%y_dim), this%nc_fname)
  end subroutine define_dim

  !> @brief netcdf export add mesh information
  !<
  subroutine add_mesh_data(this)
    class(Mesh2dDisExportType), intent(inout) :: this
    integer(I4B) :: cnt, maxvert, m
    integer(I4B), dimension(:), allocatable :: verts
    real(DP), dimension(:), allocatable :: bnds
    integer(I4B) :: i, j
    real(DP) :: x, y
    real(DP), dimension(:), allocatable :: node_x, node_y
    real(DP), dimension(:), allocatable :: cell_x, cell_y
    !
    ! -- initialize max vertices required to define cell
    maxvert = 4
    !
    ! -- set mesh container variable value to 1
    call nf_verify(nf90_put_var(this%ncid, this%var_ids%mesh, 1), &
                   this%nc_fname)
    !
    ! -- allocate temporary arrays
    allocate (verts(maxvert))
    allocate (bnds(maxvert))
    allocate (node_x(((this%dis%ncol + 1) * (this%dis%nrow + 1))))
    allocate (node_y(((this%dis%ncol + 1) * (this%dis%nrow + 1))))
    allocate (cell_x((this%dis%ncol * this%dis%nrow)))
    allocate (cell_y((this%dis%ncol * this%dis%nrow)))
    !
    ! -- set node_x and node_y arrays
    cnt = 0
    node_x = NF90_FILL_DOUBLE
    node_y = NF90_FILL_DOUBLE
    y = this%dis%yorigin + sum(this%dis%delc)
    do j = this%dis%nrow, 0, -1
      x = this%dis%xorigin
      do i = this%dis%ncol, 0, -1
        cnt = cnt + 1
        node_x(cnt) = x
        node_y(cnt) = y
        if (i > 0) x = x + this%dis%delr(i)
      end do
      if (j > 0) y = y - this%dis%delc(j)
    end do
    !
    ! -- write node_x and node_y arrays to netcdf file
    call nf_verify(nf90_put_var(this%ncid, this%var_ids%mesh_node_x, node_x), &
                   this%nc_fname)
    call nf_verify(nf90_put_var(this%ncid, this%var_ids%mesh_node_y, node_y), &
                   this%nc_fname)
    !
    ! -- set cell_x and cell_y arrays
    cnt = 1
    cell_x = NF90_FILL_DOUBLE
    cell_y = NF90_FILL_DOUBLE
    do j = 1, this%dis%nrow
      x = this%dis%xorigin
      y = this%dis%celly(j) + this%dis%yorigin
      do i = 1, this%dis%ncol
        cell_x(cnt) = x
        cell_y(cnt) = y
        cnt = cnt + 1
        x = this%dis%cellx(i) + this%dis%xorigin
      end do
    end do
    !
    ! -- write face_x and face_y arrays to netcdf file
    call nf_verify(nf90_put_var(this%ncid, this%var_ids%mesh_face_x, cell_x), &
                   this%nc_fname)
    call nf_verify(nf90_put_var(this%ncid, this%var_ids%mesh_face_y, cell_y), &
                   this%nc_fname)
    !
    ! -- set face nodes array
    cnt = 0
    do i = 1, this%dis%nrow
      do j = 1, this%dis%ncol
        cnt = cnt + 1
        verts = NF90_FILL_INT
        verts(1) = cnt + this%dis%ncol + i
        verts(2) = cnt + this%dis%ncol + i + 1
        if (i > 1) then
          verts(3) = cnt + i
          verts(4) = cnt + i - 1
        else
          verts(3) = cnt + 1
          verts(4) = cnt
        end if
        !
        ! -- write face nodes array to netcdf file
        call nf_verify(nf90_put_var(this%ncid, this%var_ids%mesh_face_nodes, &
                                    verts, start=(/1, cnt/), &
                                    count=(/maxvert, 1/)), &
                       this%nc_fname)
        !
        ! -- set face y bounds array
        bnds = NF90_FILL_DOUBLE
        do m = 1, size(bnds)
          if (verts(m) /= NF90_FILL_INT) then
            bnds(m) = node_y(verts(m))
          end if
          ! -- write face y bounds array to netcdf file
          call nf_verify(nf90_put_var(this%ncid, this%var_ids%mesh_face_ybnds, &
                                      bnds, start=(/1, cnt/), &
                                      count=(/maxvert, 1/)), &
                         this%nc_fname)
        end do
        !
        ! -- set face x bounds array
        bnds = NF90_FILL_DOUBLE
        do m = 1, size(bnds)
          if (verts(m) /= NF90_FILL_INT) then
            bnds(m) = node_x(verts(m))
          end if
          ! -- write face x bounds array to netcdf file
          call nf_verify(nf90_put_var(this%ncid, this%var_ids%mesh_face_xbnds, &
                                      bnds, start=(/1, cnt/), &
                                      count=(/maxvert, 1/)), &
                         this%nc_fname)
        end do
      end do
    end do
    !
    ! -- cleanup
    deallocate (bnds)
    deallocate (verts)
    deallocate (node_x)
    deallocate (node_y)
    deallocate (cell_x)
    deallocate (cell_y)
  end subroutine add_mesh_data

  !> @brief netcdf export 1D integer
  !<
  subroutine nc_export_int1d(ncid, dim_ids, x_dim, y_dim, var_ids, dis, p_mem, &
                             nc_varname, pkgname, tagname, gridmap_name, &
                             shapestr, longname, nc_tag, deflate, shuffle, &
                             chunk_face, nc_fname)
    use InputOutputModule, only: lowcase
    integer(I4B), intent(in) :: ncid
    type(MeshNCDimIdType), intent(inout) :: dim_ids
    integer(I4B), intent(in) :: x_dim
    integer(I4B), intent(in) :: y_dim
    type(MeshNCVarIdType), intent(inout) :: var_ids
    type(DisType), pointer, intent(in) :: dis
    integer(I4B), dimension(:), pointer, contiguous, intent(in) :: p_mem
    character(len=*), intent(in) :: nc_varname
    character(len=*), intent(in) :: pkgname
    character(len=*), intent(in) :: tagname
    character(len=*), intent(in) :: gridmap_name
    character(len=*), intent(in) :: shapestr
    character(len=*), intent(in) :: longname
    character(len=*), intent(in) :: nc_tag
    integer(I4B), intent(in) :: deflate
    integer(I4B), intent(in) :: shuffle
    integer(I4B), intent(in) :: chunk_face
    character(len=*), intent(in) :: nc_fname
    ! -- local
    integer(I4B), dimension(3) :: dis_shape
    integer(I4B), dimension(1) :: layer_shape
    integer(I4B), dimension(:, :, :), pointer, contiguous :: int3d
    integer(I4B), dimension(:), pointer, contiguous :: int1d
    integer(I4B) :: axis_dim, k
    integer(I4B), dimension(:), allocatable :: var_id
    character(len=LINELENGTH) :: longname_l, varname_l
    !
    if (shapestr == 'NROW' .or. &
        shapestr == 'NCOL') then
      !
      if (shapestr == 'NROW') then
        axis_dim = y_dim
      else
        axis_dim = x_dim
      end if
      ! -- set long_name attribute
      if (longname /= '') then
        longname_l = trim(longname)
      else
        longname_l = trim(pkgname)//' '//trim(tagname)
      end if
      !
      allocate (var_id(1))
      !
      ! -- reenter define mode and create variable
      call nf_verify(nf90_redef(ncid), nc_fname)
      call nf_verify(nf90_def_var(ncid, nc_varname, NF90_INT, &
                                  (/axis_dim/), var_id(1)), &
                     nc_fname)
      !
      ! -- NROW/NCOL shapes use default chunking
      if (deflate >= 0) then
        call nf_verify(nf90_def_var_deflate(ncid, var_id(1), shuffle=shuffle, &
                                            deflate=1, deflate_level=deflate), &
                       nc_fname)
      end if
      !
      call nf_verify(nf90_put_att(ncid, var_id(1), '_FillValue', &
                                  (/NF90_FILL_INT/)), nc_fname)
      call nf_verify(nf90_put_att(ncid, var_id(1), 'long_name', &
                                  longname_l), nc_fname)
      !
      if (nc_tag /= '') then
        call nf_verify(nf90_put_att(ncid, var_id(1), 'modflow6_input', &
                                    nc_tag), nc_fname)
      end if
      !
      ! -- exit define mode and write data
      call nf_verify(nf90_enddef(ncid), nc_fname)
      call nf_verify(nf90_put_var(ncid, var_id(1), p_mem), &
                     nc_fname)

    else
      allocate (var_id(dis%nlay))
      !
      ! -- reenter define mode and create variable
      call nf_verify(nf90_redef(ncid), nc_fname)
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
                       nc_fname)
        !
        ! -- apply chunking parameters
        if (chunk_face > 0) then
          call nf_verify(nf90_def_var_chunking(ncid, var_id(k), NF90_CHUNKED, &
                                               (/chunk_face/)), nc_fname)
        end if
        ! -- deflate and shuffle
        if (deflate >= 0) then
          call nf_verify(nf90_def_var_deflate(ncid, var_id(k), shuffle=shuffle, &
                                              deflate=1, deflate_level=deflate), &
                         nc_fname)
        end if
        !
        call nf_verify(nf90_put_att(ncid, var_id(k), '_FillValue', &
                                    (/NF90_FILL_INT/)), nc_fname)
        call nf_verify(nf90_put_att(ncid, var_id(k), 'long_name', &
                                    longname_l), nc_fname)
        if (gridmap_name /= '') then
          call nf_verify(nf90_put_att(ncid, var_id(k), 'coordinates', &
                                      'mesh_face_x mesh_face_y'), nc_fname)
          call nf_verify(nf90_put_att(ncid, var_id(k), 'grid_mapping', &
                                      gridmap_name), nc_fname)
        end if
        !
        if (nc_tag /= '') then
          call nf_verify(nf90_put_att(ncid, var_id(k), 'modflow6_input', &
                                      nc_tag), nc_fname)
          call nf_verify(nf90_put_att(ncid, var_id(k), 'modflow6_layer', &
                                      k), nc_fname)
        end if
      end do
      !
      ! -- allocate temporary 3d and reshape input
      dis_shape(1) = dis%ncol
      dis_shape(2) = dis%nrow
      dis_shape(3) = dis%nlay
      allocate (int3d(dis_shape(1), dis_shape(2), dis_shape(3)))
      int3d = reshape(p_mem, dis_shape)
      !
      ! -- exit define mode and write data
      call nf_verify(nf90_enddef(ncid), nc_fname)
      allocate (int1d(dis%nrow * dis%ncol))
      layer_shape(1) = dis%nrow * dis%ncol
      do k = 1, dis%nlay
        int1d = reshape(int3d(:, :, k), layer_shape)
        call nf_verify(nf90_put_var(ncid, var_id(k), int1d), nc_fname)
      end do
      !
      ! -- cleanup
      deallocate (int1d)
      deallocate (int3d)
      deallocate (var_id)
    end if
  end subroutine nc_export_int1d

  !> @brief netcdf export 2D integer
  !<
  subroutine nc_export_int2d(ncid, dim_ids, var_ids, dis, p_mem, nc_varname, &
                             pkgname, tagname, gridmap_name, shapestr, longname, &
                             nc_tag, deflate, shuffle, chunk_face, nc_fname)
    integer(I4B), intent(in) :: ncid
    type(MeshNCDimIdType), intent(inout) :: dim_ids
    type(MeshNCVarIdType), intent(inout) :: var_ids
    type(DisType), pointer, intent(in) :: dis
    integer(I4B), dimension(:, :), pointer, contiguous, intent(in) :: p_mem
    character(len=*), intent(in) :: nc_varname
    character(len=*), intent(in) :: pkgname
    character(len=*), intent(in) :: tagname
    character(len=*), intent(in) :: gridmap_name
    character(len=*), intent(in) :: shapestr
    character(len=*), intent(in) :: longname
    character(len=*), intent(in) :: nc_tag
    integer(I4B), intent(in) :: deflate
    integer(I4B), intent(in) :: shuffle
    integer(I4B), intent(in) :: chunk_face
    character(len=*), intent(in) :: nc_fname
    ! -- local
    integer(I4B) :: var_id
    integer(I4B), dimension(:), pointer, contiguous :: int1d
    integer(I4B), dimension(1) :: layer_shape
    character(len=LINELENGTH) :: longname_l
    !
    ! -- set long_name attribute
    if (longname /= '') then
      longname_l = trim(longname)
    else
      longname_l = trim(pkgname)//' '//trim(tagname)
    end if
    !
    ! -- reenter define mode and create variable
    call nf_verify(nf90_redef(ncid), nc_fname)
    call nf_verify(nf90_def_var(ncid, nc_varname, NF90_INT, &
                                (/dim_ids%nmesh_face/), var_id), &
                   nc_fname)
    !
    ! -- apply chunking parameters
    if (chunk_face > 0) then
      call nf_verify(nf90_def_var_chunking(ncid, var_id, NF90_CHUNKED, &
                                           (/chunk_face/)), nc_fname)
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
                                longname_l), nc_fname)
    if (gridmap_name /= '') then
      call nf_verify(nf90_put_att(ncid, var_id, 'coordinates', &
                                  'mesh_face_x mesh_face_y'), nc_fname)
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
    allocate (int1d(dis%nrow * dis%ncol))
    layer_shape(1) = dis%nrow * dis%ncol
    int1d = reshape(p_mem, layer_shape)
    call nf_verify(nf90_put_var(ncid, var_id, int1d), nc_fname)
    deallocate (int1d)
  end subroutine nc_export_int2d

  !> @brief netcdf export 3D integer
  !<
  subroutine nc_export_int3d(ncid, dim_ids, var_ids, dis, p_mem, nc_varname, &
                             pkgname, tagname, gridmap_name, shapestr, longname, &
                             nc_tag, deflate, shuffle, chunk_face, nc_fname)
    use InputOutputModule, only: lowcase
    integer(I4B), intent(in) :: ncid
    type(MeshNCDimIdType), intent(inout) :: dim_ids
    type(MeshNCVarIdType), intent(inout) :: var_ids
    type(DisType), pointer, intent(in) :: dis
    integer(I4B), dimension(:, :, :), pointer, contiguous, intent(in) :: p_mem
    character(len=*), intent(in) :: nc_varname
    character(len=*), intent(in) :: pkgname
    character(len=*), intent(in) :: tagname
    character(len=*), intent(in) :: gridmap_name
    character(len=*), intent(in) :: shapestr
    character(len=*), intent(in) :: longname
    character(len=*), intent(in) :: nc_tag
    integer(I4B), intent(in) :: deflate
    integer(I4B), intent(in) :: shuffle
    integer(I4B), intent(in) :: chunk_face
    character(len=*), intent(in) :: nc_fname
    ! -- local
    integer(I4B), dimension(:), allocatable :: var_id
    integer(I4B), dimension(:), pointer, contiguous :: int1d
    character(len=LINELENGTH) :: longname_l, varname_l
    integer(I4B), dimension(1) :: layer_shape
    integer(I4B) :: k
    !
    allocate (var_id(dis%nlay))
    !
    ! -- reenter define mode and create variable
    call nf_verify(nf90_redef(ncid), nc_fname)
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
                     nc_fname)
      !
      ! -- apply chunking parameters
      if (chunk_face > 0) then
        call nf_verify(nf90_def_var_chunking(ncid, var_id(k), NF90_CHUNKED, &
                                             (/chunk_face/)), nc_fname)
      end if
      ! -- deflate and shuffle
      if (deflate >= 0) then
        call nf_verify(nf90_def_var_deflate(ncid, var_id(k), shuffle=shuffle, &
                                            deflate=1, deflate_level=deflate), &
                       nc_fname)
      end if
      !
      call nf_verify(nf90_put_att(ncid, var_id(k), '_FillValue', &
                                  (/NF90_FILL_INT/)), nc_fname)
      call nf_verify(nf90_put_att(ncid, var_id(k), 'long_name', &
                                  longname_l), nc_fname)
      if (gridmap_name /= '') then
        call nf_verify(nf90_put_att(ncid, var_id(k), 'coordinates', &
                                    'mesh_face_x mesh_face_y'), nc_fname)
        call nf_verify(nf90_put_att(ncid, var_id(k), 'grid_mapping', &
                                    gridmap_name), nc_fname)
      end if
      !
      if (nc_tag /= '') then
        call nf_verify(nf90_put_att(ncid, var_id(k), 'modflow6_input', &
                                    nc_tag), nc_fname)
        call nf_verify(nf90_put_att(ncid, var_id(k), 'modflow6_layer', &
                                    k), nc_fname)
      end if
    end do
    !
    ! -- exit define mode and write data
    call nf_verify(nf90_enddef(ncid), nc_fname)
    allocate (int1d(dis%nrow * dis%ncol))
    layer_shape(1) = dis%nrow * dis%ncol
    do k = 1, dis%nlay
      int1d = reshape(p_mem(:, :, k), layer_shape)
      call nf_verify(nf90_put_var(ncid, var_id(k), int1d), nc_fname)
    end do
    !
    deallocate (int1d)
    deallocate (var_id)
  end subroutine nc_export_int3d

  !> @brief netcdf export 1D double
  !<
  subroutine nc_export_dbl1d(ncid, dim_ids, x_dim, y_dim, var_ids, dis, p_mem, &
                             nc_varname, pkgname, tagname, gridmap_name, &
                             shapestr, longname, nc_tag, deflate, shuffle, &
                             chunk_face, nc_fname)
    use InputOutputModule, only: lowcase
    integer(I4B), intent(in) :: ncid
    type(MeshNCDimIdType), intent(inout) :: dim_ids
    integer(I4B), intent(in) :: x_dim
    integer(I4B), intent(in) :: y_dim
    type(MeshNCVarIdType), intent(inout) :: var_ids
    type(DisType), pointer, intent(in) :: dis
    real(DP), dimension(:), pointer, contiguous, intent(in) :: p_mem
    character(len=*), intent(in) :: nc_varname
    character(len=*), intent(in) :: pkgname
    character(len=*), intent(in) :: tagname
    character(len=*), intent(in) :: gridmap_name
    character(len=*), intent(in) :: shapestr
    character(len=*), intent(in) :: longname
    character(len=*), intent(in) :: nc_tag
    integer(I4B), intent(in) :: deflate
    integer(I4B), intent(in) :: shuffle
    integer(I4B), intent(in) :: chunk_face
    character(len=*), intent(in) :: nc_fname
    ! -- local
    integer(I4B), dimension(3) :: dis_shape
    integer(I4B), dimension(1) :: layer_shape
    real(DP), dimension(:, :, :), pointer, contiguous :: dbl3d
    real(DP), dimension(:), pointer, contiguous :: dbl1d
    integer(I4B) :: axis_dim, k
    integer(I4B), dimension(:), allocatable :: var_id
    character(len=LINELENGTH) :: longname_l, varname_l
    !
    if (shapestr == 'NROW' .or. &
        shapestr == 'NCOL') then
      !
      if (shapestr == 'NROW') then
        axis_dim = y_dim
      else
        axis_dim = x_dim
      end if
      ! -- set long_name attribute
      if (longname /= '') then
        longname_l = trim(longname)
      else
        longname_l = trim(pkgname)//' '//trim(tagname)
      end if
      !
      allocate (var_id(1))
      !
      ! -- reenter define mode and create variable
      call nf_verify(nf90_redef(ncid), nc_fname)
      call nf_verify(nf90_def_var(ncid, nc_varname, NF90_DOUBLE, &
                                  (/axis_dim/), var_id(1)), &
                     nc_fname)
      !
      ! -- NROW/NCOL shapes use default chunking
      if (deflate >= 0) then
        call nf_verify(nf90_def_var_deflate(ncid, var_id(1), shuffle=shuffle, &
                                            deflate=1, deflate_level=deflate), &
                       nc_fname)
      end if
      !
      call nf_verify(nf90_put_att(ncid, var_id(1), '_FillValue', &
                                  (/NF90_FILL_DOUBLE/)), nc_fname)
      call nf_verify(nf90_put_att(ncid, var_id(1), 'long_name', &
                                  longname_l), nc_fname)
      !
      if (nc_tag /= '') then
        call nf_verify(nf90_put_att(ncid, var_id(1), 'modflow6_input', &
                                    nc_tag), nc_fname)
      end if
      !
      ! -- exit define mode and write data
      call nf_verify(nf90_enddef(ncid), nc_fname)
      call nf_verify(nf90_put_var(ncid, var_id(1), p_mem), &
                     nc_fname)

    else
      allocate (var_id(dis%nlay))
      !
      ! -- reenter define mode and create variable
      call nf_verify(nf90_redef(ncid), nc_fname)
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
                       nc_fname)
        !
        ! -- apply chunking parameters
        if (chunk_face > 0) then
          call nf_verify(nf90_def_var_chunking(ncid, var_id(k), NF90_CHUNKED, &
                                               (/chunk_face/)), nc_fname)
        end if
        ! -- deflate and shuffle
        if (deflate >= 0) then
          call nf_verify(nf90_def_var_deflate(ncid, var_id(k), shuffle=shuffle, &
                                              deflate=1, deflate_level=deflate), &
                         nc_fname)
        end if
        !
        call nf_verify(nf90_put_att(ncid, var_id(k), '_FillValue', &
                                    (/NF90_FILL_DOUBLE/)), nc_fname)
        call nf_verify(nf90_put_att(ncid, var_id(k), 'long_name', &
                                    longname_l), nc_fname)
        if (gridmap_name /= '') then
          call nf_verify(nf90_put_att(ncid, var_id(k), 'coordinates', &
                                      'mesh_face_x mesh_face_y'), nc_fname)
          call nf_verify(nf90_put_att(ncid, var_id(k), 'grid_mapping', &
                                      gridmap_name), nc_fname)
        end if
        !
        if (nc_tag /= '') then
          call nf_verify(nf90_put_att(ncid, var_id(k), 'modflow6_input', &
                                      nc_tag), nc_fname)
          call nf_verify(nf90_put_att(ncid, var_id(k), 'modflow6_layer', &
                                      k), nc_fname)
        end if
      end do
      !
      ! -- allocate temporary 3d and reshape input
      dis_shape(1) = dis%ncol
      dis_shape(2) = dis%nrow
      dis_shape(3) = dis%nlay
      allocate (dbl3d(dis_shape(1), dis_shape(2), dis_shape(3)))
      dbl3d = reshape(p_mem, dis_shape)
      !
      ! -- exit define mode and write data
      call nf_verify(nf90_enddef(ncid), nc_fname)
      allocate (dbl1d(dis%nrow * dis%ncol))
      layer_shape(1) = dis%nrow * dis%ncol
      do k = 1, dis%nlay
        dbl1d = reshape(dbl3d(:, :, k), layer_shape)
        call nf_verify(nf90_put_var(ncid, var_id(k), dbl1d), nc_fname)
      end do
      !
      ! -- cleanup
      deallocate (dbl1d)
      deallocate (dbl3d)
      deallocate (var_id)
    end if
  end subroutine nc_export_dbl1d

  !> @brief netcdf export 2D double
  !<
  subroutine nc_export_dbl2d(ncid, dim_ids, var_ids, dis, p_mem, nc_varname, &
                             pkgname, tagname, gridmap_name, shapestr, longname, &
                             nc_tag, deflate, shuffle, chunk_face, nc_fname)
    integer(I4B), intent(in) :: ncid
    type(MeshNCDimIdType), intent(inout) :: dim_ids
    type(MeshNCVarIdType), intent(inout) :: var_ids
    type(DisType), pointer, intent(in) :: dis
    real(DP), dimension(:, :), pointer, contiguous, intent(in) :: p_mem
    character(len=*), intent(in) :: nc_varname
    character(len=*), intent(in) :: pkgname
    character(len=*), intent(in) :: tagname
    character(len=*), intent(in) :: gridmap_name
    character(len=*), intent(in) :: shapestr
    character(len=*), intent(in) :: longname
    character(len=*), intent(in) :: nc_tag
    integer(I4B), intent(in) :: deflate
    integer(I4B), intent(in) :: shuffle
    integer(I4B), intent(in) :: chunk_face
    character(len=*), intent(in) :: nc_fname
    ! -- local
    integer(I4B) :: var_id
    character(len=LINELENGTH) :: longname_l
    real(DP), dimension(:), pointer, contiguous :: dbl1d
    integer(I4B), dimension(1) :: layer_shape
    !
    ! -- set long_name attribute
    if (longname /= '') then
      longname_l = trim(longname)
    else
      longname_l = trim(pkgname)//' '//trim(tagname)
    end if
    !
    ! -- reenter define mode and create variable
    call nf_verify(nf90_redef(ncid), nc_fname)
    call nf_verify(nf90_def_var(ncid, nc_varname, NF90_DOUBLE, &
                                (/dim_ids%nmesh_face/), var_id), &
                   nc_fname)
    !
    ! -- apply chunking parameters
    if (chunk_face > 0) then
      call nf_verify(nf90_def_var_chunking(ncid, var_id, NF90_CHUNKED, &
                                           (/chunk_face/)), nc_fname)
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
                                longname_l), nc_fname)
    if (gridmap_name /= '') then
      call nf_verify(nf90_put_att(ncid, var_id, 'coordinates', &
                                  'mesh_face_x mesh_face_y'), nc_fname)
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
    allocate (dbl1d(dis%nrow * dis%ncol))
    layer_shape(1) = dis%nrow * dis%ncol
    dbl1d = reshape(p_mem, layer_shape)
    call nf_verify(nf90_put_var(ncid, var_id, dbl1d), nc_fname)
    deallocate (dbl1d)
  end subroutine nc_export_dbl2d

  !> @brief netcdf export 3D double
  !<
  subroutine nc_export_dbl3d(ncid, dim_ids, var_ids, dis, p_mem, nc_varname, &
                             pkgname, tagname, gridmap_name, shapestr, longname, &
                             nc_tag, deflate, shuffle, chunk_face, nc_fname)
    use InputOutputModule, only: lowcase
    integer(I4B), intent(in) :: ncid
    type(MeshNCDimIdType), intent(inout) :: dim_ids
    type(MeshNCVarIdType), intent(inout) :: var_ids
    type(DisType), pointer, intent(in) :: dis
    real(DP), dimension(:, :, :), pointer, contiguous, intent(in) :: p_mem
    character(len=*), intent(in) :: nc_varname
    character(len=*), intent(in) :: pkgname
    character(len=*), intent(in) :: tagname
    character(len=*), intent(in) :: gridmap_name
    character(len=*), intent(in) :: shapestr
    character(len=*), intent(in) :: longname
    character(len=*), intent(in) :: nc_tag
    integer(I4B), intent(in) :: deflate
    integer(I4B), intent(in) :: shuffle
    integer(I4B), intent(in) :: chunk_face
    character(len=*), intent(in) :: nc_fname
    ! -- local
    integer(I4B), dimension(:), allocatable :: var_id
    real(DP), dimension(:), pointer, contiguous :: dbl1d
    character(len=LINELENGTH) :: longname_l, varname_l
    integer(I4B), dimension(1) :: layer_shape
    integer(I4B) :: k
    !
    allocate (var_id(dis%nlay))
    !
    ! -- reenter define mode and create variable
    call nf_verify(nf90_redef(ncid), nc_fname)
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
                     nc_fname)
      !
      ! -- apply chunking parameters
      if (chunk_face > 0) then
        call nf_verify(nf90_def_var_chunking(ncid, var_id(k), NF90_CHUNKED, &
                                             (/chunk_face/)), nc_fname)
      end if
      ! -- deflate and shuffle
      if (deflate >= 0) then
        call nf_verify(nf90_def_var_deflate(ncid, var_id(k), shuffle=shuffle, &
                                            deflate=1, deflate_level=deflate), &
                       nc_fname)
      end if
      !
      call nf_verify(nf90_put_att(ncid, var_id(k), '_FillValue', &
                                  (/NF90_FILL_DOUBLE/)), nc_fname)
      call nf_verify(nf90_put_att(ncid, var_id(k), 'long_name', &
                                  longname_l), nc_fname)
      if (gridmap_name /= '') then
        call nf_verify(nf90_put_att(ncid, var_id(k), 'coordinates', &
                                    'mesh_face_x mesh_face_y'), nc_fname)
        call nf_verify(nf90_put_att(ncid, var_id(k), 'grid_mapping', &
                                    gridmap_name), nc_fname)
      end if
      !
      if (nc_tag /= '') then
        call nf_verify(nf90_put_att(ncid, var_id(k), 'modflow6_input', &
                                    nc_tag), nc_fname)
        call nf_verify(nf90_put_att(ncid, var_id(k), 'modflow6_layer', &
                                    k), nc_fname)
      end if
    end do
    !
    ! -- exit define mode and write data
    call nf_verify(nf90_enddef(ncid), nc_fname)
    allocate (dbl1d(dis%nrow * dis%ncol))
    layer_shape(1) = dis%nrow * dis%ncol
    do k = 1, dis%nlay
      dbl1d = reshape(p_mem(:, :, k), layer_shape)
      call nf_verify(nf90_put_var(ncid, var_id(k), dbl1d), nc_fname)
    end do
    !
    deallocate (dbl1d)
    deallocate (var_id)
  end subroutine nc_export_dbl3d

end module MeshDisModelModule
