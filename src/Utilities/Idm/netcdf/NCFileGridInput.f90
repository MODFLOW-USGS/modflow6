!> @brief This module contains the NCFileGridInputModule
!!
!! This module contains the routines for loading MODFLOW 6
!! netcdf dynamic grid input to the input context.
!!
!<
module NCFileGridInputModule

  use KindModule, only: DP, I4B, LGP
  use ConstantsModule, only: LINELENGTH, LENVARNAME, DNODATA, INODATA
  use SimModule, only: store_error, store_error_filename
  use SimVariablesModule, only: errmsg
  use MemoryManagerModule, only: mem_allocate, mem_setptr
  use NCModelInputsModule, only: NCModelPackageInputType
  use DynamicParamFilterModule, only: DynamicParamFilterType
  use BoundInputContextModule, only: BoundInputContextType, ReadStateVarType
  use NCInputLoadTypeModule, only: NCDynamicPkgLoadBaseType
  use InputDefinitionModule, only: InputParamDefinitionType
  use DefinitionSelectModule, only: get_param_definition_type
  use LoadNCFileModule, only: IDM_NETCDF4_MAX_DIM, nf_verify
  use ModflowInputModule, only: ModflowInputType

  implicit none
  private
  public :: NCBoundGridInputType

  !> @brief NetCDF array based dynamic loader type
  !<
  type, extends(NCDynamicPkgLoadBaseType) :: NCBoundGridInputType
    integer(I4B) :: ncid
    integer(I4B) :: iiper
    type(ReadStateVarType), dimension(:), allocatable :: param_reads !< read states for current load
    type(DynamicParamFilterType) :: filter
    type(BoundInputContextType) :: bound_context !< boundary package input context
  contains
    procedure :: init => bndgrid_init
    procedure :: alloc => bndgrid_alloc
    procedure :: rp => bndgrid_rp
    procedure :: validate => bndgrid_validate
    procedure :: destroy => bndgrid_destroy
  end type NCBoundGridInputType

contains

  !> @brief dynamic grid loader init
  !<
  subroutine bndgrid_init(this, mf6_input, component_name, component_input_name, &
                          input_name, iperblock, iout)
    ! -- modules
    ! -- dummy
    class(NCBoundGridInputType), intent(inout) :: this
    type(ModflowInputType), intent(in) :: mf6_input
    character(len=*), intent(in) :: component_name
    character(len=*), intent(in) :: component_input_name
    character(len=*), intent(in) :: input_name
    integer(I4B), intent(in) :: iperblock
    integer(I4B), intent(in) :: iout
    !
    ! -- initialize context
    call this%DynamicPkgLoadType%init(mf6_input, component_name, &
                                      component_input_name, &
                                      input_name, iperblock, iout)
    !
    this%iiper = 0
    !
    call this%bound_context%init(this%mf6_input, this%readasarrays)
    !
    ! -- allocate bound params
    call this%alloc()
    !
    ! -- return
    return
  end subroutine bndgrid_init

  !> @brief dynamic grid loader allocate params
  !<
  subroutine bndgrid_alloc(this)
    ! -- modules
    ! -- dummy
    class(NCBoundGridInputType), intent(inout) :: this !< StressGridInputType
    character(len=LENVARNAME) :: rs_varname
    integer(I4B), pointer :: intvar
    integer(I4B) :: iparam
    !
    ! -- initialize param filter
    call this%filter%init(this%mf6_input, this%readasarrays, &
                          this%bound_context%naux, &
                          this%bound_context%inamedbound, &
                          this%iout)
    ! -- set in scope param names
    call this%filter%get_flt_params(this%param_names, this%nparam)
    ! -- allocate params
    call this%bound_context%array_params_create(this%param_names, this%nparam, &
                                                this%input_name)
    ! -- enable
    call this%bound_context%enable()
    !
    ! -- allocate and set param_reads pointer array
    allocate (this%param_reads(this%nparam))
    !
    ! store read state variable pointers
    do iparam = 1, this%nparam
      ! -- allocate and store name of read state variable
      rs_varname = this%bound_context%rsv_alloc(this%param_names(iparam))
      call mem_setptr(intvar, rs_varname, this%mf6_input%mempath)
      this%param_reads(iparam)%invar => intvar
      this%param_reads(iparam)%invar = 0
    end do
    !
    ! -- return
    return
  end subroutine bndgrid_alloc

  !> @brief dynamic grid loader read and prepare
  !<
  subroutine bndgrid_rp(this, ncid, ncpkg)
    ! -- modules
    ! -- dummy
    class(NCBoundGridInputType), intent(inout) :: this
    integer(I4B), intent(in) :: ncid
    type(NCModelPackageInputType), pointer, intent(inout) :: ncpkg
    integer(I4B) :: iparam
    !
    ! -- increment iiper
    this%iiper = this%iiper + 1
    !
    ! -- reset read state vars
    do iparam = 1, this%nparam
      this%param_reads(iparam)%invar = 0
    end do
    !
    ! -- dynamic load
    call rp_array(this%mf6_input, ncpkg, this%param_names, &
                  this%param_reads, this%bound_context, &
                  ncid, this%iiper, this%input_name, this%iout)
    !
    ! -- return
    return
  end subroutine bndgrid_rp

  !> @brief dynamic array loader validate
  !!
  !! Verify expected input package parameters are in input file
  !!
  !<
  subroutine bndgrid_validate(this, ncpkg)
    ! -- modules
    ! -- dummy
    class(NCBoundGridInputType), intent(inout) :: this
    type(NCModelPackageInputType), pointer, intent(inout) :: ncpkg
    ! -- local
    type(InputParamDefinitionType), pointer :: idt
    character(len=LINELENGTH) :: tagname, varname
    integer(I4B) :: iparam, ivar
    logical(LGP) :: found
    !
    do iparam = 1, this%nparam
      tagname = this%param_names(iparam)
      !
      idt => &
        get_param_definition_type(this%mf6_input%param_dfns, &
                                  this%mf6_input%component_type, &
                                  this%mf6_input%subcomponent_type, &
                                  'PERIOD', tagname, '')
      !
      if (idt%required) then
        found = .false.
        do ivar = 1, ncpkg%blocklist(this%iperblock)%varnum
          varname = ncpkg%blocklist(this%iperblock)%tagnames(ivar)
          if (varname == tagname) then
            found = .true.
            exit
          end if
        end do
        !
        if (.not. found) then
          errmsg = 'Required PERIOD variable not found: "'//trim(tagname)// &
                   '" for package "'//trim(this%mf6_input%subcomponent_name)//'".'
          call store_error(errmsg)
          call store_error_filename(this%input_name)
        end if
      end if
      !
    end do
    !
    ! -- return
    return
  end subroutine bndgrid_validate

  !> @brief dynamic grid loader destroy
  !<
  subroutine bndgrid_destroy(this)
    ! -- modules
    ! -- dummy
    class(NCBoundGridInputType), intent(inout) :: this
    !
    ! -- deallocate allocatables
    if (allocated(this%param_names)) deallocate (this%param_names)
    if (allocated(this%param_reads)) deallocate (this%param_reads)
    !
    ! -- destroy bnd context object
    call this%bound_context%destroy()
    !
    ! -- return
    return
  end subroutine bndgrid_destroy

  subroutine rp_array(mf6_input, ncpkg, param_names, param_reads, &
                      bndctx, ncid, iiper, nc_fname, iout)
    use netcdf
    ! -- dummy
    type(ModflowInputType), intent(in) :: mf6_input
    type(NCModelPackageInputType), intent(in) :: ncpkg
    character(len=LINELENGTH), dimension(:), &
      allocatable :: param_names !< dynamic param names
    type(ReadStateVarType), dimension(:), allocatable :: param_reads
    type(BoundInputContextType), intent(in) :: bndctx
    integer(I4B), intent(in) :: ncid
    integer(I4B), intent(in) :: iiper
    character(len=*), intent(in) :: nc_fname
    integer(I4B), intent(in) :: iout
    ! -- local
    type(InputParamDefinitionType), pointer :: idt
    integer(I4B) :: iparam, iblock, varid
    character(len=LINELENGTH) :: tagname
    !
    do iblock = 1, size(ncpkg%blocklist)
      if (ncpkg%blocklist(iblock)%blockname == 'PERIOD') then
        do iparam = 1, ncpkg%blocklist(iblock)%varnum
          !
          ! -- set varid
          varid = ncpkg%blocklist(iblock)%varids(iparam)
          !
          ! -- set tagname
          tagname = ncpkg%blocklist(iblock)%tagnames(iparam)
          !
          idt => &
            get_param_definition_type(mf6_input%param_dfns, &
                                      mf6_input%component_type, &
                                      mf6_input%subcomponent_type, &
                                      'PERIOD', tagname, nc_fname)
          !
          call load_var(mf6_input, param_names, &
                        param_reads, ncid, idt, varid, &
                        iiper, nc_fname, iout)
        end do
      end if
    end do
    !
    ! -- return
    return
  end subroutine rp_array

  subroutine load_var(mf6_input, param_names, param_reads, &
                      ncid, idt, varid, iiper, nc_fname, iout)
    use netcdf
    use TdisModule, only: kper
    ! -- dummy
    type(ModflowInputType), intent(in) :: mf6_input
    character(len=LINELENGTH), dimension(:), &
      allocatable :: param_names !< dynamic param names
    type(ReadStateVarType), dimension(:), allocatable :: param_reads
    integer(I4B), intent(in) :: ncid
    type(InputParamDefinitionType), pointer, intent(in) :: idt
    integer(I4B), intent(in) :: varid
    integer(I4B), intent(in) :: iiper
    character(len=*), intent(in) :: nc_fname
    integer(I4B), intent(in) :: iout
    ! -- local
    integer(I4B) :: n, load_idx, len
    character(len=LINELENGTH) :: varname, dimname
    integer(I4B) :: vartype, ndims, nattr
    integer(I4B), dimension(IDM_NETCDF4_MAX_DIM) :: dimids, dimlens
    integer(I4B), dimension(:), allocatable :: ipers
    !
    ! -- index for loading kper data
    load_idx = 0
    !
    ! -- inquire for variable info
    call nf_verify(nf90_inquire_variable(ncid, varid, varname, vartype, ndims, &
                                         dimids, nattr), ncid, iout)
    !
    ! -- set variable dimensions
    do n = 1, ndims
      !
      ! -- (1+: data, last-1: maxbound, last:ipers)
      call nf_verify(nf90_inquire_dimension(ncid, dimids(n), dimname, &
                                            dimlens(n)), ncid, iout)
    end do
    !
    ! -- set current load index
    if (nf90_inquire_attribute(ncid, varid, "mf6_iper", &
                               len=len) == NF90_NOERR) then
      ! -- allocate local array
      allocate (ipers(len))
      ! -- read variable ipers array
      if (nf90_get_att(ncid, varid, "mf6_iper", &
                       ipers) == NF90_NOERR) then
        ! -- check load periods
        do n = 1, size(ipers)
          !
          if (ipers(n) == kper) then
            ! -- load, save index
            load_idx = n
            exit
          end if
        end do
      end if
    end if
    !
    ! -- read and load variable data if load_idx defined
    if (load_idx > 0) then
      select case (idt%datatype)
      case ('INTEGER1D')
        call load_int1d(mf6_input, ncid, param_names, param_reads, &
                        idt, dimlens, varid, load_idx, iout)
        !
      case ('DOUBLE1D')
        call load_dbl1d(mf6_input, ncid, param_names, param_reads, &
                        idt, dimlens, varid, load_idx, iout)
        !
        ! TODO
        ! case ('DOUBLE2D')
      case default
        errmsg = 'IDM unimplemented. NCFileGridInput::load_var &
                 &datatype='//trim(idt%datatype)
        call store_error(errmsg)
        call store_error_filename(nc_fname)
      end select
    end if
    !
    ! -- return
    return
  end subroutine load_var

  subroutine load_int1d(mf6_input, ncid, param_names, param_reads, &
                        idt, dimlens, varid, load_idx, iout)
    use netcdf
    use ArrayHandlersModule, only: ifind
    ! -- dummy
    type(ModflowInputType), intent(in) :: mf6_input
    integer(I4B), intent(in) :: ncid
    character(len=LINELENGTH), dimension(:), &
      allocatable :: param_names !< dynamic param names
    type(ReadStateVarType), dimension(:), allocatable :: param_reads
    type(InputParamDefinitionType), pointer, intent(in) :: idt
    integer(I4B), dimension(IDM_NETCDF4_MAX_DIM), intent(in) :: dimlens
    integer(I4B), intent(in) :: varid
    integer(I4B), intent(in) :: load_idx
    integer(I4B), intent(in) :: iout
    ! -- local
    integer(I4B), dimension(:), pointer, contiguous :: int1d
    integer(I4B), dimension(:, :, :), pointer, contiguous :: int3d
    integer(I4B) :: i, j, k, n
    integer(I4B) :: nvals, iparam
    !
    ! -- set iparam to name index
    iparam = ifind(param_names, idt%tagname)
    !
    ! -- set nvals as ncpl
    nvals = dimlens(1) * dimlens(2)
    !
    ! -- set variable pointer
    call mem_setptr(int1d, idt%mf6varname, mf6_input%mempath)
    !
    ! -- allocate local array
    allocate (int3d(dimlens(1), dimlens(2), dimlens(3)))
    !
    ! -- read data into temporary
    call nf_verify(nf90_get_var(ncid, varid, int3d, &
                                start=(/1, 1, 1, load_idx/), &
                                count=(/dimlens(1), dimlens(2), dimlens(3), &
                                        1/)), ncid, iout)
    !
    ! -- copy to flat array
    n = 0
    do k = 1, 1
      do j = 1, dimlens(2)
        do i = 1, dimlens(1)
          n = n + 1
          ! TODO FLoPy needs to 1-index these
          int1d(n) = int3d(i, j, k) + 1
        end do
      end do
    end do
    !
    ! -- deallocate local array
    deallocate (int3d)
    !
    ! -- update read state variable
    if (iparam > 0) then
      param_reads(iparam)%invar = 1
    end if
    !
    ! -- return
    return
  end subroutine load_int1d

  subroutine load_dbl1d(mf6_input, ncid, param_names, param_reads, &
                        idt, dimlens, varid, load_idx, iout)
    use netcdf
    use ArrayHandlersModule, only: ifind
    ! -- dummy
    type(ModflowInputType), intent(in) :: mf6_input
    integer(I4B), intent(in) :: ncid
    character(len=LINELENGTH), dimension(:), &
      allocatable :: param_names !< dynamic param names
    type(ReadStateVarType), dimension(:), allocatable :: param_reads
    type(InputParamDefinitionType), pointer, intent(in) :: idt
    integer(I4B), dimension(IDM_NETCDF4_MAX_DIM), intent(in) :: dimlens
    integer(I4B), intent(in) :: varid
    integer(I4B), intent(in) :: load_idx
    integer(I4B), intent(in) :: iout
    ! -- local
    real(DP), dimension(:), pointer, contiguous :: dbl1d
    real(DP), dimension(:, :, :), pointer, contiguous :: dbl3d
    integer(I4B) :: i, j, k, n
    integer(I4B) :: nvals, iparam
    !
    ! -- set iparam to name index
    iparam = ifind(param_names, idt%tagname)
    !
    ! -- set nvals as ncpl
    nvals = dimlens(1) * dimlens(2)
    !
    ! -- set variable pointer
    call mem_setptr(dbl1d, idt%mf6varname, mf6_input%mempath)
    !
    ! -- allocate local array
    allocate (dbl3d(dimlens(1), dimlens(2), dimlens(3)))
    !
    ! -- read data into temporary
    call nf_verify(nf90_get_var(ncid, varid, dbl3d, &
                                start=(/1, 1, 1, load_idx/), &
                                count=(/dimlens(1), dimlens(2), dimlens(3), &
                                        1/)), ncid, iout)
    !
    ! -- copy to flat array
    n = 0
    do k = 1, 1
      do j = 1, dimlens(2)
        do i = 1, dimlens(1)
          n = n + 1
          dbl1d(n) = dbl3d(i, j, k)
        end do
      end do
    end do
    !
    ! -- deallocate local array
    deallocate (dbl3d)
    !
    ! -- update read state variable
    if (iparam > 0) then
      param_reads(iparam)%invar = 1
    end if
    !
    ! -- return
    return
  end subroutine load_dbl1d

end module NCFileGridInputModule
