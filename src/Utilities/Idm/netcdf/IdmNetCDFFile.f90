!> @brief This module contains the IdmNetCDFFileModule
!!
!! This module contains the high-level routines for loading
!! a MODFLOW netcdf input file to the input context.
!!
!<
module IdmNetCDFFileModule

  use KindModule, only: DP, I4B, LGP
  use ConstantsModule, only: LINELENGTH, LENCOMPONENTNAME, LENVARNAME
  use SimModule, only: store_error, store_error_filename
  use SimVariablesModule, only: errmsg
  use ModflowInputModule, only: ModflowInputType
  use InputLoadTypeModule, only: StaticPkgLoadBaseType, DynamicPkgLoadBaseType
  use NC4ModelInputsModule, only: NC4BlockVariablesType
  use NC4ModelInputsModule, only: NC4ModelInputsType, NC4ModelPackageInputType
  use BoundInputContextModule, only: BoundInputContextType
  use ArrayHandlersModule, only: expandarray
  use InputLoadTypeModule, only: DynamicPkgLoadType
  use SourceCommonModule, only: ReadStateVarType

  implicit none
  private
  public :: open_ncfile
  public :: NC4StaticPkgLoadType

  !> @brief base abstract type for netcdf source dynamic load
  !!
  !<
  type, abstract, extends(DynamicPkgLoadType) :: NCDynamicPkgLoadBaseType
  contains
    procedure(nc_period_load_if), deferred :: rp
  end type NCDynamicPkgLoadBaseType

  abstract interface
    subroutine nc_period_load_if(this, ncid, ncpkg)
      import NCDynamicPkgLoadBaseType, NC4ModelPackageInputType, I4B
      class(NCDynamicPkgLoadBaseType), intent(inout) :: this
      integer(I4B), intent(in) :: ncid
      type(NC4ModelPackageInputType), pointer, intent(inout) :: ncpkg
    end subroutine
  end interface

  type, extends(StaticPkgLoadBaseType) :: NC4StaticPkgLoadType
    integer(I4B) :: ncid
    type(NC4ModelPackageInputType), pointer :: ncpkg => null()
  contains
    procedure :: init => static_init
    procedure :: load => static_load
  end type NC4StaticPkgLoadType

  type, extends(DynamicPkgLoadBaseType) :: NC4DynamicPkgLoadType
    integer(I4B), pointer :: iper => null()
    integer(I4B), pointer :: ionper => null()
    integer(I4B) :: iiper
    integer(I4B) :: ncid
    type(NC4ModelPackageInputType), pointer :: ncpkg => null()
    class(NCDynamicPkgLoadBaseType), pointer :: loader => null()
  contains
    procedure :: init => dynamic_init
    procedure :: set => dynamic_set
    procedure :: df => dynamic_df
    procedure :: rp => dynamic_load
    procedure :: create_loader => dynamic_create_loader
    procedure :: destroy => dynamic_destroy
  end type NC4DynamicPkgLoadType

  !> @brief NetCDF list based dynamic loader type
  !<
  type, extends(NCDynamicPkgLoadBaseType) :: NCListInputType
    integer(I4B) :: ncid
    integer(I4B) :: iiper
    integer(I4B) :: nparam
    character(len=LENVARNAME), dimension(:), &
      allocatable :: param_names
    type(BoundInputContextType) :: bndctx
  contains
    procedure :: init => inlist_init
    procedure :: rp => inlist_rp
    procedure :: validate => inlist_validate
    procedure :: destroy => inlist_destroy
  end type NCListInputType

  !> @brief NetCDF array based dynamic loader type
  !<
  type, extends(NCDynamicPkgLoadBaseType) :: NCGridInputType
    integer(I4B) :: ncid
    integer(I4B) :: iiper
    integer(I4B) :: nparam !< number of in scope dynamic parameters
    character(len=LENVARNAME), dimension(:), &
      allocatable :: param_names !< dynamic param names
    type(ReadStateVarType), dimension(:), allocatable :: param_reads !< read states for current load
    type(BoundInputContextType) :: bndctx !< boundary package input context
  contains
    procedure :: init => ingrid_init
    procedure :: alloc => ingrid_alloc
    procedure :: rp => ingrid_rp
    procedure :: validate => ingrid_validate
    procedure :: destroy => ingrid_destroy
  end type NCGridInputType

contains

  subroutine static_init(this, mf6_input, modelname, modelfname, source)
    use InputModelContextModule, only: GetModelNC4Context
    class(NC4StaticPkgLoadType), intent(inout) :: this
    type(ModflowInputType), intent(in) :: mf6_input
    character(len=*), intent(in) :: modelname
    character(len=*), intent(in) :: modelfname
    character(len=*), intent(in) :: source
    type(NC4ModelInputsType), pointer :: nc4_context
    !
    call this%StaticPkgLoadType%init(mf6_input, modelname, modelfname, source)
    !
    ! -- set context from input file
    nc4_context => GetModelNC4Context(modelname)
    !
    ! -- cache ncid for model netcdf4 file
    this%ncid = nc4_context%ncid
    !
    ! -- set package description from context
    this%ncpkg => &
      nc4_context%get_package(mf6_input%component_name, &
                              mf6_input%subcomponent_name)
    ! -- return
    return
  end subroutine static_init

  function static_load(this, iout) result(period_loader)
    use LoadNetCDFDataModule, only: input_load
    class(NC4StaticPkgLoadType), intent(inout) :: this
    integer(I4B), intent(in) :: iout
    class(DynamicPkgLoadBaseType), pointer :: period_loader
    class(NC4DynamicPkgLoadType), pointer :: nc4_loader => null()
    !
    ! -- initialize
    nullify (period_loader)
    !
    ! -- load model package to input context
    call input_load(this%mf6_input, this%ncpkg, this%ncid, this%sourcename, iout)
    !
    ! -- check if package is dynamic
    if (this%iperblock > 0) then
      !
      ! -- create dynamic loader
      allocate (nc4_loader)
      !
      ! -- initailze loader
      call nc4_loader%init(this%mf6_input, this%modelname, &
                           this%modelfname, this%sourcename, &
                           this%iperblock, iout)
      !
      ! -- set period data
      call nc4_loader%set(this%ncpkg, this%ncid)
      !
      ! -- set returned base pointer
      period_loader => nc4_loader
      !
    end if
    !
    ! -- return
    return
  end function static_load

  subroutine dynamic_init(this, mf6_input, modelname, modelfname, &
                          source, iperblock, iout)
    use MemoryManagerModule, only: mem_allocate
    class(NC4DynamicPkgLoadType), intent(inout) :: this
    type(ModflowInputType), intent(in) :: mf6_input
    character(len=*), intent(in) :: modelname
    character(len=*), intent(in) :: modelfname
    character(len=*), intent(in) :: source
    integer(I4B), intent(in) :: iperblock
    integer(I4B), intent(in) :: iout
    !
    call this%DynamicPkgLoadType%init(mf6_input, modelname, modelfname, &
                                      source, iperblock, iout)
    !
    ! -- allocate iper and ionper
    call mem_allocate(this%iper, 'IPER', this%mf6_input%mempath)
    call mem_allocate(this%ionper, 'IONPER', this%mf6_input%mempath)
    !
    ! -- initialize package
    this%iper = 0
    this%ionper = 0
    this%iiper = 0
    !
    ! -- return
    return
  end subroutine dynamic_init

  subroutine dynamic_set(this, ncpkg, ncid)
    use MemoryManagerModule, only: mem_allocate
    class(NC4DynamicPkgLoadType), intent(inout) :: this
    type(NC4ModelPackageInputType), pointer, intent(in) :: ncpkg
    integer(I4B), intent(in) :: ncid
    !
    ! -- set context
    this%ncpkg => ncpkg
    this%ncid = ncid
    !
    ! -- verify block variable iper list
    if (.not. allocated(ncpkg%ipers)) then
      errmsg = 'Required NetCDF package variable IPERS not found for package "' &
               //trim(this%mf6_input%subcomponent_name)//'".'
      call store_error(errmsg)
      call store_error_filename(this%sourcename)
    end if
    !
    ! -- create the loader
    call this%create_loader()
    !
    ! -- return
    return
  end subroutine dynamic_set

  subroutine dynamic_df(this)
    use TdisModule, only: nper
    class(NC4DynamicPkgLoadType), intent(inout) :: this
    !
    ! -- set first ionper
    if (size(this%ncpkg%ipers) > this%iiper) then
      this%iiper = this%iiper + 1
      this%ionper = this%ncpkg%ipers(this%iiper)
    else
      this%ionper = nper + 1
    end if
    !
    ! -- return
    return
  end subroutine dynamic_df

  subroutine dynamic_load(this)
    use TdisModule, only: kper, nper
    class(NC4DynamicPkgLoadType), intent(inout) :: this
    !
    ! -- check if ready to load
    if (this%ionper /= kper) return
    !
    ! -- package dynamic load
    call this%loader%rp(this%ncid, this%ncpkg)
    !
    ! -- update loaded iper
    this%iper = kper
    !
    ! -- read next ionper
    if (size(this%ncpkg%ipers) > this%iiper) then
      this%iiper = this%iiper + 1
      this%ionper = this%ncpkg%ipers(this%iiper)
    else
      this%ionper = nper + 1
    end if
    !
    ! -- return
    return
  end subroutine dynamic_load

  !> @brief allocate a dynamic loader based on load context
  !<
  subroutine dynamic_create_loader(this)
    ! -- dummy
    class(NC4DynamicPkgLoadType), intent(inout) :: this
    class(NCListInputType), pointer :: list_loader
    class(NCGridInputType), pointer :: grid_loader
    !
    ! -- allocate and set loader
    if (this%readasarrays) then
      allocate (grid_loader)
      this%loader => grid_loader
    else
      allocate (list_loader)
      this%loader => list_loader
    end if
    !
    ! -- initialize loader
    call this%loader%init(this%mf6_input, &
                          this%modelname, &
                          this%modelfname, &
                          this%sourcename, &
                          this%iperblock, &
                          this%iout)
    !
    ! -- validate package netcdf inputs
    if (this%readasarrays) then
      call grid_loader%validate(this%ncpkg)
    else
      call list_loader%validate(this%ncpkg)
    end if
    !
    ! -- return
    return
  end subroutine dynamic_create_loader

  subroutine dynamic_destroy(this)
    class(NC4DynamicPkgLoadType), intent(inout) :: this
    !
    ! -- destroy and deallocate loader
    call this%loader%destroy()
    deallocate (this%loader)
    nullify (this%loader)
    !
    ! -- deallocate input context
    call this%DynamicPkgLoadType%destroy()
    !
    ! -- return
    return
  end subroutine dynamic_destroy

  !> @brief build the type list with all model package instances
  !<
  function open_ncfile(nc_fname, iout) result(ncid)
    ! -- modules
    use MemoryManagerExtModule, only: mem_set_value
    use LoadNetCDFDataModule, only: nc_fopen
    ! -- dummy
    character(len=*) :: nc_fname
    integer(I4B) :: iout
    ! -- result
    integer(I4B) :: ncid
    ! -- local
    logical(LGP) :: exists
    !
    ! -- initialize
    ncid = 0
    !
    ! -- check if NETCDF file exists
    inquire (file=nc_fname, exist=exists)
    if (.not. exists) then
      write (errmsg, '(a,a,a)') 'Specified NetCDF4 input file does &
        &not exist [file=', trim(nc_fname), '].'
      call store_error(errmsg, .true.)
    end if
    !
    ! -- open
    ncid = nc_fopen(nc_fname, iout)
    !
    ! -- return
    return
  end function open_ncfile

  subroutine inlist_init(this, mf6_input, modelname, modelfname, &
                         source, iperblock, iout)
    ! -- modules
    ! -- dummy
    class(NCListInputType), intent(inout) :: this
    type(ModflowInputType), intent(in) :: mf6_input
    character(len=*), intent(in) :: modelname
    character(len=*), intent(in) :: modelfname
    character(len=*), intent(in) :: source
    integer(I4B), intent(in) :: iperblock
    integer(I4B), intent(in) :: iout
    !
    ! --initialize
    call this%DynamicPkgLoadType%init(mf6_input, modelname, modelfname, &
                                      source, iperblock, iout)
    this%iiper = 0
    !
    ! -- initialize context
    call this%bndctx%init(this%mf6_input, .false.)
    !
    ! -- allocate bound params
    call this%bndctx%alloc(this%sourcename)
    call this%bndctx%filtered_params(this%param_names, this%nparam)
    !
    ! -- return
    return
  end subroutine inlist_init

  subroutine inlist_rp(this, ncid, ncpkg)
    ! -- modules
    use LoadNetCDFDataModule, only: nc4_rp_list
    ! -- dummy
    class(NCListInputType), intent(inout) :: this
    integer(I4B), intent(in) :: ncid
    type(NC4ModelPackageInputType), pointer, intent(inout) :: ncpkg
    !
    ! -- increment iiper
    this%iiper = this%iiper + 1
    !
    ! -- dynamic load
    call nc4_rp_list(this%mf6_input, ncpkg, this%bndctx, ncid, &
                     this%iiper, this%sourcename, this%iout)
    !
    ! -- return
    return
  end subroutine inlist_rp

  subroutine inlist_validate(this, ncpkg)
    ! -- modules
    use MemoryManagerModule, only: mem_setptr
    use InputDefinitionModule, only: InputParamDefinitionType
    use DefinitionSelectModule, only: get_param_definition_type
    ! -- dummy
    class(NCListInputType), intent(inout) :: this
    type(NC4ModelPackageInputType), pointer, intent(inout) :: ncpkg
    ! -- local
    type(InputParamDefinitionType), pointer :: idt
    integer(I4B) :: iparam, ivar
    character(len=LINELENGTH) :: tagname, varname
    logical(LGP) :: found
    !
    ! -- verify required package variables are defined
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
          varname = ncpkg%blocklist(this%iperblock)%varnames(ivar)
          if (varname == tagname) then
            found = .true.
            exit
          end if
        end do
        !
        if (.not. found) then
          errmsg = 'Required PERIOD package variable not found => '//trim(tagname)
          call store_error(errmsg)
          call store_error_filename(this%sourcename)
        end if
      end if
    end do
    !
    ! -- return
    return
  end subroutine inlist_validate

  subroutine inlist_destroy(this)
    ! -- modules
    ! -- dummy
    class(NCListInputType), intent(inout) :: this
    if (allocated(this%param_names)) deallocate (this%param_names)
    call this%bndctx%destroy()
  end subroutine inlist_destroy

  subroutine ingrid_init(this, mf6_input, modelname, modelfname, &
                         source, iperblock, iout)
    ! -- modules
    ! -- dummy
    class(NCGridInputType), intent(inout) :: this
    type(ModflowInputType), intent(in) :: mf6_input
    character(len=*), intent(in) :: modelname
    character(len=*), intent(in) :: modelfname
    character(len=*), intent(in) :: source
    integer(I4B), intent(in) :: iperblock
    integer(I4B), intent(in) :: iout
    !
    ! -- initialize context
    call this%DynamicPkgLoadType%init(mf6_input, modelname, modelfname, &
                                      source, iperblock, iout)
    !
    this%iiper = 0
    !
    call this%bndctx%init(this%mf6_input, .true.)
    !
    ! -- allocate bound params
    call this%alloc()
    !
    ! -- return
    return
  end subroutine ingrid_init

  subroutine ingrid_alloc(this)
    ! -- modules
    use MemoryManagerModule, only: mem_setptr
    ! -- dummy
    class(NCGridInputType), intent(inout) :: this !< StressGridInputType
    character(len=LENVARNAME) :: rs_varname
    integer(I4B), pointer :: intvar
    integer(I4B) :: iparam
    !
    ! -- allocate period dfn params
    call this%bndctx%alloc(this%sourcename)
    !
    ! -- set in scope param names
    call this%bndctx%filtered_params(this%param_names, this%nparam)
    !
    ! -- allocate and set param_reads pointer array
    allocate (this%param_reads(this%nparam))
    !
    ! store read state variable pointers
    do iparam = 1, this%nparam
      ! -- allocate and store name of read state variable
      rs_varname = this%bndctx%rsv_alloc(this%param_names(iparam))
      call mem_setptr(intvar, rs_varname, this%mf6_input%mempath)
      this%param_reads(iparam)%invar => intvar
      this%param_reads(iparam)%invar = 0
    end do
    !
    ! -- return
    return
  end subroutine ingrid_alloc

  subroutine ingrid_rp(this, ncid, ncpkg)
    ! -- modules
    use LoadNetCDFDataModule, only: nc4_rp_array
    ! -- dummy
    class(NCGridInputType), intent(inout) :: this
    integer(I4B), intent(in) :: ncid
    type(NC4ModelPackageInputType), pointer, intent(inout) :: ncpkg
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
    call nc4_rp_array(this%mf6_input, ncpkg, this%param_names, &
                      this%param_reads, this%bndctx, &
                      ncid, this%sourcename, this%iout)
    !
    ! -- return
    return
  end subroutine ingrid_rp

  subroutine ingrid_validate(this, ncpkg)
    ! -- modules
    use InputDefinitionModule, only: InputParamDefinitionType
    use DefinitionSelectModule, only: get_param_definition_type
    ! -- dummy
    class(NCGridInputType), intent(inout) :: this
    type(NC4ModelPackageInputType), pointer, intent(inout) :: ncpkg
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
          varname = ncpkg%blocklist(this%iperblock)%varnames(ivar)
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
          call store_error_filename(this%sourcename)
        end if
      end if
      !
    end do
    !
    ! -- return
    return
  end subroutine ingrid_validate

  subroutine ingrid_destroy(this)
    ! -- modules
    ! -- dummy
    class(NCGridInputType), intent(inout) :: this
    !
    ! -- deallocate allocatables
    if (allocated(this%param_names)) deallocate (this%param_names)
    if (allocated(this%param_reads)) deallocate (this%param_reads)
    !
    ! -- destroy bnd context object
    call this%bndctx%destroy()
    !
    ! -- return
    return
  end subroutine ingrid_destroy

end module IdmNetCDFFileModule
