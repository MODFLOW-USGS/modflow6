!> @brief This module contains the IdmNCFileModule
!!
!! This module contains the high-level routines for loading
!! a MODFLOW netcdf input file to the input context.
!!
!<
module IdmNCFileModule

  use KindModule, only: DP, I4B, LGP
  use SimModule, only: store_error, store_error_filename
  use SimVariablesModule, only: errmsg
  use InputLoadTypeModule, only: StaticPkgLoadBaseType, DynamicPkgLoadBaseType
  use NCModelInputsModule, only: NCModelInputsType, NCModelPackageInputType
  use InputLoadTypeModule, only: DynamicPkgLoadType
  use NCInputLoadTypeModule, only: NCDynamicPkgLoadBaseType
  use ModflowInputModule, only: ModflowInputType

  implicit none
  private
  public :: open_ncfile
  public :: NCStaticPkgLoadType

  !> @brief NC file static loader type
  !<
  type, extends(StaticPkgLoadBaseType) :: NCStaticPkgLoadType
    integer(I4B) :: ncid !< netcdf input file id
    type(NCModelPackageInputType), pointer :: ncpkg => null() !< description of package input
  contains
    procedure :: init => static_init
    procedure :: load => static_load
  end type NCStaticPkgLoadType

  !> @brief NC file dynamic loader type
  !<
  type, extends(DynamicPkgLoadBaseType) :: NCDynamicPkgLoadType
    integer(I4B), pointer :: iper => null() !< memory managed variable, loader iper
    integer(I4B), pointer :: ionper => null() !< memory managed variable, next load period
    integer(I4B) :: iiper !< index of package variable iper array
    integer(I4B) :: ncid !< netcdf input file id
    type(NCModelPackageInputType), pointer :: ncpkg => null() !< description of package input
    class(NCDynamicPkgLoadBaseType), pointer :: rp_loader => null() !< input specific dynamic loader
  contains
    procedure :: init => dynamic_init
    procedure :: set => dynamic_set
    procedure :: df => dynamic_df
    procedure :: ad => dynamic_ad
    procedure :: rp => dynamic_rp
    procedure :: create_loader => dynamic_create_loader
    procedure :: destroy => dynamic_destroy
  end type NCDynamicPkgLoadType

contains

  !> @brief static loader init
  !<
  subroutine static_init(this, mf6_input, component_name, component_input_name, &
                         input_name)
    use InputModelContextModule, only: GetModelNCContext
    class(NCStaticPkgLoadType), intent(inout) :: this
    type(ModflowInputType), intent(in) :: mf6_input
    character(len=*), intent(in) :: component_name
    character(len=*), intent(in) :: component_input_name
    character(len=*), intent(in) :: input_name
    type(NCModelInputsType), pointer :: nc_context
    !
    call this%StaticPkgLoadType%init(mf6_input, component_name, &
                                     component_input_name, &
                                     input_name)
    !
    ! -- set context from input file
    nc_context => GetModelNCContext(component_name)
    !
    ! -- cache ncid for model netcdf4 file
    this%ncid = nc_context%ncid
    !
    ! -- set package description from context
    this%ncpkg => &
      nc_context%get_package(mf6_input%component_name, &
                             mf6_input%subcomponent_name)
    ! -- return
    return
  end subroutine static_init

  !> @brief static loader load routine
  !<
  function static_load(this, iout) result(period_loader)
    use LoadNCFileModule, only: input_load
    class(NCStaticPkgLoadType), intent(inout) :: this
    integer(I4B), intent(in) :: iout
    class(DynamicPkgLoadBaseType), pointer :: period_loader
    class(NCDynamicPkgLoadType), pointer :: nc_loader => null()
    !
    ! -- initialize
    nullify (period_loader)
    !
    ! -- load model package to input context
    call input_load(this%mf6_input, this%ncpkg, this%ncid, this%input_name, iout)
    !
    ! -- check if package is dynamic
    if (this%iperblock > 0) then
      !
      ! -- create dynamic loader
      allocate (nc_loader)
      !
      ! -- initailze loader
      call nc_loader%init(this%mf6_input, this%component_name, &
                          this%component_input_name, this%input_name, &
                          this%iperblock, iout)
      !
      ! -- set period data
      call nc_loader%set(this%ncpkg, this%ncid)
      !
      ! -- set returned base pointer
      period_loader => nc_loader
      !
    end if
    !
    ! -- return
    return
  end function static_load

  !> @brief dynamic loader init
  !<
  subroutine dynamic_init(this, mf6_input, component_name, component_input_name, &
                          input_name, iperblock, iout)
    use MemoryManagerModule, only: mem_allocate
    class(NCDynamicPkgLoadType), intent(inout) :: this
    type(ModflowInputType), intent(in) :: mf6_input
    character(len=*), intent(in) :: component_name
    character(len=*), intent(in) :: component_input_name
    character(len=*), intent(in) :: input_name
    integer(I4B), intent(in) :: iperblock
    integer(I4B), intent(in) :: iout
    !
    call this%DynamicPkgLoadType%init(mf6_input, component_name, &
                                      component_input_name, &
                                      input_name, iperblock, iout)
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

  !> @brief dynamic loader set
  !!
  !! Set NC package info and file descriptor
  !!
  !<
  subroutine dynamic_set(this, ncpkg, ncid)
    use MemoryManagerModule, only: mem_allocate
    class(NCDynamicPkgLoadType), intent(inout) :: this
    type(NCModelPackageInputType), pointer, intent(in) :: ncpkg
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
      call store_error_filename(this%input_name)
    end if
    !
    ! -- create the loader
    call this%create_loader()
    !
    ! -- return
    return
  end subroutine dynamic_set

  !> @brief dynamic loader define
  !<
  subroutine dynamic_df(this)
    use TdisModule, only: nper
    class(NCDynamicPkgLoadType), intent(inout) :: this
    !
    ! -- define
    call this%rp_loader%df()
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

  !> @brief dynamic loader advance
  !<
  subroutine dynamic_ad(this)
    class(NCDynamicPkgLoadType), intent(inout) :: this
    !
    ! -- advance
    call this%rp_loader%ad()
    !
    ! -- return
    return
  end subroutine dynamic_ad

  !> @brief dynamic loader read and prepare
  !<
  subroutine dynamic_rp(this)
    use TdisModule, only: kper, nper
    class(NCDynamicPkgLoadType), intent(inout) :: this
    !
    ! -- check if ready to load
    if (this%ionper /= kper) return
    !
    ! -- package dynamic load
    call this%rp_loader%rp(this%ncid, this%ncpkg)
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
  end subroutine dynamic_rp

  !> @brief allocate a dynamic loader based on load context
  !<
  subroutine dynamic_create_loader(this)
    use NCFileListInputModule, only: NCBoundListInputType
    use NCFileGridInputModule, only: NCBoundGridInputType
    ! -- dummy
    class(NCDynamicPkgLoadType), intent(inout) :: this
    class(NCBoundListInputType), pointer :: bndlist_loader
    class(NCBoundGridInputType), pointer :: bndgrid_loader
    !
    ! -- allocate and set loader
    if (this%readasarrays) then
      allocate (bndgrid_loader)
      this%rp_loader => bndgrid_loader
    else
      allocate (bndlist_loader)
      this%rp_loader => bndlist_loader
    end if
    !
    ! -- initialize loader
    call this%rp_loader%init(this%mf6_input, &
                             this%component_name, &
                             this%component_input_name, &
                             this%input_name, &
                             this%iperblock, &
                             this%iout)
    !
    ! -- validate package netcdf inputs
    call this%rp_loader%validate(this%ncpkg)
    !
    ! -- return
    return
  end subroutine dynamic_create_loader

  !> @brief dynamic loader destroy
  !<
  subroutine dynamic_destroy(this)
    class(NCDynamicPkgLoadType), intent(inout) :: this
    !
    ! -- destroy and deallocate loader
    call this%rp_loader%destroy()
    deallocate (this%rp_loader)
    nullify (this%rp_loader)
    !
    ! -- deallocate input context
    call this%DynamicPkgLoadType%destroy()
    !
    ! -- return
    return
  end subroutine dynamic_destroy

  !> @brief open netcdf file
  !<
  function open_ncfile(nc_fname, iout) result(ncid)
    ! -- modules
    use MemoryManagerExtModule, only: mem_set_value
    use LoadNCFileModule, only: nc_fopen
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

end module IdmNCFileModule
