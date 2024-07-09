!> @brief This module contains the NCFileVarsModule
!!
!! These data structures organize package input information
!! associated with a single model netcdf input file.
!!
!<
module NCFileVarsModule

  use KindModule, only: DP, I4B, LGP
  use ConstantsModule, only: LINELENGTH
  use SimVariablesModule, only: errmsg
  use SimModule, only: store_error, store_error_filename
  use ListModule, only: ListType

  implicit none
  private
  public :: NCFileVarsType
  public :: NCPackageVarsType

  !> @brief Type describing input variables for a package in NetCDF file
  !<
  type :: NCPackageVarsType
    character(len=LINELENGTH), dimension(:), allocatable :: tagnames !< variable tag name
    integer(I4B), dimension(:), allocatable :: layers !< variable layer
    integer(I4B), dimension(:), allocatable :: varids !< netcdf file variable id
    character(len=LINELENGTH), pointer :: grid => null()
    character(len=LINELENGTH), pointer :: nc_fname => null()
    integer(I4B), pointer :: ncid => null()
  contains
    procedure :: init => ncvars_init
    procedure :: varid => ncvars_varid
    procedure :: destroy => ncvars_destroy
  end type NCPackageVarsType

  !> @brief Type which describes a modflow input variable in a netcdf file
  !<
  type :: NCFileMf6VarType
    character(LINELENGTH) :: pkgname !< package name
    character(LINELENGTH) :: tagname !< tag name
    integer(I4B) :: layer !< variable layer
    integer(I4B) :: varid !< NC file variable id
  contains
  end type NCFileMf6VarType

  !> @brief Type describing modflow6 input variables in model NetCDF file
  !<
  type :: NCFileVarsType
    type(ListType) :: mf6invar !< list of modflow 6 input variables in netcdf file
    character(len=LINELENGTH), dimension(:), allocatable :: pkgnames !< packages in file
    integer(I4B), dimension(:), allocatable :: pkgcounts !< variable counts in each package
    character(len=LINELENGTH), pointer :: grid => null()
    character(len=LINELENGTH), pointer :: nc_fname => null()
    integer(I4B), pointer :: ncid => null()
  contains
    procedure :: init => fv_init
    procedure :: add => fv_add
    procedure :: get => fv_get
    procedure :: destroy => fv_destroy
    procedure :: create_varlists
  end type NCFileVarsType

contains

  !> @brief create netcdf package variable lists
  !<
  subroutine ncvars_init(this)
    ! -- modules
    ! -- dummy
    class(NCPackageVarsType) :: this
    ! -- local
    !
    ! -- allocate empty arrays
    allocate (this%tagnames(0))
    allocate (this%layers(0))
    allocate (this%varids(0))
    !
    ! -- return
    return
  end subroutine ncvars_init

  !> @brief return a netcdf variable id for a package tagname
  !<
  function ncvars_varid(this, tagname, layer) result(varid)
    ! -- modules
    ! -- dummy
    class(NCPackageVarsType) :: this
    character(len=*), intent(in) :: tagname
    integer(I4B) :: layer
    ! -- return
    integer(I4B) :: varid
    ! -- local
    integer(I4B) :: n
    !
    ! -- initialize
    varid = -1
    !
    do n = 1, size(this%tagnames)
      if (this%tagnames(n) == tagname .and. &
          this%layers(n) == layer) then
        varid = this%varids(n)
        exit
      end if
    end do
    !
    ! -- return
    return
  end function ncvars_varid

  !> @brief destroy netcdf package variable lists
  !<
  subroutine ncvars_destroy(this)
    ! -- modules
    ! -- dummy
    class(NCPackageVarsType) :: this
    ! -- local
    !
    ! -- deallocate dynamic arrays
    if (allocated(this%tagnames)) deallocate (this%tagnames)
    if (allocated(this%layers)) deallocate (this%layers)
    if (allocated(this%varids)) deallocate (this%varids)
    !
    ! -- return
    return
  end subroutine ncvars_destroy

  !> @brief initialize netcdf model variable description type
  !<
  subroutine fv_init(this, modelname, nc_fname, ncid, grid)
    ! -- modules
    use ConstantsModule, only: LENMEMPATH
    use MemoryManagerModule, only: mem_allocate
    use MemoryHelperModule, only: create_mem_path
    use SimVariablesModule, only: idm_context
    ! -- dummy variables
    class(NCFileVarsType) :: this
    character(len=*), intent(in) :: modelname
    character(len=*), intent(in) :: nc_fname
    integer(I4B), intent(in) :: ncid
    character(len=*), intent(in) :: grid
    ! -- local variables
    character(len=LENMEMPATH) :: mempath
    integer(I4B) :: ilen
    !
    ! -- set mempath
    mempath = create_mem_path(component=modelname, &
                              context=idm_context)
    ! -- initialize strlen
    ilen = LINELENGTH
    !
    ! -- allocate managed memory
    call mem_allocate(this%grid, ilen, 'NETCDF_GRID', mempath)
    ! TODO now this is in 2 mempaths (also input under NAM)
    call mem_allocate(this%nc_fname, ilen, 'NETCDF_FNAME', mempath)
    call mem_allocate(this%ncid, 'NCID', mempath)
    !
    ! -- allocate local memory
    allocate (this%pkgnames(0))
    allocate (this%pkgcounts(0))
    !
    ! -- set
    this%grid = grid
    this%nc_fname = nc_fname
    this%ncid = ncid
    !
    ! -- return
    return
  end subroutine fv_init

  !> @brief add netcdf modflow6 input variable to list
  !<
  subroutine fv_add(this, pkgname, tagname, layer, varid)
    ! -- modules
    use ArrayHandlersModule, only: expandarray
    ! -- dummy variables
    class(NCFileVarsType) :: this
    character(len=*), intent(in) :: pkgname
    character(len=*), intent(in) :: tagname
    integer(I4B), intent(in) :: layer
    integer(I4B), intent(in) :: varid
    ! -- local variables
    class(NCFileMf6VarType), pointer :: invar
    class(*), pointer :: obj
    integer(I4B) :: n, pidx
    !
    ! -- initialize package index
    pidx = 0
    !
    do n = 1, size(this%pkgnames)
      if (this%pkgnames(n) == pkgname) then
        pidx = n
        exit
      end if
    end do
    !
    ! -- add new package type
    if (pidx == 0) then
      call expandarray(this%pkgnames)
      call expandarray(this%pkgcounts)
      pidx = size(this%pkgnames)
      this%pkgnames(pidx) = pkgname
      this%pkgcounts(pidx) = 0
    end if
    !
    ! --increment pkgcount
    this%pkgcounts(pidx) = this%pkgcounts(pidx) + 1
    !
    ! -- add mf6 variable to file list
    allocate (invar)
    invar%pkgname = pkgname
    invar%tagname = tagname
    invar%layer = layer
    invar%varid = varid
    !
    obj => invar
    call this%mf6invar%Add(obj)
    !
    ! -- return
    return
  end subroutine fv_add

  !> @brief get modflow6 input variable description at position idx
  !<
  function fv_get(this, idx) result(res)
    ! -- dummy variables
    class(NCFileVarsType) :: this
    integer(I4B), intent(in) :: idx !< package number
    class(NCFileMf6VarType), pointer :: res
    ! -- local variables
    class(*), pointer :: obj
    !
    ! -- initialize res
    res => null()
    !
    ! -- get the package from the list
    obj => this%mf6invar%GetItem(idx)
    if (associated(obj)) then
      select type (obj)
      class is (NCFileMf6VarType)
        res => obj
      end select
    end if
    !
    ! -- return
    return
  end function fv_get

  !> @brief destroy netcdf model variable description type
  !<
  subroutine fv_destroy(this)
    ! -- modules
    ! -- dummy
    class(NCFileVarsType) :: this
    class(NCFileMf6VarType), pointer :: invar
    integer(I4B) :: n
    !
    do n = 1, this%mf6invar%Count()
      invar => this%get(n)
      deallocate (invar)
      nullify (invar)
    end do
    !
    call this%mf6invar%Clear()
    !
    if (allocated(this%pkgnames)) deallocate (this%pkgnames)
    if (allocated(this%pkgcounts)) deallocate (this%pkgcounts)
    !
    return
  end subroutine fv_destroy

  !> @brief create list of variables that correspond to a package
  !<
  subroutine create_varlists(this, pkgname, nc_vars)
    ! -- modules
    ! -- dummy
    class(NCFileVarsType) :: this
    character(len=*), intent(in) :: pkgname
    type(NCPackageVarsType), pointer, intent(inout) :: nc_vars
    integer(I4B) :: n, cnt, pidx
    ! -- local
    class(NCFileMf6VarType), pointer :: invar
    !
    ! -- initialize
    cnt = 0
    pidx = 0
    !
    ! -- deallocate incoming lists
    if (allocated(nc_vars%tagnames)) deallocate (nc_vars%tagnames)
    if (allocated(nc_vars%layers)) deallocate (nc_vars%layers)
    if (allocated(nc_vars%varids)) deallocate (nc_vars%varids)
    !
    do n = 1, size(this%pkgnames)
      if (this%pkgnames(n) == pkgname) then
        pidx = n
        exit
      end if
    end do
    !
    if (pidx > 0) then
      ! -- package has NCFile variables
      !
      allocate (nc_vars%tagnames(this%pkgcounts(pidx)))
      allocate (nc_vars%layers(this%pkgcounts(pidx)))
      allocate (nc_vars%varids(this%pkgcounts(pidx)))
      !
      do n = 1, this%mf6invar%count()
        invar => this%get(n)
        if (invar%pkgname == pkgname) then
          cnt = cnt + 1
          nc_vars%tagnames(cnt) = invar%tagname
          nc_vars%layers(cnt) = invar%layer
          nc_vars%varids(cnt) = invar%varid
        end if
      end do
      !
      ! -- set file attribute pointers
      nc_vars%ncid => this%ncid
      nc_vars%nc_fname => this%nc_fname
      nc_vars%grid => this%grid
    else
      allocate (nc_vars%tagnames(0))
      allocate (nc_vars%layers(0))
      allocate (nc_vars%varids(0))
    end if
    !
    ! -- return
    return
  end subroutine create_varlists

end module NCFileVarsModule
