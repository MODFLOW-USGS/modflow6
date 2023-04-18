!> @brief This module contains the ModelPackageInputsModule
!!
!! This module contains the high-level routines for assembling
!! model package information and loading to the input context
!!
!<
module ModelPackageInputsModule

  use KindModule, only: DP, I4B, LGP
  use ConstantsModule, only: LINELENGTH, LENMEMPATH, LENMODELNAME, LENFTYPE, &
                             LENPACKAGETYPE, LENPACKAGENAME
  use SimModule, only: store_error, store_error_filename
  use SimVariablesModule, only: iout
  use ArrayHandlersModule, only: expandarray
  use CharacterStringModule, only: CharacterStringType

  implicit none
  private
  public :: NIUNIT_GWF, NIUNIT_GWT, NIUNIT_GWE
  public :: ModelPackageInputsType

  ! -- GWF base package types, ordered for memload
  integer(I4B), parameter :: GWF_NBASEPKG = 50
  character(len=LENPACKAGETYPE), dimension(GWF_NBASEPKG) :: GWF_BASEPKG
  data GWF_BASEPKG/'DIS6 ', 'DISV6', 'DISU6', '     ', '     ', & !  5
                  &'NPF6 ', 'BUY6 ', 'VSC6 ', 'GNC6 ', '     ', & ! 10
                  &'HFB6 ', 'STO6 ', 'IC6  ', '     ', '     ', & ! 15
                  &'MVR6 ', 'OC6  ', 'OBS6 ', '     ', '     ', & ! 20
                  &30*'     '/ ! 50

  ! -- GWF multi package types, ordered for memload
  integer(I4B), parameter :: GWF_NMULTIPKG = 50
  character(len=LENPACKAGETYPE), dimension(GWF_NMULTIPKG) :: GWF_MULTIPKG
  data GWF_MULTIPKG/'WEL6 ', 'DRN6 ', 'RIV6 ', 'GHB6 ', '     ', & !  5
                   &'RCH6 ', 'EVT6 ', 'CHD6 ', 'CSUB6', '     ', & ! 10
                   &'MAW6 ', 'SFR6 ', 'LAK6 ', 'UZF6 ', 'API6 ', & ! 15
                   &35*'     '/ ! 50

  ! -- GWT base package types, ordered for memload
  integer(I4B), parameter :: GWT_NBASEPKG = 50
  character(len=LENPACKAGETYPE), dimension(GWT_NBASEPKG) :: GWT_BASEPKG
  data GWT_BASEPKG/'DIS6 ', 'DISV6', 'DISU6', '     ', '     ', & !  5
                  &'IC6  ', 'FMI6 ', 'MST6 ', 'ADV6 ', '     ', & ! 10
                  &'DSP6 ', 'SSM6 ', 'MVT6 ', 'OC6  ', '     ', & ! 15
                  &'OBS6 ', '     ', '     ', '     ', '     ', & ! 20
                  &30*'     '/ ! 50

  ! -- GWT multi package types, ordered for memload
  integer(I4B), parameter :: GWT_NMULTIPKG = 50
  character(len=LENPACKAGETYPE), dimension(GWT_NMULTIPKG) :: GWT_MULTIPKG
  data GWT_MULTIPKG/'CNC6 ', 'SRC6 ', 'LKT6 ', 'IST6 ', '     ', & !  5
                   &'SFT6 ', 'MWT6 ', 'UZT6 ', 'API6 ', '     ', & ! 10
                   &40*'     '/ ! 50

  ! -- GWE base package types, ordered for memload
  integer(I4B), parameter :: GWE_NBASEPKG = 50
  character(len=LENPACKAGETYPE), dimension(GWE_NBASEPKG) :: GWE_BASEPKG
  data GWE_BASEPKG/'DIS6 ', 'DISV6', 'DISU6', '     ', '     ', & !  5
                  &'IC6  ', 'FMI6 ', 'MST6 ', 'ADV6 ', '     ', & ! 10
                  &'DSP6 ', 'SSM6 ', 'MVT6 ', 'OC6  ', '     ', & ! 15
                  &'OBS6 ', '     ', '     ', '     ', '     ', & ! 20
                  &30*'     '/ ! 50

  ! -- GWE multi package types, ordered for memload
  integer(I4B), parameter :: GWE_NMULTIPKG = 50
  character(len=LENPACKAGETYPE), dimension(GWE_NMULTIPKG) :: GWE_MULTIPKG
  data GWE_MULTIPKG/'TMP6 ', 'SRC6 ', 'LKE6 ', '     ', '     ', & !  5
                   &'SFE6 ', 'MWE6 ', 'UZE6 ', 'API6 ', '     ', & ! 10
                   &40*'     '/ ! 50

  ! -- size of supported model package arrays
  integer(I4B), parameter :: NIUNIT_GWF = GWF_NBASEPKG + GWF_NMULTIPKG
  integer(I4B), parameter :: NIUNIT_GWT = GWT_NBASEPKG + GWT_NMULTIPKG
  integer(I4B), parameter :: NIUNIT_GWE = GWE_NBASEPKG + GWE_NMULTIPKG

  !> @brief derived type for loadable package type
  !!
  !!  This derived type is used to store package instance
  !!  desriptions for a supported package type.
  !!
  !<
  type :: LoadablePackageType
    ! -- package type, e.g. 'DIS6 or CHD6'
    character(len=LENPACKAGETYPE) :: pkgtype
    ! -- component type, e.g. 'DIS or CHD'
    character(len=LENFTYPE) :: component_type
    ! -- package instance attribute arrays
    character(len=LINELENGTH), dimension(:), allocatable :: filenames
    character(len=LENPACKAGENAME), dimension(:), allocatable :: pkgnames
    character(len=LENMEMPATH), dimension(:), allocatable :: mempaths
    integer(I4B), dimension(:), allocatable :: inunits
    ! -- number of package instances
    integer(I4B) :: pnum
  contains
    procedure :: create => pkgtype_create
    procedure :: add => pkgtype_add
    procedure :: destroy => pkgtype_destroy
  end type LoadablePackageType

  !> @brief derived type for model package inputs type
  !!
  !!  This derived type is used to define input package
  !!  descriptors for a model and load to managed memory.
  !!
  !<
  type :: ModelPackageInputsType
    ! -- model attributes
    character(len=LENPACKAGETYPE) :: modeltype ! -- model type, e.g. 'GWF6'
    character(len=LINELENGTH) :: modelfname
    character(len=LENMODELNAME) :: modelname
    ! -- component type
    character(len=LENFTYPE) :: component_type ! -- e.g. 'GWF'
    ! -- model mempath
    character(len=LENMEMPATH) :: model_mempath
    ! -- pointers to created managed memory
    type(CharacterStringType), dimension(:), contiguous, &
      pointer :: pkgtypes => null()
    type(CharacterStringType), dimension(:), contiguous, &
      pointer :: pkgnames => null()
    type(CharacterStringType), dimension(:), contiguous, &
      pointer :: mempaths => null()
    integer(I4B), dimension(:), contiguous, &
      pointer :: inunits => null()
    ! -- loadable package type array
    type(LoadablePackageType), dimension(:), allocatable :: pkglist
    ! -- pkgtype definitions
    integer(I4B) :: niunit
    character(len=LENPACKAGETYPE), dimension(:), allocatable :: cunit
    ! -- out handle
    integer(I4B) :: iout
  contains
    procedure :: init => modelpkgs_init
    procedure :: memload => modelpkgs_memload
    procedure :: destroy => modelpkgs_destroy
    procedure, private :: create => modelpkgs_create
    procedure, private :: addpkgs => modelpkgs_addpkgs
    procedure, private :: add => modelpkgs_add
    procedure, private :: pkgcount => modelpkgs_pkgcount
  end type ModelPackageInputsType

contains

  !> @brief set supported package types for model
  !<
  subroutine supported_model_packages(mtype, pkgtypes, numpkgs)
    ! -- modules
    ! -- dummy
    character(len=LENFTYPE), intent(in) :: mtype
    character(len=LENPACKAGETYPE), dimension(:), allocatable, &
      intent(inout) :: pkgtypes
    integer(I4B), intent(inout) :: numpkgs
    ! -- local
    !
    select case (mtype)
    case ('GWF6')
      numpkgs = GWF_NBASEPKG + GWF_NMULTIPKG
      allocate (pkgtypes(numpkgs))
      pkgtypes = [GWF_BASEPKG, GWF_MULTIPKG]
      !
    case ('GWT6')
      numpkgs = GWT_NBASEPKG + GWT_NMULTIPKG
      allocate (pkgtypes(numpkgs))
      pkgtypes = [GWT_BASEPKG, GWT_MULTIPKG]
      !
    case ('GWE6')
      numpkgs = GWE_NBASEPKG + GWE_NMULTIPKG
      allocate (pkgtypes(numpkgs))
      pkgtypes = [GWE_BASEPKG, GWE_MULTIPKG]
      !
    case default
    end select
    !
    ! -- return
    return
  end subroutine supported_model_packages

  !> @brief component from package or model type
  !<
  function component_type(pkgtype) !result(componenttype)
    ! -- modules
    ! -- dummy
    character(len=LENPACKAGETYPE), intent(in) :: pkgtype
    ! -- return
    character(len=LENFTYPE) :: component_type
    ! -- local
    integer(I4B) :: i, ilen
    !
    component_type = ''
    !
    ilen = len_trim(pkgtype)
    do i = 1, ilen
      if (pkgtype(i:i) == '6') then
        write (component_type, '(a)') trim(pkgtype(1:i - 1))
      end if
    end do
    !
    ! -- return
    return
  end function component_type

  !> @brief does model support multiple instances of this package type
  !<
  function multi_pkg_type(mtype_component, ptype_component, pkgtype) &
    result(multi_pkg)
    ! -- modules
    use IdmDfnSelectorModule, only: idm_integrated, idm_multi_package
    ! -- dummy
    character(len=LENFTYPE), intent(in) :: mtype_component
    character(len=LENFTYPE), intent(in) :: ptype_component
    character(len=LENFTYPE), intent(in) :: pkgtype
    ! -- return
    logical(LGP) :: multi_pkg
    ! -- local
    integer(I4B) :: n
    !
    multi_pkg = .false.
    !
    if (idm_integrated(mtype_component, ptype_component)) then
      !
      multi_pkg = idm_multi_package(mtype_component, ptype_component)
      !
    else
      !
      select case (mtype_component)
      case ('GWF')
        do n = 1, GWF_NMULTIPKG
          if (GWF_MULTIPKG(n) == pkgtype) then
            multi_pkg = .true.
            exit
          end if
        end do
        !
      case ('GWT')
        do n = 1, GWT_NMULTIPKG
          if (GWT_MULTIPKG(n) == pkgtype) then
            multi_pkg = .true.
            exit
          end if
        end do
        !
      case ('GWE')
        do  n = 1, GWE_NMULTIPKG
          if (GWE_MULTIPKG(n) == pkgtype) then
            multi_pkg = .true.
            exit
          end if
        end do
        !
      case default
      end select
    end if
    !
    ! -- return
    return
  end function multi_pkg_type

  !> @brief create a new package type
  !<
  subroutine pkgtype_create(this, modelname, pkgtype)
    ! -- modules
    ! -- dummy
    class(LoadablePackageType) :: this
    character(len=*), intent(in) :: modelname
    character(len=*), intent(in) :: pkgtype
    ! -- local
    !
    ! -- initialize
    this%pkgtype = pkgtype
    this%component_type = component_type(pkgtype)
    this%pnum = 0
    !
    ! -- allocate arrays
    allocate (this%filenames(0))
    allocate (this%pkgnames(0))
    allocate (this%mempaths(0))
    allocate (this%inunits(0))
    !
    ! -- return
    return
  end subroutine pkgtype_create

  !> @brief add a new package instance to this package type
  !<
  subroutine pkgtype_add(this, modelname, mtype_component, filetype, &
                         filename, pkgname, iout)
    ! -- modules
    use MemoryManagerModule, only: mem_allocate
    use MemoryHelperModule, only: create_mem_path
    use SimVariablesModule, only: idm_context
    use IdmDfnSelectorModule, only: idm_integrated, idm_multi_package
    ! -- dummy
    class(LoadablePackageType) :: this
    character(len=*), intent(in) :: modelname
    character(len=*), intent(in) :: mtype_component
    character(len=*), intent(in) :: filetype
    character(len=*), intent(in) :: filename
    character(len=*), intent(in) :: pkgname
    integer(I4B), intent(in) :: iout
    ! -- local
    character(len=LENPACKAGENAME) :: sc_name
    character(len=LENMEMPATH) :: mempath
    character(len=LINELENGTH), pointer :: cstr
    !
    ! -- reallocate
    call expandarray(this%filenames)
    call expandarray(this%pkgnames)
    call expandarray(this%inunits)
    call expandarray(this%mempaths)
    !
    ! -- add new package instance
    this%pnum = this%pnum + 1
    this%filenames(this%pnum) = filename
    this%pkgnames(this%pnum) = pkgname
    this%inunits(this%pnum) = 0
    !
    ! -- set up input context for model
    if (idm_integrated(mtype_component, this%component_type)) then
      !
      ! -- set subcomponent name
      if (idm_multi_package(mtype_component, this%component_type)) then
        !
        sc_name = pkgname
      else
        !
        sc_name = this%component_type
      end if
      !
      ! -- create and store the mempath
      this%mempaths(this%pnum) = &
        create_mem_path(modelname, sc_name, idm_context)
      !
      ! -- allocate and initialize filename for package
      mempath = create_mem_path(modelname, sc_name, idm_context)
      call mem_allocate(cstr, LINELENGTH, 'INPUT_FNAME', mempath)
      cstr = filename
    else
      !
      ! -- set mempath empty
      this%mempaths(this%pnum) = ''
    end if
    !
    ! -- return
    return
  end subroutine pkgtype_add

  !> @brief deallocate object
  !<
  subroutine pkgtype_destroy(this)
    ! -- modules
    ! -- dummy
    class(LoadablePackageType) :: this
    ! -- local
    !
    ! -- deallocate dynamic arrays
    deallocate (this%filenames)
    deallocate (this%pkgnames)
    deallocate (this%inunits)
    deallocate (this%mempaths)
    !
    ! -- return
    return
  end subroutine pkgtype_destroy

  !> @brief initialize model package inputs object
  !<
  subroutine modelpkgs_init(this, modeltype, modelfname, modelname, iout)
    ! -- modules
    use MemoryHelperModule, only: create_mem_path
    use MemoryManagerModule, only: mem_allocate
    use SimVariablesModule, only: idm_context
    ! -- dummy
    class(ModelPackageInputsType) :: this
    character(len=*), intent(in) :: modeltype
    character(len=*), intent(in) :: modelfname
    character(len=*), intent(in) :: modelname
    integer(I4B), intent(in) :: iout
    ! -- local
    !
    ! -- initialize object
    this%modeltype = modeltype
    this%modelfname = modelfname
    this%modelname = modelname
    this%component_type = component_type(modeltype)
    this%iout = iout
    !
    ! -- allocate and set model supported package types
    call supported_model_packages(modeltype, this%cunit, this%niunit)
    !
    ! -- set model memory path
    this%model_mempath = create_mem_path(component=this%modelname, &
                                         context=idm_context)
    !
    ! -- allocate managed memory
    call mem_allocate(this%pkgtypes, LENPACKAGETYPE, 0, 'PKGTYPES', &
                      this%model_mempath)
    call mem_allocate(this%pkgnames, LENPACKAGENAME, 0, 'PKGNAMES', &
                      this%model_mempath)
    call mem_allocate(this%mempaths, LENMEMPATH, 0, 'MEMPATHS', &
                      this%model_mempath)
    call mem_allocate(this%inunits, 0, 'INUNITS', this%model_mempath)
    !
    ! build descriptions of packages
    call this%addpkgs()
    !
    ! -- return
    return
  end subroutine modelpkgs_init

  !> @brief create the package type list
  !<
  subroutine modelpkgs_create(this, ftypes)
    ! -- modules
    use SortModule, only: qsort
    ! -- dummy
    class(ModelPackageInputsType) :: this
    type(CharacterStringType), dimension(:), contiguous, &
      pointer :: ftypes
    ! -- local
    integer(I4B), dimension(:), allocatable :: cunit_idxs, indx
    character(len=LENPACKAGETYPE) :: ftype
    integer(I4B) :: n, m
    logical(LGP) :: found
    character(len=LINELENGTH) :: errmsg
    !
    ! -- allocate
    allocate (cunit_idxs(0))
    !
    ! -- identify input packages and check that each is supported
    do n = 1, size(ftypes)
      !
      ! -- type from model name file packages block
      ftype = ftypes(n)
      found = .false.
      !
      ! -- search supported types for this filetype
      do m = 1, this%niunit
        if (this%cunit(m) == ftype) then
          ! -- set found
          found = .true.
          !
          ! -- add to cunit list if first instance of this type
          if (any(cunit_idxs == m)) then
            ! no-op
          else
            call expandarray(cunit_idxs)
            cunit_idxs(size(cunit_idxs)) = m
          end if
          !
          ! -- exit search
          exit
        end if
      end do
      !
      ! -- set error if namfile pkg filetype is not supported
      if (.not. found) then
        write (errmsg, '(a,a,a,a,a)') 'Model package type not supported &
          &[model=', trim(this%modelname), ', type=', &
          trim(ftype), '].'
        call store_error(errmsg)
        call store_error_filename(this%modelfname)
      end if
    end do
    !
    ! -- allocate the pkglist
    allocate (this%pkglist(size(cunit_idxs)))
    !
    ! -- sort cunit indexes
    allocate (indx(size(cunit_idxs)))
    call qsort(indx, cunit_idxs)
    !
    ! -- create sorted LoadablePackageType object list
    do n = 1, size(cunit_idxs)
      call this%pkglist(n)%create(this%modelname, this%cunit(cunit_idxs(n)))
    end do
    !
    ! -- cleanup
    deallocate (cunit_idxs)
    deallocate (indx)
    !
    ! -- return
    return
  end subroutine modelpkgs_create

  !> @brief add a model package instance to package type list
  !<
  subroutine modelpkgs_add(this, pkgtype, filename, pkgname)
    ! -- modules
    ! -- dummy
    class(ModelPackageInputsType) :: this
    character(len=*), intent(in) :: pkgtype
    character(len=*), intent(in) :: filename
    character(len=*), intent(in) :: pkgname
    ! -- local
    type(LoadablePackageType) :: pkg
    integer(I4B) :: n
    !
    ! -- locate index of pkgtype in pkglist
    do n = 1, size(this%pkglist)
      pkg = this%pkglist(n)
      if (pkg%pkgtype == pkgtype) then
        call this%pkglist(n)%add(this%modelname, this%component_type, &
                                 pkgtype, filename, pkgname, this%iout)
        exit
      end if
    end do
    !
    ! -- return
    return
  end subroutine modelpkgs_add

  !> @brief build the type list with all model package instances
  !<
  subroutine modelpkgs_addpkgs(this)
    ! -- modules
    use MemoryHelperModule, only: create_mem_path
    use MemoryManagerModule, only: mem_setptr
    use SimVariablesModule, only: idm_context
    ! -- dummy
    class(ModelPackageInputsType) :: this
    ! -- local
    type(CharacterStringType), dimension(:), contiguous, &
      pointer :: ftypes !< file types
    type(CharacterStringType), dimension(:), contiguous, &
      pointer :: fnames !< file names
    type(CharacterStringType), dimension(:), contiguous, &
      pointer :: pnames !< package names
    character(len=LENMEMPATH) :: input_mempath
    character(len=LINELENGTH) :: ftype, fname, pname
    integer(I4B) :: n
    !
    ! -- set input memory path
    input_mempath = create_mem_path(this%modelname, 'NAM', idm_context)
    !
    ! -- set pointers to input context model package attribute arrays
    call mem_setptr(ftypes, 'FTYPE', input_mempath)
    call mem_setptr(fnames, 'FNAME', input_mempath)
    call mem_setptr(pnames, 'PNAME', input_mempath)
    !
    ! -- create the package list
    call this%create(ftypes)
    !
    ! -- load model packages
    do n = 1, size(ftypes)
      !
      ! -- attributes for this package
      ftype = ftypes(n)
      fname = fnames(n)
      pname = pnames(n)
      !
      ! TODO: name pkg here if not provided, this is expected to cause
      !       failures for multi-pkg types when names aren't provided
      !
      ! -- add this instance to package list
      call this%add(ftype, fname, pname)
    end do
    !
    ! --
    return
  end subroutine modelpkgs_addpkgs

  !> @brief get package instance count and verify base or multi of each
  !<
  function modelpkgs_pkgcount(this) result(pnum)
    ! -- modules
    ! -- dummy
    class(ModelPackageInputsType) :: this
    !
    ! -- return
    integer(I4B) :: pnum
    ! -- local
    integer(I4B) :: n
    character(len=LINELENGTH) :: errmsg
    !
    ! -- initialize
    pnum = 0
    !
    ! -- count model package instances
    do n = 1, size(this%pkglist)
      !
      if (multi_pkg_type(this%component_type, &
                         this%pkglist(n)%component_type, &
                         this%pkglist(n)%pkgtype)) then
        ! multiple instances ok
      else
        ! -- set error for unexpected extra packages
        if (this%pkglist(n)%pnum > 1) then
          write (errmsg, '(a,a,a,a,a)') &
            'Multiple instances specified for model base package type &
            &[model=', trim(this%modelname), ', type=', &
            trim(this%pkglist(n)%pkgtype), '].'
          call store_error(errmsg)
          call store_error_filename(this%modelfname)
        end if
      end if
      !
      ! -- add to package count
      pnum = pnum + this%pkglist(n)%pnum
    end do
    !
    ! -- return
    return
  end function modelpkgs_pkgcount

  !> @brief load package descriptors to managed memory
  !<
  subroutine modelpkgs_memload(this)
    ! -- modules
    use MemoryManagerModule, only: mem_reallocate
    ! -- dummy
    class(ModelPackageInputsType) :: this
    ! -- local
    integer(I4B) :: n, m, idx
    integer(I4B) :: pnum
    !
    ! -- initialize load index
    idx = 0
    !
    ! -- set total number of package instances
    pnum = this%pkgcount()
    !
    ! -- reallocate model input package attribute arrays
    call mem_reallocate(this%pkgtypes, LENPACKAGETYPE, pnum, 'PKGTYPES', &
                        this%model_mempath)
    call mem_reallocate(this%pkgnames, LENPACKAGENAME, pnum, 'PKGNAMES', &
                        this%model_mempath)
    call mem_reallocate(this%mempaths, LENMEMPATH, pnum, 'MEMPATHS', &
                        this%model_mempath)
    call mem_reallocate(this%inunits, pnum, 'INUNITS', this%model_mempath)
    !
    ! -- load pkinfo
    do n = 1, size(this%pkglist)
      !
      do m = 1, this%pkglist(n)%pnum
        ! -- increment index
        idx = idx + 1
        ! -- package type like 'CHD6'
        this%pkgtypes(idx) = trim(this%pkglist(n)%pkgtype)
        ! -- package name like 'CHD-2'
        this%pkgnames(idx) = trim(this%pkglist(n)%pkgnames(m))
        ! -- memory path like '__INPUT__/MYMODEL/CHD-2'
        this%mempaths(idx) = trim(this%pkglist(n)%mempaths(m))
        ! -- input file unit number
        this%inunits(idx) = this%pkglist(n)%inunits(m)
      end do
    end do
    !
    ! -- return
    return
  end subroutine modelpkgs_memload

  !> @brief deallocate object
  !<
  subroutine modelpkgs_destroy(this)
    ! -- modules
    ! -- dummy
    class(ModelPackageInputsType) :: this
    ! -- local
    integer(I4B) :: n
    !
    ! --
    do n = 1, size(this%pkglist)
      call this%pkglist(n)%destroy()
    end do
    !
    deallocate (this%pkglist)
    deallocate (this%cunit)
    !
    ! -- return
    return
  end subroutine modelpkgs_destroy

end module ModelPackageInputsModule
