!> @brief Stream Network Flow (SWF) Diffusive Wave (DFW) Module
!!
!! This module solves one-dimensional flow routing using a diffusive
!! wave approach.
!!
!<

! todo:
!   Implement length convert and time convert for mannings
!   Use cl1 and cl2 in DFW calculations
!   Move Newton to FN routines
!   Implement a proper perturbation epsilon
!   Is slope input parameter needed?
!   Parameterize the smoothing depth?
!   test ATS
!
module SwfDfwModule

  use KindModule, only: DP, I4B, LGP
  use ConstantsModule, only: LENMEMPATH, LENVARNAME, LINELENGTH, &
                             DZERO, DHALF, DONE, DTWO, DTHREE, &
                             DNODATA, DEM5, DTWOTHIRDS, DP9, DONETHIRD, &
                             DPREC
  use MemoryHelperModule, only: create_mem_path
  use MemoryManagerModule, only: mem_allocate, mem_setptr, get_isize
  use SimVariablesModule, only: errmsg, warnmsg
  use SimModule, only: count_errors, store_error, store_error_unit, &
                       store_error_filename
  use NumericalPackageModule, only: NumericalPackageType
  use BaseDisModule, only: DisBaseType
  use SwfDislModule, only: SwfDislType
  use SwfCxsModule, only: SwfCxsType
  use ObsModule, only: ObsType, obs_cr
  use ObsModule, only: DefaultObsIdProcessor
  use ObserveModule, only: ObserveType
  use MatrixBaseModule

  implicit none
  private
  public :: SwfDfwType, dfw_cr

  type, extends(NumericalPackageType) :: SwfDfwType

    ! -- user-provided input
    integer(I4B), pointer :: icentral => null() !< flag to use central in space weighting (default is upstream weighting)
    real(DP), pointer :: unitconv !< conversion factor used in mannings equation; calculated from timeconv and lengthconv
    real(DP), pointer :: timeconv !< conversion factor from model length units to meters (1.0 if model uses meters for length)
    real(DP), pointer :: lengthconv !< conversion factor frommodel time units to seconds (1.0 if model uses seconds for time)
    real(DP), dimension(:), pointer, contiguous :: width => null() !< reach width
    real(DP), dimension(:), pointer, contiguous :: manningsn => null() !< mannings roughness for each reach
    real(DP), dimension(:), pointer, contiguous :: slope => null() !< slope for each reach
    integer(I4B), dimension(:), pointer, contiguous :: idcxs => null() !< cross section id for each reach
    integer(I4B), dimension(:), pointer, contiguous :: ibound => null() !< pointer to model ibound
    integer(I4B), dimension(:), pointer, contiguous :: icelltype => null() !< set to 1 and is accessed by chd for checking

    ! -- pointer to concrete disl subclass of DisBaseType
    type(SwfDislType), pointer :: disl

    ! -- observation data
    integer(I4B), pointer :: inobspkg => null() !< unit number for obs package
    type(ObsType), pointer :: obs => null() !< observation package

    ! -- pointer to cross section data
    type(SwfCxsType), pointer :: cxs

  contains

    procedure :: allocate_scalars
    procedure :: allocate_arrays
    procedure :: dfw_load
    procedure :: source_options
    procedure :: log_options
    procedure :: source_griddata
    procedure :: log_griddata
    procedure :: dfw_ar
    procedure :: dfw_rp
    procedure :: dfw_ad
    procedure :: dfw_fc
    procedure :: dfw_qnm_fc_nr
    !procedure :: dfw_qnm_fc
    procedure :: dfw_fn
    procedure :: dfw_nur
    procedure :: dfw_cq
    procedure :: dfw_bd
    procedure :: dfw_save_model_flows
    procedure :: dfw_print_model_flows
    procedure :: dfw_da
    procedure :: dfw_df_obs
    procedure :: dfw_rp_obs
    procedure :: dfw_bd_obs
    procedure :: qcalc
    procedure :: get_cond
    !procedure :: get_cond_swr
    procedure :: get_cond_n

  end type SwfDfwType

contains

  !> @brief create package
  !<
  subroutine dfw_cr(dfwobj, name_model, input_mempath, inunit, iout, &
                    dis, cxs)
    ! -- modules
    use MemoryManagerExtModule, only: mem_set_value
    ! -- dummy
    type(SwfDfwType), pointer :: dfwobj
    character(len=*), intent(in) :: name_model
    character(len=*), intent(in) :: input_mempath
    integer(I4B), intent(in) :: inunit
    integer(I4B), intent(in) :: iout
    class(DisBaseType), pointer, intent(inout) :: dis !< the pointer to the discretization
    type(SwfCxsType), pointer, intent(in) :: cxs !< the pointer to the cxs package
    ! -- locals
    logical(LGP) :: found_fname
    ! -- formats
    character(len=*), parameter :: fmtheader = &
      "(1x, /1x, 'DFW --  DIFFUSIVE WAVE (DFW) PACKAGE, VERSION 1, 9/25/2023', &
       &' INPUT READ FROM MEMPATH: ', A, /)"
    !
    ! -- Create the object
    allocate (dfwobj)

    ! -- create name and memory path
    call dfwobj%set_names(1, name_model, 'DFW', 'DFW')

    ! -- Allocate scalars
    call dfwobj%allocate_scalars()

    ! -- Set variables
    dfwobj%input_mempath = input_mempath
    dfwobj%inunit = inunit
    dfwobj%iout = iout

    ! -- set name of input file
    call mem_set_value(dfwobj%input_fname, 'INPUT_FNAME', dfwobj%input_mempath, &
                       found_fname)

    ! -- store pointer to disl
    !    Not normally good practice, but since SWF only works with DISL
    !    may be okay
    dfwobj%dis => dis
    select type (dis)
    type is (SwfDislType)
      dfwobj%disl => dis
    end select

    ! -- Set a pointer to the cxs package
    dfwobj%cxs => cxs

    ! -- create obs package
    call obs_cr(dfwobj%obs, dfwobj%inobspkg)

    ! -- check if dfw is enabled
    if (inunit > 0) then

      ! -- Print a message identifying the package.
      write (iout, fmtheader) input_mempath

      ! -- allocate arrays
      call dfwobj%allocate_arrays()

      ! -- load dfw
      call dfwobj%dfw_load()

    end if

    ! -- Return
    return
  end subroutine dfw_cr

  !> @ brief Allocate scalars
  !!
  !! Allocate and initialize scalars for the package. The base model
  !! allocate scalars method is also called.
  !!
  !<
  subroutine allocate_scalars(this)
    ! -- modules
    ! -- dummy
    class(SwfDfwtype) :: this
    !
    ! -- allocate scalars in NumericalPackageType
    call this%NumericalPackageType%allocate_scalars()
    !
    ! -- Allocate scalars
    call mem_allocate(this%icentral, 'ICENTRAL', this%memoryPath)
    call mem_allocate(this%unitconv, 'UNITCONV', this%memoryPath)
    call mem_allocate(this%lengthconv, 'LENGTHCONV', this%memoryPath)
    call mem_allocate(this%timeconv, 'TIMECONV', this%memoryPath)
    call mem_allocate(this%inobspkg, 'INOBSPKG', this%memoryPath)

    this%icentral = 0
    this%unitconv = DONE
    this%lengthconv = DONE
    this%timeconv = DONE
    this%inobspkg = 0

    return
  end subroutine allocate_scalars

  !> @brief allocate memory for arrays
  !<
  subroutine allocate_arrays(this)
    ! -- dummy
    class(SwfDfwType) :: this
    ! -- locals
    integer(I4B) :: n
    !
    ! -- user-provided input
    call mem_allocate(this%width, this%dis%nodes, &
                      'WIDTH', this%memoryPath)
    call mem_allocate(this%manningsn, this%dis%nodes, &
                      'MANNINGSN', this%memoryPath)
    call mem_allocate(this%slope, this%dis%nodes, &
                      'SLOPE', this%memoryPath)
    call mem_allocate(this%idcxs, this%dis%nodes, &
                      'IDCXS', this%memoryPath)
    call mem_allocate(this%icelltype, this%dis%nodes, &
                      'ICELLTYPE', this%memoryPath)

    do n = 1, this%dis%nodes
      this%width(n) = DZERO
      this%manningsn(n) = DZERO
      this%slope(n) = DZERO
      this%idcxs(n) = 0
      this%icelltype(n) = 1
    end do

    ! -- Return
    return
  end subroutine allocate_arrays

  !> @brief load data from IDM to package
  !<
  subroutine dfw_load(this)
    ! -- dummy
    class(SwfDfwType) :: this
    ! -- locals
    !
    ! -- source input data
    call this%source_options()
    call this%source_griddata()
    !
    ! -- Return
    return
  end subroutine dfw_load

  !> @brief Copy options from IDM into package
  !<
  subroutine source_options(this)
    ! -- modules
    use KindModule, only: LGP
    use InputOutputModule, only: getunit, openfile
    use MemoryManagerExtModule, only: mem_set_value
    use CharacterStringModule, only: CharacterStringType
    use SwfDfwInputModule, only: SwfDfwParamFoundType
    ! -- dummy
    class(SwfDfwType) :: this
    ! -- locals
    integer(I4B) :: isize
    type(SwfDfwParamFoundType) :: found
    type(CharacterStringType), dimension(:), pointer, &
      contiguous :: obs6_fnames
    !
    ! -- update defaults with idm sourced values
    call mem_set_value(this%icentral, 'ICENTRAL', &
                       this%input_mempath, found%icentral)
    call mem_set_value(this%lengthconv, 'LENGTHCONV', &
                       this%input_mempath, found%lengthconv)
    call mem_set_value(this%timeconv, 'TIMECONV', &
                       this%input_mempath, found%timeconv)
    call mem_set_value(this%iprflow, 'IPRFLOW', &
                       this%input_mempath, found%iprflow)
    call mem_set_value(this%ipakcb, 'IPAKCB', &
                       this%input_mempath, found%ipakcb)
    !
    ! -- save flows option active
    if (found%icentral) this%icentral = 1
    if (found%ipakcb) this%ipakcb = -1
    !
    ! -- calculate unit conversion
    this%unitconv = this%lengthconv**DONETHIRD
    this%unitconv = this%unitconv * this%timeconv
    !
    ! -- check for obs6_filename
    call get_isize('OBS6_FILENAME', this%input_mempath, isize)
    if (isize > 0) then
      !
      if (isize /= 1) then
        errmsg = 'Multiple OBS6 keywords detected in OPTIONS block.'// &
                 ' Only one OBS6 entry allowed.'
        call store_error(errmsg)
        call store_error_filename(this%input_fname)
      end if
      !
      call mem_setptr(obs6_fnames, 'OBS6_FILENAME', this%input_mempath)
      !
      found%obs6_filename = .true.
      this%obs%inputFilename = obs6_fnames(1)
      this%obs%active = .true.
      this%inobspkg = GetUnit()
      this%obs%inUnitObs = this%inobspkg
      call openfile(this%inobspkg, this%iout, this%obs%inputFilename, 'OBS')
      call this%obs%obs_df(this%iout, this%packName, this%filtyp, this%dis)
      call this%dfw_df_obs()
    end if
    !
    ! -- log values to list file
    if (this%iout > 0) then
      call this%log_options(found)
    end if
    !
    ! -- Return
    return
  end subroutine source_options

  !> @brief Write user options to list file
  !<
  subroutine log_options(this, found)
    use SwfDfwInputModule, only: SwfDfwParamFoundType
    class(SwfDfwType) :: this
    type(SwfDfwParamFoundType), intent(in) :: found

    write (this%iout, '(1x,a)') 'Setting DFW Options'

    if (found%lengthconv) then
      write (this%iout, '(4x,a, G0)') 'Mannings length conversion value &
                                  &specified as ', this%lengthconv
    end if

    if (found%timeconv) then
      write (this%iout, '(4x,a, G0)') 'Mannings time conversion value &
                                  &specified as ', this%timeconv
    end if

    if (found%lengthconv .or. found%timeconv) then
      write (this%iout, '(4x,a, G0)') 'Mannings conversion value calculated &
                                  &from user-provided length_conversion and &
                                  &time_conversion is ', this%unitconv
    end if

    if (found%iprflow) then
      write (this%iout, '(4x,a)') 'Cell-by-cell flow information will be printed &
                                  &to listing file whenever ICBCFL is not zero.'
    end if

    if (found%ipakcb) then
      write (this%iout, '(4x,a)') 'Cell-by-cell flow information will be printed &
                                  &to listing file whenever ICBCFL is not zero.'
    end if

    if (found%obs6_filename) then
      write (this%iout, '(4x,a)') 'Observation package is active.'
    end if

    write (this%iout, '(1x,a,/)') 'End Setting DFW Options'

  end subroutine log_options

  !> @brief copy griddata from IDM to package
  !<
  subroutine source_griddata(this)
    ! -- modules
    use SimModule, only: count_errors, store_error
    use MemoryHelperModule, only: create_mem_path
    use MemoryManagerModule, only: mem_reallocate
    use MemoryManagerExtModule, only: mem_set_value
    use SimVariablesModule, only: idm_context
    use SwfDfwInputModule, only: SwfDfwParamFoundType
    ! -- dummy
    class(SwfDfwType) :: this
    ! -- locals
    character(len=LENMEMPATH) :: idmMemoryPath
    type(SwfDfwParamFoundType) :: found
    integer(I4B), dimension(:), pointer, contiguous :: map
    ! -- formats
    !
    ! -- set memory path
    idmMemoryPath = create_mem_path(this%name_model, 'DFW', idm_context)
    !
    ! -- set map to convert user input data into reduced data
    map => null()
    if (this%dis%nodes < this%dis%nodesuser) map => this%dis%nodeuser
    !
    ! -- update defaults with idm sourced values
    call mem_set_value(this%width, 'WIDTH', idmMemoryPath, map, found%width)
    call mem_set_value(this%manningsn, 'MANNINGSN', &
                       idmMemoryPath, map, found%manningsn)
    call mem_set_value(this%slope, 'SLOPE', idmMemoryPath, map, found%slope)
    call mem_set_value(this%idcxs, 'IDCXS', idmMemoryPath, map, found%idcxs)
    !
    ! -- ensure WIDTH was found
    if (.not. found%width) then
      write (errmsg, '(a)') 'Error in GRIDDATA block: WIDTH not found.'
      call store_error(errmsg)
    end if
    !
    ! -- ensure MANNINGSN was found
    if (.not. found%manningsn) then
      write (errmsg, '(a)') 'Error in GRIDDATA block: MANNINGSN not found.'
      call store_error(errmsg)
    end if
    !
    ! -- ensure SLOPE was found
    if (.not. found%slope) then
      write (errmsg, '(a)') 'Error in GRIDDATA block: SLOPE not found.'
      call store_error(errmsg)
    end if
    !
    ! -- log griddata
    if (this%iout > 0) then
      call this%log_griddata(found)
    end if
    !
    ! -- Return
    return
  end subroutine source_griddata

  !> @brief log griddata to list file
  !<
  subroutine log_griddata(this, found)
    use SwfDfwInputModule, only: SwfDfwParamFoundType
    class(SwfDfwType) :: this
    type(SwfDfwParamFoundType), intent(in) :: found

    write (this%iout, '(1x,a)') 'Setting DFW Griddata'

    if (found%width) then
      write (this%iout, '(4x,a)') 'WIDTH set from input file'
    end if

    if (found%manningsn) then
      write (this%iout, '(4x,a)') 'MANNINGSN set from input file'
    end if

    if (found%slope) then
      write (this%iout, '(4x,a)') 'SLOPE set from input file'
    end if

    if (found%idcxs) then
      write (this%iout, '(4x,a)') 'IDCXS set from input file'
    end if

    write (this%iout, '(1x,a,/)') 'End Setting DFW Griddata'

  end subroutine log_griddata

  !> @brief allocate memory
  !<
  subroutine dfw_ar(this, ibound)
    ! -- modules
    ! -- dummy
    class(SwfDfwType) :: this !< this instance
    integer(I4B), dimension(:), pointer, contiguous :: ibound !< model ibound array

    ! -- store pointer to ibound
    this%ibound => ibound

    ! - observation data
    call this%obs%obs_ar()

    return
  end subroutine dfw_ar

  !> @brief allocate memory
  !<
  subroutine dfw_rp(this)
    ! -- modules
    ! -- dummy
    class(SwfDfwType) :: this !< this instance
    !
    ! -- read observations
    call this%dfw_rp_obs()
    return
  end subroutine dfw_rp

  !> @brief advance
  !<
  subroutine dfw_ad(this, irestore)
    !
    class(SwfDfwType) :: this
    integer(I4B), intent(in) :: irestore

    ! -- Push simulated values to preceding time/subtime step
    call this%obs%obs_ad()

    !
    ! -- Return
    return
  end subroutine dfw_ad

  !> @brief fill coefficients
  !!
  !! The DFW Package is entirely Newton based.  All matrix and rhs terms
  !! are added from thish routine.
  !!
  !<
  subroutine dfw_fc(this, kiter, matrix_sln, idxglo, rhs, stage, stage_old)
    ! -- modules
    use ConstantsModule, only: DONE
    ! -- dummy
    class(SwfDfwType) :: this
    integer(I4B) :: kiter
    class(MatrixBaseType), pointer :: matrix_sln
    integer(I4B), intent(in), dimension(:) :: idxglo
    real(DP), intent(inout), dimension(:) :: rhs
    real(DP), intent(inout), dimension(:) :: stage
    real(DP), intent(inout), dimension(:) :: stage_old
    ! -- local
    !
    ! -- add qnm contributions to matrix equations
    call this%dfw_qnm_fc_nr(kiter, matrix_sln, idxglo, rhs, stage, stage_old)
    !
    ! -- Return
    return
  end subroutine dfw_fc

  !> @brief fill coefficients
  !!
  !! Add qnm contributions to matrix equations
  !!
  !<
  subroutine dfw_qnm_fc_nr(this, kiter, matrix_sln, idxglo, rhs, stage, stage_old)
    ! -- modules
    use ConstantsModule, only: DONE
    ! -- dummy
    class(SwfDfwType) :: this
    integer(I4B) :: kiter
    class(MatrixBaseType), pointer :: matrix_sln
    integer(I4B), intent(in), dimension(:) :: idxglo
    real(DP), intent(inout), dimension(:) :: rhs
    real(DP), intent(inout), dimension(:) :: stage
    real(DP), intent(inout), dimension(:) :: stage_old
    ! -- local
    integer(I4B) :: n, m, ii, idiag
    real(DP) :: qnm
    real(DP) :: qeps
    real(DP) :: eps
    real(DP) :: derv
    !
    ! -- set perturbation derivative epsilon
    eps = 1.D-8
    !
    ! -- Calculate conductance and put into amat
    do n = 1, this%dis%nodes
      !
      ! -- Find diagonal position for row n
      idiag = this%dis%con%ia(n)
      !
      ! -- Loop through connections adding matrix terms
      do ii = this%dis%con%ia(n) + 1, this%dis%con%ia(n + 1) - 1
        !
        ! -- skip for masked cells
        if (this%dis%con%mask(ii) == 0) cycle
        !
        ! -- connection variables
        m = this%dis%con%ja(ii)
        !
        ! -- Fill the qnm term on the right hand side
        qnm = this%qcalc(n, m, stage(n), stage(m))
        rhs(n) = rhs(n) - qnm
        !
        ! -- Derivative calculation and fill of n terms
        qeps = this%qcalc(n, m, stage(n) + eps, stage(m))
        derv = (qeps - qnm) / eps
        call matrix_sln%add_value_pos(idxglo(idiag), derv)
        rhs(n) = rhs(n) + derv * stage(n)
        !
        ! -- Derivative calculation and fill of m terms
        qeps = this%qcalc(n, m, stage(n), stage(m) + eps)
        derv = (qeps - qnm) / eps
        call matrix_sln%add_value_pos(idxglo(ii), derv)
        rhs(n) = rhs(n) + derv * stage(m)

      end do
    end do
    !
    ! -- Return
    return
  end subroutine dfw_qnm_fc_nr

  subroutine dfw_fn(this, kiter, matrix_sln, idxglo, rhs, stage)
    ! -- dummy
    class(SwfDfwType) :: this
    integer(I4B) :: kiter
    class(MatrixBaseType), pointer :: matrix_sln
    integer(I4B), intent(in), dimension(:) :: idxglo
    real(DP), intent(inout), dimension(:) :: rhs
    real(DP), intent(inout), dimension(:) :: stage
    ! -- local
    !
    ! -- add newton terms to solution matrix
    ! -- todo: add newton terms here instead?
    !
    !
    ! -- Return
    return
  end subroutine dfw_fn

  function qcalc(this, n, m, stage_n, stage_m) result(qnm)
    ! -- dummy
    class(SwfDfwType) :: this
    integer(I4B), intent(in) :: n
    integer(I4B), intent(in) :: m
    real(DP), intent(in) :: stage_n
    real(DP), intent(in) :: stage_m
    ! -- local
    real(DP) :: qnm
    real(DP) :: cond
    !
    cond = this%get_cond(n, m, stage_n, stage_m)
    qnm = cond * (stage_m - stage_n)
    return
  end function qcalc

  function get_cond(this, n, m, stage_n, stage_m) result(cond)
    ! -- modules
    use SmoothingModule, only: sQuadratic
    ! -- dummy
    class(SwfDfwType) :: this
    integer(I4B), intent(in) :: n
    integer(I4B), intent(in) :: m
    real(DP), intent(in) :: stage_n
    real(DP), intent(in) :: stage_m
    ! -- local
    real(DP) :: absdhdxsqr
    real(DP) :: depth_n
    real(DP) :: depth_m
    real(DP) :: range = 1.d-2
    real(DP) :: dydx
    real(DP) :: smooth_factor
    real(DP) :: denom
    real(DP) :: cond
    real(DP) :: cn
    real(DP) :: cm
    !
    ! we are using a harmonic conductance approach here; however
    ! the SWR Process for MODFLOW-2005/NWT uses length-weighted
    ! average areas and hydraulic radius instead.
    !
    denom = DHALF * this%disl%reach_length(n) + &
            DHALF * this%disl%reach_length(m)
    cond = DZERO
    if (denom > DPREC) then
      absdhdxsqr = abs((stage_n - stage_m) / denom)**DHALF
      if (absdhdxsqr < DPREC) then
        absdhdxsqr = 1.e-7
      end if

      depth_n = stage_n - this%disl%reach_bottom(n)
      depth_m = stage_m - this%disl%reach_bottom(m)

      if (this%icentral == 0) then
        ! -- use upstream weighting
        if (stage_n > stage_m) then
          depth_m = depth_n
        else
          depth_n = depth_m
        end if
      end if

      ! -- Calculate a smoothed depth that goes to zero over
      !    the specified range
      call sQuadratic(depth_n, range, dydx, smooth_factor)
      depth_n = depth_n * smooth_factor
      call sQuadratic(depth_m, range, dydx, smooth_factor)
      depth_m = depth_m * smooth_factor

      cn = this%get_cond_n(n, depth_n, absdhdxsqr)
      cm = this%get_cond_n(m, depth_m, absdhdxsqr)

      if ((cn + cm) > DPREC) then
        cond = cn * cm / (cn + cm)
      else
        cond = DZERO
      end if

    end if

  end function get_cond

  !> @brief Calculate half reach conductance
  !!
  !! Calculate half reach conductance for reach n
  !! using Manning's equation
  !!
  !<
  function get_cond_n(this, n, depth, absdhdxsq) result(c)
    ! -- modules
    ! -- dummy
    class(SwfDfwType) :: this
    integer(I4B), intent(in) :: n !< reach number
    real(DP), intent(in) :: depth !< simulated depth (stage - elevation) in reach n for this iteration
    real(DP), intent(in) :: absdhdxsq !< absolute value of simulated hydraulic gradient
    ! -- local
    real(DP) :: c
    real(DP) :: width
    real(DP) :: rough
    real(DP) :: slope
    real(DP) :: dx
    real(DP) :: roughc
    real(DP) :: a
    real(DP) :: r
    !
    width = this%width(n)
    rough = this%manningsn(n)
    slope = this%slope(n)
    dx = DHALF * this%disl%reach_length(n)

    roughc = this%cxs%get_roughness(this%idcxs(n), width, depth, rough, &
                                    slope)
    a = this%cxs%get_area(this%idcxs(n), width, depth)
    r = this%cxs%get_hydraulic_radius(this%idcxs(n), width, depth, area=a)

    ! -- conductance from manning's equation
    c = this%unitconv * a * r**DTWOTHIRDS / roughc / absdhdxsq / dx

  end function get_cond_n

  !> @ brief Newton under relaxation
  !!
  !!  If stage is below the bottom, then pull it up a bit
  !!
  !<
  subroutine dfw_nur(this, neqmod, x, xtemp, dx, inewtonur, dxmax, locmax)
    ! -- dummy
    class(SwfDfwType) :: this
    integer(I4B), intent(in) :: neqmod
    real(DP), dimension(neqmod), intent(inout) :: x
    real(DP), dimension(neqmod), intent(in) :: xtemp
    real(DP), dimension(neqmod), intent(inout) :: dx
    integer(I4B), intent(inout) :: inewtonur
    real(DP), intent(inout) :: dxmax
    integer(I4B), intent(inout) :: locmax
    ! -- local
    integer(I4B) :: n
    real(DP) :: botm
    real(DP) :: xx
    real(DP) :: dxx
    !
    ! -- Newton-Raphson under-relaxation
    do n = 1, this%dis%nodes
      if (this%ibound(n) < 1) cycle
      if (this%icelltype(n) > 0) then
        botm = this%disl%reach_bottom(n)
        ! -- only apply Newton-Raphson under-relaxation if
        !    solution head is below the bottom of the model
        if (x(n) < botm) then
          inewtonur = 1
          xx = xtemp(n) * (DONE - DP9) + botm * DP9
          dxx = x(n) - xx
          if (abs(dxx) > abs(dxmax)) then
            locmax = n
            dxmax = dxx
          end if
          x(n) = xx
          dx(n) = DZERO
        end if
      end if
    end do
    !
    ! -- return
    return
  end subroutine dfw_nur

  subroutine dfw_cq(this, stage, stage_old, flowja)
    ! -- dummy
    class(SwfDfwType) :: this
    real(DP), intent(inout), dimension(:) :: stage
    real(DP), intent(inout), dimension(:) :: stage_old
    real(DP), intent(inout), dimension(:) :: flowja
    ! -- local
    integer(I4B) :: n, ipos, m
    real(DP) :: qnm
    !
    !
    do n = 1, this%dis%nodes
      do ipos = this%dis%con%ia(n) + 1, this%dis%con%ia(n + 1) - 1
        m = this%dis%con%ja(ipos)
        if (m < n) cycle
        qnm = this%qcalc(n, m, stage(n), stage(m))
        flowja(ipos) = qnm
        flowja(this%dis%con%isym(ipos)) = -qnm
      end do
    end do

    !
    ! -- Return
    return
  end subroutine dfw_cq

  !> @ brief Model budget calculation for package
  !!
  !!  Budget calculation for the DFW package components. Components include
  !!  external outflow
  !!
  !<
  subroutine dfw_bd(this, isuppress_output, model_budget)
    ! -- modules
    use BudgetModule, only: BudgetType
    ! -- dummy variables
    class(SwfDfwType) :: this !< SwfDfwType object
    integer(I4B), intent(in) :: isuppress_output !< flag to suppress model output
    type(BudgetType), intent(inout) :: model_budget !< model budget object
    ! -- local variables
    !
    ! -- Add any DFW budget terms
    !    none
    !
    ! -- return
    return
  end subroutine dfw_bd

  !> @ brief save flows for package
  !<
  subroutine dfw_save_model_flows(this, flowja, icbcfl, icbcun)
    ! -- dummy
    class(SwfDfwType) :: this
    real(DP), dimension(:), intent(in) :: flowja
    integer(I4B), intent(in) :: icbcfl
    integer(I4B), intent(in) :: icbcun
    ! -- local
    integer(I4B) :: ibinun
    ! -- formats
    !
    ! -- Set unit number for binary output
    if (this%ipakcb < 0) then
      ibinun = icbcun
    elseif (this%ipakcb == 0) then
      ibinun = 0
    else
      ibinun = this%ipakcb
    end if
    if (icbcfl == 0) ibinun = 0
    !
    ! -- Write the face flows if requested
    if (ibinun /= 0) then
      !
      ! -- flowja
      call this%dis%record_connection_array(flowja, ibinun, this%iout)

    end if
    !
    ! -- Return
    return
  end subroutine dfw_save_model_flows

  !> @ brief print flows for package
  !<
  subroutine dfw_print_model_flows(this, ibudfl, flowja)
    ! -- modules
    use TdisModule, only: kper, kstp
    use ConstantsModule, only: LENBIGLINE
    ! -- dummy
    class(SwfDfwType) :: this
    integer(I4B), intent(in) :: ibudfl
    real(DP), intent(inout), dimension(:) :: flowja
    ! -- local
    character(len=LENBIGLINE) :: line
    character(len=30) :: tempstr
    integer(I4B) :: n, ipos, m
    real(DP) :: qnm
    ! -- formats
    character(len=*), parameter :: fmtiprflow = &
      &"(/,4x,'CALCULATED INTERCELL FLOW FOR PERIOD ', i0, ' STEP ', i0)"
    ! -- Write flowja to list file if requested
    if (ibudfl /= 0 .and. this%iprflow > 0) then
      write (this%iout, fmtiprflow) kper, kstp
      do n = 1, this%dis%nodes
        line = ''
        call this%dis%noder_to_string(n, tempstr)
        line = trim(tempstr)//':'
        do ipos = this%dis%con%ia(n) + 1, this%dis%con%ia(n + 1) - 1
          m = this%dis%con%ja(ipos)
          call this%dis%noder_to_string(m, tempstr)
          line = trim(line)//' '//trim(tempstr)
          qnm = flowja(ipos)
          write (tempstr, '(1pg15.6)') qnm
          line = trim(line)//' '//trim(adjustl(tempstr))
        end do
        write (this%iout, '(a)') trim(line)
      end do
    end if
    !
    ! -- Return
    return
  end subroutine dfw_print_model_flows

  !> @brief deallocate memory
  !<
  subroutine dfw_da(this)
    ! -- modules
    use MemoryManagerModule, only: mem_deallocate
    use MemoryManagerExtModule, only: memorylist_remove
    use SimVariablesModule, only: idm_context
    ! -- dummy
    class(SwfDfwType) :: this
    !
    ! -- Deallocate input memory
    call memorylist_remove(this%name_model, 'DFW', idm_context)
    !
    ! -- Scalars
    call mem_deallocate(this%icentral)
    call mem_deallocate(this%unitconv)
    call mem_deallocate(this%lengthconv)
    call mem_deallocate(this%timeconv)
    !
    ! -- Deallocate arrays
    call mem_deallocate(this%width)
    call mem_deallocate(this%manningsn)
    call mem_deallocate(this%slope)
    call mem_deallocate(this%idcxs)
    call mem_deallocate(this%icelltype)

    ! -- obs package
    call mem_deallocate(this%inobspkg)
    call this%obs%obs_da()
    deallocate (this%obs)
    nullify (this%obs)
    nullify (this%disl)
    nullify (this%cxs)

    ! -- deallocate parent
    call this%NumericalPackageType%da()
    !
    ! -- Return
    return
  end subroutine dfw_da

  !> @brief Define the observation types available in the package
  !!
  !! Method to define the observation types available in the package.
  !!
  !<
  subroutine dfw_df_obs(this)
    ! -- dummy variables
    class(SwfDfwType) :: this !< SwfDfwType object
    ! -- local variables
    integer(I4B) :: indx
    !
    ! -- Store obs type and assign procedure pointer
    !    for ext-outflow observation type.
    call this%obs%StoreObsType('ext-outflow', .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => dfwobsidprocessor
    !
    ! -- return
    return
  end subroutine dfw_df_obs

  subroutine dfwobsidprocessor(obsrv, dis, inunitobs, iout)
    ! -- dummy
    type(ObserveType), intent(inout) :: obsrv
    class(DisBaseType), intent(in) :: dis
    integer(I4B), intent(in) :: inunitobs
    integer(I4B), intent(in) :: iout
    ! -- local
    integer(I4B) :: n
    character(len=LINELENGTH) :: strng
    !
    ! -- Initialize variables
    strng = obsrv%IDstring
    read (strng, *) n
    !
    if (n > 0) then
      obsrv%NodeNumber = n
    else
      errmsg = 'Error reading data from ID string'
      call store_error(errmsg)
      call store_error_unit(inunitobs)
    end if
    !
    return
  end subroutine dfwobsidprocessor

  !> @brief Save observations for the package
  !!
  !! Method to save simulated values for the package.
  !!
  !<
  subroutine dfw_bd_obs(this)
    ! -- dummy variables
    class(SwfDfwType) :: this !< SwfDfwType object
    ! -- local variables
    integer(I4B) :: i
    integer(I4B) :: j
    integer(I4B) :: n
    real(DP) :: v
    character(len=100) :: msg
    type(ObserveType), pointer :: obsrv => null()
    !
    ! Write simulated values for all observations
    if (this%obs%npakobs > 0) then
      call this%obs%obs_bd_clear()
      do i = 1, this%obs%npakobs
        obsrv => this%obs%pakobs(i)%obsrv
        do j = 1, obsrv%indxbnds_count
          n = obsrv%indxbnds(j)
          v = DZERO
          select case (obsrv%ObsTypeId)
          case default
            msg = 'Unrecognized observation type: '//trim(obsrv%ObsTypeId)
            call store_error(msg)
          end select
          call this%obs%SaveOneSimval(obsrv, v)
        end do
      end do
      !
      ! -- write summary of package error messages
      if (count_errors() > 0) then
        call this%parser%StoreErrorUnit()
      end if
    end if
    !
    ! -- return
    return
  end subroutine dfw_bd_obs

  !> @brief Read and prepare observations for a package
  !!
  !! Method to read and prepare observations for a package.
  !!
  !<
  subroutine dfw_rp_obs(this)
    ! -- modules
    use TdisModule, only: kper
    ! -- dummy variables
    class(SwfDfwType), intent(inout) :: this !< SwfDfwType object
    ! -- local variables
    integer(I4B) :: i
    integer(I4B) :: j
    integer(I4B) :: nn1
    class(ObserveType), pointer :: obsrv => null()
    ! -- formats
    !
    ! -- process each package observation
    !    only done the first stress period since boundaries are fixed
    !    for the simulation
    if (kper == 1) then
      do i = 1, this%obs%npakobs
        obsrv => this%obs%pakobs(i)%obsrv
        !
        ! -- get node number 1
        nn1 = obsrv%NodeNumber
        if (nn1 < 1 .or. nn1 > this%disl%nodes) then
          write (errmsg, '(a,1x,a,1x,i0,1x,a,1x,i0,a)') &
            trim(adjustl(obsrv%ObsTypeId)), &
            'reach must be greater than 0 and less than or equal to', &
            this%disl%nodes, '(specified value is ', nn1, ')'
          call store_error(errmsg)
        else
          if (obsrv%indxbnds_count == 0) then
            call obsrv%AddObsIndex(nn1)
          else
            errmsg = 'Programming error in dfw_rp_obs'
            call store_error(errmsg)
          end if
        end if
        !
        ! -- check that node number 1 is valid; call store_error if not
        do j = 1, obsrv%indxbnds_count
          nn1 = obsrv%indxbnds(j)
          if (nn1 < 1 .or. nn1 > this%disl%nodes) then
            write (errmsg, '(a,1x,a,1x,i0,1x,a,1x,i0,a)') &
              trim(adjustl(obsrv%ObsTypeId)), &
              'reach must be greater than 0 and less than or equal to', &
              this%disl%nodes, '(specified value is ', nn1, ')'
            call store_error(errmsg)
          end if
        end do
      end do
      !
      ! -- evaluate if there are any observation errors
      if (count_errors() > 0) then
        call this%parser%StoreErrorUnit()
      end if
    end if
    !
    ! -- return
    return
  end subroutine dfw_rp_obs

end module SwfDfwModule
