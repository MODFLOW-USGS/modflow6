module GwfNpfModule
  use KindModule, only: DP, I4B
  use ConstantsModule, only: DZERO, DEM9, DEM8, DEM7, DEM6, DEM2, &
                             DHALF, DP9, DONE, DTWO, &
                             DLNLOW, DLNHIGH, &
                             DHNOFLO, DHDRY, DEM10
  use SmoothingModule, only: sQuadraticSaturation, &
                             sQuadraticSaturationDerivative
  use NumericalPackageModule, only: NumericalPackageType
  use GwfNpfGridDataModule, only: GwfNpfGridDataType
  use GwfNpfOptionsModule, only: GwfNpfOptionsType
  use BaseDisModule, only: DisBaseType
  use GwfIcModule, only: GwfIcType
  use Xt3dModule, only: Xt3dType
  use BlockParserModule, only: BlockParserType
  use InputOutputModule, only: GetUnit, openfile
  use TvkModule, only: TvkType, tvk_cr

  implicit none

  private
  public :: GwfNpfType
  public :: npf_cr
  public :: hcond
  public :: vcond
  public :: condmean
  public :: thksatnm
  public :: hyeff_calc

  type, extends(NumericalPackageType) :: GwfNpfType

    type(GwfIcType), pointer :: ic => null() !< initial conditions object
    type(Xt3dType), pointer :: xt3d => null() !< xt3d pointer
    integer(I4B), pointer :: iname => null() !< length of variable names
    character(len=24), dimension(:), pointer :: aname => null() !< variable names
    integer(I4B), dimension(:), pointer, contiguous :: ibound => null() !< pointer to model ibound
    real(DP), dimension(:), pointer, contiguous :: hnew => null() !< pointer to model xnew
    integer(I4B), pointer :: ixt3d => null() !< xt3d flag (0 is off, 1 is lhs, 2 is rhs)
    integer(I4B), pointer :: iperched => null() !< vertical flow corrections if 1
    integer(I4B), pointer :: ivarcv => null() !< CV is function of water table
    integer(I4B), pointer :: idewatcv => null() !< CV may be a discontinuous function of water table
    integer(I4B), pointer :: ithickstrt => null() !< thickstrt option flag
    integer(I4B), pointer :: igwfnewtonur => null() !< newton head dampening using node bottom option flag
    integer(I4B), pointer :: iusgnrhc => null() !< MODFLOW-USG saturation calculation option flag
    integer(I4B), pointer :: inwtupw => null() !< MODFLOW-NWT upstream weighting option flag
    integer(I4B), pointer :: icalcspdis => null() !< Calculate specific discharge at cell centers
    integer(I4B), pointer :: isavspdis => null() !< Save specific discharge at cell centers
    integer(I4B), pointer :: isavsat => null() !< Save sat to budget file
    real(DP), pointer :: hnoflo => null() !< default is 1.e30
    real(DP), pointer :: satomega => null() !< newton-raphson saturation omega
    integer(I4B), pointer :: irewet => null() !< rewetting (0:off, 1:on)
    integer(I4B), pointer :: iwetit => null() !< wetting interval (default is 1)
    integer(I4B), pointer :: ihdwet => null() !< (0 or not 0)
    integer(I4B), pointer :: icellavg => null() !< harmonic(0), logarithmic(1), or arithmetic thick-log K (2)
    real(DP), pointer :: wetfct => null() !< wetting factor
    real(DP), pointer :: hdry => null() !< default is -1.d30
    integer(I4B), dimension(:), pointer, contiguous :: icelltype => null() !< confined (0) or convertible (1)
    integer(I4B), dimension(:), pointer, contiguous :: ithickstartflag => null() !< array of flags for handling the thickstrt option
    !
    ! K properties
    real(DP), dimension(:), pointer, contiguous :: k11 => null() !< hydraulic conductivity; if anisotropic, then this is Kx prior to rotation
    real(DP), dimension(:), pointer, contiguous :: k22 => null() !< hydraulic conductivity; if specified then this is Ky prior to rotation
    real(DP), dimension(:), pointer, contiguous :: k33 => null() !< hydraulic conductivity; if specified then this is Kz prior to rotation
    integer(I4B), pointer :: iavgkeff => null() !< effective conductivity averaging (0: harmonic, 1: arithmetic)
    integer(I4B), pointer :: ik22 => null() !< flag that k22 is specified
    integer(I4B), pointer :: ik33 => null() !< flag that k33 is specified
    integer(I4B), pointer :: ik22overk => null() !< flag that k22 is specified as anisotropy ratio
    integer(I4B), pointer :: ik33overk => null() !< flag that k33 is specified as anisotropy ratio
    integer(I4B), pointer :: iangle1 => null() !< flag to indicate angle1 was read
    integer(I4B), pointer :: iangle2 => null() !< flag to indicate angle2 was read
    integer(I4B), pointer :: iangle3 => null() !< flag to indicate angle3 was read
    real(DP), dimension(:), pointer, contiguous :: angle1 => null() !< k ellipse rotation in xy plane around z axis (yaw)
    real(DP), dimension(:), pointer, contiguous :: angle2 => null() !< k ellipse rotation up from xy plane around y axis (pitch)
    real(DP), dimension(:), pointer, contiguous :: angle3 => null() !< k tensor rotation around x axis (roll)
    !
    integer(I4B), pointer :: iwetdry => null() !< flag to indicate angle1 was read
    real(DP), dimension(:), pointer, contiguous :: wetdry => null() !< wetdry array
    real(DP), dimension(:), pointer, contiguous :: sat => null() !< saturation (0. to 1.) for each cell
    real(DP), dimension(:), pointer, contiguous :: condsat => null() !< saturated conductance (symmetric array)
    real(DP), pointer :: satmin => null() !< minimum saturated thickness
    integer(I4B), dimension(:), pointer, contiguous :: ibotnode => null() !< bottom node used if igwfnewtonur /= 0
    !
    real(DP), dimension(:, :), pointer, contiguous :: spdis => null() !< specific discharge : qx, qy, qz (nodes, 3)
    integer(I4B), pointer :: nedges => null() !< number of cell edges
    integer(I4B), pointer :: lastedge => null() !< last edge number
    integer(I4B), dimension(:), pointer, contiguous :: nodedge => null() !< array of node numbers that have edges
    integer(I4B), dimension(:), pointer, contiguous :: ihcedge => null() !< edge type (horizontal or vertical)
    real(DP), dimension(:, :), pointer, contiguous :: propsedge => null() !< edge properties (Q, area, nx, ny, distance)
    !
    integer(I4B), pointer :: intvk => null() ! TVK (time-varying K) unit number (0 if unused)
    type(TvkType), pointer :: tvk => null() ! TVK object
    integer(I4B), pointer :: kchangeper => null() ! last stress period in which any node K (or K22, or K33) values were changed (0 if unchanged from start of simulation)
    integer(I4B), pointer :: kchangestp => null() ! last time step in which any node K (or K22, or K33) values were changed (0 if unchanged from start of simulation)
    integer(I4B), dimension(:), pointer, contiguous :: nodekchange => null() ! grid array of flags indicating for each node whether its K (or K22, or K33) value changed (1) at (kchangeper, kchangestp) or not (0)
    !
  contains
    procedure :: npf_df
    procedure :: npf_ac
    procedure :: npf_mc
    procedure :: npf_ar
    procedure :: npf_rp
    procedure :: npf_ad
    procedure :: npf_cf
    procedure :: npf_fc
    procedure :: npf_fn
    procedure :: npf_cq
    procedure :: npf_save_model_flows
    procedure :: npf_nur
    procedure :: npf_print_model_flows
    procedure :: npf_da
    procedure, private :: thksat => sgwf_npf_thksat
    procedure, private :: qcalc => sgwf_npf_qcalc
    procedure, private :: wd => sgwf_npf_wetdry
    procedure, private :: wdmsg => sgwf_npf_wdmsg
    procedure :: allocate_scalars
    procedure, private :: allocate_arrays
    procedure, private :: read_options
    procedure, private :: set_options
    procedure, private :: rewet_options
    procedure, private :: check_options
    procedure, private :: read_grid_data
    procedure, private :: set_grid_data
    procedure, private :: prepcheck
    procedure, private :: preprocess_input
    procedure, private :: calc_condsat
    procedure, private :: calc_initial_sat
    procedure, public :: rewet_check
    procedure, public :: hy_eff
    procedure, public :: calc_spdis
    procedure, public :: sav_spdis
    procedure, public :: sav_sat
    procedure, public :: increase_edge_count
    procedure, public :: set_edge_properties
    procedure, public :: calcSatThickness
  end type

contains

  subroutine npf_cr(npfobj, name_model, inunit, iout)
! ******************************************************************************
! npf_cr -- Create a new NPF object. Pass a inunit value of 0 if npf data will
!           initialized from memory
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    type(GwfNpftype), pointer :: npfobj
    character(len=*), intent(in) :: name_model
    integer(I4B), intent(in) :: inunit
    integer(I4B), intent(in) :: iout
! ------------------------------------------------------------------------------
    !
    ! -- Create the object
    allocate (npfobj)
    !
    ! -- create name and memory path
    call npfobj%set_names(1, name_model, 'NPF', 'NPF')
    !
    ! -- Allocate scalars
    call npfobj%allocate_scalars()
    !
    ! -- Set variables
    npfobj%inunit = inunit
    npfobj%iout = iout
    !
    ! -- Return
    return
  end subroutine npf_cr

  !> @brief define the NPF package instance
  !!
  !! This is a hybrid routine: it either reads the options for this package
  !! from the input file, or the optional argument @param npf_options
  !! should be passed. A consistency check is performed, and finally
  !! xt3d_df is called, when enabled.
  !<
  subroutine npf_df(this, dis, xt3d, ingnc, npf_options)
! ******************************************************************************
! npf_df -- Define
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use SimModule, only: store_error
    use Xt3dModule, only: xt3d_cr
    ! -- dummy
    class(GwfNpftype) :: this !< instance of the NPF package
    class(DisBaseType), pointer, intent(inout) :: dis !< the pointer to the discretization
    type(Xt3dType), pointer :: xt3d !< the pointer to the XT3D 'package'
    integer(I4B), intent(in) :: ingnc !< ghostnodes enabled? (>0 means yes)
    type(GwfNpfOptionsType), optional, intent(in) :: npf_options !< the optional options, for when not constructing from file
    ! -- local
    ! -- formats
    character(len=*), parameter :: fmtheader = &
      "(1x, /1x, 'NPF -- NODE PROPERTY FLOW PACKAGE, VERSION 1, 3/30/2015', &
       &' INPUT READ FROM UNIT ', i0, //)"
    ! -- data
! ------------------------------------------------------------------------------
    !
    ! -- Set a pointer to dis
    this%dis => dis
    !
    if (.not. present(npf_options)) then
      ! -- Print a message identifying the node property flow package.
      write (this%iout, fmtheader) this%inunit
      !
      ! -- Initialize block parser and read options
      call this%parser%Initialize(this%inunit, this%iout)
      call this%read_options()
    else
      call this%set_options(npf_options)
    end if

    call this%check_options()
    !
    ! -- Save pointer to xt3d object
    this%xt3d => xt3d
    if (this%ixt3d /= 0) xt3d%ixt3d = this%ixt3d
    call this%xt3d%xt3d_df(dis)
    !
    ! -- Ensure GNC and XT3D are not both on at the same time
    if (this%ixt3d /= 0 .and. ingnc > 0) then
      call store_error('Error in model '//trim(this%name_model)// &
                       '.  The XT3D option cannot be used with the GNC &
                       &Package.', terminate=.TRUE.)
    end if
    !
    ! -- Return
    return
  end subroutine npf_df

  subroutine npf_ac(this, moffset, sparse)
! ******************************************************************************
! npf_ac -- Add connections for extended neighbors to the sparse matrix
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use SparseModule, only: sparsematrix
    use MemoryManagerModule, only: mem_allocate
    ! -- dummy
    class(GwfNpftype) :: this
    integer(I4B), intent(in) :: moffset
    type(sparsematrix), intent(inout) :: sparse
    ! -- local
! ------------------------------------------------------------------------------
    !
    ! -- Add extended neighbors (neighbors of neighbors)
    if (this%ixt3d /= 0) call this%xt3d%xt3d_ac(moffset, sparse)
    !
    ! -- Return
    return
  end subroutine npf_ac

  subroutine npf_mc(this, moffset, iasln, jasln)
! ******************************************************************************
! npf_mc -- Map connections and construct iax, jax, and idxglox
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryManagerModule, only: mem_allocate
    ! -- dummy
    class(GwfNpftype) :: this
    integer(I4B), intent(in) :: moffset
    integer(I4B), dimension(:), intent(in) :: iasln
    integer(I4B), dimension(:), intent(in) :: jasln
    ! -- local
! ------------------------------------------------------------------------------
    !
    if (this%ixt3d /= 0) call this%xt3d%xt3d_mc(moffset, iasln, jasln)
    !
    ! -- Return
    return
  end subroutine npf_mc

  !> @brief allocate and read this NPF instance
  !!
  !! Allocate package arrays, read the grid data either from file or
  !! from the input argument (when the optional @param grid_data is passed),
  !! preprocess the input data and call *_ar on xt3d, when active.
  !<
  subroutine npf_ar(this, ic, ibound, hnew, grid_data)
! ******************************************************************************
! npf_ar -- Allocate and Read
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(GwfNpftype) :: this !< instance of the NPF package
    type(GwfIcType), pointer, intent(in) :: ic !< initial conditions
    integer(I4B), dimension(:), pointer, contiguous, intent(inout) :: ibound !< model ibound array
    real(DP), dimension(:), pointer, contiguous, intent(inout) :: hnew !< pointer to model head array
    type(GwfNpfGridDataType), optional, intent(in) :: grid_data !< (optional) data structure with NPF grid data
    ! -- local
    ! -- formats
    ! -- data
! ------------------------------------------------------------------------------
    !
    ! -- Store pointers to arguments that were passed in
    this%ic => ic
    this%ibound => ibound
    this%hnew => hnew
    !
    ! -- allocate arrays
    call this%allocate_arrays(this%dis%nodes, this%dis%njas)
    !
    if (.not. present(grid_data)) then
      ! -- read from file, set, and convert/check the input
      call this%read_grid_data()
      call this%prepcheck()
    else
      ! -- set the data block
      call this%set_grid_data(grid_data)
    end if
    !
    ! -- preprocess data
    call this%preprocess_input()
    !
    ! -- xt3d
    if (this%ixt3d /= 0) then
      call this%xt3d%xt3d_ar(ibound, this%k11, this%ik33, this%k33, &
                             this%sat, this%ik22, this%k22, &
                             this%iangle1, this%iangle2, this%iangle3, &
                             this%angle1, this%angle2, this%angle3, &
                             this%inewton, this%icelltype)
    end if
    !
    ! -- TVK
    if (this%intvk /= 0) then
      call this%tvk%ar(this%dis)
    end if
    !
    ! -- Return
    return
  end subroutine npf_ar

  !> @brief Read and prepare method for package
  !!
  !! Read and prepare NPF stress period data.
  !!
  !<
  subroutine npf_rp(this)
    implicit none
    class(GwfNpfType) :: this
! ------------------------------------------------------------------------------
    !
    ! -- TVK
    if (this%intvk /= 0) then
      call this%tvk%rp()
    end if
    !
    return
  end subroutine npf_rp

  subroutine npf_ad(this, nodes, hold, hnew, irestore)
! ******************************************************************************
! npf_ad -- Advance
! Subroutine (1) Sets hold to bot whenever a wettable cell is dry
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use TdisModule, only: kper, kstp
    !
    implicit none
    class(GwfNpfType) :: this
    integer(I4B), intent(in) :: nodes
    real(DP), dimension(nodes), intent(inout) :: hold
    real(DP), dimension(nodes), intent(inout) :: hnew
    integer(I4B), intent(in) :: irestore
    integer(I4B) :: n
! ------------------------------------------------------------------------------
    !
    ! -- loop through all cells and set hold=bot if wettable cell is dry
    if (this%irewet > 0) then
      do n = 1, this%dis%nodes
        if (this%wetdry(n) == DZERO) cycle
        if (this%ibound(n) /= 0) cycle
        hold(n) = this%dis%bot(n)
      end do
      !
      ! -- if restore state, then set hnew to DRY if it is a dry wettable cell
      do n = 1, this%dis%nodes
        if (this%wetdry(n) == DZERO) cycle
        if (this%ibound(n) /= 0) cycle
        hnew(n) = DHDRY
      end do
    end if
    !
    ! -- TVK
    if (this%intvk /= 0) then
      call this%tvk%ad()
    end if
    !
    ! -- If any K values have changed, we need to update CONDSAT or XT3D arrays
    if (this%kchangeper == kper .and. this%kchangestp == kstp) then
      if (this%ixt3d == 0) then
        !
        ! -- Update the saturated conductance for all connections
        ! -- of the affected nodes
        do n = 1, this%dis%nodes
          if (this%nodekchange(n) == 1) then
            call this%calc_condsat(n, .false.)
          end if
        end do
      else
        !
        ! -- Recompute XT3D coefficients for permanently confined connections
        if (this%xt3d%lamatsaved .and. .not. this%xt3d%ldispersion) then
          call this%xt3d%xt3d_fcpc(this%dis%nodes, .true.)
        end if
      end if
    end if
    !
    ! -- Return
    return
  end subroutine npf_ad

  subroutine npf_cf(this, kiter, nodes, hnew)
! ******************************************************************************
! npf_cf -- Formulate
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(GwfNpfType) :: this
    integer(I4B) :: kiter
    integer(I4B), intent(in) :: nodes
    real(DP), intent(inout), dimension(nodes) :: hnew
    ! -- local
    integer(I4B) :: n
    real(DP) :: satn
! ------------------------------------------------------------------------------
    !
    ! -- Perform wetting and drying
    if (this%inewton /= 1) then
      call this%wd(kiter, hnew)
    end if
    !
    ! -- Calculate saturation for convertible cells
    do n = 1, this%dis%nodes
      if (this%icelltype(n) /= 0) then
        if (this%ibound(n) == 0) then
          satn = DZERO
        else
          call this%thksat(n, hnew(n), satn)
        end if
        this%sat(n) = satn
      end if
    end do
    !
    ! -- Return
    return
  end subroutine npf_cf

  subroutine npf_fc(this, kiter, njasln, amat, idxglo, rhs, hnew)
! ******************************************************************************
! npf_fc -- Formulate
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use ConstantsModule, only: DONE
    ! -- dummy
    class(GwfNpfType) :: this
    integer(I4B) :: kiter
    integer(I4B), intent(in) :: njasln
    real(DP), dimension(njasln), intent(inout) :: amat
    integer(I4B), intent(in), dimension(:) :: idxglo
    real(DP), intent(inout), dimension(:) :: rhs
    real(DP), intent(inout), dimension(:) :: hnew
    ! -- local
    integer(I4B) :: n, m, ii, idiag, ihc
    integer(I4B) :: isymcon, idiagm
    real(DP) :: hyn, hym
    real(DP) :: cond
! ------------------------------------------------------------------------------
    !
    ! -- Calculate conductance and put into amat
    !
    if (this%ixt3d /= 0) then
      call this%xt3d%xt3d_fc(kiter, njasln, amat, idxglo, rhs, hnew)
    else
      !
      do n = 1, this%dis%nodes
        do ii = this%dis%con%ia(n) + 1, this%dis%con%ia(n + 1) - 1
          if (this%dis%con%mask(ii) == 0) cycle

          m = this%dis%con%ja(ii)
          !
          ! -- Calculate conductance only for upper triangle but insert into
          !    upper and lower parts of amat.
          if (m < n) cycle
          ihc = this%dis%con%ihc(this%dis%con%jas(ii))
          hyn = this%hy_eff(n, m, ihc, ipos=ii)
          hym = this%hy_eff(m, n, ihc, ipos=ii)
          !
          ! -- Vertical connection
          if (ihc == 0) then
            !
            ! -- Calculate vertical conductance
            cond = vcond(this%ibound(n), this%ibound(m), &
                         this%icelltype(n), this%icelltype(m), this%inewton, &
                         this%ivarcv, this%idewatcv, &
                         this%condsat(this%dis%con%jas(ii)), hnew(n), hnew(m), &
                         hyn, hym, &
                         this%sat(n), this%sat(m), &
                         this%dis%top(n), this%dis%top(m), &
                         this%dis%bot(n), this%dis%bot(m), &
                         this%dis%con%hwva(this%dis%con%jas(ii)))
            !
            ! -- Vertical flow for perched conditions
            if (this%iperched /= 0) then
              if (this%icelltype(m) /= 0) then
                if (hnew(m) < this%dis%top(m)) then
                  !
                  ! -- Fill row n
                  idiag = this%dis%con%ia(n)
                  rhs(n) = rhs(n) - cond * this%dis%bot(n)
                  amat(idxglo(idiag)) = amat(idxglo(idiag)) - cond
                  !
                  ! -- Fill row m
                  isymcon = this%dis%con%isym(ii)
                  amat(idxglo(isymcon)) = amat(idxglo(isymcon)) + cond
                  rhs(m) = rhs(m) + cond * this%dis%bot(n)
                  !
                  ! -- cycle the connection loop
                  cycle
                end if
              end if
            end if
            !
          else
            !
            ! -- Horizontal conductance
            cond = hcond(this%ibound(n), this%ibound(m), &
                         this%icelltype(n), this%icelltype(m), &
                         this%inewton, this%inewton, &
                         this%dis%con%ihc(this%dis%con%jas(ii)), &
                         this%icellavg, this%iusgnrhc, this%inwtupw, &
                         this%condsat(this%dis%con%jas(ii)), &
                         hnew(n), hnew(m), this%sat(n), this%sat(m), hyn, hym, &
                         this%dis%top(n), this%dis%top(m), &
                         this%dis%bot(n), this%dis%bot(m), &
                         this%dis%con%cl1(this%dis%con%jas(ii)), &
                         this%dis%con%cl2(this%dis%con%jas(ii)), &
                         this%dis%con%hwva(this%dis%con%jas(ii)), &
                         this%satomega, this%satmin)
          end if
          !
          ! -- Fill row n
          idiag = this%dis%con%ia(n)
          amat(idxglo(ii)) = amat(idxglo(ii)) + cond
          amat(idxglo(idiag)) = amat(idxglo(idiag)) - cond
          !
          ! -- Fill row m
          isymcon = this%dis%con%isym(ii)
          idiagm = this%dis%con%ia(m)
          amat(idxglo(isymcon)) = amat(idxglo(isymcon)) + cond
          amat(idxglo(idiagm)) = amat(idxglo(idiagm)) - cond
        end do
      end do
      !
    end if
    !
    ! -- Return
    return
  end subroutine npf_fc

  subroutine npf_fn(this, kiter, njasln, amat, idxglo, rhs, hnew)
! ******************************************************************************
! npf_fn -- Fill newton terms
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(GwfNpfType) :: this
    integer(I4B) :: kiter
    integer(I4B), intent(in) :: njasln
    real(DP), dimension(njasln), intent(inout) :: amat
    integer(I4B), intent(in), dimension(:) :: idxglo
    real(DP), intent(inout), dimension(:) :: rhs
    real(DP), intent(inout), dimension(:) :: hnew
    ! -- local
    integer(I4B) :: nodes, nja
    integer(I4B) :: n, m, ii, idiag
    integer(I4B) :: isymcon, idiagm
    integer(I4B) :: iups
    integer(I4B) :: idn
    real(DP) :: cond
    real(DP) :: consterm
    real(DP) :: filledterm
    real(DP) :: derv
    real(DP) :: hds
    real(DP) :: term
    real(DP) :: afac
    real(DP) :: topup
    real(DP) :: botup
    real(DP) :: topdn
    real(DP) :: botdn
! ------------------------------------------------------------------------------
    !
    ! -- add newton terms to solution matrix
    !
    nodes = this%dis%nodes
    nja = this%dis%con%nja
    if (this%ixt3d /= 0) then
      call this%xt3d%xt3d_fn(kiter, nodes, nja, njasln, amat, idxglo, rhs, hnew)
    else
      !
      do n = 1, nodes
        idiag = this%dis%con%ia(n)
        do ii = this%dis%con%ia(n) + 1, this%dis%con%ia(n + 1) - 1
          if (this%dis%con%mask(ii) == 0) cycle

          m = this%dis%con%ja(ii)
          isymcon = this%dis%con%isym(ii)
          ! work on upper triangle
          if (m < n) cycle
          if (this%dis%con%ihc(this%dis%con%jas(ii)) == 0 .and. &
              this%ivarcv == 0) then
            !call this%vcond(n,m,hnew(n),hnew(m),ii,cond)
            ! do nothing
          else
            ! determine upstream node
            iups = m
            if (hnew(m) < hnew(n)) iups = n
            idn = n
            if (iups == n) idn = m
            !
            ! -- no newton terms if upstream cell is confined
            if (this%icelltype(iups) == 0) cycle
            !
            ! -- Set the upstream top and bot, and then recalculate for a
            !    vertically staggered horizontal connection
            topup = this%dis%top(iups)
            botup = this%dis%bot(iups)
            if (this%dis%con%ihc(this%dis%con%jas(ii)) == 2) then
              topup = min(this%dis%top(n), this%dis%top(m))
              botup = max(this%dis%bot(n), this%dis%bot(m))
            end if
            !
            ! get saturated conductivity for derivative
            cond = this%condsat(this%dis%con%jas(ii))
            !
            ! -- if using MODFLOW-NWT upstream weighting option apply
            !    factor to remove average thickness
            if (this%inwtupw /= 0) then
              topdn = this%dis%top(idn)
              botdn = this%dis%bot(idn)
              afac = DTWO / (DONE + (topdn - botdn) / (topup - botup))
              cond = cond * afac
            end if
            !
            ! compute additional term
            consterm = -cond * (hnew(iups) - hnew(idn)) !needs to use hwadi instead of hnew(idn)
            !filledterm = cond
            filledterm = amat(idxglo(ii))
            derv = sQuadraticSaturationDerivative(topup, botup, hnew(iups), &
                                                  this%satomega, this%satmin)
            idiagm = this%dis%con%ia(m)
            ! fill jacobian for n being the upstream node
            if (iups == n) then
              hds = hnew(m)
              !isymcon =  this%dis%con%isym(ii)
              term = consterm * derv
              rhs(n) = rhs(n) + term * hnew(n) !+ amat(idxglo(isymcon)) * (dwadi * hds - hds) !need to add dwadi
              rhs(m) = rhs(m) - term * hnew(n) !- amat(idxglo(isymcon)) * (dwadi * hds - hds) !need to add dwadi
              ! fill in row of n
              amat(idxglo(idiag)) = amat(idxglo(idiag)) + term
              ! fill newton term in off diagonal if active cell
              if (this%ibound(n) > 0) then
                amat(idxglo(ii)) = amat(idxglo(ii)) !* dwadi !need to add dwadi
              end if
              !fill row of m
              amat(idxglo(idiagm)) = amat(idxglo(idiagm)) !- filledterm * (dwadi - DONE) !need to add dwadi
              ! fill newton term in off diagonal if active cell
              if (this%ibound(m) > 0) then
                amat(idxglo(isymcon)) = amat(idxglo(isymcon)) - term
              end if
              ! fill jacobian for m being the upstream node
            else
              hds = hnew(n)
              term = -consterm * derv
              rhs(n) = rhs(n) + term * hnew(m) !+ amat(idxglo(ii)) * (dwadi * hds - hds) !need to add dwadi
              rhs(m) = rhs(m) - term * hnew(m) !- amat(idxglo(ii)) * (dwadi * hds - hds) !need to add dwadi
              ! fill in row of n
              amat(idxglo(idiag)) = amat(idxglo(idiag)) !- filledterm * (dwadi - DONE) !need to add dwadi
              ! fill newton term in off diagonal if active cell
              if (this%ibound(n) > 0) then
                amat(idxglo(ii)) = amat(idxglo(ii)) + term
              end if
              !fill row of m
              amat(idxglo(idiagm)) = amat(idxglo(idiagm)) - term
              ! fill newton term in off diagonal if active cell
              if (this%ibound(m) > 0) then
                amat(idxglo(isymcon)) = amat(idxglo(isymcon)) !* dwadi  !need to add dwadi
              end if
            end if
          end if

        end do
      end do
      !
    end if
    !
    ! -- Return
    return
  end subroutine npf_fn

  subroutine npf_nur(this, neqmod, x, xtemp, dx, inewtonur, dxmax, locmax)
! ******************************************************************************
! bnd_nur -- under-relaxation
! Subroutine: (1) Under-relaxation of Groundwater Flow Model Heads for current
!                 outer iteration using the cell bottoms at the bottom of the
!                 model
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(GwfNpfType) :: this
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
! ------------------------------------------------------------------------------

    !
    ! -- Newton-Raphson under-relaxation
    do n = 1, this%dis%nodes
      if (this%ibound(n) < 1) cycle
      if (this%icelltype(n) > 0) then
        botm = this%dis%bot(this%ibotnode(n))
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
  end subroutine npf_nur

  subroutine npf_cq(this, hnew, flowja)
! ******************************************************************************
! npf_cq -- Calculate flowja
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(GwfNpfType) :: this
    real(DP), intent(inout), dimension(:) :: hnew
    real(DP), intent(inout), dimension(:) :: flowja
    ! -- local
    integer(I4B) :: n, ipos, m
    real(DP) :: qnm
! ------------------------------------------------------------------------------
    !
    ! -- Calculate the flow across each cell face and store in flowja
    !
    if (this%ixt3d /= 0) then
      call this%xt3d%xt3d_flowja(hnew, flowja)
    else
      !
      do n = 1, this%dis%nodes
        do ipos = this%dis%con%ia(n) + 1, this%dis%con%ia(n + 1) - 1
          m = this%dis%con%ja(ipos)
          if (m < n) cycle
          call this%qcalc(n, m, hnew(n), hnew(m), ipos, qnm)
          flowja(ipos) = qnm
          flowja(this%dis%con%isym(ipos)) = -qnm
        end do
      end do
      !
    end if
    !
    ! -- Return
    return
  end subroutine npf_cq

  subroutine sgwf_npf_thksat(this, n, hn, thksat)
! ******************************************************************************
! sgwf_npf_thksat -- Fractional cell saturation
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(GwfNpfType) :: this
    integer(I4B), intent(in) :: n
    real(DP), intent(in) :: hn
    real(DP), intent(inout) :: thksat
! ------------------------------------------------------------------------------
    !
    ! -- Standard Formulation
    if (hn >= this%dis%top(n)) then
      thksat = DONE
    else
      thksat = (hn - this%dis%bot(n)) / (this%dis%top(n) - this%dis%bot(n))
    end if
    !
    ! -- Newton-Raphson Formulation
    if (this%inewton /= 0) then
      thksat = sQuadraticSaturation(this%dis%top(n), this%dis%bot(n), hn, &
                                    this%satomega, this%satmin)
      !if (thksat < this%satmin) thksat = this%satmin
    end if
    !
    ! -- Return
    return
  end subroutine sgwf_npf_thksat

  subroutine sgwf_npf_qcalc(this, n, m, hn, hm, icon, qnm)
! ******************************************************************************
! sgwf_npf_qcalc -- Flow between two cells
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(GwfNpfType) :: this
    integer(I4B), intent(in) :: n
    integer(I4B), intent(in) :: m
    real(DP), intent(in) :: hn
    real(DP), intent(in) :: hm
    integer(I4B), intent(in) :: icon
    real(DP), intent(inout) :: qnm
    ! -- local
    real(DP) :: hyn, hym
    real(DP) :: condnm
    real(DP) :: hntemp, hmtemp
    integer(I4B) :: ihc
! ------------------------------------------------------------------------------
    !
    ! -- Initialize
    ihc = this%dis%con%ihc(this%dis%con%jas(icon))
    hyn = this%hy_eff(n, m, ihc, ipos=icon)
    hym = this%hy_eff(m, n, ihc, ipos=icon)
    !
    ! -- Calculate conductance
    if (ihc == 0) then
      condnm = vcond(this%ibound(n), this%ibound(m), &
                     this%icelltype(n), this%icelltype(m), this%inewton, &
                     this%ivarcv, this%idewatcv, &
                     this%condsat(this%dis%con%jas(icon)), hn, hm, &
                     hyn, hym, &
                     this%sat(n), this%sat(m), &
                     this%dis%top(n), this%dis%top(m), &
                     this%dis%bot(n), this%dis%bot(m), &
                     this%dis%con%hwva(this%dis%con%jas(icon)))
    else
      condnm = hcond(this%ibound(n), this%ibound(m), &
                     this%icelltype(n), this%icelltype(m), &
                     this%inewton, this%inewton, &
                     this%dis%con%ihc(this%dis%con%jas(icon)), &
                     this%icellavg, this%iusgnrhc, this%inwtupw, &
                     this%condsat(this%dis%con%jas(icon)), &
                     hn, hm, this%sat(n), this%sat(m), hyn, hym, &
                     this%dis%top(n), this%dis%top(m), &
                     this%dis%bot(n), this%dis%bot(m), &
                     this%dis%con%cl1(this%dis%con%jas(icon)), &
                     this%dis%con%cl2(this%dis%con%jas(icon)), &
                     this%dis%con%hwva(this%dis%con%jas(icon)), &
                     this%satomega, this%satmin)
    end if
    !
    ! -- Initialize hntemp and hmtemp
    hntemp = hn
    hmtemp = hm
    !
    ! -- Check and adjust for dewatered conditions
    if (this%iperched /= 0) then
      if (this%dis%con%ihc(this%dis%con%jas(icon)) == 0) then
        if (n > m) then
          if (this%icelltype(n) /= 0) then
            if (hn < this%dis%top(n)) hntemp = this%dis%bot(m)
          end if
        else
          if (this%icelltype(m) /= 0) then
            if (hm < this%dis%top(m)) hmtemp = this%dis%bot(n)
          end if
        end if
      end if
    end if
    !
    ! -- Calculate flow positive into cell n
    qnm = condnm * (hmtemp - hntemp)
    !
    ! -- Return
    return
  end subroutine sgwf_npf_qcalc

  subroutine npf_save_model_flows(this, flowja, icbcfl, icbcun)
! ******************************************************************************
! npf_save_model_flows -- Record flowja and calculate specific discharge if requested
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(GwfNpfType) :: this
    real(DP), dimension(:), intent(in) :: flowja
    integer(I4B), intent(in) :: icbcfl
    integer(I4B), intent(in) :: icbcun
    ! -- local
    integer(I4B) :: ibinun
    !data
    ! -- formats
! ------------------------------------------------------------------------------
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
      call this%dis%record_connection_array(flowja, ibinun, this%iout)
    end if
    !
    ! -- Calculate specific discharge at cell centers and write, if requested
    if (this%icalcspdis /= 0) then
      if (ibinun /= 0) call this%sav_spdis(ibinun)
    end if
    !
    ! -- Save saturation, if requested
    if (this%isavsat /= 0) then
      if (ibinun /= 0) call this%sav_sat(ibinun)
    end if
    !
    ! -- Return
    return
  end subroutine npf_save_model_flows

  subroutine npf_print_model_flows(this, ibudfl, flowja)
! ******************************************************************************
! npf_ot -- Budget
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use TdisModule, only: kper, kstp
    use ConstantsModule, only: LENBIGLINE
    ! -- dummy
    class(GwfNpfType) :: this
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
! ------------------------------------------------------------------------------
    !
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
  end subroutine npf_print_model_flows

  subroutine npf_da(this)
! ******************************************************************************
! npf_da -- Deallocate variables
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryManagerModule, only: mem_deallocate
    ! -- dummy
    class(GwfNpftype) :: this
! ------------------------------------------------------------------------------
    !
    ! -- TVK
    if (this%intvk /= 0) then
      call this%tvk%da()
      deallocate (this%tvk)
    end if
    !
    ! -- Strings
    !
    ! -- Scalars
    call mem_deallocate(this%iname)
    call mem_deallocate(this%ixt3d)
    call mem_deallocate(this%satomega)
    call mem_deallocate(this%hnoflo)
    call mem_deallocate(this%hdry)
    call mem_deallocate(this%icellavg)
    call mem_deallocate(this%iavgkeff)
    call mem_deallocate(this%ik22)
    call mem_deallocate(this%ik33)
    call mem_deallocate(this%iperched)
    call mem_deallocate(this%ivarcv)
    call mem_deallocate(this%idewatcv)
    call mem_deallocate(this%ithickstrt)
    call mem_deallocate(this%iusgnrhc)
    call mem_deallocate(this%inwtupw)
    call mem_deallocate(this%isavspdis)
    call mem_deallocate(this%isavsat)
    call mem_deallocate(this%icalcspdis)
    call mem_deallocate(this%irewet)
    call mem_deallocate(this%wetfct)
    call mem_deallocate(this%iwetit)
    call mem_deallocate(this%ihdwet)
    call mem_deallocate(this%satmin)
    call mem_deallocate(this%ibotnode)
    call mem_deallocate(this%iwetdry)
    call mem_deallocate(this%iangle1)
    call mem_deallocate(this%iangle2)
    call mem_deallocate(this%iangle3)
    call mem_deallocate(this%nedges)
    call mem_deallocate(this%lastedge)
    call mem_deallocate(this%ik22overk)
    call mem_deallocate(this%ik33overk)
    call mem_deallocate(this%intvk)
    call mem_deallocate(this%kchangeper)
    call mem_deallocate(this%kchangestp)
    !
    ! -- Deallocate arrays
    deallocate (this%aname)
    call mem_deallocate(this%ithickstartflag)
    call mem_deallocate(this%icelltype)
    call mem_deallocate(this%k11)
    call mem_deallocate(this%k22, 'K22', trim(this%memoryPath))
    call mem_deallocate(this%k33, 'K33', trim(this%memoryPath))
    call mem_deallocate(this%sat)
    call mem_deallocate(this%condsat)
    call mem_deallocate(this%wetdry)
    call mem_deallocate(this%angle1)
    call mem_deallocate(this%angle2)
    call mem_deallocate(this%angle3)
    call mem_deallocate(this%nodedge)
    call mem_deallocate(this%ihcedge)
    call mem_deallocate(this%propsedge)
    call mem_deallocate(this%spdis)
    call mem_deallocate(this%nodekchange)
    !
    ! -- deallocate parent
    call this%NumericalPackageType%da()
    !
    ! -- Return
    return
  end subroutine npf_da

  subroutine allocate_scalars(this)
! ******************************************************************************
! allocate_scalars -- Allocate scalar pointer variables
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryManagerModule, only: mem_allocate, mem_setptr
    use MemoryHelperModule, only: create_mem_path
    ! -- dummy
    class(GwfNpftype) :: this
! ------------------------------------------------------------------------------
    !
    ! -- allocate scalars in NumericalPackageType
    call this%NumericalPackageType%allocate_scalars()
    !
    ! -- Allocate scalars
    call mem_allocate(this%iname, 'INAME', this%memoryPath)
    call mem_allocate(this%ixt3d, 'IXT3D', this%memoryPath)
    call mem_allocate(this%satomega, 'SATOMEGA', this%memoryPath)
    call mem_allocate(this%hnoflo, 'HNOFLO', this%memoryPath)
    call mem_allocate(this%hdry, 'HDRY', this%memoryPath)
    call mem_allocate(this%icellavg, 'ICELLAVG', this%memoryPath)
    call mem_allocate(this%iavgkeff, 'IAVGKEFF', this%memoryPath)
    call mem_allocate(this%ik22, 'IK22', this%memoryPath)
    call mem_allocate(this%ik33, 'IK33', this%memoryPath)
    call mem_allocate(this%ik22overk, 'IK22OVERK', this%memoryPath)
    call mem_allocate(this%ik33overk, 'IK33OVERK', this%memoryPath)
    call mem_allocate(this%iperched, 'IPERCHED', this%memoryPath)
    call mem_allocate(this%ivarcv, 'IVARCV', this%memoryPath)
    call mem_allocate(this%idewatcv, 'IDEWATCV', this%memoryPath)
    call mem_allocate(this%ithickstrt, 'ITHICKSTRT', this%memoryPath)
    call mem_allocate(this%iusgnrhc, 'IUSGNRHC', this%memoryPath)
    call mem_allocate(this%inwtupw, 'INWTUPW', this%memoryPath)
    call mem_allocate(this%icalcspdis, 'ICALCSPDIS', this%memoryPath)
    call mem_allocate(this%isavspdis, 'ISAVSPDIS', this%memoryPath)
    call mem_allocate(this%isavsat, 'ISAVSAT', this%memoryPath)
    call mem_allocate(this%irewet, 'IREWET', this%memoryPath)
    call mem_allocate(this%wetfct, 'WETFCT', this%memoryPath)
    call mem_allocate(this%iwetit, 'IWETIT', this%memoryPath)
    call mem_allocate(this%ihdwet, 'IHDWET', this%memoryPath)
    call mem_allocate(this%satmin, 'SATMIN', this%memoryPath)
    call mem_allocate(this%iangle1, 'IANGLE1', this%memoryPath)
    call mem_allocate(this%iangle2, 'IANGLE2', this%memoryPath)
    call mem_allocate(this%iangle3, 'IANGLE3', this%memoryPath)
    call mem_allocate(this%iwetdry, 'IWETDRY', this%memoryPath)
    call mem_allocate(this%nedges, 'NEDGES', this%memoryPath)
    call mem_allocate(this%lastedge, 'LASTEDGE', this%memoryPath)
    call mem_allocate(this%intvk, 'INTVK', this%memoryPath)
    call mem_allocate(this%kchangeper, 'KCHANGEPER', this%memoryPath)
    call mem_allocate(this%kchangestp, 'KCHANGESTP', this%memoryPath)
    !
    ! -- set pointer to inewtonur
    call mem_setptr(this%igwfnewtonur, 'INEWTONUR', &
                    create_mem_path(this%name_model))
    !
    ! -- Initialize value
    this%iname = 8
    this%ixt3d = 0
    this%satomega = DZERO
    this%hnoflo = DHNOFLO !1.d30
    this%hdry = DHDRY !-1.d30
    this%icellavg = 0
    this%iavgkeff = 0
    this%ik22 = 0
    this%ik33 = 0
    this%ik22overk = 0
    this%ik33overk = 0
    this%iperched = 0
    this%ivarcv = 0
    this%idewatcv = 0
    this%ithickstrt = 0
    this%iusgnrhc = 0
    this%inwtupw = 0
    this%icalcspdis = 0
    this%isavspdis = 0
    this%isavsat = 0
    this%irewet = 0
    this%wetfct = DONE
    this%iwetit = 1
    this%ihdwet = 0
    this%satmin = DZERO ! DEM7
    this%iangle1 = 0
    this%iangle2 = 0
    this%iangle3 = 0
    this%iwetdry = 0
    this%nedges = 0
    this%lastedge = 0
    this%intvk = 0
    this%kchangeper = 0
    this%kchangestp = 0
    !
    ! -- If newton is on, then NPF creates asymmetric matrix
    this%iasym = this%inewton
    !
    ! -- Return
    return
  end subroutine allocate_scalars

  subroutine allocate_arrays(this, ncells, njas)
! ******************************************************************************
! allocate_arrays -- Allocate npf arrays
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryManagerModule, only: mem_allocate
    ! -- dummy
    class(GwfNpftype) :: this
    integer(I4B), intent(in) :: ncells
    integer(I4B), intent(in) :: njas
    ! -- local
    integer(I4B) :: n
! ------------------------------------------------------------------------------
    !
    call mem_allocate(this%ithickstartflag, ncells, 'ITHICKSTARTFLAG', &
                      this%memoryPath)
    call mem_allocate(this%icelltype, ncells, 'ICELLTYPE', this%memoryPath)
    call mem_allocate(this%k11, ncells, 'K11', this%memoryPath)
    call mem_allocate(this%sat, ncells, 'SAT', this%memoryPath)
    call mem_allocate(this%condsat, njas, 'CONDSAT', this%memoryPath)
    !
    ! -- Optional arrays dimensioned to full size initially
    call mem_allocate(this%k22, ncells, 'K22', this%memoryPath)
    call mem_allocate(this%k33, ncells, 'K33', this%memoryPath)
    call mem_allocate(this%wetdry, ncells, 'WETDRY', this%memoryPath)
    call mem_allocate(this%angle1, ncells, 'ANGLE1', this%memoryPath)
    call mem_allocate(this%angle2, ncells, 'ANGLE2', this%memoryPath)
    call mem_allocate(this%angle3, ncells, 'ANGLE3', this%memoryPath)
    !
    ! -- Optional arrays
    call mem_allocate(this%ibotnode, 0, 'IBOTNODE', this%memoryPath)
    !
    ! -- Specific discharge
    if (this%icalcspdis == 1) then
      call mem_allocate(this%spdis, 3, ncells, 'SPDIS', this%memoryPath)
      call mem_allocate(this%nodedge, this%nedges, 'NODEDGE', this%memoryPath)
      call mem_allocate(this%ihcedge, this%nedges, 'IHCEDGE', this%memoryPath)
      call mem_allocate(this%propsedge, 5, this%nedges, 'PROPSEDGE', &
                        this%memoryPath)
      do n = 1, ncells
        this%spdis(:, n) = DZERO
      end do
    else
      call mem_allocate(this%spdis, 3, 0, 'SPDIS', this%memoryPath)
      call mem_allocate(this%nodedge, 0, 'NODEDGE', this%memoryPath)
      call mem_allocate(this%ihcedge, 0, 'IHCEDGE', this%memoryPath)
      call mem_allocate(this%propsedge, 0, 0, 'PROPSEDGE', this%memoryPath)
    end if
    !
    ! -- Time-varying property flag arrays
    call mem_allocate(this%nodekchange, ncells, 'NODEKCHANGE', this%memoryPath)
    !
    ! -- initialize iangle1, iangle2, iangle3, and wetdry
    do n = 1, ncells
      this%angle1(n) = DZERO
      this%angle2(n) = DZERO
      this%angle3(n) = DZERO
      this%wetdry(n) = DZERO
      this%nodekchange(n) = DZERO
    end do
    !
    ! -- allocate variable names
    allocate (this%aname(this%iname))
    this%aname = ['               ICELLTYPE', '                       K', &
                  '                     K33', '                     K22', &
                  '                  WETDRY', '                  ANGLE1', &
                  '                  ANGLE2', '                  ANGLE3']
    !
    ! -- return
    return
  end subroutine allocate_arrays

  subroutine read_options(this)
! ******************************************************************************
! read_options -- Read the options
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use ConstantsModule, only: LINELENGTH
    use SimModule, only: store_error, count_errors
    implicit none
    ! -- dummy
    class(GwfNpftype) :: this
    ! -- local
    character(len=LINELENGTH) :: errmsg, keyword, fname
    integer(I4B) :: ierr
    logical :: isfound, endOfBlock
    ! -- formats
    character(len=*), parameter :: fmtiprflow = &
      "(4x,'CELL-BY-CELL FLOW INFORMATION WILL BE PRINTED TO LISTING FILE &
      &WHENEVER ICBCFL IS NOT ZERO.')"
    character(len=*), parameter :: fmtisvflow = &
      "(4x,'CELL-BY-CELL FLOW INFORMATION WILL BE SAVED TO BINARY FILE &
      &WHENEVER ICBCFL IS NOT ZERO.')"
    character(len=*), parameter :: fmtcellavg = &
      &"(4x,'ALTERNATIVE CELL AVERAGING HAS BEEN SET TO ', a)"
    character(len=*), parameter :: fmtnct = &
      &"(1x, 'Negative cell thickness at cell: ', a)"
    ! -- data
! ------------------------------------------------------------------------------
    !
    ! -- get options block
    call this%parser%GetBlock('OPTIONS', isfound, ierr, &
                              supportOpenClose=.true., blockRequired=.false.)
    !
    ! -- parse options block if detected
    if (isfound) then
      write (this%iout, '(1x,a)') 'PROCESSING NPF OPTIONS'
      do
        call this%parser%GetNextLine(endOfBlock)
        if (endOfBlock) exit
        call this%parser%GetStringCaps(keyword)
        select case (keyword)
        case ('PRINT_FLOWS')
          this%iprflow = 1
          write (this%iout, fmtiprflow)
        case ('SAVE_FLOWS')
          this%ipakcb = -1
          write (this%iout, fmtisvflow)
        case ('ALTERNATIVE_CELL_AVERAGING')
          call this%parser%GetStringCaps(keyword)
          select case (keyword)
          case ('LOGARITHMIC')
            this%icellavg = 1
            write (this%iout, fmtcellavg) 'LOGARITHMIC'
          case ('AMT-LMK')
            this%icellavg = 2
            write (this%iout, fmtcellavg) 'AMT-LMK'
          case ('AMT-HMK')
            this%icellavg = 3
            write (this%iout, fmtcellavg) 'AMT-HMK'
          case default
            write (errmsg, '(4x,a,a)') 'UNKNOWN CELL AVERAGING METHOD: ', &
              keyword
            call store_error(errmsg)
            call this%parser%StoreErrorUnit()
          end select
          write (this%iout, '(4x,a,a)') &
            'CELL AVERAGING METHOD HAS BEEN SET TO: ', keyword
        case ('THICKSTRT')
          this%ithickstrt = 1
          write (this%iout, '(4x,a)') 'THICKSTRT OPTION HAS BEEN ACTIVATED.'
        case ('PERCHED')
          this%iperched = 1
          write (this%iout, '(4x,a)') &
            'VERTICAL FLOW WILL BE ADJUSTED FOR PERCHED CONDITIONS.'
        case ('VARIABLECV')
          this%ivarcv = 1
          write (this%iout, '(4x,a)') &
            'VERTICAL CONDUCTANCE VARIES WITH WATER TABLE.'
          call this%parser%GetStringCaps(keyword)
          if (keyword == 'DEWATERED') then
            this%idewatcv = 1
            write (this%iout, '(4x,a)') &
              'VERTICAL CONDUCTANCE ACCOUNTS FOR DEWATERED PORTION OF '// &
              'AN UNDERLYING CELL.'
          end if
        case ('REWET')
          call this%rewet_options()
        case ('XT3D')
          this%ixt3d = 1
          write (this%iout, '(4x,a)') &
            'XT3D FORMULATION IS SELECTED.'
          call this%parser%GetStringCaps(keyword)
          if (keyword == 'RHS') then
            this%ixt3d = 2
          end if
        case ('SAVE_SPECIFIC_DISCHARGE')
          this%icalcspdis = 1
          this%isavspdis = 1
          write (this%iout, '(4x,a)') &
            'SPECIFIC DISCHARGE WILL BE CALCULATED AT CELL CENTERS '// &
            'AND WRITTEN TO DATA-SPDIS IN BUDGET FILE WHEN REQUESTED.'
        case ('SAVE_SATURATION')
          this%isavsat = 1
          write (this%iout, '(4x,a)') &
            'SATURATION WILL BE WRITTEN TO DATA-SAT IN BUDGET FILE '// &
            'WHEN REQUESTED.'
        case ('K22OVERK')
          this%ik22overk = 1
          write (this%iout, '(4x,a)') &
            'VALUES SPECIFIED FOR K22 ARE ANISOTROPY RATIOS AND '// &
            'WILL BE MULTIPLIED BY K BEFORE BEING USED IN CALCULATIONS.'
        case ('K33OVERK')
          this%ik33overk = 1
          write (this%iout, '(4x,a)') &
            'VALUES SPECIFIED FOR K33 ARE ANISOTROPY RATIOS AND '// &
            'WILL BE MULTIPLIED BY K BEFORE BEING USED IN CALCULATIONS.'
        case ('TVK6')
          if (this%intvk /= 0) then
            errmsg = 'Multiple TVK6 keywords detected in OPTIONS block.'// &
                     ' Only one TVK6 entry allowed.'
            call store_error(errmsg, terminate=.TRUE.)
          end if
          call this%parser%GetStringCaps(keyword)
          if (trim(adjustl(keyword)) /= 'FILEIN') then
            errmsg = 'TVK6 keyword must be followed by "FILEIN" '// &
                     'then by filename.'
            call store_error(errmsg, terminate=.TRUE.)
          end if
          call this%parser%GetString(fname)
          this%intvk = GetUnit()
          call openfile(this%intvk, this%iout, fname, 'TVK')
          call tvk_cr(this%tvk, this%name_model, this%intvk, this%iout)
          !
          ! -- The following are options that are only available in the
          !    development version and are not included in the documentation.
          !    These options are only available when IDEVELOPMODE in
          !    constants module is set to 1
        case ('DEV_NO_NEWTON')
          call this%parser%DevOpt()
          this%inewton = 0
          write (this%iout, '(4x,a)') &
            'NEWTON-RAPHSON method disabled for unconfined cells'
          this%iasym = 0
        case ('DEV_MODFLOWUSG_UPSTREAM_WEIGHTED_SATURATION')
          call this%parser%DevOpt()
          this%iusgnrhc = 1
          write (this%iout, '(4x,a)') &
            'MODFLOW-USG saturation calculation method will be used '
        case ('DEV_MODFLOWNWT_UPSTREAM_WEIGHTING')
          call this%parser%DevOpt()
          this%inwtupw = 1
          write (this%iout, '(4x,a)') &
            'MODFLOW-NWT upstream weighting method will be used '
        case ('DEV_MINIMUM_SATURATED_THICKNESS')
          call this%parser%DevOpt()
          this%satmin = this%parser%GetDouble()
          write (this%iout, '(4x,a,1pg15.6)') &
            'MINIMUM SATURATED THICKNESS HAS BEEN SET TO: ', &
            this%satmin
        case ('DEV_OMEGA')
          call this%parser%DevOpt()
          this%satomega = this%parser%GetDouble()
          write (this%iout, '(4x,a,1pg15.6)') &
            'SATURATION OMEGA: ', this%satomega

        case default
          write (errmsg, '(4x,a,a)') 'Unknown NPF option: ', trim(keyword)
          call store_error(errmsg)
          call this%parser%StoreErrorUnit()
        end select
      end do
      write (this%iout, '(1x,a)') 'END OF NPF OPTIONS'
    end if
    ! -- check if this%iusgnrhc has been enabled for a model that is not using
    !    the Newton-Raphson formulation
    if (this%iusgnrhc > 0 .and. this%inewton == 0) then
      this%iusgnrhc = 0
      write (this%iout, '(4x,a,3(1x,a))') &
        '****WARNING. MODFLOW-USG saturation calculation not needed', &
        'for a model that is using the standard conductance formulation.', &
        'Resetting DEV_MODFLOWUSG_UPSTREAM_WEIGHTED_SATURATION OPTION from', &
        '1 to 0.'
    end if
    !
    ! -- check that the this%inwtupw option is not specified for non-newton
    !    models
    if (this%inwtupw /= 0 .and. this%inewton == 0) then
      this%inwtupw = 0
      write (this%iout, '(4x,a,3(1x,a))') &
        '****WARNING. The DEV_MODFLOWNWT_UPSTREAM_WEIGHTING option has', &
        'been specified for a model that is using the standard conductance', &
        'formulation. Resetting DEV_MODFLOWNWT_UPSTREAM_WEIGHTING OPTION from', &
        '1 to 0.'
    end if
    !
    ! -- check that the transmissivity weighting functions are not specified with
    !    with the this%inwtupw option
    if (this%inwtupw /= 0 .and. this%icellavg < 2) then
      write (errmsg, '(4x,a,2(1x,a))') &
        '****ERROR. THE DEV_MODFLOWNWT_UPSTREAM_WEIGHTING OPTION CAN', &
        'ONLY BE SPECIFIED WITH THE AMT-LMK AND AMT-HMK', &
        'ALTERNATIVE_CELL_AVERAGING OPTIONS IN THE NPF PACKAGE.'
      call store_error(errmsg)
    end if
    !
    ! -- check that this%iusgnrhc and this%inwtupw have not both been enabled
    if (this%iusgnrhc /= 0 .and. this%inwtupw /= 0) then
      write (errmsg, '(4x,a,2(1x,a))') &
        '****ERROR. THE DEV_MODFLOWUSG_UPSTREAM_WEIGHTED_SATURATION', &
        'AND DEV_MODFLOWNWT_UPSTREAM_WEIGHTING OPTIONS CANNOT BE', &
        'SPECIFIED IN THE SAME NPF PACKAGE.'
      call store_error(errmsg)
    end if
    !
    ! -- set omega value used for saturation calculations
    if (this%inewton > 0) then
      this%satomega = DEM6
    end if
    !
    ! -- terminate if errors encountered in options block
    if (count_errors() > 0) then
      call this%parser%StoreErrorUnit()
    end if
    !
    ! -- Return
    return
  end subroutine read_options

  subroutine set_options(this, options)
    class(GwfNpftype) :: this
    type(GwfNpfOptionsType), intent(in) :: options

    this%icellavg = options%icellavg
    this%ithickstrt = options%ithickstrt
    this%iperched = options%iperched
    this%ivarcv = options%ivarcv
    this%idewatcv = options%idewatcv
    this%irewet = options%irewet
    this%wetfct = options%wetfct
    this%iwetit = options%iwetit
    this%ihdwet = options%ihdwet

  end subroutine set_options

  subroutine rewet_options(this)
! ******************************************************************************
! rewet_options -- Set rewet options
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use SimModule, only: store_error
    use ConstantsModule, only: LINELENGTH
    ! -- dummy
    class(GwfNpftype) :: this
    ! -- local
    integer(I4B) :: ival
    character(len=LINELENGTH) :: keyword, errmsg
    logical, dimension(3) :: lfound = .false.
! ------------------------------------------------------------------------------
    !
    ! -- If rewet already set, then terminate with error
    if (this%irewet == 1) then
      write (errmsg, '(a)') 'ERROR WITH NPF REWET OPTION.  REWET WAS '// &
        'ALREADY SET.  REMOVE DUPLICATE REWET ENTRIES '// &
        'FROM NPF OPTIONS BLOCK.'
      call store_error(errmsg)
      call this%parser%StoreErrorUnit()
    end if
    this%irewet = 1
    write (this%iout, '(4x,a)') 'REWETTING IS ACTIVE.'
    !
    ! -- Parse rewet options
    do
      call this%parser%GetStringCaps(keyword)
      if (keyword == '') exit
      select case (keyword)
      case ('WETFCT')
        this%wetfct = this%parser%GetDouble()
        write (this%iout, '(4x,a,1pg15.6)') &
          'WETTING FACTOR HAS BEEN SET TO: ', this%wetfct
        lfound(1) = .true.
      case ('IWETIT')
        if (.not. lfound(1)) then
          write (errmsg, '(4x,a)') &
            'NPF rewetting flags must be specified in order. '// &
            'Found iwetit but wetfct not specified.'
          call store_error(errmsg)
          call this%parser%StoreErrorUnit()
        end if
        ival = this%parser%GetInteger()
        if (ival <= 0) ival = 1
        this%iwetit = ival
        write (this%iout, '(4x,a,i5)') 'IWETIT HAS BEEN SET TO: ', &
          this%iwetit
        lfound(2) = .true.
      case ('IHDWET')
        if (.not. lfound(2)) then
          write (errmsg, '(4x,a)') &
            'NPF rewetting flags must be specified in order. '// &
            'Found ihdwet but iwetit not specified.'
          call store_error(errmsg)
          call this%parser%StoreErrorUnit()
        end if
        this%ihdwet = this%parser%GetInteger()
        write (this%iout, '(4x,a,i5)') 'IHDWET HAS BEEN SET TO: ', &
          this%ihdwet
        lfound(3) = .true.
      case default
        write (errmsg, '(4x,a,a)') 'Unknown NPF rewet option: ', trim(keyword)
        call store_error(errmsg)
        call this%parser%StoreErrorUnit()
      end select
    end do
    !
    if (.not. lfound(3)) then
      write (errmsg, '(4x,a)') &
        '****ERROR. NPF REWETTING FLAGS MUST BE SPECIFIED IN ORDER. '// &
        'DID NOT FIND IHDWET AS LAST REWET SETTING.'
      call store_error(errmsg)
      call this%parser%StoreErrorUnit()
    end if
    !
    ! -- Write rewet settings
    write (this%iout, '(4x, a)') 'THE FOLLOWING REWET SETTINGS WILL BE USED.'
    write (this%iout, '(6x, a,1pg15.6)') '  WETFCT = ', this%wetfct
    write (this%iout, '(6x, a,i0)') '  IWETIT = ', this%iwetit
    write (this%iout, '(6x, a,i0)') '  IHDWET = ', this%ihdwet
    !
    ! -- Return
    return
  end subroutine rewet_options

  subroutine check_options(this)
! ******************************************************************************
! check_options -- Check for conflicting NPF options
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use SimModule, only: store_error, count_errors
    use ConstantsModule, only: LINELENGTH
    ! -- dummy
    class(GwfNpftype) :: this
    ! -- local
    character(len=LINELENGTH) :: errmsg
! ------------------------------------------------------------------------------
    !
    if (this%inewton > 0) then
      if (this%iperched > 0) then
        write (errmsg, '(a)') 'ERROR IN NPF OPTIONS. NEWTON OPTION CANNOT '// &
          'BE USED WITH PERCHED OPTION.'
        call store_error(errmsg)
      end if
      if (this%ivarcv > 0) then
        write (errmsg, '(a)') 'ERROR IN NPF OPTIONS. NEWTON OPTION CANNOT '// &
          'BE USED WITH VARIABLECV OPTION.'
        call store_error(errmsg)
      end if
      if (this%irewet > 0) then
        write (errmsg, '(a)') 'ERROR IN NPF OPTIONS. NEWTON OPTION CANNOT '// &
          'BE USED WITH REWET OPTION.'
        call store_error(errmsg)
      end if
    end if
    !
    if (this%ixt3d /= 0) then
      if (this%icellavg > 0) then
        write (errmsg, '(a)') 'ERROR IN NPF OPTIONS. '// &
          'ALTERNATIVE_CELL_AVERAGING OPTION '// &
          'CANNOT BE USED WITH XT3D OPTION.'
        call store_error(errmsg)
      end if
      if (this%ithickstrt > 0) then
        write (errmsg, '(a)') 'ERROR IN NPF OPTIONS. THICKSTRT OPTION '// &
          'CANNOT BE USED WITH XT3D OPTION.'
        call store_error(errmsg)
      end if
      if (this%iperched > 0) then
        write (errmsg, '(a)') 'ERROR IN NPF OPTIONS. PERCHED OPTION '// &
          'CANNOT BE USED WITH XT3D OPTION.'
        call store_error(errmsg)
      end if
      if (this%ivarcv > 0) then
        write (errmsg, '(a)') 'ERROR IN NPF OPTIONS. VARIABLECV OPTION '// &
          'CANNOT BE USED WITH XT3D OPTION.'
        call store_error(errmsg)
      end if
    end if
    !
    ! -- Terminate if errors
    if (count_errors() > 0) then
      call this%parser%StoreErrorUnit()
    end if
    !
    ! -- Return
    return
  end subroutine check_options

  subroutine read_grid_data(this)
! ******************************************************************************
! read_grid_data -- read the npf data block
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use ConstantsModule, only: LINELENGTH, DONE, DPIO180
    use MemoryManagerModule, only: mem_allocate, mem_reallocate, mem_deallocate, &
                                   mem_reassignptr
    use SimModule, only: store_error, count_errors
    ! -- dummy
    class(GwfNpftype) :: this
    ! -- local
    character(len=LINELENGTH) :: errmsg
    integer(I4B) :: n, ierr
    logical :: isfound
    logical, dimension(8) :: lname
    character(len=24), dimension(:), pointer :: aname
    character(len=24), dimension(8) :: varinames
    ! -- formats
    character(len=*), parameter :: fmtiprflow = &
      "(4x,'CELL-BY-CELL FLOW INFORMATION WILL BE PRINTED TO LISTING FILE &
      &WHENEVER ICBCFL IS NOT ZERO.')"
    character(len=*), parameter :: fmtisvflow = &
      "(4x,'CELL-BY-CELL FLOW INFORMATION WILL BE SAVED TO BINARY FILE &
      &WHENEVER ICBCFL IS NOT ZERO.')"
    character(len=*), parameter :: fmtnct = &
      &"(1x, 'Negative cell thickness at cell: ', a)"
    ! -- data
    !data aname(1) /'               ICELLTYPE'/
    !data aname(2) /'                       K'/
    !data aname(3) /'                     K33'/
    !data aname(4) /'                     K22'/
    !data aname(5) /'                  WETDRY'/
    !data aname(6) /'                  ANGLE1'/
    !data aname(7) /'                  ANGLE2'/
    !data aname(8) /'                  ANGLE3'/
! ------------------------------------------------------------------------------
    !
    ! -- Initialize
    aname => this%aname
    do n = 1, size(aname)
      varinames(n) = adjustl(aname(n))
      lname(n) = .false.
    end do
    varinames(2) = 'K11                     '
    !
    ! -- Read all of the arrays in the GRIDDATA block using the get_block_data
    !    method, which is part of NumericalPackageType
    call this%parser%GetBlock('GRIDDATA', isfound, ierr)
    if (isfound) then
      write (this%iout, '(1x,a)') 'PROCESSING GRIDDATA'
      call this%get_block_data(aname, lname, varinames)
    else
      write (errmsg, '(1x,a)') 'Required GRIDDATA block not found.'
      call store_error(errmsg)
      call this%parser%StoreErrorUnit()
    end if
    !
    ! -- Check for ICELLTYPE
    if (.not. lname(1)) then
      write (errmsg, '(a, a, a)') 'Error in GRIDDATA block: ', &
        trim(adjustl(aname(1))), ' not found.'
      call store_error(errmsg)
    end if
    !
    ! -- Check for K
    if (.not. lname(2)) then
      write (errmsg, '(a, a, a)') 'Error in GRIDDATA block: ', &
        trim(adjustl(aname(2))), ' not found.'
      call store_error(errmsg)
    end if
    !
    ! -- set ik33 flag
    if (lname(3)) then
      this%ik33 = 1
    else
      if (this%ik33overk /= 0) then
        write (errmsg, '(a)') 'K33OVERK option specified but K33 not specified.'
        call store_error(errmsg)
      end if
      write (this%iout, '(1x, a)') 'K33 not provided.  Assuming K33 = K.'
      call mem_reassignptr(this%k33, 'K33', trim(this%memoryPath), &
                           'K11', trim(this%memoryPath))
    end if
    !
    ! -- set ik22 flag
    if (lname(4)) then
      this%ik22 = 1
    else
      if (this%ik22overk /= 0) then
        write (errmsg, '(a)') 'K22OVERK option specified but K22 not specified.'
        call store_error(errmsg)
      end if
      write (this%iout, '(1x, a)') 'K22 not provided.  Assuming K22 = K.'
      call mem_reassignptr(this%k22, 'K22', trim(this%memoryPath), &
                           'K11', trim(this%memoryPath))
    end if
    !
    ! -- Set WETDRY
    if (lname(5)) then
      this%iwetdry = 1
    else
      call mem_reallocate(this%wetdry, 1, 'WETDRY', trim(this%memoryPath))
    end if
    !
    ! -- set angle flags
    if (lname(6)) then
      this%iangle1 = 1
    else
      if (this%ixt3d == 0) then
        call mem_reallocate(this%angle1, 1, 'ANGLE1', trim(this%memoryPath))
      end if
    end if
    if (lname(7)) then
      this%iangle2 = 1
    else
      if (this%ixt3d == 0) then
        call mem_reallocate(this%angle2, 1, 'ANGLE2', trim(this%memoryPath))
      end if
    end if
    if (lname(8)) then
      this%iangle3 = 1
    else
      if (this%ixt3d == 0) then
        call mem_reallocate(this%angle3, 1, 'ANGLE3', trim(this%memoryPath))
      end if
    end if
    !
    ! -- terminate if read errors encountered
    if (count_errors() > 0) then
      call this%parser%StoreErrorUnit()
    end if
    !
    ! -- Final NPFDATA message
    write (this%iout, '(1x,a)') 'END PROCESSING GRIDDATA'
    !
    ! -- Return
    return
  end subroutine read_grid_data

  subroutine set_grid_data(this, npf_data)
    use MemoryManagerModule, only: mem_reallocate, mem_reassignptr
    class(GwfNpfType), intent(inout) :: this
    type(GwfNpfGridDataType), intent(in) :: npf_data

    ! fill grid arrays
    call this%dis%fill_grid_array(npf_data%icelltype, this%icelltype)
    call this%dis%fill_grid_array(npf_data%k11, this%k11)

    if (npf_data%ik22 == 1) then
      this%ik22 = 1
      call this%dis%fill_grid_array(npf_data%k22, this%k22)
    else
      ! if not present, then K22 = K11
      this%ik22 = 0
      call mem_reassignptr(this%k22, 'K22', trim(this%memoryPath), &
                           'K11', trim(this%memoryPath))
    end if

    if (npf_data%ik33 == 1) then
      this%ik33 = 1
      call this%dis%fill_grid_array(npf_data%k33, this%k33)
    else
      ! if not present, then K33 = K11
      this%ik33 = 0
      call mem_reassignptr(this%k33, 'K33', trim(this%memoryPath), &
                           'K11', trim(this%memoryPath))
    end if

    if (npf_data%iwetdry == 1) then
      call this%dis%fill_grid_array(npf_data%wetdry, this%wetdry)
    else
      ! if not present, then compress array
      this%iwetdry = 0
      call mem_reallocate(this%wetdry, 1, 'WETDRY', trim(this%memoryPath))
    end if

    if (npf_data%iangle1 == 1) then
      this%iangle1 = 1
      call this%dis%fill_grid_array(npf_data%angle1, this%angle1)
    else
      ! if not present, then compress array
      this%iangle1 = 0
      call mem_reallocate(this%angle1, 1, 'ANGLE1', trim(this%memoryPath))
    end if

    if (npf_data%iangle2 == 1) then
      this%iangle2 = 1
      call this%dis%fill_grid_array(npf_data%angle2, this%angle2)
    else
      ! if not present, then compress array
      this%iangle2 = 0
      call mem_reallocate(this%angle2, 1, 'ANGLE2', trim(this%memoryPath))
    end if

    if (npf_data%iangle3 == 1) then
      this%iangle3 = 1
      call this%dis%fill_grid_array(npf_data%angle3, this%angle3)
    else
      ! if not present, then compress array
      this%iangle3 = 0
      call mem_reallocate(this%angle3, 1, 'ANGLE3', trim(this%memoryPath))
    end if

  end subroutine set_grid_data

  subroutine prepcheck(this)
! ******************************************************************************
! prepcheck -- Initialize and check NPF data
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use ConstantsModule, only: LINELENGTH, DPIO180
    use SimModule, only: store_error, count_errors
    ! -- dummy
    class(GwfNpfType) :: this
    ! -- local
    character(len=24), dimension(:), pointer :: aname
    character(len=LINELENGTH) :: cellstr, errmsg
    integer(I4B) :: nerr, n
    ! -- format
    character(len=*), parameter :: fmtkerr = &
      &"(1x, 'Hydraulic property ',a,' is <= 0 for cell ',a, ' ', 1pg15.6)"
    character(len=*), parameter :: fmtkerr2 = &
      &"(1x, '... ', i0,' additional errors not shown for ',a)"
! ------------------------------------------------------------------------------
    !
    ! -- initialize
    aname => this%aname
    !
    ! -- check k11
    nerr = 0
    do n = 1, size(this%k11)
      if (this%k11(n) <= DZERO) then
        nerr = nerr + 1
        if (nerr <= 20) then
          call this%dis%noder_to_string(n, cellstr)
          write (errmsg, fmtkerr) trim(adjustl(aname(2))), trim(cellstr), &
            this%k11(n)
          call store_error(errmsg)
        end if
      end if
    end do
    if (nerr > 20) then
      write (errmsg, fmtkerr2) nerr, trim(adjustl(aname(2)))
      call store_error(errmsg)
    end if
    !
    ! -- check k33 because it was read
    if (this%ik33 /= 0) then
      !
      ! -- Check to make sure values are greater than or equal to zero
      nerr = 0
      do n = 1, size(this%k33)
        if (this%ik33overk /= 0) this%k33(n) = this%k33(n) * this%k11(n)
        if (this%k33(n) <= DZERO) then
          nerr = nerr + 1
          if (nerr <= 20) then
            call this%dis%noder_to_string(n, cellstr)
            write (errmsg, fmtkerr) trim(adjustl(aname(3))), trim(cellstr), &
              this%k33(n)
            call store_error(errmsg)
          end if
        end if
      end do
      if (nerr > 20) then
        write (errmsg, fmtkerr2) nerr, trim(adjustl(aname(3)))
        call store_error(errmsg)
      end if
    end if
    !
    ! -- check k22 because it was read
    if (this%ik22 /= 0) then
      !
      ! -- Check to make sure that angles are available
      if (this%dis%con%ianglex == 0) then
        write (errmsg, '(a)') 'Error.  ANGLDEGX not provided in '// &
          'discretization file, but K22 was specified. '
        call store_error(errmsg)
      end if
      !
      ! -- Check to make sure values are greater than or equal to zero
      nerr = 0
      do n = 1, size(this%k22)
        if (this%ik22overk /= 0) this%k22(n) = this%k22(n) * this%k11(n)
        if (this%k22(n) <= DZERO) then
          nerr = nerr + 1
          if (nerr <= 20) then
            call this%dis%noder_to_string(n, cellstr)
            write (errmsg, fmtkerr) trim(adjustl(aname(4))), trim(cellstr), &
              this%k22(n)
            call store_error(errmsg)
          end if
        end if
      end do
      if (nerr > 20) then
        write (errmsg, fmtkerr2) nerr, trim(adjustl(aname(4)))
        call store_error(errmsg)
      end if
    end if
    !
    ! -- check for wetdry conflicts
    if (this%irewet == 1) then
      if (this%iwetdry == 0) then
        write (errmsg, '(a, a, a)') 'Error in GRIDDATA block: ', &
          trim(adjustl(aname(5))), ' not found.'
        call store_error(errmsg)
      end if
    end if
    !
    ! -- Check for angle conflicts
    if (this%iangle1 /= 0) then
      do n = 1, size(this%angle1)
        this%angle1(n) = this%angle1(n) * DPIO180
      end do
    else
      if (this%ixt3d /= 0) then
        this%iangle1 = 1
        write (this%iout, '(a)') 'XT3D IN USE, BUT ANGLE1 NOT SPECIFIED. '// &
          'SETTING ANGLE1 TO ZERO.'
        do n = 1, size(this%angle1)
          this%angle1(n) = DZERO
        end do
      end if
    end if
    if (this%iangle2 /= 0) then
      if (this%iangle1 == 0) then
        write (errmsg, '(a)') 'ANGLE2 SPECIFIED BUT NOT ANGLE1. '// &
          'ANGLE2 REQUIRES ANGLE1. '
        call store_error(errmsg)
      end if
      if (this%iangle3 == 0) then
        write (errmsg, '(a)') 'ANGLE2 SPECIFIED BUT NOT ANGLE3. '// &
          'SPECIFY BOTH OR NEITHER ONE. '
        call store_error(errmsg)
      end if
      do n = 1, size(this%angle2)
        this%angle2(n) = this%angle2(n) * DPIO180
      end do
    end if
    if (this%iangle3 /= 0) then
      if (this%iangle1 == 0) then
        write (errmsg, '(a)') 'ANGLE3 SPECIFIED BUT NOT ANGLE1. '// &
          'ANGLE3 REQUIRES ANGLE1. '
        call store_error(errmsg)
      end if
      if (this%iangle2 == 0) then
        write (errmsg, '(a)') 'ANGLE3 SPECIFIED BUT NOT ANGLE2. '// &
          'SPECIFY BOTH OR NEITHER ONE. '
        call store_error(errmsg)
      end if
      do n = 1, size(this%angle3)
        this%angle3(n) = this%angle3(n) * DPIO180
      end do
    end if
    !
    ! -- terminate if data errors
    if (count_errors() > 0) then
      call this%parser%StoreErrorUnit()
    end if

    return
  end subroutine prepcheck

  !> @brief preprocess the NPF input data
  !!
  !! This routine consists of the following steps:
  !!
  !! 1. convert cells to noflow when all transmissive parameters equal zero
  !! 2. perform initial wetting and drying
  !! 3. initialize cell saturation
  !! 4. calculate saturated conductance (when not xt3d)
  !! 5. If NEWTON under-relaxation, determine lower most node
  !<
  subroutine preprocess_input(this)
    use ConstantsModule, only: LINELENGTH
    use MemoryManagerModule, only: mem_allocate, mem_reallocate, mem_deallocate
    use SimModule, only: store_error, count_errors
    class(GwfNpfType) :: this !< the instance of the NPF package
    ! local
    integer(I4B) :: n, m, ii, nn
    real(DP) :: hyn, hym
    real(DP) :: satn, topn, botn
    integer(I4B) :: nextn
    real(DP) :: minbot, botm
    logical :: finished
    character(len=LINELENGTH) :: cellstr, errmsg
    ! format strings
    character(len=*), parameter :: fmtcnv = &
      "(1X,'CELL ', A, &
      &' ELIMINATED BECAUSE ALL HYDRAULIC CONDUCTIVITIES TO NODE ARE 0.')"
    character(len=*), parameter :: fmtnct = &
      &"(1X,'Negative cell thickness at cell ', A)"
    character(len=*), parameter :: fmtihbe = &
      &"(1X,'Initial head, bottom elevation:',1P,2G13.5)"
    character(len=*), parameter :: fmttebe = &
      &"(1X,'Top elevation, bottom elevation:',1P,2G13.5)"
    !
    do n = 1, this%dis%nodes
      this%ithickstartflag(n) = 0
    end do
    !
    ! -- Insure that each cell has at least one non-zero transmissive parameter
    !    Note that a cell can be deactivated even if it has a valid connection
    !    to another model.
    nodeloop: do n = 1, this%dis%nodes
      !
      ! -- Skip if already inactive
      if (this%ibound(n) == 0) then
        if (this%irewet /= 0) then
          if (this%wetdry(n) == DZERO) cycle nodeloop
        else
          cycle nodeloop
        end if
      end if
      !
      ! -- Cycle if k11 is not zero
      if (this%k11(n) /= DZERO) cycle nodeloop
      !
      ! -- Cycle if at least one vertical connection has non-zero k33
      !    for n and m
      do ii = this%dis%con%ia(n) + 1, this%dis%con%ia(n + 1) - 1
        m = this%dis%con%ja(ii)
        if (this%dis%con%ihc(this%dis%con%jas(ii)) == 0) then
          hyn = this%k11(n)
          if (this%ik33 /= 0) hyn = this%k33(n)
          if (hyn /= DZERO) then
            hym = this%k11(m)
            if (this%ik33 /= 0) hym = this%k33(m)
            if (hym /= DZERO) cycle
          end if
        end if
      end do
      !
      ! -- If this part of the loop is reached, then all connections have
      !    zero transmissivity, so convert to noflow.
      this%ibound(n) = 0
      this%hnew(n) = this%hnoflo
      if (this%irewet /= 0) this%wetdry(n) = DZERO
      call this%dis%noder_to_string(n, cellstr)
      write (this%iout, fmtcnv) trim(adjustl(cellstr))
      !
    end do nodeloop
    !
    ! -- Preprocess cell status and heads based on initial conditions
    if (this%inewton == 0) then
      !
      ! -- For standard formulation (non-Newton) call wetdry routine
      call this%wd(0, this%hnew)
    else
      !
      ! -- Newton formulation, so adjust heads to be above bottom
      !    (Not used in present formulation because variable cv
      !    cannot be used with Newton)
      if (this%ivarcv == 1) then
        do n = 1, this%dis%nodes
          if (this%hnew(n) < this%dis%bot(n)) then
            this%hnew(n) = this%dis%bot(n) + DEM6
          end if
        end do
      end if
    end if
    !
    ! -- Initialize sat to zero for ibound=0 cells, unless the cell can
    !    rewet.  Initialize sat to the saturated fraction based on strt
    !    if icelltype is negative and the THCKSTRT option is in effect.
    !    Initialize sat to 1.0 for all other cells in order to calculate
    !    condsat in next section.
    do n = 1, this%dis%nodes
      if (this%ibound(n) == 0) then
        this%sat(n) = DONE
        if (this%icelltype(n) < 0 .and. this%ithickstrt /= 0) then
          this%ithickstartflag(n) = 1
          this%icelltype(n) = 0
        end if
      else
        topn = this%dis%top(n)
        botn = this%dis%bot(n)
        if (this%icelltype(n) < 0 .and. this%ithickstrt /= 0) then
          call this%thksat(n, this%ic%strt(n), satn)
          if (botn > this%ic%strt(n)) then
            call this%dis%noder_to_string(n, cellstr)
            write (errmsg, fmtnct) trim(adjustl(cellstr))
            call store_error(errmsg)
            write (errmsg, fmtihbe) this%ic%strt(n), botn
            call store_error(errmsg)
          end if
          this%ithickstartflag(n) = 1
          this%icelltype(n) = 0
        else
          satn = DONE
          if (botn > topn) then
            call this%dis%noder_to_string(n, cellstr)
            write (errmsg, fmtnct) trim(adjustl(cellstr))
            call store_error(errmsg)
            write (errmsg, fmttebe) topn, botn
            call store_error(errmsg)
          end if
        end if
        this%sat(n) = satn
      end if
    end do
    if (count_errors() > 0) then
      call this%parser%StoreErrorUnit()
    end if
    !
    ! -- Calculate condsat, but only if xt3d is not active.  If xt3d is
    !    active, then condsat is allocated to size of zero.
    if (this%ixt3d == 0) then
      !
      ! -- Calculate the saturated conductance for all connections assuming
      !    that saturation is 1 (except for case where icelltype was entered
      !    as a negative value and THCKSTRT option in effect)
      do n = 1, this%dis%nodes
        call this%calc_condsat(n, .true.)
      end do
      !
    end if
    !
    ! -- Determine the lower most node
    if (this%igwfnewtonur /= 0) then
      call mem_reallocate(this%ibotnode, this%dis%nodes, 'IBOTNODE', &
                          trim(this%memoryPath))
      do n = 1, this%dis%nodes
        !
        minbot = this%dis%bot(n)
        nn = n
        finished = .false.
        do while (.not. finished)
          nextn = 0
          !
          ! -- Go through the connecting cells
          do ii = this%dis%con%ia(nn) + 1, this%dis%con%ia(nn + 1) - 1
            !
            ! -- Set the m cell number
            m = this%dis%con%ja(ii)
            botm = this%dis%bot(m)
            !
            ! -- select vertical connections: ihc == 0
            if (this%dis%con%ihc(this%dis%con%jas(ii)) == 0) then
              if (m > nn .and. botm < minbot) then
                nextn = m
                minbot = botm
              end if
            end if
          end do
          if (nextn > 0) then
            nn = nextn
          else
            finished = .true.
          end if
        end do
        this%ibotnode(n) = nn
      end do
    end if
    !
    ! -- nullify unneeded gwf pointers
    this%igwfnewtonur => null()
    !
    ! -- Return
    return

  end subroutine preprocess_input

  !> @brief Calculate CONDSAT array entries for the given node
  !!
  !! Calculate saturated conductances for all connections of the given node,
  !! or optionally for the upper portion of the matrix only.
  !!
  !<
  subroutine calc_condsat(this, node, upperOnly)
    ! -- dummy variables
    class(GwfNpfType) :: this
    integer(I4B), intent(in) :: node
    logical, intent(in) :: upperOnly
    ! -- local variables
    integer(I4B) :: ii, m, n, ihc, jj
    real(DP) :: topm, topn, topnode, botm, botn, botnode, satm, satn, satnode
    real(DP) :: hyn, hym, hn, hm, fawidth, csat
    !
    satnode = this%calc_initial_sat(node)
    !
    topnode = this%dis%top(node)
    botnode = this%dis%bot(node)
    !
    ! -- Go through the connecting cells
    do ii = this%dis%con%ia(node) + 1, this%dis%con%ia(node + 1) - 1
      !
      ! -- Set the m cell number and cycle if lower triangle connection and
      ! -- we're not updating both upper and lower matrix parts for this node
      m = this%dis%con%ja(ii)
      jj = this%dis%con%jas(ii)
      if (m < node) then
        if (upperOnly) cycle
        ! m => node, n => neighbour
        n = m
        m = node
        topm = topnode
        botm = botnode
        satm = satnode
        topn = this%dis%top(n)
        botn = this%dis%bot(n)
        satn = this%calc_initial_sat(n)
      else
        ! n => node, m => neighbour
        n = node
        topn = topnode
        botn = botnode
        satn = satnode
        topm = this%dis%top(m)
        botm = this%dis%bot(m)
        satm = this%calc_initial_sat(m)
      end if
      !
      ihc = this%dis%con%ihc(jj)
      hyn = this%hy_eff(n, m, ihc, ipos=ii)
      hym = this%hy_eff(m, n, ihc, ipos=ii)
      if (this%ithickstartflag(n) == 0) then
        hn = topn
      else
        hn = this%ic%strt(n)
      end if
      if (this%ithickstartflag(m) == 0) then
        hm = topm
      else
        hm = this%ic%strt(m)
      end if
      !
      ! -- Calculate conductance depending on whether connection is
      !    vertical (0), horizontal (1), or staggered horizontal (2)
      if (ihc == 0) then
        !
        ! -- Vertical conductance for fully saturated conditions
        csat = vcond(1, 1, 1, 1, 0, 1, 1, DONE, &
                     botn, botm, &
                     hyn, hym, &
                     satn, satm, &
                     topn, topm, &
                     botn, botm, &
                     this%dis%con%hwva(jj))
      else
        !
        ! -- Horizontal conductance for fully saturated conditions
        fawidth = this%dis%con%hwva(jj)
        csat = hcond(1, 1, 1, 1, this%inewton, 0, &
                     ihc, &
                     this%icellavg, this%iusgnrhc, this%inwtupw, &
                     DONE, &
                     hn, hm, satn, satm, hyn, hym, &
                     topn, topm, &
                     botn, botm, &
                     this%dis%con%cl1(jj), &
                     this%dis%con%cl2(jj), &
                     fawidth, this%satomega, this%satmin)
      end if
      this%condsat(jj) = csat
    end do
    !
    return
  end subroutine calc_condsat

  !> @brief Calculate initial saturation for the given node
  !!
  !! Calculate saturation as a fraction of thickness for the given node, used
  !! for saturated conductance calculations: full thickness by default (1.0) or
  !! saturation based on initial conditions if the THICKSTRT option is used.
  !!
  !<
  function calc_initial_sat(this, n) result(satn)
    ! -- dummy variables
    class(GwfNpfType) :: this
    integer(I4B), intent(in) :: n
    ! -- return
    real(DP) :: satn
    !
    satn = DONE
    if (this%ibound(n) /= 0 .and. this%ithickstartflag(n) /= 0) then
      call this%thksat(n, this%ic%strt(n), satn)
    end if
    !
    return
  end function calc_initial_sat

  subroutine sgwf_npf_wetdry(this, kiter, hnew)
! ******************************************************************************
! sgwf_npf_wetdry -- Perform wetting and drying
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use TdisModule, only: kstp, kper
    use SimModule, only: store_error
    use ConstantsModule, only: LINELENGTH
    ! -- dummy
    class(GwfNpfType) :: this
    integer(I4B), intent(in) :: kiter
    real(DP), intent(inout), dimension(:) :: hnew
    ! -- local
    integer(I4B) :: n, m, ii, ihc
    real(DP) :: ttop, bbot, thck
    integer(I4B) :: ncnvrt, ihdcnv
    character(len=30), dimension(5) :: nodcnvrt
    character(len=30) :: nodestr
    character(len=3), dimension(5) :: acnvrt
    character(len=LINELENGTH) :: errmsg
    integer(I4B) :: irewet
    ! -- formats
    character(len=*), parameter :: fmtnct = &
      "(1X,/1X,'Negative cell thickness at (layer,row,col)', &
      &I4,',',I5,',',I5)"
    character(len=*), parameter :: fmttopbot = &
      &"(1X,'Top elevation, bottom elevation:',1P,2G13.5)"
    character(len=*), parameter :: fmttopbotthk = &
      &"(1X,'Top elevation, bottom elevation, thickness:',1P,3G13.5)"
    character(len=*), parameter :: fmtdrychd = &
      &"(1X,/1X,'CONSTANT-HEAD CELL WENT DRY -- SIMULATION ABORTED')"
    character(len=*), parameter :: fmtni = &
      &"(1X,'CELLID=',a,' ITERATION=',I0,' TIME STEP=',I0,' STRESS PERIOD=',I0)"
! ------------------------------------------------------------------------------
    ! -- Initialize
    ncnvrt = 0
    ihdcnv = 0
    !
    ! -- Convert dry cells to wet
    do n = 1, this%dis%nodes
      do ii = this%dis%con%ia(n) + 1, this%dis%con%ia(n + 1) - 1
        m = this%dis%con%ja(ii)
        ihc = this%dis%con%ihc(this%dis%con%jas(ii))
        call this%rewet_check(kiter, n, hnew(m), this%ibound(m), ihc, hnew, &
                              irewet)
        if (irewet == 1) then
          call this%wdmsg(2, ncnvrt, nodcnvrt, acnvrt, ihdcnv, kiter, n)
        end if
      end do
    end do
    !
    ! -- Perform drying
    do n = 1, this%dis%nodes
      !
      ! -- cycle if inactive or confined
      if (this%ibound(n) == 0) cycle
      if (this%icelltype(n) == 0) cycle
      !
      ! -- check for negative cell thickness
      bbot = this%dis%bot(n)
      ttop = this%dis%top(n)
      if (bbot > ttop) then
        write (errmsg, fmtnct) n
        call store_error(errmsg)
        write (errmsg, fmttopbot) ttop, bbot
        call store_error(errmsg)
        call this%parser%StoreErrorUnit()
      end if
      !
      ! -- Calculate saturated thickness
      if (this%icelltype(n) /= 0) then
        if (hnew(n) < ttop) ttop = hnew(n)
      end if
      thck = ttop - bbot
      !
      ! -- If thck<0 print message, set hnew, and ibound
!      if(thck<0) then
      if (thck <= DZERO) then
        call this%wdmsg(1, ncnvrt, nodcnvrt, acnvrt, ihdcnv, kiter, n)
        hnew(n) = this%hdry
        if (this%ibound(n) < 0) then
          errmsg = 'CONSTANT-HEAD CELL WENT DRY -- SIMULATION ABORTED'
          call store_error(errmsg)
          write (errmsg, fmttopbotthk) ttop, bbot, thck
          call store_error(errmsg)
          call this%dis%noder_to_string(n, nodestr)
          write (errmsg, fmtni) trim(adjustl(nodestr)), kiter, kstp, kper
          call store_error(errmsg)
          call this%parser%StoreErrorUnit()
        end if
        this%ibound(n) = 0
      end if
    end do
    !
    ! -- Print remaining cell conversions
    call this%wdmsg(0, ncnvrt, nodcnvrt, acnvrt, ihdcnv, kiter, n)
    !
    ! -- Change ibound from 30000 to 1
    do n = 1, this%dis%nodes
      if (this%ibound(n) == 30000) this%ibound(n) = 1
    end do
    !
    ! -- Return
    return
  end subroutine sgwf_npf_wetdry

  subroutine rewet_check(this, kiter, node, hm, ibdm, ihc, hnew, irewet)
! ******************************************************************************
! rewet_check -- Determine if a cell should rewet.  This method can
!   be called from any external object that has a head that can be used to
!   rewet the GWF cell node.  The ihc value is used to determine if it is a
!   vertical or horizontal connection, which can operate differently depending
!   on user settings.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(GwfNpfType) :: this
    integer(I4B), intent(in) :: kiter
    integer(I4B), intent(in) :: node
    real(DP), intent(in) :: hm
    integer(I4B), intent(in) :: ibdm
    integer(I4B), intent(in) :: ihc
    real(DP), intent(inout), dimension(:) :: hnew
    integer(I4B), intent(out) :: irewet
    ! -- local
    integer(I4B) :: itflg
    real(DP) :: wd, awd, turnon, bbot
    ! -- formats
! ------------------------------------------------------------------------------
    !
    irewet = 0
    !
    ! -- Convert a dry cell to wet if it meets the criteria
    if (this%irewet > 0) then
      itflg = mod(kiter, this%iwetit)
      if (itflg == 0) then
        if (this%ibound(node) == 0 .and. this%wetdry(node) /= DZERO) then
          !
          ! -- Calculate wetting elevation
          bbot = this%dis%bot(node)
          wd = this%wetdry(node)
          awd = wd
          if (wd < 0) awd = -wd
          turnon = bbot + awd
          !
          ! -- Check head in adjacent cells to see if wetting elevation has
          !    been reached
          if (ihc == 0) then
            !
            ! -- check cell below
            if (ibdm > 0 .and. hm >= turnon) irewet = 1
          else
            if (wd > DZERO) then
              !
              ! -- check horizontally adjacent cells
              if (ibdm > 0 .and. hm >= turnon) irewet = 1
            end if
          end if
          !
          if (irewet == 1) then
            ! -- rewet cell; use equation 3a if ihdwet=0; use equation 3b if
            !    ihdwet is not 0.
            if (this%ihdwet == 0) then
              hnew(node) = bbot + this%wetfct * (hm - bbot)
            else
              hnew(node) = bbot + this%wetfct * awd !(hm - bbot)
            end if
            this%ibound(node) = 30000
          end if
        end if
      end if
    end if
    !
    ! -- Return
    return
  end subroutine rewet_check

  subroutine sgwf_npf_wdmsg(this, icode, ncnvrt, nodcnvrt, acnvrt, ihdcnv, &
                            kiter, n)
! ******************************************************************************
! sgwf_npf_wdmsg -- Print wet/dry message
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use TdisModule, only: kstp, kper
    ! -- dummy
    class(GwfNpfType) :: this
    integer(I4B), intent(in) :: icode
    integer(I4B), intent(inout) :: ncnvrt
    character(len=30), dimension(5), intent(inout) :: nodcnvrt
    character(len=3), dimension(5), intent(inout) :: acnvrt
    integer(I4B), intent(inout) :: ihdcnv
    integer(I4B), intent(in) :: kiter
    integer(I4B), intent(in) :: n
    ! -- local
    integer(I4B) :: l
    ! -- formats
    character(len=*), parameter :: fmtcnvtn = &
      "(1X,/1X,'CELL CONVERSIONS FOR ITER.=',I0, &
       &'  STEP=',I0,'  PERIOD=',I0,'   (NODE or LRC)')"
    character(len=*), parameter :: fmtnode = "(1X,3X,5(A4, A20))"
! ------------------------------------------------------------------------------
    ! -- Keep track of cell conversions
    if (icode > 0) then
      ncnvrt = ncnvrt + 1
      call this%dis%noder_to_string(n, nodcnvrt(ncnvrt))
      if (icode == 1) then
        acnvrt(ncnvrt) = 'DRY'
      else
        acnvrt(ncnvrt) = 'WET'
      end if
    end if
    !
    ! -- Print a line if 5 conversions have occurred or if icode indicates that a
    !    partial line should be printed
    if (ncnvrt == 5 .or. (icode == 0 .and. ncnvrt > 0)) then
      if (ihdcnv == 0) write (this%iout, fmtcnvtn) kiter, kstp, kper
      ihdcnv = 1
      write (this%iout, fmtnode) &
        (acnvrt(l), trim(adjustl(nodcnvrt(l))), l=1, ncnvrt)
      ncnvrt = 0
    end if
    !
    ! -- Return
    return
  end subroutine sgwf_npf_wdmsg

  function hy_eff(this, n, m, ihc, ipos, vg) result(hy)
! ******************************************************************************
! hy_eff -- Calculate the effective hydraulic conductivity for the n-m
!   connection.
!     n is primary node node number
!     m is connected node (not used if vg is provided)
!     ihc is horizontal indicator (0 vertical, 1 horizontal, 2 vertically
!       staggered)
!     ipos_opt is position of connection in ja array
!     vg is the global unit vector that expresses the direction from which to
!       calculate an effective hydraulic conductivity.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- return
    real(DP) :: hy
    ! -- dummy
    class(GwfNpfType) :: this
    integer(I4B), intent(in) :: n
    integer(I4B), intent(in) :: m
    integer(I4B), intent(in) :: ihc
    integer(I4B), intent(in), optional :: ipos
    real(DP), dimension(3), intent(in), optional :: vg
    ! -- local
    integer(I4B) :: iipos
    real(DP) :: hy11, hy22, hy33
    real(DP) :: ang1, ang2, ang3
    real(DP) :: vg1, vg2, vg3
    ! -- formats
! ------------------------------------------------------------------------------
    !
    ! -- Initialize
    iipos = 0
    if (present(ipos)) iipos = ipos
    hy11 = this%k11(n)
    hy22 = this%k11(n)
    hy33 = this%k11(n)
    if (this%ik22 /= 0) hy22 = this%k22(n)
    if (this%ik33 /= 0) hy33 = this%k33(n)
    !
    ! -- Calculate effective K based on whether connection is vertical
    !    or horizontal
    if (ihc == 0) then
      !
      ! -- Handle rotated anisotropy case that would affect the effective
      !    vertical hydraulic conductivity
      hy = hy33
      if (this%iangle2 > 0) then
        if (present(vg)) then
          vg1 = vg(1)
          vg2 = vg(2)
          vg3 = vg(3)
        else
          call this%dis%connection_normal(n, m, ihc, vg1, vg2, vg3, iipos)
        end if
        ang1 = this%angle1(n)
        ang2 = this%angle2(n)
        ang3 = DZERO
        if (this%iangle3 > 0) ang3 = this%angle3(n)
        hy = hyeff_calc(hy11, hy22, hy33, ang1, ang2, ang3, vg1, vg2, vg3, &
                        this%iavgkeff)
      end if
      !
    else
      !
      ! -- Handle horizontal case
      hy = hy11
      if (this%ik22 > 0) then
        if (present(vg)) then
          vg1 = vg(1)
          vg2 = vg(2)
          vg3 = vg(3)
        else
          call this%dis%connection_normal(n, m, ihc, vg1, vg2, vg3, iipos)
        end if
        ang1 = DZERO
        ang2 = DZERO
        ang3 = DZERO
        if (this%iangle1 > 0) then
          ang1 = this%angle1(n)
          if (this%iangle2 > 0) then
            ang2 = this%angle2(n)
            if (this%iangle3 > 0) ang3 = this%angle3(n)
          end if
        end if
        hy = hyeff_calc(hy11, hy22, hy33, ang1, ang2, ang3, vg1, vg2, vg3, &
                        this%iavgkeff)
      end if
      !
    end if
    !
    ! -- Return
    return
  end function hy_eff

  function hcond(ibdn, ibdm, ictn, ictm, inewton, inwtup, ihc, icellavg, iusg, &
                 iupw, condsat, hn, hm, satn, satm, hkn, hkm, topn, topm, &
                 botn, botm, cln, clm, fawidth, satomega, satminopt) &
    result(condnm)
! ******************************************************************************
! hcond -- Horizontal conductance between two cells
!   inwtup: if 1, then upstream-weight condsat, otherwise recalculate
!
! hcond function uses a weighted transmissivity in the harmonic mean
! conductance calculations. This differs from the MODFLOW-NWT and MODFLOW-USG
! conductance calculations for the Newton-Raphson formulation which use a
! weighted hydraulic conductivity.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- return
    real(DP) :: condnm
    ! -- dummy
    integer(I4B), intent(in) :: ibdn
    integer(I4B), intent(in) :: ibdm
    integer(I4B), intent(in) :: ictn
    integer(I4B), intent(in) :: ictm
    integer(I4B), intent(in) :: inewton
    integer(I4B), intent(in) :: inwtup
    integer(I4B), intent(in) :: ihc
    integer(I4B), intent(in) :: icellavg
    integer(I4B), intent(in) :: iusg
    integer(I4B), intent(in) :: iupw
    real(DP), intent(in) :: condsat
    real(DP), intent(in) :: hn
    real(DP), intent(in) :: hm
    real(DP), intent(in) :: satn
    real(DP), intent(in) :: satm
    real(DP), intent(in) :: hkn
    real(DP), intent(in) :: hkm
    real(DP), intent(in) :: topn
    real(DP), intent(in) :: topm
    real(DP), intent(in) :: botn
    real(DP), intent(in) :: botm
    real(DP), intent(in) :: cln
    real(DP), intent(in) :: clm
    real(DP), intent(in) :: fawidth
    real(DP), intent(in) :: satomega
    real(DP), optional, intent(in) :: satminopt
    ! -- local
    integer(I4B) :: indk
    real(DP) :: satmin
    real(DP) :: sn
    real(DP) :: sm
    real(DP) :: thksatn
    real(DP) :: thksatm
    real(DP) :: sill_top, sill_bot
    real(DP) :: tpn, tpm
    real(DP) :: top, bot
    real(DP) :: athk
    real(DP) :: afac
! ------------------------------------------------------------------------------
    if (present(satminopt)) then
      satmin = satminopt
    else
      satmin = DZERO
    end if
    !
    ! -- If either n or m is inactive then conductance is zero
    if (ibdn == 0 .or. ibdm == 0) then
      condnm = DZERO
      !
      ! -- if both cells are non-convertible then use condsat
    elseif (ictn == 0 .and. ictm == 0) then
      if (icellavg /= 4) then
        condnm = condsat
      else
        if (hn > hm) then
          condnm = satn * (topn - botn)
        else
          condnm = satm * (topm - botm)
        end if
        condnm = condnm * condsat
      end if
      !
      ! -- At least one of the cells is convertible, so calculate average saturated
      !    thickness and multiply with saturated conductance
    else
      if (inwtup == 1) then
        ! -- set flag use to determine if bottom of cells n and m are
        !    significantly different
        indk = 0
        if (abs(botm - botn) < DEM2) indk = 1
        ! -- recalculate saturation if using MODFLOW-USG saturation
        !    calculation approach
        if (iusg == 1 .and. indk == 0) then
          if (botm > botn) then
            top = topm
            bot = botm
          else
            top = topn
            bot = botn
          end if
          sn = sQuadraticSaturation(top, bot, hn, satomega, satmin)
          sm = sQuadraticSaturation(top, bot, hm, satomega, satmin)
        else
          sn = sQuadraticSaturation(topn, botn, hn, satomega, satmin)
          sm = sQuadraticSaturation(topm, botm, hm, satomega, satmin)
        end if

        if (hn > hm) then
          condnm = sn
        else
          condnm = sm
        end if
        !
        ! -- if using MODFLOW-NWT upstream weighting option apply
        !    factor to remove average thickness
        if (iupw /= 0) then
          if (hn > hm) then
            afac = DTWO / (DONE + (topm - botm) / (topn - botn))
            condnm = condnm * afac
          else
            afac = DTWO / (DONE + (topn - botn) / (topm - botm))
            condnm = condnm * afac
          end if
        end if
        !
        ! -- multiply condsat by condnm factor
        condnm = condnm * condsat
      else
        thksatn = satn * (topn - botn)
        thksatm = satm * (topm - botm)
        !
        ! -- If staggered connection, subtract parts of cell that are above and
        !    below the sill top and bottom elevations
        if (ihc == 2) then
          !
          ! -- Calculate sill_top and sill_bot
          sill_top = min(topn, topm)
          sill_bot = max(botn, botm)
          !
          ! -- Calculate tpn and tpm
          tpn = botn + thksatn
          tpm = botm + thksatm
          !
          ! -- Calculate saturated thickness for cells n and m
          thksatn = max(min(tpn, sill_top) - sill_bot, DZERO)
          thksatm = max(min(tpm, sill_top) - sill_bot, DZERO)
        end if

        athk = DONE
        if (iusg == 1) then
          if (ihc == 2) then
            athk = min(thksatn, thksatm)
          else
            athk = DHALF * (thksatn + thksatm)
          end if
          thksatn = DONE
          thksatm = DONE
        end if
        !
        condnm = condmean(hkn, hkm, thksatn, thksatm, cln, clm, &
                          fawidth, icellavg) * athk
      end if
    end if
    !
    ! -- Return
    return
  end function hcond

  function vcond(ibdn, ibdm, ictn, ictm, inewton, ivarcv, idewatcv, &
                 condsat, hn, hm, vkn, vkm, satn, satm, topn, topm, botn, &
                 botm, flowarea) result(condnm)
! ******************************************************************************
! vcond -- Vertical conductance between two cells
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- return
    real(DP) :: condnm
    ! -- dummy
    integer(I4B), intent(in) :: ibdn
    integer(I4B), intent(in) :: ibdm
    integer(I4B), intent(in) :: ictn
    integer(I4B), intent(in) :: ictm
    integer(I4B), intent(in) :: inewton
    integer(I4B), intent(in) :: ivarcv
    integer(I4B), intent(in) :: idewatcv
    real(DP), intent(in) :: condsat
    real(DP), intent(in) :: hn
    real(DP), intent(in) :: hm
    real(DP), intent(in) :: vkn
    real(DP), intent(in) :: vkm
    real(DP), intent(in) :: satn
    real(DP), intent(in) :: satm
    real(DP), intent(in) :: topn
    real(DP), intent(in) :: topm
    real(DP), intent(in) :: botn
    real(DP), intent(in) :: botm
    real(DP), intent(in) :: flowarea
    ! -- local
    real(DP) :: satntmp, satmtmp
    real(DP) :: bovk1
    real(DP) :: bovk2
    real(DP) :: denom
! ------------------------------------------------------------------------------
    !
    ! -- If either n or m is inactive then conductance is zero
    if (ibdn == 0 .or. ibdm == 0) then
      condnm = DZERO
      !
      ! -- if constantcv then use condsat
    elseif (ivarcv == 0) then
      condnm = condsat
      !
      ! -- if both cells are non-convertible then use condsat
    elseif (ictn == 0 .and. ictm == 0) then
      condnm = condsat
      !
      ! -- if both cells are fully saturated then use condsat
    elseif (hn >= topn .and. hm >= topm) then
      condnm = condsat
      !
      ! -- At least one cell is partially saturated, so recalculate vertical
      ! -- conductance for this connection
      ! -- todo: upstream weighting?
    else
      !
      ! -- Default is for CV correction (dewatered option); use underlying
      !    saturation of 1.
      satntmp = satn
      satmtmp = satm
      if (idewatcv == 0) then
        if (botn > botm) then
          ! -- n is above m
          satmtmp = DONE
        else
          ! -- m is above n
          satntmp = DONE
        end if
      end if
      bovk1 = satntmp * (topn - botn) * DHALF / vkn
      bovk2 = satmtmp * (topm - botm) * DHALF / vkm
      denom = (bovk1 + bovk2)
      if (denom /= DZERO) then
        condnm = flowarea / denom
      else
        condnm = DZERO
      end if
    end if
    !
    ! -- Return
    return
  end function vcond

  function condmean(k1, k2, thick1, thick2, cl1, cl2, width, iavgmeth)
! ******************************************************************************
! condmean -- Calculate the conductance between two cells
!
!   k1 is hydraulic conductivity for cell 1 (in the direction of cell2)
!   k2 is hydraulic conductivity for cell 2 (in the direction of cell1)
!   thick1 is the saturated thickness for cell 1
!   thick2 is the saturated thickness for cell 2
!   cl1 is the distance from the center of cell1 to the shared face with cell2
!   cl2 is the distance from the center of cell2 to the shared face with cell1
!   h1 is the head for cell1
!   h2 is the head for cell2
!   width is the width perpendicular to flow
!   iavgmeth is the averaging method:
!     0 is harmonic averaging
!     1 is logarithmic averaging
!     2 is arithmetic averaging of sat thickness and logarithmic averaging of
!       hydraulic conductivity
!     3 is arithmetic averaging of sat thickness and harmonic averaging of
!       hydraulic conductivity
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- return
    real(DP) :: condmean
    ! -- dummy
    real(DP), intent(in) :: k1
    real(DP), intent(in) :: k2
    real(DP), intent(in) :: thick1
    real(DP), intent(in) :: thick2
    real(DP), intent(in) :: cl1
    real(DP), intent(in) :: cl2
    real(DP), intent(in) :: width
    integer(I4B), intent(in) :: iavgmeth
    ! -- local
    real(DP) :: t1
    real(DP) :: t2
    real(DP) :: tmean, kmean, denom
! ------------------------------------------------------------------------------
    !
    ! -- Initialize
    t1 = k1 * thick1
    t2 = k2 * thick2
    !
    ! -- Averaging
    select case (iavgmeth)
      !
      ! -- Harmonic-mean method
    case (0)
      !
      if (t1 * t2 > DZERO) then
        condmean = width * t1 * t2 / (t1 * cl2 + t2 * cl1)
      else
        condmean = DZERO
      end if
      !
      ! -- Logarithmic-mean method
    case (1)
      if (t1 * t2 > DZERO) then
        tmean = logmean(t1, t2)
      else
        tmean = DZERO
      end if
      condmean = tmean * width / (cl1 + cl2)
      !
      ! -- Arithmetic-mean thickness and logarithmic-mean hydraulic conductivity
    case (2)
      if (k1 * k2 > DZERO) then
        kmean = logmean(k1, k2)
      else
        kmean = DZERO
      end if
      condmean = kmean * DHALF * (thick1 + thick2) * width / (cl1 + cl2)
      !
      ! -- Arithmetic-mean thickness and harmonic-mean hydraulic conductivity
    case (3)
      denom = (k1 * cl2 + k2 * cl1)
      if (denom > DZERO) then
        kmean = k1 * k2 / denom
      else
        kmean = DZERO
      end if
      condmean = kmean * DHALF * (thick1 + thick2) * width
    end select
    !
    ! -- Return
    return
  end function condmean

  function logmean(d1, d2)
! ******************************************************************************
! logmean -- Calculate the the logarithmic mean of two double precision
!            numbers.  Use an approximation if the ratio is near 1.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- return
    real(DP) :: logmean
    ! -- dummy
    real(DP), intent(in) :: d1
    real(DP), intent(in) :: d2
    ! -- local
    real(DP) :: drat
! ------------------------------------------------------------------------------
    !
    drat = d2 / d1
    if (drat <= DLNLOW .or. drat >= DLNHIGH) then
      logmean = (d2 - d1) / log(drat)
    else
      logmean = DHALF * (d1 + d2)
    end if
    !
    ! -- Return
    return
  end function logmean

  function hyeff_calc(k11, k22, k33, ang1, ang2, ang3, vg1, vg2, vg3, &
                      iavgmeth) result(hyeff)
! ******************************************************************************
! hyeff_calc -- Calculate the effective horizontal hydraulic conductivity from
!   an ellipse using a specified direction (unit vector vg1, vg2, vg3).
!   k11 is the hydraulic conductivity of the major ellipse axis
!   k22 is the hydraulic conductivity of first minor axis
!   k33 is the hydraulic conductivity of the second minor axis
!   ang1 is the counter-clockwise rotation (radians) of the ellipse in
!     the (x, y) plane
!   ang2 is the rotation of the conductivity ellipsoid upward or
!     downward from the (x, y) plane
!   ang3 is the rotation of the conductivity ellipsoid about the major
!     axis
!   vg1, vg2, and vg3 are the components of a unit vector in model coordinates
!     in the direction of the connection between cell n and m
!  iavgmeth is the averaging method.  If zero, then use harmonic averaging.
!     if one, then use arithmetic averaging.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use ConstantsModule, only: DONE
    ! -- result
    real(DP) :: hyeff
    ! -- dummy
    real(DP), intent(in) :: k11
    real(DP), intent(in) :: k22
    real(DP), intent(in) :: k33
    real(DP), intent(in) :: ang1
    real(DP), intent(in) :: ang2
    real(DP), intent(in) :: ang3
    real(DP), intent(in) :: vg1
    real(DP), intent(in) :: vg2
    real(DP), intent(in) :: vg3
    integer(I4B), intent(in) :: iavgmeth
    ! -- local
    real(DP) :: s1, s2, s3, c1, c2, c3
    real(DP), dimension(3, 3) :: r
    real(DP) :: ve1, ve2, ve3
    real(DP) :: denom, dnum, d1, d2, d3
! ------------------------------------------------------------------------------
    !
    ! -- Sin and cos of angles
    s1 = sin(ang1)
    c1 = cos(ang1)
    s2 = sin(ang2)
    c2 = cos(ang2)
    s3 = sin(ang3)
    c3 = cos(ang3)
    !
    ! -- Rotation matrix
    r(1, 1) = c1 * c2
    r(1, 2) = c1 * s2 * s3 - s1 * c3
    r(1, 3) = -c1 * s2 * c3 - s1 * s3
    r(2, 1) = s1 * c2
    r(2, 2) = s1 * s2 * s3 + c1 * c3
    r(2, 3) = -s1 * s2 * c3 + c1 * s3
    r(3, 1) = s2
    r(3, 2) = -c2 * s3
    r(3, 3) = c2 * c3
    !
    ! -- Unit vector in direction of n-m connection in a local coordinate
    !    system aligned with the ellipse axes
    ve1 = r(1, 1) * vg1 + r(2, 1) * vg2 + r(3, 1) * vg3
    ve2 = r(1, 2) * vg1 + r(2, 2) * vg2 + r(3, 2) * vg3
    ve3 = r(1, 3) * vg1 + r(2, 3) * vg2 + r(3, 3) * vg3
    !
    ! -- Effective hydraulic conductivity calculated using harmonic (1)
    !    or arithmetic (2) weighting
    hyeff = DZERO
    if (iavgmeth == 0) then
      !
      ! -- Arithmetic weighting.  If principal direction corresponds exactly with
      !    unit vector then set to principal direction.  Otherwise weight it.
      dnum = DONE
      d1 = ve1**2
      d2 = ve2**2
      d3 = ve3**2
      if (ve1 /= DZERO) then
        dnum = dnum * k11
        d2 = d2 * k11
        d3 = d3 * k11
      end if
      if (ve2 /= DZERO) then
        dnum = dnum * k22
        d1 = d1 * k22
        d3 = d3 * k22
      end if
      if (ve3 /= DZERO) then
        dnum = dnum * k33
        d1 = d1 * k33
        d2 = d2 * k33
      end if
      denom = d1 + d2 + d3
      if (denom > DZERO) hyeff = dnum / denom
    else if (iavgmeth == 1) then
      ! -- arithmetic
      hyeff = ve1**2 * k11 + ve2**2 * k22 + ve3**2 * k33
    end if
    !
    ! -- Return
    return
  end function hyeff_calc

  subroutine calc_spdis(this, flowja)
! ******************************************************************************
! calc_spdis -- Calculate the 3 conmponents of specific discharge
!     at the cell center.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use SimModule, only: store_error
    ! -- dummy
    class(GwfNpfType) :: this
    real(DP), intent(in), dimension(:) :: flowja
    ! -- local
    integer(I4B) :: n
    integer(I4B) :: m
    integer(I4B) :: ipos
    integer(I4B) :: isympos
    integer(I4B) :: ihc
    integer(I4B) :: ic
    integer(I4B) :: iz
    integer(I4B) :: nc
    integer(I4B) :: ncz
    real(DP) :: qz
    real(DP) :: vx
    real(DP) :: vy
    real(DP) :: vz
    real(DP) :: xn
    real(DP) :: yn
    real(DP) :: zn
    real(DP) :: xc
    real(DP) :: yc
    real(DP) :: zc
    real(DP) :: cl1
    real(DP) :: cl2
    real(DP) :: dltot
    real(DP) :: ooclsum
    real(DP) :: dsumx
    real(DP) :: dsumy
    real(DP) :: dsumz
    real(DP) :: denom
    real(DP) :: area
    real(DP) :: dz
    real(DP) :: axy
    real(DP) :: ayx
    real(DP), allocatable, dimension(:) :: vi
    real(DP), allocatable, dimension(:) :: di
    real(DP), allocatable, dimension(:) :: viz
    real(DP), allocatable, dimension(:) :: diz
    real(DP), allocatable, dimension(:) :: nix
    real(DP), allocatable, dimension(:) :: niy
    real(DP), allocatable, dimension(:) :: wix
    real(DP), allocatable, dimension(:) :: wiy
    real(DP), allocatable, dimension(:) :: wiz
    real(DP), allocatable, dimension(:) :: bix
    real(DP), allocatable, dimension(:) :: biy
    logical :: nozee = .true.
! ------------------------------------------------------------------------------
    !
    ! -- Ensure dis has necessary information
    if (this%icalcspdis /= 0 .and. this%dis%con%ianglex == 0) then
      call store_error('Error.  ANGLDEGX not provided in '// &
                       'discretization file.  ANGLDEGX required for '// &
                       'calculation of specific discharge.', terminate=.TRUE.)
    end if
    !
    ! -- Find max number of connections and allocate weight arrays
    nc = 0
    do n = 1, this%dis%nodes
      !
      ! -- Count internal model connections
      ic = this%dis%con%ia(n + 1) - this%dis%con%ia(n) - 1
      !
      ! -- Count edge connections
      do m = 1, this%nedges
        if (this%nodedge(m) == n) then
          ic = ic + 1
        end if
      end do
      !
      ! -- Set max number of connections for any cell
      if (ic > nc) nc = ic
    end do
    !
    ! -- Allocate storage arrays needed for cell-centered spdis calculation
    allocate (vi(nc))
    allocate (di(nc))
    allocate (viz(nc))
    allocate (diz(nc))
    allocate (nix(nc))
    allocate (niy(nc))
    allocate (wix(nc))
    allocate (wiy(nc))
    allocate (wiz(nc))
    allocate (bix(nc))
    allocate (biy(nc))
    !
    ! -- Go through each cell and calculate specific discharge
    do n = 1, this%dis%nodes
      !
      ! -- first calculate geometric properties for x and y directions and
      !    the specific discharge at a face (vi)
      ic = 0
      iz = 0
      vi(:) = DZERO
      di(:) = DZERO
      viz(:) = DZERO
      diz(:) = DZERO
      nix(:) = DZERO
      niy(:) = DZERO
      do ipos = this%dis%con%ia(n) + 1, this%dis%con%ia(n + 1) - 1
        m = this%dis%con%ja(ipos)
        isympos = this%dis%con%jas(ipos)
        ihc = this%dis%con%ihc(isympos)
        area = this%dis%con%hwva(isympos)
        if (ihc == 0) then
          !
          ! -- vertical connection
          iz = iz + 1
          !call this%dis%connection_normal(n, m, ihc, xn, yn, zn, ipos)
          call this%dis%connection_vector(n, m, nozee, this%sat(n), this%sat(m), &
                                          ihc, xc, yc, zc, dltot)
          cl1 = this%dis%con%cl1(isympos)
          cl2 = this%dis%con%cl2(isympos)
          if (m < n) then
            cl1 = this%dis%con%cl2(isympos)
            cl2 = this%dis%con%cl1(isympos)
          end if
          ooclsum = DONE / (cl1 + cl2)
          diz(iz) = dltot * cl1 * ooclsum
          qz = flowja(ipos)
          if (n > m) qz = -qz
          viz(iz) = qz / area
        else
          !
          ! -- horizontal connection
          ic = ic + 1
          dz = thksatnm(this%ibound(n), this%ibound(m), &
                        this%icelltype(n), this%icelltype(m), &
                        this%inewton, ihc, this%iusgnrhc, &
                        this%hnew(n), this%hnew(m), this%sat(n), this%sat(m), &
                        this%dis%top(n), this%dis%top(m), this%dis%bot(n), &
                        this%dis%bot(m), this%satomega, this%satmin)
          area = area * dz
          call this%dis%connection_normal(n, m, ihc, xn, yn, zn, ipos)
          call this%dis%connection_vector(n, m, nozee, this%sat(n), this%sat(m), &
                                          ihc, xc, yc, zc, dltot)
          cl1 = this%dis%con%cl1(isympos)
          cl2 = this%dis%con%cl2(isympos)
          if (m < n) then
            cl1 = this%dis%con%cl2(isympos)
            cl2 = this%dis%con%cl1(isympos)
          end if
          ooclsum = DONE / (cl1 + cl2)
          nix(ic) = -xn
          niy(ic) = -yn
          di(ic) = dltot * cl1 * ooclsum
          if (area > DZERO) then
            vi(ic) = flowja(ipos) / area
          else
            vi(ic) = DZERO
          end if
        end if
      end do
      !
      ! -- Look through edge flows that may have been provided by an exchange
      !    and incorporate them into the averaging arrays
      do m = 1, this%nedges
        if (this%nodedge(m) == n) then
          !
          ! -- propsedge: (Q, area, nx, ny, distance)
          ihc = this%ihcedge(m)
          area = this%propsedge(2, m)
          if (ihc == 0) then
            iz = iz + 1
            viz(iz) = this%propsedge(1, m) / area
            diz(iz) = this%propsedge(5, m)
          else
            ic = ic + 1
            nix(ic) = -this%propsedge(3, m)
            niy(ic) = -this%propsedge(4, m)
            di(ic) = this%propsedge(5, m)
            if (area > DZERO) then
              vi(ic) = this%propsedge(1, m) / area
            else
              vi(ic) = DZERO
            end if
          end if
        end if
      end do
      !
      ! -- Assign number of vertical and horizontal connections
      ncz = iz
      nc = ic
      !
      ! -- calculate z weight (wiz) and z velocity
      if (ncz == 1) then
        wiz(1) = DONE
      else
        dsumz = DZERO
        do iz = 1, ncz
          dsumz = dsumz + diz(iz)
        end do
        denom = (ncz - DONE)
        if (denom < DZERO) denom = DZERO
        dsumz = dsumz + DEM10 * dsumz
        do iz = 1, ncz
          if (dsumz > DZERO) wiz(iz) = DONE - diz(iz) / dsumz
          if (denom > 0) then
            wiz(iz) = wiz(iz) / denom
          else
            wiz(iz) = DZERO
          end if
        end do
      end if
      vz = DZERO
      do iz = 1, ncz
        vz = vz + wiz(iz) * viz(iz)
      end do
      !
      ! -- distance-based weighting
      nc = ic
      dsumx = DZERO
      dsumy = DZERO
      dsumz = DZERO
      do ic = 1, nc
        wix(ic) = di(ic) * abs(nix(ic))
        wiy(ic) = di(ic) * abs(niy(ic))
        dsumx = dsumx + wix(ic)
        dsumy = dsumy + wiy(ic)
      end do
      !
      ! -- Finish computing omega weights.  Add a tiny bit
      !    to dsum so that the normalized omega weight later
      !    evaluates to (essentially) 1 in the case of a single
      !    relevant connection, avoiding 0/0.
      dsumx = dsumx + DEM10 * dsumx
      dsumy = dsumy + DEM10 * dsumy
      do ic = 1, nc
        wix(ic) = (dsumx - wix(ic)) * abs(nix(ic))
        wiy(ic) = (dsumy - wiy(ic)) * abs(niy(ic))
      end do
      !
      ! -- compute B weights
      dsumx = DZERO
      dsumy = DZERO
      do ic = 1, nc
        bix(ic) = wix(ic) * sign(DONE, nix(ic))
        biy(ic) = wiy(ic) * sign(DONE, niy(ic))
        dsumx = dsumx + wix(ic) * abs(nix(ic))
        dsumy = dsumy + wiy(ic) * abs(niy(ic))
      end do
      if (dsumx > DZERO) dsumx = DONE / dsumx
      if (dsumy > DZERO) dsumy = DONE / dsumy
      axy = DZERO
      ayx = DZERO
      do ic = 1, nc
        bix(ic) = bix(ic) * dsumx
        biy(ic) = biy(ic) * dsumy
        axy = axy + bix(ic) * niy(ic)
        ayx = ayx + biy(ic) * nix(ic)
      end do
      !
      ! -- Calculate specific discharge.  The divide by zero checking below
      !    is problematic for cells with only one flow, such as can happen
      !    with triangular cells in corners.  In this case, the resulting
      !    cell velocity will be calculated as zero.  The method should be
      !    improved so that edge flows of zero are included in these
      !    calculations.  But this needs to be done with consideration for LGR
      !    cases in which flows are submitted from an exchange.
      vx = DZERO
      vy = DZERO
      do ic = 1, nc
        vx = vx + (bix(ic) - axy * biy(ic)) * vi(ic)
        vy = vy + (biy(ic) - ayx * bix(ic)) * vi(ic)
      end do
      denom = DONE - axy * ayx
      if (denom /= DZERO) then
        vx = vx / denom
        vy = vy / denom
      end if
      !
      this%spdis(1, n) = vx
      this%spdis(2, n) = vy
      this%spdis(3, n) = vz
      !
    end do
    !
    ! -- cleanup
    deallocate (vi)
    deallocate (di)
    deallocate (nix)
    deallocate (niy)
    deallocate (wix)
    deallocate (wiy)
    deallocate (wiz)
    deallocate (bix)
    deallocate (biy)
    !
    ! -- return
    return
  end subroutine calc_spdis

  subroutine sav_spdis(this, ibinun)
! ******************************************************************************
! sav_spdis -- save specific discharge in binary format to ibinun
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(GwfNpfType) :: this
    integer(I4B), intent(in) :: ibinun
    ! -- local
    character(len=16) :: text
    character(len=16), dimension(3) :: auxtxt
    integer(I4B) :: n
    integer(I4B) :: naux
! ------------------------------------------------------------------------------
    !
    ! -- Write the header
    text = '      DATA-SPDIS'
    naux = 3
    auxtxt(:) = ['              qx', '              qy', '              qz']
    call this%dis%record_srcdst_list_header(text, this%name_model, &
                                            this%packName, this%name_model, &
                                            this%packName, naux, auxtxt, ibinun, &
                                            this%dis%nodes, this%iout)
    !
    ! -- Write a zero for Q, and then write qx, qy, qz as aux variables
    do n = 1, this%dis%nodes
      call this%dis%record_mf6_list_entry(ibinun, n, n, DZERO, naux, &
                                          this%spdis(:, n))
    end do
    !
    ! -- return
    return
  end subroutine sav_spdis

  subroutine sav_sat(this, ibinun)
! ******************************************************************************
! sav_sat -- save saturation in binary format to ibinun
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(GwfNpfType) :: this
    integer(I4B), intent(in) :: ibinun
    ! -- local
    character(len=16) :: text
    character(len=16), dimension(1) :: auxtxt
    real(DP), dimension(1) :: a
    integer(I4B) :: n
    integer(I4B) :: naux
! ------------------------------------------------------------------------------
    !
    ! -- Write the header
    text = '        DATA-SAT'
    naux = 1
    auxtxt(:) = ['             sat']
    call this%dis%record_srcdst_list_header(text, this%name_model, &
                                            this%packName, this%name_model, &
                                            this%packName, naux, auxtxt, ibinun, &
                                            this%dis%nodes, this%iout)
    !
    ! -- Write a zero for Q, and then write saturation as an aux variables
    do n = 1, this%dis%nodes
      a(1) = this%sat(n)
      call this%dis%record_mf6_list_entry(ibinun, n, n, DZERO, naux, a)
    end do
    !
    ! -- return
    return
  end subroutine sav_sat

  subroutine increase_edge_count(this, nedges)
! ******************************************************************************
! increase_edge_count -- reserve space for nedges cells that have an edge on them.
!   This must be called before the npf%allocate_arrays routine, which is called
!   from npf%ar.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(GwfNpfType) :: this
    integer(I4B), intent(in) :: nedges
    ! -- local
! ------------------------------------------------------------------------------
    !
    this%nedges = this%nedges + nedges
    !
    ! -- return
    return
  end subroutine increase_edge_count

  subroutine set_edge_properties(this, nodedge, ihcedge, q, area, nx, ny, &
                                 distance)
! ******************************************************************************
! edge_count -- provide the npf package with edge properties.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(GwfNpfType) :: this
    integer(I4B), intent(in) :: nodedge
    integer(I4B), intent(in) :: ihcedge
    real(DP), intent(in) :: q
    real(DP), intent(in) :: area
    real(DP), intent(in) :: nx
    real(DP), intent(in) :: ny
    real(DP), intent(in) :: distance
    ! -- local
    integer(I4B) :: lastedge
! ------------------------------------------------------------------------------
    !
    this%lastedge = this%lastedge + 1
    lastedge = this%lastedge
    this%nodedge(lastedge) = nodedge
    this%ihcedge(lastedge) = ihcedge
    this%propsedge(1, lastedge) = q
    this%propsedge(2, lastedge) = area
    this%propsedge(3, lastedge) = nx
    this%propsedge(4, lastedge) = ny
    this%propsedge(5, lastedge) = distance
    !
    ! -- If this is the last edge, then the next call must be starting a new
    !    edge properties assignment loop, so need to reset lastedge to 0
    if (this%lastedge == this%nedges) this%lastedge = 0
    !
    ! -- return
    return
  end subroutine set_edge_properties

  !> Calculate saturated thickness between cell n and m
  !<
  function calcSatThickness(this, n, m, ihc) result(satThickness)
    class(GwfNpfType) :: this !< this NPF instance
    integer(I4B) :: n !< node n
    integer(I4B) :: m !< node m
    integer(I4B) :: ihc !< 1 = horizonal connection, 0 for vertical
    real(DP) :: satThickness !< saturated thickness

    satThickness = thksatnm(this%ibound(n), &
                            this%ibound(m), &
                            this%icelltype(n), &
                            this%icelltype(m), &
                            this%inewton, &
                            ihc, &
                            this%iusgnrhc, &
                            this%hnew(n), &
                            this%hnew(m), &
                            this%sat(n), &
                            this%sat(m), &
                            this%dis%top(n), &
                            this%dis%top(m), &
                            this%dis%bot(n), &
                            this%dis%bot(m), &
                            this%satomega, &
                            this%satmin)

  end function calcSatThickness

  function thksatnm(ibdn, ibdm, ictn, ictm, inwtup, ihc, iusg, &
                    hn, hm, satn, satm, topn, topm, botn, botm, &
                    satomega, satminopt) result(res)
! ******************************************************************************
! thksatnm -- calculate saturated thickness at interface between two cells
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- return
    real(DP) :: res
    ! -- dummy
    integer(I4B), intent(in) :: ibdn
    integer(I4B), intent(in) :: ibdm
    integer(I4B), intent(in) :: ictn
    integer(I4B), intent(in) :: ictm
    integer(I4B), intent(in) :: inwtup
    integer(I4B), intent(in) :: ihc
    integer(I4B), intent(in) :: iusg
    real(DP), intent(in) :: hn
    real(DP), intent(in) :: hm
    real(DP), intent(in) :: satn
    real(DP), intent(in) :: satm
    real(DP), intent(in) :: topn
    real(DP), intent(in) :: topm
    real(DP), intent(in) :: botn
    real(DP), intent(in) :: botm
    real(DP), intent(in) :: satomega
    real(DP), optional, intent(in) :: satminopt
    ! -- local
    integer(I4B) :: indk
    real(DP) :: satmin
    real(DP) :: sn
    real(DP) :: sm
    real(DP) :: thksatn
    real(DP) :: thksatm
    real(DP) :: sill_top, sill_bot
    real(DP) :: tpn, tpm
    real(DP) :: top, bot
! ------------------------------------------------------------------------------
    if (present(satminopt)) then
      satmin = satminopt
    else
      satmin = DZERO
    end if
    !
    ! -- If either n or m is inactive then saturated thickness is zero
    if (ibdn == 0 .or. ibdm == 0) then
      res = DZERO
      !
      ! -- if both cells are non-convertible then use average cell thickness
    elseif (ictn == 0 .and. ictm == 0) then
      res = DHALF * (topn - botn + topm - botm)
      !
      ! -- At least one of the cells is convertible, so calculate average saturated
      !    thickness
    else
      if (inwtup == 1) then
        ! -- set flag used to determine if bottom of cells n and m are
        !    significantly different
        indk = 0
        if (abs(botm - botn) < DEM2) indk = 1
        ! -- recalculate saturation if using MODFLOW-USG saturation
        !    calculation approach
        if (iusg == 1 .and. indk == 0) then
          if (botm > botn) then
            top = topm
            bot = botm
          else
            top = topn
            bot = botn
          end if
          sn = sQuadraticSaturation(top, bot, hn, satomega, satmin)
          sm = sQuadraticSaturation(top, bot, hm, satomega, satmin)
        else
          sn = sQuadraticSaturation(topn, botn, hn, satomega, satmin)
          sm = sQuadraticSaturation(topm, botm, hm, satomega, satmin)
        end if
        !
        ! -- upstream weight the thickness
        if (hn > hm) then
          res = sn * (topn - botn)
        else
          res = sm * (topm - botm)
        end if
        !
      else
        thksatn = satn * (topn - botn)
        thksatm = satm * (topm - botm)
        !
        ! -- If staggered connection, subtract parts of cell that are above and
        !    below the sill top and bottom elevations
        if (ihc == 2) then
          !
          ! -- Calculate sill_top and sill_bot
          sill_top = min(topn, topm)
          sill_bot = max(botn, botm)
          !
          ! -- Calculate tpn and tpm
          tpn = botn + thksatn
          tpm = botm + thksatm
          !
          ! -- Calculate saturated thickness for cells n and m
          thksatn = max(min(tpn, sill_top) - sill_bot, DZERO)
          thksatm = max(min(tpm, sill_top) - sill_bot, DZERO)
        end if
        !
        res = DHALF * (thksatn + thksatm)
      end if
    end if
    !
    ! -- Return
    return
  end function thksatnm

end module GwfNpfModule
