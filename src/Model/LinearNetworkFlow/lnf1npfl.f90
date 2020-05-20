module LnfNpflModule  ! Npf package for laminar flow
  use KindModule,                 only: DP, I4B
  use ConstantsModule,            only: DZERO, DEM9, DEM8, DEM7, DEM6, DEM2,    &
                                        DHALF, DP9, DONE, DTWO,                 &
                                        DLNLOW, DLNHIGH,                        &
                                        DHNOFLO, DHDRY, DEM10
  use SmoothingModule,            only: sQuadraticSaturation,                   &
                                        sQuadraticSaturationDerivative
  use NumericalPackageModule,     only: NumericalPackageType
  use LnfDislModule,              only: LnfDislType, GeometryContainer
  use LnfIcModule,                only: LnfIcType
  use Xt3dModule,                 only: Xt3dType
  use BlockParserModule,          only: BlockParserType

  implicit none

  private
  public :: LnfNpflType
  public :: npf_cr
  !public :: hcond
  !public :: vcond

  type, extends(NumericalPackageType) :: LnfNpflType

    type(LnfIcType), pointer                        :: ic           => null()    ! initial conditions object
    integer(I4B), pointer                           :: iname        => null()    ! length of variable names
    character(len=24), dimension(:), pointer        :: aname        => null()    ! variable names
    integer(I4B), dimension(:), pointer, contiguous :: ibound       => null()    ! pointer to model ibound
    real(DP), dimension(:), pointer, contiguous     :: hnew         => null()    ! pointer to model xnew
    integer(I4B), pointer                           :: inwtupw      => null()    ! MODFLOW-NWT upstream weighting option flag
    integer(I4B), pointer                           :: icalcspdis   => null()    ! Calculate specific discharge at cell centers
    integer(I4B), pointer                           :: isavspdis    => null()    ! Save specific discharge at cell centers
    integer(I4B), pointer                           :: isavsat      => null()    ! Save sat to budget file
    real(DP), pointer                               :: hnoflo       => null()    ! default is 1.e30
    real(DP), pointer                               :: hdry         => null()    ! default is -1.d30
    integer(I4B), pointer                           :: icellavg     => null()    ! harmonic(0), logarithmic(1), or arithmetic thick-log K (2)
    integer(I4B), dimension(:), pointer, contiguous :: icelltype    => null()    ! confined (0) or convertible (1)
    !
    ! K properties
    real(DP), pointer                               :: visc         => null()    ! dynamic viscosity of fluid
    !integer(I4B), pointer                           :: ivisc        => null()    ! flag that dynamic viscosity is specified
    !
    real(DP), dimension(:), pointer, contiguous     :: sat          => null()    ! saturation (0. to 1.) for each cell
    real(DP), dimension(:), pointer, contiguous     :: condsat      => null()    ! saturated conductance (symmetric array)
    real(DP), pointer                               :: satmin       => null()    ! minimum saturated thickness
    integer(I4B), dimension(:), pointer, contiguous :: ibotnode     => null()    ! bottom node used if igwfnewtonur /= 0
    !
    integer(I4B), pointer                           :: iwetdry      => null()    ! flag to indicate angle1 was read
    real(DP), dimension(:), pointer, contiguous     :: wetdry       => null()    ! wetdry array
    !
    real(DP), dimension(:, :), pointer, contiguous  :: spdis        => null()    ! specific discharge : qx, qy, qz (nodes, 3)
    type(LnfDislType), pointer                      :: disl         => null()
    !integer(I4B), pointer                           :: nedges       => null()    ! number of cell edges
    !integer(I4B), pointer                           :: lastedge     => null()    ! last edge number
    !integer(I4B), dimension(:), pointer, contiguous :: nodedge      => null()    ! array of node numbers that have edges
    !integer(I4B), dimension(:), pointer, contiguous :: ihcedge      => null()    ! edge type (horizontal or vertical)
    !real(DP), dimension(:, :), pointer, contiguous  :: propsedge    => null()    ! edge properties (Q, area, nx, ny, distance)
    !
  contains
    procedure                               :: npf_df
    procedure                               :: npf_ac
    procedure                               :: npf_mc
    procedure                               :: npf_ar
    procedure                               :: npf_init_mem
    procedure                               :: npf_ad
    procedure                               :: npf_cf
    procedure                               :: npf_fc
    !procedure                               :: npf_fn
    procedure                               :: npf_flowja
    procedure                               :: npf_bdadj
    !procedure                               :: npf_nur
    procedure                               :: npf_ot
    procedure                               :: npf_da
    procedure, private                      :: qcalc      => slnf_npf_qcalc
    !procedure, private                      :: wd         => slnf_npf_wetdry
    !procedure, private                      :: wdmsg      => slnf_npf_wdmsg
    procedure                               :: allocate_scalars
    procedure, private                      :: allocate_arrays
    procedure, private                      :: read_options
    !procedure, private                      :: rewet_options
    !procedure, private                      :: check_options
    procedure, private                      :: prepcheck
    !procedure, public                       :: rewet_check
    !procedure, public                       :: hy_eff
    !procedure, public                       :: calc_spdis
    procedure, public                       :: sav_spdis
    procedure, public                       :: sav_sat
    procedure, public                       :: condmean
    procedure, public                       :: lcondsat
    !procedure, public                       :: increase_edge_count
    !procedure, public                       :: set_edge_properties
  endtype

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
    type(LnfNpfltype), pointer :: npfobj
    character(len=*), intent(in) :: name_model
    integer(I4B), intent(in) :: inunit
    integer(I4B), intent(in) :: iout
! ------------------------------------------------------------------------------
    !
    ! -- Create the object
    allocate(npfobj)
    !
    ! -- create name and origin
    call npfobj%set_names(1, name_model, 'NPFL', 'NPFL')
    !
    ! -- Allocate scalars
    call npfobj%allocate_scalars()
    !
    ! -- Set variables
    npfobj%inunit = inunit
    npfobj%iout   = iout
    !
    ! -- Return
    return
  end subroutine npf_cr

  subroutine npf_df(this, dis)
! ******************************************************************************
! npf_df -- Define
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use SimModule, only: ustop, store_error
    use Xt3dModule, only: xt3d_cr
    ! -- dummy
    class(LnfNpfltype) :: this
    class(LnfDislType), pointer, intent(inout) :: dis
    ! -- local
    ! -- formats
    character(len=*), parameter :: fmtheader =                                 &
      "(1x, /1x, 'NPF -- NODE PROPERTY FLOW LAMINAR PACKAGE, VERSION 1, ',     &
       &' 4/22/2020 INPUT READ FROM UNIT ', i0, //)"
    ! -- data
! ------------------------------------------------------------------------------
    !
    ! -- Print a message identifying the node property flow package.
    write(this%iout, fmtheader) this%inunit
    !
    ! -- Set a pointer to dis
    this%disl => dis
    !
    ! -- Initialize block parser
    call this%parser%Initialize(this%inunit, this%iout)
    !
    ! -- set, read, and check options
    call this%read_options()
    !call this%check_options()
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
    class(LnfNpflType) :: this
    integer(I4B), intent(in) :: moffset
    type(sparsematrix), intent(inout) :: sparse
    ! -- local
! ------------------------------------------------------------------------------
    !
    ! -- Add extended neighbors (neighbors of neighbors)
    !if(this%ixt3d /= 0) call this%xt3d%xt3d_ac(moffset, sparse)
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
    class(LnfNpflType) :: this
    integer(I4B), intent(in) :: moffset
    integer(I4B), dimension(:), intent(in) :: iasln
    integer(I4B), dimension(:), intent(in) :: jasln
    ! -- local
! ------------------------------------------------------------------------------
    !
    !if(this%ixt3d /= 0) call this%xt3d%xt3d_mc(moffset, iasln,   &
    !                                          jasln, this%inewton)
    !
    ! -- Return
    return
  end subroutine npf_mc  
  
  subroutine npf_init_mem(this, dis, ixt3d, icelltype, wetdry)
! ******************************************************************************
! npf_cr -- Create a new NPF object from memory
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(LnfNpfltype) :: this
    class(LnfDislType), pointer, intent(inout) :: dis
    integer(I4B), pointer, intent(in) :: ixt3d
    integer(I4B), dimension(:), pointer, contiguous, intent(inout) :: icelltype
    real(DP), dimension(:), pointer, contiguous, intent(inout), optional :: wetdry
    ! -- local
! ------------------------------------------------------------------------------
    !
    ! -- Store pointers to arguments that were passed in
    this%disl => dis
    !
    ! -- set ixt3d (1 - HCOF and RHS; 2 - RHS only)
    !this%ixt3d = ixt3d
    !
    ! -- allocate arrays
    call this%allocate_arrays(dis%nodes, dis%njas)

    !-- fill icelltype
    call dis%fill_grid_array(icelltype, this%icelltype)
    !
    !
    ! -- fill wetdry data
    if (present(wetdry)) then
      this%iwetdry = 1
      !this%irewet = 1
    end if
    !
    ! -- Return
    return
  end subroutine npf_init_mem

  subroutine npf_ar(this, ic, ibound, hnew)
! ******************************************************************************
! npf_ar -- Allocate and Read
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(LnfNpfltype) :: this
    type(LnfIcType), pointer, intent(in) :: ic
    integer(I4B), dimension(:), pointer, contiguous, intent(inout) :: ibound
    real(DP), dimension(:), pointer, contiguous, intent(inout) :: hnew
    ! -- local
    ! -- formats
    ! -- data
! ------------------------------------------------------------------------------
    !
    ! -- Store pointers to arguments that were passed in
    this%ic      => ic
    this%ibound  => ibound
    this%hnew    => hnew
    !
    ! -- read data from files
    if (this%inunit /= 0)  then
      !
      ! -- allocate arrays
      call this%allocate_arrays(this%disl%nodes, this%disl%njas)
      !
      ! -- read the data block
      !call this%read_data()
    end if
    !
    ! -- Initialize and check data
    call this%prepcheck()
    !
    ! -- xt3d
    !if (this%ixt3d /= 0) then
    !  call this%xt3d%xt3d_ar(ibound, this%k11, this%ik33, this%k33,              &
    !                         this%sat, this%ik22, this%k22, this%inewton,        &
    !                         this%icelltype, this%iangle1,                       &
    !                         this%iangle2, this%iangle3, this%angle1,            &
    !                         this%angle2, this%angle3)
    !end if
    !
    ! -- Return
    return
  end subroutine npf_ar

  subroutine npf_ad(this, nodes, hold)
! ******************************************************************************
! npf_ad -- Advance
! Subroutine (1) Sets hold to bot whenever a wettable cell is dry
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    implicit none
    class(LnfNpfltype) :: this
    integer(I4B),intent(in) :: nodes
    real(DP),dimension(nodes),intent(inout) :: hold
    integer(I4B) :: n
! ------------------------------------------------------------------------------
    !
    ! -- loop through all cells and set hold=bot if wettable cell is dry
    !if(this%irewet > 0) then
    !  do n = 1, this%disl%nodes
    !    if(this%wetdry(n) == DZERO) cycle
    !    if(this%ibound(n) /= 0) cycle
    !    hold(n) = this%disl%bot(n)
    !  enddo
    !endif
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
    class(LnfNpfltype) :: this
    integer(I4B) :: kiter
    integer(I4B),intent(in) :: nodes
    real(DP),intent(inout),dimension(nodes) :: hnew
    ! -- local
    integer(I4B) :: n
    real(DP) :: satn
! ------------------------------------------------------------------------------
    ! Calculate
    !
    ! -- Perform wetting and drying
    !if (this%inewton /= 1) then
    !  call this%wd(kiter, hnew)
    !end if
    !
    ! -- Calculate saturation for convertible cells
    !do n = 1, this%disl%nodes
    !  if(this%icelltype(n) /= 0) then
    !    if(this%ibound(n) == 0) then
    !      satn = DZERO
    !    else
    !      call this%thksat(n, hnew(n), satn)
    !    endif
    !    this%sat(n) = satn
    !  endif
    !enddo
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
    class(LnfNpfltype) :: this
    integer(I4B) :: kiter
    integer(I4B),intent(in) :: njasln
    real(DP),dimension(njasln),intent(inout) :: amat
    integer(I4B),intent(in),dimension(:) :: idxglo
    real(DP),intent(inout),dimension(:) :: rhs
    real(DP),intent(inout),dimension(:) :: hnew
    ! -- local
    integer(I4B) :: n, m, ii, idiag, ihc
    integer(I4B) :: isymcon, idiagm
    real(DP) :: hyn, hym
    real(DP) :: cond
! ------------------------------------------------------------------------------
    !
    ! -- Calculate conductance and put into amat
    !
    !if(this%ixt3d /= 0) then
    !  call this%xt3d%xt3d_fc(kiter, njasln, amat, idxglo, rhs, hnew)
    !else
    !
    do n = 1, this%disl%nodes
      do ii = this%disl%con%ia(n) + 1, this%disl%con%ia(n + 1) - 1
        if (this%disl%con%mask(ii) == 0) cycle

        m = this%disl%con%ja(ii)
        !
        ! -- Calculate conductance only for upper triangle but insert into
        !    upper and lower parts of amat.
        if(m < n) cycle
        !ihc = this%disl%con%ihc(this%disl%con%jas(ii))
        !hyn = this%hy_eff(n, m, ihc, ipos=ii)
        !hym = this%hy_eff(m, n, ihc, ipos=ii)
        cond = lcond(this%ibound(n), this%ibound(m),                          &
                     this%icelltype(n), this%icelltype(m),                    &
                     this%inewton, this%condsat(this%disl%con%jas(ii)))
          !
          ! -- Horizontal conductance
          !cond = hcond(this%ibound(n), this%ibound(m),                       &
          !             this%icelltype(n), this%icelltype(m),                 &
          !             this%inewton, this%inewton,                           &
          !             this%disl%con%ihc(this%disl%con%jas(ii)),               &
          !             this%icellavg, this%iusgnrhc, this%inwtupw,           &
          !             this%condsat(this%disl%con%jas(ii)),                   &
          !             hnew(n), hnew(m), this%sat(n), this%sat(m), hyn, hym, &
          !             this%disl%top(n), this%disl%top(m),                     &
          !             this%disl%bot(n), this%disl%bot(m),                     &
          !             this%disl%con%cl1(this%disl%con%jas(ii)),               &
          !             this%disl%con%cl2(this%disl%con%jas(ii)),               &
          !             this%disl%con%hwva(this%disl%con%jas(ii)),              &
          !             this%satomega, this%satmin)
        !
        ! -- Fill row n
        idiag = this%disl%con%ia(n)
        amat(idxglo(ii)) = amat(idxglo(ii)) + cond
        amat(idxglo(idiag)) = amat(idxglo(idiag)) - cond
        !
        ! -- Fill row m
        isymcon = this%disl%con%isym(ii)
        idiagm = this%disl%con%ia(m)
        amat(idxglo(isymcon)) = amat(idxglo(isymcon)) + cond
        amat(idxglo(idiagm)) = amat(idxglo(idiagm)) - cond
      enddo
    enddo
    !
    !endif
    !
    ! -- Return
    return
  end subroutine npf_fc

  subroutine npf_flowja(this, hnew, flowja)
! ******************************************************************************
! npf_flowja -- Calculate flowja
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(LnfNpfltype) :: this
    real(DP),intent(inout),dimension(:) :: hnew
    real(DP),intent(inout),dimension(:) :: flowja
    ! -- local
    integer(I4B) :: n, ipos, m
    real(DP) :: qnm
! ------------------------------------------------------------------------------
    !
    ! -- Calculate the flow across each cell face and store in flowja
    !
    do n = 1, this%disl%nodes
      do ipos = this%disl%con%ia(n) + 1, this%disl%con%ia(n + 1) - 1
        m = this%disl%con%ja(ipos)
        if(m < n) cycle
        call this%qcalc(n, m, hnew(n), hnew(m), ipos, qnm)
        flowja(ipos) = qnm
        flowja(this%disl%con%isym(ipos)) = -qnm
      enddo
    enddo
    !
    ! -- Return
    return
  end subroutine npf_flowja

  subroutine slnf_npf_qcalc(this, n, m, hn, hm, icon, qnm)
! ******************************************************************************
! slnf_npf_qcalc -- Flow between two cells
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(LnfNpfltype) :: this
    integer(I4B),intent(in) :: n
    integer(I4B),intent(in) :: m
    real(DP),intent(in) :: hn
    real(DP),intent(in) :: hm
    integer(I4B),intent(in) :: icon
    real(DP),intent(inout) :: qnm
    ! -- local
    !real(DP) :: hyn, hym
    real(DP) :: condnm
    real(DP) :: hntemp, hmtemp
    integer(I4B) :: ii, mm
! ------------------------------------------------------------------------------
    !
    ! -- Initialize
    !ihc = this%disl%con%ihc(this%disl%con%jas(icon))
    !hyn = this%hy_eff(n, m, ihc, ipos=icon)
    !hym = this%hy_eff(m, n, ihc, ipos=icon)
    !
    ! -- Calculate conductance
    outer: do ii = this%disl%con%ia(n) + 1, this%disl%con%ia(n + 1) - 1
      if (this%disl%con%mask(ii) == 0) cycle
        mm = this%disl%con%ja(ii)
        !
        ! -- Calculate conductance only for upper triangle but insert into
        !    upper and lower parts of amat.
        if(mm .ne. m) cycle
        !ihc = this%disl%con%ihc(this%disl%con%jas(ii))
        !hyn = this%hy_eff(n, m, ihc, ipos=ii)
        !hym = this%hy_eff(m, n, ihc, ipos=ii)
        condnm = lcond(this%ibound(n), this%ibound(m),                        &
                       this%icelltype(n), this%icelltype(m),                  &
                       this%inewton, this%condsat(this%disl%con%jas(ii)))
        exit outer
    end do outer

    !
    ! -- Initialize hntemp and hmtemp
    hntemp = hn
    hmtemp = hm
    !
    ! -- Calculate flow positive into cell n
    qnm = condnm * (hmtemp - hntemp)
    !
    ! -- Return
    return
  end subroutine slnf_npf_qcalc

  subroutine npf_bdadj(this, flowja, icbcfl, icbcun)
! ******************************************************************************
! npf_bdadj -- Record flowja and calculate specific discharge if requested
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(LnfNpfltype) :: this
    real(DP),dimension(:),intent(in) :: flowja
    integer(I4B), intent(in) :: icbcfl
    integer(I4B), intent(in) :: icbcun
    ! -- local
    integer(I4B) :: ibinun
    !data
    ! -- formats
! ------------------------------------------------------------------------------
    !
    ! -- Set unit number for binary output
    if(this%ipakcb < 0) then
      ibinun = icbcun
    elseif(this%ipakcb == 0) then
      ibinun = 0
    else
      ibinun = this%ipakcb
    endif
    if(icbcfl == 0) ibinun = 0
    !
    ! -- Write the face flows if requested
    if(ibinun /= 0) then
      call this%disl%record_connection_array(flowja, ibinun, this%iout)
    endif
    !
    ! -- Calculate specific discharge at cell centers and write, if requested
    !if (this%icalcspdis /= 0) then
    !  call this%calc_spdis(flowja)
    !  if(ibinun /= 0) call this%sav_spdis(ibinun)
    !endif
    !
    ! -- Save saturation, if requested
    if (this%isavsat /= 0) then
      if(ibinun /= 0) call this%sav_sat(ibinun)
    endif
    !
    ! -- Return
    return
  end subroutine npf_bdadj

  subroutine npf_ot(this, flowja)
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
    class(LnfNpfltype) :: this
    real(DP),intent(inout),dimension(:) :: flowja
    ! -- local
    character(len=LENBIGLINE) :: line
    character(len=30) :: tempstr
    integer(I4B) :: n, ipos, m
    real(DP) :: qnm
    ! -- formats
    character(len=*), parameter :: fmtiprflow =                                &
      "(/,4x,'CALCULATED INTERCELL FLOW FOR PERIOD ', i0, ' STEP ', i0)"
! ------------------------------------------------------------------------------
    !
    ! -- Write flowja to list file if requested
    if (this%iprflow > 0) then
      write(this%iout, fmtiprflow) kper, kstp
      do n = 1, this%disl%nodes
        line = ''
        call this%disl%noder_to_string(n, tempstr)
        line = trim(tempstr) // ':'
        do ipos = this%disl%con%ia(n) + 1, this%disl%con%ia(n + 1) - 1
          m = this%disl%con%ja(ipos)
          call this%disl%noder_to_string(m, tempstr)
          line = trim(line) // ' ' // trim(tempstr)
          qnm = flowja(ipos)
          write(tempstr, '(1pg15.6)') qnm
          line = trim(line) // ' ' // trim(adjustl(tempstr))
        enddo
        write(this%iout, '(a)') trim(line)
      enddo
    endif
    !
    ! -- Return
    return
  end subroutine npf_ot

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
    class(LnfNpfltype) :: this
! ------------------------------------------------------------------------------
    !
    ! -- Strings
    !
    ! -- Scalars
    call mem_deallocate(this%iname)
    call mem_deallocate(this%hnoflo)
    call mem_deallocate(this%hdry)
    call mem_deallocate(this%icellavg)
    call mem_deallocate(this%visc)
    call mem_deallocate(this%icalcspdis)
    call mem_deallocate(this%isavspdis)
    call mem_deallocate(this%isavsat)
    call mem_deallocate(this%satmin)
    call mem_deallocate(this%iwetdry)
    
    !call mem_deallocate(this%ivisc)
    !
    ! -- Deallocate arrays
    call mem_deallocate(this%ibotnode)
    call mem_deallocate(this%icelltype)
    call mem_deallocate(this%sat)
    call mem_deallocate(this%condsat)
    call mem_deallocate(this%spdis)
    call mem_deallocate(this%wetdry)
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
    ! -- dummy
    class(LnfNpfltype) :: this
! ------------------------------------------------------------------------------
    !
    ! -- allocate scalars in NumericalPackageType
    call this%NumericalPackageType%allocate_scalars()
    !
    ! -- Allocate scalars
    call mem_allocate(this%iname, 'INAME', this%origin)
    call mem_allocate(this%hnoflo, 'HNOFLO', this%origin)
    call mem_allocate(this%hdry, 'HDRY', this%origin)
    call mem_allocate(this%icellavg, 'ICELLAVG', this%origin)
    call mem_allocate(this%visc, 'VISC', this%origin)
    call mem_allocate(this%icalcspdis, 'ICALCSPDIS', this%origin)
    call mem_allocate(this%isavspdis, 'ISAVSPDIS', this%origin)
    call mem_allocate(this%isavsat, 'ISAVSAT', this%origin)
    !call mem_allocate(this%irewet, 'IREWET', this%origin)
    !call mem_allocate(this%wetfct, 'WETFCT', this%origin)
    !call mem_allocate(this%iwetit, 'IWETIT', this%origin)
    call mem_allocate(this%satmin, 'SATMIN', this%origin)
    call mem_allocate(this%iwetdry, 'IWETDRY', this%origin)
    ! -- Initialize value
    this%iname = 2
    this%hnoflo = DHNOFLO !1.d30
    this%hdry = DHDRY !-1.d30
    this%icellavg = 1
    this%visc = DZERO
    this%icalcspdis = 0
    this%isavspdis = 0
    this%isavsat = 0
    !this%irewet = 0
    !this%wetfct = DONE
    !this%iwetit = 1
    !this%ihdwet = 0
    this%satmin = DZERO ! DEM7
    this%iwetdry = 0
    !this%nedges = 0
    !this%lastedge = 0
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
    class(LnfNpfltype) :: this
    integer(I4B), intent(in) :: ncells
    integer(I4B), intent(in) :: njas
    ! -- local
    integer(I4B) :: n
! ------------------------------------------------------------------------------
    !
    call mem_allocate(this%icelltype, ncells, 'ICELLTYPE', trim(this%origin))
    call mem_allocate(this%sat, ncells, 'SAT', trim(this%origin))
    call mem_allocate(this%condsat, njas, 'CONDSAT', trim(this%origin))
    ! -- Optional arrays dimensioned to full size initially
    call mem_allocate(this%wetdry, ncells, 'WETDRY', trim(this%origin))
    !
    ! -- Optional arrays
    call mem_allocate(this%ibotnode, 0, 'IBOTNODE', trim(this%origin))
    !
    ! -- Specific discharge
    if (this%icalcspdis == 1) then
      call mem_allocate(this%spdis, 3, ncells, 'SPDIS', trim(this%origin))
    else
      call mem_allocate(this%spdis, 3, 0, 'SPDIS', trim(this%origin))
    endif
    !
    ! -- initialize wetdry
    do n = 1, ncells
      this%wetdry(n) = DZERO
    end do

    !
    ! -- allocate variable names
    allocate(this%aname(this%iname))
    this%aname = ['               ICELLTYPE', '                  WETDRY']
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
    use ConstantsModule,   only: LINELENGTH
    use SimModule, only: ustop, store_error, count_errors
    implicit none
    ! -- dummy
    class(LnfNpfltype) :: this
    ! -- local
    character(len=LINELENGTH) :: errmsg, keyword
    integer(I4B) :: ierr
    logical :: isfound, endOfBlock
    ! -- formats
    character(len=*), parameter :: fmtiprflow =                                &
      "(4x,'CELL-BY-CELL FLOW INFORMATION WILL BE PRINTED TO LISTING FILE " // &
      "WHENEVER ICBCFL IS NOT ZERO.')"
    character(len=*), parameter :: fmtisvflow =                                &
      "(4x,'CELL-BY-CELL FLOW INFORMATION WILL BE SAVED TO BINARY FILE " //    &
      "WHENEVER ICBCFL IS NOT ZERO.')"
    character(len=*), parameter :: fmtcellavg =                                &
      "(4x,'ALTERNATIVE CELL AVERAGING HAS BEEN SET TO ', a)"
    character(len=*), parameter :: fmtnct =                                    &
      "(1x, 'Negative cell thickness at cell: ', a)"
    ! -- data
! ------------------------------------------------------------------------------
    !
    ! -- get options block
    call this%parser%GetBlock('OPTIONS', isfound, ierr, &
                              supportOpenClose=.true., blockRequired=.false.)
    !
    ! -- parse options block if detected
    if (isfound) then
      write(this%iout,'(1x,a)')'PROCESSING NPF OPTIONS'
      do
        call this%parser%GetNextLine(endOfBlock)
        if (endOfBlock) exit
        call this%parser%GetStringCaps(keyword)
        select case (keyword)
          case ('PRINT_FLOWS')
            this%iprflow = 1
            write(this%iout, fmtiprflow)
          case ('SAVE_FLOWS')
            this%ipakcb = -1
            write(this%iout, fmtisvflow)
          case ('ALTERNATIVE_CELL_AVERAGING')
            call this%parser%GetStringCaps(keyword)
            select case(keyword)
              case('LOGARITHMIC')
                this%icellavg = 1
                write(this%iout, fmtcellavg) 'LOGARITHMIC'
              case('AMT-LMK')
                this%icellavg = 2
                write(this%iout, fmtcellavg) 'AMT-LMK'
              case('AMT-HMK')
                this%icellavg = 3
                write(this%iout, fmtcellavg) 'AMT-HMK'
              case default
                write(errmsg,'(4x,a,a)')'UNKNOWN CELL AVERAGING METHOD: ',     &
                                         keyword
                call store_error(errmsg)
                call this%parser%StoreErrorUnit()
                call ustop()
            end select
            write(this%iout,'(4x,a,a)')                                        &
              'CELL AVERAGING METHOD HAS BEEN SET TO: ', keyword
          !case ('REWET')
          !  call this%rewet_options()
          case ('SAVE_SPECIFIC_DISCHARGE')
            this%icalcspdis = 1
            this%isavspdis = 1
            write(this%iout,'(4x,a)')                                          &
              'SPECIFIC DISCHARGE WILL BE CALCULATED AT CELL CENTERS ' //      &
              'AND WRITTEN TO DATA-SPDIS IN BUDGET FILE WHEN REQUESTED.'
          case ('SAVE_SATURATION')
            this%isavsat = 1
            write(this%iout,'(4x,a)')                                          &
              'SATURATION WILL BE WRITTEN TO DATA-SAT IN BUDGET FILE ' //      &
              'WHEN REQUESTED.'
          case ('VISCOSITY')
            this%visc = this%parser%GetDouble()
            write(this%iout,'(4x,a,1pg24.15)') 'VISCOSITY SPECIFIED AS ',      &
                                              this%visc
          case default
            write(errmsg,'(4x,a,a)')'****ERROR. UNKNOWN NPF OPTION: ',         &
                                     trim(keyword)
            call store_error(errmsg)
            call this%parser%StoreErrorUnit()
            call ustop()
        end select
      end do
      write(this%iout,'(1x,a)') 'END OF NPF OPTIONS'
    end if
    ! -- check if this%iusgnrhc has been enabled for a model that is not using
    !    the Newton-Raphson formulation
    if (this%visc == 0) then
      this%visc = DONE
      write(this%iout, '(4x,a,3(1x,a))')                                        &
        '****WARNING. Viscosity has not been specified. A default viscosity ',  &
        'value will be used.'
    end if
    !
    ! -- terminate if errors encountered in options block
    if (count_errors() > 0) then
      call this%parser%StoreErrorUnit()
      call ustop()
    end if
    !
    ! -- Return
    return
  end subroutine read_options

  subroutine prepcheck(this)
! ******************************************************************************
! prepcheck -- Initialize and check NPF data
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use ConstantsModule,   only: LINELENGTH, DONE, DPIO180
    use MemoryManagerModule, only: mem_allocate, mem_reallocate, mem_deallocate
    use SimModule, only: store_error, ustop, count_errors
    ! -- dummy
    class(LnfNpfltype) :: this
    ! -- local
    logical :: finished
    character(len=24), dimension(:), pointer :: aname
    character(len=LINELENGTH) :: errmsg
    character(len=30) :: cellstr
    integer(I4B) :: nerr
    real(DP) :: csat
    real(DP) :: satn, topn, topm, botn
    real(DP) :: fawidth
    real(DP) :: hn, hm
    real(DP) :: hyn, hym
    integer(I4B) :: n, m, ii, nn, ihc
    integer(I4B) :: nextn
    real(DP) :: minbot, botm
    integer(I4B), dimension(:), pointer, contiguous :: ithickstartflag
    ! -- format
    character(len=*), parameter :: fmtkerr =                                   &
      "(1x, 'Hydraulic property ',a,' is <= 0 for cell ',a, ' ', 1pg15.6)"
    character(len=*), parameter :: fmtkerr2 =                                  &
      "(1x, '... ', i0,' additional errors not shown for ',a)"
    character(len=*),parameter :: fmtcnv = &
    "(1X,'CELL ', A, &
     &' ELIMINATED BECAUSE ALL HYDRAULIC CONDUCTIVITIES TO NODE ARE 0.')"
    character(len=*),parameter :: fmtnct = &
    "(1X,'Negative cell thickness at cell ', A)"
    character(len=*),parameter :: fmtihbe = &
    "(1X,'Initial head, bottom elevation:',1P,2G13.5)"
    character(len=*),parameter :: fmttebe = &
    "(1X,'Top elevation, bottom elevation:',1P,2G13.5)"
! ------------------------------------------------------------------------------
    !
    ! -- initialize
    !aname => this%aname
    !
    ! -- check for wetdry conflicts
    !if(this%irewet == 1) then
    !  if(this%iwetdry == 0) then
    !    write(errmsg, '(a, a, a)') 'Error in GRIDDATA block: ',                  &
    !                                trim(adjustl(aname(5))), ' not found.'
    !    call store_error(errmsg)
    !  end if
    !endif
    !
    ! -- terminate if data errors
    if(count_errors() > 0) then
      call this%parser%StoreErrorUnit()
      call ustop()
    endif
    !
    ! -- Preprocess cell status and heads based on initial conditions
    !if (this%inewton == 0) then
      !
      ! -- For standard formulation (non-Newton) call wetdry routine
    !call this%wd(0, this%hnew)
    !else
      !
      ! -- Newton formulation, so adjust heads to be above bottom
      !    (Not used in present formulation because variable cv
      !    cannot be used with Newton)
    !  if (this%ivarcv == 1) then
    !    do n = 1, this%disl%nodes
    !      if (this%hnew(n) < this%disl%bot(n)) then
    !        this%hnew(n) = this%disl%bot(n) + DEM6
    !      end if
    !    end do
    !  end if
    !end if
    !
    ! -- Initialize sat to zero for ibound=0 cells, unless the cell can
    !    rewet.  Initialize sat to the saturated fraction based on strt
    !    if icelltype is negative and the THCKSTRT option is in effect.
    !    Initialize sat to 1.0 for all other cells in order to calculate
    !    condsat in next section.
    do n = 1, this%disl%nodes
      if(this%ibound(n) == 0) then
        this%sat(n) = DONE
      else
        !if(this%icelltype(n) < 0 .and. this%ithickstrt /= 0) then
          !call this%thksat(n, this%ic%strt(n), satn)
          !if(botn > this%ic%strt(n)) then
          !  call this%disl%noder_to_string(n, cellstr)
          !  write(errmsg, fmtnct) trim(adjustl(cellstr))
          !  call store_error(errmsg)
          !  write(errmsg, fmtihbe) this%ic%strt(n), botn
          !  call store_error(errmsg)
          !endif
          !ithickstartflag(n) = 1
          !this%icelltype(n) = 0
        !else
        satn = DONE
        this%sat(n) = satn
      endif
    enddo
    if(count_errors() > 0) then
      call this%parser%StoreErrorUnit()
      call ustop()
    endif
    !
    ! -- Calculate condsatu, but only if xt3d is not active.  If xt3d is
    !    active, then condsat is allocated to size of zero.
    !if (this%ixt3d == 0) then
    !
    ! -- Calculate the saturated conductance for all connections assuming
    !    that saturation is 1 (except for case where icelltype was entered
    !    as a negative value and THCKSTRT option in effect)
    do n = 1, this%disl%nodes
      !
      !topn = this%disl%top(n)
      !
      ! -- Go through the connecting cells
      do ii = this%disl%con%ia(n) + 1, this%disl%con%ia(n + 1) - 1
        !
        ! -- Set the m cell number and cycle if lower triangle connection
        m = this%disl%con%ja(ii)
        if (m < n) cycle
        !
        ! -- Conductance for fully saturated conditions
        csat = this%lcondsat(n, m)
        this%condsat(this%disl%con%jas(ii)) = csat
      enddo
    enddo
    !
    !-- nullify unneeded lnf pointers
    !this%ilnfnewtonur => null()
    !
    ! - clean up local storage
    !call mem_deallocate(ithickstartflag)
    !
    ! -- Return
    return
  end subroutine prepcheck

  function lcondsat(this, cell1, cell2)           &
                 result(condsatnm)
! ******************************************************************************
! lcondsat -- Saturated conductance between two cells
!
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use ConstantsModule,   only: LINELENGTH
    use SimModule, only: store_error, store_error_unit, ustop
    ! -- return
    real(DP) :: condsatnm
    ! -- dummy
    class(LnfNpfltype) :: this
    integer(I4B), intent(in) :: cell1
    integer(I4B), intent(in) :: cell2
    ! -- local
    integer(I4B) :: ii, cellnum1, cellnum2
    real(DP) :: area1, area2, cl1, cl2
    character(len=LINELENGTH) :: errmsg
    class(GeometryContainer), pointer :: geo
!  ------------------------------------------------------------------------------
    !
    ! -- If either n or m is inactive then conductance is zero
    if(this%ibound(cell1) == 0 .or. this%ibound(cell2) == 0) then
      condsatnm = DZERO
    else
      !
      ! -- calculate conductance
      ii = this%disl%iageom(cell1)
      ! geo => this%disl%jametries(ii)
      cellnum1 = this%disl%iageocellnum(cell1)
      cellnum2 = this%disl%iageocellnum(cell2)
      area1 = this%disl%jametries(this%disl%iageom(cell1))%obj%area_sat(cellnum1)
      area2 = this%disl%jametries(this%disl%iageom(cell2))%obj%area_sat(cellnum2)
      cl1 = 0.0
      outer: do ii = this%disl%con%ia(cell1) + 1, this%disl%con%ia(cell1 + 1) - 1
        if (this%disl%con%ja(ii) == cell2) then
          ! found connection between cell1 and cell2
          cl1 = this%disl%con%cl1(this%disl%con%jas(ii))
          cl2 = this%disl%con%cl2(this%disl%con%jas(ii))
          exit outer
        end if
      end do outer
      if (cl1 == 0.0) then
        write(errmsg,'(4x,a,i0,a,i0,a)')'****ERROR. NODES ', cell1, &
             ' AND ', cell2, ' ARE NOT CONNECTED.'
        call store_error(errmsg)
        call this%parser%StoreErrorUnit()
        call ustop()
      end if
      condsatnm = this%condmean(area1, area2, cl1, cl2, this%icellavg)
    end if
    !
    ! -- Return
    return
  end function lcondsat

  function lcond(ibdn, ibdm, ictn, ictm, inewton, condsat)           &
                 result(condnm)
! ******************************************************************************
! lcond -- Conductance between two cells
!
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
    real(DP), intent(in) :: condsat
    ! -- local
!  ------------------------------------------------------------------------------
    !if (present(satminopt)) then
    !  satmin = satminopt
    !else
    !  satmin = DZERO
    !end if
    !
    ! -- If either n or m is inactive then conductance is zero
    if(ibdn == 0 .or. ibdm == 0) then
      condnm = DZERO
    else
      !
      ! -- if both cells are non-convertible then use condsat
      condnm = condsat
    end if
    !
    ! -- Return
    return
  end function lcond

  function condmean(this, area1, area2, cl1, cl2, iavgmeth)
! ******************************************************************************
! condmean -- Calculate the conductance between two cells
!
!   area1 is the saturated thickness for cell 1
!   area2 is the saturated thickness for cell 2
!   cl1 is the distance from the center of cell1 to the shared face with cell2
!   cl2 is the distance from the center of cell2 to the shared face with cell1
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
    use ConstantsModule,   only: DPI
    ! -- return
    real(DP) :: condmean
    ! -- dummy
    class(LnfNpfltype) :: this
    real(DP), intent(in) :: area1
    real(DP), intent(in) :: area2
    real(DP), intent(in) :: cl1
    real(DP), intent(in) :: cl2
    integer(I4B), intent(in) :: iavgmeth
    ! -- local
    real(DP) :: t1
    real(DP) :: t2
    real(DP) :: tmean, amean, denom
! ------------------------------------------------------------------------------
    !
    ! -- Initialize
    t1 = (area1 * area1) / (8 * DPI * this%visc)
    t2 = (area2 * area2) / (8 * DPI * this%visc)
    !
    ! -- Averaging
    select case (iavgmeth)
    !
    ! -- Harmonic-mean method
    case(0)
      !
      if (t1*t2 > DZERO) then
        condmean = t1 * t2 / (t1 * cl2 + t2 * cl1)
      else
        condmean = DZERO
      end if
    !
    ! -- Logarithmic-mean method
    case(1)
      if (t1*t2 > DZERO) then
        tmean = logmean(t1, t2)
      else
        tmean = DZERO
      endif
      condmean = tmean / (cl1 + cl2)
    !
    ! -- Arithmetic-mean thickness and logarithmic-mean area
    case(2)
      if (area1*area2 > DZERO) then
        amean = logmean(area1, area2)
      else
        amean = DZERO
      endif
      condmean = (amean * amean) / (8 * DPI * this%visc * (cl1 + cl2))
    !
    ! -- Arithmetic-mean thickness and harmonic-mean area
    case(3)
      denom = (area1 * cl2 + area2 * cl1)
      if (denom > DZERO) then
        amean = area1 * area2 / denom
      else
        amean = DZERO
      end if
      condmean = (amean * amean) / (8 * DPI * this%visc)
    !
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
    if(drat <= DLNLOW .or. drat >= DLNHIGH) then
      logmean = (d2 - d1) / log(drat)
    else
      logmean = DHALF * (d1 + d2)
    endif
    !
    ! -- Return
    return
  end function logmean

  subroutine sav_spdis(this, ibinun)
! ******************************************************************************
! sav_spdis -- save specific discharge in binary format to ibinun
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(LnfNpfltype) :: this
    integer(I4B), intent(in) :: ibinun
    ! -- local
    character(len=16) :: text
    character(len=16), dimension(1) :: auxtxt
    integer(I4B) :: n
    integer(I4B) :: naux
! ------------------------------------------------------------------------------
    !
    ! -- Write the header
    text = '      DATA-SPDIS'
    naux = 3
    auxtxt(:) = ['              q']
    call this%disl%record_srcdst_list_header(text, this%name_model, this%name,  &
      this%name_model, this%name, naux, auxtxt, ibinun, this%disl%nodes,        &
      this%iout)
    !
    ! -- Write a zero for Q, and then write qx, qy, qz as aux variables
    do n = 1, this%disl%nodes
      call this%disl%record_mf6_list_entry(ibinun, n, n, DZERO, naux,           &
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
    class(LnfNpfltype) :: this
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
    call this%disl%record_srcdst_list_header(text, this%name_model, this%name,  &
      this%name_model, this%name, naux, auxtxt, ibinun, this%disl%nodes,        &
      this%iout)
    !
    ! -- Write a zero for Q, and then write saturation as an aux variables
    do n = 1, this%disl%nodes
      a(1) = this%sat(n)
      call this%disl%record_mf6_list_entry(ibinun, n, n, DZERO, naux, a)
    end do
    !
    ! -- return
    return
  end subroutine sav_sat

end module LnfNpflModule
