module GwtDspModule

  use KindModule, only: DP, I4B
  use ConstantsModule, only: DONE, DZERO, DHALF, DPI
  use NumericalPackageModule, only: NumericalPackageType
  use BaseDisModule, only: DisBaseType
  use GwtFmiModule, only: GwtFmiType
  use Xt3dModule, only: Xt3dType, xt3d_cr
  use GwtDspOptionsModule, only: GwtDspOptionsType
  use GwtDspGridDataModule, only: GwtDspGridDataType

  implicit none
  private
  public :: GwtDspType
  public :: dsp_cr

  type, extends(NumericalPackageType) :: GwtDspType

    integer(I4B), dimension(:), pointer, contiguous :: ibound => null() ! pointer to GWT model ibound
    type(GwtFmiType), pointer :: fmi => null() ! pointer to GWT fmi object
    real(DP), dimension(:), pointer, contiguous :: porosity => null() ! pointer to GWT storage porosity
    real(DP), dimension(:), pointer, contiguous :: diffc => null() ! molecular diffusion coefficient for each cell
    real(DP), dimension(:), pointer, contiguous :: alh => null() ! longitudinal horizontal dispersivity
    real(DP), dimension(:), pointer, contiguous :: alv => null() ! longitudinal vertical dispersivity
    real(DP), dimension(:), pointer, contiguous :: ath1 => null() ! transverse horizontal dispersivity
    real(DP), dimension(:), pointer, contiguous :: ath2 => null() ! transverse horizontal dispersivity
    real(DP), dimension(:), pointer, contiguous :: atv => null() ! transverse vertical dispersivity
    integer(I4B), pointer :: idiffc => null() ! flag indicating diffusion is active
    integer(I4B), pointer :: idisp => null() ! flag indicating mechanical dispersion is active
    integer(I4B), pointer :: ixt3d => null() ! flag indicating xt3d is active
    type(Xt3dType), pointer :: xt3d => null() ! xt3d object
    real(DP), dimension(:), pointer, contiguous :: dispcoef => null() ! disp coefficient (only if xt3d not active)
    integer(I4B), pointer :: id22 => null() ! flag indicating d22 is available
    integer(I4B), pointer :: id33 => null() ! flag indicating d33 is available
    real(DP), dimension(:), pointer, contiguous :: d11 => null() ! dispersion coefficient
    real(DP), dimension(:), pointer, contiguous :: d22 => null() ! dispersion coefficient
    real(DP), dimension(:), pointer, contiguous :: d33 => null() ! dispersion coefficient
    real(DP), dimension(:), pointer, contiguous :: angle1 => null() ! rotation angle 1
    real(DP), dimension(:), pointer, contiguous :: angle2 => null() ! rotation angle 2
    real(DP), dimension(:), pointer, contiguous :: angle3 => null() ! rotation angle 3
    integer(I4B), pointer :: iangle1 => null() ! flag indicating angle1 is available
    integer(I4B), pointer :: iangle2 => null() ! flag indicating angle2 is available
    integer(I4B), pointer :: iangle3 => null() ! flag indicating angle3 is available

  contains

    procedure :: dsp_df
    procedure :: dsp_ac
    procedure :: dsp_mc
    procedure :: dsp_ar
    procedure :: dsp_ad
    procedure :: dsp_fc
    procedure :: dsp_cq
    procedure :: dsp_da
    procedure :: allocate_scalars
    procedure :: allocate_arrays
    procedure, private :: read_options
    procedure, private :: read_data
    procedure, private :: set_data
    procedure, private :: calcdispellipse
    procedure, private :: calcdispcoef

  end type GwtDspType

contains

  subroutine dsp_cr(dspobj, name_model, inunit, iout, fmi)
! ******************************************************************************
! dsp_cr -- Create a new DSP object
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    type(GwtDspType), pointer :: dspobj
    character(len=*), intent(in) :: name_model
    integer(I4B), intent(in) :: inunit
    integer(I4B), intent(in) :: iout
    type(GwtFmiType), intent(in), target :: fmi
! ------------------------------------------------------------------------------
    !
    ! -- Create the object
    allocate (dspobj)
    !
    ! -- create name and memory path
    call dspobj%set_names(1, name_model, 'DSP', 'DSP')
    !
    ! -- Allocate scalars
    call dspobj%allocate_scalars()
    !
    ! -- Set variables
    dspobj%inunit = inunit
    dspobj%iout = iout
    dspobj%fmi => fmi
    !
    ! -- Return
    return
  end subroutine dsp_cr

  subroutine dsp_df(this, dis, dspOptions)
! ******************************************************************************
! dsp_df -- Allocate and Read
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(GwtDspType) :: this
    class(DisBaseType), pointer :: dis
    type(GwtDspOptionsType), optional, intent(in) :: dspOptions !< the optional DSP options, used when not
                                                                !! creating DSP from file
    ! -- local
    ! -- formats
    character(len=*), parameter :: fmtdsp = &
      "(1x,/1x,'DSP-- DISPERSION PACKAGE, VERSION 1, 1/24/2018', &
      &' INPUT READ FROM UNIT ', i0, //)"
! ------------------------------------------------------------------------------
    !
    ! -- Store pointer to dis
    this%dis => dis
    !
    !
    ! -- set default xt3d representation to on and lhs
    this%ixt3d = 1
    !
    ! -- Read dispersion options
    if (present(dspOptions)) then
      this%ixt3d = dspOptions%ixt3d
    else
      !
      ! -- Initialize block parser
      call this%parser%Initialize(this%inunit, this%iout)
      call this%read_options()
    end if
    !
    ! -- xt3d create
    if (this%ixt3d > 0) then
      call xt3d_cr(this%xt3d, this%name_model, this%inunit, this%iout, &
                   ldispopt=.true.)
      this%xt3d%ixt3d = this%ixt3d
      call this%xt3d%xt3d_df(dis)
    end if
    !
    ! -- Return
    return
  end subroutine dsp_df

  subroutine dsp_ac(this, moffset, sparse)
! ******************************************************************************
! dsp_ac -- Add connections for extended neighbors to the sparse matrix
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use SparseModule, only: sparsematrix
    use MemoryManagerModule, only: mem_allocate
    ! -- dummy
    class(GwtDspType) :: this
    integer(I4B), intent(in) :: moffset
    type(sparsematrix), intent(inout) :: sparse
    ! -- local
! ------------------------------------------------------------------------------
    !
    ! -- Add extended neighbors (neighbors of neighbors)
    if (this%ixt3d > 0) call this%xt3d%xt3d_ac(moffset, sparse)
    !
    ! -- Return
    return
  end subroutine dsp_ac

  subroutine dsp_mc(this, moffset, iasln, jasln)
! ******************************************************************************
! dsp_mc -- Map connections and construct iax, jax, and idxglox
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryManagerModule, only: mem_allocate
    ! -- dummy
    class(GwtDspType) :: this
    integer(I4B), intent(in) :: moffset
    integer(I4B), dimension(:), intent(in) :: iasln
    integer(I4B), dimension(:), intent(in) :: jasln
    ! -- local
! ------------------------------------------------------------------------------
    !
    ! -- Call xt3d map connections
    if (this%ixt3d > 0) call this%xt3d%xt3d_mc(moffset, iasln, jasln)
    !
    ! -- Return
    return
  end subroutine dsp_mc

  subroutine dsp_ar(this, ibound, porosity, grid_data)
! ******************************************************************************
! dsp_ar -- Allocate and Read
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(GwtDspType) :: this
    integer(I4B), dimension(:), pointer, contiguous :: ibound
    real(DP), dimension(:), pointer, contiguous :: porosity
    type(GwtDspGridDataType), optional, intent(in) :: grid_data !< optional data structure with DSP grid data,
                                                                !! to create the package without input file
    ! -- local
    ! -- formats
    character(len=*), parameter :: fmtdsp = &
      "(1x,/1x,'DSP-- DISPERSION PACKAGE, VERSION 1, 1/24/2018', &
      &' INPUT READ FROM UNIT ', i0, //)"
! ------------------------------------------------------------------------------
    !
    ! -- dsp pointers to arguments that were passed in
    this%ibound => ibound
    this%porosity => porosity
    !
    ! -- Print a message identifying the dispersion package.
    if (this%iout > 0) then
      write (this%iout, fmtdsp) this%inunit
    end if
    !
    ! -- Allocate arrays
    call this%allocate_arrays(this%dis%nodes)
    !
    if (present(grid_data)) then
      ! -- Set dispersion data
      call this%set_data(grid_data)
    else
      ! -- Read dispersion data
      call this%read_data()
    end if
    !
    ! -- Return
    return
  end subroutine dsp_ar

  subroutine dsp_ad(this)
! ******************************************************************************
! dsp_ad -- Advance
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use TdisModule, only: kstp, kper
    ! -- dummy
    class(GwtDspType) :: this
    ! -- local
! ------------------------------------------------------------------------------
    !
    ! -- xt3d
    ! TODO: might consider adding a new mf6 level set pointers method, and
    ! doing this stuff there instead of in the time step loop.
    if (kstp * kper == 1) then
      if (this%ixt3d > 0) then
        call this%xt3d%xt3d_ar(this%fmi%ibdgwfsat0, this%d11, this%id33, &
                               this%d33, this%fmi%gwfsat, this%id22, this%d22, &
                               this%iangle1, this%iangle2, this%iangle3, &
                               this%angle1, this%angle2, this%angle3)
      end if
    end if
    !
    ! -- Fill d11, d22, d33, angle1, angle2, angle3 using specific discharge
    call this%calcdispellipse()
    !
    ! -- Recalculate dispersion coefficients if the flows were updated
    if (this%fmi%iflowsupdated == 1) then
      if (this%ixt3d == 0) then
        call this%calcdispcoef()
      else if (this%ixt3d > 0) then
        call this%xt3d%xt3d_fcpc(this%dis%nodes, .false.)
      end if
    end if
    !
    ! -- Return
    return
  end subroutine dsp_ad

  subroutine dsp_fc(this, kiter, nodes, nja, njasln, amatsln, idxglo, rhs, cnew)
! ******************************************************************************
! dsp_fc -- Calculate coefficients and fill amat and rhs
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(GwtDspType) :: this
    integer(I4B) :: kiter
    integer(I4B), intent(in) :: nodes
    integer(I4B), intent(in) :: nja
    integer(I4B), intent(in) :: njasln
    real(DP), dimension(njasln), intent(inout) :: amatsln
    integer(I4B), intent(in), dimension(nja) :: idxglo
    real(DP), intent(inout), dimension(nodes) :: rhs
    real(DP), intent(inout), dimension(nodes) :: cnew
    ! -- local
    integer(I4B) :: n, m, idiag, idiagm, ipos, isympos, isymcon
    real(DP) :: dnm
! ------------------------------------------------------------------------------
    !
    if (this%ixt3d > 0) then
      call this%xt3d%xt3d_fc(kiter, njasln, amatsln, idxglo, rhs, cnew)
    else
      do n = 1, nodes
        if (this%fmi%ibdgwfsat0(n) == 0) cycle
        idiag = this%dis%con%ia(n)
        do ipos = this%dis%con%ia(n) + 1, this%dis%con%ia(n + 1) - 1
          if (this%dis%con%mask(ipos) == 0) cycle
          m = this%dis%con%ja(ipos)
          if (m < n) cycle
          if (this%fmi%ibdgwfsat0(m) == 0) cycle
          isympos = this%dis%con%jas(ipos)
          dnm = this%dispcoef(isympos)
          !
          ! -- Contribution to row n
          amatsln(idxglo(ipos)) = amatsln(idxglo(ipos)) + dnm
          amatsln(idxglo(idiag)) = amatsln(idxglo(idiag)) - dnm
          !
          ! -- Contribution to row m
          idiagm = this%dis%con%ia(m)
          isymcon = this%dis%con%isym(ipos)
          amatsln(idxglo(isymcon)) = amatsln(idxglo(isymcon)) + dnm
          amatsln(idxglo(idiagm)) = amatsln(idxglo(idiagm)) - dnm
        end do
      end do
    end if
    !
    ! -- Return
    return
  end subroutine dsp_fc

  subroutine dsp_cq(this, cnew, flowja)
! ******************************************************************************
! dsp_cq -- Calculate dispersion contribution to flowja
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(GwtDspType) :: this
    real(DP), intent(inout), dimension(:) :: cnew
    real(DP), intent(inout), dimension(:) :: flowja
    ! -- local
    integer(I4B) :: n, m, ipos, isympos
    real(DP) :: dnm
! ------------------------------------------------------------------------------
    !
    ! -- Calculate dispersion and add to flowja
    if (this%ixt3d > 0) then
      call this%xt3d%xt3d_flowja(cnew, flowja)
    else
      do n = 1, this%dis%nodes
        if (this%fmi%ibdgwfsat0(n) == 0) cycle
        do ipos = this%dis%con%ia(n) + 1, this%dis%con%ia(n + 1) - 1
          m = this%dis%con%ja(ipos)
          if (this%fmi%ibdgwfsat0(m) == 0) cycle
          isympos = this%dis%con%jas(ipos)
          dnm = this%dispcoef(isympos)
          flowja(ipos) = flowja(ipos) + dnm * (cnew(m) - cnew(n))
        end do
      end do
    end if
    !
    ! -- Return
    return
  end subroutine dsp_cq

  subroutine allocate_scalars(this)
! ******************************************************************************
! allocate_scalars
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryManagerModule, only: mem_allocate
    use ConstantsModule, only: DZERO
    ! -- dummy
    class(GwtDspType) :: this
    ! -- local
! ------------------------------------------------------------------------------
    !
    ! -- allocate scalars in NumericalPackageType
    call this%NumericalPackageType%allocate_scalars()
    !
    ! -- Allocate
    call mem_allocate(this%idiffc, 'IDIFFC', this%memoryPath)
    call mem_allocate(this%idisp, 'IDISP', this%memoryPath)
    call mem_allocate(this%ixt3d, 'IXT3D', this%memoryPath)
    call mem_allocate(this%id22, 'ID22', this%memoryPath)
    call mem_allocate(this%id33, 'ID33', this%memoryPath)
    call mem_allocate(this%iangle1, 'IANGLE1', this%memoryPath)
    call mem_allocate(this%iangle2, 'IANGLE2', this%memoryPath)
    call mem_allocate(this%iangle3, 'IANGLE3', this%memoryPath)
    !
    ! -- Initialize
    this%idiffc = 0
    this%idisp = 0
    this%ixt3d = 0
    this%id22 = 1
    this%id33 = 1
    this%iangle1 = 1
    this%iangle2 = 1
    this%iangle3 = 1
    !
    ! -- Return
    return
  end subroutine allocate_scalars

  subroutine allocate_arrays(this, nodes)
! ******************************************************************************
! allocate_arrays
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryManagerModule, only: mem_allocate
    use ConstantsModule, only: DZERO
    ! -- dummy
    class(GwtDspType) :: this
    integer(I4B), intent(in) :: nodes
    ! -- local
! ------------------------------------------------------------------------------
    !
    ! -- Allocate
    call mem_allocate(this%alh, 0, 'ALH', trim(this%memoryPath))
    call mem_allocate(this%alv, 0, 'ALV', trim(this%memoryPath))
    call mem_allocate(this%ath1, 0, 'ATH1', trim(this%memoryPath))
    call mem_allocate(this%ath2, 0, 'ATH2', trim(this%memoryPath))
    call mem_allocate(this%atv, 0, 'ATV', trim(this%memoryPath))
    call mem_allocate(this%diffc, 0, 'DIFFC', trim(this%memoryPath))
    call mem_allocate(this%d11, nodes, 'D11', trim(this%memoryPath))
    call mem_allocate(this%d22, nodes, 'D22', trim(this%memoryPath))
    call mem_allocate(this%d33, nodes, 'D33', trim(this%memoryPath))
    call mem_allocate(this%angle1, nodes, 'ANGLE1', trim(this%memoryPath))
    call mem_allocate(this%angle2, nodes, 'ANGLE2', trim(this%memoryPath))
    call mem_allocate(this%angle3, nodes, 'ANGLE3', trim(this%memoryPath))
    !
    ! -- Allocate dispersion coefficient array if xt3d not in use
    if (this%ixt3d == 0) then
      call mem_allocate(this%dispcoef, this%dis%njas, 'DISPCOEF', &
                        trim(this%memoryPath))
    else
      call mem_allocate(this%dispcoef, 0, 'DISPCOEF', trim(this%memoryPath))
    end if
    !
    ! -- Return
    return
  end subroutine allocate_arrays

  subroutine dsp_da(this)
! ******************************************************************************
! dsp_da
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryManagerModule, only: mem_deallocate
    ! -- dummy
    class(GwtDspType) :: this
    ! -- local
! ------------------------------------------------------------------------------
    !
    ! -- deallocate arrays
    if (this%inunit /= 0) then
      call mem_deallocate(this%alh)
      call mem_deallocate(this%alv, 'ALV', trim(this%memoryPath))
      call mem_deallocate(this%ath1)
      call mem_deallocate(this%ath2, 'ATH2', trim(this%memoryPath))
      call mem_deallocate(this%atv, 'ATV', trim(this%memoryPath))
      call mem_deallocate(this%diffc)
      call mem_deallocate(this%d11)
      call mem_deallocate(this%d22)
      call mem_deallocate(this%d33)
      call mem_deallocate(this%angle1)
      call mem_deallocate(this%angle2)
      call mem_deallocate(this%angle3)
      call mem_deallocate(this%dispcoef)
      if (this%ixt3d > 0) call this%xt3d%xt3d_da()
    end if
    !
    ! -- deallocate objects
    if (this%ixt3d > 0) deallocate (this%xt3d)
    !
    ! -- deallocate scalars
    call mem_deallocate(this%idiffc)
    call mem_deallocate(this%idisp)
    call mem_deallocate(this%ixt3d)
    call mem_deallocate(this%id22)
    call mem_deallocate(this%id33)
    call mem_deallocate(this%iangle1)
    call mem_deallocate(this%iangle2)
    call mem_deallocate(this%iangle3)
    !
    ! -- deallocate variables in NumericalPackageType
    call this%NumericalPackageType%da()
    !
    ! -- Return
    return
  end subroutine dsp_da

  subroutine read_options(this)
! ******************************************************************************
! read_options -- Allocate and Read
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use ConstantsModule, only: LINELENGTH
    use SimModule, only: store_error
    ! -- dummy
    class(GwtDspType) :: this
    ! -- local
    character(len=LINELENGTH) :: errmsg, keyword
    integer(I4B) :: ierr
    logical :: isfound, endOfBlock
    ! -- formats
! ------------------------------------------------------------------------------
    !
    ! -- get options block
    call this%parser%GetBlock('OPTIONS', isfound, ierr, blockRequired=.false., &
                              supportOpenClose=.true.)
    !
    ! -- parse options block if detected
    if (isfound) then
      write (this%iout, '(1x,a)') 'PROCESSING DISPERSION OPTIONS'
      do
        call this%parser%GetNextLine(endOfBlock)
        if (endOfBlock) exit
        call this%parser%GetStringCaps(keyword)
        select case (keyword)
        case ('XT3D_OFF')
          this%ixt3d = 0
          write (this%iout, '(4x,a)') &
            'XT3D FORMULATION HAS BEEN SHUT OFF.'
        case ('XT3D_RHS')
          this%ixt3d = 2
          write (this%iout, '(4x,a)') &
            'XT3D RIGHT-HAND SIDE FORMULATION IS SELECTED.'
        case default
          write (errmsg, '(4x,a,a)') 'UNKNOWN DISPERSION OPTION: ', &
            trim(keyword)
          call store_error(errmsg, terminate=.TRUE.)
        end select
      end do
      write (this%iout, '(1x,a)') 'END OF DISPERSION OPTIONS'
    end if
    !
    ! -- Return
    return
  end subroutine read_options

  subroutine read_data(this)
! ******************************************************************************
! read_data -- read the dispersion data
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use ConstantsModule, only: LINELENGTH
    use SimModule, only: store_error, count_errors
    use MemoryManagerModule, only: mem_reallocate, mem_copyptr, mem_reassignptr
    ! -- dummy
    class(GwtDsptype) :: this
    ! -- local
    character(len=LINELENGTH) :: errmsg, keyword
    character(len=:), allocatable :: line
    integer(I4B) :: istart, istop, lloc, ierr
    logical :: isfound, endOfBlock
    logical, dimension(6) :: lname
    character(len=24), dimension(6) :: aname
    ! -- formats
    ! -- data
    data aname(1)/'   DIFFUSION COEFFICIENT'/
    data aname(2)/'                     ALH'/
    data aname(3)/'                     ALV'/
    data aname(4)/'                    ATH1'/
    data aname(5)/'                    ATH2'/
    data aname(6)/'                     ATV'/
! ------------------------------------------------------------------------------
    !
    ! -- initialize
    lname(:) = .false.
    isfound = .false.
    !
    ! -- get griddata block
    call this%parser%GetBlock('GRIDDATA', isfound, ierr)
    if (isfound) then
      write (this%iout, '(1x,a)') 'PROCESSING GRIDDATA'
      do
        call this%parser%GetNextLine(endOfBlock)
        if (endOfBlock) exit
        call this%parser%GetStringCaps(keyword)
        call this%parser%GetRemainingLine(line)
        lloc = 1
        select case (keyword)
        case ('DIFFC')
          call mem_reallocate(this%diffc, this%dis%nodes, 'DIFFC', &
                              trim(this%memoryPath))
          call this%dis%read_grid_array(line, lloc, istart, istop, this%iout, &
                                        this%parser%iuactive, this%diffc, &
                                        aname(1))
          lname(1) = .true.
        case ('ALH')
          call mem_reallocate(this%alh, this%dis%nodes, 'ALH', &
                              trim(this%memoryPath))
          call this%dis%read_grid_array(line, lloc, istart, istop, this%iout, &
                                        this%parser%iuactive, this%alh, &
                                        aname(2))
          lname(2) = .true.
        case ('ALV')
          call mem_reallocate(this%alv, this%dis%nodes, 'ALV', &
                              trim(this%memoryPath))
          call this%dis%read_grid_array(line, lloc, istart, istop, this%iout, &
                                        this%parser%iuactive, this%alv, &
                                        aname(3))
          lname(3) = .true.
        case ('ATH1')
          call mem_reallocate(this%ath1, this%dis%nodes, 'ATH1', &
                              trim(this%memoryPath))
          call this%dis%read_grid_array(line, lloc, istart, istop, this%iout, &
                                        this%parser%iuactive, this%ath1, &
                                        aname(4))
          lname(4) = .true.
        case ('ATH2')
          call mem_reallocate(this%ath2, this%dis%nodes, 'ATH2', &
                              trim(this%memoryPath))
          call this%dis%read_grid_array(line, lloc, istart, istop, this%iout, &
                                        this%parser%iuactive, this%ath2, &
                                        aname(5))
          lname(5) = .true.
        case ('ATV')
          call mem_reallocate(this%atv, this%dis%nodes, 'ATV', &
                              trim(this%memoryPath))
          call this%dis%read_grid_array(line, lloc, istart, istop, this%iout, &
                                        this%parser%iuactive, this%atv, &
                                        aname(6))
          lname(6) = .true.
        case default
          write (errmsg, '(4x,a,a)') 'Unknown GRIDDATA tag: ', trim(keyword)
          call store_error(errmsg)
          call this%parser%StoreErrorUnit()
        end select
      end do
      write (this%iout, '(1x,a)') 'END PROCESSING GRIDDATA'
    else
      write (errmsg, '(1x,a)') 'Required GRIDDATA block not found.'
      call store_error(errmsg)
      call this%parser%StoreErrorUnit()
    end if
    !
    if (lname(1)) this%idiffc = 1
    if (lname(2)) this%idisp = this%idisp + 1
    if (lname(3)) this%idisp = this%idisp + 1
    if (lname(4)) this%idisp = this%idisp + 1
    if (lname(5)) this%idisp = this%idisp + 1
    !
    ! -- if dispersivities are specified, then both alh and ath1 must be included
    if (this%idisp > 0) then
      !
      ! -- make sure alh was specified
      if (.not. lname(2)) then
        write (errmsg, '(1x,a)') &
          'IF DISPERSIVITIES ARE SPECIFIED THEN ALH IS REQUIRED.'
        call store_error(errmsg)
      end if
      !
      ! -- make sure ath1 was specified
      if (.not. lname(4)) then
        write (errmsg, '(1x,a)') &
          'IF DISPERSIVITIES ARE SPECIFIED THEN ATH1 IS REQUIRED.'
        call store_error(errmsg)
      end if
      !
      ! -- If alv not specified then point it to alh
      if (.not. lname(3)) then
        call mem_reassignptr(this%alv, 'ALV', trim(this%memoryPath), &
                             'ALH', trim(this%memoryPath))
      end if
      !
      ! -- If ath2 not specified then assign it to ath1
      if (.not. lname(5)) then
        call mem_reassignptr(this%ath2, 'ATH2', trim(this%memoryPath), &
                             'ATH1', trim(this%memoryPath))
      end if
      !
      ! -- If atv not specified then assign it to ath2
      if (.not. lname(6)) then
        call mem_reassignptr(this%atv, 'ATV', trim(this%memoryPath), &
                             'ATH2', trim(this%memoryPath))
      end if
    end if
    !
    ! -- terminate if errors
    if (count_errors() > 0) then
      call this%parser%StoreErrorUnit()
    end if
    !
    ! -- Return
    return
  end subroutine read_data

  !< @brief Set the grid data to the package
  !<
  subroutine set_data(this, grid_data)
    use MemoryManagerModule, only: mem_reallocate
    class(GwtDspType) :: this !< this DSP package
    type(GwtDspGridDataType), intent(in) :: grid_data !< the data structure with DSP grid data
    ! local
    integer(I4B) :: i

    call mem_reallocate(this%diffc, this%dis%nodes, 'DIFFC', &
                        trim(this%memoryPath))
    call mem_reallocate(this%alh, this%dis%nodes, 'ALH', &
                        trim(this%memoryPath))
    call mem_reallocate(this%alv, this%dis%nodes, 'ALV', &
                        trim(this%memoryPath))
    call mem_reallocate(this%ath1, this%dis%nodes, 'ATH1', &
                        trim(this%memoryPath))
    call mem_reallocate(this%ath2, this%dis%nodes, 'ATH2', &
                        trim(this%memoryPath))
    call mem_reallocate(this%atv, this%dis%nodes, 'ATV', &
                        trim(this%memoryPath))

    do i = 1, this%dis%nodes
      this%diffc(i) = grid_data%diffc(i)
      this%alh(i) = grid_data%alh(i)
      this%alv(i) = grid_data%alv(i)
      this%ath1(i) = grid_data%ath1(i)
      this%ath2(i) = grid_data%ath2(i)
      this%atv(i) = grid_data%atv(i)
    end do

  end subroutine

  subroutine calcdispellipse(this)
! ******************************************************************************
! calcdispellipse -- Calculate dispersion coefficients
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(GwtDspType) :: this
    ! -- local
    integer(I4B) :: nodes, n
    real(DP) :: q, qx, qy, qz
    real(DP) :: alh, alv, ath1, ath2, atv, a
    real(DP) :: al, at1, at2
    real(DP) :: qzoqsquared
    real(DP) :: dstar
! ------------------------------------------------------------------------------
    !
    ! -- loop through and calculate dispersion coefficients and angles
    nodes = size(this%d11)
    do n = 1, nodes
      !
      ! -- initialize
      this%d11(n) = DZERO
      this%d22(n) = DZERO
      this%d33(n) = DZERO
      this%angle1(n) = DZERO
      this%angle2(n) = DZERO
      this%angle3(n) = DZERO
      if (this%fmi%ibdgwfsat0(n) == 0) cycle
      !
      ! -- specific discharge
      qx = DZERO
      qy = DZERO
      qz = DZERO
      q = DZERO
      qx = this%fmi%gwfspdis(1, n)
      qy = this%fmi%gwfspdis(2, n)
      qz = this%fmi%gwfspdis(3, n)
      q = qx**2 + qy**2 + qz**2
      if (q > DZERO) q = sqrt(q)
      !
      ! -- dispersion coefficients
      alh = DZERO
      alv = DZERO
      ath1 = DZERO
      ath2 = DZERO
      atv = DZERO
      if (this%idisp > 0) then
        alh = this%alh(n)
        alv = this%alv(n)
        ath1 = this%ath1(n)
        ath2 = this%ath2(n)
        atv = this%atv(n)
      end if
      dstar = DZERO
      if (this%idiffc > 0) then
        dstar = this%diffc(n) * this%porosity(n)
      end if
      !
      ! -- Calculate the longitudal and transverse dispersivities
      al = DZERO
      at1 = DZERO
      at2 = DZERO
      if (q > DZERO) then
        qzoqsquared = (qz / q)**2
        al = alh * (DONE - qzoqsquared) + alv * qzoqsquared
        at1 = ath1 * (DONE - qzoqsquared) + atv * qzoqsquared
        at2 = ath2 * (DONE - qzoqsquared) + atv * qzoqsquared
      end if
      !
      ! -- Calculate and save the diagonal components of the dispersion tensor
      this%d11(n) = al * q + dstar
      this%d22(n) = at1 * q + dstar
      this%d33(n) = at2 * q + dstar
      !
      ! -- Angles of rotation if velocity based dispersion tensor
      if (this%idisp > 0) then
        !
        ! -- angles of rotation from model coordinates to direction of velocity
        ! qx / q = cos(a1) * cos(a2)
        ! qy / q = sin(a1) * cos(a2)
        ! qz / q = sin(a2)
        !
        ! -- angle3 is zero
        this%angle3(n) = DZERO
        !
        ! -- angle2
        a = DZERO
        if (q > DZERO) a = qz / q
        this%angle2(n) = asin(a)
        !
        ! -- angle1
        a = q * cos(this%angle2(n))
        if (a /= DZERO) then
          a = qx / a
        else
          a = DZERO
        end if
        !
        ! -- acos(1) not defined, so set to zero if necessary
        if (a <= -DONE) then
          this%angle1(n) = DPI
        elseif (a >= DONE) then
          this%angle1(n) = DZERO
        else
          this%angle1(n) = acos(a)
        end if
        !
      end if
    end do
    !
    ! -- Return
    return
  end subroutine calcdispellipse

  subroutine calcdispcoef(this)
! ******************************************************************************
! calcdispcoef -- Calculate dispersion coefficients
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use GwfNpfModule, only: hyeff_calc
    ! -- dummy
    class(GwtDspType) :: this
    ! -- local
    integer(I4B) :: nodes, n, m, idiag, ipos
    real(DP) :: clnm, clmn, dn, dm
    real(DP) :: vg1, vg2, vg3
    integer(I4B) :: ihc, isympos
    integer(I4B) :: iavgmeth
    real(DP) :: satn, satm, topn, topm, botn, botm
    real(DP) :: hwva, cond, cn, cm, denom
    real(DP) :: anm, amn, thksatn, thksatm, sill_top, sill_bot, tpn, tpm
! ------------------------------------------------------------------------------
    !
    ! -- set iavgmeth = 1 to use arithmetic averaging for effective dispersion
    iavgmeth = 1
    !
    ! -- Proces connections
    nodes = size(this%d11)
    do n = 1, nodes
      if (this%fmi%ibdgwfsat0(n) == 0) cycle
      idiag = this%dis%con%ia(n)
      do ipos = this%dis%con%ia(n) + 1, this%dis%con%ia(n + 1) - 1
        !
        ! -- Set m to connected cell
        m = this%dis%con%ja(ipos)
        !
        ! -- skip for lower triangle
        if (m < n) cycle
        isympos = this%dis%con%jas(ipos)
        this%dispcoef(isympos) = DZERO
        if (this%fmi%ibdgwfsat0(m) == 0) cycle
        !
        ! -- cell dimensions
        hwva = this%dis%con%hwva(isympos)
        clnm = this%dis%con%cl1(isympos)
        clmn = this%dis%con%cl2(isympos)
        ihc = this%dis%con%ihc(isympos)
        topn = this%dis%top(n)
        topm = this%dis%top(m)
        botn = this%dis%bot(n)
        botm = this%dis%bot(m)
        !
        ! -- flow model information
        satn = this%fmi%gwfsat(n)
        satm = this%fmi%gwfsat(m)
        !
        ! -- Calculate dispersion coefficient for cell n in the direction
        !    normal to the shared n-m face and for cell m in the direction
        !    normal to the shared n-m face.
        call this%dis%connection_normal(n, m, ihc, vg1, vg2, vg3, ipos)
        dn = hyeff_calc(this%d11(n), this%d22(n), this%d33(n), &
                        this%angle1(n), this%angle2(n), this%angle3(n), &
                        vg1, vg2, vg3, iavgmeth)
        dm = hyeff_calc(this%d11(m), this%d22(m), this%d33(m), &
                        this%angle1(m), this%angle2(m), this%angle3(m), &
                        vg1, vg2, vg3, iavgmeth)
        !
        ! -- Calculate dispersion conductance based on NPF subroutines and the
        !    effective dispersion coefficients dn and dm.
        if (ihc == 0) then
          clnm = satn * (topn - botn) * DHALF
          clmn = satm * (topm - botm) * DHALF
          anm = hwva
          !
          ! -- n is convertible and unsaturated
          if (satn == DZERO) then
            anm = DZERO
          else if (n > m .and. satn < DONE) then
            anm = DZERO
          end if
          !
          ! -- m is convertible and unsaturated
          if (satm == DZERO) then
            anm = DZERO
          else if (m > n .and. satm < DONE) then
            anm = DZERO
          end if
          !
          ! -- amn is the same as anm for vertical flow
          amn = anm
          !
        else
          !
          ! -- horizontal conductance
          thksatn = (topn - botn) * satn
          thksatm = (topm - botm) * satm
          !
          ! -- handle vertically staggered case
          if (ihc == 2) then
            sill_top = min(topn, topm)
            sill_bot = max(botn, botm)
            tpn = botn + thksatn
            tpm = botm + thksatm
            thksatn = max(min(tpn, sill_top) - sill_bot, DZERO)
            thksatm = max(min(tpm, sill_top) - sill_bot, DZERO)
          end if
          !
          ! -- calculate the saturated area term
          anm = thksatn * hwva
          amn = thksatm * hwva
          !
          ! -- n or m is unsaturated, so no dispersion
          if (satn == DZERO .or. satm == DZERO) then
            anm = DZERO
            amn = DZERO
          end if
          !
        end if
        !
        ! -- calculate conductance using the two half cell conductances
        cn = DZERO
        if (clnm > DZERO) cn = dn * anm / clnm
        cm = DZERO
        if (clmn > DZERO) cm = dm * amn / clmn
        denom = cn + cm
        if (denom > DZERO) then
          cond = cn * cm / denom
        else
          cond = DZERO
        end if
        !
        ! -- Assign the calculated dispersion conductance
        this%dispcoef(isympos) = cond
        !
      end do
    end do
    !
    ! -- Return
    return
  end subroutine calcdispcoef

end module GwtDspModule
