module GwtDspModule

  use KindModule,             only: DP, I4B, LGP
  use ConstantsModule,        only: DONE, DZERO, DHALF, DPI
  use NumericalPackageModule, only: NumericalPackageType
  use BaseDisModule,          only: DisBaseType
  use GwtFmiModule,           only: GwtFmiType
  use Xt3dModule,             only: Xt3dType, xt3d_cr

  implicit none
  private
  public :: GwtDspType
  public :: dsp_cr

  type, extends(NumericalPackageType) :: GwtDspType
    
    integer(I4B), dimension(:), pointer, contiguous  :: ibound     => null()    ! pointer to GWT model ibound
    type(GwtFmiType), pointer                        :: fmi        => null()    ! pointer to GWT fmi object
    real(DP), dimension(:), pointer, contiguous      :: porosity   => null()    ! pointer to GWT storage porosity
    real(DP), dimension(:), pointer, contiguous      :: diffc      => null()    ! molecular diffusion coefficient for each cell
    real(DP), dimension(:), pointer, contiguous      :: alh        => null()    ! longitudinal horizontal dispersivity
    real(DP), dimension(:), pointer, contiguous      :: alv        => null()    ! longitudinal vertical dispersivity
    real(DP), dimension(:), pointer, contiguous      :: ath1       => null()    ! transverse horizontal dispersivity
    real(DP), dimension(:), pointer, contiguous      :: ath2       => null()    ! transverse horizontal dispersivity
    real(DP), dimension(:), pointer, contiguous      :: atv        => null()    ! transverse vertical dispersivity
    integer(I4B), pointer                            :: idiffc     => null()    ! flag indicating diffusion is active
    integer(I4B), pointer                            :: idisp      => null()    ! flag indicating mechanical dispersion is active
    integer(I4B), pointer                            :: ixt3d      => null()    ! flag indicating xt3d is active
    type(Xt3dType), pointer                          :: xt3d       => null()    ! xt3d object
    real(DP), dimension(:), pointer, contiguous      :: dispcoef   => null()    ! disp coefficient (only if xt3d not active)
    integer(I4B), pointer                            :: id22       => null()    ! flag indicating d22 is available
    integer(I4B), pointer                            :: id33       => null()    ! flag indicating d33 is available
    real(DP), dimension(:), pointer, contiguous      :: d11        => null()    ! dispersion coefficient
    real(DP), dimension(:), pointer, contiguous      :: d22        => null()    ! dispersion coefficient
    real(DP), dimension(:), pointer, contiguous      :: d33        => null()    ! dispersion coefficient
    real(DP), dimension(:), pointer, contiguous      :: angle1     => null()    ! rotation angle 1
    real(DP), dimension(:), pointer, contiguous      :: angle2     => null()    ! rotation angle 2
    real(DP), dimension(:), pointer, contiguous      :: angle3     => null()    ! rotation angle 3
    integer(I4B), pointer                            :: iangle1    => null()    ! flag indicating angle1 is available
    integer(I4B), pointer                            :: iangle2    => null()    ! flag indicating angle2 is available
    integer(I4B), pointer                            :: iangle3    => null()    ! flag indicating angle3 is available
    real(DP), dimension(:), pointer, contiguous      :: gwfflowjaold => null()  ! gwf flowja values last time disp coeffs were calculated
    logical(LGP), pointer                            :: xt3dbyconn => null()    ! flag to indicate whether application of xt3d is specified by connection
    logical(LGP), pointer                            :: allnonstdf => null()    ! flag to indicate whether all connections are using a non-standard dispersion formulation
    integer(I4B), dimension(:), pointer, contiguous  :: idispform  => null()    ! flag to indicate use of non-standard dispersion formulations by connection
    integer(I4B), pointer                            :: iprxt3d    => null()    ! print (1) or do not print (0) xt3d connections in XT3DDATA block
    integer(I4B), pointer                            :: inonstdf   => null()    ! non-standard dispersion formulation flag is (0) if standard only and (1) if any non-standard
    integer(I4B), pointer                            :: nbrmax     => null()    ! maximum number of standard neighbors for any cell
    
  contains
  
    procedure :: dsp_df
    procedure :: dsp_ac
    procedure :: dsp_mc
    procedure :: dsp_ar
    procedure :: dsp_ad
    procedure :: dsp_cf
    procedure :: dsp_fc
    procedure :: dsp_flowja
    procedure :: dsp_da
    procedure :: allocate_scalars
    procedure :: allocate_arrays
    procedure, private :: read_options
    procedure, private :: read_data
    procedure, private :: read_xt3d_data   ! amp_note: currently separate but identical read_xt3d_data procedures in NPF and DSP - just have one in XT3D module if always the same???
    procedure, private :: calcdispellipse
    procedure, private :: calcdispcoef
    procedure, private :: stddisp_fc
   
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
    allocate(dspobj)
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
    ! -- Initialize block parser
    call dspobj%parser%Initialize(dspobj%inunit, dspobj%iout)
    !
    ! -- Return
    return
  end subroutine dsp_cr

  subroutine dsp_df(this, dis)
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
    ! -- local
    character(len=*), parameter :: fmtdsp =                                    &
      "(1x,/1x,'DSP-- DISPERSION PACKAGE, VERSION 1, 1/24/2018',               &
      &' INPUT READ FROM UNIT ', i0, //)"
! ------------------------------------------------------------------------------
    !
    ! -- Store pointer to dis
    this%dis => dis
    !
    ! -- Read dispersion options
    call this%read_options()
    !
    ! -- xt3d create
    if(this%ixt3d > 0) then
      call xt3d_cr(this%xt3d, this%name_model, this%inunit, this%iout,       &
                   ldispopt=.true.)
      this%xt3d%ixt3d = this%ixt3d
    endif
    !
    ! -- If xt3d active:
    !    Temporarily allocate and initialize idispform array. Read the xt3d
    !    data block if necessary to set idispform, otherwise set it to 1.    ! amp_note: doing this here because we need to know idispform array before call to _ac
    !    Call xt3d_df.
    if (this%ixt3d /= 0) then
      allocate(this%idispform(this%dis%njas))
      this%idispform = 0
      if (this%xt3dbyconn /= 0) then
        call this%read_xt3d_data(.true.)
      else
        this%idispform = 1
      end if
      call this%xt3d%xt3d_df(dis,this%idispform)
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
    if(this%ixt3d > 0) call this%xt3d%xt3d_mc(moffset, iasln, jasln)
    !
    ! -- Return
    return
  end subroutine dsp_mc

  subroutine dsp_ar(this, ibound, porosity)
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
    ! -- local
    integer(I4B) :: i, nnbrs, nbrxmax
    ! -- formats
    character(len=*), parameter :: fmtdsp =                                    &
      "(1x,/1x,'DSP-- DISPERSION PACKAGE, VERSION 1, 1/24/2018',               &
      &' INPUT READ FROM UNIT ', i0, //)"
! ------------------------------------------------------------------------------
    !
    ! -- dsp pointers to arguments that were passed in
    this%ibound  => ibound
    this%porosity => porosity
    !
    ! -- Print a message identifying the dispersion package.
    write(this%iout, fmtdsp) this%inunit
    !
    ! -- Determine the maximum number of standard neighbors
    !    for any cell (excluding self)
    this%nbrmax = 0
    do i = 1, this%dis%nodes
      nnbrs = this%dis%con%ia(i+1) - this%dis%con%ia(i) - 1
      this%nbrmax = max(nnbrs, this%nbrmax)
    end do
    !
    ! -- Allocate arrays
    call this%allocate_arrays(this%dis%nodes)
    !
    ! -- Read dispersion data
    call this%read_data()
    !
    ! -- Initialize the idispform and allnonstdf flags
    this%idispform = 0
    this%allnonstdf = .false.
    ! -- If xt3d active:
    !    Read the xt3d data block if necessary to set idispform, otherwise
    !    set it to 1. Set allnonstdf flag to .true. if all connections use XT3D.
    if (this%ixt3d /= 0) then
      if (this%xt3dbyconn /= 0) then
        call this%read_xt3d_data(.false.)
        this%allnonstdf = all(this%idispform /= 0)
      else
        this%idispform = 1
        this%allnonstdf = .true.
      end if
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
      if(this%ixt3d > 0) call this%xt3d%xt3d_ar(this%fmi%ibdgwfsat0,           &
        this%d11, this%id33, this%d33, this%fmi%gwfsat, this%id22, this%d22,   &
        this%iangle1, this%iangle2, this%iangle3,                              &
        this%angle1, this%angle2, this%angle3,                                 &
        this%idispform, this%nbrmax)
    endif
    !
    ! -- Fill d11, d22, d33, angle1, angle2, angle3 using specific discharge
    call this%calcdispellipse()
    !
    ! -- Recalculate dispersion coefficients only where the standard dispersion
    !    formulation is in use.  If a non-standard dispersion formulation is in
    !     use at all connections, dispcoef is allocated to size of zero.  ! amp_note: make it like condsat???
    if (.not.this%allnonstdf) then
      call this%calcdispcoef()
    endif
    !
    ! -- Return
    return
  end subroutine dsp_ad

  subroutine dsp_cf(this, kiter)
! ******************************************************************************
! dsp_cf --Coefficients
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use TdisModule, only: kstp, kper
    ! -- dummy
    class(GwtDspType) :: this
    integer(I4B), intent(in) :: kiter 
    ! -- local
    integer(I4B) :: n, ipos, iflwchng, isympos
    real(DP) :: fd
! ------------------------------------------------------------------------------
    !
    ! -- Calculate xt3d coefficients if flow solution has changed
    !    Force iflwchng to be 1 for the very first iteration just in case
    !    there is no advection so that the xt3d_fcpc routine is called
    if (this%ixt3d > 0) then         ! amp_note: will need to generalize & modify if more dispersion formulations are added
      iflwchng = 0
      if (kper*kstp*kiter == 1) then
        iflwchng = 1
      else
        nodeloop: do n = 1, this%dis%nodes
          do ipos = this%dis%con%ia(n) + 1, this%dis%con%ia(n + 1) - 1
            ! -- Skip if XT3D not used for this connection
            isympos = this%dis%con%jas(ipos)
            if (this%idispform(isympos) /= 1) cycle   ! amp_note: verify that only flows at xt3d connections are relevant (not associated neighbor-of-neighbor connections)
            !
            fd = abs(this%gwfflowjaold(ipos) - this%fmi%gwfflowja(ipos))
            !
            ! -- todo: this check is not robust.  Should find a better way
            !    to determine if flow has changed.
            if(fd > 1.D-8) then
              iflwchng = 1
              exit nodeloop
            endif
          enddo
        enddo nodeloop
      endif
      !
      ! -- If flow has changed, then update coefficients
      if (iflwchng == 1) then
        !
        ! -- Calculate xt3d coefficients
        call this%xt3d%xt3d_fcpc(this%dis%nodes, .false.)
        !
        ! -- Save gwf flows
        do ipos = 1, size(this%gwfflowjaold)
          this%gwfflowjaold(ipos) = this%fmi%gwfflowja(ipos)
        enddo
      endif
    endif
    !
    ! -- Return
    return
  end subroutine dsp_cf

  subroutine dsp_fc(this, kiter, nodes, nja, njasln, amatsln, idxglo, rhs, cnew)
! ******************************************************************************
! dsp_fc -- Calculate coefficients and fill amat and rhs
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use GwfNpfModule, only: thksatnm
    ! -- dummy
    class(GwtDspType) :: this
    integer(I4B) :: kiter
    integer(I4B),intent(in) :: nodes
    integer(I4B),intent(in) :: nja
    integer(I4B),intent(in) :: njasln
    real(DP),dimension(njasln),intent(inout) :: amatsln
    integer(I4B),intent(in),dimension(nja) :: idxglo
    real(DP),intent(inout),dimension(nodes) :: rhs
    real(DP),intent(inout),dimension(nodes) :: cnew
    ! -- local
    integer(I4B) :: n, m, ii, iis
    integer(I4B) :: i, il, ig, iil, ilp1
! ------------------------------------------------------------------------------
    !
    ! -- Update amat and rhs for dispersion formulation
    !
!!    this%xt3d%lamatsaved = .false.       ! kluge to debug and test
!!    !
    ! -- Apply any saved coefficient updates
    if(this%ixt3d /= 0)                                                        &
      call this%xt3d%xt3d_amatsaved_fc(njasln, amatsln, idxglo)
    !
    ! -- Loop over rows (nodes)
    do n = 1, this%dis%nodes
      !
      ! -- Loop over connections of node n (columns) and apply the
      !    appropriate dispersion formulations
      do ii = this%dis%con%ia(n) + 1, this%dis%con%ia(n + 1) - 1
        if (this%dis%con%mask(ii) == 0) cycle
        !
        m = this%dis%con%ja(ii)
        ! -- Skip if neighbor has lower cell number
        if (m < n) cycle
        !
        iis = this%dis%con%jas(ii)
        !
        ! -- If no non-standard dispersion formulations used at any
        !    connections, always use standard dispersion formulation.
        !    Otherwise, use the appropriate formulation for the connection.
        if (this%inonstdf == 0) then
          !
          ! -- Standard dispersion formulation
          call this%stddisp_fc(n, ii, njasln, amatsln, idxglo, rhs)
          !
        else
          !
          ! -- Standard dispersion formulation
          if (this%idispform(iis) == 0)                                          &
             call this%stddisp_fc(n, ii, njasln, amatsln, idxglo, rhs)
          !
          ! -- XT3D formulation
          if (this%idispform(iis) == 1)                                          &
             call this%xt3d%xt3d_fc(n, ii, njasln, amatsln, idxglo, rhs, cnew)
          !
        end if
        !
      enddo
    enddo
    !
    ! -- Return
    return
  end subroutine dsp_fc
  
  subroutine dsp_flowja(this, cnew, flowja)
! ******************************************************************************
! dsp_flowja -- Calculate dispersion contribution to flowja
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use GwfNpfModule, only: thksatnm
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
    if(this%ixt3d > 0) call this%xt3d%xt3d_flowja(cnew, flowja)
    !
    if (.not.this%allnonstdf) then
      do n = 1, this%dis%nodes
        if(this%fmi%ibdgwfsat0(n) == 0) cycle
        do ipos = this%dis%con%ia(n) + 1, this%dis%con%ia(n + 1) - 1
          isympos = this%dis%con%jas(ipos)
          ! -- Skip if non-standard dispersion formulation used for this
          !    connection
          if(this%inonstdf /= 0) then
             if (this%idispform(isympos) /= 0) cycle
          end if
          !
          m = this%dis%con%ja(ipos)
          if(this%fmi%ibdgwfsat0(m) == 0) cycle
          dnm = this%dispcoef(isympos)
          flowja(ipos) = flowja(ipos) + dnm * (cnew(m) - cnew(n))
        enddo
      enddo
    endif
    !
    ! -- Return
    return
  end subroutine dsp_flowja
 
  subroutine stddisp_fc(this, n, ipos, njasln, amatsln, idxglo, rhs)
! ******************************************************************************
! stdcond_fc -- Formulate a connection
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use ConstantsModule, only: DONE
    ! -- dummy
    class(GwtDspType) :: this
    integer(I4B) :: n, ipos
    integer(I4B),intent(in) :: njasln
    real(DP),dimension(njasln),intent(inout) :: amatsln
    integer(I4B),intent(in),dimension(:) :: idxglo
    real(DP),intent(inout),dimension(:) :: rhs
    ! -- local
    integer(I4B) :: m, idiag, idiagm, isympos, isymcon, iil, iilp1
    real(DP) :: dnm
 ! ------------------------------------------------------------------------------
    !
    if(this%fmi%ibdgwfsat0(n) == 0) return
    idiag = this%dis%con%ia(n)
    isympos = this%dis%con%jas(ipos)
    !
    m = this%dis%con%ja(ipos)
!!    if (m < n) return   ! amp_note: check for m<n now done in calling routine loop
    if(this%fmi%ibdgwfsat0(m) == 0) return
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
    !
    ! -- Return
    return
  end subroutine stddisp_fc
  
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
    call mem_allocate(this%allnonstdf, 'allnonstdf', this%memoryPath)
    call mem_allocate(this%xt3dbyconn, 'XT3DBYCONN', this%memoryPath)
    call mem_allocate(this%iprxt3d, 'IPRXT3D', this%memoryPath)
    call mem_allocate(this%inonstdf, 'inonstdf', this%memoryPath)
    call mem_allocate(this%nbrmax, 'NBRMAX', this%memoryPath)
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
    this%allnonstdf = .false.
    this%xt3dbyconn = .false.
    this%iprxt3d = 0
    this%inonstdf = 0
    this%nbrmax = 0
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
     integer(I4B) :: i
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
    call mem_allocate(this%gwfflowjaold, this%dis%con%nja, 'GWFFLOWJAOLD',     &
      trim(this%memoryPath))
    !
    ! -- Non-standard dispersion formulation flag array
    ! -- Deallocate temporary allocation in dsp_df and allocate permanently
    if (this%ixt3d /= 0) deallocate(this%idispform)
    if (this%inonstdf /= 0)                                                    &
       call mem_allocate(this%idispform, this%dis%njas, 'IDISPFORM',           &
       this%memoryPath)
    !
    ! -- Allocate dispersion coefficient array   ! amp_note: if it turns out (later) that all connections use xt3d, reallocate to size 0 ???
      call mem_allocate(this%dispcoef, this%dis%njas, 'DISPCOEF',              &
        trim(this%memoryPath))
    !
    ! -- Initialize gwfflowjaold
    do i = 1, size(this%gwfflowjaold)
      this%gwfflowjaold(i) = DZERO
    enddo
    !
    ! -- initialize dispersion formulation flag array
    if (this%inonstdf /= 0) this%idispform = 0
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
    integer(I4B) :: inonstdf
! ------------------------------------------------------------------------------
    !
    inonstdf = this%inonstdf
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
      call mem_deallocate(this%gwfflowjaold)
      call mem_deallocate(this%dispcoef)
      if (inonstdf /= 0) call mem_deallocate(this%idispform)
    end if
    !
    ! -- deallocate objects
    if (this%ixt3d /= 0) then
      call this%xt3d%xt3d_da()
      deallocate(this%xt3d)
    end if
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
    call mem_deallocate(this%allnonstdf)
    call mem_deallocate(this%xt3dbyconn)
    call mem_deallocate(this%iprxt3d)
    call mem_deallocate(this%inonstdf)
    call mem_deallocate(this%nbrmax)
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
    use ConstantsModule,   only: LINELENGTH
    use SimModule,         only: ustop, store_error
    ! -- dummy
    class(GwtDspType) :: this
    ! -- local
    character(len=LINELENGTH) :: errmsg, keyword
    integer(I4B) :: ierr, isubopt
    logical :: isfound, endOfBlock
    ! -- formats
! ------------------------------------------------------------------------------
    !
    ! -- get options block
    call this%parser%GetBlock('OPTIONS', isfound, ierr, blockRequired=.false.)
    !
    ! -- parse options block if detected
    if (isfound) then
      write(this%iout,'(1x,a)')'PROCESSING DISPERSION OPTIONS'
      do
        call this%parser%GetNextLine(endOfBlock)
        if (endOfBlock) exit
        call this%parser%GetStringCaps(keyword)
        select case (keyword)
          case ('XT3D')
            this%ixt3d = 1
            write(this%iout, '(4x,a)')                                         &
                             'XT3D FORMULATION IS SELECTED.'
            this%inonstdf = 1
            do isubopt=1,3
              call this%parser%GetStringCaps(keyword)
              if(keyword == 'RHS') then    ! amp_note: RHS option was not previously programmed - verify that there's no harm in having it for DSP
                this%ixt3d = 2
                write(this%iout, '(8x,a)')                                     &
                                 'XT3D RHS OPTION IS SELECTED.'
              else if(keyword == 'BY_CONNECTION') then
                this%xt3dbyconn = .true.
                write(this%iout, '(8x,a)')                                     &
                                 'APPLICATION OF XT3D IS SPECIFIED BY ' //     &
                                 'CONNECTION.'
              else if(keyword == 'PRINT_INPUT') then
                this%iprxt3d = 1
                write(this%iout, '(8x,a)')                                     &
                                 'THE LIST OF XT3D CONNECTIONS WILL BE '   //  &
                                 'PRINTED IF APPLICATION OF XT3D IS '      //  &
                                 'SPECIFIED BY CONNECTION.'
              endif
            end do
          case default
            write(errmsg,'(4x,a,a)')'****ERROR. UNKNOWN DISPERSION OPTION: ',  &
                                     trim(keyword)
            call store_error(errmsg)
            call ustop()
        end select
      end do
      write(this%iout,'(1x,a)')'END OF DISPERSION OPTIONS'
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
    use ConstantsModule,   only: LINELENGTH
    use SimModule,         only: ustop, store_error, count_errors
    use MemoryManagerModule, only: mem_reallocate, mem_copyptr, mem_reassignptr
    ! -- dummy
    class(GwtDsptype) :: this
    ! -- local
    character(len=LINELENGTH) :: errmsg, keyword
    character(len=:), allocatable :: line
    integer(I4B) :: istart, istop, lloc, ierr
    logical :: isfound, endOfBlock
    logical, dimension(6)           :: lname
    character(len=24), dimension(6) :: aname
    ! -- formats
    ! -- data
    data aname(1) /'   DIFFUSION COEFFICIENT'/
    data aname(2) /'                     ALH'/
    data aname(3) /'                     ALV'/
    data aname(4) /'                    ATH1'/
    data aname(5) /'                    ATH2'/
    data aname(6) /'                     ATV'/
! ------------------------------------------------------------------------------
    !
    ! -- initialize
    lname(:) = .false.
    isfound = .false.
    !
    ! -- get griddata block
    call this%parser%GetBlock('GRIDDATA', isfound, ierr)
    if(isfound) then
      write(this%iout,'(1x,a)')'PROCESSING GRIDDATA'
      do
        call this%parser%GetNextLine(endOfBlock)
        if (endOfBlock) exit
        call this%parser%GetStringCaps(keyword)
        call this%parser%GetRemainingLine(line)
        lloc = 1
        select case (keyword)
        case ('DIFFC')
            call mem_reallocate(this%diffc, this%dis%nodes, 'DIFFC',           &
                              trim(this%memoryPath))
            call this%dis%read_grid_array(line, lloc, istart, istop, this%iout,&
                                         this%parser%iuactive, this%diffc,     &
                                         aname(1))
            lname(1) = .true.
        case ('ALH')
          call mem_reallocate(this%alh, this%dis%nodes, 'ALH',                 &
                            trim(this%memoryPath))
          call this%dis%read_grid_array(line, lloc, istart, istop, this%iout,  &
                                         this%parser%iuactive, this%alh,       &
                                         aname(2))
            lname(2) = .true.
        case ('ALV')
          call mem_reallocate(this%alv, this%dis%nodes, 'ALV',                 &
                            trim(this%memoryPath))
          call this%dis%read_grid_array(line, lloc, istart, istop, this%iout,  &
                                         this%parser%iuactive, this%alv,       &
                                         aname(3))
            lname(3) = .true.
        case ('ATH1')
          call mem_reallocate(this%ath1, this%dis%nodes, 'ATH1',               &
                            trim(this%memoryPath))
          call this%dis%read_grid_array(line, lloc, istart, istop, this%iout,  &
                                         this%parser%iuactive, this%ath1,      &
                                         aname(4))
          lname(4) = .true.
        case ('ATH2')
          call mem_reallocate(this%ath2, this%dis%nodes, 'ATH2',               &
                            trim(this%memoryPath))
          call this%dis%read_grid_array(line, lloc, istart, istop, this%iout,  &
                                         this%parser%iuactive, this%ath2,      &
                                         aname(5))
          lname(5) = .true.
        case ('ATV')
          call mem_reallocate(this%atv, this%dis%nodes, 'ATV',                 &
                            trim(this%memoryPath))
          call this%dis%read_grid_array(line, lloc, istart, istop, this%iout,  &
                                         this%parser%iuactive, this%atv,       &
                                         aname(6))
            lname(6) = .true.
        case default
          write(errmsg,'(4x,a,a)')'ERROR. UNKNOWN GRIDDATA TAG: ',             &
                                    trim(keyword)
          call store_error(errmsg)
          call this%parser%StoreErrorUnit()
          call ustop()
        end select
      end do
      write(this%iout,'(1x,a)')'END PROCESSING GRIDDATA'
    else
      write(errmsg,'(1x,a)')'ERROR.  REQUIRED GRIDDATA BLOCK NOT FOUND.'
      call store_error(errmsg)
      call this%parser%StoreErrorUnit()
      call ustop()
    end if
    !
    if(lname(1)) this%idiffc = 1
    if(lname(2)) this%idisp = this%idisp + 1
    if(lname(3)) this%idisp = this%idisp + 1
    if(lname(4)) this%idisp = this%idisp + 1
    if(lname(5)) this%idisp = this%idisp + 1
    !
    ! -- if dispersivities are specified, then both alh and ath1 must be included
    if(this%idisp > 0) then
      !
      ! -- make sure alh was specified
      if (.not. lname(2)) then
        write(errmsg,'(1x,a)') 'IF DISPERSIVITIES ARE SPECIFIED THEN ALH IS REQUIRED.'
        call store_error(errmsg)
      endif
      !
      ! -- make sure ath1 was specified
      if (.not. lname(4)) then
        write(errmsg,'(1x,a)') 'IF DISPERSIVITIES ARE SPECIFIED THEN ATH1 IS REQUIRED.'
        call store_error(errmsg)
      endif
      !
      ! -- If alv not specified then point it to alh
      if(.not. lname(3)) then
        call mem_reassignptr(this%alv, 'ALV', trim(this%memoryPath),                 &
                                       'ALH', trim(this%memoryPath))
      endif
      !
      ! -- If ath2 not specified then assign it to ath1
      if (.not. lname(5)) then
        call mem_reassignptr(this%ath2, 'ATH2', trim(this%memoryPath),                 &
                                        'ATH1', trim(this%memoryPath))
      endif
      !
      ! -- If atv not specified then assign it to ath2
      if (.not. lname(6)) then
        call mem_reassignptr(this%atv, 'ATV', trim(this%memoryPath),                 &
                                       'ATH2', trim(this%memoryPath))
      endif
    endif
    !
    ! -- terminate if errors
    if(count_errors() > 0) then
      call this%parser%StoreErrorUnit()
      call ustop()
    endif
    !
    ! -- Return
    return
  end subroutine read_data

  subroutine read_xt3d_data(this,isdfcall)
! ******************************************************************************
! read_xt3d_data -- read the dsp xt3d data block
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use ConstantsModule,   only: LINELENGTH, DONE, DPIO180
    use MemoryManagerModule, only: mem_allocate, mem_reallocate, mem_deallocate, &
                                   mem_reassignptr
    use SimModule,         only: ustop, store_error, count_errors
    ! -- dummy
    class(GwtDspType) :: this
    logical :: isdfcall
    ! -- local
    character(len=LINELENGTH) :: errmsg, line, nodestr, cellidm, cellidn
    integer(I4B) :: n, m, ierr, iux, nodeun, nodeum, ii, iis
    integer(I4B) :: nxt3ddata
    logical :: isfound, endOfBlock
! ------------------------------------------------------------------------------
    !
    ! -- If call from _df, skip past GRIDDATA block
    if (isdfcall) call this%parser%GetBlock('GRIDDATA', isfound, ierr)
    !
    ! -- Check for XT3DDATA block
    call this%parser%GetBlock('XT3DDATA', isfound, ierr)
    if(.not.isfound) then
      write(errmsg,'(1x,a)')'ERROR.  REQUIRED XT3DDATA BLOCK NOT FOUND.'
      call store_error(errmsg)
      call this%parser%StoreErrorUnit()
      call ustop()
    end if
    !
    ! -- Read XT3DDATA block
    if (.not.isdfcall) write(this%iout,'(1x,a)')'PROCESSING XT3DDATA'
    nxt3ddata = 0                    ! amp_note: specify nxt3ddata in dimensions block???
    do
      call this%parser%GetNextLine(endOfBlock)
      if (endOfBlock) exit
      call this%parser%GetCurrentLine(line)
      !
      ! -- cellidn (read as cellid and convert to user node)
      call this%parser%GetCellid(this%dis%ndim, cellidn)
      ! -- convert user node to reduced node number     ! amp_note: confirm that conversion is appropriate here
      n = this%dis%noder_from_cellid(cellidn, &
                                       this%parser%iuactive, this%iout)
      ! -- cellidm (read as cellid and convert to user node)
      call this%parser%GetCellid(this%dis%ndim, cellidm)
      ! -- convert user node to reduced node number     ! amp_note: confirm that conversion is appropriate here
      m = this%dis%noder_from_cellid(cellidm, &
                                       this%parser%iuactive, this%iout)
      ! -- set idispform flag for connection to 1
      ii = this%dis%con%getjaindex(n, m)
      iis = this%dis%con%jas(ii)
      this%idispform(iis) = 1
      nxt3ddata = nxt3ddata + 1
      !
      if ((.not.isdfcall).and.(this%iprxt3d /= 0))                             &
        write(this%iout, '(2a10)') trim(adjustl(cellidn)),                     &
                                   trim(adjustl(cellidm))
    end do
    ! -- terminate if read errors encountered
    if(count_errors() > 0) then
      call this%parser%StoreErrorUnit()
      call ustop()
    endif
    !
    ! -- If call from _df, rewind dsp input file and skip past OPTIONS block
    !    so data can be reread later
    if (isdfcall) then
      rewind(this%parser%GetUnit())
      call this%parser%GetBlock('OPTIONS', isfound, ierr)
    else
      ! -- Print number of connections listed in GRIDDATA block
      write(this%iout, '(1x,a,i10)')                                           &
          'NUMBER OF XT3D CONNECTIONS LISTED:', nxt3ddata
      ! -- Specification is by connection, so warn if no connections listed
      if (nxt3ddata.eq.0) write(this%iout, '(4x,a,1(1x,a))')                   &
          '****WARNING. Application of XT3D is specified by connection',       &
          'but no connections are listed in the XT3DDATA block.'
      ! -- Final XT3DDATA message
      write(this%iout,'(1x,a)')'END PROCESSING XT3DDATA'
    end if
    !
    ! -- Return
    return
  end subroutine read_xt3d_data
 
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
      if(this%fmi%ibdgwfsat0(n) == 0) cycle
      !
      ! -- specific discharge
      qx = DZERO
      qy = DZERO
      qz = DZERO
      q = DZERO
      qx = this%fmi%gwfspdis(1, n)
      qy = this%fmi%gwfspdis(2, n)
      qz = this%fmi%gwfspdis(3, n)
      q = qx ** 2 + qy ** 2 + qz ** 2
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
      endif
      dstar = DZERO
      if (this%idiffc > 0) then
        dstar = this%diffc(n) * this%porosity(n)
      endif
      !
      ! -- Calculate the longitudal and transverse dispersivities
      al = DZERO
      at1 = DZERO
      at2 = DZERO
      if (q > DZERO) then
        qzoqsquared = (qz / q) ** 2
        al = alh * (DONE - qzoqsquared) + alv * qzoqsquared
        at1 = ath1 * (DONE - qzoqsquared) + atv * qzoqsquared
        at2 = ath2 * (DONE - qzoqsquared) + atv * qzoqsquared
      endif
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
        endif
        !
        ! -- acos(1) not defined, so set to zero if necessary
        if (a <= -DONE) then
          this%angle1(n) = DPI
        elseif (a >= DONE) then
          this%angle1(n) = DZERO
        else
          this%angle1(n) = acos(a)
        endif
        !
      endif
    enddo
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
    use GwfNpfModule, only: thksatnm, hyeff_calc, hcond, vcond
    ! -- dummy
    class(GwtDspType) :: this
    ! -- local
    integer(I4B) :: nodes, n, m, idiag, ipos
    real(DP) :: clnm, clmn, dn, dm
    real(DP) :: vg1, vg2, vg3
    integer(I4B) :: ihc, isympos
    real(DP) :: satn, satm, topn, topm, botn, botm
    real(DP) :: hwva, cond, cn, cm, denom
    real(DP) :: anm, amn, thksatn, thksatm, sill_top, sill_bot, tpn, tpm
! ------------------------------------------------------------------------------
    !
    nodes = size(this%d11)
    do n = 1, nodes
      if(this%fmi%ibdgwfsat0(n) == 0) cycle
      idiag = this%dis%con%ia(n)
      do ipos = this%dis%con%ia(n) + 1, this%dis%con%ia(n + 1) - 1
        isympos = this%dis%con%jas(ipos)
        ! -- Skip if nonstandard dispersion formulation used for this connection
        if(this%inonstdf /= 0) then
           if (this%idispform(isympos) /= 0) cycle
        end if
        !
        ! -- Set m to connected cell
        m = this%dis%con%ja(ipos)
        !
        ! -- skip for lower triangle
        if (m < n) cycle
        this%dispcoef(isympos) = DZERO
        if(this%fmi%ibdgwfsat0(m) == 0) cycle
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
        dn = hyeff_calc(this%d11(n), this%d22(n), this%d33(n),                 &
                        this%angle1(n), this%angle2(n), this%angle3(n),        &
                        vg1, vg2, vg3)
        dm = hyeff_calc(this%d11(m), this%d22(m), this%d33(m),                 &
                        this%angle1(m), this%angle2(m), this%angle3(m),        &
                        vg1, vg2, vg3)
        !
        ! -- Calculate dispersion conductance based on NPF subroutines and the
        !    effective dispersion coefficients dn and dm.
        if(ihc == 0) then
          clnm = satn * (topn - botn) * DHALF
          clmn = satm * (topm - botm) * DHALF
          anm = hwva
          !
          ! -- n is convertible and unsaturated
          if (satn == DZERO) then
            anm = DZERO
          else if (n > m .and. satn < DONE) then
            anm = DZERO
          endif
          !
          ! -- m is convertible and unsaturated
          if (satm == DZERO) then
            anm = DZERO
          else if (m > n .and. satm < DONE) then
            anm = DZERO
          endif
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
          endif
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
      enddo
    enddo
    !
    ! -- Return
    return
  end subroutine calcdispcoef
  
end module GwtDspModule
