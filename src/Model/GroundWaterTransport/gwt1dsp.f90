module GwtDspModule

  use KindModule,             only: DP, I4B
  use ConstantsModule,        only: DONE, DZERO, DHALF
  use NumericalPackageModule, only: NumericalPackageType
  use BaseDisModule,          only: DisBaseType
  use GwtFmiModule,           only: GwtFmiType
  use Xt3dModule,             only: Xt3dType, xt3d_cr

  implicit none
  private
  public :: GwtDspType
  public :: dsp_cr

  type, extends(NumericalPackageType) :: GwtDspType
    
    integer(I4B), dimension(:), pointer              :: ibound     => null()    ! pointer to GWT model ibound
    type(GwtFmiType), pointer                        :: fmi        => null()    ! pointer to GWT fmi object
    real(DP), dimension(:), pointer                  :: porosity   => null()    ! pointer to GWT storage porosity
    real(DP), dimension(:), pointer                  :: diffc      => null()    ! molecular diffusion coefficient for each cell
    real(DP), dimension(:), pointer                  :: alh        => null()    ! longitudinal horizontal dispersivity
    real(DP), dimension(:), pointer                  :: alv        => null()    ! longitudinal vertical dispersivity
    real(DP), dimension(:), pointer                  :: ath        => null()    ! transverse horizontal dispersivity
    real(DP), dimension(:), pointer                  :: atv        => null()    ! transverse vertical dispersivity
    integer(I4B), pointer                            :: idiffc     => null()    ! flag indicating diffusion is active
    integer(I4B), pointer                            :: idisp      => null()    ! flag indicating mechanical dispersion is active
    integer(I4B), pointer                            :: ixt3d      => null()    ! flag indicating xt3d is active
    type(Xt3dType), pointer                          :: xt3d       => null()    ! xt3d object
    integer(I4B), pointer                            :: id22       => null()    ! flag indicating d22 is available
    integer(I4B), pointer                            :: id33       => null()    ! flag indicating d33 is available
    real(DP), dimension(:), pointer                  :: d11        => null()    ! dispersion coefficient
    real(DP), dimension(:), pointer                  :: d22        => null()    ! dispersion coefficient
    real(DP), dimension(:), pointer                  :: d33        => null()    ! dispersion coefficient
    real(DP), dimension(:), pointer                  :: angle1     => null()    ! rotation angle 1
    real(DP), dimension(:), pointer                  :: angle2     => null()    ! rotation angle 2
    real(DP), dimension(:), pointer                  :: angle3     => null()    ! rotation angle 3
    integer(I4B), pointer                            :: iangle1    => null()    ! flag indicating angle1 is available
    integer(I4B), pointer                            :: iangle2    => null()    ! flag indicating angle2 is available
    integer(I4B), pointer                            :: iangle3    => null()    ! flag indicating angle3 is available
    real(DP), dimension(:), pointer                  :: gwfflowjaold => null()  ! stored gwf flowja values from the last time disp coeffs were calculated
    
  contains
  
    procedure :: dsp_df
    procedure :: dsp_ac
    procedure :: dsp_mc
    procedure :: dsp_ar
    procedure :: dsp_ad
    procedure :: dsp_cf
    procedure :: dsp_fc
    procedure :: dsp_bd
    procedure :: allocate_scalars
    procedure :: allocate_arrays
    procedure, private :: read_options
    procedure, private :: read_data
    procedure, private :: calcdisp
   
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
    ! -- create name and origin
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

  subroutine dsp_df(this)
! ******************************************************************************
! dsp_df -- Allocate and Read
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(GwtDspType)                       :: this
    ! -- local
    ! -- formats
    character(len=*), parameter :: fmtdsp =                                    &
      "(1x,/1x,'DSP-- DISPERSION PACKAGE, VERSION 1, 1/24/2018',               &
      &' INPUT READ FROM UNIT ', i0, //)"
! ------------------------------------------------------------------------------
    !
    ! -- Read dispersion options
    call this%read_options()
    !
    ! -- xt3d create
    if(this%ixt3d > 0) then
      call xt3d_cr(this%xt3d, trim(this%origin), this%inunit, this%iout,       &
                   ldispopt=.true.)
      this%xt3d%ixt3d = this%ixt3d
    endif
    !
    ! -- Return
    return
  end subroutine dsp_df

  subroutine dsp_ac(this, moffset, sparse, nodes, ia, ja)
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
    integer(I4B), intent(in) :: moffset, nodes
    integer(I4B), dimension(:), intent(in) :: ia
    integer(I4B), dimension(:), intent(in) :: ja
    type(sparsematrix), intent(inout) :: sparse
    ! -- local
! ------------------------------------------------------------------------------
    !
    ! -- Add extended neighbors (neighbors of neighbors)
    if(this%ixt3d > 0) call this%xt3d%xt3d_ac(moffset, sparse, nodes, ia, ja)
    !
    ! -- Return
    return
  end subroutine dsp_ac

  subroutine dsp_mc(this, moffset, nodes, ia, ja, iasln, jasln)
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
    integer(I4B), intent(in) :: moffset, nodes
    integer(I4B), dimension(:), intent(in) :: ia
    integer(I4B), dimension(:), intent(in) :: ja
    integer(I4B), dimension(:), intent(in) :: iasln
    integer(I4B), dimension(:), intent(in) :: jasln
    ! -- local
    integer(I4B) :: inewton
! ------------------------------------------------------------------------------
    !
    ! TODO: set inewton
    inewton = 0
    if(this%ixt3d > 0) call this%xt3d%xt3d_mc(moffset, nodes, ia, ja, iasln,   &
                                              jasln, inewton)
    !
    ! -- Return
    return
  end subroutine dsp_mc

  subroutine dsp_ar(this, dis, ibound, porosity)
! ******************************************************************************
! dsp_ar -- Allocate and Read
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(GwtDspType)                       :: this
    class(DisBaseType), pointer             :: dis
    integer(I4B), dimension(:), pointer          :: ibound
    real(DP), dimension(:), pointer :: porosity
    ! -- local
    ! -- formats
    character(len=*), parameter :: fmtdsp =                                    &
      "(1x,/1x,'DSP-- DISPERSION PACKAGE, VERSION 1, 1/24/2018',               &
      &' INPUT READ FROM UNIT ', i0, //)"
! ------------------------------------------------------------------------------
    !
    ! -- dsp pointers to arguments that were passed in
    this%dis     => dis
    this%ibound  => ibound
    this%porosity => porosity
    !
    ! -- Print a message identifying the dispersion package.
    write(this%iout, fmtdsp) this%inunit
    !
    ! -- Allocate arrays
    call this%allocate_arrays(dis%nodes)
    !
    ! -- Read dispersion data
    call this%read_data()
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
    real(DP), target :: min_satthk
! ------------------------------------------------------------------------------
    !
    ! -- xt3d
    ! TODO: might consider adding a new mf6 level set pointers method, and
    ! doing this stuff there instead of in the time step loop.
    if (kstp * kper == 1) then
      ! TODO MIN_SATTHK cannot be a local variable here.  it goes out of scope
      min_satthk = DZERO
      if(this%ixt3d > 0) call this%xt3d%xt3d_ar(this%dis, this%ibound,         &
        this%d11, this%id33, this%d33, this%fmi%gwfsat, this%id22, this%d22,   &
        this%fmi%igwfinwtup, min_satthk, this%fmi%gwficelltype,                &
        this%iangle1, this%iangle2, this%iangle3,                              &
        this%angle1, this%angle2, this%angle3)
    endif
    !
    ! -- Fill d11, d22, d33, angle1, angle2, angle3 using specific discharge
    call this%calcdisp()
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
    ! -- dummy
    class(GwtDspType) :: this
    integer(I4B), intent(in) :: kiter 
    ! -- local
    integer(I4B) :: n, ipos, iflwchng
    real(DP) :: fd
! ------------------------------------------------------------------------------
    !
    ! -- Calculate xt3d coefficients if flow solution has changed
    if (this%ixt3d > 0) then
      iflwchng = 0
      nodeloop: do n = 1, this%dis%nodes
        do ipos = this%dis%con%ia(n) + 1, this%dis%con%ia(n + 1) - 1
          fd = abs(this%gwfflowjaold(ipos) - this%fmi%gwfflowja(ipos))
          if(fd > 1.D-8) then
            iflwchng = 1
            exit nodeloop
          endif
        enddo
      enddo nodeloop
      !
      ! -- If flow has changed, then update coefficients
      if (iflwchng == 1) then
        !
        ! -- Calculate xt3d coefficients
        call this%xt3d%xt3d_fcpc(this%dis%nodes)
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
    integer(I4B) :: n, m, idiag, ipos
    real(DP) :: clnm, clmn, anm, wt, dstar, dnm
    integer(I4B) :: ihc, ibdn, ibdm, ictn, ictm, inwtup, iusg
    real(DP) :: hn, hm, satn, satm, topn, topm, botn, botm, satomega
! ------------------------------------------------------------------------------
    !
    if(this%ixt3d > 0) then
      call this%xt3d%xt3d_fc(kiter, nodes, nja, njasln, amatsln, idxglo, rhs,  &
                             cnew)
    else
      inwtup = this%fmi%igwfinwtup
      iusg = this%fmi%igwfiusgnrhc
      satomega = this%fmi%gwfsatomega
      do n = 1, nodes
        if(this%ibound(n) == 0) cycle
        idiag = this%dis%con%ia(n)
        do ipos = this%dis%con%ia(n) + 1, this%dis%con%ia(n + 1) - 1
          m = this%dis%con%ja(ipos)
          if(this%ibound(m) == 0) cycle
          clnm = this%dis%con%cl1(this%dis%con%jas(ipos))
          clmn = this%dis%con%cl2(this%dis%con%jas(ipos))
          wt = clmn / (clnm + clmn)
          ihc = this%dis%con%ihc(this%dis%con%jas(ipos))
          ibdn = this%fmi%gwfibound(n)
          ibdm = this%fmi%gwfibound(m)
          ictn = this%fmi%gwficelltype(n)
          ictm = this%fmi%gwficelltype(m)
          hn = this%fmi%gwfhead(n)
          hm = this%fmi%gwfhead(m)
          satn = this%fmi%gwfsat(n)
          satm = this%fmi%gwfsat(m)
          topn = this%dis%top(n)
          topm = this%dis%top(m)
          botn = this%dis%bot(n)
          botm = this%dis%bot(m)
          anm = thksatnm(ibdn, ibdm, ictn, ictm, inwtup, ihc, iusg,              &
                         hn, hm, satn, satm, topn, topm, botn, botm, satomega)
          dstar = wt * this%porosity(n) * this%diffc(n) + &
                  (DONE - wt) * this%porosity(m) * this%diffc(m)
          dnm = dstar * anm / (clnm + clmn)
          amatsln(idxglo(ipos)) = amatsln(idxglo(ipos)) + dnm
          amatsln(idxglo(idiag)) = amatsln(idxglo(idiag)) - dnm
        enddo
      enddo
    endif
    !
    ! -- Return
    return
  end subroutine dsp_fc
  
  subroutine dsp_bd(this, nodes, nja, cnew, flowja)
! ******************************************************************************
! dsp_bd -- Budget
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use GwfNpfModule, only: thksatnm
    ! -- dummy
    class(GwtDspType) :: this
    integer(I4B), intent(inout) :: nodes
    integer(I4B), intent(inout) :: nja
    real(DP), intent(inout), dimension(:) :: cnew
    real(DP), intent(inout), dimension(:) :: flowja
    ! -- local
    integer(I4B) :: n, m, idiag, ipos
    real(DP) :: clnm, clmn, anm, wt, dstar, dnm
    integer(I4B) :: ihc, ibdn, ibdm, ictn, ictm, inwtup, iusg
    real(DP) :: hn, hm, satn, satm, topn, topm, botn, botm, satomega
! ------------------------------------------------------------------------------
    !
    ! -- Calculate dispersion and add to flowja
    if(this%ixt3d > 0) then
      call this%xt3d%xt3d_flowja(nodes, nja, cnew, flowja)
    else
      inwtup = this%fmi%igwfinwtup
      iusg = this%fmi%igwfiusgnrhc
      satomega = this%fmi%gwfsatomega
      do n = 1, nodes
        if(this%ibound(n) == 0) cycle
        idiag = this%dis%con%ia(n)
        do ipos = this%dis%con%ia(n) + 1, this%dis%con%ia(n + 1) - 1
          m = this%dis%con%ja(ipos)
          if(this%ibound(m) == 0) cycle
          clnm = this%dis%con%cl1(this%dis%con%jas(ipos))
          clmn = this%dis%con%cl2(this%dis%con%jas(ipos))
          wt = clmn / (clnm + clmn)
          ihc = this%dis%con%ihc(this%dis%con%jas(ipos))
          ibdn = this%fmi%gwfibound(n)
          ibdm = this%fmi%gwfibound(m)
          ictn = this%fmi%gwficelltype(n)
          ictm = this%fmi%gwficelltype(m)
          hn = this%fmi%gwfhead(n)
          hm = this%fmi%gwfhead(m)
          satn = this%fmi%gwfsat(n)
          satm = this%fmi%gwfsat(m)
          topn = this%dis%top(n)
          topm = this%dis%top(m)
          botn = this%dis%bot(n)
          botm = this%dis%bot(m)
          anm = thksatnm(ibdn, ibdm, ictn, ictm, inwtup, ihc, iusg,              &
                         hn, hm, satn, satm, topn, topm, botn, botm, satomega)
          dstar = wt * this%porosity(n) * this%diffc(n) + &
                  (DONE - wt) * this%porosity(m) * this%diffc(m)
          dnm = dstar * anm / (clnm + clmn)
          flowja(ipos) = flowja(ipos) + dnm * (cnew(m) - cnew(n))
        enddo
      enddo
    endif
    !
    ! -- Return
    return
  end subroutine dsp_bd
  
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
    call mem_allocate(this%idiffc, 'IDIFFC', this%origin)
    call mem_allocate(this%idisp, 'IDISP', this%origin)
    call mem_allocate(this%ixt3d, 'IXT3D', this%origin)
    call mem_allocate(this%id22, 'ID22', this%origin)
    call mem_allocate(this%id33, 'ID33', this%origin)
    call mem_allocate(this%iangle1, 'IANGLE1', this%origin)
    call mem_allocate(this%iangle2, 'IANGLE2', this%origin)
    call mem_allocate(this%iangle3, 'IANGLE3', this%origin)
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
    call mem_allocate(this%diffc, 0, 'DIFFC', trim(this%origin))
    call mem_allocate(this%alh, 0, 'ALH', trim(this%origin))
    call mem_allocate(this%alv, 0, 'ALV', trim(this%origin))
    call mem_allocate(this%ath, 0, 'ATH', trim(this%origin))
    call mem_allocate(this%atv, 0, 'ATV', trim(this%origin))
    call mem_allocate(this%d11, nodes, 'D11', trim(this%origin))
    call mem_allocate(this%d22, nodes, 'D22', trim(this%origin))
    call mem_allocate(this%d33, nodes, 'D33', trim(this%origin))
    call mem_allocate(this%angle1, nodes, 'ANGLE1', trim(this%origin))
    call mem_allocate(this%angle2, nodes, 'ANGLE2', trim(this%origin))
    call mem_allocate(this%angle3, nodes, 'ANGLE3', trim(this%origin))
    call mem_allocate(this%gwfflowjaold, this%dis%con%nja, 'GWFFLOWJAOLD',     &
      trim(this%origin))
    !
    ! -- Return
    return
  end subroutine allocate_arrays

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
    integer(I4B) :: ierr
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
    use MemoryManagerModule, only: mem_reallocate
    ! -- dummy
    class(GwtDsptype) :: this
    ! -- local
    character(len=LINELENGTH) :: line, errmsg, keyword
    integer(I4B) :: istart, istop, lloc, ierr
    logical :: isfound, endOfBlock
    logical, dimension(5)           :: lname
    character(len=24), dimension(5) :: aname
    ! -- formats
    ! -- data
    data aname(1) /'   DIFFUSION COEFFICIENT'/
    data aname(2) /'                     ALH'/
    data aname(3) /'                     ALV'/
    data aname(4) /'                     ATH'/
    data aname(5) /'                     ATV'/
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
                              trim(this%origin))
            call this%dis%read_grid_array(line, lloc, istart, istop, this%iout,&
                                         this%parser%iuactive, this%diffc,     &
                                         aname(1))
            lname(1) = .true.
        case ('ALH')
          call mem_reallocate(this%alh, this%dis%nodes, 'ALH',                 &
                            trim(this%origin))
          call this%dis%read_grid_array(line, lloc, istart, istop, this%iout,  &
                                         this%parser%iuactive, this%alh,       &
                                         aname(2))
            lname(2) = .true.
        case ('ALV')
          call mem_reallocate(this%alv, this%dis%nodes, 'ALV',                 &
                            trim(this%origin))
          call this%dis%read_grid_array(line, lloc, istart, istop, this%iout,  &
                                         this%parser%iuactive, this%alv,       &
                                         aname(3))
            lname(3) = .true.
        case ('ATH')
          call mem_reallocate(this%ath, this%dis%nodes, 'ATH',                 &
                            trim(this%origin))
          call this%dis%read_grid_array(line, lloc, istart, istop, this%iout,  &
                                         this%parser%iuactive, this%ath,       &
                                         aname(4))
            lname(4) = .true.
        case ('ATV')
          call mem_reallocate(this%atv, this%dis%nodes, 'ATV',                 &
                            trim(this%origin))
          call this%dis%read_grid_array(line, lloc, istart, istop, this%iout,  &
                                         this%parser%iuactive, this%atv,       &
                                         aname(5))
            lname(5) = .true.
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
    if(this%idisp > 0 .and. this%idisp < 4) then
      write(errmsg,'(1x,a)') 'SPECIFY ALL FOUR DISPERSIVITIES OR NONE OF THEM.'
      call store_error(errmsg)
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
 
  subroutine calcdisp(this)
! ******************************************************************************
! calcdisp -- Calculate dispersion coefficients
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
    real(DP) :: alh, alv, ath, atv, a
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
      if(this%ibound(n) == 0) cycle
      !
      ! -- specific discharge
      qx = this%fmi%gwfspdis(1, n)
      qy = this%fmi%gwfspdis(2, n)
      qz = this%fmi%gwfspdis(3, n)
      q = qx ** 2 + qy ** 2 + qz ** 2
      if (q > DZERO) q = sqrt(q)
      !
      ! -- dispersion coefficients
      alh = this%alh(n)
      ath = this%ath(n)
      dstar = this%diffc(n) * this%porosity(n)
      this%d11(n) = alh * q + dstar
      this%d22(n) = ath * q + dstar
      this%d33(n) = ath * q + dstar
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
      if (a < DONE) then
        this%angle1(n) = acos(a)
      else
        this%angle1(n) = DZERO
      endif
      !
    enddo
    !
    ! -- Return
    return
  end subroutine calcdisp
  
end module GwtDspModule
