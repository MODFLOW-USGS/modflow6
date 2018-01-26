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
    
  contains
  
    procedure :: dsp_df
    procedure :: dsp_ac
    procedure :: dsp_mc
    procedure :: dsp_ar
    procedure :: dsp_fc
    procedure :: dsp_bd
    procedure :: allocate_scalars
    procedure :: allocate_arrays
    procedure, private :: read_options
    procedure, private :: read_data
   
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
    ! -- xt3d create
    if(this%ixt3d > 0) then
      call xt3d_cr(this%xt3d, trim(this%origin), this%inunit, this%iout)
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
! ------------------------------------------------------------------------------
    !
    if(this%ixt3d > 0) call this%xt3d%xt3d_mc(moffset, nodes, ia, ja, iasln,   &
                                              jasln, 0)
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
    class(DisBaseType), pointer, intent(in) :: dis
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
    ! -- Read dispersion options
    call this%read_options()
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

  subroutine dsp_fc(this, nodes, amatsln, idxglo)
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
    integer, intent(in) :: nodes
    real(DP), dimension(:), intent(inout) :: amatsln
    integer(I4B), intent(in), dimension(:) :: idxglo
    ! -- local
    integer(I4B) :: n, m, idiag, ipos
    real(DP) :: clnm, clmn, anm, wt, dstar, dnm
    integer(I4B) :: ihc, ibdn, ibdm, ictn, ictm, inwtup, iusg
    real(DP) :: hn, hm, satn, satm, topn, topm, botn, botm, satomega
! ------------------------------------------------------------------------------
    !
    ! -- loop through and calculate dispersion contribution
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
    !
    ! -- Return
    return
  end subroutine dsp_fc
  
  subroutine dsp_bd(this, cnew, flowja)
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
    real(DP), intent(in), dimension(:) :: cnew
    real(DP), intent(inout), dimension(:) :: flowja
    ! -- local
    integer(I4B) :: nodes
    integer(I4B) :: n, m, idiag, ipos
    real(DP) :: clnm, clmn, anm, wt, dstar, dnm
    integer(I4B) :: ihc, ibdn, ibdm, ictn, ictm, inwtup, iusg
    real(DP) :: hn, hm, satn, satm, topn, topm, botn, botm, satomega
! ------------------------------------------------------------------------------
    !
    ! -- Calculate advection and add to flowja
    nodes = size(cnew)
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
    !
    ! -- Initialize
    this%idiffc = 0
    this%idisp = 0
    this%ixt3d = 1
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
 
end module GwtDspModule
  