! -- Lake Transport Module
! -- todo: need to implement mass terms for rain, evap, inflow, withdrawal, etc.
! -- todo: support ibound concept for lkt?
! -- todo: write lkt budget table
! -- todo: save lkt budget to binary file
! -- todo: what to do about reactions in lake?  Decay?
! -- todo: save the lkt concentration into the lak aux variable?
! -- todo: calculate the lak DENSE aux variable using concentration?
module GwtLktModule

  use KindModule, only: DP, I4B
  use ConstantsModule, only: DZERO, DONE, DHALF, DEP20, LENFTYPE, LINELENGTH,  &
                             LENBOUNDNAME
  use BndModule, only: BndType, GetBndFromList
  use GwtFmiModule, only: GwtFmiType
  use LakModule, only: LakType
  use MemoryTypeModule, only: MemoryTSType
  use BudgetModule, only : BudgetType
  
  implicit none
  
  public lkt_create
  
  character(len=LENFTYPE) :: ftype = 'LKT'
  character(len=16)       :: text  = '             LKT'
  
  type, extends(BndType) :: GwtLktType
    character (len=8), dimension(:), pointer, contiguous :: status => null()
    character(len=16), dimension(:), pointer, contiguous :: clktbudget => NULL()
    integer(I4B), pointer                              :: imatrows => null()   ! if active, add new rows to matrix
    integer(I4B), pointer                              :: iprconc => null()
    integer(I4B), pointer                              :: iconcout => null()
    integer(I4B), pointer                              :: ibudgetout => null()
    integer(I4B), pointer                              :: cbcauxitems => NULL()
    integer(I4B), pointer                              :: nlakes => null()      ! number of lakes.  set from gwf lak nlakes
    integer(I4B), pointer                              :: bditems => NULL()
    integer(I4B), pointer                              :: igwflakpak => null()  ! package number of corresponding lak package
    real(DP), dimension(:), pointer, contiguous        :: strt => null()        ! starting lake concentration
    integer(I4B), dimension(:), pointer, contiguous    :: idxlocnode => null()      !map position in global rhs and x array of pack entry
    integer(I4B), dimension(:), pointer, contiguous    :: idxdglo => null()         !map position in global array of package diagonal row entries
    integer(I4B), dimension(:), pointer, contiguous    :: idxoffdglo => null()      !map position in global array of package off diagonal row entries
    integer(I4B), dimension(:), pointer, contiguous    :: idxsymdglo => null()      !map position in global array of package diagonal entries to model rows
    integer(I4B), dimension(:), pointer, contiguous    :: idxsymoffdglo => null()   !map position in global array of package off diagonal entries to model rows
    integer(I4B), dimension(:), pointer, contiguous    :: iboundpak => null()       !package ibound
    real(DP), dimension(:), pointer, contiguous        :: xnewpak => null()     ! lak concentration for current time step
    real(DP), dimension(:), pointer, contiguous        :: xoldpak => null()     ! lak concentration from previous time step
    real(DP), dimension(:), pointer, contiguous        :: dbuff => null()
    character(len=LENBOUNDNAME), dimension(:), pointer,                         &
                                 contiguous :: lakename => null()
    type (MemoryTSType), dimension(:), pointer, contiguous :: lauxvar => null()
    type(GwtFmiType), pointer                          :: fmi => null()         ! pointer to fmi object
    type(LakType), pointer                             :: lakptr => null()      ! pointer to lake package
    type(BudgetType), pointer :: budget => NULL()
    real(DP), dimension(:), pointer, contiguous        :: qsto => null()
  contains
    procedure :: set_pointers => lkt_set_pointers
    procedure :: bnd_ac => lkt_ac
    procedure :: bnd_mc => lkt_mc
    procedure :: bnd_ar => lkt_ar
    procedure :: bnd_rp => lkt_rp
    procedure :: bnd_ad => lkt_ad
    procedure :: bnd_fc => lkt_fc
    procedure, private :: lkt_fc_expanded
    procedure, private :: lkt_fc_nonexpanded
    procedure, private :: lkt_cfupdate
    procedure :: bnd_bd => lkt_bd
    procedure :: bnd_ot => lkt_ot
    procedure :: allocate_scalars
    procedure :: lkt_allocate_arrays
    procedure :: find_lak_package
    procedure :: lkt_solve
    procedure :: bnd_options => lkt_options
    procedure :: read_dimensions => lkt_read_dimensions
    procedure :: lkt_read_lakes
    procedure :: read_initial_attr => lkt_read_initial_attr
    procedure :: define_listlabel
    
  end type GwtLktType

  contains  
  
  subroutine lkt_create(packobj, id, ibcnum, inunit, iout, namemodel, pakname, &
                        fmi)
! ******************************************************************************
! lkt_create -- Create a New LKT Package
! Subroutine: (1) create new-style package
!             (2) point bndobj to the new package
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(BndType), pointer :: packobj
    integer(I4B),intent(in) :: id
    integer(I4B),intent(in) :: ibcnum
    integer(I4B),intent(in) :: inunit
    integer(I4B),intent(in) :: iout
    character(len=*), intent(in) :: namemodel
    character(len=*), intent(in) :: pakname
    type(GwtFmiType), pointer :: fmi
    ! -- local
    type(GwtLktType), pointer :: lktobj
! ------------------------------------------------------------------------------
    !
    ! -- allocate the object and assign values to object variables
    allocate(lktobj)
    packobj => lktobj
    !
    ! -- create name and origin
    call packobj%set_names(ibcnum, namemodel, pakname, ftype)
    packobj%text = text
    !
    ! -- allocate scalars
    call lktobj%allocate_scalars()
    !
    ! -- initialize package
    call packobj%pack_initialize()

    packobj%inunit = inunit
    packobj%iout = iout
    packobj%id = id
    packobj%ibcnum = ibcnum
    packobj%ncolbnd = 1
    packobj%iscloc = 1
    
    ! -- Store pointer to flow model interface.  When the GwfGwt exchange is
    !    created, it sets fmi%bndlist so that the GWT model has access to all
    !    the flow packages
    lktobj%fmi => fmi
    !
    ! -- return
    return
  end subroutine lkt_create

  subroutine lkt_ac(this, moffset, sparse)
! ******************************************************************************
! bnd_ac -- Add package connection to matrix
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use MemoryManagerModule, only: mem_setptr
    use SparseModule, only: sparsematrix
    use SimModule, only: store_error, ustop
    ! -- dummy
    class(GwtLktType),intent(inout) :: this
    integer(I4B), intent(in) :: moffset
    type(sparsematrix), intent(inout) :: sparse
    ! -- local
    integer(I4B) :: j, n
    integer(I4B) :: jj, jglo
    integer(I4B) :: nglo
    ! -- format
! ------------------------------------------------------------------------------
    !
    !
    ! -- Add package rows to sparse
    if (this%imatrows /= 0) then
      do n = 1, this%nlakes
        nglo = moffset + this%dis%nodes + this%ioffset + n
        call sparse%addconnection(nglo, nglo, 1)
        do j = this%lakptr%idxlakeconn(n), this%lakptr%idxlakeconn(n+1)-1
          jj = this%lakptr%cellid(j)
          jglo = jj + moffset
          call sparse%addconnection(nglo, jglo, 1)
          call sparse%addconnection(jglo, nglo, 1)
        end do
      end do
    end if
    !
    ! -- return
    return
  end subroutine lkt_ac

  subroutine lkt_mc(this, moffset, iasln, jasln)
! ******************************************************************************
! bnd_ac -- map package connection to matrix
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use SparseModule, only: sparsematrix
    use SimModule, only: store_error, ustop
    ! -- dummy
    class(GwtLktType),intent(inout) :: this
    integer(I4B), intent(in) :: moffset
    integer(I4B), dimension(:), intent(in) :: iasln
    integer(I4B), dimension(:), intent(in) :: jasln
    ! -- local
    integer(I4B) :: n, j, ii, jj, iglo, jglo
    integer(I4B) :: ipos
    ! -- format
! ------------------------------------------------------------------------------
    !
    !
    if (this%imatrows /= 0) then
      allocate(this%idxlocnode(this%nlakes))
      allocate(this%idxdglo(this%maxbound))
      allocate(this%idxoffdglo(this%maxbound))
      allocate(this%idxsymdglo(this%maxbound))
      allocate(this%idxsymoffdglo(this%maxbound))
      !
      ! -- Find the position of each connection in the global ia, ja structure
      !    and store them in idxglo.  idxglo allows this model to insert or
      !    retrieve values into or from the global A matrix
      ! -- maw rows
      ipos = 1
      do n = 1, this%nlakes
        iglo = moffset + this%dis%nodes + this%ioffset + n
        this%idxlocnode(n) = this%dis%nodes + this%ioffset + n
        do ii = this%lakptr%idxlakeconn(n), this%lakptr%idxlakeconn(n+1)-1
          j = this%lakptr%cellid(ii)
          jglo = j + moffset
          searchloop: do jj = iasln(iglo), iasln(iglo + 1) - 1
            if(jglo == jasln(jj)) then
              this%idxdglo(ipos) = iasln(iglo)
              this%idxoffdglo(ipos) = jj
              exit searchloop
            endif
          enddo searchloop
          ipos = ipos + 1
        end do
      end do
      ! -- lkt contributions gwf portion of global matrix
      ipos = 1
      do n = 1, this%nlakes
        do ii = this%lakptr%idxlakeconn(n), this%lakptr%idxlakeconn(n+1)-1
          iglo = this%lakptr%cellid(ii) + moffset
          jglo = moffset + this%dis%nodes + this%ioffset + n
          symsearchloop: do jj = iasln(iglo), iasln(iglo + 1) - 1
            if(jglo == jasln(jj)) then
              this%idxsymdglo(ipos) = iasln(iglo)
              this%idxsymoffdglo(ipos) = jj
              exit symsearchloop
            endif
          enddo symsearchloop
          ipos = ipos + 1
        end do
      end do
    else
      allocate(this%idxlocnode(0))
      allocate(this%idxdglo(0))
      allocate(this%idxoffdglo(0))
      allocate(this%idxsymdglo(0))
      allocate(this%idxsymoffdglo(0))
    endif
    !
    ! -- return
    return
  end subroutine lkt_mc

  subroutine lkt_ar(this)
! ******************************************************************************
! lkt_ar -- Allocate and Read
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use SimModule,           only: ustop, store_error
    use ConstantsModule,   only: LINELENGTH
    ! -- dummy
    class(GwtLktType), intent(inout) :: this
    ! -- local
    ! -- formats
    character(len=*), parameter :: fmtssm =                                    &
      "(1x,/1x,'LKT -- LAKE TRANSPORT PACKAGE, VERSION 1, 8/5/2019',           &
      &' INPUT READ FROM UNIT ', i0, //)"
! ------------------------------------------------------------------------------
    !
    ! -- Tell fmi that this package is being handled by LKT
    this%fmi%iatp(this%igwflakpak) = 1
    !
    ! -- Get obs setup 
    !call this%obs%obs_ar()
    !!
    !! -- find corresponding lak package
    !call this%find_lak_package()
    !!
    !! --print a message identifying the lkt package.
    !write(this%iout, fmtssm) this%inunit
    !!
    !! -- Set dimensions
    this%nlakes = this%lakptr%nlakes
    this%maxbound = this%lakptr%maxbound
    this%nbound = this%maxbound
    !!
    ! -- Allocate arrays
    call this%lkt_allocate_arrays()
    !
    ! -- read optional initial package parameters
    call this%read_initial_attr()
    !
    ! -- Return
    return
  end subroutine lkt_ar

  subroutine lkt_rp(this)
! ******************************************************************************
! lkt_rp
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(GwtLktType), intent(inout) :: this
    ! -- local
! ------------------------------------------------------------------------------
    !
    ! -- TODO: read initial packagedata
    this%nodelist(:) = this%lakptr%nodelist(:)
    !
    ! -- Return
    return
  end subroutine lkt_rp

  subroutine lkt_ad(this)
! ******************************************************************************
! lkt_ad
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(GwtLktType) :: this
    ! -- local
    integer(I4B) :: n
! ------------------------------------------------------------------------------
    !
    ! -- allocate scalars in NumericalPackageType
    do n = 1, this%nlakes
      this%xoldpak(n) = this%xnewpak(n)
    end do
    !
    ! -- Return
    return
  end subroutine lkt_ad

  subroutine lkt_fc(this, rhs, ia, idxglo, amatsln)
! ******************************************************************************
! lkt_fc
! ****************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(GwtLktType) :: this
    real(DP), dimension(:), intent(inout) :: rhs
    integer(I4B), dimension(:), intent(in) :: ia
    integer(I4B), dimension(:), intent(in) :: idxglo
    real(DP), dimension(:), intent(inout) :: amatsln
    ! -- local
! ------------------------------------------------------------------------------
    !
    ! -- Call fc depending on whether or not a matrix is expanded or not
    if (this%imatrows == 0) then
      call this%lkt_fc_nonexpanded(rhs, ia, idxglo, amatsln)
    else
      call this%lkt_fc_expanded(rhs, ia, idxglo, amatsln)
    end if
    ! -- Return
    return
  end subroutine lkt_fc

  subroutine lkt_fc_nonexpanded(this, rhs, ia, idxglo, amatsln)
! ******************************************************************************
! lkt_fc_nonexpanded -- formulate for the nonexpanded a matrix case
! ****************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(GwtLktType) :: this
    real(DP), dimension(:), intent(inout) :: rhs
    integer(I4B), dimension(:), intent(in) :: ia
    integer(I4B), dimension(:), intent(in) :: idxglo
    real(DP), dimension(:), intent(inout) :: amatsln
    ! -- local
    integer(I4B) :: n, j, igwfnode, idiag
! ------------------------------------------------------------------------------
    !
    ! -- solve for concentration in the lakes
    call this%lkt_solve()
    !
    ! -- add terms to the gwf matrix
    do n = 1, this%nlakes
      do j = this%lakptr%idxlakeconn(n), this%lakptr%idxlakeconn(n+1)-1
        igwfnode = this%lakptr%cellid(j)
        if (this%ibound(igwfnode) < 1) cycle
        idiag = idxglo(ia(igwfnode))
        amatsln(idiag) = amatsln(idiag) + this%hcof(j)
        rhs(igwfnode) = rhs(igwfnode) + this%rhs(j)
      end do
    end do
    !
    ! -- Return
    return
  end subroutine lkt_fc_nonexpanded

  subroutine lkt_fc_expanded(this, rhs, ia, idxglo, amatsln)
! ******************************************************************************
! lkt_fc_expanded -- formulate for the expanded a matrix case
! ****************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use TdisModule, only: delt
    ! -- dummy
    class(GwtLktType) :: this
    real(DP), dimension(:), intent(inout) :: rhs
    integer(I4B), dimension(:), intent(in) :: ia
    integer(I4B), dimension(:), intent(in) :: idxglo
    real(DP), dimension(:), intent(inout) :: amatsln
    ! -- local
    integer(I4B) :: j, n
    integer(I4B) :: idx
    integer(I4B) :: iloc, isymloc
    integer(I4B) :: iposd, iposoffd
    integer(I4B) :: isymnode
    integer(I4B) :: ipossymd, ipossymoffd
    real(DP) :: cold
    real(DP) :: qbnd
    real(DP) :: hlak0
    real(DP) :: hlak
    real(DP) :: v0
    real(DP) :: v1
    real(DP) :: omega
! ------------------------------------------------------------------------------
    !
    ! -- Add coefficients for LKT and GWF connections (qbnd positive into lake)
    idx = 1
    do n = 1, this%nlakes
      cold  = this%xoldpak(n)
      iloc = this%idxlocnode(n)
      iposd = this%idxdglo(idx)
      !
      ! -- mass storage in lak
      hlak0 = this%lakptr%xoldpak(n)
      hlak = this%lakptr%xnewpak(n)
      call this%lakptr%lak_calculate_vol(n, hlak0, v0)
      call this%lakptr%lak_calculate_vol(n, hlak, v1)
      amatsln(iposd) = amatsln(iposd) - v1 / delt
      rhs(iloc) = rhs(iloc) - cold * v0 / delt
      !
      ! -- todo: add or remove mass directly to and from lak (precip, evap, runoff, etc.)
      !
      ! -- go through each GWF connection
      do j = this%lakptr%idxlakeconn(n), this%lakptr%idxlakeconn(n+1)-1
        if (this%iboundpak(n) /= 0) then
          !
          ! -- set acoef and rhs to negative so they are relative to lkt and not gwt
          qbnd = this%lakptr%qleak(j)
          omega = DZERO
          if (qbnd < DZERO) omega = DONE
          !
          ! -- add to lkt row
          iposd = this%idxdglo(idx)
          iposoffd = this%idxoffdglo(idx)
          amatsln(iposd) = amatsln(iposd) + omega * qbnd
          amatsln(iposoffd) = amatsln(iposoffd) + (DONE - omega) * qbnd
          !
          ! -- add to gwf row for lkt connection
          isymnode = this%lakptr%cellid(j)
          isymloc = ia(isymnode)
          ipossymd = this%idxsymdglo(idx)
          ipossymoffd = this%idxsymoffdglo(idx)
          amatsln(ipossymd) = amatsln(ipossymd) - (DONE - omega) * qbnd
          amatsln(ipossymoffd) = amatsln(ipossymoffd) - omega * qbnd
        endif
        !
        ! -- increment lkt connection counter
        idx = idx + 1
      end do
    end do
        
    
    !
    ! -- Return
    return
  end subroutine lkt_fc_expanded

  subroutine lkt_cfupdate(this)
! ******************************************************************************
! lkt_cfupdate -- calculate package hcof and rhs so gwt budget is calculated
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(GwtLktType) :: this
    ! -- local
    integer(I4B) :: j, n
    integer(I4B) :: idx
    real(DP) :: qbnd
    real(DP) :: omega
! ------------------------------------------------------------------------------
    !
    ! -- Calculate hcof and rhs terms so GWF exchanges are calculated correctly
    idx = 1
    do n = 1, this%nlakes
      !
      ! -- go through each GWF connection
      do j = this%lakptr%idxlakeconn(n), this%lakptr%idxlakeconn(n+1)-1
        this%hcof(idx) = DZERO
        this%rhs(idx) = DZERO
        if (this%iboundpak(n) /= 0) then
          qbnd = this%lakptr%qleak(j)
          omega = DZERO
          if (qbnd < DZERO) omega = DONE
          this%hcof(idx) = - (DONE - omega) * qbnd
          this%rhs(idx) = omega * qbnd * this%xnewpak(n)
        endif
        !
        ! -- increment lkt connection counter
        idx = idx + 1
      end do
    end do
    !
    ! -- Return
    return
  end subroutine lkt_cfupdate

  subroutine lkt_bd(this, x, idvfl, icbcfl, ibudfl, icbcun, iprobs,            &
                    isuppress_output, model_budget, imap, iadv)
! ******************************************************************************
! lkt_bd -- Calculate Volumetric Budget for the lake
! Note that the compact budget will always be used.
! Subroutine: (1) Process each package entry
!             (2) Write output
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use TdisModule, only: kstp, kper, delt, pertim, totim
    use ConstantsModule, only: LENBOUNDNAME, DHNOFLO, DHDRY
    use BudgetModule, only: BudgetType
    use InputOutputModule, only: ulasav, ubdsv06
    ! -- dummy
    class(GwtLktType) :: this
    real(DP),dimension(:),intent(in) :: x
    integer(I4B), intent(in) :: idvfl
    integer(I4B), intent(in) :: icbcfl
    integer(I4B), intent(in) :: ibudfl
    integer(I4B), intent(in) :: icbcun
    integer(I4B), intent(in) :: iprobs
    integer(I4B), intent(in) :: isuppress_output
    type(BudgetType), intent(inout) :: model_budget
    integer(I4B), dimension(:), optional, intent(in) :: imap
    integer(I4B), optional, intent(in) :: iadv
    ! -- local
    integer(I4B) :: ibinun
    integer(I4B) :: n, j
    integer(I4B) :: igwfnode
    real(DP) :: c
    real(DP) :: rrate, rhs, hcof
    real(DP) :: ratein, rateout
    real(DP) :: hlak0, hlak
    real(DP) :: v0, v1
    ! -- for observations
    integer(I4B) :: iprobslocal
    integer(I4B) :: idx
    ! -- formats
! ------------------------------------------------------------------------------
    !
    ! -- update the lake hcof and rhs terms
    if (this%imatrows == 0) then
      call this%lkt_solve()
    else
      call this%lkt_cfupdate()
    end if
    !
    ! -- Suppress saving of simulated values; they
    !    will be saved at end of this procedure.
    iprobslocal = 0
    ! -- call base functionality in bnd_bd
    call this%BndType%bnd_bd(x, idvfl, icbcfl, ibudfl, icbcun, iprobslocal,    &
                             isuppress_output, model_budget)
    !
    ! -- lak budget routines (start by resetting)
    call this%budget%reset()
    !
    ! -- gwf term
    ratein = DZERO
    rateout = DZERO
    idx = 1
    do n = 1, this%nlakes
      do j = this%lakptr%idxlakeconn(n), this%lakptr%idxlakeconn(n+1)-1
        rrate = DZERO
        if (this%iboundpak(n) > 0) then
          igwfnode = this%lakptr%cellid(idx)
          rrate = this%hcof(idx) * x(igwfnode) - this%rhs(idx)
          rrate = -rrate  ! flip sign so relative to lake
        end if
        if(rrate < DZERO) then
          rateout = rateout - rrate
        else
          ratein = ratein + rrate
        endif
        idx = idx + 1
      end do
    end do
    call this%budget%addentry(ratein, rateout, delt,  &
                              this%clktbudget(1), isuppress_output)
    !
    ! -- storage term
    ratein = DZERO
    rateout = DZERO
    do n = 1, this%nlakes
      rrate = DZERO
      if (this%iboundpak(n) > 0) then
        hlak0 = this%lakptr%xoldpak(n)
        hlak = this%lakptr%xnewpak(n)
        call this%lakptr%lak_calculate_vol(n, hlak0, v0)
        call this%lakptr%lak_calculate_vol(n, hlak, v1)
        call lkt_calc_qsto(v0, v1, this%xoldpak(n), this%xnewpak(n), delt,     &
                          rhs, hcof, rrate)
      end if
      this%qsto(n) = rrate
      if(rrate < DZERO) then
        rateout = rateout - rrate
      else
        ratein = ratein + rrate
      endif
    end do
    call this%budget%addentry(ratein, rateout, delt,  &
                              this%clktbudget(2), isuppress_output)
    !
    ! -- todo: add terms to budget object
    
    !    
    ! -- todo: For continuous observations, save simulated values.
    !if (this%obs%npakobs > 0 .and. iprobs > 0) then
    !  call this%lak_bd_obs()
    !endif
    !
    ! -- set unit number for binary dependent variable output
    ibinun = 0
    if(this%iconcout /= 0) then
      ibinun = this%iconcout
    end if
    if(idvfl == 0) ibinun = 0
    if (isuppress_output /= 0) ibinun = 0
    !
    ! -- write lake binary output
    if (ibinun > 0) then
      do n = 1, this%nlakes
        c = this%xnewpak(n)
        !if (this%iboundpak(n) < 1) then
        !  v = DHNOFLO
        !end if
        this%dbuff(n) = c
      end do
      call ulasav(this%dbuff, '   CONCENTRATION', kstp, kper, pertim, totim,   &
                  this%nlakes, 1, 1, ibinun)
    end if
    !
    ! -- Set unit number for binary budget output
    ibinun = 0
    if(this%ibudgetout /= 0) then
      ibinun = this%ibudgetout
    end if
    if(icbcfl == 0) ibinun = 0
    if (isuppress_output /= 0) ibinun = 0
    !
    ! -- todo: write lake binary budget output
    !
    ! -- return
    return
  end subroutine lkt_bd

  subroutine lkt_calc_qsto(v0, v1, c0, c1, delt, rhs, hcof, rate)
    real(DP), intent(in) :: v0
    real(DP), intent(in) :: v1
    real(DP), intent(in) :: c0
    real(DP), intent(in) :: c1
    real(DP), intent(in) :: delt
    real(DP), intent(inout) :: rhs
    real(DP), intent(inout) :: hcof
    real(DP), intent(inout) :: rate
    hcof = - v1 / delt
    rhs = - c0 * v0 / delt
    rate = hcof * c1 - rhs
  end subroutine lkt_calc_qsto
  
  subroutine lkt_ot(this, kstp, kper, iout, ihedfl, ibudfl)
! ******************************************************************************
! lkt_ot
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use InputOutputModule, only: UWWORD
    ! -- dummy
    class(GwtLktType) :: this
    integer(I4B),intent(in) :: kstp
    integer(I4B),intent(in) :: kper
    integer(I4B),intent(in) :: iout
    integer(I4B),intent(in) :: ihedfl
    integer(I4B),intent(in) :: ibudfl
    ! -- local
    character(len=LINELENGTH) :: line, linesep
    character(len=16) :: text
    integer(I4B) :: n
    integer(I4B) :: iloc
    real(DP) :: q
    ! -- format
    character(len=*),parameter :: fmthdr = &
      "( 1X, ///1X, A, A, A, ' PERIOD ', I0, ' STEP ', I0)"
! ------------------------------------------------------------------------------
    !
    ! -- write lake stage
    if (ihedfl /= 0 .and. this%iprconc /= 0) then
      write (iout, fmthdr) 'LAKE (', trim(this%name), ') CONCENTRATION', kper, kstp
      iloc = 1
      line = ''
      if (this%inamedbound==1) then
        call UWWORD(line, iloc, 16, 1, 'lake', n, q, left=.TRUE.)
      end if
      call UWWORD(line, iloc, 6, 1, 'lake', n, q, CENTER=.TRUE.)
      call UWWORD(line, iloc, 11, 1, 'lake', n, q, CENTER=.TRUE.)
      ! -- create line separator
      linesep = repeat('-', iloc)
      ! -- write first line
      write(iout,'(1X,A)') linesep(1:iloc)
      write(iout,'(1X,A)') line(1:iloc)
      ! -- create second header line
      iloc = 1
      line = ''
      if (this%inamedbound==1) then
        call UWWORD(line, iloc, 16, 1, 'name', n, q, left=.TRUE.)
      end if
      call UWWORD(line, iloc, 6, 1, 'no.', n, q, CENTER=.TRUE.)
      call UWWORD(line, iloc, 11, 1, 'conc', n, q, CENTER=.TRUE.)
      ! -- write second line
      write(iout,'(1X,A)') line(1:iloc)
      write(iout,'(1X,A)') linesep(1:iloc)
      ! -- write data
      do n = 1, this%nlakes
        iloc = 1
        line = ''
        if (this%inamedbound==1) then
          call UWWORD(line, iloc, 16, 1, this%lakename(n), n, q, left=.TRUE.)
        end if
        call UWWORD(line, iloc, 6, 2, text, n, q)
        call UWWORD(line, iloc, 11, 3, text, n, this%xnewpak(n))
        write(iout, '(1X,A)') line(1:iloc)
      end do
    end if
    !
    ! -- Output maw budget
    call this%budget%budget_ot(kstp, kper, iout)
    !
    ! -- Return
    return
  end subroutine lkt_ot

  subroutine allocate_scalars(this)
! ******************************************************************************
! allocate_scalars
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryManagerModule, only: mem_allocate
    ! -- dummy
    class(GwtLktType) :: this
    ! -- local
! ------------------------------------------------------------------------------
    !
    ! -- allocate scalars in NumericalPackageType
    call this%BndType%allocate_scalars()
    !
    ! -- Allocate
    call mem_allocate(this%imatrows, 'IMATROWS', this%origin)
    call mem_allocate(this%iprconc, 'IPRCONC', this%origin)
    call mem_allocate(this%iconcout, 'ICONCOUT', this%origin)
    call mem_allocate(this%ibudgetout, 'IBUDGETOUT', this%origin)
    call mem_allocate(this%igwflakpak, 'IGWFLAKPAK', this%origin)
    call mem_allocate(this%nlakes, 'NLAKES', this%origin)
    call mem_allocate(this%bditems, 'BDITEMS', this%origin)
    !
    ! -- Initialize
    this%imatrows = 1
    this%iprconc = 0
    this%iconcout = 0
    this%ibudgetout = 0
    this%igwflakpak = 0
    this%nlakes = 0
    this%bditems = 3
    !
    ! -- Return
    return
  end subroutine allocate_scalars

  subroutine lkt_allocate_arrays(this)
! ******************************************************************************
! allocate_arrays
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryManagerModule, only: mem_allocate
    ! -- dummy
    class(GwtLktType), intent(inout) :: this
    ! -- local
    integer(I4B) :: n
! ------------------------------------------------------------------------------
    !
    ! -- call standard BndType allocate scalars
    call this%BndType%allocate_arrays()
    !    
    ! -- Allocate
    !
    ! -- allocate and initialize dbuff
    if (this%iconcout > 0) then
      call mem_allocate(this%dbuff, this%nlakes, 'DBUFF', this%origin)
      do n = 1, this%nlakes
        this%dbuff(n) = DZERO
      end do
    else
      call mem_allocate(this%dbuff, 0, 'DBUFF', this%origin)
    end if
    !
    ! -- allocate character array for budget text
    allocate(this%clktbudget(this%bditems))
    !
    ! -- allocate character array for status
    allocate(this%status(this%nlakes))
    !
    ! -- budget terms
    call mem_allocate(this%qsto, this%nlakes, 'QSTO', this%origin)
    !
    ! -- Initialize
    !
    !-- fill clktbudget
    this%clktbudget(1) = '             GWF'
    this%clktbudget(2) = '         STORAGE'
    this%clktbudget(3) = '        CONSTANT'
    !
    ! -- initialize arrays
    do n = 1, this%nlakes
      this%status(n) = 'ACTIVE'
      this%qsto(n) = DZERO
    end do
    !
    ! -- Return
    return
  end subroutine lkt_allocate_arrays
  
  subroutine find_lak_package(this)
! ******************************************************************************
! find corresponding lak package
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use SimModule,           only: ustop, store_error
    ! -- dummy
    class(GwtLktType) :: this
    ! -- local
    character(len=LINELENGTH) :: errmsg
    class(BndType), pointer :: packobj
    integer(I4B) :: ip
    logical :: found
! ------------------------------------------------------------------------------
    !
    ! -- Look through gwfbndlist for a LAK package with the same name as this
    !    LKT package name
    found = .false.
    do ip = 1, this%fmi%gwfbndlist%Count()
      packobj => GetBndFromList(this%fmi%gwfbndlist, ip)
      if (packobj%name == this%name) then
        found = .true.
        this%igwflakpak = ip
        select type (packobj)
          type is (LakType)
            this%lakptr => packobj
        end select
      end if
      if (found) exit
    end do
    !
    ! -- error if lak package not found
    if (.not. found) then
      write(errmsg, '(a)') '****ERROR. CORRESPONDING LAK PACKAGE NOT FOUND &
                            &FOR LKT.'
      call store_error(errmsg)
      call this%parser%StoreErrorUnit()
      call ustop()
    endif
    !
    ! -- Return
    return
  end subroutine find_lak_package

  subroutine lkt_options(this, option, found)
! ******************************************************************************
! lkt_options -- set options specific to GwtLktType
!
! lkt_options overrides BndType%bnd_options
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use ConstantsModule, only: MAXCHARLEN, DZERO
    use OpenSpecModule, only: access, form
    use SimModule, only: ustop, store_error
    use InputOutputModule, only: urword, getunit, openfile
    ! -- dummy
    class(GwtLktType), intent(inout) :: this
    character(len=*),  intent(inout) :: option
    logical,           intent(inout) :: found
    ! -- local
    character(len=MAXCHARLEN) :: fname, keyword
    ! -- formats
    character(len=*),parameter :: fmtlakeopt = &
      "(4x, 'LAKE ', a, ' VALUE (',g15.7,') SPECIFIED.')"
    character(len=*),parameter :: fmtlakbin = &
      "(4x, 'LAK ', 1x, a, 1x, ' WILL BE SAVED TO FILE: ', a, /4x, 'OPENED ON UNIT: ', I7)"
! ------------------------------------------------------------------------------
    !
    select case (option)
      case ('DEV_NONEXPANDING_MATRIX')
        ! -- use an iterative solution where lak concentration is not solved
        !    as part of the matrix.  It is instead solved separately with a 
        !    general mixing equation and then added to the RHS of the GWT 
        !    equations
        call this%parser%DevOpt()
        this%imatrows = 0
        write(this%iout,'(4x,a)') &
          ' LKT WILL NOT ADD ADDITIONAL ROWS TO THE A MATRIX.'
        found = .true.
      case ('PRINT_CONCENTRATION')
        this%iprconc = 1
        write(this%iout,'(4x,a)') trim(adjustl(this%text))// &
          ' CONCENTRATIONS WILL BE PRINTED TO LISTING FILE.'
        found = .true.
      case('CONCENTRATION')
        call this%parser%GetStringCaps(keyword)
        if (keyword == 'FILEOUT') then
          call this%parser%GetString(fname)
          this%iconcout = getunit()
          call openfile(this%iconcout, this%iout, fname, 'DATA(BINARY)',  &
                       form, access, 'REPLACE')
          write(this%iout,fmtlakbin) 'CONCENTRATION', fname, this%iconcout
          found = .true.
        else
          call store_error('OPTIONAL CONCENTRATION KEYWORD MUST BE FOLLOWED BY FILEOUT')
        end if
      case('BUDGET')
        call this%parser%GetStringCaps(keyword)
        if (keyword == 'FILEOUT') then
          call this%parser%GetString(fname)
          this%ibudgetout = getunit()
          call openfile(this%ibudgetout, this%iout, fname, 'DATA(BINARY)',  &
                        form, access, 'REPLACE')
          write(this%iout,fmtlakbin) 'BUDGET', fname, this%ibudgetout
          found = .true.
        else
          call store_error('OPTIONAL BUDGET KEYWORD MUST BE FOLLOWED BY FILEOUT')
        end if
      case default
        !
        ! -- No options found
        found = .false.
    end select
    !
    ! -- return
    return
  end subroutine lkt_options

  subroutine lkt_read_dimensions(this)
! ******************************************************************************
! pak1read_dimensions -- Read the dimensions for this package
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use ConstantsModule, only: LINELENGTH
    use SimModule, only: ustop, store_error, count_errors
    ! -- dummy
    class(GwtLktType),intent(inout) :: this
    ! -- local
    character(len=LINELENGTH) :: errmsg
    character(len=LINELENGTH) :: keyword
    integer(I4B) :: ierr
    ! -- format
! ------------------------------------------------------------------------------
    !
    ! -- Set a pointer from this%lakptr to the GWF LAK Package
    call this%find_lak_package()
    !
    ! -- Set dimensions from the GWF LAK package
    this%nlakes = this%lakptr%nlakes
    this%maxbound = this%lakptr%maxbound
    this%nbound = this%maxbound
    if (this%imatrows /= 0) this%npakeq = this%nlakes
    write(this%iout,'(4x,a,i7)')'LKT NLAKES = ', this%nlakes
    write(this%iout,'(4x,a,i7)')'LKT MAXBOUND = ', this%maxbound
    if (this%nlakes < 0) then
      write(errmsg, '(1x,a)') &
        'ERROR:  NUMBER OF LAKES COULD NOT BE DETERMINED CORRECTLY.'
      call store_error(errmsg)
    end if
    !
    ! -- stop if errors were encountered in the DIMENSIONS block
    ierr = count_errors()
    if (ierr > 0) then
      call ustop()
    end if
    !
    ! -- read packagedata block
    call this%lkt_read_lakes()
    !
    ! -- Call define_listlabel to construct the list label that is written
    !    when PRINT_INPUT option is used.
    call this%define_listlabel()
    !
    ! -- return
    return
  end subroutine lkt_read_dimensions

  subroutine lkt_read_lakes(this)
! ******************************************************************************
! pak1read_dimensions -- Read feature infromation for this package
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use ConstantsModule, only: LINELENGTH
    use MemoryManagerModule, only: mem_allocate
    use SimModule, only: ustop, store_error, count_errors, store_error_unit
    use TimeSeriesManagerModule, only: read_single_value_or_time_series
    ! -- dummy
    class(GwtLktType),intent(inout) :: this
    ! -- local
    character(len=LINELENGTH) :: errmsg
    character(len=LINELENGTH) :: text
    character(len=LENBOUNDNAME) :: bndName, bndNameTemp
    character(len=9) :: cno
    character(len=50), dimension(:), allocatable :: caux
    integer(I4B) :: ierr
    logical :: isfound, endOfBlock
    integer(I4B) :: n
    integer(I4B) :: ii, jj
    integer(I4B) :: iaux
    integer(I4B) :: itmp
    integer(I4B) :: nlak
    integer(I4B) :: nconn
    integer(I4B), dimension(:), pointer, contiguous :: nboundchk
    ! -- format
    !
    ! -- code
    !
    ! -- initialize itmp
    itmp = 0
    !
    ! -- allocate lake data
    call mem_allocate(this%strt, this%nlakes, 'STRT', this%origin)
    !call mem_allocate(this%rainfall, this%nlakes, 'RAINFALL', this%origin)
    !call mem_allocate(this%evaporation, this%nlakes, 'EVAPORATION', this%origin)
    !call mem_allocate(this%runoff, this%nlakes, 'RUNOFF', this%origin)
    !call mem_allocate(this%inflow, this%nlakes, 'INFLOW', this%origin)
    !call mem_allocate(this%withdrawal, this%nlakes, 'WITHDRAWAL', this%origin)
    call mem_allocate(this%lauxvar, this%naux*this%nlakes, 'LAUXVAR', this%origin)
    !
    ! -- lake boundary and concentrations
    if (this%imatrows == 0) then
      call mem_allocate(this%iboundpak, this%nlakes, 'IBOUND', this%origin)
      call mem_allocate(this%xnewpak, this%nlakes, 'XNEWPAK', this%origin)
    end if
    call mem_allocate(this%xoldpak, this%nlakes, 'XOLDPAK', this%origin)
    !
    ! -- allocate character storage not managed by the memory manager
    allocate(this%lakename(this%nlakes)) ! ditch after boundnames allocated??
    !allocate(this%status(this%nlakes))
    !
    do n = 1, this%nlakes
      !this%status(n) = 'ACTIVE'
      this%strt(n) = DEP20
      this%xoldpak(n) = DEP20
      if (this%imatrows == 0) then
        this%iboundpak(n) = 1
        this%xnewpak(n) = DEP20
      end if
    end do
    !
    ! -- allocate local storage for aux variables
    if (this%naux > 0) then
      allocate(caux(this%naux))
    end if
    !
    ! -- allocate and initialize temporary variables
    allocate(nboundchk(this%nlakes))
    do n = 1, this%nlakes
      nboundchk(n) = 0
    end do
    !
    ! -- read lake well data
    ! -- get lakes block
    call this%parser%GetBlock('PACKAGEDATA', isfound, ierr, supportOpenClose=.true.)
    !
    ! -- parse locations block if detected
    if (isfound) then
      write(this%iout,'(/1x,a)')'PROCESSING '//trim(adjustl(this%text))// &
        ' PACKAGEDATA'
      nlak = 0
      nconn = 0
      do
        call this%parser%GetNextLine(endOfBlock)
        if (endOfBlock) exit
        n = this%parser%GetInteger()

        if (n < 1 .or. n > this%nlakes) then
          write(errmsg,'(4x,a,1x,i6)') &
            '****ERROR. lakeno MUST BE > 0 and <= ', this%nlakes
          call store_error(errmsg)
          cycle
        end if
        
        ! -- increment nboundchk
        nboundchk(n) = nboundchk(n) + 1

        ! -- strt
        this%strt(n) = this%parser%GetDouble()

        ! -- get aux data
        do iaux = 1, this%naux
          call this%parser%GetString(caux(iaux))
        end do

        ! -- set default bndName
        write (cno,'(i9.9)') n
        bndName = 'Lake' // cno

        ! -- lakename
        if (this%inamedbound /= 0) then
          call this%parser%GetStringCaps(bndNameTemp)
          if (bndNameTemp /= '') then
            bndName = bndNameTemp(1:16)
          endif
        end if
        this%lakename(n) = bndName

        ! -- fill time series aware data
        ! -- fill aux data
        do iaux = 1, this%naux
          !
          ! -- Assign boundary name
          if (this%inamedbound==1) then
            bndName = this%lakename(n)
          else
            bndName = ''
          end if
          text = caux(iaux)
          jj = 1 !iaux
          ii = (n-1) * this%naux + iaux
          call read_single_value_or_time_series(text, &
                                                this%lauxvar(ii)%value, &
                                                this%lauxvar(ii)%name, &
                                                DZERO,  &
                                                this%Name, 'AUX', this%TsManager, &
                                                this%iprpak, n, jj, &
                                                this%auxname(iaux), &
                                                bndName, this%parser%iuactive)
        end do

        nlak = nlak + 1
      end do
      !
      ! -- check for duplicate or missing lakes
      do n = 1, this%nlakes
        if (nboundchk(n) == 0) then
          write(errmsg,'(a,1x,i0)')  'ERROR.  NO DATA SPECIFIED FOR LAKE', n
          call store_error(errmsg)
        else if (nboundchk(n) > 1) then
          write(errmsg,'(a,1x,i0,1x,a,1x,i0,1x,a)')                             &
            'ERROR.  DATA FOR LAKE', n, 'SPECIFIED', nboundchk(n), 'TIMES'
          call store_error(errmsg)
        end if
      end do

      write(this%iout,'(1x,a)')'END OF '//trim(adjustl(this%text))//' PACKAGEDATA'
    else
      call store_error('ERROR.  REQUIRED PACKAGEDATA BLOCK NOT FOUND.')
    end if
    !
    ! -- terminate if any errors were detected
    if (count_errors() > 0) then
      call this%parser%StoreErrorUnit()
      call ustop()
    end if
    !
    ! -- deallocate local storage for aux variables
    if (this%naux > 0) then
      deallocate(caux)
    end if
    !
    ! -- deallocate local storage for nboundchk
    deallocate(nboundchk)
    !
    ! -- return
    return
  end subroutine lkt_read_lakes
  
  subroutine lkt_read_initial_attr(this)
! ******************************************************************************
! lkt_read_initial_attr -- Read the initial parameters for this package
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use ConstantsModule, only: LINELENGTH
    use SimModule, only: ustop, store_error, count_errors
    use BudgetModule, only: budget_cr
    use TimeSeriesManagerModule, only: read_single_value_or_time_series
    ! -- dummy
    class(GwtLktType),intent(inout) :: this
    ! -- local
    !character(len=LINELENGTH) :: text
    integer(I4B) :: j, n
    !integer(I4B) :: nn
    !integer(I4B) :: idx
    integer(I4B) :: ival
    !real(DP) :: endtim
    !real(DP) :: top
    !real(DP) :: bot
    !real(DP) :: k
    !real(DP) :: area
    !real(DP) :: length
    !real(DP) :: s
    !real(DP) :: dx
    !real(DP) :: c
    !real(DP) :: sa
    !real(DP) :: wa
    !real(DP) :: v
    !real(DP) :: fact
    !real(DP) :: c1
    !real(DP) :: c2
    !real(DP), allocatable, dimension(:) :: clb, caq
    !character (len=14) :: cbedleak
    !character (len=14) :: cbedcond
    !character (len=10), dimension(0:3) :: ctype
    !character (len=15) :: nodestr
    !!data
    !data ctype(0) /'VERTICAL  '/
    !data ctype(1) /'HORIZONTAL'/
    !data ctype(2) /'EMBEDDEDH '/
    !data ctype(3) /'EMBEDDEDV '/
    ! -- format
! ------------------------------------------------------------------------------

    ! -- setup the lake budget
    call budget_cr(this%budget, this%origin)
    ival = this%bditems
    call this%budget%budget_df(ival, this%name, 'M')
    !
    ! -- initialize xnewpak and set lake concentration
    do n = 1, this%nlakes
      this%xnewpak(n) = this%strt(n)
      !write(text,'(g15.7)') this%strt(n)
      !endtim = DZERO
      !jj = 1    ! For STAGE
      !call read_single_value_or_time_series(text, &
      !                                      this%stage(n)%value, &
      !                                      this%stage(n)%name, &
      !                                      endtim,  &
      !                                      this%name, 'BND', this%TsManager, &
      !                                      this%iprpak, n, jj, 'STAGE', &
      !                                      this%lakename(n), this%inunit)

    end do
    !
    ! -- initialize status (iboundpak) of lakes to active
    do n = 1, this%nlakes
      if (this%status(n) == 'CONSTANT') then
        this%iboundpak(n) = -1
      else if (this%status(n) == 'INACTIVE') then
        this%iboundpak(n) = 0
      else if (this%status(n) == 'ACTIVE ') then
        this%iboundpak(n) = 1
      end if
    end do
    !
    ! -- set boundname for each connection
    if (this%inamedbound /= 0) then
      do n = 1, this%nlakes
        do j = this%lakptr%idxlakeconn(n), this%lakptr%idxlakeconn(n+1)-1
          this%boundname(j) = this%lakename(n)
        end do
      end do
    endif
    !
    ! -- return
    return
  end subroutine lkt_read_initial_attr

  subroutine lkt_solve(this)
! ******************************************************************************
! lkt_solve -- solve for concentration in the lakes
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use ConstantsModule,   only: LINELENGTH
    use SimModule,         only: ustop, store_error, count_errors
    use TdisModule,        only: delt
    ! -- dummy
    class(GwtLktType) :: this
    ! -- local
    integer(I4B) :: n, j, igwfnode
    real(DP) :: c1, ctmp, qbnd, v0, v1, hlak0, hlak
! ------------------------------------------------------------------------------
    !
    do n = 1, this%nlakes
      !
      ! -- mass storage from previous time step
      hlak0 = this%lakptr%xoldpak(n)
      hlak = this%lakptr%xnewpak(n)
      call this%lakptr%lak_calculate_vol(n, hlak0, v0)
      call this%lakptr%lak_calculate_vol(n, hlak, v1)
      c1 = this%xoldpak(n) * v0
      !
      ! -- mass flux from groundwater flow (qleak sign is relative to lak)
      do j = this%lakptr%idxlakeconn(n), this%lakptr%idxlakeconn(n + 1) - 1
        this%hcof(j) = DZERO
        this%rhs(j) = DZERO
        igwfnode = this%lakptr%cellid(j)
        qbnd = this%lakptr%qleak(j)
        if (qbnd <= DZERO) then
          ctmp = this%xnewpak(n)
          this%rhs(j) = qbnd * ctmp
        else
          ctmp = this%xnew(igwfnode)
          this%hcof(j) = -qbnd
        end if
        c1 = c1 + qbnd * ctmp * delt
      end do
      !
      ! -- divide by new volume and store concentration
      c1 = c1 / v1
      this%xnewpak(n) = c1
    end do
    !
    ! -- Return
    return
  end subroutine lkt_solve
  
  subroutine define_listlabel(this)
! ******************************************************************************
! define_listlabel -- Define the list heading that is written to iout when
!   PRINT_INPUT option is used.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    class(GwtLktType), intent(inout) :: this
! ------------------------------------------------------------------------------
    !
    ! -- create the header list label
    this%listlabel = trim(this%filtyp) // ' NO.'
    if(this%dis%ndim == 3) then
      write(this%listlabel, '(a, a7)') trim(this%listlabel), 'LAYER'
      write(this%listlabel, '(a, a7)') trim(this%listlabel), 'ROW'
      write(this%listlabel, '(a, a7)') trim(this%listlabel), 'COL'
    elseif(this%dis%ndim == 2) then
      write(this%listlabel, '(a, a7)') trim(this%listlabel), 'LAYER'
      write(this%listlabel, '(a, a7)') trim(this%listlabel), 'CELL2D'
    else
      write(this%listlabel, '(a, a7)') trim(this%listlabel), 'NODE'
    endif
    write(this%listlabel, '(a, a16)') trim(this%listlabel), 'STRESS RATE'
    if(this%inamedbound == 1) then
      write(this%listlabel, '(a, a16)') trim(this%listlabel), 'BOUNDARY NAME'
    endif
    !
    ! -- return
    return
  end subroutine define_listlabel

  subroutine lkt_set_pointers(this, neq, ibound, xnew, xold, flowja)
! ******************************************************************************
! set_pointers -- Set pointers to model arrays and variables so that a package
!                 has access to these things.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    class(GwtLktType) :: this
    integer(I4B), pointer :: neq
    integer(I4B), dimension(:), pointer, contiguous :: ibound
    real(DP), dimension(:), pointer, contiguous :: xnew
    real(DP), dimension(:), pointer, contiguous :: xold
    real(DP), dimension(:), pointer, contiguous :: flowja
    ! -- local
    integer(I4B) :: istart, iend
! ------------------------------------------------------------------------------
    !
    ! -- call base BndType set_pointers
    call this%BndType%set_pointers(neq, ibound, xnew, xold, flowja)
    !
    ! -- Set the LKT pointers
    !
    ! -- set package pointers
    if (this%imatrows /= 0) then
      istart = this%dis%nodes + this%ioffset + 1
      iend = istart + this%nlakes - 1
      this%iboundpak => this%ibound(istart:iend)
      this%xnewpak => this%xnew(istart:iend)
    end if
    !
    ! -- return
  end subroutine lkt_set_pointers
  
  
end module GwtLktModule