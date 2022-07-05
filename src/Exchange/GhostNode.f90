module GhostNodeModule

  use KindModule, only: DP, I4B
  use ConstantsModule, only: LINELENGTH
  use NumericalModelModule, only: NumericalModelType
  use NumericalPackageModule, only: NumericalPackageType
  use BlockParserModule, only: BlockParserType

  implicit none

  private
  public :: GhostNodeType
  public :: gnc_cr

  type, extends(NumericalPackageType) :: GhostNodeType
    logical, pointer :: smgnc => null() ! single model gnc
    logical, pointer :: implicit => null() ! lhs or rhs
    logical, pointer :: i2kn => null() ! not used
    integer(I4B), pointer :: nexg => null() ! number of gncs
    integer(I4B), pointer :: numjs => null() ! number of connecting nodes
    class(NumericalModelType), pointer :: m1 => null() ! pointer to model 1
    class(NumericalModelType), pointer :: m2 => null() ! pointer to model 2
    integer(I4B), dimension(:), pointer, contiguous :: nodem1 => null() ! array of nodes in model 1
    integer(I4B), dimension(:), pointer, contiguous :: nodem2 => null() ! array of nodes in model 2
    integer(I4B), dimension(:, :), pointer, contiguous :: nodesj => null() ! array of interpolation nodes
    real(DP), dimension(:), pointer, contiguous :: cond => null() ! array of conductance
    integer(I4B), dimension(:), pointer, contiguous :: idxglo => null() ! connection position in amat
    integer(I4B), dimension(:), pointer, contiguous :: idxsymglo => null() ! symmetric position in amat
    real(DP), dimension(:, :), pointer, contiguous :: alphasj => null() ! interpolation factors
    integer(I4B), dimension(:), pointer, contiguous :: idiagn => null() ! amat diagonal position of n
    integer(I4B), dimension(:), pointer, contiguous :: idiagm => null() ! amat diagonal position of m
    integer(I4B), dimension(:, :), pointer, contiguous :: jposinrown => null() ! amat j position in row n
    integer(I4B), dimension(:, :), pointer, contiguous :: jposinrowm => null() ! amat j position in row m
  contains
    procedure :: gnc_df
    procedure :: gnc_ac
    procedure :: gnc_mc
    procedure, private :: gnc_fmsav
    procedure :: gnc_fc
    procedure :: gnc_fn
    procedure :: gnc_cq
    procedure :: gnc_ot
    procedure :: gnc_da
    procedure :: deltaQgnc
    procedure :: allocate_scalars
    procedure, private :: allocate_arrays
    procedure, private :: read_options
    procedure, private :: read_dimensions
    procedure, private :: read_data
    procedure, private :: nodeu_to_noder
  end type GhostNodeType

contains

  subroutine gnc_cr(gncobj, name_parent, inunit, iout)
! ******************************************************************************
! gnc_cr -- Create new GNC exchange object
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    type(GhostNodeType), pointer, intent(inout) :: gncobj
    character(len=*), intent(in) :: name_parent
    integer(I4B), intent(in) :: inunit
    integer(I4B), intent(in) :: iout
! ------------------------------------------------------------------------------
    !
    ! -- Allocate the gnc exchange object
    allocate (gncobj)
    !
    ! -- create name and memory path. name_parent will either be model name or the
    !    exchange name.
    call gncobj%set_names(1, name_parent, 'GNC', 'GNC')
    !
    ! -- allocate scalars
    call gncobj%allocate_scalars()
    !
    ! -- Set variables
    gncobj%inunit = inunit
    gncobj%iout = iout
    !
    ! -- return
    return
  end subroutine gnc_cr

  subroutine gnc_df(this, m1, m2)
! ******************************************************************************
! gnc_df -- Initialize a gnc object.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use NumericalModelModule, only: NumericalModelType
    use SimModule, only: store_error, store_error_unit
    ! -- dummy
    class(GhostNodeType) :: this
    class(NumericalModelType), target :: m1
    class(NumericalModelType), target, optional :: m2
    ! -- local
    character(len=LINELENGTH) :: errmsg
! ------------------------------------------------------------------------------
    !
    ! -- Point or set attributes
    this%m1 => m1
    this%m2 => m1
    !
    ! -- If m2 is present, then GNC spans two models
    if (present(m2)) then
      this%m2 => m2
      this%smgnc = .false.
    end if
    !
    ! -- Initialize block parser
    call this%parser%Initialize(this%inunit, this%iout)
    !
    ! -- read gnc options
    call this%read_options()
    !
    ! -- read gnc dimensions
    call this%read_dimensions()
    !
    ! -- allocate arrays
    call this%allocate_arrays()
    !
    ! -- Allocate and read the gnc entries
    call this%read_data()
    !
    ! -- Trap for implicit gnc but models are in different solutions
    if (this%m1%idsoln /= this%m2%idsoln) then
      if (this%implicit) then
        write (errmsg, '(a)') 'Error.  GNC is implicit but models are in '// &
          'different solutions.'
        call store_error(errmsg)
        call store_error_unit(this%inunit)
      end if
    end if
    !
    ! -- return
    return
  end subroutine gnc_df

  subroutine gnc_ac(this, sparse)
! ******************************************************************************
! gnc_ac -- Single or Two-Model GNC Add Connections
! Subroutine: (1) For implicit GNC, expand the sparse solution matrix
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use SparseModule, only: sparsematrix
    ! -- dummy
    class(GhostNodeType) :: this
    type(sparsematrix), intent(inout) :: sparse
    ! -- local
    integer(I4B) :: ignc, jidx, noden, nodem, nodej
! ------------------------------------------------------------------------------
    !
    ! -- Expand the sparse matrix for ghost node connections.  No need to add
    !    connection between n and m as they must be connected some other way
    !    that will calculate the conductance.
    if (this%implicit) then
      do ignc = 1, this%nexg
        noden = this%nodem1(ignc) + this%m1%moffset
        nodem = this%nodem2(ignc) + this%m2%moffset
        jloop: do jidx = 1, this%numjs
          nodej = this%nodesj(jidx, ignc)
          if (nodej == 0) cycle
          nodej = nodej + this%m1%moffset
          call sparse%addconnection(nodem, nodej, 1)
          call sparse%addconnection(nodej, nodem, 1)
          call sparse%addconnection(noden, nodej, 1)
          call sparse%addconnection(nodej, noden, 1)
        end do jloop
      end do
    end if
    !
    ! -- return
    return
  end subroutine gnc_ac

  subroutine gnc_mc(this, iasln, jasln)
! ******************************************************************************
! gnc_mc -- Single or Two-Model GNC Map Connections
! Subroutine: (1) Fill the following mapping arrays:
!                 this%idiagn, this%idiagm (diagonal positions in solution amat)
!                 this%idxglo (nm connection in solution amat)
!                 this%idxsymglo (mn connection in solution amat)
!                 this%jposinrown (position of j in row n of solution amat)
!                 this%jposinrowm (position of j in row m of solution amat)
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use SimModule, only: store_error, store_error_unit, count_errors
    ! -- dummy
    class(GhostNodeType) :: this
    integer(I4B), dimension(:), intent(in) :: iasln
    integer(I4B), dimension(:), intent(in) :: jasln
    ! -- local
    character(len=LINELENGTH) :: errmsg
    integer(I4B) :: noden, nodem, ipos, j, ignc, jidx, nodej
    ! -- formats
    character(len=*), parameter :: fmterr = &
      "('GHOST NODE ERROR.  Cell ', i0, ' in model ', a, &
        &' is not connected to cell ', i0, ' in model ', a)"
! ------------------------------------------------------------------------------
    !
    ! -- Find the location of Cnm in the global solution and store it in
    !    this%idxglo
    do ignc = 1, this%nexg
      noden = this%nodem1(ignc) + this%m1%moffset
      nodem = this%nodem2(ignc) + this%m2%moffset
      !
      ! -- store diagonal positions in idiagn and idiagm
      this%idiagn(ignc) = iasln(noden)
      this%idiagm(ignc) = iasln(nodem)
      !if(this%implicit) then
      !  this%idiagn(ignc) = iasln(noden)
      !  this%idiagm(ignc) = iasln(nodem)
      !endif
      !
      ! -- find location of m in row n of global solution
      this%idxglo(ignc) = 0
      searchloopnm: do ipos = iasln(noden) + 1, iasln(noden + 1) - 1
        j = jasln(ipos)
        if (j == nodem) then
          this%idxglo(ignc) = ipos
          exit searchloopnm
        end if
      end do searchloopnm
      !
      ! -- find location of n in row m of global solution and store in idxsymglo
      this%idxsymglo(ignc) = 0
      searchloopmn: do ipos = iasln(nodem), iasln(nodem + 1) - 1
        j = jasln(ipos)
        if (j == noden) then
          this%idxsymglo(ignc) = ipos
          exit searchloopmn
        end if
      end do searchloopmn
      !
      ! -- Check to make sure idxglo is non-zero
      if (this%idxglo(ignc) == 0) then
        write (errmsg, fmterr) this%nodem1(ignc), trim(this%m1%name), &
          this%nodem2(ignc), trim(this%m2%name)
        call store_error(errmsg)
      end if
      !
    end do
    !
    ! -- Stop if errors
    if (count_errors() > 0) then
      call store_error_unit(this%inunit)
    end if
    !
    ! -- find locations of j in rows n and row m of global solution
    if (this%implicit) then
      do ignc = 1, this%nexg
        noden = this%nodem1(ignc) + this%m1%moffset
        nodem = this%nodem2(ignc) + this%m2%moffset
        !
        do jidx = 1, this%numjs
          nodej = this%nodesj(jidx, ignc)
          if (nodej > 0) nodej = nodej + this%m1%moffset
          !
          ! -- search for nodej in row n, unless it is 0
          if (nodej == 0) then
            ipos = 0
            this%jposinrown(jidx, ignc) = ipos
          else
            searchloopn: do ipos = iasln(noden), iasln(noden + 1) - 1
              j = jasln(ipos)
              if (j == nodej) then
                this%jposinrown(jidx, ignc) = ipos
                exit searchloopn
              end if
            end do searchloopn
          end if
          !
          ! -- search for nodej in row m
          if (nodej == 0) then
            ipos = 0
            this%jposinrowm(jidx, ignc) = ipos
          else
            searchloopm: do ipos = iasln(nodem) + 1, iasln(nodem + 1) - 1
              j = jasln(ipos)
              if (j == nodej) then
                this%jposinrowm(jidx, ignc) = ipos
                exit searchloopm
              end if
            end do searchloopm
          end if
        end do
      end do
    end if
    !
    ! -- return
    return
  end subroutine gnc_mc

  subroutine gnc_fmsav(this, kiter, amatsln)
! ******************************************************************************
! gnc_fmsav -- Store the n-m Picard conductance in cond prior to the Newton
!   terms being added.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use ConstantsModule, only: DZERO
    ! -- dummy
    class(GhostNodeType) :: this
    integer(I4B), intent(in) :: kiter
    real(DP), dimension(:), intent(inout) :: amatsln
    ! -- local
    integer(I4B) :: ignc, ipos
    real(DP) :: cond
! ------------------------------------------------------------------------------
    !
    ! -- An ipos value of zero indicates that noden is not connected to
    !    nodem, and therefore the conductance is zero.
    gncloop: do ignc = 1, this%nexg
      ipos = this%idxglo(ignc)
      if (ipos > 0) then
        cond = amatsln(ipos)
      else
        cond = DZERO
      end if
      this%cond(ignc) = cond
    end do gncloop
    !
    ! -- return
    return
  end subroutine gnc_fmsav

  subroutine gnc_fc(this, kiter, amatsln)
! ******************************************************************************
! gnc_fc -- Fill matrix terms
! Subroutine: (1) Add the GNC terms to the solution amat or model rhs depending
!                 on whether GNC is implicit or explicit
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use ConstantsModule, only: DZERO
    ! -- dummy
    class(GhostNodeType) :: this
    integer(I4B), intent(in) :: kiter
    real(DP), dimension(:), intent(inout) :: amatsln
    ! -- local
    integer(I4B) :: ignc, j, noden, nodem, ipos, jidx, iposjn, iposjm
    real(DP) :: cond, alpha, aterm, rterm
! ------------------------------------------------------------------------------
    !
    ! -- If this is a single model gnc (not an exchange across models), then
    !    pull conductances out of amatsln and store them in this%cond
    if (this%smgnc) call this%gnc_fmsav(kiter, amatsln)
    !
    ! -- Add gnc terms to rhs or to amat depending on whether gnc is implicit
    !    or explicit
    gncloop: do ignc = 1, this%nexg
      noden = this%nodem1(ignc)
      nodem = this%nodem2(ignc)
      if (this%m1%ibound(noden) == 0 .or. &
          this%m2%ibound(nodem) == 0) cycle gncloop
      ipos = this%idxglo(ignc)
      cond = this%cond(ignc)
      jloop: do jidx = 1, this%numjs
        j = this%nodesj(jidx, ignc)
        if (j == 0) cycle
        alpha = this%alphasj(jidx, ignc)
        if (alpha == DZERO) cycle
        aterm = alpha * cond
        if (this%implicit) then
          iposjn = this%jposinrown(jidx, ignc)
          iposjm = this%jposinrowm(jidx, ignc)
          amatsln(this%idiagn(ignc)) = amatsln(this%idiagn(ignc)) + aterm
          amatsln(iposjn) = amatsln(iposjn) - aterm
          amatsln(this%idxsymglo(ignc)) = amatsln(this%idxsymglo(ignc)) - aterm
          amatsln(iposjm) = amatsln(iposjm) + aterm
        else
          rterm = aterm * (this%m1%x(noden) - this%m1%x(j))
          this%m1%rhs(noden) = this%m1%rhs(noden) - rterm
          this%m2%rhs(nodem) = this%m2%rhs(nodem) + rterm
        end if
      end do jloop
    end do gncloop
    !
    ! -- return
    return
  end subroutine gnc_fc

  subroutine gnc_fn(this, kiter, njasln, amatsln, condsat, ihc_opt, &
                    ivarcv_opt, ictm1_opt, ictm2_opt)
! ******************************************************************************
! gnc_fn -- Fill GNC Newton terms
!
!  Required arguments:
!   kiter : outer iteration number
!   njasln : size of amatsln
!   amatsln : coefficient matrix for the solution
!   condsat is of size(njas) if single model, otherwise nexg
!
!  Optional arguments:
!    ihc_opt : an optional vector of size(nexg), which contains a horizontal
!      connection code (0=vertical, 1=horizontal, 2=vertically staggered)
!    ivarcv_opt : variable vertical conductance flag (default is 0)
!    ictm1_opt : icelltype for model 1 integer vector (default is 1)
!    ictm2_opt : icelltype for model 2 integer vector (default is 1)
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use ConstantsModule, only: DZERO
    use SmoothingModule, only: sQuadraticSaturationDerivative
    ! -- dummy
    class(GhostNodeType) :: this
    integer(I4B) :: kiter
    integer(I4B), intent(in) :: njasln
    real(DP), dimension(njasln), intent(inout) :: amatsln
    real(DP), dimension(:), intent(in) :: condsat
    integer(I4B), dimension(:), optional :: ihc_opt
    integer(I4B), optional :: ivarcv_opt
    integer(I4B), dimension(:), optional :: ictm1_opt
    integer(I4B), dimension(:), optional :: ictm2_opt
    ! -- local
    integer(I4B) :: ignc, jidx, ipos, isympos, ihc, ivarcv
    integer(I4B) :: nodej, noden, nodem
    integer(I4B) :: iups, ictup
    real(DP) :: csat, alpha, consterm, term, derv
    real(DP) :: xup, topup, botup
! ------------------------------------------------------------------------------
    !
    ! -- Set the ivarcv to indicate whether or not the vertical conductance
    !    is a function of water table
    ivarcv = 0
    if (present(ivarcv_opt)) ivarcv = ivarcv_opt
    !
    gncloop: do ignc = 1, this%nexg
      noden = this%nodem1(ignc)
      nodem = this%nodem2(ignc)
      if (this%m1%ibound(noden) == 0 .or. &
          this%m2%ibound(nodem) == 0) cycle gncloop
      !
      ! -- Assign variables depending on whether single model gnc or exchange
      !    gnc
      if (this%smgnc) then
        ipos = this%m1%dis%con%getjaindex(noden, nodem)
        isympos = this%m1%dis%con%jas(ipos)
        ihc = this%m1%dis%con%ihc(isympos)
        csat = condsat(isympos)
      else
        ihc = ihc_opt(ignc)
        csat = condsat(ignc)
      end if
      !
      ! If vertical connection and not variable cv, then cycle
      if (ihc == 0 .and. ivarcv == 0) cycle
      !
      ! determine upstream node (0 is noden, 1 is nodem)
      iups = 0
      if (this%m2%x(nodem) > this%m1%x(noden)) iups = 1
      !
      ! -- Set the upstream top and bot, and then recalculate for a
      !    vertically staggered horizontal connection
      if (iups == 0) then
        topup = this%m1%dis%top(noden)
        botup = this%m1%dis%bot(noden)
        ictup = 1
        if (present(ictm1_opt)) ictup = ictm1_opt(noden)
        xup = this%m1%x(noden)
      else
        topup = this%m2%dis%top(nodem)
        botup = this%m2%dis%bot(nodem)
        ictup = 1
        if (present(ictm2_opt)) ictup = ictm2_opt(nodem)
        xup = this%m2%x(nodem)
      end if
      !
      ! -- No newton terms if upstream cell is confined
      if (ictup == 0) cycle
      !
      ! -- Handle vertically staggered horizontal connection
      if (ihc == 2) then
        topup = min(this%m1%dis%top(noden), this%m2%dis%top(nodem))
        botup = max(this%m1%dis%bot(noden), this%m2%dis%bot(nodem))
      end if
      !
      ! -- Process each contributing node
      jloop: do jidx = 1, this%numjs
        nodej = this%nodesj(jidx, ignc)
        if (nodej == 0) cycle
        if (this%m1%ibound(nodej) == 0) cycle
        alpha = this%alphasj(jidx, ignc)
        if (alpha == DZERO) cycle
        consterm = csat * alpha * (this%m1%x(noden) - this%m1%x(nodej))
        derv = sQuadraticSaturationDerivative(topup, botup, xup)
        term = consterm * derv
        if (iups == 0) then
          amatsln(this%idiagn(ignc)) = amatsln(this%idiagn(ignc)) + term
          if (this%m2%ibound(nodem) > 0) then
            amatsln(this%idxsymglo(ignc)) = amatsln(this%idxsymglo(ignc)) - &
                                            term
          end if
          this%m1%rhs(noden) = this%m1%rhs(noden) + term * this%m1%x(noden)
          this%m2%rhs(nodem) = this%m2%rhs(nodem) - term * this%m1%x(noden)
        else
          amatsln(this%idiagm(ignc)) = amatsln(this%idiagm(ignc)) - term
          if (this%m1%ibound(noden) > 0) then
            amatsln(this%idxglo(ignc)) = amatsln(this%idxglo(ignc)) + term
          end if
          this%m1%rhs(noden) = this%m1%rhs(noden) + term * this%m2%x(nodem)
          this%m2%rhs(nodem) = this%m2%rhs(nodem) - term * this%m2%x(nodem)
        end if
      end do jloop
    end do gncloop
    !
    ! -- return
    return
  end subroutine gnc_fn

  subroutine gnc_ot(this, ibudfl)
! ******************************************************************************
! gnc_ot -- Single Model GNC Output
! Subroutine: (1) Output GNC deltaQgnc values
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(GhostNodeType) :: this
    integer(I4B), intent(in) :: ibudfl
    ! -- local
    integer(I4B) :: ignc
    real(DP) :: deltaQgnc
    character(len=LINELENGTH) :: nodenstr, nodemstr
    ! -- format
    character(len=*), parameter :: fmtgnc = "(i10, 2a10, 2(1pg15.6))"
! ------------------------------------------------------------------------------
    !
    ! -- Process each gnc and output deltaQgnc
    if (ibudfl /= 0 .and. this%iprflow /= 0) then
      write (this%iout, '(//, a)') 'GHOST NODE CORRECTION RESULTS'
      write (this%iout, '(3a10, 2a15)') 'GNC NUM', 'NODEN', 'NODEM', &
        'DELTAQGNC', 'CONDNM'
      do ignc = 1, this%nexg
        deltaQgnc = this%deltaQgnc(ignc)
        call this%m1%dis%noder_to_string(this%nodem1(ignc), nodenstr)
        call this%m2%dis%noder_to_string(this%nodem2(ignc), nodemstr)
        write (this%iout, fmtgnc) ignc, trim(adjustl(nodenstr)), &
          trim(adjustl(nodemstr)), &
          deltaQgnc, this%cond(ignc)
      end do
    end if
    !
    ! -- return
    return
  end subroutine gnc_ot

  subroutine gnc_cq(this, flowja)
! ******************************************************************************
! gnc_cq -- Add GNC to flowja
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(GhostNodeType) :: this
    real(DP), dimension(:), intent(inout) :: flowja
    ! -- local
    integer(I4B) :: ignc, n1, n2, ipos, isympos
    real(DP) :: deltaQgnc
    ! -- format
! ------------------------------------------------------------------------------
    !
    ! -- go through each gnc and add deltagnc to flowja
    do ignc = 1, this%nexg
      !
      ! -- calculate correction term between n1 and n2 connection
      n1 = this%nodem1(ignc)
      n2 = this%nodem2(ignc)
      deltaQgnc = this%deltaQgnc(ignc)
      !
      ! -- find the positions of this connection in the csr array
      ipos = this%m1%dis%con%getjaindex(n1, n2)
      isympos = this%m1%dis%con%isym(ipos)
      !
      ! -- add/subtract the corrections
      flowja(ipos) = flowja(ipos) + deltaQgnc
      flowja(isympos) = flowja(isympos) - deltaQgnc
      !
    end do
    !
    ! -- return
    return
  end subroutine gnc_cq

  function deltaQgnc(this, ignc)
! ******************************************************************************
! deltaQgnc -- Single Model deltaQgnc (ghost node correction flux)
! Subroutine: (1) Calculate the deltaQgnc value for any GNC in the GNC list
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use ConstantsModule, only: DZERO
    ! -- return
    real(DP) :: deltaQgnc
    ! -- dummy
    class(GhostNodeType) :: this
    integer(I4B), intent(in) :: ignc
    ! -- local
    integer(I4B) :: noden, nodem, nodej, jidx
    real(DP) :: sigalj, alpha, hd, aterm, cond
! ------------------------------------------------------------------------------
    !
    ! -- initialize values
    deltaQgnc = DZERO
    sigalj = DZERO
    hd = DZERO
    noden = this%nodem1(ignc)
    nodem = this%nodem2(ignc)
    !
    ! -- calculate deltaQgnc
    if (this%m1%ibound(noden) /= 0 .and. this%m2%ibound(nodem) /= 0) then
      jloop: do jidx = 1, this%numjs
        nodej = this%nodesj(jidx, ignc)
        if (nodej == 0) cycle jloop
        if (this%m1%ibound(nodej) == 0) cycle jloop
        alpha = this%alphasj(jidx, ignc)
        sigalj = sigalj + alpha
        hd = hd + alpha * this%m1%x(nodej)
      end do jloop
      aterm = sigalj * this%m1%x(noden) - hd
      cond = this%cond(ignc)
      deltaQgnc = aterm * cond
    end if
    !
    ! -- return
    return
  end function deltaQgnc

  subroutine allocate_scalars(this)
! ******************************************************************************
! allocate_scalars -- allocate gnc scalar variables
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryManagerModule, only: mem_allocate
    ! -- dummy
    class(GhostNodeType) :: this
! ------------------------------------------------------------------------------
    !
    ! -- allocate scalars in NumericalPackageType
    call this%NumericalPackageType%allocate_scalars()
    !
    call mem_allocate(this%smgnc, 'SMGNC', this%memoryPath)
    call mem_allocate(this%implicit, 'IMPLICIT', this%memoryPath)
    call mem_allocate(this%i2kn, 'I2KN', this%memoryPath)
    call mem_allocate(this%nexg, 'NEXG', this%memoryPath)
    call mem_allocate(this%numjs, 'NUMJS', this%memoryPath)
    !
    ! -- Initialize values
    this%smgnc = .true.
    this%implicit = .true.
    this%i2kn = .false.
    this%nexg = 0
    this%numjs = 0
    !
    return
  end subroutine allocate_scalars

  subroutine allocate_arrays(this)
! ******************************************************************************
! allocate_arrays -- allocate gnc scalar variables
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryManagerModule, only: mem_allocate
    ! -- dummy
    class(GhostNodeType) :: this
    ! -- local
! ------------------------------------------------------------------------------
    !
    ! -- allocate memory for arrays
    call mem_allocate(this%nodem1, this%nexg, 'NODEM1', this%memoryPath)
    call mem_allocate(this%nodem2, this%nexg, 'NODEM2', this%memoryPath)
    call mem_allocate(this%nodesj, this%numjs, this%nexg, 'NODESJ', &
                      this%memoryPath)
    call mem_allocate(this%alphasj, this%numjs, this%nexg, 'ALPHASJ', &
                      this%memoryPath)
    call mem_allocate(this%cond, this%nexg, 'COND', this%memoryPath)
    call mem_allocate(this%idxglo, this%nexg, 'IDXGLO', this%memoryPath)
    call mem_allocate(this%idiagn, this%nexg, 'IDIAGN', this%memoryPath)
    call mem_allocate(this%idiagm, this%nexg, 'IDIAGM', this%memoryPath)
    call mem_allocate(this%idxsymglo, this%nexg, 'IDXSYMGLO', this%memoryPath)
    if (this%implicit) then
      call mem_allocate(this%jposinrown, this%numjs, this%nexg, 'JPOSINROWN', &
                        this%memoryPath)
      call mem_allocate(this%jposinrowm, this%numjs, this%nexg, 'JPOSINROWM', &
                        this%memoryPath)
    else
      call mem_allocate(this%jposinrown, 0, 0, 'JPOSINROWN', this%memoryPath)
      call mem_allocate(this%jposinrowm, 0, 0, 'JPOSINROWM', this%memoryPath)
    end if
    !
    ! -- Return
    return
  end subroutine allocate_arrays

  subroutine gnc_da(this)
! ******************************************************************************
! gnc_da -- deallocate
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryManagerModule, only: mem_deallocate
    ! -- dummy
    class(GhostNodeType) :: this
! ------------------------------------------------------------------------------
    !
    call mem_deallocate(this%smgnc)
    call mem_deallocate(this%implicit)
    call mem_deallocate(this%i2kn)
    call mem_deallocate(this%nexg)
    call mem_deallocate(this%numjs)
    !
    ! -- Arrays
    if (this%inunit > 0) then
      call mem_deallocate(this%nodem1)
      call mem_deallocate(this%nodem2)
      call mem_deallocate(this%nodesj)
      call mem_deallocate(this%alphasj)
      call mem_deallocate(this%cond)
      call mem_deallocate(this%idxglo)
      call mem_deallocate(this%idiagn)
      call mem_deallocate(this%idiagm)
      call mem_deallocate(this%idxsymglo)
      call mem_deallocate(this%jposinrown)
      call mem_deallocate(this%jposinrowm)
    end if
    !
    ! -- deallocate NumericalPackageType
    call this%NumericalPackageType%da()
    !
    ! -- Return
    return
  end subroutine gnc_da

  subroutine read_options(this)
! ******************************************************************************
! read_options -- read a gnc options block
! Subroutine: (1) read options from input file
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use SimModule, only: store_error
    ! -- dummy
    class(GhostNodeType) :: this
    ! -- local
    character(len=LINELENGTH) :: errmsg, keyword
    integer(I4B) :: ierr
    logical :: isfound, endOfBlock
! ------------------------------------------------------------------------------
    !
    ! -- get options block
    call this%parser%GetBlock('OPTIONS', isfound, ierr, &
                              supportOpenClose=.true., blockRequired=.false.)
    !
    ! -- parse options block if detected
    if (isfound) then
      write (this%iout, '(1x,a)') 'PROCESSING GNC OPTIONS'
      do
        call this%parser%GetNextLine(endOfBlock)
        if (endOfBlock) exit
        call this%parser%GetStringCaps(keyword)
        select case (keyword)
        case ('PRINT_INPUT')
          this%iprpak = 1
          write (this%iout, '(4x,a)') &
            'THE LIST OF GHOST-NODE CORRECTIONS WILL BE PRINTED.'
        case ('PRINT_FLOWS')
          this%iprflow = 1
          write (this%iout, '(4x,a)') &
            'DELTAQGNC VALUES WILL BE PRINTED TO THE LIST FILE.'
        case ('I2KN')
          this%i2kn = .true.
          write (this%iout, '(4x,a)') &
            'SECOND ORDER CORRECTION WILL BE APPLIED.'
        case ('EXPLICIT')
          this%implicit = .false.
          write (this%iout, '(4x,a)') 'GHOST NODE CORRECTION IS EXPLICIT.'
        case default
          write (errmsg, '(4x,a,a)') '****ERROR. UNKNOWN GNC OPTION: ', &
            trim(keyword)
          call store_error(errmsg)
          call this%parser%StoreErrorUnit()
        end select
      end do
      write (this%iout, '(1x,a)') 'END OF GNC OPTIONS'
    end if
    !
    ! -- Set the iasym flag if the correction is implicit
    if (this%implicit) this%iasym = 1
    !
    ! -- return
    return
  end subroutine read_options

  subroutine read_dimensions(this)
! ******************************************************************************
! read_dimensions -- Single Model GNC Read Dimensions
! Subroutine: (1) read dimensions (size of gnc list) from input file
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use SimModule, only: store_error
    ! -- dummy
    class(GhostNodeType) :: this
    ! -- local
    character(len=LINELENGTH) :: errmsg, keyword
    integer(I4B) :: ierr
    logical :: isfound, endOfBlock
! ------------------------------------------------------------------------------
    !
    ! -- get options block
    call this%parser%GetBlock('DIMENSIONS', isfound, ierr, &
                              supportOpenClose=.true.)
    !
    ! -- parse options block if detected
    if (isfound) then
      write (this%iout, '(1x,a)') 'PROCESSING GNC DIMENSIONS'
      do
        call this%parser%GetNextLine(endOfBlock)
        if (endOfBlock) exit
        call this%parser%GetStringCaps(keyword)
        select case (keyword)
        case ('NUMGNC')
          this%nexg = this%parser%GetInteger()
          write (this%iout, '(4x,a,i7)') 'NUMGNC = ', this%nexg
        case ('NUMALPHAJ')
          this%numjs = this%parser%GetInteger()
          write (this%iout, '(4x,a,i7)') 'NUMAPHAJ = ', this%numjs
        case default
          write (errmsg, '(4x,a,a)') '****ERROR. UNKNOWN GNC DIMENSION: ', &
            trim(keyword)
          call store_error(errmsg)
          call this%parser%StoreErrorUnit()
        end select
      end do
      write (this%iout, '(1x,a)') 'END OF GNC DIMENSIONS'
    else
      call store_error('Required DIMENSIONS block not found.', terminate=.TRUE.)
    end if
    !
    ! -- return
    return
  end subroutine read_dimensions

  subroutine read_data(this)
! ******************************************************************************
! read_data -- Read a GNCDATA block
! Subroutine: (1) read list of GNCs from input file
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use SimModule, only: store_error, count_errors
    ! -- dummy
    class(GhostNodeType) :: this
    ! -- local
    character(len=LINELENGTH) :: line, errmsg, nodestr, fmtgnc, cellid, &
                                 cellidm, cellidn
    integer(I4B) :: lloc, ierr, ival
    integer(I4B) :: ignc, jidx, nodeun, nodeum, nerr
    integer(I4B), dimension(:), allocatable :: nodesuj
    logical :: isfound, endOfBlock
    ! -- formats
! ------------------------------------------------------------------------------
    !
    ! -- Construct the fmtgnc format
    write (fmtgnc, '("(2i10,",i0,"i10,",i0, "(1pg15.6))")') this%numjs, &
      this%numjs
    !
    ! -- Allocate the temporary nodesuj, which stores the user-based nodej
    !    node numbers
    allocate (nodesuj(this%numjs))
    !
    ! -- get GNCDATA block
    call this%parser%GetBlock('GNCDATA', isfound, ierr, supportOpenClose=.true.)
    !
    ! -- process GNC data
    if (isfound) then
      write (this%iout, '(1x,a)') 'PROCESSING GNCDATA'
      do ignc = 1, this%nexg
        call this%parser%GetNextLine(endOfBlock)
        if (endOfBlock) exit
        call this%parser%GetCurrentLine(line)
        lloc = 1
        !
        ! -- cellidn (read as cellid and convert to user node)
        call this%parser%GetCellid(this%m1%dis%ndim, cellidn)
        nodeun = this%m1%dis%nodeu_from_cellid(cellidn, this%parser%iuactive, &
                                               this%iout)
        !
        ! -- convert user node to reduced node number
        call this%nodeu_to_noder(nodeun, this%nodem1(ignc), this%m1)
        !
        ! -- cellidm (read as cellid and convert to user node)
        call this%parser%GetCellid(this%m2%dis%ndim, cellidm)
        nodeum = this%m2%dis%nodeu_from_cellid(cellidm, this%parser%iuactive, &
                                               this%iout)
        !
        ! -- convert user node to reduced node number
        call this%nodeu_to_noder(nodeum, this%nodem2(ignc), this%m2)
        !
        ! -- cellidsj (read as cellid)
        do jidx = 1, this%numjs
          ! read cellidj as cellid of model 1
          call this%parser%GetCellid(this%m1%dis%ndim, cellid)
          ival = this%m1%dis%nodeu_from_cellid(cellid, this%parser%iuactive, &
                                               this%iout, allow_zero=.true.)
          nodesuj(jidx) = ival
          if (ival > 0) then
            call this%nodeu_to_noder(ival, this%nodesj(jidx, ignc), this%m1)
          else
            this%nodesj(jidx, ignc) = 0
          end if
        end do
        !
        ! -- alphaj
        do jidx = 1, this%numjs
          this%alphasj(jidx, ignc) = this%parser%GetDouble()
        end do
        !
        ! -- Echo if requested
        if (this%iprpak /= 0) &
          write (this%iout, fmtgnc) nodeun, nodeum, &
          (nodesuj(jidx), jidx=1, this%numjs), &
          (this%alphasj(jidx, ignc), jidx=1, this%numjs)
        !
        ! -- Check to see if noden is outside of active domain
        if (this%nodem1(ignc) <= 0) then
          call this%m1%dis%nodeu_to_string(nodeun, nodestr)
          write (errmsg, *) &
            trim(adjustl(this%m1%name))// &
            ' Cell is outside active grid domain: '// &
            trim(adjustl(nodestr))
          call store_error(errmsg)
        end if
        !
        ! -- Check to see if nodem is outside of active domain
        if (this%nodem2(ignc) <= 0) then
          call this%m2%dis%nodeu_to_string(nodeum, nodestr)
          write (errmsg, *) &
            trim(adjustl(this%m2%name))// &
            ' Cell is outside active grid domain: '// &
            trim(adjustl(nodestr))
          call store_error(errmsg)
        end if
        !
        ! -- Check to see if any nodejs are outside of active domain
        do jidx = 1, this%numjs
          if (this%nodesj(jidx, ignc) < 0) then
            call this%m1%dis%nodeu_to_string(nodesuj(jidx), nodestr)
            write (errmsg, *) &
              trim(adjustl(this%m1%name))// &
              ' Cell is outside active grid domain: '// &
              trim(adjustl(nodestr))
            call store_error(errmsg)
          end if
        end do
        !
      end do
      !
      ! -- Stop if errors
      nerr = count_errors()
      if (nerr > 0) then
        call store_error('Errors encountered in GNC input file.')
        call this%parser%StoreErrorUnit()
      end if
      !
      write (this%iout, '(1x,a)') 'END OF GNCDATA'
    else
      write (errmsg, '(1x,a)') 'ERROR.  REQUIRED GNCDATA BLOCK NOT FOUND.'
      call store_error(errmsg)
      call this%parser%StoreErrorUnit()
    end if
    !
    ! -- deallocate nodesuj array
    deallocate (nodesuj)
    !
    ! -- return
    return
  end subroutine read_data

  subroutine nodeu_to_noder(this, nodeu, noder, model)
! ******************************************************************************
! nodeu_to_noder -- Convert the user-based node number into a reduced number
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use NumericalModelModule, only: NumericalModelType
    use SimModule, only: store_error
    ! -- dummy
    class(GhostNodeType) :: this
    integer(I4B), intent(in) :: nodeu
    integer(I4B), intent(inout) :: noder
    class(NumericalModelType), intent(in) :: model
    ! -- local
    character(len=LINELENGTH) :: errmsg
! ------------------------------------------------------------------------------
    !
    if (nodeu < 1 .or. nodeu > model%dis%nodesuser) then
      write (errmsg, *) &
        trim(adjustl(model%name))// &
        ' node number < 0 or > model nodes: ', nodeu
      call store_error(errmsg)
    else
      noder = model%dis%get_nodenumber(nodeu, 0)
    end if
    !
    ! -- Return
    return
  end subroutine nodeu_to_noder

end module GhostNodeModule
