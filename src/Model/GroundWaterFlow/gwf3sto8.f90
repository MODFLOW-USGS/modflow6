module GwfStoModule

  use KindModule,             only: DP, I4B
  use ConstantsModule,        only: DZERO, DEM6, DEM4, DONE, LENBUDTXT
  use SmoothingModule,        only: sQuadraticSaturation,                      &
                                    sQuadraticSaturationDerivative,            &
                                    sQSaturation, sLinearSaturation
  use BaseDisModule,          only: DisBaseType
  use NumericalPackageModule, only: NumericalPackageType
  use BlockParserModule,      only: BlockParserType

  implicit none
  public :: GwfStoType, sto_cr

  character(len=LENBUDTXT), dimension(2) :: budtxt =                           & !text labels for budget terms
      ['          STO-SS', '          STO-SY']

  type, extends(NumericalPackageType) :: GwfStoType
    integer(I4B), pointer                            :: ionper => null()        !stress period for the current PERIOD block
    integer(I4B), pointer                            :: lastonper   => null()   !last value of ionper (for checking)
    integer(I4B), pointer                            :: isfac => null()         !indicates if ss is read as storativity
    integer(I4B), pointer                            :: isseg => null()         !indicates if ss is 0 below the top of a layer
    integer(I4B), pointer                            :: iss => null()           !steady state flag
    integer(I4B), pointer                            :: iusesy => null()        !flag set if any cell is convertible (0, 1)
    integer(I4B), dimension(:), pointer              :: iconvert => null()      !confined (0) or convertible (1)
    real(DP),dimension(:), pointer                   :: sc1 => null()           !primary storage capacity (when cell is fully saturated)
    real(DP),dimension(:), pointer                   :: sc2 => null()           !secondary storage capacity (when cell is partially saturated)
    real(DP), dimension(:), pointer                  :: strgss => null()        !vector of specific storage rates
    real(DP), dimension(:), pointer                  :: strgsy => null()        !vector of specific yield rates
    integer(I4B), dimension(:), pointer              :: ibound => null()        !pointer to model ibound
    real(DP), pointer                                :: satomega => null()      !newton-raphson saturation omega
  contains
    procedure :: sto_ar
    procedure :: sto_rp
    procedure :: sto_ad
    procedure :: sto_fc
    procedure :: sto_fn
    procedure :: bdcalc   => sto_bdcalc
    procedure :: bdsav    => sto_bdsav
    procedure :: sto_da
    procedure          :: allocate_scalars
    procedure, private :: allocate_arrays
    procedure, private :: read_options
    procedure, private :: read_data
  endtype

  contains

  subroutine sto_cr(stoobj, name_model, inunit, iout)
! ******************************************************************************
! sto_cr -- Create a new STO object
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    type(GwfStoType), pointer :: stoobj
    character(len=*), intent(in) :: name_model
    integer(I4B), intent(in) :: inunit
    integer(I4B), intent(in) :: iout
! ------------------------------------------------------------------------------
    !
    ! -- Create the object
    allocate(stoobj)
    !
    ! -- create name and origin
    call stoobj%set_names(1, name_model, 'STO', 'STO')
    !
    ! -- Allocate scalars
    call stoobj%allocate_scalars()
    !
    ! -- Set variables
    stoobj%inunit = inunit
    stoobj%iout = iout
    !
    ! -- Initialize block parser
    call stoobj%parser%Initialize(stoobj%inunit, stoobj%iout)
    !
    ! -- Return
    return
  end subroutine sto_cr

  subroutine sto_ar(this, dis, ibound)
! ******************************************************************************
! sto_ar -- Allocate and Read
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    !modules
    use MemoryManagerModule, only: mem_setptr
    ! -- dummy
    class(GwfStoType)                       :: this
    class(DisBaseType), pointer, intent(in) :: dis
    integer(I4B), dimension(:), pointer          :: ibound
    ! -- local
    ! -- formats
    character(len=*), parameter :: fmtsto =                                    &
      "(1x,/1x,'STO -- STORAGE PACKAGE, VERSION 1, 5/19/2014',                 &
      ' INPUT READ FROM UNIT ', i0, //)"
! ------------------------------------------------------------------------------
    !
    ! --print a message identifying the storage package.
    write(this%iout, fmtsto) this%inunit
    !
    ! -- store pointers to arguments that were passed in
    this%dis     => dis
    this%ibound  => ibound
    !
    ! -- set pointer to gwf iss
    call mem_setptr(this%iss, 'ISS', trim(this%name_model))
    !
    ! -- Allocate arrays
    call this%allocate_arrays(dis%nodes)
    !
    ! -- Read storage options
    call this%read_options()
    !
    ! -- read the data block
    call this%read_data()
    !
    ! -- Return
    return
  end subroutine sto_ar

  subroutine sto_rp(this)
! ******************************************************************************
! sto_rp -- Read and prepare
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use ConstantsModule,   only: LINELENGTH
    use TdisModule,        only: kper, nper
    use SimModule,         only: store_error, ustop
    implicit none
    ! -- dummy
    class(GwfStoType) :: this
    ! -- local
    integer(I4B)               :: ierr
    logical                   :: isfound, readss, readsy, endOfBlock
    !character(len=24)         :: aname(4) , stotxt
    character (len=16)        :: css(0:1)
    character(len=LINELENGTH) :: line, errmsg, keyword
    ! -- formats
    character(len=*),parameter :: fmtlsp = &
      "(1X,/1X,'REUSING ',A,' FROM LAST STRESS PERIOD')"
    character(len=*),parameter :: fmtblkerr = &
      "('Error.  Looking for BEGIN PERIOD iper.  Found ', a, ' instead.')"
    !data
    data css(0) /'       TRANSIENT'/
    data css(1) /'    STEADY-STATE'/
    !data aname(1) /'                ICONVERT'/
    !data aname(2) /'        SPECIFIC STORAGE'/
    !data aname(3) /'          SPECIFIC YIELD'/
    !data aname(4) /'     STORAGE COEFFICIENT'/
! ------------------------------------------------------------------------------
    !
    ! -- get stress period data
    if (this%ionper < kper) then
      !
      ! -- get period block
      call this%parser%GetBlock('PERIOD', isfound, ierr, &
                                supportOpenClose=.true.)
      if (isfound) then
        !
        ! -- save last value and read period number
        this%lastonper = this%ionper
        this%ionper = this%parser%GetInteger()
        !
        ! -- check to make sure period blocks are increasing
        if (this%ionper < this%lastonper) then
          write(errmsg, '(a, i0)') &
            'ERROR IN STRESS PERIOD ', kper
          call store_error(errmsg)
          write(errmsg, '(a, i0)') &
            'PERIOD NUMBERS NOT INCREASING.  FOUND ', this%ionper
          call store_error(errmsg)
          write(errmsg, '(a, i0)') &
            'BUT LAST PERIOD BLOCK WAS ASSIGNED ', this%lastonper
          call store_error(errmsg)
          call this%parser%StoreErrorUnit()
          call ustop()
        endif
      else
        !
        ! -- PERIOD block not found
        if (ierr < 0) then
          ! -- End of file found; data applies for remainder of simulation.
          this%ionper = nper + 1
        else
          ! -- Found invalid block
          call this%parser%GetCurrentLine(line)
          write(errmsg, fmtblkerr) adjustl(trim(line))
          call store_error(errmsg)
          call this%parser%StoreErrorUnit()
          call ustop()
        end if
      endif
    end if
    !
    ! -- read data if ionper == kper
    readss = .false.
    readsy = .false.
    !stotxt = aname(2)
    if(this%ionper==kper) then
      write(this%iout, '(//,1x,a)') 'UPDATING STORAGE VALUES'
      do
        call this%parser%GetNextLine(endOfBlock)
        if (endOfBlock) exit
        call this%parser%GetStringCaps(keyword)
        select case (keyword)
          case ('STEADY-STATE')
            this%iss = 1
          case ('TRANSIENT')
            this%iss = 0
          case default
            write(errmsg,'(4x,a,a)') 'ERROR. UNKNOWN STORAGE DATA TAG: ',      &
                                     trim(keyword)
            call store_error(errmsg)
            call this%parser%StoreErrorUnit()
            call ustop()
        end select
      end do
      write(this%iout,'(1x,a)') 'END UPDATING STORAGE VALUES'
    !else
    !  write(this%iout,fmtlsp) 'STORAGE VALUES'
    endif

    write(this%iout,'(//1X,A,I0,A,A,/)') &
      'STRESS PERIOD ', kper, ' IS ', trim(adjustl(css(this%iss)))
    !
    ! -- Return
    return
  end subroutine sto_rp

  subroutine sto_ad(this)
! ******************************************************************************
! sto_ad -- Advance
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    class(GwfStoType) :: this
! ------------------------------------------------------------------------------
    !
    ! -- todo: determine if this subroutine is needed
    !
    ! -- Return
    return
  end subroutine sto_ad

  subroutine sto_fc(this, kiter, nodes, hold, hnew, nja, njasln, amat, &
                            idxglo, rhs)
! ******************************************************************************
! sto_fc -- Fill the solution amat and rhs with storage contribution newton
!               term
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use TdisModule, only: delt
    ! -- dummy
    class(GwfStoType) :: this
    integer(I4B),intent(in) :: kiter
    integer(I4B),intent(in) :: nodes
    real(DP), intent(in), dimension(nodes) :: hold
    real(DP), intent(in), dimension(nodes) :: hnew
    integer(I4B),intent(in) :: nja
    integer(I4B),intent(in) :: njasln
    real(DP), dimension(njasln),intent(inout) :: amat
    integer(I4B), intent(in),dimension(nja) :: idxglo
    real(DP),intent(inout),dimension(nodes) :: rhs
    ! -- local
    integer(I4B) :: n, idiag
    real(DP) :: tled, rho1, rho2
    real(DP) :: tp, bt, tthk
    real(DP) :: snold, snnew
    real(DP) :: ss0, ss1, ssh0, ssh1
    real(DP) :: rhsterm
! ------------------------------------------------------------------------------
    !
    ! -- test if steady-state stress period
    if (this%iss /= 0) return
    !
    ! -- set variables
    tled = DONE / delt
    !
    ! -- loop through and calculate storage contribution to hcof and rhs
    do n = 1, this%dis%nodes
      idiag = this%dis%con%ia(n)
      if (this%ibound(n) < 1) cycle
      ! -- aquifer elevations and thickness
      tp = this%dis%top(n)
      bt = this%dis%bot(n)
      tthk = tp - bt
      ! -- aquifer saturation
      snold = sQuadraticSaturation(tp, bt, hold(n), this%satomega)
      snnew = sQuadraticSaturation(tp, bt, hnew(n), this%satomega)
      ! -- set saturation used for ss
      ss0 = snold
      ssh0 = hold(n)
      ss1 = snnew
      ssh1 = DZERO
      if (this%isseg /= 0) then
        if (ss0 < DONE) then
          ss0 = DONE
          ssh0 = tp
        end if
        if (ss1 < DONE) then
          ss1 = DZERO
          ssh1 = tp
        end if
      end if
      ! -- storage coefficients
      rho1 = this%sc1(n) * tled
      rho2 = this%sc2(n) * tled
      ! -- calculate storage coefficients for amat and rhs
      ! -- specific storage
      if (this%iconvert(n) /= 0) then
        amat(idxglo(idiag)) = amat(idxglo(idiag)) - rho1 * ss1
        rhs(n) = rhs(n) - rho1 * ss0 * ssh0 + rho1 * ssh1
      else
        amat(idxglo(idiag)) = amat(idxglo(idiag)) - rho1
        rhs(n) = rhs(n) - rho1 * hold(n)
      end if
      ! -- specific yield
      if (this%iconvert(n) /= 0) then
        rhsterm = DZERO
        ! -- add specific yield terms to amat at rhs
        if (snnew < DONE) then
          if (snnew > DZERO) then
            amat(idxglo(idiag)) = amat(idxglo(idiag)) - rho2
            rhsterm = rho2 * tthk * snold
            rhsterm = rhsterm + rho2 * bt
          else
            rhsterm = -rho2 * tthk * (DZERO - snold)
          end if
        ! -- known flow from specific yield
        else
          rhsterm = -rho2 * tthk * (DONE - snold)
        end if
        rhs(n) = rhs(n) - rhsterm
      end if
    end do
    !
    ! -- Return
    return
  end subroutine sto_fc

  subroutine sto_fn(this, kiter, nodes, hold, hnew, nja, njasln, amat,         &
                    idxglo, rhs)
! ******************************************************************************
! sto_fn -- Fill the solution amat and rhs with storage contribution
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use TdisModule, only: delt
    ! -- dummy
    class(GwfStoType) :: this
    integer(I4B),intent(in) :: kiter
    integer(I4B),intent(in) :: nodes
    real(DP), intent(in), dimension(nodes) :: hold
    real(DP), intent(in), dimension(nodes) :: hnew
    integer(I4B),intent(in) :: nja
    integer(I4B),intent(in) :: njasln
    real(DP), dimension(njasln),intent(inout) :: amat
    integer(I4B), intent(in),dimension(nja) :: idxglo
    real(DP),intent(inout),dimension(nodes) :: rhs
    ! -- local
    integer(I4B) :: n, idiag
    real(DP) :: tled, rho1, rho2
    real(DP) :: tp, bt, tthk
    real(DP) :: snold, snnew
    real(DP) :: ss0, ss1
    real(DP) :: derv, rterm, drterm
! ------------------------------------------------------------------------------
    !
    ! -- test if steady-state stress period
    if (this%iss /= 0) return
    !
    ! -- set variables
    tled = DONE / delt
    !
    ! -- loop through and calculate storage contribution to hcof and rhs
    do n = 1, this%dis%nodes
      idiag = this%dis%con%ia(n)
      if(this%ibound(n) <= 0) cycle
      ! -- aquifer elevations and thickness
      tp = this%dis%top(n)
      bt = this%dis%bot(n)
      tthk = tp - bt
      ! -- aquifer saturation
      snold = sQuadraticSaturation(tp, bt, hold(n))
      snnew = sQuadraticSaturation(tp, bt, hnew(n))
      ! -- set saturation used for ss
      ss0 = snold
      ss1 = snnew
      if (this%isseg /= 0) then
        if (ss0 < DONE) ss0 = DZERO
        if (ss1 < DONE) ss1 = DZERO
      end if
      ! -- storage coefficients
      rho1 = this%sc1(n) * tled
      rho2 = this%sc2(n) * tled
      ! -- calculate storage coefficients for amat and rhs
      ! -- specific storage
      if (this%iconvert(n) /= 0) then
        rterm = - rho1 * ss1 * hnew(n)
        derv = sQuadraticSaturationDerivative(tp, bt, hnew(n))
        if (this%isseg /= 0) derv = DZERO
        drterm = -(rho1 * derv * hnew(n))
        amat(idxglo(idiag)) = amat(idxglo(idiag)) + drterm
        rhs(n) = rhs(n) + drterm * hnew(n)
      end if
      ! -- specific yield
      if (this%iconvert(n) /= 0) then
        ! -- newton terms for specific yield only apply if
        !    current saturation is less than one
        if (snnew < DONE) then
          ! -- calculate newton terms for specific yield
          if (snnew > DZERO) then
            rterm = - rho2 * tthk * snnew
            derv = sQuadraticSaturationDerivative(tp, bt, hnew(n))
            drterm = -rho2 * tthk * derv
            amat(idxglo(idiag)) = amat(idxglo(idiag)) + drterm + rho2
            rhs(n) = rhs(n) - rterm + drterm * hnew(n) + rho2 * bt
          end if
        end if
      end if
    end do
    !
    ! -- Return
    return
  end subroutine sto_fn

  subroutine sto_bdcalc(this, nodes, hnew, hold, isuppress_output, model_budget)
! ******************************************************************************
! sto_bdcalc -- Calculate budget terms
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use TdisModule,        only: delt
    use BudgetModule, only: BudgetType
    ! -- dummy
    class(GwfStoType) :: this
    integer(I4B), intent(in) :: nodes
    real(DP), intent(in), dimension(nodes) :: hnew
    real(DP), intent(in), dimension(nodes) :: hold
    integer(I4B), intent(in) :: isuppress_output
    type(BudgetType), intent(inout) :: model_budget
    ! -- local
    integer(I4B) :: n
    real(DP) :: rate
    real(DP) :: tled, rho1, rho2
    real(DP) :: tp, bt, tthk
    real(DP) :: snold, snnew
    real(DP) :: ss0, ss1, ssh0, ssh1
    real(DP) :: rssin, rssout, rsyin, rsyout
! ------------------------------------------------------------------------------
    !
    ! -- initialize accumulators
    rssin = DZERO
    rssout = DZERO
    rsyin = DZERO
    rsyout = DZERO
    !
    ! -- Calculate terms if not steady-state stress period
    if (this%iss == 0) then
      !
      ! -- set variables
      tled = DONE / delt
      !
      ! -- Calculate storage change
      do n = 1, nodes
        this%strgss(n) = DZERO
        this%strgsy(n) = DZERO
        if(this%ibound(n) <= 0) cycle
        ! -- aquifer elevations and thickness
        tp = this%dis%top(n)
        bt = this%dis%bot(n)
        tthk = tp - bt
        snold = sQuadraticSaturation(tp, bt, hold(n), this%satomega)
        snnew = sQuadraticSaturation(tp, bt, hnew(n), this%satomega)
        ! -- set saturation used for ss
        ss0 = snold
        ssh0 = hold(n)
        ss1 = snnew
        ssh1 = DZERO
        if (this%isseg /= 0) then
          if (ss0 < DONE) then
            ss0 = DONE
            ssh0 = tp
          end if
          if (ss1 < DONE) then
            ss1 = DZERO
            ssh1 = tp
          end if
        end if
        ! -- storage coefficients
        rho1 = this%sc1(n) * tled
        rho2 = this%sc2(n) * tled
        ! -- specific storage
        if (this%iconvert(n) /= 0) then
          rate = rho1 * ss0 * ssh0 - rho1 * ss1 * hnew(n) - rho1 * ssh1
        else
          rate = rho1 * hold(n) - rho1 * hnew(n)
        end if
        this%strgss(n) = rate
        ! -- specific yield
        rate = DZERO
        if (this%iconvert(n) /= 0) then
          rate = rho2 * tthk * snold  - rho2 * tthk * snnew
        end if
        this%strgsy(n) = rate
        !
        ! -- accumulate ss
        if(this%strgss(n) < DZERO) then
          rssout = rssout - this%strgss(n)
        else
          rssin = rssin + this%strgss(n)
        endif
        !
        ! -- accumulate sy
        if(this%strgsy(n) < DZERO) then
          rsyout = rsyout - this%strgsy(n)
        else
          rsyin = rsyin + this%strgsy(n)
        endif
      enddo
    endif
    !
    ! -- Add contributions to model budget
    call model_budget%addentry(rssin, rssout, delt, budtxt(1),                 &
                               isuppress_output, '         STORAGE')
    if (this%iusesy == 1) then
      call model_budget%addentry(rsyin, rsyout, delt, budtxt(2),               &
                                 isuppress_output, '         STORAGE')
    end if
    !
    ! -- Return
    return
  end subroutine sto_bdcalc

  subroutine sto_bdsav(this, icbcfl, icbcun)
! ******************************************************************************
! sto_bdsav -- Save budget terms
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(GwfStoType) :: this
    integer(I4B), intent(in) :: icbcfl
    integer(I4B), intent(in) :: icbcun
    ! -- local
    integer(I4B) :: ibinun
    !character(len=16), dimension(2) :: aname
    integer(I4B) :: iprint, nvaluesp, nwidthp
    character(len=1) :: cdatafmp=' ', editdesc=' '
    real(DP) :: dinact
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
    ! -- Record the storage rates if requested
    if(ibinun /= 0) then
      iprint = 0
      dinact = DZERO
      !
      ! -- storage(ss)
      call this%dis%record_array(this%strgss, this%iout, iprint, -ibinun,    &
                                 budtxt(1), cdatafmp, nvaluesp,              &
                                 nwidthp, editdesc, dinact)
      !
      ! -- storage(sy)
      if (this%iusesy == 1) then
        call this%dis%record_array(this%strgsy, this%iout, iprint, -ibinun,  &
                                   budtxt(2), cdatafmp, nvaluesp,            &
                                   nwidthp, editdesc, dinact)
      end if
    endif
    !
    ! -- Return
    return
  end subroutine sto_bdsav

  subroutine sto_da(this)
! ******************************************************************************
! sto_da -- Deallocate variables
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryManagerModule, only: mem_deallocate
    ! -- dummy
    class(GwfStoType) :: this
! ------------------------------------------------------------------------------
    !
    ! -- Deallocate arrays if package was active
    if(this%inunit > 0) then
      call mem_deallocate(this%iconvert)
      call mem_deallocate(this%sc1)
      call mem_deallocate(this%sc2)
      call mem_deallocate(this%strgss)
      call mem_deallocate(this%strgsy)
    endif
    !
    ! -- Scalars
    call mem_deallocate(this%isfac)
    call mem_deallocate(this%isseg)
    call mem_deallocate(this%ionper)
    call mem_deallocate(this%satomega)
    call mem_deallocate(this%iusesy)
    call mem_deallocate(this%lastonper)
    !
    ! -- deallocate parent
    call this%NumericalPackageType%da()
    !
    ! -- Return
    return
  end subroutine sto_da

  subroutine allocate_scalars(this)
! ******************************************************************************
! allocate_scalars
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryManagerModule, only: mem_allocate, mem_setptr
    ! -- dummy
    class(GwfStoType) :: this
    ! -- local
! ------------------------------------------------------------------------------
    !
    ! -- allocate scalars in NumericalPackageType
    call this%NumericalPackageType%allocate_scalars()
    !
    ! -- Allocate
    call mem_allocate(this%iusesy, 'IUSESY', this%origin)
    call mem_allocate(this%isfac, 'ISFAC', this%origin)
    call mem_allocate(this%isseg, 'ISSEG', this%origin)
    call mem_allocate(this%ionper, 'IONPER', this%origin)
    call mem_allocate(this%lastonper, 'LASTONPER', this%origin)
    call mem_allocate(this%satomega, 'SATOMEGA', this%origin)
    !
    ! -- Initialize
    this%iusesy = 0
    this%ionper = 0
    this%lastonper = 0
    this%isfac = 0
    this%isseg = 0
    this%satomega = DZERO
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
    use MemoryManagerModule, only: mem_allocate
    !modules
    use ConstantsModule, only: DZERO
    ! -- dummy
    class(GwfStoType) :: this
    integer(I4B), intent(in) :: nodes
    ! -- local
    integer(I4B) :: n
! ------------------------------------------------------------------------------
    !
    ! -- Allocate
    !call mem_allocate(this%iss, 'ISS', this%name_model)
    call mem_allocate(this%iconvert, nodes, 'ICONVERT', this%origin)
    call mem_allocate(this%sc1, nodes, 'SC1', this%origin)
    call mem_allocate(this%sc2, nodes, 'SC2', this%origin)
    call mem_allocate(this%strgss, nodes, 'STRGSS', this%origin)
    call mem_allocate(this%strgsy, nodes, 'STRGSY', this%origin)
    !
    ! -- Initialize
    this%iss = 0
    do n = 1, nodes
      this%iconvert(n) = 1
      this%sc1(n) = DZERO
      this%sc2(n) = DZERO
      this%strgss(n) = DZERO
      this%strgsy(n) = DZERO
    enddo
    !
    ! -- Return
    return
  end subroutine allocate_arrays

  subroutine read_options(this)
! ******************************************************************************
! gwf3sto1ar -- Allocate and Read
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    !modules
    use ConstantsModule,   only: LINELENGTH
    use SimModule,         only: ustop, store_error
    ! -- dummy
    class(GwfStoType) :: this
    ! -- local
    character(len=LINELENGTH) :: errmsg, keyword
    integer(I4B) :: ierr
    logical :: isfound, endOfBlock
    ! -- formats
    character(len=*), parameter :: fmtisvflow =                                &
      "(4x,'CELL-BY-CELL FLOW INFORMATION WILL BE SAVED TO BINARY FILE " //    &
      "WHENEVER ICBCFL IS NOT ZERO.')"
    character(len=*),parameter :: fmtflow =                                    &
      "(4x, 'FLOWS WILL BE SAVED TO FILE: ', a, /4x, 'OPENED ON UNIT: ', I7)"
    character(len=*), parameter :: fmtstoc =                                   &
      "(4X,'STORAGECOEFFICIENT OPTION:',/,                                     &
      1X,'Read storage coefficient rather than specific storage')"
    character(len=*), parameter :: fmtstoseg =                                 &
      "(4X,'OLDSTORAGEFORMULATION OPTION:',/,                                  &
      1X,'Specific storage changes only occur above cell top')"
! ------------------------------------------------------------------------------
    !
    ! -- get options block
    call this%parser%GetBlock('OPTIONS', isfound, ierr, blockRequired=.false.)
    !
    ! -- parse options block if detected
    if (isfound) then
      write(this%iout,'(1x,a)')'PROCESSING STORAGE OPTIONS'
      do
        call this%parser%GetNextLine(endOfBlock)
        if (endOfBlock) exit
        call this%parser%GetStringCaps(keyword)
        select case (keyword)
          case ('SAVE_FLOWS')
            this%ipakcb = -1
            write(this%iout, fmtisvflow)
          case ('STORAGECOEFFICIENT')
            this%isfac = 1
            write(this%iout,fmtstoc)
          !
          ! -- right now these are options that are only available in the
          !    development version and are not included in the documentation.
          !    These options are only available when IDEVELOPMODE in
          !    constants module is set to 1
          case ('NO_NEWTON')
            call this%parser%DevOpt()
            this%inewton = 0
            write(this%iout, '(4x,a)')                                         &
                             'NEWTON-RAPHSON method disabled for unconfined cell storage'
          case ('OLDSTORAGEFORMULATION')
            call this%parser%DevOpt()
            this%isseg = 1
            write(this%iout,fmtstoseg)
          case default
            write(errmsg,'(4x,a,a)')'****ERROR. UNKNOWN STO OPTION: ',         &
                                     trim(keyword)
            call store_error(errmsg)
            call ustop()
        end select
      end do
      write(this%iout,'(1x,a)')'END OF STORAGE OPTIONS'
    end if
    !
    ! -- set omega value used for saturation calculations
    if (this%inewton > 0) then
      this%satomega = DEM6
    end if
    !
    ! -- Return
    return
  end subroutine read_options

  subroutine read_data(this)
! ******************************************************************************
! read_data -- read the storage data (stodata) block
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use ConstantsModule,   only: LINELENGTH
    use SimModule,         only: ustop, store_error, count_errors
    ! -- dummy
    class(GwfStotype) :: this
    ! -- local
    character(len=LINELENGTH) :: line, errmsg, keyword
    integer(I4B) :: istart, istop, lloc, ierr
    logical :: isfound, endOfBlock
    logical :: readiconv
    logical :: readss
    logical :: readsy
    logical :: isconv
    character(len=24), dimension(4) :: aname
    integer(I4B) :: n
    real(DP) :: thick
    ! -- formats
    !data
    data aname(1) /'                ICONVERT'/
    data aname(2) /'        SPECIFIC STORAGE'/
    data aname(3) /'          SPECIFIC YIELD'/
    data aname(4) /'     STORAGE COEFFICIENT'/
! ------------------------------------------------------------------------------
    !
    ! -- initialize
    isfound = .false.
    readiconv = .false.
    readss = .false.
    readsy = .false.
    isconv = .false.
    !
    ! -- get stodata block
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
          case ('ICONVERT')
            call this%dis%read_grid_array(line, lloc, istart, istop, this%iout, &
                                         this%parser%iuactive, this%iconvert, &
                                         aname(1))
            readiconv = .true.
          case ('SS')
            call this%dis%read_grid_array(line, lloc, istart, istop, this%iout, &
                                         this%parser%iuactive, this%sc1, &
                                         aname(2))
            readss = .true.
          case ('SY')
            call this%dis%read_grid_array(line, lloc, istart, istop, this%iout, &
                                         this%parser%iuactive, this%sc2, &
                                         aname(3))
            readsy = .true.
          case default
            write(errmsg,'(4x,a,a)')'ERROR. UNKNOWN GRIDDATA TAG: ',            &
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
    ! -- Check for ICONVERT
    if(.not. readiconv) then
      write(errmsg, '(a, a, a)') 'Error in GRIDDATA block: ',                   &
                                 trim(adjustl(aname(1))), ' not found.'
      call store_error(errmsg)
    else
      isconv = .false.
      do n = 1, this%dis%nodes
        if (this%iconvert(n) /= 0) then
          isconv = .true.
          this%iusesy = 1
          exit
        end if
      end do
    end if
    !
    ! -- Check for SS
    if(.not. readss) then
      write(errmsg, '(a, a, a)') 'Error in GRIDDATA block: ',                   &
                                 trim(adjustl(aname(2))), ' not found.'
      call store_error(errmsg)
    endif
    !
    ! -- Check for SY
    if(.not. readsy .and. isconv) then
      write(errmsg, '(a, a, a)') 'Error in GRIDDATA block: ',                   &
                                 trim(adjustl(aname(3))), ' not found.'
      call store_error(errmsg)
    endif
    !
    if(count_errors() > 0) then
      call this%parser%StoreErrorUnit()
      call ustop()
    endif
    !
    ! -- calculate sc1
    if (readss) then
      if(this%isfac == 0) then
        do n = 1, this%dis%nodes
          thick = this%dis%top(n) - this%dis%bot(n)
          this%sc1(n) = this%sc1(n) * thick * this%dis%area(n)
        end do
      else
        do n = 1, this%dis%nodes
          this%sc1(n) = this%sc1(n) * this%dis%area(n)
        enddo
      endif
    endif
    !
    ! -- calculate sc2
    if(readsy) then
      do n=1, this%dis%nodes
        this%sc2(n) = this%sc2(n) * this%dis%area(n)
      enddo
    endif
    !
    ! -- Return
    return
  end subroutine read_data

end module GwfStoModule
