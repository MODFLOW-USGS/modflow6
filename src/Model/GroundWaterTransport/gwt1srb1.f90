module GwtSrbModule
  
  use KindModule,             only: DP, I4B
  use ConstantsModule,        only: DONE, DZERO
  use NumericalPackageModule, only: NumericalPackageType
  use BaseDisModule,          only: DisBaseType
  use GwtFmiModule,           only: GwtFmiType
  
  implicit none
  public :: GwtSrbType
  public :: srb_cr

  type, extends(NumericalPackageType) :: GwtSrbType
    
    real(DP), dimension(:), pointer, contiguous      :: porosity => null()      ! pointer to storage package porosity
    integer(I4B), dimension(:), pointer, contiguous  :: ibound => null()        ! pointer to model ibound
    type(GwtFmiType), pointer                        :: fmi => null()           ! pointer to fmi object
    
    integer(I4B), pointer                            :: irorder                 ! order of reaction rate (0:none, 1:first, 2:zero)
    integer(I4B), pointer                            :: isrb                    ! sorbtion active flag (0:off, 1:on)
    integer(I4B), pointer                            :: idd                     ! dual domain active flag (0:off, 1:on)
    real(DP), dimension(:), pointer, contiguous      :: strg => null()          ! rate of sorbed mass storage
    real(DP), dimension(:), pointer, contiguous      :: rhob => null()          ! bulk density
    real(DP), dimension(:), pointer, contiguous      :: srconc => null()        ! initial sorbed concentration
    real(DP), dimension(:), pointer, contiguous      :: distcoef => null()      ! distribution coefficient
    real(DP), dimension(:), pointer, contiguous      :: rc1 => null()           ! first or zero order rate constant for liquid
    real(DP), dimension(:), pointer, contiguous      :: rc2 => null()           ! first or zero order rate constant for sorbed mass
    real(DP), dimension(:), pointer, contiguous      :: csrbnew => null()       ! new sorbed concentration
    real(DP), dimension(:), pointer, contiguous      :: csrbold => null()       ! old sorbed concentration
    
  contains
  
    procedure :: srb_ar
    procedure :: srb_fc
    procedure :: srb_bdcalc
    procedure :: srb_bdsav
    procedure :: srb_da
    procedure :: allocate_scalars
    procedure, private :: allocate_arrays
    procedure, private :: read_options
    procedure, private :: read_data
  
  end type GwtSrbType
  
  contains
  
  subroutine srb_cr(srbobj, name_model, inunit, iout, fmi)
! ******************************************************************************
! srb_cr -- Create a new SRB object
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    type(GwtSrbType), pointer :: srbobj
    character(len=*), intent(in) :: name_model
    integer(I4B), intent(in) :: inunit
    integer(I4B), intent(in) :: iout
    type(GwtFmiType), intent(in), target :: fmi
! ------------------------------------------------------------------------------
    !
    ! -- Create the object
    allocate(srbobj)
    !
    ! -- create name and origin
    call srbobj%set_names(1, name_model, 'SRB', 'SRB')
    !
    ! -- Allocate scalars
    call srbobj%allocate_scalars()
    !
    ! -- Set variables
    srbobj%inunit = inunit
    srbobj%iout = iout
    srbobj%fmi => fmi
    !
    ! -- Initialize block parser
    call srbobj%parser%Initialize(srbobj%inunit, srbobj%iout)
    !
    ! -- Return
    return
  end subroutine srb_cr

  subroutine srb_ar(this, dis, ibound, porosity)
! ******************************************************************************
! srb_ar -- Allocate and Read
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryManagerModule, only: mem_setptr
    ! -- dummy
    class(GwtSrbType) :: this
    class(DisBaseType), pointer, intent(in) :: dis
    integer(I4B), dimension(:), pointer, contiguous :: ibound
    real(DP), dimension(:), pointer, contiguous :: porosity
    ! -- local
    integer(I4B) :: i
    ! -- formats
    character(len=*), parameter :: fmtsrb =                                    &
      "(1x,/1x,'SRB -- SORPTION PACKAGE, VERSION 1, 10/01/2018',               &
      &' INPUT READ FROM UNIT ', i0, //)"
! ------------------------------------------------------------------------------
    !
    ! --print a message identifying the sorption package.
    write(this%iout, fmtsrb) this%inunit
    !
    ! -- store pointers to arguments that were passed in
    this%dis => dis
    this%ibound => ibound
    this%porosity => porosity
    !
    ! -- Allocate arrays
    call this%allocate_arrays()
    !
    ! -- Read sorption options
    call this%read_options()
    !
    ! -- read the data block
    call this%read_data()
    !
    ! -- Set new and old sorbed concentrations equal to the intial sorbed conc
    if (this%isrb > 0) then
      !do i = 1, this%dis%nodes
      !  this%csrbnew(i) = this%srconc(i)
      !  this%csrbold(i) = this%srconc(i)
      !enddo
    endif
    !
    ! -- Return
    return
  end subroutine srb_ar

  subroutine srb_fc(this, nodes, cold, nja, njasln, amatsln, idxglo, rhs)
! ******************************************************************************
! srb_fc -- Calculate coefficients and fill amat and rhs
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use TdisModule, only: delt
    ! -- dummy
    class(GwtSrbType) :: this
    integer, intent(in) :: nodes
    real(DP), intent(in), dimension(nodes) :: cold
    integer(I4B), intent(in) :: nja
    integer(I4B), intent(in) :: njasln
    real(DP), dimension(njasln), intent(inout) :: amatsln
    integer(I4B), intent(in), dimension(nja) :: idxglo
    real(DP), intent(inout), dimension(nodes) :: rhs
    ! -- local
    integer(I4B) :: n, idiag
    real(DP) :: tled
    real(DP) :: hhcof, rrhs
    real(DP) :: vnew, vold
    real(DP) :: vcell
    real(DP) :: gwfsatold
    real(DP) :: eqfact
    real(DP) :: ctosrb
! ------------------------------------------------------------------------------
    !
    ! -- set variables
    tled = DONE / delt
    !
    ! -- loop through and calculate sorption contribution to hcof and rhs
    do n = 1, this%dis%nodes
      !
      ! -- skip if transport inactive
      if(this%ibound(n) <= 0) cycle
      !
      ! -- calculate new and old water volumes
      vcell = this%dis%area(n) * (this%dis%top(n) - this%dis%bot(n))
      vnew = vcell * this%fmi%gwfsat(n) * this%porosity(n)
      vold = vnew
      if (this%fmi%igwfstrgss /= 0) vold = vold + this%fmi%gwfstrgss(n) * delt
      if (this%fmi%igwfstrgsy /= 0) vold = vold + this%fmi%gwfstrgsy(n) * delt
      gwfsatold = vold / vcell / this%porosity(n)
      idiag = this%dis%con%ia(n)
      !
      ! -- add sorbtion terms to diagonal and rhs accumulators
      if (this%isrb == 1) then
        eqfact = -this%rhob(n) * vcell * tled
        ctosrb = this%distcoef(n)
        hhcof =  eqfact * ctosrb * this%fmi%gwfsat(n)
        rrhs = eqfact * ctosrb * gwfsatold * cold(n)
        amatsln(idxglo(idiag)) = amatsln(idxglo(idiag)) + hhcof
        rhs(n) = rhs(n) + rrhs
      endif
      !
      ! -- add reaction rate terms to accumulators
      if (this%irorder == 1) then
        !
        ! -- first order reaction rate is a function of concentration, so add
        !    to left hand side
        hhcof = -this%rc1(n) * vnew
        if (this%isrb == 1) hhcof = hhcof - this%rc2(n) * vcell * this%rhob(n) * ctosrb
        amatsln(idxglo(idiag)) = amatsln(idxglo(idiag)) + hhcof
      elseif (this%irorder == 2) then
        !
        ! -- zero-order reaction rate is not a function of concentration, so add
        !    to right hand side
        rrhs = this%rc1(n) * vnew
        if (this%isrb == 1) rrhs = rrhs + this%rc2(n) * ctosrb * vcell
        rhs(n) = rhs(n) + rrhs
      endif
      !
    enddo
    !
    ! -- Return
    return
  end subroutine srb_fc
  
  subroutine srb_bdcalc(this, nodes, cnew, cold, isuppress_output, model_budget)
! ******************************************************************************
! srb_bdcalc -- Calculate budget terms
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use TdisModule,        only: delt
    use BudgetModule, only: BudgetType
    ! -- dummy
    class(GwtSrbType) :: this
    integer(I4B), intent(in) :: nodes
    real(DP), intent(in), dimension(nodes) :: cnew
    real(DP), intent(in), dimension(nodes) :: cold
    integer(I4B), intent(in) :: isuppress_output
    type(BudgetType), intent(inout) :: model_budget
    ! -- local
    integer(I4B) :: n
    real(DP) :: rate
    real(DP) :: tled
    real(DP) :: rsrbin, rsrbout
    real(DP) :: rrctin, rrctout
    real(DP) :: vnew, vold
    real(DP) :: hhcof, rrhs
    real(DP) :: vcell
    real(DP) :: gwfsatold
    real(DP) :: eqfact
    real(DP) :: ctosrb
! ------------------------------------------------------------------------------
    !
    ! -- initialize 
    rsrbin = DZERO
    rsrbout = DZERO
    rrctin = DZERO
    rrctout = DZERO
    tled = DONE / delt
    !
    ! -- Calculate sorption change
    do n = 1, nodes
      !
      ! -- skip if transport inactive
      if(this%ibound(n) <= 0) cycle
      !
      ! -- calculate new and old water volumes
      vcell = this%dis%area(n) * (this%dis%top(n) - this%dis%bot(n))
      vnew = vcell * this%fmi%gwfsat(n) * this%porosity(n)
      vold = vnew
      if (this%fmi%igwfstrgss /= 0) vold = vold + this%fmi%gwfstrgss(n) * delt
      if (this%fmi%igwfstrgsy /= 0) vold = vold + this%fmi%gwfstrgsy(n) * delt
      gwfsatold = vold / vcell / this%porosity(n)
      !
      ! -- calculate sorbtion rate
      if (this%isrb == 1) then
        this%strg(n) = DZERO
        eqfact = -this%rhob(n) * vcell * tled
        ctosrb = this%distcoef(n)
        hhcof =  eqfact * ctosrb * this%fmi%gwfsat(n)
        rrhs = eqfact * ctosrb * gwfsatold * cold(n)
        rate = hhcof * cnew(n) - rrhs
        this%strg(n) = rate
        if (rate < DZERO) then
          rsrbout = rsrbout - rate
        else
          rsrbin = rsrbin + rate
        endif
      endif
      !
      ! -- calculate reaction gains and losses
      rate = DZERO
      hhcof = DZERO
      rrhs = DZERO
      if (this%irorder == 1) then
        hhcof = -this%rc1(n) * vnew
        if (this%isrb == 1) hhcof = hhcof - this%rc2(n) * vcell * this%rhob(n) * ctosrb
      elseif (this%irorder == 2) then
        rrhs = this%rc1(n) * vnew
        if (this%isrb == 1) rrhs = rrhs + this%rc2(n) * ctosrb * vcell
      endif
      rate = hhcof * cnew(n) - rrhs
      if (rate < DZERO) then
        rrctout = rrctout - rate
      else
        rrctin = rrctin + rate
      endif
      !
    enddo
    !
    ! -- Add sorbtion contributions to model budget
    if (this%isrb == 1) then
      call model_budget%addentry(rsrbin, rsrbout, delt, '        SORBTION',    &
                                 isuppress_output)
    endif
    !
    ! -- Add reaction contributions to model budget
    if (this%irorder > 0) then
      call model_budget%addentry(rrctin, rrctout, delt, '       REACTIONS',   &
                                 isuppress_output)
    endif
    !
    ! -- Return
    return
  end subroutine srb_bdcalc

  subroutine srb_bdsav(this, icbcfl, icbcun)
! ******************************************************************************
! srb_bdsav -- Save budget terms
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(GwtSrbType) :: this
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
    ! -- Record the sorption rates if requested
    if(ibinun /= 0) then
      iprint = 0
      dinact = DZERO
      !
      ! -- sorption
      call this%dis%record_array(this%strg, this%iout, iprint, -ibinun,        &
                                 '        SORPTION', cdatafmp, nvaluesp,       &
                                 nwidthp, editdesc, dinact)
    endif
    !
    ! -- Return
    return
  end subroutine srb_bdsav

  subroutine srb_da(this)
! ******************************************************************************
! srb_da -- Deallocate variables
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryManagerModule, only: mem_deallocate
    ! -- dummy
    class(GwtSrbType) :: this
! ------------------------------------------------------------------------------
    !
    ! -- Deallocate arrays if package was active
    if(this%inunit > 0) then
      call mem_deallocate(this%strg)
      call mem_deallocate(this%rhob)
      call mem_deallocate(this%srconc)
      call mem_deallocate(this%distcoef)
      call mem_deallocate(this%csrbnew)
      call mem_deallocate(this%csrbold)
      call mem_deallocate(this%rc1)
      call mem_deallocate(this%rc2)
      call mem_deallocate(this%isrb)
      call mem_deallocate(this%irorder)
      call mem_deallocate(this%idd)
      this%ibound => null()
      this%porosity => null()
      this%fmi => null()
    endif
    !
    ! -- Scalars
    !
    ! -- deallocate parent
    call this%NumericalPackageType%da()
    !
    ! -- Return
    return
  end subroutine srb_da

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
    class(GwtSrbType) :: this
    ! -- local
! ------------------------------------------------------------------------------
    !
    ! -- allocate scalars in NumericalPackageType
    call this%NumericalPackageType%allocate_scalars()
    !
    ! -- Allocate
    call mem_allocate(this%isrb, 'ISRB', this%origin)
    call mem_allocate(this%irorder, 'IRORDER', this%origin)
    call mem_allocate(this%idd, 'IDD', this%origin)
    !
    ! -- Initialize
    this%isrb = 0
    this%irorder = 0
    this%idd = 0
    !
    ! -- Return
    return
  end subroutine allocate_scalars

  subroutine allocate_arrays(this)
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
    class(GwtSrbType) :: this
    ! -- local
! ------------------------------------------------------------------------------
    !
    ! -- Allocate
    call mem_allocate(this%strg, 1, 'STRG', this%origin)
    call mem_allocate(this%rhob, 1, 'RHOB', this%origin)
    call mem_allocate(this%srconc, 1, 'SRCONC', this%origin)
    call mem_allocate(this%distcoef,  1, 'DISTCOEF', this%origin)
    call mem_allocate(this%csrbnew, 1, 'CSRBNEW', this%origin)
    call mem_allocate(this%csrbold, 1, 'CSRBOLD', this%origin)
    call mem_allocate(this%rc1, 1, 'RC1', this%origin)
    call mem_allocate(this%rc2, 1, 'RC2', this%origin)
    !
    ! -- Initialize
    !
    ! -- Return
    return
  end subroutine allocate_arrays

  subroutine read_options(this)
! ******************************************************************************
! read_options -- Read options
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    !modules
    use ConstantsModule,   only: LINELENGTH
    use SimModule,         only: ustop, store_error
    ! -- dummy
    class(GwtSrbType) :: this
    ! -- local
    character(len=LINELENGTH) :: errmsg, keyword, keyword2
    integer(I4B) :: ierr
    logical :: isfound, endOfBlock
    ! -- formats
    character(len=*), parameter :: fmtisvflow =                                &
      "(4x,'CELL-BY-CELL FLOW INFORMATION WILL BE SAVED TO BINARY FILE " //    &
      "WHENEVER ICBCFL IS NOT ZERO.')"
    character(len=*), parameter :: fmtisrb =                                   &
      "(4x,'LINEAR SORBTION IS SELECTED. ')"
    character(len=*), parameter :: fmtirorder1 =                               &
      "(4x,'FIRST ORDER REACTION IS ACTIVE. ')"
    character(len=*), parameter :: fmtirorder2 =                               &
      "(4x,'ZERO ORDER REACTION IS ACTIVE. ')"
    character(len=*), parameter :: fmtidd =                                    &
      "(4x,'DUAL DOMAIN TRANSPORT IS ACTIVE. ')"
! ------------------------------------------------------------------------------
    !
    ! -- get options block
    call this%parser%GetBlock('OPTIONS', isfound, ierr, blockRequired=.false.)
    !
    ! -- parse options block if detected
    if (isfound) then
      write(this%iout,'(1x,a)')'PROCESSING SORPTION OPTIONS'
      do
        call this%parser%GetNextLine(endOfBlock)
        if (endOfBlock) exit
        call this%parser%GetStringCaps(keyword)
        select case (keyword)
          case ('SAVE_FLOWS')
            this%ipakcb = -1
            write(this%iout, fmtisvflow)
          case ('SORBTION')
            this%isrb = 1
            write(this%iout, fmtisrb)
          case ('RATEORDER')
            this%isrb = 1
            call this%parser%GetStringCaps(keyword2)
            select case(keyword2)
              case('ONE')
                this%irorder = 1
                write(this%iout, fmtirorder1)
              case('ZERO')
                this%irorder = 2
                write(this%iout, fmtirorder2)
              case default
                write(errmsg,'(4x,a,a)')'****ERROR. UNKNOWN RATEORDER OPTION: ',         &
                                         trim(keyword2)
                call store_error(errmsg)
                call this%parser%StoreErrorUnit()
                call ustop()
            end select
          case ('DUALDOMAIN')
            this%idd = 1
            write(this%iout, fmtidd)
          case default
            write(errmsg,'(4x,a,a)')'****ERROR. UNKNOWN SRB OPTION: ',         &
                                     trim(keyword)
            call store_error(errmsg)
            call this%parser%StoreErrorUnit()
            call ustop()
        end select
      end do
      write(this%iout,'(1x,a)')'END OF SORPTION OPTIONS'
    end if
    !
    ! -- Return
    return
  end subroutine read_options

  subroutine read_data(this)
! ******************************************************************************
! read_data -- read the sorption data (griddata) block
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use ConstantsModule,   only: LINELENGTH
    use SimModule,         only: ustop, store_error, count_errors
    use MemoryManagerModule, only: mem_reallocate
    ! -- dummy
    class(GwtSrbType) :: this
    ! -- local
    character(len=LINELENGTH) :: line, errmsg, keyword
    integer(I4B) :: istart, istop, lloc, ierr
    logical :: isfound, endOfBlock
    logical, dimension(5) :: lname
    character(len=24), dimension(5) :: aname
    ! -- formats
    ! -- data
    data aname(1) /'            BULK DENSITY'/
    data aname(2) /'DISTRIBUTION COEFFICIENT'/
    data aname(3) /'  FIRST RATE COEFFICIENT'/
    data aname(4) /' SECOND RATE COEFFICIENT'/
    data aname(5) /'     INITIAL SORBED CONC'/
! ------------------------------------------------------------------------------
    !
    ! -- initialize
    isfound = .false.
    lname(:) = .false.
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
          case ('RHOB')
            call mem_reallocate(this%rhob, this%dis%nodes, 'RHOB',             &
                              trim(this%origin))
            call this%dis%read_grid_array(line, lloc, istart, istop, this%iout,&
                                         this%parser%iuactive, this%rhob,      &
                                         aname(1))
            lname(1) = .true.
          case ('DISTCOEF')
            call mem_reallocate(this%distcoef, this%dis%nodes, 'DISTCOEF',     &
                              trim(this%origin))
            call this%dis%read_grid_array(line, lloc, istart, istop, this%iout,&
                                         this%parser%iuactive, this%distcoef,  &
                                         aname(2))
            lname(2) = .true.
          case ('RC1')
            call mem_reallocate(this%rc1, this%dis%nodes, 'RC1',               &
                              trim(this%origin))
            call this%dis%read_grid_array(line, lloc, istart, istop, this%iout,&
                                         this%parser%iuactive, this%rc1,       &
                                         aname(3))
            lname(3) = .true.
          case ('RC2')
            call mem_reallocate(this%rc2, this%dis%nodes, 'RC2',               &
                              trim(this%origin))
            call this%dis%read_grid_array(line, lloc, istart, istop, this%iout,&
                                         this%parser%iuactive, this%rc2,       &
                                         aname(4))
            lname(4) = .true.
          case ('SRCONC')
            call mem_reallocate(this%srconc, this%dis%nodes, 'SRCONC',         &
                              trim(this%origin))
            call this%dis%read_grid_array(line, lloc, istart, istop, this%iout,&
                                         this%parser%iuactive, this%srconc,    &
                                         aname(5))
            lname(5) = .true.
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
    ! -- Check for required sorption variables
    if (this%isrb > 0) then
      if (.not. lname(1)) then
        write(errmsg, '(1x,a)') 'ERROR.  SORPTION IS ACTIVE BUT RHOB NOT &
          &SPECIFIED.  RHOB MUST BE SPECIFIED IN GRIDDATA BLOCK.'
        call store_error(errmsg)
      endif
      if (.not. lname(2)) then
        write(errmsg, '(1x,a)') 'ERROR.  SORPTION IS ACTIVE BUT DISTRIBUTION &
          &COEFFICIENT NOT SPECIFIED.  DISTCOEF MUST BE SPECIFIED IN &
          &GRIDDATA BLOCK.'
        call store_error(errmsg)
      endif
    else
      if (lname(1)) then
        write(this%iout, '(1x,a)') 'WARNING.  SORPTION IS NOT ACTIVE BUT RHOB &
          &WAS SPECIFIED.  RHOB WILL HAVE NO AFFECT ON SIMULATION RESULTS.'
      endif
      if (lname(2)) then
        write(this%iout, '(1x,a)') 'WARNING.  SORPTION IS NOT ACTIVE BUT &
          &DISTRIBUTION COEFFICIENT WAS SPECIFIED.  DISTCOEF WILL HAVE &
          &NO AFFECT ON SIMULATION RESULTS.'
      endif
    endif
    !
    ! -- Check for required rate coefficients
    if (this%irorder > 0) then
      if (.not. lname(3)) then
        write(errmsg, '(1x,a)') 'ERROR.  FIRST OR ZERO ORDER REACTIONS ARE &
          &ACTIVE BUT THE FIRST RATE COEFFICIENT IS NOT SPECIFIED.  RC1 MUST &
          &BE SPECIFIED IN GRIDDATA BLOCK.'
        call store_error(errmsg)
      endif
      if (.not. lname(4)) then
        write(errmsg, '(1x,a)') 'ERROR.  FIRST OR ZERO ORDER REACTIONS ARE &
          &ACTIVE BUT THE SECOND RATE COEFFICIENT IS NOT SPECIFIED.  RC2 MUST &
          &BE SPECIFIED IN GRIDDATA BLOCK.'
        call store_error(errmsg)
      endif
    else
      if (lname(3)) then
        write(this%iout, '(1x,a)') 'WARNING.  FIRST OR ZERO ORER REACTIONS &
          &ARE NOT ACTIVE BUT RC1 WAS SPECIFIED.  RC1 WILL HAVE NO AFFECT &
          &ON SIMULATION RESULTS.'
      endif
      if (lname(4)) then
        write(this%iout, '(1x,a)') 'WARNING.  FIRST OR ZERO ORER REACTIONS &
          &ARE NOT ACTIVE BUT RC2 WAS SPECIFIED.  RC2 WILL HAVE NO AFFECT &
          &ON SIMULATION RESULTS.'
      endif
    endif
    !
    ! -- terminate if errors
    if(count_errors() > 0) then
      call this%parser%StoreErrorUnit()
      call ustop()
    endif
    !
    ! -- Resize arrays, if necessary
    if (this%isrb == 1) then
      call mem_reallocate(this%strg, this%dis%nodes, 'STRG',                   &
                          trim(this%origin))      
    endif
    !
    ! -- Return
    return
  end subroutine read_data

  
  
end module GwtSrbModule