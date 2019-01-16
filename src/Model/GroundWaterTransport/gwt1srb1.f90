module GwtSrbModule
  
  use KindModule,             only: DP, I4B
  use ConstantsModule,        only: DONE, DZERO
  use NumericalPackageModule, only: NumericalPackageType
  use BaseDisModule,          only: DisBaseType
  use GwtFmiModule,           only: GwtFmiType
  use GwtStoModule,           only: GwtStoType
  use BudgetModule,           only: BudgetType
  
  implicit none
  public :: GwtSrbType
  public :: srb_cr

  type, extends(NumericalPackageType) :: GwtSrbType
    
    real(DP), dimension(:), pointer, contiguous      :: porosity => null()      ! pointer to storage package porosity
    integer(I4B), dimension(:), pointer, contiguous  :: ibound => null()        ! pointer to model ibound
    type(GwtFmiType), pointer                        :: fmi => null()           ! pointer to fmi object
    type(GwtStoType), pointer                        :: sto => null()           ! pointer to sto object
    
    integer(I4B), pointer                            :: irorder => null()       ! order of decay rate (0:none, 1:first, 2:zero)
    real(DP), dimension(:), pointer, contiguous      :: rate => null()          ! rate of transfer to sorbed
    real(DP), dimension(:), pointer, contiguous      :: rhob => null()          ! bulk density
    real(DP), dimension(:), pointer, contiguous      :: distcoef => null()      ! distribution coefficient
    real(DP), dimension(:), pointer, contiguous      :: rc => null()            ! first or zero order rate constant for sorbed mass
    
    !type(BudgetType), pointer                        :: budgetim => null()      ! separate budget object for sorbed mass
    
  contains
  
    procedure :: srb_ar
    procedure :: srb_ad
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

  subroutine srb_ar(this, dis, sto, ibound)
! ******************************************************************************
! srb_ar -- Allocate and Read
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(GwtSrbType) :: this
    class(DisBaseType), pointer, intent(in) :: dis
    type(GwtStoType), pointer, intent(in) :: sto
    integer(I4B), dimension(:), pointer, contiguous :: ibound
    ! -- local
    ! -- formats
    character(len=*), parameter :: fmtsrb =                                    &
      "(1x,/1x,'SRB -- SORBTION PACKAGE, VERSION 1, 01/15/2019',               &
      &' INPUT READ FROM UNIT ', i0, //)"
! ------------------------------------------------------------------------------
    !
    ! --print a message identifying the sorbtion package.
    write(this%iout, fmtsrb) this%inunit
    !
    ! -- store pointers to arguments that were passed in
    this%dis => dis
    this%sto => sto
    this%ibound => ibound
    this%porosity => sto%porosity
    !
    ! -- Read sorbtion options
    call this%read_options()
    !
    ! -- Allocate arrays
    call this%allocate_arrays()
    !
    ! -- read the data block
    call this%read_data()
    !
    ! -- setup the sorbed domain budget
    ! todo: keep track of separate sorbed budget?
    !
    ! -- Return
    return
  end subroutine srb_ar
  
  subroutine srb_ad(this)
! ******************************************************************************
! srb_ad -- advance
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(GwtSrbType) :: this
    ! -- local
    ! -- formats
! ------------------------------------------------------------------------------
    !
    ! -- Nothing to do 
    !
    ! -- Return
    return
  end subroutine srb_ad
  
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
    real(DP) :: swt, swtpdt
    real(DP) :: vcell
    real(DP) :: eqfact
    real(DP) :: ctosrb
    real(DP) :: thetamfrac
! ------------------------------------------------------------------------------
    !
    ! -- set variables
    tled = DONE / delt
    !
    ! -- loop through and calculate sorbtion contribution to hcof and rhs
    do n = 1, this%dis%nodes
      !
      ! -- skip if transport inactive
      if(this%ibound(n) <= 0) cycle
      !
      ! -- calculate new and old water volumes
      vcell = this%dis%area(n) * (this%dis%top(n) - this%dis%bot(n))
      swtpdt = this%fmi%gwfsat(n)
      swt = this%fmi%gwfsatold(n, delt)
      idiag = this%dis%con%ia(n)
      !
      ! -- Set thetamfrac
      thetamfrac = this%sto%get_thetamfrac(n)
      !
      ! -- add sorbtion terms to hcof and rhs accumulators
      eqfact = -this%rhob(n) * vcell * tled
      ctosrb = this%distcoef(n)
      hhcof =  thetamfrac * eqfact * ctosrb * swtpdt
      rrhs = thetamfrac * eqfact * ctosrb * swt * cold(n)
      !
      ! -- add sorbed mass decay rate terms to accumulators
      if (this%irorder == 1) then
        !
        ! -- first order decay rate is a function of concentration, so add
        !    to left hand side
        hhcof = hhcof - this%rc(n) * vcell * this%rhob(n) * ctosrb
      elseif (this%irorder == 2) then
        !
        ! -- zero-order decay rate is not a function of concentration, so add
        !    to right hand side
        rrhs = rrhs + this%rc(n) * ctosrb * vcell
      endif
      !
      ! -- Add hhcof to diagonal and rrhs to right-hand side
      amatsln(idxglo(idiag)) = amatsln(idxglo(idiag)) + hhcof
      rhs(n) = rhs(n) + rrhs
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
    integer(I4B) :: idiag
    real(DP) :: rate
    real(DP) :: tled
    real(DP) :: swt, swtpdt
    real(DP) :: rsrbin, rsrbout
    real(DP) :: rrctin, rrctout
    real(DP) :: rddmin, rddmout
    real(DP) :: hhcof, rrhs
    real(DP) :: vcell
    real(DP) :: eqfact
    real(DP) :: ctosrb
    real(DP) :: thetamfrac
! ------------------------------------------------------------------------------
    !
    ! -- initialize 
    rsrbin = DZERO
    rsrbout = DZERO
    rrctin = DZERO
    rrctout = DZERO
    rddmin = DZERO
    rddmout = DZERO
    tled = DONE / delt
    !
    ! -- Calculate sorbtion change
    do n = 1, nodes
      !
      ! -- skip if transport inactive
      if(this%ibound(n) <= 0) cycle
      !
      ! -- calculate new and old water volumes
      vcell = this%dis%area(n) * (this%dis%top(n) - this%dis%bot(n))
      swtpdt = this%fmi%gwfsat(n)
      swt = this%fmi%gwfsatold(n, delt)
      idiag = this%dis%con%ia(n)
      !
      ! -- Set thetamfrac
      thetamfrac = this%sto%get_thetamfrac(n)
      !
      ! -- calculate sorbtion rate
      this%rate(n) = DZERO
      eqfact = -this%rhob(n) * vcell * tled
      ctosrb = this%distcoef(n)
      hhcof =  thetamfrac * eqfact * ctosrb * swtpdt
      rrhs = thetamfrac * eqfact * ctosrb * swt * cold(n)
      rate = hhcof * cnew(n) - rrhs
      this%rate(n) = rate
      if (rate < DZERO) then
        rsrbout = rsrbout - rate
      else
        rsrbin = rsrbin + rate
      endif
      !
      ! -- calculate decay gains and losses
      rate = DZERO
      hhcof = DZERO
      rrhs = DZERO
      if (this%irorder == 1) then
        hhcof = - this%rc(n) * vcell * this%rhob(n) * ctosrb
      elseif (this%irorder == 2) then
        rrhs = this%rc(n) * ctosrb * vcell
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
    call model_budget%addentry(rsrbin, rsrbout, delt, '        SORBTION',      &
                                isuppress_output)
    !
    ! -- Add decay contributions to model budget
    if (this%irorder > 0) then
      call model_budget%addentry(rrctin, rrctout, delt, '           DECAY',    &
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
    ! -- Record the sorbtion rates if requested
    if(ibinun /= 0) then
      iprint = 0
      dinact = DZERO
      call this%dis%record_array(this%rate, this%iout, iprint, -ibinun,        &
                                 '        SORBTION', cdatafmp, nvaluesp,       &
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
      call mem_deallocate(this%rate)
      call mem_deallocate(this%rhob)
      call mem_deallocate(this%distcoef)
      call mem_deallocate(this%rc)
      this%ibound => null()
      this%porosity => null()
      this%fmi => null()
    endif
    !
    ! -- Scalars
    call mem_deallocate(this%irorder)
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
    call mem_allocate(this%irorder, 'IRORDER', this%origin)
    !
    ! -- Initialize
    this%irorder = 0
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
    call mem_allocate(this%rate, this%dis%nodes, 'RATE', this%origin)
    call mem_allocate(this%rhob, this%dis%nodes, 'RHOB', this%origin)
    call mem_allocate(this%distcoef,  this%dis%nodes, 'DISTCOEF', this%origin)
    if (this%irorder == 0) then
      call mem_allocate(this%rc, 1, 'RC', this%origin)
    else
      call mem_allocate(this%rc, this%dis%nodes, 'RC', this%origin)
    endif
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
    ! -- modules
    use ConstantsModule,   only: LINELENGTH
    use SimModule,         only: ustop, store_error
    ! -- dummy
    class(GwtSrbType) :: this
    ! -- local
    character(len=LINELENGTH) :: errmsg, keyword
    integer(I4B) :: ierr
    logical :: isfound, endOfBlock
    ! -- formats
    character(len=*), parameter :: fmtisvflow =                                &
      "(4x,'CELL-BY-CELL FLOW INFORMATION WILL BE SAVED TO BINARY FILE " //    &
      "WHENEVER ICBCFL IS NOT ZERO.')"
    character(len=*), parameter :: fmtirorder1 =                               &
      "(4x,'FIRST-ORDER DECAY IS ACTIVE. ')"
    character(len=*), parameter :: fmtirorder2 =                               &
      "(4x,'ZERO-ORDER DECAY IS ACTIVE. ')"
! ------------------------------------------------------------------------------
    !
    ! -- get options block
    call this%parser%GetBlock('OPTIONS', isfound, ierr, blockRequired=.false.)
    !
    ! -- parse options block if detected
    if (isfound) then
      write(this%iout,'(1x,a)')'PROCESSING SORBTION OPTIONS'
      do
        call this%parser%GetNextLine(endOfBlock)
        if (endOfBlock) exit
        call this%parser%GetStringCaps(keyword)
        select case (keyword)
          case ('SAVE_FLOWS')
            this%ipakcb = -1
            write(this%iout, fmtisvflow)
          case ('FIRST_ORDER_DECAY')
            this%irorder = 1
            write(this%iout, fmtirorder1)
          case ('ZERO_ORDER_DECAY')
            this%irorder = 2
            write(this%iout, fmtirorder2)
          case default
            write(errmsg,'(4x,a,a)')'****ERROR. UNKNOWN SRB OPTION: ',         &
                                     trim(keyword)
            call store_error(errmsg)
            call this%parser%StoreErrorUnit()
            call ustop()
        end select
      end do
      write(this%iout,'(1x,a)')'END OF SORBTION OPTIONS'
    end if
    !
    ! -- Return
    return
  end subroutine read_options

  subroutine read_data(this)
! ******************************************************************************
! read_data -- read the sorbtion data (griddata) block
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
    logical, dimension(3) :: lname
    character(len=24), dimension(3) :: aname
    ! -- formats
    ! -- data
    data aname(1) /'            BULK DENSITY'/
    data aname(2) /'DISTRIBUTION COEFFICIENT'/
    data aname(3) /'        RATE COEFFICIENT'/
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
            call this%dis%read_grid_array(line, lloc, istart, istop, this%iout,&
                                         this%parser%iuactive, this%rhob,      &
                                         aname(1))
            lname(1) = .true.
          case ('DISTCOEF')
            call this%dis%read_grid_array(line, lloc, istart, istop, this%iout,&
                                         this%parser%iuactive, this%distcoef,  &
                                         aname(2))
            lname(2) = .true.
          case ('RC')
            if (this%irorder == 0)                                             &
              call mem_reallocate(this%rc, this%dis%nodes, 'RC', this%origin)
            call this%dis%read_grid_array(line, lloc, istart, istop, this%iout,&
                                         this%parser%iuactive, this%rc,        &
                                         aname(3))
            lname(3) = .true.
          case default
            write(errmsg,'(4x,a,a)')'ERROR. UNKNOWN GRIDDATA TAG: ',           &
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
    ! -- Check for required sorbtion variables
    if (.not. lname(1)) then
      write(errmsg, '(1x,a)') 'ERROR.  SORBTION IS ACTIVE BUT RHOB NOT &
        &SPECIFIED.  RHOB MUST BE SPECIFIED IN GRIDDATA BLOCK.'
      call store_error(errmsg)
    endif
    if (.not. lname(2)) then
      write(errmsg, '(1x,a)') 'ERROR.  SORBTION IS ACTIVE BUT DISTRIBUTION &
        &COEFFICIENT NOT SPECIFIED.  DISTCOEF MUST BE SPECIFIED IN &
        &GRIDDATA BLOCK.'
      call store_error(errmsg)
    endif
    !
    ! -- Check for required decay/production rate coefficients
    if (this%irorder > 0) then
      if (.not. lname(3)) then
        write(errmsg, '(1x,a)') 'ERROR.  FIRST OR ZERO ORDER DECAY IS &
          &ACTIVE BUT THE RATE COEFFICIENT IS NOT SPECIFIED.  RC MUST &
          &BE SPECIFIED IN GRIDDATA BLOCK.'
        call store_error(errmsg)
      endif
    else
      if (lname(3)) then
        write(this%iout, '(1x,a)') 'WARNING.  FIRST OR ZERO ORER DECAY &
          &IS NOT ACTIVE BUT RC WAS SPECIFIED.  RC WILL HAVE NO AFFECT &
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
    ! -- Return
    return
  end subroutine read_data
    
end module GwtSrbModule
