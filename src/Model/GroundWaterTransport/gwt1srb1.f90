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
    real(DP), dimension(:), pointer, contiguous      :: strg => null()          ! rate of sorbed mass storage
    integer(I4B), dimension(:), pointer, contiguous  :: ibound => null()        ! pointer to model ibound
    type(GwtFmiType), pointer                        :: fmi => null()           ! pointer to fmi object
    
    real(DP), dimension(:), pointer, contiguous      :: rhob => null()          ! bulk density
    real(DP), dimension(:), pointer, contiguous      :: srconc => null()        ! initial sorbed concentration
    real(DP), dimension(:), pointer, contiguous      :: distcoef => null()      ! distribution coefficient
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
    call this%allocate_arrays(dis%nodes)
    !
    ! -- Read sorption options
    call this%read_options()
    !
    ! -- read the data block
    call this%read_data()
    !
    ! -- Set new and old sorbed concentrations equal to the intial sorbed conc
    do i = 1, this%dis%nodes
      this%csrbnew(i) = this%srconc(i)
      this%csrbold(i) = this%srconc(i)
    enddo
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
    real(DP) :: cfact
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
      !
      ! -- add terms to diagonal and rhs accumulators
      cfact = this%distcoef(n)
      hhcof = this%rhob(n) * vcell * tled * cfact * this%fmi%gwfsat(n)
      rrhs = this%rhob(n) * vcell * tled * cfact * gwfsatold * cold(n)
      idiag = this%dis%con%ia(n)
      amatsln(idxglo(idiag)) = amatsln(idxglo(idiag)) + hhcof
      rhs(n) = rhs(n) + rrhs
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
    real(DP) :: rin, rout
    real(DP) :: vnew, vold
    real(DP) :: hhcof, rrhs
    real(DP) :: vcell
    real(DP) :: gwfsatold
    real(DP) :: cfact
! ------------------------------------------------------------------------------
    !
    ! -- initialize 
    rin = DZERO
    rout = DZERO
    tled = DONE / delt
    !
    ! -- Calculate sorption change
    do n = 1, nodes
      this%strg(n) = DZERO
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
      ! -- calculate rate
      cfact = this%distcoef(n)
      hhcof = this%rhob(n) * vcell * tled * cfact * this%fmi%gwfsat(n)
      rrhs = this%rhob(n) * vcell * tled * cfact * gwfsatold * cold(n)
      rate = hhcof * cnew(n) - rrhs
      this%strg(n) = rate
      if(rate < DZERO) then
        rout = rout - rate
      else
        rin = rin + rate
      endif
    enddo
    !
    ! -- Add contributions to model budget
    call model_budget%addentry(rin, rout, delt, '        SORPTION',            &
                               isuppress_output)
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
      call mem_deallocate(this%porosity)
      call mem_deallocate(this%strg)
      this%ibound => null()
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
    !
    ! -- Initialize
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
    class(GwtSrbType) :: this
    integer(I4B), intent(in) :: nodes
    ! -- local
    integer(I4B) :: n
! ------------------------------------------------------------------------------
    !
    ! -- Allocate
    call mem_allocate(this%strg, nodes, 'STRG', this%origin)
    call mem_allocate(this%rhob, nodes, 'RHOB', this%origin)
    call mem_allocate(this%srconc, nodes, 'SRCONC', this%origin)
    call mem_allocate(this%distcoef, nodes, 'DISTCOEF', this%origin)
    !
    ! -- Initialize
    do n = 1, nodes
      this%strg(n) = DZERO
      this%rhob(n) = DZERO
      this%srconc(n) = DZERO
      this%distcoef(n) = DZERO
    enddo
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
    character(len=LINELENGTH) :: errmsg, keyword
    integer(I4B) :: ierr
    logical :: isfound, endOfBlock
    ! -- formats
    character(len=*), parameter :: fmtisvflow =                                &
      "(4x,'CELL-BY-CELL FLOW INFORMATION WILL BE SAVED TO BINARY FILE " //    &
      "WHENEVER ICBCFL IS NOT ZERO.')"
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
          case default
            write(errmsg,'(4x,a,a)')'****ERROR. UNKNOWN SRB OPTION: ',         &
                                     trim(keyword)
            call store_error(errmsg)
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
    ! -- dummy
    class(GwtSrbType) :: this
    ! -- local
    character(len=LINELENGTH) :: line, errmsg, keyword
    integer(I4B) :: istart, istop, lloc, ierr
    integer(I4B) :: i
    logical :: isfound, endOfBlock
    logical, dimension(3) :: lname
    character(len=24), dimension(3) :: aname
    ! -- formats
    ! -- data
    data aname(1) /'            BULK DENSITY'/
    data aname(2) /'     INITIAL SORBED CONC'/
    data aname(3) /'DISTRIBUTION COEFFICIENT'/
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
          case ('SRCONC')
            call this%dis%read_grid_array(line, lloc, istart, istop, this%iout,&
                                         this%parser%iuactive, this%srconc,    &
                                         aname(2))
            lname(2) = .true.
          case ('DISTCOEF')
            call this%dis%read_grid_array(line, lloc, istart, istop, this%iout,&
                                         this%parser%iuactive, this%distcoef,  &
                                         aname(3))
            lname(3) = .true.
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
    ! -- Check for required variables
    do i = 1, 3
      if(.not. lname(i)) then
        write(errmsg, '(a, a, a)') 'Error in GRIDDATA block: ',                   &
                                   trim(adjustl(aname(i))), ' not found.'
        call store_error(errmsg)
      end if
    end do
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