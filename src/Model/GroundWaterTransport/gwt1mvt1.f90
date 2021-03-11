! -- Groundwater Transport Mover Module
! -- This module is responsible for sending mass from providers into
! -- receiver qmfrommvr arrays and writing a mover transport budget
  
module GwtMvtModule
  
  use KindModule, only: DP, I4B
  use ConstantsModule, only: LINELENGTH, MAXCHARLEN, DZERO, LENPAKLOC, &
                             DNODATA, LENPACKAGENAME, TABCENTER, LENMODELNAME
  use SimModule, only: store_error, ustop
  use BaseDisModule, only: DisBaseType
  use NumericalPackageModule, only: NumericalPackageType
  use GwtFmiModule, only: GwtFmiType
  use BudgetModule, only: BudgetType, budget_cr
  use BudgetObjectModule, only: BudgetObjectType, budgetobject_cr
  use TableModule, only: TableType, table_cr

  implicit none
  
  private
  public :: GwtMvtType
  public :: mvt_cr
  
  type, extends(NumericalPackageType) :: GwtMvtType
    integer(I4B), pointer                              :: maxpackages           ! max number of packages
    integer(I4B), pointer                              :: ibudgetout => null()  ! unit number for budget output file
    type(GwtFmiType), pointer                          :: fmi => null()         ! pointer to fmi object
    type(BudgetType), pointer                          :: budget => null()      ! mover budget object (used to write balance table)
    type(BudgetObjectType), pointer                    :: budobj => null()      ! budget container (used to write binary file)
    character(len=LENPACKAGENAME),                                             &
      dimension(:), pointer, contiguous                :: paknames => null()       !array of package names
    !
    ! -- table objects
    type(TableType), pointer :: outputtab => null()
  contains
    procedure :: mvt_df
    procedure :: mvt_ar
    procedure :: mvt_rp
    procedure :: mvt_fc
    procedure :: mvt_cc
    procedure :: mvt_bd
    procedure :: mvt_ot_saveflow
    procedure :: mvt_ot_printflow
    procedure :: mvt_ot_bdsummary
    procedure :: mvt_da
    procedure :: allocate_scalars
    procedure :: read_options
    procedure :: mvt_setup_budobj
    procedure :: mvt_fill_budobj
    procedure :: mvt_scan_mvrbudobj
    procedure, private :: mvt_setup_outputtab
    procedure, private :: mvt_print_outputtab
  end type GwtMvtType

  contains

  subroutine mvt_cr(mvt, name_model, inunit, iout, fmi)
! ******************************************************************************
! mvt_cr -- Create a new initial conditions object
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    type(GwtMvtType), pointer :: mvt
    character(len=*), intent(in) :: name_model
    integer(I4B), intent(in) :: inunit
    integer(I4B), intent(in) :: iout
    type(GwtFmiType), intent(in), target :: fmi
! ------------------------------------------------------------------------------
    !
    ! -- Create the object
    allocate(mvt)
    !
    ! -- create name and memory path
    call mvt%set_names(1, name_model, 'MVT', 'MVT')
    !
    ! -- Allocate scalars
    call mvt%allocate_scalars()
    !
    mvt%inunit = inunit
    mvt%iout = iout
    !
    ! -- set pointers
    mvt%fmi => fmi
    !
    ! -- Return
    return
  end subroutine mvt_cr

  subroutine mvt_df(this, dis)
! ******************************************************************************
! mvt_df -- Define
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(GwtMvtType) :: this
    class(DisBaseType), pointer, intent(in) :: dis
    ! -- local
    ! -- formats
    character(len=*), parameter :: fmtmvt =                                    &
      "(1x,/1x,'MVT -- MOVER TRANSPORT PACKAGE, VERSION 1, 4/15/2020',         &
      &' INPUT READ FROM UNIT ', i0, //)"
! ------------------------------------------------------------------------------
    !
    ! -- set pointer to dis
    this%dis => dis
    !
    ! --print a message identifying the MVT package.
    write(this%iout, fmtmvt) this%inunit
    !
    ! -- Initialize block parser
    call this%parser%Initialize(this%inunit, this%iout)
    !
    ! -- initialize the budget table writer
    call budget_cr(this%budget, this%memoryPath)
    !
    ! -- Read mvt options
    call this%read_options()
    !
    ! -- Return
    return
  end subroutine mvt_df
  
  subroutine mvt_ar(this)
! ******************************************************************************
! mvt_ar -- Allocate and read water mover information
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(GwtMvtType) :: this
    ! -- locals
! ------------------------------------------------------------------------------
    !
    ! -- setup the output table
    call this%mvt_setup_outputtab()
    !
    ! -- Return
    return
  end subroutine mvt_ar

  subroutine mvt_rp(this)
! ******************************************************************************
! mvt_rp -- Read and prepare
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use TdisModule, only: kper, kstp
    ! -- dummy
    class(GwtMvtType) :: this
    ! -- local
    ! -- formats
! ------------------------------------------------------------------------------
    !
    ! -- At this point, the mvrbudobj is available to set up the mvt budobj
    if (kper * kstp == 1) then
      !
      ! -- set up the mvt budobject
      call this%mvt_scan_mvrbudobj()
      call this%mvt_setup_budobj()
      !
      ! -- Define the budget object to be the size of maxpackages
      call this%budget%budget_df(this%maxpackages, 'TRANSPORT MOVER', bddim='M')
    end if
    !
    ! -- Return
    return
  end subroutine mvt_rp
  
  subroutine mvt_fc(this, nodes, cold, nja, njasln, amatsln, idxglo, cnew, rhs)
! ******************************************************************************
! mvt_fc -- Calculate coefficients and fill amat and rhs
!
!   The mvt package adds the mass flow rate to the provider qmfrommvr
!   array.  The advanced packages know enough to subract any mass that is
!   leaving, so the mvt just adds mass coming in from elsewhere.  Because the
!   movers change change by stress period, their solute effects must be
!   added to the right-hand side of the gwt matrix equations.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(GwtMvtType) :: this
    integer, intent(in) :: nodes
    real(DP), intent(in), dimension(nodes) :: cold
    integer(I4B), intent(in) :: nja
    integer(I4B), intent(in) :: njasln
    real(DP), dimension(njasln), intent(inout) :: amatsln
    integer(I4B), intent(in), dimension(nja) :: idxglo
    real(DP), intent(in), dimension(nodes) :: cnew
    real(DP), intent(inout), dimension(nodes) :: rhs
    ! -- local
    integer(I4B) :: i, n
    integer(I4B) :: id1, id2, nlist
    integer(I4B) :: ipr, irc
    integer(I4B) :: igwtnode
    integer(I4B) :: nbudterm
    real(DP) :: q, cp
    real(DP), dimension(:), pointer :: concpak
! ------------------------------------------------------------------------------
    !
    ! -- initialize the mass flow into advanced package from the mover
    do i = 1, this%fmi%nflowpack
      if (this%fmi%iatp(i) == 0) cycle
      do n = 1, size(this%fmi%datp(i)%qmfrommvr)
        this%fmi%datp(i)%qmfrommvr(n) = DZERO
      end do
    end do
    !
    ! -- Add mover QC terms to the receiver packages
    nbudterm = this%fmi%mvrbudobj%nbudterm
    do i = 1, nbudterm
      nlist = this%fmi%mvrbudobj%budterm(i)%nlist
      if (nlist > 0) then
        call this%fmi%get_package_index(this%fmi%mvrbudobj%budterm(i)%text2id1, ipr)
        call this%fmi%get_package_index(this%fmi%mvrbudobj%budterm(i)%text2id2, irc)
        if (this%fmi%iatp(ipr) /= 0) concpak => this%fmi%datp(ipr)%concpack
        do n = 1, nlist
          !
          ! -- lak/sfr/maw/uzf id1 (provider) and id2 (receiver)
          id1 = this%fmi%mvrbudobj%budterm(i)%id1(n)
          id2 = this%fmi%mvrbudobj%budterm(i)%id2(n)
          !
          ! -- Obtain mover flow rate from the mover flow budget object
          q = this%fmi%mvrbudobj%budterm(i)%flow(n)
          !
          ! -- Assign concentration of the provider
          cp = DZERO
          if (this%fmi%iatp(ipr) /= 0) then
            !
            ! -- Provider package is being represented by an APT (SFT, LKT, MWT, UZT)
            !    so set the concentration to the simulated concentation of APT
            cp = concpak(id1)
          else
            !
            ! -- Provider is a regular stress package (WEL, DRN, RIV, etc.) or the
            !    provider is an advanced stress package but is not represented with
            !    SFT, LKT, MWT, or UZT, so use the GWT cell concentration 
            igwtnode = this%fmi%gwfpackages(ipr)%nodelist(id1)
            cp = cnew(igwtnode)
          end if
          !
          ! -- add the mover rate times the provider concentration into the receiver
          !    make sure these are accumulated since multiple providers can move
          !    water into the same receiver
          if (this%fmi%iatp(irc) /= 0) then
            this%fmi%datp(irc)%qmfrommvr(id2) = this%fmi%datp(irc)%qmfrommvr(id2) - q * cp
          end if
        end do
      end if
    end do
    !
    ! -- Return
    return
  end subroutine mvt_fc

  subroutine mvt_cc(this, kiter, iend, icnvgmod, cpak, dpak)
! ******************************************************************************
! mvt_cc -- extra convergence check for mover
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(GwtMvtType) :: this
    integer(I4B),intent(in) :: kiter
    integer(I4B),intent(in) :: iend
    integer(I4B),intent(in) :: icnvgmod
    character(len=LENPAKLOC), intent(inout) :: cpak
    real(DP), intent(inout) :: dpak
    ! -- local
    ! -- formats
    character(len=*),parameter :: fmtmvrcnvg = &
      "(/,1x,'MOVER PACKAGE REQUIRES AT LEAST TWO OUTER ITERATIONS. CONVERGE &
      &FLAG HAS BEEN RESET TO FALSE.')"
! ------------------------------------------------------------------------------
    !
    ! -- If there are active movers, then at least 2 outers required
    if (associated(this%fmi%mvrbudobj)) then
      if (icnvgmod == 1 .and. kiter == 1) then
        dpak = DNODATA
        cpak = trim(this%packName)
        write(this%iout, fmtmvrcnvg)
      endif
    endif
    !
    ! -- return
    return
  end subroutine mvt_cc
  
  subroutine mvt_bd(this, cnew)
! ******************************************************************************
! mvt_bd -- Write mover terms to listing file
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(GwtMvtType) :: this
    real(DP), dimension(:), intent(in)  :: cnew
    ! -- local
! ------------------------------------------------------------------------------
    !
    ! -- fill the budget object
    call this%mvt_fill_budobj(cnew)
    !
    ! -- return
    return
  end subroutine mvt_bd
  
  subroutine mvt_ot_saveflow(this, icbcfl, ibudfl)
! ******************************************************************************
! mvt_bd -- Write mover terms
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use TdisModule, only : kstp, kper, delt, pertim, totim
    ! -- dummy
    class(GwtMvttype) :: this
    integer(I4B), intent(in) :: icbcfl
    integer(I4B), intent(in) :: ibudfl
    ! -- locals
    integer(I4B) :: ibinun
! ------------------------------------------------------------------------------
    !
    ! -- Save the mover flows from the budobj to a mover binary file
    ibinun = 0
    if(this%ibudgetout /= 0) then
      ibinun = this%ibudgetout
    end if
    if(icbcfl == 0) ibinun = 0
    if (ibinun > 0) then
      call this%budobj%save_flows(this%dis, ibinun, kstp, kper, delt, &
                        pertim, totim, this%iout)
    end if
    !
    ! -- Return
    return
  end subroutine mvt_ot_saveflow

  subroutine mvt_ot_printflow(this, icbcfl, ibudfl)
! ******************************************************************************
! mvr_ot_printflow -- Print mover flow table
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(GwtMvtType) :: this
    integer(I4B), intent(in) :: icbcfl
    integer(I4B), intent(in) :: ibudfl
    ! -- locals
! ------------------------------------------------------------------------------
    !
    ! -- Print the mover flow table
    if (ibudfl /= 0 .and. this%iprflow /= 0) then
      call this%mvt_print_outputtab()
    end if
    !
    ! -- Return
    return
  end subroutine mvt_ot_printflow

  subroutine mvt_ot_bdsummary(this)
! ******************************************************************************
! mvt_ot_bdsummary -- Write mover budget to listing file
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use TdisModule, only: kstp, kper, delt
    use ArrayHandlersModule, only: ifind, expandarray
    ! -- dummy
    class(GwtMvtType) :: this
    ! -- locals
    integer(I4B) :: i, j, n
    real(DP), allocatable, dimension(:) :: ratin, ratout
! ------------------------------------------------------------------------------
    !
    ! -- Allocate and initialize ratin/ratout
    allocate(ratin(this%maxpackages), ratout(this%maxpackages))
    do j = 1, this%maxpackages
      ratin(j) = DZERO
      ratout(j) = DZERO
    enddo
    !
    ! -- Accumulate the rates
    do i = 1, this%maxpackages
      
      do j = 1, this%budobj%nbudterm
      
        do n = 1, this%budobj%budterm(j)%nlist

          !
          ! -- provider is inflow to mover
          if(this%paknames(i) == this%budobj%budterm(j)%text2id1) then
            ratin(i) = ratin(i) + this%budobj%budterm(j)%flow(n)
          endif
          !
          ! -- receiver is outflow from mover
          if(this%paknames(i) == this%budobj%budterm(j)%text2id2) then
            ratout(i) = ratout(i) + this%budobj%budterm(j)%flow(n)
          endif
        
        end do
        
      end do
      
    end do
    
    !
    ! -- Send rates to budget object
    call this%budget%reset()
    do j = 1, this%maxpackages
      call this%budget%addentry(ratin(j), ratout(j), delt, this%paknames(j))
    enddo
    !
    ! -- Write the budget
    call this%budget%budget_ot(kstp, kper, this%iout)
    !
    ! -- Deallocate
    deallocate(ratin, ratout)
    !
    ! -- Output mvr budget
    !    Not using budobj write_table here because it would result
    !    in a table that has one entry.  A custom table looks
    !    better here with a row for each package.
    !call this%budobj%write_budtable(kstp, kper, this%iout)
    !
    ! -- Return
    return
  end subroutine mvt_ot_bdsummary

  subroutine mvt_da(this)
! ******************************************************************************
! mvt_da -- deallocate
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryManagerModule, only: mem_deallocate
    ! -- dummy
    class(GwtMvtType) :: this
    ! -- local
! ------------------------------------------------------------------------------
    !
    ! -- Deallocate arrays if package was active
    if(this%inunit > 0) then
      !
      ! -- character array
      deallocate(this%paknames)
      !
      ! -- budget object
      call this%budget%budget_da()
      deallocate(this%budget)
      !
      ! -- budobj
      call this%budobj%budgetobject_da()
      deallocate(this%budobj)
      nullify(this%budobj)
      !
      ! -- output table object
      if (associated(this%outputtab)) then
        call this%outputtab%table_da()
        deallocate(this%outputtab)
        nullify(this%outputtab)
      end if
    endif
    !
    ! -- Scalars
    this%fmi => null()
    call mem_deallocate(this%maxpackages)
    call mem_deallocate(this%ibudgetout)
    !
    ! -- deallocate scalars in NumericalPackageType
    call this%NumericalPackageType%da()
    !
    ! -- Return
    return
  end subroutine mvt_da

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
    class(GwtMvtType) :: this
    ! -- local
! ------------------------------------------------------------------------------
    !
    ! -- allocate scalars in NumericalPackageType
    call this%NumericalPackageType%allocate_scalars()
    !
    ! -- Allocate
    call mem_allocate(this%maxpackages, 'MAXPACKAGES', this%memoryPath)
    call mem_allocate(this%ibudgetout, 'IBUDGETOUT', this%memoryPath)
    !
    ! -- Initialize
    this%maxpackages = 0
    this%ibudgetout = 0
    !
    ! -- Return
    return
  end subroutine allocate_scalars

  subroutine read_options(this)
! ******************************************************************************
! read_options -- Read Options
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use OpenSpecModule, only: access, form
    use InputOutputModule, only: getunit, openfile
    ! -- dummy
    class(GwtMvtType) :: this
    ! -- local
    character(len=LINELENGTH) :: errmsg, keyword
    character(len=MAXCHARLEN) :: fname
    integer(I4B) :: ierr
    logical :: isfound, endOfBlock
    character(len=*),parameter :: fmtflow = &
      "(4x, a, 1x, a, 1x, ' WILL BE SAVED TO FILE: ', a, /4x, 'OPENED ON UNIT: ', I7)"
    character(len=*),parameter :: fmtflow2 = &
      "(4x, 'FLOWS WILL BE SAVED TO BUDGET FILE')"
! ------------------------------------------------------------------------------
    !
    ! -- get options block
    call this%parser%GetBlock('OPTIONS', isfound, ierr, blockRequired=.false., &
                              supportOpenClose=.true.)
    !
    ! -- parse options block if detected
    if (isfound) then
      write(this%iout,'(1x,a)')'PROCESSING MVT OPTIONS'
      do
        call this%parser%GetNextLine(endOfBlock)
        if (endOfBlock) exit
        call this%parser%GetStringCaps(keyword)
        select case (keyword)
          case ('SAVE_FLOWS')
            this%ipakcb = -1
            write(this%iout, fmtflow2)
          case ('PRINT_INPUT')
            this%iprpak = 1
            write(this%iout,'(4x,a)') 'MVT INPUT WILL BE PRINTED.'
          case ('PRINT_FLOWS')
            this%iprflow = 1
            write(this%iout,'(4x,a)') &
              'MVT FLOWS WILL BE PRINTED TO LISTING FILE.'
          case('BUDGET')
            call this%parser%GetStringCaps(keyword)
            if (keyword == 'FILEOUT') then
              call this%parser%GetString(fname)
              this%ibudgetout = getunit()
              call openfile(this%ibudgetout, this%iout, fname, 'DATA(BINARY)',  &
                            form, access, 'REPLACE')
              write(this%iout,fmtflow) 'MVT', 'BUDGET', fname, this%ibudgetout
            else
              call store_error('OPTIONAL BUDGET KEYWORD MUST BE FOLLOWED BY FILEOUT')
            end if
          case default
            write(errmsg,'(4x,a,a)')'***ERROR. UNKNOWN MVT OPTION: ', &
                                     trim(keyword)
            call store_error(errmsg)
            call this%parser%StoreErrorUnit()
            call ustop()
        end select
      end do
      write(this%iout,'(1x,a)') 'END OF MVT OPTIONS'
    end if
    !
    ! -- return
    return
  end subroutine read_options

  subroutine mvt_setup_budobj(this)
! ******************************************************************************
! mvt_setup_budobj -- Set up the budget object that stores all the mvr flows
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use ConstantsModule, only: LENBUDTXT, LENMODELNAME, LENPACKAGENAME
    ! -- dummy
    class(GwtMvtType) :: this
    ! -- local
    integer(I4B) :: nbudterm
    integer(I4B) :: ncv
    integer(I4B) :: maxlist
    integer(I4B) :: i
    integer(I4B) :: naux
    character (len=LENMODELNAME) :: modelname1, modelname2
    character (len=LENPACKAGENAME) :: packagename1, packagename2
    character(len=LENBUDTXT) :: text
! ------------------------------------------------------------------------------
    !
    ! -- Assign terms to set up the mover budget object
    nbudterm = this%fmi%mvrbudobj%nbudterm
    ncv = 0
    text = '        MVT-FLOW'
    naux = 0
    !
    ! -- set up budobj
    call budgetobject_cr(this%budobj, 'TRANSPORT MOVER')
    call this%budobj%budgetobject_df(ncv, nbudterm, 0, 0, bddim_opt='M')
    !
    ! -- Go through the water mover budget terms and set up the transport
    !    mover budget terms
    do i = 1, nbudterm
      modelname1 = this%fmi%mvrbudobj%budterm(i)%text1id1
      packagename1 = this%fmi%mvrbudobj%budterm(i)%text2id1
      modelname2 = this%fmi%mvrbudobj%budterm(i)%text1id2
      packagename2 = this%fmi%mvrbudobj%budterm(i)%text2id2
      maxlist = this%fmi%mvrbudobj%budterm(i)%maxlist
      call this%budobj%budterm(i)%initialize(text, &
                                             modelname1, &
                                             packagename1, &
                                             modelname2, &
                                             packagename2, &
                                             maxlist, .false., .false., &
                                             naux)
    end do
    
    !
    ! -- return
    return
  end subroutine mvt_setup_budobj

  subroutine mvt_fill_budobj(this, cnew)
! ******************************************************************************
! mvt_fill_budobj -- copy flow terms into this%budobj
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(GwtMvtType) :: this
    real(DP), intent(in), dimension(:) :: cnew
    ! -- local
    integer(I4B) :: nbudterm
    integer(I4B) :: nlist
    integer(I4B) :: ipr
    integer(I4B) :: irc
    integer(I4B) :: i
    integer(I4B) :: j
    integer(I4B) :: n1, n2
    integer(I4B) :: igwtnode
    real(DP) :: cp
    real(DP) :: q
    real(DP) :: rate
    ! -- formats
! -----------------------------------------------------------------------------
    !
    ! -- Go through the water mover budget terms and set up the transport
    !    mover budget terms
    nbudterm = this%fmi%mvrbudobj%nbudterm
    do i = 1, nbudterm
      nlist = this%fmi%mvrbudobj%budterm(i)%nlist
      call this%fmi%get_package_index(this%fmi%mvrbudobj%budterm(i)%text2id1, ipr)
      call this%fmi%get_package_index(this%fmi%mvrbudobj%budterm(i)%text2id2, irc)
      call this%budobj%budterm(i)%reset(nlist)
      do j = 1, nlist
        n1 = this%fmi%mvrbudobj%budterm(i)%id1(j)
        n2 = this%fmi%mvrbudobj%budterm(i)%id2(j)
        q = this%fmi%mvrbudobj%budterm(i)%flow(j)
        cp = DZERO
        if (this%fmi%iatp(ipr) /= 0) then
          cp = this%fmi%datp(ipr)%concpack(n1)
        else
          ! -- Must be a regular stress package
          igwtnode = this%fmi%gwfpackages(ipr)%nodelist(n1)
          cp = cnew(igwtnode)
        end if
        !
        ! -- Calculate solute mover rate
        rate = DZERO
        if (this%fmi%iatp(irc) /= 0) then
          rate = -q * cp
        end if
        !
        ! -- add the rate to the budterm
        call this%budobj%budterm(i)%update_term(n1, n2, rate)
      end do
    end do
    !
    ! --Terms are filled, now accumulate them for this time step
    call this%budobj%accumulate_terms()
    !
    ! -- return
    return
  end subroutine mvt_fill_budobj

  subroutine mvt_scan_mvrbudobj(this)
! ******************************************************************************
! mvt_scan_mvrbudobj -- scan through the gwf water mover budget object and
!   determine the maximum number of packages and unique package names
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    class(GwtMvtType) :: this
    integer(I4B) :: nbudterm
    integer(I4B) :: maxpackages
    integer(I4B) :: i, j
    integer(I4B) :: ipos
    logical :: found
! ------------------------------------------------------------------------------
    !
    ! -- Calculate maxpackages, which is the the square of nbudterm
    nbudterm = this%fmi%mvrbudobj%nbudterm
    do i = 1, nbudterm
      if (i * i == nbudterm) then
        maxpackages = i
        exit
      end if
    end do
    this%maxpackages = maxpackages
    !
    ! -- allocate paknames
    allocate(this%paknames(this%maxpackages))
    do i = 1, this%maxpackages
      this%paknames(i) = ''
    end do
    !
    ! -- scan through mvrbudobj and create unique paknames
    ipos = 1
    do i = 1, nbudterm
      found = .false.
      do j = 1, ipos
        if (this%fmi%mvrbudobj%budterm(i)%text2id1 == this%paknames(j)) then
          found = .true.
          exit
        end if
      end do
      if (.not. found) then
        this%paknames(ipos) = this%fmi%mvrbudobj%budterm(i)%text2id1
        ipos = ipos + 1
      end if
    end do
    !
    ! -- Return
    return
  end subroutine mvt_scan_mvrbudobj
  
  subroutine mvt_setup_outputtab(this)
! ******************************************************************************
! mvt_setup_outputtab -- set up output table
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(GwtMvtType),intent(inout) :: this
    ! -- local
    character(len=LINELENGTH) :: title
    character(len=LINELENGTH) :: text
    integer(I4B) :: ntabcol
    integer(I4B) :: maxrow
    integer(I4B) :: ilen
! ------------------------------------------------------------------------------
    !
    ! -- allocate and initialize the output table
    if (this%iprflow /= 0) then
      !
      ! -- dimension table
      ntabcol = 7
      maxrow = 0
      !
      ! -- initialize the output table object
      title = 'TRANSPORT MOVER PACKAGE (' // trim(this%packName) // &
              ') FLOW RATES'
      call table_cr(this%outputtab, this%packName, title)
      call this%outputtab%table_df(maxrow, ntabcol, this%iout, &
                                    transient=.TRUE.)
      text = 'NUMBER'
      call this%outputtab%initialize_column(text, 10, alignment=TABCENTER)
      text = 'PROVIDER LOCATION'
      ilen = LENMODELNAME+LENPACKAGENAME+1
      call this%outputtab%initialize_column(text, ilen)
      text = 'PROVIDER ID'
      call this%outputtab%initialize_column(text, 10)
      text = 'PROVIDER FLOW RATE'
      call this%outputtab%initialize_column(text, 10)
      text = 'PROVIDER TRANSPORT RATE'
      call this%outputtab%initialize_column(text, 10)
      text = 'RECEIVER LOCATION'
      ilen = LENMODELNAME+LENPACKAGENAME+1
      call this%outputtab%initialize_column(text, ilen)
      text = 'RECEIVER ID'
      call this%outputtab%initialize_column(text, 10)
      
    end if
    !
    ! -- return
    return
  end subroutine mvt_setup_outputtab

  subroutine mvt_print_outputtab(this)
! ******************************************************************************
! mvt_print_outputtab -- set up output table
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- module
    use TdisModule, only: kstp, kper
    ! -- dummy
    class(GwtMvttype),intent(inout) :: this
    ! -- local
    character (len=LINELENGTH) :: title
    character(len=LENMODELNAME+LENPACKAGENAME+1) :: cloc1, cloc2
    integer(I4B) :: i
    integer(I4B) :: n
    integer(I4B) :: inum
    integer(I4B) :: ntabrows
    integer(I4B) :: nlist
! ------------------------------------------------------------------------------
    !
    ! -- determine number of table rows
    ntabrows = 0
    do i = 1, this%budobj%nbudterm
      nlist = this%budobj%budterm(i)%nlist
      ntabrows = ntabrows + nlist
    end do
    !
    ! -- set table kstp and kper
    call this%outputtab%set_kstpkper(kstp, kper)
    !
    ! -- Add terms and print the table
    title = 'TRANSPORT MOVER PACKAGE (' // trim(this%packName) //     &
            ') FLOW RATES'
    call this%outputtab%set_title(title)
    call this%outputtab%set_maxbound(ntabrows)
    !
    ! -- Process each table row
    inum = 1
    do i = 1, this%budobj%nbudterm
      nlist = this%budobj%budterm(i)%nlist
      do n = 1, nlist
        cloc1 = trim(adjustl(this%budobj%budterm(i)%text1id1)) // ' ' // &
                trim(adjustl(this%budobj%budterm(i)%text2id1))
        cloc2 = trim(adjustl(this%budobj%budterm(i)%text1id2)) // ' ' // &
                trim(adjustl(this%budobj%budterm(i)%text2id2))
        call this%outputtab%add_term(inum)
        call this%outputtab%add_term(cloc1)
        call this%outputtab%add_term(this%budobj%budterm(i)%id1(n))
        call this%outputtab%add_term(-this%fmi%mvrbudobj%budterm(i)%flow(n))
        call this%outputtab%add_term(this%budobj%budterm(i)%flow(n))
        call this%outputtab%add_term(cloc2)
        call this%outputtab%add_term(this%budobj%budterm(i)%id2(n))
        inum = inum + 1
      end do
    end do
    !
    ! -- return
    return
  end subroutine mvt_print_outputtab

end module GwtMvtModule
  