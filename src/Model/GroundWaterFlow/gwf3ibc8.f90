module IbcModule
  use ConstantsModule, only: DPREC, DZERO, DHALF, DONE, DTWO, DTHREE, DTEN, &
                             LENFTYPE, LENPACKAGENAME, LINELENGTH, LENBOUNDNAME, &
                             NAMEDBOUNDFLAG
  use KindModule, only: I4B, DP
  use BndModule, only: BndType
  use ObserveModule,        only: ObserveType
  use ObsModule, only: ObsType
  use BlockParserModule,      only: BlockParserType
  use TimeSeriesLinkModule, only: TimeSeriesLinkType, &
                                  GetTimeSeriesLinkFromList
  use InputOutputModule, only: get_node, extract_idnum_or_bndname
  use BaseDisModule, only: DisBaseType
  use SimModule, only: count_errors, store_error, store_error_unit, ustop
  use ArrayHandlersModule, only: ExpandArray
  !
  implicit none
  !
  private
  public :: ibc_create
  public :: IbcType
  !
  real(DP), parameter :: dlog10es = 0.4342942_DP
  character(len=LENFTYPE)       :: ftype = 'IBC'
  character(len=LENPACKAGENAME) :: text  = '            IBC'
  !

  type, extends(BndType) :: IbcType
    integer(I4B), pointer :: iconstantb
    integer(I4B), pointer :: istoragec
    integer(I4B), pointer :: icellf
    integer(I4B), pointer :: ibedstressoff
    integer(I4B), pointer :: igeostressoff
    integer(I4B), pointer :: ndelaycells
    integer(I4B), pointer :: ndelaybeds
    integer(I4B), pointer :: igeocalc

    integer(I4B) :: nibcobs
    logical :: first_time
    integer, pointer :: gwfiss => NULL()
    !integer, dimension(:), pointer :: nodelist => null()  !reduced node that the ibs is attached to
    integer, dimension(:), pointer :: unodelist => null()  !user node that the ibs is attached to

    integer, dimension(:), pointer :: idelay => null() ! 0 = nodelay, > 0 = delay
    !character(len=LENBOUNDNAME), pointer, dimension(:) :: boundname => null()
    real(DP), dimension(:), pointer :: sgm         => null()   !specific gravity moist sediments
    real(DP), dimension(:), pointer :: sgs         => null()   !specific gravity saturated sediments
    real(DP), dimension(:), pointer :: sig0        => null()   !geostatic offset
    real(DP), dimension(:), pointer :: ci          => null()   !compression index
    real(DP), dimension(:), pointer :: rci         => null()   !recompression
    real(DP), dimension(:), pointer :: gs          => null()   !geostatic stress
    real(DP), dimension(:), pointer :: es          => null()   !effective stress
    real(DP), dimension(:), pointer :: es0         => null()   !last effective stress
    real(DP), dimension(:), pointer :: pcs         => null()   !preconsolidation stress
    real(DP), dimension(:), pointer :: znode       => null()   !elevation of node center
    real(DP), dimension(:), pointer :: thick       => null()   !fraction of cell thickness that interbed system occupies
    real(DP), dimension(:), pointer :: rnb         => null()   !interbed system material factor
    real(DP), dimension(:), pointer :: void        => null()   !void space
    real(DP), dimension(:), pointer :: kv          => null()   !vertical hydraulic conductivity of interbed
    real(DP), dimension(:), pointer :: h0          => null()   !initial head in interbed
    real(DP), dimension(:), pointer :: comp        => null()   !interbed compaction
    real(DP), dimension(:), pointer :: totalcomp   => null()   !total interbed compaction
    real(DP), dimension(:), pointer :: gwflow      => null()   !gwf-flow
    ! -- delay interbed arrays
    real(DP), dimension(:,:), pointer :: dbdz     => null()    !delay bed dz
    real(DP), dimension(:,:), pointer :: dbz      => null()    !delay bed cell z
    real(DP), dimension(:,:), pointer :: dbh      => null()    !delay bed cell h
    real(DP), dimension(:,:), pointer :: dbh0     => null()    !delay bed cell previous h
    real(DP), dimension(:,:), pointer :: dbvoid   => null()    !delay bed cell void ratio
    real(DP), dimension(:,:), pointer :: dbvoid0  => null()    !delay bed cell previous void ratio
    real(DP), dimension(:,:), pointer :: dbgeo    => null()    !delay bed cell geostatic stress
    real(DP), dimension(:,:), pointer :: dbgeo0   => null()    !delay bed cell previous geostatic stress
    real(DP), dimension(:,:), pointer :: dbes     => null()    !delay bed cell effective stress
    real(DP), dimension(:,:), pointer :: dbes0    => null()    !delay bed cell previous effective stress
    real(DP), dimension(:,:), pointer :: dbpcs    => null()    !delay bed cell preconsolidation stress
    ! -- delay interbed solution arrays
    real(DP), dimension(:), pointer :: dbal       => null()    !delay bed lower diagonal
    real(DP), dimension(:), pointer :: dbad       => null()    !delay bed diagonal
    real(DP), dimension(:), pointer :: dbau       => null()    !delay bed upper diagonal
    real(DP), dimension(:), pointer :: dbrhs      => null()    !delay bed right hand side
    real(DP), dimension(:), pointer :: dbdh       => null()    !delay bed dh
    real(DP), dimension(:), pointer :: dbaw       => null()    !delay bed work vector
    ! -- observation data
    !real(DP), dimension(:), pointer :: simvals        => null()

  contains
    procedure :: bnd_ck => ibc_ck
    procedure :: bnd_cf => ibc_cf
    procedure :: define_listlabel
    procedure :: bnd_options => ibc_options
    !procedure :: bnd_df => ibc_df
    procedure :: bnd_ar => ibc_ar
    procedure :: bnd_da => ibc_da
    procedure :: bnd_rp => ibc_rp
    procedure :: bnd_ad => ibc_ad
    procedure :: bnd_bd => ibc_bd
    procedure :: read_dimensions => ibc_read_dimensions
    procedure, private :: ibc_allocate_scalars
    procedure, private :: ibc_allocate_arrays
    procedure, private :: ibc_read_packagedata
    procedure, private :: calc_aqgeo_stress
    procedure, private :: calc_aqeff_stress
    procedure, private :: calc_aqznode
    procedure, private :: calc_nodelay_gwf
    !procedure :: geo_st_above
    !
    ! -- delay interbed methods
    procedure, private :: calc_delay_interbed
    procedure, private :: calc_delay_z
    procedure, private :: calc_delay_stress
    procedure, private :: calc_delay_sskessk
    procedure, private :: assemble_delay
    procedure, private :: calc_delay_comp
    procedure, private :: calc_delay_dstor
    procedure, private :: calc_delay_gwf
    procedure, private :: calc_delay_err
!    procedure, private :: solve_delay
    ! -- methods for observations
    procedure, public :: bnd_obs_supported => ibc_obs_supported
    procedure, public :: bnd_df_obs => ibc_df_obs
    procedure, public :: bnd_rp_obs => ibc_rp_obs
    procedure, private :: ibc_bd_obs
    ! -- method for time series
    !procedure, public :: bnd_rp_ts => ibc_rp_ts
  end type IbcType

contains

  subroutine ibc_create(packobj, id, ibcnum, inunit, iout, namemodel, pakname)
! ******************************************************************************
! ibc_create -- Create a New ibc Package
! Subroutine: (1) create new-style package
!             (2) point bndobj to the new package
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    implicit none
    class(BndType), pointer :: packobj
    integer,intent(in) :: id
    integer,intent(in) :: ibcnum
    integer,intent(in) :: inunit
    integer,intent(in) :: iout
    character(len=*), intent(in) :: namemodel
    character(len=*), intent(in) :: pakname
    ! -- local
    type(IbcType), pointer :: ibcobj
! ------------------------------------------------------------------------------
    !
    ! -- allocate the object and assign values to object variables
    allocate(ibcobj)
    packobj => ibcobj
    !
    ! -- create name and origin
    call packobj%set_names(ibcnum, namemodel, pakname, ftype)
    ibcobj%text = text
    !
    ! -- allocate scalars
    !call packobj%allocate_scalars()
    call ibcobj%ibc_allocate_scalars()
    !
    ! -- initialize package
    call packobj%pack_initialize()
    !
    packobj%inunit=inunit
    packobj%iout=iout
    packobj%id=id
    packobj%ibcnum = ibcnum
    packobj%ncolbnd=6 !initial_stress frac cr(or ci) cc(or rci) void delay
    packobj%iscloc=1 !sfac applies to initial stress?
    !
    ! -- return
    return
  end subroutine ibc_create


   subroutine ibc_allocate_scalars(this)
! ******************************************************************************
! allocate_scalars -- allocate scalar members
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryManagerModule, only: mem_allocate
    ! -- dummy
    class(IbcType),   intent(inout) :: this
! ------------------------------------------------------------------------------
    !
    ! -- call standard BndType allocate scalars
    call this%BndType%allocate_scalars()
    !
    ! -- allocate the object and assign values to object variables
    call mem_allocate(this%ndelaycells, 'NDELAYCELLS', this%origin)
    call mem_allocate(this%ndelaybeds, 'NDELAYBEDS', this%origin)
    call mem_allocate(this%igeocalc, 'IGEOCALC', this%origin)
    call mem_allocate(this%ibedstressoff, 'IBEDSTRESSOFF', this%origin)
    call mem_allocate(this%igeostressoff, 'IGEOSTRESSOFF', this%origin)
    call mem_allocate(this%istoragec, 'ISTORAGEC', this%origin)
    call mem_allocate(this%iconstantb, 'ICONSTANTB', this%origin)
    call mem_allocate(this%icellf, 'ICELLF', this%origin)
    call mem_allocate(this%nbound, 'NIBCCELLS', this%origin)
    !
    ! -- initialize values
    this%ndelaycells = 19
    this%ndelaybeds = 0
    this%igeocalc = 0
    this%ibedstressoff = 0
    this%igeostressoff = 0
    this%istoragec = 0
    this%iconstantb = 0
    this%icellf = 0
    this%nbound = 0
    this%first_time = .TRUE.
    !
    ! -- return
    return
   end subroutine ibc_allocate_scalars


   subroutine ibc_bd(this, x, idvfl, icbcfl, ibudfl, icbcun, iprobs,            &
                     isuppress_output, model_budget, imap, iadv)
! ******************************************************************************
! ibc_bd -- calculate budget for interbeds
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
   ! -- modules
    use TdisModule, only: kstp, kper, delt
    use ConstantsModule, only: LENBOUNDNAME, DZERO, DONE
    use BudgetModule, only: BudgetType
    ! -- dummy
    class(IbcType) :: this
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
    !character (len=LENPACKAGENAME) :: text
    character(len=LINELENGTH) :: msg
    !integer(I4B) :: imover
    integer(I4B) :: i,n
    real(DP) :: es, pcs, rho1, rho2, tled
    real(DP) :: delt_sto, es0, strain
    real(DP) :: top, bot, thk_node, thick, rrate, ratein,rateout
    real(DP) :: comp
    real(DP) :: area
    real(DP) :: h
    real(DP) :: hcof
    real(DP) :: rhs
    real(DP) :: err
    real(DP) :: qaq
    real(DP) :: dsto
    real(DP) :: qaqa
    real(DP) :: qaqrhs
    !real(DP) :: qtomvr
    !real(DP) :: ratin, ratout, rrate
    !integer(I4B) :: ibdlbl, naux
    ! -- for observations
    integer(I4B) :: iprobslocal
    ! -- formats
! --------------------------------------------------------------------------
    !
    ! -- Suppress saving of simulated values; they
    !    will be saved at end of this procedure.
    iprobslocal = 0
    !
    !
    call this%BndType%bnd_bd(x, idvfl, icbcfl, ibudfl, icbcun, iprobslocal,     &
                    isuppress_output, model_budget, imap, iadv)
    ratein = DZERO
    rateout= DZERO

    if(this%gwfiss.ne.0) then
        call model_budget%addentry(ratein, rateout, delt, this%text,            &
                               isuppress_output, this%text)
        return
    end if

    tled = DONE

    do i = 1, this%nbound
      n = this%nodelist(i)
      area = this%dis%get_area(n)
      !
      ! -- skip inactive and constant head cells
      if (this%ibound(n) < 1) cycle
      !
      ! -- no delay interbeds
      if (this%idelay(i) == 0) then
        !
        ! -- calculate ibc rho1 and rho2
        call this%calc_nodelay_gwf(i, rho1, rho2, rhs, tled)
        bot = this%dis%bot(n)
        top = this%dis%top(n)
        thk_node = top - bot
        es = this%es(i)
        pcs = this%pcs(i)
        es0 = this%es0(i)
        ! -- calculate compaction
        if (this%igeocalc == 0) then
          h = this%xnew(n)
          comp = rho2 * (pcs - h) + rho1 * (es0 - pcs)
        else
          comp = -pcs * (rho2 - rho1) - (rho1 * es0) + (rho2 * es)
        end if
        delt_sto = comp * area
        !!
        !write(1051, '(i10,8(1x,g20.7))') i, this%gs(n), es, es0, &
        !                                 pcs, rho1, rho2, delt_sto, comp
        !
        ! -- update compaction and total compaction
        this%comp(i) = comp
        this%totalcomp(i) = this%totalcomp(i) + comp
        !
        ! - calculate strain and change in interbed void ratio and thickness
        strain = DZERO
        thick = this%thick(i)
        if (thick > DZERO) strain = -comp / thick
        if (this%iconstantb == 0) then
          this%void(i) = strain + this%void(i) * (strain + DONE)
          this%thick(i) = thick * (strain + DONE)
        end if
        !
        !write(*,*) n,trim(msg),delt_sto
        !call this%dis%noder_to_string(n,msg)
        !write(*,*) n,trim(msg),this%rhs(i),this%hcof(i),rho1,rho2,rhs
        !
        ! -- water budget terms
        if (delt_sto >= DZERO) then
            ratein = ratein + delt_sto / delt
        else
            rateout = rateout + delt_sto / delt
        end if
        !
        ! -- delay interbeds
      else
        h = this%xnew(n)
        call this%calc_delay_dstor(i, rhs)
        dsto = rhs / delt
        delt_sto = rhs * area * this%rnb(i) / delt
        !
        ! -- calculate sum of compaction in delay interbed
        call this%calc_delay_comp(i)
      end if
      this%gwflow(i) = delt_sto
      !
      !call model_budget%addentry(ratein, rateout, delt, this%text,                 &
      !                           isuppress_output,this%text)
    end do
    !
    ! -- For continuous observations, save simulated values.
    if (this%obs%npakobs > 0 .and. iprobs > 0) then
      call this%ibc_bd_obs()
    end if

    ! -- return
    return

   end subroutine ibc_bd

  subroutine ibc_read_packagedata(this)
! ******************************************************************************
! pak1read_dimensions -- Read the dimensions for this package
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use ConstantsModule, only: LINELENGTH
    use MemoryManagerModule, only: mem_reallocate
!    use SimModule, only: ustop, store_error, count_errors, store_error_unit
    use TimeSeriesManagerModule, only: read_single_value_or_time_series
    ! -- dummy
    class(IbcType),intent(inout) :: this
    ! -- local
    character(len=LINELENGTH) :: errmsg
    character(len=LINELENGTH) :: text, keyword
    character(len=LINELENGTH) :: strttext, cellid
    character(len=LENBOUNDNAME) :: bndName, bndNameTemp
    character(len=7) :: cdelay
    character(len=9) :: cno
    character(len=50), dimension(:), allocatable :: caux
    integer(I4B) :: ival
    logical :: isfound, endOfBlock
    real(DP) :: rval, top, bot
    integer(I4B) :: n, nn
    integer(I4B) :: j, jj
    integer(I4B) :: iaux
    integer(I4B) :: itmp
    integer(I4B) :: ierr
    integer(I4B) :: ndelaybeds
    integer(I4B) :: idelay
    real(DP) :: endtim
    real(DP) :: baq
    integer, allocatable, dimension(:) :: nboundchk
    !
    ! -- initialize temporary variables
    ndelaybeds = 0
    !
    ! -- allocate temporary arrays
    allocate(nboundchk(this%nbound))
    do n = 1, this%nbound
      nboundchk(n) = 0
    end do
    !
    call this%parser%GetBlock('PACKAGEDATA', isfound, ierr,                     &
                              supportopenclose=.true.)
    !
    ! -- parse locations block if detected
    if (isfound) then
      write(this%iout,'(/1x,a)')'PROCESSING '//trim(adjustl(this%text))// &
        ' PACKAGEDATA'
      do
        call this%parser%GetNextLine(endOfBlock)
        if (endOfBlock) exit
        ! -- read interbed number
        itmp = this%parser%GetInteger()

        if (itmp < 1 .or. itmp > this%nbound) then
          write(errmsg,'(4x,a,1x,i0,1x,a,1x,i0)') &
            '****ERROR. INTERBED NUMBER (', itmp, ') MUST BE > 0 and <= ', this%nbound
          call store_error(errmsg)
          cycle
        end if

        ! -- increment nboundchk
        nboundchk(itmp) = nboundchk(itmp) + 1

        ! -- read cellid
        call this%parser%GetCellid(this%dis%ndim,cellid)
        nn = this%dis%noder_from_cellid(cellid, &
                                      this%parser%iuactive, this%iout)
        n = this%dis%nodeu_from_cellid(cellid, &
                                      this%parser%iuactive, this%iout)
        top = this%dis%top(nn)
        bot = this%dis%bot(nn)
        baq = top - bot
        ! -- determine if a valid cell location was provided
          if (nn < 1) then
            write(errmsg,'(4x,a,1x,i4,1x)') &
              '****ERROR. INVALID cellid FOR PACKAGEDATA ENTRY', itmp
            call store_error(errmsg)
          end if
        !todo error trapping here...
        this%nodelist(itmp) = nn
        this%unodelist(itmp) = n

        ! cdelay
        call this%parser%GetStringCaps(cdelay)
        select case (cdelay)
          case ('NODELAY')
            ival = 0
          case ('DELAY')
            ndelaybeds = ndelaybeds + 1
            ival = ndelaybeds
          case default
            write(errmsg,'(4x,a,1x,a,1x,i0,1x)') &
              '****ERROR. INVALID CDELAY ', trim(adjustl(cdelay)), &
              ' FOR PACKAGEDATA ENTRY', itmp
            call store_error(errmsg)
            cycle
        end select
        this%idelay(itmp) = ival

        ! get initial preconsolidation stress
        this%pcs(itmp) = this%parser%GetDouble()

        ! -- get thickness or cell fraction
        rval = this%parser%GetDouble()
        if (this%icellf == 0) then
          if (rval <= 0.0 .or. rval > baq) then
              write(errmsg,'(4x,a,1x,g0,1x,a,1x,g0,1x,a,1x,i0)') &
                '****ERROR. thick (', rval,') MUST BE > 0 AND LESS THAN ', baq, &
                'FOR PACKAGEDATA ENTRY', itmp
              call store_error(errmsg)
          end if
        else
          if (rval <= DZERO .or. rval > DONE) then
              write(errmsg,'(4x,a,1x,i0)') &
                '****ERROR. frac MUST BE > 0 AND < 1 FOR PACKAGEDATA ENTRY', itmp
              call store_error(errmsg)
          end if
          rval = rval * baq
        end if
        this%thick(itmp) = rval

        ! -- get rnb
        rval = this%parser%GetDouble()
        if (rval < DONE) then
            write(errmsg,'(4x,a,1x,g0,1x,a,1x,a,1x,i0)') &
              '****ERROR. rnb (', rval,') MUST BE >= 1.', &
              'FOR PACKAGEDATA ENTRY', itmp
            call store_error(errmsg)
        end if
        this%rnb(itmp) = rval

        ! -- get ci or skv
        rval =  this%parser%GetDouble()
        if (rval <= 0.0) then
            write(errmsg,'(4x,a,1x,i4,1x)') &
              '****ERROR. INVALID (ci,ssv) FOR PACKAGEDATA ENTRY', itmp
            call store_error(errmsg)
        end if
        this%ci(itmp) = rval

        ! -- get rci or ske
        rval =  this%parser%GetDouble()
        if (rval <= 0.0) then
            write(errmsg,'(4x,a,1x,i4,1x)') &
              '****ERROR. INVALID (rci,sse) FOR PACKAGEDATA ENTRY', itmp
            call store_error(errmsg)
        end if
        this%rci(itmp) = rval

        ! -- get void ratio
        rval =  this%parser%GetDouble()
        if (rval <= 0.0) then
            write(errmsg,'(4x,a,1x,i4,1x)') &
              '****ERROR. INVALID void FOR PACKAGEDATA ENTRY', itmp
            call store_error(errmsg)
        end if
        this%void(itmp) = rval

        ! -- get kv
        rval =  this%parser%GetDouble()
        if (this%idelay(itmp) == 1) then
          if (rval <= 0.0) then
             write(errmsg,'(4x,a,1x,i4,1x)') &
               '****ERROR. kv MUST BE GREATER THAN ZERO FOR PACKAGEDATA ENTRY', itmp
             call store_error(errmsg)
          end if
        end if
        this%kv(itmp) = rval

        ! -- get h0
        rval =  this%parser%GetDouble()
        this%h0(itmp) = rval

        ! -- get bound names
        write (cno,'(i9.9)') nn
          bndName = 'nsystem' // cno
        call this%parser%GetStringCaps(bndNameTemp)
        if (bndNameTemp /= '') then
        bndName = bndNameTemp(1:16)
        endif
        this%boundname(itmp) = bndName
        this%rhs(itmp) = 0.0
        this%hcof(itmp) = 0.0
      end do
      write(this%iout,'(1x,a)')'END OF '//trim(adjustl(this%text))//' PACKAGEDATA'
    else
      call store_error('ERROR.  REQUIRED PACKAGEDATA BLOCK NOT FOUND.')
    endif
    !
    ! -- Check to make sure that every reach is specified and that no reach
    !    is specified more than once.
    do n = 1, this%nbound
      if (nboundchk(n) == 0) then
        write(errmsg, '(a, i0, a)') 'ERROR: INFORMATION FOR INTERBED ', n,     &
                                    ' NOT SPECIFIED IN PACKAGEDATA BLOCK.'
        call store_error(errmsg)
      else if (nboundchk(n) > 1) then
        write(errmsg, '(a, i0, i0)') 'ERROR: INFORMATION SPECIFIED ',          &
                                     nboundchk(n), ' TIMES FOR INTERBED ', n
        call store_error(errmsg)
      endif
    end do
    deallocate(nboundchk)
    !
    ! -- reallocate and initialize delay bed arrays
    this%ndelaybeds = ndelaybeds
    if (ndelaybeds > 0) then
      !
      ! -- delay bed storage
      call mem_reallocate(this%dbdz, this%ndelaycells, ndelaybeds, 'dbdz', trim(this%origin))
      call mem_reallocate(this%dbz, this%ndelaycells, ndelaybeds, 'dbz', trim(this%origin))
      call mem_reallocate(this%dbh, this%ndelaycells, ndelaybeds, 'dbh', trim(this%origin))
      call mem_reallocate(this%dbh0, this%ndelaycells, ndelaybeds, 'dbh0', trim(this%origin))
      call mem_reallocate(this%dbvoid, this%ndelaycells, ndelaybeds, 'dbvoid', trim(this%origin))
      call mem_reallocate(this%dbvoid0, this%ndelaycells, ndelaybeds, 'dbvoid0', trim(this%origin))
      call mem_reallocate(this%dbgeo, this%ndelaycells, ndelaybeds, 'dbgeo', trim(this%origin))
      call mem_reallocate(this%dbgeo0, this%ndelaycells, ndelaybeds, 'dbgeo0', trim(this%origin))
      call mem_reallocate(this%dbes, this%ndelaycells, ndelaybeds, 'dbes', trim(this%origin))
      call mem_reallocate(this%dbes0, this%ndelaycells, ndelaybeds, 'dbes0', trim(this%origin))
      call mem_reallocate(this%dbpcs, this%ndelaycells, ndelaybeds, 'dbpcs', trim(this%origin))
      !
      ! -- delay interbed solution arrays
      call mem_reallocate(this%dbal, this%ndelaycells, 'dbal', trim(this%origin))
      call mem_reallocate(this%dbad, this%ndelaycells, 'dbad', trim(this%origin))
      call mem_reallocate(this%dbau, this%ndelaycells, 'dbau', trim(this%origin))
      call mem_reallocate(this%dbrhs, this%ndelaycells, 'dbrhs', trim(this%origin))
      call mem_reallocate(this%dbdh, this%ndelaycells, 'dbdh', trim(this%origin))
      call mem_reallocate(this%dbaw, this%ndelaycells, 'dbaw', trim(this%origin))
      !
      ! -- initialize delay bed storage
      do n = 1, this%nbound
        idelay = this%idelay(n)
        if (idelay == 0) then
          cycle
        end if
        do j = 1, this%ndelaycells
          this%dbdz(j, idelay) = this%thick(n) / real(this%ndelaycells, DP)
          this%dbh(j, idelay) = this%h0(n)
          this%dbh0(j, idelay) = this%h0(n)
          this%dbvoid(j, idelay) = this%void(n)
          this%dbvoid0(j, idelay) = this%void(n)
          this%dbgeo(j, idelay) = DZERO
          this%dbgeo0(j, idelay) = DZERO
          this%dbes(j, idelay) = DZERO
          this%dbes0(j, idelay) = DZERO
          this%dbpcs(j, idelay) = this%pcs(n)
        end do
      end do
      do n = 1, this%ndelaycells
        this%dbal(n) = DZERO
        this%dbad(n) = DZERO
        this%dbau(n) = DZERO
        this%dbrhs(n) = DZERO
        this%dbdh(n) = DZERO
        this%dbaw(n) = DZERO
      end do
    end if

    !
    ! -- terminate if errors encountered in reach block
    if (count_errors() > 0) then
      call this%parser%StoreErrorUnit()
      call ustop()
    end if

    !TODO - check the total frac for each nbound node to make sure < 1.0
    !
    ! -- return
    return
  end subroutine ibc_read_packagedata

  subroutine ibc_options(this, option, found)
! ******************************************************************************
! ibc_options -- set options specific to IbcType
!
! ibc_options overrides BndType%bnd_options
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use ConstantsModule, only: MAXCHARLEN, DZERO
    use OpenSpecModule, only: access, form
!    use SimModule, only: ustop, store_error
    use InputOutputModule, only: urword, getunit, openfile
    implicit none
    !dummy
    class(IbcType),   intent(inout) :: this
    character(len=*), intent(inout) :: option
    logical,          intent(inout) :: found
    !local
    !formats
    character(len=*),parameter :: fmtssessv = &
      "(4x, 'USING SSE AND SSV INSTEAD OF CR AND CC.')"
    character(len=*),parameter :: fmtoffset = &
      "(4x, 'INITIAL_STRESS TREATED AS AN OFFSET.')"
    character(len=*),parameter :: fmtopt = &
      "(4x, A)"
    character(len=*),parameter :: fmtopti = &
      "(4x, A, 1X, I0)"
! -----------------------------------------------------------------------------
    select case (option)
    ! user specified number of delay cells used in each system of delay intebeds
    case ('NDELAYCELLS')
      this%ndelaycells =  this%parser%GetInteger()
      write(this%iout, fmtopti) 'NUMBER OF DELAY CELLS =', this%ndelaycells
      found = .true.
    ! offset is applied to geostatic stress
    ! initial stress is actually an offset from calculated initial stress
    case ('INTERBED_STRESS_OFFSET')
      this%ibedstressoff = 1
      write(this%iout, fmtopt) 'OFFSET WILL BE APPLIED TO INITIAL INTERBED EFFECTIVE STRESS'
      found = .true.
    case ('GEO_STRESS_OFFSET')
      this%igeostressoff = 1
      write(this%iout, fmtopt) 'OFFSET WILL BE APPLIED TO STRESSES'
      found = .true.
    ! storage coefficients (SSE and SSV) will be specified instead of CR and CC
    case ('STORAGECOEFFICIENT')
      this%istoragec = 1
      write(this%iout, fmtopt) 'ELASTIC AND INELASTIC STORAGE WILL BE ' //  &
                               'SPECIFIED INSTEAD OF COMPRESSION INDICES'
      found = .true.
    ! constant delay interbed thickness and void ratio
    case ('CONSTANT_THICKNESS')
      this%iconstantb = 1
      write(this%iout, fmtopt) 'DELAY INTERBED THICKNESS AND VOID RATIO ' //  &
                               'WILL REMAIN CONSTANT IN THE SIMULATION'
      found = .true.
    ! cell fraction will be specified instead of interbed thickness
    case ('CELL_FRACTION')
      this%icellf = 1
      write(this%iout, fmtopt) 'INTERBED THICKNESS WILL BE SPECIFIED ' //  &
                               'AS A CELL FRACTION'
      found = .true.
    ! default case
    case default
      !
      ! -- No options found
      found = .false.
    end select
    !
    ! -- return
    return
  end subroutine ibc_options

  subroutine ibc_allocate_arrays(this)
! ******************************************************************************
! allocate_arrays -- Allocate Package Members
! Subroutine: (1) allocate
!             (2) initialize
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use MemoryManagerModule, only: mem_allocate, mem_setptr
    implicit none
    class(IbcType),   intent(inout) :: this
    ! -- local variables
    integer(I4B) :: n

    !call this%BndType%allocate_arrays()
    call mem_allocate(this%unodelist, this%nbound, 'unodelist', trim(this%origin))
    call mem_allocate(this%nodelist, this%nbound, 'nodelist', trim(this%origin))
    call mem_allocate(this%gs, this%dis%nodes, 'gs', trim(this%origin))
    call mem_allocate(this%es, this%nbound, 'es', trim(this%origin))
    call mem_allocate(this%es0, this%nbound, 'es0', trim(this%origin))
    call mem_allocate(this%pcs, this%nbound, 'pcs', trim(this%origin))
    call mem_allocate(this%znode, this%nbound, 'znode', trim(this%origin))
    call mem_allocate(this%thick, this%nbound, 'thick', trim(this%origin))
    call mem_allocate(this%rnb, this%nbound, 'rnb', trim(this%origin))
    call mem_allocate(this%kv, this%nbound, 'kv', trim(this%origin))
    call mem_allocate(this%h0, this%nbound, 'h0', trim(this%origin))
    call mem_allocate(this%void, this%nbound, 'void', trim(this%origin))
    call mem_allocate(this%ci, this%nbound, 'ci', trim(this%origin))
    call mem_allocate(this%rci, this%nbound, 'rci', trim(this%origin))
    call mem_allocate(this%sgm, this%dis%nodes, 'sgm', trim(this%origin))
    call mem_allocate(this%sgs, this%dis%nodes, 'sgs', trim(this%origin))
    if (this%igeostressoff == 1) then
      call mem_allocate(this%sig0, this%dis%nodes, 'sig0', trim(this%origin))
    else
      call mem_allocate(this%sig0, 1, 'sig0', trim(this%origin))
    end if
    call mem_allocate(this%idelay, this%nbound, 'idelay', trim(this%origin))
    call mem_allocate(this%comp, this%nbound, 'comp', trim(this%origin))
    call mem_allocate(this%totalcomp, this%nbound, 'totalcomp', trim(this%origin))
    call mem_allocate(this%gwflow, this%nbound, 'gwflow', trim(this%origin))
    !
    ! -- delay bed storage
    call mem_allocate(this%dbdz, 0, 0, 'dbdz', trim(this%origin))
    call mem_allocate(this%dbz, 0, 0, 'dbz', trim(this%origin))
    call mem_allocate(this%dbh, 0, 0, 'dbh', trim(this%origin))
    call mem_allocate(this%dbh0, 0, 0, 'dbh0', trim(this%origin))
    call mem_allocate(this%dbvoid, 0, 0, 'dbvoid', trim(this%origin))
    call mem_allocate(this%dbvoid0, 0, 0, 'dbvoid0', trim(this%origin))
    call mem_allocate(this%dbgeo, 0, 0, 'dbgeo', trim(this%origin))
    call mem_allocate(this%dbgeo0, 0, 0, 'dbgeo0', trim(this%origin))
    call mem_allocate(this%dbes, 0, 0, 'dbes', trim(this%origin))
    call mem_allocate(this%dbes0, 0, 0, 'dbes0', trim(this%origin))
    call mem_allocate(this%dbpcs, 0, 0, 'dbpcs', trim(this%origin))
    !
    ! -- delay interbed solution arrays
    call mem_allocate(this%dbal, 0, 'dbal', trim(this%origin))
    call mem_allocate(this%dbad, 0, 'dbad', trim(this%origin))
    call mem_allocate(this%dbau, 0, 'dbau', trim(this%origin))
    call mem_allocate(this%dbrhs, 0, 'dbrhs', trim(this%origin))
    call mem_allocate(this%dbdh, 0, 'dbdh', trim(this%origin))
    call mem_allocate(this%dbaw, 0, 'dbaw', trim(this%origin))
    !
    call mem_allocate(this%rhs, this%nbound, 'rhs', trim(this%origin))
    call mem_allocate(this%hcof, this%nbound, 'hcof', trim(this%origin))
    !
    ! --
    call mem_allocate(this%simvals,this%nbound,'simvals', trim(this%origin))
    allocate(this%boundname(this%nbound))

    call mem_setptr(this%gwfiss, 'ISS', trim(this%name_model))
    !
    ! -- initialize variables that are not specified by user
    do n = 1, this%dis%nodes
      this%gs(n) = DZERO
    end do
    do n = 1, this%nbound
      this%totalcomp(n) = DZERO
    end do
    !
    ! -- return
    return

  end subroutine ibc_allocate_arrays

   subroutine ibc_da(this)
! ******************************************************************************
! ibc_da -- Deallocate variables
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryManagerModule, only: mem_deallocate
    implicit none
    ! -- dummy
    class(IbcType) :: this
! ------------------------------------------------------------------------------
    !
    ! -- Deallocate arrays
    call mem_deallocate(this%unodelist)
    call mem_deallocate(this%nodelist)
    call mem_deallocate(this%idelay)
    deallocate(this%boundname)

    call mem_deallocate(this%sgm)
    call mem_deallocate(this%sgs)
    call mem_deallocate(this%sig0)
    call mem_deallocate(this%ci)
    call mem_deallocate(this%rci)
    call mem_deallocate(this%gs)
    call mem_deallocate(this%es)
    call mem_deallocate(this%es0)
    call mem_deallocate(this%pcs)
    call mem_deallocate(this%znode)
    call mem_deallocate(this%thick)
    call mem_deallocate(this%rnb)
    call mem_deallocate(this%void)
    call mem_deallocate(this%kv)
    call mem_deallocate(this%h0)
    call mem_deallocate(this%comp)
    call mem_deallocate(this%totalcomp)
    call mem_deallocate(this%gwflow)
    !
    ! -- delay bed storage
    call mem_deallocate(this%dbdz)
    call mem_deallocate(this%dbz)
    call mem_deallocate(this%dbh)
    call mem_deallocate(this%dbh0)
    call mem_deallocate(this%dbvoid)
    call mem_deallocate(this%dbvoid0)
    call mem_deallocate(this%dbgeo)
    call mem_deallocate(this%dbgeo0)
    call mem_deallocate(this%dbes)
    call mem_deallocate(this%dbes0)
    call mem_deallocate(this%dbpcs)
    !
    ! -- delay interbed solution arrays
    call mem_deallocate(this%dbal)
    call mem_deallocate(this%dbad)
    call mem_deallocate(this%dbau)
    call mem_deallocate(this%dbrhs)
    call mem_deallocate(this%dbdh)
    call mem_deallocate(this%dbaw)
    !
    !
    call mem_deallocate(this%rhs)
    call mem_deallocate(this%hcof)
    call mem_deallocate(this%simvals)
    !
    ! -- pointers to gwf variables
    nullify(this%gwfiss)
    !
    ! -- deallocate scalars
    call mem_deallocate(this%ndelaycells)
    call mem_deallocate(this%ndelaybeds)
    call mem_deallocate(this%ibedstressoff)
    call mem_deallocate(this%igeostressoff)
    call mem_deallocate(this%istoragec)
    call mem_deallocate(this%iconstantb)
    call mem_deallocate(this%icellf)


    !call this%BndType%bnd_da()

    !
    ! -- Return
    return
   end subroutine ibc_da

   subroutine ibc_read_dimensions(this)
! ******************************************************************************
! pak1read_dimensions -- Read the dimensions for this package
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use ConstantsModule, only: LINELENGTH, LENBOUNDNAME
    use KindModule, only: I4B
!    use SimModule, only: ustop, store_error, count_errors
    ! -- dummy
    class(IbcType),intent(inout) :: this
    ! -- local
    character(len=LINELENGTH) :: errmsg
    character(len=LENBOUNDNAME) :: keyword
    integer(I4B) :: ierr
    logical :: isfound, endOfBlock
    ! -- format
! ------------------------------------------------------------------------------
    !
    ! -- initialize dimensions to -1
    this%nbound= -1
    !
    ! -- get dimensions block
    call this%parser%GetBlock('DIMENSIONS', isfound, ierr, &
                              supportOpenClose=.true.)
    !
    ! -- parse dimensions block if detected
    if (isfound) then
      write(this%iout,'(/1x,a)')'PROCESSING '//trim(adjustl(this%text))// &
        ' DIMENSIONS'
      do
        call this%parser%GetNextLine(endOfBlock)
        if (endOfBlock) exit
        call this%parser%GetStringCaps(keyword)
        select case (keyword)
          case ('NIBCCELLS')
            this%nbound = this%parser%GetInteger()
            write(this%iout,'(4x,a,i7)')'NIBCCELLS = ', this%nbound
          case default
            write(errmsg,'(4x,a,a)') &
              '****ERROR. UNKNOWN '//trim(this%text)//' DIMENSION: ', &
                                     trim(keyword)
            call store_error(errmsg)
            call this%parser%StoreErrorUnit()
            call ustop()
        end select
      end do
      write(this%iout,'(1x,a)')'END OF '//trim(adjustl(this%text))//' DIMENSIONS'
    else
      call store_error('ERROR.  REQUIRED DIMENSIONS BLOCK NOT FOUND.')
      call ustop()
    end if
    !
    ! -- verify dimensions were set correctly
    if (this%nbound < 0) then
      write(errmsg, '(1x,a)') &
        'ERROR:  nibccells WAS NOT SPECIFIED OR WAS SPECIFIED INCORRECTLY.'
      call store_error(errmsg)
    end if
    !
    ! -- stop if errors were encountered in the DIMENSIONS block
    ierr = count_errors()
    if (ierr > 0) then
      call ustop()
    end if

    ! -- Call define_listlabel to construct the list label that is written
    !    when PRINT_INPUT option is used.
    call this%define_listlabel()
    !
    ! -- return
    return
  end subroutine ibc_read_dimensions

  subroutine ibc_ar(this)
! ******************************************************************************
! ibc_ar -- Allocate and Read
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryManagerModule, only: mem_setptr
    use ConstantsModule, only: LINELENGTH
    use KindModule, only: I4B
    use InputOutputModule, only: urword, uget_block, u8rdcom, &
                                 uterminate_block
!    use SimModule, only: ustop, store_error, count_errors, store_error_unit
    implicit none
    ! -- dummy
    class(IbcType),intent(inout) :: this
    ! -- local
    logical :: isfound, endOfBlock
    integer(I4B) :: ierr, lloc, istart, istop, ival, nlist, i, n
    real(DP) :: rval, pcs_n, area, bot, fact
    character(len=LINELENGTH) :: line, errmsg, aname, keyword

    ! -- format
! ------------------------------------------------------------------------------
    !
    ! - observation data
    call this%obs%obs_ar()
    !
    ! -- time series data
    this%TasManager%dis => this%dis

    ! -- Allocate arrays in package superclass
    call this%ibc_allocate_arrays()

    !
    ! -- read griddata block
    call this%parser%GetBlock('GRIDDATA', isfound, ierr)
    if (isfound) then
        do
        call this%parser%GetNextLine(endOfBlock)
        if (endOfBlock) exit
        call this%parser%GetStringCaps(keyword)
        call this%parser%GetRemainingLine(line)
        lloc = 1
        select case (keyword)
        case ('SGM')

            call this%dis%read_grid_array(line, lloc, istart, istop, this%iout, &
                                         this%parser%iuactive, this%sgm, 'SGM')
        case ('SGS')
            call this%dis%read_grid_array(line, lloc, istart, istop, this%iout, &
                                         this%parser%iuactive, this%sgs, 'SGS')
        case default
            write(errmsg,'(4x,a,a)')'ERROR. UNKNOWN GRIDDATA TAG: ',           &
                                     trim(keyword)
            call store_error(errmsg)
            call this%parser%StoreErrorUnit()
            call ustop()
        end select
      end do
    else
      call store_error('ERROR.  REQUIRED GRIDDATA BLOCK NOT FOUND.')
      call ustop()
    end if
    !
    ! -- determine if geostatic stresses will be calculated
    do n = 1, this%dis%nodes
      if (this%sgm(n) > DZERO .or. this%sgs(n) > DZERO) then
        this%igeocalc = 1
        exit
      end if
    end do
    !
    ! -- read interbed data
    call  this%ibc_read_packagedata()


    !
    ! -- return
    return
  end subroutine ibc_ar

  subroutine ibc_ck(this)
! ******************************************************************************
! ibc_ck -- Check ibc boundary condition data
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use ConstantsModule, only: LINELENGTH
!    use SimModule, only: ustop, store_error, count_errors, store_error_unit
    implicit none
    ! -- dummy
    class(IbcType),intent(inout) :: this
    ! -- local
    character(len=LINELENGTH) :: errmsg

    ! -- return
    return
  end subroutine ibc_ck


  subroutine calc_aqznode(this)
! ******************************************************************************
! calc_aqznode -- calculate the z of the node using current (xnew) water levels
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    implicit none
    class(IbcType) :: this
    integer(I4B) :: i,n
    real(DP) :: top,bot,x

! ------------------------------------------------------------------------------

    do i = 1, this%nbound
      n = this%nodelist(i)
      bot = this%dis%bot(n)
      top = this%dis%top(n)
      x = this%xnew(n)
      if (x.gt.top .or. x.lt.bot) then
          this%znode(i) = (top + bot) * 0.5
      else
          this%znode(i) = (x + bot) * 0.5
      end if
    end do
    !
    ! -- return
    return
  end subroutine calc_aqznode


  subroutine calc_aqgeo_stress(this)
! ******************************************************************************
! calc_aqgeo_stress -- calculate the geostatic stress for every node in the model
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    implicit none
    class(IbcType) :: this
    integer(I4B) :: i,n,ii,m,temp, iis
    real(DP) :: gs, top, bot, va_scale, x, gs_conn, area_n, area_conn
    real(DP) :: hwva
    character(len=LINELENGTH) :: errmsg, msg1,msg2

! ------------------------------------------------------------------------------
    !
    ! -- calculate geostatic stress if necessary
    if (this%igeocalc /= 0) then
      do n = 1, this%dis%nodes
        ! -- calc gln for this node
        bot = this%dis%bot(n)
        top = this%dis%top(n)
        x = this%xnew(n)
        gs = DZERO

        if (x >= top) then
            gs = (top-bot) * this%sgs(n)
        else if (x <= bot) then
            gs = (top-bot) * this%sgm(n)
        else
            gs = ((top-x) * this%sgm(n)) + ((x-bot) * this%sgs(n))
        end if
        this%gs(n) = gs
      end do
      !
      ! -- calculate the area weighted geostatic stress above cell
      !   *** this needs to be checked for a complicated discretization ***
      do n = 1, this%dis%nodes
        gs = this%gs(n)
        ! -- Go through the connecting cells
        do ii = this%dis%con%ia(n) + 1, this%dis%con%ia(n + 1) - 1
          ! -- Set the m cell number
          !m = this%dis%con%jas(ii)
          m = this%dis%con%ja(ii)
          iis = this%dis%con%jas(ii)
          !
          ! -- Calculate conductance depending on whether connection is
          !    vertical (0), horizontal (1), or staggered horizontal (2)
          ! m < n = m is vertically above n
          area_n = this%dis%get_area(n)
          if (this%dis%con%ihc(iis) == 0 .and. m < n) then
              area_conn = this%dis%get_area(m)
              hwva = this%dis%con%hwva(iis)
              va_scale = this%dis%con%hwva(iis) / this%dis%get_area(m)
              gs_conn = this%gs(m)
              !call this%dis%noder_to_string(n,msg1)
              !call this%dis%noder_to_string(m,msg2)
              !write(*,*) n, trim(msg1), gs, m, trim(msg2), gs_conn
              gs = gs + (gs_conn * va_scale)
          end if
        end do
        !write (*,*) n, gs
        this%gs(n) = gs
      end do
   end if
   !
   ! -- return
   return

  end subroutine calc_aqgeo_stress


  subroutine calc_aqeff_stress(this)
! ******************************************************************************
! calc_aqeff_stress -- calculate the effective stress for nodes with interbeds
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
!    use SimModule, only: ustop, store_error, count_errors, store_error_unit
    implicit none
    class(IbcType) :: this
    integer(I4B) :: i, n, ii, m
    real(DP) :: es, bot, x
    real(DP) :: sadd
    character(len=LINELENGTH) :: errmsg
    character(len=*), parameter :: fmtneg = &
        "('NEGATIVE EFFECTIVE STRESS FOR NODE (',i0,') GEOSTATIC (',f10.4,'), " // &
        "XNEW (',f10.4,'), BOTM (',f10.4,')')"
! ------------------------------------------------------------------------------

    do i = 1, this%nbound
      n = this%nodelist(i)
      x = this%xnew(n)
      if (this%igeocalc == 0) then
        es = x
      else
        bot = this%dis%bot(n)
        es = this%gs(n) - x + bot
        !if (es < DZERO) then
        !  write(errmsg, fmt=fmtneg) n, this%gs(n), x, bot
        !  call store_error(errmsg)
        !end if
      end if
      sadd = DZERO
      if (this%igeostressoff == 1) then
        sadd = this%sig0(n)
      end if
      this%es(i) = es + sadd
    end do

    if (count_errors() > 0) then
      call store_error_unit(this%inunit)
      call ustop()
    end if
  end subroutine calc_aqeff_stress


  subroutine calc_nodelay_gwf(this, i, rho1, rho2, rhs, argtled)
! ******************************************************************************
! calc_nodelay_gwf -- Calculate rho1, rho2, and rhs for no-delay
!                               interbeds
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use TdisModule, only: delt
    implicit none
    ! -- dummy variables
    class(IbcType) :: this
    integer(I4B), intent(in) :: i
    real(DP), intent(inout) :: rho1
    real(DP), intent(inout) :: rho2
    real(DP), intent(inout) :: rhs
    real(DP), intent(in), optional :: argtled
    ! -- local variables
    integer(I4B) :: n
    real(DP) :: tled,top,bot,thk_fac,sto_fac
    real(DP) :: area
    real(DP) :: x
    real(DP) :: thk_node,thk_ibs,denom,pcs_n, fact
    real(DP) :: es0
    real(DP) :: f
    real(DP) :: haq
    real(DP) :: haq0
    character(len=LINELENGTH) :: msg
! ------------------------------------------------------------------------------
    if (present(argtled)) then
      tled = argtled
    else
      tled = DONE / delt
    endif
    n = this%nodelist(i)
    area = this%dis%get_area(n)
    bot = this%dis%bot(n)
    top = this%dis%top(n)
    thk_node = top - bot
    haq = this%xnew(n)
    x = haq
    if (x > top) x = top
    thk_fac = DONE
    if (this%iconstantb == 0) then
      thk_fac = (x - bot) / thk_node
    end if
    thk_ibs = thk_fac * this%thick(i)
    if (this%igeocalc == 0) then
      f = DONE
    else
      es0 = this%es0(i)
      denom = (DONE + this%void(i)) * (es0 - (this%znode(i) - bot)) * &
              (this%sgs(n) - DONE)
      if (denom /= DZERO) then
        f = DONE / denom
      else
        f = DZERO
      end if
    end if
    sto_fac = tled * thk_ibs * f
    !
    ! -- calculate rho1 and rho2
    rho1 = this%rci(i) * sto_fac
    rho2 = rho1
    if (this%igeocalc == 0) then
      if (haq < this%pcs(i)) then
        rho2 = this%ci(i) * sto_fac
      end if
      rhs = -(rho2 * this%pcs(i) + rho1 * (this%es0(i) - this%pcs(i)))
    else
      if (this%es(i) > this%pcs(i)) then
          rho2 = this%ci(i) * sto_fac
      end if
      rhs = -rho2 * (this%gs(n) + bot) + (this%pcs(i) * (rho2 - rho1)) + &
             (rho1 * this%es0(i))
    end if
    !
    !
    !call this%dis%noder_to_string(n,msg)
    !write(*,*) n, trim(msg), this%gs(n), this%es(n), sto_fac,           &
    !           rho1, rho2, rhs
    !write(*,*) n,trim(msg), rho1, rho2, rhs
    !
    ! -- return
    return

  end subroutine calc_nodelay_gwf

  subroutine ibc_rp(this)
! ******************************************************************************
! ibc_rp -- Read and Prepare
! Subroutine: (1) read itmp
!             (2) read new boundaries if itmp>0
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use ConstantsModule, only: LINELENGTH
    use TdisModule, only: kper, nper
!    use SimModule, only: store_error, ustop, count_errors
    implicit none
    ! -- dummy
    class(IbcType),intent(inout) :: this
    ! -- local
    character(len=LINELENGTH) :: line, errmsg, keyword
    integer(I4B) :: istart, istop, lloc, ierr
    logical :: isfound, endOfBlock
    ! -- formats
    character(len=*),parameter :: fmtblkerr =                                  &
      "('Error.  Looking for BEGIN PERIOD iper.  Found ', a, ' instead.')"
    character(len=*),parameter :: fmtlsp =                                     &
      "(1X,/1X,'REUSING ',A,'S FROM LAST STRESS PERIOD')"
! ------------------------------------------------------------------------------
    !
    if(this%inunit == 0) return
    !
    ! -- Set ionper to the stress period number for which a new block of data
    !    will be read.
    if (this%ionper < kper) then
      !
      ! -- get period block
      ! When reading a list, OPEN/CLOSE is handled by list reader,
      ! so supportOpenClose needs to be false in call the GetBlock.
      ! When reading as arrays, set supportOpenClose as desired.
      call this%parser%GetBlock('PERIOD', isfound, ierr)
      if(isfound) then
        !
        ! -- read ionper and check for increasing period numbers
        call this%read_check_ionper()
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
        end if
      endif
    end if
    !
    ! -- Read data if ionper == kper
    if(this%ionper == kper) then
      do
        call this%parser%GetNextLine(endOfBlock)
        if (endOfBlock) exit
        call this%parser%GetStringCaps(keyword)
        call this%parser%GetRemainingLine(line)
        lloc = 1
        !
        ! -- Parse the keywords
        select case (keyword)
        case ('SIG0')
          if (this%igeostressoff == 1) then
            call this%dis%read_grid_array(line, lloc, istart, istop, this%iout, &
                                          this%parser%iuactive, this%sig0, 'SIG0')
          else
            write (errmsg, '(a)') &
              '****ERROR. SIG0 SPECIFIED BUT GEO_STRESS_OFFSET NOT SPECIFIED IN OPTIONS BLOCK'
            call store_error(trim(errmsg))
            call this%parser%StoreErrorUnit()
          end if
        case default
          call store_error('****ERROR. LOOKING FOR VALID VARIABLE NAME.  FOUND: ')
          call store_error(trim(line))
          call this%parser%StoreErrorUnit()
        end select
      end do
    !
    else
      write(this%iout,fmtlsp) trim(this%filtyp)
    endif
    !
    ! -- terminate if errors encountered in reach block
    if (count_errors() > 0) then
      call this%parser%StoreErrorUnit()
      call ustop()
    end if
    !
    ! -- return
    return
  end subroutine ibc_rp

  subroutine ibc_ad(this)
! ******************************************************************************
! ibc_ad -- Advance ibc data
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(IbcType) :: this
    ! -- local
    integer(I4B) :: n
    integer(I4B) :: j
    integer(I4B) :: idelay
    integer(I4B) :: node
    real(DP) :: h
    real(DP) :: es
    real(DP) :: pcs
    real(DP) :: b
    real(DP) :: bot
    real(DP) :: fact
! ------------------------------------------------------------------------------
    !
    ! -- Advance the time series manager
    call this%TsManager%ad()
    !
    ! -- update state variables
    do n = 1, this%nbound
      idelay = this%idelay(n)
      ! no delay beds
      if (idelay == 0) then
        if (.not. this%first_time) then
          node = this%nodelist(n)
          es = this%es(n)
          pcs = this%pcs(n)
          if (this%igeocalc == 0) then
            h = this%xnew(node)
            if (h < pcs) then
              this%pcs(n) = h
            end if
          else
            if (es > pcs) then
              this%pcs(n) = es
            end if
          end if
          ! -- update previous effective stress
          if (this%igeocalc == 0) then
            this%es0(n) = this%xnew(node)
          else
            this%es0(n) = this%es(n)
          end if
        end if
      !
      ! -- delay beds
      else
        b = DZERO
        do j = 1, this%ndelaycells
          b = b + this%dbdz(j, idelay)
          ! update preconsolidation stress
          if (.not. this%first_time) then
            if (this%igeocalc == 0) then
              if (this%dbh(j, idelay) < this%dbpcs(j, idelay)) then
                this%dbpcs(j, idelay) = this%dbh(j, idelay)
              end if
              this%dbes(j, idelay) = this%dbh(j, idelay)
            else
              if (this%dbes(j, idelay) > this%dbpcs(j, idelay)) then
                this%dbpcs(j, idelay) = this%dbes(j, idelay)
              end if
            end if
          end if
          this%dbh0(j, idelay) = this%dbh(j, idelay)
          this%dbgeo0(j, idelay) = this%dbgeo(j, idelay)
          this%dbes0(j, idelay) = this%dbes(j, idelay)
        end do
        if (this%iconstantb /= 0) then
          this%thick(n) = b
        end if
      end if
    end do
    !
    ! -- For each observation, push simulated value and corresponding
    !    simulation time from "current" to "preceding" and reset
    !    "current" value.
    call this%obs%obs_ad()
    !
    ! -- return
    return
  end subroutine ibc_ad

  subroutine ibc_cf(this)
! ******************************************************************************
! ibc_cf -- Formulate the HCOF and RHS terms
! Subroutine: (1) skip if no ibcs
!             (2) calculate hcof and rhs
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use TdisModule, only: delt, kper
    implicit none
    class(IbcType) :: this
    integer(I4B) :: i, n
    integer(I4B) :: ibc
    real(DP) :: tled, top, bot, x, thk_fac, sto_fac
    real(DP) :: thk_node, thk_ibs, demon, rho1, rho2
    real(DP) :: pcs, area, fact, rhs
    real(DP) :: f
! ------------------------------------------------------------------------------
!
! -- initialize variables
    rhs = DZERO
    rho2 = DZERO
    ibc = 1
!
! -- Return if no ibcs
    if (this%nbound == 0) then
      ibc = 0
    end if
! -- Return if steady state
    if (this%gwfiss /= 0) then
      ibc = 0
    end if

    tled = DONE / delt
!
! -- only make calculations if ibc = 1
    if (ibc == 1) then
      !
      ! -- update geostatic load calculation
      call this%calc_aqgeo_stress()
      call this%calc_aqznode()
      call this%calc_aqeff_stress()

      if (this%first_time) then
        do i = 1, this%nbound
          n = this%nodelist(i)
          pcs = this%pcs(i)
          if (this%igeocalc == 0) then
            if (pcs < this%es(i)) then
              pcs = this%es(i)
            end if
          else
            ! -- transfer initial preconsolidation stress (and apply offset if needed)
            if (this%ibedstressoff == 1) then
                pcs = this%es(i) + this%pcs(i)
            else
              if (pcs < this%es(i)) then
                pcs = this%es(i)
              end if
            end if
          end if
          this%pcs(i) = pcs
          bot = this%dis%bot(n)
          ! scale cr and cc
          if (this%istoragec == 1) then
            if (this%igeocalc == 0) then
              fact = DONE
            else
              ! fact = area * (1.0 - void) * (eff st) - (znode - bot) * (sgs  -DONE)
              fact = this%es(i) - (this%znode(i) - bot) * (this%sgs(n) - DONE)
              fact = fact *  (DONE + this%void(i))
            end if
          else
              fact = dlog10es
          end if
          this%ci(i) = this%ci(i) * fact
          this%rci(i) = this%rci(i) * fact
          this%es0(i) = this%es(i)
        end do
        this%first_time = .false.
      end if

      do i = 1, this%nbound
        n = this%nodelist(i)
        area = this%dis%get_area(n)
        !
        ! -- skip inactive and constant head cells
        if (this%ibound(n) < 1) then
          this%rhs(i) = DZERO
          this%hcof(i) = DZERO
          cycle
        end if
        if (this%idelay(i) == 0) then
          !
          ! -- calculate ibc rho1 and rho2
          call this%calc_nodelay_gwf(i, rho1, rho2, rhs)
          f = area
        else
          call this%calc_delay_interbed(i)
          call this%calc_delay_gwf(i, rho2, rhs)
          !call this%calc_delay_dstor(i, rhs)
          !rhs = -rhs * tled
          f = area * this%rnb(i)
        end if
        this%rhs(i) = rhs * f
        this%hcof(i) = -rho2 * f
      end do
    else
      do i = 1, this%nbound
        this%rhs(i) = rhs
        this%hcof(i) = rho2
      end do
    end if
    !
    ! -- return
    return
  end subroutine ibc_cf

  subroutine define_listlabel(this)
! ******************************************************************************
! define_listlabel -- Define the list heading that is written to iout when
!   PRINT_INPUT option is used.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    class(IbcType), intent(inout) :: this
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
    write(this%listlabel, '(a, a16)') trim(this%listlabel), 'INITIAL_STRESS'
    write(this%listlabel, '(a, a16)') trim(this%listlabel), 'FRAC'
    if (this%istoragec == 1) then
        write(this%listlabel, '(a, a16)') trim(this%listlabel), 'SSE'
        write(this%listlabel, '(a, a16)') trim(this%listlabel), 'SSV'
    else
        write(this%listlabel, '(a, a16)') trim(this%listlabel), 'RCI'
        write(this%listlabel, '(a, a16)') trim(this%listlabel), 'CI'
    endif
        write(this%listlabel, '(a, a16)') trim(this%listlabel), 'VOID'
        write(this%listlabel, '(a, a16)') trim(this%listlabel), 'IDELAY'
    if(this%inamedbound == 1) then
      write(this%listlabel, '(a, a16)') trim(this%listlabel), 'BOUNDARY NAME'
    endif
    !
    ! -- return
    return
  end subroutine define_listlabel

  subroutine calc_delay_interbed(this, ib)
! ******************************************************************************
! calc_delay_interbed -- Calculate flow in delay interbeds.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    class(IbcType), intent(inout) :: this
    integer(I4B), intent(in) :: ib
    ! -- local variables
    integer(I4B) :: n
    integer(I4B) :: node
    integer(I4B) :: idelay
    integer(I4B) :: icnvg
    integer(I4B) :: iter
    real(DP) :: dh
    real(DP) :: dhmax
    real(DP) :: area
    real(DP) :: c1
    real(DP) :: c2
    real(DP) :: f
    real(DP), parameter :: dclose = DTEN * DPREC
! ------------------------------------------------------------------------------
    !
    ! -- initialize variables
    !
    ! -- calculate z for each delay bed cell
    call this%calc_delay_z(ib)
    !
    ! -- calculate geostatic stress for each delay bed cell
    call this%calc_delay_stress(ib)
    !
    ! -- solve for delay bed heads
    if (this%thick(ib) > DZERO) then
      icnvg = 0
      iter = 0
      do
        iter = iter + 1
        ! -- assemble coefficients
        call this%assemble_delay(ib, 0)
        ! -- solve for head change in delay intebed cells
        call solve_delay(this%ndelaycells, this%dbal, this%dbad, this%dbau, &
                         this%dbrhs, this%dbdh, this%dbaw)
        ! -- update delay bed head and check convergence
        dhmax = DZERO
        idelay = this%idelay(ib)
        do n = 1, this%ndelaycells
          dh = this%dbh(n, idelay) - this%dbdh(n)
          this%dbh(n, idelay) = this%dbdh(n)
          if (abs(dh) > abs(dhmax)) then
            dhmax = this%dbdh(n)
          end if
        end do
        if (abs(dhmax) < dclose) then
          icnvg = 1
          exit
        end if
      end do
    end if
    !
    ! -- return
    return
  end subroutine calc_delay_interbed


  subroutine calc_delay_z(this, ib)
! ******************************************************************************
! calc_delay_z -- Calculate z for delay interbeds cells.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    class(IbcType), intent(inout) :: this
    integer(I4B), intent(in) :: ib
    ! -- local variables
    integer(I4B) :: n
    integer(I4B) :: idelay
    real(DP) :: znode
    real(DP) :: dzz
    real(DP) :: z
    real(DP) :: b
    real(DP) :: dz
! ------------------------------------------------------------------------------
    !
    ! -- initialize variables
    idelay = this%idelay(ib)
    znode = this%znode(ib)
    b = this%thick(ib)
    dzz = DHALF * b
    z = znode + dzz
    dz = this%dbdz(1, idelay)
    ! -- calculate z for each delay interbed node
    do n = 1, this%ndelaycells
      z = z - dz
      this%dbz(n, idelay) = z
      z = z - dz
      if (n < this%ndelaycells) then
        dz = this%dbdz(n+1, idelay)
      end if
    end do
    !
    ! -- return
    return

  end subroutine calc_delay_z

  subroutine calc_delay_stress(this, ib)
! ******************************************************************************
! calc_delay_stress -- Calculate geostatic and effective stress in delay
!                      interbeds.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    class(IbcType), intent(inout) :: this
    integer(I4B), intent(in) :: ib
    ! -- local variables
    integer(I4B) :: n
    integer(I4B) :: idelay
    integer(I4B) :: node
    real(DP) :: haq
    real(DP) :: sigma
    real(DP) :: topaq
    real(DP) :: botaq
    real(DP) :: dzhalf
    real(DP) :: sadd
    real(DP) :: sgm
    real(DP) :: sgs
    real(DP) :: h
    real(DP) :: z
    real(DP) :: top
    real(DP) :: bot
! ------------------------------------------------------------------------------
    !
    ! -- initialize variables
    n = this%nodelist(ib)
    idelay = this%idelay(ib)
    node = this%nodelist(ib)
    haq = this%xnew(node)
    sigma = this%gs(node)
    topaq = this%dis%top(node)
    botaq = this%dis%bot(node)
    dzhalf = DHALF * this%dbdz(1, idelay)
    sgm = this%sgm(node)
    sgs = this%sgs(node)
    top = this%dbz(1, idelay) + dzhalf
    !
    ! -- calculate the geostatic load in the aquifer at the top of the interbed
    if (this%igeocalc > 0) then
      if (haq > top) then
        sadd = (top - botaq) * sgs
      else if (haq < botaq) then
        sadd = (top - botaq) * sgm
      else
        sadd = ((top - haq) * sgm) + ((haq - botaq) * sgs)
      end if
      sigma = sigma - sadd
    end if
    !
    ! -- calculate geostatic and effective stress for each interbed node
    do n = 1, this%ndelaycells
      h = this%dbh(n, idelay)
      if (this%igeocalc == 0) then
        this%dbes(n, idelay) = h
      ! -- geostatic calculated at the bottom of the delay cell
      ! -- CHECK ***evaluate if this is the correct location***
      else
        z = this%dbz(n, idelay)
        top = z + dzhalf
        bot = z - dzhalf
        if (h > top) then
            sadd = (top - bot) * sgs
        else if (h < bot) then
            sadd = (top - bot) * sgm
        else
            sadd = ((top - h) * sgm) + ((h - bot) * sgs)
        end if
        sigma = sigma + sadd
        this%dbgeo(n, idelay) = sigma
      end if
    end do
    !
    ! -- return
    return
  end subroutine calc_delay_stress

  subroutine calc_delay_sskessk(this, ib, n, sske, ssk)
! ******************************************************************************
! calc_delay_sskessk -- Calculate sske and sske for a node in a delay
!                       interbed cell.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    class(IbcType), intent(inout) :: this
    integer(I4B), intent(in) :: ib
    integer(I4B), intent(in) :: n
    real(DP), intent(inout) :: sske
    real(DP), intent(inout) :: ssk
    ! -- local variables
    integer(I4B) :: idelay
    real(DP) :: es
    real(DP) :: denom
    real(DP) :: f
! ------------------------------------------------------------------------------
    !
    ! -- initialize variables
    sske = DZERO
    ssk = DZERO
    idelay = this%idelay(ib)
    !
    !
    if (this%igeocalc == 0) then
      f = DONE
    else
      es = this%dbes0(n, idelay)
      denom = (DONE + this%dbvoid(n, idelay)) * es
      if (denom /= DZERO) then
        f = DONE / denom
      else
        f = DZERO
      end if
    end if
    sske = f * this%rci(ib)
    if (this%igeocalc == 0) then
      if (this%dbh(n, idelay) < this%dbpcs(n, idelay)) then
        ssk = f * this%ci(ib)
      else
        ssk = sske
      end if
    else
      if (es > this%dbpcs(n, idelay)) then
        ssk = f * this%ci(ib)
      else
        ssk = sske
      end if
    end if
    !
    ! -- return
    return
  end subroutine calc_delay_sskessk


  subroutine assemble_delay(this, ib, ihd)
! ******************************************************************************
! assemble_delay -- Assemble coefficients for delay interbeds cells.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use TdisModule, only: delt
    ! -- dummy variables
    class(IbcType), intent(inout) :: this
    integer(I4B), intent(in) :: ib
    integer(I4B), intent(in), optional :: ihd
    ! -- local variables
    integer(I4B) :: iadd
    integer(I4B) :: n
    integer(I4B) :: node
    integer(I4B) :: idelay
    real(DP) :: haq
    real(DP) :: dz
    real(DP) :: c
    real(DP) :: c2
    real(DP) :: c3
    real(DP) :: f
    real(DP) :: sske
    real(DP) :: ssk
    real(DP) :: z
    real(DP) :: top
    real(DP) :: bot
    real(DP) :: h
    real(DP) :: aii
    real(DP) :: r
    real(DP) :: ahm1
! ------------------------------------------------------------------------------
    !
    ! -- process optional variable
    if (present(ihd)) then
        if (ihd == 0) then
          iadd = 0
        else
          iadd = 1
        end if
    else
      iadd = 1
    endif
    !
    ! -- initialize variables
    idelay = this%idelay(ib)
    node = this%nodelist(ib)
    haq = this%xnew(node)
    !
    !
    do n = 1, this%ndelaycells
      dz = this%dbdz(n, idelay)
      c = this%kv(ib) / dz
      c2 = DTWO * c
      c3 = DTHREE * c
      f = dz / delt
      call this%calc_delay_sskessk(ib, n, sske, ssk)
      ! -- diagonal and right hand side
      aii = -ssk * f
      z = this%dbz(n, idelay)
      top = z + dz
      bot = z - dz
      h = this%dbh(n, idelay)
      if (this%igeocalc == 0) then
        r = -f * &
             (ssk * (this%dbpcs(n, idelay)) + &
              sske * (this%dbh0(n, idelay) - this%dbpcs(n, idelay)))
      else
        r = -f * &
             (ssk * (this%dbgeo(n, idelay) + z - this%dbpcs(n, idelay)) + &
              sske * (this%dbpcs(n, idelay) - this%dbes0(n, idelay)))
      end if
      if (n == 1 .or. n == this%ndelaycells) then
        aii = aii - c3
        r = r - c2 * haq
      else
        aii = aii - c2
      end if
      ahm1 = aii * h
      ! -- off diagonals
      ! -- lower
      if (n > 1) then
        this%dbal(n) = c
        ahm1 = ahm1 + c * this%dbh(n-1, idelay)
      end if
      ! -- upper
      if (n < this%ndelaycells) then
        this%dbau(n) = c
        ahm1 = ahm1 + c * this%dbh(n+1, idelay)
      end if
      if (iadd == 0) then
        ahm1 = DZERO
      end if
      this%dbad(n) = aii
      this%dbrhs(n) = r - ahm1
    end do
    !
    ! -- return
    return

  end subroutine assemble_delay

  subroutine solve_delay(n, tl, td, tu, b, x, w)
! ******************************************************************************
! solve_delay -- Solve for head change in delay interbeds cells.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    integer(I4B), intent(in) :: n
    real(DP), dimension(n), intent(in) :: tl
    real(DP), dimension(n), intent(in) :: td
    real(DP), dimension(n), intent(in) :: tu
    real(DP), dimension(n), intent(in) :: b
    real(DP), dimension(n), intent(inout) :: x
    real(DP), dimension(n), intent(inout) :: w
    ! -- local variables
    integer(I4B) :: j
    real(DP) :: bet
    real(DP) :: beti
! ------------------------------------------------------------------------------
    ! -- initialize variables
    w(1) = DZERO
    bet = td(1)
    beti = DONE / bet
    x(1) = b(1) * beti
    ! -- decomposition and forward substitution
    do j = 2, n
      w(j) = tu(j-1) * beti
      bet = td(j) - tl(j) * w(j)
      beti = DONE / bet
      x(j) = (b(j) - tl(j) * x(j-1)) * beti
    end do
    ! -- backsubstitution
    do j = n-1, 1, -1
      x(j) = x(j) - w(j+1) * x(j+1)
    end do
    ! -- return
    return
  end subroutine solve_delay

  subroutine calc_delay_dstor(this, ib, rhs)
! ******************************************************************************
! calc_delay_dstor -- Calculate change in storage in a delay interbed.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    class(IbcType), intent(inout) :: this
    integer(I4B), intent(in) :: ib
    real(DP), intent(inout) :: rhs
    ! -- local variables
    integer(I4B) :: idelay
    integer(I4B) :: n
    real(DP) :: comp
    real(DP) :: dz
    real(DP) :: sske
    real(DP) :: ssk
    real(DP) :: es
    real(DP) :: denom
    real(DP) :: f
    real(DP) :: v
    real(DP) :: strain
    real(DP) :: void
! ------------------------------------------------------------------------------
    !
    ! -- initialize variables
    idelay = this%idelay(ib)
    rhs = DZERO
    !
    !
    if (this%thick(ib) > DZERO) then
      do n = 1, this%ndelaycells
        dz = this%dbdz(n, idelay)
        void = this%dbvoid(n, idelay)
        call this%calc_delay_sskessk(ib, n, sske, ssk)
        if (this%igeocalc == 0) then
          v = ssk * (this%dbpcs(n, idelay) - this%dbh(n, idelay))
          v = v + sske * (this%dbh0(n, idelay) - this%dbpcs(n, idelay))
          v = v * dz
        else
          denom = (DONE + void) * this%dbes0(n, idelay)
          if (denom /= DZERO) then
            f = dz / denom
          else
            f = DZERO
          end if
          v = ssk * (this%dbes(n, idelay) - this%dbpcs(n, idelay))
          v = v + sske * (this%dbpcs(n, idelay) - this%dbes0(n, idelay))
        end if
        rhs = rhs + v
      end do
    end if
    !
    ! -- return
    return
  end subroutine calc_delay_dstor

  subroutine calc_delay_err(this, ib, err)
! ******************************************************************************
! calc_delay_err -- Calculate error in a delay interbed.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    class(IbcType), intent(inout) :: this
    integer(I4B), intent(in) :: ib
    real(DP), intent(inout) :: err
    ! -- local variables
    integer(I4B) :: idelay
    integer(I4B) :: n
    real(DP) :: hcof
    real(DP) :: v
    real(DP) :: strain
    real(DP) :: void
! ------------------------------------------------------------------------------
    !
    ! -- initialize variables
    idelay = this%idelay(ib)
    err = DZERO
    !
    !
    if (this%thick(ib) > DZERO) then
      call this%assemble_delay(ib, 0)
      do n = 1, this%ndelaycells
        v = this%dbad(n) * this%dbh(n, idelay)
        if (n > 1) then
          v = v + this%dbal(n) * this%dbh(n-1, idelay)
        end if
        if (n < this%ndelaycells) then
          v = v + this%dbau(n) * this%dbh(n+1, idelay)
        end if
        v = v - this%dbrhs(n)
        err = err + v
      end do
    end if
    !
    ! -- return
    return
  end subroutine calc_delay_err

  subroutine calc_delay_comp(this, ib)
! ******************************************************************************
! calc_delay_comp -- Calculate compaction in a delay interbed.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    class(IbcType), intent(inout) :: this
    integer(I4B), intent(in) :: ib
    ! -- local variables
    integer(I4B) :: idelay
    integer(I4B) :: n
    real(DP) :: comp
    real(DP) :: dz
    real(DP) :: sske
    real(DP) :: ssk
    real(DP) :: es
    real(DP) :: denom
    real(DP) :: f
    real(DP) :: v
    real(DP) :: strain
    real(DP) :: void
! ------------------------------------------------------------------------------
    !
    ! -- initialize variables
    idelay = this%idelay(ib)
    comp = DZERO
    !
    !
    if (this%thick(ib) > DZERO) then
      do n = 1, this%ndelaycells
        dz = this%dbdz(n, idelay)
        void = this%dbvoid(n, idelay)
        call this%calc_delay_sskessk(ib, n, sske, ssk)
        if (this%igeocalc == 0) then
          v = ssk * (this%dbpcs(n, idelay) - this%dbh(n, idelay))
          v = v + sske * (this%dbh0(n, idelay) - this%dbpcs(n, idelay))
          v = v * dz
        else
          denom = (DONE + void) * this%dbes0(n, idelay)
          if (denom /= DZERO) then
            f = dz / denom
          else
            f = DZERO
          end if
          v = ssk * (this%dbes(n, idelay) - this%dbpcs(n, idelay))
          v = v + sske * (this%dbpcs(n, idelay) - this%dbes0(n, idelay))
        end if
        comp = comp + v
        if (this%iconstantb == 0) then
          strain = comp / dz
          this%dbvoid(n, idelay) = strain + void * (strain + DONE)
          this%dbdz(n, idelay) = dz * (strain + DONE)
        end if
      end do
    end if
    !
    ! -- fill compaction
    this%comp(ib) = comp
    this%totalcomp(ib) = this%totalcomp(ib) + comp * this%rnb(ib)
    !
    ! -- return
    return
  end subroutine calc_delay_comp

  subroutine calc_delay_gwf(this, ib, hcof, rhs)
! ******************************************************************************
! calc_delay_gwf -- Calculate hcof and rhs for delay interbed contribution to
!                   GWF cell.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    class(IbcType), intent(inout) :: this
    integer(I4B), intent(in) :: ib
    real(DP), intent(inout) :: hcof
    real(DP), intent(inout) :: rhs
    ! -- local variables
    integer(I4B) :: idelay
    integer(I4B) :: node
    real(DP) :: haq
    real(DP) :: area
    real(DP) :: f
    real(DP) :: c1
    real(DP) :: c2
! ------------------------------------------------------------------------------
    !
    ! -- initialize variables
    idelay = this%idelay(ib)
    hcof = DZERO
    rhs = DZERO
    if (this%thick(ib) > DZERO) then
      ! -- calculate terms for gwf matrix
      node = this%nodelist(ib)
      haq = this%xnew(node)
      area = this%dis%get_area(node)
      c1 = DTWO * this%kv(ib) / this%dbdz(1, idelay)
      rhs = -c1 * this%dbh(1, idelay)
      c2 = DTWO * this%kv(ib) / this%dbdz(this%ndelaycells, idelay)
      rhs = rhs - c2 * this%dbh(this%ndelaycells, idelay)
      hcof = c1 + c2
    end if
    !
    ! -- return
    return
  end subroutine calc_delay_gwf
  !
  ! -- Procedures related to observations (type-bound)
  logical function ibc_obs_supported(this)
  ! ******************************************************************************
  ! ibc_obs_supported
  !   -- Return true because ibc package supports observations.
  !   -- Overrides BndType%bnd_obs_supported()
  ! ******************************************************************************
  !
  !    SPECIFICATIONS:
  ! ------------------------------------------------------------------------------
    class(IbcType) :: this
  ! ------------------------------------------------------------------------------
    ibc_obs_supported = .true.
    return
  end function ibc_obs_supported


  subroutine ibc_df_obs(this)
  ! ******************************************************************************
  ! ibc_df_obs (implements bnd_df_obs)
  !   -- Store observation type supported by ibc package.
  !   -- Overrides BndType%bnd_df_obs
  ! ******************************************************************************
  !
  !    SPECIFICATIONS:
  ! ------------------------------------------------------------------------------
    ! -- dummy
    class(IbcType) :: this
    ! -- local
    integer(I4B) :: indx
  ! ------------------------------------------------------------------------------
    !
    ! -- Store obs type and assign procedure pointer
    !    for ibc observation type.
    call this%obs%StoreObsType('ibc', .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => ibc_process_obsID
    !
    ! -- Store obs type and assign procedure pointer
    !    for compaction observation type.
    call this%obs%StoreObsType('compaction', .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => ibc_process_obsID
    !
    ! -- Store obs type and assign procedure pointer
    !    for total-compaction observation type.
    call this%obs%StoreObsType('total-compaction', .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => ibc_process_obsID
    !
    ! -- Store obs type and assign procedure pointer
    !    for total-compaction observation type.
    call this%obs%StoreObsType('thickness', .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => ibc_process_obsID
    !
    ! -- Store obs type and assign procedure pointer
    !    for delay-head observation type.
    call this%obs%StoreObsType('delay-head', .false., indx)
    this%obs%obsData(indx)%ProcessIdPtr => ibc_process_obsID
    !
    return
  end subroutine ibc_df_obs


  subroutine ibc_bd_obs(this)
    ! **************************************************************************
    ! ibc_bd_obs
    !   -- Calculate observations this time step and call
    !      ObsType%SaveOneSimval for each IbcType observation.
    ! **************************************************************************
    !
    !    SPECIFICATIONS:
    ! --------------------------------------------------------------------------
    ! -- dummy
    class(IbcType), intent(inout) :: this
    ! -- local
    integer(I4B) :: i, j, n, nn
    integer(I4B) :: idelay
    integer(I4B) :: ncol
    real(DP) :: v
    real(DP) :: r
    character(len=100) :: msg
    type(ObserveType), pointer :: obsrv => null()
    !---------------------------------------------------------------------------
    !
    ! Write simulated values for all ibc observations
    if (this%obs%npakobs>0) then
      call this%obs%obs_bd_clear()
      do i = 1, this%obs%npakobs
        obsrv => this%obs%pakobs(i)%obsrv
        nn = size(obsrv%indxbnds)
        do j = 1, nn
          n = obsrv%indxbnds(j)
          v = DZERO
          select case (obsrv%ObsTypeId)
            case ('IBC')
              v = this%gwflow(n)
            case ('COMPACTION')
              v = this%comp(n)
            case ('TOTAL-COMPACTION')
              v = this%totalcomp(n)
            case ('THICKNESS')
              v = this%thick(n)
            case ('DELAY-HEAD')
              if (n > this%ndelaycells) then
                r = real(n, DP) / real(this%ndelaycells, DP)
                idelay = int(floor(r)) + 1
                ncol = mod(n, this%ndelaycells)
              else
                idelay = 1
                ncol = n
              end if
              v = this%dbh(ncol, idelay)
            case default
              msg = 'Error: Unrecognized observation type: ' // trim(obsrv%ObsTypeId)
              call store_error(msg)
          end select
          call this%obs%SaveOneSimval(obsrv, v)
        end do
      end do
    end if
    !
    ! -- write summary of package block error messages
    if (count_errors() > 0) then
      call this%parser%StoreErrorUnit()
      call ustop()
    end if
    !
    return
  end subroutine ibc_bd_obs


  subroutine ibc_rp_obs(this)
    ! -- dummy
    class(IbcType), intent(inout) :: this
    ! -- local
    integer(I4B) :: i, j, n, nn1, nn2
    integer(I4B) :: idelay
    character(len=200) :: ermsg
    character(len=LENBOUNDNAME) :: bname
    logical :: jfound
    class(ObserveType),   pointer :: obsrv => null()
    ! --------------------------------------------------------------------------
    ! -- formats
10  format('Error: Boundary "',a,'" for observation "',a, &
           '" is invalid in package "',a,'"')
30  format('Error: Boundary name not provided for observation "',a, &
           '" in package "',a,'"')
60  format('Error: Invalid node number in OBS input: ',i5)
    do i = 1, this%obs%npakobs
      obsrv => this%obs%pakobs(i)%obsrv
      !
      ! -- indxbnds needs to be deallocated and reallocated (using
      !    ExpandArray) each stress period because list of boundaries
      !    can change each stress period.
      if (allocated(obsrv%indxbnds)) then
        deallocate(obsrv%indxbnds)
      end if
      !
      ! -- get node number 1
      nn1 = obsrv%NodeNumber
      if (nn1 == NAMEDBOUNDFLAG) then
        bname = obsrv%FeatureName
        if (bname /= '') then
          ! -- Observation location(s) is(are) based on a boundary name.
          !    Iterate through all boundaries to identify and store
          !    corresponding index(indices) in bound array.
          jfound = .false.
          do j = 1, this%nbound
            if (this%boundname(j) == bname) then
              jfound = .true.
              call ExpandArray(obsrv%indxbnds)
              n = size(obsrv%indxbnds)
              obsrv%indxbnds(n) = j
            endif
          enddo
          if (.not. jfound) then
            write(ermsg,10)trim(bname), trim(obsrv%name), trim(this%name)
            call store_error(ermsg)
          endif
        else
          write (ermsg,30) trim(obsrv%name), trim(this%name)
          call store_error(ermsg)
        endif
      elseif (nn1 < 1 .or. nn1 > this%nbound) then
        write (ermsg, '(4x,a,1x,a,1x,a,1x,i0,1x,a,1x,i0,1x,a)') &
          'ERROR:', trim(adjustl(obsrv%ObsTypeId)), &
          ' interbed must be > 0 and <=', this%nbound, &
          '(specified value is ', nn1, ')'
        call store_error(ermsg)
      else
        idelay = this%idelay(nn1)
        call ExpandArray(obsrv%indxbnds)
        n = size(obsrv%indxbnds)
        if (n == 1) then
          if (obsrv%ObsTypeId=='DELAY-HEAD') then
            j = (idelay - 1) * this%ndelaycells + 1
            nn2 = obsrv%NodeNumber2
            if (nn2 < 1 .or. nn2 > this%ndelaycells) then
              write (ermsg, '(4x,a,1x,a,1x,a,1x,i0,1x,a,1x,i0,1x,a)') &
                'ERROR:', trim(adjustl(obsrv%ObsTypeId)), &
                ' interbed cell must be > 0 and <=', this%ndelaycells, &
                '(specified value is ', nn2, ')'
              call store_error(ermsg)
            else
              j = (idelay - 1) * this%ndelaycells + nn2
            end if
            obsrv%indxbnds(1) = j
          else
            obsrv%indxbnds(1) = nn1
          end if
        else
          ermsg = 'Programming error in ibc_rp_obs'
          call store_error(ermsg)
        end if
      end if
      !
      ! -- catch non-cumulative observation assigned to observation defined
      !    by a boundname that is assigned to more than one element
      if (obsrv%ObsTypeId == 'DELAY-HEAD') then
        nn1 = obsrv%NodeNumber
        if (nn1 == NAMEDBOUNDFLAG) then
          n = size(obsrv%indxbnds)
          if (n > 1) then
            write (ermsg, '(4x,a,4(1x,a))') &
              'ERROR:', trim(adjustl(obsrv%ObsTypeId)), &
              'for observation', trim(adjustl(obsrv%Name)), &
              ' must be assigned to a reach with a unique boundname.'
            call store_error(ermsg)
          end if
        end if
      end if
      !
      ! -- check that node number 1 is valid; call store_error if not
      n = size(obsrv%indxbnds)
      if (obsrv%ObsTypeId /= 'DELAY-HEAD') then
        do j = 1, n
          nn1 = obsrv%indxbnds(j)
          if (nn1 < 1 .or. nn1 > this%nbound) then
            write (ermsg, '(4x,a,1x,a,1x,a,1x,i0,1x,a,1x,i0,1x,a)') &
              'ERROR:', trim(adjustl(obsrv%ObsTypeId)), &
              ' interbed must be > 0 and <=', this%nbound, &
              '(specified value is ', nn1, ')'
            call store_error(ermsg)
          end if
        end do
      end if
    end do
    if (count_errors() > 0) then
      call this%parser%StoreErrorUnit()
      call ustop()
    endif
    !
    return
  end subroutine ibc_rp_obs


  !
  ! -- Procedures related to observations (NOT type-bound)
  subroutine ibc_process_obsID(obsrv, dis, inunitobs, iout)
    ! -- This procedure is pointed to by ObsDataType%ProcesssIdPtr. It processes
    !    the ID string of an observation definition for ibc-package observations.
    ! -- dummy
    type(ObserveType),      intent(inout) :: obsrv
    class(DisBaseType), intent(in)    :: dis
    integer(I4B),            intent(in)    :: inunitobs
    integer(I4B),            intent(in)    :: iout
    ! -- local
    integer(I4B) :: nn1
    integer(I4B) :: nn2
    integer(I4B) :: icol, istart, istop
    character(len=LINELENGTH) :: strng
    character(len=LENBOUNDNAME) :: bndname
    ! formats
 30 format(i10)
    !
    strng = obsrv%IDstring
    ! -- Extract reach number from strng and store it.
    !    If 1st item is not an integer(I4B), it should be a
    !    boundary name--deal with it.
    icol = 1
    ! -- get reach number or boundary name
    call extract_idnum_or_bndname(strng, icol, istart, istop, nn1, bndname)
    if (nn1 == NAMEDBOUNDFLAG) then
      obsrv%FeatureName = bndname
    else
      if (obsrv%ObsTypeId=='DELAY-HEAD') then
        call extract_idnum_or_bndname(strng, icol, istart, istop, nn2, bndname)
        if (nn2 == NAMEDBOUNDFLAG) then
          obsrv%FeatureName = bndname
          ! -- reset nn1
          nn1 = nn2
        else
          obsrv%NodeNumber2 = nn2
        end if
      end if
    endif
    ! -- store reach number (NodeNumber)
    obsrv%NodeNumber = nn1
    !
    return
  end subroutine ibc_process_obsID


end module IbcModule
