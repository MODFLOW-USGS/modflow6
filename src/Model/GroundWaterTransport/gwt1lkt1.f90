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
  use BudgetModule, only: BudgetType
  use BudgetObjectModule, only: BudgetObjectType
  
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
    integer(I4B), dimension(:), pointer, contiguous    :: idxpakdiag => null()      !map diag position of lake in global amat
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
    type(BudgetType), pointer :: budget => NULL()
    real(DP), dimension(:), pointer, contiguous        :: qsto => null()        ! mass flux due to storage change
    real(DP), dimension(:), pointer, contiguous        :: ccterm => null()      ! mass flux required to maintain constant concentration
    type(BudgetObjectType), pointer                    :: lakbudptr => null()
    integer(I4B), pointer                              :: idxbudfjf => null()   ! index of flow ja face in lakbudptr
    integer(I4B), pointer                              :: idxbudgwf => null()   ! index of gwf terms in lakbudptr
    integer(I4B), pointer                              :: idxbudsto => null()   ! index of storage terms in lakbudptr
    integer(I4B), pointer                              :: idxbudrain => null()  ! index of rainfall terms in lakbudptr
    integer(I4B), pointer                              :: idxbudevap => null()  ! index of evaporation terms in lakbudptr
    integer(I4B), pointer                              :: idxbudroff => null()  ! index of runoff terms in lakbudptr
    integer(I4B), pointer                              :: idxbudiflw => null()  ! index of inflow terms in lakbudptr
    integer(I4B), pointer                              :: idxbudwdrl => null()  ! index of withdrawal terms in lakbudptr
    integer(I4B), pointer                              :: idxbudoutf => null()  ! index of outflow terms in lakbudptr
    integer(I4B), pointer                              :: idxbudaux => null()   ! index of auxiliary terms in lakbudptr
    integer(I4B), dimension(:), pointer, contiguous    :: idxbudssm => null()   ! flag that lakbudptr%buditem is a general solute source/sink
    integer(I4B), pointer                              :: nconcbudssm => null() ! number of concbudssm terms (columns)
    real(DP), dimension(:, : ), pointer, contiguous    :: concbudssm => null()  ! user specified concentrations for lake flow terms

    ! -- time series aware data
    type (MemoryTSType), dimension(:), pointer, contiguous :: conclak => null()  ! lake concentration
    type (MemoryTSType), dimension(:), pointer, contiguous :: concrain => null() ! rainfall concentration
    type (MemoryTSType), dimension(:), pointer, contiguous :: concevap => null() ! evaporation concentration
    type (MemoryTSType), dimension(:), pointer, contiguous :: concroff => null() ! runoff concentration
    type (MemoryTSType), dimension(:), pointer, contiguous :: conciflw => null() ! inflow concentration
    
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
    procedure, private :: lkt_check_valid
    procedure, private :: lkt_set_stressperiod
    procedure, private :: lkt_accumulate_ccterm
    procedure :: bnd_bd => lkt_bd
    procedure :: bnd_ot => lkt_ot
    procedure :: bnd_da => lkt_da
    procedure :: allocate_scalars
    procedure :: lkt_allocate_arrays
    procedure :: find_lak_package
    procedure :: lkt_solve
    procedure :: bnd_options => lkt_options
    procedure :: read_dimensions => lkt_read_dimensions
    procedure :: lkt_read_lakes
    procedure :: read_initial_attr => lkt_read_initial_attr
    procedure :: define_listlabel
    procedure :: get_volumes
    
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
    integer(I4B) :: i, n
    integer(I4B) :: jj, jglo
    integer(I4B) :: nglo
    ! -- format
! ------------------------------------------------------------------------------
    !
    ! -- Add package rows to sparse
    if (this%imatrows /= 0) then
      !
      ! -- diagonal
      do n = 1, this%nlakes
        nglo = moffset + this%dis%nodes + this%ioffset + n
        call sparse%addconnection(nglo, nglo, 1)
      end do
      !
      ! -- lake-gwf connections
      do i = 1, this%lakbudptr%budterm(this%idxbudgwf)%nlist
        n = this%lakbudptr%budterm(this%idxbudgwf)%id1(i)
        jj = this%lakbudptr%budterm(this%idxbudgwf)%id2(i)
        nglo = moffset + this%dis%nodes + this%ioffset + n
        jglo = jj + moffset
        call sparse%addconnection(nglo, jglo, 1)
        call sparse%addconnection(jglo, nglo, 1)
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
    integer(I4B) :: n, j, jj, iglo, jglo
    integer(I4B) :: ipos
    ! -- format
! ------------------------------------------------------------------------------
    !
    !
    if (this%imatrows /= 0) then
      !
      ! -- allocate pointers to global matrix
      allocate(this%idxlocnode(this%nlakes))
      allocate(this%idxpakdiag(this%nlakes))
      allocate(this%idxdglo(this%maxbound))
      allocate(this%idxoffdglo(this%maxbound))
      allocate(this%idxsymdglo(this%maxbound))
      allocate(this%idxsymoffdglo(this%maxbound))
      !
      ! -- Find the position of each connection in the global ia, ja structure
      !    and store them in idxglo.  idxglo allows this model to insert or
      !    retrieve values into or from the global A matrix
      ! -- lkt rows
      do n = 1, this%nlakes
        this%idxlocnode(n) = this%dis%nodes + this%ioffset + n
        iglo = moffset + this%dis%nodes + this%ioffset + n
        this%idxpakdiag(n) = iasln(iglo)
      end do
      do ipos = 1, this%lakbudptr%budterm(this%idxbudgwf)%nlist
        n = this%lakbudptr%budterm(this%idxbudgwf)%id1(ipos)
        j = this%lakbudptr%budterm(this%idxbudgwf)%id2(ipos)
        iglo = moffset + this%dis%nodes + this%ioffset + n
        jglo = j + moffset
        searchloop: do jj = iasln(iglo), iasln(iglo + 1) - 1
          if(jglo == jasln(jj)) then
            this%idxdglo(ipos) = iasln(iglo)
            this%idxoffdglo(ipos) = jj
            exit searchloop
          endif
        enddo searchloop
      end do
      !
      ! -- lkt contributions to gwf portion of global matrix
      do ipos = 1, this%lakbudptr%budterm(this%idxbudgwf)%nlist
        n = this%lakbudptr%budterm(this%idxbudgwf)%id1(ipos)
        j = this%lakbudptr%budterm(this%idxbudgwf)%id2(ipos)
        iglo = j + moffset
        jglo = moffset + this%dis%nodes + this%ioffset + n
        symsearchloop: do jj = iasln(iglo), iasln(iglo + 1) - 1
          if(jglo == jasln(jj)) then
            this%idxsymdglo(ipos) = iasln(iglo)
            this%idxsymoffdglo(ipos) = jj
            exit symsearchloop
          endif
        enddo symsearchloop
      end do
    else
      allocate(this%idxlocnode(0))
      allocate(this%idxpakdiag(0))
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
    character(len=*), parameter :: fmtlkt =                                    &
      "(1x,/1x,'LKT -- LAKE TRANSPORT PACKAGE, VERSION 1, 8/5/2019',           &
      &' INPUT READ FROM UNIT ', i0, //)"
! ------------------------------------------------------------------------------
    !
    ! -- Tell fmi that this package is being handled by LKT, otherwise
    !    SSM would handle the flows into GWT from this LAK
    this%fmi%iatp(this%igwflakpak) = 1
    !
    ! -- Get obs setup 
    !call this%obs%obs_ar()
    !
    ! --print a message identifying the lkt package.
    write(this%iout, fmtlkt) this%inunit
    !
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
! lkt_rp -- Read and Prepare
! Subroutine: (1) read itmp
!             (2) read new boundaries if itmp>0
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use ConstantsModule, only: LINELENGTH
    use TdisModule, only: kper, nper
    use SimModule, only: ustop, store_error, count_errors
    ! -- dummy
    class(GwtLktType), intent(inout) :: this
    ! -- local
    integer(I4B) :: ierr
    integer(I4B) :: node, n
    logical :: isfound, endOfBlock
    character(len=LINELENGTH) :: line
    character(len=LINELENGTH) :: errmsg
    integer(I4B) :: itemno
    integer(I4B) :: j
    integer(I4B) :: isfirst
    integer(I4B) :: igwfnode
    ! -- formats
    character(len=*),parameter :: fmtblkerr = &
      "('Error.  Looking for BEGIN PERIOD iper.  Found ', a, ' instead.')"
    character(len=*),parameter :: fmtlsp = &
      "(1X,/1X,'REUSING ',A,'S FROM LAST STRESS PERIOD')"
! ------------------------------------------------------------------------------
    !
    ! -- initialize flags
    isfirst = 1
    !
    ! -- set nbound to maxbound
    this%nbound = this%maxbound
    !
    ! -- Set ionper to the stress period number for which a new block of data
    !    will be read.
    if(this%inunit == 0) return
    !
    ! -- get stress period data
    if (this%ionper < kper) then
      !
      ! -- get period block
      call this%parser%GetBlock('PERIOD', isfound, ierr, &
                                supportOpenClose=.true.)
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
          write(errmsg, fmtblkerr) adjustl(trim(line))
          call store_error(errmsg)
          call this%parser%StoreErrorUnit()
          call ustop()
        end if
      endif
    end if
    !
    ! -- Read data if ionper == kper
    if(this%ionper == kper) then
      stressperiod: do
        call this%parser%GetNextLine(endOfBlock)
        if (endOfBlock) exit
        if (isfirst /= 0) then
          isfirst = 0
          if (this%iprpak /= 0) then
            write(this%iout,'(/1x,a,1x,i6,/)')                                  &
              'READING '//trim(adjustl(this%text))//' DATA FOR PERIOD', kper
            write(this%iout,'(3x,a)')  '     LKT KEYWORD AND DATA'
            write(this%iout,'(3x,78("-"))')
          end if
        end if
        itemno = this%parser%GetInteger()
        call this%parser%GetRemainingLine(line)
        call this%lkt_set_stressperiod(itemno, line)
      end do stressperiod

      if (this%iprpak /= 0) then
        write(this%iout,'(/1x,a,1x,i6,/)')                                      &
          'END OF '//trim(adjustl(this%text))//' DATA FOR PERIOD', kper
      end if
    !
    else
      write(this%iout,fmtlsp) trim(this%filtyp)
    endif
    !
    ! -- write summary of lake stress period error messages
    ierr = count_errors()
    if (ierr > 0) then
      call this%parser%StoreErrorUnit()
      call ustop()
    end if
    !
    ! -- fill arrays
    do n = 1, this%lakbudptr%budterm(this%idxbudgwf)%nlist
      igwfnode = this%lakbudptr%budterm(this%idxbudgwf)%id2(n)
      this%nodelist(n) = igwfnode
    end do
    !
    ! -- return
    return
  end subroutine lkt_rp

  subroutine lkt_set_stressperiod(this, itemno, line)
! ******************************************************************************
! lkt_set_stressperiod -- Set a stress period attribute for lakweslls(itemno)
!                         using keywords.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use TdisModule, only: kper, perlen, totimsav
    use TimeSeriesManagerModule, only: read_single_value_or_time_series
    use InputOutputModule, only: urword
    use SimModule, only: ustop, store_error, count_errors
    ! -- dummy
    class(GwtLktType),intent(inout) :: this
    integer(I4B), intent(in) :: itemno
    character (len=*), intent(in) :: line
    ! -- local
    character(len=LINELENGTH) :: text
    character(len=LINELENGTH) :: caux
    character(len=LINELENGTH) :: keyword
    character(len=LINELENGTH) :: errmsg
    character(len=LENBOUNDNAME) :: bndName
    character(len=9) :: citem
    integer(I4B) :: ierr
    integer(I4B) :: itmp
    integer(I4B) :: ival, istart, istop
    integer(I4B) :: i0
    integer(I4B) :: lloc
    integer(I4B) :: ii
    integer(I4B) :: jj
    integer(I4B) :: iaux
    real(DP) :: rval
    real(DP) :: endtim
    ! -- formats
! ------------------------------------------------------------------------------
    !
    ! STATUS <status>
    ! STAGE <stage>
    ! RAINFALL <rainfall>
    ! EVAPORATION <evaporation>
    ! RUNOFF <runoff>
    ! INFLOW <inflow>
    ! WITHDRAWAL <withdrawal>
    ! AUXILIARY <auxname> <auxval>    
    !
    ! -- Find time interval of current stress period.
    endtim = totimsav + perlen(kper)
    !
    ! -- write abs(itemno) to citem string
    itmp = ABS(itemno)
    write (citem,'(i9.9)') itmp
    !
    ! -- Assign boundary name
    if (this%inamedbound==1) then
      bndName = this%boundname(itemno)
    else
      bndName = ''
    end if
    !
    ! -- read line
    lloc = 1
    call urword(line, lloc, istart, istop, 1, ival, rval, this%iout, this%inunit)
    i0 = istart
    keyword = line(istart:istop)
    select case (line(istart:istop))
      case ('STATUS')
        ierr = this%lkt_check_valid(itemno)
        if (ierr /= 0) goto 999
        !bndName = this%boundname(itemno)
        call urword(line, lloc, istart, istop, 1, ival, rval, this%iout, this%inunit)
        text = line(istart:istop)
        this%status(itmp) = text(1:8)
        if (text == 'CONSTANT') then
          this%iboundpak(itmp) = -1
        else if (text == 'INACTIVE') then
          this%iboundpak(itmp) = 0
        else if (text == 'ACTIVE') then
          this%iboundpak(itmp) = 1
        else
          write(errmsg,'(4x,a,a)') &
            '****ERROR. UNKNOWN '//trim(this%text)//' LAK STATUS KEYWORD: ', &
            text
          call store_error(errmsg)
        end if
      case ('CONCENTRATION')
        ierr = this%lkt_check_valid(itemno)
        if (ierr /= 0) goto 999
        call urword(line, lloc, istart, istop, 0, ival, rval, this%iout, this%inunit)
        text = line(istart:istop)
        jj = 1    ! For lake concentration
        call read_single_value_or_time_series(text, &
                                              this%conclak(itmp)%value, &
                                              this%conclak(itmp)%name, &
                                              endtim,  &
                                              this%name, 'BND', this%TsManager, &
                                              this%iprpak, itmp, jj, 'CONCENTRATION', &
                                              bndName, this%inunit)
      case ('RAINFALL')
        ierr = this%lkt_check_valid(itemno)
        if (ierr /= 0) goto 999
        call urword(line, lloc, istart, istop, 0, ival, rval, this%iout, this%inunit)
        text = line(istart:istop)
        jj = 1    ! For RAINFALL
        call read_single_value_or_time_series(text, &
                                              this%concrain(itmp)%value, &
                                              this%concrain(itmp)%name, &
                                              endtim,  &
                                              this%name, 'BND', this%TsManager, &
                                              this%iprpak, itmp, jj, 'RAINFALL', &
                                              bndName, this%inunit)
      case ('EVAPORATION')
        ierr = this%lkt_check_valid(itemno)
        if (ierr /= 0) goto 999
        call urword(line, lloc, istart, istop, 0, ival, rval, this%iout, this%inunit)
        text = line(istart:istop)
        jj = 1    ! For EVAPORATION
        call read_single_value_or_time_series(text, &
                                              this%concevap(itmp)%value, &
                                              this%concevap(itmp)%name, &
                                              endtim,  &
                                              this%name, 'BND', this%TsManager, &
                                              this%iprpak, itmp, jj, 'EVAPORATION', &
                                              bndName, this%inunit)
      case ('RUNOFF')
        ierr = this%lkt_check_valid(itemno)
        if (ierr /= 0) goto 999
        call urword(line, lloc, istart, istop, 0, ival, rval, this%iout, this%inunit)
        text = line(istart:istop)
        jj = 1    ! For RUNOFF
        call read_single_value_or_time_series(text, &
                                              this%concroff(itmp)%value, &
                                              this%concroff(itmp)%name, &
                                              endtim,  &
                                              this%name, 'BND', this%TsManager, &
                                              this%iprpak, itmp, jj, 'RUNOFF', &
                                              bndName, this%inunit)
      case ('INFLOW')
        ierr = this%lkt_check_valid(itemno)
        if (ierr /= 0) goto 999
        call urword(line, lloc, istart, istop, 0, ival, rval, this%iout, this%inunit)
        text = line(istart:istop)
        jj = 1    ! For INFLOW
        call read_single_value_or_time_series(text, &
                                              this%conciflw(itmp)%value, &
                                              this%conciflw(itmp)%name, &
                                              endtim,  &
                                              this%name, 'BND', this%TsManager, &
                                              this%iprpak, itmp, jj, 'INFLOW', &
                                              bndName, this%inunit)
      case ('AUXILIARY')
        ierr = this%lkt_check_valid(itemno)
        if (ierr /= 0) goto 999
        !bndName = this%boundname(itemno)
        call urword(line, lloc, istart, istop, 1, ival, rval, this%iout, this%inunit)
        caux = line(istart:istop)
        do iaux = 1, this%naux
          if (trim(adjustl(caux)) /= trim(adjustl(this%auxname(iaux)))) cycle
          call urword(line, lloc, istart, istop, 0, ival, rval, this%iout, this%inunit)
          text = line(istart:istop)
          jj = 1 !iaux
          ii = (itmp-1) * this%naux + iaux
          !call read_single_value_or_time_series(text, &
          !                                      this%lauxvar(ii)%value, &
          !                                      this%lauxvar(ii)%name, &
          !                                      endtim,  &
          !                                      this%Name, 'AUX', this%TsManager, &
          !                                      this%iprpak, itmp, jj, &
          !                                      this%auxname(iaux), bndName, &
          !                                      this%inunit)
          exit
        end do
      case default
        write(errmsg,'(4x,a,a)') &
          '****ERROR. UNKNOWN '//trim(this%text)//' LAK DATA KEYWORD: ', &
                                  line(istart:istop)
        call store_error(errmsg)
        call ustop()
    end select
    !
    ! -- terminate if any errors were detected
999 if (count_errors() > 0) then
      call this%parser%StoreErrorUnit()
      call ustop()
    end if
    !
    ! -- write keyword data to output file
    if (this%iprpak /= 0) then
      write (this%iout, '(3x,i10,1x,a)') itmp, line(i0:istop)
    end if
    !
    ! -- return
    return
  end subroutine lkt_set_stressperiod

  function lkt_check_valid(this, itemno) result(ierr)
! ******************************************************************************
!  lkt_check_valid -- Determine if a valid lake or outlet number has been
!                     specified.
! ******************************************************************************
    use SimModule, only: ustop, store_error
    ! -- return
    integer(I4B) :: ierr
    ! -- dummy
    class(GwtLktType),intent(inout) :: this
    integer(I4B), intent(in) :: itemno
    ! -- local
    character(len=LINELENGTH) :: errmsg
    ! -- formats
! ------------------------------------------------------------------------------
    ierr = 0
    if (itemno < 1 .or. itemno > this%nlakes) then
      write(errmsg,'(4x,a,1x,i6,1x,a,1x,i6)') &
        '****ERROR. LAKENO ', itemno, 'MUST BE > 0 and <= ', this%nlakes
      call store_error(errmsg)
      ierr = 1
    end if
  end function lkt_check_valid


  subroutine lkt_rp2(this)
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
    integer(I4B) :: i, igwfnode
! ------------------------------------------------------------------------------
    !
    ! -- TODO: read period data
    do i = 1, this%lakbudptr%budterm(this%idxbudgwf)%nlist
      igwfnode = this%lakbudptr%budterm(this%idxbudgwf)%id2(i)
      this%nodelist(i) = igwfnode
    end do
    !
    ! -- Return
    return
  end subroutine lkt_rp2

  subroutine lkt_ad(this)
! ******************************************************************************
! lkt_ad -- Add package connection to matrix
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(GwtLktType) :: this
    ! -- local
    integer(I4B) :: n
    integer(I4B) :: j, iaux, ii
! ------------------------------------------------------------------------------
    !
    ! -- Advance the time series
    call this%TsManager%ad()
    !
    ! -- update auxiliary variables by copying from the derived-type time
    !    series variable into the bndpackage auxvar variable so that this
    !    information is properly written to the GWF budget file
    !if (this%naux > 0) then
    !  do n = 1, this%nlakes
    !    do j = this%idxlakeconn(n), this%idxlakeconn(n + 1) - 1
    !      do iaux = 1, this%naux
    !        ii = (n - 1) * this%naux + iaux
    !        this%auxvar(iaux, j) = this%lauxvar(ii)%value
    !      end do
    !    end do
    !  end do
    !end if
    !
    ! -- copy xnew into xold and set xnewpak to stage%value for
    !    constant stage lakes
    do n = 1, this%nlakes
      this%xoldpak(n) = this%xnewpak(n)
      if (this%iboundpak(n) < 0) then
        this%xnewpak(n) = this%conclak(n)%value
      end if
    end do
    !
    ! -- pakmvrobj ad
    !if (this%imover == 1) then
    !  call this%pakmvrobj%ad()
    !end if
    !
    ! -- For each observation, push simulated value and corresponding
    !    simulation time from "current" to "preceding" and reset
    !    "current" value.
    !call this%obs%obs_ad()
    !
    ! -- return
    return
  end subroutine lkt_ad

  subroutine lkt_ad2(this)
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
  end subroutine lkt_ad2

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
    integer(I4B) :: j, igwfnode, idiag
! ------------------------------------------------------------------------------
    !
    ! -- solve for concentration in the lakes
    call this%lkt_solve()
    !
    ! -- add hcof and rhs terms (from lkt_solve) to the gwf matrix
    do j = 1, this%lakbudptr%budterm(this%idxbudgwf)%nlist
      igwfnode = this%lakbudptr%budterm(this%idxbudgwf)%id2(j)
      if (this%ibound(igwfnode) < 1) cycle
      idiag = idxglo(ia(igwfnode))
      amatsln(idiag) = amatsln(idiag) + this%hcof(j)
      rhs(igwfnode) = rhs(igwfnode) + this%rhs(j)
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
    integer(I4B) :: iloc
    integer(I4B) :: iposd, iposoffd
    integer(I4B) :: ipossymd, ipossymoffd
    real(DP) :: cold
    real(DP) :: qbnd
    real(DP) :: v0
    real(DP) :: v1
    real(DP) :: omega
    real(DP) :: ctmp
! ------------------------------------------------------------------------------
    !
    ! -- Add coefficients for LKT and GWF connections (qbnd positive into lake)
    do n = 1, this%nlakes
      cold  = this%xoldpak(n)
      iloc = this%idxlocnode(n)
      iposd = this%idxpakdiag(n)
      !
      ! -- mass storage in lak
      call this%get_volumes(n, v1, v0, delt)
      amatsln(iposd) = amatsln(iposd) - v1 / delt
      rhs(iloc) = rhs(iloc) - cold * v0 / delt
      !
      ! -- todo: add or remove mass directly to and from lak (precip, evap, runoff, etc.)
      ! -- add em here
      
    end do
    !
    ! -- add rainfall contribution
    if (this%idxbudrain /= 0) then
      do j = 1, this%lakbudptr%budterm(this%idxbudrain)%nlist
        n = this%lakbudptr%budterm(this%idxbudrain)%id1(j)
        iloc = this%idxlocnode(n)
        qbnd = this%lakbudptr%budterm(this%idxbudrain)%flow(j)
        ctmp = this%concrain(n)%value
        rhs(iloc) = rhs(iloc) - ctmp * qbnd
      end do
    end if
    !
    ! -- add evaporation contribution
    if (this%idxbudevap /= 0) then
      do j = 1, this%lakbudptr%budterm(this%idxbudevap)%nlist
        n = this%lakbudptr%budterm(this%idxbudevap)%id1(j)
        iloc = this%idxlocnode(n)
        iposd = this%idxpakdiag(n)
        qbnd = this%lakbudptr%budterm(this%idxbudevap)%flow(j)
        ctmp = this%concevap(n)%value
        if (this%xnewpak(n) < ctmp) then
          omega = DONE
        else
          omega = DZERO
        end if
        amatsln(iposd) = amatsln(iposd) + omega * qbnd
        rhs(iloc) = rhs(iloc) - (DONE - omega) * qbnd * ctmp
      end do
    end if
    !
    ! -- add runoff contribution
    if (this%idxbudroff /= 0) then
      do j = 1, this%lakbudptr%budterm(this%idxbudroff)%nlist
        n = this%lakbudptr%budterm(this%idxbudroff)%id1(j)
        iloc = this%idxlocnode(n)
        qbnd = this%lakbudptr%budterm(this%idxbudroff)%flow(j)
        ctmp = this%concroff(n)%value
        rhs(iloc) = rhs(iloc) - ctmp * qbnd
      end do
    end if
    !
    ! -- add inflow contribution
    if (this%idxbudiflw /= 0) then
      do j = 1, this%lakbudptr%budterm(this%idxbudiflw)%nlist
        n = this%lakbudptr%budterm(this%idxbudiflw)%id1(j)
        iloc = this%idxlocnode(n)
        qbnd = this%lakbudptr%budterm(this%idxbudroff)%flow(j)
        ctmp = this%conciflw(n)%value
        rhs(iloc) = rhs(iloc) - ctmp * qbnd
      end do
    end if
    !
    ! -- add withdrawal contribution
    if (this%idxbudwdrl /= 0) then
      do j = 1, this%lakbudptr%budterm(this%idxbudwdrl)%nlist
        n = this%lakbudptr%budterm(this%idxbudwdrl)%id1(j)
        iposd = this%idxpakdiag(n)
        qbnd = this%lakbudptr%budterm(this%idxbudwdrl)%flow(j)
        amatsln(iposd) = amatsln(iposd) + qbnd
      end do
    end if
    !
    ! -- add outflow contribution
    if (this%idxbudoutf /= 0) then
      do j = 1, this%lakbudptr%budterm(this%idxbudoutf)%nlist
        n = this%lakbudptr%budterm(this%idxbudoutf)%id1(j)
        iposd = this%idxpakdiag(n)
        qbnd = this%lakbudptr%budterm(this%idxbudoutf)%flow(j)
        amatsln(iposd) = amatsln(iposd) + qbnd
      end do
    end if
    !
    ! -- go through each lak-gwf connection
    do j = 1, this%lakbudptr%budterm(this%idxbudgwf)%nlist
      !
      ! -- set n to lake number and process if active lake
      n = this%lakbudptr%budterm(this%idxbudgwf)%id1(j)
      if (this%iboundpak(n) /= 0) then
        !
        ! -- set acoef and rhs to negative so they are relative to lkt and not gwt
        qbnd = this%lakbudptr%budterm(this%idxbudgwf)%flow(j)
        omega = DZERO
        if (qbnd < DZERO) omega = DONE
        !
        ! -- add to lkt row
        iposd = this%idxdglo(j)
        iposoffd = this%idxoffdglo(j)
        amatsln(iposd) = amatsln(iposd) + omega * qbnd
        amatsln(iposoffd) = amatsln(iposoffd) + (DONE - omega) * qbnd
        !
        ! -- add to gwf row for lkt connection
        ipossymd = this%idxsymdglo(j)
        ipossymoffd = this%idxsymoffdglo(j)
        amatsln(ipossymd) = amatsln(ipossymd) - (DONE - omega) * qbnd
        amatsln(ipossymoffd) = amatsln(ipossymoffd) - omega * qbnd
      end if    
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
    real(DP) :: qbnd
    real(DP) :: omega
! ------------------------------------------------------------------------------
    !
    ! -- Calculate hcof and rhs terms so GWF exchanges are calculated correctly
    ! -- go through each lak-gwf connection and calculate
    !    rhs and hcof terms for gwt matrix rows
    do j = 1, this%lakbudptr%budterm(this%idxbudgwf)%nlist
      n = this%lakbudptr%budterm(this%idxbudgwf)%id1(j)
      this%hcof(j) = DZERO
      this%rhs(j) = DZERO
      if (this%iboundpak(n) /= 0) then
        qbnd = this%lakbudptr%budterm(this%idxbudgwf)%flow(j)
        omega = DZERO
        if (qbnd < DZERO) omega = DONE
        this%hcof(j) = - (DONE - omega) * qbnd
        this%rhs(j) = omega * qbnd * this%xnewpak(n)
      endif
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
    real(DP) :: v0, v1
    real(DP) :: qbnd, ctmp, omega
    real(DP) :: ccratin, ccratout
    ! -- for observations
    integer(I4B) :: iprobslocal
    ! -- formats
! ------------------------------------------------------------------------------
    !
    ! -- Solve the lak concentrations again or update the lake hcof 
    !    and rhs terms
    if (this%imatrows == 0) then
      call this%lkt_solve()
    else
      call this%lkt_cfupdate()
    end if
    !
    ! -- Suppress saving of simulated values; they
    !    will be saved at end of this procedure.
    iprobslocal = 0
    !
    ! -- call base functionality in bnd_bd
    call this%BndType%bnd_bd(x, idvfl, icbcfl, ibudfl, icbcun, iprobslocal,    &
                             isuppress_output, model_budget)
    !
    ! -- lak budget routines (start by resetting)
    call this%budget%reset()
    !
    ! -- initialize ccterm, which is used to sum up all mass flows
    !    into a constant concentration cell
    ccratin = DZERO
    ccratout = DZERO
    do n = 1, this%nlakes
      this%ccterm(n) = DZERO
    end do
    !
    ! -- gwf term
    ratein = DZERO
    rateout = DZERO
    do j = 1, this%lakbudptr%budterm(this%idxbudgwf)%nlist
      n = this%lakbudptr%budterm(this%idxbudgwf)%id1(j)
      rrate = DZERO
      if (this%iboundpak(n) > 0) then
        igwfnode = this%lakbudptr%budterm(this%idxbudgwf)%id2(j)
        rrate = this%hcof(j) * x(igwfnode) - this%rhs(j)
        rrate = -rrate  ! flip sign so relative to lake
      end if
      call this%lkt_accumulate_ccterm(n, rrate, ccratin, ccratout)
      if(rrate < DZERO) then
        rateout = rateout - rrate
      else
        ratein = ratein + rrate
      endif
    end do
    !
    ! -- check the lkt-gwt rates into the lkt package budget
    call this%budget%addentry(ratein, rateout, delt,  &
                              this%clktbudget(1), isuppress_output)
    !
    ! -- storage term
    ratein = DZERO
    rateout = DZERO
    do n = 1, this%nlakes
      rrate = DZERO
      if (this%iboundpak(n) > 0) then
        call this%get_volumes(n, v1, v0, delt)
        call lkt_calc_qsto(v0, v1, this%xoldpak(n), this%xnewpak(n), delt,     &
                          rhs, hcof, rrate)
      end if
      this%qsto(n) = rrate
      call this%lkt_accumulate_ccterm(n, rrate, ccratin, ccratout)
      if(rrate < DZERO) then
        rateout = rateout - rrate
      else
        ratein = ratein + rrate
      endif
    end do
    call this%budget%addentry(ratein, rateout, delt,  &
                              this%clktbudget(2), isuppress_output)

    !
    ! -- rainfall contribution
    ! -- todo: need to accumulate chterm for constant concentration lakes
    ratein = DZERO
    rateout = DZERO
    if (this%idxbudrain /= 0) then
      do j = 1, this%lakbudptr%budterm(this%idxbudrain)%nlist
        n = this%lakbudptr%budterm(this%idxbudrain)%id1(j)
        qbnd = this%lakbudptr%budterm(this%idxbudrain)%flow(j)
        ctmp = this%concrain(n)%value
        rrate = ctmp * qbnd
        call this%lkt_accumulate_ccterm(n, rrate, ccratin, ccratout)
        if(rrate < DZERO) then
          rateout = rateout - rrate
        else
          ratein = ratein + rrate
        endif
      end do
    end if
    call this%budget%addentry(ratein, rateout, delt,  &
                              this%clktbudget(4), isuppress_output)
    !
    ! -- evaporation contribution
    ! -- todo: need to accumulate chterm for constant concentration lakes
    ratein = DZERO
    rateout = DZERO
    if (this%idxbudevap /= 0) then
      do j = 1, this%lakbudptr%budterm(this%idxbudevap)%nlist
        n = this%lakbudptr%budterm(this%idxbudevap)%id1(j)
        qbnd = this%lakbudptr%budterm(this%idxbudevap)%flow(j)
        ctmp = this%concevap(n)%value
        if (this%xnewpak(n) < ctmp) then
          omega = DONE
        else
          omega = DZERO
        end if
        rrate = omega * qbnd * this%xnewpak(n) + (DONE - omega) * qbnd * ctmp
        call this%lkt_accumulate_ccterm(n, rrate, ccratin, ccratout)
        if(rrate < DZERO) then
          rateout = rateout - rrate
        else
          ratein = ratein + rrate
        endif
      end do
    end if
    call this%budget%addentry(ratein, rateout, delt,  &
                              this%clktbudget(5), isuppress_output)
    !
    ! -- runoff contribution
    ! -- todo: need to accumulate chterm for constant concentration lakes
    ratein = DZERO
    rateout = DZERO
    if (this%idxbudroff /= 0) then
      do j = 1, this%lakbudptr%budterm(this%idxbudroff)%nlist
        n = this%lakbudptr%budterm(this%idxbudroff)%id1(j)
        qbnd = this%lakbudptr%budterm(this%idxbudroff)%flow(j)
        ctmp = this%concroff(n)%value
        rrate = ctmp * qbnd
        call this%lkt_accumulate_ccterm(n, rrate, ccratin, ccratout)
        if(rrate < DZERO) then
          rateout = rateout - rrate
        else
          ratein = ratein + rrate
        endif
      end do
    end if
    call this%budget%addentry(ratein, rateout, delt,  &
                              this%clktbudget(6), isuppress_output)
    !
    ! -- inflow contribution
    ! -- todo: need to accumulate chterm for constant concentration lakes
    ratein = DZERO
    rateout = DZERO
    if (this%idxbudiflw /= 0) then
      do j = 1, this%lakbudptr%budterm(this%idxbudiflw)%nlist
        n = this%lakbudptr%budterm(this%idxbudiflw)%id1(j)
        qbnd = this%lakbudptr%budterm(this%idxbudiflw)%flow(j)
        ctmp = this%conciflw(n)%value
        rrate = ctmp * qbnd
        call this%lkt_accumulate_ccterm(n, rrate, ccratin, ccratout)
        if(rrate < DZERO) then
          rateout = rateout - rrate
        else
          ratein = ratein + rrate
        endif
      end do
    end if
    call this%budget%addentry(ratein, rateout, delt,  &
                              this%clktbudget(7), isuppress_output)
    !
    ! -- withdrawal contribution
    ! -- todo: need to accumulate chterm for constant concentration lakes
    ratein = DZERO
    rateout = DZERO
    if (this%idxbudwdrl /= 0) then
      do j = 1, this%lakbudptr%budterm(this%idxbudwdrl)%nlist
        n = this%lakbudptr%budterm(this%idxbudwdrl)%id1(j)
        qbnd = this%lakbudptr%budterm(this%idxbudwdrl)%flow(j)
        ctmp = this%xnewpak(n)
        rrate = qbnd * this%xnewpak(n)
        call this%lkt_accumulate_ccterm(n, rrate, ccratin, ccratout)
        if(rrate < DZERO) then
          rateout = rateout - rrate
        else
          ratein = ratein + rrate
        endif
      end do
    end if
    call this%budget%addentry(ratein, rateout, delt,  &
                              this%clktbudget(8), isuppress_output)
    !
    ! -- outflow contribution
    ! -- todo: need to accumulate chterm for constant concentration lakes
    ratein = DZERO
    rateout = DZERO
    if (this%idxbudoutf /= 0) then
      do j = 1, this%lakbudptr%budterm(this%idxbudoutf)%nlist
        n = this%lakbudptr%budterm(this%idxbudoutf)%id1(j)
        qbnd = this%lakbudptr%budterm(this%idxbudoutf)%flow(j)
        ctmp = this%xnewpak(n)
        rrate = qbnd * this%xnewpak(n)
        call this%lkt_accumulate_ccterm(n, rrate, ccratin, ccratout)
        if(rrate < DZERO) then
          rateout = rateout - rrate
        else
          ratein = ratein + rrate
        endif
      end do
    end if
    call this%budget%addentry(ratein, rateout, delt,  &
                              this%clktbudget(9), isuppress_output)
    !
    ! -- add entry for constant concentration terms
    call this%budget%addentry(ccratin, ccratout, delt,  &
                              this%clktbudget(3), isuppress_output)
    
    !
    ! -- todo: implement the budobj for printing the budget
    !    and writing the individual budget terms to an lkt binary bud file
    
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
    ! -- write lake concentration
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
    ! -- Output lkt budget
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
    call mem_allocate(this%idxbudfjf, 'IDXBUDFJF', this%origin)
    call mem_allocate(this%idxbudgwf, 'IDXBUDGWF', this%origin)
    call mem_allocate(this%idxbudsto, 'IDXBUDSTO', this%origin)
    call mem_allocate(this%idxbudrain, 'IDXBUDRAIN', this%origin)
    call mem_allocate(this%idxbudevap, 'IDXBUDEVAP', this%origin)
    call mem_allocate(this%idxbudroff, 'IDXBUDROFF', this%origin)
    call mem_allocate(this%idxbudiflw, 'IDXBUDIFLW', this%origin)
    call mem_allocate(this%idxbudwdrl, 'IDXBUDWDRL', this%origin)
    call mem_allocate(this%idxbudoutf, 'IDXBUDOUTF', this%origin)
    call mem_allocate(this%idxbudaux, 'IDXBUDAUX', this%origin)
    call mem_allocate(this%nconcbudssm, 'NCONCBUDSSM', this%origin)
    ! 
    ! -- Initialize
    this%imatrows = 1
    this%iprconc = 0
    this%iconcout = 0
    this%ibudgetout = 0
    this%igwflakpak = 0
    this%nlakes = 0
    this%bditems = 9
    this%idxbudfjf = 0
    this%idxbudgwf = 0
    this%idxbudsto = 0
    this%idxbudrain = 0
    this%idxbudevap = 0
    this%idxbudroff = 0
    this%idxbudiflw = 0
    this%idxbudwdrl = 0
    this%idxbudoutf = 0
    this%idxbudaux = 0
    this%nconcbudssm = 0
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
    ! -- time series
    call mem_allocate(this%conclak, this%nlakes, 'CONCLAK', this%origin)
    call mem_allocate(this%concrain, this%nlakes, 'CONCRAIN', this%origin)
    call mem_allocate(this%concevap, this%nlakes, 'CONCEVAP', this%origin)
    call mem_allocate(this%concroff, this%nlakes, 'CONCROFF', this%origin)
    call mem_allocate(this%conciflw, this%nlakes, 'CONCIFLW', this%origin)
    !
    ! -- budget terms
    call mem_allocate(this%qsto, this%nlakes, 'QSTO', this%origin)
    call mem_allocate(this%ccterm, this%nlakes, 'CCTERM', this%origin)
    !
    ! -- concentration for budget terms
    call mem_allocate(this%concbudssm, this%nconcbudssm, this%nlakes, &
      'CONCBUDSSM', this%origin)
    !
    ! -- Initialize
    !
    !-- fill clktbudget
    this%clktbudget(1) = '             GWF'
    this%clktbudget(2) = '         STORAGE'
    this%clktbudget(3) = '        CONSTANT'
    this%clktbudget(4) = '        RAINFALL'
    this%clktbudget(5) = '     EVAPORATION'
    this%clktbudget(6) = '          RUNOFF'
    this%clktbudget(7) = '      EXT-INFLOW'
    this%clktbudget(8) = '      WITHDRAWAL'
    this%clktbudget(9) = '     EXT-OUTFLOW'
    !
    ! -- initialize arrays
    do n = 1, this%nlakes
      this%status(n) = 'ACTIVE'
      this%qsto(n) = DZERO
      this%ccterm(n) = DZERO
      this%concbudssm(:, n) = DZERO
    end do
    !
    ! -- Return
    return
  end subroutine lkt_allocate_arrays
  
  subroutine lkt_da(this)
! ******************************************************************************
! lkt_da
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryManagerModule, only: mem_deallocate
    ! -- dummy
    class(GwtLktType) :: this
    ! -- local
! ------------------------------------------------------------------------------
    !
    ! -- deallocate arrays
    call mem_deallocate(this%dbuff)
    call mem_deallocate(this%qsto)
    call mem_deallocate(this%ccterm)
    call mem_deallocate(this%strt)
    call mem_deallocate(this%lauxvar)
    call mem_deallocate(this%xoldpak)
    if (this%imatrows == 0) then
      call mem_deallocate(this%iboundpak)
      call mem_deallocate(this%xnewpak)
    end if
    call mem_deallocate(this%concbudssm)
    call mem_deallocate(this%conclak)
    call mem_deallocate(this%concrain)
    call mem_deallocate(this%concevap)
    call mem_deallocate(this%concroff)
    call mem_deallocate(this%conciflw)
    deallocate(this%clktbudget)
    deallocate(this%status)
    deallocate(this%lakename)
    !
    ! -- index pointers
    deallocate(this%idxlocnode)
    deallocate(this%idxpakdiag)
    deallocate(this%idxdglo)
    deallocate(this%idxoffdglo)
    deallocate(this%idxsymdglo)
    deallocate(this%idxsymoffdglo)
    !
    ! -- deallocate scalars
    call mem_deallocate(this%imatrows)
    call mem_deallocate(this%iprconc)
    call mem_deallocate(this%iconcout)
    call mem_deallocate(this%ibudgetout)
    call mem_deallocate(this%igwflakpak)
    call mem_deallocate(this%nlakes)
    call mem_deallocate(this%bditems)
    call mem_deallocate(this%idxbudfjf)
    call mem_deallocate(this%idxbudgwf)
    call mem_deallocate(this%idxbudsto)
    call mem_deallocate(this%idxbudrain)
    call mem_deallocate(this%idxbudevap)
    call mem_deallocate(this%idxbudroff)
    call mem_deallocate(this%idxbudiflw)
    call mem_deallocate(this%idxbudwdrl)
    call mem_deallocate(this%idxbudoutf)
    call mem_deallocate(this%idxbudaux)
    call mem_deallocate(this%idxbudssm)
    call mem_deallocate(this%nconcbudssm)
    !
    ! -- deallocate scalars in NumericalPackageType
    call this%BndType%bnd_da()
    !
    ! -- Return
    return
  end subroutine lkt_da

  subroutine find_lak_package(this)
! ******************************************************************************
! find corresponding lak package
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryManagerModule, only: mem_allocate
    use SimModule, only: ustop, store_error
    ! -- dummy
    class(GwtLktType) :: this
    ! -- local
    character(len=LINELENGTH) :: errmsg
    class(BndType), pointer :: packobj
    integer(I4B) :: ip, icount
    integer(I4B) :: nbudterm
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
            this%lakbudptr => packobj%budobj
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
    ! -- allocate space for idxbudssm, which indicates whether this is a 
    !    special budget term or one that is a general source and sink
    nbudterm = this%lakbudptr%nbudterm
    call mem_allocate(this%idxbudssm, nbudterm, 'IDXBUDSSM', this%origin)
    !
    ! -- Process budget terms and identify special budget terms
    write(this%iout, '(/, a, a)') &
      'PROCESSING LKT INFORMATION FOR ', this%name
    write(this%iout, '(a)') '  IDENTIFYING FLOW TERMS IN LAK PACKAGE'
    write(this%iout, '(a, i0)') &
      '  NUMBER OF LAKES = ', this%lakbudptr%ncv
    icount = 1
    do ip = 1, this%lakbudptr%nbudterm
      select case(trim(adjustl(this%lakbudptr%budterm(ip)%flowtype)))
      case('FLOW-JA-FACE')
        this%idxbudfjf = ip
        this%idxbudssm(ip) = 0
      case('GWF')
        this%idxbudgwf = ip
        this%idxbudssm(ip) = 0
      case('STORAGE')
        this%idxbudsto = ip
        this%idxbudssm(ip) = 0
      case('RAINFALL')
        this%idxbudrain = ip
        this%idxbudssm(ip) = 0
      case('EVAPORATION')
        this%idxbudevap = ip
        this%idxbudssm(ip) = 0
      case('RUNOFF')
        this%idxbudroff = ip
        this%idxbudssm(ip) = 0
      case('EXT-INFLOW')
        this%idxbudiflw = ip
        this%idxbudssm(ip) = 0
      case('WITHDRAWAL')
        this%idxbudwdrl = ip
        this%idxbudssm(ip) = 0
      case('EXT-OUTFLOW')
        this%idxbudoutf = ip
        this%idxbudssm(ip) = 0
      case('AUXILIARY')
        this%idxbudaux = ip
        this%idxbudssm(ip) = 0
      case default
        !
        ! -- set idxbudssm equal to a column index for where the concentrations
        !    are stored in the concbud(nbudssm, nlake) array
        this%idxbudssm(ip) = icount
        icount = icount + 1
      end select
      write(this%iout, '(a, i0, " = ", a,/, a, i0)') &
        '  TERM ', ip, trim(adjustl(this%lakbudptr%budterm(ip)%flowtype)), &
        '   MAX NO. OF ENTRIES = ', this%lakbudptr%budterm(ip)%maxlist
    end do
    write(this%iout, '(a, //)') 'DONE PROCESSING LKT INFORMATION'
    !
    ! -- Return
    return
  end subroutine find_lak_package

  subroutine  lkt_options(this, option, found)
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
! lkt_read_dimensions -- Determine dimensions for this package
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
    integer(I4B) :: ierr
    ! -- format
! ------------------------------------------------------------------------------
    !
    ! -- Set a pointer to the GWF LAK Package budobj
    call this%find_lak_package()
    !
    ! -- Set dimensions from the GWF LAK package
    this%nlakes = this%lakbudptr%ncv
    this%maxbound = this%lakbudptr%budterm(this%idxbudgwf)%maxlist
    this%nbound = this%maxbound
    write(this%iout, '(a, a)') 'SETTING DIMENSIONS FOR LKT PACKAGE ', this%name
    write(this%iout,'(2x,a,i0)')'NLAKES = ', this%nlakes
    write(this%iout,'(2x,a,i0)')'MAXBOUND = ', this%maxbound
    write(this%iout,'(2x,a,i0)')'NBOUND = ', this%nbound
    if (this%imatrows /= 0) then
      this%npakeq = this%nlakes
      write(this%iout,'(2x,a)') 'LKT SOLVED AS PART OF GWT MATRIX EQUATIONS'
    else
      write(this%iout,'(2x,a)') 'LKT SOLVED SEPARATELY FROM GWT MATRIX EQUATIONS '
    end if
    write(this%iout, '(a, //)') 'DONE SETTING LKT DIMENSIONS'
    !
    ! -- Check for errors
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
! lkt_read_lakes -- Read feature infromation for this package
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
! ------------------------------------------------------------------------------
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
    ! -- get packagedata block
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
    ! -- todo: this should be a time series?
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

      ! -- todo: read aux
      
      ! -- todo: read boundname
      
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
      do j = 1, this%lakbudptr%budterm(this%idxbudgwf)%nlist
        n = this%lakbudptr%budterm(this%idxbudgwf)%id1(j)
        this%boundname(j) = this%lakename(n)
      end do
    end if
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
    real(DP) :: c1, ctmp, qbnd, v0, v1, omega
! ------------------------------------------------------------------------------
    !
    !
    ! -- first initialize dbuff with initial solute mass in lake
    do n = 1, this%nlakes
      call this%get_volumes(n, v1, v0, delt)
      c1 = this%xoldpak(n) * v0
      this%dbuff(n) = c1
    end do
    !
    ! -- add rainfall contribution
    if (this%idxbudrain /= 0) then
      do j = 1, this%lakbudptr%budterm(this%idxbudrain)%nlist
        n = this%lakbudptr%budterm(this%idxbudrain)%id1(j)
        qbnd = this%lakbudptr%budterm(this%idxbudrain)%flow(j)
        ctmp = this%concrain(n)%value
        this%dbuff(n) = this%dbuff(n) + ctmp * qbnd
      end do
    end if
    !
    ! -- add evaporation contribution
    if (this%idxbudevap /= 0) then
      do j = 1, this%lakbudptr%budterm(this%idxbudevap)%nlist
        n = this%lakbudptr%budterm(this%idxbudevap)%id1(j)
        qbnd = this%lakbudptr%budterm(this%idxbudevap)%flow(j)
        ctmp = this%concevap(n)%value
        if (this%xnewpak(n) < ctmp) then
          omega = DONE
        else
          omega = DZERO
        end if
        c1 = omega * qbnd * this%xnewpak(n) + &
             (DONE - omega) * qbnd * ctmp
        this%dbuff(n) = this%dbuff(n) + c1
      end do
    end if
    !
    ! -- add runoff contribution
    if (this%idxbudroff /= 0) then
      do j = 1, this%lakbudptr%budterm(this%idxbudroff)%nlist
        n = this%lakbudptr%budterm(this%idxbudroff)%id1(j)
        qbnd = this%lakbudptr%budterm(this%idxbudroff)%flow(j)
        ctmp = this%concroff(n)%value
        this%dbuff(n) = this%dbuff(n) + ctmp * qbnd
      end do
    end if
    !
    ! -- add inflow contribution
    if (this%idxbudiflw /= 0) then
      do j = 1, this%lakbudptr%budterm(this%idxbudiflw)%nlist
        n = this%lakbudptr%budterm(this%idxbudiflw)%id1(j)
        qbnd = this%lakbudptr%budterm(this%idxbudiflw)%flow(j)
        ctmp = this%conciflw(n)%value
        this%dbuff(n) = this%dbuff(n) + ctmp * qbnd
      end do
    end if
    !
    ! -- add withdrawal contribution
    if (this%idxbudwdrl /= 0) then
      do j = 1, this%lakbudptr%budterm(this%idxbudwdrl)%nlist
        n = this%lakbudptr%budterm(this%idxbudwdrl)%id1(j)
        qbnd = this%lakbudptr%budterm(this%idxbudwdrl)%flow(j)
        ctmp = this%xnewpak(n)
        c1 = qbnd * ctmp
        this%dbuff(n) = this%dbuff(n) + c1
      end do
    end if
    !
    ! -- add outflow contribution
    if (this%idxbudoutf /= 0) then
      do j = 1, this%lakbudptr%budterm(this%idxbudoutf)%nlist
        n = this%lakbudptr%budterm(this%idxbudoutf)%id1(j)
        qbnd = this%lakbudptr%budterm(this%idxbudoutf)%flow(j)
        ctmp = this%xnewpak(n)
        c1 = qbnd * ctmp
        this%dbuff(n) = this%dbuff(n) + c1
      end do
    end if
    !
    ! -- go through each gwf connection and accumulate 
    !    total mass in dbuff mass
    do j = 1, this%lakbudptr%budterm(this%idxbudgwf)%nlist
      n = this%lakbudptr%budterm(this%idxbudgwf)%id1(j)
      this%hcof(j) = DZERO
      this%rhs(j) = DZERO
      igwfnode = this%lakbudptr%budterm(this%idxbudgwf)%id2(j)
      qbnd = this%lakbudptr%budterm(this%idxbudgwf)%flow(j)
      if (qbnd <= DZERO) then
        ctmp = this%xnewpak(n)
        this%rhs(j) = qbnd * ctmp
      else
        ctmp = this%xnew(igwfnode)
        this%hcof(j) = -qbnd
      end if
      c1 = qbnd * ctmp * delt
      this%dbuff(n) = this%dbuff(n) + c1
    end do
    !
    ! -- Now divide total accumulated mass in lake by the lake volume
    do n = 1, this%nlakes
      call this%get_volumes(n, v1, v0, delt)
      c1 = this%dbuff(n) / v1
      if (this%iboundpak(n) > 0) then
        this%xnewpak(n) = c1
      end if
    end do
    !
    ! -- Return
    return
  end subroutine lkt_solve
  
  subroutine lkt_accumulate_ccterm(this, ilak, rrate, ccratin, ccratout)
! ******************************************************************************
! lkt_accumulate_ccterm -- Accumulate constant concentration terms for budget.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(GwtLktType) :: this
    integer(I4B), intent(in) :: ilak
    real(DP), intent(in) :: rrate
    real(DP), intent(inout) :: ccratin
    real(DP), intent(inout) :: ccratout
    ! -- locals
    real(DP) :: q
    ! format
    ! code
! ------------------------------------------------------------------------------
    !
    if (this%iboundpak(ilak) < 0) then
      q = -rrate
      this%ccterm(ilak) = this%ccterm(ilak) + q
      !
      ! -- See if flow is into lake or out of lake.
      if (q < DZERO) then
        !
        ! -- Flow is out of lake subtract rate from ratout.
        ccratout = ccratout - q
      else
        !
        ! -- Flow is into lake; add rate to ratin.
        ccratin = ccratin + q
      end if
    end if
    ! -- return
    return
  end subroutine lkt_accumulate_ccterm

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
  
  subroutine get_volumes(this, ilake, vnew, vold, delt)
! ******************************************************************************
! get_volumes -- return the lake new volume and old volume
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(GwtLktType) :: this
    integer(I4B), intent(in) :: ilake
    real(DP), intent(inout) :: vnew, vold
    real(DP), intent(in) :: delt
    ! -- local
    real(DP) :: qss
! ------------------------------------------------------------------------------
    !
    ! -- get volumes
    vold = DZERO
    vnew = vold
    if (this%idxbudsto /= 0) then
      qss = this%lakbudptr%budterm(this%idxbudsto)%flow(ilake)
      vnew = this%lakbudptr%budterm(this%idxbudsto)%auxvar(1, ilake)
      vold = vnew - qss * delt
    end if
    !
    ! -- Return
    return
  end subroutine get_volumes
  
end module GwtLktModule