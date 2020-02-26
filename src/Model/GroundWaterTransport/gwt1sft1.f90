! -- Stream Transport Module
! -- todo: what to do about reactions in stream?  Decay?
! -- todo: save the sft concentration into the sfr aux variable?
! -- todo: calculate the sfr DENSE aux variable using concentration?
!
! SFR flows (sfrbudptr)     index var     SFT term              Transport Type
!---------------------------------------------------------------------------------
! FLOW-JA-FACE              idxbudfjf     FLOW-JA-FACE          sfr2sfr
! GWF (aux FLOW-AREA)       idxbudgwf     GWF                   sfr2gwf
! RAINFALL                  idxbudrain    RAINFALL              q * crain
! EVAPORATION               idxbudevap    EVAPORATION           csfr<cevap: q*csfr, else: q*cevap
! RUNOFF                    idxbudroff    RUNOFF                q * croff
! EXT-INFLOW                idxbudiflw    EXT-INFLOW            q * ciflw
! EXT-OUTFLOW               idxbudoutf    EXT-OUTFLOW           q * csfr
! STORAGE (aux VOLUME)      idxbudsto     none                  used for reach volumes
! none                      none          STORAGE (aux MASS)    
! CONSTANT                  none          none                  none
! FROM-MVR                  ?             FROM-MVR              q * cext
! TO-MVR                    idxbudtmvr?   TO-MVR                q * csfr
! AUXILIARY                 none          none                  none
! none                      none          AUXILIARY             none
! none                      none          CONSTANT              accumulate
!
!
module GwtSftModule

  use KindModule, only: DP, I4B
  use ConstantsModule, only: DZERO, DONE, DHALF, DEP20, LENFTYPE, LINELENGTH,  &
                             LENBOUNDNAME, NAMEDBOUNDFLAG, DNODATA,            &
                             TABLEFT, TABCENTER, TABRIGHT,                     &
                             TABSTRING, TABUCSTRING, TABINTEGER, TABREAL
  use SimModule, only: store_error, count_errors, store_error_unit, ustop
  use BndModule, only: BndType, GetBndFromList
  use GwtFmiModule, only: GwtFmiType
  use SfrModule, only: SfrType
  use MemoryTypeModule, only: MemoryTSType
  use BudgetModule, only: BudgetType
  use BudgetObjectModule, only: BudgetObjectType, budgetobject_cr
  use ObserveModule, only: ObserveType
  use InputOutputModule, only: extract_idnum_or_bndname
  use BaseDisModule, only: DisBaseType
  use ArrayHandlersModule, only: ExpandArray
  
  implicit none
  
  public sft_create
  
  character(len=LENFTYPE) :: ftype = 'SFT'
  character(len=16)       :: text  = '             SFT'
  
  type, extends(BndType) :: GwtSftType
    
    character (len=8), dimension(:), pointer, contiguous :: status => null()
    character(len=16), dimension(:), pointer, contiguous :: csftbudget => NULL()
    integer(I4B), pointer                              :: imatrows => null()   ! if active, add new rows to matrix
    integer(I4B), pointer                              :: iprconc => null()
    integer(I4B), pointer                              :: iconcout => null()
    integer(I4B), pointer                              :: ibudgetout => null()
    integer(I4B), pointer                              :: cbcauxitems => NULL()
    integer(I4B), pointer                              :: nstrm => null()      ! number of streams, set set to maxbound
    integer(I4B), pointer                              :: bditems => NULL()
    integer(I4B), pointer                              :: igwfsfrpak => null()  ! package number of corresponding sfr package
    real(DP), dimension(:), pointer, contiguous        :: strt => null()        ! starting reach concentration
    integer(I4B), dimension(:), pointer, contiguous    :: idxlocnode => null()      !map position in global rhs and x array of pack entry
    integer(I4B), dimension(:), pointer, contiguous    :: idxpakdiag => null()      !map diag position of reach in global amat
    integer(I4B), dimension(:), pointer, contiguous    :: idxdglo => null()         !map position in global array of package diagonal row entries
    integer(I4B), dimension(:), pointer, contiguous    :: idxoffdglo => null()      !map position in global array of package off diagonal row entries
    integer(I4B), dimension(:), pointer, contiguous    :: idxsymdglo => null()      !map position in global array of package diagonal entries to model rows
    integer(I4B), dimension(:), pointer, contiguous    :: idxsymoffdglo => null()   !map position in global array of package off diagonal entries to model rows
    integer(I4B), dimension(:), pointer, contiguous    :: idxfjfdglo => null()      !map diagonal sft to sft in global amat
    integer(I4B), dimension(:), pointer, contiguous    :: idxfjfoffdglo => null()   !map off diagonal sft to sft in global amat
    integer(I4B), dimension(:), pointer, contiguous    :: iboundpak => null()       !package ibound
    real(DP), dimension(:), pointer, contiguous        :: xnewpak => null()         !reach concentration for current time step
    real(DP), dimension(:), pointer, contiguous        :: xoldpak => null()         !reach concentration from previous time step
    real(DP), dimension(:), pointer, contiguous        :: dbuff => null()
    character(len=LENBOUNDNAME), dimension(:), pointer,                         &
                                 contiguous :: streamname => null()
    type (MemoryTSType), dimension(:), pointer, contiguous :: lauxvar => null()
    type(GwtFmiType), pointer                          :: fmi => null()         ! pointer to fmi object
    real(DP), dimension(:), pointer, contiguous        :: qsto => null()        ! mass flux due to storage change
    real(DP), dimension(:), pointer, contiguous        :: ccterm => null()      ! mass flux required to maintain constant concentration
    type(BudgetObjectType), pointer                    :: sfrbudptr => null()
    integer(I4B), pointer                              :: idxbudfjf => null()   ! index of flow ja face in sfrbudptr
    integer(I4B), pointer                              :: idxbudgwf => null()   ! index of gwf terms in sfrbudptr
    integer(I4B), pointer                              :: idxbudsto => null()   ! index of storage terms in sfrbudptr
    integer(I4B), pointer                              :: idxbudrain => null()  ! index of rainfall terms in sfrbudptr
    integer(I4B), pointer                              :: idxbudevap => null()  ! index of evaporation terms in sfrbudptr
    integer(I4B), pointer                              :: idxbudroff => null()  ! index of runoff terms in sfrbudptr
    integer(I4B), pointer                              :: idxbudiflw => null()  ! index of inflow terms in sfrbudptr
    integer(I4B), pointer                              :: idxbudoutf => null()  ! index of outflow terms in sfrbudptr
    integer(I4B), pointer                              :: idxbudtmvr => null()  ! index of to mover terms in sfrbudptr
    integer(I4B), pointer                              :: idxbudfmvr => null()  ! index of from mover terms in sfrbudptr
    integer(I4B), pointer                              :: idxbudaux => null()   ! index of auxiliary terms in sfrbudptr
    integer(I4B), dimension(:), pointer, contiguous    :: idxbudssm => null()   ! flag that sfrbudptr%buditem is a general solute source/sink
    integer(I4B), pointer                              :: nconcbudssm => null() ! number of concbudssm terms (columns)
    real(DP), dimension(:, : ), pointer, contiguous    :: concbudssm => null()  ! user specified concentrations for reach flow terms
    real(DP), dimension(:), pointer, contiguous        :: qmfrommvr => null()   ! a mass flow coming from the mover that needs to be added
    !
    ! -- sfr budget object
    type(BudgetObjectType), pointer :: budobj => null()

    ! -- time series aware data
    type (MemoryTSType), dimension(:), pointer, contiguous :: concsfr => null()  ! reach concentration
    type (MemoryTSType), dimension(:), pointer, contiguous :: concrain => null() ! rainfall concentration
    type (MemoryTSType), dimension(:), pointer, contiguous :: concevap => null() ! evaporation concentration
    type (MemoryTSType), dimension(:), pointer, contiguous :: concroff => null() ! runoff concentration
    type (MemoryTSType), dimension(:), pointer, contiguous :: conciflw => null() ! inflow concentration
    
  contains
  
    procedure :: set_pointers => sft_set_pointers
    procedure :: bnd_ac => sft_ac
    procedure :: bnd_mc => sft_mc
    procedure :: bnd_ar => sft_ar
    procedure :: bnd_rp => sft_rp
    procedure :: bnd_ad => sft_ad
    procedure :: bnd_fc => sft_fc
    procedure, private :: sft_fc_expanded
    procedure, private :: sft_fc_nonexpanded
    procedure, private :: sft_cfupdate
    procedure, private :: sft_check_valid
    procedure, private :: sft_set_stressperiod
    procedure, private :: sft_accumulate_ccterm
    procedure :: bnd_bd => sft_bd
    procedure :: bnd_ot => sft_ot
    procedure :: bnd_da => sft_da
    procedure :: allocate_scalars
    procedure :: sft_allocate_arrays
    procedure :: find_sfr_package
    procedure :: sft_solve
    procedure :: bnd_options => sft_options
    procedure :: read_dimensions => sft_read_dimensions
    procedure :: sft_read_reaches
    procedure :: read_initial_attr => sft_read_initial_attr
    procedure :: define_listlabel
    ! -- methods for observations
    procedure, public :: bnd_obs_supported => sft_obs_supported
    procedure, public :: bnd_df_obs => sft_df_obs
    procedure, public :: bnd_rp_obs => sft_rp_obs
    procedure, private :: sft_bd_obs
    procedure :: get_volumes
    procedure, private :: sft_setup_budobj
    procedure, private :: sft_fill_budobj
    procedure, private :: sft_rain_term
    procedure, private :: sft_evap_term
    procedure, private :: sft_roff_term
    procedure, private :: sft_iflw_term
    procedure, private :: sft_outf_term
    procedure, private :: sft_tmvr_term
    procedure, private :: sft_fjf_term
    
  end type GwtSftType

  contains  
  
  subroutine sft_create(packobj, id, ibcnum, inunit, iout, namemodel, pakname, &
                        fmi)
! ******************************************************************************
! sft_create -- Create a New SFT Package
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
    type(GwtSftType), pointer :: sftobj
! ------------------------------------------------------------------------------
    !
    ! -- allocate the object and assign values to object variables
    allocate(sftobj)
    packobj => sftobj
    !
    ! -- create name and origin
    call packobj%set_names(ibcnum, namemodel, pakname, ftype)
    packobj%text = text
    !
    ! -- allocate scalars
    call sftobj%allocate_scalars()
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
    sftobj%fmi => fmi
    !
    ! -- return
    return
  end subroutine sft_create

  subroutine sft_ac(this, moffset, sparse)
! ******************************************************************************
! bnd_ac -- Add package connection to matrix
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use MemoryManagerModule, only: mem_setptr
    use SparseModule, only: sparsematrix
    ! -- dummy
    class(GwtSftType),intent(inout) :: this
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
      do n = 1, this%nstrm
        nglo = moffset + this%dis%nodes + this%ioffset + n
        call sparse%addconnection(nglo, nglo, 1)
      end do
      !
      ! -- reach-gwf connections
      do i = 1, this%sfrbudptr%budterm(this%idxbudgwf)%nlist
        n = this%sfrbudptr%budterm(this%idxbudgwf)%id1(i)
        jj = this%sfrbudptr%budterm(this%idxbudgwf)%id2(i)
        nglo = moffset + this%dis%nodes + this%ioffset + n
        jglo = jj + moffset
        call sparse%addconnection(nglo, jglo, 1)
        call sparse%addconnection(jglo, nglo, 1)
      end do
      !
      ! -- reach-reach connections
      if (this%idxbudfjf /= 0) then
        do i = 1, this%sfrbudptr%budterm(this%idxbudfjf)%maxlist
          n = this%sfrbudptr%budterm(this%idxbudfjf)%id1(i)
          jj = this%sfrbudptr%budterm(this%idxbudfjf)%id2(i)
          nglo = moffset + this%dis%nodes + this%ioffset + n
          jglo = moffset + this%dis%nodes + this%ioffset + jj
          call sparse%addconnection(nglo, jglo, 1)
        end do
      end if
    end if
    !
    ! -- return
    return
  end subroutine sft_ac

  subroutine sft_mc(this, moffset, iasln, jasln)
! ******************************************************************************
! bnd_ac -- map package connection to matrix
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use SparseModule, only: sparsematrix
    ! -- dummy
    class(GwtSftType),intent(inout) :: this
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
      allocate(this%idxlocnode(this%nstrm))
      allocate(this%idxpakdiag(this%nstrm))
      allocate(this%idxdglo(this%maxbound))
      allocate(this%idxoffdglo(this%maxbound))
      allocate(this%idxsymdglo(this%maxbound))
      allocate(this%idxsymoffdglo(this%maxbound))
      n = 0
      if (this%idxbudfjf /= 0) then
        n = this%sfrbudptr%budterm(this%idxbudfjf)%maxlist
      end if
      allocate(this%idxfjfdglo(n))
      allocate(this%idxfjfoffdglo(n))
      !
      ! -- Find the position of each connection in the global ia, ja structure
      !    and store them in idxglo.  idxglo allows this model to insert or
      !    retrieve values into or from the global A matrix
      ! -- sft rows
      do n = 1, this%nstrm
        this%idxlocnode(n) = this%dis%nodes + this%ioffset + n
        iglo = moffset + this%dis%nodes + this%ioffset + n
        this%idxpakdiag(n) = iasln(iglo)
      end do
      do ipos = 1, this%sfrbudptr%budterm(this%idxbudgwf)%nlist
        n = this%sfrbudptr%budterm(this%idxbudgwf)%id1(ipos)
        j = this%sfrbudptr%budterm(this%idxbudgwf)%id2(ipos)
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
      ! -- sft contributions to gwf portion of global matrix
      do ipos = 1, this%sfrbudptr%budterm(this%idxbudgwf)%nlist
        n = this%sfrbudptr%budterm(this%idxbudgwf)%id1(ipos)
        j = this%sfrbudptr%budterm(this%idxbudgwf)%id2(ipos)
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
      !
      ! -- lak-lak contributions to gwf portion of global matrix
      if (this%idxbudfjf /= 0) then
        do ipos = 1, this%sfrbudptr%budterm(this%idxbudfjf)%nlist
          n = this%sfrbudptr%budterm(this%idxbudfjf)%id1(ipos)
          j = this%sfrbudptr%budterm(this%idxbudfjf)%id2(ipos)
          iglo = moffset + this%dis%nodes + this%ioffset + n
          jglo = moffset + this%dis%nodes + this%ioffset + j
          fjfsearchloop: do jj = iasln(iglo), iasln(iglo + 1) - 1
            if(jglo == jasln(jj)) then
              this%idxfjfdglo(ipos) = iasln(iglo)
              this%idxfjfoffdglo(ipos) = jj
              exit fjfsearchloop
            endif
          enddo fjfsearchloop
        end do
      end if
    else
      allocate(this%idxlocnode(0))
      allocate(this%idxpakdiag(0))
      allocate(this%idxdglo(0))
      allocate(this%idxoffdglo(0))
      allocate(this%idxsymdglo(0))
      allocate(this%idxsymoffdglo(0))
      allocate(this%idxfjfdglo(0))
      allocate(this%idxfjfoffdglo(0))
    endif
    !
    ! -- return
    return
  end subroutine sft_mc

  subroutine sft_ar(this)
! ******************************************************************************
! sft_ar -- Allocate and Read
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use ConstantsModule,   only: LINELENGTH
    ! -- dummy
    class(GwtSftType), intent(inout) :: this
    ! -- local
    ! -- formats
    character(len=*), parameter :: fmtsft =                                    &
      "(1x,/1x,'SFT -- SFR TRANSPORT PACKAGE, VERSION 1, 8/5/2019',           &
      &' INPUT READ FROM UNIT ', i0, //)"
! ------------------------------------------------------------------------------
    !
    ! -- Get obs setup 
    call this%obs%obs_ar()
    !
    ! --print a message identifying the sft package.
    write(this%iout, fmtsft) this%inunit
    !
    ! -- Allocate arrays
    call this%sft_allocate_arrays()
    !
    ! -- read optional initial package parameters
    call this%read_initial_attr()
    !
    ! -- Tell fmi that this package is being handled by SFT, otherwise
    !    SSM would handle the flows into GWT from this LAK.  Then point the
    !    fmi data for an advanced package to xnewpak and qmfrommvr
    this%fmi%iatp(this%igwfsfrpak) = 1
    this%fmi%datp(this%igwfsfrpak)%concpack => this%xnewpak
    this%fmi%datp(this%igwfsfrpak)%qmfrommvr => this%qmfrommvr
    !
    ! -- Return
    return
  end subroutine sft_ar

  subroutine sft_rp(this)
! ******************************************************************************
! sft_rp -- Read and Prepare
! Subroutine: (1) read itmp
!             (2) read new boundaries if itmp>0
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use ConstantsModule, only: LINELENGTH
    use TdisModule, only: kper, nper
    ! -- dummy
    class(GwtSftType), intent(inout) :: this
    ! -- local
    integer(I4B) :: ierr
    integer(I4B) :: n
    logical :: isfound, endOfBlock
    character(len=LINELENGTH) :: line
    character(len=LINELENGTH) :: errmsg
    integer(I4B) :: itemno
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
            write(this%iout,'(3x,a)')  '     SFT KEYWORD AND DATA'
            write(this%iout,'(3x,78("-"))')
          end if
        end if
        itemno = this%parser%GetInteger()
        call this%parser%GetRemainingLine(line)
        call this%sft_set_stressperiod(itemno, line)
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
    ! -- write summary of sfr stress period error messages
    ierr = count_errors()
    if (ierr > 0) then
      call this%parser%StoreErrorUnit()
      call ustop()
    end if
    !
    ! -- fill arrays
    do n = 1, this%sfrbudptr%budterm(this%idxbudgwf)%nlist
      igwfnode = this%sfrbudptr%budterm(this%idxbudgwf)%id2(n)
      this%nodelist(n) = igwfnode
    end do
    !
    ! -- return
    return
  end subroutine sft_rp

  subroutine sft_set_stressperiod(this, itemno, line)
! ******************************************************************************
! sft_set_stressperiod -- Set a stress period attribute for lakweslls(itemno)
!                         using keywords.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use TdisModule, only: kper, perlen, totimsav
    use TimeSeriesManagerModule, only: read_single_value_or_time_series
    use InputOutputModule, only: urword
    ! -- dummy
    class(GwtSftType),intent(inout) :: this
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
        ierr = this%sft_check_valid(itemno)
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
        ierr = this%sft_check_valid(itemno)
        if (ierr /= 0) goto 999
        call urword(line, lloc, istart, istop, 0, ival, rval, this%iout, this%inunit)
        text = line(istart:istop)
        jj = 1    ! For sfr concentration
        call read_single_value_or_time_series(text, &
                                              this%concsfr(itmp)%value, &
                                              this%concsfr(itmp)%name, &
                                              endtim,  &
                                              this%name, 'BND', this%TsManager, &
                                              this%iprpak, itmp, jj, 'CONCENTRATION', &
                                              bndName, this%inunit)
      case ('RAINFALL')
        ierr = this%sft_check_valid(itemno)
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
        ierr = this%sft_check_valid(itemno)
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
        ierr = this%sft_check_valid(itemno)
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
        ierr = this%sft_check_valid(itemno)
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
        ierr = this%sft_check_valid(itemno)
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
          call read_single_value_or_time_series(text, &
                                                this%lauxvar(ii)%value, &
                                                this%lauxvar(ii)%name, &
                                                endtim,  &
                                                this%Name, 'AUX', this%TsManager, &
                                                this%iprpak, itmp, jj, &
                                                this%auxname(iaux), bndName, &
                                                this%inunit)
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
  end subroutine sft_set_stressperiod

  function sft_check_valid(this, itemno) result(ierr)
! ******************************************************************************
!  sft_check_valid -- Determine if a valid reach number has been
!                     specified.
! ******************************************************************************
    ! -- return
    integer(I4B) :: ierr
    ! -- dummy
    class(GwtSftType),intent(inout) :: this
    integer(I4B), intent(in) :: itemno
    ! -- local
    character(len=LINELENGTH) :: errmsg
    ! -- formats
! ------------------------------------------------------------------------------
    ierr = 0
    if (itemno < 1 .or. itemno > this%nstrm) then
      write(errmsg,'(4x,a,1x,i6,1x,a,1x,i6)') &
        '****ERROR. REACHNO ', itemno, 'MUST BE > 0 and <= ', this%nstrm
      call store_error(errmsg)
      ierr = 1
    end if
  end function sft_check_valid

  subroutine sft_ad(this)
! ******************************************************************************
! sft_ad -- Add package connection to matrix
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(GwtSftType) :: this
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
    if (this%naux > 0) then
      do j = 1, this%sfrbudptr%budterm(this%idxbudgwf)%nlist
        n = this%sfrbudptr%budterm(this%idxbudgwf)%id1(j)
        do iaux = 1, this%naux
          ii = (n - 1) * this%naux + iaux
          this%auxvar(iaux, j) = this%lauxvar(ii)%value
        end do
      end do
    end if
    !
    ! -- copy xnew into xold and set xnewpak to stage%value for
    !    constant stage reaches
    do n = 1, this%nstrm
      this%xoldpak(n) = this%xnewpak(n)
      if (this%iboundpak(n) < 0) then
        this%xnewpak(n) = this%concsfr(n)%value
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
    call this%obs%obs_ad()
    !
    ! -- return
    return
  end subroutine sft_ad

  subroutine sft_fc(this, rhs, ia, idxglo, amatsln)
! ******************************************************************************
! sft_fc
! ****************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(GwtSftType) :: this
    real(DP), dimension(:), intent(inout) :: rhs
    integer(I4B), dimension(:), intent(in) :: ia
    integer(I4B), dimension(:), intent(in) :: idxglo
    real(DP), dimension(:), intent(inout) :: amatsln
    ! -- local
! ------------------------------------------------------------------------------
    !
    ! -- Call fc depending on whether or not a matrix is expanded or not
    if (this%imatrows == 0) then
      call this%sft_fc_nonexpanded(rhs, ia, idxglo, amatsln)
    else
      call this%sft_fc_expanded(rhs, ia, idxglo, amatsln)
    end if
    ! -- Return
    return
  end subroutine sft_fc

  subroutine sft_fc_nonexpanded(this, rhs, ia, idxglo, amatsln)
! ******************************************************************************
! sft_fc_nonexpanded -- formulate for the nonexpanded a matrix case
! ****************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(GwtSftType) :: this
    real(DP), dimension(:), intent(inout) :: rhs
    integer(I4B), dimension(:), intent(in) :: ia
    integer(I4B), dimension(:), intent(in) :: idxglo
    real(DP), dimension(:), intent(inout) :: amatsln
    ! -- local
    integer(I4B) :: j, igwfnode, idiag
! ------------------------------------------------------------------------------
    !
    ! -- solve for concentration in the reaches
    call this%sft_solve()
    !
    ! -- add hcof and rhs terms (from sft_solve) to the gwf matrix
    do j = 1, this%sfrbudptr%budterm(this%idxbudgwf)%nlist
      igwfnode = this%sfrbudptr%budterm(this%idxbudgwf)%id2(j)
      if (this%ibound(igwfnode) < 1) cycle
      idiag = idxglo(ia(igwfnode))
      amatsln(idiag) = amatsln(idiag) + this%hcof(j)
      rhs(igwfnode) = rhs(igwfnode) + this%rhs(j)
    end do
    !
    ! -- Return
    return
  end subroutine sft_fc_nonexpanded

  subroutine sft_fc_expanded(this, rhs, ia, idxglo, amatsln)
! ******************************************************************************
! sft_fc_expanded -- formulate for the expanded a matrix case
! ****************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use TdisModule, only: delt
    ! -- dummy
    class(GwtSftType) :: this
    real(DP), dimension(:), intent(inout) :: rhs
    integer(I4B), dimension(:), intent(in) :: ia
    integer(I4B), dimension(:), intent(in) :: idxglo
    real(DP), dimension(:), intent(inout) :: amatsln
    ! -- local
    integer(I4B) :: j, n, n1, n2
    integer(I4B) :: iloc
    integer(I4B) :: iposd, iposoffd
    integer(I4B) :: ipossymd, ipossymoffd
    real(DP) :: cold
    real(DP) :: qbnd
    real(DP) :: v0
    real(DP) :: v1
    real(DP) :: omega
    real(DP) :: rrate
    real(DP) :: rhsval
    real(DP) :: hcofval
! ------------------------------------------------------------------------------
    !
    ! -- Add coefficients for change in reach volume
    do n = 1, this%nstrm
      cold  = this%xoldpak(n)
      iloc = this%idxlocnode(n)
      iposd = this%idxpakdiag(n)
      !
      ! -- mass storage in sfr
      call this%get_volumes(n, v1, v0, delt)
      amatsln(iposd) = amatsln(iposd) - v1 / delt
      rhs(iloc) = rhs(iloc) - cold * v0 / delt
      !
    end do
    !
    ! -- add rainfall contribution
    if (this%idxbudrain /= 0) then
      do j = 1, this%sfrbudptr%budterm(this%idxbudrain)%nlist
        call this%sft_rain_term(j, n1, n2, rrate, rhsval, hcofval)
        iloc = this%idxlocnode(n1)
        iposd = this%idxpakdiag(n1)
        amatsln(iposd) = amatsln(iposd) + hcofval
        rhs(iloc) = rhs(iloc) + rhsval
      end do
    end if
    !
    ! -- add evaporation contribution
    if (this%idxbudevap /= 0) then
      do j = 1, this%sfrbudptr%budterm(this%idxbudevap)%nlist
        call this%sft_evap_term(j, n1, n2, rrate, rhsval, hcofval)
        iloc = this%idxlocnode(n1)
        iposd = this%idxpakdiag(n1)
        amatsln(iposd) = amatsln(iposd) + hcofval
        rhs(iloc) = rhs(iloc) + rhsval
      end do
    end if
    !
    ! -- add runoff contribution
    if (this%idxbudroff /= 0) then
      do j = 1, this%sfrbudptr%budterm(this%idxbudroff)%nlist
        call this%sft_roff_term(j, n1, n2, rrate, rhsval, hcofval)
        iloc = this%idxlocnode(n1)
        iposd = this%idxpakdiag(n1)
        amatsln(iposd) = amatsln(iposd) + hcofval
        rhs(iloc) = rhs(iloc) + rhsval
      end do
    end if
    !
    ! -- add inflow contribution
    if (this%idxbudiflw /= 0) then
      do j = 1, this%sfrbudptr%budterm(this%idxbudiflw)%nlist
        call this%sft_iflw_term(j, n1, n2, rrate, rhsval, hcofval)
        iloc = this%idxlocnode(n1)
        iposd = this%idxpakdiag(n1)
        amatsln(iposd) = amatsln(iposd) + hcofval
        rhs(iloc) = rhs(iloc) + rhsval
      end do
    end if
    !
    ! -- add outflow contribution
    if (this%idxbudoutf /= 0) then
      do j = 1, this%sfrbudptr%budterm(this%idxbudoutf)%nlist
        call this%sft_outf_term(j, n1, n2, rrate, rhsval, hcofval)
        iloc = this%idxlocnode(n1)
        iposd = this%idxpakdiag(n1)
        amatsln(iposd) = amatsln(iposd) + hcofval
        rhs(iloc) = rhs(iloc) + rhsval
      end do
    end if
    !
    ! -- add to mover contribution
    if (this%idxbudtmvr /= 0) then
      do j = 1, this%sfrbudptr%budterm(this%idxbudtmvr)%nlist
        call this%sft_tmvr_term(j, n1, n2, rrate, rhsval, hcofval)
        iloc = this%idxlocnode(n1)
        iposd = this%idxpakdiag(n1)
        amatsln(iposd) = amatsln(iposd) + hcofval
        rhs(iloc) = rhs(iloc) + rhsval
      end do
    end if
    !
    ! -- add from mover contribution
    if (this%idxbudfmvr /= 0) then
      do n = 1, this%nstrm
        rhsval = this%qmfrommvr(n)
        iloc = this%idxlocnode(n)
        rhs(iloc) = rhs(iloc) - rhsval
      end do
    end if
    !
    ! -- go through each lak-gwf connection
    do j = 1, this%sfrbudptr%budterm(this%idxbudgwf)%nlist
      !
      ! -- set n to reach number and process if active reach
      n = this%sfrbudptr%budterm(this%idxbudgwf)%id1(j)
      if (this%iboundpak(n) /= 0) then
        !
        ! -- set acoef and rhs to negative so they are relative to sft and not gwt
        qbnd = this%sfrbudptr%budterm(this%idxbudgwf)%flow(j)
        omega = DZERO
        if (qbnd < DZERO) omega = DONE
        !
        ! -- add to sft row
        iposd = this%idxdglo(j)
        iposoffd = this%idxoffdglo(j)
        amatsln(iposd) = amatsln(iposd) + omega * qbnd
        amatsln(iposoffd) = amatsln(iposoffd) + (DONE - omega) * qbnd
        !
        ! -- add to gwf row for sft connection
        ipossymd = this%idxsymdglo(j)
        ipossymoffd = this%idxsymoffdglo(j)
        amatsln(ipossymd) = amatsln(ipossymd) - (DONE - omega) * qbnd
        amatsln(ipossymoffd) = amatsln(ipossymoffd) - omega * qbnd
      end if    
    end do
    !
    ! -- go through each lak-lak connection
    if (this%idxbudfjf /= 0) then
      do j = 1, this%sfrbudptr%budterm(this%idxbudfjf)%nlist
        n1 = this%sfrbudptr%budterm(this%idxbudfjf)%id1(j)
        n2 = this%sfrbudptr%budterm(this%idxbudfjf)%id2(j)
        qbnd = this%sfrbudptr%budterm(this%idxbudfjf)%flow(j)
        if (qbnd <= DZERO) then
          omega = DONE
        else
          omega = DZERO
        end if
        iposd = this%idxfjfdglo(j)
        iposoffd = this%idxfjfoffdglo(j)
        amatsln(iposd) = amatsln(iposd) + omega * qbnd
        amatsln(iposoffd) = amatsln(iposoffd) + (DONE - omega) * qbnd
      end do
    end if
    !
    ! -- Return
    return
  end subroutine sft_fc_expanded

  subroutine sft_cfupdate(this)
! ******************************************************************************
! sft_cfupdate -- calculate package hcof and rhs so gwt budget is calculated
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(GwtSftType) :: this
    ! -- local
    integer(I4B) :: j, n
    real(DP) :: qbnd
    real(DP) :: omega
! ------------------------------------------------------------------------------
    !
    ! -- Calculate hcof and rhs terms so GWF exchanges are calculated correctly
    ! -- go through each lak-gwf connection and calculate
    !    rhs and hcof terms for gwt matrix rows
    do j = 1, this%sfrbudptr%budterm(this%idxbudgwf)%nlist
      n = this%sfrbudptr%budterm(this%idxbudgwf)%id1(j)
      this%hcof(j) = DZERO
      this%rhs(j) = DZERO
      if (this%iboundpak(n) /= 0) then
        qbnd = this%sfrbudptr%budterm(this%idxbudgwf)%flow(j)
        omega = DZERO
        if (qbnd < DZERO) omega = DONE
        this%hcof(j) = - (DONE - omega) * qbnd
        this%rhs(j) = omega * qbnd * this%xnewpak(n)
      endif
    end do    
    !
    ! -- Return
    return
  end subroutine sft_cfupdate

  subroutine sft_bd(this, x, idvfl, icbcfl, ibudfl, icbcun, iprobs,            &
                    isuppress_output, model_budget, imap, iadv)
! ******************************************************************************
! sft_bd -- Calculate Volumetric Budget for the reach
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
    class(GwtSftType) :: this
    real(DP),dimension(:), intent(in) :: x
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
    integer(I4B) :: n
    real(DP) :: c
    real(DP) :: rrate, rhs, hcof
    real(DP) :: v0, v1
    ! -- for observations
    integer(I4B) :: iprobslocal
    ! -- formats
! ------------------------------------------------------------------------------
    !
    ! -- Solve the lak concentrations again or update the reach hcof 
    !    and rhs terms
    if (this%imatrows == 0) then
      call this%sft_solve()
    else
      call this%sft_cfupdate()
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
    ! -- calculate storage term
    do n = 1, this%nstrm
      rrate = DZERO
      if (this%iboundpak(n) > 0) then
        call this%get_volumes(n, v1, v0, delt)
        call sft_calc_qsto(v0, v1, this%xoldpak(n), this%xnewpak(n), delt,     &
                          rhs, hcof, rrate)
      end if
      this%qsto(n) = rrate
    end do
    !
    ! -- set unit number for binary dependent variable output
    ibinun = 0
    if(this%iconcout /= 0) then
      ibinun = this%iconcout
    end if
    if(idvfl == 0) ibinun = 0
    if (isuppress_output /= 0) ibinun = 0
    !
    ! -- write reach binary output
    if (ibinun > 0) then
      do n = 1, this%nstrm
        c = this%xnewpak(n)
        if (this%iboundpak(n) == 0) then
          c = DHNOFLO
        end if
        this%dbuff(n) = c
      end do
      call ulasav(this%dbuff, '   CONCENTRATION', kstp, kper, pertim, totim,   &
                  this%nstrm, 1, 1, ibinun)
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
    ! -- fill the budget object
    call this%sft_fill_budobj(x)
    !
    ! -- write the flows from the budobj
    ibinun = 0
    if(this%ibudgetout /= 0) then
      ibinun = this%ibudgetout
    end if
    if(icbcfl == 0) ibinun = 0
    if (isuppress_output /= 0) ibinun = 0
    if (ibinun > 0) then
      call this%budobj%save_flows(this%dis, ibinun, kstp, kper, delt, &
                        pertim, totim, this%iout)
    end if
    !    
    ! -- For continuous observations, save simulated values.  This
    !    needs to be called after sft_fill_budobj() so that the budget
    !    terms have been calculated
    if (this%obs%npakobs > 0 .and. iprobs > 0) then
      call this%sft_bd_obs()
    endif
    !
    ! -- return
    return
  end subroutine sft_bd

  subroutine sft_calc_qsto(v0, v1, c0, c1, delt, rhs, hcof, rate)
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
  end subroutine sft_calc_qsto
  
  subroutine sft_ot(this, kstp, kper, iout, ihedfl, ibudfl)
! ******************************************************************************
! sft_ot
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use InputOutputModule, only: UWWORD
    ! -- dummy
    class(GwtSftType) :: this
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
    ! -- write reach concentration
    if (ihedfl /= 0 .and. this%iprconc /= 0) then
      write (iout, fmthdr) 'REACH (', trim(this%name), ') CONCENTRATION', kper, kstp
      
      iloc = 1
      line = ''
      if (this%inamedbound==1) then
        call UWWORD(line, iloc, 16, TABUCSTRING,                                 &
                    'lake', n, q, ALIGNMENT=TABLEFT)
      end if
      call UWWORD(line, iloc, 6, TABUCSTRING,                                    &
                  'lake', n, q, ALIGNMENT=TABCENTER, SEP=' ')
      call UWWORD(line, iloc, 11, TABUCSTRING,                                   &
                  'lake', n, q, ALIGNMENT=TABCENTER)
      ! -- create line separator
      linesep = repeat('-', iloc)
      ! -- write first line
      write(iout,'(1X,A)') linesep(1:iloc)
      write(iout,'(1X,A)') line(1:iloc)
      ! -- create second header line
      iloc = 1
      line = ''
      if (this%inamedbound==1) then
        call UWWORD(line, iloc, 16, TABUCSTRING,                                 &
                    'name', n, q, ALIGNMENT=TABLEFT)
      end if
      call UWWORD(line, iloc, 6, TABUCSTRING,                                    &
                  'no.', n, q, ALIGNMENT=TABCENTER, SEP=' ')
      call UWWORD(line, iloc, 11, TABUCSTRING,                                   &
                  'conc', n, q, ALIGNMENT=TABCENTER)
      ! -- write second line
      write(iout,'(1X,A)') line(1:iloc)
      write(iout,'(1X,A)') linesep(1:iloc)
      ! -- write data
      do n = 1, this%nstrm
        iloc = 1
        line = ''
        if (this%inamedbound==1) then
          call UWWORD(line, iloc, 16, TABUCSTRING,                               &
                      this%streamname(n), n, q, ALIGNMENT=TABLEFT)
        end if
        call UWWORD(line, iloc, 6, TABINTEGER, text, n, q, SEP=' ')
        call UWWORD(line, iloc, 11, TABREAL, text, n, this %xnewpak(n))
        write(iout, '(1X,A)') line(1:iloc)
      end do
    end if      
      
      
      
    !  iloc = 1
    !  line = ''
    !  if (this%inamedbound==1) then
    !    call UWWORD(line, iloc, 16, 1, 'reach', n, q, left=.TRUE.)
    !  end if
    !  call UWWORD(line, iloc, 6, 1, 'reach', n, q, CENTER=.TRUE.)
    !  call UWWORD(line, iloc, 11, 1, 'reach', n, q, CENTER=.TRUE.)
    !  ! -- create line separator
    !  linesep = repeat('-', iloc)
    !  ! -- write first line
    !  write(iout,'(1X,A)') linesep(1:iloc)
    !  write(iout,'(1X,A)') line(1:iloc)
    !  ! -- create second header line
    !  iloc = 1
    !  line = ''
    !  if (this%inamedbound==1) then
    !    call UWWORD(line, iloc, 16, 1, 'name', n, q, left=.TRUE.)
    !  end if
    !  call UWWORD(line, iloc, 6, 1, 'no.', n, q, CENTER=.TRUE.)
    !  call UWWORD(line, iloc, 11, 1, 'conc', n, q, CENTER=.TRUE.)
    !  ! -- write second line
    !  write(iout,'(1X,A)') line(1:iloc)
    !  write(iout,'(1X,A)') linesep(1:iloc)
    !  ! -- write data
    !  do n = 1, this%nstrm
    !    iloc = 1
    !    line = ''
    !    if (this%inamedbound==1) then
    !      call UWWORD(line, iloc, 16, 1, this%streamname(n), n, q, left=.TRUE.)
    !    end if
    !    call UWWORD(line, iloc, 6, 2, text, n, q)
    !    call UWWORD(line, iloc, 11, 3, text, n, this%xnewpak(n))
    !    write(iout, '(1X,A)') line(1:iloc)
    !  end do
    !end if
    !
    ! -- Output sfr flow table
    if (ibudfl /= 0 .and. this%iprflow /= 0) then
      call this%budobj%write_flowtable(this%dis)
    end if
    !
    ! -- Output reach budget
    call this%budobj%write_budtable(kstp, kper, iout)
    !
    ! -- Return
    return
  end subroutine sft_ot

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
    class(GwtSftType) :: this
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
    call mem_allocate(this%igwfsfrpak, 'IGWFSFRPAK', this%origin)
    call mem_allocate(this%nstrm, 'NSTRM', this%origin)
    call mem_allocate(this%bditems, 'BDITEMS', this%origin)
    call mem_allocate(this%idxbudfjf, 'IDXBUDFJF', this%origin)
    call mem_allocate(this%idxbudgwf, 'IDXBUDGWF', this%origin)
    call mem_allocate(this%idxbudsto, 'IDXBUDSTO', this%origin)
    call mem_allocate(this%idxbudrain, 'IDXBUDRAIN', this%origin)
    call mem_allocate(this%idxbudevap, 'IDXBUDEVAP', this%origin)
    call mem_allocate(this%idxbudroff, 'IDXBUDROFF', this%origin)
    call mem_allocate(this%idxbudiflw, 'IDXBUDIFLW', this%origin)
    call mem_allocate(this%idxbudoutf, 'IDXBUDOUTF', this%origin)
    call mem_allocate(this%idxbudtmvr, 'IDXBUDTMVR', this%origin)
    call mem_allocate(this%idxbudfmvr, 'IDXBUDFMVR', this%origin)
    call mem_allocate(this%idxbudaux, 'IDXBUDAUX', this%origin)
    call mem_allocate(this%nconcbudssm, 'NCONCBUDSSM', this%origin)
    ! 
    ! -- Initialize
    this%imatrows = 1
    this%iprconc = 0
    this%iconcout = 0
    this%ibudgetout = 0
    this%igwfsfrpak = 0
    this%nstrm = 0
    this%bditems = 9
    this%idxbudfjf = 0
    this%idxbudgwf = 0
    this%idxbudsto = 0
    this%idxbudrain = 0
    this%idxbudevap = 0
    this%idxbudroff = 0
    this%idxbudiflw = 0
    this%idxbudoutf = 0
    this%idxbudtmvr = 0
    this%idxbudfmvr = 0
    this%idxbudaux = 0
    this%nconcbudssm = 0
    !
    ! -- Return
    return
  end subroutine allocate_scalars

  subroutine sft_allocate_arrays(this)
! ******************************************************************************
! allocate_arrays
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryManagerModule, only: mem_allocate
    ! -- dummy
    class(GwtSftType), intent(inout) :: this
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
      call mem_allocate(this%dbuff, this%nstrm, 'DBUFF', this%origin)
      do n = 1, this%nstrm
        this%dbuff(n) = DZERO
      end do
    else
      call mem_allocate(this%dbuff, 0, 'DBUFF', this%origin)
    end if
    !
    ! -- allocate character array for budget text
    allocate(this%csftbudget(this%bditems))
    !
    ! -- allocate character array for status
    allocate(this%status(this%nstrm))
    !
    ! -- time series
    call mem_allocate(this%concsfr, this%nstrm, 'CONCSFR', this%origin)
    call mem_allocate(this%concrain, this%nstrm, 'CONCRAIN', this%origin)
    call mem_allocate(this%concevap, this%nstrm, 'CONCEVAP', this%origin)
    call mem_allocate(this%concroff, this%nstrm, 'CONCROFF', this%origin)
    call mem_allocate(this%conciflw, this%nstrm, 'CONCIFLW', this%origin)
    !
    ! -- budget terms
    call mem_allocate(this%qsto, this%nstrm, 'QSTO', this%origin)
    call mem_allocate(this%ccterm, this%nstrm, 'CCTERM', this%origin)
    !
    ! -- concentration for budget terms
    call mem_allocate(this%concbudssm, this%nconcbudssm, this%nstrm, &
      'CONCBUDSSM', this%origin)
    !
    ! -- mass added from the mover transport package
    call mem_allocate(this%qmfrommvr, this%nstrm, 'QMFROMMVR', this%origin)
    !
    ! -- Initialize
    !
    !-- fill csftbudget
    this%csftbudget(1) = '             GWF'
    this%csftbudget(2) = '         STORAGE'
    this%csftbudget(3) = '        CONSTANT'
    this%csftbudget(4) = '        RAINFALL'
    this%csftbudget(5) = '     EVAPORATION'
    this%csftbudget(6) = '          RUNOFF'
    this%csftbudget(7) = '      EXT-INFLOW'
    this%csftbudget(8) = '      WITHDRAWAL'
    this%csftbudget(9) = '     EXT-OUTFLOW'
    !
    ! -- initialize arrays
    do n = 1, this%nstrm
      this%status(n) = 'ACTIVE'
      this%qsto(n) = DZERO
      this%ccterm(n) = DZERO
      this%qmfrommvr(n) = DZERO
      this%concbudssm(:, n) = DZERO
    end do
    !
    ! -- Return
    return
  end subroutine sft_allocate_arrays
  
  subroutine sft_da(this)
! ******************************************************************************
! sft_da
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryManagerModule, only: mem_deallocate
    ! -- dummy
    class(GwtSftType) :: this
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
    call mem_deallocate(this%concsfr)
    call mem_deallocate(this%concrain)
    call mem_deallocate(this%concevap)
    call mem_deallocate(this%concroff)
    call mem_deallocate(this%conciflw)
    call mem_deallocate(this%qmfrommvr)
    deallocate(this%csftbudget)
    deallocate(this%status)
    deallocate(this%streamname)
    !
    ! -- budobj
    call this%budobj%budgetobject_da()
    deallocate(this%budobj)
    nullify(this%budobj)
    !
    ! -- index pointers
    deallocate(this%idxlocnode)
    deallocate(this%idxpakdiag)
    deallocate(this%idxdglo)
    deallocate(this%idxoffdglo)
    deallocate(this%idxsymdglo)
    deallocate(this%idxsymoffdglo)
    deallocate(this%idxfjfdglo)
    deallocate(this%idxfjfoffdglo)
    !
    ! -- deallocate scalars
    call mem_deallocate(this%imatrows)
    call mem_deallocate(this%iprconc)
    call mem_deallocate(this%iconcout)
    call mem_deallocate(this%ibudgetout)
    call mem_deallocate(this%igwfsfrpak)
    call mem_deallocate(this%nstrm)
    call mem_deallocate(this%bditems)
    call mem_deallocate(this%idxbudfjf)
    call mem_deallocate(this%idxbudgwf)
    call mem_deallocate(this%idxbudsto)
    call mem_deallocate(this%idxbudrain)
    call mem_deallocate(this%idxbudevap)
    call mem_deallocate(this%idxbudroff)
    call mem_deallocate(this%idxbudiflw)
    call mem_deallocate(this%idxbudoutf)
    call mem_deallocate(this%idxbudtmvr)
    call mem_deallocate(this%idxbudfmvr)
    call mem_deallocate(this%idxbudaux)
    call mem_deallocate(this%idxbudssm)
    call mem_deallocate(this%nconcbudssm)
    !
    ! -- deallocate scalars in NumericalPackageType
    call this%BndType%bnd_da()
    !
    ! -- Return
    return
  end subroutine sft_da

  subroutine find_sfr_package(this)
! ******************************************************************************
! find corresponding lak package
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryManagerModule, only: mem_allocate
    ! -- dummy
    class(GwtSftType) :: this
    ! -- local
    character(len=LINELENGTH) :: errmsg
    class(BndType), pointer :: packobj
    integer(I4B) :: ip, icount
    integer(I4B) :: nbudterm
    logical :: found
! ------------------------------------------------------------------------------
    !
    ! -- Look through gwfbndlist for a SFR package with the same name as this
    !    SFT package name
    found = .false.
    if (associated(this%fmi%gwfbndlist)) then
      do ip = 1, this%fmi%gwfbndlist%Count()
        packobj => GetBndFromList(this%fmi%gwfbndlist, ip)
        if (packobj%name == this%name) then
          found = .true.
          this%igwfsfrpak = ip
          select type (packobj)
            type is (SfrType)
              this%sfrbudptr => packobj%budobj
          end select
        end if
        if (found) exit
      end do
    end if
    !
    ! -- error if lak package not found
    if (.not. found) then
      write(errmsg, '(a)') '****ERROR. CORRESPONDING LAK PACKAGE NOT FOUND &
                            &FOR SFT.'
      call store_error(errmsg)
      call this%parser%StoreErrorUnit()
      call ustop()
    endif
    !
    ! -- allocate space for idxbudssm, which indicates whether this is a 
    !    special budget term or one that is a general source and sink
    nbudterm = this%sfrbudptr%nbudterm
    call mem_allocate(this%idxbudssm, nbudterm, 'IDXBUDSSM', this%origin)
    !
    ! -- Process budget terms and identify special budget terms
    write(this%iout, '(/, a, a)') &
      'PROCESSING SFT INFORMATION FOR ', this%name
    write(this%iout, '(a)') '  IDENTIFYING FLOW TERMS IN LAK PACKAGE'
    write(this%iout, '(a, i0)') &
      '  NUMBER OF REACHES = ', this%sfrbudptr%ncv
    icount = 1
    do ip = 1, this%sfrbudptr%nbudterm
      select case(trim(adjustl(this%sfrbudptr%budterm(ip)%flowtype)))
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
      case('EXT-OUTFLOW')
        this%idxbudoutf = ip
        this%idxbudssm(ip) = 0
      case('TO-MVR')
        this%idxbudtmvr = ip
        this%idxbudssm(ip) = 0
      case('FROM-MVR')
        this%idxbudfmvr = ip
        this%idxbudssm(ip) = 0
      case('AUXILIARY')
        this%idxbudaux = ip
        this%idxbudssm(ip) = 0
      case default
        !
        ! -- set idxbudssm equal to a column index for where the concentrations
        !    are stored in the concbud(nbudssm, nreaches) array
        this%idxbudssm(ip) = icount
        icount = icount + 1
      end select
      write(this%iout, '(a, i0, " = ", a,/, a, i0)') &
        '  TERM ', ip, trim(adjustl(this%sfrbudptr%budterm(ip)%flowtype)), &
        '   MAX NO. OF ENTRIES = ', this%sfrbudptr%budterm(ip)%maxlist
    end do
    write(this%iout, '(a, //)') 'DONE PROCESSING SFT INFORMATION'
    !
    ! -- Return
    return
  end subroutine find_sfr_package

  subroutine  sft_options(this, option, found)
! ******************************************************************************
! sft_options -- set options specific to GwtSftType
!
! sft_options overrides BndType%bnd_options
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use ConstantsModule, only: MAXCHARLEN, DZERO
    use OpenSpecModule, only: access, form
    use InputOutputModule, only: urword, getunit, openfile
    ! -- dummy
    class(GwtSftType), intent(inout) :: this
    character(len=*),  intent(inout) :: option
    logical,           intent(inout) :: found
    ! -- local
    character(len=MAXCHARLEN) :: fname, keyword
    ! -- formats
    character(len=*),parameter :: fmtreachopt = &
      "(4x, 'REACH ', a, ' VALUE (',g15.7,') SPECIFIED.')"
    character(len=*),parameter :: fmtlakbin = &
      "(4x, 'REACH ', 1x, a, 1x, ' WILL BE SAVED TO FILE: ', a, /4x, 'OPENED ON UNIT: ', I7)"
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
          ' SFT WILL NOT ADD ADDITIONAL ROWS TO THE A MATRIX.'
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
  end subroutine sft_options

  subroutine sft_read_dimensions(this)
! ******************************************************************************
! sft_read_dimensions -- Determine dimensions for this package
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use ConstantsModule, only: LINELENGTH
    ! -- dummy
    class(GwtSftType),intent(inout) :: this
    ! -- local
    character(len=LINELENGTH) :: errmsg
    integer(I4B) :: ierr
    ! -- format
! ------------------------------------------------------------------------------
    !
    ! -- Set a pointer to the GWF SFR Package budobj
    call this%find_sfr_package()
    !
    ! -- Set dimensions from the GWF LAK package
    this%nstrm = this%sfrbudptr%ncv
    this%maxbound = this%sfrbudptr%budterm(this%idxbudgwf)%maxlist
    this%nbound = this%maxbound
    write(this%iout, '(a, a)') 'SETTING DIMENSIONS FOR SFT PACKAGE ', this%name
    write(this%iout,'(2x,a,i0)')'NSTRM = ', this%nstrm
    write(this%iout,'(2x,a,i0)')'MAXBOUND = ', this%maxbound
    write(this%iout,'(2x,a,i0)')'NBOUND = ', this%nbound
    if (this%imatrows /= 0) then
      this%npakeq = this%nstrm
      write(this%iout,'(2x,a)') 'SFT SOLVED AS PART OF GWT MATRIX EQUATIONS'
    else
      write(this%iout,'(2x,a)') 'SFT SOLVED SEPARATELY FROM GWT MATRIX EQUATIONS '
    end if
    write(this%iout, '(a, //)') 'DONE SETTING SFT DIMENSIONS'
    !
    ! -- Check for errors
    if (this%nstrm < 0) then
      write(errmsg, '(1x,a)') &
        'ERROR:  NUMBER OF REACHES COULD NOT BE DETERMINED CORRECTLY.'
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
    call this%sft_read_reaches()
    !
    ! -- Call define_listlabel to construct the list label that is written
    !    when PRINT_INPUT option is used.
    call this%define_listlabel()
    !
    ! -- setup the budget object
    call this%sft_setup_budobj()
    !
    ! -- return
    return
  end subroutine sft_read_dimensions

  subroutine sft_read_reaches(this)
! ******************************************************************************
! sft_read_reaches -- Read feature infromation for this package
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use ConstantsModule, only: LINELENGTH
    use MemoryManagerModule, only: mem_allocate
    use TimeSeriesManagerModule, only: read_single_value_or_time_series
    ! -- dummy
    class(GwtSftType),intent(inout) :: this
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
    ! -- allocate reach data
    call mem_allocate(this%strt, this%nstrm, 'STRT', this%origin)
    !call mem_allocate(this%rainfall, this%nstrm, 'RAINFALL', this%origin)
    !call mem_allocate(this%evaporation, this%nstrm, 'EVAPORATION', this%origin)
    !call mem_allocate(this%runoff, this%nstrm, 'RUNOFF', this%origin)
    !call mem_allocate(this%inflow, this%nstrm, 'INFLOW', this%origin)
    !call mem_allocate(this%withdrawal, this%nstrm, 'WITHDRAWAL', this%origin)
    call mem_allocate(this%lauxvar, this%naux*this%nstrm, 'LAUXVAR', this%origin)
    !
    ! -- reach boundary and concentrations
    if (this%imatrows == 0) then
      call mem_allocate(this%iboundpak, this%nstrm, 'IBOUND', this%origin)
      call mem_allocate(this%xnewpak, this%nstrm, 'XNEWPAK', this%origin)
    end if
    call mem_allocate(this%xoldpak, this%nstrm, 'XOLDPAK', this%origin)
    !
    ! -- allocate character storage not managed by the memory manager
    allocate(this%streamname(this%nstrm)) ! ditch after boundnames allocated??
    !allocate(this%status(this%nstrm))
    !
    do n = 1, this%nstrm
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
    allocate(nboundchk(this%nstrm))
    do n = 1, this%nstrm
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

        if (n < 1 .or. n > this%nstrm) then
          write(errmsg,'(4x,a,1x,i6)') &
            '****ERROR. reachno MUST BE > 0 and <= ', this%nstrm
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

        ! -- streamname
        if (this%inamedbound /= 0) then
          call this%parser%GetStringCaps(bndNameTemp)
          if (bndNameTemp /= '') then
            bndName = bndNameTemp(1:16)
          endif
        end if
        this%streamname(n) = bndName

        ! -- fill time series aware data
        ! -- fill aux data
        do iaux = 1, this%naux
          !
          ! -- Assign boundary name
          if (this%inamedbound==1) then
            bndName = this%streamname(n)
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
      ! -- check for duplicate or missing reaches
      do n = 1, this%nstrm
        if (nboundchk(n) == 0) then
          write(errmsg,'(a,1x,i0)')  'ERROR.  NO DATA SPECIFIED FOR REACH', n
          call store_error(errmsg)
        else if (nboundchk(n) > 1) then
          write(errmsg,'(a,1x,i0,1x,a,1x,i0,1x,a)')                             &
            'ERROR.  DATA FOR REACH', n, 'SPECIFIED', nboundchk(n), 'TIMES'
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
  end subroutine sft_read_reaches
  
  subroutine sft_read_initial_attr(this)
! ******************************************************************************
! sft_read_initial_attr -- Read the initial parameters for this package
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use ConstantsModule, only: LINELENGTH
    use BudgetModule, only: budget_cr
    use TimeSeriesManagerModule, only: read_single_value_or_time_series
    ! -- dummy
    class(GwtSftType),intent(inout) :: this
    ! -- local
    !character(len=LINELENGTH) :: text
    integer(I4B) :: j, n
    !integer(I4B) :: nn
    !integer(I4B) :: idx
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

    !
    ! -- initialize xnewpak and set reach concentration
    ! -- todo: this should be a time series?
    do n = 1, this%nstrm
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
      !                                      this%streamname(n), this%inunit)

      ! -- todo: read aux
      
      ! -- todo: read boundname
      
    end do
    !
    ! -- initialize status (iboundpak) of reaches to active
    do n = 1, this%nstrm
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
      do j = 1, this%sfrbudptr%budterm(this%idxbudgwf)%nlist
        n = this%sfrbudptr%budterm(this%idxbudgwf)%id1(j)
        this%boundname(j) = this%streamname(n)
      end do
    end if
    !
    ! -- return
    return
  end subroutine sft_read_initial_attr

  subroutine sft_solve(this)
! ******************************************************************************
! sft_solve -- solve for concentration in the reaches
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use ConstantsModule, only: LINELENGTH
    use TdisModule, only: delt
    ! -- dummy
    class(GwtSftType) :: this
    ! -- local
    integer(I4B) :: n, j, igwfnode
    integer(I4B) :: n1, n2
    real(DP) :: rrate
    real(DP) :: ctmp
    real(DP) :: c1, qbnd, v0, v1
! ------------------------------------------------------------------------------
    !
    !
    ! -- first initialize dbuff with initial solute mass in reach
    do n = 1, this%nstrm
      call this%get_volumes(n, v1, v0, delt)
      c1 = this%xoldpak(n) * v0
      this%dbuff(n) = c1
    end do
    !
    ! -- add rainfall contribution
    if (this%idxbudrain /= 0) then
      do j = 1, this%sfrbudptr%budterm(this%idxbudrain)%nlist
        call this%sft_rain_term(j, n1, n2, rrate)
        this%dbuff(n1) = this%dbuff(n1) + rrate
      end do
    end if
    !
    ! -- add evaporation contribution
    if (this%idxbudevap /= 0) then
      do j = 1, this%sfrbudptr%budterm(this%idxbudevap)%nlist
        call this%sft_evap_term(j, n1, n2, rrate)
        this%dbuff(n1) = this%dbuff(n1) + rrate
      end do
    end if
    !
    ! -- add runoff contribution
    if (this%idxbudroff /= 0) then
      do j = 1, this%sfrbudptr%budterm(this%idxbudroff)%nlist
        call this%sft_roff_term(j, n1, n2, rrate)
        this%dbuff(n1) = this%dbuff(n1) + rrate
      end do
    end if
    !
    ! -- add inflow contribution
    if (this%idxbudiflw /= 0) then
      do j = 1, this%sfrbudptr%budterm(this%idxbudiflw)%nlist
        call this%sft_iflw_term(j, n1, n2, rrate)
        this%dbuff(n1) = this%dbuff(n1) + rrate
      end do
    end if
    !
    ! -- add outflow contribution
    if (this%idxbudoutf /= 0) then
      do j = 1, this%sfrbudptr%budterm(this%idxbudoutf)%nlist
        call this%sft_outf_term(j, n1, n2, rrate)
        this%dbuff(n1) = this%dbuff(n1) + rrate
      end do
    end if
    !
    ! -- add to mover contribution
    if (this%idxbudtmvr /= 0) then
      do j = 1, this%sfrbudptr%budterm(this%idxbudtmvr)%nlist
        call this%sft_tmvr_term(j, n1, n2, rrate)
        this%dbuff(n1) = this%dbuff(n1) + rrate
      end do
    end if
    !
    ! -- add from mover contribution
    if (this%idxbudfmvr /= 0) then
      do n1 = 1, size(this%qmfrommvr)
        rrate = this%qmfrommvr(n1)
        this%dbuff(n1) = this%dbuff(n1) + rrate
      end do
    end if
    !
    ! -- go through each gwf connection and accumulate 
    !    total mass in dbuff
    do j = 1, this%sfrbudptr%budterm(this%idxbudgwf)%nlist
      n = this%sfrbudptr%budterm(this%idxbudgwf)%id1(j)
      this%hcof(j) = DZERO
      this%rhs(j) = DZERO
      igwfnode = this%sfrbudptr%budterm(this%idxbudgwf)%id2(j)
      qbnd = this%sfrbudptr%budterm(this%idxbudgwf)%flow(j)
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
    ! -- go through each lak-lak connection and accumulate 
    !    total mass in dbuff mass
    if (this%idxbudfjf /= 0) then
      do j = 1, this%sfrbudptr%budterm(this%idxbudfjf)%nlist
        call this%sft_fjf_term(j, n1, n2, rrate)
        c1 = rrate * delt
        this%dbuff(n1) = this%dbuff(n1) + c1
      end do
    end if
    !
    ! -- Now divide total accumulated mass in reach by the reach volume
    do n = 1, this%nstrm
      call this%get_volumes(n, v1, v0, delt)
      c1 = this%dbuff(n) / v1
      if (this%iboundpak(n) > 0) then
        this%xnewpak(n) = c1
      end if
    end do
    !
    ! -- Return
    return
  end subroutine sft_solve
  
  subroutine sft_accumulate_ccterm(this, ilak, rrate, ccratin, ccratout)
! ******************************************************************************
! sft_accumulate_ccterm -- Accumulate constant concentration terms for budget.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(GwtSftType) :: this
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
      ! -- See if flow is into reach or out of reach.
      if (q < DZERO) then
        !
        ! -- Flow is out of reach subtract rate from ratout.
        ccratout = ccratout - q
      else
        !
        ! -- Flow is into reach; add rate to ratin.
        ccratin = ccratin + q
      end if
    end if
    ! -- return
    return
  end subroutine sft_accumulate_ccterm

  subroutine define_listlabel(this)
! ******************************************************************************
! define_listlabel -- Define the list heading that is written to iout when
!   PRINT_INPUT option is used.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    class(GwtSftType), intent(inout) :: this
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

  subroutine sft_set_pointers(this, neq, ibound, xnew, xold, flowja)
! ******************************************************************************
! set_pointers -- Set pointers to model arrays and variables so that a package
!                 has access to these things.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    class(GwtSftType) :: this
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
    ! -- Set the SFT pointers
    !
    ! -- set package pointers
    if (this%imatrows /= 0) then
      istart = this%dis%nodes + this%ioffset + 1
      iend = istart + this%nstrm - 1
      this%iboundpak => this%ibound(istart:iend)
      this%xnewpak => this%xnew(istart:iend)
    end if
    !
    ! -- return
  end subroutine sft_set_pointers
  
  subroutine get_volumes(this, ireach, vnew, vold, delt)
! ******************************************************************************
! get_volumes -- return the reach new volume and old volume
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(GwtSftType) :: this
    integer(I4B), intent(in) :: ireach
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
      qss = this%sfrbudptr%budterm(this%idxbudsto)%flow(ireach)
      vnew = this%sfrbudptr%budterm(this%idxbudsto)%auxvar(1, ireach)
      vold = vnew - qss * delt
    end if
    !
    ! -- Return
    return
  end subroutine get_volumes
  
  subroutine sft_setup_budobj(this)
! ******************************************************************************
! sft_setup_budobj -- Set up the budget object that stores all the reach flows
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use ConstantsModule, only: LENBUDTXT
    ! -- dummy
    class(GwtSftType) :: this
    ! -- local
    integer(I4B) :: nbudterm
    integer(I4B) :: nlen
    integer(I4B) :: n, n1, n2
    integer(I4B) :: maxlist, naux
    integer(I4B) :: idx
    real(DP) :: q
    character(len=LENBUDTXT) :: text
    character(len=LENBUDTXT), dimension(1) :: auxtxt
! ------------------------------------------------------------------------------
    !
    ! -- Determine the number of reach budget terms. These are fixed for 
    !    the simulation and cannot change
    nbudterm = 8
    nlen = 0
    if (this%idxbudfjf /= 0) then
      nlen = this%sfrbudptr%budterm(this%idxbudfjf)%maxlist
    end if
    if (nlen > 0) nbudterm = nbudterm + 1
    if (this%idxbudtmvr /= 0) nbudterm = nbudterm + 1
    if (this%idxbudfmvr /= 0) nbudterm = nbudterm + 1
    if (this%naux > 0) nbudterm = nbudterm + 1
    !
    ! -- set up budobj
    call budgetobject_cr(this%budobj, this%name)
    call this%budobj%budgetobject_df(this%nstrm, nbudterm, 0, 0)
    idx = 0
    !
    ! -- Go through and set up each budget term
    if (nlen > 0) then
      text = '    FLOW-JA-FACE'
      idx = idx + 1
      maxlist = this%sfrbudptr%budterm(this%idxbudfjf)%maxlist
      naux = 0
      call this%budobj%budterm(idx)%initialize(text, &
                                               this%name_model, &
                                               this%name, &
                                               this%name_model, &
                                               this%name, &
                                               maxlist, .false., .false., &
                                               naux)
      !
      ! -- store outlet connectivity
      call this%budobj%budterm(idx)%reset(maxlist)
      q = DZERO
      do n = 1, maxlist
        n1 = this%sfrbudptr%budterm(this%idxbudfjf)%id1(n)
        n2 = this%sfrbudptr%budterm(this%idxbudfjf)%id2(n)
        call this%budobj%budterm(idx)%update_term(n1, n2, q)
      end do      
    end if
    !
    ! -- 
    text = '             GWF'
    idx = idx + 1
    maxlist = this%sfrbudptr%budterm(this%idxbudgwf)%maxlist
    naux = 0
    call this%budobj%budterm(idx)%initialize(text, &
                                             this%name_model, &
                                             this%name, &
                                             this%name_model, &
                                             this%name_model, &
                                             maxlist, .false., .true., &
                                             naux)
    call this%budobj%budterm(idx)%reset(maxlist)
    q = DZERO
    do n = 1, maxlist
      n1 = this%sfrbudptr%budterm(this%idxbudgwf)%id1(n)
      n2 = this%sfrbudptr%budterm(this%idxbudgwf)%id2(n)
      call this%budobj%budterm(idx)%update_term(n1, n2, q)
    end do
    !
    ! -- 
    text = '        RAINFALL'
    idx = idx + 1
    maxlist = this%sfrbudptr%budterm(this%idxbudrain)%maxlist
    naux = 0
    call this%budobj%budterm(idx)%initialize(text, &
                                             this%name_model, &
                                             this%name, &
                                             this%name_model, &
                                             this%name, &
                                             maxlist, .false., .false., &
                                             naux)
    !
    ! -- 
    text = '     EVAPORATION'
    idx = idx + 1
    maxlist = this%sfrbudptr%budterm(this%idxbudevap)%maxlist
    naux = 0
    call this%budobj%budterm(idx)%initialize(text, &
                                             this%name_model, &
                                             this%name, &
                                             this%name_model, &
                                             this%name, &
                                             maxlist, .false., .false., &
                                             naux)
    !
    ! -- 
    text = '          RUNOFF'
    idx = idx + 1
    maxlist = this%sfrbudptr%budterm(this%idxbudroff)%maxlist
    naux = 0
    call this%budobj%budterm(idx)%initialize(text, &
                                             this%name_model, &
                                             this%name, &
                                             this%name_model, &
                                             this%name, &
                                             maxlist, .false., .false., &
                                             naux)
    !
    ! -- 
    text = '      EXT-INFLOW'
    idx = idx + 1
    maxlist = this%sfrbudptr%budterm(this%idxbudiflw)%maxlist
    naux = 0
    call this%budobj%budterm(idx)%initialize(text, &
                                             this%name_model, &
                                             this%name, &
                                             this%name_model, &
                                             this%name, &
                                             maxlist, .false., .false., &
                                             naux)
    !
    ! -- 
    text = '     EXT-OUTFLOW'
    idx = idx + 1
    maxlist = this%sfrbudptr%budterm(this%idxbudoutf)%maxlist
    naux = 0
    call this%budobj%budterm(idx)%initialize(text, &
                                             this%name_model, &
                                             this%name, &
                                             this%name_model, &
                                             this%name, &
                                             maxlist, .false., .false., &
                                             naux)
    !
    ! -- 
    text = '         STORAGE'
    idx = idx + 1
    maxlist = this%sfrbudptr%budterm(this%idxbudsto)%maxlist
    naux = 1
    auxtxt(1) = '            MASS'
    call this%budobj%budterm(idx)%initialize(text, &
                                             this%name_model, &
                                             this%name, &
                                             this%name_model, &
                                             this%name, &
                                             maxlist, .false., .false., &
                                             naux, auxtxt)
    if (this%idxbudtmvr /= 0) then
      !
      ! -- 
      text = '          TO-MVR'
      idx = idx + 1
      maxlist = this%sfrbudptr%budterm(this%idxbudtmvr)%maxlist
      naux = 0
      call this%budobj%budterm(idx)%initialize(text, &
                                               this%name_model, &
                                               this%name, &
                                               this%name_model, &
                                               this%name, &
                                               maxlist, .false., .false., &
                                               naux)
    end if
    if (this%idxbudfmvr /= 0) then
      !
      ! -- 
      text = '        FROM-MVR'
      idx = idx + 1
      maxlist = this%nstrm
      naux = 0
      call this%budobj%budterm(idx)%initialize(text, &
                                               this%name_model, &
                                               this%name, &
                                               this%name_model, &
                                               this%name, &
                                               maxlist, .false., .false., &
                                               naux)
    end if
    !
    ! -- 
    text = '        CONSTANT'
    idx = idx + 1
    maxlist = this%nstrm
    naux = 0
    call this%budobj%budterm(idx)%initialize(text, &
                                             this%name_model, &
                                             this%name, &
                                             this%name_model, &
                                             this%name, &
                                             maxlist, .false., .false., &
                                             naux)
    !
    ! -- 
    naux = this%naux
    if (naux > 0) then
      !
      ! -- 
      text = '       AUXILIARY'
      idx = idx + 1
      maxlist = this%nstrm
      call this%budobj%budterm(idx)%initialize(text, &
                                               this%name_model, &
                                               this%name, &
                                               this%name_model, &
                                               this%name, &
                                               maxlist, .false., .false., &
                                               naux, this%auxname)
    end if
    !
    ! -- if sft flow for each reach are written to the listing file
    if (this%iprflow /= 0) then
      call this%budobj%flowtable_df(this%iout, cellids='GWF')
    end if
    !
    ! -- return
    return
  end subroutine sft_setup_budobj

  subroutine sft_fill_budobj(this, x)
! ******************************************************************************
! sft_fill_budobj -- copy flow terms into this%budobj
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use TdisModule, only: delt
    ! -- dummy
    class(GwtSftType) :: this
    real(DP), dimension(:), intent(in) :: x
    ! -- local
    integer(I4B) :: naux
    real(DP), dimension(:), allocatable :: auxvartmp
    integer(I4B) :: i, j, n1, n2
    integer(I4B) :: ii
    integer(I4B) :: idx
    integer(I4B) :: nlen
    integer(I4B) :: nlist
    integer(I4B) :: igwfnode
    real(DP) :: q
    real(DP) :: v0, v1
    real(DP) :: ccratin, ccratout
    ! -- formats
! -----------------------------------------------------------------------------
    !
    ! -- initialize counter
    idx = 0
    !
    ! -- initialize ccterm, which is used to sum up all mass flows
    !    into a constant concentration cell
    ccratin = DZERO
    ccratout = DZERO
    do n1 = 1, this%nstrm
      this%ccterm(n1) = DZERO
    end do

    
    ! -- FLOW JA FACE
    nlen = 0
    if (this%idxbudfjf /= 0) then
      nlen = this%sfrbudptr%budterm(this%idxbudfjf)%maxlist
    end if
    if (nlen > 0) then
      idx = idx + 1
      nlist = this%sfrbudptr%budterm(this%idxbudfjf)%maxlist
      call this%budobj%budterm(idx)%reset(nlist)
      q = DZERO
      do j = 1, nlist
        call this%sft_fjf_term(j, n1, n2, q)
        call this%budobj%budterm(idx)%update_term(n1, n2, q)
        call this%sft_accumulate_ccterm(n1, q, ccratin, ccratout)
      end do      
    end if

    
    ! -- GWF (LEAKAGE)
    idx = idx + 1
    call this%budobj%budterm(idx)%reset(this%maxbound)
    do j = 1, this%sfrbudptr%budterm(this%idxbudgwf)%nlist
      q = DZERO
      n1 = this%sfrbudptr%budterm(this%idxbudgwf)%id1(j)
      if (this%iboundpak(n1) /= 0) then
        igwfnode = this%sfrbudptr%budterm(this%idxbudgwf)%id2(j)
        q = this%hcof(j) * x(igwfnode) - this%rhs(j)
        q = -q  ! flip sign so relative to reach
      end if
      call this%budobj%budterm(idx)%update_term(n1, igwfnode, q)
      call this%sft_accumulate_ccterm(n1, q, ccratin, ccratout)
    end do

    
    ! -- RAIN
    idx = idx + 1
    nlist = this%sfrbudptr%budterm(this%idxbudrain)%nlist
    call this%budobj%budterm(idx)%reset(nlist)
    do j = 1, nlist
      call this%sft_rain_term(j, n1, n2, q)
      call this%budobj%budterm(idx)%update_term(n1, n2, q)
      call this%sft_accumulate_ccterm(n1, q, ccratin, ccratout)
    end do
    
    
    ! -- EVAPORATION
    idx = idx + 1
    nlist = this%sfrbudptr%budterm(this%idxbudevap)%nlist
    call this%budobj%budterm(idx)%reset(nlist)
    do j = 1, nlist
      call this%sft_evap_term(j, n1, n2, q)
      call this%budobj%budterm(idx)%update_term(n1, n2, q)
      call this%sft_accumulate_ccterm(n1, q, ccratin, ccratout)
    end do
    
    
    ! -- RUNOFF
    idx = idx + 1
    nlist = this%sfrbudptr%budterm(this%idxbudroff)%nlist
    call this%budobj%budterm(idx)%reset(nlist)
    do j = 1, nlist
      call this%sft_roff_term(j, n1, n2, q)
      call this%budobj%budterm(idx)%update_term(n1, n2, q)
      call this%sft_accumulate_ccterm(n1, q, ccratin, ccratout)
    end do
    
    
    ! -- INFLOW
    idx = idx + 1
    nlist = this%sfrbudptr%budterm(this%idxbudiflw)%nlist
    call this%budobj%budterm(idx)%reset(nlist)
    do j = 1, nlist
      call this%sft_iflw_term(j, n1, n2, q)
      call this%budobj%budterm(idx)%update_term(n1, n2, q)
      call this%sft_accumulate_ccterm(n1, q, ccratin, ccratout)
    end do
    
    
    ! -- OUTFLOW
    idx = idx + 1
    nlist = this%sfrbudptr%budterm(this%idxbudoutf)%nlist
    call this%budobj%budterm(idx)%reset(nlist)
    do j = 1, nlist
      call this%sft_outf_term(j, n1, n2, q)
      call this%budobj%budterm(idx)%update_term(n1, n2, q)
      call this%sft_accumulate_ccterm(n1, q, ccratin, ccratout)
    end do
    
    
    ! -- MASS STORAGE
    idx = idx + 1
    call this%budobj%budterm(idx)%reset(this%nstrm)
    allocate(auxvartmp(1))
    do n1 = 1, this%nstrm
      call this%get_volumes(n1, v1, v0, delt)
      auxvartmp(1) = v1 * this%xnewpak(n1)
      q = this%qsto(n1)
      call this%budobj%budterm(idx)%update_term(n1, n1, q, auxvartmp)
      call this%sft_accumulate_ccterm(n1, q, ccratin, ccratout)
    end do
    deallocate(auxvartmp)
    
    
    ! -- TO MOVER
    if (this%idxbudtmvr /= 0) then
      idx = idx + 1
      nlist = this%sfrbudptr%budterm(this%idxbudtmvr)%nlist
      call this%budobj%budterm(idx)%reset(nlist)
      do j = 1, nlist
        call this%sft_tmvr_term(j, n1, n2, q)
        call this%budobj%budterm(idx)%update_term(n1, n2, q)
        call this%sft_accumulate_ccterm(n1, q, ccratin, ccratout)
      end do
    end if
    
    ! -- FROM MOVER
    if (this%idxbudfmvr /= 0) then
      idx = idx + 1
      nlist = this%nstrm
      call this%budobj%budterm(idx)%reset(nlist)
      do n1 = 1, nlist
        q = this%qmfrommvr(n1)
        call this%budobj%budterm(idx)%update_term(n1, n1, q)
        call this%sft_accumulate_ccterm(n1, q, ccratin, ccratout)
      end do
    end if
    
    ! -- CONSTANT FLOW
    idx = idx + 1
    call this%budobj%budterm(idx)%reset(this%nstrm)
    do n1 = 1, this%nstrm
      q = this%ccterm(n1)
      call this%budobj%budterm(idx)%update_term(n1, n1, q)
    end do
    
    
    ! -- AUXILIARY VARIABLES
    naux = this%naux
    if (naux > 0) then
      idx = idx + 1
      allocate(auxvartmp(naux))
      call this%budobj%budterm(idx)%reset(this%nstrm)
      do n1 = 1, this%nstrm
        q = DZERO
        do i = 1, naux
          ii = (n1 - 1) * naux + i
          auxvartmp(i) = this%lauxvar(ii)%value
        end do
        call this%budobj%budterm(idx)%update_term(n1, n1, q, auxvartmp)
      end do
      deallocate(auxvartmp)
    end if
    !
    ! --Terms are filled, now accumulate them for this time step
    call this%budobj%accumulate_terms()
    !
    ! -- return
    return
  end subroutine sft_fill_budobj

  subroutine sft_rain_term(this, ientry, n1, n2, rrate, &
                           rhsval, hcofval)
    class(GwtSftType) :: this
    integer(I4B), intent(in) :: ientry
    integer(I4B), intent(inout) :: n1
    integer(I4B), intent(inout) :: n2
    real(DP), intent(inout), optional :: rrate
    real(DP), intent(inout), optional :: rhsval
    real(DP), intent(inout), optional :: hcofval
    real(DP) :: qbnd
    real(DP) :: ctmp
    n1 = this%sfrbudptr%budterm(this%idxbudrain)%id1(ientry)
    n2 = this%sfrbudptr%budterm(this%idxbudrain)%id2(ientry)
    qbnd = this%sfrbudptr%budterm(this%idxbudrain)%flow(ientry)
    ctmp = this%concrain(n1)%value
    if (present(rrate)) rrate = ctmp * qbnd
    if (present(rhsval)) rhsval = -rrate
    if (present(hcofval)) hcofval = DZERO
    !
    ! -- return
    return
  end subroutine sft_rain_term
  
  subroutine sft_evap_term(this, ientry, n1, n2, rrate, &
                           rhsval, hcofval)
    class(GwtSftType) :: this
    integer(I4B), intent(in) :: ientry
    integer(I4B), intent(inout) :: n1
    integer(I4B), intent(inout) :: n2
    real(DP), intent(inout), optional :: rrate
    real(DP), intent(inout), optional :: rhsval
    real(DP), intent(inout), optional :: hcofval
    real(DP) :: qbnd
    real(DP) :: ctmp
    real(DP) :: omega
    n1 = this%sfrbudptr%budterm(this%idxbudevap)%id1(ientry)
    n2 = this%sfrbudptr%budterm(this%idxbudevap)%id2(ientry)
    ! -- note that qbnd is negative for evap
    qbnd = this%sfrbudptr%budterm(this%idxbudevap)%flow(ientry)
    ctmp = this%concevap(n1)%value
    if (this%xnewpak(n1) < ctmp) then
      omega = DONE
    else
      omega = DZERO
    end if
    if (present(rrate)) &
      rrate = omega * qbnd * this%xnewpak(n1) + &
              (DONE - omega) * qbnd * ctmp
    if (present(rhsval)) rhsval = - (DONE - omega) * qbnd * ctmp
    if (present(hcofval)) hcofval = omega * qbnd
    !
    ! -- return
    return
  end subroutine sft_evap_term
  
  subroutine sft_roff_term(this, ientry, n1, n2, rrate, &
                           rhsval, hcofval)
    class(GwtSftType) :: this
    integer(I4B), intent(in) :: ientry
    integer(I4B), intent(inout) :: n1
    integer(I4B), intent(inout) :: n2
    real(DP), intent(inout), optional :: rrate
    real(DP), intent(inout), optional :: rhsval
    real(DP), intent(inout), optional :: hcofval
    real(DP) :: qbnd
    real(DP) :: ctmp
    n1 = this%sfrbudptr%budterm(this%idxbudroff)%id1(ientry)
    n2 = this%sfrbudptr%budterm(this%idxbudroff)%id2(ientry)
    qbnd = this%sfrbudptr%budterm(this%idxbudroff)%flow(ientry)
    ctmp = this%concroff(n1)%value
    if (present(rrate)) rrate = ctmp * qbnd
    if (present(rhsval)) rhsval = -rrate
    if (present(hcofval)) hcofval = DZERO
    !
    ! -- return
    return
  end subroutine sft_roff_term
  
  subroutine sft_iflw_term(this, ientry, n1, n2, rrate, &
                           rhsval, hcofval)
    class(GwtSftType) :: this
    integer(I4B), intent(in) :: ientry
    integer(I4B), intent(inout) :: n1
    integer(I4B), intent(inout) :: n2
    real(DP), intent(inout), optional :: rrate
    real(DP), intent(inout), optional :: rhsval
    real(DP), intent(inout), optional :: hcofval
    real(DP) :: qbnd
    real(DP) :: ctmp
    n1 = this%sfrbudptr%budterm(this%idxbudiflw)%id1(ientry)
    n2 = this%sfrbudptr%budterm(this%idxbudiflw)%id2(ientry)
    qbnd = this%sfrbudptr%budterm(this%idxbudiflw)%flow(ientry)
    ctmp = this%conciflw(n1)%value
    if (present(rrate)) rrate = ctmp * qbnd
    if (present(rhsval)) rhsval = -rrate
    if (present(hcofval)) hcofval = DZERO
    !
    ! -- return
    return
  end subroutine sft_iflw_term
  
  subroutine sft_outf_term(this, ientry, n1, n2, rrate, &
                           rhsval, hcofval)
    class(GwtSftType) :: this
    integer(I4B), intent(in) :: ientry
    integer(I4B), intent(inout) :: n1
    integer(I4B), intent(inout) :: n2
    real(DP), intent(inout), optional :: rrate
    real(DP), intent(inout), optional :: rhsval
    real(DP), intent(inout), optional :: hcofval
    real(DP) :: qbnd
    real(DP) :: ctmp
    n1 = this%sfrbudptr%budterm(this%idxbudoutf)%id1(ientry)
    n2 = this%sfrbudptr%budterm(this%idxbudoutf)%id2(ientry)
    qbnd = this%sfrbudptr%budterm(this%idxbudoutf)%flow(ientry)
    ctmp = this%xnewpak(n1)
    if (present(rrate)) rrate = ctmp * qbnd
    if (present(rhsval)) rhsval = DZERO
    if (present(hcofval)) hcofval = qbnd
    !
    ! -- return
    return
  end subroutine sft_outf_term
  
  subroutine sft_tmvr_term(this, ientry, n1, n2, rrate, &
                           rhsval, hcofval)
    class(GwtSftType) :: this
    integer(I4B), intent(in) :: ientry
    integer(I4B), intent(inout) :: n1
    integer(I4B), intent(inout) :: n2
    real(DP), intent(inout), optional :: rrate
    real(DP), intent(inout), optional :: rhsval
    real(DP), intent(inout), optional :: hcofval
    real(DP) :: qbnd
    real(DP) :: ctmp
    n1 = this%sfrbudptr%budterm(this%idxbudtmvr)%id1(ientry)
    n2 = this%sfrbudptr%budterm(this%idxbudtmvr)%id2(ientry)
    qbnd = this%sfrbudptr%budterm(this%idxbudtmvr)%flow(ientry)
    ctmp = this%xnewpak(n1)
    if (present(rrate)) rrate = ctmp * qbnd
    if (present(rhsval)) rhsval = DZERO
    if (present(hcofval)) hcofval = qbnd
    !
    ! -- return
    return
  end subroutine sft_tmvr_term
  
  subroutine sft_fjf_term(this, ientry, n1, n2, rrate, &
                          rhsval, hcofval)
    class(GwtSftType) :: this
    integer(I4B), intent(in) :: ientry
    integer(I4B), intent(inout) :: n1
    integer(I4B), intent(inout) :: n2
    real(DP), intent(inout), optional :: rrate
    real(DP), intent(inout), optional :: rhsval
    real(DP), intent(inout), optional :: hcofval
    real(DP) :: qbnd
    real(DP) :: ctmp
    n1 = this%sfrbudptr%budterm(this%idxbudfjf)%id1(ientry)
    n2 = this%sfrbudptr%budterm(this%idxbudfjf)%id2(ientry)
    qbnd = this%sfrbudptr%budterm(this%idxbudfjf)%flow(ientry)
    if (qbnd <= 0) then
      ctmp = this%xnewpak(n1)
    else
      ctmp = this%xnewpak(n2)
    end if
    if (present(rrate)) rrate = ctmp * qbnd
    if (present(rhsval)) rhsval = -rrate
    if (present(hcofval)) hcofval = DZERO
    !
    ! -- return
    return
  end subroutine sft_fjf_term
  
  logical function sft_obs_supported(this)
! ******************************************************************************
! sft_obs_supported -- obs are supported?
!   -- Return true because LAK package supports observations.
!   -- Overrides BndType%bnd_obs_supported()
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(GwtSftType) :: this
! ------------------------------------------------------------------------------
    !
    ! -- Set to true
    sft_obs_supported = .true.
    !
    ! -- return
    return
  end function sft_obs_supported
  
  subroutine sft_df_obs(this)
! ******************************************************************************
! sft_df_obs -- obs are supported?
!   -- Store observation type supported by LAK package.
!   -- Overrides BndType%bnd_df_obs
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(GwtSftType) :: this
    ! -- local
    integer(I4B) :: indx
! ------------------------------------------------------------------------------
    !
    ! -- Store obs type and assign procedure pointer
    !    for stage observation type.
    call this%obs%StoreObsType('concentration', .false., indx)
    this%obs%obsData(indx)%ProcessIdPtr => sft_process_obsID
    !
    ! -- Store obs type and assign procedure pointer
    !    for ext-inflow observation type.
    call this%obs%StoreObsType('ext-inflow', .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => sft_process_obsID
    !
    ! -- Store obs type and assign procedure pointer
    !    for outlet-inflow observation type.
    call this%obs%StoreObsType('outlet-inflow', .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => sft_process_obsID
    !
    ! -- Store obs type and assign procedure pointer
    !    for inflow observation type.
    call this%obs%StoreObsType('inflow', .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => sft_process_obsID
    !
    ! -- Store obs type and assign procedure pointer
    !    for from-mvr observation type.
    call this%obs%StoreObsType('from-mvr', .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => sft_process_obsID
    !
    ! -- Store obs type and assign procedure pointer
    !    for rainfall observation type.
    call this%obs%StoreObsType('rainfall', .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => sft_process_obsID
    !
    ! -- Store obs type and assign procedure pointer
    !    for runoff observation type.
    call this%obs%StoreObsType('runoff', .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => sft_process_obsID
    !
    ! -- Store obs type and assign procedure pointer
    !    for lak observation type.
    call this%obs%StoreObsType('sft', .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => sft_process_obsID
    !
    ! -- Store obs type and assign procedure pointer
    !    for evaporation observation type.
    call this%obs%StoreObsType('evaporation', .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => sft_process_obsID
    !
    ! -- Store obs type and assign procedure pointer
    !    for withdrawal observation type.
    call this%obs%StoreObsType('withdrawal', .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => sft_process_obsID
    !
    ! -- Store obs type and assign procedure pointer
    !    for ext-outflow observation type.
    call this%obs%StoreObsType('ext-outflow', .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => sft_process_obsID
    !
    ! -- Store obs type and assign procedure pointer
    !    for to-mvr observation type.
    call this%obs%StoreObsType('to-mvr', .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => sft_process_obsID
    !
    ! -- Store obs type and assign procedure pointer
    !    for storage observation type.
    call this%obs%StoreObsType('storage', .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => sft_process_obsID
    !
    ! -- Store obs type and assign procedure pointer
    !    for constant observation type.
    call this%obs%StoreObsType('constant', .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => sft_process_obsID
    !
    ! -- Store obs type and assign procedure pointer
    !    for outlet observation type.
    call this%obs%StoreObsType('outlet', .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => sft_process_obsID
    !
    ! -- Store obs type and assign procedure pointer
    !    for volume observation type.
    call this%obs%StoreObsType('volume', .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => sft_process_obsID
    !
    return
  end subroutine sft_df_obs
  
  subroutine sft_rp_obs(this)
! ******************************************************************************
! sft_rp_obs -- 
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(GwtSftType), intent(inout) :: this
    ! -- local
    integer(I4B) :: i, j, n, nn1, nn2, idx
    character(len=200) :: ermsg
    character(len=LENBOUNDNAME) :: bname
    logical :: jfound
    class(ObserveType), pointer :: obsrv => null()
! ------------------------------------------------------------------------------
    ! -- formats
10  format('Error: Boundary "',a,'" for observation "',a, &
           '" is invalid in package "',a,'"')
    !
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
          ! -- Observation reach is based on a boundary name.
          !    Iterate through all reaches to identify and store
          !    corresponding index in bound array.
          jfound = .false.
          if (obsrv%ObsTypeId=='SFT') then
            do j = 1, this%sfrbudptr%budterm(this%idxbudgwf)%nlist
              !n1 = this%sfrbudptr%budterm(this%idxbudgwf)%id1(j)
              if (this%boundname(j) == bname) then
                jfound = .true.
                call ExpandArray(obsrv%indxbnds)
                n = size(obsrv%indxbnds)
                obsrv%indxbnds(n) = j
              end if
            end do
          else if (obsrv%ObsTypeId=='EXT-OUTFLOW' .or.   &
                   obsrv%ObsTypeId=='TO-MVR') then
            
            ! -- todo: need to get this working for outlet flows
            !do j = 1, this%noutlets
            !  jj = this%lakein(j)
            !  if (this%streamname(jj) == bname) then
            !    jfound = .true.
            !    call ExpandArray(obsrv%indxbnds)
            !    n = size(obsrv%indxbnds)
            !    obsrv%indxbnds(n) = j
            !  end if
            !end do
            
          else
            do j = 1, this%nstrm
              if (this%streamname(j) == bname) then
                jfound = .true.
                call ExpandArray(obsrv%indxbnds)
                n = size(obsrv%indxbnds)
                obsrv%indxbnds(n) = j
              end if
            end do
          end if
          if (.not. jfound) then
            write(ermsg,10) trim(bname), trim(obsrv%Name), trim(this%name)
            call store_error(ermsg)
          end if
        end if
      else
        call ExpandArray(obsrv%indxbnds)
        n = size(obsrv%indxbnds)
        if (n == 1) then
          if (obsrv%ObsTypeId=='SFT') then
            nn2 = obsrv%NodeNumber2
            ! -- Look for the first occurrence of nn1, then set indxbnds
            !    to the nn2 record after that
            do j = 1, this%sfrbudptr%budterm(this%idxbudgwf)%nlist
              if (this%sfrbudptr%budterm(this%idxbudgwf)%id1(j) == nn1) then
                idx = j + nn2 - 1
                obsrv%indxbnds(1) = idx
                exit
              end if
            end do
            if (this%sfrbudptr%budterm(this%idxbudgwf)%id1(idx) /= nn1) then
              write (ermsg, '(4x,a,1x,a,1x,a,1x,i0,1x,a,1x,i0,1x,a)') &
                'ERROR:', trim(adjustl(obsrv%ObsTypeId)), &
                ' reach connection number =', nn2, &
                '(does not correspond to reach ', nn1, ')'
              call store_error(ermsg)
            end if
          else
            obsrv%indxbnds(1) = nn1
          end if
        else
          ermsg = 'Programming error in sft_rp_obs'
          call store_error(ermsg)
        endif
      end if
      !
      ! -- catch non-cumulative observation assigned to observation defined
      !    by a boundname that is assigned to more than one element
      if (obsrv%ObsTypeId == 'CONCENTRATION') then
        n = size(obsrv%indxbnds)
        if (n > 1) then
          write (ermsg, '(4x,a,4(1x,a))') &
            'ERROR:', trim(adjustl(obsrv%ObsTypeId)), &
            'for observation', trim(adjustl(obsrv%Name)), &
            ' must be assigned to a reach with a unique boundname.'
          call store_error(ermsg)
        end if
      end if
      !
      ! -- check that index values are valid
      if (obsrv%ObsTypeId=='TO-MVR' .or. &
          obsrv%ObsTypeId=='EXT-OUTFLOW' .or. &
          obsrv%ObsTypeId=='OUTLET') then
        do j = 1, size(obsrv%indxbnds)
          nn1 =  obsrv%indxbnds(j)
          ! -- todo: how do we replace noutlets here?
          !if (nn1 < 1 .or. nn1 > this%noutlets) then
          !  write (ermsg, '(4x,a,1x,a,1x,a,1x,i0,1x,a,1x,i0,1x,a)') &
          !    'ERROR:', trim(adjustl(obsrv%ObsTypeId)), &
          !    ' outlet must be > 0 and <=', this%noutlets, &
          !    '(specified value is ', nn1, ')'
            call store_error(ermsg)
          !end if
        end do
      else if (obsrv%ObsTypeId=='SFT') then
        do j = 1, size(obsrv%indxbnds)
          nn1 =  obsrv%indxbnds(j)
          if (nn1 < 1 .or. nn1 > this%maxbound) then
            write (ermsg, '(4x,a,1x,a,1x,a,1x,i0,1x,a,1x,i0,1x,a)') &
              'ERROR:', trim(adjustl(obsrv%ObsTypeId)), &
              ' reach connection number must be > 0 and <=', this%maxbound, &
              '(specified value is ', nn1, ')'
            call store_error(ermsg)
          end if
        end do
      else
        do j = 1, size(obsrv%indxbnds)
          nn1 =  obsrv%indxbnds(j)
          if (nn1 < 1 .or. nn1 > this%nstrm) then
            write (ermsg, '(4x,a,1x,a,1x,a,1x,i0,1x,a,1x,i0,1x,a)') &
              'ERROR:', trim(adjustl(obsrv%ObsTypeId)), &
              ' reach must be > 0 and <=', this%nstrm, &
              '(specified value is ', nn1, ')'
            call store_error(ermsg)
          end if
        end do
      end if
    end do
    if (count_errors() > 0) call ustop()
    !
    return
  end subroutine sft_rp_obs
  
  subroutine sft_bd_obs(this)
! ******************************************************************************
! sft_bd_obs -- 
!   -- Calculate observations this time step and call
!      ObsType%SaveOneSimval for each GwtSftType observation.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(GwtSftType), intent(inout) :: this
    ! -- local
    integer(I4B) :: i, igwfnode, j, jj, n, nn
    integer(I4B) :: n1, n2
    real(DP) :: v
    character(len=100) :: errmsg
    type(ObserveType), pointer :: obsrv => null()
! ------------------------------------------------------------------------------
    !
    ! -- Write simulated values for all LAK observations
    if (this%obs%npakobs > 0) then
      call this%obs%obs_bd_clear()
      do i = 1, this%obs%npakobs
        obsrv => this%obs%pakobs(i)%obsrv
        nn = size(obsrv%indxbnds)
        do j = 1, nn
          v = DNODATA
          jj = obsrv%indxbnds(j)
          select case (obsrv%ObsTypeId)
            case ('CONCENTRATION')
              if (this%iboundpak(jj) /= 0) then
                v = this%xnewpak(jj)
              end if
            case ('EXT-INFLOW')
              if (this%iboundpak(jj) /= 0) then
                call this%sft_iflw_term(jj, n1, n2, v)
              end if
            !case ('OUTLET-INFLOW')
            !  if (this%iboundpak(jj) /= 0) then
            !    call this%lak_calculate_outlet_inflow(jj, v)
            !  end if
            !case ('INFLOW')
            !  if (this%iboundpak(jj) /= 0) then
            !    call this%lak_calculate_inflow(jj, v)
            !    call this%lak_calculate_outlet_inflow(jj, v2)
            !    v = v + v2
            !  end if
            !case ('FROM-MVR')
            !  if (this%iboundpak(jj) /= 0) then
            !    if (this%imover == 1) then
            !      v = this%pakmvrobj%get_qfrommvr(jj)
            !    end if
            !  end if
            case ('RAINFALL')
              if (this%iboundpak(jj) /= 0) then
                call this%sft_rain_term(jj, n1, n2, v)
              end if
            case ('RUNOFF')
              if (this%iboundpak(jj) /= 0) then
                call this%sft_rain_term(jj, n1, n2, v)
              end if
            case ('SFT')
              n = this%sfrbudptr%budterm(this%idxbudgwf)%id1(jj)
              if (this%iboundpak(n) /= 0) then
                igwfnode = this%sfrbudptr%budterm(this%idxbudgwf)%id2(jj)
                v = this%hcof(jj) * this%xnew(igwfnode) - this%rhs(jj)
                v = -v
              end if
            case ('EVAPORATION')
              if (this%iboundpak(jj) /= 0) then
                call this%sft_evap_term(jj, n1, n2, v)
              end if
            !case ('EXT-OUTFLOW')
            !  n = this%lakein(jj)
            !  if (this%iboundpak(n) /= 0) then
            !    if (this%lakeout(jj) == 0) then
            !      v = this%simoutrate(jj)
            !      if (v < DZERO) then
            !        if (this%imover == 1) then
            !          v = v + this%pakmvrobj%get_qtomvr(jj)
            !        end if
            !      end if
            !    end if
            !  end if
            !case ('TO-MVR')
            !  n = this%lakein(jj)
            !  if (this%iboundpak(n) /= 0) then
            !    if (this%imover == 1) then
            !      v = this%pakmvrobj%get_qtomvr(jj)
            !      if (v > DZERO) then
            !        v = -v
            !      end if
            !    end if
            !  end if
            case ('STORAGE')
              if (this%iboundpak(jj) /= 0) then
                v = this%qsto(jj)
              end if
            case ('CONSTANT')
              if (this%iboundpak(jj) /= 0) then
                v = this%ccterm(jj)
              end if
            !case ('OUTLET')
            !  n = this%lakein(jj)
            !  if (this%iboundpak(jj) /= 0) then
            !    v = this%simoutrate(jj)
            !    !if (this%imover == 1) then
            !    !  v = v + this%pakmvrobj%get_qtomvr(jj)
            !    !end if
            !  end if
            !case ('VOLUME')
            !  if (this%iboundpak(jj) /= 0) then
            !    call this%lak_calculate_vol(jj, this%xnewpak(jj), v)
            !  end if
            case default
              errmsg = 'Error: Unrecognized observation type: ' // &
                        trim(obsrv%ObsTypeId)
              call store_error(errmsg)
              call ustop()
          end select
          call this%obs%SaveOneSimval(obsrv, v)
        end do
      end do
    end if
    !
    return
  end subroutine sft_bd_obs

  subroutine sft_process_obsID(obsrv, dis, inunitobs, iout)
! ******************************************************************************
! sft_process_obsID --
! -- This procedure is pointed to by ObsDataType%ProcesssIdPtr. It processes
!    the ID string of an observation definition for LAK package observations.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    type(ObserveType), intent(inout) :: obsrv
    class(DisBaseType), intent(in)    :: dis
    integer(I4B), intent(in)    :: inunitobs
    integer(I4B), intent(in)    :: iout
    ! -- local
    integer(I4B) :: nn1, nn2
    integer(I4B) :: icol, istart, istop
    character(len=LINELENGTH) :: strng
    character(len=LENBOUNDNAME) :: bndname
    ! -- formats
! ------------------------------------------------------------------------------
    !
    strng = obsrv%IDstring
    ! -- Extract reach number from strng and store it.
    !    If 1st item is not an integer(I4B), it should be a
    !    reach name--deal with it.
    icol = 1
    ! -- get reach number or boundary name
    call extract_idnum_or_bndname(strng, icol, istart, istop, nn1, bndname)
    if (nn1 == NAMEDBOUNDFLAG) then
      obsrv%FeatureName = bndname
    else
      if (obsrv%ObsTypeId=='SFT') then
        call extract_idnum_or_bndname(strng, icol, istart, istop, nn2, bndname)
        if (nn2 == NAMEDBOUNDFLAG) then
          obsrv%FeatureName = bndname
          ! -- reset nn1
          nn1 = nn2
        else
          obsrv%NodeNumber2 = nn2
        end if
        !! -- store connection number (NodeNumber2)
        !obsrv%NodeNumber2 = nn2
      endif
    endif
    ! -- store reach number (NodeNumber)
    obsrv%NodeNumber = nn1
    !
    return
  end subroutine sft_process_obsID

end module GwtSftModule