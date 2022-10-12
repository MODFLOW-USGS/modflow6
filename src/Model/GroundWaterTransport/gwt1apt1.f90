! -- Advanced Package Transport Module
! -- This module contains most of the routines for simulating transport
! -- through the advanced packages.
! -- Future work:
!      * support decay, sorption
!      * dispersion in SFT and UZT?
!
! AFP flows (flowbudptr)    index var     ATP term              Transport Type
!---------------------------------------------------------------------------------

! -- specialized terms in the flow budget
! FLOW-JA-FACE              idxbudfjf     FLOW-JA-FACE          cv2cv
! GWF (aux FLOW-AREA)       idxbudgwf     GWF                   cv2gwf
! STORAGE (aux VOLUME)      idxbudsto     none                  used for cv volumes
! FROM-MVR                  idxbudfmvr    FROM-MVR              q * cext = this%qfrommvr(:)
! TO-MVR                    idxbudtmvr    TO-MVR                q * cfeat

! -- generalized source/sink terms (except ET?)
! RAINFALL                  idxbudrain    RAINFALL              q * crain
! EVAPORATION               idxbudevap    EVAPORATION           cfeat<cevap: q*cfeat, else: q*cevap
! RUNOFF                    idxbudroff    RUNOFF                q * croff
! EXT-INFLOW                idxbudiflw    EXT-INFLOW            q * ciflw
! WITHDRAWAL                idxbudwdrl    WITHDRAWAL            q * cfeat
! EXT-OUTFLOW               idxbudoutf    EXT-OUTFLOW           q * cfeat

! -- terms from a flow file that should be skipped
! CONSTANT                  none          none                  none
! AUXILIARY                 none          none                  none

! -- terms that are written to the transport budget file
! none                      none          STORAGE (aux MASS)    dM/dt
! none                      none          AUXILIARY             none
! none                      none          CONSTANT              accumulate
!
!
module GwtAptModule

  use KindModule, only: DP, I4B, LGP
  use ConstantsModule, only: DZERO, DONE, DEP20, LENFTYPE, LINELENGTH, &
                             LENBOUNDNAME, LENPACKAGENAME, NAMEDBOUNDFLAG, &
                             DNODATA, TABLEFT, TABCENTER, TABRIGHT, &
                             TABSTRING, TABUCSTRING, TABINTEGER, TABREAL, &
                             LENAUXNAME
  use SimModule, only: store_error, store_error_unit, count_errors
  use SimVariablesModule, only: errmsg
  use BndModule, only: BndType
  use GwtFmiModule, only: GwtFmiType
  use BudgetObjectModule, only: BudgetObjectType, budgetobject_cr
  use BudgetTermModule, only: BudgetTermType
  use TableModule, only: TableType, table_cr
  use ObserveModule, only: ObserveType
  use InputOutputModule, only: extract_idnum_or_bndname
  use BaseDisModule, only: DisBaseType

  implicit none

  public :: GwtAptType
  public :: apt_process_obsID
  public :: apt_process_obsID12

  character(len=LENFTYPE) :: ftype = 'APT'
  character(len=16) :: text = '             APT'

  type, extends(BndType) :: GwtAptType

    character(len=LENPACKAGENAME) :: flowpackagename = '' !< name of corresponding flow package
    character(len=8), &
      dimension(:), pointer, contiguous :: status => null() !< active, inactive, constant
    character(len=LENAUXNAME) :: cauxfpconc = '' !< name of aux column in flow package auxvar array for concentration
    integer(I4B), pointer :: iauxfpconc => null() !< column in flow package bound array to insert concs
    integer(I4B), pointer :: imatrows => null() !< if active, add new rows to matrix
    integer(I4B), pointer :: iprconc => null() !< print conc to listing file
    integer(I4B), pointer :: iconcout => null() !< unit number for conc output file
    integer(I4B), pointer :: ibudgetout => null() !< unit number for budget output file
    integer(I4B), pointer :: ibudcsv => null() !< unit number for csv budget output file
    integer(I4B), pointer :: ncv => null() !< number of control volumes
    integer(I4B), pointer :: igwfaptpak => null() !< package number of corresponding this package
    real(DP), dimension(:), pointer, contiguous :: strt => null() !< starting feature concentration
    integer(I4B), dimension(:), pointer, contiguous :: idxlocnode => null() !< map position in global rhs and x array of pack entry
    integer(I4B), dimension(:), pointer, contiguous :: idxpakdiag => null() !< map diag position of feature in global amat
    integer(I4B), dimension(:), pointer, contiguous :: idxdglo => null() !< map position in global array of package diagonal row entries
    integer(I4B), dimension(:), pointer, contiguous :: idxoffdglo => null() !< map position in global array of package off diagonal row entries
    integer(I4B), dimension(:), pointer, contiguous :: idxsymdglo => null() !< map position in global array of package diagonal entries to model rows
    integer(I4B), dimension(:), pointer, contiguous :: idxsymoffdglo => null() !< map position in global array of package off diagonal entries to model rows
    integer(I4B), dimension(:), pointer, contiguous :: idxfjfdglo => null() !< map diagonal feature to feature in global amat
    integer(I4B), dimension(:), pointer, contiguous :: idxfjfoffdglo => null() !< map off diagonal feature to feature in global amat
    integer(I4B), dimension(:), pointer, contiguous :: iboundpak => null() !< package ibound
    real(DP), dimension(:), pointer, contiguous :: xnewpak => null() !< feature concentration for current time step
    real(DP), dimension(:), pointer, contiguous :: xoldpak => null() !< feature concentration from previous time step
    real(DP), dimension(:), pointer, contiguous :: dbuff => null() !< temporary storage array
    character(len=LENBOUNDNAME), &
      dimension(:), pointer, contiguous :: featname => null()
    real(DP), dimension(:), pointer, contiguous :: concfeat => null() !< concentration of the feature
    real(DP), dimension(:, :), pointer, contiguous :: lauxvar => null() !< auxiliary variable
    type(GwtFmiType), pointer :: fmi => null() !< pointer to fmi object
    real(DP), dimension(:), pointer, contiguous :: qsto => null() !< mass flux due to storage change
    real(DP), dimension(:), pointer, contiguous :: ccterm => null() !< mass flux required to maintain constant concentration
    integer(I4B), pointer :: idxbudfjf => null() !< index of flow ja face in flowbudptr
    integer(I4B), pointer :: idxbudgwf => null() !< index of gwf terms in flowbudptr
    integer(I4B), pointer :: idxbudsto => null() !< index of storage terms in flowbudptr
    integer(I4B), pointer :: idxbudtmvr => null() !< index of to mover terms in flowbudptr
    integer(I4B), pointer :: idxbudfmvr => null() !< index of from mover terms in flowbudptr
    integer(I4B), pointer :: idxbudaux => null() !< index of auxiliary terms in flowbudptr
    integer(I4B), dimension(:), pointer, contiguous :: idxbudssm => null() !< flag that flowbudptr%buditem is a general solute source/sink
    integer(I4B), pointer :: nconcbudssm => null() !< number of concbudssm terms (columns)
    real(DP), dimension(:, :), pointer, contiguous :: concbudssm => null() !< user specified concentrations for flow terms
    real(DP), dimension(:), pointer, contiguous :: qmfrommvr => null() !< a mass flow coming from the mover that needs to be added
    !
    ! -- pointer to flow package boundary
    type(BndType), pointer :: flowpackagebnd => null()
    !
    ! -- budget objects
    type(BudgetObjectType), pointer :: budobj => null() !< apt solute budget object
    type(BudgetObjectType), pointer :: flowbudptr => null() !< GWF flow budget object
    !
    ! -- table objects
    type(TableType), pointer :: dvtab => null()

  contains

    procedure :: set_pointers => apt_set_pointers
    procedure :: bnd_ac => apt_ac
    procedure :: bnd_mc => apt_mc
    procedure :: bnd_ar => apt_ar
    procedure :: bnd_rp => apt_rp
    procedure :: bnd_ad => apt_ad
    procedure :: bnd_cf => apt_cf
    procedure :: bnd_fc => apt_fc
    procedure, private :: apt_fc_expanded
    procedure :: pak_fc_expanded
    procedure, private :: apt_fc_nonexpanded
    procedure, private :: apt_cfupdate
    procedure :: apt_check_valid
    procedure :: apt_set_stressperiod
    procedure :: pak_set_stressperiod
    procedure :: apt_accumulate_ccterm
    procedure :: bnd_cq => apt_cq
    procedure :: bnd_ot_package_flows => apt_ot_package_flows
    procedure :: bnd_ot_dv => apt_ot_dv
    procedure :: bnd_ot_bdsummary => apt_ot_bdsummary
    procedure :: bnd_da => apt_da
    procedure :: allocate_scalars
    procedure :: apt_allocate_index_arrays
    procedure :: apt_allocate_arrays
    procedure :: find_apt_package
    procedure :: apt_solve
    procedure :: pak_solve
    procedure :: bnd_options => apt_options
    procedure :: read_dimensions => apt_read_dimensions
    procedure :: apt_read_cvs
    procedure :: read_initial_attr => apt_read_initial_attr
    procedure :: define_listlabel
    ! -- methods for observations
    procedure :: bnd_obs_supported => apt_obs_supported
    procedure :: bnd_df_obs => apt_df_obs
    procedure :: pak_df_obs
    procedure :: pak_rp_obs
    procedure :: bnd_rp_obs => apt_rp_obs
    procedure :: rp_obs_byfeature
    procedure :: rp_obs_budterm
    procedure :: rp_obs_flowjaface
    procedure :: bnd_bd_obs => apt_bd_obs
    procedure :: pak_bd_obs
    procedure :: get_volumes
    procedure :: pak_get_nbudterms
    procedure :: apt_setup_budobj
    procedure :: pak_setup_budobj
    procedure :: apt_fill_budobj
    procedure :: pak_fill_budobj
    procedure, private :: apt_stor_term
    procedure, private :: apt_tmvr_term
    procedure, private :: apt_fjf_term
    procedure, private :: apt_copy2flowp
    procedure, private :: apt_setup_tableobj

  end type GwtAptType

contains

  subroutine apt_ac(this, moffset, sparse)
! ******************************************************************************
! bnd_ac -- Add package connection to matrix
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use MemoryManagerModule, only: mem_setptr
    use SparseModule, only: sparsematrix
    ! -- dummy
    class(GwtAptType), intent(inout) :: this
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
      do n = 1, this%ncv
        nglo = moffset + this%dis%nodes + this%ioffset + n
        call sparse%addconnection(nglo, nglo, 1)
      end do
      !
      ! -- apt-gwf connections
      do i = 1, this%flowbudptr%budterm(this%idxbudgwf)%nlist
        n = this%flowbudptr%budterm(this%idxbudgwf)%id1(i)
        jj = this%flowbudptr%budterm(this%idxbudgwf)%id2(i)
        nglo = moffset + this%dis%nodes + this%ioffset + n
        jglo = jj + moffset
        call sparse%addconnection(nglo, jglo, 1)
        call sparse%addconnection(jglo, nglo, 1)
      end do
      !
      ! -- apt-apt connections
      if (this%idxbudfjf /= 0) then
        do i = 1, this%flowbudptr%budterm(this%idxbudfjf)%maxlist
          n = this%flowbudptr%budterm(this%idxbudfjf)%id1(i)
          jj = this%flowbudptr%budterm(this%idxbudfjf)%id2(i)
          nglo = moffset + this%dis%nodes + this%ioffset + n
          jglo = moffset + this%dis%nodes + this%ioffset + jj
          call sparse%addconnection(nglo, jglo, 1)
        end do
      end if
    end if
    !
    ! -- return
    return
  end subroutine apt_ac

  subroutine apt_mc(this, moffset, iasln, jasln)
! ******************************************************************************
! apt_mc -- map package connection to matrix
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use SparseModule, only: sparsematrix
    ! -- dummy
    class(GwtAptType), intent(inout) :: this
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
    ! -- allocate memory for index arrays
    call this%apt_allocate_index_arrays()
    !
    ! -- store index positions
    if (this%imatrows /= 0) then
      !
      ! -- Find the position of each connection in the global ia, ja structure
      !    and store them in idxglo.  idxglo allows this model to insert or
      !    retrieve values into or from the global A matrix
      ! -- apt rows
      do n = 1, this%ncv
        this%idxlocnode(n) = this%dis%nodes + this%ioffset + n
        iglo = moffset + this%dis%nodes + this%ioffset + n
        this%idxpakdiag(n) = iasln(iglo)
      end do
      do ipos = 1, this%flowbudptr%budterm(this%idxbudgwf)%nlist
        n = this%flowbudptr%budterm(this%idxbudgwf)%id1(ipos)
        j = this%flowbudptr%budterm(this%idxbudgwf)%id2(ipos)
        iglo = moffset + this%dis%nodes + this%ioffset + n
        jglo = j + moffset
        searchloop: do jj = iasln(iglo), iasln(iglo + 1) - 1
          if (jglo == jasln(jj)) then
            this%idxdglo(ipos) = iasln(iglo)
            this%idxoffdglo(ipos) = jj
            exit searchloop
          end if
        end do searchloop
      end do
      !
      ! -- apt contributions to gwf portion of global matrix
      do ipos = 1, this%flowbudptr%budterm(this%idxbudgwf)%nlist
        n = this%flowbudptr%budterm(this%idxbudgwf)%id1(ipos)
        j = this%flowbudptr%budterm(this%idxbudgwf)%id2(ipos)
        iglo = j + moffset
        jglo = moffset + this%dis%nodes + this%ioffset + n
        symsearchloop: do jj = iasln(iglo), iasln(iglo + 1) - 1
          if (jglo == jasln(jj)) then
            this%idxsymdglo(ipos) = iasln(iglo)
            this%idxsymoffdglo(ipos) = jj
            exit symsearchloop
          end if
        end do symsearchloop
      end do
      !
      ! -- apt-apt contributions to gwf portion of global matrix
      if (this%idxbudfjf /= 0) then
        do ipos = 1, this%flowbudptr%budterm(this%idxbudfjf)%nlist
          n = this%flowbudptr%budterm(this%idxbudfjf)%id1(ipos)
          j = this%flowbudptr%budterm(this%idxbudfjf)%id2(ipos)
          iglo = moffset + this%dis%nodes + this%ioffset + n
          jglo = moffset + this%dis%nodes + this%ioffset + j
          fjfsearchloop: do jj = iasln(iglo), iasln(iglo + 1) - 1
            if (jglo == jasln(jj)) then
              this%idxfjfdglo(ipos) = iasln(iglo)
              this%idxfjfoffdglo(ipos) = jj
              exit fjfsearchloop
            end if
          end do fjfsearchloop
        end do
      end if
    end if
    !
    ! -- return
    return
  end subroutine apt_mc

  subroutine apt_ar(this)
! ******************************************************************************
! apt_ar -- Allocate and Read
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(GwtAptType), intent(inout) :: this
    ! -- local
    integer(I4B) :: j
    logical :: found
    ! -- formats
    character(len=*), parameter :: fmtapt = &
      "(1x,/1x,'APT -- ADVANCED PACKAGE TRANSPORT, VERSION 1, 3/5/2020', &
      &' INPUT READ FROM UNIT ', i0, //)"
! ------------------------------------------------------------------------------
    !
    ! -- Get obs setup
    call this%obs%obs_ar()
    !
    ! --print a message identifying the apt package.
    write (this%iout, fmtapt) this%inunit
    !
    ! -- Allocate arrays
    call this%apt_allocate_arrays()
    !
    ! -- read optional initial package parameters
    call this%read_initial_attr()
    !
    ! -- Find the package index in the GWF model or GWF budget file
    !    for the corresponding apt flow package
    call this%fmi%get_package_index(this%flowpackagename, this%igwfaptpak)
    !
    ! -- Tell fmi that this package is being handled by APT, otherwise
    !    SSM would handle the flows into GWT from this pack.  Then point the
    !    fmi data for an advanced package to xnewpak and qmfrommvr
    this%fmi%iatp(this%igwfaptpak) = 1
    this%fmi%datp(this%igwfaptpak)%concpack => this%xnewpak
    this%fmi%datp(this%igwfaptpak)%qmfrommvr => this%qmfrommvr
    !
    ! -- If there is an associated flow package and the user wishes to put
    !    simulated concentrations into a aux variable column, then find
    !    the column number.
    if (associated(this%flowpackagebnd)) then
      if (this%cauxfpconc /= '') then
        found = .false.
        do j = 1, this%flowpackagebnd%naux
          if (this%flowpackagebnd%auxname(j) == this%cauxfpconc) then
            this%iauxfpconc = j
            found = .true.
            exit
          end if
        end do
        if (this%iauxfpconc == 0) then
          errmsg = 'COULD NOT FIND AUXILIARY VARIABLE '// &
                   trim(adjustl(this%cauxfpconc))//' IN FLOW PACKAGE '// &
                   trim(adjustl(this%flowpackagename))
          call store_error(errmsg)
          call this%parser%StoreErrorUnit()
        else
          ! -- tell package not to update this auxiliary variable
          this%flowpackagebnd%noupdateauxvar(this%iauxfpconc) = 1
          call this%apt_copy2flowp()
        end if
      end if
    end if
    !
    ! -- Return
    return
  end subroutine apt_ar

  subroutine apt_rp(this)
! ******************************************************************************
! apt_rp -- Read and Prepare
! Subroutine: (1) read itmp
!             (2) read new boundaries if itmp>0
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use TdisModule, only: kper, nper
    ! -- dummy
    class(GwtAptType), intent(inout) :: this
    ! -- local
    integer(I4B) :: ierr
    integer(I4B) :: n
    logical :: isfound, endOfBlock
    character(len=LINELENGTH) :: title
    character(len=LINELENGTH) :: line
    integer(I4B) :: itemno
    integer(I4B) :: igwfnode
    ! -- formats
    character(len=*), parameter :: fmtblkerr = &
      &"('Error.  Looking for BEGIN PERIOD iper.  Found ', a, ' instead.')"
    character(len=*), parameter :: fmtlsp = &
      &"(1X,/1X,'REUSING ',A,'S FROM LAST STRESS PERIOD')"
! ------------------------------------------------------------------------------
    !
    ! -- set nbound to maxbound
    this%nbound = this%maxbound
    !
    ! -- Set ionper to the stress period number for which a new block of data
    !    will be read.
    if (this%inunit == 0) return
    !
    ! -- get stress period data
    if (this%ionper < kper) then
      !
      ! -- get period block
      call this%parser%GetBlock('PERIOD', isfound, ierr, &
                                supportOpenClose=.true., &
                                blockRequired=.false.)
      if (isfound) then
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
          write (errmsg, fmtblkerr) adjustl(trim(line))
          call store_error(errmsg)
          call this%parser%StoreErrorUnit()
        end if
      end if
    end if
    !
    ! -- Read data if ionper == kper
    if (this%ionper == kper) then
      !
      ! -- setup table for period data
      if (this%iprpak /= 0) then
        !
        ! -- reset the input table object
        title = trim(adjustl(this%text))//' PACKAGE ('// &
                trim(adjustl(this%packName))//') DATA FOR PERIOD'
        write (title, '(a,1x,i6)') trim(adjustl(title)), kper
        call table_cr(this%inputtab, this%packName, title)
        call this%inputtab%table_df(1, 4, this%iout, finalize=.FALSE.)
        text = 'NUMBER'
        call this%inputtab%initialize_column(text, 10, alignment=TABCENTER)
        text = 'KEYWORD'
        call this%inputtab%initialize_column(text, 20, alignment=TABLEFT)
        do n = 1, 2
          write (text, '(a,1x,i6)') 'VALUE', n
          call this%inputtab%initialize_column(text, 15, alignment=TABCENTER)
        end do
      end if
      !
      ! -- read data
      stressperiod: do
        call this%parser%GetNextLine(endOfBlock)
        if (endOfBlock) exit
        !
        ! -- get feature number
        itemno = this%parser%GetInteger()
        !
        ! -- read data from the rest of the line
        call this%apt_set_stressperiod(itemno)
        !
        ! -- write line to table
        if (this%iprpak /= 0) then
          call this%parser%GetCurrentLine(line)
          call this%inputtab%line_to_columns(line)
        end if
      end do stressperiod

      if (this%iprpak /= 0) then
        call this%inputtab%finalize_table()
      end if
      !
      ! -- using stress period data from the previous stress period
    else
      write (this%iout, fmtlsp) trim(this%filtyp)
    end if
    !
    ! -- write summary of stress period error messages
    ierr = count_errors()
    if (ierr > 0) then
      call this%parser%StoreErrorUnit()
    end if
    !
    ! -- fill arrays
    do n = 1, this%flowbudptr%budterm(this%idxbudgwf)%nlist
      igwfnode = this%flowbudptr%budterm(this%idxbudgwf)%id2(n)
      this%nodelist(n) = igwfnode
    end do
    !
    ! -- return
    return
  end subroutine apt_rp

  subroutine apt_set_stressperiod(this, itemno)
! ******************************************************************************
! apt_set_stressperiod -- Set a stress period attribute for feature (itemno)
!                         using keywords.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- module
    use TimeSeriesManagerModule, only: read_value_or_time_series_adv
    ! -- dummy
    class(GwtAptType), intent(inout) :: this
    integer(I4B), intent(in) :: itemno
    ! -- local
    character(len=LINELENGTH) :: text
    character(len=LINELENGTH) :: caux
    character(len=LINELENGTH) :: keyword
    integer(I4B) :: ierr
    integer(I4B) :: ii
    integer(I4B) :: jj
    real(DP), pointer :: bndElem => null()
    logical :: found
    ! -- formats
! ------------------------------------------------------------------------------
    !
    ! -- Support these general options with apply to LKT, SFT, MWT, UZT
    ! STATUS <status>
    ! CONCENTRATION <concentration>
    ! WITHDRAWAL <withdrawal>
    ! AUXILIARY <auxname> <auxval>
    !
    ! -- read line
    call this%parser%GetStringCaps(keyword)
    select case (keyword)
    case ('STATUS')
      ierr = this%apt_check_valid(itemno)
      if (ierr /= 0) then
        goto 999
      end if
      call this%parser%GetStringCaps(text)
      this%status(itemno) = text(1:8)
      if (text == 'CONSTANT') then
        this%iboundpak(itemno) = -1
      else if (text == 'INACTIVE') then
        this%iboundpak(itemno) = 0
      else if (text == 'ACTIVE') then
        this%iboundpak(itemno) = 1
      else
        write (errmsg, '(a,a)') &
          'Unknown '//trim(this%text)//' status keyword: ', text//'.'
        call store_error(errmsg)
      end if
    case ('CONCENTRATION')
      ierr = this%apt_check_valid(itemno)
      if (ierr /= 0) then
        goto 999
      end if
      call this%parser%GetString(text)
      jj = 1 ! For feature concentration
      bndElem => this%concfeat(itemno)
      call read_value_or_time_series_adv(text, itemno, jj, bndElem, &
                                         this%packName, 'BND', this%tsManager, &
                                         this%iprpak, 'CONCENTRATION')
    case ('AUXILIARY')
      ierr = this%apt_check_valid(itemno)
      if (ierr /= 0) then
        goto 999
      end if
      call this%parser%GetStringCaps(caux)
      do jj = 1, this%naux
        if (trim(adjustl(caux)) /= trim(adjustl(this%auxname(jj)))) cycle
        call this%parser%GetString(text)
        ii = itemno
        bndElem => this%lauxvar(jj, ii)
        call read_value_or_time_series_adv(text, itemno, jj, bndElem, &
                                           this%packName, 'AUX', &
                                           this%tsManager, this%iprpak, &
                                           this%auxname(jj))
        exit
      end do
    case default
      !
      ! -- call the specific package to look for stress period data
      call this%pak_set_stressperiod(itemno, keyword, found)
      !
      ! -- terminate with error if data not valid
      if (.not. found) then
        write (errmsg, '(2a)') &
          'Unknown '//trim(adjustl(this%text))//' data keyword: ', &
          trim(keyword)//'.'
        call store_error(errmsg)
      end if
    end select
    !
    ! -- terminate if any errors were detected
999 if (count_errors() > 0) then
      call this%parser%StoreErrorUnit()
    end if
    !
    ! -- return
    return
  end subroutine apt_set_stressperiod

  subroutine pak_set_stressperiod(this, itemno, keyword, found)
! ******************************************************************************
! pak_set_stressperiod -- Set a stress period attribute for individual package.
!   This must be overridden.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(GwtAptType), intent(inout) :: this
    integer(I4B), intent(in) :: itemno
    character(len=*), intent(in) :: keyword
    logical, intent(inout) :: found
    ! -- local

    ! -- formats
! ------------------------------------------------------------------------------
    !
    ! -- this routine should never be called
    found = .false.
    call store_error('Program error: pak_set_stressperiod not implemented.', &
                     terminate=.TRUE.)
    !
    ! -- return
    return
  end subroutine pak_set_stressperiod

  function apt_check_valid(this, itemno) result(ierr)
! ******************************************************************************
!  apt_check_valid -- Determine if a valid feature number has been
!                     specified.
! ******************************************************************************
    ! -- return
    integer(I4B) :: ierr
    ! -- dummy
    class(GwtAptType), intent(inout) :: this
    integer(I4B), intent(in) :: itemno
    ! -- local
    ! -- formats
! ------------------------------------------------------------------------------
    ierr = 0
    if (itemno < 1 .or. itemno > this%ncv) then
      write (errmsg, '(4x,a,1x,i6,1x,a,1x,i6)') &
        '****ERROR. FEATURENO ', itemno, 'MUST BE > 0 and <= ', this%ncv
      call store_error(errmsg)
      ierr = 1
    end if
  end function apt_check_valid

  subroutine apt_ad(this)
! ******************************************************************************
! apt_ad -- Add package connection to matrix
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use SimVariablesModule, only: iFailedStepRetry
    ! -- dummy
    class(GwtAptType) :: this
    ! -- local
    integer(I4B) :: n
    integer(I4B) :: j, iaux
! ------------------------------------------------------------------------------
    !
    ! -- Advance the time series
    call this%TsManager%ad()
    !
    ! -- update auxiliary variables by copying from the derived-type time
    !    series variable into the bndpackage auxvar variable so that this
    !    information is properly written to the GWF budget file
    if (this%naux > 0) then
      do j = 1, this%flowbudptr%budterm(this%idxbudgwf)%nlist
        n = this%flowbudptr%budterm(this%idxbudgwf)%id1(j)
        do iaux = 1, this%naux
          this%auxvar(iaux, j) = this%lauxvar(iaux, n)
        end do
      end do
    end if
    !
    ! -- copy xnew into xold and set xnewpak to specified concentration for
    !    constant concentration features
    if (iFailedStepRetry == 0) then
      do n = 1, this%ncv
        this%xoldpak(n) = this%xnewpak(n)
        if (this%iboundpak(n) < 0) then
          this%xnewpak(n) = this%concfeat(n)
        end if
      end do
    else
      do n = 1, this%ncv
        this%xnewpak(n) = this%xoldpak(n)
        if (this%iboundpak(n) < 0) then
          this%xnewpak(n) = this%concfeat(n)
        end if
      end do
    end if
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
  end subroutine apt_ad

  !> @ brief Formulate the package hcof and rhs terms.
  !!
  !!  For the APT Package, the sole purpose here is to
  !!  reset the qmfrommvr term.
  !!
  !<
  subroutine apt_cf(this, reset_mover)
    ! -- modules
    class(GwtAptType) :: this !< GwtAptType object
    logical(LGP), intent(in), optional :: reset_mover !< boolean for resetting mover
    ! -- local
    integer(I4B) :: i
    logical :: lrm
    !
    ! -- reset qmfrommvr
    lrm = .true.
    if (present(reset_mover)) lrm = reset_mover
    if (lrm) then
      do i = 1, size(this%qmfrommvr)
        this%qmfrommvr(i) = DZERO
      end do
    end if
    !
    ! -- return
    return
  end subroutine apt_cf

  subroutine apt_fc(this, rhs, ia, idxglo, amatsln)
! ******************************************************************************
! apt_fc
! ****************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(GwtAptType) :: this
    real(DP), dimension(:), intent(inout) :: rhs
    integer(I4B), dimension(:), intent(in) :: ia
    integer(I4B), dimension(:), intent(in) :: idxglo
    real(DP), dimension(:), intent(inout) :: amatsln
    ! -- local
! ------------------------------------------------------------------------------
    !
    ! -- Call fc depending on whether or not a matrix is expanded or not
    if (this%imatrows == 0) then
      call this%apt_fc_nonexpanded(rhs, ia, idxglo, amatsln)
    else
      call this%apt_fc_expanded(rhs, ia, idxglo, amatsln)
    end if
    !
    ! -- Return
    return
  end subroutine apt_fc

  subroutine apt_fc_nonexpanded(this, rhs, ia, idxglo, amatsln)
! ******************************************************************************
! apt_fc_nonexpanded -- formulate for the nonexpanded a matrix case in which
!   feature concentrations are solved explicitly
! ****************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(GwtAptType) :: this
    real(DP), dimension(:), intent(inout) :: rhs
    integer(I4B), dimension(:), intent(in) :: ia
    integer(I4B), dimension(:), intent(in) :: idxglo
    real(DP), dimension(:), intent(inout) :: amatsln
    ! -- local
    integer(I4B) :: j, igwfnode, idiag
! ------------------------------------------------------------------------------
    !
    ! -- solve for concentration in the features
    call this%apt_solve()
    !
    ! -- add hcof and rhs terms (from apt_solve) to the gwf matrix
    do j = 1, this%flowbudptr%budterm(this%idxbudgwf)%nlist
      igwfnode = this%flowbudptr%budterm(this%idxbudgwf)%id2(j)
      if (this%ibound(igwfnode) < 1) cycle
      idiag = idxglo(ia(igwfnode))
      amatsln(idiag) = amatsln(idiag) + this%hcof(j)
      rhs(igwfnode) = rhs(igwfnode) + this%rhs(j)
    end do
    !
    ! -- Return
    return
  end subroutine apt_fc_nonexpanded

  subroutine apt_fc_expanded(this, rhs, ia, idxglo, amatsln)
! ******************************************************************************
! apt_fc_expanded -- formulate for the expanded matrix case in which new
!   rows are added to the system of equations for each feature
! ****************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(GwtAptType) :: this
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
    real(DP) :: omega
    real(DP) :: rrate
    real(DP) :: rhsval
    real(DP) :: hcofval
! ------------------------------------------------------------------------------
    !
    ! -- call the specific method for the advanced transport package, such as
    !    what would be overridden by
    !      GwtLktType, GwtSftType, GwtMwtType, GwtUztType
    !    This routine will add terms for rainfall, runoff, or other terms
    !    specific to the package
    call this%pak_fc_expanded(rhs, ia, idxglo, amatsln)
    !
    ! -- mass storage in features
    do n = 1, this%ncv
      cold = this%xoldpak(n)
      iloc = this%idxlocnode(n)
      iposd = this%idxpakdiag(n)
      call this%apt_stor_term(n, n1, n2, rrate, rhsval, hcofval)
      amatsln(iposd) = amatsln(iposd) + hcofval
      rhs(iloc) = rhs(iloc) + rhsval
    end do
    !
    ! -- add to mover contribution
    if (this%idxbudtmvr /= 0) then
      do j = 1, this%flowbudptr%budterm(this%idxbudtmvr)%nlist
        call this%apt_tmvr_term(j, n1, n2, rrate, rhsval, hcofval)
        iloc = this%idxlocnode(n1)
        iposd = this%idxpakdiag(n1)
        amatsln(iposd) = amatsln(iposd) + hcofval
        rhs(iloc) = rhs(iloc) + rhsval
      end do
    end if
    !
    ! -- add from mover contribution
    if (this%idxbudfmvr /= 0) then
      do n = 1, this%ncv
        rhsval = this%qmfrommvr(n)
        iloc = this%idxlocnode(n)
        rhs(iloc) = rhs(iloc) - rhsval
      end do
    end if
    !
    ! -- go through each apt-gwf connection
    do j = 1, this%flowbudptr%budterm(this%idxbudgwf)%nlist
      !
      ! -- set n to feature number and process if active feature
      n = this%flowbudptr%budterm(this%idxbudgwf)%id1(j)
      if (this%iboundpak(n) /= 0) then
        !
        ! -- set acoef and rhs to negative so they are relative to apt and not gwt
        qbnd = this%flowbudptr%budterm(this%idxbudgwf)%flow(j)
        omega = DZERO
        if (qbnd < DZERO) omega = DONE
        !
        ! -- add to apt row
        iposd = this%idxdglo(j)
        iposoffd = this%idxoffdglo(j)
        amatsln(iposd) = amatsln(iposd) + omega * qbnd
        amatsln(iposoffd) = amatsln(iposoffd) + (DONE - omega) * qbnd
        !
        ! -- add to gwf row for apt connection
        ipossymd = this%idxsymdglo(j)
        ipossymoffd = this%idxsymoffdglo(j)
        amatsln(ipossymd) = amatsln(ipossymd) - (DONE - omega) * qbnd
        amatsln(ipossymoffd) = amatsln(ipossymoffd) - omega * qbnd
      end if
    end do
    !
    ! -- go through each apt-apt connection
    if (this%idxbudfjf /= 0) then
      do j = 1, this%flowbudptr%budterm(this%idxbudfjf)%nlist
        n1 = this%flowbudptr%budterm(this%idxbudfjf)%id1(j)
        n2 = this%flowbudptr%budterm(this%idxbudfjf)%id2(j)
        qbnd = this%flowbudptr%budterm(this%idxbudfjf)%flow(j)
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
  end subroutine apt_fc_expanded

  subroutine pak_fc_expanded(this, rhs, ia, idxglo, amatsln)
! ******************************************************************************
! pak_fc_expanded -- allow a subclass advanced transport package to inject
!   terms into the matrix assembly.  This method must be overridden.
! ****************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(GwtAptType) :: this
    real(DP), dimension(:), intent(inout) :: rhs
    integer(I4B), dimension(:), intent(in) :: ia
    integer(I4B), dimension(:), intent(in) :: idxglo
    real(DP), dimension(:), intent(inout) :: amatsln
    ! -- local
! ------------------------------------------------------------------------------
    !
    ! -- this routine should never be called
    call store_error('Program error: pak_fc_expanded not implemented.', &
                     terminate=.TRUE.)
    !
    ! -- Return
    return
  end subroutine pak_fc_expanded

  subroutine apt_cfupdate(this)
! ******************************************************************************
! apt_cfupdate -- calculate package hcof and rhs so gwt budget is calculated
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(GwtAptType) :: this
    ! -- local
    integer(I4B) :: j, n
    real(DP) :: qbnd
    real(DP) :: omega
! ------------------------------------------------------------------------------
    !
    ! -- Calculate hcof and rhs terms so GWF exchanges are calculated correctly
    ! -- go through each apt-gwf connection and calculate
    !    rhs and hcof terms for gwt matrix rows
    do j = 1, this%flowbudptr%budterm(this%idxbudgwf)%nlist
      n = this%flowbudptr%budterm(this%idxbudgwf)%id1(j)
      this%hcof(j) = DZERO
      this%rhs(j) = DZERO
      if (this%iboundpak(n) /= 0) then
        qbnd = this%flowbudptr%budterm(this%idxbudgwf)%flow(j)
        omega = DZERO
        if (qbnd < DZERO) omega = DONE
        this%hcof(j) = -(DONE - omega) * qbnd
        this%rhs(j) = omega * qbnd * this%xnewpak(n)
      end if
    end do
    !
    ! -- Return
    return
  end subroutine apt_cfupdate

  subroutine apt_cq(this, x, flowja, iadv)
! ******************************************************************************
! apt_cq -- Calculate flows for the feature
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(GwtAptType), intent(inout) :: this
    real(DP), dimension(:), intent(in) :: x
    real(DP), dimension(:), contiguous, intent(inout) :: flowja
    integer(I4B), optional, intent(in) :: iadv
    ! -- local
    integer(I4B) :: n, n1, n2
    real(DP) :: rrate
! ------------------------------------------------------------------------------
    !
    ! -- Solve the feature concentrations again or update the feature hcof
    !    and rhs terms
    if (this%imatrows == 0) then
      call this%apt_solve()
    else
      call this%apt_cfupdate()
    end if
    !
    ! -- call base functionality in bnd_cq
    call this%BndType%bnd_cq(x, flowja)
    !
    ! -- calculate storage term
    do n = 1, this%ncv
      rrate = DZERO
      if (this%iboundpak(n) > 0) then
        call this%apt_stor_term(n, n1, n2, rrate)
      end if
      this%qsto(n) = rrate
    end do
    !
    ! -- Copy concentrations into the flow package auxiliary variable
    call this%apt_copy2flowp()
    !
    ! -- fill the budget object
    call this%apt_fill_budobj(x)
    !
    ! -- return
    return
  end subroutine apt_cq

  subroutine apt_ot_package_flows(this, icbcfl, ibudfl)
    use TdisModule, only: kstp, kper, delt, pertim, totim
    class(GwtAptType) :: this
    integer(I4B), intent(in) :: icbcfl
    integer(I4B), intent(in) :: ibudfl
    integer(I4B) :: ibinun
    !
    ! -- write the flows from the budobj
    ibinun = 0
    if (this%ibudgetout /= 0) then
      ibinun = this%ibudgetout
    end if
    if (icbcfl == 0) ibinun = 0
    if (ibinun > 0) then
      call this%budobj%save_flows(this%dis, ibinun, kstp, kper, delt, &
                                  pertim, totim, this%iout)
    end if
    !
    ! -- Print lake flows table
    if (ibudfl /= 0 .and. this%iprflow /= 0) then
      call this%budobj%write_flowtable(this%dis, kstp, kper)
    end if

  end subroutine apt_ot_package_flows

  subroutine apt_ot_dv(this, idvsave, idvprint)
    use TdisModule, only: kstp, kper, pertim, totim
    use ConstantsModule, only: DHNOFLO, DHDRY
    use InputOutputModule, only: ulasav
    class(GwtAptType) :: this
    integer(I4B), intent(in) :: idvsave
    integer(I4B), intent(in) :: idvprint
    integer(I4B) :: ibinun
    integer(I4B) :: n
    real(DP) :: c
    !
    ! -- set unit number for binary dependent variable output
    ibinun = 0
    if (this%iconcout /= 0) then
      ibinun = this%iconcout
    end if
    if (idvsave == 0) ibinun = 0
    !
    ! -- write binary output
    if (ibinun > 0) then
      do n = 1, this%ncv
        c = this%xnewpak(n)
        if (this%iboundpak(n) == 0) then
          c = DHNOFLO
        end if
        this%dbuff(n) = c
      end do
      call ulasav(this%dbuff, '   CONCENTRATION', kstp, kper, pertim, totim, &
                  this%ncv, 1, 1, ibinun)
    end if
    !
    ! -- write apt conc table
    if (idvprint /= 0 .and. this%iprconc /= 0) then
      !
      ! -- set table kstp and kper
      call this%dvtab%set_kstpkper(kstp, kper)
      !
      ! -- fill concentration data
      do n = 1, this%ncv
        if (this%inamedbound == 1) then
          call this%dvtab%add_term(this%featname(n))
        end if
        call this%dvtab%add_term(n)
        call this%dvtab%add_term(this%xnewpak(n))
      end do
    end if

  end subroutine apt_ot_dv

  subroutine apt_ot_bdsummary(this, kstp, kper, iout, ibudfl)
    ! -- module
    use TdisModule, only: totim
    ! -- dummy
    class(GwtAptType) :: this !< GwtAptType object
    integer(I4B), intent(in) :: kstp !< time step number
    integer(I4B), intent(in) :: kper !< period number
    integer(I4B), intent(in) :: iout !< flag and unit number for the model listing file
    integer(I4B), intent(in) :: ibudfl !< flag indicating budget should be written
    !
    call this%budobj%write_budtable(kstp, kper, iout, ibudfl, totim)
    !
    ! -- return
    return
  end subroutine apt_ot_bdsummary

  !> @ brief Allocate scalars
  !!
  !! Allocate scalar variables for this package
  !!
  !<
  subroutine allocate_scalars(this)
    ! -- modules
    use MemoryManagerModule, only: mem_allocate
    ! -- dummy
    class(GwtAptType) :: this
    ! -- local
    !
    ! -- allocate scalars in NumericalPackageType
    call this%BndType%allocate_scalars()
    !
    ! -- Allocate
    call mem_allocate(this%iauxfpconc, 'IAUXFPCONC', this%memoryPath)
    call mem_allocate(this%imatrows, 'IMATROWS', this%memoryPath)
    call mem_allocate(this%iprconc, 'IPRCONC', this%memoryPath)
    call mem_allocate(this%iconcout, 'ICONCOUT', this%memoryPath)
    call mem_allocate(this%ibudgetout, 'IBUDGETOUT', this%memoryPath)
    call mem_allocate(this%ibudcsv, 'IBUDCSV', this%memoryPath)
    call mem_allocate(this%igwfaptpak, 'IGWFAPTPAK', this%memoryPath)
    call mem_allocate(this%ncv, 'NCV', this%memoryPath)
    call mem_allocate(this%idxbudfjf, 'IDXBUDFJF', this%memoryPath)
    call mem_allocate(this%idxbudgwf, 'IDXBUDGWF', this%memoryPath)
    call mem_allocate(this%idxbudsto, 'IDXBUDSTO', this%memoryPath)
    call mem_allocate(this%idxbudtmvr, 'IDXBUDTMVR', this%memoryPath)
    call mem_allocate(this%idxbudfmvr, 'IDXBUDFMVR', this%memoryPath)
    call mem_allocate(this%idxbudaux, 'IDXBUDAUX', this%memoryPath)
    call mem_allocate(this%nconcbudssm, 'NCONCBUDSSM', this%memoryPath)
    !
    ! -- Initialize
    this%iauxfpconc = 0
    this%imatrows = 1
    this%iprconc = 0
    this%iconcout = 0
    this%ibudgetout = 0
    this%ibudcsv = 0
    this%igwfaptpak = 0
    this%ncv = 0
    this%idxbudfjf = 0
    this%idxbudgwf = 0
    this%idxbudsto = 0
    this%idxbudtmvr = 0
    this%idxbudfmvr = 0
    this%idxbudaux = 0
    this%nconcbudssm = 0
    !
    ! -- set this package as causing asymmetric matrix terms
    this%iasym = 1
    !
    ! -- Return
    return
  end subroutine allocate_scalars

  !> @ brief Allocate index arrays
  !!
  !! Allocate arrays that map to locations in the
  !! numerical solution
  !!
  !<
  subroutine apt_allocate_index_arrays(this)
    ! -- modules
    use MemoryManagerModule, only: mem_allocate
    ! -- dummy
    class(GwtAptType), intent(inout) :: this
    ! -- local
    integer(I4B) :: n

    if (this%imatrows /= 0) then
      !
      ! -- count number of flow-ja-face connections
      n = 0
      if (this%idxbudfjf /= 0) then
        n = this%flowbudptr%budterm(this%idxbudfjf)%maxlist
      end if
      !
      ! -- allocate pointers to global matrix
      call mem_allocate(this%idxlocnode, this%ncv, 'IDXLOCNODE', &
                        this%memoryPath)
      call mem_allocate(this%idxpakdiag, this%ncv, 'IDXPAKDIAG', &
                        this%memoryPath)
      call mem_allocate(this%idxdglo, this%maxbound, 'IDXGLO', &
                        this%memoryPath)
      call mem_allocate(this%idxoffdglo, this%maxbound, 'IDXOFFDGLO', &
                        this%memoryPath)
      call mem_allocate(this%idxsymdglo, this%maxbound, 'IDXSYMDGLO', &
                        this%memoryPath)
      call mem_allocate(this%idxsymoffdglo, this%maxbound, 'IDXSYMOFFDGLO', &
                        this%memoryPath)
      call mem_allocate(this%idxfjfdglo, n, 'IDXFJFDGLO', &
                        this%memoryPath)
      call mem_allocate(this%idxfjfoffdglo, n, 'IDXFJFOFFDGLO', &
                        this%memoryPath)
    else
      call mem_allocate(this%idxlocnode, 0, 'IDXLOCNODE', &
                        this%memoryPath)
      call mem_allocate(this%idxpakdiag, 0, 'IDXPAKDIAG', &
                        this%memoryPath)
      call mem_allocate(this%idxdglo, 0, 'IDXGLO', &
                        this%memoryPath)
      call mem_allocate(this%idxoffdglo, 0, 'IDXOFFDGLO', &
                        this%memoryPath)
      call mem_allocate(this%idxsymdglo, 0, 'IDXSYMDGLO', &
                        this%memoryPath)
      call mem_allocate(this%idxsymoffdglo, 0, 'IDXSYMOFFDGLO', &
                        this%memoryPath)
      call mem_allocate(this%idxfjfdglo, 0, 'IDXFJFDGLO', &
                        this%memoryPath)
      call mem_allocate(this%idxfjfoffdglo, 0, 'IDXFJFOFFDGLO', &
                        this%memoryPath)
    end if
    return
  end subroutine apt_allocate_index_arrays

  !> @ brief Allocate arrays
  !!
  !! Allocate package arrays
  !!
  !<
  subroutine apt_allocate_arrays(this)
    ! -- modules
    use MemoryManagerModule, only: mem_allocate
    ! -- dummy
    class(GwtAptType), intent(inout) :: this
    ! -- local
    integer(I4B) :: n
    !
    ! -- call standard BndType allocate scalars
    call this%BndType%allocate_arrays()
    !
    ! -- Allocate
    !
    ! -- allocate and initialize dbuff
    if (this%iconcout > 0) then
      call mem_allocate(this%dbuff, this%ncv, 'DBUFF', this%memoryPath)
      do n = 1, this%ncv
        this%dbuff(n) = DZERO
      end do
    else
      call mem_allocate(this%dbuff, 0, 'DBUFF', this%memoryPath)
    end if
    !
    ! -- allocate character array for status
    allocate (this%status(this%ncv))
    !
    ! -- time series
    call mem_allocate(this%concfeat, this%ncv, 'CONCFEAT', this%memoryPath)
    !
    ! -- budget terms
    call mem_allocate(this%qsto, this%ncv, 'QSTO', this%memoryPath)
    call mem_allocate(this%ccterm, this%ncv, 'CCTERM', this%memoryPath)
    !
    ! -- concentration for budget terms
    call mem_allocate(this%concbudssm, this%nconcbudssm, this%ncv, &
                      'CONCBUDSSM', this%memoryPath)
    !
    ! -- mass added from the mover transport package
    call mem_allocate(this%qmfrommvr, this%ncv, 'QMFROMMVR', this%memoryPath)
    !
    ! -- initialize arrays
    do n = 1, this%ncv
      this%status(n) = 'ACTIVE'
      this%qsto(n) = DZERO
      this%ccterm(n) = DZERO
      this%qmfrommvr(n) = DZERO
      this%concbudssm(:, n) = DZERO
      this%concfeat(n) = DZERO
    end do
    !
    ! -- Return
    return
  end subroutine apt_allocate_arrays

  !> @ brief Deallocate memory
  !!
  !! Deallocate memory associated with this package
  !!
  !<
  subroutine apt_da(this)
    ! -- modules
    use MemoryManagerModule, only: mem_deallocate
    ! -- dummy
    class(GwtAptType) :: this
    ! -- local
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
    call mem_deallocate(this%concfeat)
    call mem_deallocate(this%qmfrommvr)
    deallocate (this%status)
    deallocate (this%featname)
    !
    ! -- budobj
    call this%budobj%budgetobject_da()
    deallocate (this%budobj)
    nullify (this%budobj)
    !
    ! -- conc table
    if (this%iprconc > 0) then
      call this%dvtab%table_da()
      deallocate (this%dvtab)
      nullify (this%dvtab)
    end if
    !
    ! -- index pointers
    call mem_deallocate(this%idxlocnode)
    call mem_deallocate(this%idxpakdiag)
    call mem_deallocate(this%idxdglo)
    call mem_deallocate(this%idxoffdglo)
    call mem_deallocate(this%idxsymdglo)
    call mem_deallocate(this%idxsymoffdglo)
    call mem_deallocate(this%idxfjfdglo)
    call mem_deallocate(this%idxfjfoffdglo)
    !
    ! -- deallocate scalars
    call mem_deallocate(this%iauxfpconc)
    call mem_deallocate(this%imatrows)
    call mem_deallocate(this%iprconc)
    call mem_deallocate(this%iconcout)
    call mem_deallocate(this%ibudgetout)
    call mem_deallocate(this%ibudcsv)
    call mem_deallocate(this%igwfaptpak)
    call mem_deallocate(this%ncv)
    call mem_deallocate(this%idxbudfjf)
    call mem_deallocate(this%idxbudgwf)
    call mem_deallocate(this%idxbudsto)
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
  end subroutine apt_da

  subroutine find_apt_package(this)
! ******************************************************************************
! find corresponding flow package
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryManagerModule, only: mem_allocate
    ! -- dummy
    class(GwtAptType) :: this
    ! -- local
! ------------------------------------------------------------------------------
    !
    ! -- this routine should never be called
    call store_error('Program error: pak_solve not implemented.', &
                     terminate=.TRUE.)
    !
    ! -- Return
    return
  end subroutine find_apt_package

  subroutine apt_options(this, option, found)
! ******************************************************************************
! apt_options -- set options specific to GwtAptType
!
! apt_options overrides BndType%bnd_options
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use ConstantsModule, only: MAXCHARLEN, DZERO
    use OpenSpecModule, only: access, form
    use InputOutputModule, only: urword, getunit, openfile
    ! -- dummy
    class(GwtAptType), intent(inout) :: this
    character(len=*), intent(inout) :: option
    logical, intent(inout) :: found
    ! -- local
    character(len=MAXCHARLEN) :: fname, keyword
    ! -- formats
    character(len=*), parameter :: fmtaptbin = &
      "(4x, a, 1x, a, 1x, ' WILL BE SAVED TO FILE: ', a, &
      &/4x, 'OPENED ON UNIT: ', I0)"
! ------------------------------------------------------------------------------
    !
    found = .true.
    select case (option)
    case ('FLOW_PACKAGE_NAME')
      call this%parser%GetStringCaps(this%flowpackagename)
      write (this%iout, '(4x,a)') &
        'THIS '//trim(adjustl(this%text))//' PACKAGE CORRESPONDS TO A GWF &
        &PACKAGE WITH THE NAME '//trim(adjustl(this%flowpackagename))
    case ('FLOW_PACKAGE_AUXILIARY_NAME')
      call this%parser%GetStringCaps(this%cauxfpconc)
      write (this%iout, '(4x,a)') &
        'SIMULATED CONCENTRATIONS WILL BE COPIED INTO THE FLOW PACKAGE &
        &AUXILIARY VARIABLE WITH THE NAME '//trim(adjustl(this%cauxfpconc))
    case ('DEV_NONEXPANDING_MATRIX')
      ! -- use an iterative solution where concentration is not solved
      !    as part of the matrix.  It is instead solved separately with a
      !    general mixing equation and then added to the RHS of the GWT
      !    equations
      call this%parser%DevOpt()
      this%imatrows = 0
      write (this%iout, '(4x,a)') &
        trim(adjustl(this%text))// &
        ' WILL NOT ADD ADDITIONAL ROWS TO THE A MATRIX.'
    case ('PRINT_CONCENTRATION')
      this%iprconc = 1
      write (this%iout, '(4x,a)') trim(adjustl(this%text))// &
        ' CONCENTRATIONS WILL BE PRINTED TO LISTING FILE.'
    case ('CONCENTRATION')
      call this%parser%GetStringCaps(keyword)
      if (keyword == 'FILEOUT') then
        call this%parser%GetString(fname)
        this%iconcout = getunit()
        call openfile(this%iconcout, this%iout, fname, 'DATA(BINARY)', &
                      form, access, 'REPLACE')
        write (this%iout, fmtaptbin) &
          trim(adjustl(this%text)), 'CONCENTRATION', trim(fname), this%iconcout
      else
        call store_error('OPTIONAL CONCENTRATION KEYWORD MUST &
                         &BE FOLLOWED BY FILEOUT')
      end if
    case ('BUDGET')
      call this%parser%GetStringCaps(keyword)
      if (keyword == 'FILEOUT') then
        call this%parser%GetString(fname)
        this%ibudgetout = getunit()
        call openfile(this%ibudgetout, this%iout, fname, 'DATA(BINARY)', &
                      form, access, 'REPLACE')
        write (this%iout, fmtaptbin) trim(adjustl(this%text)), 'BUDGET', &
          trim(fname), this%ibudgetout
      else
        call store_error('OPTIONAL BUDGET KEYWORD MUST BE FOLLOWED BY FILEOUT')
      end if
    case ('BUDGETCSV')
      call this%parser%GetStringCaps(keyword)
      if (keyword == 'FILEOUT') then
        call this%parser%GetString(fname)
        this%ibudcsv = getunit()
        call openfile(this%ibudcsv, this%iout, fname, 'CSV', &
                      filstat_opt='REPLACE')
        write (this%iout, fmtaptbin) trim(adjustl(this%text)), 'BUDGET CSV', &
          trim(fname), this%ibudcsv
      else
        call store_error('OPTIONAL BUDGETCSV KEYWORD MUST BE FOLLOWED BY &
          &FILEOUT')
      end if
    case default
      !
      ! -- No options found
      found = .false.
    end select
    !
    ! -- return
    return
  end subroutine apt_options

  subroutine apt_read_dimensions(this)
! ******************************************************************************
! apt_read_dimensions -- Determine dimensions for this package
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(GwtAptType), intent(inout) :: this
    ! -- local
    integer(I4B) :: ierr
    ! -- format
! ------------------------------------------------------------------------------
    !
    ! -- Set a pointer to the GWF LAK Package budobj
    if (this%flowpackagename == '') then
      this%flowpackagename = this%packName
      write (this%iout, '(4x,a)') &
        'THE FLOW PACKAGE NAME FOR '//trim(adjustl(this%text))//' WAS NOT &
        &SPECIFIED.  SETTING FLOW PACKAGE NAME TO '// &
        &trim(adjustl(this%flowpackagename))

    end if
    call this%find_apt_package()
    !
    ! -- Set dimensions from the GWF advanced package
    this%ncv = this%flowbudptr%ncv
    this%maxbound = this%flowbudptr%budterm(this%idxbudgwf)%maxlist
    this%nbound = this%maxbound
    write (this%iout, '(a, a)') 'SETTING DIMENSIONS FOR PACKAGE ', this%packName
    write (this%iout, '(2x,a,i0)') 'NUMBER OF CONTROL VOLUMES = ', this%ncv
    write (this%iout, '(2x,a,i0)') 'MAXBOUND = ', this%maxbound
    write (this%iout, '(2x,a,i0)') 'NBOUND = ', this%nbound
    if (this%imatrows /= 0) then
      this%npakeq = this%ncv
      write (this%iout, '(2x,a)') trim(adjustl(this%text))// &
        ' SOLVED AS PART OF GWT MATRIX EQUATIONS'
    else
      write (this%iout, '(2x,a)') trim(adjustl(this%text))// &
        ' SOLVED SEPARATELY FROM GWT MATRIX EQUATIONS '
    end if
    write (this%iout, '(a, //)') 'DONE SETTING DIMENSIONS FOR '// &
      trim(adjustl(this%text))
    !
    ! -- Check for errors
    if (this%ncv < 0) then
      write (errmsg, '(1x,a)') &
        'ERROR:  NUMBER OF CONTROL VOLUMES COULD NOT BE DETERMINED CORRECTLY.'
      call store_error(errmsg)
    end if
    !
    ! -- stop if errors were encountered in the DIMENSIONS block
    ierr = count_errors()
    if (ierr > 0) then
      call store_error_unit(this%inunit)
    end if
    !
    ! -- read packagedata block
    call this%apt_read_cvs()
    !
    ! -- Call define_listlabel to construct the list label that is written
    !    when PRINT_INPUT option is used.
    call this%define_listlabel()
    !
    ! -- setup the budget object
    call this%apt_setup_budobj()
    !
    ! -- setup the conc table object
    call this%apt_setup_tableobj()
    !
    ! -- return
    return
  end subroutine apt_read_dimensions

  subroutine apt_read_cvs(this)
! ******************************************************************************
! apt_read_cvs -- Read feature information for this package
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryManagerModule, only: mem_allocate
    use TimeSeriesManagerModule, only: read_value_or_time_series_adv
    ! -- dummy
    class(GwtAptType), intent(inout) :: this
    ! -- local
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
    real(DP), pointer :: bndElem => null()
! ------------------------------------------------------------------------------
    !
    ! -- initialize itmp
    itmp = 0
    !
    ! -- allocate apt data
    call mem_allocate(this%strt, this%ncv, 'STRT', this%memoryPath)
    call mem_allocate(this%lauxvar, this%naux, this%ncv, 'LAUXVAR', &
                      this%memoryPath)
    !
    ! -- lake boundary and concentrations
    if (this%imatrows == 0) then
      call mem_allocate(this%iboundpak, this%ncv, 'IBOUND', this%memoryPath)
      call mem_allocate(this%xnewpak, this%ncv, 'XNEWPAK', this%memoryPath)
    end if
    call mem_allocate(this%xoldpak, this%ncv, 'XOLDPAK', this%memoryPath)
    !
    ! -- allocate character storage not managed by the memory manager
    allocate (this%featname(this%ncv)) ! ditch after boundnames allocated??
    !allocate(this%status(this%ncv))
    !
    do n = 1, this%ncv
      this%strt(n) = DEP20
      this%lauxvar(:, n) = DZERO
      this%xoldpak(n) = DEP20
      if (this%imatrows == 0) then
        this%iboundpak(n) = 1
        this%xnewpak(n) = DEP20
      end if
    end do
    !
    ! -- allocate local storage for aux variables
    if (this%naux > 0) then
      allocate (caux(this%naux))
    end if
    !
    ! -- allocate and initialize temporary variables
    allocate (nboundchk(this%ncv))
    do n = 1, this%ncv
      nboundchk(n) = 0
    end do
    !
    ! -- get packagedata block
    call this%parser%GetBlock('PACKAGEDATA', isfound, ierr, &
                              supportOpenClose=.true.)
    !
    ! -- parse locations block if detected
    if (isfound) then
      write (this%iout, '(/1x,a)') 'PROCESSING '//trim(adjustl(this%text))// &
        ' PACKAGEDATA'
      nlak = 0
      nconn = 0
      do
        call this%parser%GetNextLine(endOfBlock)
        if (endOfBlock) exit
        n = this%parser%GetInteger()

        if (n < 1 .or. n > this%ncv) then
          write (errmsg, '(4x,a,1x,i6)') &
            '****ERROR. itemno MUST BE > 0 and <= ', this%ncv
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
        write (cno, '(i9.9)') n
        bndName = 'Feature'//cno

        ! -- featname
        if (this%inamedbound /= 0) then
          call this%parser%GetStringCaps(bndNameTemp)
          if (bndNameTemp /= '') then
            bndName = bndNameTemp
          end if
        end if
        this%featname(n) = bndName

        ! -- fill time series aware data
        ! -- fill aux data
        do jj = 1, this%naux
          text = caux(jj)
          ii = n
          bndElem => this%lauxvar(jj, ii)
          call read_value_or_time_series_adv(text, ii, jj, bndElem, &
                                             this%packName, 'AUX', &
                                             this%tsManager, this%iprpak, &
                                             this%auxname(jj))
        end do

        nlak = nlak + 1
      end do
      !
      ! -- check for duplicate or missing lakes
      do n = 1, this%ncv
        if (nboundchk(n) == 0) then
          write (errmsg, '(a,1x,i0)') 'ERROR.  NO DATA SPECIFIED FOR FEATURE', n
          call store_error(errmsg)
        else if (nboundchk(n) > 1) then
          write (errmsg, '(a,1x,i0,1x,a,1x,i0,1x,a)') &
            'ERROR.  DATA FOR FEATURE', n, 'SPECIFIED', nboundchk(n), 'TIMES'
          call store_error(errmsg)
        end if
      end do

      write (this%iout, '(1x,a)') &
        'END OF '//trim(adjustl(this%text))//' PACKAGEDATA'
    else
      call store_error('ERROR.  REQUIRED PACKAGEDATA BLOCK NOT FOUND.')
    end if
    !
    ! -- terminate if any errors were detected
    if (count_errors() > 0) then
      call this%parser%StoreErrorUnit()
    end if
    !
    ! -- deallocate local storage for aux variables
    if (this%naux > 0) then
      deallocate (caux)
    end if
    !
    ! -- deallocate local storage for nboundchk
    deallocate (nboundchk)
    !
    ! -- return
    return
  end subroutine apt_read_cvs

  subroutine apt_read_initial_attr(this)
! ******************************************************************************
! apt_read_initial_attr -- Read the initial parameters for this package
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use ConstantsModule, only: LINELENGTH
    use BudgetModule, only: budget_cr
    ! -- dummy
    class(GwtAptType), intent(inout) :: this
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
    ! -- initialize xnewpak and set lake concentration
    ! -- todo: this should be a time series?
    do n = 1, this%ncv
      this%xnewpak(n) = this%strt(n)
      !write(text,'(g15.7)') this%strt(n)
      !endtim = DZERO
      !jj = 1    ! For STAGE
      !call read_single_value_or_time_series(text, &
      !                                      this%stage(n)%value, &
      !                                      this%stage(n)%name, &
      !                                      endtim, &
      !                                      this%name, 'BND', this%TsManager, &
      !                                      this%iprpak, n, jj, 'STAGE', &
      !                                      this%featname(n), this%inunit)

      ! -- todo: read aux

      ! -- todo: read boundname

    end do
    !
    ! -- initialize status (iboundpak) of lakes to active
    do n = 1, this%ncv
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
      do j = 1, this%flowbudptr%budterm(this%idxbudgwf)%nlist
        n = this%flowbudptr%budterm(this%idxbudgwf)%id1(j)
        this%boundname(j) = this%featname(n)
      end do
    end if
    !
    ! -- copy boundname into boundname_cst
    call this%copy_boundname()
    !
    ! -- return
    return
  end subroutine apt_read_initial_attr

  subroutine apt_solve(this)
! ******************************************************************************
! apt_solve -- explicit solve for concentration in features, which is an
!   alternative to the iterative implicit solve
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use ConstantsModule, only: LINELENGTH
    ! -- dummy
    class(GwtAptType) :: this
    ! -- local
    integer(I4B) :: n, j, igwfnode
    integer(I4B) :: n1, n2
    real(DP) :: rrate
    real(DP) :: ctmp
    real(DP) :: c1, qbnd
    real(DP) :: hcofval, rhsval
! ------------------------------------------------------------------------------
    !
    !
    ! -- first initialize dbuff
    do n = 1, this%ncv
      this%dbuff(n) = DZERO
    end do
    !
    ! -- call the individual package routines to add terms specific to the
    !    advanced transport package
    call this%pak_solve()
    !
    ! -- add to mover contribution
    if (this%idxbudtmvr /= 0) then
      do j = 1, this%flowbudptr%budterm(this%idxbudtmvr)%nlist
        call this%apt_tmvr_term(j, n1, n2, rrate)
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
    !    total mass in dbuff mass
    do j = 1, this%flowbudptr%budterm(this%idxbudgwf)%nlist
      n = this%flowbudptr%budterm(this%idxbudgwf)%id1(j)
      this%hcof(j) = DZERO
      this%rhs(j) = DZERO
      igwfnode = this%flowbudptr%budterm(this%idxbudgwf)%id2(j)
      qbnd = this%flowbudptr%budterm(this%idxbudgwf)%flow(j)
      if (qbnd <= DZERO) then
        ctmp = this%xnewpak(n)
        this%rhs(j) = qbnd * ctmp
      else
        ctmp = this%xnew(igwfnode)
        this%hcof(j) = -qbnd
      end if
      c1 = qbnd * ctmp
      this%dbuff(n) = this%dbuff(n) + c1
    end do
    !
    ! -- go through each lak-lak connection and accumulate
    !    total mass in dbuff mass
    if (this%idxbudfjf /= 0) then
      do j = 1, this%flowbudptr%budterm(this%idxbudfjf)%nlist
        call this%apt_fjf_term(j, n1, n2, rrate)
        c1 = rrate
        this%dbuff(n1) = this%dbuff(n1) + c1
      end do
    end if
    !
    ! -- calulate the feature concentration
    do n = 1, this%ncv
      call this%apt_stor_term(n, n1, n2, rrate, rhsval, hcofval)
      !
      ! -- at this point, dbuff has q * c for all sources, so now
      !    add Vold / dt * Cold
      this%dbuff(n) = this%dbuff(n) - rhsval
      !
      ! -- Now to calculate c, need to divide dbuff by hcofval
      c1 = -this%dbuff(n) / hcofval
      if (this%iboundpak(n) > 0) then
        this%xnewpak(n) = c1
      end if
    end do
    !
    ! -- Return
    return
  end subroutine apt_solve

  subroutine pak_solve(this)
! ******************************************************************************
! pak_solve -- must be overridden
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(GwtAptType) :: this
    ! -- local
! ------------------------------------------------------------------------------
    !
    ! -- this routine should never be called
    call store_error('Program error: pak_solve not implemented.', &
                     terminate=.TRUE.)
    !
    ! -- Return
    return
  end subroutine pak_solve

  subroutine apt_accumulate_ccterm(this, ilak, rrate, ccratin, ccratout)
! ******************************************************************************
! apt_accumulate_ccterm -- Accumulate constant concentration terms for budget.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(GwtAptType) :: this
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
  end subroutine apt_accumulate_ccterm

  subroutine define_listlabel(this)
! ******************************************************************************
! define_listlabel -- Define the list heading that is written to iout when
!   PRINT_INPUT option is used.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    class(GwtAptType), intent(inout) :: this
! ------------------------------------------------------------------------------
    !
    ! -- create the header list label
    this%listlabel = trim(this%filtyp)//' NO.'
    if (this%dis%ndim == 3) then
      write (this%listlabel, '(a, a7)') trim(this%listlabel), 'LAYER'
      write (this%listlabel, '(a, a7)') trim(this%listlabel), 'ROW'
      write (this%listlabel, '(a, a7)') trim(this%listlabel), 'COL'
    elseif (this%dis%ndim == 2) then
      write (this%listlabel, '(a, a7)') trim(this%listlabel), 'LAYER'
      write (this%listlabel, '(a, a7)') trim(this%listlabel), 'CELL2D'
    else
      write (this%listlabel, '(a, a7)') trim(this%listlabel), 'NODE'
    end if
    write (this%listlabel, '(a, a16)') trim(this%listlabel), 'STRESS RATE'
    if (this%inamedbound == 1) then
      write (this%listlabel, '(a, a16)') trim(this%listlabel), 'BOUNDARY NAME'
    end if
    !
    ! -- return
    return
  end subroutine define_listlabel

  subroutine apt_set_pointers(this, neq, ibound, xnew, xold, flowja)
! ******************************************************************************
! set_pointers -- Set pointers to model arrays and variables so that a package
!                 has access to these things.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    class(GwtAptType) :: this
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
    ! -- Set the pointers
    !
    ! -- set package pointers
    if (this%imatrows /= 0) then
      istart = this%dis%nodes + this%ioffset + 1
      iend = istart + this%ncv - 1
      this%iboundpak => this%ibound(istart:iend)
      this%xnewpak => this%xnew(istart:iend)
    end if
    !
    ! -- return
  end subroutine apt_set_pointers

  subroutine get_volumes(this, icv, vnew, vold, delt)
! ******************************************************************************
! get_volumes -- return the feature new volume and old volume
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(GwtAptType) :: this
    integer(I4B), intent(in) :: icv
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
      qss = this%flowbudptr%budterm(this%idxbudsto)%flow(icv)
      vnew = this%flowbudptr%budterm(this%idxbudsto)%auxvar(1, icv)
      vold = vnew + qss * delt
    end if
    !
    ! -- Return
    return
  end subroutine get_volumes

  function pak_get_nbudterms(this) result(nbudterms)
! ******************************************************************************
! pak_get_nbudterms -- function to return the number of budget terms just for
!   this package.  Must be overridden.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(GwtAptType) :: this
    ! -- return
    integer(I4B) :: nbudterms
    ! -- local
! ------------------------------------------------------------------------------
    !
    ! -- this routine should never be called
    call store_error('Program error: pak_get_nbudterms not implemented.', &
                     terminate=.TRUE.)
    nbudterms = 0
  end function pak_get_nbudterms

  subroutine apt_setup_budobj(this)
! ******************************************************************************
! apt_setup_budobj -- Set up the budget object that stores all the lake flows
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use ConstantsModule, only: LENBUDTXT
    ! -- dummy
    class(GwtAptType) :: this
    ! -- local
    integer(I4B) :: nbudterm
    integer(I4B) :: nlen
    integer(I4B) :: n, n1, n2
    integer(I4B) :: maxlist, naux
    integer(I4B) :: idx
    logical :: ordered_id1
    real(DP) :: q
    character(len=LENBUDTXT) :: text
    character(len=LENBUDTXT), dimension(1) :: auxtxt
! ------------------------------------------------------------------------------
    !
    ! -- Determine if there are flow-ja-face terms
    nlen = 0
    if (this%idxbudfjf /= 0) then
      nlen = this%flowbudptr%budterm(this%idxbudfjf)%maxlist
    end if
    !
    ! -- Determine the number of lake budget terms. These are fixed for
    !    the simulation and cannot change
    ! -- the first 3 is for GWF, STORAGE, and CONSTANT
    nbudterm = 3
    !
    ! -- add terms for the specific package
    nbudterm = nbudterm + this%pak_get_nbudterms()
    !
    ! -- add one for flow-ja-face
    if (nlen > 0) nbudterm = nbudterm + 1
    !
    ! -- add for mover terms and auxiliary
    if (this%idxbudtmvr /= 0) nbudterm = nbudterm + 1
    if (this%idxbudfmvr /= 0) nbudterm = nbudterm + 1
    if (this%naux > 0) nbudterm = nbudterm + 1
    !
    ! -- set up budobj
    call budgetobject_cr(this%budobj, this%packName)
    call this%budobj%budgetobject_df(this%ncv, nbudterm, 0, 0, &
                                     bddim_opt='M', ibudcsv=this%ibudcsv)
    idx = 0
    !
    ! -- Go through and set up each budget term
    if (nlen > 0) then
      text = '    FLOW-JA-FACE'
      idx = idx + 1
      maxlist = this%flowbudptr%budterm(this%idxbudfjf)%maxlist
      naux = 0
      ordered_id1 = this%flowbudptr%budterm(this%idxbudfjf)%ordered_id1
      call this%budobj%budterm(idx)%initialize(text, &
                                               this%name_model, &
                                               this%packName, &
                                               this%name_model, &
                                               this%packName, &
                                               maxlist, .false., .false., &
                                               naux, ordered_id1=ordered_id1)
      !
      ! -- store outlet connectivity
      call this%budobj%budterm(idx)%reset(maxlist)
      q = DZERO
      do n = 1, maxlist
        n1 = this%flowbudptr%budterm(this%idxbudfjf)%id1(n)
        n2 = this%flowbudptr%budterm(this%idxbudfjf)%id2(n)
        call this%budobj%budterm(idx)%update_term(n1, n2, q)
      end do
    end if
    !
    ! --
    text = '             GWF'
    idx = idx + 1
    maxlist = this%flowbudptr%budterm(this%idxbudgwf)%maxlist
    naux = 0
    call this%budobj%budterm(idx)%initialize(text, &
                                             this%name_model, &
                                             this%packName, &
                                             this%name_model, &
                                             this%name_model, &
                                             maxlist, .false., .true., &
                                             naux)
    call this%budobj%budterm(idx)%reset(maxlist)
    q = DZERO
    do n = 1, maxlist
      n1 = this%flowbudptr%budterm(this%idxbudgwf)%id1(n)
      n2 = this%flowbudptr%budterm(this%idxbudgwf)%id2(n)
      call this%budobj%budterm(idx)%update_term(n1, n2, q)
    end do
    !
    ! -- Reserve space for the package specific terms
    call this%pak_setup_budobj(idx)
    !
    ! --
    text = '         STORAGE'
    idx = idx + 1
    maxlist = this%flowbudptr%budterm(this%idxbudsto)%maxlist
    naux = 1
    auxtxt(1) = '            MASS'
    call this%budobj%budterm(idx)%initialize(text, &
                                             this%name_model, &
                                             this%packName, &
                                             this%name_model, &
                                             this%packName, &
                                             maxlist, .false., .false., &
                                             naux, auxtxt)
    if (this%idxbudtmvr /= 0) then
      !
      ! --
      text = '          TO-MVR'
      idx = idx + 1
      maxlist = this%flowbudptr%budterm(this%idxbudtmvr)%maxlist
      naux = 0
      ordered_id1 = this%flowbudptr%budterm(this%idxbudtmvr)%ordered_id1
      call this%budobj%budterm(idx)%initialize(text, &
                                               this%name_model, &
                                               this%packName, &
                                               this%name_model, &
                                               this%packName, &
                                               maxlist, .false., .false., &
                                               naux, ordered_id1=ordered_id1)
    end if
    if (this%idxbudfmvr /= 0) then
      !
      ! --
      text = '        FROM-MVR'
      idx = idx + 1
      maxlist = this%ncv
      naux = 0
      call this%budobj%budterm(idx)%initialize(text, &
                                               this%name_model, &
                                               this%packName, &
                                               this%name_model, &
                                               this%packName, &
                                               maxlist, .false., .false., &
                                               naux)
    end if
    !
    ! --
    text = '        CONSTANT'
    idx = idx + 1
    maxlist = this%ncv
    naux = 0
    call this%budobj%budterm(idx)%initialize(text, &
                                             this%name_model, &
                                             this%packName, &
                                             this%name_model, &
                                             this%packName, &
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
      maxlist = this%ncv
      call this%budobj%budterm(idx)%initialize(text, &
                                               this%name_model, &
                                               this%packName, &
                                               this%name_model, &
                                               this%packName, &
                                               maxlist, .false., .false., &
                                               naux, this%auxname)
    end if
    !
    ! -- if flow for each control volume are written to the listing file
    if (this%iprflow /= 0) then
      call this%budobj%flowtable_df(this%iout)
    end if
    !
    ! -- return
    return
  end subroutine apt_setup_budobj

  subroutine pak_setup_budobj(this, idx)
! ******************************************************************************
! pak_setup_budobj -- Individual packages set up their budget terms.  Must
!   be overridden
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(GwtAptType) :: this
    integer(I4B), intent(inout) :: idx
    ! -- local
! ------------------------------------------------------------------------------
    !
    ! -- this routine should never be called
    call store_error('Program error: pak_setup_budobj not implemented.', &
                     terminate=.TRUE.)
    !
    ! -- return
    return
  end subroutine pak_setup_budobj

  subroutine apt_fill_budobj(this, x)
! ******************************************************************************
! apt_fill_budobj -- copy flow terms into this%budobj
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use TdisModule, only: delt
    ! -- dummy
    class(GwtAptType) :: this
    real(DP), dimension(:), intent(in) :: x
    ! -- local
    integer(I4B) :: naux
    real(DP), dimension(:), allocatable :: auxvartmp
    integer(I4B) :: i, j, n1, n2
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
    do n1 = 1, this%ncv
      this%ccterm(n1) = DZERO
    end do

    ! -- FLOW JA FACE
    nlen = 0
    if (this%idxbudfjf /= 0) then
      nlen = this%flowbudptr%budterm(this%idxbudfjf)%maxlist
    end if
    if (nlen > 0) then
      idx = idx + 1
      nlist = this%flowbudptr%budterm(this%idxbudfjf)%maxlist
      call this%budobj%budterm(idx)%reset(nlist)
      q = DZERO
      do j = 1, nlist
        call this%apt_fjf_term(j, n1, n2, q)
        call this%budobj%budterm(idx)%update_term(n1, n2, q)
        call this%apt_accumulate_ccterm(n1, q, ccratin, ccratout)
      end do
    end if

    ! -- GWF (LEAKAGE)
    idx = idx + 1
    call this%budobj%budterm(idx)%reset(this%maxbound)
    do j = 1, this%flowbudptr%budterm(this%idxbudgwf)%nlist
      q = DZERO
      n1 = this%flowbudptr%budterm(this%idxbudgwf)%id1(j)
      if (this%iboundpak(n1) /= 0) then
        igwfnode = this%flowbudptr%budterm(this%idxbudgwf)%id2(j)
        q = this%hcof(j) * x(igwfnode) - this%rhs(j)
        q = -q ! flip sign so relative to advanced package feature
      end if
      call this%budobj%budterm(idx)%update_term(n1, igwfnode, q)
      call this%apt_accumulate_ccterm(n1, q, ccratin, ccratout)
    end do

    ! -- individual package terms
    call this%pak_fill_budobj(idx, x, ccratin, ccratout)

    ! -- STORAGE
    idx = idx + 1
    call this%budobj%budterm(idx)%reset(this%ncv)
    allocate (auxvartmp(1))
    do n1 = 1, this%ncv
      call this%get_volumes(n1, v1, v0, delt)
      auxvartmp(1) = v1 * this%xnewpak(n1)
      q = this%qsto(n1)
      call this%budobj%budterm(idx)%update_term(n1, n1, q, auxvartmp)
      call this%apt_accumulate_ccterm(n1, q, ccratin, ccratout)
    end do
    deallocate (auxvartmp)

    ! -- TO MOVER
    if (this%idxbudtmvr /= 0) then
      idx = idx + 1
      nlist = this%flowbudptr%budterm(this%idxbudtmvr)%nlist
      call this%budobj%budterm(idx)%reset(nlist)
      do j = 1, nlist
        call this%apt_tmvr_term(j, n1, n2, q)
        call this%budobj%budterm(idx)%update_term(n1, n2, q)
        call this%apt_accumulate_ccterm(n1, q, ccratin, ccratout)
      end do
    end if

    ! -- FROM MOVER
    if (this%idxbudfmvr /= 0) then
      idx = idx + 1
      nlist = this%ncv
      call this%budobj%budterm(idx)%reset(nlist)
      do n1 = 1, nlist
        q = this%qmfrommvr(n1)
        call this%budobj%budterm(idx)%update_term(n1, n1, q)
        call this%apt_accumulate_ccterm(n1, q, ccratin, ccratout)
      end do
    end if

    ! -- CONSTANT FLOW
    idx = idx + 1
    call this%budobj%budterm(idx)%reset(this%ncv)
    do n1 = 1, this%ncv
      q = this%ccterm(n1)
      call this%budobj%budterm(idx)%update_term(n1, n1, q)
    end do

    ! -- AUXILIARY VARIABLES
    naux = this%naux
    if (naux > 0) then
      idx = idx + 1
      allocate (auxvartmp(naux))
      call this%budobj%budterm(idx)%reset(this%ncv)
      do n1 = 1, this%ncv
        q = DZERO
        do i = 1, naux
          auxvartmp(i) = this%lauxvar(i, n1)
        end do
        call this%budobj%budterm(idx)%update_term(n1, n1, q, auxvartmp)
      end do
      deallocate (auxvartmp)
    end if
    !
    ! --Terms are filled, now accumulate them for this time step
    call this%budobj%accumulate_terms()
    !
    ! -- return
    return
  end subroutine apt_fill_budobj

  subroutine pak_fill_budobj(this, idx, x, ccratin, ccratout)
! ******************************************************************************
! pak_fill_budobj -- copy flow terms into this%budobj, must be overridden
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(GwtAptType) :: this
    integer(I4B), intent(inout) :: idx
    real(DP), dimension(:), intent(in) :: x
    real(DP), intent(inout) :: ccratin
    real(DP), intent(inout) :: ccratout
    ! -- local
    ! -- formats
! -----------------------------------------------------------------------------
    !
    ! -- this routine should never be called
    call store_error('Program error: pak_fill_budobj not implemented.', &
                     terminate=.TRUE.)
    !
    ! -- return
    return
  end subroutine pak_fill_budobj

  subroutine apt_stor_term(this, ientry, n1, n2, rrate, &
                           rhsval, hcofval)
    use TdisModule, only: delt
    class(GwtAptType) :: this
    integer(I4B), intent(in) :: ientry
    integer(I4B), intent(inout) :: n1
    integer(I4B), intent(inout) :: n2
    real(DP), intent(inout), optional :: rrate
    real(DP), intent(inout), optional :: rhsval
    real(DP), intent(inout), optional :: hcofval
    real(DP) :: v0, v1
    real(DP) :: c0, c1
    n1 = ientry
    n2 = ientry
    call this%get_volumes(n1, v1, v0, delt)
    c0 = this%xoldpak(n1)
    c1 = this%xnewpak(n1)
    if (present(rrate)) rrate = -c1 * v1 / delt + c0 * v0 / delt
    if (present(rhsval)) rhsval = -c0 * v0 / delt
    if (present(hcofval)) hcofval = -v1 / delt
    !
    ! -- return
    return
  end subroutine apt_stor_term

  subroutine apt_tmvr_term(this, ientry, n1, n2, rrate, &
                           rhsval, hcofval)
    class(GwtAptType) :: this
    integer(I4B), intent(in) :: ientry
    integer(I4B), intent(inout) :: n1
    integer(I4B), intent(inout) :: n2
    real(DP), intent(inout), optional :: rrate
    real(DP), intent(inout), optional :: rhsval
    real(DP), intent(inout), optional :: hcofval
    real(DP) :: qbnd
    real(DP) :: ctmp
    n1 = this%flowbudptr%budterm(this%idxbudtmvr)%id1(ientry)
    n2 = this%flowbudptr%budterm(this%idxbudtmvr)%id2(ientry)
    qbnd = this%flowbudptr%budterm(this%idxbudtmvr)%flow(ientry)
    ctmp = this%xnewpak(n1)
    if (present(rrate)) rrate = ctmp * qbnd
    if (present(rhsval)) rhsval = DZERO
    if (present(hcofval)) hcofval = qbnd
    !
    ! -- return
    return
  end subroutine apt_tmvr_term

  subroutine apt_fjf_term(this, ientry, n1, n2, rrate, &
                          rhsval, hcofval)
    class(GwtAptType) :: this
    integer(I4B), intent(in) :: ientry
    integer(I4B), intent(inout) :: n1
    integer(I4B), intent(inout) :: n2
    real(DP), intent(inout), optional :: rrate
    real(DP), intent(inout), optional :: rhsval
    real(DP), intent(inout), optional :: hcofval
    real(DP) :: qbnd
    real(DP) :: ctmp
    n1 = this%flowbudptr%budterm(this%idxbudfjf)%id1(ientry)
    n2 = this%flowbudptr%budterm(this%idxbudfjf)%id2(ientry)
    qbnd = this%flowbudptr%budterm(this%idxbudfjf)%flow(ientry)
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
  end subroutine apt_fjf_term

  subroutine apt_copy2flowp(this)
! ******************************************************************************
! apt_copy2flowp -- copy concentrations into flow package aux variable
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(GwtAptType) :: this
    ! -- local
    integer(I4B) :: n, j
! ------------------------------------------------------------------------------
    !
    ! -- copy
    if (this%iauxfpconc /= 0) then
      !
      ! -- go through each apt-gwf connection
      do j = 1, this%flowbudptr%budterm(this%idxbudgwf)%nlist
        !
        ! -- set n to feature number and process if active feature
        n = this%flowbudptr%budterm(this%idxbudgwf)%id1(j)
        this%flowpackagebnd%auxvar(this%iauxfpconc, j) = this%xnewpak(n)
      end do
    end if
    !
    ! -- return
    return
  end subroutine apt_copy2flowp

  logical function apt_obs_supported(this)
! ******************************************************************************
! apt_obs_supported -- obs are supported?
!   -- Return true because APT package supports observations.
!   -- Overrides BndType%bnd_obs_supported()
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(GwtAptType) :: this
! ------------------------------------------------------------------------------
    !
    ! -- Set to true
    apt_obs_supported = .true.
    !
    ! -- return
    return
  end function apt_obs_supported

  subroutine apt_df_obs(this)
! ******************************************************************************
! apt_df_obs -- obs are supported?
!   -- Store observation type supported by APT package.
!   -- Overrides BndType%bnd_df_obs
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(GwtAptType) :: this
    ! -- local
! ------------------------------------------------------------------------------
    !
    ! -- call additional specific observations for lkt, sft, mwt, and uzt
    call this%pak_df_obs()
    !
    return
  end subroutine apt_df_obs

  subroutine pak_df_obs(this)
! ******************************************************************************
! pak_df_obs -- obs are supported?
!   -- Store observation type supported by APT package.
!   -- must be overridden by child class
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(GwtAptType) :: this
    ! -- local
! ------------------------------------------------------------------------------
    !
    ! -- this routine should never be called
    call store_error('Program error: pak_df_obs not implemented.', &
                     terminate=.TRUE.)
    !
    return
  end subroutine pak_df_obs

  !> @brief Process package specific obs
    !!
    !! Method to process specific observations for this package.
    !!
  !<
  subroutine pak_rp_obs(this, obsrv, found)
    ! -- dummy
    class(GwtAptType), intent(inout) :: this !< package class
    type(ObserveType), intent(inout) :: obsrv !< observation object
    logical, intent(inout) :: found !< indicate whether observation was found
    ! -- local
    !
    ! -- this routine should never be called
    call store_error('Program error: pak_rp_obs not implemented.', &
                     terminate=.TRUE.)
    !
    return
  end subroutine pak_rp_obs

  !> @brief Prepare observation
    !!
    !! Find the indices for this observation assuming
    !! they are indexed by feature number
    !!
  !<
  subroutine rp_obs_byfeature(this, obsrv)
    class(GwtAptType), intent(inout) :: this !< object
    type(ObserveType), intent(inout) :: obsrv !< observation
    integer(I4B) :: nn1
    integer(I4B) :: j
    logical :: jfound
    character(len=*), parameter :: fmterr = &
      "('Boundary ', a, ' for observation ', a, &
      &' is invalid in package ', a)"
    nn1 = obsrv%NodeNumber
    if (nn1 == NAMEDBOUNDFLAG) then
      jfound = .false.
      do j = 1, this%ncv
        if (this%featname(j) == obsrv%FeatureName) then
          jfound = .true.
          call obsrv%AddObsIndex(j)
        end if
      end do
      if (.not. jfound) then
        write (errmsg, fmterr) trim(obsrv%FeatureName), trim(obsrv%Name), &
          trim(this%packName)
        call store_error(errmsg)
      end if
    else
      !
      ! -- ensure nn1 is > 0 and < ncv
      if (nn1 < 0 .or. nn1 > this%ncv) then
        write (errmsg, '(7a, i0, a, i0, a)') &
          'Observation ', trim(obsrv%Name), ' of type ', &
          trim(adjustl(obsrv%ObsTypeId)), ' in package ', &
          trim(this%packName), ' was assigned ID = ', nn1, &
          '.  ID must be >= 1 and <= ', this%ncv, '.'
        call store_error(errmsg)
      end if
      call obsrv%AddObsIndex(nn1)
    end if
    return
  end subroutine rp_obs_byfeature

  !> @brief Prepare observation
    !!
    !! Find the indices for this observation assuming
    !! they are first indexed by feature number and
    !! secondly by a connection number
    !!
  !<
  subroutine rp_obs_budterm(this, obsrv, budterm)
    class(GwtAptType), intent(inout) :: this !< object
    type(ObserveType), intent(inout) :: obsrv !< observation
    type(BudgetTermType), intent(in) :: budterm !< budget term
    integer(I4B) :: nn1
    integer(I4B) :: iconn
    integer(I4B) :: icv
    integer(I4B) :: idx
    integer(I4B) :: j
    logical :: jfound
    character(len=*), parameter :: fmterr = &
      "('Boundary ', a, ' for observation ', a, &
      &' is invalid in package ', a)"
    nn1 = obsrv%NodeNumber
    if (nn1 == NAMEDBOUNDFLAG) then
      jfound = .false.
      do j = 1, budterm%nlist
        icv = budterm%id1(j)
        if (this%featname(icv) == obsrv%FeatureName) then
          jfound = .true.
          call obsrv%AddObsIndex(j)
        end if
      end do
      if (.not. jfound) then
        write (errmsg, fmterr) trim(obsrv%FeatureName), trim(obsrv%Name), &
          trim(this%packName)
        call store_error(errmsg)
      end if
    else
      !
      ! -- ensure nn1 is > 0 and < ncv
      if (nn1 < 0 .or. nn1 > this%ncv) then
        write (errmsg, '(7a, i0, a, i0, a)') &
          'Observation ', trim(obsrv%Name), ' of type ', &
          trim(adjustl(obsrv%ObsTypeId)), ' in package ', &
          trim(this%packName), ' was assigned ID = ', nn1, &
          '.  ID must be >= 1 and <= ', this%ncv, '.'
        call store_error(errmsg)
      end if
      iconn = obsrv%NodeNumber2
      do j = 1, budterm%nlist
        if (budterm%id1(j) == nn1) then
          ! -- Look for the first occurrence of nn1, then set indxbnds
          !    to the iconn record after that
          idx = j + iconn - 1
          call obsrv%AddObsIndex(idx)
          exit
        end if
      end do
      if (idx < 1 .or. idx > budterm%nlist) then
        write (errmsg, '(7a, i0, a, i0, a)') &
          'Observation ', trim(obsrv%Name), ' of type ', &
          trim(adjustl(obsrv%ObsTypeId)), ' in package ', &
          trim(this%packName), ' specifies iconn = ', iconn, &
          ',  but this is not a valid connection for ID ', nn1, '.'
        call store_error(errmsg)
      else if (budterm%id1(idx) /= nn1) then
        write (errmsg, '(7a, i0, a, i0, a)') &
          'Observation ', trim(obsrv%Name), ' of type ', &
          trim(adjustl(obsrv%ObsTypeId)), ' in package ', &
          trim(this%packName), ' specifies iconn = ', iconn, &
          ',  but this is not a valid connection for ID ', nn1, '.'
        call store_error(errmsg)
      end if
    end if
    return
  end subroutine rp_obs_budterm

  !> @brief Prepare observation
    !!
    !! Find the indices for this observation assuming
    !! they are first indexed by a feature number and
    !! secondly by a second feature number
    !!
  !<
  subroutine rp_obs_flowjaface(this, obsrv, budterm)
    class(GwtAptType), intent(inout) :: this !< object
    type(ObserveType), intent(inout) :: obsrv !< observation
    type(BudgetTermType), intent(in) :: budterm !< budget term
    integer(I4B) :: nn1
    integer(I4B) :: nn2
    integer(I4B) :: icv
    integer(I4B) :: j
    logical :: jfound
    character(len=*), parameter :: fmterr = &
      "('Boundary ', a, ' for observation ', a, &
      &' is invalid in package ', a)"
    nn1 = obsrv%NodeNumber
    if (nn1 == NAMEDBOUNDFLAG) then
      jfound = .false.
      do j = 1, budterm%nlist
        icv = budterm%id1(j)
        if (this%featname(icv) == obsrv%FeatureName) then
          jfound = .true.
          call obsrv%AddObsIndex(j)
        end if
      end do
      if (.not. jfound) then
        write (errmsg, fmterr) trim(obsrv%FeatureName), trim(obsrv%Name), &
          trim(this%packName)
        call store_error(errmsg)
      end if
    else
      !
      ! -- ensure nn1 is > 0 and < ncv
      if (nn1 < 0 .or. nn1 > this%ncv) then
        write (errmsg, '(7a, i0, a, i0, a)') &
          'Observation ', trim(obsrv%Name), ' of type ', &
          trim(adjustl(obsrv%ObsTypeId)), ' in package ', &
          trim(this%packName), ' was assigned ID = ', nn1, &
          '.  ID must be >= 1 and <= ', this%ncv, '.'
        call store_error(errmsg)
      end if
      nn2 = obsrv%NodeNumber2
      !
      ! -- ensure nn2 is > 0 and < ncv
      if (nn2 < 0 .or. nn2 > this%ncv) then
        write (errmsg, '(7a, i0, a, i0, a)') &
          'Observation ', trim(obsrv%Name), ' of type ', &
          trim(adjustl(obsrv%ObsTypeId)), ' in package ', &
          trim(this%packName), ' was assigned ID2 = ', nn2, &
          '.  ID must be >= 1 and <= ', this%ncv, '.'
        call store_error(errmsg)
      end if
      ! -- Look for nn1 and nn2 in id1 and id2
      jfound = .false.
      do j = 1, budterm%nlist
        if (budterm%id1(j) == nn1 .and. budterm%id2(j) == nn2) then
          call obsrv%AddObsIndex(j)
          jfound = .true.
        end if
      end do
      if (.not. jfound) then
        write (errmsg, '(7a, i0, a, i0, a)') &
          'Observation ', trim(obsrv%Name), ' of type ', &
          trim(adjustl(obsrv%ObsTypeId)), ' in package ', &
          trim(this%packName), &
          ' specifies a connection between feature ', nn1, &
          ' feature ', nn2, ', but these features are not connected.'
        call store_error(errmsg)
      end if
    end if
    return
  end subroutine rp_obs_flowjaface

  subroutine apt_rp_obs(this)
! ******************************************************************************
! apt_rp_obs --
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use TdisModule, only: kper
    ! -- dummy
    class(GwtAptType), intent(inout) :: this
    ! -- local
    integer(I4B) :: i
    logical :: found
    class(ObserveType), pointer :: obsrv => null()
! ------------------------------------------------------------------------------
    !
    if (kper == 1) then
      do i = 1, this%obs%npakobs
        obsrv => this%obs%pakobs(i)%obsrv
        select case (obsrv%ObsTypeId)
        case ('CONCENTRATION')
          call this%rp_obs_byfeature(obsrv)
          !
          ! -- catch non-cumulative observation assigned to observation defined
          !    by a boundname that is assigned to more than one element
          if (obsrv%indxbnds_count > 1) then
            write (errmsg, '(a, a, a)') &
              'CONCENTRATION for observation', trim(adjustl(obsrv%Name)), &
              ' must be assigned to a feature with a unique boundname.'
            call store_error(errmsg)
          end if
        case ('LKT', 'SFT', 'MWT', 'UZT')
          call this%rp_obs_budterm(obsrv, &
                                   this%flowbudptr%budterm(this%idxbudgwf))
        case ('FLOW-JA-FACE')
          if (this%idxbudfjf > 0) then
            call this%rp_obs_flowjaface(obsrv, &
                                        this%flowbudptr%budterm(this%idxbudfjf))
          else
            write (errmsg, '(7a)') &
              'Observation ', trim(obsrv%Name), ' of type ', &
              trim(adjustl(obsrv%ObsTypeId)), ' in package ', &
              trim(this%packName), &
              ' cannot be processed because there are no flow connections.'
            call store_error(errmsg)
          end if
        case ('STORAGE')
          call this%rp_obs_byfeature(obsrv)
        case ('CONSTANT')
          call this%rp_obs_byfeature(obsrv)
        case ('FROM-MVR')
          call this%rp_obs_byfeature(obsrv)
        case default
          !
          ! -- check the child package for any specific obs
          found = .false.
          call this%pak_rp_obs(obsrv, found)
          !
          ! -- if none found then terminate with an error
          if (.not. found) then
            errmsg = 'Unrecognized observation type "'// &
                     trim(obsrv%ObsTypeId)//'" for '// &
                     trim(adjustl(this%text))//' package '// &
                     trim(this%packName)
            call store_error(errmsg, terminate=.TRUE.)
          end if
        end select

      end do
      !
      ! -- check for errors
      if (count_errors() > 0) then
        call store_error_unit(this%obs%inunitobs)
      end if
    end if
    !
    return
  end subroutine apt_rp_obs

  subroutine apt_bd_obs(this)
! ******************************************************************************
! apt_bd_obs -- Calculate observations common to SFT/LKT/MWT/UZT
!      ObsType%SaveOneSimval for each GwtAptType observation.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(GwtAptType) :: this
    ! -- local
    integer(I4B) :: i
    integer(I4B) :: igwfnode
    integer(I4B) :: j
    integer(I4B) :: jj
    integer(I4B) :: n
    integer(I4B) :: n1
    integer(I4B) :: n2
    real(DP) :: v
    type(ObserveType), pointer :: obsrv => null()
    logical :: found
! ------------------------------------------------------------------------------
    !
    ! -- Write simulated values for all LAK observations
    if (this%obs%npakobs > 0) then
      call this%obs%obs_bd_clear()
      do i = 1, this%obs%npakobs
        obsrv => this%obs%pakobs(i)%obsrv
        do j = 1, obsrv%indxbnds_count
          v = DNODATA
          jj = obsrv%indxbnds(j)
          select case (obsrv%ObsTypeId)
          case ('CONCENTRATION')
            if (this%iboundpak(jj) /= 0) then
              v = this%xnewpak(jj)
            end if
          case ('LKT', 'SFT', 'MWT', 'UZT')
            n = this%flowbudptr%budterm(this%idxbudgwf)%id1(jj)
            if (this%iboundpak(n) /= 0) then
              igwfnode = this%flowbudptr%budterm(this%idxbudgwf)%id2(jj)
              v = this%hcof(jj) * this%xnew(igwfnode) - this%rhs(jj)
              v = -v
            end if
          case ('FLOW-JA-FACE')
            n = this%flowbudptr%budterm(this%idxbudfjf)%id1(jj)
            if (this%iboundpak(n) /= 0) then
              call this%apt_fjf_term(jj, n1, n2, v)
            end if
          case ('STORAGE')
            if (this%iboundpak(jj) /= 0) then
              v = this%qsto(jj)
            end if
          case ('CONSTANT')
            if (this%iboundpak(jj) /= 0) then
              v = this%ccterm(jj)
            end if
          case ('FROM-MVR')
            if (this%iboundpak(jj) /= 0 .and. this%idxbudfmvr > 0) then
              v = this%qmfrommvr(jj)
            end if
          case ('TO-MVR')
            if (this%idxbudtmvr > 0) then
              n = this%flowbudptr%budterm(this%idxbudtmvr)%id1(jj)
              if (this%iboundpak(n) /= 0) then
                call this%apt_tmvr_term(jj, n1, n2, v)
              end if
            end if
          case default
            found = .false.
            !
            ! -- check the child package for any specific obs
            call this%pak_bd_obs(obsrv%ObsTypeId, jj, v, found)
            !
            ! -- if none found then terminate with an error
            if (.not. found) then
              errmsg = 'Unrecognized observation type "'// &
                       trim(obsrv%ObsTypeId)//'" for '// &
                       trim(adjustl(this%text))//' package '// &
                       trim(this%packName)
              call store_error(errmsg, terminate=.TRUE.)
            end if
          end select
          call this%obs%SaveOneSimval(obsrv, v)
        end do
      end do
      !
      ! -- write summary of error messages
      if (count_errors() > 0) then
        call store_error_unit(this%obs%inunitobs)
      end if
    end if
    !
    return
  end subroutine apt_bd_obs

  subroutine pak_bd_obs(this, obstypeid, jj, v, found)
! ******************************************************************************
! pak_bd_obs --
!   -- check for observations in concrete packages.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(GwtAptType), intent(inout) :: this
    character(len=*), intent(in) :: obstypeid
    integer(I4B), intent(in) :: jj
    real(DP), intent(inout) :: v
    logical, intent(inout) :: found
    ! -- local
! ------------------------------------------------------------------------------
    !
    ! -- set found = .false. because obstypeid is not known
    found = .false.
    !
    return
  end subroutine pak_bd_obs

  !> @brief Process observation IDs for a package
    !!
    !! Method to process observation ID strings for an APT package.
    !! This processor is only for observation types that support ID1
    !! and not ID2.
    !!
  !<
  subroutine apt_process_obsID(obsrv, dis, inunitobs, iout)
    ! -- dummy variables
    type(ObserveType), intent(inout) :: obsrv !< Observation object
    class(DisBaseType), intent(in) :: dis !< Discretization object
    integer(I4B), intent(in) :: inunitobs !< file unit number for the package observation file
    integer(I4B), intent(in) :: iout !< model listing file unit number
    ! -- local variables
    integer(I4B) :: nn1
    integer(I4B) :: icol
    integer(I4B) :: istart
    integer(I4B) :: istop
    character(len=LINELENGTH) :: strng
    character(len=LENBOUNDNAME) :: bndname
    !
    ! -- initialize local variables
    strng = obsrv%IDstring
    !
    ! -- Extract reach number from strng and store it.
    !    If 1st item is not an integer(I4B), it should be a
    !    boundary name--deal with it.
    icol = 1
    !
    ! -- get reach number or boundary name
    call extract_idnum_or_bndname(strng, icol, istart, istop, nn1, bndname)
    if (nn1 == NAMEDBOUNDFLAG) then
      obsrv%FeatureName = bndname
    end if
    !
    ! -- store reach number (NodeNumber)
    obsrv%NodeNumber = nn1
    !
    ! -- store NodeNumber2 as 1 so that this can be used
    !    as the iconn value for SFT.  This works for SFT
    !    because there is only one reach per GWT connection.
    obsrv%NodeNumber2 = 1
    !
    ! -- return
    return
  end subroutine apt_process_obsID

  !> @brief Process observation IDs for a package
    !!
    !! Method to process observation ID strings for an APT package.
    !! This processor is for the case where if ID1 is an integer
    !! then ID2 must be provided.
    !!
  !<
  subroutine apt_process_obsID12(obsrv, dis, inunitobs, iout)
    ! -- dummy variables
    type(ObserveType), intent(inout) :: obsrv !< Observation object
    class(DisBaseType), intent(in) :: dis !< Discretization object
    integer(I4B), intent(in) :: inunitobs !< file unit number for the package observation file
    integer(I4B), intent(in) :: iout !< model listing file unit number
    ! -- local variables
    integer(I4B) :: nn1
    integer(I4B) :: iconn
    integer(I4B) :: icol
    integer(I4B) :: istart
    integer(I4B) :: istop
    character(len=LINELENGTH) :: strng
    character(len=LENBOUNDNAME) :: bndname
    !
    ! -- initialize local variables
    strng = obsrv%IDstring
    !
    ! -- Extract reach number from strng and store it.
    !    If 1st item is not an integer(I4B), it should be a
    !    boundary name--deal with it.
    icol = 1
    !
    ! -- get reach number or boundary name
    call extract_idnum_or_bndname(strng, icol, istart, istop, nn1, bndname)
    if (nn1 == NAMEDBOUNDFLAG) then
      obsrv%FeatureName = bndname
    else
      call extract_idnum_or_bndname(strng, icol, istart, istop, iconn, bndname)
      if (len_trim(bndName) < 1 .and. iconn < 0) then
        write (errmsg, '(a,1x,a,a,1x,a,1x,a)') &
          'For observation type', trim(adjustl(obsrv%ObsTypeId)), &
          ', ID given as an integer and not as boundname,', &
          'but ID2 is missing.  Either change ID to valid', &
          'boundname or supply valid entry for ID2.'
        call store_error(errmsg)
      end if
      obsrv%NodeNumber2 = iconn
    end if
    !
    ! -- store reach number (NodeNumber)
    obsrv%NodeNumber = nn1
    !
    ! -- return
    return
  end subroutine apt_process_obsID12

  subroutine apt_setup_tableobj(this)
! ******************************************************************************
! apt_setup_tableobj -- Set up the table object that is used to write the apt
!                       conc data. The terms listed here must correspond in
!                       in the apt_ot method.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use ConstantsModule, only: LINELENGTH, LENBUDTXT
    ! -- dummy
    class(GwtAptType) :: this
    ! -- local
    integer(I4B) :: nterms
    character(len=LINELENGTH) :: title
    character(len=LINELENGTH) :: text_temp
! ------------------------------------------------------------------------------
    !
    ! -- setup well head table
    if (this%iprconc > 0) then
      !
      ! -- Determine the number of head table columns
      nterms = 2
      if (this%inamedbound == 1) nterms = nterms + 1
      !
      ! -- set up table title
      title = trim(adjustl(this%text))//' PACKAGE ('// &
              trim(adjustl(this%packName))// &
              ') CONCENTRATION FOR EACH CONTROL VOLUME'
      !
      ! -- set up dv tableobj
      call table_cr(this%dvtab, this%packName, title)
      call this%dvtab%table_df(this%ncv, nterms, this%iout, &
                               transient=.TRUE.)
      !
      ! -- Go through and set up table budget term
      if (this%inamedbound == 1) then
        text_temp = 'NAME'
        call this%dvtab%initialize_column(text_temp, 20, alignment=TABLEFT)
      end if
      !
      ! -- feature number
      text_temp = 'NUMBER'
      call this%dvtab%initialize_column(text_temp, 10, alignment=TABCENTER)
      !
      ! -- feature conc
      text_temp = 'CONC'
      call this%dvtab%initialize_column(text_temp, 12, alignment=TABCENTER)
    end if
    !
    ! -- return
    return
  end subroutine apt_setup_tableobj

end module GwtAptModule
