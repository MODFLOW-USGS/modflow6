! -- Unsaturated Zone Flow Energy Transport Module
! -- todo: save the uze temperature into the uze aux variable?
! -- todo: calculate the uzf DENSE aux variable using temperature?
! -- todo: GWD and GWD-TO-MVR do not seem to be included; prob in UZF?
!
! UZF flows (flowbudptr)     index var    UZE term              Transport Type
!---------------------------------------------------------------------------------

! -- terms from UZF that will be handled by parent APT Package
! FLOW-JA-FACE              idxbudfjf     FLOW-JA-FACE          cv2cv
! GWF (aux FLOW-AREA)       idxbudgwf     GWF                   uzf2gwf
! STORAGE (aux VOLUME)      idxbudsto     none                  used for water volumes
! FROM-MVR                  idxbudfmvr    FROM-MVR              q * cext = this%qfrommvr(:)
! AUXILIARY                 none          none                  none
! none                      none          STORAGE (aux MASS)
! none                      none          AUXILIARY             none

! -- terms from UZF that need to be handled here
! INFILTRATION              idxbudinfl    INFILTRATION          q < 0: q * cwell, else q * cuser
! REJ-INF                   idxbudrinf    REJ-INF               q * cuze
! UZET                      idxbuduzet    UZET                  q * cet
! REJ-INF-TO-MVR            idxbudritm    REJ-INF-TO-MVR        q * cinfil?
! THERMAL-EQUIL             idxbudtheq    THERMAL-EQUIL         residual

! -- terms from UZF that should be skipped

module GweUzeModule

  use KindModule, only: DP, I4B
  use ConstantsModule, only: DZERO, DONE, LINELENGTH
  use SimModule, only: store_error
  use BndModule, only: BndType, GetBndFromList
  use TspFmiModule, only: TspFmiType
  use UzfModule, only: UzfType
  use ObserveModule, only: ObserveType
  use TspAptModule, only: TspAptType, apt_process_obsID, &
                          apt_process_obsID12
  use TspLabelsModule, only: TspLabelsType
  use GweInputDataModule, only: GweInputDataType
  use MatrixModule
 
  implicit none
 
  public uze_create

  character(len=*), parameter :: ftype = 'UZE'
  character(len=*), parameter :: flowtype = 'UZF'
  character(len=16) :: text = '             UZE'

  type, extends(TspAptType) :: GweUzeType
    
    type(GweInputDataType), pointer :: gwecommon => null() !< pointer to shared gwe data used by multiple packages but set in mst

    integer(I4B), pointer :: idxbudinfl => null() ! index of uzf infiltration terms in flowbudptr
    integer(I4B), pointer :: idxbudrinf => null() ! index of rejected infiltration terms in flowbudptr
    integer(I4B), pointer :: idxbuduzet => null() ! index of unsat et terms in flowbudptr
    integer(I4B), pointer :: idxbudritm => null() ! index of rej infil to mover rate to mover terms in flowbudptr
    integer(I4B), pointer :: idxbudtheq => null() ! index of thermal equilibration terms in flowbudptr
    real(DP), dimension(:), pointer, contiguous :: tempinfl => null() ! infiltration temperature
    real(DP), dimension(:), pointer, contiguous :: tempuzet => null() ! unsat et temperature

  contains

    procedure :: bnd_da => uze_da
    procedure :: allocate_scalars
    procedure :: apt_allocate_arrays => uze_allocate_arrays
    procedure :: find_apt_package => find_uze_package
    procedure :: apt_fc_expanded => uze_fc_expanded
    procedure :: apt_cfupdate => uze_cfupdate
    procedure :: pak_solve => uze_solve
    procedure :: pak_get_nbudterms => uze_get_nbudterms
    procedure :: pak_setup_budobj => uze_setup_budobj
    procedure :: pak_fill_budobj => uze_fill_budobj
    procedure :: uze_infl_term
    procedure :: uze_rinf_term
    procedure :: uze_uzet_term
    procedure :: uze_ritm_term
    procedure :: uze_theq_term
    procedure :: apt_stor_term => uze_stor_term
    procedure :: apt_tmvr_term => uze_tmvr_term
    procedure :: apt_fmvr_term => uze_fmvr_term
    procedure :: apt_fjf_term => uze_fjf_term
    procedure :: pak_df_obs => uze_df_obs
    procedure :: pak_rp_obs => uze_rp_obs
    procedure :: pak_bd_obs => uze_bd_obs
    procedure :: pak_set_stressperiod => uze_set_stressperiod
    procedure :: bnd_ac => uze_ac
    procedure :: bnd_mc => uze_mc

  end type GweUzeType

contains

  subroutine uze_create(packobj, id, ibcnum, inunit, iout, namemodel, pakname, &
                        fmi, tsplab, eqnsclfac, gwecommon)
! ******************************************************************************
! uze_create -- Create a New UZE Package
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(BndType), pointer :: packobj
    integer(I4B), intent(in) :: id
    integer(I4B), intent(in) :: ibcnum
    integer(I4B), intent(in) :: inunit
    integer(I4B), intent(in) :: iout
    character(len=*), intent(in) :: namemodel
    character(len=*), intent(in) :: pakname
    type(TspFmiType), pointer :: fmi
    type(TspLabelsType), pointer :: tsplab
    real(DP), intent(in), pointer :: eqnsclfac !< governing equation scale factor
    type(GweInputDataType), intent(in), target :: gwecommon !< shared data container for use by multiple GWE packages
    ! -- local
    type(GweUzeType), pointer :: uzeobj
! ------------------------------------------------------------------------------
    !
    ! -- allocate the object and assign values to object variables
    allocate (uzeobj)
    packobj => uzeobj
    !
    ! -- create name and memory path
    call packobj%set_names(ibcnum, namemodel, pakname, ftype)
    packobj%text = text
    !
    ! -- allocate scalars
    call uzeobj%allocate_scalars()
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
    uzeobj%fmi => fmi
    !
    ! -- Store pointer to the labels module for dynamic setting of 
    !    concentration vs temperature
    uzeobj%tsplab => tsplab
    !
    ! -- Store pointer to governing equation scale factor
    uzeobj%eqnsclfac => eqnsclfac
    !
    ! -- Store pointer to shared data module for accessing cpw, rhow
    !    for the budget calculations, and for accessing the latent heat of
    !    vaporization
    uzeobj%gwecommon => gwecommon
    !
    ! -- return
    return
  end subroutine uze_create

  subroutine find_uze_package(this)
! ******************************************************************************
! find corresponding uze package
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryManagerModule, only: mem_allocate
    ! -- dummy
    class(GweUzeType) :: this
    ! -- local
    character(len=LINELENGTH) :: errmsg
    class(BndType), pointer :: packobj
    integer(I4B) :: ip, icount
    integer(I4B) :: nbudterm
    logical :: found
! ------------------------------------------------------------------------------
    !
    ! -- Initialize found to false, and error later if flow package cannot
    !    be found
    found = .false.
    !
    ! -- If user is specifying flows in a binary budget file, then set up
    !    the budget file reader, otherwise set a pointer to the flow package
    !    budobj
    if (this%fmi%flows_from_file) then
      call this%fmi%set_aptbudobj_pointer(this%flowpackagename, this%flowbudptr)
      if (associated(this%flowbudptr)) found = .true.
      !
    else
      if (associated(this%fmi%gwfbndlist)) then
        ! -- Look through gwfbndlist for a flow package with the same name as
        !    this transport package name
        do ip = 1, this%fmi%gwfbndlist%Count()
          packobj => GetBndFromList(this%fmi%gwfbndlist, ip)
          if (packobj%packName == this%flowpackagename) then
            found = .true.
            !
            ! -- store BndType pointer to packobj, and then
            !    use the select type to point to the budobj in flow package
            this%flowpackagebnd => packobj
            select type (packobj)
            type is (UzfType)
              this%flowbudptr => packobj%budobj
            end select
          end if
          if (found) exit
        end do
      end if
    end if
    !
    ! -- error if flow package not found
    if (.not. found) then
      write (errmsg, '(a)') 'COULD NOT FIND FLOW PACKAGE WITH NAME '&
                            &//trim(adjustl(this%flowpackagename))//'.'
      call store_error(errmsg)
      call this%parser%StoreErrorUnit()
    end if
    !
    ! -- allocate space for idxbudssm, which indicates whether this is a
    !    special budget term or one that is a general source and sink
    nbudterm = this%flowbudptr%nbudterm
    call mem_allocate(this%idxbudssm, nbudterm, 'IDXBUDSSM', this%memoryPath)
    !
    ! -- Process budget terms and identify special budget terms
    write (this%iout, '(/, a, a)') &
      'PROCESSING '//ftype//' INFORMATION FOR ', this%packName
    write (this%iout, '(a)') '  IDENTIFYING FLOW TERMS IN '//flowtype//' PACKAGE'
    write (this%iout, '(a, i0)') &
      '  NUMBER OF '//flowtype//' = ', this%flowbudptr%ncv
    icount = 1
    do ip = 1, this%flowbudptr%nbudterm
      select case (trim(adjustl(this%flowbudptr%budterm(ip)%flowtype)))
      case ('FLOW-JA-FACE')
        this%idxbudfjf = ip
        this%idxbudssm(ip) = 0
      case ('GWF')
        this%idxbudgwf = ip
        this%idxbudssm(ip) = 0
      case ('STORAGE')
        this%idxbudsto = ip
        this%idxbudssm(ip) = 0
      case ('INFILTRATION')
        this%idxbudinfl = ip
        this%idxbudssm(ip) = 0
      case ('REJ-INF')
        this%idxbudrinf = ip
        this%idxbudssm(ip) = 0
      case ('UZET')
        this%idxbuduzet = ip
        this%idxbudssm(ip) = 0
      case ('REJ-INF-TO-MVR')
        this%idxbudritm = ip
        this%idxbudssm(ip) = 0
      case ('TO-MVR')
        this%idxbudtmvr = ip
        this%idxbudssm(ip) = 0
      case ('FROM-MVR')
        this%idxbudfmvr = ip
        this%idxbudssm(ip) = 0
      case ('THERMAL-EQUIL')
        this%idxbudtheq= ip
        this%idxbudssm(ip) = 0
      case ('AUXILIARY')
        this%idxbudaux = ip
        this%idxbudssm(ip) = 0
      case default
        !
        ! -- set idxbudssm equal to a column index for where the temperatures
        !    are stored in the tempbud(nbudssm, ncv) array
        this%idxbudssm(ip) = icount
        icount = icount + 1
      end select
      !
      ! -- thermal equilibration term
      this%idxbudtheq = this%flowbudptr%nbudterm + 1
      !
      write (this%iout, '(a, i0, " = ", a,/, a, i0)') &
        '  TERM ', ip, trim(adjustl(this%flowbudptr%budterm(ip)%flowtype)), &
        '   MAX NO. OF ENTRIES = ', this%flowbudptr%budterm(ip)%maxlist
    end do
    write (this%iout, '(a, //)') 'DONE PROCESSING '//ftype//' INFORMATION'
    !
    ! -- Return
    return
  end subroutine find_uze_package
  
  subroutine uze_ac(this, moffset, sparse)
! ******************************************************************************
! uze_ac -- Add package connection to matrix. Overrides apt_ac to fold the
!           UZE heat balance terms into the row corresponding to the host cell
!           and enforce thermal equilibrium between UZE and the GWE cell.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use MemoryManagerModule, only: mem_setptr
    use SparseModule, only: sparsematrix
    ! -- dummy
    class(GweUzeType), intent(inout) :: this
    integer(I4B), intent(in) :: moffset !< current models starting position in global matrix
    type(sparsematrix), intent(inout) :: sparse
    ! -- local
    integer(I4B) :: i, ii
    integer(I4B) :: n !< index of a uze object within the complete list of uze objects
    integer(I4B) :: jj !< 
    integer(I4B) :: nglo !< index of uze object in global matrix
    integer(I4B) :: jglo !< host cell's position in global matrix for a uze object
    integer(I4B) :: idxn !< used for cross-check
    integer(I4B) :: idxjj !< used for cross-check
    integer(I4B) :: idxnglo !< used for cross-check
    integer(I4B) :: idxjglo !< used for cross-check
    ! -- format
! ------------------------------------------------------------------------------
    !
    ! -- Add package rows to sparse
    if (this%imatrows /= 0) then
      !
      ! -- diagonal on the row assoc. with the uze feature
      do n = 1, this%ncv
        nglo = moffset + this%dis%nodes + this%ioffset + n
        call sparse%addconnection(nglo, nglo, 1)
      end do
      !
      ! -- add uze-to-gwe connections. For uze, this particular do loop
      !    is the same as its counterpart in apt_ac.
      ! nlist: number of gwe cells with a connection to at least one uze object
      do i = 1, this%flowbudptr%budterm(this%idxbudgwf)%nlist
        n = this%flowbudptr%budterm(this%idxbudgwf)%id1(i) !< uze object position within uze object list
        jj = this%flowbudptr%budterm(this%idxbudgwf)%id2(i) !< position of gwe cell to which uze feature is connected
        nglo = moffset + this%dis%nodes + this%ioffset + n !< uze feature position
        jglo = moffset + jj !< gwe cell position
        call sparse%addconnection(nglo, jglo, 1)
        call sparse%addconnection(jglo, nglo, 1)
      end do
      !
      ! -- For uze, add feature-to-feature connections (i.e., 
      !    vertically stacked UZ objects) to row corresponding 
      !    to the host cell. Terms added to the row associated with host 
      !    cell are added to columns associated with other uze features. 
      !    This approach deviates from the approach taken in apt_ac.
      if (this%idxbudfjf /= 0) then
        do i = 1, this%flowbudptr%budterm(this%idxbudfjf)%maxlist
          n = this%flowbudptr%budterm(this%idxbudfjf)%id1(i) !< position of currently considered uze feature
          jj = this%flowbudptr%budterm(this%idxbudfjf)%id2(i) !< position of connected uze feature
          nglo = moffset + this%dis%nodes + this%ioffset + n !< global position of currently considered uze feature
          jglo = moffset + this%dis%nodes + this%ioffset + jj !< global position of connected uze feature
          ! -- if connected uze feature is upstream, find cell that hosts currently 
          !    considered uze feature and add connection to that cell's row
!!          if (jj < n) then ! presumes that deeper uze object has id with larger integer
            do ii = 1, this%flowbudptr%budterm(this%idxbudgwf)%nlist !< uze object id among uze objects
              idxn = this%flowbudptr%budterm(this%idxbudgwf)%id1(ii) !< uze object position within uze object list
              idxjj = this%flowbudptr%budterm(this%idxbudgwf)%id2(ii) !< position of gwe cell to which uze feature is connected
              idxnglo = moffset + this%dis%nodes + this%ioffset + idxn !< uze feature global position
              idxjglo = moffset + idxjj !< gwe cell global position
              if (nglo == idxnglo) exit
            end do
            call sparse%addconnection(idxjglo, jglo, 1)
!!          end if
        end do
      end if
    end if
    !
    ! -- return
    return
  end subroutine uze_ac
  
  subroutine uze_mc(this, moffset, matrix_sln)
! ******************************************************************************
! uze_mc -- map package connection to matrix
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use SparseModule, only: sparsematrix
    ! -- dummy
    class(GweUzeType), intent(inout) :: this
    integer(I4B), intent(in) :: moffset
    class(MatrixBaseType), pointer :: matrix_sln
    ! -- local
    integer(I4B) :: n, j, iglo, jglo
    integer(I4B) :: idxn, idxj, idxiglo, idxjglo
    integer(I4B) :: ipos, idxpos
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
      !    apt rows
      !
      ! -- feature diagonal in global matrix
      do n = 1, this%ncv
!!        this%idxlocnode(n) = this%dis%nodes + this%ioffset + n
        iglo = moffset + this%dis%nodes + this%ioffset + n
        this%idxpakdiag(n) = matrix_sln%get_position_diag(iglo)
      end do
      !
      ! -- cell to feature connection in global matrix
      do ipos = 1, this%flowbudptr%budterm(this%idxbudgwf)%nlist
        n = this%flowbudptr%budterm(this%idxbudgwf)%id1(ipos) !< feature number
        j = this%flowbudptr%budterm(this%idxbudgwf)%id2(ipos) !< cell number
        iglo = moffset + this%dis%nodes + this%ioffset + n    !< feature row index
        jglo = j + moffset                                    !< cell row index
        ! -- Note that this is where idxlocnode is set for uze; it is set
!!        !    to the host cell global row rather than the feature global row
!!        this%idxlocnode(n) = jglo
        !    to the host cell local row index rather than the feature local
        !    row index
        this%idxlocnode(n) = j        ! kluge note: do we want to introduce a new array instead of co-opting idxlocnode???
        ! -- for connection ipos in list of feature-cell connections,
        !    global positions of feature-row diagonal and off-diagonal
        !    corresponding to the cell
        this%idxdglo(ipos) = matrix_sln%get_position_diag(iglo)
        this%idxoffdglo(ipos) = matrix_sln%get_position(iglo, jglo)
      end do
      !
      ! -- feature to cell connection in global matrix
      do ipos = 1, this%flowbudptr%budterm(this%idxbudgwf)%nlist
        n = this%flowbudptr%budterm(this%idxbudgwf)%id1(ipos) !< feature number
        j = this%flowbudptr%budterm(this%idxbudgwf)%id2(ipos) !< cell number
        iglo = j + moffset                                    !< cell row index
        jglo = moffset + this%dis%nodes + this%ioffset + n    !< feature row index
        ! -- for connection ipos in list of feature-cell connections,
        !    global positions of cell-row diagonal and off-diagonal
        !    corresponding to the feature
        this%idxsymdglo(ipos) = matrix_sln%get_position_diag(iglo)
        this%idxsymoffdglo(ipos) = matrix_sln%get_position(iglo, jglo)
      end do
      !
      ! -- feature to feature connection in global matrix
      if (this%idxbudfjf /= 0) then
        do ipos = 1, this%flowbudptr%budterm(this%idxbudfjf)%nlist
          n = this%flowbudptr%budterm(this%idxbudfjf)%id1(ipos) !< number of currently considered uze feature
          j = this%flowbudptr%budterm(this%idxbudfjf)%id2(ipos) !< number of connected uze feature
          iglo = moffset + this%dis%nodes + this%ioffset + n !< global position of currently considered uze feature
          jglo = moffset + this%dis%nodes + this%ioffset + j !< global position of connected uze feature
          ! -- if connected uze feature is upstream, find cell that hosts currently 
          !    considered uze feature and map connection to that cell's row
!!          if (j < n) then           ! jiffylube: determine ordering of features; is id1 always upstream of id2?
            do idxpos = 1, this%flowbudptr%budterm(this%idxbudgwf)%nlist
              idxn = this%flowbudptr%budterm(this%idxbudgwf)%id1(idxpos) !< feature number
              idxj = this%flowbudptr%budterm(this%idxbudgwf)%id2(idxpos) !< cell number
              ! jiffylube: should be able to base search simply on (idxn == n)
              idxjglo = moffset + this%dis%nodes + this%ioffset + idxn   !< feature row index
              idxiglo = moffset + idxj                                   !< cell row index
              if (idxjglo == iglo) exit
            end do
            ! -- for connection ipos in list of feature-feature connections,
            !    global positions of host-cell-row entries corresponding to
            !    (in the same columns as) the feature-id1-row diagonal and the
            !    feature-id1-row off-diagonal corresponding to feature id2
            this%idxfjfdglo(ipos) = matrix_sln%get_position_diag(idxiglo)
            this%idxfjfoffdglo(ipos) = matrix_sln%get_position(idxiglo, jglo)
!!          end if  
        end do
      end if
    end if
    !
    ! -- return
    return
  end subroutine uze_mc

  subroutine uze_fc_expanded(this, rhs, ia, idxglo, matrix_sln)
! ******************************************************************************
! uze_fc_expanded -- this will be called from TspAptType%apt_fc_expanded()
!   in order to add matrix terms specifically for this package
! ****************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use TdisModule, only: kper, kstp
    ! -- dummy
    class(GweUzeType) :: this
    real(DP), dimension(:), intent(inout) :: rhs
    integer(I4B), dimension(:), intent(in) :: ia
    integer(I4B), dimension(:), intent(in) :: idxglo
    class(MatrixBaseType), pointer :: matrix_sln
    ! -- local
    integer(I4B) :: j, n, n1, n2
    integer(I4B) :: iloc, ihostcell
    integer(I4B) :: iposd, iposoffd
    integer(I4B) :: ipossymd, ipossymoffd
    real(DP) :: cold
    real(DP) :: qbnd
    real(DP) :: omega
    real(DP) :: rrate
    real(DP) :: rhsval
    real(DP) :: hcofval
    real(DP) :: dummy
! ------------------------------------------------------------------------------
    !
    ! -- add infiltration contribution
    !    uze does not put feature balance coefficients in the row 
    !    associated with the feature.  Instead, these coefficients are
    !    moved into the row associated with cell hosting the uze feature
    if (this%idxbudinfl /= 0) then
      do j = 1, this%flowbudptr%budterm(this%idxbudinfl)%nlist
        call this%uze_infl_term(j, n1, n2, rrate, rhsval, hcofval)
        iloc = this%idxlocnode(n1)  ! for uze idxlocnode stores the host cell local row index
!!        iposd = this%idxpakdiag(n1)
!!        call matrix_sln%add_value_pos(iposd, hcofval)
        ipossymoffd = this%idxsymoffdglo(j)
        call matrix_sln%add_value_pos(ipossymoffd, hcofval)
        rhs(iloc) = rhs(iloc) + rhsval
      end do
    end if
    !
    ! -- add rejected infiltration contribution
    if (this%idxbudrinf /= 0) then
      do j = 1, this%flowbudptr%budterm(this%idxbudrinf)%nlist
        call this%uze_rinf_term(j, n1, n2, rrate, rhsval, hcofval)
        iloc = this%idxlocnode(n1)  ! for uze idxlocnode stores the host cell local row index
!!        iposd = this%idxpakdiag(n1)
!!        call matrix_sln%add_value_pos(iposd, hcofval)
        ipossymoffd = this%idxsymoffdglo(j)
        call matrix_sln%add_value_pos(ipossymoffd, hcofval)
        rhs(iloc) = rhs(iloc) + rhsval
      end do
    end if
    !
    ! -- add unsaturated et contribution
    if (this%idxbuduzet /= 0) then
      do j = 1, this%flowbudptr%budterm(this%idxbuduzet)%nlist
        call this%uze_uzet_term(j, n1, n2, rrate, rhsval, hcofval)
        iloc = this%idxlocnode(n1)  ! for uze idxlocnode stores the host cell local row index
!!        iposd = this%idxpakdiag(n1)
!!        call matrix_sln%add_value_pos(iposd, hcofval)
        ipossymoffd = this%idxsymoffdglo(j)
        call matrix_sln%add_value_pos(ipossymoffd, hcofval)
        rhs(iloc) = rhs(iloc) + rhsval
      end do
    end if
    !
    ! -- add rejected infiltration to mover contribution
    if (this%idxbudritm /= 0) then
      do j = 1, this%flowbudptr%budterm(this%idxbudritm)%nlist
        call this%uze_ritm_term(j, n1, n2, rrate, rhsval, hcofval)
        iloc = this%idxlocnode(n1)  ! for uze idxlocnode stores the host cell local row index
!!        iposd = this%idxpakdiag(n1)
!!        call matrix_sln%add_value_pos(iposd, hcofval)
        ipossymoffd = this%idxsymoffdglo(j)
        call matrix_sln%add_value_pos(ipossymoffd, hcofval)
        rhs(iloc) = rhs(iloc) + rhsval
      end do
    end if
    !
    ! -- For UZE, content of apt_fc_expanded placed here as the approach is to 
    !    completely override apt_fc_expanded() with what follows
    !
    ! -- mass (or energy) storage in features
    do n = 1, this%ncv
      cold = this%xoldpak(n)
!!      iloc = this%idxlocnode(n)
!!      iposd = this%idxpakdiag(n)
!!      call this%apt_stor_term(n, n1, n2, rrate, rhsval, hcofval)
!!      call matrix_sln%add_value_pos(iposd, hcofval)
      iloc = this%idxlocnode(n)  ! for uze idxlocnode stores the host cell local row index
      ipossymoffd = this%idxsymoffdglo(n)  ! TO DO: convince ourselves that "n" is ok here, since it's not aloop over "j"
      call this%apt_stor_term(n, n1, n2, rrate, rhsval, hcofval)
      call matrix_sln%add_value_pos(ipossymoffd, hcofval)
      rhs(iloc) = rhs(iloc) + rhsval
    end do
    !
    ! -- add to mover contribution
    if (this%idxbudtmvr /= 0) then
      do j = 1, this%flowbudptr%budterm(this%idxbudtmvr)%nlist
        call this%apt_tmvr_term(j, n1, n2, rrate, rhsval, hcofval)
!!        iloc = this%idxlocnode(n1)
!!        iposd = this%idxpakdiag(n1)
!!
!!      NOTE: originally was iposd, but changed to idxsymdglo on the first 
!!            modification.  It was later realized we needed idxsymoffdglo.
!!           (If this works, consider changing 'ipossymd' to 'ipossymoffd'
!! 
!!        call matrix_sln%add_value_pos(iposd, hcofval)
        iloc = this%idxlocnode(n1)  ! for uze idxlocnode stores the host cell local row index
!!        iposd = this%idxpakdiag(n1)
        ipossymoffd = this%idxsymoffdglo(j)  !< TODO: Need
        call matrix_sln%add_value_pos(ipossymoffd, hcofval)
        rhs(iloc) = rhs(iloc) + rhsval
      end do
    end if
    !
    ! -- add from mover contribution
    if (this%idxbudfmvr /= 0) then
      do n = 1, this%ncv
        rhsval = this%qmfrommvr(n)
        iloc = this%idxlocnode(n)  ! for uze idxlocnode stores the host cell local row index
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
!!        ! -- set acoef and rhs to negative so they are relative to apt and not gwt
!!        qbnd = this%flowbudptr%budterm(this%idxbudgwf)%flow(j)   ! jiffylube: shouldn't need these 3 lines
!!        omega = DZERO
!!        if (qbnd < DZERO) omega = DONE
!!        !
        ! -- this code altered from its counterpart appearing in apt; this equates
        !    uze temperature to cell temperature using the feature's row
        iposd = this%idxdglo(j)
        iposoffd = this%idxoffdglo(j)
        call matrix_sln%add_value_pos(iposd, DONE)
        call matrix_sln%add_value_pos(iposoffd, -DONE)
        !
        !! -- add to gwf row for apt connection (recharge)
        !!ipossymd = this%idxsymdglo(j)
        !!ipossymoffd = this%idxsymoffdglo(j)
        !!call matrix_sln%add_value_pos(ipossymd, -(DONE - omega) * qbnd)
        !!call matrix_sln%add_value_pos(ipossymoffd, -omega * qbnd)
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
        iposd = this%idxfjfdglo(j)        !< position of feature-id1 column in feature id1's host-cell row
        iposoffd = this%idxfjfoffdglo(j)  !< position of feature-id2 column in feature id1's host-cell row
        call matrix_sln%add_value_pos(iposd, omega * qbnd)
        call matrix_sln%add_value_pos(iposoffd, (DONE - omega) * qbnd)
      end do
    end if
    !
    ! -- Return
    return
  end subroutine uze_fc_expanded

  subroutine uze_cfupdate(this)
! ******************************************************************************
! uze_cfupdate -- calculate package hcof and rhs so gwt budget is calculated
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(GweUzeType) :: this
    ! -- local
    integer(I4B) :: j, n
    real(DP) :: qbnd
    real(DP) :: omega
! ------------------------------------------------------------------------------
    !
    ! -- Calculate hcof and rhs terms so GWF exchanges are calculated correctly
    ! -- go through each apt-gwf connection and calculate
    !    rhs and hcof terms for gwt/gwe matrix rows
    call this%TspAptType%apt_cfupdate()
    !
    ! -- Apply scaling to units of energy per time
    do j = 1, this%flowbudptr%budterm(this%idxbudgwf)%nlist
      this%hcof(j) = this%hcof(j) * this%eqnsclfac
      this%rhs(j) = this%rhs(j) * this%eqnsclfac
    end do
    !
    ! -- Return
    return
  end subroutine uze_cfupdate

  subroutine uze_solve(this)   ! kluge note: no explicit solve for uze
! ******************************************************************************
! uze_solve -- add terms specific to the unsaturated zone to the explicit
!              unsaturated-zone solve
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(GweUzeType) :: this
    ! -- local
    integer(I4B) :: j
    integer(I4B) :: n1, n2
    real(DP) :: rrate
! ------------------------------------------------------------------------------
    !
    ! -- add infiltration contribution
    if (this%idxbudinfl /= 0) then
      do j = 1, this%flowbudptr%budterm(this%idxbudinfl)%nlist
        call this%uze_infl_term(j, n1, n2, rrate)
        this%dbuff(n1) = this%dbuff(n1) + rrate
      end do
    end if
    !
    ! -- add rejected infiltration contribution
    if (this%idxbudrinf /= 0) then
      do j = 1, this%flowbudptr%budterm(this%idxbudrinf)%nlist
        call this%uze_rinf_term(j, n1, n2, rrate)
        this%dbuff(n1) = this%dbuff(n1) + rrate
      end do
    end if
    !
    ! -- add unsaturated et contribution
    if (this%idxbuduzet /= 0) then
      do j = 1, this%flowbudptr%budterm(this%idxbuduzet)%nlist
        call this%uze_uzet_term(j, n1, n2, rrate)
        this%dbuff(n1) = this%dbuff(n1) + rrate
      end do
    end if
    !
    ! -- add rejected infiltration to mover contribution
    if (this%idxbudritm /= 0) then
      do j = 1, this%flowbudptr%budterm(this%idxbudritm)%nlist
        call this%uze_ritm_term(j, n1, n2, rrate)
        this%dbuff(n1) = this%dbuff(n1) + rrate
      end do
    end if
    !
    ! -- Return
    return
  end subroutine uze_solve

  function uze_get_nbudterms(this) result(nbudterms)
! ******************************************************************************
! uze_get_nbudterms -- function to return the number of budget terms just for
!   this package.  This overrides function in parent.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(GweUzeType) :: this
    ! -- return
    integer(I4B) :: nbudterms
    ! -- local
! ------------------------------------------------------------------------------
    !
    ! -- Number of budget terms is 5
    nbudterms = 0
    if (this%idxbudinfl /= 0) nbudterms = nbudterms + 1
    if (this%idxbudrinf /= 0) nbudterms = nbudterms + 1
    if (this%idxbuduzet /= 0) nbudterms = nbudterms + 1
    if (this%idxbudritm /= 0) nbudterms = nbudterms + 1
    if (this%idxbudtheq /= 0) nbudterms = nbudterms + 1
    !
    ! -- Return
    return
  end function uze_get_nbudterms

  subroutine uze_setup_budobj(this, idx)
! ******************************************************************************
! uze_setup_budobj -- Set up the budget object that stores all the unsaturated-
!                     zone flows
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use ConstantsModule, only: LENBUDTXT
    ! -- dummy
    class(GweUzeType) :: this
    integer(I4B), intent(inout) :: idx
    ! -- local
    integer(I4B) :: maxlist, naux, n, n1, n2
    character(len=LENBUDTXT) :: text
    real(DP) :: q
! ------------------------------------------------------------------------------
    !
    ! --
    text = '    INFILTRATION'
    idx = idx + 1
    maxlist = this%flowbudptr%budterm(this%idxbudinfl)%maxlist
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
    if (this%idxbudrinf /= 0) then
      text = '         REJ-INF'
      idx = idx + 1
      maxlist = this%flowbudptr%budterm(this%idxbudrinf)%maxlist
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
    if (this%idxbuduzet /= 0) then
      text = '            UZET'
      idx = idx + 1
      maxlist = this%flowbudptr%budterm(this%idxbuduzet)%maxlist
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
    if (this%idxbudritm /= 0) then
      text = '  INF-REJ-TO-MVR'
      idx = idx + 1
      maxlist = this%flowbudptr%budterm(this%idxbudritm)%maxlist
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
    text = '   THERMAL-EQUIL'
    idx = idx + 1
    ! -- use dimension of GWF term
    maxlist = this%flowbudptr%budterm(this%idxbudgwf)%maxlist
    naux = 0
    call this%budobj%budterm(idx)%initialize(text, &
                                             this%name_model, &
                                             this%packName, &
                                             this%name_model, &
                                             this%packName, &
                                             maxlist, .false., .false., &
                                             naux)

    !
    ! -- return
    return
  end subroutine uze_setup_budobj

  subroutine uze_fill_budobj(this, idx, x, flowja, ccratin, ccratout)
! ******************************************************************************
! uze_fill_budobj -- copy flow terms into this%budobj
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(GweUzeType) :: this
    integer(I4B), intent(inout) :: idx
    real(DP), dimension(:), intent(in) :: x
    real(DP), dimension(:), contiguous, intent(inout) :: flowja
    real(DP), intent(inout) :: ccratin
    real(DP), intent(inout) :: ccratout
    ! -- local
    integer(I4B) :: j, n1, n2, i
    integer(I4B) :: nlist, nbudterm
    integer(I4B) :: igwfnode
    integer(I4B) :: idiag
    real(DP) :: q
    ! -- formats
! -----------------------------------------------------------------------------

    ! -- INFILTRATION
    idx = idx + 1
    nlist = this%flowbudptr%budterm(this%idxbudinfl)%nlist
    call this%budobj%budterm(idx)%reset(nlist)
    do j = 1, nlist
      call this%uze_infl_term(j, n1, n2, q)
      call this%budobj%budterm(idx)%update_term(n1, n2, q)
      call this%apt_accumulate_ccterm(n1, q, ccratin, ccratout)
    end do

    ! -- REJ-INF
    if (this%idxbudrinf /= 0) then
      idx = idx + 1
      nlist = this%flowbudptr%budterm(this%idxbudrinf)%nlist
      call this%budobj%budterm(idx)%reset(nlist)
      do j = 1, nlist
        call this%uze_rinf_term(j, n1, n2, q)
        call this%budobj%budterm(idx)%update_term(n1, n2, q)
        call this%apt_accumulate_ccterm(n1, q, ccratin, ccratout)
      end do
    end if

    ! -- UZET
    if (this%idxbuduzet /= 0) then
      idx = idx + 1
      nlist = this%flowbudptr%budterm(this%idxbuduzet)%nlist
      call this%budobj%budterm(idx)%reset(nlist)
      do j = 1, nlist
        call this%uze_uzet_term(j, n1, n2, q)
        call this%budobj%budterm(idx)%update_term(n1, n2, q)
        call this%apt_accumulate_ccterm(n1, q, ccratin, ccratout)
      end do
    end if

    ! -- REJ-INF-TO-MVR
    if (this%idxbudritm /= 0) then
      idx = idx + 1
      nlist = this%flowbudptr%budterm(this%idxbudritm)%nlist
      call this%budobj%budterm(idx)%reset(nlist)
      do j = 1, nlist
        call this%uze_ritm_term(j, n1, n2, q)
        call this%budobj%budterm(idx)%update_term(n1, n2, q)
        call this%apt_accumulate_ccterm(n1, q, ccratin, ccratout)
      end do
    end if

    ! -- THERMAL-EQUIL
    ! -- processed last because it is calculated from the residual
    idx = idx + 1
    nlist = this%flowbudptr%budterm(this%idxbudgwf)%nlist
    call this%budobj%budterm(idx)%reset(nlist)
    do j = 1, nlist
      call this%uze_theq_term(j, n1, igwfnode, q)
      call this%budobj%budterm(idx)%update_term(n1, igwfnode, q)
      call this%apt_accumulate_ccterm(n1, q, ccratin, ccratout)
      if (this%iboundpak(n1) /= 0) then 
        ! -- contribution to gwe cell budget
        this%simvals(n1) = this%simvals(n1) - q
        idiag = this%dis%con%ia(igwfnode)
        flowja(idiag) = flowja(idiag) - q
      end if
    end do

    !
    ! -- return
    return
  end subroutine uze_fill_budobj

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
    class(GweUzeType) :: this
    ! -- local
! ------------------------------------------------------------------------------
    !
    ! -- allocate scalars in TspAptType
    call this%TspAptType%allocate_scalars()
    !
    ! -- Allocate
    call mem_allocate(this%idxbudinfl, 'IDXBUDINFL', this%memoryPath)
    call mem_allocate(this%idxbudrinf, 'IDXBUDRINF', this%memoryPath)
    call mem_allocate(this%idxbuduzet, 'IDXBUDUZET', this%memoryPath)
    call mem_allocate(this%idxbudritm, 'IDXBUDRITM', this%memoryPath)
    call mem_allocate(this%idxbudtheq, 'IDXBUDTHEQ', this%memoryPath)
    !
    ! -- Initialize
    this%idxbudinfl = 0
    this%idxbudrinf = 0
    this%idxbuduzet = 0
    this%idxbudritm = 0
    this%idxbudtheq = 0
    !
    ! -- Return
    return
  end subroutine allocate_scalars

  subroutine uze_allocate_arrays(this)
! ******************************************************************************
! uze_allocate_arrays
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryManagerModule, only: mem_allocate
    ! -- dummy
    class(GweUzeType), intent(inout) :: this
    ! -- local
    integer(I4B) :: n
! ------------------------------------------------------------------------------
    !
    ! -- time series
    call mem_allocate(this%tempinfl, this%ncv, 'TEMPINFL', this%memoryPath)
    call mem_allocate(this%tempuzet, this%ncv, 'TEMPUZET', this%memoryPath)
    !
    ! -- call standard TspAptType allocate arrays
    call this%TspAptType%apt_allocate_arrays()
    !
    ! -- Initialize
    do n = 1, this%ncv
      this%tempinfl(n) = DZERO
      this%tempuzet(n) = DZERO
    end do
    !
    !
    ! -- Return
    return
  end subroutine uze_allocate_arrays

  subroutine uze_da(this)
! ******************************************************************************
! uze_da
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryManagerModule, only: mem_deallocate
    ! -- dummy
    class(GweUzeType) :: this
    ! -- local
! ------------------------------------------------------------------------------
    !
    ! -- deallocate scalars
    call mem_deallocate(this%idxbudinfl)
    call mem_deallocate(this%idxbudrinf)
    call mem_deallocate(this%idxbuduzet)
    call mem_deallocate(this%idxbudritm)
    call mem_deallocate(this%idxbudtheq)
    !
    ! -- deallocate time series
    call mem_deallocate(this%tempinfl)
    call mem_deallocate(this%tempuzet)
    !
    ! -- deallocate scalars in TspAptType
    call this%TspAptType%bnd_da()
    !
    ! -- Return
    return
  end subroutine uze_da

  subroutine uze_infl_term(this, ientry, n1, n2, rrate, &
                           rhsval, hcofval)
! ******************************************************************************
! uze_infl_term
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(GweUzeType) :: this
    integer(I4B), intent(in) :: ientry
    integer(I4B), intent(inout) :: n1
    integer(I4B), intent(inout) :: n2
    real(DP), intent(inout), optional :: rrate
    real(DP), intent(inout), optional :: rhsval
    real(DP), intent(inout), optional :: hcofval
    ! -- local
    real(DP) :: qbnd
    real(DP) :: ctmp
    real(DP) :: h, r
! ------------------------------------------------------------------------------
    ! 
    n1 = this%flowbudptr%budterm(this%idxbudinfl)%id1(ientry)
    n2 = this%flowbudptr%budterm(this%idxbudinfl)%id2(ientry)
    ! -- note that qbnd is negative for negative infiltration
    qbnd = this%flowbudptr%budterm(this%idxbudinfl)%flow(ientry)
    if (qbnd < DZERO) then
      ctmp = this%xnewpak(n1)
      h = qbnd
      r = DZERO
    else
      ctmp = this%tempinfl(n1)
      h = DZERO
      r = -qbnd * ctmp
    end if
    if (present(rrate)) rrate = qbnd * ctmp * this%eqnsclfac
    if (present(rhsval)) rhsval = r
    if (present(hcofval)) hcofval = h
    !
    ! -- return
    return
  end subroutine uze_infl_term

  subroutine uze_rinf_term(this, ientry, n1, n2, rrate, &
                           rhsval, hcofval)
! ******************************************************************************
! uze_rinf_term
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(GweUzeType) :: this
    integer(I4B), intent(in) :: ientry
    integer(I4B), intent(inout) :: n1
    integer(I4B), intent(inout) :: n2
    real(DP), intent(inout), optional :: rrate
    real(DP), intent(inout), optional :: rhsval
    real(DP), intent(inout), optional :: hcofval
    ! -- local
    real(DP) :: qbnd
    real(DP) :: ctmp
! ------------------------------------------------------------------------------
    ! 
    n1 = this%flowbudptr%budterm(this%idxbudrinf)%id1(ientry)
    n2 = this%flowbudptr%budterm(this%idxbudrinf)%id2(ientry)
    qbnd = this%flowbudptr%budterm(this%idxbudrinf)%flow(ientry)
    ctmp = this%tempinfl(n1)
    if (present(rrate)) rrate = ctmp * qbnd * this%eqnsclfac
    if (present(rhsval)) rhsval = DZERO
    if (present(hcofval)) hcofval = qbnd
    !
    ! -- return
    return
  end subroutine uze_rinf_term

  subroutine uze_uzet_term(this, ientry, n1, n2, rrate, &
                           rhsval, hcofval)
! ******************************************************************************
! uze_uzet_term
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(GweUzeType) :: this
    integer(I4B), intent(in) :: ientry
    integer(I4B), intent(inout) :: n1
    integer(I4B), intent(inout) :: n2
    real(DP), intent(inout), optional :: rrate
    real(DP), intent(inout), optional :: rhsval
    real(DP), intent(inout), optional :: hcofval
    ! -- local
    real(DP) :: qbnd
    real(DP) :: ctmp
    real(DP) :: omega    
! ------------------------------------------------------------------------------
    ! 
    n1 = this%flowbudptr%budterm(this%idxbuduzet)%id1(ientry)
    n2 = this%flowbudptr%budterm(this%idxbuduzet)%id2(ientry)
    ! -- note that qbnd is negative for uzet
    qbnd = this%flowbudptr%budterm(this%idxbuduzet)%flow(ientry)
    ctmp = this%tempuzet(n1)
    if (this%xnewpak(n1) < ctmp) then
      omega = DONE
    else
      omega = DZERO
    end if
    if (present(rrate)) &
      rrate = (omega * qbnd * this%xnewpak(n1) + &
              (DONE - omega) * qbnd * ctmp) * this%eqnsclfac
    if (present(rhsval)) rhsval = -(DONE - omega) * qbnd * ctmp
    if (present(hcofval)) hcofval = omega * qbnd
    !
    ! -- return
    return
  end subroutine uze_uzet_term

  subroutine uze_ritm_term(this, ientry, n1, n2, rrate, &
                           rhsval, hcofval)
! ******************************************************************************
! uze_ritm_term
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(GweUzeType) :: this
    integer(I4B), intent(in) :: ientry
    integer(I4B), intent(inout) :: n1
    integer(I4B), intent(inout) :: n2
    real(DP), intent(inout), optional :: rrate
    real(DP), intent(inout), optional :: rhsval
    real(DP), intent(inout), optional :: hcofval
    ! -- local
    real(DP) :: qbnd
    real(DP) :: ctmp
! ------------------------------------------------------------------------------
    ! 
    n1 = this%flowbudptr%budterm(this%idxbudritm)%id1(ientry)
    n2 = this%flowbudptr%budterm(this%idxbudritm)%id2(ientry)
    qbnd = this%flowbudptr%budterm(this%idxbudritm)%flow(ientry)
    ctmp = this%tempinfl(n1)
    if (present(rrate)) rrate = ctmp * qbnd * this%eqnsclfac
    if (present(rhsval)) rhsval = DZERO
    if (present(hcofval)) hcofval = qbnd
    !
    ! -- return
    return
  end subroutine uze_ritm_term

  subroutine uze_theq_term(this, ientry, n1, n2, rrate)
! ******************************************************************************
! uze_theq_term
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use ConstantsModule, only: LENBUDTXT
    ! -- dummy
    class(GweUzeType) :: this
    integer(I4B), intent(in) :: ientry
    integer(I4B), intent(inout) :: n1
    integer(I4B), intent(inout) :: n2
    real(DP), intent(inout) :: rrate
    ! -- local
    real(DP) :: qbnd
    real(DP) :: ctmp
    real(DP) :: r
    integer(I4B) :: i
    character(len=LENBUDTXT) :: flowtype
! ------------------------------------------------------------------------------
    !
    r = DZERO
    n1 = this%flowbudptr%budterm(this%idxbudgwf)%id1(ientry)
    n2 = this%flowbudptr%budterm(this%idxbudgwf)%id2(ientry)
    if (this%iboundpak(n1) /= 0) then
      do i = 1, this%budobj%nbudterm
        flowtype = this%budobj%budterm(i)%flowtype
        select case (trim(adjustl(flowtype)))
        case ('THERMAL-EQUIL')
          ! skip
          continue
        case ('FLOW-JA-FACE')
          ! skip
          continue
        case default
          r = r - this%budobj%budterm(i)%flow(ientry)
        end select
      end do
    end if
    rrate = r
    !
    ! -- return
    return
  end subroutine uze_theq_term

  subroutine uze_stor_term(this, ientry, n1, n2, rrate, &
                           rhsval, hcofval)
    ! -- modules
    ! -- dummy
    class(GweUzeType) :: this
    integer(I4B), intent(in) :: ientry
    integer(I4B), intent(inout) :: n1
    integer(I4B), intent(inout) :: n2
    real(DP), intent(inout), optional :: rrate
    real(DP), intent(inout), optional :: rhsval
    real(DP), intent(inout), optional :: hcofval
    ! -- local
! -----------------------------------------------------------------
    !
    call this%TspAptType%apt_stor_term(ientry, n1, n2, rrate, &
                                       rhsval, hcofval)
    !
    ! -- Apply scaling to units of energy per time
    if (present(rrate)) rrate = rrate * this%eqnsclfac
    !
    ! -- return
    return
  end subroutine uze_stor_term

  subroutine uze_tmvr_term(this, ientry, n1, n2, rrate, &
                           rhsval, hcofval)
    ! -- modules
    ! -- dummy
    class(GweUzeType) :: this
    integer(I4B), intent(in) :: ientry
    integer(I4B), intent(inout) :: n1
    integer(I4B), intent(inout) :: n2
    real(DP), intent(inout), optional :: rrate
    real(DP), intent(inout), optional :: rhsval
    real(DP), intent(inout), optional :: hcofval
    ! -- local
! ------------------------------------------------------------------------------
    !
    call this%TspAptType%apt_tmvr_term(ientry, n1, n2, rrate, &
                                       rhsval, hcofval)
    !
    ! -- Apply scaling to units of energy per time
    if (present(rrate)) rrate = rrate * this%eqnsclfac
    !
    ! -- return
    return
  end subroutine uze_tmvr_term

  subroutine uze_fmvr_term(this, ientry, n1, n2, rrate, &
                           rhsval, hcofval)
    ! -- modules
    ! -- dummy
    class(GweUzeType) :: this
    integer(I4B), intent(in) :: ientry
    integer(I4B), intent(inout) :: n1
    integer(I4B), intent(inout) :: n2
    real(DP), intent(inout), optional :: rrate
    real(DP), intent(inout), optional :: rhsval
    real(DP), intent(inout), optional :: hcofval
    ! -- local
! ------------------------------------------------------------------------------
    !
    call this%TspAptType%apt_fmvr_term(ientry, n1, n2, rrate, &
                                       rhsval, hcofval)
    !
    ! -- Apply scaling to units of energy per time
    if (present(rrate)) rrate = rrate * this%eqnsclfac
    !
    ! -- return
    return
  end subroutine uze_fmvr_term

  subroutine uze_fjf_term(this, ientry, n1, n2, rrate, &
                          rhsval, hcofval)
    ! -- modules
    ! -- dummy
    class(GweUzeType) :: this
    integer(I4B), intent(in) :: ientry
    integer(I4B), intent(inout) :: n1
    integer(I4B), intent(inout) :: n2
    real(DP), intent(inout), optional :: rrate
    real(DP), intent(inout), optional :: rhsval
    real(DP), intent(inout), optional :: hcofval
    ! -- local
! ------------------------------------------------------------------------------
    !
    call this%TspAptType%apt_fjf_term(ientry, n1, n2, rrate, &
                                      rhsval, hcofval)
    !
    ! -- Apply scaling to units of energy per time
    if (present(rrate)) rrate = rrate * this%eqnsclfac
    !
    ! -- return
    return
  end subroutine uze_fjf_term

  subroutine uze_df_obs(this)
! ******************************************************************************
! uze_df_obs -- obs are supported?
!   -- Store observation type supported by APT package.
!   -- Overrides BndType%bnd_df_obs
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(GweUzeType) :: this
    ! -- local
    integer(I4B) :: indx
! ------------------------------------------------------------------------------
    !
    ! -- Store obs type and assign procedure pointer
    !    for temperature observation type.
    call this%obs%StoreObsType('temperature', .false., indx)
    this%obs%obsData(indx)%ProcessIdPtr => apt_process_obsID
    !
    ! -- Store obs type and assign procedure pointer
    !    for flow between uze cells.
    call this%obs%StoreObsType('flow-ja-face', .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => apt_process_obsID12
    !
    ! -- Store obs type and assign procedure pointer
    !    for from-mvr observation type.
    call this%obs%StoreObsType('from-mvr', .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => apt_process_obsID
    !
    ! -- to-mvr not supported for uze
    !call this%obs%StoreObsType('to-mvr', .true., indx)
    !this%obs%obsData(indx)%ProcessIdPtr => apt_process_obsID
    !
    ! -- Store obs type and assign procedure pointer
    !    for storage observation type.
    call this%obs%StoreObsType('storage', .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => apt_process_obsID
    !
    ! -- Store obs type and assign procedure pointer
    !    for constant observation type.
    call this%obs%StoreObsType('constant', .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => apt_process_obsID
    !
    ! -- Store obs type and assign procedure pointer
    !    for observation type: uze
    call this%obs%StoreObsType('uze', .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => apt_process_obsID
    !
    ! -- Store obs type and assign procedure pointer
    !    for observation type.
    call this%obs%StoreObsType('infiltration', .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => apt_process_obsID
    !
    ! -- Store obs type and assign procedure pointer
    !    for observation type.
    call this%obs%StoreObsType('rej-inf', .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => apt_process_obsID
    !
    ! -- Store obs type and assign procedure pointer
    !    for observation type.
    call this%obs%StoreObsType('uzet', .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => apt_process_obsID
    !
    ! -- Store obs type and assign procedure pointer
    !    for observation type.
    call this%obs%StoreObsType('rej-inf-to-mvr', .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => apt_process_obsID
    !
    ! -- Store obs type and assign procedure pointer
    !    for observation type.
    call this%obs%StoreObsType('thermal-equil', .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => apt_process_obsID
    !
    return
  end subroutine uze_df_obs

  !> @brief Process package specific obs
    !!
    !! Method to process specific observations for this package.
    !!
  !<
  subroutine uze_rp_obs(this, obsrv, found)
    ! -- dummy
    class(GweUzeType), intent(inout) :: this !< package class
    type(ObserveType), intent(inout) :: obsrv !< observation object
    logical, intent(inout) :: found !< indicate whether observation was found
    ! -- local
    !
    found = .true.
    select case (obsrv%ObsTypeId)
    case ('INFILTRATION')
      call this%rp_obs_byfeature(obsrv)
    case ('REJ-INF')
      call this%rp_obs_byfeature(obsrv)
    case ('UZET')
      call this%rp_obs_byfeature(obsrv)
    case ('REJ-INF-TO-MVR')
      call this%rp_obs_byfeature(obsrv)
    case ('THERMAL-EQUIL')
      call this%rp_obs_byfeature(obsrv)
    case default
      found = .false.
    end select
    !
    return
  end subroutine uze_rp_obs

  subroutine uze_bd_obs(this, obstypeid, jj, v, found)
! ******************************************************************************
! uze_bd_obs -- calculate observation value and pass it back to APT
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(GweUzeType), intent(inout) :: this
    character(len=*), intent(in) :: obstypeid
    real(DP), intent(inout) :: v
    integer(I4B), intent(in) :: jj
    logical, intent(inout) :: found
    ! -- local
    integer(I4B) :: n1, n2
! ------------------------------------------------------------------------------
    !
    found = .true.
    select case (obstypeid)
    case ('INFILTRATION')
      if (this%iboundpak(jj) /= 0 .and. this%idxbudinfl > 0) then
        call this%uze_infl_term(jj, n1, n2, v)
      end if
    case ('REJ-INF')
      if (this%iboundpak(jj) /= 0 .and. this%idxbudrinf > 0) then
        call this%uze_rinf_term(jj, n1, n2, v)
      end if
    case ('UZET')
      if (this%iboundpak(jj) /= 0 .and. this%idxbuduzet > 0) then
        call this%uze_uzet_term(jj, n1, n2, v)
      end if
    case ('REJ-INF-TO-MVR')
      if (this%iboundpak(jj) /= 0 .and. this%idxbudritm > 0) then
        call this%uze_ritm_term(jj, n1, n2, v)
      end if
    case ('THERMAL-EQUIL')
      if (this%iboundpak(jj) /= 0 .and. this%idxbudtheq > 0) then
        call this%uze_theq_term(jj, n1, n2, v)
      end if
    case default
      found = .false.
    end select
    !
    return
  end subroutine uze_bd_obs

  subroutine uze_set_stressperiod(this, itemno, keyword, found)
! ******************************************************************************
! uze_set_stressperiod -- Set a stress period attribute for using keywords.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use TimeSeriesManagerModule, only: read_value_or_time_series_adv
    ! -- dummy
    class(GweUzeType), intent(inout) :: this
    integer(I4B), intent(in) :: itemno
    character(len=*), intent(in) :: keyword
    logical, intent(inout) :: found
    ! -- local
    character(len=LINELENGTH) :: temp_text
    integer(I4B) :: ierr
    integer(I4B) :: jj
    real(DP), pointer :: bndElem => null()
    ! -- formats
! ------------------------------------------------------------------------------
    !
    ! INFILTRATION <infiltration>
    ! UZET <uzet>
    !
    found = .true.
    select case (keyword)
    case ('INFILTRATION')
      ierr = this%apt_check_valid(itemno)
      if (ierr /= 0) then
        goto 999
      end if
      call this%parser%GetString(temp_text)
      jj = 1
      bndElem => this%tempinfl(itemno)
      call read_value_or_time_series_adv(temp_text, itemno, jj, bndElem, &
                                         this%packName, 'BND', this%tsManager, &
                                         this%iprpak, 'INFILTRATION')
    case ('UZET')
      ierr = this%apt_check_valid(itemno)
      if (ierr /= 0) then
        goto 999
      end if
      call this%parser%GetString(temp_text)
      jj = 1
      bndElem => this%tempuzet(itemno)
      call read_value_or_time_series_adv(temp_text, itemno, jj, bndElem, &
                                         this%packName, 'BND', this%tsManager, &
                                         this%iprpak, 'UZET')
    case default
      !
      ! -- keyword not recognized so return to caller with found = .false.
      found = .false.
    end select
    !
999 continue
    !
    ! -- return
    return
  end subroutine uze_set_stressperiod

end module GweUzeModule
