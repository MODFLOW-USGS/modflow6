! -- Stream Energy Transport Module
! -- todo: Temperature decay?
! -- todo: save the sfe temperature into the sfr aux variable? (perhaps needed for GWT-GWE exchanges)
! -- todo: calculate the sfr VISC aux variable using temperature?
!
! SFR flows (sfrbudptr)     index var     SFE term              Transport Type
!---------------------------------------------------------------------------------

! -- terms from SFR that will be handled by parent APT Package
! FLOW-JA-FACE              idxbudfjf     FLOW-JA-FACE          cv2cv
! GWF (aux FLOW-AREA)       idxbudgwf     GWF                   cv2gwf
! STORAGE (aux VOLUME)      idxbudsto     none                  used for cv volumes
! FROM-MVR                  idxbudfmvr    FROM-MVR              q * tmpext = this%qfrommvr(:)  ! kluge note: include rhow*cpw in comments for various terms
! TO-MVR                    idxbudtmvr    TO-MVR                q * tfeat

! -- SFR terms
! RAINFALL                  idxbudrain    RAINFALL              q * train
! EVAPORATION               idxbudevap    EVAPORATION           tfeat<tevap: q*tfeat, else: q*tevap (latent heat will likely need to modify these calcs in the future) ! kluge note
! RUNOFF                    idxbudroff    RUNOFF                q * troff
! EXT-INFLOW                idxbudiflw    EXT-INFLOW            q * tiflw
! EXT-OUTFLOW               idxbudoutf    EXT-OUTFLOW           q * tfeat
! STRMBD-COND               idxbudsbcd    STRMBD-COND           ! kluge note: expression for this

! -- terms from a flow file that should be skipped
! CONSTANT                  none          none                  none
! AUXILIARY                 none          none                  none

! -- terms that are written to the transport budget file
! none                      none          STORAGE (aux MASS)    dE/dt
! none                      none          AUXILIARY             none
! none                      none          CONSTANT              accumulate
!
!
module GweSfeModule

  use KindModule, only: DP, I4B
  use ConstantsModule, only: DZERO, DONE, LINELENGTH
  use SimModule, only: store_error
  use BndModule, only: BndType, GetBndFromList
  use TspFmiModule, only: TspFmiType
  use SfrModule, only: SfrType
  use ObserveModule, only: ObserveType
  use TspAptModule, only: TspAptType, apt_process_obsID, &
                          apt_process_obsID12
  use GweInputDataModule, only: GweInputDataType
  use MatrixBaseModule
  !
  implicit none
  !
  private
  public :: sfe_create
  !
  character(len=*), parameter :: ftype = 'SFE'
  character(len=*), parameter :: flowtype = 'SFR'
  character(len=16) :: text = '             SFE'

  type, extends(TspAptType) :: GweSfeType

    type(GweInputDataType), pointer :: gwecommon => null() !< pointer to shared gwe data used by multiple packages but set in mst

    integer(I4B), pointer :: idxbudrain => null() !< index of rainfall terms in flowbudptr
    integer(I4B), pointer :: idxbudevap => null() !< index of evaporation terms in flowbudptr
    integer(I4B), pointer :: idxbudroff => null() !< index of runoff terms in flowbudptr
    integer(I4B), pointer :: idxbudiflw => null() !< index of inflow terms in flowbudptr
    integer(I4B), pointer :: idxbudoutf => null() !< index of outflow terms in flowbudptr
    integer(I4B), pointer :: idxbudsbcd => null() !< index of streambed conduction terms in flowbudptr

    real(DP), dimension(:), pointer, contiguous :: temprain => null() !< rainfall temperature
    real(DP), dimension(:), pointer, contiguous :: tempevap => null() !< evaporation temperature
    real(DP), dimension(:), pointer, contiguous :: temproff => null() !< runoff temperature
    real(DP), dimension(:), pointer, contiguous :: tempiflw => null() !< inflow temperature

  contains

    procedure :: bnd_da => sfe_da
    procedure :: allocate_scalars
    procedure :: apt_allocate_arrays => sfe_allocate_arrays
    procedure :: find_apt_package => find_sfe_package
    procedure :: pak_fc_expanded => sfe_fc_expanded
    procedure :: pak_solve => sfe_solve
    procedure :: pak_get_nbudterms => sfe_get_nbudterms
    procedure :: pak_setup_budobj => sfe_setup_budobj
    procedure :: pak_fill_budobj => sfe_fill_budobj
    procedure :: sfe_rain_term
    procedure :: sfe_evap_term
    procedure :: sfe_roff_term
    procedure :: sfe_iflw_term
    procedure :: sfe_outf_term
    procedure :: pak_df_obs => sfe_df_obs
    procedure :: pak_rp_obs => sfe_rp_obs
    procedure :: pak_bd_obs => sfe_bd_obs
    procedure :: pak_set_stressperiod => sfe_set_stressperiod

  end type GweSfeType

contains

  !> @brief Create a new sfe package
  !<
  subroutine sfe_create(packobj, id, ibcnum, inunit, iout, namemodel, pakname, &
                        fmi, eqnsclfac, gwecommon, dvt, dvu, dvua)
    ! -- dummy
    class(BndType), pointer :: packobj
    integer(I4B), intent(in) :: id
    integer(I4B), intent(in) :: ibcnum
    integer(I4B), intent(in) :: inunit
    integer(I4B), intent(in) :: iout
    character(len=*), intent(in) :: namemodel
    character(len=*), intent(in) :: pakname
    type(TspFmiType), pointer :: fmi
    real(DP), intent(in), pointer :: eqnsclfac !< Governing equation scale factor
    type(GweInputDataType), intent(in), target :: gwecommon !< Shared data container for use by multiple GWE packages
    character(len=*), intent(in) :: dvt !< For GWE, set to "TEMPERATURE" in TspAptType
    character(len=*), intent(in) :: dvu !< For GWE, set to "energy" in TspAptType
    character(len=*), intent(in) :: dvua !< For GWE, set to "E" in TspAptType
    ! -- local
    type(GweSfeType), pointer :: sfeobj
    !
    ! -- Allocate the object and assign values to object variables
    allocate (sfeobj)
    packobj => sfeobj
    !
    ! -- Create name and memory path
    call packobj%set_names(ibcnum, namemodel, pakname, ftype)
    packobj%text = text
    !
    ! -- Allocate scalars
    call sfeobj%allocate_scalars()
    !
    ! -- Initialize package
    call packobj%pack_initialize()
    !
    packobj%inunit = inunit
    packobj%iout = iout
    packobj%id = id
    packobj%ibcnum = ibcnum
    packobj%ncolbnd = 1
    packobj%iscloc = 1
    !
    ! -- Store pointer to flow model interface.  When the GwfGwt exchange is
    !    created, it sets fmi%bndlist so that the GWT model has access to all
    !    the flow packages
    sfeobj%fmi => fmi
    !
    ! -- Store pointer to governing equation scale factor
    sfeobj%eqnsclfac => eqnsclfac
    !
    ! -- Store pointer to shared data module for accessing cpw, rhow
    !    for the budget calculations, and for accessing the latent heat of
    !    vaporization for evaporative cooling.
    sfeobj%gwecommon => gwecommon
    !
    ! -- Set labels that will be used in generalized APT class
    sfeobj%depvartype = dvt
    sfeobj%depvarunit = dvu
    sfeobj%depvarunitabbrev = dvua
    !
    ! -- Return
    return
  end subroutine sfe_create

  !> @brief Find corresponding sfe package
  !<
  subroutine find_sfe_package(this)
    ! -- modules
    use MemoryManagerModule, only: mem_allocate
    ! -- dummy
    class(GweSfeType) :: this
    ! -- local
    character(len=LINELENGTH) :: errmsg
    class(BndType), pointer :: packobj
    integer(I4B) :: ip, icount
    integer(I4B) :: nbudterm
    logical :: found
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
            ! -- Store BndType pointer to packobj, and then
            !    use the select type to point to the budobj in flow package
            this%flowpackagebnd => packobj
            select type (packobj)
            type is (SfrType)
              this%flowbudptr => packobj%budobj
            end select
          end if
          if (found) exit
        end do
      end if
    end if
    !
    ! -- Error if flow package not found
    if (.not. found) then
      write (errmsg, '(a)') 'COULD NOT FIND FLOW PACKAGE WITH NAME '&
                            &//trim(adjustl(this%flowpackagename))//'.'
      call store_error(errmsg)
      call this%parser%StoreErrorUnit()
    end if
    !
    ! -- Allocate space for idxbudssm, which indicates whether this is a
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
      case ('RAINFALL')
        this%idxbudrain = ip
        this%idxbudssm(ip) = 0
      case ('EVAPORATION')
        this%idxbudevap = ip
        this%idxbudssm(ip) = 0
      case ('RUNOFF')
        this%idxbudroff = ip
        this%idxbudssm(ip) = 0
      case ('EXT-INFLOW')
        this%idxbudiflw = ip
        this%idxbudssm(ip) = 0
      case ('EXT-OUTFLOW')
        this%idxbudoutf = ip
        this%idxbudssm(ip) = 0
      case ('TO-MVR')
        this%idxbudtmvr = ip
        this%idxbudssm(ip) = 0
      case ('FROM-MVR')
        this%idxbudfmvr = ip
        this%idxbudssm(ip) = 0
      case ('AUXILIARY')
        this%idxbudaux = ip
        this%idxbudssm(ip) = 0
      case default
        !
        ! -- Set idxbudssm equal to a column index for where the temperatures
        !    are stored in the concbud(nbudssm, ncv) array
        this%idxbudssm(ip) = icount
        icount = icount + 1
      end select
      write (this%iout, '(a, i0, " = ", a,/, a, i0)') &
        '  TERM ', ip, trim(adjustl(this%flowbudptr%budterm(ip)%flowtype)), &
        '   MAX NO. OF ENTRIES = ', this%flowbudptr%budterm(ip)%maxlist
    end do
    write (this%iout, '(a, //)') 'DONE PROCESSING '//ftype//' INFORMATION'
    !
    ! -- Streambed conduction term
    this%idxbudsbcd = this%idxbudgwf
    !
    ! -- Return
    return
  end subroutine find_sfe_package

  !> @brief Add matrix terms related to SFE
  !!
  !! This will be called from TspAptType%apt_fc_expanded()
  !! in order to add matrix terms specifically for SFE
  !<
  subroutine sfe_fc_expanded(this, rhs, ia, idxglo, matrix_sln)
    ! -- dummy
    class(GweSfeType) :: this
    real(DP), dimension(:), intent(inout) :: rhs
    integer(I4B), dimension(:), intent(in) :: ia
    integer(I4B), dimension(:), intent(in) :: idxglo
    class(MatrixBaseType), pointer :: matrix_sln
    ! -- local
    integer(I4B) :: j, n, n1, n2
    integer(I4B) :: iloc
    integer(I4B) :: iposd, iposoffd
    integer(I4B) :: ipossymd, ipossymoffd
    integer(I4B) :: auxpos
    real(DP) :: rrate
    real(DP) :: rhsval
    real(DP) :: hcofval
    real(DP) :: ctherm
    real(DP) :: wa !< wetted area
    real(DP) :: ktf !< thermal conductivity of streambed material
    real(DP) :: s !< thickness of conductive streambed material
    !
    ! -- Add rainfall contribution
    if (this%idxbudrain /= 0) then
      do j = 1, this%flowbudptr%budterm(this%idxbudrain)%nlist
        call this%sfe_rain_term(j, n1, n2, rrate, rhsval, hcofval)
        iloc = this%idxlocnode(n1)
        iposd = this%idxpakdiag(n1)
        call matrix_sln%add_value_pos(iposd, hcofval)
        rhs(iloc) = rhs(iloc) + rhsval
      end do
    end if
    !
    ! -- Add evaporation contribution
    if (this%idxbudevap /= 0) then
      do j = 1, this%flowbudptr%budterm(this%idxbudevap)%nlist
        call this%sfe_evap_term(j, n1, n2, rrate, rhsval, hcofval)
        iloc = this%idxlocnode(n1)
        iposd = this%idxpakdiag(n1)
        call matrix_sln%add_value_pos(iposd, hcofval)
        rhs(iloc) = rhs(iloc) + rhsval
      end do
    end if
    !
    ! -- Add runoff contribution
    if (this%idxbudroff /= 0) then
      do j = 1, this%flowbudptr%budterm(this%idxbudroff)%nlist
        call this%sfe_roff_term(j, n1, n2, rrate, rhsval, hcofval)
        iloc = this%idxlocnode(n1)
        iposd = this%idxpakdiag(n1)
        call matrix_sln%add_value_pos(iposd, hcofval)
        rhs(iloc) = rhs(iloc) + rhsval
      end do
    end if
    !
    ! -- Add inflow contribution
    if (this%idxbudiflw /= 0) then
      do j = 1, this%flowbudptr%budterm(this%idxbudiflw)%nlist
        call this%sfe_iflw_term(j, n1, n2, rrate, rhsval, hcofval)
        iloc = this%idxlocnode(n1)
        iposd = this%idxpakdiag(n1)
        call matrix_sln%add_value_pos(iposd, hcofval)
        rhs(iloc) = rhs(iloc) + rhsval
      end do
    end if
    !
    ! -- Add outflow contribution
    if (this%idxbudoutf /= 0) then
      do j = 1, this%flowbudptr%budterm(this%idxbudoutf)%nlist
        call this%sfe_outf_term(j, n1, n2, rrate, rhsval, hcofval)
        iloc = this%idxlocnode(n1)
        iposd = this%idxpakdiag(n1)
        call matrix_sln%add_value_pos(iposd, hcofval)
        rhs(iloc) = rhs(iloc) + rhsval
      end do
    end if
    !
    ! -- Add streambed conduction contribution
    do j = 1, this%flowbudptr%budterm(this%idxbudgwf)%nlist
      !
      ! -- Set n to feature number and process if active feature
      n = this%flowbudptr%budterm(this%idxbudgwf)%id1(j)
      if (this%iboundpak(n) /= 0) then
        !
        ! -- Set acoef and rhs to negative so they are relative to sfe and not gwe
        auxpos = this%flowbudptr%budterm(this%idxbudgwf)%naux
        wa = this%flowbudptr%budterm(this%idxbudgwf)%auxvar(auxpos, j)
        ktf = this%ktf(n)
        s = this%rfeatthk(n)
        ctherm = ktf * wa / s
        !
        ! -- Add to sfe row
        iposd = this%idxdglo(j)
        iposoffd = this%idxoffdglo(j)
        call matrix_sln%add_value_pos(iposd, -ctherm)
        call matrix_sln%add_value_pos(iposoffd, ctherm)
        !
        ! -- Add to gwe row for sfe connection
        ipossymd = this%idxsymdglo(j)
        ipossymoffd = this%idxsymoffdglo(j)
        call matrix_sln%add_value_pos(ipossymd, -ctherm)
        call matrix_sln%add_value_pos(ipossymoffd, ctherm)
      end if
    end do
    !
    ! -- Return
    return
  end subroutine sfe_fc_expanded

  !> @ brief Add terms specific to sfr to the explicit sfe solve
  !<
  subroutine sfe_solve(this)
    ! -- dummy
    class(GweSfeType) :: this
    ! -- local
    integer(I4B) :: j
    integer(I4B) :: n1, n2
    real(DP) :: rrate
    !
    ! -- Add rainfall contribution
    if (this%idxbudrain /= 0) then
      do j = 1, this%flowbudptr%budterm(this%idxbudrain)%nlist
        call this%sfe_rain_term(j, n1, n2, rrate)
        this%dbuff(n1) = this%dbuff(n1) + rrate
      end do
    end if
    !
    ! -- Add evaporation contribution
    if (this%idxbudevap /= 0) then
      do j = 1, this%flowbudptr%budterm(this%idxbudevap)%nlist
        call this%sfe_evap_term(j, n1, n2, rrate)
        this%dbuff(n1) = this%dbuff(n1) + rrate
      end do
    end if
    !
    ! -- Add runoff contribution
    if (this%idxbudroff /= 0) then
      do j = 1, this%flowbudptr%budterm(this%idxbudroff)%nlist
        call this%sfe_roff_term(j, n1, n2, rrate)
        this%dbuff(n1) = this%dbuff(n1) + rrate
      end do
    end if
    !
    ! -- Add inflow contribution
    if (this%idxbudiflw /= 0) then
      do j = 1, this%flowbudptr%budterm(this%idxbudiflw)%nlist
        call this%sfe_iflw_term(j, n1, n2, rrate)
        this%dbuff(n1) = this%dbuff(n1) + rrate
      end do
    end if
    !
    ! -- Add outflow contribution
    if (this%idxbudoutf /= 0) then
      do j = 1, this%flowbudptr%budterm(this%idxbudoutf)%nlist
        call this%sfe_outf_term(j, n1, n2, rrate)
        this%dbuff(n1) = this%dbuff(n1) + rrate
      end do
    end if
    !
    ! Note: explicit streambed conduction terms???
    !
    ! -- Return
    return
  end subroutine sfe_solve

  !> @brief Function to return the number of budget terms just for this package.
  !!
  !! This overrides a function in the parent class.
  !<
  function sfe_get_nbudterms(this) result(nbudterms)
    ! -- dummy
    class(GweSfeType) :: this
    ! -- return
    integer(I4B) :: nbudterms
    !
    ! -- Number of budget terms is 6:
    !    1. rainfall
    !    2. evaporation
    !    3. runoff
    !    4. ext-inflow
    !    5. ext-outflow
    !    6. streambed-cond
    nbudterms = 6
    !
    ! -- Return
    return
  end function sfe_get_nbudterms

  !> @brief Set up the budget object that stores all the sfe flows
  !<
  subroutine sfe_setup_budobj(this, idx)
    ! -- modules
    use ConstantsModule, only: LENBUDTXT
    ! -- dummy
    class(GweSfeType) :: this
    integer(I4B), intent(inout) :: idx
    ! -- local
    integer(I4B) :: n, n1, n2
    integer(I4B) :: maxlist, naux
    real(DP) :: q
    character(len=LENBUDTXT) :: text
    !
    ! --
    text = '        RAINFALL'
    idx = idx + 1
    maxlist = this%flowbudptr%budterm(this%idxbudrain)%maxlist
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
    text = '     EVAPORATION'
    idx = idx + 1
    maxlist = this%flowbudptr%budterm(this%idxbudevap)%maxlist
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
    text = '          RUNOFF'
    idx = idx + 1
    maxlist = this%flowbudptr%budterm(this%idxbudroff)%maxlist
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
    text = '      EXT-INFLOW'
    idx = idx + 1
    maxlist = this%flowbudptr%budterm(this%idxbudiflw)%maxlist
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
    text = '     EXT-OUTFLOW'
    idx = idx + 1
    maxlist = this%flowbudptr%budterm(this%idxbudoutf)%maxlist
    naux = 0
    call this%budobj%budterm(idx)%initialize(text, &
                                             this%name_model, &
                                             this%packName, &
                                             this%name_model, &
                                             this%packName, &
                                             maxlist, .false., .false., &
                                             naux)
    !
    ! -- Conduction through the wetted streambed
    text = '  STREAMBED-COND'
    idx = idx + 1
    maxlist = this%flowbudptr%budterm(this%idxbudsbcd)%maxlist
    naux = 0
    call this%budobj%budterm(idx)%initialize(text, &
                                             this%name_model, &
                                             this%packName, &
                                             this%name_model, &
                                             this%packName, &
                                             maxlist, .false., .false., &
                                             naux)
    call this%budobj%budterm(idx)%reset(maxlist)
    q = DZERO
    do n = 1, maxlist
      n1 = this%flowbudptr%budterm(this%idxbudgwf)%id1(n)
      n2 = this%flowbudptr%budterm(this%idxbudgwf)%id2(n)
      call this%budobj%budterm(idx)%update_term(n1, n2, q)
    end do
    !
    ! -- Return
    return
  end subroutine sfe_setup_budobj

  !> @brief Copy flow terms into this%budobj
  !<
  subroutine sfe_fill_budobj(this, idx, x, flowja, ccratin, ccratout)
    ! -- dummy
    class(GweSfeType) :: this
    integer(I4B), intent(inout) :: idx
    real(DP), dimension(:), intent(in) :: x
    real(DP), dimension(:), contiguous, intent(inout) :: flowja
    real(DP), intent(inout) :: ccratin
    real(DP), intent(inout) :: ccratout
    ! -- local
    integer(I4B) :: j, n1, n2
    integer(I4B) :: nlist
    integer(I4B) :: igwfnode
    integer(I4B) :: idiag
    integer(I4B) :: auxpos
    real(DP) :: q
    real(DP) :: ctherm
    real(DP) :: wa !< wetted area
    real(DP) :: ktf !< thermal conductivity of streambed material
    real(DP) :: s !< thickness of conductive streambed materia
    !
    ! -- Rain
    idx = idx + 1
    nlist = this%flowbudptr%budterm(this%idxbudrain)%nlist
    call this%budobj%budterm(idx)%reset(nlist)
    do j = 1, nlist
      call this%sfe_rain_term(j, n1, n2, q)
      call this%budobj%budterm(idx)%update_term(n1, n2, q)
      call this%apt_accumulate_ccterm(n1, q, ccratin, ccratout)
    end do
    !
    ! -- Evaporation
    idx = idx + 1
    nlist = this%flowbudptr%budterm(this%idxbudevap)%nlist
    call this%budobj%budterm(idx)%reset(nlist)
    do j = 1, nlist
      call this%sfe_evap_term(j, n1, n2, q)
      call this%budobj%budterm(idx)%update_term(n1, n2, q)
      call this%apt_accumulate_ccterm(n1, q, ccratin, ccratout)
    end do
    !
    ! -- Runoff
    idx = idx + 1
    nlist = this%flowbudptr%budterm(this%idxbudroff)%nlist
    call this%budobj%budterm(idx)%reset(nlist)
    do j = 1, nlist
      call this%sfe_roff_term(j, n1, n2, q)
      call this%budobj%budterm(idx)%update_term(n1, n2, q)
      call this%apt_accumulate_ccterm(n1, q, ccratin, ccratout)
    end do
    !
    ! -- Ext-inflow
    idx = idx + 1
    nlist = this%flowbudptr%budterm(this%idxbudiflw)%nlist
    call this%budobj%budterm(idx)%reset(nlist)
    do j = 1, nlist
      call this%sfe_iflw_term(j, n1, n2, q)
      call this%budobj%budterm(idx)%update_term(n1, n2, q)
      call this%apt_accumulate_ccterm(n1, q, ccratin, ccratout)
    end do
    !
    ! -- Ext-outflow
    idx = idx + 1
    nlist = this%flowbudptr%budterm(this%idxbudoutf)%nlist
    call this%budobj%budterm(idx)%reset(nlist)
    do j = 1, nlist
      call this%sfe_outf_term(j, n1, n2, q)
      call this%budobj%budterm(idx)%update_term(n1, n2, q)
      call this%apt_accumulate_ccterm(n1, q, ccratin, ccratout)
    end do
    !
    ! -- Strmbd-cond
    idx = idx + 1
    call this%budobj%budterm(idx)%reset(this%maxbound)
    do j = 1, this%flowbudptr%budterm(this%idxbudsbcd)%nlist
      q = DZERO
      n1 = this%flowbudptr%budterm(this%idxbudsbcd)%id1(j)
      if (this%iboundpak(n1) /= 0) then
        igwfnode = this%flowbudptr%budterm(this%idxbudsbcd)%id2(j)
        ! -- For now, there is only 1 aux variable under 'GWF'
        auxpos = this%flowbudptr%budterm(this%idxbudgwf)%naux
        wa = this%flowbudptr%budterm(this%idxbudgwf)%auxvar(auxpos, j)
        ktf = this%ktf(n1)
        s = this%rfeatthk(n1)
        ctherm = ktf * wa / s
        q = ctherm * (x(igwfnode) - this%xnewpak(n1))
      end if
      call this%budobj%budterm(idx)%update_term(n1, igwfnode, q)
      call this%apt_accumulate_ccterm(n1, q, ccratin, ccratout)
      if (this%iboundpak(n1) /= 0) then
        ! -- Contribution to gwe cell budget
        this%simvals(n1) = this%simvals(n1) - q
        idiag = this%dis%con%ia(igwfnode)
        flowja(idiag) = flowja(idiag) - q
      end if
    end do
    !
    ! -- Return
    return
  end subroutine sfe_fill_budobj

  !> @brief Allocate scalars specific to the streamflow energy transport (SFE)
  !! package.
  !<
  subroutine allocate_scalars(this)
    ! -- modules
    use MemoryManagerModule, only: mem_allocate
    ! -- dummy
    class(GweSfeType) :: this
    !
    ! -- Allocate scalars in TspAptType
    call this%TspAptType%allocate_scalars()
    !
    ! -- Allocate
    call mem_allocate(this%idxbudrain, 'IDXBUDRAIN', this%memoryPath)
    call mem_allocate(this%idxbudevap, 'IDXBUDEVAP', this%memoryPath)
    call mem_allocate(this%idxbudroff, 'IDXBUDROFF', this%memoryPath)
    call mem_allocate(this%idxbudiflw, 'IDXBUDIFLW', this%memoryPath)
    call mem_allocate(this%idxbudoutf, 'IDXBUDOUTF', this%memoryPath)
    call mem_allocate(this%idxbudsbcd, 'IDXBUDSBCD', this%memoryPath)
    !
    ! -- Initialize
    this%idxbudrain = 0
    this%idxbudevap = 0
    this%idxbudroff = 0
    this%idxbudiflw = 0
    this%idxbudoutf = 0
    this%idxbudsbcd = 0
    !
    ! -- Return
    return
  end subroutine allocate_scalars

  !> @brief Allocate arrays specific to the streamflow energy transport (SFE)
  !! package.
  !<
  subroutine sfe_allocate_arrays(this)
    ! -- modules
    use MemoryManagerModule, only: mem_allocate
    ! -- dummy
    class(GweSfeType), intent(inout) :: this
    ! -- local
    integer(I4B) :: n
    !
    ! -- Time series
    call mem_allocate(this%temprain, this%ncv, 'TEMPRAIN', this%memoryPath)
    call mem_allocate(this%tempevap, this%ncv, 'TEMPEVAP', this%memoryPath)
    call mem_allocate(this%temproff, this%ncv, 'TEMPROFF', this%memoryPath)
    call mem_allocate(this%tempiflw, this%ncv, 'TEMPIFLW', this%memoryPath)
    !
    ! -- Call standard TspAptType allocate arrays
    call this%TspAptType%apt_allocate_arrays()
    !
    ! -- Initialize
    do n = 1, this%ncv
      this%temprain(n) = DZERO
      this%tempevap(n) = DZERO
      this%temproff(n) = DZERO
      this%tempiflw(n) = DZERO
    end do
    !
    ! -- Return
    return
  end subroutine sfe_allocate_arrays

  !> @brief Deallocate memory
  !<
  subroutine sfe_da(this)
    ! -- modules
    use MemoryManagerModule, only: mem_deallocate
    ! -- dummy
    class(GweSfeType) :: this
    !
    ! -- Deallocate scalars
    call mem_deallocate(this%idxbudrain)
    call mem_deallocate(this%idxbudevap)
    call mem_deallocate(this%idxbudroff)
    call mem_deallocate(this%idxbudiflw)
    call mem_deallocate(this%idxbudoutf)
    call mem_deallocate(this%idxbudsbcd)
    !
    ! -- Deallocate time series
    call mem_deallocate(this%temprain)
    call mem_deallocate(this%tempevap)
    call mem_deallocate(this%temproff)
    call mem_deallocate(this%tempiflw)
    !
    ! -- Deallocate scalars in TspAptType
    call this%TspAptType%bnd_da()
    !
    ! -- Return
    return
  end subroutine sfe_da

  !> @brief Rain term
  !<
  subroutine sfe_rain_term(this, ientry, n1, n2, rrate, rhsval, hcofval)
    ! -- dummy
    class(GweSfeType) :: this
    integer(I4B), intent(in) :: ientry
    integer(I4B), intent(inout) :: n1
    integer(I4B), intent(inout) :: n2
    real(DP), intent(inout), optional :: rrate
    real(DP), intent(inout), optional :: rhsval
    real(DP), intent(inout), optional :: hcofval
    ! -- local
    real(DP) :: qbnd
    real(DP) :: ctmp
    !
    n1 = this%flowbudptr%budterm(this%idxbudrain)%id1(ientry)
    n2 = this%flowbudptr%budterm(this%idxbudrain)%id2(ientry)
    qbnd = this%flowbudptr%budterm(this%idxbudrain)%flow(ientry)
    ctmp = this%temprain(n1)
    if (present(rrate)) rrate = ctmp * qbnd * this%eqnsclfac ! kluge note: think about budget / sensible heat issue
    if (present(rhsval)) rhsval = -rrate
    if (present(hcofval)) hcofval = DZERO
    !
    ! -- Return
    return
  end subroutine sfe_rain_term

  !> @brief Evaporative term
  !<
  subroutine sfe_evap_term(this, ientry, n1, n2, rrate, rhsval, hcofval)
    ! -- dummy
    class(GweSfeType) :: this
    integer(I4B), intent(in) :: ientry
    integer(I4B), intent(inout) :: n1
    integer(I4B), intent(inout) :: n2
    real(DP), intent(inout), optional :: rrate
    real(DP), intent(inout), optional :: rhsval
    real(DP), intent(inout), optional :: hcofval
    ! -- local
    real(DP) :: qbnd
    real(DP) :: heatlat
    !
    n1 = this%flowbudptr%budterm(this%idxbudevap)%id1(ientry)
    n2 = this%flowbudptr%budterm(this%idxbudevap)%id2(ientry)
    ! -- note that qbnd is negative for evap
    qbnd = this%flowbudptr%budterm(this%idxbudevap)%flow(ientry)
    heatlat = this%gwecommon%gwerhow * this%gwecommon%gwelatheatvap
    if (present(rrate)) rrate = qbnd * heatlat
    !!if (present(rhsval)) rhsval = -rrate / this%eqnsclfac  ! kluge note: divided by eqnsclfac for fc purposes because rrate is in terms of energy
    if (present(rhsval)) rhsval = -rrate
    if (present(hcofval)) hcofval = DZERO
    !
    ! -- Return
    return
  end subroutine sfe_evap_term

  !> @brief Runoff term
  !<
  subroutine sfe_roff_term(this, ientry, n1, n2, rrate, rhsval, hcofval)
    ! -- dummy
    class(GweSfeType) :: this
    integer(I4B), intent(in) :: ientry
    integer(I4B), intent(inout) :: n1
    integer(I4B), intent(inout) :: n2
    real(DP), intent(inout), optional :: rrate
    real(DP), intent(inout), optional :: rhsval
    real(DP), intent(inout), optional :: hcofval
    ! -- local
    real(DP) :: qbnd
    real(DP) :: ctmp
    !
    n1 = this%flowbudptr%budterm(this%idxbudroff)%id1(ientry)
    n2 = this%flowbudptr%budterm(this%idxbudroff)%id2(ientry)
    qbnd = this%flowbudptr%budterm(this%idxbudroff)%flow(ientry)
    ctmp = this%temproff(n1)
    if (present(rrate)) rrate = ctmp * qbnd * this%eqnsclfac
    if (present(rhsval)) rhsval = -rrate
    if (present(hcofval)) hcofval = DZERO
    !
    ! -- Return
    return
  end subroutine sfe_roff_term

  !> @brief Inflow Term
  !!
  !! Accounts for energy added via streamflow entering into a stream channel;
  !! for example, energy entering the model domain via a specified flow in a
  !! stream channel.
  !<
  subroutine sfe_iflw_term(this, ientry, n1, n2, rrate, rhsval, hcofval)
    ! -- dummy
    class(GweSfeType) :: this
    integer(I4B), intent(in) :: ientry
    integer(I4B), intent(inout) :: n1
    integer(I4B), intent(inout) :: n2
    real(DP), intent(inout), optional :: rrate
    real(DP), intent(inout), optional :: rhsval
    real(DP), intent(inout), optional :: hcofval
    ! -- local
    real(DP) :: qbnd
    real(DP) :: ctmp
    !
    n1 = this%flowbudptr%budterm(this%idxbudiflw)%id1(ientry)
    n2 = this%flowbudptr%budterm(this%idxbudiflw)%id2(ientry)
    qbnd = this%flowbudptr%budterm(this%idxbudiflw)%flow(ientry)
    ctmp = this%tempiflw(n1)
    if (present(rrate)) rrate = ctmp * qbnd * this%eqnsclfac
    if (present(rhsval)) rhsval = -rrate
    if (present(hcofval)) hcofval = DZERO
    !
    ! -- Return
    return
  end subroutine sfe_iflw_term

  !> @brief Outflow term
  !!
  !! Accounts for the energy leaving a stream channel, for example, energy exiting the
  !! model domain via a flow in a stream channel flowing out of the active domain.
  !<
  subroutine sfe_outf_term(this, ientry, n1, n2, rrate, rhsval, hcofval)
    ! -- dummy
    class(GweSfeType) :: this
    integer(I4B), intent(in) :: ientry
    integer(I4B), intent(inout) :: n1
    integer(I4B), intent(inout) :: n2
    real(DP), intent(inout), optional :: rrate
    real(DP), intent(inout), optional :: rhsval
    real(DP), intent(inout), optional :: hcofval
    ! -- local
    real(DP) :: qbnd
    real(DP) :: ctmp
    !
    n1 = this%flowbudptr%budterm(this%idxbudoutf)%id1(ientry)
    n2 = this%flowbudptr%budterm(this%idxbudoutf)%id2(ientry)
    qbnd = this%flowbudptr%budterm(this%idxbudoutf)%flow(ientry)
    ctmp = this%xnewpak(n1)
    if (present(rrate)) rrate = ctmp * qbnd * this%eqnsclfac
    if (present(rhsval)) rhsval = DZERO
    if (present(hcofval)) hcofval = qbnd * this%eqnsclfac
    !
    ! -- Return
    return
  end subroutine sfe_outf_term

  !> @brief Observations
  !!
  !! Store the observation type supported by the APT package and overide
  !! BndType%bnd_df_obs
  !<
  subroutine sfe_df_obs(this)
    ! -- modules
    ! -- dummy
    class(GweSfeType) :: this
    ! -- local
    integer(I4B) :: indx
    !
    ! -- Store obs type and assign procedure pointer
    !    for temperature observation type.
    call this%obs%StoreObsType('temperature', .false., indx)
    this%obs%obsData(indx)%ProcessIdPtr => apt_process_obsID
    !
    ! -- Store obs type and assign procedure pointer
    !    for flow between reaches.
    call this%obs%StoreObsType('flow-ja-face', .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => apt_process_obsID12
    !
    ! -- Store obs type and assign procedure pointer
    !    for from-mvr observation type.
    call this%obs%StoreObsType('from-mvr', .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => apt_process_obsID
    !
    ! -- Store obs type and assign procedure pointer
    !    for to-mvr observation type.
    call this%obs%StoreObsType('to-mvr', .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => apt_process_obsID
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
    !    for observation type: sfe
    call this%obs%StoreObsType('sfe', .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => apt_process_obsID
    !
    ! -- Store obs type and assign procedure pointer
    !    for rainfall observation type.
    call this%obs%StoreObsType('rainfall', .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => apt_process_obsID
    !
    ! -- Store obs type and assign procedure pointer
    !    for evaporation observation type.
    call this%obs%StoreObsType('evaporation', .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => apt_process_obsID
    !
    ! -- Store obs type and assign procedure pointer
    !    for runoff observation type.
    call this%obs%StoreObsType('runoff', .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => apt_process_obsID
    !
    ! -- Store obs type and assign procedure pointer
    !    for inflow observation type.
    call this%obs%StoreObsType('ext-inflow', .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => apt_process_obsID
    !
    ! -- Store obs type and assign procedure pointer
    !    for ext-outflow observation type.
    call this%obs%StoreObsType('ext-outflow', .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => apt_process_obsID
    !
    ! -- Return
    return
  end subroutine sfe_df_obs

  !> @brief Process package specific obs
  !!
  !! Method to process specific observations for this package.
  !<
  subroutine sfe_rp_obs(this, obsrv, found)
    ! -- dummy
    class(GweSfeType), intent(inout) :: this !< package class
    type(ObserveType), intent(inout) :: obsrv !< observation object
    logical, intent(inout) :: found !< indicate whether observation was found
    ! -- local
    !
    found = .true.
    select case (obsrv%ObsTypeId)
    case ('RAINFALL')
      call this%rp_obs_byfeature(obsrv)
    case ('EVAPORATION')
      call this%rp_obs_byfeature(obsrv)
    case ('RUNOFF')
      call this%rp_obs_byfeature(obsrv)
    case ('EXT-INFLOW')
      call this%rp_obs_byfeature(obsrv)
    case ('EXT-OUTFLOW')
      call this%rp_obs_byfeature(obsrv)
    case ('TO-MVR')
      call this%rp_obs_byfeature(obsrv)
    case default
      found = .false.
    end select
    !
    ! -- Return
    return
  end subroutine sfe_rp_obs

  !> @brief Calculate observation value and pass it back to APT
  !<
  subroutine sfe_bd_obs(this, obstypeid, jj, v, found)
    ! -- dummy
    class(GweSfeType), intent(inout) :: this
    character(len=*), intent(in) :: obstypeid
    real(DP), intent(inout) :: v
    integer(I4B), intent(in) :: jj
    logical, intent(inout) :: found
    ! -- local
    integer(I4B) :: n1, n2
    !
    found = .true.
    select case (obstypeid)
    case ('RAINFALL')
      if (this%iboundpak(jj) /= 0) then
        call this%sfe_rain_term(jj, n1, n2, v)
      end if
    case ('EVAPORATION')
      if (this%iboundpak(jj) /= 0) then
        call this%sfe_evap_term(jj, n1, n2, v)
      end if
    case ('RUNOFF')
      if (this%iboundpak(jj) /= 0) then
        call this%sfe_roff_term(jj, n1, n2, v)
      end if
    case ('EXT-INFLOW')
      if (this%iboundpak(jj) /= 0) then
        call this%sfe_iflw_term(jj, n1, n2, v)
      end if
    case ('EXT-OUTFLOW')
      if (this%iboundpak(jj) /= 0) then
        call this%sfe_outf_term(jj, n1, n2, v)
      end if
    case default
      found = .false.
    end select
    !
    ! -- Return
    return
  end subroutine sfe_bd_obs

  !> @brief Sets the stress period attributes for keyword use.
  !<
  subroutine sfe_set_stressperiod(this, itemno, keyword, found)
    ! -- modules
    use TimeSeriesManagerModule, only: read_value_or_time_series_adv
    ! -- dummy
    class(GweSfeType), intent(inout) :: this
    integer(I4B), intent(in) :: itemno
    character(len=*), intent(in) :: keyword
    logical, intent(inout) :: found
    ! -- local
    character(len=LINELENGTH) :: text
    integer(I4B) :: ierr
    integer(I4B) :: jj
    real(DP), pointer :: bndElem => null()
    !
    ! RAINFALL <rainfall>
    ! EVAPORATION <evaporation>
    ! RUNOFF <runoff>
    ! INFLOW <inflow>
    ! WITHDRAWAL <withdrawal>
    !
    found = .true.
    select case (keyword)
    case ('RAINFALL')
      ierr = this%apt_check_valid(itemno)
      if (ierr /= 0) then
        goto 999
      end if
      call this%parser%GetString(text)
      jj = 1
      bndElem => this%temprain(itemno)
      call read_value_or_time_series_adv(text, itemno, jj, bndElem, &
                                         this%packName, 'BND', this%tsManager, &
                                         this%iprpak, 'RAINFALL')
    case ('EVAPORATION')
      ierr = this%apt_check_valid(itemno)
      if (ierr /= 0) then
        goto 999
      end if
      call this%parser%GetString(text)
      jj = 1
      bndElem => this%tempevap(itemno)
      call read_value_or_time_series_adv(text, itemno, jj, bndElem, &
                                         this%packName, 'BND', this%tsManager, &
                                         this%iprpak, 'EVAPORATION')
    case ('RUNOFF')
      ierr = this%apt_check_valid(itemno)
      if (ierr /= 0) then
        goto 999
      end if
      call this%parser%GetString(text)
      jj = 1
      bndElem => this%temproff(itemno)
      call read_value_or_time_series_adv(text, itemno, jj, bndElem, &
                                         this%packName, 'BND', this%tsManager, &
                                         this%iprpak, 'RUNOFF')
    case ('INFLOW')
      ierr = this%apt_check_valid(itemno)
      if (ierr /= 0) then
        goto 999
      end if
      call this%parser%GetString(text)
      jj = 1
      bndElem => this%tempiflw(itemno)
      call read_value_or_time_series_adv(text, itemno, jj, bndElem, &
                                         this%packName, 'BND', this%tsManager, &
                                         this%iprpak, 'INFLOW')
    case default
      !
      ! -- Keyword not recognized so return to caller with found = .false.
      found = .false.
    end select
    !
999 continue
    !
    ! -- Return
    return
  end subroutine sfe_set_stressperiod

end module GweSfeModule
