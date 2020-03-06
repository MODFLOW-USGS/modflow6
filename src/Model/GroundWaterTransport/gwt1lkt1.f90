! -- Lake Transport Module
! -- todo: what to do about reactions in lake?  Decay?
! -- todo: save the lkt concentration into the lak aux variable?
! -- todo: calculate the lak DENSE aux variable using concentration?
!
! LAK flows (lakbudptr)     index var     LKT term              Transport Type
!---------------------------------------------------------------------------------
! FLOW-JA-FACE              idxbudfjf     FLOW-JA-FACE          lak2lak
! GWF (aux FLOW-AREA)       idxbudgwf     GWF                   lak2gwf
! RAINFALL                  idxbudrain    RAINFALL              q * crain
! EVAPORATION               idxbudevap    EVAPORATION           clak<cevap: q*clak, else: q*cevap
! RUNOFF                    idxbudroff    RUNOFF                q * croff
! EXT-INFLOW                idxbudiflw    EXT-INFLOW            q * ciflw
! WITHDRAWAL                idxbudwdrl    WITHDRAWAL            q * clak
! EXT-OUTFLOW               idxbudoutf    EXT-OUTFLOW           q * clak
! TO-MVR                    idxbudtmvr    TO-MVR                q * clak
! FROM-MVR                  idxbudfmvr    FROM-MVR              q * cext
! STORAGE (aux VOLUME)      idxbudsto     none                  used for lake volumes
! none                      none          STORAGE (aux MASS)    
! CONSTANT                  none          none                  none
! AUXILIARY                 none          none                  none
! none                      none          AUXILIARY             none
! none                      none          CONSTANT              accumulate
!
!

module GwtLktModule

  use KindModule, only: DP, I4B
  use ConstantsModule, only: DZERO, DONE, DHALF, DEP20, LENFTYPE, LINELENGTH,  &
                             LENBOUNDNAME, NAMEDBOUNDFLAG, DNODATA,            &
                             TABLEFT, TABCENTER, TABRIGHT,                     &
                             TABSTRING, TABUCSTRING, TABINTEGER, TABREAL
  use SimModule, only: store_error, count_errors, store_error_unit, ustop
  use BndModule, only: BndType, GetBndFromList
  use GwtFmiModule, only: GwtFmiType
  use LakModule, only: LakType
  use MemoryTypeModule, only: MemoryTSType
  use BudgetModule, only: BudgetType
  use BudgetObjectModule, only: BudgetObjectType, budgetobject_cr, budgetobject_cr_bfr
  use BudgetFileReaderModule, only: BudgetFileReaderType
  use ObserveModule, only: ObserveType
  use InputOutputModule, only: extract_idnum_or_bndname
  use BaseDisModule, only: DisBaseType
  use ArrayHandlersModule, only: ExpandArray
  use GwtAptModule, only: GwtAptType
  
  implicit none
  
  public lkt_create
  
  character(len=*), parameter :: ftype = 'LKT'
  character(len=*), parameter :: flowtype = 'LAK'
  character(len=16)       :: text  = '             LKT'
  
  type, extends(GwtAptType) :: GwtLktType
    
    integer(I4B), pointer                              :: idxbudrain => null()  ! index of rainfall terms in flowbudptr
    integer(I4B), pointer                              :: idxbudevap => null()  ! index of evaporation terms in flowbudptr
    integer(I4B), pointer                              :: idxbudroff => null()  ! index of runoff terms in flowbudptr
    integer(I4B), pointer                              :: idxbudiflw => null()  ! index of inflow terms in flowbudptr
    integer(I4B), pointer                              :: idxbudwdrl => null()  ! index of withdrawal terms in flowbudptr
    integer(I4B), pointer                              :: idxbudoutf => null()  ! index of outflow terms in flowbudptr

    type (MemoryTSType), dimension(:), pointer, contiguous :: concrain => null() ! rainfall concentration
    type (MemoryTSType), dimension(:), pointer, contiguous :: concevap => null() ! evaporation concentration
    type (MemoryTSType), dimension(:), pointer, contiguous :: concroff => null() ! runoff concentration
    type (MemoryTSType), dimension(:), pointer, contiguous :: conciflw => null() ! inflow concentration

  contains
  
    procedure :: bnd_da => lkt_da
    procedure :: allocate_scalars
    procedure :: apt_allocate_arrays => lkt_allocate_arrays
    procedure :: find_apt_package => find_lkt_package
    procedure :: pak_fc_expanded => lkt_fc_expanded
    procedure :: pak_solve => lkt_solve
    procedure :: pak_get_nbudterms => lkt_get_nbudterms
    procedure :: pak_setup_budobj => lkt_setup_budobj
    procedure :: pak_fill_budobj => lkt_fill_budobj
    procedure :: lkt_rain_term
    procedure :: lkt_evap_term
    procedure :: lkt_roff_term
    procedure :: lkt_iflw_term
    procedure :: lkt_wdrl_term
    procedure :: lkt_outf_term
    procedure :: pak_df_obs => lkt_df_obs
    procedure :: pak_bd_obs => lkt_bd_obs
    procedure :: pak_set_stressperiod => lkt_set_stressperiod
    
  end type GwtLktType

  contains  
  
  subroutine lkt_create(packobj, id, ibcnum, inunit, iout, namemodel, pakname, &
                        fmi)
! ******************************************************************************
! mwt_create -- Create a New MWT Package
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

  subroutine find_lkt_package(this)
! ******************************************************************************
! find corresponding lkt package
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryManagerModule, only: mem_allocate
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
    ! -- Initialize found to false, and error later if flow package cannot
    !    be found
    found = .false.
    !
    ! -- If user is specifying flows in a binary budget file, then set up
    !    the budget file reader, otherwise set a pointer to the flow package
    !    budobj
    if (this%iflowbudget /= 0) then
      !
      ! -- Set up the flowbudptr by filling it from a preexisting binary
      !    file created by a previous GWF simulation
      call budgetobject_cr_bfr(this%flowbudptr, this%name, this%iflowbudget,    &
                               this%iout, colconv2=['GWF             '])
      call this%flowbudptr%fill_from_bfr(this%dis, this%iout)
      found = .true.
      !
    else
      if (associated(this%fmi%gwfbndlist)) then
        ! -- Look through gwfbndlist for a flow package with the same name as 
        !    this transport package name
        do ip = 1, this%fmi%gwfbndlist%Count()
          packobj => GetBndFromList(this%fmi%gwfbndlist, ip)
          if (packobj%name == this%name) then
            found = .true.
            select type (packobj)
              type is (LakType)
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
      write(errmsg, '(a)') '****ERROR. CORRESPONDING FLOW PACKAGE NOT FOUND &
                            &FOR' // ftype // '.'
      call store_error(errmsg)
      call this%parser%StoreErrorUnit()
      call ustop()
    endif
    !
    ! -- allocate space for idxbudssm, which indicates whether this is a 
    !    special budget term or one that is a general source and sink
    nbudterm = this%flowbudptr%nbudterm
    call mem_allocate(this%idxbudssm, nbudterm, 'IDXBUDSSM', this%origin)
    !
    ! -- Process budget terms and identify special budget terms
    write(this%iout, '(/, a, a)') &
      'PROCESSING ' // ftype // ' INFORMATION FOR ', this%name
    write(this%iout, '(a)') '  IDENTIFYING FLOW TERMS IN ' // flowtype // ' PACKAGE'
    write(this%iout, '(a, i0)') &
      '  NUMBER OF ' // flowtype // ' = ', this%flowbudptr%ncv
    icount = 1
    do ip = 1, this%flowbudptr%nbudterm
      select case(trim(adjustl(this%flowbudptr%budterm(ip)%flowtype)))
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
        !    are stored in the concbud(nbudssm, ncv) array
        this%idxbudssm(ip) = icount
        icount = icount + 1
      end select
      write(this%iout, '(a, i0, " = ", a,/, a, i0)') &
        '  TERM ', ip, trim(adjustl(this%flowbudptr%budterm(ip)%flowtype)), &
        '   MAX NO. OF ENTRIES = ', this%flowbudptr%budterm(ip)%maxlist
    end do
    write(this%iout, '(a, //)') 'DONE PROCESSING ' // ftype // ' INFORMATION'
    !
    ! -- Return
    return
end subroutine find_lkt_package

  subroutine lkt_fc_expanded(this, rhs, ia, idxglo, amatsln)
! ******************************************************************************
! lkt_fc_expanded -- this will be called from GwtAptType%apt_fc_expanded()
!   in order to add matrix terms specifically for LKT
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
    integer(I4B) :: j, n1, n2
    integer(I4B) :: iloc
    integer(I4B) :: iposd
    real(DP) :: rrate
    real(DP) :: rhsval
    real(DP) :: hcofval
! ------------------------------------------------------------------------------
    !
    ! -- add rainfall contribution
    if (this%idxbudrain /= 0) then
      do j = 1, this%flowbudptr%budterm(this%idxbudrain)%nlist
        call this%lkt_rain_term(j, n1, n2, rrate, rhsval, hcofval)
        iloc = this%idxlocnode(n1)
        iposd = this%idxpakdiag(n1)
        amatsln(iposd) = amatsln(iposd) + hcofval
        rhs(iloc) = rhs(iloc) + rhsval
      end do
    end if
    !
    ! -- add evaporation contribution
    if (this%idxbudevap /= 0) then
      do j = 1, this%flowbudptr%budterm(this%idxbudevap)%nlist
        call this%lkt_evap_term(j, n1, n2, rrate, rhsval, hcofval)
        iloc = this%idxlocnode(n1)
        iposd = this%idxpakdiag(n1)
        amatsln(iposd) = amatsln(iposd) + hcofval
        rhs(iloc) = rhs(iloc) + rhsval
      end do
    end if
    !
    ! -- add runoff contribution
    if (this%idxbudroff /= 0) then
      do j = 1, this%flowbudptr%budterm(this%idxbudroff)%nlist
        call this%lkt_roff_term(j, n1, n2, rrate, rhsval, hcofval)
        iloc = this%idxlocnode(n1)
        iposd = this%idxpakdiag(n1)
        amatsln(iposd) = amatsln(iposd) + hcofval
        rhs(iloc) = rhs(iloc) + rhsval
      end do
    end if
    !
    ! -- add inflow contribution
    if (this%idxbudiflw /= 0) then
      do j = 1, this%flowbudptr%budterm(this%idxbudiflw)%nlist
        call this%lkt_iflw_term(j, n1, n2, rrate, rhsval, hcofval)
        iloc = this%idxlocnode(n1)
        iposd = this%idxpakdiag(n1)
        amatsln(iposd) = amatsln(iposd) + hcofval
        rhs(iloc) = rhs(iloc) + rhsval
      end do
    end if
    !
    ! -- add withdrawal contribution
    if (this%idxbudwdrl /= 0) then
      do j = 1, this%flowbudptr%budterm(this%idxbudwdrl)%nlist
        call this%lkt_wdrl_term(j, n1, n2, rrate, rhsval, hcofval)
        iloc = this%idxlocnode(n1)
        iposd = this%idxpakdiag(n1)
        amatsln(iposd) = amatsln(iposd) + hcofval
        rhs(iloc) = rhs(iloc) + rhsval
      end do
    end if
    !
    ! -- add outflow contribution
    if (this%idxbudoutf /= 0) then
      do j = 1, this%flowbudptr%budterm(this%idxbudoutf)%nlist
        call this%lkt_outf_term(j, n1, n2, rrate, rhsval, hcofval)
        iloc = this%idxlocnode(n1)
        iposd = this%idxpakdiag(n1)
        amatsln(iposd) = amatsln(iposd) + hcofval
        rhs(iloc) = rhs(iloc) + rhsval
      end do
    end if
    !
    ! -- Return
    return
  end subroutine lkt_fc_expanded

  subroutine lkt_solve(this)
! ******************************************************************************
! lkt_solve -- add terms specific to lakes to the explicit lake solve
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(GwtLktType) :: this
    ! -- local
    integer(I4B) :: j
    integer(I4B) :: n1, n2
    real(DP) :: rrate
! ------------------------------------------------------------------------------
    !
    ! -- add rainfall contribution
    if (this%idxbudrain /= 0) then
      do j = 1, this%flowbudptr%budterm(this%idxbudrain)%nlist
        call this%lkt_rain_term(j, n1, n2, rrate)
        this%dbuff(n1) = this%dbuff(n1) + rrate
      end do
    end if
    !
    ! -- add evaporation contribution
    if (this%idxbudevap /= 0) then
      do j = 1, this%flowbudptr%budterm(this%idxbudevap)%nlist
        call this%lkt_evap_term(j, n1, n2, rrate)
        this%dbuff(n1) = this%dbuff(n1) + rrate
      end do
    end if
    !
    ! -- add runoff contribution
    if (this%idxbudroff /= 0) then
      do j = 1, this%flowbudptr%budterm(this%idxbudroff)%nlist
        call this%lkt_roff_term(j, n1, n2, rrate)
        this%dbuff(n1) = this%dbuff(n1) + rrate
      end do
    end if
    !
    ! -- add inflow contribution
    if (this%idxbudiflw /= 0) then
      do j = 1, this%flowbudptr%budterm(this%idxbudiflw)%nlist
        call this%lkt_iflw_term(j, n1, n2, rrate)
        this%dbuff(n1) = this%dbuff(n1) + rrate
      end do
    end if
    !
    ! -- add withdrawal contribution
    if (this%idxbudwdrl /= 0) then
      do j = 1, this%flowbudptr%budterm(this%idxbudwdrl)%nlist
        call this%lkt_wdrl_term(j, n1, n2, rrate)
        this%dbuff(n1) = this%dbuff(n1) + rrate
      end do
    end if
    !
    ! -- add outflow contribution
    if (this%idxbudoutf /= 0) then
      do j = 1, this%flowbudptr%budterm(this%idxbudoutf)%nlist
        call this%lkt_outf_term(j, n1, n2, rrate)
        this%dbuff(n1) = this%dbuff(n1) + rrate
      end do
    end if
    !
    ! -- Return
    return
  end subroutine lkt_solve
  
  function lkt_get_nbudterms(this) result(nbudterms)
! ******************************************************************************
! lkt_get_nbudterms -- function to return the number of budget terms just for
!   this package.  This overrides function in parent.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(GwtLktType) :: this
    ! -- return
    integer(I4B) :: nbudterms
    ! -- local
! ------------------------------------------------------------------------------
    !
    ! -- Number of budget terms is 6
    nbudterms = 6
    !
    ! -- Return
    return
  end function lkt_get_nbudterms
  
  subroutine lkt_setup_budobj(this, idx)
! ******************************************************************************
! lkt_setup_budobj -- Set up the budget object that stores all the lake flows
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use ConstantsModule, only: LENBUDTXT
    ! -- dummy
    class(GwtLktType) :: this
    integer(I4B), intent(inout) :: idx
    ! -- local
    integer(I4B) :: maxlist, naux
    character(len=LENBUDTXT) :: text
! ------------------------------------------------------------------------------
    !
    ! -- 
    text = '        RAINFALL'
    idx = idx + 1
    maxlist = this%flowbudptr%budterm(this%idxbudrain)%maxlist
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
    maxlist = this%flowbudptr%budterm(this%idxbudevap)%maxlist
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
    maxlist = this%flowbudptr%budterm(this%idxbudroff)%maxlist
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
    maxlist = this%flowbudptr%budterm(this%idxbudiflw)%maxlist
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
    text = '      WITHDRAWAL'
    idx = idx + 1
    maxlist = this%flowbudptr%budterm(this%idxbudwdrl)%maxlist
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
    maxlist = this%flowbudptr%budterm(this%idxbudoutf)%maxlist
    naux = 0
    call this%budobj%budterm(idx)%initialize(text, &
                                             this%name_model, &
                                             this%name, &
                                             this%name_model, &
                                             this%name, &
                                             maxlist, .false., .false., &
                                             naux)
    !
    ! -- return
    return
  end subroutine lkt_setup_budobj

  subroutine lkt_fill_budobj(this, idx, x, ccratin, ccratout)
! ******************************************************************************
! lkt_fill_budobj -- copy flow terms into this%budobj
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(GwtLktType) :: this
    integer(I4B), intent(inout) :: idx
    real(DP), dimension(:), intent(in) :: x
    real(DP), intent(inout) :: ccratin
    real(DP), intent(inout) :: ccratout
    ! -- local
    integer(I4B) :: j, n1, n2
    integer(I4B) :: nlist
    real(DP) :: q
    ! -- formats
! -----------------------------------------------------------------------------
    
    ! -- RAIN
    idx = idx + 1
    nlist = this%flowbudptr%budterm(this%idxbudrain)%nlist
    call this%budobj%budterm(idx)%reset(nlist)
    do j = 1, nlist
      call this%lkt_rain_term(j, n1, n2, q)
      call this%budobj%budterm(idx)%update_term(n1, n2, q)
      call this%apt_accumulate_ccterm(n1, q, ccratin, ccratout)
    end do
    
    
    ! -- EVAPORATION
    idx = idx + 1
    nlist = this%flowbudptr%budterm(this%idxbudevap)%nlist
    call this%budobj%budterm(idx)%reset(nlist)
    do j = 1, nlist
      call this%lkt_evap_term(j, n1, n2, q)
      call this%budobj%budterm(idx)%update_term(n1, n2, q)
      call this%apt_accumulate_ccterm(n1, q, ccratin, ccratout)
    end do
    
    
    ! -- RUNOFF
    idx = idx + 1
    nlist = this%flowbudptr%budterm(this%idxbudroff)%nlist
    call this%budobj%budterm(idx)%reset(nlist)
    do j = 1, nlist
      call this%lkt_roff_term(j, n1, n2, q)
      call this%budobj%budterm(idx)%update_term(n1, n2, q)
      call this%apt_accumulate_ccterm(n1, q, ccratin, ccratout)
    end do
    
    
    ! -- EXT-INFLOW
    idx = idx + 1
    nlist = this%flowbudptr%budterm(this%idxbudiflw)%nlist
    call this%budobj%budterm(idx)%reset(nlist)
    do j = 1, nlist
      call this%lkt_iflw_term(j, n1, n2, q)
      call this%budobj%budterm(idx)%update_term(n1, n2, q)
      call this%apt_accumulate_ccterm(n1, q, ccratin, ccratout)
    end do
    
    
    ! -- WITHDRAWAL
    idx = idx + 1
    nlist = this%flowbudptr%budterm(this%idxbudwdrl)%nlist
    call this%budobj%budterm(idx)%reset(nlist)
    do j = 1, nlist
      call this%lkt_wdrl_term(j, n1, n2, q)
      call this%budobj%budterm(idx)%update_term(n1, n2, q)
      call this%apt_accumulate_ccterm(n1, q, ccratin, ccratout)
    end do
    
    
    ! -- EXT-OUTFLOW
    idx = idx + 1
    nlist = this%flowbudptr%budterm(this%idxbudoutf)%nlist
    call this%budobj%budterm(idx)%reset(nlist)
    do j = 1, nlist
      call this%lkt_outf_term(j, n1, n2, q)
      call this%budobj%budterm(idx)%update_term(n1, n2, q)
      call this%apt_accumulate_ccterm(n1, q, ccratin, ccratout)
    end do
    

    !
    ! -- return
    return
  end subroutine lkt_fill_budobj

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
    ! -- allocate scalars in GwtAptType
    call this%GwtAptType%allocate_scalars()
    !
    ! -- Allocate
    call mem_allocate(this%idxbudrain, 'IDXBUDRAIN', this%origin)
    call mem_allocate(this%idxbudevap, 'IDXBUDEVAP', this%origin)
    call mem_allocate(this%idxbudroff, 'IDXBUDROFF', this%origin)
    call mem_allocate(this%idxbudiflw, 'IDXBUDIFLW', this%origin)
    call mem_allocate(this%idxbudwdrl, 'IDXBUDWDRL', this%origin)
    call mem_allocate(this%idxbudoutf, 'IDXBUDOUTF', this%origin)
    ! 
    ! -- Initialize
    this%idxbudrain = 0
    this%idxbudevap = 0
    this%idxbudroff = 0
    this%idxbudiflw = 0
    this%idxbudwdrl = 0
    this%idxbudoutf = 0
    !
    ! -- Return
    return
  end subroutine allocate_scalars

  subroutine lkt_allocate_arrays(this)
! ******************************************************************************
! lkt_allocate_arrays
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryManagerModule, only: mem_allocate
    ! -- dummy
    class(GwtLktType), intent(inout) :: this
    ! -- local
! ------------------------------------------------------------------------------
    !    
    ! -- time series
    call mem_allocate(this%concrain, this%ncv, 'CONCRAIN', this%origin)
    call mem_allocate(this%concevap, this%ncv, 'CONCEVAP', this%origin)
    call mem_allocate(this%concroff, this%ncv, 'CONCROFF', this%origin)
    call mem_allocate(this%conciflw, this%ncv, 'CONCIFLW', this%origin)
    !
    ! -- call standard GwtApttype allocate arrays
    call this%GwtAptType%apt_allocate_arrays()
    !
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
    ! -- deallocate scalars
    call mem_deallocate(this%idxbudrain)
    call mem_deallocate(this%idxbudevap)
    call mem_deallocate(this%idxbudroff)
    call mem_deallocate(this%idxbudiflw)
    call mem_deallocate(this%idxbudwdrl)
    call mem_deallocate(this%idxbudoutf)
    !
    ! -- deallocate time series
    call mem_deallocate(this%concrain)
    call mem_deallocate(this%concevap)
    call mem_deallocate(this%concroff)
    call mem_deallocate(this%conciflw)
    !
    ! -- deallocate scalars in GwtAptType
    call this%GwtAptType%bnd_da()
    !
    ! -- Return
    return
  end subroutine lkt_da

  subroutine lkt_rain_term(this, ientry, n1, n2, rrate, &
                           rhsval, hcofval)
! ******************************************************************************
! lkt_rain_term
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(GwtLktType) :: this
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
    n1 = this%flowbudptr%budterm(this%idxbudrain)%id1(ientry)
    n2 = this%flowbudptr%budterm(this%idxbudrain)%id2(ientry)
    qbnd = this%flowbudptr%budterm(this%idxbudrain)%flow(ientry)
    ctmp = this%concrain(n1)%value
    if (present(rrate)) rrate = ctmp * qbnd
    if (present(rhsval)) rhsval = -rrate
    if (present(hcofval)) hcofval = DZERO
    !
    ! -- return
    return
  end subroutine lkt_rain_term
  
  subroutine lkt_evap_term(this, ientry, n1, n2, rrate, &
                           rhsval, hcofval)
! ******************************************************************************
! lkt_evap_term
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(GwtLktType) :: this
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
    n1 = this%flowbudptr%budterm(this%idxbudevap)%id1(ientry)
    n2 = this%flowbudptr%budterm(this%idxbudevap)%id2(ientry)
    ! -- note that qbnd is negative for evap
    qbnd = this%flowbudptr%budterm(this%idxbudevap)%flow(ientry)
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
  end subroutine lkt_evap_term
  
  subroutine lkt_roff_term(this, ientry, n1, n2, rrate, &
                           rhsval, hcofval)
! ******************************************************************************
! lkt_roff_term
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(GwtLktType) :: this
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
    n1 = this%flowbudptr%budterm(this%idxbudroff)%id1(ientry)
    n2 = this%flowbudptr%budterm(this%idxbudroff)%id2(ientry)
    qbnd = this%flowbudptr%budterm(this%idxbudroff)%flow(ientry)
    ctmp = this%concroff(n1)%value
    if (present(rrate)) rrate = ctmp * qbnd
    if (present(rhsval)) rhsval = -rrate
    if (present(hcofval)) hcofval = DZERO
    !
    ! -- return
    return
  end subroutine lkt_roff_term
  
  subroutine lkt_iflw_term(this, ientry, n1, n2, rrate, &
                           rhsval, hcofval)
! ******************************************************************************
! lkt_iflw_term
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(GwtLktType) :: this
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
    n1 = this%flowbudptr%budterm(this%idxbudiflw)%id1(ientry)
    n2 = this%flowbudptr%budterm(this%idxbudiflw)%id2(ientry)
    qbnd = this%flowbudptr%budterm(this%idxbudiflw)%flow(ientry)
    ctmp = this%conciflw(n1)%value
    if (present(rrate)) rrate = ctmp * qbnd
    if (present(rhsval)) rhsval = -rrate
    if (present(hcofval)) hcofval = DZERO
    !
    ! -- return
    return
  end subroutine lkt_iflw_term
  
  subroutine lkt_wdrl_term(this, ientry, n1, n2, rrate, &
                           rhsval, hcofval)
! ******************************************************************************
! lkt_wdrl_term
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(GwtLktType) :: this
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
    n1 = this%flowbudptr%budterm(this%idxbudwdrl)%id1(ientry)
    n2 = this%flowbudptr%budterm(this%idxbudwdrl)%id2(ientry)
    qbnd = this%flowbudptr%budterm(this%idxbudwdrl)%flow(ientry)
    ctmp = this%xnewpak(n1)
    if (present(rrate)) rrate = ctmp * qbnd
    if (present(rhsval)) rhsval = DZERO
    if (present(hcofval)) hcofval = qbnd
    !
    ! -- return
    return
  end subroutine lkt_wdrl_term
  
  subroutine lkt_outf_term(this, ientry, n1, n2, rrate, &
                           rhsval, hcofval)
! ******************************************************************************
! lkt_outf_term
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(GwtLktType) :: this
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
    n1 = this%flowbudptr%budterm(this%idxbudoutf)%id1(ientry)
    n2 = this%flowbudptr%budterm(this%idxbudoutf)%id2(ientry)
    qbnd = this%flowbudptr%budterm(this%idxbudoutf)%flow(ientry)
    ctmp = this%xnewpak(n1)
    if (present(rrate)) rrate = ctmp * qbnd
    if (present(rhsval)) rhsval = DZERO
    if (present(hcofval)) hcofval = qbnd
    !
    ! -- return
    return
  end subroutine lkt_outf_term
  
  subroutine lkt_df_obs(this)
! ******************************************************************************
! lkt_df_obs -- obs are supported?
!   -- Store observation type supported by APT package.
!   -- Overrides BndType%bnd_df_obs
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use GwtAptModule, only: apt_process_obsID
    ! -- dummy
    class(GwtLktType) :: this
    ! -- local
    integer(I4B) :: indx
! ------------------------------------------------------------------------------
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
    !    for withdrawal observation type.
    call this%obs%StoreObsType('withdrawal', .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => apt_process_obsID
    !
    ! -- Store obs type and assign procedure pointer
    !    for ext-outflow observation type.
    call this%obs%StoreObsType('outflow', .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => apt_process_obsID
    !
    return
  end subroutine lkt_df_obs
  
  subroutine lkt_bd_obs(this, obstypeid, jj, v, found)
! ******************************************************************************
! lkt_bd_obs -- calculate observation value and pass it back to APT
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(GwtLktType), intent(inout) :: this
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
      case ('RAINFALL')
        if (this%iboundpak(jj) /= 0) then
          call this%lkt_rain_term(jj, n1, n2, v)
        end if
      case ('EVAPORATION')
        if (this%iboundpak(jj) /= 0) then
          call this%lkt_evap_term(jj, n1, n2, v)
        end if
      case ('RUNOFF')
        if (this%iboundpak(jj) /= 0) then
          call this%lkt_roff_term(jj, n1, n2, v)
        end if
      case ('EXT-INFLOW')
        if (this%iboundpak(jj) /= 0) then
          call this%lkt_iflw_term(jj, n1, n2, v)
        end if
      case ('WITHDRAWAL')
        if (this%iboundpak(jj) /= 0) then
          call this%lkt_wdrl_term(jj, n1, n2, v)
        end if
      case ('EXT-OUTFLOW')
        if (this%iboundpak(jj) /= 0) then
          call this%lkt_outf_term(jj, n1, n2, v)
        end if
      case default
        found = .false.
    end select
    !
    return
  end subroutine lkt_bd_obs

  subroutine lkt_set_stressperiod(this, itemno, itmp, line, found, &
                                  lloc, istart, istop, endtim, bndName)
! ******************************************************************************
! lkt_set_stressperiod -- Set a stress period attribute for using keywords.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use TimeSeriesManagerModule, only: read_single_value_or_time_series
    use InputOutputModule, only: urword
    ! -- dummy
    class(GwtLktType),intent(inout) :: this
    integer(I4B), intent(in) :: itemno
    integer(I4B), intent(in) :: itmp
    character (len=*), intent(in) :: line
    logical, intent(inout) :: found
    integer(I4B), intent(inout) :: lloc
    integer(I4B), intent(inout) :: istart
    integer(I4B), intent(inout) :: istop
    real(DP), intent(in) :: endtim
    character(len=LENBOUNDNAME), intent(in) :: bndName
    ! -- local
    character(len=LINELENGTH) :: text
    integer(I4B) :: ierr
    integer(I4B) :: ival
    integer(I4B) :: jj
    real(DP) :: rval
    ! -- formats
! ------------------------------------------------------------------------------
    !
    ! RAINFALL <rainfall>
    ! EVAPORATION <evaporation>
    ! RUNOFF <runoff>
    ! INFLOW <inflow>
    ! WITHDRAWAL <withdrawal>
    !
    found = .true.
    select case (line(istart:istop))
      case ('RAINFALL')
        ierr = this%apt_check_valid(itemno)
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
        ierr = this%apt_check_valid(itemno)
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
        ierr = this%apt_check_valid(itemno)
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
        ierr = this%apt_check_valid(itemno)
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
  end subroutine lkt_set_stressperiod


end module GwtLktModule
