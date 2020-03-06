! -- Multi-Aquifer Well Transport Module
! -- todo: what to do about reactions in maw?  Decay?
! -- todo: save the mwt concentration into the mwt aux variable?
! -- todo: calculate the maw DENSE aux variable using concentration?
!
! MAW flows (flowbudptr)     index var    MWT term              Transport Type
!---------------------------------------------------------------------------------
! GWF (aux FLOW-AREA)       idxbudgwf     GWF                   maw2gwf
! RATE                      ?             RATE                  q < 0: q * cwell, else q * cuser
! FW-RATE                   ?             FW-RATE               q * cwell
! STORAGE (aux VOLUME)      ?             none                  used for well volumes
! CONSTANT                  none          none                  none
! FROM-MVR                  ?             FROM-MVR              q * cext = this%qfrommvr(:)
! RATE TO-MVR               ?             RATE TO-MVR           q * cwell
! CONSTANT TO-MVR           ?             CONSTANT TO-MVR       q * cwell
! FW-RATE TO-MVR            ?             FW-RATE TO-MVR        q * cwell
! AUXILIARY                 none          none                  none
! none                      none          STORAGE (aux MASS)    
! none                      none          AUXILIARY             none
! none                      none          CONSTANT              accumulate


  
  
module GwtMwtModule

  use KindModule, only: DP, I4B
  use ConstantsModule, only: DZERO, DONE, DHALF, DEP20, LENFTYPE, LINELENGTH,  &
                             LENBOUNDNAME, NAMEDBOUNDFLAG, DNODATA,            &
                             TABLEFT, TABCENTER, TABRIGHT,                     &
                             TABSTRING, TABUCSTRING, TABINTEGER, TABREAL
  use SimModule, only: store_error, count_errors, store_error_unit, ustop
  use BndModule, only: BndType, GetBndFromList
  use GwtFmiModule, only: GwtFmiType
  use MawModule, only: MawType
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
  
  public mwt_create
  
  character(len=*), parameter :: ftype = 'MWT'
  character(len=*), parameter :: flowtype = 'MAW'
  character(len=16)       :: text  = '             MWT'
  
  type, extends(GwtAptType) :: GwtMwtType
    
    integer(I4B), pointer                              :: idxbudrate => null()  ! index of well rate terms in flowbudptr
    type (MemoryTSType), dimension(:), pointer, contiguous :: concrate => null() ! well rate concentration

  contains
  
    procedure :: bnd_da => mwt_da
    procedure :: allocate_scalars
    procedure :: apt_allocate_arrays => mwt_allocate_arrays
    procedure :: find_apt_package => find_mwt_package
    procedure :: pak_fc_expanded => mwt_fc_expanded
    procedure :: pak_solve => mwt_solve
    procedure :: pak_get_nbudterms => mwt_get_nbudterms
    procedure :: pak_setup_budobj => mwt_setup_budobj
    procedure :: pak_fill_budobj => mwt_fill_budobj
    procedure :: mwt_rate_term
    procedure :: pak_df_obs => mwt_df_obs
    procedure :: pak_bd_obs => mwt_bd_obs
    procedure :: pak_set_stressperiod => mwt_set_stressperiod
    
  end type GwtMwtType

  contains  
  
  subroutine mwt_create(packobj, id, ibcnum, inunit, iout, namemodel, pakname, &
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
    type(GwtMwtType), pointer :: mwtobj
! ------------------------------------------------------------------------------
    !
    ! -- allocate the object and assign values to object variables
    allocate(mwtobj)
    packobj => mwtobj
    !
    ! -- create name and origin
    call packobj%set_names(ibcnum, namemodel, pakname, ftype)
    packobj%text = text
    !
    ! -- allocate scalars
    call mwtobj%allocate_scalars()
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
    mwtobj%fmi => fmi
    !
    ! -- return
    return
  end subroutine mwt_create

  subroutine find_mwt_package(this)
! ******************************************************************************
! find corresponding mwt package
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryManagerModule, only: mem_allocate
    ! -- dummy
    class(GwtMwtType) :: this
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
              type is (MawType)
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
      case('RATE')
        this%idxbudrate = ip
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
  end subroutine find_mwt_package

  subroutine mwt_fc_expanded(this, rhs, ia, idxglo, amatsln)
! ******************************************************************************
! mwt_fc_expanded -- this will be called from GwtAptType%apt_fc_expanded()
!   in order to add matrix terms specifically for this package
! ****************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(GwtMwtType) :: this
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
    ! -- add puping rate contribution
    if (this%idxbudrate /= 0) then
      do j = 1, this%flowbudptr%budterm(this%idxbudrate)%nlist
        call this%mwt_rate_term(j, n1, n2, rrate, rhsval, hcofval)
        iloc = this%idxlocnode(n1)
        iposd = this%idxpakdiag(n1)
        amatsln(iposd) = amatsln(iposd) + hcofval
        rhs(iloc) = rhs(iloc) + rhsval
      end do
    end if
    !
    ! -- Return
    return
  end subroutine mwt_fc_expanded

  subroutine mwt_solve(this)
! ******************************************************************************
! mwt_solve -- add terms specific to lakes to the explicit lake solve
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(GwtMwtType) :: this
    ! -- local
    integer(I4B) :: j
    integer(I4B) :: n1, n2
    real(DP) :: rrate
! ------------------------------------------------------------------------------
    !
    ! -- add rainfall contribution
    if (this%idxbudrate /= 0) then
      do j = 1, this%flowbudptr%budterm(this%idxbudrate)%nlist
        call this%mwt_rate_term(j, n1, n2, rrate)
        this%dbuff(n1) = this%dbuff(n1) + rrate
      end do
    end if
    !
    ! -- Return
    return
  end subroutine mwt_solve
  
  function mwt_get_nbudterms(this) result(nbudterms)
! ******************************************************************************
! mwt_get_nbudterms -- function to return the number of budget terms just for
!   this package.  This overrides function in parent.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(GwtMwtType) :: this
    ! -- return
    integer(I4B) :: nbudterms
    ! -- local
! ------------------------------------------------------------------------------
    !
    ! -- Number of budget terms is 1
    nbudterms = 1
    !
    ! -- Return
    return
  end function mwt_get_nbudterms
  
  subroutine mwt_setup_budobj(this, idx)
! ******************************************************************************
! mwt_setup_budobj -- Set up the budget object that stores all the lake flows
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use ConstantsModule, only: LENBUDTXT
    ! -- dummy
    class(GwtMwtType) :: this
    integer(I4B), intent(inout) :: idx
    ! -- local
    integer(I4B) :: maxlist, naux
    character(len=LENBUDTXT) :: text
! ------------------------------------------------------------------------------
    !
    ! -- 
    text = '            RATE'
    idx = idx + 1
    maxlist = this%flowbudptr%budterm(this%idxbudrate)%maxlist
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
  end subroutine mwt_setup_budobj

  subroutine mwt_fill_budobj(this, idx, x, ccratin, ccratout)
! ******************************************************************************
! mwt_fill_budobj -- copy flow terms into this%budobj
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(GwtMwtType) :: this
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
    
    ! -- RATE
    idx = idx + 1
    nlist = this%flowbudptr%budterm(this%idxbudrate)%nlist
    call this%budobj%budterm(idx)%reset(nlist)
    do j = 1, nlist
      call this%mwt_rate_term(j, n1, n2, q)
      call this%budobj%budterm(idx)%update_term(n1, n2, q)
      call this%apt_accumulate_ccterm(n1, q, ccratin, ccratout)
    end do
    
    !
    ! -- return
    return
  end subroutine mwt_fill_budobj

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
    class(GwtMwtType) :: this
    ! -- local
! ------------------------------------------------------------------------------
    !
    ! -- allocate scalars in GwtAptType
    call this%GwtAptType%allocate_scalars()
    !
    ! -- Allocate
    call mem_allocate(this%idxbudrate, 'IDXBUDRATE', this%origin)
    ! 
    ! -- Initialize
    this%idxbudrate = 0
    !
    ! -- Return
    return
  end subroutine allocate_scalars

  subroutine mwt_allocate_arrays(this)
! ******************************************************************************
! mwt_allocate_arrays
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryManagerModule, only: mem_allocate
    ! -- dummy
    class(GwtMwtType), intent(inout) :: this
    ! -- local
! ------------------------------------------------------------------------------
    !    
    ! -- time series
    call mem_allocate(this%concrate, this%ncv, 'CONCRATE', this%origin)
    !
    ! -- call standard GwtApttype allocate arrays
    call this%GwtAptType%apt_allocate_arrays()
    !
    !
    ! -- Return
    return
  end subroutine mwt_allocate_arrays
  
  subroutine mwt_da(this)
! ******************************************************************************
! mwt_da
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryManagerModule, only: mem_deallocate
    ! -- dummy
    class(GwtMwtType) :: this
    ! -- local
! ------------------------------------------------------------------------------
    !
    ! -- deallocate scalars
    call mem_deallocate(this%idxbudrate)
    !
    ! -- deallocate time series
    call mem_deallocate(this%concrate)
    !
    ! -- deallocate scalars in GwtAptType
    call this%GwtAptType%bnd_da()
    !
    ! -- Return
    return
  end subroutine mwt_da

  subroutine mwt_rate_term(this, ientry, n1, n2, rrate, &
                           rhsval, hcofval)
! ******************************************************************************
! mwt_rate_term
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(GwtMwtType) :: this
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
    n1 = this%flowbudptr%budterm(this%idxbudrate)%id1(ientry)
    n2 = this%flowbudptr%budterm(this%idxbudrate)%id2(ientry)
    ! -- note that qbnd is negative for extracting well
    qbnd = this%flowbudptr%budterm(this%idxbudrate)%flow(ientry)
    if (qbnd < DZERO) then
      ctmp = this%xnewpak(n1)
      h = qbnd
      r = DZERO
    else
      ctmp = this%concrate(n1)%value
      h = DZERO
      r = -qbnd * ctmp
    end if
    if (present(rrate)) rrate = qbnd * ctmp
    if (present(rhsval)) rhsval = r
    if (present(hcofval)) hcofval = h
    !
    ! -- return
    return
  end subroutine mwt_rate_term
  
  subroutine mwt_df_obs(this)
! ******************************************************************************
! mwt_df_obs -- obs are supported?
!   -- Store observation type supported by APT package.
!   -- Overrides BndType%bnd_df_obs
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use GwtAptModule, only: apt_process_obsID
    ! -- dummy
    class(GwtMwtType) :: this
    ! -- local
    integer(I4B) :: indx
! ------------------------------------------------------------------------------
    !
    ! -- Store obs type and assign procedure pointer
    !    for rainfall observation type.
    call this%obs%StoreObsType('rate', .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => apt_process_obsID
    !
    return
  end subroutine mwt_df_obs
  
  subroutine mwt_bd_obs(this, obstypeid, jj, v, found)
! ******************************************************************************
! mwt_bd_obs -- calculate observation value and pass it back to APT
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(GwtMwtType), intent(inout) :: this
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
      case ('RATE')
        if (this%iboundpak(jj) /= 0) then
          call this%mwt_rate_term(jj, n1, n2, v)
        end if
      case default
        found = .false.
    end select
    !
    return
  end subroutine mwt_bd_obs

  subroutine mwt_set_stressperiod(this, itemno, itmp, line, found, &
                                  lloc, istart, istop, endtim, bndName)
! ******************************************************************************
! mwt_set_stressperiod -- Set a stress period attribute for using keywords.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use TimeSeriesManagerModule, only: read_single_value_or_time_series
    use InputOutputModule, only: urword
    ! -- dummy
    class(GwtMwtType),intent(inout) :: this
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
    ! RATE <rate>
    !
    found = .true.
    select case (line(istart:istop))
      case ('RATE')
        ierr = this%apt_check_valid(itemno)
        if (ierr /= 0) goto 999
        call urword(line, lloc, istart, istop, 0, ival, rval, this%iout, this%inunit)
        text = line(istart:istop)
        jj = 1    ! For RATE
        call read_single_value_or_time_series(text, &
                                              this%concrate(itmp)%value, &
                                              this%concrate(itmp)%name, &
                                              endtim,  &
                                              this%name, 'BND', this%TsManager, &
                                              this%iprpak, itmp, jj, 'RATE', &
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
  end subroutine mwt_set_stressperiod

end module GwtMwtModule