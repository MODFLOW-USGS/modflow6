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
    
  contains
  
    procedure :: find_apt_package => find_mwt_package
    
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
      !case('RAINFALL')
      !  this%idxbudrain = ip
      !  this%idxbudssm(ip) = 0
      !case('EVAPORATION')
      !  this%idxbudevap = ip
      !  this%idxbudssm(ip) = 0
      !case('RUNOFF')
      !  this%idxbudroff = ip
      !  this%idxbudssm(ip) = 0
      !case('EXT-INFLOW')
      !  this%idxbudiflw = ip
      !  this%idxbudssm(ip) = 0
      !case('WITHDRAWAL')
      !  this%idxbudwdrl = ip
      !  this%idxbudssm(ip) = 0
      !case('EXT-OUTFLOW')
      !  this%idxbudoutf = ip
      !  this%idxbudssm(ip) = 0
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

end module GwtMwtModule