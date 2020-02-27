! Comprehensive budget object that stores all of the 
! intercell flows, and the inflows and the outflows for 
! an advanced package.
module BudgetObjectModule
  
  use KindModule, only: I4B, DP
  use ConstantsModule, only: LENBUDTXT, LINELENGTH,                              &      
                             TABLEFT, TABCENTER, TABRIGHT,                       &
                             TABSTRING, TABUCSTRING, TABINTEGER, TABREAL,        &
                             DZERO, DHALF, DHUNDRED
  use BudgetModule, only : BudgetType, budget_cr
  use BudgetTermModule, only: BudgetTermType
  use TableModule, only: TableType, table_cr
  use BaseDisModule, only: DisBaseType
  
  implicit none
  
  public :: BudgetObjectType
  public :: budgetobject_cr
  
  type :: BudgetObjectType
    !
    ! -- name, number of control volumes, and number of budget terms
    character(len=LENBUDTXT) :: name
    integer(I4B) :: ncv
    integer(I4B) :: nbudterm
    !
    ! -- state variables
    real(DP), dimension(:), pointer :: xnew => null()
    real(DP), dimension(:), pointer :: xold => null()
    !
    ! -- csr intercell flows
    integer(I4B) :: iflowja
    real(DP), dimension(:), pointer :: flowja => null()
    !
    ! -- storage
    integer(I4B) :: nsto
    real(DP), dimension(:, :), pointer :: qsto => null()
    !
    ! -- array of budget terms, with one separate entry for each term
    !    such as rainfall, et, leakage, etc.
    integer(I4B) :: iterm
    type(BudgetTermType), dimension(:), allocatable :: budterm
    !
    ! -- budget table object, for writing the typical MODFLOW budget
    type(BudgetType), pointer :: budtable => null()
    !
    ! -- flow table object, for writing the flow budget for 
    !    each control volume
    logical, pointer :: add_cellids => null()
    integer(I4B), pointer :: icellid => null()
    integer(I4B), pointer :: nflowterms => null()
    integer(I4B), dimension(:), pointer :: istart => null()
    integer(I4B), dimension(:), pointer :: iflowterms => null()
    type(TableType), pointer :: flowtab => null()    
    
  contains
  
    procedure :: budgetobject_df
    procedure :: flowtable_df
    procedure :: accumulate_terms
    procedure :: write_budtable
    procedure :: write_flowtable
    procedure :: save_flows
    procedure :: budgetobject_da
    
  end type BudgetObjectType
  
  contains

  subroutine budgetobject_cr(this, name)
! ******************************************************************************
! budgetobject_cr -- Create a new budget object
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    type(BudgetObjectType), pointer :: this
    character(len=*), intent(in) :: name
! ------------------------------------------------------------------------------
    !
    ! -- Create the object
    allocate(this)
    !
    ! -- initialize variables
    this%name = name
    this%ncv = 0
    this%nbudterm = 0
    this%iflowja = 0
    this%nsto = 0
    this%iterm = 0
    !
    ! -- initialize budget table
    call budget_cr(this%budtable, name)
    !
    ! -- Return
    return
  end subroutine budgetobject_cr

  subroutine budgetobject_df(this, ncv, nbudterm, iflowja, nsto)
! ******************************************************************************
! budgetobject_df -- Define the new budget object
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(BudgetObjectType) :: this
    integer(I4B), intent(in) :: ncv
    integer(I4B), intent(in) :: nbudterm
    integer(I4B), intent(in) :: iflowja
    integer(I4B), intent(in) :: nsto
! ------------------------------------------------------------------------------
    !
    ! -- set values
    this%ncv = ncv
    this%nbudterm = nbudterm
    this%iflowja = iflowja
    this%nsto = nsto
    !
    ! -- allocate space for budterm
    allocate(this%budterm(nbudterm))
    !
    ! -- setup the budget table object
    ! -- TODO: GET DIMENSIONS (e.g. L**3) IN HERE
    call this%budtable%budget_df(nbudterm, this%name)
    !
    ! -- Return
    return
  end subroutine budgetobject_df
 
  subroutine flowtable_df(this, iout, cellids)
! ******************************************************************************
! flowtable_df -- Define the new flow table object
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(BudgetObjectType) :: this
    integer(I4B), intent(in) :: iout
    character(len=*), intent(in), optional :: cellids
    ! -- local
    character(len=LINELENGTH) :: title
    character(len=LINELENGTH) :: text
    character(len=LENBUDTXT) :: flowtype
    character(len=LENBUDTXT) :: tag
    character(len=LENBUDTXT) :: coupletype
    logical :: lfound
    logical :: add_cellids
    integer(I4B) :: maxcol
    integer(I4B) :: idx
    integer(I4B) :: ipos
    integer(I4B) :: i
! ------------------------------------------------------------------------------
    !
    ! -- process optional variables
    if (present(cellids)) then
      add_cellids = .TRUE.
      coupletype = cellids
    else
      add_cellids = .FALSE.
    end if
    !
    ! -- allocate scalars
    allocate(this%add_cellids)
    allocate(this%icellid)
    allocate(this%nflowterms)
    !
    ! -- initialize scalars
    this%add_cellids = add_cellids
    this%nflowterms = 0
    this%icellid = 0
    !
    ! -- determine the number of columns in the table
    maxcol = 3
    if (add_cellids) then
      maxcol = maxcol + 1
    end if
    do i = 1, this%nbudterm
      lfound = .FALSE.
      flowtype = this%budterm(i)%get_flowtype()
      if (trim(adjustl(flowtype)) == 'FLOW-JA-FACE') then
        lfound = .TRUE.
        maxcol = maxcol + 2
      else if (trim(adjustl(flowtype)) /= 'AUXILIARY') then
        lfound = .TRUE.
        maxcol = maxcol + 1
      end if
      if (lfound) then
        this%nflowterms = this%nflowterms + 1
        if (add_cellids) then
          if (trim(adjustl(flowtype)) == trim(adjustl(coupletype))) then
            this%icellid = i
          end if
        end if
      end if
    end do
    !
    ! -- allocate arrays
    allocate(this%istart(this%nflowterms))
    allocate(this%iflowterms(this%nflowterms))
    !
    ! -- set up flow tableobj
    title = trim(this%name) // ' PACKAGE - SUMMARY OF FLOWS FOR ' //             &
            'EACH CONTROL VOLUME'
    call table_cr(this%flowtab, this%name, title)
    call this%flowtab%table_df(this%ncv, maxcol, iout, transient=.TRUE.)
    !
    ! -- Go through and set up flow table budget terms
    text = 'NUMBER'
    call this%flowtab%initialize_column(text, 10, alignment=TABCENTER)
    if (add_cellids) then
      text = 'CELLID'
      call this%flowtab%initialize_column(text, 20, alignment=TABLEFT)
    end if
    idx = 1
    do i = 1, this%nbudterm
      lfound = .FALSE.
      flowtype = this%budterm(i)%get_flowtype()
      tag = trim(adjustl(flowtype))
      ipos = index(tag, '-')
      if (ipos > 0) then
        tag(ipos:ipos) = ' '
      end if
      if (trim(adjustl(flowtype)) == 'FLOW-JA-FACE') then
        lfound = .TRUE.
        text = 'INFLOW'
        call this%flowtab%initialize_column(text, 12, alignment=TABCENTER)
        text = 'OUTFLOW'
        call this%flowtab%initialize_column(text, 12, alignment=TABCENTER)
      else if (trim(adjustl(flowtype)) /= 'AUXILIARY') then
        lfound = .TRUE.
        call this%flowtab%initialize_column(tag, 12, alignment=TABCENTER)
      end if
      if (lfound) then
        this%iflowterms(idx) = i
        idx = idx + 1
      end if
    end do
    text = 'IN - OUT'
    call this%flowtab%initialize_column(text, 12, alignment=TABCENTER)
    text = 'PERCENT DIFFERENCE'
    call this%flowtab%initialize_column(text, 12, alignment=TABCENTER)
    !
    ! -- Return
    return
  end subroutine flowtable_df
  
  subroutine accumulate_terms(this)
! ******************************************************************************
! accumulate_terms -- add up accumulators and submit to budget table
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use TdisModule, only: delt
    ! -- dummy
    class(BudgetObjectType) :: this
    ! -- dummy
    character(len=LENBUDTXT) :: flowtype    
    integer(I4B) :: i
    real(DP) :: ratin, ratout
! ------------------------------------------------------------------------------
    !
    ! -- reset the budget table
    call this%budtable%reset()
    !
    ! -- calculate the budget table terms
    do i = 1, this%nbudterm
      !
      ! -- accumulate positive and negative flows for each budget term
      flowtype = this%budterm(i)%flowtype
      select case (trim(adjustl(flowtype)))
      case ('FLOW-JA-FACE')
        ! skip
      case default
        !
        ! -- calculate sum of positive and negative flows
        call this%budterm(i)%accumulate_flow(ratin, ratout)
        !
        ! -- pass accumulators into the budget table
        call this%budtable%addentry(ratin, ratout, delt, flowtype)
      end select
    end do
    !
    ! -- return
    return
  end subroutine accumulate_terms

  subroutine write_flowtable(this, dis)
! ******************************************************************************
! write_flowtable -- Write the flow table for each advanced package control
!                    volume  
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(BudgetObjectType) :: this
    class(DisBaseType), intent(in) :: dis
    ! -- dummy
    character(len=LENBUDTXT) :: flowtype
    character(len=20) :: cellid
    integer(I4B) :: nlist
    integer(I4B) :: id1
    integer(I4B) :: id2
    integer(I4B) :: icv
    integer(I4B) :: idx
    integer(I4B) :: i
    integer(I4B) :: j
    real(DP) :: v
    real(DP) :: qin
    real(DP) :: qout
    real(DP) :: q
    real(DP) :: qinflow
    real(DP) :: qoutflow
    real(DP) :: qerr
    real(DP) :: qavg
    real(DP) :: qpd
    
! ------------------------------------------------------------------------------
    !
    ! -- reset starting position
    do j = 1, this%nflowterms
      this%istart(j) = 1
    end do
    !
    ! -- write the table
    do icv = 1, this%ncv
      call this%flowtab%add_term(icv)
      !
      ! -- initialize flow terms for the control volume
      qin = DZERO
      qout = DZERO
      !
      ! -- add cellid if required
      if (this%add_cellids) then
        j = this%icellid 
        idx = this%iflowterms(j)
        i = this%istart(j)
        id2 = this%budterm(idx)%get_id2(i)
        if (id2 > 0) then
          call dis%noder_to_string(id2, cellid)
        else
          cellid = 'NONE'
        end if
        call this%flowtab%add_term(cellid)
      end if
      !
      ! -- iterate over the flow terms
      do j = 1, this%nflowterms
        !
        ! -- initialize flow terms for the row
        q = DZERO
        qinflow = DZERO
        qoutflow = DZERO
        !
        ! -- determine the index, flowtype and length of  
        !    the flowterm
        idx = this%iflowterms(j)
        flowtype = this%budterm(idx)%get_flowtype()
        nlist = this%budterm(idx)%get_nlist()
        !
        ! -- iterate over the enteries in the flowtype
        colterm: do i = this%istart(j), nlist
          id1 = this%budterm(idx)%get_id1(i)
          if(id1 > icv) then
            this%istart(j) = i
            exit colterm
          end if
          v = this%budterm(idx)%get_flow(i)
          if (trim(adjustl(flowtype)) == 'FLOW-JA-FACE') then
            if (v < DZERO) then
              qoutflow = qoutflow + v
            else
              qinflow = qinflow + v
            end if
          end if
          !
          ! -- accumulators
          q = q + v
          if (v < DZERO) then
            qout = qout + v
          else
            qin = qin + v
          end if
        end do colterm
        !
        ! -- add entry to table
        if (trim(adjustl(flowtype)) == 'FLOW-JA-FACE') then
          call this%flowtab%add_term(qinflow)
          call this%flowtab%add_term(qoutflow)
        else
          call this%flowtab%add_term(q)
        end if
      end do
      !
      ! -- calculate in-out and percent difference
      qerr = qin + qout
      qavg = DHALF * (qin - qout)
      qpd = DZERO
      if (qavg > DZERO) then
        qpd = DHUNDRED * qerr / qavg
      end if
      call this%flowtab%add_term(qerr)
      call this%flowtab%add_term(qpd)
    end do
    !
    ! -- return
    return
  end subroutine write_flowtable

  subroutine write_budtable(this, kstp, kper, iout)
! ******************************************************************************
! write_budtable -- Write the budget table
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(BudgetObjectType) :: this
    integer(I4B),intent(in) :: kstp
    integer(I4B),intent(in) :: kper
    integer(I4B),intent(in) :: iout
    ! -- dummy
! ------------------------------------------------------------------------------
    !
    ! -- write the table
    call this%budtable%budget_ot(kstp, kper, iout)
    !
    ! -- return
    return
  end subroutine write_budtable
  
  subroutine save_flows(this, dis, ibinun, kstp, kper, delt, &
                        pertim, totim, iout)
! ******************************************************************************
! write_budtable -- Write the budget table
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(BudgetObjectType) :: this
    class(DisBaseType), intent(in) :: dis
    integer(I4B), intent(in) :: ibinun
    integer(I4B), intent(in) :: kstp
    integer(I4B), intent(in) :: kper
    real(DP), intent(in) :: delt
    real(DP), intent(in) :: pertim
    real(DP), intent(in) :: totim
    integer(I4B), intent(in) :: iout
    ! -- dummy
    integer(I4B) :: i
! ------------------------------------------------------------------------------
    !
    ! -- save flows for each budget term
    do i = 1, this%nbudterm
      call this%budterm(i)%save_flows(dis, ibinun, kstp, kper, delt, &
                                      pertim, totim, iout)
    end do
    !
    ! -- return
    return
  end subroutine save_flows
  
  subroutine budgetobject_da(this)
! ******************************************************************************
! budgetobject_da -- deallocate
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(BudgetObjectType) :: this
    ! -- dummy
    integer(I4B) :: i
! ------------------------------------------------------------------------------
    !
    ! -- save flows for each budget term
    do i = 1, this%nbudterm
      call this%budterm(i)%deallocate_arrays()
    end do
    !
    ! --
    if (associated(this%flowtab)) then
      deallocate(this%add_cellids)
      deallocate(this%icellid)
      deallocate(this%nflowterms)
      deallocate(this%istart)
      deallocate(this%iflowterms)
      call this%flowtab%table_da()
      deallocate(this%flowtab)
      nullify(this%flowtab)
    end if
    !
    ! -- Return
    return
  end subroutine budgetobject_da
  
end module BudgetObjectModule