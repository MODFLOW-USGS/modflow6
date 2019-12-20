! Comprehensive budget object that stores all of the 
! intercell flows, and the inflows and the outflows for a model
module BudgetObjectModule
  
  use KindModule, only: I4B, DP
  use ConstantsModule, only: LENBUDTXT
  use BudgetModule, only : BudgetType, budget_cr
  use BudgetTermModule, only: BudgetTermType
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
    
  contains
  
    procedure :: budgetobject_df
    procedure :: accumulate_terms
    procedure :: write_budtable
    procedure :: save_flows
    
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
    ! -- TODO: GET DIMENSIONS IN HERE
    call this%budtable%budget_df(nbudterm, this%name)
    !
    ! -- Return
    return
  end subroutine budgetobject_df
  
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
  
end module BudgetObjectModule