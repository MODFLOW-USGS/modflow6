! Comprehensive budget object that stores all of the 
! intercell flows, and the inflows and the outflows for a model
module BudgetObjectModule
  
  use KindModule, only: I4B, DP
  use ConstantsModule, only: LENBUDTXT
  use BudgetTermModule, only: BudgetTermType
  
  implicit none
  
  public :: BudgetObjectType
  public :: budgetobject_cr
  
  type :: BudgetObjectType
    !
    ! -- name and sizes
    character(len=LENBUDTXT) :: name
    integer(I4B) :: ncv
    integer(I4B) :: nbudterm
    !
    ! -- state variables
    real(DP), dimension(:), pointer :: xnew => null()
    real(DP), dimension(:), pointer :: xold => null()
    !
    ! -- csr flows
    integer(I4B) :: iflowja
    real(DP), dimension(:), pointer :: flowja => null()
    !
    ! -- storage
    integer(I4B) :: nsto
    real(DP), dimension(:, :), pointer :: qsto => null()
    !
    ! -- array of budget terms
    type(BudgetTermType), dimension(:), allocatable :: budterm
    
  contains
  
    procedure :: budgetobject_df
    
  end type BudgetObjectType
  
  contains

  subroutine budgetobject_cr(this, name_model)
! ******************************************************************************
! budgetobject_cr -- Create a new budget object
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    type(BudgetObjectType), pointer :: this
    character(len=*), intent(in) :: name_model
! ------------------------------------------------------------------------------
    !
    ! -- Create the object
    allocate(this)
    !
    ! -- initialize variables
    this%name = name_model
    this%ncv = 0
    this%nbudterm = 0
    this%iflowja = 0
    this%nsto = 0
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
    ! -- Return
    return
  end subroutine budgetobject_df

  
  
end module BudgetObjectModule