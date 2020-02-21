! Comprehensive budget object that stores all of the 
! intercell flows, and the inflows and the outflows for 
! an advanced package.
module BudgetObjectModule
  
  use KindModule, only: I4B, DP
  use ConstantsModule, only: LENBUDTXT
  use BudgetModule, only : BudgetType, budget_cr
  use BudgetTermModule, only: BudgetTermType
  use BaseDisModule, only: DisBaseType
  use BudgetFileReaderModule, only: BudgetFileReaderType
  
  implicit none
  
  public :: BudgetObjectType
  public :: budgetobject_cr
  public :: budgetobject_cr_bfr
  
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
    ! -- budget file reader, for reading flows from a binary file
    type(BudgetFileReaderType), pointer :: bfr => null()
    
  contains
  
    procedure :: budgetobject_df
    procedure :: accumulate_terms
    procedure :: write_budtable
    procedure :: save_flows
    procedure :: read_flows
    procedure :: budgetobject_da
    procedure :: bfr_init
    procedure :: fill_from_bfr
    
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
  
  subroutine read_flows(this, dis, ibinun)
! ******************************************************************************
! read_flows -- Read froms from a binary file into this BudgetObjectType
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(BudgetObjectType) :: this
    class(DisBaseType), intent(in) :: dis
    integer(I4B), intent(in) :: ibinun
    ! -- local
    integer(I4B) :: kstp
    integer(I4B) :: kper
    real(DP) :: delt
    real(DP) :: pertim
    real(DP) :: totim
    ! -- dummy
    integer(I4B) :: i
! ------------------------------------------------------------------------------
    !
    ! -- read flows for each budget term
    do i = 1, this%nbudterm
      call this%budterm(i)%read_flows(dis, ibinun, kstp, kper, delt, &
                                      pertim, totim)
    end do
    !
    ! -- return
    return
  end subroutine read_flows
  
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
    ! -- Return
    return
  end subroutine budgetobject_da
  
  subroutine budgetobject_cr_bfr(this, name, ibinun, iout, colconv1, colconv2)
! ******************************************************************************
! budgetobject_cr_bfr -- Create a new budget object from a binary flow file
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    type(BudgetObjectType), pointer :: this
    character(len=*), intent(in) :: name
    integer(I4B), intent(in) :: ibinun
    integer(I4B), intent(in) :: iout
    character(len=16), dimension(:), optional :: colconv1
    character(len=16), dimension(:), optional :: colconv2
    ! -- local
    integer(I4B) :: ncv, nbudterm
    integer(I4B) :: iflowja, nsto
    integer(I4B) :: i, j
! ------------------------------------------------------------------------------
    !
    ! -- Create the object
    call budgetobject_cr(this, name)
    !
    ! -- Initialize the budget file reader
    call this%bfr_init(ibinun, ncv, nbudterm, iout)
    !
    ! -- Define this budget object using number of control volumes and number
    !    of budget terms read from ibinun
    iflowja = 0
    nsto = 0
    call this%budgetobject_df(ncv, nbudterm, iflowja, nsto)
    !
    ! -- Set the conversion flags, which cause id1 or id2 to be converted from
    !    user node numbers to reduced node numbers
    do i = 1, nbudterm
      if (present(colconv1)) then
        do j = 1, size(colconv1)
          if (colconv1(j) == this%bfr%budtxtarray(i)) then
            this%budterm(i)%olconv1 = .true.
          end if
        end do
      end if
      if (present(colconv2)) then
        do j = 1, size(colconv2)
          if (colconv2(j) == this%bfr%budtxtarray(i)) then
            this%budterm(i)%olconv2 = .true.
          end if
        end do
      end if
    end do
    !
    ! -- Return
    return
  end subroutine budgetobject_cr_bfr
  
  subroutine bfr_init(this, ibinun, ncv, nbudterm, iout)
! ******************************************************************************
! bfr_init -- initialize the budget file reader
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(BudgetObjectType) :: this
    integer(I4B), intent(in) :: ibinun
    integer(I4B), intent(inout) :: ncv
    integer(I4B), intent(inout) :: nbudterm
    integer(I4B), intent(in) :: iout
    ! -- local
! ------------------------------------------------------------------------------
    !
    ! -- initialize budget file reader
    allocate(this%bfr)
    call this%bfr%initialize(ibinun, iout, ncv)
    nbudterm = this%bfr%nbudterms
    !
    ! -- Return
    return
  end subroutine bfr_init
  
  subroutine fill_from_bfr(this, dis, iout)
! ******************************************************************************
! fill_from_bfr -- copy the information from the binary file into budterms
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(BudgetObjectType) :: this
    class(DisBaseType), intent(in) :: dis
    integer(I4B), intent(in) :: iout
    ! -- dummy
    integer(I4B) :: i
    logical :: success
! ------------------------------------------------------------------------------
    !
    ! -- read flows from the binary file and copy them into this%budterm(:)
    do i = 1, this%nbudterm
      call this%bfr%read_record(success, iout)
      call this%budterm(i)%fill_from_bfr(this%bfr, dis)
    end do
    !
    ! -- Return
    return
  end subroutine fill_from_bfr
  
end module BudgetObjectModule