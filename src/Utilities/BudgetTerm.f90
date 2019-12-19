
module BudgetTermModule

  use KindModule, only: I4B, DP
  use ConstantsModule, only: LENBUDTXT, DZERO

  implicit none

  public :: BudgetTermType
  
  type :: BudgetTermType
    
    character(len=LENBUDTXT) :: flowtype                           ! type of flow (WEL, DRN, ...)
    character(len=LENBUDTXT) :: text1id1                           ! model
    character(len=LENBUDTXT) :: text2id1                           ! to model
    character(len=LENBUDTXT) :: text1id2                           ! package/model
    character(len=LENBUDTXT) :: text2id2                           ! to package/model
    integer(I4B) :: maxlist                                        ! allocated size of arrays
    integer(I4B) :: nlist                                          ! size of arrays for this period
    integer(I4B) :: naux                                           ! number of auxiliary variables
    integer(I4B), dimension(:), pointer :: id1 => null()           ! first id (maxlist)
    integer(I4B), dimension(:), pointer :: id2 => null()           ! second id (maxlist)
    real(DP), dimension(:), pointer :: flow => null()              ! point this to simvals or simtomvr (maxlist)
    real(DP), dimension(:, :), pointer :: auxvar => null()         ! auxiliary variables (naux, maxlist)
    character(len=LENBUDTXT), dimension(:), pointer :: auxtxt      ! name of auxiliary variables
    
  contains
  
    procedure :: initialize
    procedure :: allocate_arrays
    procedure :: accumulate_flow
    procedure :: printme
    
  end type BudgetTermType

  contains
  
  subroutine initialize(this, flowtype, text1id1, text2id1, &
                        text1id2, text2id2, maxlist, naux)
    class(BudgetTermType) :: this
    character(len=LENBUDTXT), intent(in) :: flowtype
    character(len=LENBUDTXT), intent(in) :: text1id1
    character(len=LENBUDTXT), intent(in) :: text2id1
    character(len=LENBUDTXT), intent(in) :: text1id2
    character(len=LENBUDTXT), intent(in) :: text2id2
    integer(I4B), intent(in) :: maxlist
    integer(I4B), intent(in) :: naux
    this%flowtype = flowtype
    this%text1id1 = text1id1
    this%text2id1 = text2id1
    this%text1id2 = text1id2
    this%text2id2 = text2id2
    this%maxlist = maxlist
    this%naux = naux
    this%nlist = maxlist
    call this%allocate_arrays()
  end subroutine initialize
  
  subroutine allocate_arrays(this)
    class(BudgetTermType) :: this
    allocate(this%id1(this%maxlist))
    allocate(this%id2(this%maxlist))
    allocate(this%flow(this%maxlist))
    allocate(this%auxvar(this%naux, this%maxlist))
    allocate(this%auxtxt(this%naux))
  end subroutine allocate_arrays
  
  subroutine accumulate_flow(this, ratin, ratout)
    class(BudgetTermType) :: this
    real(DP), intent(inout) :: ratin
    real(DP), intent(inout) :: ratout
    integer(I4B) :: i
    real(DP) :: q
    ratin = DZERO
    ratout = DZERO
    do i = 1, this%nlist
      q = this%flow(i)
      if (q < DZERO) then
        ratout = ratout - q
      else
        ratin = ratin + q
      end if
    end do
  end subroutine accumulate_flow
  
  subroutine printme(this)
    class(BudgetTermType) :: this
    integer(I4B) :: i
    print *, this%flowtype
    do i = 1, this%nlist
      print *, i, this%id1(i), this%id2(i), this%flow(i)
    end do
  end subroutine printme
  
end module BudgetTermModule