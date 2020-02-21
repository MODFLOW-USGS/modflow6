! A budget term is the information needed to describe flow.
! The budget object contains an array of budget terms.  
! For an advanced package.  The budget object describes all of 
! the flows.
module BudgetTermModule

  use KindModule, only: I4B, DP
  use ConstantsModule, only: LENBUDTXT, DZERO
  use BaseDisModule, only: DisBaseType
  use InputOutputModule, only: ubdsv06

  implicit none

  public :: BudgetTermType
  
  type :: BudgetTermType
    
    character(len=LENBUDTXT) :: flowtype                           ! type of flow (WEL, DRN, ...)
    character(len=LENBUDTXT) :: text1id1                           ! model
    character(len=LENBUDTXT) :: text2id1                           ! to model
    character(len=LENBUDTXT) :: text1id2                           ! package/model
    character(len=LENBUDTXT) :: text2id2                           ! to package/model
    character(len=LENBUDTXT), dimension(:), pointer :: auxtxt => null()     ! name of auxiliary variables
    integer(I4B) :: maxlist                                        ! allocated size of arrays
    integer(I4B) :: naux                                           ! number of auxiliary variables
    integer(I4B) :: nlist                                          ! size of arrays for this period
    logical :: olconv1 = .false.                                   ! convert id1 to user node upon output
    logical :: olconv2 = .false.                                   ! convert id2 to user node upon output
    integer(I4B), dimension(:), pointer :: id1 => null()           ! first id (maxlist)
    integer(I4B), dimension(:), pointer :: id2 => null()           ! second id (maxlist)
    real(DP), dimension(:), pointer :: flow => null()              ! point this to simvals or simtomvr (maxlist)
    real(DP), dimension(:, :), pointer :: auxvar => null()         ! auxiliary variables (naux, maxlist)
    integer(I4B) :: icounter                                       ! counter variable
  
  contains
  
    procedure :: initialize
    procedure :: allocate_arrays
    procedure :: reset
    procedure :: update_term
    procedure :: accumulate_flow
    procedure :: save_flows
    procedure :: read_flows
    procedure :: fill_from_bfr
    procedure :: deallocate_arrays
    
  end type BudgetTermType

  contains
  
  subroutine initialize(this, flowtype, text1id1, text2id1, &
                        text1id2, text2id2, maxlist, olconv1, olconv2, &
                        naux, auxtxt)
! ******************************************************************************
! initialize -- initialize the budget term
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(BudgetTermType) :: this
    character(len=LENBUDTXT), intent(in) :: flowtype
    character(len=LENBUDTXT), intent(in) :: text1id1
    character(len=LENBUDTXT), intent(in) :: text2id1
    character(len=LENBUDTXT), intent(in) :: text1id2
    character(len=LENBUDTXT), intent(in) :: text2id2
    integer(I4B), intent(in) :: maxlist
    logical, intent(in) :: olconv1
    logical, intent(in) :: olconv2
    integer(I4B), intent(in) :: naux
    character(len=LENBUDTXT), dimension(:), intent(in), optional :: auxtxt
    ! -- local
! ------------------------------------------------------------------------------
    this%flowtype = flowtype
    this%text1id1 = text1id1
    this%text2id1 = text2id1
    this%text1id2 = text1id2
    this%text2id2 = text2id2
    this%maxlist = maxlist
    this%olconv1 = olconv1
    this%olconv2 = olconv2
    this%naux = naux
    this%nlist = 0
    call this%allocate_arrays()
    if (present(auxtxt)) this%auxtxt(:) = auxtxt(1:naux)
  end subroutine initialize
  
  subroutine allocate_arrays(this)
! ******************************************************************************
! allocate_arrays -- allocate budget term arrays
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(BudgetTermType) :: this
! ------------------------------------------------------------------------------
    allocate(this%id1(this%maxlist))
    allocate(this%id2(this%maxlist))
    allocate(this%flow(this%maxlist))
    allocate(this%auxvar(this%naux, this%maxlist))
    allocate(this%auxtxt(this%naux))
  end subroutine allocate_arrays
  
  subroutine deallocate_arrays(this)
! ******************************************************************************
! deallocate_arrays -- deallocate budget term arrays
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(BudgetTermType) :: this
! ------------------------------------------------------------------------------
    deallocate(this%id1)
    deallocate(this%id2)
    deallocate(this%flow)
    deallocate(this%auxvar)
    deallocate(this%auxtxt)
  end subroutine deallocate_arrays
  
  subroutine reset(this, nlist)
! ******************************************************************************
! reset -- reset the budget term and counter so terms can be updated
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(BudgetTermType) :: this
    integer(I4B), intent(in) :: nlist
! ------------------------------------------------------------------------------
    this%nlist = nlist
    this%icounter = 1
  end subroutine reset
  
  subroutine update_term(this, id1, id2, flow, auxvar)
! ******************************************************************************
! update_term -- replace the terms in position this%icounter 
!   for id1, id2, flow, and aux
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(BudgetTermType) :: this
    integer(I4B), intent(in) :: id1
    integer(I4B), intent(in) :: id2
    real(DP), intent(in) :: flow
    real(DP), dimension(:), intent(in), optional :: auxvar
! ------------------------------------------------------------------------------
    this%id1(this%icounter) = id1
    this%id2(this%icounter) = id2
    this%flow(this%icounter) = flow
    if (present(auxvar)) this%auxvar(:, this%icounter) = auxvar(1:this%naux)
    this%icounter = this%icounter + 1
  end subroutine update_term
  
  subroutine accumulate_flow(this, ratin, ratout)
! ******************************************************************************
! accumulate_flow -- calculate ratin and ratout for all the flow terms
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(BudgetTermType) :: this
    real(DP), intent(inout) :: ratin
    real(DP), intent(inout) :: ratout
    ! -- local
    integer(I4B) :: i
    real(DP) :: q
! ------------------------------------------------------------------------------
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
  
  subroutine save_flows(this, dis, ibinun, kstp, kper, delt, pertim, totim, &
                        iout)
! ******************************************************************************
! save_flows -- write flows to a binary file
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(BudgetTermType) :: this
    class(DisBaseType), intent(in) :: dis
    integer(I4B), intent(in) :: ibinun
    integer(I4B), intent(in) :: kstp
    integer(I4B), intent(in) :: kper
    real(DP), intent(in) :: delt
    real(DP), intent(in) :: pertim
    real(DP), intent(in) :: totim
    integer(I4B), intent(in) :: iout
    ! -- local
    integer(I4B) :: i
    integer(I4B) :: n1
    integer(I4B) :: n2
    real(DP) :: q
! ------------------------------------------------------------------------------
    call ubdsv06(kstp, kper, this%flowtype, &
                 this%text1id1, this%text2id1, &
                 this%text1id2, this%text2id2, &
                 ibinun, this%naux, this%auxtxt, &
                 this%nlist, 1, 1, this%nlist, &
                 iout, delt, pertim, totim)
    do i = 1, this%nlist
      q = this%flow(i)
      n1 = this%id1(i)
      n2 = this%id2(i)
      call dis%record_mf6_list_entry(ibinun, n1, n2, q, &
                                     this%naux, this%auxvar(:, i), &
                                     olconv=this%olconv1, &
                                     olconv2=this%olconv2)
    end do
  end subroutine save_flows
  
  subroutine read_flows(this, dis, ibinun, kstp, kper, delt, pertim, totim)
! ******************************************************************************
! read_flows -- read flows from a binary file
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(BudgetTermType) :: this
    class(DisBaseType), intent(in) :: dis
    integer(I4B), intent(in) :: ibinun
    integer(I4B), intent(inout) :: kstp
    integer(I4B), intent(inout) :: kper
    real(DP), intent(inout) :: delt
    real(DP), intent(inout) :: pertim
    real(DP), intent(inout) :: totim
    ! -- local
    integer(I4B) :: idum1, idum2, imeth
    integer(I4B) :: i, j
    integer(I4B) :: n1
    integer(I4B) :: n2
    real(DP) :: q
! ------------------------------------------------------------------------------
    read(ibinun) kstp, kper, this%flowtype, this%nlist, idum1, idum2
    read(ibinun) imeth, delt, pertim, totim
    read(ibinun) this%text1id1
    read(ibinun) this%text2id1
    read(ibinun) this%text1id2
    read(ibinun) this%text2id2
    read(ibinun) this%naux
    this%naux = this%naux - 1
    if (.not. associated(this%auxtxt)) then
      allocate(this%auxtxt(this%naux))
    else
      if (size(this%auxtxt) /= this%naux) then
        deallocate(this%auxtxt)
        allocate(this%auxtxt(this%naux))
      end if
    end if
    if (this%naux > 0) read(ibinun) (this%auxtxt(j), j = 1, this%naux)
    read(ibinun) this%nlist
    if (.not. associated(this%id1)) then
      this%maxlist = this%nlist
      allocate(this%id1(this%maxlist))
      allocate(this%id2(this%maxlist))
      allocate(this%flow(this%maxlist))
      allocate(this%auxvar(this%naux, this%maxlist))
    else
      if (this%nlist > this%maxlist) then
        this%maxlist = this%nlist
        deallocate(this%id1)
        deallocate(this%id2)
        deallocate(this%flow)
        deallocate(this%auxvar)
        allocate(this%id1(this%maxlist))
        allocate(this%id2(this%maxlist))
        allocate(this%flow(this%maxlist))
        allocate(this%auxvar(this%naux, this%maxlist))
      end if
    end if
    do i = 1, this%nlist
      read(ibinun) n1
      read(ibinun) n2
      read(ibinun) q
      read(ibinun) (this%auxvar(j, i), j = 1, this%naux)
      if (this%olconv1) n1 = dis%get_nodenumber(n1, 0)
      if (this%olconv2) n2 = dis%get_nodenumber(n2, 0)
      this%id1(i) = n1
      this%id2(i) = n2
      this%flow(i) = q
    end do
  end subroutine read_flows
  
  subroutine fill_from_bfr(this, bfr, dis)
! ******************************************************************************
! fill_from_bfr -- copy the flow from the binary file reader into this budterm
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use BudgetFileReaderModule, only: BudgetFileReaderType
    ! -- dummy
    class(BudgetTermType) :: this
    type(BudgetFileReaderType) :: bfr
    class(DisBaseType), intent(in) :: dis
    !integer(I4B), intent(in) :: ibinun
    !integer(I4B), intent(inout) :: kstp
    !integer(I4B), intent(inout) :: kper
    !real(DP), intent(inout) :: delt
    !real(DP), intent(inout) :: pertim
    !real(DP), intent(inout) :: totim
    ! -- local
    integer(I4B) :: i
    integer(I4B) :: n1
    integer(I4B) :: n2
    real(DP) :: q
! ------------------------------------------------------------------------------
    this%flowtype = bfr%budtxt
    this%text1id1 = bfr%srcmodelname
    this%text2id1 = bfr%srcpackagename
    this%text1id2 = bfr%dstmodelname
    this%text2id2 = bfr%dstpackagename
    this%naux = bfr%naux
    if (.not. associated(this%auxtxt)) then
      allocate(this%auxtxt(this%naux))
    else
      if (size(this%auxtxt) /= this%naux) then
        deallocate(this%auxtxt)
        allocate(this%auxtxt(this%naux))
      end if
    end if
    if (this%naux > 0) this%auxtxt(:) = bfr%auxtxt(:)
    this%nlist = bfr%nlist
    if (.not. associated(this%id1)) then
      this%maxlist = this%nlist
      allocate(this%id1(this%maxlist))
      allocate(this%id2(this%maxlist))
      allocate(this%flow(this%maxlist))
      allocate(this%auxvar(this%naux, this%maxlist))
    else
      if (this%nlist > this%maxlist) then
        this%maxlist = this%nlist
        deallocate(this%id1)
        deallocate(this%id2)
        deallocate(this%flow)
        deallocate(this%auxvar)
        allocate(this%id1(this%maxlist))
        allocate(this%id2(this%maxlist))
        allocate(this%flow(this%maxlist))
        allocate(this%auxvar(this%naux, this%maxlist))
      end if
    end if
    do i = 1, this%nlist
      n1 = bfr%nodesrc(i)
      n2 = bfr%nodedst(i)
      q = bfr%flow(i)
      this%auxvar(:, i) = bfr%auxvar(:, i)
      if (this%olconv1) n1 = dis%get_nodenumber(n1, 0)
      if (this%olconv2) n2 = dis%get_nodenumber(n2, 0)
      this%id1(i) = n1
      this%id2(i) = n2
      this%flow(i) = q
    end do
  end subroutine fill_from_bfr
  
end module BudgetTermModule