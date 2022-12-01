module SolutionGroupModule
  use KindModule, only: DP, I4B
  use ListsModule, only: basesolutionlist
  use BaseSolutionModule, only: BaseSolutionType, AddBaseSolutionToList, &
                                GetBaseSolutionFromList
  use ListModule, only: ListType

  implicit none
  private
  public :: SolutionGroupType, AddSolutionGroupToList, &
            GetSolutionGroupFromList, solutiongroup_create
  private :: CastAsSolutionGroupClass

  type :: SolutionGroupType
    integer(I4B), pointer :: id
    integer(I4B), pointer :: mxiter
    integer(I4B), pointer :: nsolutions
    integer(I4B), dimension(:), allocatable :: idsolutions !array of solution ids in basesolutionlist
  contains
    procedure :: sgp_ca
    procedure :: sgp_da
    procedure, private :: allocate_scalars
    procedure :: add_solution
  end type SolutionGroupType

contains

  subroutine solutiongroup_create(sgp, id)
! ******************************************************************************
! solutiongroup_create -- Create a new solution group
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    type(SolutionGroupType), pointer :: sgp
    integer(I4B), intent(in) :: id
! ------------------------------------------------------------------------------
    !
    allocate (sgp)
    call sgp%allocate_scalars()
    sgp%id = id
    !
    ! -- return
    return
  end subroutine solutiongroup_create

  subroutine sgp_ca(this)
! ******************************************************************************
! sgp_ca -- Calculate the solution group
!    Solve each solution group and each solution.  Start with converge
!    flag equal true and reset to zero if any non-convergence triggers
!    are encountered.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use ConstantsModule, only: LINELENGTH
    use SimVariablesModule, only: iout, isimcnvg, lastStepFailed
    use TdisModule, only: kstp, kper
    ! -- dummy
    class(SolutionGroupType) :: this
    ! -- local
    class(BaseSolutionType), pointer :: sp
    integer(I4B) :: kpicard, isgcnvg, isuppress_output
    integer(I4B) :: is, isoln
    ! -- formats
    character(len=*), parameter :: fmtnocnvg = &
      "(1X,'Solution Group ', i0, ' did not converge for stress period ', i0, &
       &' and time step ', i0)"
! ------------------------------------------------------------------------------
    !
    ! -- Suppress output during picard iterations
    if (this%mxiter > 1) then
      isuppress_output = 1
    else
      isuppress_output = 0
    end if
    !
    ! -- set failed flag
    lastStepFailed = 0
    !
    ! -- Picard loop
    picardloop: do kpicard = 1, this%mxiter
      if (this%mxiter > 1) then
        write (iout, '(/a,i6/)') 'SOLUTION GROUP PICARD ITERATION: ', kpicard
      end if
      isgcnvg = 1
      do is = 1, this%nsolutions
        isoln = this%idsolutions(is)
        sp => GetBaseSolutionFromList(basesolutionlist, isoln)
        call sp%sln_ca(isgcnvg, isuppress_output)
      end do
      if (isgcnvg == 1) exit picardloop
    end do picardloop
    !
    ! -- if a picard loop was used and the solution group converged
    !    then rerun the timestep and save the output.  Or if there
    !    is only one picard iteration, then do nothing as models
    !    are assumed to be explicitly coupled.
    if (isgcnvg == 1) then
      if (this%mxiter > 1) then
        isuppress_output = 0
        do is = 1, this%nsolutions
          isoln = this%idsolutions(is)
          sp => GetBaseSolutionFromList(basesolutionlist, isoln)
          call sp%sln_ca(isgcnvg, isuppress_output)
        end do
      end if
    else
      isimcnvg = 0
      lastStepFailed = 1
      write (iout, fmtnocnvg) this%id, kper, kstp
    end if
    !
    ! -- return
    return
  end subroutine sgp_ca

  subroutine sgp_da(this)
! ******************************************************************************
! deallocate
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    class(SolutionGroupType) :: this
! ------------------------------------------------------------------------------
    !
    deallocate (this%id)
    deallocate (this%mxiter)
    deallocate (this%nsolutions)
    deallocate (this%idsolutions)
    !
    ! -- return
    return
  end subroutine sgp_da

  subroutine allocate_scalars(this)
! ******************************************************************************
! allocate_scalars
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    class(SolutionGroupType) :: this
! ------------------------------------------------------------------------------
    !
    allocate (this%id)
    allocate (this%mxiter)
    allocate (this%nsolutions)
    this%id = 0
    this%mxiter = 1
    this%nsolutions = 0
    !
    ! -- return
    return
  end subroutine allocate_scalars

  subroutine add_solution(this, isoln, sp)
! ******************************************************************************
! add_solution
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use ArrayHandlersModule, only: ExpandArray
    ! -- dummy
    class(SolutionGroupType) :: this
    integer(I4B), intent(in) :: isoln
    class(BaseSolutionType), pointer, intent(in) :: sp
    ! -- local
    integer(I4B) :: ipos
! ------------------------------------------------------------------------------
    !
    call ExpandArray(this%idsolutions)
    ipos = size(this%idsolutions)
    this%idsolutions(ipos) = isoln
    this%nsolutions = this%nsolutions + 1
    !
    ! -- return
    return
  end subroutine add_solution

  function CastAsSolutionGroupClass(obj) result(res)
    implicit none
    class(*), pointer, intent(inout) :: obj
    class(SolutionGroupType), pointer :: res
    !
    res => null()
    if (.not. associated(obj)) return
    !
    select type (obj)
    class is (SolutionGroupType)
      res => obj
    end select
    return
  end function CastAsSolutionGroupClass

  subroutine AddSolutionGroupToList(list, solutiongroup)
    implicit none
    ! -- dummy
    type(ListType), intent(inout) :: list
    type(SolutionGroupType), pointer, intent(inout) :: solutiongroup
    ! -- local
    class(*), pointer :: obj
    !
    obj => solutiongroup
    call list%Add(obj)
    !
    return
  end subroutine AddSolutionGroupToList

  function GetSolutionGroupFromList(list, idx) result(res)
    implicit none
    ! -- dummy
    type(ListType), intent(inout) :: list
    integer(I4B), intent(in) :: idx
    class(SolutionGroupType), pointer :: res
    ! -- local
    class(*), pointer :: obj
    !
    obj => list%GetItem(idx)
    res => CastAsSolutionGroupClass(obj)
    !
    return
  end function GetSolutionGroupFromList

end module SolutionGroupModule
