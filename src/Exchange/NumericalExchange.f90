module NumericalExchangeModule

  use KindModule, only: DP, I4B
  use BaseModelModule, only: BaseModelType
  use BaseExchangeModule, only: BaseExchangeType, AddBaseExchangeToList
  use NumericalModelModule, only: NumericalModelType
  use ListModule, only: ListType

  implicit none

  private
  public :: NumericalExchangeType, &
            AddNumericalExchangeToList, GetNumericalExchangeFromList

  type, extends(BaseExchangeType) :: NumericalExchangeType
    character(len=7) :: typename !< name of the type (e.g., 'GWF-GWF')
  contains
    procedure :: exg_df
    procedure :: exg_ac
    procedure :: exg_mc
    procedure :: exg_ar
    !procedure :: exg_rp (not needed yet; base exg_rp does nothing)
    procedure :: exg_ad
    procedure :: exg_cf
    procedure :: exg_fc
    procedure :: exg_nr
    procedure :: exg_cc
    procedure :: exg_cq
    procedure :: exg_bd
    procedure :: exg_ot
    procedure :: exg_da
    procedure :: get_iasym
  end type NumericalExchangeType

contains

  subroutine exg_df(this)
! ******************************************************************************
! exg_df -- define the exchange
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use BaseModelModule, only: BaseModelType
    use InputOutputModule, only: getunit, openfile
    ! -- dummy
    class(NumericalExchangeType) :: this
    ! -- local
! ------------------------------------------------------------------------------
    !
    ! -- return
    return
  end subroutine exg_df

  subroutine exg_ac(this, sparse)
! ******************************************************************************
! exg_ac -- If an implicit exchange then add connections to sparse
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use SparseModule, only: sparsematrix
    ! -- dummy
    class(NumericalExchangeType) :: this
    type(sparsematrix), intent(inout) :: sparse
    ! -- local
! ------------------------------------------------------------------------------
    !
    ! -- return
    return
  end subroutine exg_ac

  subroutine exg_mc(this, iasln, jasln)
! ******************************************************************************
! exg_mc -- Map the connections in the global matrix
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- module
    use SparseModule, only: sparsematrix
    ! -- dummy
    class(NumericalExchangeType) :: this
    integer(I4B), dimension(:), intent(in) :: iasln
    integer(I4B), dimension(:), intent(in) :: jasln
    ! -- local
! ------------------------------------------------------------------------------
    !
    ! -- return
  end subroutine exg_mc

  subroutine exg_ar(this)
! ******************************************************************************
! exg_ar -- Allocate and read
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    class(NumericalExchangeType) :: this
! ------------------------------------------------------------------------------
    !
    ! -- return
    return
  end subroutine exg_ar

  subroutine exg_ad(this)
! ******************************************************************************
! exg_ad -- Advance
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(NumericalExchangeType) :: this
    ! -- local
! ------------------------------------------------------------------------------
    !
    ! -- return
    return
  end subroutine exg_ad

  subroutine exg_cf(this, kiter)
! ******************************************************************************
! exg_cf -- Calculate conductance, and for explicit exchanges, set the
!   conductance in the boundary package.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(NumericalExchangeType) :: this
    integer(I4B), intent(in) :: kiter
    ! -- local
! ------------------------------------------------------------------------------
    !
    ! -- return
    return
  end subroutine exg_cf

  subroutine exg_fc(this, kiter, iasln, amatsln, rhssln, inwtflag)
! ******************************************************************************
! exg_fc -- Fill the matrix
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(NumericalExchangeType) :: this
    integer(I4B), intent(in) :: kiter
    integer(I4B), dimension(:), intent(in) :: iasln
    real(DP), dimension(:), intent(inout) :: amatsln
    real(DP), dimension(:), intent(inout) :: rhssln
    integer(I4B), optional, intent(in) :: inwtflag
    ! -- local
! ------------------------------------------------------------------------------
    !
    ! -- return
    return
  end subroutine exg_fc

  subroutine exg_nr(this, kiter, iasln, amatsln, inwtflag)
! ******************************************************************************
! exg_nr -- Add Newton-Raphson terms to the solution
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(NumericalExchangeType) :: this
    integer(I4B), intent(in) :: kiter
    integer(I4B), dimension(:), intent(in) :: iasln
    real(DP), dimension(:), intent(inout) :: amatsln
    integer(I4B), optional, intent(in) :: inwtflag
    ! -- local
! ------------------------------------------------------------------------------
    !
    ! -- return
    return
  end subroutine exg_nr

  subroutine exg_cc(this, icnvg)
! ******************************************************************************
! exg_cc -- Additional convergence check
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(NumericalExchangeType) :: this
    integer(I4B), intent(inout) :: icnvg
    ! -- local
! ------------------------------------------------------------------------------
    !
    ! -- return
    return
  end subroutine exg_cc

  subroutine exg_cq(this, icnvg, isuppress_output, isolnid)
! ******************************************************************************
! exg_cq -- Calculate flow
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use ConstantsModule, only: LENBUDTXT
    ! -- dummy
    class(NumericalExchangeType) :: this
    integer(I4B), intent(inout) :: icnvg
    integer(I4B), intent(in) :: isuppress_output
    integer(I4B), intent(in) :: isolnid
    ! -- local
! ------------------------------------------------------------------------------
    !
    ! -- return
    return
  end subroutine exg_cq

  subroutine exg_bd(this, icnvg, isuppress_output, isolnid)
! ******************************************************************************
! exg_bd -- Exchange budget
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use ConstantsModule, only: LENBUDTXT
    ! -- dummy
    class(NumericalExchangeType) :: this
    integer(I4B), intent(inout) :: icnvg
    integer(I4B), intent(in) :: isuppress_output
    integer(I4B), intent(in) :: isolnid
    ! -- local
! ------------------------------------------------------------------------------
    !
    ! -- return
    return
  end subroutine exg_bd

  subroutine exg_ot(this)
! ******************************************************************************
! exg_ot
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(NumericalExchangeType) :: this
! ------------------------------------------------------------------------------
    !
    ! -- return
    return
  end subroutine exg_ot

  subroutine exg_da(this)
! ******************************************************************************
! exg_da
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryManagerModule, only: mem_deallocate
    ! -- dummy
    class(NumericalExchangeType) :: this
    ! -- local
! ------------------

    return
  end subroutine exg_da

  function get_iasym(this) result(iasym)
    class(NumericalExchangeType) :: this
    integer(I4B) :: iasym

    iasym = 0

  end function get_iasym

  function CastAsNumericalExchangeClass(obj) result(res)
    implicit none
    class(*), pointer, intent(inout) :: obj
    class(NumericalExchangeType), pointer :: res
    !
    res => null()
    if (.not. associated(obj)) return
    !
    select type (obj)
    class is (NumericalExchangeType)
      res => obj
    end select
    return
  end function CastAsNumericalExchangeClass

  subroutine AddNumericalExchangeToList(list, exchange)
    implicit none
    ! -- dummy
    type(ListType), intent(inout) :: list
    class(NumericalExchangeType), pointer, intent(in) :: exchange
    ! -- local
    class(*), pointer :: obj
    !
    obj => exchange
    call list%Add(obj)
    !
    return
  end subroutine AddNumericalExchangeToList

  function GetNumericalExchangeFromList(list, idx) result(res)
    implicit none
    ! -- dummy
    type(ListType), intent(inout) :: list
    integer(I4B), intent(in)    :: idx
    class(NumericalExchangeType), pointer    :: res
    ! -- local
    class(*), pointer :: obj
    !
    obj => list%GetItem(idx)
    res => CastAsNumericalExchangeClass(obj)
    !
    return
  end function GetNumericalExchangeFromList

end module NumericalExchangeModule
