module BaseExchangeModule
  
  use KindModule,         only: DP, I4B
  use ConstantsModule,    only: LENPACKAGENAME
  use BaseSolutionModule, only: BaseSolutionType
  use ListModule,         only: ListType
  
  implicit none
  
  private
  public :: BaseExchangeType, AddBaseExchangeToList, GetBaseExchangeFromList
  private :: CastAsBaseExchangeClass
  
  type, abstract :: BaseExchangeType
    character(len=LENPACKAGENAME)    :: name
    class(BaseSolutionType), pointer :: solution => null()
    integer(I4B)                     :: id
  contains
    procedure(exg_df), deferred :: exg_df
    procedure(exg_ar), deferred :: exg_ar
    procedure :: exg_rp
    procedure :: exg_ot
    procedure :: exg_fp
    procedure :: exg_da
  end type BaseExchangeType

  abstract interface

    subroutine exg_df(this)
      import BaseExchangeType
      class(BaseExchangeType) :: this
    end subroutine

    subroutine exg_ar(this)
      import BaseExchangeType
      class(BaseExchangeType) :: this
    end subroutine

  end interface
  
  contains
  
  subroutine exg_rp(this)
! ******************************************************************************
! exg_rp -- Read and prepare
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use TdisModule, only: readnewdata
    ! -- dummy
    class(BaseExchangeType) :: this
! ------------------------------------------------------------------------------
    !
    ! -- Check with TDIS on whether or not it is time to RP
    if (.not. readnewdata) return
    !
    ! -- Nothing to do for RP
    !
    ! -- Return
    return
  end subroutine exg_rp
  
  subroutine exg_ot(this)
! ******************************************************************************
! exg_ot -- Output
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(BaseExchangeType) :: this
! ------------------------------------------------------------------------------
    !
    ! -- Return
    return
  end subroutine exg_ot
  
  subroutine exg_fp(this)
! ******************************************************************************
! exg_fp -- Final processing
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(BaseExchangeType) :: this
! ------------------------------------------------------------------------------
    !
    ! -- Return
    return
  end subroutine exg_fp
  
  subroutine exg_da(this)
! ******************************************************************************
! exg_da -- Deallocate
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(BaseExchangeType) :: this
! ------------------------------------------------------------------------------
    !
    ! -- Return
    return
  end subroutine exg_da

  function CastAsBaseExchangeClass(obj) result (res)
! ******************************************************************************
! CastAsBaseExchangeClass
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(*), pointer, intent(inout) :: obj
    ! -- return
    class(BaseExchangeType), pointer :: res
! ------------------------------------------------------------------------------
    !
    res => null()
    if (.not. associated(obj)) return
    !
    select type (obj)
    class is (BaseExchangeType)
      res => obj
    end select
    return
  end function CastAsBaseExchangeClass

  subroutine AddBaseExchangeToList(list, exchange)
! ******************************************************************************
! AddBaseExchangeToList
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    type(ListType),       intent(inout) :: list
    class(BaseExchangeType), pointer, intent(inout) :: exchange
    ! -- local
    class(*), pointer :: obj
! ------------------------------------------------------------------------------
    !
    obj => exchange
    call list%Add(obj)
    !
    return
  end subroutine AddBaseExchangeToList
  
  function GetBaseExchangeFromList(list, idx) result (res)
! ******************************************************************************
! GetBaseExchangeFromList
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    type(ListType),       intent(inout) :: list
    integer(I4B),              intent(in)    :: idx
    class(BaseExchangeType), pointer    :: res
    ! -- local
    class(*), pointer :: obj
! ------------------------------------------------------------------------------
    !
    obj => list%GetItem(idx)
    res => CastAsBaseExchangeClass(obj)
    !
    return
  end function GetBaseExchangeFromList

end module BaseExchangeModule
