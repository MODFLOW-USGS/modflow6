! -- A solution contains a list of models, packages, and exchanges
module BaseSolutionModule

  use KindModule, only: DP, I4B
  use ConstantsModule, only: LENSOLUTIONNAME
  use BaseModelModule, only: BaseModelType
  use ListModule,      only: ListType
  implicit none

  private
  public :: BaseSolutionType, AddBaseSolutionToList, GetBaseSolutionFromList
  private :: CastAsBaseSolutionClass

  type, abstract :: BaseSolutionType
    character(len=LENSOLUTIONNAME) :: name
  contains
    procedure (sln_df), deferred :: sln_df
    procedure (sln_ar), deferred :: sln_ar
    procedure (sln_calculate_delt), deferred :: sln_calculate_delt
    procedure (sln_ad), deferred :: sln_ad
    procedure (sln_ca), deferred :: sln_ca
    procedure (sln_ot), deferred :: sln_ot
    procedure (sln_fp), deferred :: sln_fp
    procedure (sln_da), deferred :: sln_da
    procedure (slnsave), deferred :: save
    procedure (slnaddmodel), deferred :: addmodel
    procedure (slnassignexchanges), deferred :: slnassignexchanges
  end type BaseSolutionType

  abstract interface
  
    subroutine sln_df(this)
      import BaseSolutionType
      class(BaseSolutionType) :: this
    end subroutine

    subroutine slnassignexchanges(this)
      import BaseSolutionType
      class(BaseSolutionType) :: this
    end subroutine

    subroutine sln_ar(this)
      import BaseSolutionType
      class(BaseSolutionType) :: this
    end subroutine

    subroutine sln_rp(this)
      import BaseSolutionType
      class(BaseSolutionType) :: this
    end subroutine
    
    subroutine sln_calculate_delt(this)
      import BaseSolutionType
      class(BaseSolutionType) :: this
    end subroutine
    
    subroutine sln_ad(this)
      import BaseSolutionType
      class(BaseSolutionType) :: this
    end subroutine

    subroutine sln_ot(this)
      import BaseSolutionType
      class(BaseSolutionType) :: this
    end subroutine

    subroutine sln_ca(this, isgcnvg, isuppress_output)
      use KindModule, only: DP, I4B
      import BaseSolutionType
      class(BaseSolutionType) :: this
      integer(I4B),intent(in) :: isuppress_output
      integer(I4B), intent(inout) :: isgcnvg
    end subroutine

    subroutine slnsave(this,filename)
      import BaseSolutionType
      class(BaseSolutionType) :: this
      character(len=*), intent(in) :: filename
    end subroutine

    subroutine slnaddmodel(this,mp)
      import BaseSolutionType
      import BaseModelType
      class(BaseSolutionType) :: this
      class(BaseModelType),pointer,intent(in) :: mp
    end subroutine

    subroutine sln_fp(this)
      import BaseSolutionType
      class(BaseSolutionType) :: this
    end subroutine

    subroutine sln_da(this)
      import BaseSolutionType
      class(BaseSolutionType) :: this
    end subroutine

  end interface

contains

  function CastAsBaseSolutionClass(obj) result (res)
    implicit none
    class(*), pointer, intent(inout) :: obj
    class(BaseSolutionType), pointer :: res
    !
    res => null()
    if (.not. associated(obj)) return
    !
    select type (obj)
    class is (BaseSolutionType)
      res => obj
    end select
    return
  end function CastAsBaseSolutionClass

  subroutine AddBaseSolutionToList(list, solution)
    implicit none
    ! -- dummy
    type(ListType),       intent(inout) :: list
    class(BaseSolutionType), pointer, intent(in) :: solution
    ! -- local
    class(*), pointer :: obj
    !
    obj => solution
    call list%Add(obj)
    !
    return
  end subroutine AddBaseSolutionToList
  
  function GetBaseSolutionFromList(list, idx) result (res)
    implicit none
    ! -- dummy
    type(ListType),       intent(inout) :: list
    integer(I4B),              intent(in)    :: idx
    class(BaseSolutionType), pointer    :: res
    ! -- local
    class(*), pointer :: obj
    !
    obj => list%GetItem(idx)
    res => CastAsBaseSolutionClass(obj)
    !
    return
  end function GetBaseSolutionFromList

end module BaseSolutionModule
