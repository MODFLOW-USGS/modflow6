module IteratorModule
  use KindModule, only: LGP
  implicit none
  private

  public :: IteratorType

  type, abstract :: IteratorType
  contains
    procedure(has_next_if), deferred :: has_next
    procedure(next_if), deferred :: next
    procedure(value_if), deferred :: value

  end type IteratorType

  abstract interface

    function has_next_if(this) result(res)
      import IteratorType
      import LGP
      class(IteratorType) :: this
      logical(LGP) :: res
    end function

    subroutine next_if(this)
      import IteratorType
      class(IteratorType) :: this
    end subroutine

    function value_if(this) result(res)
      import IteratorType
      class(IteratorType) :: this
      class(*), pointer :: res
    end function

  end interface

end module IteratorModule
