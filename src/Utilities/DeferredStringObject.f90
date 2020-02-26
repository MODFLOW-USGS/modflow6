module DeferredStringModule
  implicit none

  public :: deferred_string_type
  
  type deferred_string_type
    character(len=:), allocatable :: string
  end type deferred_string_type
  
end module DeferredStringModule
