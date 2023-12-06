module ErrorUtilModule
  use KindModule, only: I4B
  implicit none
  private
  public :: stop_with_error

  !> @brief Exit the program, optionally specifying status code and error message.
  subroutine stop_with_error(status, message)
    ! -- dummy variables
    integer(I4B), intent(in), optional :: status !< optional error code to return (default=0)
    character(len=*), intent(in), optional :: message !< optional message to print before stopping
    ! -- local variables
    integer(I4B) :: istatus
    
    ! -- process optional dummy variables
    if (present(status)) then
      istatus = status
    else
      istatus = 0
    end if

    ! -- print message, if there is one
    if (present(message)) print *, message

    ! -- exit with the given status code
    call exit(istatus)

  end subroutine stop_with_error

end module ErrorUtilModule