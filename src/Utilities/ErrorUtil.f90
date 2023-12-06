module ErrorUtilModule
  use KindModule, only: I4B
  implicit none
contains

  !> @brief Stop the program with an optional status code.
  !!
  !! If a non-zero status is specified, the program is terminated with the
  !! given status code. If no status is specified or status=0, the program
  !! stops with code 0. A message may be provided to print before exiting,
  !! useful e.g. for "contact developer" messages upon programming errors.
  !<
  subroutine pstop(status, message)
    integer(I4B), intent(in), optional :: status !< optional error code to return (default=0)
    character(len=*), intent(in), optional :: message !< optional message to print before stopping

    if (present(message)) print *, message
    if (present(status)) then
      if (status == 0) stop
      call exit(status)
    else
      stop
    end if
  end subroutine pstop

end module ErrorUtilModule
