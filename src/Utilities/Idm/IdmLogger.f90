!> @brief This module contains the Input Data Model Logger Module
!!
!! This module contains the subroutines for logging messages
!! to the list file as the input data model loads model input.
!!
!<
module IdmLoggerModule

  use KindModule, only: DP, LGP, I4B

  implicit none
  private
  public :: idm_log_header
  public :: idm_log_close
  public :: idm_log_var

  interface idm_log_var
    module procedure idm_log_var_logical, idm_log_var_int, &
      idm_log_var_int1d, idm_log_var_int2d, &
      idm_log_var_int3d, idm_log_var_dbl, &
      idm_log_var_dbl1d, idm_log_var_dbl2d, &
      idm_log_var_dbl3d
  end interface idm_log_var

contains

  !> @ brief log a header message
  !<
  subroutine idm_log_header(component, subcomponent, iout)
    character(len=*), intent(in) :: component !< component name
    character(len=*), intent(in) :: subcomponent !< subcomponent name
    integer(I4B), intent(in) :: iout

    if (iout > 0) then
      write (iout, '(1x,a)') 'Loading input for '//trim(component)//&
        &'/'//trim(subcomponent)
    end if
  end subroutine idm_log_header

  !> @ brief log the closing message
  !<
  subroutine idm_log_close(component, subcomponent, iout)
    character(len=*), intent(in) :: component !< component name
    character(len=*), intent(in) :: subcomponent !< subcomponent name
    integer(I4B) :: iout

    write (iout, '(1x,a,/)') 'Loading input complete...'
  end subroutine idm_log_close

  !> @brief Log type specific information logical
  !<
  subroutine idm_log_var_logical(p_mem, varname, mempath, iout)
    logical(LGP), intent(in) :: p_mem !< logical scalar
    character(len=*), intent(in) :: varname !< variable name
    character(len=*), intent(in) :: mempath !< variable memory path
    integer(I4B) :: iout

    write (iout, '(3x,a, " = ", l)') trim(varname), p_mem
  end subroutine idm_log_var_logical

  !> @brief Log type specific information integer
  !<
  subroutine idm_log_var_int(p_mem, varname, mempath, iout)
    integer(I4B), intent(in) :: p_mem !< int scalar
    character(len=*), intent(in) :: varname !< variable name
    character(len=*), intent(in) :: mempath !< variable memory path
    integer(I4B) :: iout

    write (iout, '(3x,a, " = ", i0)') trim(varname), p_mem
  end subroutine idm_log_var_int

  !> @brief Log type specific information int1d
  !<
  subroutine idm_log_var_int1d(p_mem, varname, mempath, iout)
    integer(I4B), dimension(:), contiguous, intent(in) :: p_mem !< 1d int array
    character(len=*), intent(in) :: varname !< variable name
    character(len=*), intent(in) :: mempath !< variable memory path
    integer(I4B) :: iout
    integer(I4B) :: min_val, max_val

    min_val = minval(p_mem)
    max_val = maxval(p_mem)
    if (min_val == max_val) then
      write (iout, '(3x,a, " = ", i0)') trim(varname), min_val
    else
      write (iout, '(3x, a, a, i0, a, i0)') &
        trim(varname), &
        ' = variable 1D integer array ranging from ', &
        min_val, ' to ', max_val
    end if
  end subroutine idm_log_var_int1d

  !> @brief Log type specific information int2d
  !<
  subroutine idm_log_var_int2d(p_mem, varname, mempath, iout)
    integer(I4B), dimension(:, :), contiguous, intent(in) :: p_mem !< 2d int array
    character(len=*), intent(in) :: varname !< variable name
    character(len=*), intent(in) :: mempath !< variable memory path
    integer(I4B) :: iout
    integer(I4B) :: min_val, max_val

    min_val = minval(p_mem)
    max_val = maxval(p_mem)
    if (min_val == max_val) then
      write (iout, '(3x,a, " = ", i0)') trim(varname), min_val
    else
      write (iout, '(3x, a, a, i0, a, i0)') &
        trim(varname), &
        ' = variable 2D integer array ranging from ', &
        min_val, ' to ', max_val
    end if
  end subroutine idm_log_var_int2d

  !> @brief Log type specific information int3d
  !<
  subroutine idm_log_var_int3d(p_mem, varname, mempath, iout)
    integer(I4B), dimension(:, :, :), contiguous, intent(in) :: p_mem !< 3d int array
    character(len=*), intent(in) :: varname !< variable name
    character(len=*), intent(in) :: mempath !< variable memory path
    integer(I4B) :: iout
    integer(I4B) :: min_val, max_val

    min_val = minval(p_mem)
    max_val = maxval(p_mem)
    if (min_val == max_val) then
      write (iout, '(3x,a, " = ", i0)') trim(varname), min_val
    else
      write (iout, '(3x, a, a, i0, a, i0)') &
        trim(varname), &
        ' = variable 3D integer array ranging from ', &
        min_val, ' to ', max_val
    end if
  end subroutine idm_log_var_int3d

  !> @brief Log type specific information double
  !<
  subroutine idm_log_var_dbl(p_mem, varname, mempath, iout)
    real(DP), intent(in) :: p_mem !< dbl scalar
    character(len=*), intent(in) :: varname !< variable name
    character(len=*), intent(in) :: mempath !< variable memory path
    integer(I4B) :: iout

    write (iout, '(3x,a, " = ", G0)') trim(varname), p_mem
  end subroutine idm_log_var_dbl

  !> @brief Log type specific information dbl1d
  !<
  subroutine idm_log_var_dbl1d(p_mem, varname, mempath, iout)
    real(DP), dimension(:), contiguous, intent(in) :: p_mem !< 1d real array
    character(len=*), intent(in) :: varname !< variable name
    character(len=*), intent(in) :: mempath !< variable memory path
    integer(I4B) :: iout
    real(DP) :: min_val, max_val

    min_val = minval(p_mem)
    max_val = maxval(p_mem)
    if (min_val == max_val) then
      write (iout, '(3x,a, " = ", G0)') trim(varname), min_val
    else
      write (iout, '(3x, a, a, G0, a, G0)') &
        trim(varname), &
        ' = variable 1D double precision array ranging from ', &
        min_val, ' to ', max_val
    end if
  end subroutine idm_log_var_dbl1d

  !> @brief Log type specific information dbl2d
  !<
  subroutine idm_log_var_dbl2d(p_mem, varname, mempath, iout)
    real(DP), dimension(:, :), contiguous, intent(in) :: p_mem !< 2d dbl array
    character(len=*), intent(in) :: varname !< variable name
    character(len=*), intent(in) :: mempath !< variable memory path
    integer(I4B) :: iout
    real(DP) :: min_val, max_val

    min_val = minval(p_mem)
    max_val = maxval(p_mem)
    if (min_val == max_val) then
      write (iout, '(3x,a, " = ", G0)') trim(varname), min_val
    else
      write (iout, '(3x, a, a, G0, a, G0)') &
        trim(varname), &
        ' = variable 2D double precision array ranging from ', &
        min_val, ' to ', max_val
    end if
  end subroutine idm_log_var_dbl2d

  !> @brief Log type specific information dbl3d
  !<
  subroutine idm_log_var_dbl3d(p_mem, varname, mempath, iout)
    real(DP), dimension(:, :, :), contiguous, intent(in) :: p_mem !< 3d dbl array
    character(len=*), intent(in) :: varname !< variable name
    character(len=*), intent(in) :: mempath !< variable memory path
    integer(I4B) :: iout
    real(DP) :: min_val, max_val

    min_val = minval(p_mem)
    max_val = maxval(p_mem)
    if (min_val == max_val) then
      write (iout, '(3x,a, " = ", G0)') trim(varname), min_val
    else
      write (iout, '(3x, a, a, G0, a, G0)') &
        trim(varname), &
        ' = variable 3D double precision array ranging from ', &
        min_val, ' to ', max_val
    end if
  end subroutine idm_log_var_dbl3d

end module IdmLoggerModule
