!> @brief This module contains the Input Data Model Logger Module
!!
!! This module contains the subroutines for logging messages
!! to the list file as the input data model loads model input.
!!
!<
module IdmLoggerModule

  use KindModule, only: DP, LGP, I4B
  use SimVariablesModule, only: iparamlog
  use ConstantsModule, only: LINELENGTH

  implicit none
  private
  public :: idm_log_header
  public :: idm_log_close
  public :: idm_log_period_header
  public :: idm_log_period_close
  public :: idm_log_var

  interface idm_log_var
    module procedure idm_log_var_logical, idm_log_var_int, &
      idm_log_var_int1d, idm_log_var_int2d, &
      idm_log_var_int3d, idm_log_var_dbl, &
      idm_log_var_dbl1d, idm_log_var_dbl2d, &
      idm_log_var_dbl3d, idm_log_var_str, &
      idm_log_var_ts
  end interface idm_log_var

contains

  !> @ brief log a header message
  !<
  subroutine idm_log_header(component, subcomponent, iout)
    character(len=*), intent(in) :: component !< component name
    character(len=*), intent(in) :: subcomponent !< subcomponent name
    integer(I4B), intent(in) :: iout

    if (iparamlog > 0 .and. iout > 0) then
      write (iout, '(1x,a)') 'Loading input for '//trim(component)//&
        &'/'//trim(subcomponent)
    end if
  end subroutine idm_log_header

  !> @ brief log the closing message
  !<
  subroutine idm_log_close(component, subcomponent, iout)
    character(len=*), intent(in) :: component !< component name
    character(len=*), intent(in) :: subcomponent !< subcomponent name
    integer(I4B), intent(in) :: iout

    if (iparamlog > 0 .and. iout > 0) then
      write (iout, '(1x,a)') 'Loading input complete...'
    end if
  end subroutine idm_log_close

  !> @ brief log a dynamic header message
  !<
  subroutine idm_log_period_header(component, iout)
    use TdisModule, only: kper, kstp
    character(len=*), intent(in) :: component !< component name
    integer(I4B), intent(in) :: iout

    if (iparamlog > 0 .and. iout > 0 .and. kstp == 1) then
      write (iout, '(/1x,a,i0,a)') 'IDP PERIOD ', kper, &
        ' load for component: '//trim(component)
    end if
  end subroutine idm_log_period_header

  !> @ brief log the period closing message
  !<
  subroutine idm_log_period_close(iout)
    use TdisModule, only: kstp
    integer(I4B), intent(in) :: iout

    if (iparamlog > 0 .and. iout > 0 .and. kstp == 1) then
      !backspace iout
      write (iout, '(1x,a,/)') 'IDP component dynamic load complete...'
    end if
  end subroutine idm_log_period_close

  !> @ brief log the period closing message
  !<
  subroutine idm_log_var_ts(varname, mempath, iout, is_tas)
    character(len=*), intent(in) :: varname !< variable name
    character(len=*), intent(in) :: mempath !< variable memory path
    integer(I4B), intent(in) :: iout
    logical(LGP), intent(in) :: is_tas

    if (iparamlog > 0 .and. iout > 0) then
      if (is_tas) then
        write (iout, '(3x, a, ": ", a)') &
          'Time-array-series controlled dynamic variable detected', trim(varname)
      else
        write (iout, '(3x, a, ": ", a)') &
          'Time-series controlled dynamic variable detected', trim(varname)
      end if
    end if
  end subroutine idm_log_var_ts

  !> @brief Log type specific information logical
  !<
  subroutine idm_log_var_logical(p_mem, varname, mempath, iout)
    logical(LGP), intent(in) :: p_mem !< logical scalar
    character(len=*), intent(in) :: varname !< variable name
    character(len=*), intent(in) :: mempath !< variable memory path
    integer(I4B), intent(in) :: iout
    character(len=LINELENGTH) :: description

    if (iparamlog > 0 .and. iout > 0) then
      description = 'Logical detected'
      write (iout, '(3x, a, ": ", a, " = ", l1)') &
        trim(description), trim(varname), p_mem
    end if
  end subroutine idm_log_var_logical

  !> @brief Log type specific information integer
  !<
  subroutine idm_log_var_int(p_mem, varname, mempath, datatype, iout)
    integer(I4B), intent(in) :: p_mem !< int scalar
    character(len=*), intent(in) :: varname !< variable name
    character(len=*), intent(in) :: mempath !< variable memory path
    character(len=*), intent(in) :: datatype !< variable data type
    integer(I4B), intent(in) :: iout
    character(len=LINELENGTH) :: description

    if (iparamlog > 0 .and. iout > 0) then
      if (datatype == 'KEYWORD') then
        description = 'Keyword detected'
        write (iout, '(3x, a, ": ", a)') trim(description), trim(varname)
      else
        description = 'Integer detected'
        write (iout, '(3x, a, ": ", a, " = ", i0)') &
          trim(description), trim(varname), p_mem
      end if
    end if
  end subroutine idm_log_var_int

  !> @brief Log type specific information int1d
  !<
  subroutine idm_log_var_int1d(p_mem, varname, mempath, iout)
    integer(I4B), dimension(:), contiguous, intent(in) :: p_mem !< 1d int array
    character(len=*), intent(in) :: varname !< variable name
    character(len=*), intent(in) :: mempath !< variable memory path
    integer(I4B), intent(in) :: iout
    integer(I4B) :: min_val, max_val
    character(len=LINELENGTH) :: description

    if (iparamlog > 0 .and. iout > 0) then
      min_val = minval(p_mem)
      max_val = maxval(p_mem)
      if (min_val == max_val) then
        description = 'Integer 1D constant array detected'
        write (iout, '(3x, a, ": ", a, " = ", i0)') &
          trim(description), trim(varname), min_val
      else
        description = 'Integer 1D array detected'
        write (iout, '(3x, a, ": ", a, a, i0, a, i0)') &
          trim(description), trim(varname), &
          ' ranges from ', min_val, ' to ', max_val
      end if
    end if
  end subroutine idm_log_var_int1d

  !> @brief Log type specific information int2d
  !<
  subroutine idm_log_var_int2d(p_mem, varname, mempath, iout)
    integer(I4B), dimension(:, :), contiguous, intent(in) :: p_mem !< 2d int array
    character(len=*), intent(in) :: varname !< variable name
    character(len=*), intent(in) :: mempath !< variable memory path
    integer(I4B), intent(in) :: iout
    integer(I4B) :: min_val, max_val
    character(len=LINELENGTH) :: description

    if (iparamlog > 0 .and. iout > 0) then
      min_val = minval(p_mem)
      max_val = maxval(p_mem)
      if (min_val == max_val) then
        description = 'Integer 2D constant array detected'
        write (iout, '(3x, a, ": ", a, " = ", i0)') &
          trim(description), trim(varname), min_val
      else
        description = 'Integer 2D array detected'
        write (iout, '(3x, a, ": ", a, a, i0, a, i0)') &
          trim(description), trim(varname), &
          ' ranges from ', min_val, ' to ', max_val
      end if
    end if
  end subroutine idm_log_var_int2d

  !> @brief Log type specific information int3d
  !<
  subroutine idm_log_var_int3d(p_mem, varname, mempath, iout)
    integer(I4B), dimension(:, :, :), contiguous, intent(in) :: p_mem !< 3d int array
    character(len=*), intent(in) :: varname !< variable name
    character(len=*), intent(in) :: mempath !< variable memory path
    integer(I4B), intent(in) :: iout
    integer(I4B) :: min_val, max_val
    character(len=LINELENGTH) :: description

    if (iparamlog > 0 .and. iout > 0) then
      min_val = minval(p_mem)
      max_val = maxval(p_mem)
      if (min_val == max_val) then
        description = 'Integer 3D constant array detected'
        write (iout, '(3x, a, ": ", a, " = ", i0)') &
          trim(description), trim(varname), min_val
      else
        description = 'Integer 3D array detected'
        write (iout, '(3x, a, ": ", a, a, i0, a, i0)') &
          trim(description), trim(varname), &
          ' ranges from ', min_val, ' to ', max_val
      end if
    end if
  end subroutine idm_log_var_int3d

  !> @brief Log type specific information double
  !<
  subroutine idm_log_var_dbl(p_mem, varname, mempath, iout)
    real(DP), intent(in) :: p_mem !< dbl scalar
    character(len=*), intent(in) :: varname !< variable name
    character(len=*), intent(in) :: mempath !< variable memory path
    integer(I4B), intent(in) :: iout
    character(len=LINELENGTH) :: description

    if (iparamlog > 0 .and. iout > 0) then
      description = 'Double detected'
      write (iout, '(3x, a, ": ", a, " = ", G0)') &
        trim(description), trim(varname), p_mem
    end if
  end subroutine idm_log_var_dbl

  !> @brief Log type specific information dbl1d
  !<
  subroutine idm_log_var_dbl1d(p_mem, varname, mempath, iout)
    real(DP), dimension(:), contiguous, intent(in) :: p_mem !< 1d real array
    character(len=*), intent(in) :: varname !< variable name
    character(len=*), intent(in) :: mempath !< variable memory path
    integer(I4B), intent(in) :: iout
    real(DP) :: min_val, max_val
    character(len=LINELENGTH) :: description

    if (iparamlog > 0 .and. iout > 0) then
      min_val = minval(p_mem)
      max_val = maxval(p_mem)
      if (min_val == max_val) then
        description = 'Double precision 1D constant array detected'
        write (iout, '(3x, a, ": ", a, " = ", G0)') &
          trim(description), trim(varname), min_val
      else
        description = 'Double precision 1D array detected'
        write (iout, '(3x, a, ": ", a, a, G0, a, G0)') &
          trim(description), trim(varname), &
          ' ranges from ', min_val, ' to ', max_val
      end if
    end if
  end subroutine idm_log_var_dbl1d

  !> @brief Log type specific information dbl2d
  !<
  subroutine idm_log_var_dbl2d(p_mem, varname, mempath, iout)
    real(DP), dimension(:, :), contiguous, intent(in) :: p_mem !< 2d dbl array
    character(len=*), intent(in) :: varname !< variable name
    character(len=*), intent(in) :: mempath !< variable memory path
    integer(I4B), intent(in) :: iout
    real(DP) :: min_val, max_val
    character(len=LINELENGTH) :: description

    if (iparamlog > 0 .and. iout > 0) then
      min_val = minval(p_mem)
      max_val = maxval(p_mem)
      if (min_val == max_val) then
        description = 'Double precision 2D constant array detected'
        write (iout, '(3x, a, ": ", a, " = ", G0)') &
          trim(description), trim(varname), min_val
      else
        description = 'Double precision 2D array detected'
        write (iout, '(3x, a, ": ", a, a, G0, a, G0)') &
          trim(description), trim(varname), &
          ' ranges from ', min_val, ' to ', max_val
      end if
    end if
  end subroutine idm_log_var_dbl2d

  !> @brief Log type specific information dbl3d
  !<
  subroutine idm_log_var_dbl3d(p_mem, varname, mempath, iout)
    real(DP), dimension(:, :, :), contiguous, intent(in) :: p_mem !< 3d dbl array
    character(len=*), intent(in) :: varname !< variable name
    character(len=*), intent(in) :: mempath !< variable memory path
    integer(I4B), intent(in) :: iout
    real(DP) :: min_val, max_val
    character(len=LINELENGTH) :: description

    if (iparamlog > 0 .and. iout > 0) then
      min_val = minval(p_mem)
      max_val = maxval(p_mem)
      if (min_val == max_val) then
        description = 'Double precision 3D constant array detected'
        write (iout, '(3x, a, ": ", a, " = ", G0)') &
          trim(description), trim(varname), min_val
      else
        description = 'Double precision 3D array detected'
        write (iout, '(3x, a, ": ", a, a, G0, a, G0)') &
          trim(description), trim(varname), &
          ' ranges from ', min_val, ' to ', max_val
      end if
    end if
  end subroutine idm_log_var_dbl3d

  !> @brief Log type specific information str
  !<
  subroutine idm_log_var_str(p_mem, varname, mempath, iout)
    character(len=*), intent(in) :: p_mem !< pointer to str scalar
    character(len=*), intent(in) :: varname !< variable name
    character(len=*), intent(in) :: mempath !< variable memory path
    integer(I4B), intent(in) :: iout
    character(len=LINELENGTH) :: description

    if (iparamlog > 0 .and. iout > 0) then
      description = 'String detected'
      write (iout, '(3x, a, ": ", a, " = ", a)') &
        trim(description), trim(varname), trim(p_mem)
    end if
  end subroutine idm_log_var_str

end module IdmLoggerModule
