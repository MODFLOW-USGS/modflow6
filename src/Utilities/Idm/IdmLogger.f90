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
  public :: idm_print_array

  interface idm_log_var
    module procedure idm_log_var_logical, idm_log_var_int, &
      idm_log_var_int1d, idm_log_var_int2d, &
      idm_log_var_int3d, idm_log_var_dbl, &
      idm_log_var_dbl1d, idm_log_var_dbl2d, &
      idm_log_var_dbl3d, idm_log_var_str, &
      idm_log_var_ts
  end interface idm_log_var

  interface idm_print_array
    module procedure idm_print_array_int1d, idm_print_array_int2d, &
      idm_print_array_int3d, idm_print_array_dbl1d, &
      idm_print_array_dbl2d, idm_print_array_dbl3d
  end interface idm_print_array

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
    use TdisModule, only: kper
    character(len=*), intent(in) :: component !< component name
    integer(I4B), intent(in) :: iout

    if (iparamlog > 0 .and. iout > 0) then
      write (iout, '(/1x,a,i0,a)') 'IDP PERIOD ', kper, &
        ' load for component: '//trim(component)
    end if
  end subroutine idm_log_period_header

  !> @ brief log the period closing message
  !<
  subroutine idm_log_period_close(iout)
    integer(I4B), intent(in) :: iout

    if (iparamlog > 0 .and. iout > 0) then
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

  subroutine idm_print_array_int1d(p_mem, varname, mempath, iout)
    integer(I4B), dimension(:), contiguous, intent(in) :: p_mem !< 2d dbl array
    character(len=*), intent(in) :: varname !< variable name
    character(len=*), intent(in) :: mempath !< variable memory path
    integer(I4B), intent(in) :: iout
    ! -- dummy
    integer(I4B) :: inunit
    !
    inunit = create_array_echofile(varname, mempath, 0, iout)
    !
    write (inunit, '(*(i0, " "))') p_mem
    !
    close (inunit)
  end subroutine idm_print_array_int1d

  subroutine idm_print_array_int2d(p_mem, varname, mempath, iout)
    integer(I4B), dimension(:, :), contiguous, intent(in) :: p_mem !< 2d dbl array
    character(len=*), intent(in) :: varname !< variable name
    character(len=*), intent(in) :: mempath !< variable memory path
    integer(I4B), intent(in) :: iout
    ! -- dummy
    integer(I4B) :: i, inunit
    !
    inunit = create_array_echofile(varname, mempath, 0, iout)
    !
    do i = 1, size(p_mem, dim=2)
      write (inunit, '(*(i0, " "))') p_mem(:, i)
    end do
    !
    close (inunit)
  end subroutine idm_print_array_int2d

  subroutine idm_print_array_int3d(p_mem, varname, mempath, iout)
    integer(I4B), dimension(:, :, :), contiguous, intent(in) :: p_mem !< 2d dbl array
    character(len=*), intent(in) :: varname !< variable name
    character(len=*), intent(in) :: mempath !< variable memory path
    integer(I4B), intent(in) :: iout
    ! -- dummy
    integer(I4B) :: i, j, inunit
    !
    do i = 1, size(p_mem, dim=3)
      inunit = create_array_echofile(varname, mempath, i, iout)
      do j = 1, size(p_mem, dim=2)
        write (inunit, '(*(i0, " "))') p_mem(:, j, i)
      end do
      close (inunit)
    end do
  end subroutine idm_print_array_int3d

  subroutine idm_print_array_dbl1d(p_mem, varname, mempath, iout)
    real(DP), dimension(:), contiguous, intent(in) :: p_mem !< 2d dbl array
    character(len=*), intent(in) :: varname !< variable name
    character(len=*), intent(in) :: mempath !< variable memory path
    integer(I4B), intent(in) :: iout
    ! -- dummy
    integer(I4B) :: inunit
    !
    inunit = create_array_echofile(varname, mempath, 0, iout)
    !
    write (inunit, '(*(G0.10, " "))') p_mem
    !
    close (inunit)
  end subroutine idm_print_array_dbl1d

  subroutine idm_print_array_dbl2d(p_mem, varname, mempath, iout)
    real(DP), dimension(:, :), contiguous, intent(in) :: p_mem !< 2d dbl array
    character(len=*), intent(in) :: varname !< variable name
    character(len=*), intent(in) :: mempath !< variable memory path
    integer(I4B), intent(in) :: iout
    ! -- dummy
    integer(I4B) :: i, inunit
    !
    inunit = create_array_echofile(varname, mempath, 0, iout)
    !
    do i = 1, size(p_mem, dim=2)
      write (inunit, '(*(G0.10, " "))') p_mem(:, i)
    end do
    !
    close (inunit)
  end subroutine idm_print_array_dbl2d

  subroutine idm_print_array_dbl3d(p_mem, varname, mempath, iout)
    real(DP), dimension(:, :, :), contiguous, intent(in) :: p_mem !< 2d dbl array
    character(len=*), intent(in) :: varname !< variable name
    character(len=*), intent(in) :: mempath !< variable memory path
    integer(I4B), intent(in) :: iout
    ! -- dummy
    integer(I4B) :: i, j, inunit
    !
    do i = 1, size(p_mem, dim=3)
      inunit = create_array_echofile(varname, mempath, i, iout)
      do j = 1, size(p_mem, dim=2)
        write (inunit, '(*(G0.10, " "))') p_mem(:, j, i)
      end do
      close (inunit)
    end do
  end subroutine idm_print_array_dbl3d

  function create_array_echofile(varname, mempath, layer, iout) result(inunit)
    use ConstantsModule, only: LENCOMPONENTNAME, LENVARNAME
    use InputOutputModule, only: openfile, getunit
    use InputOutputModule, only: upcase, lowcase
    use MemoryHelperModule, only: create_mem_path, split_mem_path
    character(len=*), intent(in) :: varname !< variable name
    character(len=*), intent(in) :: mempath !< variable memory path
    integer(I4B), intent(in) :: layer
    integer(I4B), intent(in) :: iout
    integer(I4B) :: inunit
    ! -- dummy
    character(len=LENCOMPONENTNAME) :: comp, subcomp
    character(len=LINELENGTH) :: filename
    character(len=LENVARNAME) :: suffix
    !
    call split_mem_path(mempath, comp, subcomp)
    suffix = varname
    call lowcase(suffix)
    filename = trim(comp)//'-'//trim(subcomp)//'.'//trim(suffix)
    if (layer > 0) then
      write (filename, '(a,i0)') trim(filename)//'.l', layer
    end if
    filename = trim(filename)//'.txt'
    !
    ! -- create the array file
    inunit = getunit()
    call openfile(inunit, iout, filename, 'ECHO', filstat_opt='REPLACE')
  end function create_array_echofile

end module IdmLoggerModule
