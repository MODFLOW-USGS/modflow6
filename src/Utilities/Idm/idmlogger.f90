module IdmLoggerModule

  use KindModule, only: DP, LGP, I4B, I8B
  use ConstantsModule, only: MAXMEMRANK
  use SimModule, only: store_error
  use MemoryManagerModule, only: get_mem_rank, get_mem_shape

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

  subroutine idm_log_header(component, iout)
    character(len=*), intent(in) :: component !< path where variable is stored
    integer(I4B) :: iout

    write (iout, '(1x,a)') 'IDM parameter input sourcing, component='//component
  end subroutine idm_log_header

  subroutine idm_log_close(component, iout)
    character(len=*), intent(in) :: component !< path where variable is stored
    integer(I4B) :: iout

    write (iout, '(1x,a)') 'IDM parameter input sourcing complete, component='&
                           &//component
  end subroutine idm_log_close

  !> @brief Log type specific information
  !<
  subroutine idm_log_var_logical(p_mem, varname, mempath, iout)
    logical(LGP), pointer, intent(inout) :: p_mem !< pointer to logical scalar
    character(len=*), intent(in) :: varname !< variable name
    character(len=*), intent(in) :: mempath !< variable memory path
    integer(I4B) :: iout

    write (iout, '(3x,a)') trim(varname)
    write (iout, '(5x,a,l)') 'VALUE:', p_mem
  end subroutine idm_log_var_logical

  !> @brief Log type specific information
  !<
  subroutine idm_log_var_int(p_mem, varname, mempath, iout)
    integer(I4B), pointer, intent(inout) :: p_mem !< pointer to int scalar
    character(len=*), intent(in) :: varname !< variable name
    character(len=*), intent(in) :: mempath !< variable memory path
    integer(I4B) :: iout

    write (iout, '(3x,a)') trim(varname)
    write (iout, '(5x,a,i3)') 'VALUE:', p_mem
  end subroutine idm_log_var_int

  !> @brief Log type specific information
  !<
  subroutine idm_log_var_int1d(p_mem, varname, mempath, iout)
    integer(I4B), dimension(:), pointer, contiguous, intent(inout) :: p_mem !< pointer to 1d int array
    character(len=*), intent(in) :: varname !< variable name
    character(len=*), intent(in) :: mempath !< variable memory path
    integer(I4B) :: iout
    integer(I4B) :: rank
    integer(I4B), dimension(1) :: mem_shape, varubound, varlbound, varmaxloc, &
                                  varminloc

    call get_mem_rank(varname, mempath, rank)
    call get_mem_shape(varname, mempath, mem_shape)
    varubound = ubound(p_mem)
    varlbound = lbound(p_mem)
    varmaxloc = maxloc(p_mem)
    varminloc = minloc(p_mem)

    write (iout, '(3x,a)') trim(varname)
    write (iout, '(5x,a,i0)') 'RANK:', rank
    write (iout, '(5x,a,i0)') 'SIZE:', size(p_mem)
    write (iout, "(5x,a,' (',i0,')')") 'SHAPE:', mem_shape(1)
    write (iout, "(5x,a,' (',i0,')')") 'UBOUND:', varubound(1)
    write (iout, "(5x,a,' (',i0,')')") 'LBOUND:', varlbound(1)
    write (iout, *) '    MAXVAL:', maxval(p_mem)
    write (iout, "(5x,a,' (',i0,')')") 'MAXLOC:', varmaxloc(1)
    write (iout, *) '    MINVAL:', minval(p_mem)
    write (iout, "(5x,a,' (',i0,')')") 'MINLOC:', varminloc(1)
  end subroutine idm_log_var_int1d

  !> @brief Log type specific information
  !<
  subroutine idm_log_var_int2d(p_mem, varname, mempath, iout)
    integer(I4B), dimension(:, :), pointer, contiguous, intent(inout) :: p_mem !< pointer to 2d int array
    character(len=*), intent(in) :: varname !< variable name
    character(len=*), intent(in) :: mempath !< variable memory path
    integer(I4B) :: iout
    integer(I4B) :: rank
    integer(I4B), dimension(2) :: mem_shape, varubound, varlbound, varmaxloc, &
                                  varminloc

    call get_mem_rank(varname, mempath, rank)
    call get_mem_shape(varname, mempath, mem_shape)
    varubound = ubound(p_mem)
    varlbound = lbound(p_mem)
    varmaxloc = maxloc(p_mem)
    varminloc = minloc(p_mem)

    write (iout, '(3x,a)') trim(varname)
    write (iout, '(5x,a,i0)') 'RANK:', rank
    write (iout, '(5x,a,i0)') 'SIZE:', size(p_mem)
    write (iout, "(5x,a,' (',i0,',',i0,')')") 'SHAPE:', mem_shape(1), mem_shape(2)
    write (iout, "(5x,a,' (',i0,',',i0,')')") 'UBOUND:', varubound(1), &
      varubound(2)
    write (iout, "(5x,a,' (',i0,',',i0,')')") 'LBOUND:', varlbound(1), &
      varlbound(2)
    write (iout, *) '    MAXVAL:', maxval(p_mem)
    write (iout, "(5x,a,' (',i0,',',i0,')')") 'MAXLOC:', varmaxloc(1), &
      varmaxloc(2)
    write (iout, *) '    MINVAL:', minval(p_mem)
    write (iout, "(5x,a,' (',i0,',',i0,')')") 'MINLOC:', varminloc(1), &
      varminloc(2)
  end subroutine idm_log_var_int2d

  !> @brief Log type specific information
  !<
  subroutine idm_log_var_int3d(p_mem, varname, mempath, iout)
    integer(I4B), dimension(:, :, :), pointer, contiguous, intent(inout) :: p_mem !< pointer to 3d int array
    character(len=*), intent(in) :: varname !< variable name
    character(len=*), intent(in) :: mempath !< variable memory path
    integer(I4B) :: iout
    integer(I4B) :: rank
    integer(I4B), dimension(MAXMEMRANK) :: mem_shape, varubound, varlbound, &
                                           varmaxloc, varminloc

    call get_mem_rank(varname, mempath, rank)
    call get_mem_shape(varname, mempath, mem_shape)
    varubound = ubound(p_mem)
    varlbound = lbound(p_mem)
    varmaxloc = maxloc(p_mem)
    varminloc = minloc(p_mem)

    write (iout, '(3x,a)') trim(varname)
    write (iout, '(5x,a,i0)') 'RANK:', rank
    write (iout, '(5x,a,i0)') 'SIZE:', size(p_mem)
    write (iout, "(5x,a,' (',i0,',',i0,',',i0,')')") 'SHAPE:', mem_shape(1), &
      mem_shape(2), mem_shape(3)
    write (iout, "(5x,a,' (',i0,',',i0,',',i0,')')") 'UBOUND:', varubound(1), &
      varubound(2), varubound(3)
    write (iout, "(5x,a,' (',i0,',',i0,',',i0,')')") 'LBOUND:', varlbound(1), &
      varlbound(2), varlbound(3)
    write (iout, *) '    MAXVAL:', maxval(p_mem)
    write (iout, "(5x,a,' (',i0,',',i0,',',i0,')')") 'MAXLOC:', varmaxloc(1), &
      varmaxloc(2), varmaxloc(3)
    write (iout, *) '    MINVAL:', minval(p_mem)
    write (iout, "(5x,a,' (',i0,',',i0,',',i0,')')") 'MINLOC:', varminloc(1), &
      varminloc(2), varminloc(3)
  end subroutine idm_log_var_int3d

  !> @brief Log type specific information
  !<
  subroutine idm_log_var_dbl(p_mem, varname, mempath, iout)
    real(DP), pointer, intent(inout) :: p_mem !< pointer to dbl scalar
    character(len=*), intent(in) :: varname !< variable name
    character(len=*), intent(in) :: mempath !< variable memory path
    integer(I4B) :: iout

    write (iout, '(3x,a)') trim(varname)
    write (iout, '(5x,a,1pg24.15)') 'VALUE:', p_mem
  end subroutine idm_log_var_dbl

  !> @brief Log type specific information
  !<
  subroutine idm_log_var_dbl1d(p_mem, varname, mempath, iout)
    real(DP), dimension(:), pointer, contiguous, intent(inout) :: p_mem !< pointer to 1d real array
    character(len=*), intent(in) :: varname !< variable name
    character(len=*), intent(in) :: mempath !< variable memory path
    integer(I4B) :: iout
    integer(I4B) :: rank
    integer(I4B), dimension(1) :: mem_shape, varubound, varlbound, varmaxloc, &
                                  varminloc

    call get_mem_rank(varname, mempath, rank)
    call get_mem_shape(varname, mempath, mem_shape)
    varubound = ubound(p_mem)
    varlbound = lbound(p_mem)
    varmaxloc = maxloc(p_mem)
    varminloc = minloc(p_mem)

    write (iout, '(3x,a)') trim(varname)
    write (iout, '(5x,a,i0)') 'RANK:', rank
    write (iout, '(5x,a,i0)') 'SIZE:', size(p_mem)
    write (iout, "(5x,a,' (',i0,')')") 'SHAPE:', mem_shape(1)
    write (iout, "(5x,a,' (',i0,')')") 'UBOUND:', varubound(1)
    write (iout, "(5x,a,' (',i0,')')") 'LBOUND:', varlbound(1)
    write (iout, *) '    MAXVAL:', maxval(p_mem)
    write (iout, "(5x,a,' (',i0,')')") 'MAXLOC:', varmaxloc(1)
    write (iout, *) '    MINVAL:', minval(p_mem)
    write (iout, "(5x,a,' (',i0,')')") 'MINLOC:', varminloc(1)
  end subroutine idm_log_var_dbl1d

  !> @brief Log type specific information
  !<
  subroutine idm_log_var_dbl2d(p_mem, varname, mempath, iout)
    real(DP), dimension(:, :), pointer, contiguous, intent(inout) :: p_mem !< pointer to 2d dbl array
    character(len=*), intent(in) :: varname !< variable name
    character(len=*), intent(in) :: mempath !< variable memory path
    integer(I4B) :: iout
    integer(I4B) :: rank
    integer(I4B), dimension(2) :: mem_shape, varubound, varlbound, varmaxloc, &
                                  varminloc

    call get_mem_rank(varname, mempath, rank)
    call get_mem_shape(varname, mempath, mem_shape)
    varubound = ubound(p_mem)
    varlbound = lbound(p_mem)
    varmaxloc = maxloc(p_mem)
    varminloc = minloc(p_mem)

    write (iout, '(3x,a)') trim(varname)
    write (iout, '(5x,a,i0)') 'RANK:', rank
    write (iout, '(5x,a,i0)') 'SIZE:', size(p_mem)
    write (iout, "(5x,a,' (',i0,',',i0,')')") 'SHAPE:', mem_shape(1), mem_shape(2)
    write (iout, "(5x,a,' (',i0,',',i0,')')") 'UBOUND:', varubound(1), &
      varubound(2)
    write (iout, "(5x,a,' (',i0,',',i0,')')") 'LBOUND:', varlbound(1), &
      varlbound(2)
    write (iout, *) '    MAXVAL:', maxval(p_mem)
    write (iout, "(5x,a,' (',i0,',',i0,')')") 'MAXLOC:', varmaxloc(1), &
      varmaxloc(2)
    write (iout, *) '    MINVAL:', minval(p_mem)
    write (iout, "(5x,a,' (',i0,',',i0,')')") 'MINLOC:', varminloc(1), &
      varminloc(2)
  end subroutine idm_log_var_dbl2d

  !> @brief Log type specific information
  !<
  subroutine idm_log_var_dbl3d(p_mem, varname, mempath, iout)
    real(DP), dimension(:, :, :), pointer, contiguous, intent(inout) :: p_mem !< pointer to 3d dbl array
    character(len=*), intent(in) :: varname !< variable name
    character(len=*), intent(in) :: mempath !< variable memory path
    integer(I4B) :: iout
    integer(I4B) :: rank
    integer(I4B), dimension(MAXMEMRANK) :: mem_shape, varubound, varlbound, &
                                           varmaxloc, varminloc

    call get_mem_rank(varname, mempath, rank)
    call get_mem_shape(varname, mempath, mem_shape)
    varubound = ubound(p_mem)
    varlbound = lbound(p_mem)
    varmaxloc = maxloc(p_mem)
    varminloc = minloc(p_mem)

    write (iout, '(3x,a)') trim(varname)
    write (iout, '(5x,a,i0)') 'RANK:', rank
    write (iout, '(5x,a,i0)') 'SIZE:', size(p_mem)
    write (iout, "(5x,a,' (',i0,',',i0,',',i0,')')") 'SHAPE:', mem_shape(1), &
      mem_shape(2), mem_shape(3)
    write (iout, "(5x,a,' (',i0,',',i0,',',i0,')')") 'UBOUND:', varubound(1), &
      varubound(2), varubound(3)
    write (iout, "(5x,a,' (',i0,',',i0,',',i0,')')") 'LBOUND:', varlbound(1), &
      varlbound(2), varlbound(3)
    write (iout, *) '    MAXVAL:', maxval(p_mem)
    write (iout, "(5x,a,' (',i0,',',i0,',',i0,')')") 'MAXLOC:', varmaxloc(1), &
      varmaxloc(2), varmaxloc(3)
    write (iout, *) '    MINVAL:', minval(p_mem)
    write (iout, "(5x,a,' (',i0,',',i0,',',i0,')')") 'MINLOC:', varminloc(1), &
      varminloc(2), varminloc(3)
  end subroutine idm_log_var_dbl3d

end module IdmLoggerModule
