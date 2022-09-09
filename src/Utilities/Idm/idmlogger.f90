module IdmLoggerModule

  use KindModule, only: DP, LGP, I4B, I8B
  use ConstantsModule, only: MAXMEMRANK
  use SimModule, only: store_error
  use MemoryManagerModule, only: get_mem_rank, get_mem_shape

  implicit none
  private
  public :: idm_log_header
  public :: idm_log_close
  public :: idm_log_type

  interface idm_log_type
    module procedure idm_log_type_logical, idm_log_type_int, &
                     idm_log_type_int1d, idm_log_type_int2d, &
                     idm_log_type_int3d, idm_log_type_dbl, &
                     idm_log_type_dbl1d, idm_log_type_dbl2d, &
                     idm_log_type_dbl3d
  end interface idm_log_type

contains

  subroutine idm_log_header(component, iout)
    character(len=*), intent(in) :: component !< path where variable is stored
    integer(I4B) :: iout

    write(iout, '(a)') 'IDM parameter input sourcing, component='//component
  end subroutine idm_log_header

  subroutine idm_log_close(component, iout)
    character(len=*), intent(in) :: component !< path where variable is stored
    integer(I4B) :: iout

    write(iout, '(a)') 'IDM parameter input sourcing complete, component='//component
  end subroutine idm_log_close

  !> @brief Log type specific information
  !<
  subroutine idm_log_type_logical(p_mem, varname, mempath, component, iout)
    logical(LGP), pointer, intent(inout) :: p_mem !< pointer to logical scalar
    character(len=*), intent(in) :: varname !< variable name
    character(len=*), intent(in) :: mempath !< variable memory path
    character(len=*), intent(in) :: component !< component
    integer(I4B) :: iout

    write(iout, '(2x,a)') trim(varname)
    write(iout, '(4x,a)') 'Type     => logical'
    write(iout, '(4x,a,l)') 'Value    => ', p_mem
    write(iout, '(a)')
  end subroutine idm_log_type_logical

  !> @brief Log type specific information
  !<
  subroutine idm_log_type_int(p_mem, varname, mempath, component, iout)
    integer(I4B), pointer, intent(inout) :: p_mem !< pointer to int scalar
    character(len=*), intent(in) :: varname !< variable name
    character(len=*), intent(in) :: mempath !< variable memory path
    character(len=*), intent(in) :: component !< component
    integer(I4B) :: iout

    write(iout, '(2x,a)') trim(varname)
    write(iout, '(4x,a)') 'Type     => integer'
    write(iout, '(4x,a,i3)') 'Value    => ', p_mem
    write(iout, '(a)')
  end subroutine idm_log_type_int

  !> @brief Log type specific information
  !<
  subroutine idm_log_type_int1d(p_mem, varname, mempath, component, iout)
    integer(I4B), dimension(:), pointer, contiguous, intent(inout) :: p_mem !< pointer to 1d int array
    character(len=*), intent(in) :: varname !< variable name
    character(len=*), intent(in) :: mempath !< variable memory path
    character(len=*), intent(in) :: component !< component
    integer(I4B) :: iout
    integer(I4B) :: rank
    integer(I4B), dimension(MAXMEMRANK) :: mem_shape !< shape of the variable

    call get_mem_rank(varname, mempath, rank)
    call get_mem_shape(varname, mempath, mem_shape)

    write(iout, '(2x,a)') trim(varname)
    write(iout, '(4x,a)') 'Type     => 1D integer Array'
    write(iout, '(4x,a,i7)') 'Rank     => ', rank
    write(iout, '(4x,a,i7)') 'Size     => ', size(p_mem)
    write(iout, '(a)')
  end subroutine idm_log_type_int1d

  !> @brief Log type specific information
  !<
  subroutine idm_log_type_int2d(p_mem, varname, mempath, component, iout)
    integer(I4B), dimension(:,:), pointer, contiguous, intent(inout) :: p_mem !< pointer to 2d int array
    character(len=*), intent(in) :: varname !< variable name
    character(len=*), intent(in) :: mempath !< variable memory path
    character(len=*), intent(in) :: component !< component
    integer(I4B) :: iout
    integer(I4B) :: rank
    integer(I4B), dimension(MAXMEMRANK) :: mem_shape !< shape of the variable

    call get_mem_rank(varname, mempath, rank)
    call get_mem_shape(varname, mempath, mem_shape)

    write(iout, '(2x,a)') trim(varname)
    write(iout, '(4x,a)') 'Type     => 2D integer Array'
    write(iout, '(4x,a,i7)') 'Rank     => ', rank
    write(iout, '(4x,a,i7)') 'Size     => ', size(p_mem)
    write(iout, '(a)')
  end subroutine idm_log_type_int2d

  !> @brief Log type specific information
  !<
  subroutine idm_log_type_int3d(p_mem, varname, mempath, component, iout)
    integer(I4B), dimension(:,:,:), pointer, contiguous, intent(inout) :: p_mem !< pointer to 3d int array
    character(len=*), intent(in) :: varname !< variable name
    character(len=*), intent(in) :: mempath !< variable memory path
    character(len=*), intent(in) :: component !< component
    integer(I4B) :: iout
    integer(I4B) :: rank
    integer(I4B), dimension(MAXMEMRANK) :: mem_shape !< shape of the variable

    call get_mem_rank(varname, mempath, rank)
    call get_mem_shape(varname, mempath, mem_shape)

    write(iout, '(2x,a)') trim(varname)
    write(iout, '(4x,a)') 'Type     => 3D integer Array'
    write(iout, '(4x,a,i7)') 'Rank     => ', rank
    write(iout, '(4x,a,i7)') 'Size     => ', size(p_mem)
    write(iout, '(a)')
  end subroutine idm_log_type_int3d

  !> @brief Log type specific information
  !<
  subroutine idm_log_type_dbl(p_mem, varname, mempath, component, iout)
    real(DP), pointer, intent(inout) :: p_mem !< pointer to dbl scalar
    character(len=*), intent(in) :: varname !< variable name
    character(len=*), intent(in) :: mempath !< variable memory path
    character(len=*), intent(in) :: component !< component
    integer(I4B) :: iout

    write(iout, '(2x,a)') trim(varname)
    write(iout, '(4x,a)') 'Type     => double'
    write(iout, '(4x,a,1pg24.15)') 'Value    => ', p_mem
    write(iout, '(a)')
  end subroutine idm_log_type_dbl

  !> @brief Log type specific information
  !<
  subroutine idm_log_type_dbl1d(p_mem, varname, mempath, component, iout)
    real(DP), dimension(:), pointer, contiguous, intent(inout) :: p_mem !< pointer to 1d real array
    character(len=*), intent(in) :: varname !< variable name
    character(len=*), intent(in) :: mempath !< variable memory path
    character(len=*), intent(in) :: component !< component
    integer(I4B) :: iout
    integer(I4B) :: rank
    integer(I4B), dimension(MAXMEMRANK) :: mem_shape !< shape of the variable

    call get_mem_rank(varname, mempath, rank)
    call get_mem_shape(varname, mempath, mem_shape)

    write(iout, '(2x,a)') trim(varname)
    write(iout, '(4x,a)') 'Type     => 1D double Array'
    write(iout, '(4x,a,i7)') 'Rank     => ', rank
    write(iout, '(4x,a,i7)') 'Size     => ', size(p_mem)
    write(iout, '(a)')
  end subroutine idm_log_type_dbl1d

  !> @brief Log type specific information
  !<
  subroutine idm_log_type_dbl2d(p_mem, varname, mempath, component, iout)
    real(DP), dimension(:,:), pointer, contiguous, intent(inout) :: p_mem !< pointer to 2d dbl array
    character(len=*), intent(in) :: varname !< variable name
    character(len=*), intent(in) :: mempath !< variable memory path
    character(len=*), intent(in) :: component !< component
    integer(I4B) :: iout
    integer(I4B) :: rank
    integer(I4B), dimension(MAXMEMRANK) :: mem_shape !< shape of the variable

    call get_mem_rank(varname, mempath, rank)
    call get_mem_shape(varname, mempath, mem_shape)

    write(iout, '(2x,a)') trim(varname)
    write(iout, '(4x,a)') 'Type     => 2D double Array'
    write(iout, '(4x,a,i7)') 'Rank     => ', rank
    write(iout, '(4x,a,i7)') 'Size     => ', size(p_mem)
    write(iout, '(a)')
  end subroutine idm_log_type_dbl2d

  !> @brief Log type specific information
  !<
  subroutine idm_log_type_dbl3d(p_mem, varname, mempath, component, iout)
    real(DP), dimension(:,:,:), pointer, contiguous, intent(inout) :: p_mem !< pointer to 3d dbl array
    character(len=*), intent(in) :: varname !< variable name
    character(len=*), intent(in) :: mempath !< variable memory path
    character(len=*), intent(in) :: component !< component
    integer(I4B) :: iout
    integer(I4B) :: rank
    integer(I4B), dimension(MAXMEMRANK) :: mem_shape !< shape of the variable

    call get_mem_rank(varname, mempath, rank)
    call get_mem_shape(varname, mempath, mem_shape)

    write(iout, '(2x,a)') trim(varname)
    write(iout, '(4x,a)') 'Type     => 3D double Array'
    write(iout, '(4x,a,i7)') 'Rank     => ', rank
    write(iout, '(4x,a,i7)') 'Size     => ', size(p_mem)
    write(iout, '(a)')
  end subroutine idm_log_type_dbl3d

end module IdmLoggerModule
