module mf6dll
  use mf6core
  use bmif
  use iso_c_binding, only: c_int, c_char, c_double, C_NULL_CHAR, c_loc, c_ptr
  use KindModule, only: DP, I4B
  use ConstantsModule, only: LENORIGIN, LENVARNAME
  implicit none
  
contains
    
  function initialize() result(bmi_status) bind(C, name="initialize")
  !DEC$ ATTRIBUTES DLLEXPORT :: initialize
    integer :: bmi_status
      
    call mf6_initialize()
    bmi_status = BMI_SUCCESS
    
  end function initialize
   
  function update() result(bmi_status) bind(C, name="update")
  !DEC$ ATTRIBUTES DLLEXPORT :: update
    integer :: bmi_status
    ! local
    logical :: hasConverged
    
    hasConverged = mf6_update()
    if (hasConverged) then
      bmi_status = BMI_SUCCESS
    else
      bmi_status = BMI_FAILURE
    end if
    
  end function update
     
  ! Perform teardown tasks for the model.
  function finalize() result(bmi_status) bind(C, name="finalize")
  !DEC$ ATTRIBUTES DLLEXPORT :: finalize
    integer :: bmi_status
    
    call mf6_finalize()
    bmi_status = BMI_SUCCESS
    
  end function finalize
  
  ! Start time of the model.
  function get_start_time(time) result(bmi_status) bind(C, name="get_start_time")
  !DEC$ ATTRIBUTES DLLEXPORT :: get_start_time
    double precision, intent(out) :: time
    integer :: bmi_status
    
    time = 0.0_DP
    bmi_status = BMI_SUCCESS
    
  end function get_start_time
  
  ! End time of the model.
  function get_end_time(time) result(bmi_status) bind(C, name="get_end_time")
  !DEC$ ATTRIBUTES DLLEXPORT :: get_end_time
    use TdisModule, only: totalsimtime
    double precision, intent(out) :: time
    integer :: bmi_status
    
    time = totalsimtime
    bmi_status = BMI_SUCCESS
    
  end function get_end_time

  ! Current time of the model.
  function get_current_time(time) result(bmi_status) bind(C, name="get_current_time")
  !DEC$ ATTRIBUTES DLLEXPORT :: get_current_time
    use TdisModule, only: totim
    double precision, intent(out) :: time
    integer :: bmi_status
    
    time = totim
    bmi_status = BMI_SUCCESS    
    
  end function get_current_time
  
  ! Get memory use per array element, in bytes.
  function get_var_itemsize(c_var_name, var_size) result(bmi_status) bind(C, name="get_var_itemsize")
  !DEC$ ATTRIBUTES DLLEXPORT :: get_var_itemsize
    use MemoryManagerModule, only: get_var_size
    character (kind=c_char), intent(in) :: c_var_name(*)
    integer, intent(out) :: var_size
    integer :: bmi_status
    ! local
    integer(I4B) :: idx
    character(len=LENORIGIN) :: origin, var_name
    character(len=LENVARNAME) :: var_name_only
    
    var_name = char_array_to_string(c_var_name, strlen(c_var_name))
    
    idx = index(var_name, '/', back=.true.)
    origin = var_name(:idx-1)
    var_name_only = var_name(idx+1:)
    
    bmi_status = BMI_SUCCESS
    call get_var_size(var_name_only, origin, var_size)    
    if (var_size == -1) bmi_status = BMI_FAILURE
        
  end function get_var_itemsize

  ! Get size of the given variable, in bytes.
  function get_var_nbytes(c_var_name, var_nbytes) result(bmi_status) bind(C, name="get_var_nbytes")
  !DEC$ ATTRIBUTES DLLEXPORT :: get_var_nbytes
    use MemoryManagerModule, only: get_var_size, get_isize
    character (kind=c_char), intent(in) :: c_var_name(*)
    integer, intent(out) :: var_nbytes
    integer :: bmi_status
    ! local
    integer :: var_size, isize, idx
    character(len=LENORIGIN) :: origin, var_name
    character(len=LENVARNAME) :: var_name_only
    
    var_name = char_array_to_string(c_var_name, strlen(c_var_name))
    
    idx = index(var_name, '/', back=.true.)
    origin = var_name(:idx-1)
    var_name_only = var_name(idx+1:)
    
    bmi_status = BMI_SUCCESS
    call get_var_size(var_name_only, origin, var_size)    
    if (var_size == -1) bmi_status = BMI_FAILURE
    call get_isize(var_name_only, origin, isize)
    if (isize == -1) bmi_status = BMI_FAILURE
    
    var_nbytes = var_size*isize
    
  end function get_var_nbytes

  ! set the pointer to the array of the given double variable.
  function get_value_ptr_double(c_var_name, x) result(bmi_status) bind(C, name="get_value_ptr_double")
  !DEC$ ATTRIBUTES DLLEXPORT :: get_value_ptr_double
    use MemoryManagerModule, only: setptr_dbl1d
    character (kind=c_char), intent(in) :: c_var_name(*)    
    type(c_ptr), intent(inout) :: x
    integer :: bmi_status
    ! local
    integer :: idx, i
    character(len=LENORIGIN) :: origin, var_name
    character(len=LENVARNAME) :: var_name_only
    real(DP), dimension(:), pointer, contiguous :: adbl
    
    var_name = char_array_to_string(c_var_name, strlen(c_var_name))
    
    idx = index(var_name, '/', back=.true.)
    origin = var_name(:idx-1)
    var_name_only = var_name(idx+1:)
    call setptr_dbl1d(adbl, var_name_only, origin)
    
    ! set the C pointer to the internal array
    x = c_loc(adbl)
    bmi_status = BMI_SUCCESS
    
  end function get_value_ptr_double

  ! Get a copy of values (flattened!) of the given double variable.
  function get_value_double(c_var_name, x, nx) result(bmi_status) bind(C, name="get_value_double")
  !DEC$ ATTRIBUTES DLLEXPORT :: get_value_double
    use MemoryManagerModule, only: copy_dbl1d
    character (kind=c_char), intent(in) :: c_var_name(*)
    integer, intent(in) :: nx
    real(DP), dimension(nx), intent(inout) :: x    
    integer :: bmi_status
    ! local
    integer :: idx, i
    character(len=LENORIGIN) :: origin, var_name
    character(len=LENVARNAME) :: var_name_only
    
    var_name = char_array_to_string(c_var_name, strlen(c_var_name))
    
    idx = index(var_name, '/', back=.true.)
    origin = var_name(:idx-1)
    var_name_only = var_name(idx+1:)
    
    call copy_dbl1d(x, var_name_only, origin)
    bmi_status = BMI_SUCCESS
    
  end function get_value_double
  
  integer(c_int) pure function strlen(char_array)
    character(c_char), intent(in) :: char_array(LENORIGIN)
    integer :: inull, i
    strlen = 0
    do i = 1, size(char_array)
      if (char_array(i) .eq. C_NULL_CHAR) then
          strlen = i-1
          exit
      end if
    end do
  end function strlen
  
  pure function char_array_to_string(char_array, length)
    integer(c_int), intent(in) :: length
    character(c_char),intent(in) :: char_array(length)
    character(len=length) :: char_array_to_string
    integer :: i
    do i = 1, length
      char_array_to_string(i:i) = char_array(i)
    enddo
  end function char_array_to_string

end module mf6dll