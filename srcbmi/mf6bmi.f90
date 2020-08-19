!> @brief This module contains the MODFLOW 6 BMI
!!
!! This BMI interface matches the CSDMS standard, with a few modifications:
!!
!! - This interface will build into a shared library that can be called from other
!!   executables and scripts, not necessarily written in Fortran. Therefore we have
!!   omitted the type-boundness of the routines, since they cannot have the
!!   bind(C,"...") attribute.
!! - MODFLOW has internal data arrays with rank > 1 that we would like to expose. An
!!   example would be access to data in the BOUND array of GWF boundary packages (BndType).
!!   The get_value_ptr calls below support this, returning a C-style pointer to the arrays
!!   and methods have been added to query the variable's rank and shape.
!!
!! Note on style: BMI apparently uses underscores, we use underscores in some 
!! places but camelcase in other. Since this is a dedicated BMI interface module,
!! we'll use underscores here as well.
!<
module mf6bmi
  use mf6bmiUtil
  use bmif, only: BMI_FAILURE, BMI_SUCCESS
  use Mf6CoreModule
  use TdisModule, only: kper, kstp
  use iso_c_binding, only: c_int, c_char, c_double, C_NULL_CHAR, c_loc, c_ptr
  use KindModule, only: DP, I4B
  use ConstantsModule, only: LENMEMPATH, LENVARNAME
  use MemoryManagerModule, only: mem_setptr, get_mem_elem_size, get_isize, get_mem_rank, get_mem_shape, get_mem_type
  use SimVariablesModule, only: simstdout, istdout
  use InputOutputModule, only: getunit
  implicit none
 
  integer(c_int), bind(C, name="ISTDOUTTOFILE") :: istdout_to_file = 1  !< output control: =0 to screen, >0 to file
  !DEC$ ATTRIBUTES DLLEXPORT :: istdout_to_file
  
  contains  

  !> @brief Initialize the computational core
  !!
  !! It is required to have the MODFLOW 6 configuration file 'mfsim.nam' 
  !! available in the current working directory when calling this routine.
  !!
  !! NOTE: initialization should always be matched with a call to 
  !! bmi_finalize(). However, we currently do not support the reinitialization 
  !! of a model in the same memory space... You would have to create a new 
  !! process for that.
  !!
  !! @return      bmi_status      the BMI status code
  !<
  function bmi_initialize() result(bmi_status) bind(C, name="initialize")
  !DEC$ ATTRIBUTES DLLEXPORT :: bmi_initialize
    integer(kind=c_int) :: bmi_status
    ! local
    
    if (istdout_to_file > 0) then
      ! -- open stdout file mfsim.stdout
      istdout = getunit() 
      !
      ! -- set STDOUT to a physical file unit
      open(unit=istdout, file=simstdout)
    end if        
    !
    ! -- initialize MODFLOW 6
    call Mf6Initialize()
    
    bmi_status = BMI_SUCCESS
    
  end function bmi_initialize
  

  !> @brief Perform a computational time step
  !!
  !! It will prepare the timestep, call the calculation routine 
  !! on all the solution groups in the simulation, and finalize
  !! the timestep by printing out diagnostics and writing output.
  !! It can be called in succession to perform multiple steps up
  !! to the simulation's end time is reached.
  !!
  !! @return      bmi_status      the BMI status code
  !<
  function bmi_update() result(bmi_status) bind(C, name="update")
  !DEC$ ATTRIBUTES DLLEXPORT :: bmi_update
    integer(kind=c_int) :: bmi_status
    ! local
    logical :: hasConverged
    
    hasConverged = Mf6Update()
    
    bmi_status = BMI_SUCCESS
  end function bmi_update
  

  !> @brief Clean up the initialized simulation
  !!
  !! Performs teardown tasks for the initialized simulation, this
  !! call should match the call to bmi_initialize()
  !!
  !! @return      bmi_status      the BMI status code
  !<
  function bmi_finalize() result(bmi_status) bind(C, name="finalize")
  !DEC$ ATTRIBUTES DLLEXPORT :: bmi_finalize
    use SimVariablesModule, only: iforcestop
    integer(kind=c_int) :: bmi_status
    
    ! we don't want a full stop() here, this disables it:    
    iforcestop = 0    
    call Mf6Finalize()
      
    bmi_status = BMI_SUCCESS
      
  end function bmi_finalize
  

  !> @brief Get the start time of the simulation
  !!
  !! As MODFLOW currently does not have internal time, this will be 
  !! returning 0.0 for now. New version...
  !<
  function get_start_time(start_time) result(bmi_status) bind(C, name="get_start_time")
  !DEC$ ATTRIBUTES DLLEXPORT :: get_start_time
    double precision, intent(out) :: start_time !< the start time
    integer(kind=c_int) :: bmi_status           !< the BMI status code
    
    start_time = 0.0_DP    
    bmi_status = BMI_SUCCESS
    
  end function get_start_time


  !> @brief Get the end time of the simulation
  !!
  !! As MODFLOW does currently does not have internal time, this will be
  !! equal to the total runtime.
  !!
  !! @param[out]  end_time        the end time
  !! @return      bmi_status      the BMI status code
  !<
  function get_end_time(end_time) result(bmi_status) bind(C, name="get_end_time")
  !DEC$ ATTRIBUTES DLLEXPORT :: get_end_time
    use TdisModule, only: totalsimtime
    double precision, intent(out) :: end_time
    integer(kind=c_int) :: bmi_status
    
    end_time = totalsimtime
    bmi_status = BMI_SUCCESS
    
  end function get_end_time

  !> @brief Get the current time of the simulation
  !!
  !! As MODFLOW currently does not have internal time, this will be
  !! equal to the time passed w.r.t. the start time of the simulation.
  !!
  !! @param[out]  current_time    the current time
  !! @return      bmi_status      the BMI status code
  !<
  function get_current_time(current_time) result(bmi_status) bind(C, name="get_current_time")
  !DEC$ ATTRIBUTES DLLEXPORT :: get_current_time
    use TdisModule, only: totim
    double precision, intent(out) :: current_time
    integer(kind=c_int) :: bmi_status
    
    current_time = totim
    bmi_status = BMI_SUCCESS    
    
  end function get_current_time

  
  !> @brief Get the time step for the simulation
  !!
  !! Note that the returned value may vary between and within stress periods,
  !! depending on your time discretization settings in the TDIS package.
  !!
  !! @param[out]  time_step       the current time step
  !! @return      bmi_status      the BMI status code
  !<
  function get_time_step(time_step) result(bmi_status) bind(C, name="get_time_step")
  !DEC$ ATTRIBUTES DLLEXPORT :: get_time_step
    use TdisModule, only: delt
    double precision, intent(out) :: time_step
    integer(kind=c_int) :: bmi_status

    time_step = delt
    bmi_status = BMI_SUCCESS

  end function get_time_step
  

  !> @brief Get the size (in bytes) of a single element of a variable
  !! 
  !! @param[in]   c_var_address     the memory address string of the variable
  !! @param[out]  var_size          the size of the element in bytes
  !! @return      bmi_status        the BMI status code
  !< 
  function get_var_itemsize(c_var_address, var_size) result(bmi_status) bind(C, name="get_var_itemsize")
  !DEC$ ATTRIBUTES DLLEXPORT :: get_var_itemsize
    character (kind=c_char), intent(in) :: c_var_address(*)
    integer, intent(out) :: var_size
    integer(kind=c_int) :: bmi_status
    ! local
    character(len=LENMEMPATH) :: mem_path
    character(len=LENVARNAME) :: var_name_only
        
    call split_address(c_var_address, mem_path, var_name_only)
    
    bmi_status = BMI_SUCCESS
    call get_mem_elem_size(var_name_only, mem_path, var_size)
    if (var_size == -1) bmi_status = BMI_FAILURE
        
  end function get_var_itemsize

  !> @brief Get size of the variable, in bytes
  !!
  !! @param[in]    c_var_address     the memory address string of the variable
  !! @param[out]   var_nbytes        the size in bytes
  !! @return      bmi_status        the BMI status code
  !<
  function get_var_nbytes(c_var_address, var_nbytes) result(bmi_status) bind(C, name="get_var_nbytes")
  !DEC$ ATTRIBUTES DLLEXPORT :: get_var_nbytes
    character (kind=c_char), intent(in) :: c_var_address(*)
    integer, intent(out) :: var_nbytes
    integer(kind=c_int) :: bmi_status
    ! local
    integer(I4B) :: var_size, isize
    character(len=LENMEMPATH) :: mem_path
    character(len=LENVARNAME) :: var_name
        
    call split_address(c_var_address, mem_path, var_name)
    
    bmi_status = BMI_SUCCESS
    call get_mem_elem_size(var_name, mem_path, var_size)    
    if (var_size == -1) bmi_status = BMI_FAILURE
    call get_isize(var_name, mem_path, isize)
    if (isize == -1) bmi_status = BMI_FAILURE
    
    var_nbytes = var_size*isize
    
  end function get_var_nbytes


  !> @brief Get a pointer to the array of double precision numbers
  !!
  !! The array is located at @p c_var_address. There is no copying of data involved.
  !! Multi-dimensional arrays are supported and the get_var_rank() function 
  !! can be used to get the variable's dimensionality.
  !!
  !! @param[in]     c_var_address     the memory address string of the variable
  !! @param[out]    c_arr_ptr         the pointer to the array
  !! @return        bmi_status        the BMI status code
  !<
  function get_value_ptr_double(c_var_address, c_arr_ptr) result(bmi_status) bind(C, name="get_value_ptr_double")
  !DEC$ ATTRIBUTES DLLEXPORT :: get_value_ptr_double
    character (kind=c_char), intent(in) :: c_var_address(*)
    type(c_ptr), intent(inout) :: c_arr_ptr
    integer(kind=c_int) :: bmi_status
    ! local
    character(len=LENMEMPATH) :: mem_path
    character(len=LENVARNAME) :: var_name
    real(DP), pointer :: scalar_ptr
    real(DP), dimension(:), pointer, contiguous :: array_ptr
    real(DP), dimension(:,:), pointer, contiguous :: array2D_ptr
    integer(I4B) :: rank
    
    call split_address(c_var_address, mem_path, var_name)
    
    rank = -1
    call get_mem_rank(var_name, mem_path, rank)
    if (rank == 0) then
      call mem_setptr(scalar_ptr, var_name, mem_path)
      c_arr_ptr = c_loc(scalar_ptr)
    else if (rank == 1) then
      call mem_setptr(array_ptr, var_name, mem_path)
      c_arr_ptr = c_loc(array_ptr)
    else if (rank == 2) then
      call mem_setptr(array2D_ptr, var_name, mem_path)
      c_arr_ptr = c_loc(array2D_ptr)
    else
      bmi_status = BMI_FAILURE
      return
    end if
    bmi_status = BMI_SUCCESS    
    
  end function get_value_ptr_double
  

  !> @brief Get a pointer to the array of integer numbers
  !!
  !! The array is located at @p c_var_address. There is no copying of data involved.
  !! Multi-dimensional arrays are supported and the get_var_rank() function 
  !! can be used to get the variable's dimensionality.
  !!
  !! @param[in]     c_var_address     the memory address string of the variable
  !! @param[out]    c_arr_ptr         the pointer to the array
  !! @return        bmi_status        the BMI status code
  !<
  function get_value_ptr_int(c_var_address, c_arr_ptr) result(bmi_status) bind(C, name="get_value_ptr_int")
  !DEC$ ATTRIBUTES DLLEXPORT :: get_value_ptr_int
    character (kind=c_char), intent(in) :: c_var_address(*)    
    type(c_ptr), intent(inout) :: c_arr_ptr
    integer(kind=c_int) :: bmi_status
    ! local
    character(len=LENMEMPATH) :: mem_path
    character(len=LENVARNAME) :: var_name
    integer(I4B) :: rank
    integer(I4B), pointer :: scalar_ptr
    integer(I4B), dimension(:), pointer, contiguous :: array_ptr
    integer(I4B), dimension(:,:), pointer, contiguous :: array2D_ptr
    
    call split_address(c_var_address, mem_path, var_name)
    
    rank = -1
    call get_mem_rank(var_name, mem_path, rank)
        
    if (rank == 0) then
      call mem_setptr(scalar_ptr, var_name, mem_path)      
      c_arr_ptr = c_loc(scalar_ptr)
    else if (rank == 1) then
      call mem_setptr(array_ptr, var_name, mem_path)
      c_arr_ptr = c_loc(array_ptr)
    else if (rank == 2) then
      call mem_setptr(array_ptr, var_name, mem_path)
      c_arr_ptr = c_loc(array2D_ptr)
    else
      bmi_status = BMI_FAILURE
      return
    end if
    
    bmi_status = BMI_SUCCESS
    
  end function get_value_ptr_int
  
  !> @brief Get the variable type as a string
  !!
  !! The type returned is that of a single element. Currently we
  !! support 'INTEGER' and 'DOUBLE'.  When the variable cannot
  !! be found, the string 'UNKNOWN' is assigned.
  !!
  !! @param[in]     c_var_address     the memory address string of the variable
  !! @param[out]    c_var_type        the variable type as a string
  !! @return        bmi_status        the BMI status code 
  !<
  function get_var_type(c_var_address, c_var_type) result(bmi_status) bind(C, name="get_var_type")
  !DEC$ ATTRIBUTES DLLEXPORT :: get_var_type
    use ConstantsModule, only: LENMEMTYPE
    character (kind=c_char), intent(in) :: c_var_address(*)
    character (kind=c_char), intent(out) :: c_var_type(BMI_LENVARTYPE)
    integer(kind=c_int) :: bmi_status    
    ! local
    character(len=LENMEMPATH) :: mem_path
    character(len=LENVARNAME) :: var_name
    character(len=LENMEMTYPE) :: mem_type
    
    call split_address(c_var_address, mem_path, var_name)
    
    bmi_status = BMI_SUCCESS
    call get_mem_type(var_name, mem_path, mem_type)
    c_var_type(1:len(trim(mem_type))+1) = string_to_char_array(trim(mem_type), len(trim(mem_type)))

    if (mem_type == 'UNKNOWN') then
      bmi_status = BMI_FAILURE
    end if

  end function get_var_type
    
  !> @brief Get the variable rank (non-BMI)
  !!
  !! In order to support multi-dimensional arrays, this function gives
  !! access to the rank of the array.
  !!
  !! @param[in]     c_var_address     the memory address string of the variable
  !! @param[out]    c_var_rank        the variable rank
  !! @return        bmi_status        the BMI status code 
  !<
  function get_var_rank(c_var_address, c_var_rank) result(bmi_status) bind(C, name="get_var_rank")
  !DEC$ ATTRIBUTES DLLEXPORT :: get_var_rank
    character (kind=c_char), intent(in) :: c_var_address(*)
    integer(kind=c_int), intent(out) :: c_var_rank
    integer(kind=c_int) :: bmi_status
    ! local
    character(len=LENMEMPATH) :: mem_path
    character(len=LENVARNAME) :: var_name
    
    call split_address(c_var_address, mem_path, var_name)
    
    call get_mem_rank(var_name, mem_path, c_var_rank)
    if (c_var_rank == -1) then
        bmi_status = BMI_FAILURE
        return
    end if
    
    bmi_status = BMI_SUCCESS
    
  end function get_var_rank
  

  !> @brief Get the shape of the array for the variable (non-BMI)
  !!
  !! The shape is an integer array with size equal to the rank of 
  !! the variable (see get_var_rank()) and values that give the
  !! length of the array in each dimension. The target shape array
  !! @p c_var_shape should be allocated before calling this routine.
  !!
  !! @param[in]     c_var_address     the memory address string of the variable
  !! @param[out]    c_var_shape       the 1D array with the variable's shape
  !! @return        bmi_status        the BMI status code
  !<
  function get_var_shape(c_var_address, c_var_shape) result(bmi_status) bind(C, name="get_var_shape")
  !DEC$ ATTRIBUTES DLLEXPORT :: get_var_shape
    use ConstantsModule, only: MAXMEMRANK
    character (kind=c_char), intent(in) :: c_var_address(*)
    integer(c_int), intent(inout) :: c_var_shape(*)
    integer(kind=c_int) :: bmi_status
    ! local
    integer(I4B), dimension(MAXMEMRANK) :: var_shape
    integer(I4B) :: var_rank
    character(len=LENMEMPATH) :: mem_path
    character(len=LENVARNAME) :: var_name
        
    call split_address(c_var_address, mem_path, var_name)
    
    var_shape = 0
    var_rank = 0
    call get_mem_rank(var_name, mem_path, var_rank)
    call get_mem_shape(var_name, mem_path, var_shape)
    if (var_shape(1) == -1 .or. var_rank == -1) then
      bmi_status = BMI_FAILURE
      return
    end if
        
    ! External calls to this BMI are assumed C style, so if the internal shape 
    ! is (100,1) we get (100,1,undef) from the call get_mem_shape
    ! This we need to convert to C-style which should be (1,100).
    ! Hence, we reverse the array and drop undef:
    c_var_shape(1:var_rank) = var_shape(var_rank:1:-1)
    
    bmi_status = BMI_SUCCESS
    
  end function get_var_shape  
 
end module mf6bmi
