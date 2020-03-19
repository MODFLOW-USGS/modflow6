! Module description:
! TODO_MJR
!
! Note on style: BMI apparently uses underscores, we use underscores in some 
! places but camelcase in other. Since this is a dedicated BMI interface module,
! we'll use underscores here as well.
module mf6bmi
  use Mf6CoreModule
  use bmif, only: BMI_SUCCESS, BMI_FAILURE
  use iso_c_binding, only: c_int, c_char, c_double, C_NULL_CHAR, c_loc, c_ptr
  use KindModule, only: DP, I4B
  use ConstantsModule, only: LENORIGIN, LENVARNAME, LENMODELNAME, MAXCHARLEN
  use SimVariablesModule, only: istdout
  use InputOutputModule, only: getunit
  implicit none
  
  ! Define global constants  
  integer(c_int), BIND(C, name="MAXSTRLEN") :: MAXSTRLEN = MAXCHARLEN
  !DEC$ ATTRIBUTES DLLEXPORT :: MAXSTRLEN
  
  contains  
  
  ! initialize the computational core, assuming to have the configuration 
  ! file 'mfsim.nam' in the working directory
  ! NOTE: initialize should be matched with a call to finalize, but there
  ! is currently no reason to believe that we can reinitialize a model in
  ! the same memory space... currently you would have to create a new process
  ! for that.
  function bmi_initialize() result(bmi_status) bind(C, name="initialize")
  !DEC$ ATTRIBUTES DLLEXPORT :: bmi_initialize
    integer(kind=c_int) :: bmi_status
    !
    ! -- set STDOUT to a physical file unit
    istdout = getunit()  
    !
    ! -- open stdout file mfsim.stdout
    open(unit=istdout, file='mfsim.stdout')
    !
    ! -- initialize MODFLOW 6
    call Mf6Initialize()
    bmi_status = BMI_SUCCESS
    
  end function bmi_initialize
  
  ! perform a computational time step, it will prepare the timestep and 
  ! then call sgp_ca (calculate) on all the solution groups in the simulation
  function bmi_update() result(bmi_status) bind(C, name="update")
  !DEC$ ATTRIBUTES DLLEXPORT :: bmi_update
    integer(kind=c_int) :: bmi_status
    ! local
    logical :: hasConverged
    
    hasConverged = Mf6Update()
    
    ! TODO_MJR: not sure about this. Should bmi_status only represent the
    ! state of the BMI, or does it include the state of the simulation?
    if (hasConverged) then
      bmi_status = BMI_SUCCESS
    else
      bmi_status = BMI_FAILURE
    end if
    
  end function bmi_update
     
  ! Perform teardown tasks for the model.
  function bmi_finalize() result(bmi_status) bind(C, name="finalize")
  !DEC$ ATTRIBUTES DLLEXPORT :: bmi_finalize
    use SimVariablesModule, only: iforcestop, ireturnerr
    integer(kind=c_int) :: bmi_status
    
    ! we don't want a full stop() here, this disables it:    
    iforcestop = 0    
    call Mf6Finalize()
      
    bmi_status = BMI_SUCCESS
      
  end function bmi_finalize
  
  ! Start time of the model, as MODFLOW does not have internal time,
  ! this will currently be returning 0.0
  function get_start_time(time) result(bmi_status) bind(C, name="get_start_time")
  !DEC$ ATTRIBUTES DLLEXPORT :: get_start_time
    double precision, intent(out) :: time
    integer(kind=c_int) :: bmi_status
    
    time = 0.0_DP
    bmi_status = BMI_SUCCESS
    
  end function get_start_time
  
  ! End time of the model.
  function get_end_time(time) result(bmi_status) bind(C, name="get_end_time")
  !DEC$ ATTRIBUTES DLLEXPORT :: get_end_time
    use TdisModule, only: totalsimtime
    double precision, intent(out) :: time
    integer(kind=c_int) :: bmi_status
    
    time = totalsimtime
    bmi_status = BMI_SUCCESS
    
  end function get_end_time

  ! Current time of the model.
  function get_current_time(time) result(bmi_status) bind(C, name="get_current_time")
  !DEC$ ATTRIBUTES DLLEXPORT :: get_current_time
    use TdisModule, only: totim
    double precision, intent(out) :: time
    integer(kind=c_int) :: bmi_status
    
    time = totim
    bmi_status = BMI_SUCCESS    
    
  end function get_current_time
  
  ! Get memory use per array element, in bytes.
  function get_var_itemsize(c_var_name, var_size) result(bmi_status) bind(C, name="get_var_itemsize")
  !DEC$ ATTRIBUTES DLLEXPORT :: get_var_itemsize
    use MemoryManagerModule, only: get_mem_size
    character (kind=c_char), intent(in) :: c_var_name(*)
    integer, intent(out) :: var_size
    integer(kind=c_int) :: bmi_status
    ! local
    character(len=LENORIGIN) :: origin
    character(len=LENVARNAME) :: var_name_only
        
    call split_c_var_name(c_var_name, origin, var_name_only)
    
    bmi_status = BMI_SUCCESS
    call get_mem_size(var_name_only, origin, var_size)    
    if (var_size == -1) bmi_status = BMI_FAILURE
        
  end function get_var_itemsize

  ! Get size of the given variable, in bytes.
  function get_var_nbytes(c_var_name, var_nbytes) result(bmi_status) bind(C, name="get_var_nbytes")
  !DEC$ ATTRIBUTES DLLEXPORT :: get_var_nbytes
    use MemoryManagerModule, only: get_mem_size, get_isize
    character (kind=c_char), intent(in) :: c_var_name(*)
    integer, intent(out) :: var_nbytes
    integer(kind=c_int) :: bmi_status
    ! local
    integer :: var_size, isize
    character(len=LENORIGIN) :: origin
    character(len=LENVARNAME) :: var_name_only
        
    call split_c_var_name(c_var_name, origin, var_name_only)
    
    bmi_status = BMI_SUCCESS
    call get_mem_size(var_name_only, origin, var_size)    
    if (var_size == -1) bmi_status = BMI_FAILURE
    call get_isize(var_name_only, origin, isize)
    if (isize == -1) bmi_status = BMI_FAILURE
    
    var_nbytes = var_size*isize
    
  end function get_var_nbytes


  ! set the pointer to the array of the given double variable, there
  ! is no copying of data involved!!
  function get_value_ptr_double(c_var_name, x) result(bmi_status) bind(C, name="get_value_ptr_double")
  !DEC$ ATTRIBUTES DLLEXPORT :: get_value_ptr_double
    use MemoryManagerModule, only: mem_setptr, get_mem_rank
    character (kind=c_char), intent(in) :: c_var_name(*)
    type(c_ptr), intent(inout) :: x
    integer(kind=c_int) :: bmi_status
    ! local
    integer :: i
    character(len=LENORIGIN) :: origin
    character(len=LENVARNAME) :: var_name_only
    real(DP), pointer :: dblptr
    real(DP), dimension(:), pointer, contiguous :: arrayptr
    real(DP), dimension(:,:), pointer, contiguous :: arrayptr2D
    integer(I4B) :: rank
    
    call split_c_var_name(c_var_name, origin, var_name_only)
    
    rank = -1
    call get_mem_rank(var_name_only, origin, rank)
    if (rank == 0) then
      call mem_setptr(dblptr, var_name_only, origin)
      x = c_loc(dblptr)
    else if (rank == 1) then
      call mem_setptr(arrayptr, var_name_only, origin)
      x = c_loc(arrayptr)
    else if (rank == 2) then
      call mem_setptr(arrayptr2D, var_name_only, origin)
      x = c_loc(arrayptr2D)
    else
      bmi_status = BMI_FAILURE
      return
    end if
    bmi_status = BMI_SUCCESS    
    
  end function get_value_ptr_double
  
  ! set the pointer to the array of the given integer variable, there
  ! is no copying of data involved!!
  !
  ! NB: in the future this might merge with get_value_ptr_double, we could 
  ! dispatch on the type ourselves and the c_ptr will work for both...  
  function get_value_ptr_int(c_var_name, x) result(bmi_status) bind(C, name="get_value_ptr_int")
  !DEC$ ATTRIBUTES DLLEXPORT :: get_value_ptr_int
    use MemoryManagerModule, only: mem_setptr, get_mem_rank
    character (kind=c_char), intent(in) :: c_var_name(*)    
    type(c_ptr), intent(inout) :: x
    integer(kind=c_int) :: bmi_status
    ! local
    integer :: i
    character(len=LENORIGIN) :: origin
    character(len=LENVARNAME) :: var_name_only
    integer(I4B) :: rank
    integer(I4B), pointer :: scalarptr
    integer(I4B), dimension(:), pointer, contiguous :: arrayptr
    integer(I4B), dimension(:,:), pointer, contiguous :: arrayptr2D
    
    call split_c_var_name(c_var_name, origin, var_name_only)
    
    rank = -1
    call get_mem_rank(var_name_only, origin, rank)
        
    if (rank == 0) then
      call mem_setptr(scalarptr, var_name_only, origin)      
      x = c_loc(scalarptr)
    else if (rank == 1) then
      call mem_setptr(arrayptr, var_name_only, origin)
      x = c_loc(arrayptr)
    else if (rank == 2) then
      call mem_setptr(arrayptr, var_name_only, origin)
      x = c_loc(arrayptr2D)
    else
      bmi_status = BMI_FAILURE
      return
    end if
    
    bmi_status = BMI_SUCCESS
    
  end function get_value_ptr_int
  
  function get_var_type(c_var_name, c_var_type) result(bmi_status) bind(C, name="get_var_type")
  !DEC$ ATTRIBUTES DLLEXPORT :: get_var_type
    use MemoryManagerModule, only: get_mem_type
    use ConstantsModule, only: LENMEMTYPE
    character (kind=c_char), intent(in) :: c_var_name(*)
    character (kind=c_char), intent(out) :: c_var_type(MAXSTRLEN)
    integer(kind=c_int) :: bmi_status    
    ! local
    character(len=LENORIGIN) :: origin
    character(len=LENVARNAME) :: var_name_only
    character(len=LENMEMTYPE) :: mem_type
    
    call split_c_var_name(c_var_name, origin, var_name_only)
    
    bmi_status = BMI_SUCCESS
    call get_mem_type(var_name_only, origin, mem_type)
    c_var_type = string_to_char_array(trim(mem_type), len(trim(mem_type)))
    
  end function get_var_type
  
  ! TODO_MJR: this isn't BMI, move?
  function get_var_rank(c_var_name, c_var_rank) result(bmi_status) bind(C, name="get_var_rank")
  !DEC$ ATTRIBUTES DLLEXPORT :: get_var_rank
    use MemoryManagerModule, only: get_mem_rank
    character (kind=c_char), intent(in) :: c_var_name(*)
    integer(kind=c_int), intent(out) :: c_var_rank
    integer(kind=c_int) :: bmi_status
    ! local
    character(len=LENORIGIN) :: origin
    character(len=LENVARNAME) :: var_name_only
    
    call split_c_var_name(c_var_name, origin, var_name_only)
    
    call get_mem_rank(var_name_only, origin, c_var_rank)
    if (c_var_rank == -1) then
        bmi_status = BMI_FAILURE
        return
    end if
    
    bmi_status = BMI_SUCCESS
    
  end function get_var_rank
  
  ! TODO_MJR: this isn't BMI, move? 
  function get_var_shape(c_var_name, c_var_shape) result(bmi_status) bind(C, name="get_var_shape")
  !DEC$ ATTRIBUTES DLLEXPORT :: get_var_shape
    use ConstantsModule, only: MAXMEMRANK
    use MemoryManagerModule, only: get_mem_shape, get_mem_rank
    character (kind=c_char), intent(in) :: c_var_name(*)
    integer(c_int), intent(inout) :: c_var_shape(*)
    integer(kind=c_int) :: bmi_status
    ! local
    integer(I4B), dimension(MAXMEMRANK) :: var_shape
    integer :: var_rank
    character(len=LENORIGIN) :: origin
    character(len=LENVARNAME) :: var_name_only
        
    call split_c_var_name(c_var_name, origin, var_name_only)
    
    var_shape = 0
    var_rank = 0
    call get_mem_rank(var_name_only, origin, var_rank)
    call get_mem_shape(var_name_only, origin, var_shape)
    if (var_shape(1) == -1 .or. var_rank == -1) then
      bmi_status = BMI_FAILURE
      return
    end if
        
    ! The user of the BMI is assumed C style, so if the internal shape 
    ! is (100,1) we get (100,1,undef) from the call get_mem_shape
    ! This we need to revert to C-style which should be (1,100) 
    ! hence, we reverse the array and drop undef
    c_var_shape(1:var_rank) = var_shape(var_rank:1:-1)
    bmi_status = BMI_SUCCESS
    
  end function get_var_shape
   
  
  ! Get the grid identifier for the given variable.
  function get_var_grid(c_var_name, var_grid) result(bmi_status) bind(C, name="get_var_grid")
  !DEC$ ATTRIBUTES DLLEXPORT :: get_var_grid
    use ListsModule, only: basemodellist
    use BaseModelModule, only: BaseModelType, GetBaseModelFromList
    character (kind=c_char), intent(in) :: c_var_name(*) 
    integer(kind=c_int), intent(out) :: var_grid
    integer(kind=c_int) :: bmi_status
    ! local
    character(len=LENMODELNAME) :: model_name
    character(len=LENORIGIN) :: var_name
    integer :: i
    class(BaseModelType), pointer :: baseModel
    
    var_name = char_array_to_string(c_var_name, strlen(c_var_name))    
    model_name = extract_model_name(var_name)
    
    var_grid = 0
    do i = 1,basemodellist%Count()
      baseModel => GetBaseModelFromList(basemodellist, i)
      if (baseModel%name == model_name) then
        var_grid = baseModel%id
        bmi_status = BMI_SUCCESS
        return
      end if
    end do
    
    ! TODO_MJR: some variables will not have a model associated, 
    ! but maybe a numerical solution instead, e.g. head "X", and then
    ! even have multiple grids (from multiple models)
    ! How should this work?
    
    bmi_status = BMI_FAILURE
  end function get_var_grid
  
  ! Get the grid type as a string.
  function get_grid_type(grid_id, grid_type) result(bmi_status) bind(C, name="get_grid_type")
  !DEC$ ATTRIBUTES DLLEXPORT :: get_grid_type  
    integer(kind=c_int), intent(in) :: grid_id
    character(kind=c_char), intent(out) :: grid_type(MAXSTRLEN)
    integer(kind=c_int) :: bmi_status
    ! local
    character(len=MAXSTRLEN) :: grid_type_f
    character(len=LENMODELNAME) :: model_name
    
    model_name = get_model_name(grid_id) 
    bmi_status = get_grid_type_model(model_name, grid_type_f)
    if (bmi_status == BMI_FAILURE) return
    
    grid_type = string_to_char_array(trim(grid_type_f), len(trim(grid_type_f)))
    
  end function get_grid_type
  
  ! internal helper function to return the grid type for a 
  ! named model as a fortran string following BMI convention
  function get_grid_type_model(model_name, grid_type_f) result(bmi_status)
    use ListsModule, only: basemodellist
    use NumericalModelModule, only: NumericalModelType, GetNumericalModelFromList
    character(len=LENMODELNAME) :: model_name
    character(len=MAXSTRLEN) :: grid_type_f
    integer(kind=c_int) :: bmi_status
    ! local
    integer :: i    
    class(NumericalModelType), pointer :: numericalModel
    
    do i = 1,basemodellist%Count()
      numericalModel => GetNumericalModelFromList(basemodellist, i)
      if (numericalModel%name == model_name) then
        call numericalModel%dis%get_dis_type(grid_type_f)
      end if
    end do
    
    if (grid_type_f == "DIS") then
      grid_type_f = "rectilinear"
    else if ((grid_type_f == "DISV") .or. (grid_type_f == "DISU")) then
      grid_type_f = "unstructured"
    else
      bmi_status = BMI_FAILURE
      return
    end if
    bmi_status = BMI_SUCCESS
    
  end function get_grid_type_model
  
  !TODO_JH: Currently only works for rectilinear grids
  ! Get number of dimensions of the computational grid.
  function get_grid_rank(grid_id, grid_rank) result(bmi_status) bind(C, name="get_grid_rank")
  !DEC$ ATTRIBUTES DLLEXPORT :: get_grid_rank
    use MemoryManagerModule, only: mem_setptr
    integer(kind=c_int), intent(in) :: grid_id
    integer(kind=c_int), intent(out) :: grid_rank
    integer(kind=c_int) :: bmi_status
    ! local
    character(len=LENMODELNAME) :: model_name
    integer(I4B), dimension(:), pointer, contiguous :: grid_shape
    character(kind=c_char) :: grid_type(MAXSTRLEN)
    
    ! make sure function is only used for implemented grid_types
    if (get_grid_type(grid_id, grid_type) /= BMI_SUCCESS) then
      bmi_status = BMI_FAILURE
      return
    end if
    
    ! get shape array
    model_name = get_model_name(grid_id)
    call mem_setptr(grid_shape, "MSHAPE", trim(model_name) // " DIS")
    
    if (grid_shape(1) == 1) then
      grid_rank = 2
    else    
      grid_rank = 3
    end if
    
    bmi_status = BMI_SUCCESS
  end function get_grid_rank
  
  ! Get the total number of elements in the computational grid.
  function get_grid_size(grid_id, grid_size) result(bmi_status) bind(C, name="get_grid_size")
  !DEC$ ATTRIBUTES DLLEXPORT :: get_grid_size
    use MemoryManagerModule, only: mem_setptr
    integer(kind=c_int), intent(in) :: grid_id
    integer(kind=c_int), intent(out) :: grid_size
    integer(kind=c_int) :: bmi_status
    ! local
    character(len=LENMODELNAME) :: model_name
    integer(I4B), dimension(:), pointer, contiguous :: grid_shape
    character(kind=c_char) :: grid_type(MAXSTRLEN)
    character(len=MAXSTRLEN) :: grid_type_f
    integer :: status
    
    ! make sure function is only used for implemented grid_types
    if (get_grid_type(grid_id, grid_type) /= BMI_SUCCESS) then
      bmi_status = BMI_FAILURE
      return
    end if
    grid_type_f = char_array_to_string(grid_type, strlen(grid_type))
        
    model_name = get_model_name(grid_id)
    
    if (grid_type_f == "rectilinear") then
      call mem_setptr(grid_shape, "MSHAPE", trim(model_name) // " DIS")
      grid_size = grid_shape(1) * grid_shape(2) * grid_shape(3)
      bmi_status = BMI_SUCCESS
    else if (grid_type_f == "unstructured") then
      status = get_grid_node_count(grid_id, grid_size)
      bmi_status = BMI_SUCCESS
    else
      bmi_status = BMI_FAILURE
    end if
  end function get_grid_size
  
  ! TODO_MJR: refactor this, grid_shape should be copied into an externally
  ! allocated array (same for get_grid_x and get_grid_y)
  !
  ! Get the dimensions of the computational grid.
  function get_grid_shape(grid_id, grid_shape) result(bmi_status) bind(C, name="get_grid_shape")
  !DEC$ ATTRIBUTES DLLEXPORT :: get_grid_shape
    use MemoryManagerModule, only: mem_setptr
    integer(kind=c_int), intent(in) :: grid_id
    type(c_ptr), intent(out) :: grid_shape
    integer(kind=c_int) :: bmi_status
    ! local
    integer, dimension(:), pointer, contiguous :: grid_shape_ptr
    integer, dimension(:), target, allocatable, save :: array
    character(len=LENMODELNAME) :: model_name
    character(kind=c_char) :: grid_type(MAXSTRLEN)
    
    ! make sure function is only used for implemented grid_types
    if (get_grid_type(grid_id, grid_type) /= BMI_SUCCESS) then
      bmi_status = BMI_FAILURE
      return
    end if
    
    ! get shape array
    model_name = get_model_name(grid_id)
    call mem_setptr(grid_shape_ptr, "MSHAPE", trim(model_name) // " DIS")
    
    ! TODO_MJR: review this!
    if (grid_shape_ptr(1) == 1) then
      array = grid_shape_ptr(2:3)
      grid_shape_ptr => array
    end if
    
    grid_shape = c_loc(grid_shape_ptr)
    bmi_status = BMI_SUCCESS
  end function get_grid_shape
  

  ! Provides an array (whose length is the number of rows) that gives the x-coordinate for each row.
  function get_grid_x(grid_id, grid_x) result(bmi_status) bind(C, name="get_grid_x")
  !DEC$ ATTRIBUTES DLLEXPORT :: get_grid_x
    use MemoryManagerModule, only: mem_setptr
    integer(kind=c_int), intent(in) :: grid_id
    type(c_ptr), intent(out) :: grid_x
    integer(kind=c_int) :: bmi_status
    ! local
    integer :: i
    integer, dimension(:), pointer, contiguous :: grid_shape_ptr
    real(DP), dimension(:), pointer, contiguous :: array_ptr
    real(DP), dimension(:), target, allocatable, save :: array
    character(len=LENMODELNAME) :: model_name
    character(kind=c_char) :: grid_type(MAXSTRLEN)
    real(DP), dimension(:,:), pointer, contiguous :: vertices_ptr
    character(len=MAXSTRLEN) :: grid_type_f
    
    ! make sure function is only used for implemented grid_types
    if (get_grid_type(grid_id, grid_type) /= BMI_SUCCESS) then
      bmi_status = BMI_FAILURE
      return
    end if
    grid_type_f = char_array_to_string(grid_type, strlen(grid_type))
    
    model_name = get_model_name(grid_id)
    if (grid_type_f == "rectilinear") then      
      call mem_setptr(grid_shape_ptr, "MSHAPE", trim(model_name) // " DIS")
      array = [ (i, i=0,grid_shape_ptr(size(grid_shape_ptr))) ]      
    else if (grid_type_f == "unstructured") then
      call mem_setptr(vertices_ptr, "VERTICES", trim(model_name) // " DIS")
      array = vertices_ptr(1, :)
    else
      bmi_status = BMI_FAILURE
      return
    end if
    
    array_ptr => array
    grid_x = c_loc(array_ptr)
    bmi_status = BMI_SUCCESS
  
  end function get_grid_x
  
  ! Provides an array (whose length is the number of rows) that gives the y-coordinate for each row.
  function get_grid_y(grid_id, grid_y) result(bmi_status) bind(C, name="get_grid_y")
  !DEC$ ATTRIBUTES DLLEXPORT :: get_grid_y
    use MemoryManagerModule, only: mem_setptr
    integer(kind=c_int), intent(in) :: grid_id
    type(c_ptr), intent(out) :: grid_y
    integer(kind=c_int) :: bmi_status
    ! local
    integer :: i
    integer, dimension(:), pointer, contiguous :: grid_shape_ptr
    real(DP), dimension(:), pointer, contiguous :: array_ptr
    real(DP), dimension(:), target, allocatable, save :: array
    character(len=LENMODELNAME) :: model_name
    character(kind=c_char) :: grid_type(MAXSTRLEN)
    real(DP), dimension(:,:), pointer, contiguous :: vertices_ptr
    character(len=MAXSTRLEN) :: grid_type_f
    
    ! make sure function is only used for implemented grid_types
    if (get_grid_type(grid_id, grid_type) /= BMI_SUCCESS) then
      bmi_status = BMI_FAILURE
      return
    end if
    grid_type_f = char_array_to_string(grid_type, strlen(grid_type))
    
    model_name = get_model_name(grid_id)
    if (grid_type_f == "rectilinear") then      
      call mem_setptr(grid_shape_ptr, "MSHAPE", trim(model_name) // " DIS")
      array = [ (i, i=grid_shape_ptr(size(grid_shape_ptr)-1),0,-1) ]      
    else if (grid_type_f == "unstructured") then
      call mem_setptr(vertices_ptr, "VERTICES", trim(model_name) // " DIS")
      array = vertices_ptr(2, :)
    else
      bmi_status = BMI_FAILURE
      return
    end if
    
    array_ptr => array
    grid_y = c_loc(array_ptr)
    bmi_status = BMI_SUCCESS
  end function get_grid_y
  

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
  
  ! NOTE: node in BMI-terms is a vertex in Modflow terms
  ! Get the number of nodes in an unstructured grid.
  function get_grid_node_count(grid_id, count) result(bmi_status) bind(C, name="get_grid_node_count")
  !DEC$ ATTRIBUTES DLLEXPORT :: get_grid_node_count
    use MemoryManagerModule, only: mem_setptr
    integer(kind=c_int), intent(in) :: grid_id
    integer(kind=c_int), intent(out) :: count
    integer(kind=c_int) :: bmi_status
    ! local
    character(len=LENMODELNAME) :: model_name
    integer :: status
    integer(I4B), pointer :: nvert_ptr
    
    ! make sure function is only used for unstructured grids
    bmi_status = BMI_FAILURE
    if (.not. confirm_grid_type(grid_id, "unstructured")) return   
    
    model_name = get_model_name(grid_id)
    call mem_setptr(nvert_ptr, "NVERT", trim(model_name) // " DIS")
    count = nvert_ptr
    bmi_status = BMI_SUCCESS  
  end function get_grid_node_count
  
  ! TODO_JH: This is a simplified implementation which ignores vertical face
  ! Get the number of faces in an unstructured grid.
  function get_grid_face_count(grid_id, count) result(bmi_status) bind(C, name="get_grid_face_count")
  !DEC$ ATTRIBUTES DLLEXPORT :: get_grid_face_count
    use ListsModule, only: basemodellist
    use NumericalModelModule, only: NumericalModelType, GetNumericalModelFromList
    integer(kind=c_int), intent(in) :: grid_id
    integer(kind=c_int), intent(out) :: count
    integer(kind=c_int) :: bmi_status
    ! local
    character(len=LENMODELNAME) :: model_name
    integer :: i
    integer :: status
    class(NumericalModelType), pointer :: numericalModel
    
    ! make sure function is only used for unstructured grids
    bmi_status = BMI_FAILURE
    if (.not. confirm_grid_type(grid_id, "unstructured")) return
    
    model_name = get_model_name(grid_id)    
    do i = 1,basemodellist%Count()
      numericalModel => GetNumericalModelFromList(basemodellist, i)
      if (numericalModel%name == model_name) then
        count = numericalModel%dis%nodes 
      end if
    end do  
    bmi_status = BMI_SUCCESS  
  end function get_grid_face_count
  
  ! Get the face-node connectivity.
  function get_grid_face_nodes(grid_id, face_nodes) result(bmi_status) bind(C, name="get_grid_face_nodes")
  !DEC$ ATTRIBUTES DLLEXPORT :: get_grid_face_nodes
    use MemoryManagerModule, only: mem_setptr
    integer(kind=c_int), intent(in) :: grid_id
    type(c_ptr), intent(out) :: face_nodes
    integer(kind=c_int) :: bmi_status
    ! local
    integer :: status
    character(len=LENMODELNAME) :: model_name
    integer, dimension(:), pointer, contiguous :: javert_ptr
    
    ! make sure function is only used for unstructured grids
    bmi_status = BMI_FAILURE
    if (.not. confirm_grid_type(grid_id, "unstructured")) return
    
    model_name = get_model_name(grid_id)
    call mem_setptr(javert_ptr, "JAVERT", trim(model_name) // " DIS")
    face_nodes = c_loc(javert_ptr)
    bmi_status = BMI_SUCCESS
  end function get_grid_face_nodes
  
  ! Get the number of nodes for each face.
  function get_grid_nodes_per_face(grid_id, nodes_per_face) result(bmi_status) bind(C, name="get_grid_nodes_per_face")
  !DEC$ ATTRIBUTES DLLEXPORT :: get_grid_nodes_per_face
    use MemoryManagerModule, only: mem_setptr
    integer(kind=c_int), intent(in) :: grid_id
    type(c_ptr), intent(out) :: nodes_per_face
    integer(kind=c_int) :: bmi_status
    ! local
    integer :: i
    integer :: status
    character(len=LENMODELNAME) :: model_name
    integer, dimension(:), pointer, contiguous :: iavert_ptr
    integer, dimension(:), pointer, contiguous :: array_ptr
    ! TODO_MJR: this array will not work for multiple models, or multiple calls,
    ! should we let the outside manage the memory?
    integer, dimension(:), target, allocatable, save :: array
    
    ! make sure function is only used for unstructured grids
    bmi_status = BMI_FAILURE
    if (.not. confirm_grid_type(grid_id, "unstructured")) return
    
    model_name = get_model_name(grid_id)
    call mem_setptr(iavert_ptr, "IAVERT", trim(model_name) // " DIS")
    
    if (allocated(array)) deallocate(array)
    allocate(array(size(iavert_ptr) - 1))
    do i = 2, size(iavert_ptr)
      array(i-1) = iavert_ptr(i) - iavert_ptr(i-1) - 1
    end do
    
    array_ptr => array
    nodes_per_face = c_loc(array_ptr)
    bmi_status = BMI_SUCCESS
  end function get_grid_nodes_per_face
  
  ! -----------------------------------------------------------------------
  ! convenience functions follow here, TODO_MJR: move to dedicated module?
  ! -----------------------------------------------------------------------
  
  ! Helper function to check the grid, not all bmi routines are implemented
  ! for all types of discretizations
  function confirm_grid_type(grid_id, expected_type) result(is_match)
    integer(kind=c_int), intent(in) :: grid_id
    character(kind=c_char), intent(in) :: expected_type(MAXSTRLEN) ! this is a C-style string
    logical :: is_match
    ! local
    integer :: status
    character(len=LENMODELNAME) :: model_name
    character(len=MAXSTRLEN) :: expected_type_f ! this is a fortran style string
    character(len=MAXSTRLEN) :: grid_type_f
    
    is_match = .false.
     
    model_name = get_model_name(grid_id)
    status = get_grid_type_model(model_name, grid_type_f) 
    
    ! careful comparison:
    expected_type_f = char_array_to_string(expected_type, strlen(expected_type))
    if (expected_type_f == grid_type_f) is_match = .true.
    
  end function confirm_grid_type
  
  ! splits the variable name from the full address string into
  ! an origin and name as used by the memory manager
  subroutine split_c_var_name(c_var_name, origin, var_name_only)
    character (kind=c_char), intent(in) :: c_var_name(*)
    character(len=LENORIGIN), intent(out) :: origin
    character(len=LENVARNAME), intent(out) :: var_name_only    
    ! local
    integer :: idx
    character(len=LENORIGIN) :: var_name    
    
    var_name = char_array_to_string(c_var_name, strlen(c_var_name))    
    idx = index(var_name, '/', back=.true.)
    origin = var_name(:idx-1)
    var_name_only = var_name(idx+1:)
    
  end subroutine split_c_var_name
  
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
  
  pure function string_to_char_array(string, length)
   integer(c_int),intent(in) :: length
   character(len=length), intent(in) :: string
   character(kind=c_char,len=1) :: string_to_char_array(length+1)
   integer :: i
   
   do i = 1, length
      string_to_char_array(i) = string(i:i)
   enddo
   string_to_char_array(length+1) = C_NULL_CHAR
   
  end function string_to_char_array
  
  ! get the model name from the string, assuming that it is
  ! the substring in front of the first space
  pure function extract_model_name(var_name)
    character(len=*), intent(in) :: var_name
    character(len=LENMODELNAME) :: extract_model_name
    integer :: idx
    
    idx = index(var_name, ' ')
    extract_model_name = var_name(:idx-1)
    
  end function extract_model_name
  
  function get_model_name(grid_id) result(model_name)
    use ListsModule, only: basemodellist
    use BaseModelModule, only: BaseModelType, GetBaseModelFromList
    integer(kind=c_int), intent(in) :: grid_id
    character(len=LENMODELNAME) :: model_name
    ! local
    integer :: i
    class(BaseModelType), pointer :: baseModel    

    model_name = ''
    
    do i = 1,basemodellist%Count()
      baseModel => GetBaseModelFromList(basemodellist, i)
      if (baseModel%id == grid_id) then
        model_name = baseModel%name
        return
      end if
    end do
    
    ! TODO_MJR: error message, should never get here...
    
  end function get_model_name
  
  


end module mf6bmi
