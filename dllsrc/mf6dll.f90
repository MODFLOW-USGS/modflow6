module mf6dll
  use mf6core
  use bmif, only: BMI_SUCCESS, BMI_FAILURE
  use iso_c_binding, only: c_int, c_char, c_double, C_NULL_CHAR, c_loc, c_ptr
  use KindModule, only: DP, I4B
  use ConstantsModule, only: LENORIGIN, LENVARNAME, LENMODELNAME, MAXCHARLEN
  implicit none
  
  
  ! Define global constants
  
  integer(c_int), BIND(C, name="MAXSTRLEN") :: MAXSTRLEN = MAXCHARLEN
  !DEC$ ATTRIBUTES DLLEXPORT :: MAXSTRLEN
  
  contains    
  
  ! initialize the computational core, assuming to have the configuration 
  ! file 'mfsim.nam' in the working directory
  ! TODO_MJR: or do we use the path?
  function bmi_initialize() result(bmi_status) bind(C, name="initialize")
  !DEC$ ATTRIBUTES DLLEXPORT :: bmi_initialize
    integer(kind=c_int) :: bmi_status
      
    call initialize()
    bmi_status = BMI_SUCCESS
    
  end function bmi_initialize
  
  ! perform a time step
  function bmi_update() result(bmi_status) bind(C, name="update")
  !DEC$ ATTRIBUTES DLLEXPORT :: bmi_update
    integer(kind=c_int) :: bmi_status
    ! local
    logical :: hasConverged
    
    hasConverged = update()
    if (hasConverged) then
      bmi_status = BMI_SUCCESS
    else
      bmi_status = BMI_FAILURE
    end if
    
  end function bmi_update
     
  ! Perform teardown tasks for the model.
  function bmi_finalize() result(bmi_status) bind(C, name="finalize")
  !DEC$ ATTRIBUTES DLLEXPORT :: bmi_finalize
    integer(kind=c_int) :: bmi_status
    
    call finalize()
    bmi_status = BMI_SUCCESS
    
  end function bmi_finalize
  
  ! Start time of the model.
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
    use MemoryManagerModule, only: get_var_size
    character (kind=c_char), intent(in) :: c_var_name(*)
    integer, intent(out) :: var_size
    integer(kind=c_int) :: bmi_status
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
    integer(kind=c_int) :: bmi_status
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
    use MemoryManagerModule, only: mem_setptr
    character (kind=c_char), intent(in) :: c_var_name(*)    
    type(c_ptr), intent(inout) :: x
    integer(kind=c_int) :: bmi_status
    ! local
    integer :: idx, i
    character(len=LENORIGIN) :: origin, var_name
    character(len=LENVARNAME) :: var_name_only
    real(DP), dimension(:), pointer, contiguous :: arrayptr
    
    var_name = char_array_to_string(c_var_name, strlen(c_var_name))
    
    idx = index(var_name, '/', back=.true.)
    origin = var_name(:idx-1)
    var_name_only = var_name(idx+1:)
    call mem_setptr(arrayptr, var_name_only, origin)
    
    ! set the C pointer to the internal array
    x = c_loc(arrayptr)
    bmi_status = BMI_SUCCESS
    
  end function get_value_ptr_double
  
  function get_value_ptr_int(c_var_name, x) result(bmi_status) bind(C, name="get_value_ptr_int")
  !DEC$ ATTRIBUTES DLLEXPORT :: get_value_ptr_int
    use MemoryManagerModule, only: mem_setptr, get_var_rank
    character (kind=c_char), intent(in) :: c_var_name(*)    
    type(c_ptr), intent(inout) :: x
    integer(kind=c_int) :: bmi_status
    ! local
    integer :: idx, i
    character(len=LENORIGIN) :: origin, var_name
    character(len=LENVARNAME) :: var_name_only
    integer(I4B) :: rank
    integer(I4B), dimension(:), pointer, contiguous :: arrayptr
    integer(I4B), pointer :: scalarptr
    
    var_name = char_array_to_string(c_var_name, strlen(c_var_name))
    
    idx = index(var_name, '/', back=.true.)
    origin = var_name(:idx-1)
    var_name_only = var_name(idx+1:)
    
    rank = -1
    call get_var_rank(var_name_only, origin, rank)
    if (rank == -1 .or. rank > 1) then
        bmi_status = BMI_FAILURE
        return
    end if
    
    if (rank == 0) then
      ! set the C pointer to the internal scalar
      call mem_setptr(scalarptr, var_name_only, origin)      
      x = c_loc(scalarptr)
    else if (rank ==1) then    
      ! set the C pointer to the internal array
      call mem_setptr(arrayptr, var_name_only, origin)
      x = c_loc(arrayptr)
    end if
    
    bmi_status = BMI_SUCCESS
    
  end function get_value_ptr_int
  
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
    
    bmi_status = BMI_FAILURE
  end function get_var_grid
  
  ! Get the grid type as a string.
  function get_grid_type(grid_id, grid_type) result(bmi_status) bind(C, name="get_grid_type")
  !DEC$ ATTRIBUTES DLLEXPORT :: get_grid_type
    use ListsModule, only: basemodellist
    use NumericalModelModule, only: NumericalModelType, GetNumericalModelFromList
    integer(kind=c_int), intent(in) :: grid_id
    character(kind=c_char), intent(out) :: grid_type(MAXSTRLEN)
    integer(kind=c_int) :: bmi_status
    ! local
    character(len=MAXSTRLEN) :: grid_type_f
    character(len=LENMODELNAME) :: model_name
    character(len=LENORIGIN) :: var_name
    integer :: i
    class(NumericalModelType), pointer :: numericalModel
    
    model_name = get_model_name(grid_id) 
    
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

    grid_type = string_to_char_array(trim(grid_type_f), len(trim(grid_type_f)))
    bmi_status = BMI_SUCCESS
  end function get_grid_type
  
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
    integer(I4B), dimension(:), pointer :: grid_shape
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
    integer(I4B), dimension(:), pointer :: grid_shape
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
  
  ! Get the dimensions of the computational grid.
  function get_grid_shape(grid_id, grid_shape) result(bmi_status) bind(C, name="get_grid_shape")
  !DEC$ ATTRIBUTES DLLEXPORT :: get_grid_shape
    use MemoryManagerModule, only: mem_setptr
    integer(kind=c_int), intent(in) :: grid_id
    type(c_ptr), intent(out) :: grid_shape
    integer(kind=c_int) :: bmi_status
    ! local
    integer, dimension(:), pointer :: grid_shape_ptr
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
    integer, dimension(:), pointer :: grid_shape_ptr
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
    integer, dimension(:), pointer :: grid_shape_ptr
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
    character(kind=c_char) :: grid_type(MAXSTRLEN)
    character(len=MAXSTRLEN) :: grid_type_f
    
    ! make sure function is only used for unstructured grids
    status = get_grid_type(grid_id, grid_type)
    grid_type_f = char_array_to_string(grid_type, strlen(grid_type))
    if (grid_type_f /= "unstructured") then
      bmi_status = BMI_FAILURE
      return
    end if    
    
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
    character(kind=c_char) :: grid_type(MAXSTRLEN)
    character(len=MAXSTRLEN) :: grid_type_f
    class(NumericalModelType), pointer :: numericalModel
    
    ! make sure function is only used for unstructured grids
    status = get_grid_type(grid_id, grid_type)
    grid_type_f = char_array_to_string(grid_type, strlen(grid_type))
    if (grid_type_f /= "unstructured") then
      bmi_status = BMI_FAILURE
      return
    end if
    
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
    character(kind=c_char) :: grid_type(MAXSTRLEN)
    integer, dimension(:), pointer, contiguous :: javert_ptr
    character(len=MAXSTRLEN) :: grid_type_f
    
    ! make sure function is only used for unstructured grids
    status = get_grid_type(grid_id, grid_type)
    grid_type_f = char_array_to_string(grid_type, strlen(grid_type))
    if (grid_type_f /= "unstructured") then
      bmi_status = BMI_FAILURE
      return
    end if
    
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
    character(kind=c_char) :: grid_type(MAXSTRLEN)
    integer, dimension(:), pointer, contiguous :: iavert_ptr
    character(len=MAXSTRLEN) :: grid_type_f
    integer, dimension(:), pointer, contiguous :: array_ptr
    ! TODO_MJR: this array will not work for multiple models, or multiple calls
    integer, dimension(:), target, allocatable, save :: array
    
    ! make sure function is only used for unstructured grids
    status = get_grid_type(grid_id, grid_type)
    grid_type_f = char_array_to_string(grid_type, strlen(grid_type))
    if (grid_type_f /= "unstructured") then
      bmi_status = BMI_FAILURE
      return
    end if
    
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
  
  pure function extract_model_name(var_name)
    character(len=*), intent(in) :: var_name
    character(len=LENMODELNAME) :: extract_model_name
    integer :: idx
    idx = index(var_name, ' ')
    extract_model_name = var_name(:idx-1)
  end function extract_model_name
  
  function get_model_name(grid_id)
    use ListsModule, only: basemodellist
    use BaseModelModule, only: BaseModelType, GetBaseModelFromList
    integer(kind=c_int), intent(in) :: grid_id
    character(len=LENMODELNAME) :: get_model_name
    ! local
    integer :: i
    class(BaseModelType), pointer :: baseModel    

    do i = 1,basemodellist%Count()
      baseModel => GetBaseModelFromList(basemodellist, i)
      if (baseModel%id == grid_id) then
        get_model_name = baseModel%name
        return
      end if
    end do
  end function get_model_name
  
  


end module mf6dll