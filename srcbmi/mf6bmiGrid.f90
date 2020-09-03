!> @brief This module contains BMI routines to expose the MODFLOW 6 discretization
!!
!! NB: this module is experimental and still under development.
!<
module mf6bmiGrid  
  use bmif, only: BMI_SUCCESS, BMI_FAILURE
  use mf6bmiUtil
  use iso_c_binding, only: c_double, c_ptr, c_loc
  use ConstantsModule, only: LENMODELNAME, LENMEMPATH
  use KindModule, only: DP, I4B
  use MemoryManagerModule, only: mem_setptr
  use MemoryHelperModule, only: create_mem_path
  implicit none

contains

  ! Get the grid identifier for the given variable.
  function get_var_grid(c_var_address, var_grid) result(bmi_status) bind(C, name="get_var_grid")
  !DEC$ ATTRIBUTES DLLEXPORT :: get_var_grid
    use ListsModule, only: basemodellist
    use BaseModelModule, only: BaseModelType, GetBaseModelFromList
    character (kind=c_char), intent(in) :: c_var_address(*) 
    integer(kind=c_int), intent(out) :: var_grid
    integer(kind=c_int) :: bmi_status
    ! local
    character(len=LENMODELNAME) :: model_name
    character(len=LENMEMPATH) :: var_address
    integer(I4B) :: i
    class(BaseModelType), pointer :: baseModel
    
    var_address = char_array_to_string(c_var_address, strlen(c_var_address))    
    model_name = extract_model_name(var_address)
    
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
    integer(kind=c_int), intent(in) :: grid_id
    character(kind=c_char), intent(out) :: grid_type(BMI_LENGRIDTYPE)
    integer(kind=c_int) :: bmi_status
    ! local
    character(len=LENGRIDTYPE) :: grid_type_f
    character(len=LENMODELNAME) :: model_name
    
    model_name = get_model_name(grid_id)
    if (model_name == '') then      
      bmi_status = BMI_FAILURE
      return
    end if

    bmi_status = BMI_SUCCESS
    call get_grid_type_model(model_name, grid_type_f) 
    grid_type(1:len(trim(grid_type_f))+1) = string_to_char_array(trim(grid_type_f), len(trim(grid_type_f)))
    
  end function get_grid_type
  
  ! internal helper function to return the grid type for a 
  ! named model as a fortran string following BMI convention
  subroutine get_grid_type_model(model_name, grid_type_f)
    use ListsModule, only: basemodellist
    use NumericalModelModule, only: NumericalModelType, GetNumericalModelFromList
    character(len=LENMODELNAME) :: model_name
    character(len=LENGRIDTYPE) :: grid_type_f
    ! local
    integer(I4B) :: i    
    class(NumericalModelType), pointer :: numericalModel

    grid_type_f = "unknown"
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
    end if
    
  end subroutine get_grid_type_model
  
  ! TODO_JH: Currently only works for rectilinear grids
  ! Get number of dimensions of the computational grid.
  function get_grid_rank(grid_id, grid_rank) result(bmi_status) bind(C, name="get_grid_rank")
  !DEC$ ATTRIBUTES DLLEXPORT :: get_grid_rank
    integer(kind=c_int), intent(in) :: grid_id
    integer(kind=c_int), intent(out) :: grid_rank
    integer(kind=c_int) :: bmi_status
    ! local
    character(len=LENMODELNAME) :: model_name
    integer(I4B), dimension(:), pointer, contiguous :: grid_shape
    character(kind=c_char) :: grid_type(BMI_LENGRIDTYPE)
    
    bmi_status = BMI_FAILURE
    ! make sure function is only used for implemented grid_types
    if (get_grid_type(grid_id, grid_type) /= BMI_SUCCESS) return
    
    ! get shape array
    model_name = get_model_name(grid_id)
    call mem_setptr(grid_shape, "MSHAPE", create_mem_path(model_name, 'DIS'))
    
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
    integer(kind=c_int), intent(in) :: grid_id
    integer(kind=c_int), intent(out) :: grid_size
    integer(kind=c_int) :: bmi_status
    ! local
    character(len=LENMODELNAME) :: model_name
    integer(I4B), dimension(:), pointer, contiguous :: grid_shape
    character(kind=c_char) :: grid_type(BMI_LENGRIDTYPE)
    character(len=LENGRIDTYPE) :: grid_type_f
    integer(I4B) :: status
    
    bmi_status = BMI_FAILURE
    ! make sure function is only used for implemented grid_types
    if (get_grid_type(grid_id, grid_type) /= BMI_SUCCESS) return
    grid_type_f = char_array_to_string(grid_type, strlen(grid_type))
        
    model_name = get_model_name(grid_id)
    
    if (grid_type_f == "rectilinear") then
      call mem_setptr(grid_shape, "MSHAPE", create_mem_path(model_name, 'DIS'))
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
    integer(kind=c_int), intent(in) :: grid_id
    integer(kind=c_int), intent(out) :: grid_shape(*)
    integer(kind=c_int) :: bmi_status
    ! local
    integer, dimension(:), pointer, contiguous :: grid_shape_ptr
    character(len=LENMODELNAME) :: model_name
    character(kind=c_char) :: grid_type(BMI_LENGRIDTYPE)
    
    bmi_status = BMI_FAILURE
    ! make sure function is only used for implemented grid_types
    if (get_grid_type(grid_id, grid_type) /= BMI_SUCCESS) return
    
    ! get shape array
    model_name = get_model_name(grid_id)
    call mem_setptr(grid_shape_ptr, "MSHAPE", create_mem_path(model_name, 'DIS'))
    
    if (grid_shape_ptr(1) == 1) then
      grid_shape(1:2) = grid_shape_ptr(2:3)  ! 2D
    else
      grid_shape(1:3) = grid_shape_ptr       ! 3D
    end if

    bmi_status = BMI_SUCCESS
  end function get_grid_shape
  

  ! Provides an array (whose length is the number of rows) that gives the x-coordinate for each row.
  function get_grid_x(grid_id, grid_x) result(bmi_status) bind(C, name="get_grid_x")
  !DEC$ ATTRIBUTES DLLEXPORT :: get_grid_x
    integer(kind=c_int), intent(in) :: grid_id
    real(kind=c_double), intent(out) :: grid_x(*)
    integer(kind=c_int) :: bmi_status
    ! local
    integer(I4B) :: i
    integer, dimension(:), pointer, contiguous :: grid_shape_ptr
    character(len=LENMODELNAME) :: model_name
    character(kind=c_char) :: grid_type(BMI_LENGRIDTYPE)
    real(DP), dimension(:,:), pointer, contiguous :: vertices_ptr
    character(len=LENGRIDTYPE) :: grid_type_f
    integer(I4B) :: x_size
    
    bmi_status = BMI_FAILURE
    ! make sure function is only used for implemented grid_types
    if (get_grid_type(grid_id, grid_type) /= BMI_SUCCESS) return
    grid_type_f = char_array_to_string(grid_type, strlen(grid_type))
    
    model_name = get_model_name(grid_id)
    if (grid_type_f == "rectilinear") then      
      call mem_setptr(grid_shape_ptr, "MSHAPE", create_mem_path(model_name, 'DIS'))
      ! The dimension of x is in the last element of the shape array.
      ! + 1 because we count corners, not centers.
      x_size = grid_shape_ptr(size(grid_shape_ptr)) + 1
      grid_x(1:x_size) = [ (i, i=0,x_size-1) ]
    else if (grid_type_f == "unstructured") then
      call mem_setptr(vertices_ptr, "VERTICES", create_mem_path(model_name, 'DIS'))
      ! x-coordinates are in the 1st column
      x_size = size(vertices_ptr(1, :))
      grid_x(1:x_size) = vertices_ptr(1, :)
    else
      bmi_status = BMI_FAILURE
      return
    end if
    bmi_status = BMI_SUCCESS
  end function get_grid_x
  
  ! Provides an array (whose length is the number of rows) that gives the y-coordinate for each row.
  function get_grid_y(grid_id, grid_y) result(bmi_status) bind(C, name="get_grid_y")
  !DEC$ ATTRIBUTES DLLEXPORT :: get_grid_y
    integer(kind=c_int), intent(in) :: grid_id
    real(kind=c_double), intent(out) :: grid_y(*)
    integer(kind=c_int) :: bmi_status
    ! local
    integer(I4B) :: i
    integer, dimension(:), pointer, contiguous :: grid_shape_ptr
    character(len=LENMODELNAME) :: model_name
    character(kind=c_char) :: grid_type(BMI_LENGRIDTYPE)
    real(DP), dimension(:,:), pointer, contiguous :: vertices_ptr
    character(len=LENGRIDTYPE) :: grid_type_f
    integer(I4B) :: y_size
    
    bmi_status = BMI_FAILURE
    ! make sure function is only used for implemented grid_types
    if (get_grid_type(grid_id, grid_type) /= BMI_SUCCESS) return
    grid_type_f = char_array_to_string(grid_type, strlen(grid_type))
    
    model_name = get_model_name(grid_id)
    if (grid_type_f == "rectilinear") then      
      call mem_setptr(grid_shape_ptr, "MSHAPE", create_mem_path(model_name, 'DIS'))
      ! The dimension of y is in the second last element of the shape array.
      ! + 1 because we count corners, not centers.
      y_size = grid_shape_ptr(size(grid_shape_ptr-1)) + 1
      grid_y(1:y_size) = [ (i, i=y_size-1,0,-1) ]
    else if (grid_type_f == "unstructured") then
      call mem_setptr(vertices_ptr, "VERTICES", create_mem_path(model_name, 'DIS'))
      ! y-coordinates are in the 2nd column
      y_size = size(vertices_ptr(2, :))
      grid_y(1:y_size) = vertices_ptr(2, :)
    else
      bmi_status = BMI_FAILURE
      return
    end if
    bmi_status = BMI_SUCCESS
  end function get_grid_y
    
  ! NOTE: node in BMI-terms is a vertex in Modflow terms
  ! Get the number of nodes in an unstructured grid.
  function get_grid_node_count(grid_id, count) result(bmi_status) bind(C, name="get_grid_node_count")
  !DEC$ ATTRIBUTES DLLEXPORT :: get_grid_node_count    
    integer(kind=c_int), intent(in) :: grid_id
    integer(kind=c_int), intent(out) :: count
    integer(kind=c_int) :: bmi_status
    ! local
    character(len=LENMODELNAME) :: model_name
    integer(I4B), pointer :: nvert_ptr
    
    ! make sure function is only used for unstructured grids
    bmi_status = BMI_FAILURE
    if (.not. confirm_grid_type(grid_id, "unstructured")) return   
    
    model_name = get_model_name(grid_id)
    call mem_setptr(nvert_ptr, "NVERT", create_mem_path(model_name, 'DIS'))
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
    integer(I4B) :: i
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
    integer(kind=c_int), intent(in) :: grid_id
    integer(kind=c_int), intent(out) :: face_nodes(*)
    integer(kind=c_int) :: bmi_status
    ! local
    character(len=LENMODELNAME) :: model_name
    integer, dimension(:), pointer, contiguous :: javert_ptr
    integer, dimension(:), allocatable :: nodes_per_face
    integer :: face_count
    integer :: face_nodes_count
    
    
    ! make sure function is only used for unstructured grids
    bmi_status = BMI_FAILURE
    if (.not. confirm_grid_type(grid_id, "unstructured")) return
    
    model_name = get_model_name(grid_id)
    call mem_setptr(javert_ptr, "JAVERT", create_mem_path(model_name, 'DIS'))
    
    bmi_status = get_grid_face_count(grid_id, face_count)
    if (bmi_status == BMI_FAILURE) return
    
    allocate(nodes_per_face(face_count))
    bmi_status = get_grid_nodes_per_face(grid_id, nodes_per_face)
    if (bmi_status == BMI_FAILURE) return
    
    face_nodes_count = sum(nodes_per_face + 1)
    
    face_nodes(1:face_nodes_count) = javert_ptr(:)
    bmi_status = BMI_SUCCESS
  end function get_grid_face_nodes
  
  ! Get the number of nodes for each face.
  function get_grid_nodes_per_face(grid_id, nodes_per_face) result(bmi_status) bind(C, name="get_grid_nodes_per_face")
  !DEC$ ATTRIBUTES DLLEXPORT :: get_grid_nodes_per_face
    integer(kind=c_int), intent(in) :: grid_id
    integer(kind=c_int), intent(out) :: nodes_per_face(*)
    integer(kind=c_int) :: bmi_status
    ! local
    integer(I4B) :: i
    character(len=LENMODELNAME) :: model_name
    integer, dimension(:), pointer, contiguous :: iavert_ptr
    
    ! make sure function is only used for unstructured grids
    bmi_status = BMI_FAILURE
    if (.not. confirm_grid_type(grid_id, "unstructured")) return
    
    model_name = get_model_name(grid_id)
    call mem_setptr(iavert_ptr, "IAVERT", create_mem_path(model_name, 'DIS'))
    
    do i = 1, size(iavert_ptr)-1
      nodes_per_face(i) = iavert_ptr(i+1) - iavert_ptr(i) - 1
    end do
    bmi_status = BMI_SUCCESS
  end function get_grid_nodes_per_face
  
  
  ! Helper function to check the grid, not all bmi routines are implemented
  ! for all types of discretizations
  function confirm_grid_type(grid_id, expected_type) result(is_match)
    integer(kind=c_int), intent(in) :: grid_id
    character(kind=c_char), intent(in) :: expected_type(BMI_LENGRIDTYPE) ! this is a C-style string
    logical :: is_match
    ! local
    character(len=LENMODELNAME) :: model_name
    character(len=LENGRIDTYPE) :: expected_type_f ! this is a fortran style string
    character(len=LENGRIDTYPE) :: grid_type_f
    
    is_match = .false.
     
    model_name = get_model_name(grid_id)
    call get_grid_type_model(model_name, grid_type_f) 
    
    ! careful comparison:
    expected_type_f = char_array_to_string(expected_type, strlen(expected_type))
    if (expected_type_f == grid_type_f) is_match = .true.
    
  end function confirm_grid_type
  
end module mf6bmiGrid