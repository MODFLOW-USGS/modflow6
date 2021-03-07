module bmif

  implicit none

  integer, parameter :: BMI_MAX_COMPONENT_NAME = 2048
  integer, parameter :: BMI_MAX_VAR_NAME = 2048
  integer, parameter :: BMI_MAX_TYPE_NAME = 2048
  integer, parameter :: BMI_MAX_UNITS_NAME = 2048

  integer, parameter :: BMI_FAILURE = 1
  integer, parameter :: BMI_SUCCESS = 0

  type, abstract :: bmi
  contains
    procedure(bmif_get_component_name), deferred :: get_component_name
    procedure(bmif_get_input_var_names), deferred :: get_input_var_names
    procedure(bmif_get_output_var_names), deferred :: get_output_var_names
    procedure(bmif_initialize), deferred :: initialize
    procedure(bmif_finalize), deferred :: finalize
    procedure(bmif_get_start_time), deferred :: get_start_time
    procedure(bmif_get_end_time), deferred :: get_end_time
    procedure(bmif_get_current_time), deferred :: get_current_time
    procedure(bmif_get_time_step), deferred :: get_time_step
    procedure(bmif_get_time_units), deferred :: get_time_units
    procedure(bmif_update), deferred :: update
    procedure(bmif_update_frac), deferred :: update_frac
    procedure(bmif_update_until), deferred :: update_until
    procedure(bmif_get_var_grid), deferred :: get_var_grid
    procedure(bmif_get_grid_type), deferred :: get_grid_type
    procedure(bmif_get_grid_rank), deferred :: get_grid_rank
    procedure(bmif_get_grid_shape), deferred :: get_grid_shape
    procedure(bmif_get_grid_size), deferred :: get_grid_size
    procedure(bmif_get_grid_spacing), deferred :: get_grid_spacing
    procedure(bmif_get_grid_origin), deferred :: get_grid_origin
    procedure(bmif_get_grid_x), deferred :: get_grid_x
    procedure(bmif_get_grid_y), deferred :: get_grid_y
    procedure(bmif_get_grid_z), deferred :: get_grid_z
    procedure(bmif_get_grid_connectivity), deferred :: get_grid_connectivity
    procedure(bmif_get_grid_offset), deferred :: get_grid_offset
    procedure(bmif_get_var_type), deferred :: get_var_type
    procedure(bmif_get_var_units), deferred :: get_var_units
    procedure(bmif_get_var_itemsize), deferred :: get_var_itemsize
    procedure(bmif_get_var_nbytes), deferred :: get_var_nbytes
    procedure(bmif_get_value_int), deferred :: get_value_int
    procedure(bmif_get_value_float), deferred :: get_value_float
    procedure(bmif_get_value_double), deferred :: get_value_double
    procedure(bmif_get_value_ptr_int), deferred :: get_value_ptr_int
    procedure(bmif_get_value_ptr_float), deferred :: get_value_ptr_float
    procedure(bmif_get_value_ptr_double), deferred :: get_value_ptr_double
    procedure(bmif_get_value_at_indices_int), deferred :: &
      get_value_at_indices_int
    procedure(bmif_get_value_at_indices_float), deferred :: &
      get_value_at_indices_float
    procedure(bmif_get_value_at_indices_double), deferred :: &
      get_value_at_indices_double
    procedure(bmif_set_value_int), deferred :: set_value_int
    procedure(bmif_set_value_float), deferred :: set_value_float
    procedure(bmif_set_value_double), deferred :: set_value_double
    procedure(bmif_set_value_at_indices_int), deferred :: &
      set_value_at_indices_int
    procedure(bmif_set_value_at_indices_float), deferred :: &
      set_value_at_indices_float
    procedure(bmif_set_value_at_indices_double), deferred :: &
      set_value_at_indices_double
  end type bmi

  abstract interface

    ! Get the name of the model.
    function bmif_get_component_name(this, name) result(bmi_status)
      import :: bmi
      class(bmi), intent(in) :: this
      character(len=*), pointer, intent(out) :: name
      integer :: bmi_status
    end function bmif_get_component_name

    ! List a model's input variables.
    function bmif_get_input_var_names(this, names) result(bmi_status)
      import :: bmi
      class(bmi), intent(in) :: this
      character(len=*), pointer, intent(out) :: names(:)
      integer :: bmi_status
    end function bmif_get_input_var_names

    ! List a model's output variables.
    function bmif_get_output_var_names(this, names) result(bmi_status)
      import :: bmi
      class(bmi), intent(in) :: this
      character(len=*), pointer, intent(out) :: names(:)
      integer :: bmi_status
    end function bmif_get_output_var_names

    ! Perform startup tasks for the model.
    function bmif_initialize(this, config_file) result(bmi_status)
      import :: bmi
      class(bmi), intent(out) :: this
      character(len=*), intent(in) :: config_file
      integer :: bmi_status
    end function bmif_initialize

    ! Perform teardown tasks for the model.
    function bmif_finalize(this) result(bmi_status)
      import :: bmi
      class(bmi), intent(inout) :: this
      integer :: bmi_status
    end function bmif_finalize

    ! Start time of the model.
    function bmif_get_start_time(this, time) result(bmi_status)
      import :: bmi
      class(bmi), intent(in) :: this
      double precision, intent(out) :: time
      integer :: bmi_status
    end function bmif_get_start_time

    ! End time of the model.
    function bmif_get_end_time(this, time) result(bmi_status)
      import :: bmi
      class(bmi), intent(in) :: this
      double precision, intent(out) :: time
      integer :: bmi_status
    end function bmif_get_end_time

    ! Current time of the model.
    function bmif_get_current_time(this, time) result(bmi_status)
      import :: bmi
      class(bmi), intent(in) :: this
      double precision, intent(out) :: time
      integer :: bmi_status
    end function bmif_get_current_time

    ! Time step of the model.
    function bmif_get_time_step(this, time_step) result(bmi_status)
      import :: bmi
      class(bmi), intent(in) :: this
      double precision, intent(out) :: time_step
      integer :: bmi_status
    end function bmif_get_time_step

    ! Time units of the model.
    function bmif_get_time_units(this, time_units) result(bmi_status)
      import :: bmi
      class(bmi), intent(in) :: this
      character(len=*), intent(out) :: time_units
      integer :: bmi_status
    end function bmif_get_time_units

    ! Advance the model one time step.
    function bmif_update(this) result(bmi_status)
      import :: bmi
      class(bmi), intent(inout) :: this
      integer :: bmi_status
    end function bmif_update

    ! Advance the model by a fraction of a time step.
    function bmif_update_frac(this, time_frac) result(bmi_status)
      import :: bmi
      class(bmi), intent(inout) :: this
      double precision, intent(in) :: time_frac
      integer :: bmi_status
    end function bmif_update_frac

    ! Advance the model until the given time.
    function bmif_update_until(this, time) result(bmi_status)
      import :: bmi
      class(bmi), intent(inout) :: this
      double precision, intent(in) :: time
      integer :: bmi_status
    end function bmif_update_until

    ! Get the grid identifier for the given variable.
    function bmif_get_var_grid(this, var_name, grid_id) result(bmi_status)
      import :: bmi
      class(bmi), intent(in) :: this
      character(len=*), intent(in) :: var_name
      integer, intent(out) :: grid_id
      integer :: bmi_status
    end function bmif_get_var_grid

    ! Get the grid type as a string.
    function bmif_get_grid_type(this, grid_id, grid_type) result(bmi_status)
      import :: bmi
      class(bmi), intent(in) :: this
      integer, intent(in) :: grid_id
      character(len=*), intent(out) :: grid_type
      integer :: bmi_status
    end function bmif_get_grid_type

    ! Get number of dimensions of the computational grid.
    function bmif_get_grid_rank(this, grid_id, grid_rank) result(bmi_status)
      import :: bmi
      class(bmi), intent(in) :: this
      integer, intent(in) :: grid_id
      integer, intent(out) :: grid_rank
      integer :: bmi_status
    end function bmif_get_grid_rank

    ! Get the dimensions of the computational grid.
    function bmif_get_grid_shape(this, grid_id, grid_shape) result(bmi_status)
      import :: bmi
      class(bmi), intent(in) :: this
      integer, intent(in) :: grid_id
      integer, dimension(:), intent(out) :: grid_shape
      integer :: bmi_status
    end function bmif_get_grid_shape

    ! Get the total number of elements in the computational grid.
    function bmif_get_grid_size(this, grid_id, grid_size) result(bmi_status)
      import :: bmi
      class(bmi), intent(in) :: this
      integer, intent(in) :: grid_id
      integer, intent(out) :: grid_size
      integer :: bmi_status
    end function bmif_get_grid_size

    ! Get distance between nodes of the computational grid.
    function bmif_get_grid_spacing(this, grid_id, grid_spacing) result(bmi_status)
      import :: bmi
      class(bmi), intent(in) :: this
      integer, intent(in) :: grid_id
      double precision, dimension(:), intent(out) :: grid_spacing
      integer :: bmi_status
    end function bmif_get_grid_spacing

    ! Get coordinates of the origin of the computational grid.
    function bmif_get_grid_origin(this, grid_id, grid_origin) result(bmi_status)
      import :: bmi
      class(bmi), intent(in) :: this
      integer, intent(in) :: grid_id
      double precision, dimension(:), intent(out) :: grid_origin
      integer :: bmi_status
    end function bmif_get_grid_origin

    ! Get the x-coordinates of the nodes of a computational grid.
    function bmif_get_grid_x(this, grid_id, grid_x) result(bmi_status)
      import :: bmi
      class(bmi), intent(in) :: this
      integer, intent(in) :: grid_id
      double precision, dimension(:), intent(out) :: grid_x
      integer :: bmi_status
    end function bmif_get_grid_x

    ! Get the y-coordinates of the nodes of a computational grid.
    function bmif_get_grid_y(this, grid_id, grid_y) result(bmi_status)
      import :: bmi
      class(bmi), intent(in) :: this
      integer, intent(in) :: grid_id
      double precision, dimension(:), intent(out) :: grid_y
      integer :: bmi_status
    end function bmif_get_grid_y

    ! Get the z-coordinates of the nodes of a computational grid.
    function bmif_get_grid_z(this, grid_id, grid_z) result(bmi_status)
      import :: bmi
      class(bmi), intent(in) :: this
      integer, intent(in) :: grid_id
      double precision, dimension(:), intent(out) :: grid_z
      integer :: bmi_status
    end function bmif_get_grid_z

    ! Get the connectivity array of the nodes of an unstructured grid.
    function bmif_get_grid_connectivity(this, grid_id, grid_conn) &
      result(bmi_status)
      import :: bmi
      class(bmi), intent(in) :: this
      integer, intent(in) :: grid_id
      integer, dimension(:), intent(out) :: grid_conn
      integer :: bmi_status
    end function bmif_get_grid_connectivity

    ! Get the offsets of the nodes of an unstructured grid.
    function bmif_get_grid_offset(this, grid_id, grid_offset) &
      result(bmi_status)
      import :: bmi
      class(bmi), intent(in) :: this
      integer, intent(in) :: grid_id
      integer, dimension(:), intent(out) :: grid_offset
      integer :: bmi_status
    end function bmif_get_grid_offset

    ! Get the data type of the given variable as a string.
    function bmif_get_var_type(this, var_name, var_type) result(bmi_status)
      import :: bmi
      class(bmi), intent(in) :: this
      character(len=*), intent(in) :: var_name
      character(len=*), intent(out) :: var_type
      integer :: bmi_status
    end function bmif_get_var_type

    ! Get the units of the given variable.
    function bmif_get_var_units(this, var_name, var_units) result(bmi_status)
      import :: bmi
      class(bmi), intent(in) :: this
      character(len=*), intent(in) :: var_name
      character(len=*), intent(out) :: var_units
      integer :: bmi_status
    end function bmif_get_var_units

    ! Get memory use per array element, in bytes.
    function bmif_get_var_itemsize(this, var_name, var_size) result(bmi_status)
      import :: bmi
      class(bmi), intent(in) :: this
      character(len=*), intent(in) :: var_name
      integer, intent(out) :: var_size
      integer :: bmi_status
    end function bmif_get_var_itemsize

    ! Get size of the given variable, in bytes.
    function bmif_get_var_nbytes(this, var_name, var_nbytes) result(bmi_status)
      import :: bmi
      class(bmi), intent(in) :: this
      character(len=*), intent(in) :: var_name
      integer, intent(out) :: var_nbytes
      integer :: bmi_status
    end function bmif_get_var_nbytes

    ! Get a copy of values (flattened!) of the given integer variable.
    function bmif_get_value_int(this, var_name, dest) result(bmi_status)
      import :: bmi
      class(bmi), intent(in) :: this
      character(len=*), intent(in) :: var_name
      integer, intent(inout) :: dest(:)
      integer :: bmi_status
    end function bmif_get_value_int

    ! Get a copy of values (flattened!) of the given real variable.
    function bmif_get_value_float(this, var_name, dest) result(bmi_status)
      import :: bmi
      class(bmi), intent(in) :: this
      character(len=*), intent(in) :: var_name
      real, intent(inout) :: dest(:)
      integer :: bmi_status
    end function bmif_get_value_float

    ! Get a copy of values (flattened!) of the given double variable.
    function bmif_get_value_double(this, var_name, dest) result(bmi_status)
      import :: bmi
      class(bmi), intent(in) :: this
      character(len=*), intent(in) :: var_name
      double precision, intent(inout) :: dest(:)
      integer :: bmi_status
    end function bmif_get_value_double

    ! Get a reference to the given integer variable.
    function bmif_get_value_ptr_int(this, var_name, dest) &
      result(bmi_status)
      import :: bmi
      class(bmi), intent(in) :: this
      character(len=*), intent(in) :: var_name
      integer, pointer, intent(inout) :: dest(:)
      integer :: bmi_status
    end function bmif_get_value_ptr_int

    ! Get a reference to the given real variable.
    function bmif_get_value_ptr_float(this, var_name, dest) &
      result(bmi_status)
      import :: bmi
      class(bmi), intent(in) :: this
      character(len=*), intent(in) :: var_name
      real, pointer, intent(inout) :: dest(:)
      integer :: bmi_status
    end function bmif_get_value_ptr_float

    ! Get a reference to the given double variable.
    function bmif_get_value_ptr_double(this, var_name, dest) &
      result(bmi_status)
      import :: bmi
      class(bmi), intent(in) :: this
      character(len=*), intent(in) :: var_name
      double precision, pointer, intent(inout) :: dest(:)
      integer :: bmi_status
    end function bmif_get_value_ptr_double

    ! Get integer values at particular (one-dimensional) indices.
    function bmif_get_value_at_indices_int(this, var_name, dest, indices) &
      result(bmi_status)
      import :: bmi
      class(bmi), intent(in) :: this
      character(len=*), intent(in) :: var_name
      integer, intent(inout) :: dest(:)
      integer, intent(in) :: indices(:)
      integer :: bmi_status
    end function bmif_get_value_at_indices_int

    ! Get real values at particular (one-dimensional) indices.
    function bmif_get_value_at_indices_float(this, var_name, dest, indices) &
      result(bmi_status)
      import :: bmi
      class(bmi), intent(in) :: this
      character(len=*), intent(in) :: var_name
      real, intent(inout) :: dest(:)
      integer, intent(in) :: indices(:)
      integer :: bmi_status
    end function bmif_get_value_at_indices_float

    ! Get double values at particular (one-dimensional) indices.
    function bmif_get_value_at_indices_double(this, var_name, dest, indices) &
      result(bmi_status)
      import :: bmi
      class(bmi), intent(in) :: this
      character(len=*), intent(in) :: var_name
      double precision, intent(inout) :: dest(:)
      integer, intent(in) :: indices(:)
      integer :: bmi_status
    end function bmif_get_value_at_indices_double

    ! Set new values for an integer model variable.
    function bmif_set_value_int(this, var_name, src) result(bmi_status)
      import :: bmi
      class(bmi), intent(inout) :: this
      character(len=*), intent(in) :: var_name
      integer, intent(in) :: src(:)
      integer :: bmi_status
    end function bmif_set_value_int

    ! Set new values for a real model variable.
    function bmif_set_value_float(this, var_name, src) result(bmi_status)
      import :: bmi
      class(bmi), intent(inout) :: this
      character(len=*), intent(in) :: var_name
      real, intent(in) :: src(:)
      integer :: bmi_status
    end function bmif_set_value_float

    ! Set new values for a double model variable.
    function bmif_set_value_double(this, var_name, src) result(bmi_status)
      import :: bmi
      class(bmi), intent(inout) :: this
      character(len=*), intent(in) :: var_name
      double precision, intent(in) :: src(:)
      integer :: bmi_status
    end function bmif_set_value_double

    ! Set integer values at particular (one-dimensional) indices.
    function bmif_set_value_at_indices_int(this, var_name, indices, src) &
      result(bmi_status)
      import :: bmi
      class(bmi), intent(inout) :: this
      character(len=*), intent(in) :: var_name
      integer, intent(in) :: indices(:)
      integer, intent(in) :: src(:)
      integer :: bmi_status
    end function bmif_set_value_at_indices_int

    ! Set real values at particular (one-dimensional) indices.
    function bmif_set_value_at_indices_float(this, var_name, indices, src) &
      result(bmi_status)
      import :: bmi
      class(bmi), intent(inout) :: this
      character(len=*), intent(in) :: var_name
      integer, intent(in) :: indices(:)
      real, intent(in) :: src(:)
      integer :: bmi_status
    end function bmif_set_value_at_indices_float

    ! Set double values at particular (one-dimensional) indices.
    function bmif_set_value_at_indices_double(this, var_name, indices, src) &
      result(bmi_status)
      import :: bmi
      class(bmi), intent(inout) :: this
      character(len=*), intent(in) :: var_name
      integer, intent(in) :: indices(:)
      double precision, intent(in) :: src(:)
      integer :: bmi_status
    end function bmif_set_value_at_indices_double

  end interface

end module bmif
