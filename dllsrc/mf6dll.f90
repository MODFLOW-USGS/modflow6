module mf6dll
  use bmif
  implicit none
  private

  type, extends(bmi) :: MF6DllType
  
  contains
    procedure, pass :: initialize
    procedure, pass :: update
    procedure, pass :: finalize
    procedure, pass :: get_start_time
    procedure, pass :: get_end_time
    procedure, pass :: get_current_time
    
  end type MF6DllType
  
contains
  
  function initialize(this, config_file) result(bmi_status)
      class (MF6DllType), intent(out) :: this
      character (len=*), intent(in) :: config_file
      integer :: bmi_status
  end function initialize
   
  function update(this) result(bmi_status)
    class (MF6DllType), intent(inout) :: this
    integer :: bmi_status
  end function update
     
  ! Perform teardown tasks for the model.
  function finalize(this) result(bmi_status)
    class (MF6DllType), intent(inout) :: this
    integer :: bmi_status
  end function finalize

  ! Start time of the model.
  function get_start_time(this, time) result(bmi_status)
    class (MF6DllType), intent(in) :: this
    double precision, intent(out) :: time
    integer :: bmi_status
  end function get_start_time

  ! End time of the model.
  function get_end_time(this, time) result(bmi_status)
    class (MF6DllType), intent(in) :: this
    double precision, intent(out) :: time
    integer :: bmi_status
  end function get_end_time

  ! Current time of the model.
  function get_current_time(this, time) result(bmi_status)
    class (MF6DllType), intent(in) :: this
    double precision, intent(out) :: time
    integer :: bmi_status
  end function get_current_time
  

end module mf6dll