module CpuTimerModule
  use KindModule, only: I4B, DP, LGP
  use ConstantsModule, only: DNODATA, DZERO
  use STLVecIntModule
  implicit none
  private

  ! predefined sections as integers
  ! root:
  integer(I4B), public :: SECTION_RUN

  ! level 1 (BMI):
  integer(I4B), public :: SECTION_INIT
  integer(I4B), public :: SECTION_UPDATE
  integer(I4B), public :: SECTION_FINALIZE

  ! level 2 (XMI):
  integer(I4B), public :: SECTION_PREP_TSTP
  integer(I4B), public :: SECTION_DO_TSTP
  integer(I4B), public :: SECTION_FINAL_TSTP

  ! constants for memory allocation
  integer(I4B), parameter :: MAX_NR_TIMED_SECTIONS = 20
  integer(I4B), public, parameter :: LEN_SECTION_TITLE = 128

  type, private :: TimedSectionType
    character(len=LEN_SECTION_TITLE) :: title !< title to identify timed section in log
    real(DP) :: walltime !< walltime spent in section
    integer(I4B) :: count !< number of times section was entered
    integer(I4B) :: status !< =1 means section timer started, =0 otherwise
    integer(I4B) :: parent_id !< id of parent, or 0 when root
    type(STLVecInt) :: children !< ids of children
  end type TimedSectionType

  type, public :: CpuTimerType
    private
    integer(I4B) :: nr_sections
    integer(I4B) :: root_id
    type(TimedSectionType), dimension(:), pointer :: all_sections => null()
  contains
    procedure :: initialize
    procedure :: add_section
    procedure :: start
    procedure :: stop
    procedure :: print_timings
    procedure :: destroy
    procedure :: is_initialized
    ! private
    procedure, private :: get_section_id
    procedure, private :: print_section
  end type CpuTimerType

  type(CpuTimerType), public :: g_timer !< the global timer object (to reduce trivial lines of code)
  public :: set_timer_func, timer_func_iface
  procedure(timer_func_iface), private, pointer :: g_timer_func => serial_timer ! typically set to use MPI_Wtime for parallel, using the method below

  abstract interface
    subroutine timer_func_iface(walltime)
      import DP
      real(DP), intent(inout) :: walltime
    end subroutine
  end interface

contains

  !> @brief Set the timer function to be used, e.g. based on MPI_Wtime
  !<
  subroutine set_timer_func(timer_func)
    procedure(timer_func_iface), pointer :: timer_func

    g_timer_func => timer_func

  end subroutine set_timer_func

  
  !< @brief Initialize the CPU timer object
  !<
  subroutine initialize(this)
    class(CpuTimerType) :: this
    ! local
    integer(I4B) :: i

    allocate(this%all_sections(MAX_NR_TIMED_SECTIONS))
    do i = 1, MAX_NR_TIMED_SECTIONS
      this%all_sections(i)%title = "undefined"
      this%all_sections(i)%status = 0
      this%all_sections(i)%walltime = DZERO
      this%all_sections(i)%count = 0
      this%all_sections(i)%parent_id = 0
      call this%all_sections(i)%children%init()
    end do
    
    this%nr_sections = 0
    this%root_id = 0

    SECTION_RUN = g_timer%add_section("Run")

    SECTION_INIT = g_timer%add_section("Init", SECTION_RUN)
    SECTION_UPDATE = g_timer%add_section("Update", SECTION_RUN)
    SECTION_FINALIZE = g_timer%add_section("Finalize", SECTION_RUN)

    SECTION_PREP_TSTP = g_timer%add_section("Prepare timestep", SECTION_UPDATE)
    SECTION_DO_TSTP = g_timer%add_section("Do timestep", SECTION_UPDATE)
    SECTION_FINAL_TSTP = g_timer%add_section("Finalize timestep", SECTION_UPDATE)

  end subroutine initialize

  !> @brief Add a new timed section to the tree,
  !! passing the parent id will add it as a child
  !< in the tree
  function add_section(this, title, parent_id) result(section_id)
    class(CpuTimerType) :: this
    character(len=*) :: title
    integer(I4B), optional :: parent_id
    integer(I4B) :: section_id

    ! increment to new section id
    this%nr_sections = this%nr_sections + 1
    section_id = this%nr_sections

    ! initialize new section
    this%all_sections(section_id)%title = title
    this%all_sections(section_id)%walltime = DZERO
    this%all_sections(section_id)%status = 0
    
    ! if parent, otherwise root section
    if (present(parent_id)) then
      ! add child to parent
      this%all_sections(section_id)%parent_id = parent_id
      call this%all_sections(parent_id)%children%push_back(section_id)
    else
      ! this is the root, assume there's only one!
      this%all_sections(section_id)%parent_id = 0
      this%root_id = section_id
    end if

  end function add_section

  !> @brief Return section id for title, 0 when not found
  !! @note Currently not used, but could be useful later on, else remove
  !<
  function get_section_id(this, title) result(section_id)
    class(CpuTimerType) :: this
    character(len=*) :: title
    integer(I4B) :: section_id
    ! local
    integer(I4B) :: i

    section_id = 0
    do i = 1, this%nr_sections
      if (this%all_sections(i)%title == title) then
        section_id = i
        exit
      end if
    end do

  end function get_section_id

  subroutine start(this, section_id)
    class(CpuTimerType) :: this
    integer(I4B) :: section_id
    ! local
    real(DP) :: start_time
    type(TimedSectionType), pointer :: section

    call cpu_time(start_time)
    
    section => this%all_sections(section_id)
    section%count = section%count + 1
    section%status = 1
    section%walltime = section%walltime - start_time

  end subroutine start

  subroutine stop(this, section_id)
    class(CpuTimerType) :: this
    integer(I4B) :: section_id
    ! local
    real(DP) :: end_time
    type(TimedSectionType), pointer :: section    

    call cpu_time(end_time)

    ! nett result (c.f. start(...)) is adding (dt = end_time - start_time)
    section => this%all_sections(section_id)
    section%status = 0
    section%walltime = section%walltime + end_time

  end subroutine stop

  subroutine print_timings(this)
    class(CpuTimerType) :: this
    ! local
    integer(I4B) :: level

    ! print timing call stack
    level = 0
    call this%print_section(this%root_id, level)

    ! print walltime per category
    

  end subroutine print_timings

  recursive subroutine print_section(this, section_id, level)
    class(CpuTimerType) :: this
    integer(I4B) :: section_id
    integer(I4B) :: level
    ! local
    integer(I4B) :: i, new_level
    real(DP) :: percent
    type(TimedSectionType), pointer :: section

    section => this%all_sections(section_id)

    ! calculate percentage
    percent = 100.0_DP
    if (section%parent_id /= 0) then
      percent = section%walltime / this%all_sections(this%root_id)%walltime * 100.0_DP
    end if

    ! print section timing
    write(*,'(3a,f0.2,3a,f14.6,2a,i0,a)') &
      " ", repeat('....', level), "[", percent, "%] ", &
      trim(section%title), ": ", section%walltime, "s", " (", &
      section%count, "x)"
    
    ! print children
    new_level = level + 1
    do i = 1, section%children%size
      call this%print_section(section%children%at(i), new_level)
    end do

    if (level == 0) write(*,*)

  end subroutine print_section

  subroutine destroy(this)
    class(CpuTimerType) :: this
    ! local
    integer(I4B) :: i
    
    do i = 1, this%nr_sections
      call this%all_sections(i)%children%destroy()
    end do
    deallocate (this%all_sections)
    nullify (this%all_sections)

  end subroutine destroy

  function is_initialized(this) result(initialized)
    class(CpuTimerType) :: this
    logical(LGP) :: initialized

    initialized = associated(this%all_sections)

  end function is_initialized

  subroutine serial_timer(walltime)
    real(DP), intent(inout) :: walltime
    call cpu_time(walltime)
  end subroutine serial_timer

end module CpuTimerModule