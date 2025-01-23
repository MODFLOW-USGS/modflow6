module CpuTimerModule
  use KindModule, only: I4B, DP, LGP
  use ConstantsModule, only: DNODATA, DZERO
  use STLVecIntModule
  implicit none
  private

  ! predefined skeleton of section ids
  ! level 0 (root):
  integer(I4B), public :: SECTION_RUN = -1

  ! level 1 (BMI):
  integer(I4B), public :: SECTION_INIT = -1
  integer(I4B), public :: SECTION_UPDATE = -1
  integer(I4B), public :: SECTION_FINALIZE = -1

  ! level 2 (XMI):
  integer(I4B), public :: SECTION_PREP_TSTP = -1
  integer(I4B), public :: SECTION_DO_TSTP = -1
  integer(I4B), public :: SECTION_FINAL_TSTP = -1

  ! constants for memory allocation
  integer(I4B), parameter :: MAX_NR_TIMED_SECTIONS = 50
  integer(I4B), public, parameter :: LEN_SECTION_TITLE = 128

  ! data structure to store measurements for a section
  type, private :: TimedSectionType
    character(len=LEN_SECTION_TITLE) :: title !< title to identify timed section in log
    real(DP) :: walltime !< walltime spent in section
    integer(I4B) :: count !< number of times section was entered
    integer(I4B) :: status !< =1 means section timer started, =0 otherwise
    integer(I4B) :: parent_id !< id of parent, or 0 when root
    type(STLVecInt) :: children !< ids of children
  end type TimedSectionType

  ! this is the timer object
  type, public :: CpuTimerType
    private
    integer(I4B) :: nr_sections
    integer(I4B) :: root_id !< 
    type(TimedSectionType), dimension(:), pointer :: all_sections => null() !< all timed sections, dynamic up to MAX_NR_TIMED_SECTIONS
    type(STLVecInt) :: callstack !< call stack of section ids
  contains
    procedure :: initialize
    procedure :: add_section
    procedure :: start
    procedure :: stop
    procedure :: print_timings
    procedure :: destroy
    procedure :: is_initialized
    ! private
    procedure, private :: print_section
    procedure, private :: print_total
    procedure, private :: aggregate_walltime
    procedure, private :: aggregate_counts
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

    call this%callstack%init()

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

  end subroutine initialize

  !> @brief Add a new timed section to the tree,
  !! passing the parent id will add it as a child
  !< in the tree
  function add_section(this, title, parent_id) result(section_id)
    class(CpuTimerType) :: this
    character(len=*) :: title
    integer(I4B) :: parent_id
    integer(I4B) :: section_id

    ! increment to new section id
    this%nr_sections = this%nr_sections + 1
    section_id = this%nr_sections

    ! initialize new section
    this%all_sections(section_id)%title = title
    this%all_sections(section_id)%walltime = DZERO
    this%all_sections(section_id)%status = 0
    
    ! if parent, otherwise root section
    if (parent_id > 0) then
      ! add child to parent
      this%all_sections(section_id)%parent_id = parent_id
      call this%all_sections(parent_id)%children%push_back(section_id)
    else
      ! this is the root, assume there's only one!
      this%all_sections(section_id)%parent_id = 0
      this%root_id = section_id
    end if

  end function add_section

  !> @brief Start section timing, add when not exist yet (i.e. when id < 1)
  !<
  subroutine start(this, title, section_id)
    class(CpuTimerType) :: this
    character(len=*) :: title
    integer(I4B) :: section_id, parent_id
    ! local
    real(DP) :: start_time
    type(TimedSectionType), pointer :: section

    call cpu_time(start_time)
    
    if (section_id < 1) then
      ! add section if not exist
      parent_id = 0 ! root
      if (this%callstack%size > 0) then
        parent_id = this%callstack%at(this%callstack%size)
      end if
      section_id = this%add_section(title, parent_id)
    end if
    call this%callstack%push_back(section_id)

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

    ! pop from call stack
    call this%callstack%pop()

  end subroutine stop

  subroutine print_timings(this)
    class(CpuTimerType) :: this
    ! local
    integer(I4B) :: level

    ! print timing call stack
    level = 0
    write(*,'(a/)') "-------------------- Timing: Call Stack --------------------"
    call this%print_section(this%root_id, level)

    ! print walltime per category from substring (if exist)
    write(*,'(a/)') "-------------------- Timing: Cumulative --------------------"
    call this%print_total("Formulate")
    call this%print_total("Linear solve")
    call this%print_total("MPI_WaitAll")
    write(*,'(/a/)') "------------------------------------------------------------"

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
    percent = 1.0_DP
    if (section%parent_id /= 0) then
      percent = section%walltime / this%all_sections(this%root_id)%walltime
    end if
    percent = percent * 100.0_DP

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

  subroutine print_total(this, subtitle)
    class(CpuTimerType) :: this
    character(len=*) :: subtitle
    ! local
    integer(I4B) :: count
    real(DP) :: walltime, percent
    
    count = this%aggregate_counts(subtitle)
    if (count > 0) then
      walltime = aggregate_walltime(this, subtitle)
      percent = (walltime / this%all_sections(this%root_id)%walltime) * 100.0_DP
      write(*,'(2a,f0.2,3a,f14.6,2a,i0,a)') &
        " ", "[", percent, "%] ", &
        trim(subtitle), ": ", walltime, "s", " (", &
        count, "x)"
    end if

  end subroutine print_total

  !> @brief Aggregate walltime over sections with a certain title
  !<
  function aggregate_walltime(this, title) result(walltime)
    class(CpuTimerType) :: this
    character(len=*) :: title
    real(DP) :: walltime
    ! local
    integer(I4B) :: i

    walltime = DZERO
    do i = 1, this%nr_sections
      if (index(this%all_sections(i)%title, trim(title)) > 0) then
        walltime = walltime + this%all_sections(i)%walltime
      end if
    end do

  end function aggregate_walltime

  !> @brief Aggregate counts over sections with a certain title
  !<
  function aggregate_counts(this, title) result(counts)
    class(CpuTimerType) :: this
    character(len=*) :: title
    integer(I4B) :: counts
    ! local
    integer(I4B) :: i

    counts = 0
    do i = 1, this%nr_sections
      if (index(this%all_sections(i)%title, trim(title)) > 0) then
        counts = counts + this%all_sections(i)%count
      end if
    end do

  end function aggregate_counts

  !> @brief Clean up the CPU timer object
  !<
  subroutine destroy(this)
    class(CpuTimerType) :: this
    ! local
    integer(I4B) :: i
    
    call this%callstack%destroy()

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