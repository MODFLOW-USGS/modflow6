module MethodCellPollockModule

  use KindModule, only: DP, I4B
  use ConstantsModule, only: DONE
  use MethodModule, only: MethodType
  use MethodSubcellPoolModule, only: method_subcell_plck, &
                                     method_subcell_tern
  use CellRectModule, only: CellRectType, create_cell_rect
  use SubcellRectModule, only: SubcellRectType, create_subcell_rect
  use ParticleModule, only: ParticleType
  use TrackModule, only: TrackFileControlType
  implicit none

  private
  public :: MethodCellPollockType
  public :: create_method_cell_pollock

  type, extends(MethodType) :: MethodCellPollockType
  contains
    procedure, public :: apply => apply_mcp
    procedure, public :: destroy => destroy_mcp
    procedure, public :: load => load_mcp
    procedure, public :: load_subcell
    procedure, public :: pass => pass_mcp
  end type MethodCellPollockType

contains

  !> @brief Create a tracking method
  subroutine create_method_cell_pollock(method)
    ! -- dummy
    type(MethodCellPollockType), pointer :: method
    ! -- local
    type(CellRectType), pointer :: cell
    type(SubcellRectType), pointer :: subcell

    allocate (method)
    call create_cell_rect(cell)
    method%cell => cell
    method%type => method%cell%type
    method%delegates = .true.
    call create_subcell_rect(subcell)
    method%subcell => subcell
  end subroutine create_method_cell_pollock

  !> @brief Destroy the tracking method
  subroutine destroy_mcp(this)
    class(MethodCellPollockType), intent(inout) :: this
    deallocate (this%type)
  end subroutine destroy_mcp

  !> @brief Load subcell tracking method
  subroutine load_mcp(this, particle, next_level, submethod)
    ! -- modules
    use SubcellModule, only: SubcellType
    ! -- dummy
    class(MethodCellPollockType), intent(inout) :: this
    type(ParticleType), pointer, intent(inout) :: particle
    integer, intent(in) :: next_level
    class(MethodType), pointer, intent(inout) :: submethod

    select type (subcell => this%subcell)
    type is (SubcellRectType)
      call this%load_subcell(particle, subcell)
    end select
    call method_subcell_plck%init( &
      cell=this%cell, &
      subcell=this%subcell, &
      trackfilectl=this%trackfilectl, &
      tracktimes=this%tracktimes)
    submethod => method_subcell_plck
    particle%idomain(next_level) = 1
  end subroutine load_mcp

  !> @brief Having exited the lone subcell, pass the particle to the cell face
  !! In this case the lone subcell is the cell.
  subroutine pass_mcp(this, particle)
    ! -- dummy
    class(MethodCellPollockType), intent(inout) :: this
    type(ParticleType), pointer, intent(inout) :: particle
    ! -- local
    integer(I4B) :: exitface
    integer(I4B) :: entryface

    exitface = particle%iboundary(3)
    ! -- Map subcell exit face to cell face
    select case (exitface) ! note: exitFace uses Dave's iface convention
    case (0)
      entryface = -1
    case (1)
      entryface = 1
    case (2)
      entryface = 3
    case (3)
      entryface = 4
    case (4)
      entryface = 2
    case (5)
      entryface = 6 ! note: inface=5 same as inface=1 due to wraparound
    case (6)
      entryface = 7
    end select
    if (entryface .eq. -1) then
      particle%iboundary(2) = 0
    else
      if ((entryface .ge. 1) .and. (entryface .le. 4)) then
        ! -- Account for local cell rotation
        select type (cell => this%cell)
        type is (CellRectType)
          entryface = entryface + cell%ipvOrigin - 1
        end select
        if (entryface .gt. 4) entryface = entryface - 4
      end if
      particle%iboundary(2) = entryface
    end if
  end subroutine pass_mcp

  !> @brief Apply Pollock's method to a rectangular cell
  subroutine apply_mcp(this, particle, tmax)
    use TdisModule, only: kper, kstp
    ! -- dummy
    class(MethodCellPollockType), intent(inout) :: this
    type(ParticleType), pointer, intent(inout) :: particle
    real(DP), intent(in) :: tmax
    ! -- local
    real(DP) :: xOrigin
    real(DP) :: yOrigin
    real(DP) :: zOrigin
    real(DP) :: sinrot
    real(DP) :: cosrot

    select type (cell => this%cell)
    type is (CellRectType)
      ! -- Update particle state, checking whether any reporting or
      ! -- termination conditions apply
      call this%update(particle, cell%defn)

      ! -- Return early if particle is done advancing
      if (.not. particle%advancing) return

      ! -- If the particle is above the top of the cell (which is presumed to
      ! -- represent a water table above the cell bottom), pass the particle
      ! -- vertically and instantaneously to the cell top elevation and save
      ! -- the particle state to output file(s).
      if (particle%z > cell%defn%top) then
        particle%z = cell%defn%top
        call this%trackfilectl%save(particle, kper=kper, &
                                    kstp=kstp, reason=1) ! reason=1: cell transition
      end if

      !  Transform particle location into local cell coordinates
      !  (translated and rotated but not scaled relative to model).
      !  Transform particle location back to model coordinates, then
      !  reset transformation and eliminate accumulated roundoff error.
      xOrigin = cell%xOrigin
      yOrigin = cell%yOrigin
      zOrigin = cell%zOrigin
      sinrot = cell%sinrot
      cosrot = cell%cosrot
      call particle%transform(xOrigin, yOrigin, zOrigin, &
                              sinrot, cosrot)
      call this%track(particle, 2, tmax) ! kluge, hardwired to level 2
      call particle%transform(xOrigin, yOrigin, zOrigin, &
                              sinrot, cosrot, invert=.true.)
      call particle%transform(reset=.true.)
    end select
  end subroutine apply_mcp

  !> @brief Loads the lone rectangular subcell from the rectangular cell
  !! kluge note: is levelNext needed here and in similar "load" routines???
  subroutine load_subcell(this, particle, subcell) !
    ! -- dummy
    class(MethodCellPollockType), intent(inout) :: this
    type(ParticleType), pointer, intent(inout) :: particle
    type(SubcellRectType), intent(inout) :: subcell

    select type (cell => this%cell)
    type is (CellRectType)
      ! -- Set subcell number to 1
      subcell%isubcell = 1

      ! -- Subcell calculations will be done in local subcell coordinates
      subcell%dx = cell%dx
      subcell%dy = cell%dy
      subcell%dz = cell%dz
      subcell%sinrot = 0d0
      subcell%cosrot = 1d0 ! kluge note: rethink how/where to store subcell data???
      subcell%xOrigin = 0d0
      subcell%yOrigin = 0d0
      subcell%zOrigin = 0d0

      ! -- Set subcell edge velocities
      subcell%vx1 = cell%vx1 ! kluge note: cell velocities now already account for retfactor and porosity
      subcell%vx2 = cell%vx2
      subcell%vy1 = cell%vy1
      subcell%vy2 = cell%vy2
      subcell%vz1 = cell%vz1
      subcell%vz2 = cell%vz2
    end select
  end subroutine load_subcell

end module MethodCellPollockModule
