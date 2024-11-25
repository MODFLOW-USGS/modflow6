module MethodCellPassToBotModule

  use KindModule, only: DP, I4B
  use MethodModule, only: MethodType
  use CellDefnModule, only: CellDefnType, create_defn
  use PrtFmiModule, only: PrtFmiType
  use BaseDisModule, only: DisBaseType
  use ParticleModule, only: ParticleType
  use CellModule, only: CellType
  use SubcellModule, only: SubcellType
  use TrackControlModule, only: TrackControlType
  implicit none

  private
  public :: MethodCellPassToBotType
  public :: create_method_cell_ptb

  type, extends(MethodType) :: MethodCellPassToBotType
    private
  contains
    procedure, public :: apply => apply_ptb
    procedure, public :: deallocate
  end type MethodCellPassToBotType

contains

  !> @brief Create a new pass-to-bottom tracking method
  subroutine create_method_cell_ptb(method)
    type(MethodCellPassToBotType), pointer :: method
    allocate (method)
    allocate (method%type)
    method%type = "passtobottom"
    method%delegates = .false.
  end subroutine create_method_cell_ptb

  !> @brief Deallocate the pass-to-bottom tracking method
  subroutine deallocate (this)
    class(MethodCellPassToBotType), intent(inout) :: this
    deallocate (this%type)
  end subroutine deallocate

  !> @brief Pass particle vertically and instantaneously to the cell bottom
  subroutine apply_ptb(this, particle, tmax)
    ! dummy
    class(MethodCellPassToBotType), intent(inout) :: this
    type(ParticleType), pointer, intent(inout) :: particle
    real(DP), intent(in) :: tmax

    call this%prepare(particle, this%cell%defn)
    if (.not. particle%advancing) return
    particle%z = this%cell%defn%bot
    particle%iboundary(2) = this%cell%defn%npolyverts + 2
    call this%save(particle, reason=1)
  end subroutine apply_ptb

end module MethodCellPassToBotModule
