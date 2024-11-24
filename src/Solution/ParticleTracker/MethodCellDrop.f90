module MethodCellDropModule

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
  public :: MethodCellDropType
  public :: create_method_cell_drop

  type, extends(MethodType) :: MethodCellDropType
    private
  contains
    procedure, public :: apply => apply_drop
    procedure, public :: deallocate
  end type MethodCellDropType

contains

  !> @brief Create a vertical drop tracking method
  subroutine create_method_cell_drop(method)
    type(MethodCellDropType), pointer :: method
    allocate (method)
    allocate (method%type)
    method%type = "drop"
    method%delegates = .false.
  end subroutine create_method_cell_drop

  !> @brief Deallocate the tracking method
  subroutine deallocate (this)
    class(MethodCellDropType), intent(inout) :: this
    deallocate (this%type)
  end subroutine deallocate

  !> @brief Pass the particle vertically and instantaneously to
  !! the cell bottom or to the water table, whichever is higher.
  subroutine apply_drop(this, particle, tmax)
    ! dummy
    class(MethodCellDropType), intent(inout) :: this
    type(ParticleType), pointer, intent(inout) :: particle
    real(DP), intent(in) :: tmax

    call this%prepare(particle, this%cell%defn)
    if (.not. particle%advancing) return

    if (this%cell%defn%is_dry()) then
      print *, 'dry drop'
      particle%z = this%cell%defn%bot
      particle%iboundary(2) = this%cell%defn%npolyverts + 2
    else if (particle%z > this%cell%defn%top) then
      print *, 'wet drop from ', particle%z, ' to ', this%cell%defn%top
      particle%z = this%cell%defn%top
    end if

    call this%save(particle, reason=1)
    
  end subroutine apply_drop

end module MethodCellDropModule
  