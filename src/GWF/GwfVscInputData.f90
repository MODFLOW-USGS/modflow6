module GwfVscInputDataModule
  use KindModule, only: I4B, DP
  use ConstantsModule, only: LENMODELNAME, LENAUXNAME, DZERO

  implicit none
  private

  !> Data structure to transfer input configuration to the
  !< VSC package, as opposed to reading from file
  type, public :: GwfVscInputDataType

    ! options
    real(DP) :: viscref !< see VSC for description
    ! dim
    integer(I4B) :: nviscspecies !< see VSC for description
    ! pkg data
    integer(I4B), dimension(:), pointer :: ivisc => null() !< indicates if species uses linear or nonlinear relationship
    real(DP), dimension(:), pointer, contiguous :: dviscdc => null() !< see VSC for description
    real(DP), dimension(:), pointer, contiguous :: cviscref => null() !< see VSC for description
    character(len=LENMODELNAME), dimension(:), allocatable :: cmodelname !< see VSC for description
    character(len=LENAUXNAME), dimension(:), allocatable :: cauxspeciesname !< see VSC for description

  contains
    procedure, pass(this) :: construct
    procedure, pass(this) :: destruct
  end type GwfVscInputDataType

contains

!> @brief Allocate the input data
!<
  subroutine construct(this, nviscspecies)
    class(GwfVscInputDataType) :: this !< the input data block
    integer(I4B) :: nviscspecies !< the number of species

    allocate (this%ivisc(nviscspecies))
    allocate (this%dviscdc(nviscspecies))
    allocate (this%cviscref(nviscspecies))
    allocate (this%cmodelname(nviscspecies))
    allocate (this%cauxspeciesname(nviscspecies))

  end subroutine construct

  !> @brief clean up
  !<
  subroutine destruct(this)
    class(GwfVscInputDataType) :: this !< the input data block

    deallocate (this%ivisc)
    deallocate (this%dviscdc)
    deallocate (this%cviscref)
    deallocate (this%cmodelname)
    deallocate (this%cauxspeciesname)

  end subroutine destruct

end module GwfVscInputDataModule
