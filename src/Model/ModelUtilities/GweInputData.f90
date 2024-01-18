module GweInputDataModule

  use KindModule, only: I4B, DP
  use ConstantsModule, only: DZERO, LENMEMPATH

  implicit none
  private
  public :: GweInputDataType
  public :: gweshared_dat_cr
  public :: gweshared_dat_df
  public :: set_gwe_dat_ptrs

  !> Data for sharing among multiple packages.  Originally read in from
  !< the EST package

  type GweInputDataType

    ! dim
    integer(I4B) :: nnodes !< number of cells

    ! strings
    character(len=LENMEMPATH) :: memoryPath = '' !< the location in the memory manager where the variables are stored

    ! est data to be share across multiple packages
    real(DP), pointer :: gwerhow => null() !< Density of water (for GWE purposes, a constant scalar)
    real(DP), pointer :: gwecpw => null() !< Heat capacity of water (non-spatially varying)
    real(DP), pointer :: gwelatheatvap => null() !< latent heat of vaporization
    real(DP), dimension(:), pointer, contiguous :: gwerhos => null() !< Density of the aquifer material
    real(DP), dimension(:), pointer, contiguous :: gwecps => null() !< Heat capacity of solids (spatially varying)

  contains

    ! -- public
    procedure, public :: gweshared_dat_df
    procedure, public :: set_gwe_dat_ptrs
    procedure, public :: gweshared_dat_da
    ! -- private
    procedure, private :: allocate_shared_vars
    procedure, private :: set_gwe_shared_scalars
    procedure, private :: set_gwe_shared_arrays

  end type GweInputDataType

contains

!> @brief Allocate the shared data
!<
  subroutine gweshared_dat_cr(gwe_dat)
    ! -- modules
    ! -- dummy
    type(GweInputDataType), pointer :: gwe_dat !< the input data block
    !
    ! -- Create the object
    allocate (gwe_dat)
    !
    ! -- Return
    return
  end subroutine gweshared_dat_cr

!> @brief Define the shared data
!<
  subroutine gweshared_dat_df(this, nodes)
    ! -- dummy
    class(GweInputDataType) :: this !< the input data block
    integer(I4B), intent(in) :: nodes
    ! -- local
    !
    ! -- Allocate variables
    call this%allocate_shared_vars(nodes)
    !
    ! -- Return
    return
  end subroutine gweshared_dat_df

  !> @brief Define the information this object holds
  !!
  !! Allocate strings for storing label names
  !! Intended to be analogous to allocate_scalars()
  !<
  subroutine allocate_shared_vars(this, nodes)
    ! -- dummy
    class(GweInputDataType) :: this !< TspLabelsType object
    integer(I4B), intent(in) :: nodes
    ! -- local
    integer(I4B) :: i
    !
    allocate (this%gwecpw)
    allocate (this%gwerhow)
    allocate (this%gwelatheatvap)
    allocate (this%gwerhos(nodes))
    allocate (this%gwecps(nodes))
    !
    ! -- Initialize values
    this%gwecpw = DZERO
    this%gwerhow = DZERO
    this%gwelatheatvap = DZERO
    do i = 1, nodes
      this%gwecps(i) = DZERO
      this%gwerhos(i) = DZERO
    end do
    !
    ! -- Return
    return
  end subroutine allocate_shared_vars

  !> @brief Allocate and read data from EST
  !!
  !! EST data, including heat capacity of water (cpw), density of water
  !! (rhow), latent heat of vaporization (latheatvap), heat capacity of
  !! the aquifer material (cps), and density of the aquifer material
  !! (rhow) is used among other packages and is therefore stored in a
  !! separate class
  subroutine set_gwe_dat_ptrs(this, gwerhow, gwecpw, gwerhos, gwecps, &
                              gwelatheatvap)
    ! -- dummy
    class(GweInputDataType) :: this !< the input data block
    real(DP), intent(in) :: gwerhow !< ptr to density of water specified in EST
    real(DP), intent(in) :: gwecpw !< ptr to heat capacity of water specified in EST
    real(DP), intent(in) :: gwerhos !< ptr to sptially-variably density of aquifer material specified in EST
    real(DP), intent(in) :: gwecps !< ptr to sptially-variably heat capacity of aquifer material specified in EST
    real(DP), intent(in), optional :: gwelatheatvap !< ptr to latent heat of vaporization specified in EST
    !
    ! -- Allocate scalars
    if (present(gwelatheatvap)) then
      call this%set_gwe_shared_scalars(gwerhow, gwecpw, gwelatheatvap)
    else
      call this%set_gwe_shared_scalars(gwerhow, gwecpw)
    end if
    !
    ! -- Allocate arrays
    call this%set_gwe_shared_arrays(gwerhos, gwecps)
    !
    ! -- Return
    return
  end subroutine set_gwe_dat_ptrs

  !> @brief Set pointers to scalars read by the EST package
  !! for use by other packages
  !!
  !! Set pointers to GWE-related scalars and arrays for use
  !! by multiple packages.  For example, a package capable of
  !! simulating evaporation will need access to latent heat of
  !! of vaporization.
  !<
  subroutine set_gwe_shared_scalars(this, gwerhow, gwecpw, gwelatheatvap)
    ! -- dummy
    class(GweInputDataType) :: this !< GweInputDataType object
    real(DP), intent(in) :: gwerhow
    real(DP), intent(in) :: gwecpw
    real(DP), intent(in), optional :: gwelatheatvap
    ! -- local
    !
    ! -- Set the pointers
    ! -- Fixed density of water to be used by GWE
    this%gwerhow = gwerhow
    ! -- Spatially constant heat capacity of water   ! kluge note: "specific heat" (which is heat capacity per unit mass) is probably the more correct term
    this%gwecpw = gwecpw
    ! -- Latent heat of vaporization
    if (present(gwelatheatvap)) then
      this%gwelatheatvap = gwelatheatvap
    end if
    !
    ! -- Return
    return
  end subroutine set_gwe_shared_scalars

  !> @brief Set pointers to data arrays read by the EST package
  !! for use by other packages
  !!
  !! Set pointers to GWE-related arrays for use
  !! by multiple packages.
  !<
  subroutine set_gwe_shared_arrays(this, gwerhos, gwecps)
    ! -- dummy
    class(GweInputDataType) :: this !< GweInputDataType object
    real(DP), intent(in) :: gwerhos
    real(DP), intent(in) :: gwecps
    ! -- local
    !
    ! -- Set the pointers
    ! -- Spatially-variable density of aquifer solid material
    this%gwerhos = gwerhos
    ! -- Spatially-variable heat capacity of aquifer solid material
    this%gwecps = gwecps
    !
    ! -- Return
    return
  end subroutine set_gwe_shared_arrays

  !> @ breif Deallocate memory
  !!
  !!  Deallocate GWE shared data array memory
  !<
  subroutine gweshared_dat_da(this)
    ! -- dummy
    class(GweInputDataType) :: this !< the input data block
    !
    ! -- Scalars
    deallocate (this%gwelatheatvap)
    deallocate (this%gwerhow)
    deallocate (this%gwecpw)
    !
    ! -- Arrays
    deallocate (this%gwerhos)
    deallocate (this%gwecps)
    !
    ! -- Return
    return
  end subroutine gweshared_dat_da

end module GweInputDataModule
