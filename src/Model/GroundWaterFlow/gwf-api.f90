!> @brief This module contains the API package methods
!!
!! This module contains the overridden methods from the base model package
!! class for the API package. The API package is designed to be used with the
!! shared object and have period data specified using the MODFLOW API. Several
!! methods need to be overridden since no period data are specified in the
!! API input file. Overridden methods include:
!!   - bnd_rp no period data is specified
!!   - bnd_fc BOUND array is not filled. hcof and rhs are specified dierctly
!!
!<
module apimodule
  use KindModule, only: DP, I4B
  use ConstantsModule, only: DZERO, LENFTYPE, LENPACKAGENAME
  use MemoryHelperModule, only: create_mem_path
  use BndModule, only: BndType
  use ObsModule, only: DefaultObsIdProcessor
  use TimeSeriesLinkModule, only: TimeSeriesLinkType, &
                                  GetTimeSeriesLinkFromList
  use MatrixBaseModule
  !
  implicit none
  !
  private
  public :: api_create
  public :: ApiType
  !
  character(len=LENFTYPE) :: ftype = 'API'
  character(len=LENPACKAGENAME) :: text = '             API'
  !
  type, extends(BndType) :: ApiType
  contains
    procedure :: bnd_options => api_options
    procedure :: bnd_rp => api_rp
    procedure :: bnd_fc => api_fc
    ! -- methods for observations
    procedure, public :: bnd_obs_supported => api_obs_supported
    procedure, public :: bnd_df_obs => api_df_obs
  end type ApiType

contains

  !> @ brief Create a new package object
  !!
  !!  Create a new USR Package object
  !!
  !<
  subroutine api_create(packobj, id, ibcnum, inunit, iout, namemodel, pakname)
    ! -- dummy variables
    class(BndType), pointer :: packobj !< pointer to default package type
    integer(I4B), intent(in) :: id !< package id
    integer(I4B), intent(in) :: ibcnum !< boundary condition number
    integer(I4B), intent(in) :: inunit !< unit number of USR package input file
    integer(I4B), intent(in) :: iout !< unit number of model listing file
    character(len=*), intent(in) :: namemodel !< model name
    character(len=*), intent(in) :: pakname !< package name
    ! -- local variables
    type(ApiType), pointer :: apiobj
    !
    ! -- allocate the object and assign values to object variables
    allocate (apiobj)
    packobj => apiobj
    !
    ! -- create name and memory path
    call packobj%set_names(ibcnum, namemodel, pakname, ftype)
    packobj%text = text
    !
    ! -- allocate scalars
    call packobj%allocate_scalars()
    !
    ! -- initialize package
    call packobj%pack_initialize()
    !
    packobj%inunit = inunit
    packobj%iout = iout
    packobj%id = id
    packobj%ibcnum = ibcnum
    packobj%ncolbnd = 2
    packobj%iscloc = 2
    packobj%ictMemPath = create_mem_path(namemodel, 'NPF')
  end subroutine api_create

  !> @ brief Read additional options for package
  !!
  !!  Read additional options for USR package.
  !!
  !<
  subroutine api_options(this, option, found)
    ! -- dummy variables
    class(ApiType), intent(inout) :: this
    character(len=*), intent(inout) :: option
    logical, intent(inout) :: found
    !
    ! -- process package options
    select case (option)
    case ('MOVER')
      this%imover = 1
      write (this%iout, '(4x,A)') 'MOVER OPTION ENABLED'
      found = .true.
    case default
      !
      ! -- No options found
      found = .false.
    end select
  end subroutine api_options

  !> @ brief Read and prepare stress period data for package
  !!
  !!  Method reads and prepares stress period data for the USR package.
  !!  This method overrides the base read and prepare method and does not read
  !!  any stress period data from the USR package input file.
  !!
  !<
  subroutine api_rp(this)
    ! -- dummy variables
    class(ApiType), intent(inout) :: this
  end subroutine api_rp

  !> @ brief Fill A and r for the package
  !!
  !!  Fill the coefficient matrix and right-hand side with the USR
  !!  package terms.
  !!
  !<
  subroutine api_fc(this, rhs, ia, idxglo, matrix_sln)
    ! -- dummy variables
    class(ApiType) :: this
    real(DP), dimension(:), intent(inout) :: rhs !< right-hand side vector
    integer(I4B), dimension(:), intent(in) :: ia !< pointer to the rows in A matrix
    integer(I4B), dimension(:), intent(in) :: idxglo !< position of entries in A matrix
    class(MatrixBaseType), pointer :: matrix_sln !< A matrix for solution
    ! -- local variables
    integer(I4B) :: i
    integer(I4B) :: n
    integer(I4B) :: ipos
    real(DP) :: qusr
    !
    ! -- pakmvrobj fc
    if (this%imover == 1) then
      call this%pakmvrobj%fc()
    end if
    !
    ! -- Copy package rhs and hcof into solution rhs and amat
    do i = 1, this%nbound
      n = this%nodelist(i)
      rhs(n) = rhs(n) + this%rhs(i)
      ipos = ia(n)
      call matrix_sln%add_value_pos(idxglo(ipos), this%hcof(i))
      !
      ! -- If mover is active and this boundary is discharging,
      !    store available water (as positive value).
      qusr = this%rhs(i) - this%hcof(i) * this%xnew(n)
      if (this%imover == 1 .and. qusr > DZERO) then
        call this%pakmvrobj%accumulate_qformvr(i, qusr)
      end if
    end do
  end subroutine api_fc

  ! -- Procedures related to observations

  !> @brief Determine if observations are supported.
  !!
  !! Function to determine if observations are supported by the USR package.
  !! Observations are supported by the USR package.
  !!
  !<
  logical function api_obs_supported(this)
    ! -- dummy variables
    class(ApiType) :: this
    !
    ! -- set variables
    api_obs_supported = .true.
  end function api_obs_supported

  !> @brief Define the observation types available in the package
  !!
  !! Method to define the observation types available in the USR package.
  !!
  !<
  subroutine api_df_obs(this)
    ! -- dummy variables
    class(ApiType) :: this
    ! -- local variables
    integer(I4B) :: indx
    !
    ! -- initialize observations
    call this%obs%StoreObsType('api', .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => DefaultObsIdProcessor
    !
    ! -- Store obs type and assign procedure pointer
    !    for to-mvr observation type.
    call this%obs%StoreObsType('to-mvr', .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => DefaultObsIdProcessor
  end subroutine api_df_obs

end module apimodule
