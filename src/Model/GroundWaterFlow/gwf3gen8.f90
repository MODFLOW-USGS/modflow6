module genmodule
  use KindModule, only: DP, I4B
  use ConstantsModule, only: DZERO, LENFTYPE, LENPACKAGENAME
  use MemoryHelperModule, only: create_mem_path
  use BndModule, only: BndType
  use ObsModule, only: DefaultObsIdProcessor
  use TimeSeriesLinkModule, only: TimeSeriesLinkType, &
                                  GetTimeSeriesLinkFromList
  !
  implicit none
  !
  private
  public :: gen_create
  public :: GenType
  !
  character(len=LENFTYPE)       :: ftype = 'GEN'
  character(len=LENPACKAGENAME) :: text = '             GEN'
  !
  type, extends(BndType) :: GenType
  contains
    procedure :: bnd_options => gen_options
    procedure :: bnd_rp => gen_rp
    procedure :: bnd_fc => gen_fc
    ! -- methods for observations
    procedure, public :: bnd_obs_supported => gen_obs_supported
    procedure, public :: bnd_df_obs => gen_df_obs
  end type GenType

contains

  !> @ brief Create a new package object
  !!
  !!  Create a new GEN Package object
  !!
  !<
  subroutine gen_create(packobj, id, ibcnum, inunit, iout, namemodel, pakname)
! ******************************************************************************
! gen_create -- Create a New Gen Package
! Subroutine: (1) create new-style package
!             (2) point bndobj to the new package
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy variables
    class(BndType), pointer :: packobj         !< pointer to default package type
    integer(I4B), intent(in) :: id              !< package id
    integer(I4B), intent(in) :: ibcnum          !< boundary condition number
    integer(I4B), intent(in) :: inunit          !< unit number of GEN package input file
    integer(I4B), intent(in) :: iout            !< unit number of model listing file
    character(len=*), intent(in) :: namemodel  !< model name
    character(len=*), intent(in) :: pakname    !< package name
    ! -- local variables
    type(GenType), pointer :: genobj
! ------------------------------------------------------------------------------
    !
    ! -- allocate the object and assign values to object variables
    allocate (genobj)
    packobj => genobj
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
    !
    ! -- return
    return
  end subroutine gen_create

  !> @ brief Read additional options for package
  !!
  !!  Read additional options for GEN package.
  !!
  !! @param[in,out]  option  string with option text
  !! @param[in,out]  found   boolean indicating if a valid option was provided
  !!
  !<
  subroutine gen_options(this, option, found)
! ******************************************************************************
! gen_options -- set options specific to GenType
!
! gen_options overrides BndType%bnd_options
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy variables
    class(GenType), intent(inout) :: this
    character(len=*), intent(inout) :: option
    logical, intent(inout) :: found
! ------------------------------------------------------------------------------
    !
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
    !
    ! -- return
    return
  end subroutine gen_options

  !> @ brief Read and prepare stress period data for package
  !!
  !!  Method reads and prepares stress period data for the GEN package.
  !!  This method overides the base read and prepare method and does not read
  !!  any stress period data from the GEN package input file.
  !!
  !<
  subroutine gen_rp(this)
    ! ******************************************************************************
    ! gen_rp -- Read and Prepare
    ! Subroutine: (1) read itmp
    !             (2) read new boundaries if itmp>0
    ! ******************************************************************************
    !
    !    SPECIFICATIONS:
    ! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy variables
    class(GenType), intent(inout) :: this
    ! -- local variables
    ! -- formats
    ! ------------------------------------------------------------------------------
    !
    ! -- return
    return
  end subroutine gen_rp

  !> @ brief Fill A and r for the package
  !!
  !!  Fill the coefficient matrix and right-hand side with the GEN
  !!  package terms.
  !!
  !! @param[in,out]  rhs     right-hand side vector
  !! @param[in,out]  amatsln A matrix for the solution
  !!
  !<
  subroutine gen_fc(this, rhs, ia, idxglo, amatsln)
! **************************************************************************
! gen_fc -- Copy rhs and hcof into solution rhs and amat
! **************************************************************************
!
!    SPECIFICATIONS:
! --------------------------------------------------------------------------
    ! -- dummy variables
    class(GenType) :: this
    real(DP), dimension(:), intent(inout) :: rhs      !< right-hand side vector
    integer(I4B), dimension(:), intent(in) :: ia      !< pointer to the rows in A matrix
    integer(I4B), dimension(:), intent(in) :: idxglo  !< position of entries in A matrix
    real(DP), dimension(:), intent(inout) :: amatsln  !< A matrix for solution
    ! -- local variables
    integer(I4B) :: i
    integer(I4B) :: n
    integer(I4B) :: ipos
    real(DP) :: qgen
! --------------------------------------------------------------------------
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
      amatsln(idxglo(ipos)) = amatsln(idxglo(ipos)) + this%hcof(i)
      !
      ! -- If mover is active and this boundary is discharging,
      !    store available water (as positive value).
      qgen = this%rhs(i) - this%hcof(i)*this%xnew(n)
      if (this%imover == 1 .and. qgen > DZERO) then
        call this%pakmvrobj%accumulate_qformvr(i, qgen)
      end if
    end do
    !
    ! -- return
    return
  end subroutine gen_fc

  ! -- Procedures related to observations

  !> @brief Determine if observations are supported.
  !!
  !! Function to determine if observations are supported by the GEN package.
  !! Observations are supported by the GEN package.
  !!
  !<
  logical function gen_obs_supported(this)
    ! ******************************************************************************
    ! gen_obs_supported
    !   -- Return true because GEN package supports observations.
    !   -- Overrides BndType%bnd_obs_supported()
    ! ******************************************************************************
    !
    !    SPECIFICATIONS:
    ! ------------------------------------------------------------------------------
    implicit none
    class(GenType) :: this
    ! ------------------------------------------------------------------------------
    gen_obs_supported = .true.
    return
  end function gen_obs_supported

  !> @brief Define the observation types available in the package
  !!
  !! Method to define the observation types available in the GEN package.
  !!
  !<
  subroutine gen_df_obs(this)
    ! ******************************************************************************
    ! gen_df_obs (implements bnd_df_obs)
    !   -- Store observation type supported by GEN package.
    !   -- Overrides BndType%bnd_df_obs
    ! ******************************************************************************
    !
    !    SPECIFICATIONS:
    ! ------------------------------------------------------------------------------
    implicit none
    ! -- dummy variables
    class(GenType) :: this
    ! -- local variables
    integer(I4B) :: indx
    ! ------------------------------------------------------------------------------
    call this%obs%StoreObsType('gen', .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => DefaultObsIdProcessor
    !
    ! -- Store obs type and assign procedure pointer
    !    for to-mvr observation type.
    call this%obs%StoreObsType('to-mvr', .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => DefaultObsIdProcessor
    !
    ! -- return
    return
  end subroutine gen_df_obs

end module genmodule
