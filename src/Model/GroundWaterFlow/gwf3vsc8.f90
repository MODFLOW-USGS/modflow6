! Viscosity Package for representing variable-viscosity groundwater flow

module GwfVscModule

  use KindModule, only: DP, I4B
  use SimModule, only: store_error, store_warning, count_errors
  use MemoryManagerModule, only: mem_allocate, mem_reallocate, &
                                 mem_deallocate, mem_setptr
  use MemoryHelperModule, only: create_mem_path
  use ConstantsModule, only: DHALF, DZERO, DONE, LENMODELNAME, LENAUXNAME, &
                             DHNOFLO, MAXCHARLEN, LINELENGTH, LENMEMPATH
  use TdisModule, only: kper, kstp
  use NumericalPackageModule, only: NumericalPackageType
  use BaseDisModule, only: DisBaseType
  use GwfVscInputDataModule, only: GwfVscInputDataType
  use ListsModule, only: basemodellist

  implicit none

  private
  public :: GwfVscType
  public :: vsc_cr

  type :: ConcentrationPointer
    real(DP), dimension(:), pointer :: conc => null() !< pointer to concentration array
    integer(I4B), dimension(:), pointer :: icbund => null() !< store pointer to gwt ibound array
  end type ConcentrationPointer

  type, extends(NumericalPackageType) :: GwfVscType
    integer(I4B), pointer :: thermivisc => null() !< viscosity formulation flag (1:Linear, 2:Nonlinear)
    integer(I4B), pointer :: idxtmpr => null() !< if greater than 0 then an index for identifying whether the "species" array is temperature
    integer(I4B), pointer :: ioutvisc => null() !< unit number for saving viscosity
    integer(I4B), pointer :: iconcset => null() !< if 1 then conc points to a gwt (or gwe) model%x array
    integer(I4B), pointer :: ireadelev => null() !< if 1 then elev has been allocated and filled
    integer(I4B), dimension(:), pointer, contiguous :: ivisc => null() !< viscosity formulation flag for each species (1:Linear, 2:Nonlinear)
    real(DP), pointer :: viscref => null() !< reference fluid viscosity
    real(DP), dimension(:), pointer, contiguous :: visc => null() !< viscosity
    real(DP), dimension(:), pointer, contiguous :: elev => null() !< cell center elevation (optional; if not specified, then use (top+bot)/2)
    integer(I4B), dimension(:), pointer :: ibound => null() !< store pointer to ibound

    integer(I4B), pointer :: nviscspecies => null() !< number of concentration species used in viscosity equation
    real(DP), dimension(:), pointer, contiguous :: dviscdc => null() !< linear change in viscosity with change in concentration
    real(DP), dimension(:), pointer, contiguous :: cviscref => null() !< reference concentration used in viscosity equation
    real(DP), dimension(:), pointer, contiguous :: ctemp => null() !< temporary array of size (nviscspec) to pass to calc_visc_x
    character(len=LENMODELNAME), dimension(:), allocatable :: cmodelname !< names of gwt (or gwe) models used in viscosity equation
    character(len=LENAUXNAME), dimension(:), allocatable :: cauxspeciesname !< names of aux columns used in viscosity equation
    !
    ! -- Viscosity constants
    real(DP), pointer :: a2 => null() !< an empirical parameter specified by the user for calculating viscosity
    real(DP), pointer :: a3 => null() !< an empirical parameter specified by the user for calculating viscosity
    real(DP), pointer :: a4 => null() !< an empirical parameter specified by the user for calculating viscosity

    type(ConcentrationPointer), allocatable, dimension(:) :: modelconc !< concentration (or temperature) pointer for each solute (or heat) transport model

    integer(I4B), pointer :: ik22overk => null() !< NPF flag that k22 is specified as anisotropy ratio
    integer(I4B), pointer :: ik33overk => null() !< NPF flag that k33 is specified as anisotropy ratio
    real(DP), dimension(:), pointer, contiguous :: k11 => null() !< NPF hydraulic conductivity; if anisotropic, then this is Kx prior to rotation
    real(DP), dimension(:), pointer, contiguous :: k22 => null() !< NPF hydraulic conductivity; if specified then this is Ky prior to rotation
    real(DP), dimension(:), pointer, contiguous :: k33 => null() !< NPF hydraulic conductivity; if specified then this is Kz prior to rotation
    real(DP), dimension(:), pointer, contiguous :: k11input => null() !< NPF hydraulic conductivity as originally specified by the user
    real(DP), dimension(:), pointer, contiguous :: k22input => null() !< NPF hydraulic conductivity as originally specified by the user
    real(DP), dimension(:), pointer, contiguous :: k33input => null() !< NPF hydraulic conductivity as originally specified by the user
    integer(I4B), pointer :: ik22 => null() !< NPF flag that k22 is specified
    integer(I4B), pointer :: ik33 => null() !< NPF flag that k33 is specified
    integer(I4B), pointer :: kchangeper => null() ! last stress period in which any node K (or K22, or K33) values were changed (0 if unchanged from start of simulation)
    integer(I4B), pointer :: kchangestp => null() ! last time step in which any node K (or K22, or K33) values were changed (0 if unchanged from start of simulation)
    integer(I4B), dimension(:), pointer, contiguous :: nodekchange => null() ! grid array of flags indicating for each node whether its K (or K22, or K33) value changed (1) at (kchangeper, kchangestp) or not (0)

  contains
    procedure :: vsc_df
    procedure :: vsc_ar
    procedure, public :: vsc_ar_bnd
    procedure :: vsc_rp
    procedure :: vsc_ad
    procedure, public :: vsc_ad_bnd
    procedure :: vsc_ot_dv
    procedure :: vsc_da
    procedure, private :: vsc_calcvisc
    procedure :: allocate_scalars
    procedure, private :: allocate_arrays
    procedure, private :: read_options
    procedure, private :: set_options
    procedure, private :: read_dimensions
    procedure, private :: read_packagedata
    procedure, private :: set_packagedata
    procedure, private :: set_npf_pointers
    procedure, public :: update_k_with_vsc
    procedure, private :: vsc_set_changed_at
    procedure, public :: set_hfb_visc
    procedure :: set_concentration_pointer
  end type GwfVscType

contains

  function calc_visc(ivisc, viscref, dviscdc, cviscref, conc, &
                     a2, a3, a4) result(visc)
! ******************************************************************************
! calc_visc -- generic function to calculate changes in fluid viscosity
!                  using a linear formulation
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------

    ! -- dummy
    integer(I4B), dimension(:), intent(in) :: ivisc
    real(DP), intent(in) :: viscref
    real(DP), dimension(:), intent(in) :: dviscdc
    real(DP), dimension(:), intent(in) :: cviscref
    real(DP), dimension(:), intent(in) :: conc
    real(DP), intent(in) :: a2, a3, a4
    ! -- return
    real(DP) :: visc
    ! -- local
    integer(I4B) :: nviscspec
    integer(I4B) :: i
    real(DP) :: mu_t
    real(DP) :: expon
! ------------------------------------------------------------------------------
    !
    nviscspec = size(dviscdc)
    visc = viscref

    do i = 1, nviscspec
      if (ivisc(i) == 1) then
        visc = visc + dviscdc(i) * (conc(i) - cviscref(i))
      else
        expon = -1 * a3 * ((conc(i) - cviscref(i)) / &
                           ((conc(i) + a4) * (cviscref(i) + a4)))
        mu_t = viscref * a2**expon
        ! Order matters!! (This assumes we apply the temperature correction after
        ! accounting for solute concentrations)
        ! If a nonlinear correction is applied, then b/c it takes into account
        ! viscref, need to subtract it in this case
        ! At most, there will only ever be 1 nonlinear correction
        visc = (visc - viscref) + mu_t
      end if
      ! end if
    end do
    !
    ! -- return
    return
  end function calc_visc

  subroutine vsc_cr(vscobj, name_model, inunit, iout)
! ******************************************************************************
! vsc_cr -- Create a new VSC object
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    type(GwfVscType), pointer :: vscobj
    character(len=*), intent(in) :: name_model
    integer(I4B), intent(in) :: inunit
    integer(I4B), intent(in) :: iout
! ------------------------------------------------------------------------------
    !
    ! -- Create the object
    allocate (vscobj)
    !
    ! -- create name and memory path
    call vscobj%set_names(1, name_model, 'VSC', 'VSC')
    !
    ! -- Allocate scalars
    call vscobj%allocate_scalars()
    !
    ! -- Set variables
    vscobj%inunit = inunit
    vscobj%iout = iout
    !
    ! -- Initialize block parser
    call vscobj%parser%Initialize(vscobj%inunit, vscobj%iout)
    !
    ! -- Return
    return
  end subroutine vsc_cr

  !> @brief Read options and package data, or set from argument
  !<
  subroutine vsc_df(this, dis, vsc_input)
! ******************************************************************************
! vsc_df -- Define
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(GwfVscType) :: this !< this viscosity package
    class(DisBaseType), pointer, intent(in) :: dis !< pointer to discretization
    type(GwfVscInputDataType), optional, intent(in) :: vsc_input !< optional vsc input data, otherwise read from file
    ! -- local
    ! -- formats
    character(len=*), parameter :: fmtvsc = &
      "(1x,/1x,'VSC -- VISCOSITY PACKAGE, VERSION 1, 10/30/2022', &
      &' INPUT READ FROM UNIT ', i0, //)"
! ------------------------------------------------------------------------------
    !
    ! --print a message identifying the viscosity package
    write (this%iout, fmtvsc) this%inunit
    !
    ! -- store pointers to arguments that were passed in
    this%dis => dis

    if (.not. present(vsc_input)) then
      !
      ! -- Read viscosity options
      call this%read_options()
      !
      ! -- Read viscosity dimensions
      call this%read_dimensions()
    else
      ! set from input data instead
      call this%set_options(vsc_input)
      this%nviscspecies = vsc_input%nviscspecies
    end if
    !
    ! -- Allocate arrays
    call this%allocate_arrays(dis%nodes)

    if (.not. present(vsc_input)) then
      !
      ! -- Read viscosity packagedata
      call this%read_packagedata()
    else
      ! set from input data instead
      call this%set_packagedata(vsc_input)
    end if
    !
    ! -- Return
    return
  end subroutine vsc_df

  subroutine vsc_ar(this, ibound)
! ******************************************************************************
! vsc_ar -- Allocate and Read
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(GwfVscType) :: this
    integer(I4B), dimension(:), pointer :: ibound
    ! -- local
    ! -- formats
! ------------------------------------------------------------------------------
    !
    ! -- store pointers to arguments that were passed in
    this%ibound => ibound
    !
    ! -- Set pointers to npf variables
    call this%set_npf_pointers()
    !
    ! -- Return
    return
  end subroutine vsc_ar

  !> @brief Activate viscosity in advanced packages
  !!
  !! Viscosity ar_bnd rountine to activate viscosity in the advanced
  !! packages.  This routine is called from gwf_ar() as it moves through each
  !! package
  !!
  !<
  subroutine vsc_ar_bnd(this, packobj)
    !
    !    SPECIFICATIONS:
    ! ----------------------------------------------------------------------------
    ! -- modules
    use BndModule, only: BndType
    use DrnModule, only: DrnType
    use GhbModule, only: GhbType
    use RivModule, only: RivType
    use LakModule, only: LakType
    use SfrModule, only: SfrType
    use MawModule, only: MawType
    ! -- dummy
    class(GwfVscType) :: this
    class(BndType), pointer :: packobj
    ! -- local
    ! ----------------------------------------------------------------------------
    !
    ! -- Add density terms based on boundary package type
    select case (packobj%filtyp)
    case ('DRN')
      !
      ! -- activate viscosity for the drain package
      select type (packobj)
      type is (DrnType)
        call packobj%bnd_activate_viscosity()
      end select
    case ('GHB')
      !
      ! -- activate viscosity for the drain package
      select type (packobj)
      type is (GhbType)
        call packobj%bnd_activate_viscosity()
      end select
    case ('RIV')
      !
      ! -- activate viscosity for the drain package
      select type (packobj)
      type is (RivType)
        call packobj%bnd_activate_viscosity()
      end select
    case ('LAK')
      !
      ! -- activate viscosity for lake package
      select type (packobj)
      type is (LakType)
        call packobj%lak_activate_viscosity()
      end select

    case ('SFR')
      !
      ! -- activate viscosity for sfr package
      select type (packobj)
      type is (SfrType)
        call packobj%sfr_activate_viscosity()
      end select

    case ('MAW')
      !
      ! -- activate viscosity for maw package
      select type (packobj)
      type is (MawType)
        call packobj%maw_activate_viscosity()
      end select

    case default
      !
      ! -- nothing
    end select
    !
    ! -- Return
    return
  end subroutine vsc_ar_bnd

  !> @brief Set pointers to NPF variables
  !!
  !! Set array and variable pointers from the NPF
  !! package for access by VSC.
  !!
  !<
  subroutine set_npf_pointers(this)
    ! -- dummy variables
    class(GwfVscType) :: this
    ! -- local variables
    character(len=LENMEMPATH) :: npfMemoryPath
    !
    ! -- Set pointers to other package variables
    ! -- NPF
    npfMemoryPath = create_mem_path(this%name_model, 'NPF')
    call mem_setptr(this%ik22overk, 'IK22OVERK', npfMemoryPath)
    call mem_setptr(this%ik33overk, 'IK33OVERK', npfMemoryPath)
    call mem_setptr(this%k11, 'K11', npfMemoryPath)
    call mem_setptr(this%k22, 'K22', npfMemoryPath)
    call mem_setptr(this%k33, 'K33', npfMemoryPath)
    call mem_setptr(this%k11input, 'K11INPUT', npfMemoryPath)
    call mem_setptr(this%k22input, 'K22INPUT', npfMemoryPath)
    call mem_setptr(this%k33input, 'K33INPUT', npfMemoryPath)
    call mem_setptr(this%ik22, 'IK22', npfMemoryPath)
    call mem_setptr(this%ik33, 'IK33', npfMemoryPath)
    call mem_setptr(this%kchangeper, 'KCHANGEPER', npfMemoryPath)
    call mem_setptr(this%kchangestp, 'KCHANGESTP', npfMemoryPath)
    call mem_setptr(this%nodekchange, 'NODEKCHANGE', npfMemoryPath)
    !
    return
  end subroutine set_npf_pointers

  subroutine vsc_rp(this)
! ******************************************************************************
! vsc_rp -- Check for new vsc period data
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use TdisModule, only: kstp, kper
    ! -- dummy
    class(GwfVscType) :: this
    ! -- local
    character(len=LINELENGTH) :: errmsg
    integer(I4B) :: i
    ! -- formats
    character(len=*), parameter :: fmtc = &
      "('VISCOSITY PACKAGE DOES NOT HAVE A CONCENTRATION SET &
       &FOR SPECIES ',i0,'. ONE OR MORE MODEL NAMES MAY BE SPECIFIED &
       &INCORRECTLY IN THE PACKAGEDATA BLOCK OR A GWF-GWT EXCHANGE MAY NEED &
       &TO BE ACTIVATED.')"
! ------------------------------------------------------------------------------
    !
    ! -- Check to make sure all concentration pointers have been set
    if (kstp * kper == 1) then
      do i = 1, this%nviscspecies
        if (.not. associated(this%modelconc(i)%conc)) then
          write (errmsg, fmtc) i
          call store_error(errmsg)
        end if
      end do
      if (count_errors() > 0) then
        call this%parser%StoreErrorUnit()
      end if
    end if
    !
    ! -- return
    return
  end subroutine vsc_rp

  subroutine vsc_ad(this)
! ******************************************************************************
! vsc_ad -- Advance
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(GwfVscType) :: this
    ! -- local
! ------------------------------------------------------------------------------
    !
    ! -- update viscosity using the latest concentration/temperature
    call this%vsc_calcvisc()
    !
    ! -- Return
    return
  end subroutine vsc_ad

  !> @brief advance the boundary packages when viscosity is active
  !!
  !! Update the conductance values associate with inflow from a boundary
  !! when VSC package is active.
  !<
  subroutine vsc_ad_bnd(this, packobj, hnew)
    ! -- modules
    use BndModule, only: BndType
    ! -- dummy
    class(GwfVscType) :: this
    class(BndType), pointer :: packobj
    real(DP), intent(in), dimension(:) :: hnew
    ! -- local
    integer(I4B) :: i, j
    integer(I4B) :: n, locvisc, locelev
    integer(I4B), dimension(:), allocatable :: locconc
    !
    ! -- initialize
    locvisc = 0
    locelev = 0
    allocate (locconc(this%nviscspecies))
    locconc(:) = 0
    !
    ! -- Add viscosity terms for conductance-dependent boundaries
    do n = 1, packobj%naux
      if (packobj%auxname(n) == 'VISCOSITY') then
        locvisc = n
      else if (packobj%auxname(n) == 'ELEVATION') then
        locelev = n
      end if
    end do
    !
    ! -- find aux columns for conc (or temp.) that affect viscosity
    do i = 1, this%nviscspecies
      locconc(i) = 0
      do j = 1, packobj%naux
        if (this%cauxspeciesname(i) == packobj%auxname(j)) then
          locconc(i) = j
          exit
        end if
      end do
      if (locconc(i) == 0) then
        ! -- one not found, so don't use and mark all as 0
        locconc(:) = 0
        exit
      end if
    end do
    !
    ! -- apply viscosity terms to inflow from boundary based on package type
    select case (packobj%filtyp)
    case ('GHB', 'DRN', 'RIV')
      !
      ! -- general head, drain, and river boundary
      call vsc_ad_standard_bnd(packobj, hnew, this%visc, this%viscref, &
                               locelev, locvisc, locconc, this%dviscdc, &
                               this%cviscref, this%ivisc, this%a2, this%a3, &
                               this%a4, this%ctemp)
    case ('LAK')
      !
      ! -- lake
      !  Update 'viscratios' internal to lak such that they are
      !  automatically applied in the LAK calc_cond() routine
      call vsc_ad_lak(packobj, this%visc, this%viscref, this%elev, locvisc, &
                      locconc, this%dviscdc, this%cviscref, this%ivisc, &
                      this%a2, this%a3, this%a4, this%ctemp)
    case ('SFR')
      !
      ! -- streamflow routing
      !  Update 'viscratios' internal to sfr such that they are
      !  automatically applied in the SFR calc_cond() routine
      call vsc_ad_sfr(packobj, this%visc, this%viscref, this%elev, locvisc, &
                      locconc, this%dviscdc, this%cviscref, this%ivisc, &
                      this%a2, this%a3, this%a4, this%ctemp)
    case ('MAW')
      !
      ! -- multi-aquifer well
      call vsc_ad_maw(packobj, this%visc, this%viscref, this%elev, locvisc, &
                      locconc, this%dviscdc, this%cviscref, this%ivisc, &
                      this%a2, this%a3, this%a4, this%ctemp)
    case ('UZF')
      !
      ! -- unsaturated-zone flow
    case default
      !
      ! -- nothing
    end select
    !
    ! -- deallocate
    deallocate (locconc)
    !
    ! -- Return
    return
  end subroutine vsc_ad_bnd

  !> @brief advance ghb while accounting for viscosity
  !!
  !! When flow enters from ghb boundary type, take into account the effects
  !! of viscosity on the user-specified conductance terms
  !<
  subroutine vsc_ad_standard_bnd(packobj, hnew, visc, viscref, locelev, &
                                 locvisc, locconc, dviscdc, cviscref, &
                                 ivisc, a2, a3, a4, ctemp)
    ! -- modules
    use BndModule, only: BndType
    class(BndType), pointer :: packobj
    ! -- dummy
    real(DP), intent(in), dimension(:) :: hnew
    real(DP), intent(in), dimension(:) :: visc
    real(DP), intent(in) :: a2, a3, a4
    real(DP), intent(in) :: viscref
    integer(I4B), intent(in) :: locelev
    integer(I4B), intent(in) :: locvisc
    integer(I4B), dimension(:), intent(in) :: locconc
    integer(I4B), dimension(:), intent(in) :: ivisc
    real(DP), dimension(:), intent(in) :: dviscdc
    real(DP), dimension(:), intent(in) :: cviscref
    real(DP), dimension(:), intent(inout) :: ctemp
    ! -- local
    integer(I4B) :: n
    integer(I4B) :: node
    real(DP) :: viscghb
! -------------------------------------------------------------------------------
    !
    ! -- Process density terms for each GHB
    do n = 1, packobj%nbound
      node = packobj%nodelist(n)
      !
      ! -- Check if boundary cell is active, cycle if not
      if (packobj%ibound(node) <= 0) cycle
      !
      ! -- calculate the viscosity associcated with the boundary
      viscghb = calc_bnd_viscosity(n, locvisc, locconc, viscref, dviscdc, &
                                   cviscref, ctemp, ivisc, a2, a3, a4, &
                                   packobj%auxvar)
      !
      ! -- update boundary conductance based on viscosity effects
      packobj%bound(2, n) = update_bnd_cond(viscghb, viscref, &
                                            packobj%condinput(n))
      !
    end do
    !
    ! -- Return
    return
  end subroutine vsc_ad_standard_bnd

  !> @brief Update sfr-related viscosity ratios
  !!
  !! When the viscosity package is active, update the viscosity ratio that is
  !! applied to the hydraulic conductivity specified in the SFR package
  !<
  subroutine vsc_ad_sfr(packobj, visc, viscref, elev, locvisc, locconc, &
                        dviscdc, cviscref, ivisc, a2, a3, a4, ctemp)
    ! -- modules
    use BndModule, only: BndType
    use SfrModule, only: SfrType
    class(BndType), pointer :: packobj
    ! -- dummy
    real(DP), intent(in) :: viscref
    real(DP), intent(in) :: a2, a3, a4
    integer(I4B), intent(in) :: locvisc
    integer(I4B), dimension(:), intent(in) :: locconc
    integer(I4B), dimension(:), intent(in) :: ivisc
    real(DP), dimension(:), intent(in) :: visc
    real(DP), dimension(:), intent(in) :: elev
    real(DP), dimension(:), intent(in) :: dviscdc
    real(DP), dimension(:), intent(in) :: cviscref
    real(DP), dimension(:), intent(inout) :: ctemp
    ! -- local
    integer(I4B) :: n
    integer(I4B) :: node
    real(DP) :: viscsfr
! -------------------------------------------------------------------------------
    !
    ! -- update viscosity ratios for updating hyd. cond (and conductance)
    select type (packobj)
    type is (SfrType)
      do n = 1, packobj%nbound
        !
        ! -- get gwf node number
        node = packobj%nodelist(n)
        !
        ! -- Check if boundary cell is active, cycle if not
        if (packobj%ibound(node) <= 0) cycle
        !
        ! --
        !
        ! -- calculate the viscosity associcated with the boundary
        viscsfr = calc_bnd_viscosity(n, locvisc, locconc, viscref, dviscdc, &
                                     cviscref, ctemp, ivisc, a2, a3, a4, &
                                     packobj%auxvar)
        !
        ! -- fill sfr relative viscosity into column 1 of viscratios
        packobj%viscratios(1, n) = calc_vsc_ratio(viscref, viscsfr)
        !
        ! -- fill gwf relative viscosity into column 2 of viscratios
        packobj%viscratios(2, n) = calc_vsc_ratio(viscref, visc(node))
      end do
    end select
    !
    ! -- Return
    return
  end subroutine vsc_ad_sfr

  !> @brief Update lak-related viscosity ratios
  !!
  !! When the viscosity package is active, update the viscosity ratio that is
  !! applied to the lakebed conductance calculated in the LAK package
  !<
  subroutine vsc_ad_lak(packobj, visc, viscref, elev, locvisc, locconc, &
                        dviscdc, cviscref, ivisc, a2, a3, a4, ctemp)
    ! -- modules
    use BndModule, only: BndType
    use LakModule, only: LakType
    class(BndType), pointer :: packobj
    ! -- dummy
    real(DP), intent(in) :: viscref
    real(DP), intent(in) :: a2, a3, a4
    integer(I4B), intent(in) :: locvisc
    integer(I4B), dimension(:), intent(in) :: locconc
    integer(I4B), dimension(:), intent(in) :: ivisc
    real(DP), dimension(:), intent(in) :: visc
    real(DP), dimension(:), intent(in) :: elev
    real(DP), dimension(:), intent(in) :: dviscdc
    real(DP), dimension(:), intent(in) :: cviscref
    real(DP), dimension(:), intent(inout) :: ctemp
    ! -- local
    integer(I4B) :: n
    integer(I4B) :: node
    real(DP) :: visclak
! -------------------------------------------------------------------------------
    !
    ! -- update viscosity ratios for updating hyd. cond (and conductance)
    select type (packobj)
    type is (LakType)
      do n = 1, packobj%nbound
        !
        ! -- get gwf node number
        node = packobj%nodelist(n)
        !
        ! -- Check if boundary cell is active, cycle if not
        if (packobj%ibound(node) <= 0) cycle
        !
        ! --
        !
        ! -- calculate the viscosity associcated with the boundary
        visclak = calc_bnd_viscosity(n, locvisc, locconc, viscref, dviscdc, &
                                     cviscref, ctemp, ivisc, a2, a3, a4, &
                                     packobj%auxvar)
        !
        ! -- fill lak relative viscosity into column 1 of viscratios
        packobj%viscratios(1, n) = calc_vsc_ratio(viscref, visclak)
        !
        ! -- fill gwf relative viscosity into column 2 of viscratios
        packobj%viscratios(2, n) = calc_vsc_ratio(viscref, visc(node))
      end do
    end select
    !
    ! -- Return
    return
  end subroutine vsc_ad_lak

  !> @brief Update maw-related viscosity ratios
  !!
  !! When the viscosity package is active, update the viscosity ratio that is
  !! applied to the conductance calculated in the MAW package
  !<
  subroutine vsc_ad_maw(packobj, visc, viscref, elev, locvisc, locconc, &
                        dviscdc, cviscref, ivisc, a2, a3, a4, ctemp)
    ! -- modules
    use BndModule, only: BndType
    use MawModule, only: MawType
    class(BndType), pointer :: packobj
    ! -- dummy
    real(DP), intent(in) :: viscref
    real(DP), intent(in) :: a2, a3, a4
    integer(I4B), intent(in) :: locvisc
    integer(I4B), dimension(:), intent(in) :: locconc
    integer(I4B), dimension(:), intent(in) :: ivisc
    real(DP), dimension(:), intent(in) :: visc
    real(DP), dimension(:), intent(in) :: elev
    real(DP), dimension(:), intent(in) :: dviscdc
    real(DP), dimension(:), intent(in) :: cviscref
    real(DP), dimension(:), intent(inout) :: ctemp
    ! -- local
    integer(I4B) :: n
    integer(I4B) :: node
    real(DP) :: viscmaw
! -------------------------------------------------------------------------------
    !
    ! -- update viscosity ratios for updating hyd. cond (and conductance)
    select type (packobj)
    type is (MawType)
      do n = 1, packobj%nbound
        !
        ! -- get gwf node number
        node = packobj%nodelist(n)
        !
        ! -- Check if boundary cell is active, cycle if not
        if (packobj%ibound(node) <= 0) cycle
        !
        ! --
        !
        ! -- calculate the viscosity associcated with the boundary
        viscmaw = calc_bnd_viscosity(n, locvisc, locconc, viscref, dviscdc, &
                                     cviscref, ctemp, ivisc, a2, a3, a4, &
                                     packobj%auxvar)
        !
        ! -- fill lak relative viscosity into column 1 of viscratios
        packobj%viscratios(1, n) = calc_vsc_ratio(viscref, viscmaw)
        !
        ! -- fill gwf relative viscosity into column 2 of viscratios
        packobj%viscratios(2, n) = calc_vsc_ratio(viscref, visc(node))
      end do
    end select
    !
    ! -- Return
    return
  end subroutine vsc_ad_maw

  !> @brief apply bnd viscosity to the conductance term
  !!
  !! When the viscosity package is active apply the viscosity ratio to the
  !! active boundary package's conductance term.
  !<
  function update_bnd_cond(bndvisc, viscref, spcfdcond) result(updatedcond)
    ! -- modules
    ! -- dummy
    real(DP), intent(in) :: viscref
    real(DP), intent(in) :: bndvisc
    real(DP), intent(in) :: spcfdcond
    ! -- local
    real(DP) :: vscratio
    real(DP) :: updatedcond
! -------------------------------------------------------------------------------
    !
    vscratio = calc_vsc_ratio(viscref, bndvisc)
    !
    ! -- calculate new conductance here!!
    updatedcond = vscratio * spcfdcond
    !
    ! -- Return
    return
  end function update_bnd_cond

  !> @brief calculate and return the viscosity ratio
  !<
  function calc_vsc_ratio(viscref, bndvisc) result(viscratio)
    ! -- dummy
    real(DP), intent(in) :: viscref
    real(DP), intent(in) :: bndvisc
    ! -- local
    real(DP) :: viscratio
! -------------------------------------------------------------------------------
    !
    viscratio = viscref / bndvisc
    !
    ! -- Return
    return
  end function calc_vsc_ratio

  function calc_bnd_viscosity(n, locvisc, locconc, viscref, dviscdc, cviscref, &
                              ctemp, ivisc, a2, a3, a4, auxvar) result(viscbnd)
! ******************************************************************************
! get_bnd_viscosity -- Return the viscosity of the boundary package using one of
!   several different options in the following order of priority:
!     1. Assign as aux variable in column with name 'VISCOSITY'
!     2. Calculate using viscosity equation and nviscspecies aux columns
!     3. If neither of those, then assign as viscref
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    integer(I4B), intent(in) :: n
    integer(I4B), intent(in) :: locvisc
    real(DP), intent(in) :: a2, a3, a4
    integer(I4B), dimension(:), intent(in) :: ivisc
    integer(I4B), dimension(:), intent(in) :: locconc
    real(DP), intent(in) :: viscref
    real(DP), dimension(:), intent(in) :: dviscdc
    real(DP), dimension(:), intent(in) :: cviscref
    real(DP), dimension(:), intent(inout) :: ctemp
    real(DP), dimension(:, :), intent(in) :: auxvar
    ! -- return
    real(DP) :: viscbnd
    ! -- local
    integer(I4B) :: i
! ------------------------------------------------------------------------------
    !
    ! -- assign boundary viscosity based on one of three options
    if (locvisc > 0) then
      ! -- assign viscosity to an aux column named 'VISCOSITY'
      viscbnd = auxvar(locvisc, n)
    else if (locconc(1) > 0) then
      ! -- calculate viscosity using one or more concentration auxcolumns
      do i = 1, size(locconc)
        ctemp(i) = DZERO
        if (locconc(i) > 0) then
          ctemp(i) = auxvar(locconc(i), n)
        end if
      end do
      viscbnd = calc_visc(ivisc, viscref, dviscdc, cviscref, ctemp, a2, a3, a4)
    else
      ! -- neither of the above, so assign as viscref
      viscbnd = viscref
    end if
    !
    ! -- return
    return
  end function calc_bnd_viscosity

  !> @brief Account for viscosity in horizontal flow barriers
  !!
  !! Will return the viscosity associated with the upgradient node (cell)
  !! to the HFB package for adjusting the hydraulic characteristic (hydchr)
  !! of the barrier
  !<
  subroutine set_hfb_visc(this, cellid, hfbviscratio)
    ! -- dummy variables
    class(GwfVscType) :: this
    integer(I4B), intent(in) :: cellid
    ! -- return
    real(DP), intent(inout) :: hfbviscratio
    ! -- local
    real(DP) :: visc
! ------------------------------------------------------------------------------
    !
    ! -- Retrieve viscosity for the passed node number
    visc = this%visc(cellid)
    !
    ! -- Calculate the viscosity ratio for the
    hfbviscratio = calc_vsc_ratio(this%viscref, visc)
    !
    ! -- return
    return
  end subroutine set_hfb_visc

  !> @brief hit the hydraulic conductivity values with the ratio mu_o/mu
  !!
  !! This routine called after updating the viscosity values using the latest
  !! concentration and/or temperature values.  The ratio mu_o/mu, reference
  !! viscosity divided by the updated viscosity value, is multiplied by K
  !! for each cell.
  !<
  subroutine update_k_with_vsc(this)
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(GwfVscType) :: this
    integer(I4B) :: n
! ------------------------------------------------------------------------------
    !
    ! -- For viscosity-based K's, apply change of K to K11 by starting with
    !    user-specified K values and not the K's leftover from the last viscosity
    !    update.
    do n = 1, this%dis%nodes
      this%k11(n) = this%k11input(n) * (this%viscref / this%visc(n))
      if (this%ik22 /= 0) then
        this%k22(n) = this%k22input(n) * (this%viscref / this%visc(n))
      end if
      if (this%ik33 /= 0) then
        this%k33(n) = this%k33input(n) * (this%viscref / this%visc(n))
      end if
      this%nodekchange(n) = 1
    end do
    !
    ! -- Flag kchange
    call this%vsc_set_changed_at(kper, kstp)
    !
    ! -- return
    return
  end subroutine update_k_with_vsc

  !> @brief Mark K changes as having occurred at (kper, kstp)
  !!
  !! Procedure called by VSC code when K updated due to viscosity changes.
  !! K values changed at (kper, kstp).
  !!
  !<
  subroutine vsc_set_changed_at(this, kper, kstp)
    ! -- dummy variables
    class(GwfVscType) :: this
    integer(I4B), intent(in) :: kper
    integer(I4B), intent(in) :: kstp
    !
    this%kchangeper = kper
    this%kchangestp = kstp
    !
    return
  end subroutine vsc_set_changed_at

  subroutine vsc_ot_dv(this, idvfl)
! ******************************************************************************
! vsc_ot_dv -- Save calculated viscosity array to binary file
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(GwfVscType) :: this
    integer(I4B), intent(in) :: idvfl
    ! -- local
    character(len=1) :: cdatafmp = ' ', editdesc = ' '
    integer(I4B) :: ibinun
    integer(I4B) :: iprint
    integer(I4B) :: nvaluesp
    integer(I4B) :: nwidthp
    real(DP) :: dinact
! ------------------------------------------------------------------------------
    !
    ! -- Set unit number for viscosity output
    if (this%ioutvisc /= 0) then
      ibinun = 1
    else
      ibinun = 0
    end if
    if (idvfl == 0) ibinun = 0
    !
    ! -- save viscosity array
    if (ibinun /= 0) then
      iprint = 0
      dinact = DHNOFLO
      !
      ! -- write viscosity to binary file
      if (this%ioutvisc /= 0) then
        ibinun = this%ioutvisc
        call this%dis%record_array(this%visc, this%iout, iprint, ibinun, &
                                   '         VISCOSITY', cdatafmp, nvaluesp, &
                                   nwidthp, editdesc, dinact)
      end if
    end if
    !
    ! -- Return
    return
  end subroutine vsc_ot_dv

  subroutine vsc_da(this)
! ******************************************************************************
! vsc_da -- Deallocate
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(GwfVscType) :: this
! ------------------------------------------------------------------------------
    !
    ! -- Deallocate arrays if package was active
    if (this%inunit > 0) then
      call mem_deallocate(this%visc)
      call mem_deallocate(this%ivisc)
      call mem_deallocate(this%dviscdc)
      call mem_deallocate(this%cviscref)
      call mem_deallocate(this%ctemp)
      deallocate (this%cmodelname)
      deallocate (this%cauxspeciesname)
      deallocate (this%modelconc)
    end if
    !
    ! -- Scalars
    call mem_deallocate(this%thermivisc)
    call mem_deallocate(this%idxtmpr)
    call mem_deallocate(this%ioutvisc)
    call mem_deallocate(this%ireadelev)
    call mem_deallocate(this%iconcset)
    call mem_deallocate(this%viscref)
    call mem_deallocate(this%nviscspecies)
    call mem_deallocate(this%a2)
    call mem_deallocate(this%a3)
    call mem_deallocate(this%a4)
    !
    ! -- Nullify pointers to other package variables
    nullify (this%ik22overk)
    nullify (this%ik33overk)
    nullify (this%k11)
    nullify (this%k22)
    nullify (this%k33)
    nullify (this%k11input)
    nullify (this%k22input)
    nullify (this%k33input)
    nullify (this%ik22)
    nullify (this%ik33)
    nullify (this%kchangeper)
    nullify (this%kchangestp)
    nullify (this%nodekchange)
    !
    ! -- deallocate parent
    call this%NumericalPackageType%da()
    !
    ! -- Return
    return
  end subroutine vsc_da

  subroutine read_dimensions(this)
! ******************************************************************************
! read_dimensions -- Read the dimensions for this package
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(GwfVscType), intent(inout) :: this
    ! -- local
    character(len=LINELENGTH) :: errmsg, keyword
    integer(I4B) :: ierr
    logical :: isfound, endOfBlock
    ! -- format
! ------------------------------------------------------------------------------
    !
    ! -- get dimensions block
    call this%parser%GetBlock('DIMENSIONS', isfound, ierr, &
                              supportOpenClose=.true.)
    !
    ! -- parse dimensions block if detected
    if (isfound) then
      write (this%iout, '(/1x,a)') 'PROCESSING VSC DIMENSIONS'
      do
        call this%parser%GetNextLine(endOfBlock)
        if (endOfBlock) exit
        call this%parser%GetStringCaps(keyword)
        select case (keyword)
        case ('NVISCSPECIES')
          this%nviscspecies = this%parser%GetInteger()
          write (this%iout, '(4x,a,i0)') 'NVISCSPECIES = ', this%nviscspecies
        case default
          write (errmsg, '(4x,a,a)') &
            'UNKNOWN VSC DIMENSION: ', trim(keyword)
          call store_error(errmsg)
          call this%parser%StoreErrorUnit()
        end select
      end do
      write (this%iout, '(1x,a)') 'END OF VSC DIMENSIONS'
    else
      call store_error('REQUIRED VSC DIMENSIONS BLOCK NOT FOUND.')
      call this%parser%StoreErrorUnit()
    end if
    !
    ! -- check dimension
    if (this%nviscspecies < 1) then
      call store_error('NVISCSPECIES MUST BE GREATER THAN ZERO.')
      call this%parser%StoreErrorUnit()
    end if
    !
    ! -- return
    return
  end subroutine read_dimensions

  !> @ brief Read data for package
  !!
  !!  Method to read data for the package.
  !!
  !<
  subroutine read_packagedata(this)
    ! -- modules
    ! -- dummy
    class(GwfVscType) :: this
    ! -- local
    character(len=LINELENGTH) :: errmsg
    character(len=LINELENGTH) :: line
    integer(I4B) :: ierr
    integer(I4B) :: iviscspec
    logical :: isfound, endOfBlock
    logical :: blockrequired
    integer(I4B), dimension(:), allocatable :: itemp
    character(len=10) :: c10
    character(len=16) :: c16
    ! -- format
    character(len=*), parameter :: fmterr = &
      "('INVALID VALUE FOR IRHOSPEC (',i0,') DETECTED IN VSC PACKAGE. &
      &IRHOSPEC MUST BE > 0 AND <= NVISCSPECIES, AND DUPLICATE VALUES &
      &ARE NOT ALLOWED.')"
! ------------------------------------------------------------------------------
    !
    ! -- initialize
    allocate (itemp(this%nviscspecies))
    itemp(:) = 0
    !
    ! -- get packagedata block
    blockrequired = .true.
    call this%parser%GetBlock('PACKAGEDATA', isfound, ierr, &
                              blockRequired=blockRequired, &
                              supportOpenClose=.true.)
    !
    ! -- parse packagedata block
    if (isfound) then
      write (this%iout, '(1x,a)') 'PROCESSING VSC PACKAGEDATA'
      do
        call this%parser%GetNextLine(endOfBlock)
        if (endOfBlock) exit
        iviscspec = this%parser%GetInteger()
        if (iviscspec < 1 .or. iviscspec > this%nviscspecies) then
          write (errmsg, fmterr) iviscspec
          call store_error(errmsg)
        end if
        if (itemp(iviscspec) /= 0) then
          write (errmsg, fmterr) iviscspec
          call store_error(errmsg)
        end if
        itemp(iviscspec) = 1
        !
        this%dviscdc(iviscspec) = this%parser%GetDouble()
        this%cviscref(iviscspec) = this%parser%GetDouble()
        call this%parser%GetStringCaps(this%cmodelname(iviscspec))
        call this%parser%GetStringCaps(this%cauxspeciesname(iviscspec))
        !
        if (this%cauxspeciesname(iviscspec) == 'TEMPERATURE') then
          if (this%idxtmpr > 0) then
            write (errmsg, '(a)') 'MORE THAN ONE SPECIES IN VSC INPUT IDENTIFIED &
              &AS "TEMPERATURE".  ONLY ONE SPECIES MAY BE DESIGNATED AS &
              &TEMPERATURE.'
            call store_error(errmsg)
          else
            this%idxtmpr = iviscspec
            if (this%thermivisc == 2) then
              this%ivisc(iviscspec) = 2
            end if
          end if
        end if
      end do
    end if
    !
    ! -- Check for errors.
    if (count_errors() > 0) then
      call this%parser%StoreErrorUnit()
    end if
    !
    ! -- write packagedata information
    write (this%iout, '(/,1x,a)') 'SUMMARY OF SPECIES INFORMATION IN VSC PACKAGE'
    write (this%iout, '(1a11,5a17)') &
      'SPECIES', 'DVISCDC', 'CVISCREF', 'MODEL', 'AUXSPECIESNAME'
    do iviscspec = 1, this%nviscspecies
      write (c10, '(i0)') iviscspec
      line = ' '//adjustr(c10)

      write (c16, '(g15.6)') this%dviscdc(iviscspec)
      line = trim(line)//' '//adjustr(c16)
      write (c16, '(g15.6)') this%cviscref(iviscspec)
      line = trim(line)//' '//adjustr(c16)
      write (c16, '(a)') this%cmodelname(iviscspec)
      line = trim(line)//' '//adjustr(c16)
      write (c16, '(a)') this%cauxspeciesname(iviscspec)
      line = trim(line)//' '//adjustr(c16)
      write (this%iout, '(a)') trim(line)
    end do
    !
    ! -- deallocate
    deallocate (itemp)
    !
    write (this%iout, '(/,1x,a)') 'END OF VSC PACKAGEDATA'
    !
    ! -- return
    return
  end subroutine read_packagedata

  !> @brief Sets package data instead of reading from file
  !<
  subroutine set_packagedata(this, input_data)
    class(GwfVscType) :: this !< this vscoancy pkg
    type(GwfVscInputDataType), intent(in) :: input_data !< the input data to be set
    ! local
    integer(I4B) :: ispec

    do ispec = 1, this%nviscspecies
      this%dviscdc(ispec) = input_data%dviscdc(ispec)
      this%cviscref(ispec) = input_data%cviscref(ispec)
      this%cmodelname(ispec) = input_data%cmodelname(ispec)
      this%cauxspeciesname(ispec) = input_data%cauxspeciesname(ispec)
    end do

  end subroutine set_packagedata

  subroutine vsc_calcvisc(this)
! ******************************************************************************
! vsc_calcvisc -- calculate fluid viscosity from concentration
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(GwfVscType) :: this

    ! -- local
    integer(I4B) :: n
    integer(I4B) :: i
! ------------------------------------------------------------------------------
    !
    ! -- Calculate the viscosity using the specified concentration and/or
    !    temperature arrays
    do n = 1, this%dis%nodes
      do i = 1, this%nviscspecies
        if (this%modelconc(i)%icbund(n) == 0) then
          this%ctemp = DZERO
        else
          this%ctemp(i) = this%modelconc(i)%conc(n)
        end if
      end do
      !
      this%visc(n) = calc_visc(this%ivisc, this%viscref, this%dviscdc, &
                               this%cviscref, this%ctemp, this%a2, &
                               this%a3, this%a4)

    end do
    !
    ! -- Return
    return
  end subroutine vsc_calcvisc

  subroutine allocate_scalars(this)
! ******************************************************************************
! allocate_scalars
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use ConstantsModule, only: DZERO
    ! -- dummy
    class(GwfVscType) :: this
    ! -- local
! ------------------------------------------------------------------------------
    !
    ! -- allocate scalars in NumericalPackageType
    call this%NumericalPackageType%allocate_scalars()
    !
    ! -- Allocate
    call mem_allocate(this%thermivisc, 'THERMIVISC', this%memoryPath)
    call mem_allocate(this%idxtmpr, 'IDXTMPR', this%memoryPath)
    call mem_allocate(this%ioutvisc, 'IOUTVISC', this%memoryPath)
    call mem_allocate(this%ireadelev, 'IREADELEV', this%memoryPath)
    call mem_allocate(this%iconcset, 'ICONCSET', this%memoryPath)
    call mem_allocate(this%viscref, 'VISCREF', this%memoryPath)
    call mem_allocate(this%a2, 'A2', this%memoryPath)
    call mem_allocate(this%a3, 'A3', this%memoryPath)
    call mem_allocate(this%a4, 'A4', this%memoryPath)
    !
    call mem_allocate(this%nviscspecies, 'NVISCSPECIES', this%memoryPath)
    !
    ! -- Initialize
    this%thermivisc = 0
    this%idxtmpr = 0
    this%ioutvisc = 0
    this%ireadelev = 0
    this%iconcset = 0
    this%viscref = 1000.d0
    this%A2 = DZERO
    this%A3 = DZERO
    this%A4 = DZERO
    !
    this%nviscspecies = 0
    !
    ! -- Return
    return
  end subroutine allocate_scalars

  subroutine allocate_arrays(this, nodes)
! ******************************************************************************
! allocate_arrays
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(GwfVscType) :: this
    integer(I4B), intent(in) :: nodes
    ! -- local
    integer(I4B) :: i
! ------------------------------------------------------------------------------
    !
    ! -- Allocate
    call mem_allocate(this%visc, nodes, 'VISC', this%memoryPath)
    call mem_allocate(this%ivisc, this%nviscspecies, 'IVISC', this%memoryPath)
    call mem_allocate(this%dviscdc, this%nviscspecies, 'DRHODC', &
                      this%memoryPath)
    call mem_allocate(this%cviscref, this%nviscspecies, 'CRHOREF', &
                      this%memoryPath)
    call mem_allocate(this%ctemp, this%nviscspecies, 'CTEMP', this%memoryPath)
    allocate (this%cmodelname(this%nviscspecies))
    allocate (this%cauxspeciesname(this%nviscspecies))
    allocate (this%modelconc(this%nviscspecies))
    !
    ! -- Initialize
    do i = 1, nodes
      this%visc(i) = this%viscref
    end do
    !
    ! -- Initialize nviscspecies arrays
    do i = 1, this%nviscspecies
      this%ivisc(i) = 1
      this%dviscdc(i) = DZERO
      this%cviscref(i) = DZERO
      this%ctemp(i) = DZERO
      this%cmodelname(i) = ''
      this%cauxspeciesname(i) = ''
    end do
    !
    ! -- Return
    return
  end subroutine allocate_arrays

  subroutine read_options(this)
! ******************************************************************************
! read_options
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use OpenSpecModule, only: access, form
    use InputOutputModule, only: urword, getunit, urdaux, openfile
    ! -- dummy
    class(GwfVscType) :: this
    ! -- local
    character(len=LINELENGTH) :: warnmsg, errmsg, keyword, keyword2
    character(len=MAXCHARLEN) :: fname
    character(len=LINELENGTH) :: line
    character(len=16) :: c16
    integer(I4B) :: ierr
    logical :: isfound, endOfBlock
    ! -- formats
    character(len=*), parameter :: fmtfileout = &
      "(1x, 'VSC', 1x, a, 1x, 'WILL BE SAVED TO FILE: ', &
      &a, /4x, 'OPENED ON UNIT: ', I7)"
    character(len=*), parameter :: fmtlinear = &
      "(/,1x,'VISCOSITY WILL VARY LINEARLY WITH TEMPERATURE &
      &CHANGE ')"
    character(len=*), parameter :: fmtnonlinear = &
      "(/,1x,'VISCOSITY WILL VARY NON-LINEARLY WITH TEMPERATURE &
      &CHANGE ')"
! ------------------------------------------------------------------------------
    !
    ! -- get options block
    call this%parser%GetBlock('OPTIONS', isfound, ierr, &
                              supportOpenClose=.true., blockRequired=.false.)
    !
    ! -- parse options block if detected
    if (isfound) then
      write (this%iout, '(1x,a)') 'PROCESSING VSC OPTIONS'
      do
        call this%parser%GetNextLine(endOfBlock)
        if (endOfBlock) exit
        call this%parser%GetStringCaps(keyword)
        select case (keyword)
        case ('VISCREF')
          this%viscref = this%parser%GetDouble()
          write (this%iout, '(4x,a,1pg15.6)') &
            'REFERENCE VISCOSITY HAS BEEN SET TO: ', &
            this%viscref
        case ('VISCOSITY')
          call this%parser%GetStringCaps(keyword)
          if (keyword == 'FILEOUT') then
            call this%parser%GetString(fname)
            this%ioutvisc = getunit()
            call openfile(this%ioutvisc, this%iout, fname, 'DATA(BINARY)', &
                          form, access, 'REPLACE')
            write (this%iout, fmtfileout) &
              'VISCOSITY', fname, this%ioutvisc
          else
            errmsg = 'OPTIONAL VISCOSITY KEYWORD MUST BE '// &
                     'FOLLOWED BY FILEOUT'
            call store_error(errmsg)
          end if
        case ('THERMAL_VISCOSITY_FUNC')
          call this%parser%GetStringCaps(keyword2)
          if (trim(adjustl(keyword2)) == 'LINEAR') this%thermivisc = 1
          if (trim(adjustl(keyword2)) == 'NONLINEAR') this%thermivisc = 2
          select case (this%thermivisc)
          case (1)
            write (this%iout, fmtlinear)
          case (2)
            write (this%iout, fmtnonlinear)
            this%a2 = this%parser%GetDouble()
            this%a3 = this%parser%GetDouble()
            this%a4 = this%parser%GetDouble()
            !
            ! -- Write viscosity function selection to lst file
            write (this%iout, '(/,1x,a,a,a)') 'CONSTANTS USED IN ', &
              trim(keyword2), ' VISCOSITY FORMULATION ARE '
            write (this%iout, '(1x,a)') &
              '              A2,              A3,              A4'
            line = ' '
            write (c16, '(g15.6)') this%a2
            line = trim(line)//' '//adjustr(c16)
            write (c16, '(g15.6)') this%a3
            line = trim(line)//' '//adjustr(c16)
            write (c16, '(g15.6)') this%a4
            line = trim(line)//' '//adjustr(c16)
            write (this%iout, '(a)') trim(line)

          end select
        case default
          write (errmsg, '(4x,a,a)') '**ERROR. UNKNOWN VSC OPTION: ', &
            trim(keyword)
          call store_error(errmsg)
          call this%parser%StoreErrorUnit()
        end select
      end do
      !
      if (this%thermivisc == 1) then
        if (this%a2 == 0.0) then
          write (errmsg, '(a)') 'LINEAR OPTION SELECTED FOR VARYING  &
            &VISCOSITY WITH TEMPERTURE, BUT A1, A SURROGATE FOR &
            &dVISC/dT, SET EQUAL TO 0.0'
          call store_error(errmsg)
        end if
      end if
      if (this%thermivisc > 1) then
        if (this%a2 == 0) then
          write (warnmsg, '(a)') 'NONLINEAR OPTION SELECTED FOR &
            &VARYING VISCOSITY WITH TEMPERATURE, BUT A2 SET EQUAL TO &
            &ZERO WHICH MAY LEAD TO UNINTENDED VALUES FOR VISCOSITY'
          call store_warning(errmsg)
        end if
        if (this%a3 == 0) then
          write (warnmsg, '(a)') 'NONLINEAR OPTION SELECTED FOR &
            &VARYING VISCOSITY WITH TEMPERATURE, BUT A3 SET EQUAL TO &
            &ZERO WHICH MAY LEAD TO UNINTENDED VALUES FOR VISCOSITY'
          call store_warning(warnmsg)
        end if
        if (this%a4 == 0) then
          write (warnmsg, '(a)') 'NONLINEAR OPTION SELECTED FOR &
            &VARYING VISCOSITY WITH TEMPERATURE, BUT A4 SET EQUAL TO &
            &ZERO WHICH MAY LEAD TO UNINTENDED VALUES FOR VISCOSITY'
          call store_warning(warnmsg)
        end if
      end if
    end if
    !
    write (this%iout, '(/,1x,a)') 'END OF VSC OPTIONS'
    !
    ! -- Return
    return
  end subroutine read_options

  !> @brief Sets options as opposed to reading them from a file
  !<
  subroutine set_options(this, input_data)
    class(GwfVscType) :: this
    type(GwfVscInputDataType), intent(in) :: input_data !< the input data to be set

    this%viscref = input_data%viscref
    !
    ! -- Return
    return
  end subroutine set_options

  subroutine set_concentration_pointer(this, modelname, conc, icbund, istmpr)
! ******************************************************************************
! set_concentration_pointer -- pass in a gwt model name, concentration array
!   and ibound, and store a pointer to these in the VSC package so that
!   viscosity can be calculated from them.
!   This routine is called from the gwfgwt exchange in the exg_ar() method.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(GwfVscType) :: this
    character(len=LENMODELNAME), intent(in) :: modelname
    real(DP), dimension(:), pointer :: conc
    integer(I4B), dimension(:), pointer :: icbund
    integer(I4B), optional, intent(in) :: istmpr
    ! -- local
    integer(I4B) :: i
    logical :: found
! ------------------------------------------------------------------------------
    !
    this%iconcset = 1
    found = .false.
    do i = 1, this%nviscspecies
      if (this%cmodelname(i) == modelname) then
        this%modelconc(i)%conc => conc
        this%modelconc(i)%icbund => icbund
        found = .true.
        exit
      end if
    end do
    !
    ! -- Return
    return
  end subroutine set_concentration_pointer

end module GwfVscModule
