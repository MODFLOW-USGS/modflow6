! Generalized Transport Base Class
! Base class for solute (mass) and energy (thermal) transport
!   (The following copied from gwt1.f90)
!   * Add check that discretization is the same between both models
!   * Program GWT-GWT exchange transport (awaiting implementation of interface model)
!   * Consider implementation of steady-state transport (affects MST, IST)
!   * Check and handle pore space discrepancy between flow and transport (porosity vs specific yield)
!   * UZT may not have the required porosity term
!
! This classes uses strings for storing labels used by different parts of the
! code.  Labels are based on which type of transport model inherits
! from this module (GWT or GWE)
!
! Labels that are transport model specific and used in different packages:
!
!        GWT       |        GWE        | src files w/label
! -----------------|-------------------|--------------
! "Concentration"  |"Temperature"      | gwt1.f90/gwe1.f90
!                  |                   | gwt1apt1.f90
!                  |                   | gwt1cnc1.f90
!                  |                   | gwt1ist1.f90
!                  |                   | gwt1lkt1.f90
!                  |                   | gwt1mst1.f90
!                  |                   | gwt1obs1.f90
!                  |                   | gwt1oc1.f90
!                  |                   | gwt1sft1.f90 (?)
!                  |                   | gwt1ssm1.f90
!                  |                   | gwt1fmi1.f90
!                  |                   | tsp1ic1.f90
!                  |                   | GwtSpc.f90
! "Cumulative Mass"|"Cumulative Energy"| Budget.f90 (_ot routine)
! "MASS", "M"      |"ENERGY", "E"      | gwt1.f90 (gwt_df routine & _ot routine)
! "M/T"            |"Watts"  (?)       |
! "M"              |"Joules" or "E"    |

module TransportModelModule
  use KindModule, only: DP, I4B
  use InputOutputModule, only: ParseLine
  use VersionModule, only: write_listfile_header
  use ConstantsModule, only: LENFTYPE, DZERO, LENPAKLOC, LENMEMPATH, LENVARNAME
  use SimVariablesModule, only: errmsg
  use NumericalModelModule, only: NumericalModelType
  use NumericalPackageModule, only: NumericalPackageType
  use BndModule, only: BndType, GetBndFromList
  use TspIcModule, only: TspIcType
  use TspFmiModule, only: TspFmiType
  use TspAdvModule, only: TspAdvType
  use TspSsmModule, only: TspSsmType
  use TspMvtModule, only: TspMvtType
  use TspOcModule, only: TspOcType
  use TspObsModule, only: TspObsType
  use BudgetModule, only: BudgetType
  use MatrixBaseModule

  implicit none

  private

  public :: TransportModelType
  public :: niunit, cunit

  type, extends(NumericalModelType) :: TransportModelType

    ! Generalized transport package types common to either GWT or GWE
    type(TspAdvType), pointer :: adv => null() !< advection package
    type(TspFmiType), pointer :: fmi => null() !< flow model interface
    type(TspIcType), pointer :: ic => null() !< initial conditions package
    type(TspMvtType), pointer :: mvt => null() !< mover transport package
    type(TspObsType), pointer :: obs => null() !< observation package
    type(TspOcType), pointer :: oc => null() !< output control package
    type(TspSsmType), pointer :: ssm => null() !< source sink mixing package
    type(BudgetType), pointer :: budget => null() !< budget object
    integer(I4B), pointer :: inic => null() !< unit number IC
    integer(I4B), pointer :: infmi => null() !< unit number FMI
    integer(I4B), pointer :: inmvt => null() !< unit number MVT
    integer(I4B), pointer :: inadv => null() !< unit number ADV
    integer(I4B), pointer :: inssm => null() !< unit number SSM
    integer(I4B), pointer :: inoc => null() !< unit number OC
    integer(I4B), pointer :: inobs => null() !< unit number OBS
    real(DP), pointer :: eqnsclfac => null() !< constant factor by which all terms in the model's governing equation are scaled (divided) for formulation and solution
    ! Labels that will be defined
    character(len=LENVARNAME) :: tsptype = '' !< "solute" or "heat"
    character(len=LENVARNAME) :: depvartype = '' !< "concentration" or "temperature"
    character(len=LENVARNAME) :: depvarunit = '' !< "mass" or "energy"
    character(len=LENVARNAME) :: depvarunitabbrev = '' !< "M" or "J"

  contains

    ! -- public
    procedure, public :: allocate_tsp_scalars
    procedure, public :: set_tsp_labels
    procedure, public :: ftype_check
    procedure, public :: tsp_cr
    procedure, public :: tsp_df
    procedure, public :: tsp_da
    procedure, public :: tsp_ac
    procedure, public :: tsp_mc
    procedure, public :: tsp_ar
    procedure, public :: tsp_rp
    procedure, public :: tsp_ad
    procedure, public :: tsp_fc
    procedure, public :: tsp_cc
    procedure, public :: tsp_cq
    procedure, public :: tsp_bd
    procedure, public :: tsp_ot
    ! -- private
    procedure, private :: tsp_ot_obs
    procedure, private :: tsp_ot_flow
    procedure, private :: tsp_ot_flowja
    procedure, private :: tsp_ot_dv
    procedure, private :: tsp_ot_bdsummary
    procedure, private :: create_lstfile
    procedure, private :: create_tsp_packages
    procedure, private :: log_namfile_options

  end type TransportModelType

  ! -- Module variables constant for simulation
  integer(I4B), parameter :: NIUNIT = 100
  character(len=LENFTYPE), dimension(NIUNIT) :: cunit
  data cunit/'DIS6 ', 'DISV6', 'DISU6', 'IC6  ', 'MST6 ', & !  5
    'ADV6 ', 'DSP6 ', 'SSM6 ', '     ', 'CNC6 ', & ! 10
    'OC6  ', 'OBS6 ', 'FMI6 ', 'SRC6 ', 'IST6 ', & ! 15
    'LKT6 ', 'SFT6 ', 'MWT6 ', 'UZT6 ', 'MVT6 ', & ! 20
    'API6 ', '     ', 'LKE6 ', 'SFE6 ', 'MWE6 ', & ! 25
    'UZE6 ', '     ', '     ', '     ', '     ', & ! 30
    70*'     '/

contains

  !> @brief Create a new generalized transport model object
  !!
  !! Create a new transport model that will be further refined into GWT or GWE
  !<
  subroutine tsp_cr(this, filename, id, modelname, macronym, indis)
    ! -- modules
    use SimModule, only: store_error
    use MemoryManagerModule, only: mem_allocate
    use MemoryHelperModule, only: create_mem_path
    use MemoryManagerExtModule, only: mem_set_value
    use SimVariablesModule, only: idm_context
    use GwfNamInputModule, only: GwfNamParamFoundType
    use BudgetModule, only: budget_cr
    use ConstantsModule, only: LINELENGTH
    use InputOutputModule, only: upcase
    ! -- dummy
    class(TransportModelType) :: this
    character(len=*), intent(in) :: filename
    integer(I4B), intent(in) :: id
    integer(I4B), intent(inout) :: indis
    character(len=*), intent(in) :: modelname
    character(len=*), intent(in) :: macronym
    ! -- local
    character(len=LENMEMPATH) :: input_mempath
    character(len=LINELENGTH) :: lst_fname
    type(GwfNamParamFoundType) :: found
! ------------------------------------------------------------------------------
    !
    ! -- Assign values
    this%filename = filename
    this%name = modelname
    this%id = id
    this%macronym = macronym
    !
    ! -- set input model namfile memory path
    input_mempath = create_mem_path(modelname, 'NAM', idm_context)
    !
    ! -- copy option params from input context
    call mem_set_value(lst_fname, 'LIST', input_mempath, found%list)
    call mem_set_value(this%iprpak, 'PRINT_INPUT', input_mempath, &
                       found%print_input)
    call mem_set_value(this%iprflow, 'PRINT_FLOWS', input_mempath, &
                       found%print_flows)
    call mem_set_value(this%ipakcb, 'SAVE_FLOWS', input_mempath, found%save_flows)
    !
    ! -- create the list file
    call this%create_lstfile(lst_fname, filename, found%list)
    !
    ! -- activate save_flows if found
    if (found%save_flows) then
      this%ipakcb = -1
    end if
    !
    ! -- log set options
    if (this%iout > 0) then
      call this%log_namfile_options(found)
    end if
    !
    ! -- Create utility objects
    call budget_cr(this%budget, this%name)
    !
    ! -- create model packages
    call this%create_tsp_packages(indis)
    !
    ! -- Return
    return
  end subroutine tsp_cr

  !> @brief Generalized transport model define model
  !!
  !! This subroutine extended by either GWT or GWE.  This routine calls the
  !! define (df) routines for each attached package and sets variables and
  !! pointers.
  !<
  subroutine tsp_df(this)
    ! -- dummy variables
    class(TransportModelType) :: this
    !
    ! -- Return
    return
  end subroutine tsp_df

  !> @brief Generalized transport model add connections
  !!
  !! This subroutine extended by either GWT or GWE.  This routine adds the
  !! internal connections of this model to the sparse matrix
  !<
  subroutine tsp_ac(this, sparse)
    ! -- modules
    use SparseModule, only: sparsematrix
    ! -- dummy variables
    class(TransportModelType) :: this
    type(sparsematrix), intent(inout) :: sparse
    ! -- local
! ------------------------------------------------------------------------------
    !
    ! -- Return
    return
  end subroutine tsp_ac

  !> @brief Generalized transport model map coefficients
  !!
  !! This subroutine extended by either GWT or GWE.  This routine maps the
  !! positions of this models connections in the numerical solution coefficient
  !! matrix.
  !<
  subroutine tsp_mc(this, matrix_sln)
    ! -- dummy
    class(TransportModelType) :: this
    class(MatrixBaseType), pointer :: matrix_sln !< global system matrix
    ! -- local
! ------------------------------------------------------------------------------
    !
    ! -- Return
    return
  end subroutine tsp_mc

  !> @brief Generalized transport model allocate and read
  !!
  !! This subroutine extended by either GWT or GWE.  This routine calls
  !! the allocate and reads (ar) routines of attached packages and allocates
  !! memory for arrays required by the model object.
  !<
  subroutine tsp_ar(this)
    ! -- dummy variables
    class(TransportModelType) :: this
! ------------------------------------------------------------------------------
    !
    ! -- Return
    return
  end subroutine tsp_ar

  !> @brief Generalized transport model read and prepare
  !!
  !! This subroutine extended by either GWT or GWE.  This routine calls
  !! the read and prepare (rp) routines of attached packages.
  !<
  subroutine tsp_rp(this)
    ! -- dummy variables
    class(TransportModelType) :: this
! ------------------------------------------------------------------------------
    !
    ! -- Return
    return
  end subroutine tsp_rp

  !> @brief Generalized transport model time step advance
  !!
  !! This subroutine extended by either GWT or GWE.  This routine calls
  !! the advance time step (ad) routines of attached packages.
  !<
  subroutine tsp_ad(this)
    ! -- dummy variables
    class(TransportModelType) :: this
! ------------------------------------------------------------------------------
    !
    ! -- Return
    return
  end subroutine tsp_ad

  !> @brief Generalized transport model fill coefficients
  !!
  !! This subroutine extended by either GWT or GWE.  This routine calls
  !! the fill coefficients (fc) routines of attached packages.
  !<
  subroutine tsp_fc(this, kiter, matrix_sln, inwtflag)
! ******************************************************************************
! gwt_fc -- GroundWater Transport Model fill coefficients
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy variables
    class(TransportModelType) :: this
    integer(I4B), intent(in) :: kiter
    class(MatrixBaseType), pointer :: matrix_sln
    integer(I4B), intent(in) :: inwtflag
! ------------------------------------------------------------------------------
    !
    ! -- Return
    return
  end subroutine tsp_fc

  !> @brief Generalized transport model final convergence check
  !!
  !! This subroutine extended by either GWT or GWE.  This routine calls
  !! the convergence check (cc) routines of attached packages.
  !<
  subroutine tsp_cc(this, innertot, kiter, iend, icnvgmod, cpak, ipak, dpak)
    ! -- dummy
    class(TransportModelType) :: this
    integer(I4B), intent(in) :: innertot
    integer(I4B), intent(in) :: kiter
    integer(I4B), intent(in) :: iend
    integer(I4B), intent(in) :: icnvgmod
    character(len=LENPAKLOC), intent(inout) :: cpak
    integer(I4B), intent(inout) :: ipak
    real(DP), intent(inout) :: dpak
    ! -- local
! ------------------------------------------------------------------------------
    !
    ! -- Return
    return
  end subroutine tsp_cc

  !> @brief Generalized transport model calculate flows
  !!
  !! This subroutine extended by either GWT or GWE.  This routine calculates
  !! intercell flows (flowja)
  !<
  subroutine tsp_cq(this, icnvg, isuppress_output)
    ! -- dummy variables
    class(TransportModelType) :: this
    integer(I4B), intent(in) :: icnvg
    integer(I4B), intent(in) :: isuppress_output
    ! -- local
! ------------------------------------------------------------------------------
    !
    ! -- Return
    return
  end subroutine tsp_cq

  !> @brief Generalized transport model budget
  !!
  !! This subroutine extended by either GWT or GWE. This routine calculates
  !! package contributions to model budget
  !<
  subroutine tsp_bd(this, icnvg, isuppress_output)
    ! -- dummy
    class(TransportModelType) :: this
    integer(I4B), intent(in) :: icnvg
    integer(I4B), intent(in) :: isuppress_output
! ------------------------------------------------------------------------------
    !
    ! -- Return
    return
  end subroutine tsp_bd

  !> @brief Generalized transport model output routine
  !!
  !! Generalized transport model output
  !<
  subroutine tsp_ot(this, inmst)
    ! -- modules
    use TdisModule, only: kstp, kper, tdis_ot, endofperiod
    ! -- dummy
    class(TransportModelType) :: this
    integer(I4B), intent(in) :: inmst
    ! -- local
    integer(I4B) :: idvsave
    integer(I4B) :: idvprint
    integer(I4B) :: icbcfl
    integer(I4B) :: icbcun
    integer(I4B) :: ibudfl
    integer(I4B) :: ipflag
    ! -- formats
    character(len=*), parameter :: fmtnocnvg = &
      "(1X,/9X,'****FAILED TO MEET SOLVER CONVERGENCE CRITERIA IN TIME STEP ', &
      &I0,' OF STRESS PERIOD ',I0,'****')"
! ------------------------------------------------------------------------------
    !
    ! -- Set write and print flags
    idvsave = 0
    idvprint = 0
    icbcfl = 0
    ibudfl = 0
    if (this%oc%oc_save(trim(this%depvartype))) idvsave = 1
    if (this%oc%oc_print(trim(this%depvartype))) idvprint = 1
    if (this%oc%oc_save('BUDGET')) icbcfl = 1
    if (this%oc%oc_print('BUDGET')) ibudfl = 1
    icbcun = this%oc%oc_save_unit('BUDGET')
    !
    ! -- Override ibudfl and idvprint flags for nonconvergence
    !    and end of period
    ibudfl = this%oc%set_print_flag('BUDGET', this%icnvg, endofperiod)
    idvprint = this%oc%set_print_flag(trim(this%depvartype), &
                                      this%icnvg, endofperiod)
    !
    !   Calculate and save observations
    call this%tsp_ot_obs()
    !
    !   Save and print flows
    call this%tsp_ot_flow(icbcfl, ibudfl, icbcun, inmst)
    !
    !   Save and print dependent variables
    call this%tsp_ot_dv(idvsave, idvprint, ipflag)
    !
    !   Print budget summaries
    call this%tsp_ot_bdsummary(ibudfl, ipflag)
    !
    ! -- Timing Output; if any dependendent variables or budgets
    !    are printed, then ipflag is set to 1.
    if (ipflag == 1) call tdis_ot(this%iout)
    !
    ! -- Write non-convergence message
    if (this%icnvg == 0) then
      write (this%iout, fmtnocnvg) kstp, kper
    end if
    !
    ! -- Return
    return
  end subroutine tsp_ot

  !> @brief Generalized transport model output routine
  !!
  !! Calculate and save observations
  !<
  subroutine tsp_ot_obs(this)
    class(TransportModelType) :: this
    class(BndType), pointer :: packobj
    integer(I4B) :: ip
! ------------------------------------------------------------------------------
    ! -- Calculate and save observations
    call this%obs%obs_bd()
    call this%obs%obs_ot()

    ! -- Calculate and save package obserations
    do ip = 1, this%bndlist%Count()
      packobj => GetBndFromList(this%bndlist, ip)
      call packobj%bnd_bd_obs()
      call packobj%bnd_ot_obs()
    end do

  end subroutine tsp_ot_obs

  !> @brief Generalized transport model output routine
  !!
  !! Save and print flows
  !<
  subroutine tsp_ot_flow(this, icbcfl, ibudfl, icbcun, inmst)
    ! -- dummy
    class(TransportModelType) :: this
    integer(I4B), intent(in) :: icbcfl
    integer(I4B), intent(in) :: ibudfl
    integer(I4B), intent(in) :: icbcun
    integer(I4B), intent(in) :: inmst
    ! -- local
    class(BndType), pointer :: packobj
    integer(I4B) :: ip
! ------------------------------------------------------------------------------
    ! -- Save TSP flows
    call this%tsp_ot_flowja(this%nja, this%flowja, icbcfl, icbcun)
    if (this%infmi > 0) call this%fmi%fmi_ot_flow(icbcfl, icbcun)
    if (this%inssm > 0) then
      call this%ssm%ssm_ot_flow(icbcfl=icbcfl, ibudfl=0, icbcun=icbcun)
    end if
    do ip = 1, this%bndlist%Count()
      packobj => GetBndFromList(this%bndlist, ip)
      call packobj%bnd_ot_model_flows(icbcfl=icbcfl, ibudfl=0, icbcun=icbcun)
    end do

    ! -- Save advanced package flows
    do ip = 1, this%bndlist%Count()
      packobj => GetBndFromList(this%bndlist, ip)
      call packobj%bnd_ot_package_flows(icbcfl=icbcfl, ibudfl=0)
    end do
    if (this%inmvt > 0) then
      call this%mvt%mvt_ot_saveflow(icbcfl, ibudfl)
    end if

    ! -- Print Model (GWT or GWE) flows
    ! no need to print flowja
    ! no need to print mst
    ! no need to print fmi
    if (this%inssm > 0) then
      call this%ssm%ssm_ot_flow(icbcfl=icbcfl, ibudfl=ibudfl, icbcun=0)
    end if
    do ip = 1, this%bndlist%Count()
      packobj => GetBndFromList(this%bndlist, ip)
      call packobj%bnd_ot_model_flows(icbcfl=icbcfl, ibudfl=ibudfl, icbcun=0)
    end do

    ! -- Print advanced package flows
    do ip = 1, this%bndlist%Count()
      packobj => GetBndFromList(this%bndlist, ip)
      call packobj%bnd_ot_package_flows(icbcfl=0, ibudfl=ibudfl)
    end do
    if (this%inmvt > 0) then
      call this%mvt%mvt_ot_printflow(icbcfl, ibudfl)
    end if

  end subroutine tsp_ot_flow

  !> @brief Generalized transport model output routine
  !!
  !! Write intercell flows for the transport model
  !<
  subroutine tsp_ot_flowja(this, nja, flowja, icbcfl, icbcun)
    ! -- dummy
    class(TransportModelType) :: this
    integer(I4B), intent(in) :: nja
    real(DP), dimension(nja), intent(in) :: flowja
    integer(I4B), intent(in) :: icbcfl
    integer(I4B), intent(in) :: icbcun
    ! -- local
    integer(I4B) :: ibinun
    ! -- formats
! ------------------------------------------------------------------------------
    !
    ! -- Set unit number for binary output
    if (this%ipakcb < 0) then
      ibinun = icbcun
    elseif (this%ipakcb == 0) then
      ibinun = 0
    else
      ibinun = this%ipakcb
    end if
    if (icbcfl == 0) ibinun = 0
    !
    ! -- Write the face flows if requested
    if (ibinun /= 0) then
      call this%dis%record_connection_array(flowja, ibinun, this%iout)
    end if
    !
    ! -- Return
    return
  end subroutine tsp_ot_flowja

  !> @brief Generalized tranpsort model output routine
  !!
  !! Loop through attached packages saving and printing dependent variables
  !<
  subroutine tsp_ot_dv(this, idvsave, idvprint, ipflag)
    class(TransportModelType) :: this
    integer(I4B), intent(in) :: idvsave
    integer(I4B), intent(in) :: idvprint
    integer(I4B), intent(inout) :: ipflag
    class(BndType), pointer :: packobj
    integer(I4B) :: ip
! ------------------------------------------------------------------------------
    ! -- Print advanced package dependent variables
    do ip = 1, this%bndlist%Count()
      packobj => GetBndFromList(this%bndlist, ip)
      call packobj%bnd_ot_dv(idvsave, idvprint)
    end do

    ! -- save head and print head
    call this%oc%oc_ot(ipflag)
    !
    ! -- Return
    return
  end subroutine tsp_ot_dv

  !> @brief Generalized tranpsort model output budget summary
  !!
  !! Loop through attached packages and write budget summaries
  !<
  subroutine tsp_ot_bdsummary(this, ibudfl, ipflag)
    use TdisModule, only: kstp, kper, totim
    class(TransportModelType) :: this
    integer(I4B), intent(in) :: ibudfl
    integer(I4B), intent(inout) :: ipflag
    class(BndType), pointer :: packobj
    integer(I4B) :: ip

    !
    ! -- Package budget summary
    do ip = 1, this%bndlist%Count()
      packobj => GetBndFromList(this%bndlist, ip)
      call packobj%bnd_ot_bdsummary(kstp, kper, this%iout, ibudfl)
    end do

    ! -- mover budget summary
    if (this%inmvt > 0) then
      call this%mvt%mvt_ot_bdsummary(ibudfl)
    end if

    ! -- model budget summary
    if (ibudfl /= 0) then
      ipflag = 1
      call this%budget%budget_ot(kstp, kper, this%iout)
    end if

    ! -- Write to budget csv
    call this%budget%writecsv(totim)
    !
    ! -- Return
    return
  end subroutine tsp_ot_bdsummary

  !> @brief Allocate scalar variables for transport model
  !!
  !!  Method to allocate memory for non-allocatable members.
  !<
  subroutine allocate_tsp_scalars(this, modelname)
    ! -- modules
    use MemoryManagerModule, only: mem_allocate
    ! -- dummy
    class(TransportModelType) :: this
    character(len=*), intent(in) :: modelname
! ------------------------------------------------------------------------------
    !
    ! -- allocate members from (grand)parent class
    call this%NumericalModelType%allocate_scalars(modelname)
    !
    ! -- allocate members that are part of model class
    call mem_allocate(this%inic, 'INIC', this%memoryPath)
    call mem_allocate(this%infmi, 'INFMI', this%memoryPath)
    call mem_allocate(this%inmvt, 'INMVT', this%memoryPath)
    call mem_allocate(this%inadv, 'INADV', this%memoryPath)
    call mem_allocate(this%inssm, 'INSSM', this%memoryPath)
    call mem_allocate(this%inoc, 'INOC ', this%memoryPath)
    call mem_allocate(this%inobs, 'INOBS', this%memoryPath)
    call mem_allocate(this%eqnsclfac, 'EQNSCLFAC', this%memoryPath)
    !
    this%inic = 0
    this%infmi = 0
    this%inmvt = 0
    this%inadv = 0
    this%inssm = 0
    this%inoc = 0
    this%inobs = 0
    this%eqnsclfac = DZERO
    !
    ! -- Return
    return
  end subroutine allocate_tsp_scalars

  !> @brief Define the labels corresponding to the flavor of
  !! transport model
  !!
  !! Set variable names according to type of transport model
  !<
  subroutine set_tsp_labels(this, tsptype, depvartype, depvarunit, &
                            depvarunitabbrev)
    class(TransportModelType) :: this
    character(len=*), intent(in), pointer :: tsptype !< type of model, default is GWT (alternative is GWE)
    character(len=*), intent(in) :: depvartype !< dependent variable type, default is "CONCENTRATION"
    character(len=*), intent(in) :: depvarunit !< units of dependent variable for writing to list file
    character(len=*), intent(in) :: depvarunitabbrev !< abbreviation of associated units
    !
    ! -- Set the model type
    this%tsptype = tsptype
    !
    ! -- Set the type of dependent variable being solved for
    this%depvartype = depvartype
    !
    ! -- Set the units associated with the dependent variable
    this%depvarunit = depvarunit
    !
    ! -- Set the units abbreviation
    this%depvarunitabbrev = depvarunitabbrev
    !
    ! -- Return
    return
  end subroutine set_tsp_labels

  !> @brief Deallocate memory
  !!
  !! Deallocate memmory at conclusion of model run
  !<
  subroutine tsp_da(this)
    ! -- modules
    use MemoryManagerModule, only: mem_deallocate
    ! -- dummy
    class(TransportModelType) :: this
    ! -- local
! ------------------------------------------------------------------------------
    !
    ! -- Scalars
    call mem_deallocate(this%inic)
    call mem_deallocate(this%infmi)
    call mem_deallocate(this%inadv)
    call mem_deallocate(this%inssm)
    call mem_deallocate(this%inmvt)
    call mem_deallocate(this%inoc)
    call mem_deallocate(this%inobs)
    call mem_deallocate(this%eqnsclfac)
    !
    ! -- Return
    return
  end subroutine tsp_da

  !> @brief Generalized tranpsort model routine
  !!
  !! Check to make sure required input files have been specified
  !<
  subroutine ftype_check(this, indis, inmst)
    ! -- modules
    use ConstantsModule, only: LINELENGTH
    use SimModule, only: store_error, count_errors, store_error_filename
    ! -- dummy
    class(TransportModelType) :: this
    integer(I4B), intent(in) :: indis
    integer(I4B), intent(in) :: inmst
    ! -- local
    character(len=LINELENGTH) :: errmsg
! ------------------------------------------------------------------------------
    !
    ! -- Check for IC6, DIS(u), and MST. Stop if not present.
    if (this%inic == 0) then
      write (errmsg, '(a)') &
        'Initial conditions (IC6) package not specified.'
      call store_error(errmsg)
    end if
    if (indis == 0) then
      write (errmsg, '(a)') &
        'Discretization (DIS6 or DISU6) package not specified.'
      call store_error(errmsg)
    end if
    if (inmst == 0) then
      write (errmsg, '(a)') 'Mass storage and transfer (MST6) &
        &package not specified.'
      call store_error(errmsg)
    end if
    !
    if (count_errors() > 0) then
      write (errmsg, '(a)') 'Required package(s) not specified.'
      call store_error(errmsg)
      call store_error_filename(this%filename)
    end if
    !
    ! -- Return
    return
  end subroutine ftype_check

  !> @brief Create listing output file
  !<
  subroutine create_lstfile(this, lst_fname, model_fname, defined)
    ! -- modules
    use KindModule, only: LGP
    use InputOutputModule, only: openfile, getunit
    ! -- dummy
    class(TransportModelType) :: this
    character(len=*), intent(inout) :: lst_fname
    character(len=*), intent(in) :: model_fname
    logical(LGP), intent(in) :: defined
    ! -- local
    integer(I4B) :: i, istart, istop
    !
    ! -- set list file name if not provided
    if (.not. defined) then
      !
      ! -- initialize
      lst_fname = ' '
      istart = 0
      istop = len_trim(model_fname)
      !
      ! -- identify '.' character position from back of string
      do i = istop, 1, -1
        if (model_fname(i:i) == '.') then
          istart = i
          exit
        end if
      end do
      !
      ! -- if not found start from string end
      if (istart == 0) istart = istop + 1
      !
      ! -- set list file name
      lst_fname = model_fname(1:istart)
      istop = istart + 3
      lst_fname(istart:istop) = '.lst'
    end if
    !
    ! -- create the list file
    this%iout = getunit()
    call openfile(this%iout, 0, lst_fname, 'LIST', filstat_opt='REPLACE')
    !
    ! -- write list file header
    call write_listfile_header(this%iout, 'GROUNDWATER TRANSPORT MODEL (GWT)')
    !
    ! -- Return
    return
  end subroutine create_lstfile

  !> @brief Write model name file options to list file
  !<
  subroutine log_namfile_options(this, found)
    use GwfNamInputModule, only: GwfNamParamFoundType
    class(TransportModelType) :: this
    type(GwfNamParamFoundType), intent(in) :: found

    write (this%iout, '(1x,a)') 'NAMEFILE OPTIONS:'

    if (found%newton) then
      write (this%iout, '(4x,a)') &
        'NEWTON-RAPHSON method enabled for the model.'
      if (found%under_relaxation) then
        write (this%iout, '(4x,a,a)') &
          'NEWTON-RAPHSON UNDER-RELAXATION based on the bottom ', &
          'elevation of the model will be applied to the model.'
      end if
    end if

    if (found%print_input) then
      write (this%iout, '(4x,a)') 'STRESS PACKAGE INPUT WILL BE PRINTED '// &
        'FOR ALL MODEL STRESS PACKAGES'
    end if

    if (found%print_flows) then
      write (this%iout, '(4x,a)') 'PACKAGE FLOWS WILL BE PRINTED '// &
        'FOR ALL MODEL PACKAGES'
    end if

    if (found%save_flows) then
      write (this%iout, '(4x,a)') &
        'FLOWS WILL BE SAVED TO BUDGET FILE SPECIFIED IN OUTPUT CONTROL'
    end if

    write (this%iout, '(1x,a)') 'END NAMEFILE OPTIONS:'
  end subroutine log_namfile_options

  !> @brief Source package info and begin to process
  !<
  subroutine create_tsp_packages(this, indis)
    ! -- modules
    use ConstantsModule, only: LINELENGTH, LENPACKAGENAME
    use CharacterStringModule, only: CharacterStringType
    use ArrayHandlersModule, only: expandarray
    use MemoryManagerModule, only: mem_setptr
    use MemoryHelperModule, only: create_mem_path
    use SimVariablesModule, only: idm_context
    use GwfDisModule, only: dis_cr
    use GwfDisvModule, only: disv_cr
    use GwfDisuModule, only: disu_cr
    use TspIcModule, only: ic_cr
    use TspFmiModule, only: fmi_cr
    use TspAdvModule, only: adv_cr
    use TspSsmModule, only: ssm_cr
    use TspMvtModule, only: mvt_cr
    use TspOcModule, only: oc_cr
    use TspObsModule, only: tsp_obs_cr
    ! -- dummy
    class(TransportModelType) :: this
    integer(I4B), intent(inout) :: indis ! DIS enabled flag
    ! -- local
    type(CharacterStringType), dimension(:), contiguous, &
      pointer :: pkgtypes => null()
    type(CharacterStringType), dimension(:), contiguous, &
      pointer :: pkgnames => null()
    type(CharacterStringType), dimension(:), contiguous, &
      pointer :: mempaths => null()
    integer(I4B), dimension(:), contiguous, &
      pointer :: inunits => null()
    character(len=LENMEMPATH) :: model_mempath
    character(len=LENFTYPE) :: pkgtype
    character(len=LENPACKAGENAME) :: pkgname
    character(len=LENMEMPATH) :: mempath
    integer(I4B), pointer :: inunit
    integer(I4B) :: n
    !
    ! -- Initialize
    indis = 0
    !
    ! -- set input memory paths, input/model and input/model/namfile
    model_mempath = create_mem_path(component=this%name, context=idm_context)
    !
    ! -- set pointers to model path package info
    call mem_setptr(pkgtypes, 'PKGTYPES', model_mempath)
    call mem_setptr(pkgnames, 'PKGNAMES', model_mempath)
    call mem_setptr(mempaths, 'MEMPATHS', model_mempath)
    call mem_setptr(inunits, 'INUNITS', model_mempath)
    !
    do n = 1, size(pkgtypes)
      !
      ! attributes for this input package
      pkgtype = pkgtypes(n)
      pkgname = pkgnames(n)
      mempath = mempaths(n)
      inunit => inunits(n)
      !
      ! -- create dis package as it is a prerequisite for other packages
      select case (pkgtype)
      case ('DIS6')
        indis = 1
        call dis_cr(this%dis, this%name, mempath, indis, this%iout)
      case ('DISV6')
        indis = 1
        call disv_cr(this%dis, this%name, mempath, indis, this%iout)
      case ('DISU6')
        indis = 1
        call disu_cr(this%dis, this%name, mempath, indis, this%iout)
      case ('IC6')
        this%inic = inunit
      case ('FMI6')
        this%infmi = inunit
      case ('MVT6')
        this%inmvt = inunit
      case ('ADV6')
        this%inadv = inunit
      case ('SSM6')
        this%inssm = inunit
      case ('OC6')
        this%inoc = inunit
      case ('OBS6')
        this%inobs = inunit
        !case default
        ! TODO
      end select
    end do
    !
    ! -- Create packages that are tied directly to model
    call ic_cr(this%ic, this%name, this%inic, this%iout, this%dis, &
               this%depvartype)
    call fmi_cr(this%fmi, this%name, this%infmi, this%iout, this%eqnsclfac, &
                this%depvartype)
    call adv_cr(this%adv, this%name, this%inadv, this%iout, this%fmi, &
                this%eqnsclfac)
    call ssm_cr(this%ssm, this%name, this%inssm, this%iout, this%fmi, &
                this%eqnsclfac, this%depvartype)
    call mvt_cr(this%mvt, this%name, this%inmvt, this%iout, this%fmi, &
                this%eqnsclfac)
    call oc_cr(this%oc, this%name, this%inoc, this%iout)
    call tsp_obs_cr(this%obs, this%inobs)
    !
    ! -- Return
    return
  end subroutine create_tsp_packages

end module TransportModelModule
