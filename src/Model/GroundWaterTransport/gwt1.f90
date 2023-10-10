! Groundwater Transport (GWT) Model
! The following are additional features/checks to add
!   * Add check that discretization is the same between both models
!   * Consider implementation of steady-state transport (affects MST, IST)
!   * Check and handle pore space discrepancy between flow and transport (porosity vs specific yield)
!   * UZT may not have the required porosity term

module GwtModule

  use KindModule, only: DP, I4B
  use ConstantsModule, only: LENFTYPE, LENMEMPATH, DZERO, DONE, &
                             LENPAKLOC, LENVARNAME
  use VersionModule, only: write_listfile_header
  use NumericalModelModule, only: NumericalModelType
  use TransportModelModule, only: TransportModelType
  use BaseModelModule, only: BaseModelType
  use BndModule, only: BndType, AddBndToList, GetBndFromList
  use TspFmiModule, only: TspFmiType
  use GwtDspModule, only: GwtDspType
  use GwtSsmModule, only: GwtSsmType
  use GwtMvtModule, only: GwtMvtType
  use GwtMstModule, only: GwtMstType
  use GwtOcModule, only: GwtOcType
  use GwtObsModule, only: GwtObsType
  use BudgetModule, only: BudgetType
  use MatrixBaseModule

  implicit none

  private
  public :: gwt_cr
  public :: GwtModelType
  public :: CastAsGwtModel
  character(len=LENVARNAME), parameter :: dvt = 'CONCENTRATION   ' !< dependent variable type, varies based on model type
  character(len=LENVARNAME), parameter :: dvu = 'MASS            ' !< dependent variable unit of measure, either "mass" or "energy"
  character(len=LENVARNAME), parameter :: dvua = 'M               ' !< abbreviation of the dependent variable unit of measure, either "M" or "E"

  type, extends(TransportModelType) :: GwtModelType

    type(GwtMstType), pointer :: mst => null() ! mass storage and transfer package
    type(GwtDspType), pointer :: dsp => null() ! dispersion package
    type(GwtSsmType), pointer :: ssm => null() ! source sink mixing package
    type(GwtMvtType), pointer :: mvt => null() ! mover transport package
    type(GwtOcType), pointer :: oc => null() ! output control package
    type(GwtObsType), pointer :: obs => null() ! observation package
    integer(I4B), pointer :: inmvt => null() ! unit number MVT
    integer(I4B), pointer :: inmst => null() ! unit number MST
    integer(I4B), pointer :: indsp => null() ! DSP enabled flag
    integer(I4B), pointer :: inssm => null() ! unit number SSM
    integer(I4B), pointer :: inoc => null() ! unit number OC
    integer(I4B), pointer :: inobs => null() ! unit number OBS

  contains

    procedure :: model_df => gwt_df
    procedure :: model_ac => gwt_ac
    procedure :: model_mc => gwt_mc
    procedure :: model_ar => gwt_ar
    procedure :: model_rp => gwt_rp
    procedure :: model_ad => gwt_ad
    procedure :: model_cf => gwt_cf
    procedure :: model_fc => gwt_fc
    procedure :: model_cc => gwt_cc
    procedure :: model_cq => gwt_cq
    procedure :: model_bd => gwt_bd
    procedure :: model_ot => gwt_ot
    procedure :: model_da => gwt_da
    procedure :: model_bdentry => gwt_bdentry
    procedure :: allocate_scalars
    procedure :: get_iasym => gwt_get_iasym
    procedure, private :: gwt_ot_flow
    procedure, private :: gwt_ot_flowja
    procedure, private :: gwt_ot_dv
    procedure, private :: gwt_ot_bdsummary
    procedure, private :: gwt_ot_obs
    procedure :: create_packages => create_gwt_packages
    procedure, private :: create_bndpkgs
    procedure, private :: package_create

  end type GwtModelType

contains

  !> @brief Create a new groundwater transport model object
  !<
  subroutine gwt_cr(filename, id, modelname)
    ! -- modules
    use ListsModule, only: basemodellist
    use BaseModelModule, only: AddBaseModelToList
    use ConstantsModule, only: LINELENGTH
    use MemoryHelperModule, only: create_mem_path
    use MemoryManagerExtModule, only: mem_set_value
    use SimVariablesModule, only: idm_context
    use GwfNamInputModule, only: GwfNamParamFoundType
    use BudgetModule, only: budget_cr
    ! -- dummy
    character(len=*), intent(in) :: filename
    integer(I4B), intent(in) :: id
    character(len=*), intent(in) :: modelname
    ! -- local
    integer(I4B) :: indis
    type(GwtModelType), pointer :: this
    class(BaseModelType), pointer :: model
    character(len=LENMEMPATH) :: input_mempath
    character(len=LINELENGTH) :: lst_fname
    type(GwfNamParamFoundType) :: found
    !
    ! -- Allocate a new GWT Model (this)
    allocate (this)
    !
    ! -- Set memory path before allocation in memory manager can be done
    this%memoryPath = create_mem_path(modelname)
    !
    ! -- Allocate scalars and add model to basemodellist
    call this%allocate_scalars(modelname)
    !
    ! -- set labels for transport model - needed by create_packages() below
    call this%set_tsp_labels(this%macronym, dvt, dvu, dvua)
    !
    model => this
    call AddBaseModelToList(basemodellist, model)
    !
    ! -- Assign values
    this%filename = filename
    this%name = modelname
    this%macronym = 'GWT'
    this%id = id
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
    ! -- activate save_flows if found
    if (found%save_flows) then
      this%ipakcb = -1
    end if
    !
    ! -- Create utility objects
    call budget_cr(this%budget, this%name)
    !
    ! -- Call parent class routine
    call this%tsp_cr(filename, id, modelname, indis)
    !
    ! -- create model packages
    call this%create_packages(indis)
    !
    ! -- Return
    return
  end subroutine gwt_cr

  !> @brief Define packages of the GWT model
  !!
  !! This subroutine defines a gwt model type. Steps include:
  !!  (1) call df routines for each package
  !!  (2) set variables and pointers
  !<
  subroutine gwt_df(this)
    ! -- modules
    use ModelPackageInputsModule, only: NIUNIT_GWT
    use SimModule, only: store_error
    ! -- dummy
    class(GwtModelType) :: this
    ! -- local
    integer(I4B) :: ip
    class(BndType), pointer :: packobj
    !
    ! -- Define packages and utility objects
    call this%dis%dis_df()
    call this%fmi%fmi_df(this%dis)
    if (this%inmvt > 0) call this%mvt%mvt_df(this%dis)
    if (this%inadv > 0) call this%adv%adv_df()
    if (this%indsp > 0) call this%dsp%dsp_df(this%dis)
    if (this%inssm > 0) call this%ssm%ssm_df()
    call this%oc%oc_df()
    call this%budget%budget_df(NIUNIT_GWT, 'MASS', 'M')
    !
    ! -- Check for SSM package
    if (this%inssm == 0) then
      if (this%fmi%nflowpack > 0) then
        call store_error('Flow model has boundary packages, but there &
          &is no SSM package.  The SSM package must be activated.', &
          terminate=.TRUE.)
      end if
    end if
    !
    ! -- Assign or point model members to dis members
    this%neq = this%dis%nodes
    this%nja = this%dis%nja
    this%ia => this%dis%con%ia
    this%ja => this%dis%con%ja
    !
    ! -- Allocate model arrays, now that neq and nja are assigned
    call this%allocate_arrays()
    !
    ! -- Define packages and assign iout for time series managers
    do ip = 1, this%bndlist%Count()
      packobj => GetBndFromList(this%bndlist, ip)
      call packobj%bnd_df(this%neq, this%dis)
      packobj%TsManager%iout = this%iout
      packobj%TasManager%iout = this%iout
    end do
    !
    ! -- Store information needed for observations
    call this%obs%obs_df(this%iout, this%name, 'GWT', this%dis)
    !
    ! -- Return
    return
  end subroutine gwt_df

  !> @brief Add the internal connections of this model to the sparse matrix
  !<
  subroutine gwt_ac(this, sparse)
    ! -- modules
    use SparseModule, only: sparsematrix
    ! -- dummy
    class(GwtModelType) :: this
    type(sparsematrix), intent(inout) :: sparse
    ! -- local
    class(BndType), pointer :: packobj
    integer(I4B) :: ip
    !
    ! -- Add the internal connections of this model to sparse
    call this%dis%dis_ac(this%moffset, sparse)
    if (this%indsp > 0) &
      call this%dsp%dsp_ac(this%moffset, sparse)
    !
    ! -- Add any package connections
    do ip = 1, this%bndlist%Count()
      packobj => GetBndFromList(this%bndlist, ip)
      call packobj%bnd_ac(this%moffset, sparse)
    end do
    !
    ! -- Return
    return
  end subroutine gwt_ac

  !> @brief Map the positions of the GWT model connections in the numerical
  !! solution coefficient matrix.
  !<
  subroutine gwt_mc(this, matrix_sln)
    ! -- dummy
    class(GwtModelType) :: this
    class(MatrixBaseType), pointer :: matrix_sln !< global system matrix
    ! -- local
    class(BndType), pointer :: packobj
    integer(I4B) :: ip
    !
    ! -- Find the position of each connection in the global ia, ja structure
    !    and store them in idxglo.
    call this%dis%dis_mc(this%moffset, this%idxglo, matrix_sln)
    !
    if (this%indsp > 0) call this%dsp%dsp_mc(this%moffset, matrix_sln)
    !
    ! -- Map any package connections
    do ip = 1, this%bndlist%Count()
      packobj => GetBndFromList(this%bndlist, ip)
      call packobj%bnd_mc(this%moffset, matrix_sln)
    end do
    !
    ! -- Return
    return
  end subroutine gwt_mc

  !> @brief GWT Model Allocate and Read
  !!
  !! This subroutine:
  !!   - allocates and reads packages that are part of this model,
  !!   - allocates memory for arrays used by this model object
  !<
  subroutine gwt_ar(this)
    ! -- modules
    use ConstantsModule, only: DHNOFLO
    ! -- dummy
    class(GwtModelType) :: this
    ! -- locals
    integer(I4B) :: ip
    class(BndType), pointer :: packobj
    !
    ! -- Allocate and read modules attached to model
    call this%fmi%fmi_ar(this%ibound)
    if (this%inmvt > 0) call this%mvt%mvt_ar()
    if (this%inic > 0) call this%ic%ic_ar(this%x)
    if (this%inmst > 0) call this%mst%mst_ar(this%dis, this%ibound)
    if (this%inadv > 0) call this%adv%adv_ar(this%dis, this%ibound)
    if (this%indsp > 0) call this%dsp%dsp_ar(this%ibound, this%mst%thetam)
    if (this%inssm > 0) call this%ssm%ssm_ar(this%dis, this%ibound, this%x)
    if (this%inobs > 0) call this%obs%gwt_obs_ar(this%ic, this%x, this%flowja)
    !
    ! -- Set governing equation scale factor. Note that this scale factor
    ! -- cannot be set arbitrarily. For solute transport, it must be set
    ! -- to 1.  Setting it to a different value will NOT automatically
    ! -- scale all the terms of the governing equation correctly by that
    ! -- value. This is because much of the coding in the associated
    ! -- packages implicitly assumes the governing equation for solute
    ! -- transport is scaled by 1. (effectively unscaled).
    this%eqnsclfac = DONE
    !
    ! -- Call dis_ar to write binary grid file
    !call this%dis%dis_ar(this%npf%icelltype)
    !
    ! -- set up output control
    call this%oc%oc_ar(this%x, this%dis, DHNOFLO)
    call this%budget%set_ibudcsv(this%oc%ibudcsv)
    !
    ! -- Package input files now open, so allocate and read
    do ip = 1, this%bndlist%Count()
      packobj => GetBndFromList(this%bndlist, ip)
      call packobj%set_pointers(this%dis%nodes, this%ibound, this%x, &
                                this%xold, this%flowja)
      ! -- Read and allocate package
      call packobj%bnd_ar()
    end do
    !
    ! -- Return
    return
  end subroutine gwt_ar

  !> @brief GWT Model Read and Prepare
  !!
  !! Call the read and prepare routines of the attached packages
  !<
  subroutine gwt_rp(this)
    ! -- modules
    use TdisModule, only: readnewdata
    ! -- dummy
    class(GwtModelType) :: this
    ! -- local
    class(BndType), pointer :: packobj
    integer(I4B) :: ip
    !
    ! -- In fmi, check for mvt and mvrbudobj consistency
    call this%fmi%fmi_rp(this%inmvt)
    if (this%inmvt > 0) call this%mvt%mvt_rp()
    !
    ! -- Check with TDIS on whether or not it is time to RP
    if (.not. readnewdata) return
    !
    ! -- Read and prepare
    if (this%inoc > 0) call this%oc%oc_rp()
    if (this%inssm > 0) call this%ssm%ssm_rp()
    do ip = 1, this%bndlist%Count()
      packobj => GetBndFromList(this%bndlist, ip)
      call packobj%bnd_rp()
      call packobj%bnd_rp_obs()
    end do
    !
    ! -- Return
    return
  end subroutine gwt_rp

  !> @brief GWT Model Time Step Advance
  !!
  !! Call the advance subroutines of the attached packages
  !<
  subroutine gwt_ad(this)
    ! -- modules
    use SimVariablesModule, only: isimcheck, iFailedStepRetry
    ! -- dummy
    class(GwtModelType) :: this
    class(BndType), pointer :: packobj
    ! -- local
    integer(I4B) :: irestore
    integer(I4B) :: ip, n
    !
    ! -- Reset state variable
    irestore = 0
    if (iFailedStepRetry > 0) irestore = 1
    if (irestore == 0) then
      !
      ! -- copy x into xold
      do n = 1, this%dis%nodes
        if (this%ibound(n) == 0) then
          this%xold(n) = DZERO
        else
          this%xold(n) = this%x(n)
        end if
      end do
    else
      !
      ! -- copy xold into x if this time step is a redo
      do n = 1, this%dis%nodes
        this%x(n) = this%xold(n)
      end do
    end if
    !
    ! -- Advance fmi
    call this%fmi%fmi_ad(this%x)
    !
    ! -- Advance
    !if(this%inmst > 0) call this%mst%mst_ad()
    if (this%indsp > 0) call this%dsp%dsp_ad()
    if (this%inssm > 0) call this%ssm%ssm_ad()
    do ip = 1, this%bndlist%Count()
      packobj => GetBndFromList(this%bndlist, ip)
      call packobj%bnd_ad()
      if (isimcheck > 0) then
        call packobj%bnd_ck()
      end if
    end do
    !
    ! -- Push simulated values to preceding time/subtime step
    call this%obs%obs_ad()
    !
    ! -- Return
    return
  end subroutine gwt_ad

  !> @brief GWT Model calculate coefficients
  !!
  !! Call the calculate coefficients subroutines of the attached packages
  !<
  subroutine gwt_cf(this, kiter)
    ! -- modules
    ! -- dummy
    class(GwtModelType) :: this
    integer(I4B), intent(in) :: kiter
    ! -- local
    class(BndType), pointer :: packobj
    integer(I4B) :: ip
    !
    ! -- Call package cf routines
    do ip = 1, this%bndlist%Count()
      packobj => GetBndFromList(this%bndlist, ip)
      call packobj%bnd_cf()
    end do
    !
    ! -- Return
    return
  end subroutine gwt_cf

  !> @brief GWT Model fill coefficients
  !!
  !! Call the fill coefficients subroutines attached packages
  !<
  subroutine gwt_fc(this, kiter, matrix_sln, inwtflag)
    ! -- modules
    ! -- dummy
    class(GwtModelType) :: this
    integer(I4B), intent(in) :: kiter
    class(MatrixBaseType), pointer :: matrix_sln
    integer(I4B), intent(in) :: inwtflag
    ! -- local
    class(BndType), pointer :: packobj
    integer(I4B) :: ip
    !
    ! -- call fc routines
    call this%fmi%fmi_fc(this%dis%nodes, this%xold, this%nja, matrix_sln, &
                         this%idxglo, this%rhs)
    if (this%inmvt > 0) then
      call this%mvt%mvt_fc(this%x, this%x)
    end if
    if (this%inmst > 0) then
      call this%mst%mst_fc(this%dis%nodes, this%xold, this%nja, matrix_sln, &
                           this%idxglo, this%x, this%rhs, kiter)
    end if
    if (this%inadv > 0) then
      call this%adv%adv_fc(this%dis%nodes, matrix_sln, this%idxglo, this%x, &
                           this%rhs)
    end if
    if (this%indsp > 0) then
      call this%dsp%dsp_fc(kiter, this%dis%nodes, this%nja, matrix_sln, &
                           this%idxglo, this%rhs, this%x)
    end if
    if (this%inssm > 0) then
      call this%ssm%ssm_fc(matrix_sln, this%idxglo, this%rhs)
    end if
    !
    ! -- packages
    do ip = 1, this%bndlist%Count()
      packobj => GetBndFromList(this%bndlist, ip)
      call packobj%bnd_fc(this%rhs, this%ia, this%idxglo, matrix_sln)
    end do
    !
    ! -- Return
    return
  end subroutine gwt_fc

  !> @brief GWT Model Final Convergence Check
  !!
  !! If MVR/MVT is active, call the MVR convergence check subroutines
  !<
  subroutine gwt_cc(this, innertot, kiter, iend, icnvgmod, cpak, ipak, dpak)
    ! -- dummy
    class(GwtModelType) :: this
    integer(I4B), intent(in) :: innertot
    integer(I4B), intent(in) :: kiter
    integer(I4B), intent(in) :: iend
    integer(I4B), intent(in) :: icnvgmod
    character(len=LENPAKLOC), intent(inout) :: cpak
    integer(I4B), intent(inout) :: ipak
    real(DP), intent(inout) :: dpak
    ! -- local
    ! class(BndType), pointer :: packobj
    ! integer(I4B) :: ip
    ! -- formats
    !
    ! -- If mover is on, then at least 2 outers required
    if (this%inmvt > 0) call this%mvt%mvt_cc(kiter, iend, icnvgmod, cpak, dpak)
    !
    ! -- Call package cc routines
    ! do ip = 1, this%bndlist%Count()
    !   packobj => GetBndFromList(this%bndlist, ip)
    !   call packobj%bnd_cc(iend, icnvg, hclose, rclose)
    ! enddo
    !
    ! -- Return
    return
  end subroutine gwt_cc

  !> @brief GWT Model calculate flow
  !!
  !! Call the intercell flows (flow ja) subroutine
  !<
  subroutine gwt_cq(this, icnvg, isuppress_output)
    ! -- modules
    use SparseModule, only: csr_diagsum
    ! -- dummy
    class(GwtModelType) :: this
    integer(I4B), intent(in) :: icnvg
    integer(I4B), intent(in) :: isuppress_output
    ! -- local
    integer(I4B) :: i
    integer(I4B) :: ip
    class(BndType), pointer :: packobj
    !
    ! -- Construct the flowja array.  Flowja is calculated each time, even if
    !    output is suppressed.  (flowja is positive into a cell.)  The diagonal
    !    position of the flowja array will contain the flow residual after
    !    these routines are called, so each package is responsible for adding
    !    its flow to this diagonal position.
    do i = 1, this%nja
      this%flowja(i) = DZERO
    end do
    if (this%inadv > 0) call this%adv%adv_cq(this%x, this%flowja)
    if (this%indsp > 0) call this%dsp%dsp_cq(this%x, this%flowja)
    if (this%inmst > 0) call this%mst%mst_cq(this%dis%nodes, this%x, this%xold, &
                                             this%flowja)
    if (this%inssm > 0) call this%ssm%ssm_cq(this%flowja)
    if (this%infmi > 0) call this%fmi%fmi_cq(this%x, this%flowja)
    !
    ! -- Go through packages and call cq routines.  cf() routines are called
    !    first to regenerate non-linear terms to be consistent with the final
    !    conc solution.
    do ip = 1, this%bndlist%Count()
      packobj => GetBndFromList(this%bndlist, ip)
      call packobj%bnd_cf(reset_mover=.false.)
      call packobj%bnd_cq(this%x, this%flowja)
    end do
    !
    ! -- Finalize calculation of flowja by adding face flows to the diagonal.
    !    This results in the flow residual being stored in the diagonal
    !    position for each cell.
    call csr_diagsum(this%dis%con%ia, this%flowja)
    !
    ! -- Return
    return
  end subroutine gwt_cq

  !> @brief GWT Model Budget
  !!
  !! This subroutine:
  !!  (1) calculates intercell flows (flowja)
  !!  (2) calculates package contributions to the model budget
  !<
  subroutine gwt_bd(this, icnvg, isuppress_output)
    use ConstantsModule, only: DZERO
    ! -- dummy
    class(GwtModelType) :: this
    integer(I4B), intent(in) :: icnvg
    integer(I4B), intent(in) :: isuppress_output
    ! -- local
    integer(I4B) :: ip
    class(BndType), pointer :: packobj
    !
    ! -- Save the solution convergence flag
    this%icnvg = icnvg
    !
    ! -- Budget routines (start by resetting).  Sole purpose of this section
    !    is to add in and outs to model budget.  All ins and out for a model
    !    should be added here to this%budget.  In a subsequent exchange call,
    !    exchange flows might also be added.
    call this%budget%reset()
    if (this%inmst > 0) call this%mst%mst_bd(isuppress_output, this%budget)
    if (this%inssm > 0) call this%ssm%ssm_bd(isuppress_output, this%budget)
    if (this%infmi > 0) call this%fmi%fmi_bd(isuppress_output, this%budget)
    if (this%inmvt > 0) call this%mvt%mvt_bd(this%x, this%x)
    do ip = 1, this%bndlist%Count()
      packobj => GetBndFromList(this%bndlist, ip)
      call packobj%bnd_bd(this%budget)
    end do
    !
    ! -- Return
    return
  end subroutine gwt_bd

  !> @brief Print and/or save model output
  !!
  !! Call the parent class output routine
  !<
  subroutine gwt_ot(this)
    ! -- modules
    use TdisModule, only: kstp, kper, tdis_ot, endofperiod
    ! -- dummy
    class(GwtModelType) :: this
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
    !
    ! -- Set write and print flags
    idvsave = 0
    idvprint = 0
    icbcfl = 0
    ibudfl = 0
    if (this%oc%oc_save('CONCENTRATION')) idvsave = 1
    if (this%oc%oc_print('CONCENTRATION')) idvprint = 1
    if (this%oc%oc_save('BUDGET')) icbcfl = 1
    if (this%oc%oc_print('BUDGET')) ibudfl = 1
    icbcun = this%oc%oc_save_unit('BUDGET')
    !
    ! -- Override ibudfl and idvprint flags for nonconvergence
    !    and end of period
    ibudfl = this%oc%set_print_flag('BUDGET', this%icnvg, endofperiod)
    idvprint = this%oc%set_print_flag('CONCENTRATION', this%icnvg, endofperiod)
    !
    !   Calculate and save observations
    call this%gwt_ot_obs()
    !
    !   Save and print flows
    call this%gwt_ot_flow(icbcfl, ibudfl, icbcun)
    !
    !   Save and print dependent variables
    call this%gwt_ot_dv(idvsave, idvprint, ipflag)
    !
    !   Print budget summaries
    call this%gwt_ot_bdsummary(ibudfl, ipflag)
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
  end subroutine gwt_ot

  !> @brief Calculate and save observations
  !<
  subroutine gwt_ot_obs(this)
    class(GwtModelType) :: this
    class(BndType), pointer :: packobj
    integer(I4B) :: ip

    ! -- Calculate and save observations
    call this%obs%obs_bd()
    call this%obs%obs_ot()

    ! -- Calculate and save package obserations
    do ip = 1, this%bndlist%Count()
      packobj => GetBndFromList(this%bndlist, ip)
      call packobj%bnd_bd_obs()
      call packobj%bnd_ot_obs()
    end do
    !
    ! -- Return
    return
  end subroutine gwt_ot_obs

  !> @brief Save flows
  !<
  subroutine gwt_ot_flow(this, icbcfl, ibudfl, icbcun)
    class(GwtModelType) :: this
    integer(I4B), intent(in) :: icbcfl
    integer(I4B), intent(in) :: ibudfl
    integer(I4B), intent(in) :: icbcun
    class(BndType), pointer :: packobj
    integer(I4B) :: ip

    ! -- Save GWT flows
    call this%gwt_ot_flowja(this%nja, this%flowja, icbcfl, icbcun)
    if (this%inmst > 0) call this%mst%mst_ot_flow(icbcfl, icbcun)
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
    !
    ! -- Print GWF flows
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
    !
    ! -- Print advanced package flows
    do ip = 1, this%bndlist%Count()
      packobj => GetBndFromList(this%bndlist, ip)
      call packobj%bnd_ot_package_flows(icbcfl=0, ibudfl=ibudfl)
    end do
    if (this%inmvt > 0) then
      call this%mvt%mvt_ot_printflow(icbcfl, ibudfl)
    end if
    !
    ! -- Return
    return
  end subroutine gwt_ot_flow

  !> @brief Write intercell flows
  !<
  subroutine gwt_ot_flowja(this, nja, flowja, icbcfl, icbcun)
    ! -- dummy
    class(GwtModelType) :: this
    integer(I4B), intent(in) :: nja
    real(DP), dimension(nja), intent(in) :: flowja
    integer(I4B), intent(in) :: icbcfl
    integer(I4B), intent(in) :: icbcun
    ! -- local
    integer(I4B) :: ibinun
    ! -- formats
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
  end subroutine gwt_ot_flowja

  !> @brief Print dependent variables
  !<
  subroutine gwt_ot_dv(this, idvsave, idvprint, ipflag)
    class(GwtModelType) :: this
    integer(I4B), intent(in) :: idvsave
    integer(I4B), intent(in) :: idvprint
    integer(I4B), intent(inout) :: ipflag
    class(BndType), pointer :: packobj
    integer(I4B) :: ip
    !
    ! -- Print advanced package dependent variables
    do ip = 1, this%bndlist%Count()
      packobj => GetBndFromList(this%bndlist, ip)
      call packobj%bnd_ot_dv(idvsave, idvprint)
    end do
    !
    ! -- save head and print head
    call this%oc%oc_ot(ipflag)
    !
    ! -- Return
    return
  end subroutine gwt_ot_dv

  !> @brief Print budget summary
  !<
  subroutine gwt_ot_bdsummary(this, ibudfl, ipflag)
    use TdisModule, only: kstp, kper, totim
    class(GwtModelType) :: this
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
    !
    ! -- mover budget summary
    if (this%inmvt > 0) then
      call this%mvt%mvt_ot_bdsummary(ibudfl)
    end if
    !
    ! -- model budget summary
    if (ibudfl /= 0) then
      ipflag = 1
      call this%budget%budget_ot(kstp, kper, this%iout)
    end if
    !
    ! -- Write to budget csv
    call this%budget%writecsv(totim)
    !
    ! -- Return
    return
  end subroutine gwt_ot_bdsummary

  !> @brief Deallocate
  !!
  !! Deallocate memmory at conclusion of model run
  !<
  subroutine gwt_da(this)
    ! -- modules
    use MemoryManagerModule, only: mem_deallocate
    use MemoryManagerExtModule, only: memorylist_remove
    use SimVariablesModule, only: idm_context
    ! -- dummy
    class(GwtModelType) :: this
    ! -- local
    integer(I4B) :: ip
    class(BndType), pointer :: packobj
    !
    ! -- Deallocate idm memory
    call memorylist_remove(this%name, 'NAM', idm_context)
    call memorylist_remove(component=this%name, context=idm_context)
    !
    ! -- Internal flow packages deallocate
    call this%dis%dis_da()
    call this%ic%ic_da()
    call this%fmi%fmi_da()
    call this%adv%adv_da()
    call this%dsp%dsp_da()
    call this%ssm%ssm_da()
    call this%mst%mst_da()
    call this%mvt%mvt_da()
    call this%budget%budget_da()
    call this%oc%oc_da()
    call this%obs%obs_da()
    !
    ! -- Internal package objects
    deallocate (this%dis)
    deallocate (this%ic)
    deallocate (this%dsp)
    deallocate (this%ssm)
    deallocate (this%mst)
    deallocate (this%adv)
    deallocate (this%mvt)
    deallocate (this%budget)
    deallocate (this%oc)
    deallocate (this%obs)
    !
    ! -- Boundary packages
    do ip = 1, this%bndlist%Count()
      packobj => GetBndFromList(this%bndlist, ip)
      call packobj%bnd_da()
      deallocate (packobj)
    end do
    !
    ! -- Scalars
    call mem_deallocate(this%indsp)
    call mem_deallocate(this%inssm)
    call mem_deallocate(this%inmst)
    call mem_deallocate(this%inmvt)
    call mem_deallocate(this%inoc)
    call mem_deallocate(this%inobs)
    !
    ! -- Parent class members
    call this%TransportModelType%tsp_da()
    !
    ! -- NumericalModelType
    call this%NumericalModelType%model_da()
    !
    ! -- Return
    return
  end subroutine gwt_da

  !> @brief GroundWater Transport Model Budget Entry
  !!
  !! This subroutine adds a budget entry to the flow budget.  It was added as
  !! a method for the gwt model object so that the exchange object could add its
  !! contributions.
  !<
  subroutine gwt_bdentry(this, budterm, budtxt, rowlabel)
    ! -- modules
    use ConstantsModule, only: LENBUDTXT
    use TdisModule, only: delt
    ! -- dummy
    class(GwtModelType) :: this
    real(DP), dimension(:, :), intent(in) :: budterm
    character(len=LENBUDTXT), dimension(:), intent(in) :: budtxt
    character(len=*), intent(in) :: rowlabel
    !
    call this%budget%addentry(budterm, delt, budtxt, rowlabel=rowlabel)
    !
    ! -- Return
    return
  end subroutine gwt_bdentry

  !> @brief return 1 if any package causes the matrix to be asymmetric.
  !! Otherwise return 0.
  !<
  function gwt_get_iasym(this) result(iasym)
    class(GwtModelType) :: this
    ! -- local
    integer(I4B) :: iasym
    integer(I4B) :: ip
    class(BndType), pointer :: packobj
    !
    ! -- Start by setting iasym to zero
    iasym = 0
    !
    ! -- ADV
    if (this%inadv > 0) then
      if (this%adv%iasym /= 0) iasym = 1
    end if
    !
    ! -- DSP
    if (this%indsp > 0) then
      if (this%dsp%ixt3d /= 0) iasym = 1
    end if
    !
    ! -- Check for any packages that introduce matrix asymmetry
    do ip = 1, this%bndlist%Count()
      packobj => GetBndFromList(this%bndlist, ip)
      if (packobj%iasym /= 0) iasym = 1
    end do
    !
    ! -- Return
    return
  end function gwt_get_iasym

  !> Allocate memory for non-allocatable members
  !!
  !! A subroutine for allocating the scalars specific to the GWT model type.
  !! Additional scalars used by the parent class are allocated by the parent
  !! class.
  !<
  subroutine allocate_scalars(this, modelname)
    ! -- modules
    use MemoryManagerModule, only: mem_allocate
    ! -- dummy
    class(GwtModelType) :: this
    character(len=*), intent(in) :: modelname
    !
    ! -- allocate parent class scalars
    call this%allocate_tsp_scalars(modelname)
    !
    ! -- allocate members that are part of model class
    call mem_allocate(this%inmvt, 'INMVT', this%memoryPath)
    call mem_allocate(this%inmst, 'INMST', this%memoryPath)
    call mem_allocate(this%indsp, 'INDSP', this%memoryPath)
    call mem_allocate(this%inssm, 'INSSM', this%memoryPath)
    call mem_allocate(this%inoc, 'INOC ', this%memoryPath)
    call mem_allocate(this%inobs, 'INOBS', this%memoryPath)
    !
    this%inmvt = 0
    this%inmst = 0
    this%indsp = 0
    this%inssm = 0
    this%inoc = 0
    this%inobs = 0
    !
    ! -- Return
    return
  end subroutine allocate_scalars

  !> @brief Create boundary condition packages for this model
  !!
  !! Call the package create routines for packages activated by the user.
  !<
  subroutine package_create(this, filtyp, ipakid, ipaknum, pakname, inunit, &
                            iout)
    ! -- modules
    use ConstantsModule, only: LINELENGTH
    use SimModule, only: store_error
    use GwtCncModule, only: cnc_create
    use GwtSrcModule, only: src_create
    use GwtIstModule, only: ist_create
    use GwtLktModule, only: lkt_create
    use GwtSftModule, only: sft_create
    use GwtMwtModule, only: mwt_create
    use GwtUztModule, only: uzt_create
    use ApiModule, only: api_create
    ! -- dummy
    class(GwtModelType) :: this
    character(len=*), intent(in) :: filtyp
    character(len=LINELENGTH) :: errmsg
    integer(I4B), intent(in) :: ipakid
    integer(I4B), intent(in) :: ipaknum
    character(len=*), intent(in) :: pakname
    integer(I4B), intent(in) :: inunit
    integer(I4B), intent(in) :: iout
    ! -- local
    class(BndType), pointer :: packobj
    class(BndType), pointer :: packobj2
    integer(I4B) :: ip
    !
    ! -- This part creates the package object
    select case (filtyp)
    case ('CNC6')
      call cnc_create(packobj, ipakid, ipaknum, inunit, iout, this%name, pakname)
    case ('SRC6')
      call src_create(packobj, ipakid, ipaknum, inunit, iout, this%name, pakname)
    case ('LKT6')
      call lkt_create(packobj, ipakid, ipaknum, inunit, iout, this%name, &
                      pakname, this%fmi)
    case ('SFT6')
      call sft_create(packobj, ipakid, ipaknum, inunit, iout, this%name, &
                      pakname, this%fmi)
    case ('MWT6')
      call mwt_create(packobj, ipakid, ipaknum, inunit, iout, this%name, &
                      pakname, this%fmi)
    case ('UZT6')
      call uzt_create(packobj, ipakid, ipaknum, inunit, iout, this%name, &
                      pakname, this%fmi)
    case ('IST6')
      call ist_create(packobj, ipakid, ipaknum, inunit, iout, this%name, &
                      pakname, this%fmi, this%mst)
    case ('API6')
      call api_create(packobj, ipakid, ipaknum, inunit, iout, this%name, pakname)
    case default
      write (errmsg, *) 'Invalid package type: ', filtyp
      call store_error(errmsg, terminate=.TRUE.)
    end select
    !
    ! -- Packages is the bndlist that is associated with the parent model
    ! -- The following statement puts a pointer to this package in the ipakid
    ! -- position of packages.
    do ip = 1, this%bndlist%Count()
      packobj2 => GetBndFromList(this%bndlist, ip)
      if (packobj2%packName == pakname) then
        write (errmsg, '(a,a)') 'Cannot create package.  Package name  '// &
          'already exists: ', trim(pakname)
        call store_error(errmsg, terminate=.TRUE.)
      end if
    end do
    call AddBndToList(this%bndlist, packobj)
    !
    ! -- Return
    return
  end subroutine package_create

  !> @brief Cast to GwtModelType
  !<
  function CastAsGwtModel(model) result(gwtmodel)
    class(*), pointer :: model !< The object to be cast
    class(GwtModelType), pointer :: gwtmodel !< The GWT model
    !
    gwtmodel => null()
    if (.not. associated(model)) return
    select type (model)
    type is (GwtModelType)
      gwtmodel => model
    end select
    !
    ! -- Return
    return
  end function CastAsGwtModel

  !> @brief Source package info and begin to process
  !<
  subroutine create_bndpkgs(this, bndpkgs, pkgtypes, pkgnames, &
                            mempaths, inunits)
    ! -- modules
    use ConstantsModule, only: LINELENGTH, LENPACKAGENAME
    use CharacterStringModule, only: CharacterStringType
    ! -- dummy
    class(GwtModelType) :: this
    integer(I4B), dimension(:), allocatable, intent(inout) :: bndpkgs
    type(CharacterStringType), dimension(:), contiguous, &
      pointer, intent(inout) :: pkgtypes
    type(CharacterStringType), dimension(:), contiguous, &
      pointer, intent(inout) :: pkgnames
    type(CharacterStringType), dimension(:), contiguous, &
      pointer, intent(inout) :: mempaths
    integer(I4B), dimension(:), contiguous, &
      pointer, intent(inout) :: inunits
    ! -- local
    integer(I4B) :: ipakid, ipaknum
    character(len=LENFTYPE) :: pkgtype, bndptype
    character(len=LENPACKAGENAME) :: pkgname
    character(len=LENMEMPATH) :: mempath
    integer(I4B), pointer :: inunit
    integer(I4B) :: n
    !
    if (allocated(bndpkgs)) then
      !
      ! -- create stress packages
      ipakid = 1
      bndptype = ''
      do n = 1, size(bndpkgs)
        !
        pkgtype = pkgtypes(bndpkgs(n))
        pkgname = pkgnames(bndpkgs(n))
        mempath = mempaths(bndpkgs(n))
        inunit => inunits(bndpkgs(n))
        !
        if (bndptype /= pkgtype) then
          ipaknum = 1
          bndptype = pkgtype
        end if
        !
        call this%package_create(pkgtype, ipakid, ipaknum, pkgname, inunit, &
                                 this%iout)
        ipakid = ipakid + 1
        ipaknum = ipaknum + 1
      end do
      !
      ! -- cleanup
      deallocate (bndpkgs)
    end if
    !
    ! -- Return
    return
  end subroutine create_bndpkgs

  !> @brief Source package info and begin to process
  !<
  subroutine create_gwt_packages(this, indis)
    ! -- modules
    use ConstantsModule, only: LINELENGTH, LENPACKAGENAME
    use CharacterStringModule, only: CharacterStringType
    use ArrayHandlersModule, only: expandarray
    use MemoryManagerModule, only: mem_setptr
    use MemoryHelperModule, only: create_mem_path
    use SimVariablesModule, only: idm_context
    use GwtMstModule, only: mst_cr
    use GwtDspModule, only: dsp_cr
    use GwtSsmModule, only: ssm_cr
    use GwtMvtModule, only: mvt_cr
    use GwtOcModule, only: oc_cr
    use GwtObsModule, only: gwt_obs_cr
    ! -- dummy
    class(GwtModelType) :: this
    integer(I4B), intent(in) :: indis
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
    integer(I4B), dimension(:), allocatable :: bndpkgs
    integer(I4B) :: n
    character(len=LENMEMPATH) :: mempathdsp = ''
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
      ! -- create dis package first as it is a prerequisite for other packages
      select case (pkgtype)
      case ('MVT6')
        this%inmvt = inunit
      case ('MST6')
        this%inmst = inunit
      case ('DSP6')
        this%indsp = 1
        mempathdsp = mempath
      case ('SSM6')
        this%inssm = inunit
      case ('OC6')
        this%inoc = inunit
      case ('OBS6')
        this%inobs = inunit
      case ('CNC6', 'SRC6', 'LKT6', 'SFT6', &
            'MWT6', 'UZT6', 'IST6', 'API6')
        call expandarray(bndpkgs)
        bndpkgs(size(bndpkgs)) = n
      case default
        ! TODO
      end select
    end do
    !
    ! -- Create packages that are tied directly to model
    call mst_cr(this%mst, this%name, this%inmst, this%iout, this%fmi)
    call dsp_cr(this%dsp, this%name, mempathdsp, this%indsp, this%iout, &
                this%fmi)
    call ssm_cr(this%ssm, this%name, this%inssm, this%iout, this%fmi)
    call mvt_cr(this%mvt, this%name, this%inmvt, this%iout, this%fmi)
    call oc_cr(this%oc, this%name, this%inoc, this%iout)
    call gwt_obs_cr(this%obs, this%inobs)
    !
    ! -- Check to make sure that required ftype's have been specified
    call this%ftype_check(indis, this%inmst)
    !
    call this%create_bndpkgs(bndpkgs, pkgtypes, pkgnames, mempaths, inunits)
    !
    ! -- Return
    return
  end subroutine create_gwt_packages

end module GwtModule
