! Groundwater Energy Transport (GWE) Model

module GweModule

  use KindModule, only: DP, I4B
  use InputOutputModule, only: ParseLine, upcase
  use ConstantsModule, only: LENFTYPE, LENMEMPATH, DZERO, LENPAKLOC
  use VersionModule, only: write_listfile_header
  use NumericalModelModule, only: NumericalModelType
  use BaseModelModule, only: BaseModelType
  use BndModule, only: BndType, AddBndToList, GetBndFromList
  use GweDspModule, only: GweDspType
  use GweMstModule, only: GweMstType
  use BudgetModule, only: BudgetType
  use GweInputDataModule, only: GweInputDataType
  use TransportModelModule
  use MatrixBaseModule

  implicit none

  private
  public :: gwe_cr
  public :: GweModelType
  public :: CastAsGweModel

  type, extends(TransportModelType) :: GweModelType

    type(GweInputDataType), pointer :: gwecommon => null() !< container for data shared with multiple packages
    type(GweMstType), pointer :: mst => null() !< mass storage and transfer package
    type(GweDspType), pointer :: dsp => null() !< dispersion package
    !type(BudgetType), pointer :: budget => null() ! budget object
    !integer(I4B), pointer :: inic => null() ! unit number IC
    !integer(I4B), pointer :: infmi => null() ! unit number FMI
    !integer(I4B), pointer :: inmvt => null() ! unit number MVT
    !integer(I4B), pointer :: inmst => null() ! unit number MST
    !integer(I4B), pointer :: inadv => null() ! unit number ADV
    !integer(I4B), pointer :: indsp => null() ! unit number DSP
    !integer(I4B), pointer :: inssm => null() ! unit number SSM
    !integer(I4B), pointer :: inoc => null() ! unit number OC
    !integer(I4B), pointer :: inobs => null() ! unit number OBS

  contains

    procedure :: model_df => gwe_df
    procedure :: model_ac => gwe_ac
    procedure :: model_mc => gwe_mc
    procedure :: model_ar => gwe_ar
    procedure :: model_rp => gwe_rp
    procedure :: model_ad => gwe_ad
    procedure :: model_cf => gwe_cf
    procedure :: model_fc => gwe_fc
    procedure :: model_cc => gwe_cc
    procedure :: model_cq => gwe_cq
    procedure :: model_bd => gwe_bd
    procedure :: model_ot => gwe_ot
    procedure :: model_da => gwe_da
    procedure :: model_bdentry => gwe_bdentry

    procedure :: allocate_scalars
    procedure, private :: package_create
    !procedure, private :: ftype_check
    procedure :: get_iasym => gwe_get_iasym
    procedure, private :: gwe_ot_flow
    procedure, private :: gwe_ot_flowja
    procedure, private :: gwe_ot_dv
    procedure, private :: gwe_ot_bdsummary
    procedure, private :: gwe_ot_obs
    procedure, private :: create_packages
    procedure, private :: create_bndpkgs
    procedure, private :: create_lstfile
    procedure, private :: log_namfile_options

  end type GweModelType

  ! -- Module variables constant for simulation
  !integer(I4B), parameter :: NIUNIT=100
  !character(len=LENFTYPE), dimension(NIUNIT) :: cunit
  !data cunit/   'DIS6 ', 'DISV6', 'DISU6', 'IC6  ', 'MST6 ', & !  5
  !              'ADV6 ', 'DSP6 ', 'SSM6 ', '     ', 'CNC6 ', & ! 10
  !              'OC6  ', 'OBS6 ', 'FMI6 ', 'SRC6 ', 'IST6 ', & ! 15
  !              'LKT6 ', 'SFT6 ', 'MWT6 ', 'UZT6 ', 'MVT6 ', & ! 20
  !              'API6 ', '     ', '     ', '     ', '     ', & ! 25
  !              75 * '     '/

contains

  subroutine gwe_cr(filename, id, modelname)
! ******************************************************************************
! gwe_cr -- Create a new groundwater energy transport model object
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use ListsModule, only: basemodellist
    use BaseModelModule, only: AddBaseModelToList
    use ConstantsModule, only: LINELENGTH, LENPACKAGENAME
    use MemoryManagerModule, only: mem_allocate
    use MemoryHelperModule, only: create_mem_path
    use SimVariablesModule, only: idm_context
    use GwfNamInputModule, only: GwfNamParamFoundType
    use BudgetModule, only: budget_cr
    use TspLabelsModule, only: tsplabels_cr
    !use SimModule, only: store_error, count_errors
    !use NameFileModule, only: NameFileType
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
    use GweMstModule, only: mst_cr
    use GweDspModule, only: dsp_cr
    use GweInputDataModule, only: gweshared_dat_cr
    ! -- dummy
    character(len=*), intent(in) :: filename
    integer(I4B), intent(in) :: id
    character(len=*), intent(in) :: modelname
    ! -- local
    integer(I4B) :: indis, indis6, indisu6, indisv6
    integer(I4B) :: ipakid, i, j, iu, ipaknum
    character(len=LINELENGTH) :: errmsg
    character(len=LENPACKAGENAME) :: pakname
    !type(NameFileType) :: namefile_obj
    type(GweModelType), pointer :: this
    class(BaseModelType), pointer :: model
    character(len=LENMEMPATH) :: input_mempath
    character(len=LINELENGTH) :: lst_fname
    type(GwfNamParamFoundType) :: found
    
    cunit(10) = 'TMP6 '
! ------------------------------------------------------------------------------
    !
    ! -- Allocate a new GWE Model (this) and add it to basemodellist
    allocate (this)
    !
    ! -- Set memory path before allocation in memory manager can be done
    this%memoryPath = create_mem_path(modelname)
    !
    call this%allocate_scalars(modelname)
    model => this
    call AddBaseModelToList(basemodellist, model)
    !
    ! -- Assign values
    this%filename = filename
    this%name = modelname
    this%macronym = 'GWE'
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
    ! -- create the list file
    call this%create_lstfile(lst_fname, filename, found%list)
    !
    ! -- activate save_flows if found
    if (found%save_flows) then
      this%ipakcb = -1
    end if
    !
    ! -- Instantiate generalized labels
    call tsplabels_cr(this%tsplab, this%name)
    !
    ! -- Instantiate shared data container
    call gweshared_dat_cr(this%gwecommon)
    !
    ! -- log set options
    if (this%iout > 0) then
      call this%log_namfile_options(found)
    end if
    !
    ! -- Create utility objects
    call budget_cr(this%budget, this%name, this%tsplab)
    !
    ! -- create model packages
    call this%create_packages()
    !
    ! -- return
    return
  end subroutine gwe_cr

  subroutine gwe_df(this)
! ******************************************************************************
! gwe_df -- Define packages of the model
! Subroutine: (1) call df routines for each package
!             (2) set variables and pointers
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use TspLabelsModule, only: setTspLabels
    use GweInputDataModule, only: gweshared_dat_df
    ! -- dummy
    class(GweModelType) :: this
    ! -- local
    integer(I4B) :: ip
    class(BndType), pointer :: packobj
! ------------------------------------------------------------------------------
    !
    ! -- Set labels to be used with transport model
    call this%tsplab%setTspLabels(this%macronym, 'TEMPERATURE', 'ENERGY', 'E')
    !
    ! -- Define packages and utility objects
    call this%dis%dis_df()
    call this%fmi%fmi_df(this%dis, this%inssm, 0)
    if (this%inmvt > 0) call this%mvt%mvt_df(this%dis)
    if (this%inadv > 0) call this%adv%adv_df()
    if (this%indsp > 0) call this%dsp%dsp_df(this%dis)
    if (this%inssm > 0) call this%ssm%ssm_df()
    call this%oc%oc_df()
    call this%budget%budget_df(niunit, this%tsplab%depvarunit, &
                               this%tsplab%depvarunitabbrev)
    !
    ! -- Assign or point model members to dis members
    this%neq = this%dis%nodes
    this%nja = this%dis%nja
    this%ia => this%dis%con%ia
    this%ja => this%dis%con%ja
    !
    ! -- Define shared data (cpw, rhow, latent heat of vaporization)
    call this%gwecommon%gweshared_dat_df(this%neq)
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
    call this%obs%obs_df(this%iout, this%name, 'GWE', this%dis)
    !
    ! -- return
    return
  end subroutine gwe_df

  subroutine gwe_ac(this, sparse)
! ******************************************************************************
! gwe_ac -- Add the internal connections of this model to the sparse matrix
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use SparseModule, only: sparsematrix
    ! -- dummy
    class(GweModelType) :: this
    type(sparsematrix), intent(inout) :: sparse
    ! -- local
    class(BndType), pointer :: packobj
    integer(I4B) :: ip
! ------------------------------------------------------------------------------
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
    ! -- return
    return
  end subroutine gwe_ac

  subroutine gwe_mc(this, matrix_sln)
! ******************************************************************************
! gwe_mc -- Map the positions of this models connections in the
! numerical solution coefficient matrix.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(GweModelType) :: this
    class(MatrixBaseType), pointer :: matrix_sln !< global system matrix
    ! -- local
    class(BndType), pointer :: packobj
    integer(I4B) :: ip
! ------------------------------------------------------------------------------
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
    ! -- return
    return
  end subroutine gwe_mc

  subroutine gwe_ar(this)
! ******************************************************************************
! gwe_ar -- GroundWater Energy Transport Model Allocate and Read
! Subroutine: (1) allocates and reads packages part of this model,
!             (2) allocates memory for arrays part of this model object
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use ConstantsModule, only: DHNOFLO
    ! -- dummy
    class(GweModelType) :: this
    ! -- locals
    integer(I4B) :: ip
    class(BndType), pointer :: packobj
! ------------------------------------------------------------------------------
    !
    ! -- Allocate and read modules attached to model
    call this%fmi%fmi_ar(this%ibound)
    if (this%inmvt > 0) call this%mvt%mvt_ar()
    if (this%inic > 0) call this%ic%ic_ar(this%x)
    if (this%inmst > 0) call this%mst%mst_ar(this%dis, this%ibound)
    if (this%inadv > 0) call this%adv%adv_ar(this%dis, this%ibound)
    if (this%indsp > 0) call this%dsp%dsp_ar(this%ibound, this%mst%porosity)
    if (this%inssm > 0) call this%ssm%ssm_ar(this%dis, this%ibound, this%x)
    if (this%inobs > 0) call this%obs%tsp_obs_ar(this%ic, this%x, this%flowja)
    !
    ! -- Set governing equation scale factor
    this%eqnsclfac = this%gwecommon%gwerhow * this%gwecommon%gwecpw
    !
    ! -- Call dis_ar to write binary grid file
    !call this%dis%dis_ar(this%npf%icelltype)
    !
    ! -- set up output control
    call this%oc%oc_ar(this%x, this%dis, DHNOFLO, this%tsplab%depvartype)
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
    ! -- return
    return
  end subroutine gwe_ar

  subroutine gwe_rp(this)
! ******************************************************************************
! gwe_rp -- GroundWater Energy Transport Model Read and Prepare
! Subroutine: (1) calls package read and prepare routines
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use TdisModule, only: readnewdata
    ! -- dummy
    class(GweModelType) :: this
    ! -- local
    class(BndType), pointer :: packobj
    integer(I4B) :: ip
! ------------------------------------------------------------------------------
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
  end subroutine gwe_rp

  subroutine gwe_ad(this)
! ******************************************************************************
! gwe_ad -- GroundWater Energy Transport Model Time Step Advance
! Subroutine: (1) calls package advance subroutines
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use SimVariablesModule, only: isimcheck, iFailedStepRetry
    ! -- dummy
    class(GweModelType) :: this
    class(BndType), pointer :: packobj
    ! -- local
    integer(I4B) :: irestore
    integer(I4B) :: ip, n
! ------------------------------------------------------------------------------
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
    ! -- return
    return
  end subroutine gwe_ad

  subroutine gwe_cf(this, kiter)
! ******************************************************************************
! gwe_cf -- GroundWater Energy Transport Model calculate coefficients
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(GweModelType) :: this
    integer(I4B), intent(in) :: kiter
    ! -- local
    class(BndType), pointer :: packobj
    integer(I4B) :: ip
! ------------------------------------------------------------------------------
    !
    ! -- Call package cf routines
    do ip = 1, this%bndlist%Count()
      packobj => GetBndFromList(this%bndlist, ip)
      call packobj%bnd_cf()
    end do
    !
    ! -- return
    return
  end subroutine gwe_cf

  subroutine gwe_fc(this, kiter, matrix_sln, inwtflag)
! ******************************************************************************
! gwe_fc -- GroundWater Energy Transport Model fill coefficients
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(GweModelType) :: this
    integer(I4B), intent(in) :: kiter
    class(MatrixBaseType), pointer :: matrix_sln
    integer(I4B), intent(in) :: inwtflag
    ! -- local
    class(BndType), pointer :: packobj
    integer(I4B) :: ip
! ------------------------------------------------------------------------------
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
    ! -- return
    return
  end subroutine gwe_fc

  subroutine gwe_cc(this, innertot, kiter, iend, icnvgmod, cpak, ipak, dpak)
! ******************************************************************************
! gwe_cc -- GroundWater Energy Transport Model Final Convergence Check
! Subroutine: (1) calls package cc routines
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(GweModelType) :: this
    integer(I4B), intent(in) :: innertot
    integer(I4B), intent(in) :: kiter
    integer(I4B), intent(in) :: iend
    integer(I4B), intent(in) :: icnvgmod
    character(len=LENPAKLOC), intent(inout) :: cpak
    integer(I4B), intent(inout) :: ipak
    real(DP), intent(inout) :: dpak
    ! -- local
    !class(BndType), pointer :: packobj
    !integer(I4B) :: ip
    ! -- formats
! ------------------------------------------------------------------------------
    !
    ! -- If mover is on, then at least 2 outers required
    if (this%inmvt > 0) call this%mvt%mvt_cc(kiter, iend, icnvgmod, cpak, dpak)
    !
    ! -- Call package cc routines
    !do ip = 1, this%bndlist%Count()
    !  packobj => GetBndFromList(this%bndlist, ip)
    !  call packobj%bnd_cc(iend, icnvg, hclose, rclose)
    !enddo
    !
    ! -- return
    return
  end subroutine gwe_cc

  subroutine gwe_cq(this, icnvg, isuppress_output)
! ******************************************************************************
! gwe_cq --Groundwater energy transport model calculate flow
! Subroutine: (1) Calculate intercell flows (flowja)
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use SparseModule, only: csr_diagsum
    ! -- dummy
    class(GweModelType) :: this
    integer(I4B), intent(in) :: icnvg
    integer(I4B), intent(in) :: isuppress_output
    ! -- local
    integer(I4B) :: i
    integer(I4B) :: ip
    class(BndType), pointer :: packobj
! ------------------------------------------------------------------------------
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
  end subroutine gwe_cq

  subroutine gwe_bd(this, icnvg, isuppress_output)
! ******************************************************************************
! gwe_bd --GroundWater Energy Transport Model Budget
! Subroutine: (1) Calculate intercell flows (flowja)
!             (2) Calculate package contributions to model budget
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use ConstantsModule, only: DZERO
    ! -- dummy
    class(GweModelType) :: this
    integer(I4B), intent(in) :: icnvg
    integer(I4B), intent(in) :: isuppress_output
    ! -- local
    integer(I4B) :: ip
    class(BndType), pointer :: packobj
! ------------------------------------------------------------------------------
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
  end subroutine gwe_bd

  subroutine gwe_ot(this)
! ******************************************************************************
! gwe_ot -- GroundWater Energy Transport Model Output
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use TdisModule, only: kstp, kper, tdis_ot, endofperiod
    ! -- dummy
    class(GweModelType) :: this
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
    if (this%oc%oc_save(trim(this%tsplab%depvartype))) idvsave = 1
    if (this%oc%oc_print(trim(this%tsplab%depvartype))) idvprint = 1
    if (this%oc%oc_save('BUDGET')) icbcfl = 1
    if (this%oc%oc_print('BUDGET')) ibudfl = 1
    icbcun = this%oc%oc_save_unit('BUDGET')
    !
    ! -- Override ibudfl and idvprint flags for nonconvergence
    !    and end of period
    ibudfl = this%oc%set_print_flag('BUDGET', this%icnvg, endofperiod)
    idvprint = this%oc%set_print_flag(trim(this%tsplab%depvartype), &
                                      this%icnvg, endofperiod)
    !
    !   Calculate and save observations
    call this%gwe_ot_obs()
    !
    !   Save and print flows
    call this%gwe_ot_flow(icbcfl, ibudfl, icbcun)
    !
    !   Save and print dependent variables
    call this%gwe_ot_dv(idvsave, idvprint, ipflag)
    !
    !   Print budget summaries
    call this%gwe_ot_bdsummary(ibudfl, ipflag)
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
  end subroutine gwe_ot

  subroutine gwe_ot_obs(this)
    class(GweModelType) :: this
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

  end subroutine gwe_ot_obs

  subroutine gwe_ot_flow(this, icbcfl, ibudfl, icbcun)
    class(GweModelType) :: this
    integer(I4B), intent(in) :: icbcfl
    integer(I4B), intent(in) :: ibudfl
    integer(I4B), intent(in) :: icbcun
    class(BndType), pointer :: packobj
    integer(I4B) :: ip

    ! -- Save GWE flows
    call this%gwe_ot_flowja(this%nja, this%flowja, icbcfl, icbcun)
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

    ! -- Print advanced package flows
    do ip = 1, this%bndlist%Count()
      packobj => GetBndFromList(this%bndlist, ip)
      call packobj%bnd_ot_package_flows(icbcfl=0, ibudfl=ibudfl)
    end do
    if (this%inmvt > 0) then
      call this%mvt%mvt_ot_printflow(icbcfl, ibudfl)
    end if

  end subroutine gwe_ot_flow

  subroutine gwe_ot_flowja(this, nja, flowja, icbcfl, icbcun)
! ******************************************************************************
! gwe_ot_flowja -- Write intercell flows
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(GweModelType) :: this
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
  end subroutine gwe_ot_flowja

  subroutine gwe_ot_dv(this, idvsave, idvprint, ipflag)
    class(GweModelType) :: this
    integer(I4B), intent(in) :: idvsave
    integer(I4B), intent(in) :: idvprint
    integer(I4B), intent(inout) :: ipflag
    class(BndType), pointer :: packobj
    integer(I4B) :: ip

    ! -- Print advanced package dependent variables
    do ip = 1, this%bndlist%Count()
      packobj => GetBndFromList(this%bndlist, ip)
      call packobj%bnd_ot_dv(idvsave, idvprint)
    end do

    ! -- save head and print head
    call this%oc%oc_ot(ipflag)

  end subroutine gwe_ot_dv

  subroutine gwe_ot_bdsummary(this, ibudfl, ipflag)
    use TdisModule, only: kstp, kper, totim
    class(GweModelType) :: this
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

  end subroutine gwe_ot_bdsummary

  subroutine gwe_da(this)
! ******************************************************************************
! gwe_da -- Deallocate
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryManagerModule, only: mem_deallocate
    use MemoryManagerExtModule, only: memorylist_remove
    use SimVariablesModule, only: idm_context
    ! -- dummy
    class(GweModelType) :: this
    ! -- local
    integer(I4B) :: ip
    class(BndType), pointer :: packobj
! ------------------------------------------------------------------------------
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
    call this%tsplab%tsplabels_da()
    call this%gwecommon%gweshared_dat_da()
    !
    ! -- Internal package objects
    deallocate (this%dis)
    deallocate (this%ic)
    deallocate (this%fmi)
    deallocate (this%adv)
    deallocate (this%dsp)
    deallocate (this%ssm)
    deallocate (this%mst)
    deallocate (this%mvt)
    deallocate (this%budget)
    deallocate (this%oc)
    deallocate (this%obs)
    deallocate (this%tsplab)
    nullify (this%gwecommon)
    !
    ! -- Boundary packages
    do ip = 1, this%bndlist%Count()
      packobj => GetBndFromList(this%bndlist, ip)
      call packobj%bnd_da()
      deallocate (packobj)
    end do
    !
    ! -- Scalars
    call this%TransportModelType%tsp_da()
    !
    ! -- NumericalModelType
    call this%NumericalModelType%model_da()
    !
    ! -- return
    return
  end subroutine gwe_da

  !> @brief GroundWater Energy Transport Model Budget Entry
  !!
  !! This subroutine adds a budget entry to the flow budget.  It was added as
  !! a method for the gwe model object so that the exchange object could add its
  !! contributions.
  !!
  !! (1) adds the entry to the budget object
  !<
  subroutine gwe_bdentry(this, budterm, budtxt, rowlabel)
    ! -- modules
    use ConstantsModule, only: LENBUDTXT
    use TdisModule, only: delt
    ! -- dummy
    class(GweModelType) :: this
    real(DP), dimension(:, :), intent(in) :: budterm
    character(len=LENBUDTXT), dimension(:), intent(in) :: budtxt
    character(len=*), intent(in) :: rowlabel
! ------------------------------------------------------------------------------
    !
    call this%budget%addentry(budterm, delt, budtxt, rowlabel=rowlabel)
    !
    ! -- return
    return
  end subroutine gwe_bdentry

  !> @brief return 1 if any package causes the matrix to be asymmetric.
  !! Otherwise return 0.
  !<
  function gwe_get_iasym(this) result(iasym)
    class(GweModelType) :: this
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
    ! -- return
    return
  end function gwe_get_iasym

  subroutine allocate_scalars(this, modelname)
! ******************************************************************************
! allocate_scalars -- Allocate memory for non-allocatable members
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryManagerModule, only: mem_allocate
    ! -- dummy
    class(GweModelType) :: this
    character(len=*), intent(in) :: modelname
! ------------------------------------------------------------------------------
    !
    ! -- allocate members from parent class
    call this%NumericalModelType%allocate_scalars(modelname)
    !
    ! -- allocate members that are part of model class
    !call mem_allocate(this%inic, 'INIC', this%memoryPath)
    !call mem_allocate(this%infmi, 'INFMI', this%memoryPath)
    !call mem_allocate(this%inmvt, 'INMVT', this%memoryPath)
    call mem_allocate(this%inmst, 'INMST', this%memoryPath)
    !call mem_allocate(this%inadv, 'INADV', this%memoryPath)
    call mem_allocate(this%indsp, 'INDSP', this%memoryPath)
    !call mem_allocate(this%inssm, 'INSSM', this%memoryPath)
    !call mem_allocate(this%inoc, 'INOC ', this%memoryPath)
    !call mem_allocate(this%inobs, 'INOBS', this%memoryPath)
    !
    !this%inic = 0
    !this%infmi = 0
    !this%inmvt = 0
    this%inmst = 0
    !this%inadv = 0
    this%indsp = 0
    !this%inssm = 0
    !this%inoc = 0
    !this%inobs = 0
    !
    ! -- return
    return
  end subroutine allocate_scalars

  subroutine package_create(this, filtyp, ipakid, ipaknum, pakname, inunit, &
                            iout)
! ******************************************************************************
! package_create -- Create boundary condition packages for this model
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use ConstantsModule, only: LINELENGTH
    use SimModule, only: store_error
    use TspCncModule, only: cnc_create
    use GweSrcModule, only: src_create
!    use GweLktModule, only: lkt_create
    use GweSfeModule, only: sfe_create
!    use GweMwtModule, only: mwt_create
    use GweUzeModule, only: uze_create
!    use ApiModule, only: api_create
    ! -- dummy
    class(GweModelType) :: this
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
! ------------------------------------------------------------------------------
    !
    ! -- This part creates the package object
    select case (filtyp)
    case ('TMP6')
      call cnc_create(packobj, ipakid, ipaknum, inunit, iout, this%name, &
                      pakname, this%tsplab, this%eqnsclfac, this%gwecommon)
    case ('SRC6')
      call src_create(packobj, ipakid, ipaknum, inunit, iout, this%name, &
                      pakname, this%tsplab, this%gwecommon)
      !case('LKE6')
      !  call lkt_create(packobj, ipakid, ipaknum, inunit, iout, this%name, &
      !                  pakname, this%fmi)
    case ('SFE6')
      call sfe_create(packobj, ipakid, ipaknum, inunit, iout, this%name, &
                        pakname, this%fmi, this%tsplab, this%eqnsclfac, &
                        this%gwecommon)
      !case('MWT6')
      !  call mwt_create(packobj, ipakid, ipaknum, inunit, iout, this%name, &
      !                  pakname, this%fmi)
    case ('UZE6')
      call uze_create(packobj, ipakid, ipaknum, inunit, iout, this%name, &
                        pakname, this%fmi, this%tsplab, this%eqnsclfac, &
                        this%gwecommon)
      !case('IST6')
      !  call ist_create(packobj, ipakid, ipaknum, inunit, iout, this%name, &
      !                  pakname, this%fmi, this%mst)
      !case('API6')
      !  call api_create(packobj, ipakid, ipaknum, inunit, iout, this%name, &
      !                  pakname)
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
    ! -- return
    return
  end subroutine package_create

!  subroutine ftype_check(this, namefile_obj, indis)
! ******************************************************************************
! ftype_check -- Check to make sure required input files have been specified
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
!    ! -- modules
!    use ConstantsModule, only: LINELENGTH
!    use SimModule, only: store_error, count_errors
!    !use NameFileModule, only: NameFileType
!    ! -- dummy
!    class(GweModelType) :: this
!    type(NameFileType), intent(in) :: namefile_obj
!    integer(I4B), intent(in) :: indis
!    ! -- local
!    character(len=LINELENGTH) :: errmsg
!    integer(I4B) :: i, iu
!    character(len=LENFTYPE), dimension(10) :: nodupftype = &
!        &(/'DIS6 ', 'DISU6', 'DISV6', 'IC6  ', 'MST6 ', &
!        &'ADV6 ', 'DSP6 ', 'SSM6 ', 'OC6  ', 'OBS6 '/)
! ------------------------------------------------------------------------------
!    !
!    ! -- Check for IC6, DIS(u), and MST. Stop if not present.
!    if (this%inic == 0) then
!      write (errmsg, '(1x,a)') 'ERROR. INITIAL CONDITIONS (IC6) PACKAGE NOT '// &
!        'SPECIFIED.'
!      call store_error(errmsg)
!    end if
!    if (indis == 0) then
!      write (errmsg, '(1x,a)') &
!        'ERROR. DISCRETIZATION (DIS6 or DISU6) PACKAGE NOT SPECIFIED.'
!      call store_error(errmsg)
!    end if
!    if (this%inmst == 0) then
!      write (errmsg, '(1x,a)') 'ERROR. MASS STORAGE AND TRANSFER (MST6) &
!        &PACKAGE NOT SPECIFIED.'
!      call store_error(errmsg)
!    end if
!    if (count_errors() > 0) then
!      write (errmsg, '(1x,a)') 'ERROR. REQUIRED PACKAGE(S) NOT SPECIFIED.'
!      call store_error(errmsg)
!    end if
!    !
!    ! -- Check to make sure that some GWE packages are not specified more
!    !    than once
!    do i = 1, size(nodupftype)
!      call namefile_obj%get_unitnumber(trim(nodupftype(i)), iu, 0)
!      if (iu > 0) then
!        write (errmsg, '(1x, a, a, a)') &
!          'DUPLICATE ENTRIES FOR FTYPE ', trim(nodupftype(i)), &
!          ' NOT ALLOWED FOR GWE MODEL.'
!        call store_error(errmsg)
!      end if
!    end do
!    !
!    ! -- Stop if errors
!    if (count_errors() > 0) then
!      write (errmsg, '(a, a)') 'ERROR OCCURRED WHILE READING FILE: ', &
!        trim(namefile_obj%filename)
!      call store_error(errmsg, terminate=.TRUE.)
!    end if
!    !
!    ! -- return
!    return
!  end subroutine ftype_check

  !> @brief Cast to GweModelType
  function CastAsGweModel(model) result(gwemodel)
    class(*), pointer :: model !< The object to be cast
    class(GweModelType), pointer :: gwemodel !< The GWE model

    gwemodel => null()
    if (.not. associated(model)) return
    select type (model)
    type is (GweModelType)
      gwemodel => model
    end select

  end function CastAsGweModel
  
  !> @brief Source package info and begin to process
  !<
  subroutine create_bndpkgs(this, bndpkgs, pkgtypes, pkgnames, &
                            mempaths, inunits)
    ! -- modules
    use ConstantsModule, only: LINELENGTH, LENPACKAGENAME
    use CharacterStringModule, only: CharacterStringType
    ! -- dummy
    class(GweModelType) :: this
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
    ! -- return
    return
  end subroutine create_bndpkgs

  !> @brief Source package info and begin to process
  !<
  subroutine create_packages(this)
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
    use GweMstModule, only: mst_cr
    use TspAdvModule, only: adv_cr
    use GweDspModule, only: dsp_cr
    use TspSsmModule, only: ssm_cr
    use TspMvtModule, only: mvt_cr
    use TspOcModule, only: oc_cr
    use TspObsModule, only: tsp_obs_cr
    ! -- dummy
    class(GweModelType) :: this
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
    integer(I4B) :: indis = 0 ! DIS enabled flag
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
      case ('MST6')
        this%inmst = inunit
      case ('ADV6')
        this%inadv = inunit
    case ('DSP6')
        this%indsp = 1
        mempathdsp = mempath
      case ('SSM6')
        this%inssm = inunit
      case ('OC6')
        this%inoc = inunit
      case ('OBS6')
        this%inobs = inunit
      case ('TMP6', 'SRC6', 'LKE6', 'SFE6', &
            'MWE6', 'UZE6', '    ', 'API6')
        call expandarray(bndpkgs)
        bndpkgs(size(bndpkgs)) = n
      case default
        ! TODO
    end select
    end do
    !
    ! -- Create packages that are tied directly to model
    call ic_cr(this%ic, this%name, this%inic, this%iout, this%dis, this%tsplab)
    call fmi_cr(this%fmi, this%name, this%infmi, this%iout, this%tsplab)
    call mst_cr(this%mst, this%name, this%inmst, this%iout, this%fmi, &
                this%eqnsclfac, this%gwecommon)
    call adv_cr(this%adv, this%name, this%inadv, this%iout, this%fmi, &
                this%eqnsclfac)
    call dsp_cr(this%dsp, this%name, mempathdsp, this%indsp, this%iout, &
                this%fmi, this%eqnsclfac, this%gwecommon)
    call ssm_cr(this%ssm, this%name, this%inssm, this%iout, this%fmi, &
                this%tsplab, this%eqnsclfac, this%gwecommon)
    call mvt_cr(this%mvt, this%name, this%inmvt, this%iout, this%fmi)
    call oc_cr(this%oc, this%name, this%inoc, this%iout)
    call tsp_obs_cr(this%obs, this%inobs)
    !
    ! -- Check to make sure that required ftype's have been specified
    call this%ftype_check(indis)
    !
    call this%create_bndpkgs(bndpkgs, pkgtypes, pkgnames, mempaths, inunits)

  end subroutine create_packages

  subroutine create_lstfile(this, lst_fname, model_fname, defined)
    ! -- modules
    use KindModule, only: LGP
    use InputOutputModule, only: openfile, getunit
    ! -- dummy
    class(GweModelType) :: this
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
    call write_listfile_header(this%iout, 'GROUNDWATER ENERGY TRANSPORT MODEL (GWE)')
    !
    ! -- return
    return
  end subroutine create_lstfile

  !> @brief Write model namfile options to list file
  !<
  subroutine log_namfile_options(this, found)
    use GwfNamInputModule, only: GwfNamParamFoundType
    class(GweModelType) :: this
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

end module GweModule
