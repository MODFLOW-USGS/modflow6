! Generalized Transport Base Class
! Base class for solute (mass) and energy (thermal) transport
!   (The following copied from gwt1.f90)
!   * Add check that discretization is the same between both models
!   * Program GWT-GWT exchange transport (awaiting implementation of interface model)
!   * Consider implementation of steady-state transport (affects MST, IST)
!   * Check and handle pore space discrepancy between flow and transport (porosity vs specific yield)
!   * UZT may not have the required porosity term

module TransportModelModule
  use KindModule, only: DP, I4B
  use InputOutputModule, only: ParseLine
  use VersionModule, only: write_listfile_header
  use ConstantsModule, only: LENFTYPE, DZERO, LENPAKLOC, LENMEMPATH
  use SimVariablesModule, only: errmsg
  use NumericalModelModule, only: NumericalModelType
  use NumericalPackageModule, only: NumericalPackageType
  use TspLabelsModule, only: TspLabelsType
  use BndModule, only: BndType, GetBndFromList
  use GwtMstModule, only: GwtMstType
  use GweMstModule, only: GweMstType
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
    type(TspAdvType), pointer :: adv => null() ! advection package
    type(TspFmiType), pointer :: fmi => null() ! flow model interface
    type(TspIcType), pointer :: ic => null() ! initial conditions package
    type(TspMvtType), pointer :: mvt => null() ! mover transport package
    type(TspObsType), pointer :: obs => null() ! observation package
    type(TspOcType), pointer :: oc => null() ! output control package
    type(TspSsmType), pointer :: ssm => null() ! source sink mixing package
    type(TspLabelsType), pointer :: tsplab => null() ! object defining the appropriate labels
    type(BudgetType), pointer :: budget => null() ! budget object
    integer(I4B), pointer :: inic => null() ! unit number IC
    integer(I4B), pointer :: infmi => null() ! unit number FMI
    integer(I4B), pointer :: inmvt => null() ! unit number MVT
    integer(I4B), pointer :: inadv => null() ! unit number ADV
    integer(I4B), pointer :: inssm => null() ! unit number SSM
    integer(I4B), pointer :: inoc => null() ! unit number OC
    integer(I4B), pointer :: inobs => null() ! unit number OBS
    integer(I4B), pointer :: inmst => null() ! unit number MST
    integer(I4B), pointer :: indsp => null() ! unit number DSP
    real(DP), pointer :: eqnsclfac => null() !< constant factor by which all terms in the model's governing equation are scaled (divided) for formulation and solution

  contains
  
    ! -- public
    procedure :: allocate_tsp_scalars
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
    procedure, private :: tsp_ot_obs
    procedure, private :: tsp_ot_flow
    procedure, private :: tsp_ot_flowja
    procedure, private :: tsp_ot_dv
    procedure, private :: tsp_ot_bdsummary
    procedure, private :: create_lstfile
    procedure, private :: create_packages
    procedure, private :: log_namfile_options

  end type TransportModelType

  ! -- Module variables constant for simulation
  integer(I4B), parameter :: NIUNIT = 100
  character(len=LENFTYPE), dimension(NIUNIT) :: cunit
  data cunit/'DIS6 ', 'DISV6', 'DISU6', 'IC6  ', 'MST6 ', & !  5
    'ADV6 ', 'DSP6 ', 'SSM6 ', '     ', 'CNC6 ', & ! 10
    'OC6  ', 'OBS6 ', 'FMI6 ', 'SRC6 ', 'IST6 ', & ! 15
    'LKT6 ', 'SFT6 ', 'MWT6 ', 'UZT6 ', 'MVT6 ', & ! 20
    'API6 ', '     ', 'SFE6 ', 'UZE6 ', '     ', & ! 25
    75*'     '/

    contains

  subroutine tsp_cr(this, filename, id, modelname, macronym, indis)  ! kluge note: not used/needed
    ! -- modules
    use SimModule, only: store_error
    use MemoryManagerModule, only: mem_allocate
    use MemoryHelperModule, only: create_mem_path
    use MemoryManagerExtModule, only: mem_set_value
    use SimVariablesModule, only: idm_context
    use GwfNamInputModule, only: GwfNamParamFoundType
    use TspLabelsModule, only: tsplabels_cr
    use GwfDisModule, only: dis_cr
    use GwfDisvModule, only: disv_cr
    use GwfDisuModule, only: disu_cr
    use TspAdvModule, only: adv_cr
    use TspFmiModule, only: fmi_cr
    use TspIcModule, only: ic_cr
    use TspMvtModule, only: mvt_cr
    use TspObsModule, only: tsp_obs_cr
    use TspOcModule, only: oc_cr
    use TspSsmModule, only: ssm_cr
    use BudgetModule, only: budget_cr
    use ConstantsModule, only: LINELENGTH
    !use NameFileModule, only: NameFileType
    use InputOutputModule, only: upcase
    ! -- dummy
    class(TransportModelType) :: this
    character(len=*), intent(in) :: filename
    integer(I4B), intent(in) :: id
    integer(I4B), intent(inout) :: indis
    character(len=*), intent(in) :: modelname
    character(len=*), intent(in) :: macronym
    ! -- local
    class(*), pointer :: mstobjPtr
    !type(NameFileType) :: namefile_obj
    !integer(I4B) :: indis, indis6, indisu6, indisv6
    character(len=LINELENGTH) :: errmsg
    character(len=LENMEMPATH) :: input_mempath
    integer(I4B) :: nwords
    integer(I4B) :: i
    character(len=LINELENGTH), allocatable, dimension(:) :: words
    character(len=LINELENGTH) :: lst_fname
    type(GwfNamParamFoundType) :: found
! ------------------------------------------------------------------------------
    !
    ! -- Set memory path before allocation in memory manager can be done
    !this%memoryPath = create_mem_path(modelname)
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
    ! -- Instantiate generalized labels 
    call tsplabels_cr(this%tsplab, this%name)
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
    call this%create_packages(indis)
    !
    ! -- Open namefile and set iout
    !call namefile_obj%init(this%filename, 0)
    !call namefile_obj%add_cunit(niunit, cunit)
    !call namefile_obj%openlistfile(this%iout)
    !
    ! -- Write header to model list file
    !call write_listfile_header(this%iout, 'GROUNDWATER TRANSPORT MODEL (GWT)')
    !
    ! -- Open files
    !call namefile_obj%openfiles(this%iout)
    !
    ! --
    !if (size(namefile_obj%opts) > 0) then
    !  write (this%iout, '(1x,a)') 'NAMEFILE OPTIONS:'
    !end if
    !
    ! -- parse options in the gwt name file
    !do i = 1, size(namefile_obj%opts)
    !  call ParseLine(namefile_obj%opts(i), nwords, words)
    !  call upcase(words(1))
    !  select case (words(1))
    !  case ('PRINT_INPUT')
    !    this%iprpak = 1
    !    write (this%iout, '(4x,a)') 'STRESS PACKAGE INPUT WILL BE PRINTED '// &
    !      'FOR ALL MODEL STRESS PACKAGES'
    !  case ('PRINT_FLOWS')
    !    this%iprflow = 1
    !    write (this%iout, '(4x,a)') 'PACKAGE FLOWS WILL BE PRINTED '// &
    !      'FOR ALL MODEL PACKAGES'
    !  case ('SAVE_FLOWS')
    !    this%ipakcb = -1
    !    write (this%iout, '(4x,a)') &
    !      'FLOWS WILL BE SAVED TO BUDGET FILE SPECIFIED IN OUTPUT CONTROL'
    !  case default
    !    write (errmsg, '(4x,a,a,a,a)') &
    !      'UNKNOWN GWT NAMEFILE (', &
    !      trim(adjustl(this%filename)), ') OPTION: ', &
    !      trim(adjustl(namefile_obj%opts(i)))
    !    call store_error(errmsg, terminate=.TRUE.)
    !  end select
    !end do
    !
    ! -- Assign unit numbers to attached modules, and remove
    ! -- from unitnumber (by specifying 1 for iremove)
    !
    !indis = 0
    !indis6 = 0
    !indisu6 = 0
    !indisv6 = 0
    !call namefile_obj%get_unitnumber('DIS6', indis6, 1)
    !if (indis6 > 0) indis = indis6
    !if (indis <= 0) call namefile_obj%get_unitnumber('DISU6', indisu6, 1)
    !if (indisu6 > 0) indis = indisu6
    !if (indis <= 0) call namefile_obj%get_unitnumber('DISV6', indisv6, 1)
    !if (indisv6 > 0) indis = indisv6
    !call namefile_obj%get_unitnumber('ADV6', this%inadv, 1)
    !call namefile_obj%get_unitnumber('FMI6', this%infmi, 1)
    !call namefile_obj%get_unitnumber('IC6', this%inic, 1)
    !call namefile_obj%get_unitnumber('MVT6', this%inmvt, 1)
    !call namefile_obj%get_unitnumber('OBS6', this%inobs, 1)
    !call namefile_obj%get_unitnumber('OC6', this%inoc, 1)
    !call namefile_obj%get_unitnumber('SSM6', this%inssm, 1)
    !
    ! -- Check to make sure that required ftype's have been specified
    !call this%ftype_check(namefile_obj, indis)
    !
    ! -- Create discretization object
    !if (indis6 > 0) then
    !  call this%load_input_context('DIS6', this%name, 'DIS', indis, this%iout)
    !  call dis_cr(this%dis, this%name, indis, this%iout)
    !elseif (indisu6 > 0) then
    !  call this%load_input_context('DISU6', this%name, 'DISU', indis, this%iout)
    !  call disu_cr(this%dis, this%name, indis, this%iout)
    !elseif (indisv6 > 0) then
    !  call this%load_input_context('DISV6', this%name, 'DISV', indis, this%iout)
    !  call disv_cr(this%dis, this%name, indis, this%iout)
    !end if
    !
    ! -- Create utility objects
    !call budget_cr(this%budget, this%name, this%tsplab)
    !
    ! -- Create packages that are tied directly to model
    !call ic_cr(this%ic, this%name, this%inic, this%iout, this%dis, this%tsplab)
    !call fmi_cr(this%fmi, this%name, this%infmi, this%iout, this%tsplab)
    !call adv_cr(this%adv, this%name, this%inadv, this%iout, this%fmi,          &
    !            this%eqnsclfac)
    !call ssm_cr(this%ssm, this%name, this%inssm, this%iout, this%fmi,          &
    !            this%tsplab, this%eqnsclfac)
    !call mvt_cr(this%mvt, this%name, this%inmvt, this%iout, this%fmi)
    !call oc_cr(this%oc, this%name, this%inoc, this%iout)
    !call tsp_obs_cr(this%obs, this%inobs)
    !
    ! -- Return
    return
  end subroutine tsp_cr
  
  subroutine tsp_df(this)
! ******************************************************************************
! gwt_df -- Define packages of the model
! Subroutine: (1) call df routines for each package
!             (2) set variables and pointers
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy variables
    class(TransportModelType) :: this
    !
    ! -- Function extended by either GWT or GWE
    !
    ! -- return
    return
  end subroutine tsp_df
    
  subroutine tsp_ac(this, sparse)
! ******************************************************************************
! gwt_ac -- Add the internal connections of this model to the sparse matrix
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use SparseModule, only: sparsematrix
    ! -- dummy variables
    class(TransportModelType) :: this
    type(sparsematrix), intent(inout) :: sparse
    ! -- local
! ------------------------------------------------------------------------------
    !
    ! -- Function extended by either GWT or GWE
    !
    ! -- return
    return
  end subroutine tsp_ac
  
  subroutine tsp_mc(this, matrix_sln)
! ******************************************************************************
! gwt_mc -- Map the positions of this models connections in the
! numerical solution coefficient matrix.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(TransportModelType) :: this
    class(MatrixBaseType), pointer :: matrix_sln !< global system matrix
    ! -- local
! ------------------------------------------------------------------------------
    !
    ! -- Function extended by either GWT or GWE
    !
    ! -- return
    return
  end subroutine tsp_mc

  subroutine tsp_ar(this)
! ******************************************************************************
! gwt_ar -- GroundWater Transport Model Allocate and Read
! Subroutine: (1) allocates and reads packages part of this model,
!             (2) allocates memory for arrays part of this model object
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy variables
    class(TransportModelType) :: this
! ------------------------------------------------------------------------------
    !
    ! -- Function extended by either GWT or GWE
    !
    ! -- return
    return
  end subroutine tsp_ar

  subroutine tsp_rp(this)
! ******************************************************************************
! gwt_rp -- GroundWater Transport Model Read and Prepare
! Subroutine: (1) calls package read and prepare routines
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy variables
    class(TransportModelType) :: this
! ------------------------------------------------------------------------------
    !
    ! -- Function extended by either GWT or GWE
    !
    ! -- Return
    return
  end subroutine tsp_rp
  
  subroutine tsp_ad(this)
! ******************************************************************************
! gwt_ad -- GroundWater Transport Model Time Step Advance
! Subroutine: (1) calls package advance subroutines
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy variables
    class(TransportModelType) :: this
! ------------------------------------------------------------------------------
    !
    ! -- Function extended by either GWT or GWE
    !
    ! -- return
    return
  end subroutine tsp_ad

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
    ! -- Function extended by either GWT or GWE
    !
    ! -- return
    return
  end subroutine tsp_fc

  subroutine tsp_cc(this, innertot, kiter, iend, icnvgmod, cpak, ipak, dpak)
! ******************************************************************************
! gwt_cc -- GroundWater Transport Model Final Convergence Check
! Subroutine: (1) calls package cc routines
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
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
    ! -- Function extended by either GWT or GWE
    !
    ! -- return
    return
  end subroutine tsp_cc

  subroutine tsp_cq(this, icnvg, isuppress_output)
! ******************************************************************************
! tsp_cq -- Transport model calculate flow
! Subroutine: (1) Calculate intercell flows (flowja)
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy variables
    class(TransportModelType) :: this
    integer(I4B), intent(in) :: icnvg
    integer(I4B), intent(in) :: isuppress_output
    ! -- local
    integer(I4B) :: i
! ------------------------------------------------------------------------------
    !
    ! -- Function extended by either GWT or GWE
    !
    ! -- Return
    return
  end subroutine tsp_cq
  
  subroutine tsp_bd(this, icnvg, isuppress_output)
! ******************************************************************************
! tsp_bd --GroundWater Transport Model Budget
! Subroutine: (1) Calculate intercell flows (flowja)
!             (2) Calculate package contributions to model budget
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(TransportModelType) :: this
    integer(I4B), intent(in) :: icnvg
    integer(I4B), intent(in) :: isuppress_output
! ------------------------------------------------------------------------------
    !
    ! -- Function extended by either GWT or GWE
    !
    ! -- Return
    return
  end subroutine tsp_bd

  subroutine tsp_ot(this)
! ******************************************************************************
! tsp_ot -- Transport Model Output
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use TdisModule, only: kstp, kper, tdis_ot, endofperiod
    ! -- dummy
    class(TransportModelType) :: this
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
    idvprint = this%oc%set_print_flag(trim(this%tsplab%depvartype), this%icnvg, endofperiod)
    !
    !   Calculate and save observations
    call this%tsp_ot_obs()
    !
    !   Save and print flows
    call this%tsp_ot_flow(icbcfl, ibudfl, icbcun)
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
  
  subroutine tsp_ot_flow(this, icbcfl, ibudfl, icbcun)
    class(TransportModelType) :: this
    integer(I4B), intent(in) :: icbcfl
    integer(I4B), intent(in) :: ibudfl
    integer(I4B), intent(in) :: icbcun
    class(BndType), pointer :: packobj
    integer(I4B) :: ip
! ------------------------------------------------------------------------------
    ! -- Save TSP flows
    call this%tsp_ot_flowja(this%nja, this%flowja, icbcfl, icbcun)
    if (this%inmst > 0) call this%tsp_ot_flowja(this%nja, this%flowja, & 
                                                icbcfl, icbcun)
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

  subroutine tsp_ot_flowja(this, nja, flowja, icbcfl, icbcun)
! ******************************************************************************
! gwt_ot_flowja -- Write intercell flows
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
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

  end subroutine tsp_ot_dv

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

  end subroutine tsp_ot_bdsummary
  
  subroutine allocate_tsp_scalars(this, modelname)
! ******************************************************************************
! allocate_scalars -- Allocate memory for non-allocatable members
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
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
    ! -- return
    return
  end subroutine allocate_tsp_scalars
  
  subroutine tsp_da(this)
! ******************************************************************************
! tsp_da -- Deallocate
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
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
    call mem_deallocate(this%indsp)
    call mem_deallocate(this%inssm)
    call mem_deallocate(this%inmst)
    call mem_deallocate(this%inmvt)
    call mem_deallocate(this%inoc)
    call mem_deallocate(this%inobs)
    call mem_deallocate(this%eqnsclfac)
    !
    ! -- return
    return
  end subroutine tsp_da
  
  subroutine ftype_check(this, indis)
! ******************************************************************************
! ftype_check -- Check to make sure required input files have been specified
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use ConstantsModule, only: LINELENGTH
    use SimModule, only: store_error, count_errors, store_error_filename
    ! -- dummy
    class(TransportModelType) :: this
    integer(I4B), intent(in) :: indis
    ! -- local
    character(len=LINELENGTH) :: errmsg
! ------------------------------------------------------------------------------
    !
    ! -- Check for IC6, DIS(u), and MST. Stop if not present.
    if (this%inic == 0) then
      write (errmsg, '(1x,a)') &
        'ERROR. INITIAL CONDITIONS (IC6) PACKAGE NOT SPECIFIED.'
      call store_error(errmsg)
    end if
    if (indis == 0) then
      write (errmsg, '(1x,a)') &
        'ERROR. DISCRETIZATION (DIS6 or DISU6) PACKAGE NOT SPECIFIED.'
      call store_error(errmsg)
    end if
    if (this%inmst == 0) then
      write (errmsg, '(1x,a)') 'ERROR. MASS STORAGE AND TRANSFER (MST6) &
        &PACKAGE NOT SPECIFIED.'
      call store_error(errmsg)
    end if
    !
    if (count_errors() > 0) then
      write (errmsg, '(1x,a)') 'ERROR. REQUIRED PACKAGE(S) NOT SPECIFIED.'
      call store_error(errmsg)
      call store_error_filename(this%filename)
    end if
    !
    ! -- return
    return
  end subroutine ftype_check
  
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
    ! -- return
    return
  end subroutine create_lstfile

  !> @brief Write model namfile options to list file
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
  subroutine create_packages(this, indis)
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
    !use GwtMstModule, only: mst_cr
    use TspAdvModule, only: adv_cr
    !use GwtDspModule, only: dsp_cr
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
    integer(I4B), dimension(:), allocatable :: bndpkgs
    integer(I4B) :: n
    character(len=LENMEMPATH) :: mempathdsp = ''
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
      !case ('MST6')
      !  this%inmst = inunit
      case ('ADV6')
        this%inadv = inunit
      !case ('DSP6')
      !  this%indsp = 1
      !  mempathdsp = mempath
      case ('SSM6')
        this%inssm = inunit
      case ('OC6')
        this%inoc = inunit
      case ('OBS6')
        this%inobs = inunit
      !case ('CNC6', 'SRC6', 'LKT6', 'SFT6', &
      !      'MWT6', 'UZT6', 'IST6', 'API6')
      !  call expandarray(bndpkgs)
      !  bndpkgs(size(bndpkgs)) = n
      !case default
        ! TODO
      end select
    end do
    !
    ! -- Create packages that are tied directly to model
    call ic_cr(this%ic, this%name, this%inic, this%iout, this%dis, this%tsplab)
    call fmi_cr(this%fmi, this%name, this%infmi, this%iout, this%tsplab,       &
                this%eqnsclfac)
    !call mst_cr(this%mst, this%name, this%inmst, this%iout, this%fmi)
    call adv_cr(this%adv, this%name, this%inadv, this%iout, this%fmi, this%eqnsclfac)
    !call dsp_cr(this%dsp, this%name, mempathdsp, this%indsp, this%iout, this%fmi)
    call ssm_cr(this%ssm, this%name, this%inssm, this%iout, this%fmi, this%tsplab, this%eqnsclfac)
    call mvt_cr(this%mvt, this%name, this%inmvt, this%iout, this%fmi,          &
                this%eqnsclfac)
    call oc_cr(this%oc, this%name, this%inoc, this%iout)
    call tsp_obs_cr(this%obs, this%inobs)
    !
    ! -- Check to make sure that required ftype's have been specified
    !call this%ftype_check(indis)
    !
    !call this%create_bndpkgs(bndpkgs, pkgtypes, pkgnames, mempaths, inunits)
    !
    ! -- return
    return
  end subroutine create_packages  
  

end module TransportModelModule
