! Groundwater Transport (GWT) Model
! The following are additional features/checks to add
!   * Add check that discretization is the same between both models
!   * Consider implementation of steady-state transport (affects MST, IST)
!   * Check and handle pore space discrepancy between flow and transport (porosity vs specific yield)
!   * UZT may not have the required porosity term

module GwtModule

  use KindModule, only: DP, I4B
  use InputOutputModule, only: ParseLine, upcase
  use ConstantsModule, only: LENFTYPE, DZERO, LENPAKLOC
  use VersionModule, only: write_listfile_header
  use NumericalModelModule, only: NumericalModelType
  use TransportModelModule, only: TransportModelType
  use BaseModelModule, only: BaseModelType
  use BndModule, only: BndType, AddBndToList, GetBndFromList
  use GwtIcModule, only: GwtIcType
  use GwtFmiModule, only: GwtFmiType
  use GwtAdvModule, only: GwtAdvType
  use GwtDspModule, only: GwtDspType
  use GwtSsmModule, only: GwtSsmType
  use GwtMvtModule, only: GwtMvtType
  use GwtMstModule, only: GwtMstType
  use GwtOcModule, only: GwtOcType
  use GwtObsModule, only: GwtObsType
  use BudgetModule, only: BudgetType

  implicit none

  private
  public :: gwt_cr
  public :: GwtModelType
  public :: CastAsGwtModel

  type, extends(TransportModelType) :: GwtModelType

    type(GwtIcType), pointer :: ic => null() ! initial conditions package
    type(GwtFmiType), pointer :: fmi => null() ! flow model interface
    type(GwtMstType), pointer :: mst => null() ! mass storage and transfer package
    type(GwtAdvType), pointer :: adv => null() ! advection package
    type(GwtDspType), pointer :: dsp => null() ! dispersion package
    type(GwtSsmType), pointer :: ssm => null() ! source sink mixing package
    type(GwtMvtType), pointer :: mvt => null() ! mover transport package
    type(GwtOcType), pointer :: oc => null() ! output control package
    type(GwtObsType), pointer :: obs => null() ! observation package
    type(BudgetType), pointer :: budget => null() ! budget object
    integer(I4B), pointer :: inic => null() ! unit number IC
    integer(I4B), pointer :: infmi => null() ! unit number FMI
    integer(I4B), pointer :: inmvt => null() ! unit number MVT
    integer(I4B), pointer :: inmst => null() ! unit number MST
    integer(I4B), pointer :: inadv => null() ! unit number ADV
    integer(I4B), pointer :: indsp => null() ! unit number DSP
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
    procedure, private :: package_create
    procedure, private :: ftype_check
    procedure :: get_iasym => gwt_get_iasym
    procedure, private :: gwt_ot_flow
    procedure, private :: gwt_ot_flowja
    procedure, private :: gwt_ot_dv
    procedure, private :: gwt_ot_bdsummary
    procedure, private :: gwt_ot_obs

  end type GwtModelType

  ! -- Module variables constant for simulation
  integer(I4B), parameter :: NIUNIT = 100
  character(len=LENFTYPE), dimension(NIUNIT) :: cunit
  data cunit/'DIS6 ', 'DISV6', 'DISU6', 'IC6  ', 'MST6 ', & !  5
            &'ADV6 ', 'DSP6 ', 'SSM6 ', '     ', 'CNC6 ', & ! 10
            &'OC6  ', 'OBS6 ', 'FMI6 ', 'SRC6 ', 'IST6 ', & ! 15
            &'LKT6 ', 'SFT6 ', 'MWT6 ', 'UZT6 ', 'MVT6 ', & ! 20
            &'API6 ', '     ', '     ', '     ', '     ', & ! 25
            &75*'     '/

contains

  subroutine gwt_cr(filename, id, modelname)
! ******************************************************************************
! gwt_cr -- Create a new groundwater transport model object
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use ListsModule, only: basemodellist
    use BaseModelModule, only: AddBaseModelToList
    use SimModule, only: store_error, count_errors
    use ConstantsModule, only: LINELENGTH, LENPACKAGENAME
    use CompilerVersion
    use MemoryManagerModule, only: mem_allocate
    use MemoryHelperModule, only: create_mem_path
    use GwfDisModule, only: dis_cr
    use GwfDisvModule, only: disv_cr
    use GwfDisuModule, only: disu_cr
    use GwtIcModule, only: ic_cr
    use GwtFmiModule, only: fmi_cr
    use GwtMstModule, only: mst_cr
    use GwtAdvModule, only: adv_cr
    use GwtDspModule, only: dsp_cr
    use GwtSsmModule, only: ssm_cr
    use GwtMvtModule, only: mvt_cr
    use GwtOcModule, only: oc_cr
    use GwtObsModule, only: gwt_obs_cr
    use BudgetModule, only: budget_cr
    use NameFileModule, only: NameFileType
    ! -- dummy
    character(len=*), intent(in) :: filename
    integer(I4B), intent(in) :: id
    character(len=*), intent(in) :: modelname
    ! -- local
    integer(I4B) :: indis, indis6, indisu6, indisv6
    integer(I4B) :: ipakid, i, j, iu, ipaknum
    character(len=LINELENGTH) :: errmsg
    character(len=LENPACKAGENAME) :: pakname
    type(NameFileType) :: namefile_obj
    type(GwtModelType), pointer :: this
    class(BaseModelType), pointer :: model
    integer(I4B) :: nwords
    character(len=LINELENGTH), allocatable, dimension(:) :: words
! ------------------------------------------------------------------------------
    !
    ! -- Allocate a new GWT Model (this) and add it to basemodellist
    allocate (this)
    !
    ! -- Set this before any allocs in the memory manager can be done
    this%memoryPath = create_mem_path(modelname)
    !
    call this%allocate_scalars(modelname)
    model => this
    call AddBaseModelToList(basemodellist, model)
    !
    ! -- Assign values
    this%filename = filename
    this%name = modelname
    this%macronym = 'GWT'
    this%id = id
    !
    ! -- Open namefile and set iout
    call namefile_obj%init(this%filename, 0)
    call namefile_obj%add_cunit(niunit, cunit)
    call namefile_obj%openlistfile(this%iout)
    !
    ! -- Write header to model list file
    call write_listfile_header(this%iout, 'GROUNDWATER TRANSPORT MODEL (GWT)')
    !
    ! -- Open files
    call namefile_obj%openfiles(this%iout)
    !
    ! --
    if (size(namefile_obj%opts) > 0) then
      write (this%iout, '(1x,a)') 'NAMEFILE OPTIONS:'
    end if
    !
    ! -- parse options in the gwt name file
    do i = 1, size(namefile_obj%opts)
      call ParseLine(namefile_obj%opts(i), nwords, words)
      call upcase(words(1))
      select case (words(1))
      case ('PRINT_INPUT')
        this%iprpak = 1
        write (this%iout, '(4x,a)') 'STRESS PACKAGE INPUT WILL BE PRINTED '// &
          'FOR ALL MODEL STRESS PACKAGES'
      case ('PRINT_FLOWS')
        this%iprflow = 1
        write (this%iout, '(4x,a)') 'PACKAGE FLOWS WILL BE PRINTED '// &
          'FOR ALL MODEL PACKAGES'
      case ('SAVE_FLOWS')
        this%ipakcb = -1
        write (this%iout, '(4x,a)') &
          'FLOWS WILL BE SAVED TO BUDGET FILE SPECIFIED IN OUTPUT CONTROL'
      case default
        write (errmsg, '(4x,a,a,a,a)') &
          'UNKNOWN GWT NAMEFILE (', &
          trim(adjustl(this%filename)), ') OPTION: ', &
          trim(adjustl(namefile_obj%opts(i)))
        call store_error(errmsg, terminate=.TRUE.)
      end select
    end do
    !
    ! -- Assign unit numbers to attached modules, and remove
    ! -- from unitnumber (by specifying 1 for iremove)
    !
    indis = 0
    indis6 = 0
    indisu6 = 0
    indisv6 = 0
    call namefile_obj%get_unitnumber('DIS6', indis6, 1)
    if (indis6 > 0) indis = indis6
    if (indis <= 0) call namefile_obj%get_unitnumber('DISU6', indisu6, 1)
    if (indisu6 > 0) indis = indisu6
    if (indis <= 0) call namefile_obj%get_unitnumber('DISV6', indisv6, 1)
    if (indisv6 > 0) indis = indisv6
    call namefile_obj%get_unitnumber('IC6', this%inic, 1)
    call namefile_obj%get_unitnumber('FMI6', this%infmi, 1)
    call namefile_obj%get_unitnumber('MVT6', this%inmvt, 1)
    call namefile_obj%get_unitnumber('MST6', this%inmst, 1)
    call namefile_obj%get_unitnumber('ADV6', this%inadv, 1)
    call namefile_obj%get_unitnumber('DSP6', this%indsp, 1)
    call namefile_obj%get_unitnumber('SSM6', this%inssm, 1)
    call namefile_obj%get_unitnumber('OC6', this%inoc, 1)
    call namefile_obj%get_unitnumber('OBS6', this%inobs, 1)
    !
    ! -- Check to make sure that required ftype's have been specified
    call this%ftype_check(namefile_obj, indis)
    !
    ! -- Create discretization object
    if (indis6 > 0) then
      call dis_cr(this%dis, this%name, indis, this%iout)
    elseif (indisu6 > 0) then
      call disu_cr(this%dis, this%name, indis, this%iout)
    elseif (indisv6 > 0) then
      call disv_cr(this%dis, this%name, indis, this%iout)
    end if
    !
    ! -- Create utility objects
    call budget_cr(this%budget, this%name)
    !
    ! -- Create packages that are tied directly to model
    call ic_cr(this%ic, this%name, this%inic, this%iout, this%dis)
    call fmi_cr(this%fmi, this%name, this%infmi, this%iout)
    call mst_cr(this%mst, this%name, this%inmst, this%iout, this%fmi)
    call adv_cr(this%adv, this%name, this%inadv, this%iout, this%fmi)
    call dsp_cr(this%dsp, this%name, this%indsp, this%iout, this%fmi)
    call ssm_cr(this%ssm, this%name, this%inssm, this%iout, this%fmi)
    call mvt_cr(this%mvt, this%name, this%inmvt, this%iout, this%fmi)
    call oc_cr(this%oc, this%name, this%inoc, this%iout)
    call gwt_obs_cr(this%obs, this%inobs)
    !
    ! -- Create stress packages
    ipakid = 1
    do i = 1, niunit
      ipaknum = 1
      do j = 1, namefile_obj%get_nval_for_row(i)
        iu = namefile_obj%get_unitnumber_rowcol(i, j)
        call namefile_obj%get_pakname(i, j, pakname)
        call this%package_create(cunit(i), ipakid, ipaknum, pakname, iu, &
                                 this%iout)
        ipaknum = ipaknum + 1
        ipakid = ipakid + 1
      end do
    end do
    !
    ! -- return
    return
  end subroutine gwt_cr

  subroutine gwt_df(this)
! ******************************************************************************
! gwt_df -- Define packages of the model
! Subroutine: (1) call df routines for each package
!             (2) set variables and pointers
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(GwtModelType) :: this
    ! -- local
    integer(I4B) :: ip
    class(BndType), pointer :: packobj
! ------------------------------------------------------------------------------
    !
    ! -- Define packages and utility objects
    call this%dis%dis_df()
    call this%fmi%fmi_df(this%dis, this%inssm)
    if (this%inmvt > 0) call this%mvt%mvt_df(this%dis)
    if (this%inadv > 0) call this%adv%adv_df()
    if (this%indsp > 0) call this%dsp%dsp_df(this%dis)
    if (this%inssm > 0) call this%ssm%ssm_df()
    call this%oc%oc_df()
    call this%budget%budget_df(niunit, 'MASS', 'M')
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
    ! -- return
    return
  end subroutine gwt_df

  subroutine gwt_ac(this, sparse)
! ******************************************************************************
! gwt_ac -- Add the internal connections of this model to the sparse matrix
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use SparseModule, only: sparsematrix
    ! -- dummy
    class(GwtModelType) :: this
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
  end subroutine gwt_ac

  subroutine gwt_mc(this, iasln, jasln)
! ******************************************************************************
! gwt_mc -- Map the positions of this models connections in the
! numerical solution coefficient matrix.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(GwtModelType) :: this
    integer(I4B), dimension(:), intent(in) :: iasln
    integer(I4B), dimension(:), intent(in) :: jasln
    ! -- local
    class(BndType), pointer :: packobj
    integer(I4B) :: ip
! ------------------------------------------------------------------------------
    !
    ! -- Find the position of each connection in the global ia, ja structure
    !    and store them in idxglo.
    call this%dis%dis_mc(this%moffset, this%idxglo, iasln, jasln)
    if (this%indsp > 0) call this%dsp%dsp_mc(this%moffset, iasln, jasln)
    !
    ! -- Map any package connections
    do ip = 1, this%bndlist%Count()
      packobj => GetBndFromList(this%bndlist, ip)
      call packobj%bnd_mc(this%moffset, iasln, jasln)
    end do
    !
    ! -- return
    return
  end subroutine gwt_mc

  subroutine gwt_ar(this)
! ******************************************************************************
! gwt_ar -- GroundWater Transport Model Allocate and Read
! Subroutine: (1) allocates and reads packages part of this model,
!             (2) allocates memory for arrays part of this model object
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use ConstantsModule, only: DHNOFLO
    ! -- dummy
    class(GwtModelType) :: this
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
    if (this%inobs > 0) call this%obs%gwt_obs_ar(this%ic, this%x, this%flowja)
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
    ! -- return
    return
  end subroutine gwt_ar

  subroutine gwt_rp(this)
! ******************************************************************************
! gwt_rp -- GroundWater Transport Model Read and Prepare
! Subroutine: (1) calls package read and prepare routines
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use TdisModule, only: readnewdata
    ! -- dummy
    class(GwtModelType) :: this
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
  end subroutine gwt_rp

  subroutine gwt_ad(this)
! ******************************************************************************
! gwt_ad -- GroundWater Transport Model Time Step Advance
! Subroutine: (1) calls package advance subroutines
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use SimVariablesModule, only: isimcheck, iFailedStepRetry
    ! -- dummy
    class(GwtModelType) :: this
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
    ! -- return
    return
  end subroutine gwt_ad

  subroutine gwt_cf(this, kiter)
! ******************************************************************************
! gwt_cf -- GroundWater Transport Model calculate coefficients
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(GwtModelType) :: this
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
  end subroutine gwt_cf

  subroutine gwt_fc(this, kiter, amatsln, njasln, inwtflag)
! ******************************************************************************
! gwt_fc -- GroundWater Transport Model fill coefficients
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(GwtModelType) :: this
    integer(I4B), intent(in) :: kiter
    integer(I4B), intent(in) :: njasln
    real(DP), dimension(njasln), intent(inout) :: amatsln
    integer(I4B), intent(in) :: inwtflag
    ! -- local
    class(BndType), pointer :: packobj
    integer(I4B) :: ip
! ------------------------------------------------------------------------------
    !
    ! -- call fc routines
    call this%fmi%fmi_fc(this%dis%nodes, this%xold, this%nja, njasln, &
                         amatsln, this%idxglo, this%rhs)
    if (this%inmvt > 0) then
      call this%mvt%mvt_fc(this%x, this%x)
    end if
    if (this%inmst > 0) then
      call this%mst%mst_fc(this%dis%nodes, this%xold, this%nja, njasln, &
                           amatsln, this%idxglo, this%x, this%rhs, kiter)
    end if
    if (this%inadv > 0) then
      call this%adv%adv_fc(this%dis%nodes, amatsln, this%idxglo, this%x, &
                           this%rhs)
    end if
    if (this%indsp > 0) then
      call this%dsp%dsp_fc(kiter, this%dis%nodes, this%nja, njasln, amatsln, &
                           this%idxglo, this%rhs, this%x)
    end if
    if (this%inssm > 0) then
      call this%ssm%ssm_fc(amatsln, this%idxglo, this%rhs)
    end if
    !
    ! -- packages
    do ip = 1, this%bndlist%Count()
      packobj => GetBndFromList(this%bndlist, ip)
      call packobj%bnd_fc(this%rhs, this%ia, this%idxglo, amatsln)
    end do
    !
    ! -- return
    return
  end subroutine gwt_fc

  subroutine gwt_cc(this, innertot, kiter, iend, icnvgmod, cpak, ipak, dpak)
! ******************************************************************************
! gwt_cc -- GroundWater Transport Model Final Convergence Check
! Subroutine: (1) calls package cc routines
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
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
  end subroutine gwt_cc

  subroutine gwt_cq(this, icnvg, isuppress_output)
! ******************************************************************************
! gwt_cq --Groundwater transport model calculate flow
! Subroutine: (1) Calculate intercell flows (flowja)
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
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
  end subroutine gwt_cq

  subroutine gwt_bd(this, icnvg, isuppress_output)
! ******************************************************************************
! gwt_bd --GroundWater Transport Model Budget
! Subroutine: (1) Calculate intercell flows (flowja)
!             (2) Calculate package contributions to model budget
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use ConstantsModule, only: DZERO
    ! -- dummy
    class(GwtModelType) :: this
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
  end subroutine gwt_bd

  subroutine gwt_ot(this)
! ******************************************************************************
! gwt_ot -- GroundWater Transport Model Output
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
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
! ------------------------------------------------------------------------------
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

  end subroutine gwt_ot_obs

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

  end subroutine gwt_ot_flow

  subroutine gwt_ot_flowja(this, nja, flowja, icbcfl, icbcun)
! ******************************************************************************
! gwt_ot_flowja -- Write intercell flows
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(GwtModelType) :: this
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
  end subroutine gwt_ot_flowja

  subroutine gwt_ot_dv(this, idvsave, idvprint, ipflag)
    class(GwtModelType) :: this
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

  end subroutine gwt_ot_dv

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

  end subroutine gwt_ot_bdsummary

  subroutine gwt_da(this)
! ******************************************************************************
! gwt_da -- Deallocate
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryManagerModule, only: mem_deallocate
    ! -- dummy
    class(GwtModelType) :: this
    ! -- local
    integer(I4B) :: ip
    class(BndType), pointer :: packobj
! ------------------------------------------------------------------------------
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
    deallocate (this%fmi)
    deallocate (this%adv)
    deallocate (this%dsp)
    deallocate (this%ssm)
    deallocate (this%mst)
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
    call mem_deallocate(this%inic)
    call mem_deallocate(this%infmi)
    call mem_deallocate(this%inadv)
    call mem_deallocate(this%indsp)
    call mem_deallocate(this%inssm)
    call mem_deallocate(this%inmst)
    call mem_deallocate(this%inmvt)
    call mem_deallocate(this%inoc)
    call mem_deallocate(this%inobs)
    !
    ! -- NumericalModelType
    call this%NumericalModelType%model_da()
    !
    ! -- return
    return
  end subroutine gwt_da

  !> @brief GroundWater Transport Model Budget Entry
  !!
  !! This subroutine adds a budget entry to the flow budget.  It was added as
  !! a method for the gwt model object so that the exchange object could add its
  !! contributions.
  !!
  !! (1) adds the entry to the budget object
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
! ------------------------------------------------------------------------------
    !
    call this%budget%addentry(budterm, delt, budtxt, rowlabel=rowlabel)
    !
    ! -- return
    return
  end subroutine gwt_bdentry

  function gwt_get_iasym(this) result(iasym)
! ******************************************************************************
! gwt_get_iasym -- return 1 if any package causes the matrix to be asymmetric.
!   Otherwise return 0.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    class(GwtModelType) :: this
    ! -- local
    integer(I4B) :: iasym
! ------------------------------------------------------------------------------
    !
    ! -- Start by setting iasym to zero
    iasym = 0
    !
    ! -- ADV
    if (this%inadv > 0) then
      if (this%adv%iasym /= 0) iasym = 1
    end if
    !
    ! -- return
    return
  end function gwt_get_iasym

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
    class(GwtModelType) :: this
    character(len=*), intent(in) :: modelname
! ------------------------------------------------------------------------------
    !
    ! -- allocate members from parent class
    call this%NumericalModelType%allocate_scalars(modelname)
    !
    ! -- allocate members that are part of model class
    call mem_allocate(this%inic, 'INIC', this%memoryPath)
    call mem_allocate(this%infmi, 'INFMI', this%memoryPath)
    call mem_allocate(this%inmvt, 'INMVT', this%memoryPath)
    call mem_allocate(this%inmst, 'INMST', this%memoryPath)
    call mem_allocate(this%inadv, 'INADV', this%memoryPath)
    call mem_allocate(this%indsp, 'INDSP', this%memoryPath)
    call mem_allocate(this%inssm, 'INSSM', this%memoryPath)
    call mem_allocate(this%inoc, 'INOC ', this%memoryPath)
    call mem_allocate(this%inobs, 'INOBS', this%memoryPath)
    !
    this%inic = 0
    this%infmi = 0
    this%inmvt = 0
    this%inmst = 0
    this%inadv = 0
    this%indsp = 0
    this%inssm = 0
    this%inoc = 0
    this%inobs = 0
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
! ------------------------------------------------------------------------------
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
    ! -- return
    return
  end subroutine package_create

  subroutine ftype_check(this, namefile_obj, indis)
! ******************************************************************************
! ftype_check -- Check to make sure required input files have been specified
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use ConstantsModule, only: LINELENGTH
    use SimModule, only: store_error, count_errors
    use NameFileModule, only: NameFileType
    ! -- dummy
    class(GwtModelType) :: this
    type(NameFileType), intent(in) :: namefile_obj
    integer(I4B), intent(in) :: indis
    ! -- local
    character(len=LINELENGTH) :: errmsg
    integer(I4B) :: i, iu
    character(len=LENFTYPE), dimension(10) :: nodupftype = &
      &(/'DIS6 ', 'DISU6', 'DISV6', 'IC6  ', 'MST6 ', 'ADV6 ', 'DSP6 ', &
        &'SSM6 ', 'OC6  ', 'OBS6 '/)
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
    if (count_errors() > 0) then
      write (errmsg, '(1x,a)') 'ERROR. REQUIRED PACKAGE(S) NOT SPECIFIED.'
      call store_error(errmsg)
    end if
    !
    ! -- Check to make sure that some GWT packages are not specified more
    !    than once
    do i = 1, size(nodupftype)
      call namefile_obj%get_unitnumber(trim(nodupftype(i)), iu, 0)
      if (iu > 0) then
        write (errmsg, '(1x, a, a, a)') &
          'DUPLICATE ENTRIES FOR FTYPE ', trim(nodupftype(i)), &
          ' NOT ALLOWED FOR GWT MODEL.'
        call store_error(errmsg)
      end if
    end do
    !
    ! -- Stop if errors
    if (count_errors() > 0) then
      write (errmsg, '(a, a)') 'ERROR OCCURRED WHILE READING FILE: ', &
        trim(namefile_obj%filename)
      call store_error(errmsg, terminate=.TRUE.)
    end if
    !
    ! -- return
    return
  end subroutine ftype_check

  !> @brief Cast to GwtModelType
  function CastAsGwtModel(model) result(gwtmodel)
    class(*), pointer :: model !< The object to be cast
    class(GwtModelType), pointer :: gwtmodel !< The GWT model

    gwtmodel => null()
    if (.not. associated(model)) return
    select type (model)
    type is (GwtModelType)
      gwtmodel => model
    end select

  end function CastAsGwtModel

end module GwtModule
