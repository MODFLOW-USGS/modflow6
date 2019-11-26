module GwfModule

  use KindModule,                  only: DP, I4B
  use InputOutputModule,           only: ParseLine, upcase
  use ConstantsModule,             only: LENFTYPE, DZERO, DEM1, DTEN, DEP20
  use NumericalModelModule,        only: NumericalModelType
  use BaseDisModule,               only: DisBaseType
  use BndModule,                   only: BndType, AddBndToList, GetBndFromList
  use GwfIcModule,                 only: GwfIcType
  use GwfNpfModule,                only: GwfNpfType
  use Xt3dModule,                  only: Xt3dType
  use GwfHfbModule,                only: GwfHfbType
  use GwfStoModule,                only: GwfStoType
  use GwfCsubModule,               only: GwfCsubType
  use GwfMvrModule,                only: GwfMvrType
  use BudgetModule,                only: BudgetType
  use GwfOcModule,                 only: GwfOcType
  use GhostNodeModule,             only: GhostNodeType, gnc_cr
  use GwfObsModule,                only: GwfObsType, gwf_obs_cr
  use SimModule,                   only: count_errors, store_error,            &
                                         store_error_unit, ustop
  use BaseModelModule,             only: BaseModelType

  implicit none

  private
  public :: gwf_cr
  public :: GwfModelType

  type, extends(NumericalModelType) :: GwfModelType

    type(GwfIcType),                pointer :: ic      => null()                ! initial conditions package
    type(GwfNpfType),               pointer :: npf     => null()                ! node property flow package
    type(Xt3dType),                 pointer :: xt3d    => null()                ! xt3d option for npf
    type(GwfStoType),               pointer :: sto     => null()                ! storage package
    type(GwfCsubType),              pointer :: csub    => null()                ! subsidence package    
    type(GwfOcType),                pointer :: oc      => null()                ! output control package
    type(GhostNodeType),            pointer :: gnc     => null()                ! ghost node correction package
    type(GwfHfbType),               pointer :: hfb     => null()                ! horizontal flow barrier package
    type(GwfMvrType),               pointer :: mvr     => null()                ! water mover package
    type(GwfObsType),               pointer :: obs     => null()                ! observation package
    type(BudgetType),               pointer :: budget  => null()                ! budget object
    integer(I4B),                   pointer :: inic    => null()                ! unit number IC
    integer(I4B),                   pointer :: inoc    => null()                ! unit number OC
    integer(I4B),                   pointer :: innpf   => null()                ! unit number NPF
    integer(I4B),                   pointer :: insto   => null()                ! unit number STO
    integer(I4B),                   pointer :: incsub  => null()                ! unit number CSUB
    integer(I4B),                   pointer :: inmvr   => null()                ! unit number MVR
    integer(I4B),                   pointer :: inhfb   => null()                ! unit number HFB
    integer(I4B),                   pointer :: ingnc   => null()                ! unit number GNC
    integer(I4B),                   pointer :: inobs   => null()                ! unit number OBS
    integer(I4B),                   pointer :: iss     => null()                ! steady state flag
    integer(I4B),                   pointer :: inewtonur => null()              ! newton under relaxation flag

  contains

    procedure :: model_df                => gwf_df
    procedure :: model_ac                => gwf_ac
    procedure :: model_mc                => gwf_mc
    procedure :: model_ar                => gwf_ar
    procedure :: model_rp                => gwf_rp
    procedure :: model_ad                => gwf_ad
    procedure :: model_cf                => gwf_cf
    procedure :: model_fc                => gwf_fc
    procedure :: model_cc                => gwf_cc
    procedure :: model_ptcchk            => gwf_ptcchk
    procedure :: model_ptc               => gwf_ptc
    procedure :: model_nur               => gwf_nur
    procedure :: model_cq                => gwf_cq
    procedure :: model_bd                => gwf_bd
    procedure :: model_ot                => gwf_ot
    procedure :: model_fp                => gwf_fp
    procedure :: model_da                => gwf_da
    procedure :: get_nsubtimes           => gwf_get_nsubtimes
    procedure :: model_bdentry           => gwf_bdentry
    procedure :: get_iasym               => gwf_get_iasym
    ! -- private
    procedure :: allocate_scalars
    procedure :: package_create
    procedure :: ftype_check
    !
  end type GwfModelType

  ! -- Module variables constant for simulation
  integer(I4B), parameter :: NIUNIT=100
  character(len=LENFTYPE), dimension(NIUNIT) :: cunit
  data cunit/   'IC6  ', 'DIS6 ', 'DISU6', 'OC6  ', 'NPF6 ', & !  5
                'STO6 ', 'HFB6 ', 'WEL6 ', 'DRN6 ', 'RIV6 ', & ! 10
                'GHB6 ', 'RCH6 ', 'EVT6 ', 'OBS6 ', 'GNC6 ', & ! 15
                '     ', 'CHD6 ', '     ', '     ', '     ', & ! 20
                '     ', 'MAW6 ', 'SFR6 ', 'LAK6 ', 'UZF6 ', & ! 25
                'DISV6', 'MVR6 ', 'CSUB6', '     ', '     ', & ! 30
                70 * '     '/

  contains

  subroutine gwf_cr(filename, id, modelname, smr)
! ******************************************************************************
! gwf_cr -- Create a new groundwater flow model object
! Subroutine: (1) creates model object and add to modellist
!             (2) assign values
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use ListsModule,                only: basemodellist
    use BaseModelModule,            only: AddBaseModelToList
    use SimModule,                  only: ustop, store_error, count_errors
    use InputOutputModule,          only: write_centered
    use ConstantsModule,            only: LINELENGTH, LENPACKAGENAME
    use VersionModule,              only: VERSION, MFVNAM, MFTITLE,             &
                                          FMTDISCLAIMER, IDEVELOPMODE
    use CompilerVersion
    use MemoryManagerModule,        only: mem_allocate
    use GwfDisModule,               only: dis_cr
    use GwfDisvModule,              only: disv_cr
    use GwfDisuModule,              only: disu_cr
    use GwfNpfModule,               only: npf_cr
    use Xt3dModule,                 only: xt3d_cr
    use GwfStoModule,               only: sto_cr
    use GwfCsubModule,              only: csub_cr
    use GwfMvrModule,               only: mvr_cr
    use GwfHfbModule,               only: hfb_cr
    use GwfIcModule,                only: ic_cr
    use GwfOcModule,                only: oc_cr
    use BudgetModule,               only: budget_cr
    use NameFileModule,             only: NameFileType
    ! -- dummy
    character(len=*), intent(in)  :: filename
    integer(I4B), intent(in)           :: id
    character(len=*), intent(in)  :: modelname
    logical, optional, intent(in) :: smr
    ! -- local
    integer(I4B) :: indis, indis6, indisu6, indisv6
    integer(I4B) :: ipakid, i, j, iu, ipaknum
    character(len=LINELENGTH) :: errmsg
    character(len=LENPACKAGENAME) :: pakname
    type(NameFileType) :: namefile_obj
    type(GwfModelType), pointer        :: this
    class(BaseModelType), pointer       :: model
    integer(I4B) :: nwords
    character(len=LINELENGTH), allocatable, dimension(:) :: words
    character(len=80) :: compiler
    ! -- format
! ------------------------------------------------------------------------------
    !
    ! -- Allocate a new GWF Model (this) and add it to basemodellist
    allocate(this)
    call this%allocate_scalars(modelname)
    model => this
    call AddBaseModelToList(basemodellist, model)
    !
    ! -- Assign values
    this%filename = filename
    this%name = modelname
    this%macronym = 'GWF'
    this%id = id
    if(present(smr)) this%single_model_run = smr
    !
    ! -- Open namefile and set iout
    call namefile_obj%init(this%filename, 0)
    call namefile_obj%add_cunit(niunit, cunit)
    call namefile_obj%openlistfile(this%iout)
    !
    ! -- Write title to list file
    call write_centered('MODFLOW'//MFVNAM, this%iout, 80)
    call write_centered(MFTITLE, this%iout, 80)
    call write_centered('GROUNDWATER FLOW MODEL (GWF)', this%iout, 80)
    call write_centered('VERSION '//VERSION, this%iout, 80)
    !
    ! -- Write if develop mode
    if (IDEVELOPMODE == 1) call write_centered('***DEVELOP MODE***',           &
      this%iout, 80)
    !
    ! -- Write compiler version
    call get_compiler(compiler)
    call write_centered(' ', this%iout, 80)
    call write_centered(trim(adjustl(compiler)), this%iout, 80)
    !
    ! -- Write disclaimer
    write(this%iout, FMTDISCLAIMER)
    !
    ! -- Write precision of real variables
    write(this%iout, '(/,a)') 'MODFLOW was compiled using uniform precision.'
    write(this%iout, '(a,i0,/)') 'Precision of REAL variables: ',              &
                                 precision(DZERO)
    !
    ! -- Open files
    call namefile_obj%openfiles(this%iout)
    !
    ! -- GWF options
    if (size(namefile_obj%opts) > 0) then
      write(this%iout, '(1x,a)') 'NAMEFILE OPTIONS:'
    end if
    !
    ! -- Parse options in the GWF name file
    do i = 1, size(namefile_obj%opts)
      call ParseLine(namefile_obj%opts(i), nwords, words)
      call upcase(words(1))
      select case(words(1))
        case('NEWTON')
          this%inewton = 1
          write(this%iout, '(4x,a)')                                           &
                            'NEWTON-RAPHSON method enabled for the model.'
          if (nwords > 1) then
            call upcase(words(2))
            if (words(2) == 'UNDER_RELAXATION') then
              this%inewtonur = 1
              write(this%iout, '(4x,a,a)')                                     &
                'NEWTON-RAPHSON UNDER-RELAXATION based on the bottom ',        &
                'elevation of the model will be applied to the model.'
            end if
          end if
        case ('PRINT_INPUT')
          this%iprpak = 1
          write(this%iout,'(4x,a)') 'STRESS PACKAGE INPUT WILL BE PRINTED '//  &
                                    'FOR ALL MODEL STRESS PACKAGES'
        case ('PRINT_FLOWS')
          this%iprflow = 1
          write(this%iout,'(4x,a)') 'PACKAGE FLOWS WILL BE PRINTED '//         &
                                    'FOR ALL MODEL PACKAGES'
        case ('SAVE_FLOWS')
          this%ipakcb = -1
          write(this%iout, '(4x,a)')                                           &
            'FLOWS WILL BE SAVED TO BUDGET FILE SPECIFIED IN OUTPUT CONTROL'
        case default
          write(errmsg,'(4x,a,a,a,a)')                                         &
            '****ERROR. UNKNOWN GWF NAMEFILE (',                               &
            trim(adjustl(this%filename)), ') OPTION: ',                        &
            trim(adjustl(namefile_obj%opts(i)))
          call store_error(errmsg)
          call ustop()
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
    if(indis6 > 0) indis = indis6
    if(indis <= 0) call namefile_obj%get_unitnumber('DISU6', indisu6, 1)
    if(indisu6 > 0) indis = indisu6
    if(indis <= 0) call namefile_obj%get_unitnumber('DISV6', indisv6, 1)
    if(indisv6 > 0) indis = indisv6
    call namefile_obj%get_unitnumber('IC6',  this%inic, 1)
    call namefile_obj%get_unitnumber('OC6',  this%inoc, 1)
    call namefile_obj%get_unitnumber('NPF6', this%innpf, 1)
    call namefile_obj%get_unitnumber('STO6', this%insto, 1)
    call namefile_obj%get_unitnumber('CSUB6', this%incsub, 1)
    call namefile_obj%get_unitnumber('MVR6', this%inmvr, 1)
    call namefile_obj%get_unitnumber('HFB6', this%inhfb, 1)
    call namefile_obj%get_unitnumber('GNC6', this%ingnc, 1)
    call namefile_obj%get_unitnumber('OBS6', this%inobs, 1)
    !
    ! -- Check to make sure that required ftype's have been specified
    call this%ftype_check(namefile_obj, indis)
    !
    ! -- Create discretization object
    if(indis6 > 0) then
      call dis_cr(this%dis, this%name, indis, this%iout)
    elseif(indisu6 > 0) then
      call disu_cr(this%dis, this%name, indis, this%iout)
    elseif(indisv6 > 0) then
      call disv_cr(this%dis, this%name, indis, this%iout)
    endif
    !
    ! -- Create utility objects
    call budget_cr(this%budget, this%name)
    !
    ! -- Create packages that are tied directly to model
    call npf_cr(this%npf, this%name, this%innpf, this%iout)
    call xt3d_cr(this%xt3d, this%name, this%innpf, this%iout)
    call gnc_cr(this%gnc, this%name, this%ingnc, this%iout)
    call hfb_cr(this%hfb, this%name, this%inhfb, this%iout)
    call sto_cr(this%sto, this%name, this%insto, this%iout)
    call csub_cr(this%csub, this%name, this%insto, this%sto%name,               &
                 this%incsub, this%iout)
    call ic_cr(this%ic, this%name, this%inic, this%iout, this%dis)
    call mvr_cr(this%mvr, this%name, this%inmvr, this%iout)
    call oc_cr(this%oc, this%name, this%inoc, this%iout)
    call gwf_obs_cr(this%obs, this%inobs)
    !
    ! -- Create stress packages
    ipakid = 1
    do i = 1, niunit
      ipaknum = 1
      do j = 1, namefile_obj%get_nval_for_row(i)
        iu = namefile_obj%get_unitnumber_rowcol(i, j)
        call namefile_obj%get_pakname(i, j, pakname)
        call this%package_create(cunit(i), ipakid, ipaknum, pakname, iu,       &
          this%iout)
        ipaknum = ipaknum + 1
        ipakid = ipakid + 1
      enddo
    enddo
    !
    ! -- return
    return
  end subroutine gwf_cr

  subroutine gwf_df(this)
! ******************************************************************************
! gwf_df -- Define packages of the model
! Subroutine: (1) call df routines for each package
!             (2) set gwf variables and pointers
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(GwfModelType) :: this
    ! -- local
    integer(I4B) :: ip
    class(BndType), pointer :: packobj
! ------------------------------------------------------------------------------
    !
    ! -- Define packages and utility objects
    call this%dis%dis_df()
    call this%npf%npf_df(this%dis, this%xt3d, this%ingnc)
    call this%oc%oc_df()
    call this%budget%budget_df(niunit, 'VOLUME', 'L**3')
    if(this%ingnc > 0) call this%gnc%gnc_df(this)
    !
    ! -- Assign or point model members to dis members
    !    this%neq will be incremented if packages add additional unknowns
    this%neq = this%dis%nodes
    this%nja = this%dis%nja
    this%ia  => this%dis%con%ia
    this%ja  => this%dis%con%ja
    !
    ! -- Allocate model arrays, now that neq and nja are known
    call this%allocate_arrays()
    !
    ! -- Define packages and assign iout for time series managers
    do ip=1,this%bndlist%Count()
      packobj => GetBndFromList(this%bndlist, ip)
      call packobj%bnd_df(this%neq, this%dis)
    enddo
    !
    ! -- Store information needed for observations
    call this%obs%obs_df(this%iout, this%name, 'GWF', this%dis)
    !
    ! -- return
    return
  end subroutine gwf_df

  subroutine gwf_ac(this, sparse)
! ******************************************************************************
! gwf_ac -- Add the internal connections of this model to the sparse matrix
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use SparseModule, only: sparsematrix
    ! -- dummy
    class(GwfModelType) :: this
    type(sparsematrix), intent(inout) :: sparse
    ! -- local
    class(BndType), pointer :: packobj
    integer(I4B) :: ip
! ------------------------------------------------------------------------------
    !
    ! -- Add the primary grid connections of this model to sparse
    call this%dis%dis_ac(this%moffset, sparse)
    !
    ! -- Add any additional connections that NPF may need
    if(this%innpf > 0) call this%npf%npf_ac(this%moffset, sparse)
    !
    ! -- Add any package connections
    do ip = 1, this%bndlist%Count()
      packobj => GetBndFromList(this%bndlist, ip)
      call packobj%bnd_ac(this%moffset, sparse)
    enddo
    !
    ! -- If GNC is active, then add the gnc connections to sparse
    if(this%ingnc > 0) call this%gnc%gnc_ac(sparse)
    !
    ! -- return
    return
  end subroutine gwf_ac

  subroutine gwf_mc(this, iasln, jasln)
! ******************************************************************************
! gwf_mc -- Map the positions of this models connections in the
! numerical solution coefficient matrix.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(GwfModelType) :: this
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
    !
    ! -- Map any additional connections that NPF may need
    if(this%innpf > 0) call this%npf%npf_mc(this%moffset, iasln, jasln)
    !
    ! -- Map any package connections
    do ip=1,this%bndlist%Count()
      packobj => GetBndFromList(this%bndlist, ip)
      call packobj%bnd_mc(this%moffset, iasln, jasln)
    enddo
    !
    ! -- For implicit gnc, need to store positions of gnc connections
    !    in solution matrix connection
    if(this%ingnc > 0) call this%gnc%gnc_mc(iasln, jasln)
    !
    ! -- return
    return
  end subroutine gwf_mc

  subroutine gwf_ar(this)
! ******************************************************************************
! gwf_ar -- GroundWater Flow Model Allocate and Read
! Subroutine: (1) allocates and reads packages part of this model,
!             (2) allocates memory for arrays part of this model object
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(GwfModelType) :: this
    ! -- locals
    integer(I4B) :: ip
    class(BndType), pointer :: packobj
! ------------------------------------------------------------------------------
    !
    ! -- Allocate and read modules attached to model
    if(this%inic  > 0) call this%ic%ic_ar(this%x)
    if(this%innpf > 0) call this%npf%npf_ar(this%ic, this%ibound, this%x)
    if(this%inhfb > 0) call this%hfb%hfb_ar(this%ibound, this%xt3d, this%dis)
    if(this%insto > 0) call this%sto%sto_ar(this%dis, this%ibound)
    if(this%incsub > 0) call this%csub%csub_ar(this%dis, this%ibound)
    if(this%inmvr > 0) call this%mvr%mvr_ar()
    if(this%inobs > 0) call this%obs%gwf_obs_ar(this%ic, this%x, this%flowja)
    !
    ! -- Call dis_ar to write binary grid file
    call this%dis%dis_ar(this%npf%icelltype)
    !
    ! -- set up output control
    call this%oc%oc_ar(this%x, this%dis, this%npf%hnoflo)
    !
    ! -- Package input files now open, so allocate and read
    do ip=1,this%bndlist%Count()
      packobj => GetBndFromList(this%bndlist, ip)
      call packobj%set_pointers(this%dis%nodes, this%ibound, this%x,           &
                                this%xold, this%flowja)
      ! -- Read and allocate package
      call packobj%bnd_ar()
    enddo
    !
    ! -- return
    return
  end subroutine gwf_ar

  subroutine gwf_rp(this)
! ******************************************************************************
! gwf_rp -- GroundWater Flow Model Read and Prepare
! Subroutine: (1) calls package read and prepare routines
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use TdisModule, only: readnewdata
    ! -- dummy
    class(GwfModelType) :: this
    ! -- local
    class(BndType), pointer :: packobj
    integer(I4B) :: ip
! ------------------------------------------------------------------------------
    !
    ! -- Check with TDIS on whether or not it is time to RP
    if (.not. readnewdata) return
    !
    ! -- Read and prepare
    if(this%inhfb > 0) call this%hfb%hfb_rp()
    if(this%inoc > 0)  call this%oc%oc_rp()
    if(this%insto > 0) call this%sto%sto_rp()
    if(this%incsub > 0) call this%csub%csub_rp()
    if(this%inmvr > 0) call this%mvr%mvr_rp()
    do ip = 1, this%bndlist%Count()
      packobj => GetBndFromList(this%bndlist, ip)
      call packobj%bnd_rp()
      call packobj%bnd_rp_obs()
    enddo
    !
    ! -- Return
    return
  end subroutine gwf_rp

  subroutine gwf_ad(this, ipicard, isubtime)
! ******************************************************************************
! gwf_ad -- GroundWater Flow Model Time Step Advance
! Subroutine: (1) calls package advance subroutines
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use SimVariablesModule, only: isimcheck
    ! -- dummy
    class(GwfModelType) :: this
    class(BndType), pointer :: packobj
    integer(I4B), intent(in) :: ipicard
    integer(I4B), intent(in) :: isubtime
    ! -- local
    integer(I4B) :: ip, n
! ------------------------------------------------------------------------------
    !
    ! -- copy x into xold
    do n=1,this%dis%nodes
      this%xold(n)=this%x(n)
    enddo
    !
    ! -- Advance
    if(this%innpf > 0) call this%npf%npf_ad(this%dis%nodes, this%xold)
    if(this%insto > 0) call this%sto%sto_ad()
    if(this%incsub > 0)  call this%csub%csub_ad(this%dis%nodes, this%x)
    if(this%inmvr > 0) call this%mvr%mvr_ad()
    do ip=1,this%bndlist%Count()
      packobj => GetBndFromList(this%bndlist, ip)
      call packobj%bnd_ad()
      if (isimcheck > 0) then
        call packobj%bnd_ck()
      end if
    enddo
    !
    ! -- Push simulated values to preceding time/subtime step
    call this%obs%obs_ad()
    !
    ! -- return
    return
  end subroutine gwf_ad

  subroutine gwf_cf(this, kiter)
! ******************************************************************************
! gwf_cf -- GroundWater Flow Model calculate coefficients
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(GwfModelType) :: this
    integer(I4B),intent(in) :: kiter
    ! -- local
    class(BndType), pointer :: packobj
    integer(I4B) :: ip
! ------------------------------------------------------------------------------
    !
    ! -- Call package cf routines
    if(this%innpf > 0) call this%npf%npf_cf(kiter, this%dis%nodes, this%x)
    do ip = 1, this%bndlist%Count()
      packobj => GetBndFromList(this%bndlist, ip)
      call packobj%bnd_cf()
    enddo
    !
    ! -- return
    return
  end subroutine gwf_cf

  subroutine gwf_fc(this, kiter, amatsln, njasln, inwtflag)
! ******************************************************************************
! gwf_fc -- GroundWater Flow Model fill coefficients
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(GwfModelType) :: this
    integer(I4B), intent(in) :: kiter
    integer(I4B), intent(in) :: njasln
    real(DP), dimension(njasln), intent(inout) :: amatsln
    integer(I4B), intent(in) :: inwtflag
    ! -- local
    class(BndType), pointer :: packobj
    integer(I4B) :: ip
    integer(I4B) :: inwt, inwtsto, inwtcsub, inwtpak
! ------------------------------------------------------------------------------
    !
    ! -- newton flags
    inwt = inwtflag
    if(inwtflag == 1) inwt = this%npf%inewton
    inwtsto = inwtflag
    if(this%insto > 0) then
      if(inwtflag == 1) inwtsto = this%sto%inewton
    endif
    inwtcsub = inwtflag
    if(this%incsub > 0) then
      if(inwtflag == 1) inwtcsub = this%csub%inewton
    endif
    !
    ! -- Fill standard conductance terms
    if(this%innpf > 0) call this%npf%npf_fc(kiter, njasln, amatsln,            &
                                            this%idxglo, this%rhs, this%x)
    if(this%inhfb > 0) call this%hfb%hfb_fc(kiter, njasln, amatsln,            &
                                            this%idxglo, this%rhs, this%x)
    if(this%ingnc > 0) call this%gnc%gnc_fc(kiter, amatsln)
    ! -- storage
    if(this%insto > 0) then
      call this%sto%sto_fc(kiter, this%xold, this%x, njasln, amatsln,          &
                           this%idxglo, this%rhs)
    end if
    ! -- skeletal storage, compaction, and land subsidence 
    if(this%incsub > 0) then
      call this%csub%csub_fc(kiter, this%xold, this%x, njasln, amatsln,        &
                             this%idxglo, this%rhs)
    end if
    if(this%inmvr > 0) call this%mvr%mvr_fc()
    do ip = 1, this%bndlist%Count()
      packobj => GetBndFromList(this%bndlist, ip)
      call packobj%bnd_fc(this%rhs, this%ia, this%idxglo, amatsln)
    enddo
    !
    !--Fill newton terms
    if(this%innpf > 0) then
      if(inwt /= 0) then
        call this%npf%npf_fn(kiter, njasln, amatsln, this%idxglo, this%rhs,    &
                             this%x)
      endif
    endif
    !
    ! -- Fill newton terms for ghost nodes
    if(this%ingnc > 0) then
      if(inwt /= 0) then
        call this%gnc%gnc_fn(kiter, njasln, amatsln, this%npf%condsat,         &
          ivarcv_opt=this%npf%ivarcv,                                          &
          ictm1_opt=this%npf%icelltype,                                        &
          ictm2_opt=this%npf%icelltype)
      endif
    endif
    !
    ! -- Fill newton terms for storage
    if(this%insto > 0) then
      if (inwtsto /= 0) then
        call this%sto%sto_fn(kiter, this%xold, this%x, njasln, amatsln,        &
                             this%idxglo, this%rhs)
      end if
    end if
    !
    ! -- Fill newton terms for skeletal storage, compaction, and land subsidence 
    if(this%incsub > 0) then
      if (inwtcsub /= 0) then
        call this%csub%csub_fn(kiter, this%xold, this%x, njasln, amatsln,      &
                               this%idxglo, this%rhs)
      end if
    end if
    !
    ! -- Fill Newton terms for packages
    do ip = 1, this%bndlist%Count()
      packobj => GetBndFromList(this%bndlist, ip)
      inwtpak = inwtflag
      if(inwtflag == 1) inwtpak = packobj%inewton
      if (inwtpak /= 0) then
        call packobj%bnd_fn(this%rhs, this%ia, this%idxglo, amatsln)
      end if
    enddo
    !
    ! -- return
    return
  end subroutine gwf_fc

  subroutine gwf_cc(this, kiter, iend, icnvg, hclose, rclose)
! ******************************************************************************
! gwf_cc -- GroundWater Flow Model Final Convergence Check for Boundary Packages
! Subroutine: (1) calls package cc routines
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(GwfModelType) :: this
    integer(I4B),intent(in) :: kiter
    integer(I4B),intent(in) :: iend
    integer(I4B),intent(inout) :: icnvg
    real(DP), intent(in) :: hclose
    real(DP), intent(in) :: rclose
    ! -- local
    class(BndType), pointer :: packobj
    integer(I4B) :: ip
    ! -- formats
! ------------------------------------------------------------------------------
    !
    ! -- If mover is on, then at least 2 outers required
    if (this%inmvr > 0) call this%mvr%mvr_cc(kiter, iend, icnvg)
    !
    ! -- csub convergence check
    if (this%incsub > 0) then
      call this%csub%csub_cc(iend, icnvg, this%dis%nodes, this%x, this%xold,     &
                             hclose, rclose)
    end if
    !
    ! -- Call package cc routines
    do ip = 1, this%bndlist%Count()
      packobj => GetBndFromList(this%bndlist, ip)
      call packobj%bnd_cc(iend, icnvg, hclose, rclose)
    enddo
    !
    ! -- return
    return
  end subroutine gwf_cc
  
  subroutine gwf_ptcchk(this, iptc)
! ******************************************************************************
! gwf_ptcchk -- check if pseudo-transient continuation factor should be used
! Subroutine: (1) Check if pseudo-transient continuation factor should be used
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! modules
    ! -- dummy
    class(GwfModelType) :: this
    integer(I4B), intent(inout) :: iptc
! ------------------------------------------------------------------------------
    ! -- determine if pseudo-transient continuation should be applied to this 
    !    model - pseudo-transient continuation only applied to problems that
    !    use the Newton-Raphson formulation during steady-state stress periods
    iptc = 0
    if (this%iss > 0) then
      if (this%inewton > 0) then
        iptc = this%inewton
      else
        iptc = this%npf%inewton
      end if
    end if
    !
    ! -- return
    return
  end subroutine gwf_ptcchk

  subroutine gwf_ptc(this, kiter, neqsln, njasln, ia, ja,                       &
                     x, rhs, amatsln, iptc, ptcf)
! ******************************************************************************
! gwf_ptc -- calculate maximum pseudo-transient continuation factor
! Subroutine: (1) Calculate maximum pseudo-transient continuation factor
!                 for the current outer iteration
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! modules
    use ConstantsModule, only: DONE, DP9
    ! -- dummy
    class(GwfModelType) :: this
    integer(I4B),intent(in) :: kiter
    integer(I4B), intent(in) :: neqsln
    integer(I4B),intent(in) :: njasln
    integer(I4B), dimension(neqsln+1), intent(in) :: ia
    integer(I4B),dimension(njasln),intent(in) :: ja
    real(DP), dimension(neqsln), intent(in) :: x
    real(DP), dimension(neqsln), intent(in) :: rhs
    real(DP),dimension(njasln),intent(in) :: amatsln
    integer(I4B), intent(inout) :: iptc
    real(DP),intent(inout) :: ptcf
    ! -- local
    integer(I4B) :: iptct
    integer(I4B) :: n
    integer(I4B) :: jcol
    integer(I4B) :: j, jj
    real(DP) :: v
    real(DP) :: resid
    real(DP) :: ptcdelem1
    real(DP) :: diag
    real(DP) :: diagcnt
    real(DP) :: diagmin
    real(DP) :: diagmax
! ------------------------------------------------------------------------------
    ! -- set temporary flag indicating if pseudo-transient continuation should
    !    be used for this model and time step
    iptct = 0
    ! -- only apply pseudo-transient continuation to problems using the 
    !    Newton-Raphson formulations for steady-state stress periods
    if (this%iss > 0) then
      if (this%inewton > 0) then
        iptct = this%inewton
      else
        iptct = this%npf%inewton
      end if
    end if
    !
    ! -- calculate pseudo-transient continuation factor for model
    if (iptct > 0) then
      diagmin = DEP20
      diagmax = DZERO
      diagcnt = DZERO
      do n = 1, this%dis%nodes
        if (this%npf%ibound(n) < 1) cycle
        jcol = n + this%moffset
        !
        ! get the maximum volume of the cell (head at top of cell)        
        v = this%dis%get_cell_volume(n, this%dis%top(n))
        !
        ! -- calculate the residual for the cell
        resid = DZERO
        do j = ia(jcol), ia(jcol+1)-1
          jj = ja(j)
          resid = resid + amatsln(j) * x(jcol)
        end do
        resid = resid - rhs(jcol)
        !
        ! -- calculate the reciprocal of the pseudo-time step
        !    resid [L3/T] / volume [L3] = [1/T]
        ptcdelem1 = abs(resid) / v
        !
        ! -- set ptcf if the reciprocal of the pseudo-time step
        !    exceeds the current value (equivalent to using the 
        !    smallest pseudo-time step) 
        if (ptcdelem1 > ptcf) ptcf = ptcdelem1
        !
        ! -- determine minimum and maximum diagonal entries
        j = ia(jcol)
        diag = abs(amatsln(j))
        diagcnt = diagcnt + DONE
        if (diag > DZERO) then
          if (diag < diagmin) diagmin = diag
          if (diag > diagmax) diagmax = diag
        end if
      end do
      !
      ! -- set the reciprocal of the pseudo-time step
      !    to a fraction of the minimum or maximum
      !    diagonal entry to prevent excessively small
      !    or large values
      if (diagcnt > DZERO) then
        diagmin = diagmin * DEM1
        diagmax = diagmax * DEM1
        if (ptcf < diagmin) ptcf = diagmin
        if (ptcf > diagmax) ptcf = diagmax
      end if
    end if

    ! reset ipc if needed
    if (iptc == 0) then
      if (iptct > 0) iptc = 1
    end if
    !
    ! -- return
    return
  end subroutine gwf_ptc

  subroutine gwf_nur(this, neqmod, x, xtemp, dx, inewtonur)
! ******************************************************************************
! gwf_nur -- under-relaxation
! Subroutine: (1) Under-relaxation of Groundwater Flow Model Heads for current
!                 outer iteration using the cell bottoms at the bottom of the
!                 model
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! modules
    use ConstantsModule, only: DONE, DP9
    ! -- dummy
    class(GwfModelType) :: this
    integer(I4B), intent(in) :: neqmod
    real(DP), dimension(neqmod), intent(inout) :: x
    real(DP), dimension(neqmod), intent(in) :: xtemp
    real(DP), dimension(neqmod), intent(inout) :: dx
    integer(I4B), intent(inout) :: inewtonur
    ! -- local
    !integer(I4B) :: n
    !integer(I4B) :: jcol
    !real(DP) :: botm
    integer(I4B) :: i0
    integer(I4B) :: i1
    class(BndType), pointer :: packobj
    integer(I4B) :: ip
! ------------------------------------------------------------------------------
    !
    ! -- apply Newton-Raphson under-relaxation if model is using
    !    the Newton-Raphson formulation and this Newton-Raphson
    !    under-relaxation is turned on.
    if (this%inewton /= 0 .and. this%inewtonur /= 0) then
      if (this%innpf > 0) then
        call this%npf%npf_nur(neqmod, x, xtemp, dx, inewtonur)
      end if
      !
      ! -- Call package nur routines
      i0 = this%dis%nodes + 1
      do ip = 1, this%bndlist%Count()
        packobj => GetBndFromList(this%bndlist, ip)
        if (packobj%npakeq > 0) then
          i1 = i0 + packobj%npakeq - 1
          call packobj%bnd_nur(packobj%npakeq, x(i0:i1), xtemp(i0:i1), &
                               dx(i0:i1), inewtonur)
          i0 = i1 + 1
        end if
      enddo
    end if
    !
    ! -- return
    return
  end subroutine gwf_nur

  subroutine gwf_cq(this, icnvg, isuppress_output)
! ******************************************************************************
! gwf_cq --Groundwater flow model calculate flow
! Subroutine: (1) Calculate intercell flows (flowja)
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(GwfModelType) :: this
    integer(I4B), intent(in) :: icnvg
    integer(I4B), intent(in) :: isuppress_output
    ! -- local
    integer(I4B) :: i
! ------------------------------------------------------------------------------
    !
    ! -- Construct the flowja array.  Flowja is calculated each time, even if
    !    output is suppressed.  (flowja is positive into a cell.)
    do i = 1, this%nja
      this%flowja(i) = DZERO
    enddo
    if(this%innpf > 0) call this%npf%npf_flowja(this%x, this%flowja)
    if(this%inhfb > 0) call this%hfb%hfb_flowja(this%x, this%flowja)
    if(this%ingnc > 0) call this%gnc%flowja(this%flowja)
    !
    ! -- Return
    return
  end subroutine gwf_cq

  subroutine gwf_bd(this, icnvg, isuppress_output)
! ******************************************************************************
! gwf_bd --GroundWater Flow Model Budget
! Subroutine: (1) Calculate stress package contributions to model budget
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(GwfModelType) :: this
    integer(I4B), intent(in) :: icnvg
    integer(I4B), intent(in) :: isuppress_output
    ! -- local
    integer(I4B) :: icbcfl, ibudfl, icbcun, iprobs, idvfl
    integer(I4B) :: ip
    class(BndType),pointer :: packobj
! ------------------------------------------------------------------------------
    !
    ! -- Save the solution convergence flag
    this%icnvg = icnvg
    !
    ! -- Set write and print flags differently if output is suppressed.
    if(isuppress_output == 0) then
      idvfl = 0
      if(this%oc%oc_save('HEAD')) idvfl = 1
      icbcfl = 0
      if(this%oc%oc_save('BUDGET')) icbcfl = 1
      icbcun = this%oc%oc_save_unit('BUDGET')
      ibudfl = 0
      if(this%oc%oc_print('BUDGET')) ibudfl = 1
      iprobs = 1
    else
      icbcfl = 0
      ibudfl = 0
      icbcun = 0
      iprobs = 0
      idvfl  = 0
    endif
    !
    ! -- Budget routines (start by resetting)
    call this%budget%reset()
    !
    ! -- Storage
    if(this%insto > 0) then
      call this%sto%bdcalc(this%dis%nodes, this%x, this%xold,                  &
                           isuppress_output, this%budget)
      call this%sto%bdsav(icbcfl, icbcun)
    endif
    ! -- Skeletal storage, compaction and subsidence
    if (this%incsub > 0) then
      call this%csub%bdcalc(this%dis%nodes, this%x, this%xold,                 &
                            isuppress_output, this%budget)
      call this%csub%bdsav(idvfl, icbcfl, icbcun)
    end if
    !
    ! -- Node Property Flow
    if(this%innpf > 0) then
      call this%npf%npf_bdadj(this%flowja, icbcfl, icbcun)
    endif
    !
    ! -- Clear obs
    call this%obs%obs_bd_clear()
    !
    ! -- Mover budget
    if(this%inmvr > 0) call this%mvr%mvr_bd(icbcfl, ibudfl, isuppress_output)
    !
    ! -- Boundary packages calculate budget and total flows to model budget
    do ip = 1, this%bndlist%Count()
      packobj => GetBndFromList(this%bndlist, ip)
      call packobj%bnd_bd(this%x, idvfl, icbcfl, ibudfl, icbcun, iprobs,       &
                          isuppress_output, this%budget)
    enddo
    !
    ! -- Calculate and write simulated values for observations
    if(iprobs /= 0) then
      if (icnvg > 0) then
        call this%obs%obs_bd()
      endif
    endif
    !
    ! -- Return
    return
  end subroutine gwf_bd

  subroutine gwf_ot(this)
! ******************************************************************************
! gwf_ot -- GroundWater Flow Model Output
! Subroutine: (1) Output budget items
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use TdisModule,only:kstp, kper, endofperiod, tdis_ot
    ! -- dummy
    class(GwfModelType) :: this
    ! -- local
    integer(I4B) :: ipflg, ibudfl, ihedfl
    integer(I4B) :: ip
    class(BndType), pointer :: packobj
    ! -- formats
    character(len=*),parameter :: fmtnocnvg = &
      "(1X,/9X,'****FAILED TO MEET SOLVER CONVERGENCE CRITERIA IN TIME STEP ', &
      &I0,' OF STRESS PERIOD ',I0,'****')"
! ------------------------------------------------------------------------------
    !
    ! -- Set ibudfl flag for printing budget information
    ibudfl = 0
    if(this%oc%oc_print('BUDGET')) ibudfl = 1
    if(this%icnvg == 0) ibudfl = 1
    if(endofperiod) ibudfl = 1
    !
    ! -- Set ibudfl flag for printing dependent variable information
    ihedfl = 0
    if(this%oc%oc_print('HEAD')) ihedfl = 1
    if(this%icnvg == 0) ihedfl = 1
    if(endofperiod) ihedfl = 1
    !
    ! -- Output individual flows if requested
    if(ibudfl /= 0) then
      !
      ! -- NPF output
      if(this%innpf > 0) call this%npf%npf_ot(this%flowja)
      !
      ! -- GNC output
      if(this%ingnc > 0) &
        call this%gnc%gnc_ot()
    endif
    !
    ! -- Output control
    ipflg = 0
    this%budget%budperc = 1.e30
    if(this%icnvg == 0) then
      write(this%iout,fmtnocnvg) kstp, kper
      ipflg = 1
    endif
    call this%oc%oc_ot(ipflg)
    !
    ! -- Write Budget and Head if these conditions are met
    if (ibudfl /= 0 .or. ihedfl /=0) then
      ipflg = 1
      !
      ! -- Package budget output
      do ip = 1, this%bndlist%Count()
        packobj => GetBndFromList(this%bndlist, ip)
        call packobj%bnd_ot(kstp, kper, this%iout, ihedfl, ibudfl)
      enddo
      !
      if (ibudfl /= 0) then
        !
        ! -- Mover budget output
        if(this%inmvr > 0) call this%mvr%mvr_ot()
        !
        ! -- gwf model budget
        call this%budget%budget_ot(kstp, kper, this%iout)
      end if
    end if
    !
    ! -- Timing Output
    if(ipflg == 1) call tdis_ot(this%iout)
    !
    ! -- OBS output
    call this%obs%obs_ot()
    do ip = 1, this%bndlist%Count()
      packobj => GetBndFromList(this%bndlist, ip)
      call packobj%bnd_ot_obs()
    enddo
    !
    ! -- return
    return
  end subroutine gwf_ot

  subroutine gwf_fp(this)
! ******************************************************************************
! gwf_fp -- Final processing
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(GwfModelType) :: this
    ! -- local
! ------------------------------------------------------------------------------
    !
    ! -- csub final processing
    if (this%incsub > 0) then
      call this%csub%csub_fp()
    end if
    !
    return
  end subroutine gwf_fp

  subroutine gwf_da(this)
! ******************************************************************************
! gwf_da -- Deallocate
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryManagerModule, only: mem_deallocate
    ! -- dummy
    class(GwfModelType) :: this
    ! -- local
    integer(I4B) :: ip
    class(BndType),pointer :: packobj
! ------------------------------------------------------------------------------
    !
    ! -- Internal flow packages deallocate
    call this%dis%dis_da()
    call this%ic%ic_da()
    call this%npf%npf_da()
    call this%xt3d%xt3d_da()
    call this%gnc%gnc_da()
    call this%sto%sto_da()
    call this%csub%csub_da()
    call this%budget%budget_da()
    call this%hfb%hfb_da()
    call this%mvr%mvr_da()
    call this%oc%oc_da()
    call this%obs%obs_da()
    !
    ! -- Internal package objects
    deallocate(this%dis)
    deallocate(this%ic)
    deallocate(this%npf)
    deallocate(this%xt3d)
    deallocate(this%gnc)
    deallocate(this%sto)
    deallocate(this%csub)
    deallocate(this%budget)
    deallocate(this%hfb)
    deallocate(this%mvr)
    deallocate(this%obs)
    deallocate(this%oc)
    !
    ! -- Boundary packages
    do ip = 1, this%bndlist%Count()
      packobj => GetBndFromList(this%bndlist, ip)
      call packobj%bnd_da()
      deallocate(packobj)
    enddo
    !
    ! -- Scalars
    call mem_deallocate(this%inic)
    call mem_deallocate(this%inoc)
    call mem_deallocate(this%inobs)
    call mem_deallocate(this%innpf)
    call mem_deallocate(this%insto)
    call mem_deallocate(this%incsub)
    call mem_deallocate(this%inmvr)
    call mem_deallocate(this%inhfb)
    call mem_deallocate(this%ingnc)
    call mem_deallocate(this%iss)
    call mem_deallocate(this%inewtonur)
    !
    ! -- NumericalModelType
    call this%NumericalModelType%model_da()
    !
    ! -- return
    return
  end subroutine gwf_da

  function gwf_get_nsubtimes(this) result(nsubtimes)
! ******************************************************************************
! gwf_get_nsubtimes -- Return number of subtimesteps
! Subtimesteps not implemented yet, so just return 1.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    !
    ! -- result
    integer(I4B) :: nsubtimes
    class(GwfModelType) :: this
! ------------------------------------------------------------------------------
    !
    nsubtimes = 1
    !
    ! -- return
    return
  end function gwf_get_nsubtimes

  subroutine gwf_bdentry(this, budterm, budtxt, rowlabel)
! ******************************************************************************
! gwf_bdentry -- GroundWater Flow Model Budget Entry
! This subroutine adds a budget entry to the flow budget.  It was added as
! a method for the gwf3 model object so that the exchange object could add its
! contributions.
! Subroutine: (1) adds the entry to the budget object
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use ConstantsModule, only: LENBUDTXT, LENPACKAGENAME
    use TdisModule, only:delt
    ! -- dummy
    class(GwfModelType) :: this
    real(DP), dimension(:, :), intent(in) :: budterm
    character(len=LENBUDTXT), dimension(:), intent(in) :: budtxt
    character(len=LENPACKAGENAME), intent(in) :: rowlabel
! ------------------------------------------------------------------------------
    !
    call this%budget%addentry(budterm, delt, budtxt, rowlabel=rowlabel)
    !
    ! -- return
    return
  end subroutine gwf_bdentry

  function gwf_get_iasym(this) result (iasym)
! ******************************************************************************
! gwf_get_iasym -- return 1 if any package causes the matrix to be asymmetric.
!   Otherwise return 0.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    class(GwfModelType) :: this
    ! -- local
    integer(I4B) :: iasym
    integer(I4B) :: ip
    class(BndType), pointer :: packobj
! ------------------------------------------------------------------------------
    !
    ! -- Start by setting iasym to zero
    iasym = 0
    !
    ! -- NPF
    if (this%innpf > 0) then
      if (this%npf%iasym /= 0) iasym = 1
      if (this%npf%ixt3d /= 0) iasym = 1
    endif
    !
    ! -- GNC
    if (this%ingnc > 0) then
      if (this%gnc%iasym /= 0) iasym = 1
    endif
    !
    ! -- Check for any packages that introduce matrix asymmetry
    do ip=1,this%bndlist%Count()
      packobj => GetBndFromList(this%bndlist, ip)
      if (packobj%iasym /= 0) iasym = 1
    enddo
    !
    ! -- return
    return
  end function gwf_get_iasym

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
    class(GwfModelType) :: this
    character(len=*), intent(in)  :: modelname
! ------------------------------------------------------------------------------
    !
    ! -- allocate members from parent class
    call this%NumericalModelType%allocate_scalars(modelname)
    !
    ! -- allocate members that are part of model class
    call mem_allocate(this%inic,  'INIC',  modelname)
    call mem_allocate(this%inoc,  'INOC',  modelname)
    call mem_allocate(this%innpf, 'INNPF', modelname)
    call mem_allocate(this%insto, 'INSTO', modelname)
    call mem_allocate(this%incsub, 'INCSUB', modelname)
    call mem_allocate(this%inmvr, 'INMVR', modelname)
    call mem_allocate(this%inhfb, 'INHFB', modelname)
    call mem_allocate(this%ingnc, 'INGNC', modelname)
    call mem_allocate(this%inobs, 'INOBS', modelname)
    call mem_allocate(this%iss,   'ISS',   modelname)
    call mem_allocate(this%inewtonur, 'INEWTONUR', modelname)
    !
    this%inic = 0
    this%inoc = 0
    this%innpf = 0
    this%insto = 0
    this%incsub = 0
    this%inmvr = 0
    this%inhfb = 0
    this%ingnc = 0
    this%inobs = 0
    this%iss = 1       !default is steady-state (i.e., no STO package)
    this%inewtonur = 0 !default is to not use newton bottom head dampening
    !
    ! -- return
    return
  end subroutine allocate_scalars

  subroutine package_create(this, filtyp, ipakid, ipaknum, pakname, inunit,    &
                            iout)
! ******************************************************************************
! package_create -- Create boundary condition packages for this model
! Subroutine: (1) create new-style package
!             (2) add a pointer to the package
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use ConstantsModule, only: LINELENGTH
    use SimModule, only: store_error, ustop
    use ChdModule, only: chd_create
    use WelModule, only: wel_create
    use DrnModule, only: drn_create
    use RivModule, only: riv_create
    use GhbModule, only: ghb_create
    use RchModule, only: rch_create
    use EvtModule, only: evt_create
    use MawModule, only: maw_create
    use SfrModule, only: sfr_create
    use LakModule, only: lak_create
    use UzfModule, only: uzf_create
    ! -- dummy
    class(GwfModelType) :: this
    character(len=*),intent(in) :: filtyp
    character(len=LINELENGTH) :: errmsg
    integer(I4B),intent(in) :: ipakid
    integer(I4B),intent(in) :: ipaknum
    character(len=*), intent(in) :: pakname
    integer(I4B),intent(in) :: inunit
    integer(I4B),intent(in) :: iout
    ! -- local
    class(BndType), pointer :: packobj
    class(BndType), pointer :: packobj2
    integer(I4B) :: ip
! ------------------------------------------------------------------------------
    !
    ! -- This part creates the package object
    select case(filtyp)
    case('CHD6')
      call chd_create(packobj, ipakid, ipaknum, inunit, iout, this%name, pakname)
    case('WEL6')
      call wel_create(packobj, ipakid, ipaknum, inunit, iout, this%name, pakname)
    case('DRN6')
      call drn_create(packobj, ipakid, ipaknum, inunit, iout, this%name, pakname)
    case('RIV6')
      call riv_create(packobj, ipakid, ipaknum, inunit, iout, this%name, pakname)
    case('GHB6')
      call ghb_create(packobj, ipakid, ipaknum, inunit, iout, this%name, pakname)
    case('RCH6')
      call rch_create(packobj, ipakid, ipaknum, inunit, iout, this%name, pakname)
    case('EVT6')
      call evt_create(packobj, ipakid, ipaknum, inunit, iout, this%name, pakname)
    case('MAW6')
      call maw_create(packobj, ipakid, ipaknum, inunit, iout, this%name, pakname)
    case('SFR6')
      call sfr_create(packobj, ipakid, ipaknum, inunit, iout, this%name, pakname)
    case('LAK6')
      call lak_create(packobj, ipakid, ipaknum, inunit, iout, this%name, pakname)
    case('UZF6')
      call uzf_create(packobj, ipakid, ipaknum, inunit, iout, this%name, pakname)
    case default
      write(errmsg, *) 'Invalid package type: ', filtyp
      call store_error(errmsg)
      call ustop()
    end select
    !
    ! -- Check to make sure that the package name is unique, then store a
    !    pointer to the package in the model bndlist
    do ip = 1, this%bndlist%Count()
      packobj2 => GetBndFromList(this%bndlist, ip)
      if(packobj2%name == pakname) then
        write(errmsg, '(a,a)') 'Cannot create package.  Package name  ' //   &
          'already exists: ', trim(pakname)
        call store_error(errmsg)
        call ustop()
      endif
    enddo
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
    use ConstantsModule,   only: LINELENGTH
    use SimModule,         only: ustop, store_error, count_errors
    use NameFileModule,    only: NameFileType
    ! -- dummy
    class(GwfModelType) :: this
    type(NameFileType), intent(in) :: namefile_obj
    integer(I4B), intent(in) :: indis
    ! -- local
    character(len=LINELENGTH) :: errmsg
    integer(I4B) :: i, iu
    character(len=LENFTYPE), dimension(11) :: nodupftype =                     &
      (/'DIS6 ', 'DISU6', 'DISV6', 'IC6  ', 'OC6  ', 'NPF6 ', 'STO6 ',         &
        'MVR6 ', 'HFB6 ', 'GNC6 ', 'OBS6 '/)
! ------------------------------------------------------------------------------
    !
    if(this%single_model_run) then
      !
      ! -- Ensure TDIS6 is present
      call namefile_obj%get_unitnumber('TDIS6', iu, 1)
      if(iu == 0) then
        call store_error('TDIS6 ftype not specified in name file.')
      endif
      !
      ! -- Ensure IMS6 is present
      call namefile_obj%get_unitnumber('IMS6', iu, 1)
      if(iu == 0) then
        call store_error('IMS6 ftype not specified in name file.')
      endif
      !
    else
      !
      ! -- Warn if TDIS6 is present
      call namefile_obj%get_unitnumber('TDIS6', iu, 1)
      if(iu > 0) then
        write(this%iout, '(/a)') 'Warning TDIS6 detected in GWF name file.'
        write(this%iout, *) 'Simulation TDIS file will be used instead.'
        close(iu)
      endif
      !
      ! -- Warn if SMS8 is present
      call namefile_obj%get_unitnumber('IMS6', iu, 1)
      if(iu > 0) then
        write(this%iout, '(/a)') 'Warning IMS6 detected in GWF name file.'
        write(this%iout, *) 'Simulation IMS6 file will be used instead.'
        close(iu)
      endif
    endif
    !
    ! -- Check for IC8, DIS(u), and NPF. Stop if not present.
    if(this%inic==0) then
      write(errmsg, '(1x,a)') 'ERROR. INITIAL CONDITIONS (IC6) PACKAGE NOT SPECIFIED.'
      call store_error(errmsg)
    endif
    if(indis==0) then
      write(errmsg, '(1x,a)') &
        'ERROR. DISCRETIZATION (DIS6, DISV6, or DISU6) PACKAGE NOT SPECIFIED.'
      call store_error(errmsg)
    endif
    if(this%innpf==0) then
      write(errmsg, '(1x,a)') &
        'ERROR.  NODE PROPERTY FLOW (NPF6) PACKAGE NOT SPECIFIED.'
      call store_error(errmsg)
    endif
    if(count_errors() > 0) then
      write(errmsg,'(1x,a)') 'ERROR. REQUIRED PACKAGE(S) NOT SPECIFIED.'
      call store_error(errmsg)
    endif
    !
    ! -- Check to make sure that some GWF packages are not specified more
    !    than once
    do i = 1, size(nodupftype)
      call namefile_obj%get_unitnumber(trim(nodupftype(i)), iu, 0)
      if (iu > 0) then
        write(errmsg,'(1x, a, a, a)')                                          &
          'DUPLICATE ENTRIES FOR FTYPE ', trim(nodupftype(i)),                 &
          ' NOT ALLOWED FOR GWF MODEL.'
        call store_error(errmsg)
      endif
    enddo
    !
    ! -- Stop if errors
    if(count_errors() > 0) then
      write(errmsg, '(a, a)') 'ERROR OCCURRED WHILE READING FILE: ',           &
        trim(namefile_obj%filename)
      call store_error(errmsg)
      call ustop()
    endif
    !
    ! -- return
    return
  end subroutine ftype_check

end module GwfModule
