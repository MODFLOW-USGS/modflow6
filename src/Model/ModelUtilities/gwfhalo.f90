module GwfHaloModule
  
  use KindModule,                  only: DP, I4B
  use GwfModule,                   only: GwfModelType
  use ConstantsModule,             only: LINELENGTH, DZERO

  implicit none

  private
  public :: gwfhalo_cr
  public :: GwfHaloModelType

  type, extends(GwfModelType) :: GwfHaloModelType
    
    type(GwfModelType), pointer :: gwf1
    type(GwfModelType), pointer :: gwf2
    integer(I4B), dimension(:), pointer :: nodem1
    integer(I4B), dimension(:), pointer :: nodem2
    integer(I4B), dimension(:), pointer :: imapm1tohalo
    integer(I4B), dimension(:), pointer :: imapm2tohalo
    integer(I4B), dimension(:), pointer :: imaphalotom1
    integer(I4B), dimension(:), pointer :: imaphalotom2
    real(DP), dimension(:), pointer :: amat
    
  contains
  
    procedure :: gwfhalo_df
    procedure :: gwfhalo_ar
    procedure :: gwfhalo_cf
    !procedure :: gwfhalo_fc
    procedure :: gwfhalo_fc_calc
    procedure :: gwfhalo_fn_calc
    
  end type GwfHaloModelType
  
  contains

  subroutine gwfhalo_cr(this, id, modelname)
! ******************************************************************************
! gwfhalo_cr -- Create a new groundwater flow model object for exchange
! Subroutine: (1) creates model object and add to exchange modellist
!             (2) assign values
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use GwfDisuModule,              only: disu_cr
    use GwfNpfModule,               only: npf_cr
    use Xt3dModule,                 only: xt3d_cr
    use GhostNodeModule,            only: gnc_cr
    use GwfMvrModule,               only: mvr_cr
    use GwfIcModule,                only: ic_cr
    use GwfObsModule,               only: gwf_obs_cr
    ! -- dummy
    type(GwfHaloModelType), pointer   :: this
    integer(I4B), intent(in)      :: id
    character(len=*), intent(in)  :: modelname
    ! -- local
    integer(I4B), target :: in_dum, iout_dum
    ! -- format
! ------------------------------------------------------------------------------
    !
    ! -- Allocate a new GWF Model (this)
    allocate(this)
    call this%allocate_scalars(modelname)
    !
    ! -- Assign values
    this%name = modelname
    this%macronym = 'GWF'
    this%id = id
    !
    ! -- TODO: to be replaced by optional arguments
    in_dum = -1
    iout_dum = -1
    !
    ! -- Create discretization object
    call disu_cr(this%dis, this%name, in_dum, iout_dum)
    !
    ! -- Create packages that are tied directly to model
    call npf_cr(this%npf, this%name, in_dum, iout_dum)
    call xt3d_cr(this%xt3d, this%name, in_dum, iout_dum)
    call gnc_cr(this%gnc, this%name, in_dum, iout_dum)
    call ic_cr(this%ic, this%name, in_dum, iout_dum, this%dis)
    call mvr_cr(this%mvr, this%name, in_dum, iout_dum)
    call gwf_obs_cr(this%obs, in_dum)
    !
    ! -- return
    return
  end subroutine gwfhalo_cr
  
  subroutine gwfhalo_df(this, gwf1, gwf2, nodem1, nodem2, ihc, cl1, cl2,   &
    hwva, inewton)
! ******************************************************************************
! gwfhalo_df -- define the halo model by creating the ia/ja arrays and copying
!   information from each individual model into the halo model
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use SparseModule, only: sparsematrix
    use ConnectionsModule, only: fillisym, filljas
    ! -- dummy
    class(GwfHaloModelType) :: this
    type(GwfModelType), target :: gwf1
    type(GwfModelType), target :: gwf2
    integer(I4B), dimension(:), intent(in), target :: nodem1
    integer(I4B), dimension(:), intent(in), target :: nodem2
    integer(I4B), dimension(:), intent(in) :: ihc
    real(DP), dimension(:), intent(in) :: cl1
    real(DP), dimension(:), intent(in) :: cl2
    real(DP), dimension(:), intent(in) :: hwva
    integer(I4B), intent(in) :: inewton
    ! -- local
    integer(I4B) :: nexg
    integer(I4B) :: nodesm1, nodesm2
    integer(I4B) :: n, m
    integer(I4B) :: ngwf, mgwf
    integer(I4B) :: ipos, iexg, i
    integer(I4B) :: isym, isymgwf
    integer(I4B) :: ierror
    type(sparsematrix) :: sparse
    integer(I4B), dimension(:), allocatable :: rowmaxnnz
! ------------------------------------------------------------------------------
    !
    ! -- Set newton flag
    this%inewton = inewton
    this%npf%inewton = inewton
    !
    ! -- set pointers
    this%gwf1 => gwf1
    this%gwf2 => gwf2
    this%nodem1 => nodem1
    this%nodem2 => nodem2
    !
    ! -- initialize nexg to size of nodem1 which is also size of nodem2
    nexg = size(nodem1)
    allocate(this%imapm1tohalo(gwf1%neq))  ! todo: memory manager?
    allocate(this%imapm2tohalo(gwf2%neq))  ! todo: memory manager?
    do n = 1, gwf1%neq
      this%imapm1tohalo(n) = 0
    enddo
    do n = 1, gwf2%neq
      this%imapm2tohalo(n) = 0
    enddo
    !
    ! -- Determine the number of m1 nodes in the halo model
    do n = 1, nexg
      ngwf = nodem1(n)
      this%imapm1tohalo(ngwf) = 1
      do ipos = gwf1%dis%con%ia(ngwf) + 1, gwf1%dis%con%ia(ngwf + 1) - 1
        mgwf = gwf1%dis%con%ja(ipos)
        this%imapm1tohalo(mgwf) = 1
      end do
    enddo
    nodesm1 = sum(this%imapm1tohalo)
    !
    ! -- Determine the number of m2 nodes in the halo model
    do n = 1, nexg
      ngwf = nodem2(n)
      this%imapm2tohalo(ngwf) = 1
      do ipos = gwf2%dis%con%ia(ngwf) + 1, gwf2%dis%con%ia(ngwf + 1) - 1
        mgwf = gwf2%dis%con%ja(ipos)
        this%imapm2tohalo(mgwf) = 1
      end do
    enddo
    nodesm2 = sum(this%imapm2tohalo)
    !
    ! -- Assign halo node numbers to mapping arrays
    n = 1
    do ngwf = 1, gwf1%neq
      if (this%imapm1tohalo(ngwf) > 0) then
        this%imapm1tohalo(ngwf) = n
        n = n + 1
      endif
    end do
    do ngwf = 1, gwf2%neq
      if (this%imapm2tohalo(ngwf) > 0) then
        this%imapm2tohalo(ngwf) = n
        n = n + 1
      endif
    end do
    !
    ! -- Determine the size of the halo model and allocate mapping arrays
    this%neq = nodesm1 + nodesm2
    this%dis%nodes = this%neq
    this%dis%nodesuser = this%neq
    !
    ! -- Allocate the arrays of the halo model disu package
    call this%dis%allocate_arrays()
    !
    ! -- Copy the top, bot, and areas to halo
    call copydbltohalo(gwf1%dis%top, this%dis%top, this%imapm1tohalo)
    call copydbltohalo(gwf1%dis%bot, this%dis%bot, this%imapm1tohalo)
    call copydbltohalo(gwf1%dis%area, this%dis%area, this%imapm1tohalo)
    call copydbltohalo(gwf2%dis%top, this%dis%top, this%imapm2tohalo)
    call copydbltohalo(gwf2%dis%bot, this%dis%bot, this%imapm2tohalo)
    call copydbltohalo(gwf2%dis%area, this%dis%area, this%imapm2tohalo)
    !
    ! -- Setup a sparse matrix object in order to determine halo connectivity
    allocate(rowmaxnnz(this%neq))
    do n = 1, this%neq
      rowmaxnnz(n) = 6
    enddo
    call sparse%init(this%neq, this%neq, rowmaxnnz)
    !
    ! -- Diagonals for all halo model cells
    do n = 1, this%neq
      call sparse%addconnection(n, n, 1)
    enddo
    !
    ! -- Connections between gwf1 and gwf2
    do iexg = 1, nexg
      ngwf = nodem1(iexg)
      mgwf = nodem2(iexg)
      n = this%imapm1tohalo(ngwf)
      m = this%imapm2tohalo(mgwf)
      call sparse%addconnection(n, m, 1)
      call sparse%addconnection(m, n, 1)
    enddo
    !
    ! -- Internal connections of model 1
    do iexg = 1, nexg
      ngwf = nodem1(iexg)
      do ipos = gwf1%dis%con%ia(ngwf) + 1, gwf1%dis%con%ia(ngwf + 1) - 1
        mgwf = gwf1%dis%con%ja(ipos)
        n = this%imapm1tohalo(ngwf)
        m = this%imapm1tohalo(mgwf)
        call sparse%addconnection(n, m, 1)
        call sparse%addconnection(m, n, 1)
      end do
    enddo
    !
    ! -- Internal connections of model 2
    do iexg = 1, nexg
      ngwf = nodem2(iexg)
      do ipos = gwf2%dis%con%ia(ngwf) + 1, gwf2%dis%con%ia(ngwf + 1) - 1
        mgwf = gwf2%dis%con%ja(ipos)
        n = this%imapm2tohalo(ngwf)
        m = this%imapm2tohalo(mgwf)
        call sparse%addconnection(n, m, 1)
        call sparse%addconnection(m, n, 1)
      end do
    enddo
    !
    allocate(this%dis%con)
    call this%dis%con%allocate_scalars(this%name)
    this%dis%con%nodes = this%neq
    this%dis%con%nja = sparse%nnz
    this%dis%con%njas = (this%dis%con%nja - this%dis%con%nodes) / 2
    this%dis%njas = this%dis%con%njas
    !
    ! -- Allocate index arrays of size nja and symmetric arrays
    call this%dis%con%allocate_arrays()
    !
    ! -- Fill the IA and JA arrays from sparse, then destroy sparse
    call sparse%filliaja(this%dis%con%ia, this%dis%con%ja, ierror)
    call sparse%destroy()
    !
    ! -- Create the isym and jas arrays
    call fillisym(this%dis%con%nodes, this%dis%con%nja,                        &
      this%dis%con%ia, this%dis%con%ja, this%dis%con%isym)
    call filljas(this%dis%con%nodes, this%dis%con%nja,                         &
      this%dis%con%ia, this%dis%con%ja, this%dis%con%isym,                     &
      this%dis%con%jas)    
    !
    ! -- Fill the ihc, cl1, cl2, and hwva arrays in the halo model
    do iexg = 1, nexg
      ngwf = nodem1(iexg)
      do ipos = gwf1%dis%con%ia(ngwf) + 1, gwf1%dis%con%ia(ngwf + 1) - 1
        mgwf = gwf1%dis%con%ja(ipos)
        n = this%imapm1tohalo(ngwf)
        m = this%imapm1tohalo(mgwf)
        i = this%dis%con%getjaindex(n, m)
        isym = this%dis%con%jas(i)
        isymgwf = gwf1%dis%con%jas(ipos)
        this%dis%con%ihc(isym) = gwf1%dis%con%ihc(isymgwf)
        this%dis%con%cl1(isym) = gwf1%dis%con%cl1(isymgwf)
        this%dis%con%cl2(isym) = gwf1%dis%con%cl2(isymgwf)
        this%dis%con%hwva(isym) = gwf1%dis%con%hwva(isymgwf)
      end do
    enddo
    do iexg = 1, nexg
      ngwf = nodem2(iexg)
      do ipos = gwf2%dis%con%ia(ngwf) + 1, gwf2%dis%con%ia(ngwf + 1) - 1
        mgwf = gwf2%dis%con%ja(ipos)
        n = this%imapm2tohalo(ngwf)
        m = this%imapm2tohalo(mgwf)
        i = this%dis%con%getjaindex(n, m)
        isym = this%dis%con%jas(i)
        isymgwf = gwf2%dis%con%jas(ipos)
        this%dis%con%ihc(isym) = gwf2%dis%con%ihc(isymgwf)
        this%dis%con%cl1(isym) = gwf2%dis%con%cl1(isymgwf)
        this%dis%con%cl2(isym) = gwf2%dis%con%cl2(isymgwf)
        this%dis%con%hwva(isym) = gwf2%dis%con%hwva(isymgwf)
      end do
    enddo
    do iexg = 1, nexg
      n = this%imapm1tohalo(nodem1(iexg))
      m = this%imapm2tohalo(nodem2(iexg))
      i = this%dis%con%getjaindex(n, m)
      isym = this%dis%con%jas(i)
      this%dis%con%ihc(isym) = ihc(iexg)
      this%dis%con%cl1(isym) = cl1(iexg)
      this%dis%con%cl2(isym) = cl2(iexg)
      this%dis%con%hwva(isym) = hwva(iexg)
    enddo
    !
    ! -- Finish setting up the halo model
    this%nja = this%dis%con%nja
    call this%allocate_arrays()
    allocate(this%amat(this%nja))
    !
    ! -- Store information needed for observations
    call this%obs%obs_df(this%iout, this%name, 'GWFH', this%dis)
    !
    ! -- return
    return
  end subroutine gwfhalo_df
  
  subroutine gwfhalo_ar(this)
! ******************************************************************************
! gwfhalo_ar -- allocate in the halo model, and copy the package information
!   from gwf1 and gwf2 into the halo model
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryManagerModule, only: mem_allocate, mem_reallocate
    ! -- dummy
    class(GwfHaloModelType) :: this
    ! -- local
    integer(I4B) :: n
! ------------------------------------------------------------------------------
    !
    ! -- Create ibound and x arrays for halo model
    call mem_allocate(this%ibound, this%neq, 'IBOUND', this%name)
    call mem_allocate(this%x, this%neq, 'X', this%name)
    !
    ! -- Copy ibound from gwf models into this halo model
    call copyinttohalo(this%gwf1%ibound, this%ibound, this%imapm1tohalo)
    call copyinttohalo(this%gwf2%ibound, this%ibound, this%imapm2tohalo)
    !
    ! -- set up ic package and copy strt from gwf models into halo model
    call this%ic%allocate_arrays(this%dis%nodes)
    call copydbltohalo(this%gwf1%ic%strt, this%ic%strt, this%imapm1tohalo)
    call copydbltohalo(this%gwf2%ic%strt, this%ic%strt, this%imapm2tohalo)
    do n = 1, this%dis%nodes
      this%x(n) = this%ic%strt(n)
    enddo
    !
    ! -- Copy or point npf pointers
    this%npf%dis => this%dis
    this%npf%ic  => this%ic
    this%npf%ibound  => this%ibound
    this%npf%hnew    => this%x
    !
    ! -- allocate NPF arrays
    call this%npf%allocate_arrays(this%dis%nodes, this%dis%njas)
    !
    ! -- Copy required NPF arrays into halo model
    call copyinttohalo(this%gwf1%npf%icelltype, this%npf%icelltype, this%imapm1tohalo)
    call copyinttohalo(this%gwf2%npf%icelltype, this%npf%icelltype, this%imapm2tohalo)
    call copydbltohalo(this%gwf1%npf%k11, this%npf%k11, this%imapm1tohalo)
    call copydbltohalo(this%gwf2%npf%k11, this%npf%k11, this%imapm2tohalo)
    call copydbltohalo(this%gwf1%npf%sat, this%npf%sat, this%imapm1tohalo)
    call copydbltohalo(this%gwf2%npf%sat, this%npf%sat, this%imapm2tohalo)
    !
    ! -- Copy k22
    if (this%gwf1%npf%ik22 /= 0 .or. this%gwf2%npf%ik22 /= 0) then
      this%npf%ik22 = 1
      call mem_reallocate(this%npf%k22, this%dis%nodes, 'K22', trim(this%npf%origin))
      if (this%gwf1%npf%ik22 /= 0) then
        call copydbltohalo(this%gwf1%npf%k22, this%npf%k22, this%imapm1tohalo)
      else
        call copydbltohalo(this%gwf1%npf%k11, this%npf%k22, this%imapm1tohalo)
      endif
      if (this%gwf2%npf%ik22 /= 0) then
        call copydbltohalo(this%gwf2%npf%k22, this%npf%k22, this%imapm2tohalo)
      else
        call copydbltohalo(this%gwf2%npf%k11, this%npf%k22, this%imapm2tohalo)
      endif
    endif
    !
    ! -- Copy k33
    if (this%gwf1%npf%ik33 /= 0 .or. this%gwf2%npf%ik33 /= 0) then
      this%npf%ik33 = 1
      call mem_reallocate(this%npf%k33, this%dis%nodes, 'K33', trim(this%npf%origin))
      if (this%gwf1%npf%ik33 /= 0) then
        call copydbltohalo(this%gwf1%npf%k33, this%npf%k33, this%imapm1tohalo)
      else
        call copydbltohalo(this%gwf1%npf%k11, this%npf%k33, this%imapm1tohalo)
      endif
      if (this%gwf2%npf%ik22 /= 0) then
        call copydbltohalo(this%gwf2%npf%k33, this%npf%k33, this%imapm2tohalo)
      else
        call copydbltohalo(this%gwf2%npf%k11, this%npf%k33, this%imapm2tohalo)
      endif
    endif
    !
    ! -- Copy angle1
    if (this%gwf1%npf%iangle1 /= 0 .or. this%gwf2%npf%iangle1 /= 0) then
      this%npf%iangle1 = 1
      call mem_reallocate(this%npf%angle1, this%dis%nodes, 'ANGLE1', trim(this%npf%origin))
      do n = 1, this%dis%nodes
        this%npf%angle1(n) = DZERO
      end do
      if (this%gwf1%npf%iangle1 /= 0) then
        call copydbltohalo(this%gwf1%npf%angle1, this%npf%angle1, this%imapm1tohalo)
      endif
      if (this%gwf2%npf%iangle1 /= 0) then
        call copydbltohalo(this%gwf2%npf%angle1, this%npf%angle1, this%imapm2tohalo)
      endif
    endif
    !
    ! -- Copy angle2
    if (this%gwf1%npf%iangle2 /= 0 .or. this%gwf2%npf%iangle2 /= 0) then
      this%npf%iangle2 = 1
      call mem_reallocate(this%npf%angle2, this%dis%nodes, 'ANGLE2', trim(this%npf%origin))
      do n = 1, this%dis%nodes
        this%npf%angle2(n) = DZERO
      end do
      if (this%gwf1%npf%iangle2 /= 0) then
        call copydbltohalo(this%gwf1%npf%angle2, this%npf%angle2, this%imapm1tohalo)
      endif
      if (this%gwf2%npf%iangle2 /= 0) then
        call copydbltohalo(this%gwf2%npf%angle2, this%npf%angle2, this%imapm2tohalo)
      endif
    endif
    !
    ! -- Copy angle3
    if (this%gwf1%npf%iangle3 /= 0 .or. this%gwf2%npf%iangle3 /= 0) then
      this%npf%iangle3 = 1
      call mem_reallocate(this%npf%angle3, this%dis%nodes, 'ANGLE3', trim(this%npf%origin))
      do n = 1, this%dis%nodes
        this%npf%angle3(n) = DZERO
      end do
      if (this%gwf1%npf%iangle3 /= 0) then
        call copydbltohalo(this%gwf1%npf%angle3, this%npf%angle3, this%imapm1tohalo)
      endif
      if (this%gwf2%npf%iangle3 /= 0) then
        call copydbltohalo(this%gwf2%npf%angle3, this%npf%angle3, this%imapm2tohalo)
      endif
    endif
    !
    ! -- prepcheck
    call this%npf%prepcheck()
    !
    ! -- return
    return
  end subroutine gwfhalo_ar

  subroutine gwfhalo_cf(this, kiter)
! ******************************************************************************
! gwfhalo_cf -- update heads in halo model and make rewetting calculations
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(GwfHaloModelType) :: this
    integer(I4B), intent(in) :: kiter
    ! -- local
! ------------------------------------------------------------------------------
    !
    ! -- Copy x from gwf models into this halo model x
    call copydbltohalo(this%gwf1%x, this%x, this%imapm1tohalo)
    call copydbltohalo(this%gwf2%x, this%x, this%imapm2tohalo)
    call copyinttohalo(this%gwf1%ibound, this%ibound, this%imapm1tohalo)
    call copyinttohalo(this%gwf2%ibound, this%ibound, this%imapm2tohalo)
    !
    ! -- rewet
    ! -- TODO: this is a problem.  Wetting and drying prints messages, which
    !    doesn't make sense for the halo model.  And the halo model should 
    !    only check for wetting for the n-m connection, and not recheck for
    !    all the cells in the halo model.  This is not good.
    ! call this%npf%npf_cf(kiter, this%dis%nodes, this%x)
    !
    ! -- Calculate saturation for convertible cells
    call this%npf%sat_calc(this%x)
    !
    ! -- return
    return
  end subroutine gwfhalo_cf

!  subroutine gwfhalo_fc(this, kiter, cond)
!! ******************************************************************************
!! gwfhalo_fc -- fill halo model conductance terms into separate amat and rhs 
!!   vectors so that the gwf-gwf exchange can add them to the solution amat
!! ******************************************************************************
!!
!!    SPECIFICATIONS:
!! ------------------------------------------------------------------------------
!    ! -- modules
!    use SimModule, only: ustop, store_error, count_errors
!    ! -- dummy
!    class(GwfHaloModelType) :: this
!    integer(I4B), intent(in) :: kiter
!    real(DP), dimension(:), intent(inout) :: cond
!    ! -- local
!    character(len=LINELENGTH) :: errmsg
!    integer(I4B) :: n
!    integer(I4B) :: m
!    integer(I4B) :: nexg
!    integer(I4B) :: ipos
!    real(DP), dimension(:), allocatable :: rhs
!    integer(I4B), dimension(:), allocatable :: idxglo
!! ------------------------------------------------------------------------------
!    !
!    ! -- allocate
!    allocate(rhs(this%dis%nodes))
!    allocate(idxglo(this%nja))
!    !
!    ! -- Copy x from models into this halo model x
!    call copydbltohalo(this%gwf1%x, this%x, this%imapm1tohalo)
!    call copydbltohalo(this%gwf2%x, this%x, this%imapm2tohalo)
!    !
!    ! -- Fill halo amat so that the exchange object can add the terms
!    do n = 1, this%nja
!      this%amat(n) = DZERO
!      idxglo(n) = n
!    enddo
!    do n = 1, this%dis%nodes
!      rhs(n) = DZERO
!    enddo
!    call this%npf%npf_fc(kiter, this%dis%nodes, this%nja, this%nja, this%amat, &
!      idxglo, rhs, this%x)
!    !
!    ! -- TODO: for now just compare halo cond with gwf-gwf cond
!    do nexg = 1, size(this%nodem1)
!      n = this%imapm1tohalo(this%nodem1(nexg))
!      m = this%imapm2tohalo(this%nodem2(nexg))
!      ipos = this%dis%con%getjaindex(n, m)
!      if (this%amat(ipos) - cond(nexg) /= DZERO) then
!        write(errmsg, '(a, 1x, i0, 1x, i0, 2(1pg25.16))') 'HALO MODEL COND /= COND', n, m, &
!          this%amat(ipos), cond(nexg)
!        call store_error(errmsg)
!      endif
!    enddo
!    if (count_errors() > 0) call ustop() 
!    !
!    ! -- return
!    return
!  end subroutine gwfhalo_fc
  
  subroutine gwfhalo_fc_calc(this, ngwf, mgwf, terms)
! ******************************************************************************
! gwfhalo_fc_calc -- calculate the amat and rhs terms for connected gwf cells
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(GwfHaloModelType) :: this
    integer(I4B), intent(in) :: ngwf
    integer(I4B), intent(in) :: mgwf
    real(DP), dimension(:, :), intent(inout) :: terms
    ! -- local
    integer(I4B) :: n
    integer(I4B) :: m
    integer(I4B) :: ii
! ------------------------------------------------------------------------------
    n = this%imapm1tohalo(ngwf)
    m = this%imapm2tohalo(mgwf)
    ii = this%dis%con%getjaindex(n, m)
    call this%npf%npf_fc_calc(n, m, ii, this%x, terms)
    !
    ! -- return
    return
  end subroutine gwfhalo_fc_calc

  subroutine gwfhalo_fn_calc(this, ngwf, mgwf, terms)
! ******************************************************************************
! gwfhalo_fn_calc -- calculate the newton amat and rhs terms for connected gwf cells
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(GwfHaloModelType) :: this
    integer(I4B), intent(in) :: ngwf
    integer(I4B), intent(in) :: mgwf
    real(DP), dimension(:, :), intent(inout) :: terms
    ! -- local
    integer(I4B) :: n
    integer(I4B) :: m
    integer(I4B) :: ii
! ------------------------------------------------------------------------------
    n = this%imapm1tohalo(ngwf)
    m = this%imapm2tohalo(mgwf)
    ii = this%dis%con%getjaindex(n, m)
    call this%npf%npf_fn_calc(n, m, ii, this%x, terms)
    !
    ! -- return
    return
  end subroutine gwfhalo_fn_calc

  subroutine copydbltohalo(gwfarray, gwfhaloarray, imaptohalo)
! ******************************************************************************
! copydbltohalo -- allocate in the halo model 
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    real(DP), dimension(:), intent(in) :: gwfarray
    real(DP), dimension(:), intent(inout) :: gwfhaloarray
    integer(I4B), dimension(:), intent(in) :: imaptohalo
    ! -- integer
    integer(I4B) :: ngwf, n
! ------------------------------------------------------------------------------
    do ngwf = 1, size(gwfarray)
      n = imaptohalo(ngwf)
      if (n > 0) then
        gwfhaloarray(n) = gwfarray(ngwf)
      endif
    enddo
    !
    ! -- return
    return
  end subroutine copydbltohalo

  subroutine copyinttohalo(gwfarray, gwfhaloarray, imaptohalo)
! ******************************************************************************
! copyinttohalo -- allocate in the halo model 
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    integer(I4B), dimension(:), intent(in) :: gwfarray
    integer(I4B), dimension(:), intent(inout) :: gwfhaloarray
    integer(I4B), dimension(:), intent(in) :: imaptohalo
    ! -- integer
    integer(I4B) :: ngwf, n
! ------------------------------------------------------------------------------
    do ngwf = 1, size(gwfarray)
      n = imaptohalo(ngwf)
      if (n > 0) then
        gwfhaloarray(n) = gwfarray(ngwf)
      endif
    enddo
    !
    ! -- return
    return
  end subroutine copyinttohalo
        
end module GwfHaloModule