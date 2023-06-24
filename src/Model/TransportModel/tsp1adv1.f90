module TspAdvModule

  use KindModule, only: DP, I4B
  use ConstantsModule, only: DONE, DZERO, DHALF, DTWO
  use NumericalPackageModule, only: NumericalPackageType
  use BaseDisModule, only: DisBaseType
  use TspFmiModule, only: TspFmiType
  use TspAdvOptionsModule, only: TspAdvOptionsType
  use MatrixBaseModule

  implicit none
  private
  public :: TspAdvType
  public :: adv_cr

  type, extends(NumericalPackageType) :: TspAdvType

    integer(I4B), pointer :: iadvwt => null() !< advection scheme (0 up, 1 central, 2 tvd)
    integer(I4B), dimension(:), pointer, contiguous :: ibound => null() !< pointer to model ibound
    type(TspFmiType), pointer :: fmi => null() !< pointer to fmi object
    real(DP), dimension(:), pointer, contiguous :: cpw => null() ! pointer to GWE heat capacity of water
    real(DP), dimension(:), pointer, contiguous :: rhow => null() ! fixed density of water
    real(DP), pointer :: eqnsclfac => null() !< governing equation scale factor; =1. for solute; =rhow*cpw for energy

  contains

    procedure :: adv_df
    procedure :: adv_ar
    procedure :: adv_fc
    procedure :: adv_cq
    procedure :: adv_da

    procedure :: allocate_scalars
    procedure, private :: read_options
    procedure, private :: advqtvd
    procedure, private :: advtvd_bd
    procedure :: adv_weight
    procedure :: advtvd

  end type TspAdvType

contains

  !> @ brief Create a new ADV object
  !!
  !!  Create a new ADV package
  !<
  subroutine adv_cr(advobj, name_model, inunit, iout, fmi, eqnsclfac)
    ! -- dummy
    type(TspAdvType), pointer :: advobj
    character(len=*), intent(in) :: name_model
    integer(I4B), intent(in) :: inunit
    integer(I4B), intent(in) :: iout
    type(TspFmiType), intent(in), target :: fmi
    real(DP), intent(in), pointer :: eqnsclfac !< governing equation scale factor
! ------------------------------------------------------------------------------
    !
    ! -- Create the object
    allocate (advobj)
    !
    ! -- create name and memory path
    call advobj%set_names(1, name_model, 'ADV', 'ADV')
    !
    ! -- Allocate scalars
    call advobj%allocate_scalars()
    !
    ! -- Set variables
    advobj%inunit = inunit
    advobj%iout = iout
    advobj%fmi => fmi
    advobj%eqnsclfac => eqnsclfac
    !
    ! -- Return
    return
  end subroutine adv_cr

  !> @brief Define ADV object
  !!
  !! Define the ADV package
  !<
  subroutine adv_df(this, adv_options)
    ! -- dummy
    class(TspAdvType) :: this
    type(TspAdvOptionsType), optional, intent(in) :: adv_options !< the optional options, for when not constructing from file
    ! -- local
    character(len=*), parameter :: fmtadv = &
      "(1x,/1x,'ADV-- ADVECTION PACKAGE, VERSION 1, 8/25/2017', &
      &' INPUT READ FROM UNIT ', i0, //)"
! ------------------------------------------------------------------------------
    !
    ! -- Read or set advection options
    if (.not. present(adv_options)) then
      !
      ! -- Initialize block parser (adv has no define, so it's
      ! not done until here)
      call this%parser%Initialize(this%inunit, this%iout)
      !
      ! --print a message identifying the advection package.
      write (this%iout, fmtadv) this%inunit
      !
      ! --read options from file
      call this%read_options()
    else
      !
      ! --set options from input arg
      this%iadvwt = adv_options%iAdvScheme
    end if
    !
    ! -- Return
    return
  end subroutine adv_df

  !> @brief Allocate and read method for package
  !!
  !!  Method to allocate and read static data for the ADV package.
  !<
  subroutine adv_ar(this, dis, ibound, cpw, rhow)
    ! -- modules
    ! -- dummy
    class(TspAdvType) :: this
    class(DisBaseType), pointer, intent(in) :: dis
    integer(I4B), dimension(:), pointer, contiguous, intent(in) :: ibound
    real(DP), dimension(:), pointer, contiguous, optional, intent(in) :: cpw
    real(DP), dimension(:), pointer, contiguous, optional, intent(in) :: rhow
    ! -- local
    ! -- formats
! ------------------------------------------------------------------------------
    !
    ! -- adv pointers to arguments that were passed in
    this%dis => dis
    this%ibound => ibound
    !
    ! -- if part of a GWE simulation, need heat capacity(cpw) and density (rhow)
    if (present(cpw)) this%cpw => cpw
    if (present(rhow)) this%rhow => rhow
    !
    ! -- Return
    return
  end subroutine adv_ar

  !> @brief  Fill coefficient method for ADV package
  !!
  !!  Method to calculate coefficients and fill amat and rhs.
  !<
  subroutine adv_fc(this, nodes, matrix_sln, idxglo, cnew, rhs)
    ! -- modules
    ! -- dummy
    class(TspAdvType) :: this
    integer, intent(in) :: nodes
    class(MatrixBaseType), pointer :: matrix_sln
    integer(I4B), intent(in), dimension(:) :: idxglo
    real(DP), intent(in), dimension(:) :: cnew
    real(DP), dimension(:), intent(inout) :: rhs
    ! -- local
    integer(I4B) :: n, m, idiag, ipos
    real(DP) :: omega, qnm
! ------------------------------------------------------------------------------
    !
    ! -- Calculate advection terms and add to solution rhs and hcof.  qnm
    !    is the volumetric flow rate and has dimensions of L^/T.
    do n = 1, nodes
      if (this%ibound(n) == 0) cycle
      idiag = this%dis%con%ia(n)
      do ipos = this%dis%con%ia(n) + 1, this%dis%con%ia(n + 1) - 1
        if (this%dis%con%mask(ipos) == 0) cycle
        m = this%dis%con%ja(ipos)
        if (this%ibound(m) == 0) cycle
!!        qnm = this%fmi%gwfflowja(ipos)
        qnm = this%fmi%gwfflowja(ipos) * this%eqnsclfac
        omega = this%adv_weight(this%iadvwt, ipos, n, m, qnm)
        call matrix_sln%add_value_pos(idxglo(ipos), qnm * (DONE - omega))
        call matrix_sln%add_value_pos(idxglo(idiag), qnm * omega)
      end do
    end do
    !
    ! -- TVD
    if (this%iadvwt == 2) then
      do n = 1, nodes
        if (this%ibound(n) == 0) cycle
        call this%advtvd(n, cnew, rhs)
      end do
    end if
    !
    ! -- Return
    return
  end subroutine adv_fc

  !> @brief  Calculate TVD
  !!
  !! Use explicit scheme to calculate the advective component of transport.
  !! TVD is an acronym for Total-Variation Diminishing
  !<
  subroutine advtvd(this, n, cnew, rhs)
    ! -- modules
    ! -- dummy
    class(TspAdvType) :: this
    integer(I4B), intent(in) :: n
    real(DP), dimension(:), intent(in) :: cnew
    real(DP), dimension(:), intent(inout) :: rhs
    ! -- local
    real(DP) :: qtvd
    integer(I4B) :: m, ipos
! ------------------------------------------------------------------------------
    !
    ! -- Loop through each n connection.  This will
    do ipos = this%dis%con%ia(n) + 1, this%dis%con%ia(n + 1) - 1
      if (this%dis%con%mask(ipos) == 0) cycle
      m = this%dis%con%ja(ipos)
      if (m > n .and. this%ibound(m) /= 0) then
        qtvd = this%advqtvd(n, m, ipos, cnew)
        rhs(n) = rhs(n) - qtvd
        rhs(m) = rhs(m) + qtvd
      end if
    end do
    !
    ! -- Return
    return
  end subroutine advtvd

  !> @brief  Calculate TVD
  !!
  !! Use explicit scheme to calculate the advective component of transport.
  !! TVD is an acronym for Total-Variation Diminishing
  !<
  function advqtvd(this, n, m, iposnm, cnew) result(qtvd)
    ! -- modules
    use ConstantsModule, only: DPREC
    ! -- Return
    real(DP) :: qtvd
    ! -- dummy
    class(TspAdvType) :: this
    integer(I4B), intent(in) :: n
    integer(I4B), intent(in) :: m
    integer(I4B), intent(in) :: iposnm
    real(DP), dimension(:), intent(in) :: cnew
    ! -- local
    integer(I4B) :: ipos, isympos, iup, idn, i2up, j
    real(DP) :: qnm, qmax, qupj, elupdn, elup2up
    real(DP) :: smooth, cdiff, alimiter
! ------------------------------------------------------------------------------
    !
    ! -- intialize
    qtvd = DZERO
    !
    ! -- Find upstream node
    isympos = this%dis%con%jas(iposnm)
    qnm = this%fmi%gwfflowja(iposnm)
    if (qnm > DZERO) then
      ! -- positive flow into n means m is upstream
      iup = m
      idn = n
    else
      iup = n
      idn = m
    end if
    elupdn = this%dis%con%cl1(isympos) + this%dis%con%cl2(isympos)
    !
    ! -- Find second node upstream to iup
    i2up = 0
    qmax = DZERO
    do ipos = this%dis%con%ia(iup) + 1, this%dis%con%ia(iup + 1) - 1
      j = this%dis%con%ja(ipos)
      if (this%ibound(j) == 0) cycle
      qupj = this%fmi%gwfflowja(ipos)
      isympos = this%dis%con%jas(ipos)
      if (qupj > qmax) then
        qmax = qupj
        i2up = j
        elup2up = this%dis%con%cl1(isympos) + this%dis%con%cl2(isympos)
      end if
    end do
    !
    ! -- Calculate flux limiting term
    if (i2up > 0) then
      smooth = DZERO
      cdiff = ABS(cnew(idn) - cnew(iup))
      if (cdiff > DPREC) then
        smooth = (cnew(iup) - cnew(i2up)) / elup2up * &
                 elupdn / (cnew(idn) - cnew(iup))
      end if
      if (smooth > DZERO) then
        alimiter = DTWO * smooth / (DONE + smooth)
        qtvd = DHALF * alimiter * qnm * (cnew(idn) - cnew(iup))
        qtvd = qtvd * this%eqnsclfac
      end if
    end if
    !
    ! -- Return
    return
  end function advqtvd

  !> @brief Calculate advection contribution to flowja
  !<
  subroutine adv_cq(this, cnew, flowja)
    ! -- modules
    ! -- dummy
    class(TspAdvType) :: this
    real(DP), intent(in), dimension(:) :: cnew
    real(DP), intent(inout), dimension(:) :: flowja
    ! -- local
    integer(I4B) :: nodes
    integer(I4B) :: n, m, idiag, ipos
    real(DP) :: omega, qnm
! ------------------------------------------------------------------------------
    !
    ! -- Calculate advection and add to flowja. qnm is the volumetric flow
    !    rate and has dimensions of L^/T.
    nodes = this%dis%nodes
    do n = 1, nodes
      if (this%ibound(n) == 0) cycle
      idiag = this%dis%con%ia(n)
      do ipos = this%dis%con%ia(n) + 1, this%dis%con%ia(n + 1) - 1
        m = this%dis%con%ja(ipos)
        if (this%ibound(m) == 0) cycle
        qnm = this%fmi%gwfflowja(ipos) * this%eqnsclfac
        omega = this%adv_weight(this%iadvwt, ipos, n, m, qnm)
        flowja(ipos) = flowja(ipos) + qnm * omega * cnew(n) + &
                       qnm * (DONE - omega) * cnew(m)
      end do
    end do
    !
    ! -- TVD
    if (this%iadvwt == 2) call this%advtvd_bd(cnew, flowja)
    !
    ! -- Return
    return
  end subroutine adv_cq

  !> @brief Add TVD contribution to flowja
  subroutine advtvd_bd(this, cnew, flowja)
    ! -- modules
    ! -- dummy
    class(TspAdvType) :: this
    real(DP), dimension(:), intent(in) :: cnew
    real(DP), dimension(:), intent(inout) :: flowja
    ! -- local
    real(DP) :: qtvd, qnm
    integer(I4B) :: nodes, n, m, ipos
! ------------------------------------------------------------------------------
    !
    nodes = this%dis%nodes
    do n = 1, nodes
      if (this%ibound(n) == 0) cycle
      do ipos = this%dis%con%ia(n) + 1, this%dis%con%ia(n + 1) - 1
        m = this%dis%con%ja(ipos)
        if (this%ibound(m) /= 0) then
          qnm = this%fmi%gwfflowja(ipos)
          qtvd = this%advqtvd(n, m, ipos, cnew)
          flowja(ipos) = flowja(ipos) + qtvd
        end if
      end do
    end do
    !
    ! -- Return
    return
  end subroutine advtvd_bd

  !> @brief Deallocate memory
  !<
  subroutine adv_da(this)
    ! -- modules
    use MemoryManagerModule, only: mem_deallocate
    ! -- dummy
    class(TspAdvType) :: this
! ------------------------------------------------------------------------------
    !
    ! -- Deallocate arrays if package was active
    if (this%inunit > 0) then
    end if
    !
    ! -- nullify pointers
    this%ibound => null()
    nullify (this%cpw)
    nullify (this%rhow)
    !
    ! -- Scalars
    call mem_deallocate(this%iadvwt)
    !
    ! -- deallocate parent
    call this%NumericalPackageType%da()
    !
    ! -- Return
    return
  end subroutine adv_da

  !> @brief Allocate scalars specific to the streamflow energy transport (SFE)
  !! package.
  !<
  subroutine allocate_scalars(this)
    ! -- modules
    use MemoryManagerModule, only: mem_allocate, mem_setptr
    ! -- dummy
    class(TspAdvType) :: this
    ! -- local
! ------------------------------------------------------------------------------
    !
    ! -- allocate scalars in NumericalPackageType
    call this%NumericalPackageType%allocate_scalars()
    !
    ! -- Allocate
    call mem_allocate(this%iadvwt, 'IADVWT', this%memoryPath)
    !
    ! -- Initialize
    this%iadvwt = 0
    !
    ! -- Advection creates an asymmetric coefficient matrix
    this%iasym = 1
    !
    ! -- Return
    return
  end subroutine allocate_scalars

  !> @brief Read options
  !!
  !! Read the options block
  !<
  subroutine read_options(this)
    ! -- modules
    use ConstantsModule, only: LINELENGTH
    use SimModule, only: store_error
    ! -- dummy
    class(TspAdvType) :: this
    ! -- local
    character(len=LINELENGTH) :: errmsg, keyword
    integer(I4B) :: ierr
    logical :: isfound, endOfBlock
    ! -- formats
    character(len=*), parameter :: fmtiadvwt = &
      &"(4x,'ADVECTION WEIGHTING SCHEME HAS BEEN SET TO: ', a)"
! ------------------------------------------------------------------------------
    !
    ! -- get options block
    call this%parser%GetBlock('OPTIONS', isfound, ierr, blockRequired=.false., &
                              supportOpenClose=.true.)
    !
    ! -- parse options block if detected
    if (isfound) then
      write (this%iout, '(1x,a)') 'PROCESSING ADVECTION OPTIONS'
      do
        call this%parser%GetNextLine(endOfBlock)
        if (endOfBlock) exit
        call this%parser%GetStringCaps(keyword)
        select case (keyword)
        case ('SCHEME')
          call this%parser%GetStringCaps(keyword)
          select case (keyword)
          case ('UPSTREAM')
            this%iadvwt = 0
            write (this%iout, fmtiadvwt) 'UPSTREAM'
          case ('CENTRAL')
            this%iadvwt = 1
            write (this%iout, fmtiadvwt) 'CENTRAL'
          case ('TVD')
            this%iadvwt = 2
            write (this%iout, fmtiadvwt) 'TVD'
          case default
            write (errmsg, '(a, a)') &
              'Unknown scheme: ', trim(keyword)
            call store_error(errmsg)
            write (errmsg, '(a, a)') &
              'Scheme must be "UPSTREAM", "CENTRAL" or "TVD"'
            call store_error(errmsg)
            call this%parser%StoreErrorUnit()
          end select
        case default
          write (errmsg, '(a,a)') 'Unknown ADVECTION option: ', &
            trim(keyword)
          call store_error(errmsg, terminate=.TRUE.)
        end select
      end do
      write (this%iout, '(1x,a)') 'END OF ADVECTION OPTIONS'
    end if
    !
    ! -- Return
    return
  end subroutine read_options

  !> @ brief Advection weight
  !!
  !! Calculate the advection weight
  !<
  function adv_weight(this, iadvwt, ipos, n, m, qnm) result(omega)
    ! -- Return
    real(DP) :: omega
    ! -- dummy
    class(TspAdvType) :: this
    integer, intent(in) :: iadvwt
    integer, intent(in) :: ipos
    integer, intent(in) :: n
    integer, intent(in) :: m
    real(DP), intent(in) :: qnm
    ! -- local
    real(DP) :: lnm, lmn
! ------------------------------------------------------------------------------
    select case (iadvwt)
    case (1)
      ! -- calculate weight based on distances between nodes and the shared
      !    face of the connection
      if (this%dis%con%ihc(this%dis%con%jas(ipos)) == 0) then
        ! -- vertical connection; assume cell is fully saturated
        lnm = DHALF * (this%dis%top(n) - this%dis%bot(n))
        lmn = DHALF * (this%dis%top(m) - this%dis%bot(m))
      else
        ! -- horizontal connection
        lnm = this%dis%con%cl1(this%dis%con%jas(ipos))
        lmn = this%dis%con%cl2(this%dis%con%jas(ipos))
      end if
      omega = lmn / (lnm + lmn)
    case (0, 2)
      ! -- use upstream weighting for upstream and tvd schemes
      if (qnm > DZERO) then
        omega = DZERO
      else
        omega = DONE
      end if
    end select
    !
    ! -- Return
    return
  end function adv_weight

end module TspAdvModule
