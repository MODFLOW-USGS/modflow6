module GwtAdvModule
  
  use KindModule,             only: DP, I4B
  use ConstantsModule,        only: DONE, DZERO, DHALF
  use NumericalPackageModule, only: NumericalPackageType
  use BaseDisModule,          only: DisBaseType
  use GwtFmiModule,           only: GwtFmiType

  implicit none
  private
  public :: GwtAdvType
  public :: adv_cr

  type, extends(NumericalPackageType) :: GwtAdvType
    
    integer(I4B), pointer                            :: iadvwt => null()        ! advection scheme (0 up, 1 central, 2 tvd)
    integer(I4B), dimension(:), pointer              :: ibound => null()        ! pointer to model ibound
    type(GwtFmiType), pointer                        :: fmi => null()           ! pointer to fmi object
    
  contains
  
    procedure :: adv_ar
    procedure :: adv_fc
    procedure :: adv_bd
    
    procedure :: allocate_scalars
    procedure, private :: read_options
    procedure :: adv_weight
    
  end type GwtAdvType
  
  contains

  subroutine adv_cr(advobj, name_model, inunit, iout, fmi)
! ******************************************************************************
! adv_cr -- Create a new ADV object
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    type(GwtAdvType), pointer :: advobj
    character(len=*), intent(in) :: name_model
    integer(I4B), intent(in) :: inunit
    integer(I4B), intent(in) :: iout
    type(GwtFmiType), intent(in), target :: fmi
! ------------------------------------------------------------------------------
    !
    ! -- Create the object
    allocate(advobj)
    !
    ! -- create name and origin
    call advobj%set_names(1, name_model, 'ADV', 'ADV')
    !
    ! -- Allocate scalars
    call advobj%allocate_scalars()
    !
    ! -- Set variables
    advobj%inunit = inunit
    advobj%iout = iout
    advobj%fmi => fmi
    !
    ! -- Initialize block parser
    call advobj%parser%Initialize(advobj%inunit, advobj%iout)
    !
    ! -- Return
    return
  end subroutine adv_cr

  subroutine adv_ar(this, dis, ibound)
! ******************************************************************************
! adv_ar -- Allocate and Read
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(GwtAdvType)                       :: this
    class(DisBaseType), pointer, intent(in) :: dis
    integer(I4B), dimension(:), pointer          :: ibound
    ! -- local
    ! -- formats
    character(len=*), parameter :: fmtadv =                                    &
      "(1x,/1x,'ADV-- ADVECTION PACKAGE, VERSION 1, 8/25/2017',                &
      &' INPUT READ FROM UNIT ', i0, //)"
! ------------------------------------------------------------------------------
    !
    ! --print a message identifying the advection package.
    write(this%iout, fmtadv) this%inunit
    !
    ! -- adv pointers to arguments that were passed in
    this%dis     => dis
    this%ibound  => ibound
    !
    ! -- Allocate arrays (not needed for adv)
    !call this%allocate_arrays(dis%nodes)
    !
    ! -- Read advection options
    call this%read_options()
    !
    ! -- Return
    return
  end subroutine adv_ar

  subroutine adv_fc(this, nodes, amatsln, idxglo)
! ******************************************************************************
! adv_fc -- Calculate coefficients and fill amat and rhs
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(GwtAdvType) :: this
    integer, intent(in) :: nodes
    real(DP), dimension(:), intent(inout) :: amatsln
    integer(I4B), intent(in), dimension(:) :: idxglo
    ! -- local
    integer(I4B) :: n, m, idiag, ipos
    real(DP) :: omega, qnm
! ------------------------------------------------------------------------------
    !
    ! -- Calculate advection terms and add to solution rhs and hcof.  qnm 
    !    is the volumetric flow rate and has dimensions of L^/T.
    do n = 1, nodes
      if(this%ibound(n) == 0) cycle
      idiag = this%dis%con%ia(n)
      do ipos = this%dis%con%ia(n) + 1, this%dis%con%ia(n + 1) - 1
        m = this%dis%con%ja(ipos)
        if(this%ibound(m) == 0) cycle
        qnm = this%fmi%gwfflowja(ipos)
        omega = this%adv_weight(this%iadvwt, ipos, n, m, qnm)
        amatsln(idxglo(ipos)) = amatsln(idxglo(ipos)) + qnm * (DONE - omega)
        amatsln(idxglo(idiag)) = amatsln(idxglo(idiag)) + qnm * omega
      enddo
    enddo
    !
    ! -- Return
    return
  end subroutine adv_fc
  
  subroutine adv_bd(this, cnew, flowja)
! ******************************************************************************
! adv_bd -- Budget
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(GwtAdvType) :: this
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
    nodes = size(cnew)
    do n = 1, nodes
      if(this%ibound(n) == 0) cycle
      idiag = this%dis%con%ia(n)
      do ipos = this%dis%con%ia(n) + 1, this%dis%con%ia(n + 1) - 1
        m = this%dis%con%ja(ipos)
        if(this%ibound(m) == 0) cycle
        qnm = this%fmi%gwfflowja(ipos)
        omega = this%adv_weight(this%iadvwt, ipos, n, m, qnm)
        flowja(ipos) = flowja(ipos) + qnm * omega * cnew(n) +                  &
                                      qnm * (DONE - omega) * cnew(m)
      enddo
    enddo
    !
    ! -- Return
    return
  end subroutine adv_bd
  
  subroutine adv_da(this)
! ******************************************************************************
! adv_da -- Deallocate variables
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryManagerModule, only: mem_deallocate
    ! -- dummy
    class(GwtAdvType) :: this
! ------------------------------------------------------------------------------
    !
    ! -- Deallocate arrays if package was active
    if(this%inunit > 0) then
      call mem_deallocate(this%iadvwt)
      this%ibound => null()
    endif
    !
    ! -- Scalars
    !
    ! -- deallocate parent
    call this%NumericalPackageType%da()
    !
    ! -- Return
    return
  end subroutine adv_da

  subroutine allocate_scalars(this)
! ******************************************************************************
! allocate_scalars
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryManagerModule, only: mem_allocate, mem_setptr
    ! -- dummy
    class(GwtAdvType) :: this
    ! -- local
! ------------------------------------------------------------------------------
    !
    ! -- allocate scalars in NumericalPackageType
    call this%NumericalPackageType%allocate_scalars()
    !
    ! -- Allocate
    call mem_allocate(this%iadvwt, 'IADVWT', this%origin)
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

  subroutine read_options(this)
! ******************************************************************************
! read_options -- Allocate and Read
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use ConstantsModule,   only: LINELENGTH
    use SimModule,         only: ustop, store_error
    ! -- dummy
    class(GwtAdvType) :: this
    ! -- local
    character(len=LINELENGTH) :: errmsg, keyword
    integer(I4B) :: ierr
    logical :: isfound, endOfBlock
    ! -- formats
    character(len=*), parameter :: fmtiadvwt =                                 &
      "(4x,'ADVECTION WEIGHTING SCHEME HAS BEEN SET TO: ', a)"
! ------------------------------------------------------------------------------
    !
    ! -- get options block
    call this%parser%GetBlock('OPTIONS', isfound, ierr, blockRequired=.false.)
    !
    ! -- parse options block if detected
    if (isfound) then
      write(this%iout,'(1x,a)')'PROCESSING ADVECTION OPTIONS'
      do
        call this%parser%GetNextLine(endOfBlock)
        if (endOfBlock) exit
        call this%parser%GetStringCaps(keyword)
        select case (keyword)
          case ('SCHEME')
            call this%parser%GetStringCaps(keyword)
            select case (keyword)
              case('UPSTREAM')
                this%iadvwt = 0
                this%iasym = 1
                write(this%iout, fmtiadvwt) 'UPSTREAM'
              case ('CENTRAL')
                this%iadvwt = 1
                write(this%iout, fmtiadvwt) 'CENTRAL'
              case('TVD')
                this%iadvwt = 2
                write(this%iout, fmtiadvwt) 'TVD'
              case default
                write(errmsg,'(4x, a, a)')                                     &
                  'ERROR. UNKNOWN SCHEME: ', trim(keyword)
                call store_error(errmsg)
                write(errmsg,'(4x, a, a)')                                     &
                  'SCHEME MUST BE "UPSTREAM", "CENTRAL" OR "TVD"'
                call store_error(errmsg)
                call this%parser%StoreErrorUnit()
                call ustop()
            end select
          case default
            write(errmsg,'(4x,a,a)')'****ERROR. UNKNOWN ADVECTION OPTION: ',   &
                                     trim(keyword)
            call store_error(errmsg)
            call ustop()
        end select
      end do
      if (this%iadvwt /= 1) then
        this%iasym = 1
        write(this%iout,'(1x,a)')'SELECTED ADVECTION SCHEME RESULTS IN AN '&
          &'ASYMMETRIC MATRIX.'
      endif
      write(this%iout,'(1x,a)')'END OF ADVECTION OPTIONS'
    end if
    !
    ! -- Return
    return
  end subroutine read_options

  function adv_weight(this, iadvwt, ipos, n, m, qnm) result(omega)
! ******************************************************************************
! adv_weight -- calculate advection weight
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use SimModule, only: ustop  
    ! -- return
    real(DP) :: omega
    ! -- dummy
    class(GwtAdvType) :: this
    integer, intent(in) :: iadvwt
    integer, intent(in) :: ipos
    integer, intent(in) :: n
    integer, intent(in) :: m
    real(DP), intent(in) :: qnm
    ! -- local
    real(DP) :: lnm, lmn
! ------------------------------------------------------------------------------
    if(iadvwt == 1) then
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
      endif
      omega = lmn / (lnm + lmn)
    elseif (iadvwt == 0) then
      ! -- use upstream weighting
      if(qnm > DZERO) then
        omega = DZERO
      else
        omega = DONE
      endif
    else
      ! tvd not implemented          
      !omega = 1./0.
      call ustop()
    endif
    !
    ! -- return
    return
  end function adv_weight
  

  
end module GwtAdvModule