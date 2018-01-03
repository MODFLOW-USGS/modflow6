module PackageMoverModule
  
  use KindModule,          only: DP, I4B
  use ConstantsModule,     only: LENORIGIN, DZERO
  use MemoryManagerModule, only: mem_allocate, mem_reallocate, mem_setptr,     &
                                 mem_deallocate

  implicit none
  private
  public :: PackageMoverType
  
  type PackageMoverType
    
    character(len=LENORIGIN)         :: origin
    integer, pointer                 :: nproviders
    integer, pointer                 :: nreceivers
    real(DP), pointer, dimension(:)  :: qtformvr      => null()
    real(DP), pointer, dimension(:)  :: qformvr       => null()
    real(DP), pointer, dimension(:)  :: qtomvr        => null()
    real(DP), pointer, dimension(:)  :: qfrommvr      => null()

  contains
    procedure :: ar
    procedure :: ad
    procedure :: cf
    procedure :: fc
    procedure :: da
    procedure :: allocate_scalars
    procedure :: allocate_arrays
    procedure :: get_qfrommvr
    procedure :: get_qtomvr
    procedure :: accumulate_qformvr
    
  end type PackageMoverType
  
  contains

  subroutine ar(this, nproviders, nreceivers, origin)
    class(PackageMoverType) :: this
    integer, intent(in) :: nproviders
    integer, intent(in) :: nreceivers
    character(len=*), intent(in) :: origin
    !
    call this%allocate_scalars(origin)
    this%nproviders = nproviders
    this%nreceivers = nreceivers
    !
    call this%allocate_arrays()
    !
    ! -- return
    return
  end subroutine ar  
  
  subroutine ad(this)
    class(PackageMoverType) :: this
    integer :: i
    !
    ! -- set qtomvr and qformvr to zero
    do i = 1, this%nproviders
      this%qtomvr(i) = DZERO
      this%qformvr(i) = DZERO
    enddo
    !
    ! -- return
    return
  end subroutine ad
  
  subroutine cf(this)
    class(PackageMoverType) :: this
    integer :: i
    !
    ! -- set frommvr and qtomvr to zero
    do i = 1, this%nreceivers
      this%qfrommvr(i) = DZERO
    enddo
    do i = 1, this%nproviders
      this%qtomvr(i) = DZERO
      this%qtformvr(i) = this%qformvr(i)
    enddo
    !
    ! -- return
    return
  end subroutine cf
  
  subroutine fc(this)
    class(PackageMoverType) :: this
    integer :: i
    !
    ! -- set formvr to zero
    do i = 1, this%nproviders
      this%qformvr(i) = DZERO
    enddo
    !
    ! -- return
    return
  end subroutine fc
  
  subroutine da(this)
    class(PackageMoverType) :: this
    !
    ! -- arrays
    call mem_deallocate(this%qtformvr)
    call mem_deallocate(this%qformvr)
    call mem_deallocate(this%qtomvr)
    call mem_deallocate(this%qfrommvr)
    !
    ! -- scalars
    call mem_deallocate(this%nproviders)
    call mem_deallocate(this%nreceivers)
    !
    ! -- return
    return
  end subroutine da
  
  subroutine allocate_scalars(this, origin)
    class(PackageMoverType) :: this
    character(len=*), intent(in) :: origin
    !
    call mem_allocate(this%nproviders, 'NPROVIDERS', origin)
    call mem_allocate(this%nreceivers, 'NRECEIVERS', origin)
    !
    this%nproviders = 0
    this%nreceivers = 0
    this%origin = origin
    !
    ! -- return
    return
  end subroutine allocate_scalars  
  
  subroutine allocate_arrays(this)
    class(PackageMoverType) :: this
    integer(I4B) :: i
    !
    call mem_allocate(this%qtformvr, this%nproviders, 'QTFORMVR', this%origin)
    call mem_allocate(this%qformvr, this%nproviders, 'QFORMVR', this%origin)
    call mem_allocate(this%qtomvr, this%nproviders, 'QTOMVR', this%origin)
    call mem_allocate(this%qfrommvr, this%nreceivers, 'QFROMMVR', this%origin)
    !
    ! -- initialize
    do i = 1, this%nproviders
      this%qtformvr(i) = DZERO
      this%qformvr(i) = DZERO
      this%qtomvr(i) = DZERO
    enddo
    do i = 1, this%nreceivers
      this%qfrommvr(i) = DZERO
    enddo
    !
    ! -- return
    return
  end subroutine allocate_arrays
  
  function get_qfrommvr(this, ireceiver) result(qfrommvr)
    class(PackageMoverType) :: this
    real(DP) :: qfrommvr
    integer, intent(in) :: ireceiver
    qfrommvr = this%qfrommvr(ireceiver)
  end function get_qfrommvr
  
  function get_qtomvr(this, iprovider) result(qtomvr)
    class(PackageMoverType) :: this
    real(DP) :: qtomvr
    integer, intent(in) :: iprovider
    qtomvr = this%qtomvr(iprovider)
  end function get_qtomvr

  subroutine accumulate_qformvr(this, iprovider, qformvr)
    class(PackageMoverType) :: this
    integer, intent(in) :: iprovider
    real(DP), intent(in) :: qformvr
    this%qformvr(iprovider) = this%qformvr(iprovider) + qformvr
  end subroutine accumulate_qformvr

end module PackageMoverModule