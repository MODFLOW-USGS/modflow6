module VKDModule
  use KindModule,                 only: DP, I4B
  use ConstantsModule,            only: DZERO, DEM9, DEM8, DEM7, DEM6, DEM2,    &
                                        DHALF, DP9, DONE, DLNLOW, DLNHIGH,      &
                                        DHNOFLO, DHDRY, DEM10, LENORIGIN
  use SmoothingModule,            only: sQuadraticSaturation, sPChip_set_derivatives, sPChip_integrate, &
                                        sQuadraticSaturationDerivative
  use BaseDisModule,              only: DisBaseType
  use NumericalPackageModule,     only: NumericalPackageType
  use BlockParserModule,          only: BlockParserType

  implicit none

  public :: VKDType

  type, extends(NumericalPackageType) :: VKDType
!!$    integer(I4B), pointer                   :: inunit     => null()
!!$    integer(I4B), pointer                   :: iout       => null()
!!$    character(len=LENORIGIN), pointer       :: origin     => null()   !origin name of this package (e.g. 'GWF_1 NPF')
    integer(I4B), pointer, dimension(:)     :: ibound     => null()   !pointer to model ibound
    integer(I4B), pointer                   :: ivkd        => null()    !vkd flag (0 is off, 1 is lhs, 2 is rhs)
!!$    class(DisBaseType), pointer             :: dis        => null()   !discretization object
!!$    class(BlockParserType), pointer             :: parser     => null()   !discretization object
    real(DP), dimension(:,:), pointer               :: kk           => null()   ! k knots
    real(DP), dimension(:,:), pointer               :: ek           => null()   ! elevation knots
    real(DP), dimension(:), pointer                 :: pt           => null()   ! tmp pointer
    integer(I4B), pointer                           :: ikk          => null()   ! flag that kk is specified
    integer(I4B), pointer                           :: iek          => null()   ! flag that ek is specified
    
    ! pointers to npf variables
    real(DP),dimension(:),pointer           :: k11        => null()   !horizontal hydraulic conductivity
    real(DP),dimension(:),pointer           :: k22        => null()   !minor axis of horizontal hydraulic conductivity ellipse
    real(DP),dimension(:),pointer           :: k33        => null()   !vertical hydraulic conductivity
    integer(I4B), pointer                   :: ik22      => null()    !flag indicates K22 was read
    integer(I4B), pointer                   :: ik33      => null()    !flag indicates K33 was read
    real(DP), dimension(:), pointer         :: sat        => null()   !saturation (0. to 1.) for each cell
!!$    integer(I4B), pointer                   :: inewton    => null()   !Newton flag
    real(DP), pointer                       :: min_satthk => null()   !min saturated thickness
    real(DP), pointer                       :: satomega => null()
    integer(I4B), dimension(:), pointer     :: icelltype  => null()   !cell type (confined or unconfined)
  contains
!!$    procedure :: vkd_ac
    procedure :: vkd_ar
    procedure :: vkd_hcond
!!$    procedure :: read_vkd
!!$    procedure :: read_kk
!!$    procedure :: read_ek
    procedure :: vkd_satThk
    procedure :: allocate_scalars
    procedure, private :: allocate_arrays
  end type VKDType
  
  
!
! -- Mathematical core of the VKD method.
!
contains
!

    subroutine vkd_cr(vkdobj, name_model, inunit, iout)
! ******************************************************************************
! vkd_cr -- Create a new vkd object
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    type(VKDType), pointer :: vkdobj
    character(len=*), intent(in) :: name_model
    integer(I4B), intent(in) :: inunit
    integer(I4B), intent(in) :: iout
! ------------------------------------------------------------------------------
    !
    ! -- Create the object
    allocate(vkdobj)
    !
    ! -- Allocate scalars
    call vkdobj%allocate_scalars()
    !
    ! -- Set variables
    vkdobj%inunit = inunit
    vkdobj%iout   = iout
    !
    ! -- Return
    return
    end subroutine vkd_cr

  subroutine vkd_ar(this, dis, ibound, k11, ik33, k33, sat, ik22, k22,        &
    inewton, min_satthk, icelltype, satomega)
! ******************************************************************************
! vkd_ar -- Allocate and Read
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use SimModule, only: store_error, ustop
    ! -- dummy
    class(VKDType) :: this
    class(DisBaseType),pointer,intent(inout) :: dis
    integer(I4B), pointer, dimension(:), intent(inout) :: ibound
    real(DP), dimension(:), intent(in), pointer :: k11
    integer(I4B), intent(in), pointer :: ik33
    real(DP), dimension(:), intent(in), pointer :: k33
    real(DP), dimension(:), intent(in), pointer :: sat
    integer(I4B), intent(in), pointer :: ik22
    real(DP), dimension(:), intent(in), pointer :: k22
    integer(I4B), intent(in), pointer :: inewton
    real(DP), intent(in), pointer :: min_satthk
    integer(I4B), dimension(:), intent(in), pointer :: icelltype
    real(DP), intent(in), pointer              :: satomega
    
    ! -- local
    ! -- formats
    character(len=*), parameter :: fmtheader =                                 &
      "(1x, /1x, ' VKD is active.'//)"
    ! -- data
! ------------------------------------------------------------------------------
    !
    ! -- Print a message identifying the xt3d module.
    write(this%iout, fmtheader)
    !
    ! -- Store pointers to arguments that were passed in
    this%dis     => dis
    this%ibound  => ibound
    this%k11 => k11
    this%ik33 => ik33
    this%k33 => k33
    this%sat => sat
    this%ik22 => ik22
    this%k22 => k22
    this%inewton => inewton
    this%min_satthk => min_satthk
    this%icelltype => icelltype
    this%satomega => satomega
    
    ! hmmm
    ! -- Initialize block parser
    call this%parser%Initialize(this%inunit, this%iout)
    
    !
    ! -- allocate arrays
    call this%allocate_arrays(this%dis%nodes)
    !
    ! -- Return
    return
  end subroutine vkd_ar
  
!!$  subroutine read_kk(this, line, lloc, istart, istop, name)
!!$! ******************************************************************************
!!$! read_ek -- read the EK npf data block
!!$! ******************************************************************************
!!$!
!!$!    SPECIFICATIONS:
!!$! ------------------------------------------------------------------------------
!!$    ! -- modules
!!$    use MemoryManagerModule, only: mem_reallocate, mem_allocate
!!$    use ConstantsModule,     only: LINELENGTH
!!$    ! -- dummy
!!$    class(VKDType) :: this
!!$    character(len=LINELENGTH), intent(in) :: line
!!$    integer(I4B), intent(in) :: istart, istop, lloc
!!$    character(len=24), intent(in) :: name
!!$    ! -- local
!!$    this%iek = this%iek + 1
!!$    call mem_reallocate(this%kk, this%dis%nodes, this%iek, trim(adjustl(name)), &
!!$        trim(this%origin))
!!$    this%pt => this%kk(:, this%iek)
!!$    call this%dis%read_grid_array(line, lloc, istart, istop, this%iout, &
!!$        this%parser%iuactive, this%pt, name)
!!$
!!$
!!$    this%ikk = this%ikk + 1
!!$    call mem_reallocate(this%kk, this%dis%nodes, this%ikk, 'KK', &
!!$        trim(this%origin))
!!$    this%pt => this%kk(:, this%ikk)
!!$    call this%dis%read_grid_array(line, lloc, istart, istop, this%iout, &
!!$        this%parser%iuactive, this%pt,name)
!!$
!!$
!!$    
!!$  end subroutine read_kk
!!$
!!$  subroutine read_ek(this, line, lloc, istart, istop, name)
!!$! ******************************************************************************
!!$! read_ek -- read the EK npf data block
!!$! ******************************************************************************
!!$!
!!$!    SPECIFICATIONS:
!!$! ------------------------------------------------------------------------------
!!$    ! -- modules
!!$    use MemoryManagerModule, only: mem_reallocate, mem_allocate
!!$    use ConstantsModule,     only: LINELENGTH
!!$    ! -- dummy
!!$    class(VKDType) :: this
!!$    character(len=LINELENGTH), intent(in) :: line
!!$    integer(I4B), intent(in) :: istart, istop, lloc
!!$    character(len=24), intent(in) :: name
!!$    ! -- local
!!$    this%iek = this%iek + 1
!!$    call mem_reallocate(this%ek, this%dis%nodes, this%iek, trim(adjustl(name)), &
!!$        trim(this%origin))
!!$    this%pt => this%ek(:, this%iek)
!!$    call this%dis%read_grid_array(line, lloc, istart, istop, this%iout, &
!!$        this%parser%iuactive, this%pt, name)
!!$      
!!$  end subroutine read_ek

  
  function vkd_hcond(this, n, m, cl1, cl2, width, csat, hn, hm, inwtup) result(cond)
! ******************************************************************************
! hcond -- Horizontal conductance between two cells
!   inwtup: if 1, then upstream-weight condsat, otherwise recalculate
!
! hcond function uses a weighted transmissivity in the harmonic mean 
! conductance calculations. This differs from the MODFLOW-NWT and MODFLOW-USG 
! conductance calculations for the Newton-Raphson formulation which use a 
! weighted hydraulic conductivity.  
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use SmoothingModule, only: sPChip_set_derivatives, sPChip_integrate, sQuadraticSaturation
    use ConstantsModule, only: DONE, DZERO
    ! -- return
    real(DP) :: cond
    ! -- dummy
    class(VKDType), intent(inout) :: this
    integer(I4B), intent(in) :: n, m, inwtup
    real(DP), intent(in) :: width, cl1, cl2, csat, hn, hm
    ! -- local
    real(DP) :: t1, t2, sn, sm, tsn, tsm
    real(DP), allocatable :: d(:)


    ! anisotropy hyeff ***************
    if(this%ibound(n) == 0 .or. this%ibound(m) == 0) then
      cond = DZERO
    else
      
!!$      allocate(d(size(this%ek(n,:))))
!!$      call sPChip_set_derivatives (size(this%ek(n,:)),this%ek(n,:), this%kk(n,:), d)
!!$      t1 = sPChip_integrate (size(this%ek(n,:)), this%ek(n,:), this%kk(n,:), d, this%dis%bot(n),hn)
!!$      t2 = sPChip_integrate (size(this%ek(m,:)), this%ek(m,:), this%kk(m,:), d, this%dis%bot(m),hm)
!!$      tsn = sPChip_integrate (size(this%ek(n,:)), this%ek(n,:), this%kk(n,:), d, this%dis%bot(n),this%dis%top(n))
!!$      tsm = sPChip_integrate (Size(this%ek(m,:)), this%ek(m,:), this%kk(m,:), d, this%dis%bot(m),this%dis%top(m))              
!!$      deallocate(d)

      allocate(d(size(this%ek(n,:))))
      call sPChip_set_derivatives (size(this%ek(n,:)),this%ek(n,:), this%kk(n,:), d)
      t1 = sPChip_integrate (size(this%ek(n,:)), this%ek(n,:), this%kk(n,:), d, this%dis%bot(n),hn)
      tsn = sPChip_integrate (size(this%ek(n,:)), this%ek(n,:), this%kk(n,:), d, this%dis%bot(n),this%dis%top(n))
      deallocate(d)

      allocate(d(size(this%ek(m,:))))
      call sPChip_set_derivatives (size(this%ek(m,:)),this%ek(m,:), this%kk(m,:), d)
      t2 = sPChip_integrate (size(this%ek(m,:)), this%ek(m,:), this%kk(m,:), d, this%dis%bot(m),hm)
      tsm = sPChip_integrate (Size(this%ek(m,:)), this%ek(m,:), this%kk(m,:), d, this%dis%bot(m),this%dis%top(m))              
      deallocate(d)
      
      if (t1*t2 > DZERO) then
        sn = sQuadraticSaturation(tsn, DZERO, t1, this%satomega)
        sm = sQuadraticSaturation(tsm, DZERO, t2, this%satomega)
        if (hn > hm) then
          cond = sn
        else
          cond = sm
        end if
        if(inwtup == 1) then
          cond = cond * csat
        else
          cond = width * t1 * t2 / (t1 * cl2 + t2 * cl1)
        endif
      else
        cond = DZERO
      end if
    endif


  end function vkd_hcond

  function vkd_satThk(this, n, hn) result(satThk)
! ******************************************************************************
! hcond -- Horizontal conductance between two cells
!   inwtup: if 1, then upstream-weight condsat, otherwise recalculate
!
! hcond function uses a weighted transmissivity in the harmonic mean 
! conductance calculations. This differs from the MODFLOW-NWT and MODFLOW-USG 
! conductance calculations for the Newton-Raphson formulation which use a 
! weighted hydraulic conductivity.  
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use SmoothingModule, only: sPChip_set_derivatives, sPChip_integrate, sQuadraticSaturation
    use ConstantsModule, only: DZERO
    ! -- return
    real(DP) :: satThk
    ! -- dummy
    class(VKDType), intent(inout) :: this
    integer(I4B), intent(in) :: n
    real(DP), intent(in) :: hn
    ! -- local
    real(DP) :: th, ts
    real(DP), allocatable :: d(:)

    ! T at h / T at top
    allocate(d(size(this%ek(n,:))))
    call sPChip_set_derivatives(size(this%ek(n,:)), this%ek(n,:), this%kk(n,:), d)
    ts = sPChip_integrate(size(this%ek(n,:)), this%ek(n,:), this%kk(n,:), d, this%dis%bot(n), this%dis%top(n))
    th = sPChip_integrate(size(this%ek(n,:)), this%ek(n,:), this%kk(n,:), d, this%dis%bot(n), hn)      
    deallocate(d)

    ! -- Newton-Raphson Formulation
    if(this%inewton /= 0) then
      satThk = sQuadraticSaturation(ts, DZERO, th, this%satomega)
    else
      satThk = th/ts
    endif
    
  end function vkd_satThk

  subroutine allocate_scalars(this)  
! ******************************************************************************
! allocate_scalars -- Allocate scalar pointer variables
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryManagerModule, only: mem_allocate
    ! -- dummy
    class(VKDType) :: this
! ------------------------------------------------------------------------------
    !
    ! -- Assign origin
    this%origin = 'VKD'
    !
    ! -- Allocate scalars
    call mem_allocate(this%ivkd, 'IVKD', this%origin) !wittw
    call mem_allocate(this%ikk, 'IKK', this%origin) !wittw
    call mem_allocate(this%iek, 'IEK', this%origin) !wittw
    call mem_allocate(this%inunit, 'INUNIT', this%origin)
    call mem_allocate(this%iout, 'IOUT', this%origin)
!!$
!!$    call mem_allocate(this%ixt3d, 'IXT3D', this%origin)
!!$    call mem_allocate(this%nbrmax, 'NBRMAX', this%origin)
!!$
!!$    call mem_allocate(this%numextnbrs, 'NUMEXTNBRS', this%origin)
!!$    call mem_allocate(this%nozee, 'NOZEE', this%origin)
!!$    call mem_allocate(this%vcthresh, 'VCTHRESH', this%origin)
!!$    call mem_allocate(this%lamatsaved, 'LAMATSAVED', this%origin)
    !  
    ! -- Initialize value
  
!!$    this%inunit = 0
!!$    this%iout = 0
    this%ivkd = 0
    this%ikk = 0 !wittw
    this%iek = 0 !wittw
    !
    ! -- Return
    return
  end subroutine allocate_scalars

  subroutine allocate_arrays(this, ncells)
! ******************************************************************************
! allocate_scalars -- Allocate npf arrays
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryManagerModule, only: mem_allocate
    ! -- dummy
    class(VKDtype) :: this
    integer(I4B), intent(in) :: ncells
! ------------------------------------------------------------------------------
    !wittw
    call mem_allocate(this%kk, 0, 0, 'KK', trim(this%origin))
    call mem_allocate(this%ek, 0, 0, 'EK', trim(this%origin))
    call mem_allocate(this%pt, ncells, 'TMP', trim(this%origin))
    !
    ! -- Return
    return
  end subroutine allocate_arrays

end module VKDModule
