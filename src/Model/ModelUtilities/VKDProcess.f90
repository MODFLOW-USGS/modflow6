module VKDModule
  use KindModule,                 only: DP, I4B
  use ConstantsModule,            only: DZERO, DEM9, DEM8, DEM7, DEM6, DEM2,    &
                                        DHALF, DP9, DONE, DLNLOW, DLNHIGH,      &
                                        DHNOFLO, DHDRY, DEM10, LENORIGIN,       &
                                        LINELENGTH
  use SmoothingModule,            only: sQuadraticSaturation, sPChip_set_derivatives, sPChip_integrate, &
                                        sQuadraticSaturationDerivative
  use BaseDisModule,              only: DisBaseType
  use NumericalPackageModule,     only: NumericalPackageType
  use BlockParserModule,          only: BlockParserType

  implicit none

  public :: VKDType

  type, extends(NumericalPackageType) :: VKDType
    integer(I4B), pointer, dimension(:)             :: ibound       => null()   !pointer to model ibound
    integer(I4B), pointer, contiguous, dimension(:) :: ibvkd        => null()   !pointer to model vkd ibound
    integer(I4B), pointer                           :: ivkd         => null()   ! vkd flag (0 is off, 1 is on)
    integer(I4B), pointer                           :: implicit     => null()   ! exclude implicit nodes for testing
    integer(I4B), pointer                           :: numvkd       => null()   ! number of cells with VKD active
    integer(I4B), pointer                           :: numelevs     => null()   ! number of VKD knots
    integer(I4B), pointer                           :: inUnitVkd    => null()   ! vkd input file unit number
    real(DP), dimension(:,:), pointer, contiguous               :: kk           => null()   ! k knots
    real(DP), dimension(:,:), pointer, contiguous               :: ek           => null()   ! elevation knots
    real(DP), dimension(:), pointer, contiguous                 :: pt           => null()   ! tmp pointer
    integer(I4B), pointer                           :: ikk          => null()   ! flag that kk is specified
    integer(I4B), pointer                           :: iek          => null()   ! flag that ek is specified
    
    ! pointers to npf variables
    real(DP),dimension(:),pointer           :: k11        => null()   !horizontal hydraulic conductivity
    real(DP),dimension(:),pointer           :: k22        => null()   !minor axis of horizontal hydraulic conductivity ellipse
    real(DP),dimension(:),pointer           :: k33        => null()   !vertical hydraulic conductivity
    integer(I4B), pointer                   :: ik22      => null()    !flag indicates K22 was read
    integer(I4B), pointer                   :: ik33      => null()    !flag indicates K33 was read
    real(DP), dimension(:), pointer         :: sat        => null()   !saturation (0. to 1.) for each cell
    real(DP), pointer                       :: min_satthk => null()   !min saturated thickness
    real(DP), pointer                       :: satomega => null()
    integer(I4B), dimension(:), pointer     :: icelltype  => null()   !cell type (confined or unconfined)
    type (VKDCellType), dimension(:), pointer, contiguous :: cells => null()                    ! VKD cell data
  contains
    procedure :: vkd_ar
    procedure :: vkd_hcond
    procedure :: read_data
    procedure :: vkd_satThk
    procedure :: allocate_scalars
    procedure, private :: allocate_arrays
    procedure, private :: read_dimensions
    procedure, private :: read_options
  end type VKDType

  type :: VKDCellType
    integer(I4B), pointer :: igwfnode => null()
    real(DP), dimension(:), pointer, contiguous :: ek => null()
    real(DP), dimension(:), pointer, contiguous :: kk => null()
  end type VKDCellType
  
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
    vkdobj%implicit = 0
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
    ! -- Print a message identifying the VKD module.
    !
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
    ! -- read vkd options
    call this%read_options()
    !
    ! -- read vkd dimensions
    call this%read_dimensions()
    !
    ! -- allocate arrays
    call this%allocate_arrays(this%dis%nodes)
    !
    ! -- allocate space for vkd cell data
    !
    allocate(this%cells(this%numvkd))
    ! -- put new read in here
    call this%read_data()
    !
    ! -- Return
    return
  end subroutine vkd_ar
!
  subroutine read_options(this)
! ******************************************************************************
! read_options -- read a gnc options block
! Subroutine: (1) read options from input file
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use SimModule, only: ustop, store_error
    ! -- dummy
    class(VKDType) :: this
    ! -- local
    character(len=LINELENGTH) :: errmsg, keyword
    integer(I4B) :: ierr
    logical :: isfound, endOfBlock
! ------------------------------------------------------------------------------
    !
    ! -- get options block
    call this%parser%GetBlock('OPTIONS', isfound, ierr, blockRequired=.false.)
    !
    ! -- parse options block if detected
    if (isfound) then
      write(this%iout,'(1x,a)')'PROCESSING VKD OPTIONS'
      do
        call this%parser%GetNextLine(endOfBlock)
        if (endOfBlock) exit
        call this%parser%GetStringCaps(keyword)
        select case (keyword)
          case ('PRINT_INPUT')
            this%iprpak = 1
            write(this%iout,'(4x,a)') &
                'THE LIST OF VKD CORRECTIONS WILL BE PRINTED.'
          case ('EXCLUDE_IMPLICIT')
            this%implicit = 1
            write(this%iout,'(4x,a)') &
              'IMPLICIT KNOTS EXCLUDED - DEVELOPMENT OPTION ONLY'
          case default
            write(errmsg,'(4x,a,a)')'****ERROR. UNKNOWN VKD OPTION: ', &
                                     trim(keyword)
            call store_error(errmsg)
            call this%parser%StoreErrorUnit()
            call ustop()
        end select
      end do
      write(this%iout,'(1x,a)')'END OF VKD OPTIONS'
    end if
    !
    ! -- return
    return
  end subroutine read_options
!
! 
  subroutine read_dimensions(this)
! ******************************************************************************
! read_dimensions -- Single Model GNC Read Dimensions
! Subroutine: (1) read dimensions (size of gnc list) from input file
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use SimModule, only: ustop, store_error
    ! -- dummy
    class(VKDType) :: this
    ! -- local
    character(len=LINELENGTH) :: errmsg, keyword
    integer(I4B) :: ierr
    logical :: isfound, endOfBlock
! ------------------------------------------------------------------------------
    !
    ! -- get options block
    call this%parser%GetBlock('DIMENSIONS', isfound, ierr)
    !
    ! -- parse options block if detected
    if (isfound) then
      write(this%iout,'(1x,a)')'PROCESSING VKD DIMENSIONS'
      do
        call this%parser%GetNextLine(endOfBlock)
        if (endOfBlock) exit
        call this%parser%GetStringCaps(keyword)
        select case (keyword)
          case ('NUMVKD')
            this%numvkd = this%parser%GetInteger()
            write(this%iout,'(4x,a,i7)')'NUMVKD = ', this%numvkd
          case ('NUMELEVS')
            ! N.B. plus two for implicit knots at top and bottom of layer
            this%numelevs = this%parser%GetInteger()
            ! allow option to not have implicit nodes (just for development)
            if(this%implicit .eq. 0) this%numelevs = this%numelevs + 2
            write(this%iout,'(4x,a,i7)')'NUMELEVS = ', this%numelevs
          case default
            write(errmsg,'(4x,a,a)')'****ERROR. UNKNOWN VKD DIMENSION: ', &
                                     trim(keyword)
            call store_error(errmsg)
            call this%parser%StoreErrorUnit()
            call ustop()
        end select
      end do
      write(this%iout,'(1x,a)')'END OF VKD DIMENSIONS'
    else
      call store_error('ERROR.  REQUIRED DIMENSIONS BLOCK NOT FOUND.')
      call ustop()
    end if
    !
    ! -- return
    return
  end subroutine read_dimensions


  
  subroutine read_data(this)
! ******************************************************************************
! read_data -- read the vkd data block
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use ConstantsModule,   only: LINELENGTH, DONE, DPIO180
    use MemoryManagerModule, only: mem_reallocate
    use SimModule,         only: ustop, store_error, count_errors
    ! -- dummy
    class(VKDType) :: this
    ! -- local
    character(len=LINELENGTH) :: line, errmsg, cellid, keyword
    integer(I4B) :: n, istart, istop, lloc, ierr, nerr, i
    logical :: isfound, endOfBlock
    logical, dimension(10)           :: lname
    character(len=24), dimension(10) :: aname
    integer(I4B), pointer                         :: index           => null()   ! tmp pointer
    ! -- formats
    ! -- formats
    character(len=*), parameter :: fmtiprflow =                                &
      "(4x,'CELL-BY-CELL FLOW INFORMATION WILL BE PRINTED TO LISTING FILE " // &
      "WHENEVER ICBCFL IS NOT ZERO.')"
    character(len=*), parameter :: fmtisvflow =                                &
      "(4x,'CELL-BY-CELL FLOW INFORMATION WILL BE SAVED TO BINARY FILE " //    &
      "WHENEVER ICBCFL IS NOT ZERO.')"
    character(len=*), parameter :: fmtnct =                                    &
      "(1x, 'Negative cell thickness at cell: ', a)"
    character(len=*), parameter :: fmtkerr =                                   &
      "(1x, 'Hydraulic property ',a,' is <= 0 for cell ',a, ' ', 1pg15.6)"
    character(len=*), parameter :: fmtkerr2 =                                  &
        "(1x, '... ', i0,' additional errors not shown for ',a)"
!
! ------------------------------------------------------------------------------
    !
    ! -- Initialize
    lname(:) = .false.

    n = 1
    allocate(index)
    !
    ! -- get npfdata block
    call this%parser%GetBlock('PACKAGEDATA', isfound, ierr)
    if(isfound) then
      write(this%iout,'(1x,a)')'PROCESSING PACKAGEDATA'
      do
        call this%parser%GetNextLine(endOfBlock)
        if (endOfBlock) exit

        ! -- get model node number
        call this%parser%GetCellid(this%dis%ndim, cellid, flag_string=.true.)
        allocate(this%cells(n)%igwfnode)
        this%cells(n)%igwfnode = this%dis%noder_from_cellid(cellid, &
            this%inunit, this%iout, flag_string=.true.)
        ! set cell vkd active (use as node => n dict)
        this%ibvkd(this%cells(n)%igwfnode) = n
        allocate(this%cells(n)%kk(this%numelevs))
        allocate(this%cells(n)%ek(this%numelevs))
        !
        ! -- get ek & kk
        if(this%implicit .eq. 0) then
          do i = 2, this%numelevs - 1
            this%cells(n)%ek(i) = this%parser%GetDouble()
          enddo
          do i = 2, this%numelevs - 1
            this%cells(n)%kk(i) = this%parser%GetDouble()
          enddo
          !
          ! add implicit knots at top and bottom of layer
          this%cells(n)%ek(1) = this%dis%bot(this%cells(n)%igwfnode)
          this%cells(n)%ek(this%numelevs) = this%dis%top(this%cells(n)%igwfnode)
          this%cells(n)%kk(1) = this%cells(n)%kk(2)
          this%cells(n)%kk(this%numelevs) = this%cells(n)%kk(this%numelevs - 1)
        else
          ! no implicit nodes development option
          do i = 1, this%numelevs
            this%cells(n)%ek(i) = this%parser%GetDouble()
          enddo
          do i = 1, this%numelevs
            this%cells(n)%kk(i) = this%parser%GetDouble()
          enddo
        endif
        !
        if (this%iprpak /= 0) then
          write (this%iout,'(A8,20F10.3)') cellid, this%cells(n)%ek(:), this%cells(n)%kk(:)
        end if
        ! increment cell counter
        n = n + 1
      end do
    else
      write(errmsg,'(1x,a)')'ERROR.  REQUIRED GRIDDATA BLOCK NOT FOUND.'
      call store_error(errmsg)
      call this%parser%StoreErrorUnit()
      call ustop()
    end if
    !
    ! -- Final NPFDATA message
    write(this%iout,'(1x,a)')'END PROCESSING GRIDDATA'
    !
    ! -- Return
    return
  end subroutine read_data

    
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
    integer(I4B) :: posn, posm ! position of node in vkd%cells
    !
    ! anisotropy hyeff ***************
    if(this%ibound(n) == 0 .or. this%ibound(m) == 0) then
      cond = DZERO
    else

      posn = this%ibvkd(n)
      posm = this%ibvkd(m)
      !
      allocate(d(this%numelevs))
      call sPChip_set_derivatives (this%numelevs, this%cells(posn)%ek, this%cells(posn)%kk, d)
      t1 = sPChip_integrate (this%numelevs, this%cells(posn)%ek, this%cells(posn)%kk, d, this%dis%bot(n), hn)
      tsn = sPChip_integrate (this%numelevs, this%cells(posn)%ek, this%cells(posn)%kk, d, this%dis%bot(n), this%dis%top(n))
      deallocate(d)

      allocate(d(this%numelevs))
      call sPChip_set_derivatives (this%numelevs, this%cells(posm)%ek, this%cells(posm)%kk, d)
      t2 = sPChip_integrate (this%numelevs, this%cells(posm)%ek, this%cells(posm)%kk, d, this%dis%bot(m), hm)
      tsm = sPChip_integrate (this%numelevs, this%cells(posm)%ek, this%cells(posm)%kk, d, this%dis%bot(m), this%dis%top(m))              
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
! satThk -- Saturated thickness of cell
!
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
    integer(I4B) :: posn, posm ! position of node in vkd%cells
    !
    posn = this%ibvkd(n)
    !
    ! T at h / T at top
    allocate(d(this%numelevs))
    !
    call sPChip_set_derivatives(this%numelevs, this%cells(posn)%ek, this%cells(posn)%kk, d)
    ts = sPChip_integrate(this%numelevs, this%cells(posn)%ek, this%cells(posn)%kk, d, &
        this%dis%bot(n), this%dis%top(n))
    th = sPChip_integrate(this%numelevs, this%cells(posn)%ek, this%cells(posn)%kk, d, &
        this%dis%bot(n), hn)      
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

    call mem_allocate(this%ivkd, 'IVKD', this%origin)
    call mem_allocate(this%inUnitVkd, 'INUNITVKD', this%origin)
    call mem_allocate(this%numvkd, 'NUMVKD', this%origin)
    call mem_allocate(this%numelevs, 'NUMELEVS', this%origin)
    call mem_allocate(this%ikk, 'IKK', this%origin) !wittw
    call mem_allocate(this%iek, 'IEK', this%origin) !wittw
    call mem_allocate(this%inunit, 'INUNIT', this%origin)
    call mem_allocate(this%iout, 'IOUT', this%origin)
    call mem_allocate(this%iprpak, 'IPRPAK', this%origin)
    call mem_allocate(this%implicit, 'IMPLICIT', this%origin)
    !  
    ! -- Initialize value
    !
    this%ivkd = 0
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
    allocate(this%cells(this%numvkd))
    call mem_allocate(this%ibvkd, ncells, 'IBVKD', trim(this%origin))  
    !
    ! -- Return
    return
  end subroutine allocate_arrays

end module VKDModule
