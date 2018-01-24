module GwtDspModule

  use KindModule,             only: DP, I4B
  use ConstantsModule,        only: DONE, DZERO, DHALF
  use NumericalPackageModule, only: NumericalPackageType
  use BaseDisModule,          only: DisBaseType
  use GwtFmiModule,           only: GwtFmiType

  implicit none
  private
  public :: GwtDspType
  public :: dsp_cr

  type, extends(NumericalPackageType) :: GwtDspType
    
    integer(I4B), dimension(:), pointer              :: ibound => null()        ! pointer to model ibound
    type(GwtFmiType), pointer                        :: fmi => null()           ! pointer to fmi object
    real(DP), dimension(:), pointer                  :: porosity   => null()    ! pointer to storage porosity
    real(DP), dimension(:), pointer                  :: diffc      => null()    ! molecular diffusion coefficient for each cell
    
  contains
  
    procedure :: dsp_ar
    procedure :: dsp_fc
    procedure :: allocate_arrays
    procedure, private :: read_options
    procedure, private :: read_data
   
  end type GwtDspType
  
  contains
  
  subroutine dsp_cr(dspobj, name_model, inunit, iout, fmi)
! ******************************************************************************
! dsp_cr -- Create a new DSP object
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    type(GwtDspType), pointer :: dspobj
    character(len=*), intent(in) :: name_model
    integer(I4B), intent(in) :: inunit
    integer(I4B), intent(in) :: iout
    type(GwtFmiType), intent(in), target :: fmi
! ------------------------------------------------------------------------------
    !
    ! -- Create the object
    allocate(dspobj)
    !
    ! -- create name and origin
    call dspobj%set_names(1, name_model, 'DSP', 'DSP')
    !
    ! -- Allocate scalars
    call dspobj%allocate_scalars()
    !
    ! -- Set variables
    dspobj%inunit = inunit
    dspobj%iout = iout
    dspobj%fmi => fmi
    !
    ! -- Initialize block parser
    call dspobj%parser%Initialize(dspobj%inunit, dspobj%iout)
    !
    ! -- Return
    return
  end subroutine dsp_cr

  subroutine dsp_ar(this, dis, ibound, porosity)
! ******************************************************************************
! dsp_ar -- Allocate and Read
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(GwtDspType)                       :: this
    class(DisBaseType), pointer, intent(in) :: dis
    integer(I4B), dimension(:), pointer          :: ibound
    real(DP), dimension(:), pointer :: porosity
    ! -- local
    ! -- formats
    character(len=*), parameter :: fmtdsp =                                    &
      "(1x,/1x,'ADV-- DISPERSION PACKAGE, VERSION 1, 1/24/2018',               &
      &' INPUT READ FROM UNIT ', i0, //)"
! ------------------------------------------------------------------------------
    !
    ! -- Print a message identifying the advection package.
    write(this%iout, fmtdsp) this%inunit
    !
    ! -- adv pointers to arguments that were passed in
    this%dis     => dis
    this%ibound  => ibound
    this%porosity => porosity
    !
    ! -- Allocate arrays
    call this%allocate_arrays(dis%nodes)
    !
    ! -- Read dispersion options
    call this%read_options()
    !
    ! -- Read dispersion data
    call this%read_data()
    !
    ! -- Return
    return
  end subroutine dsp_ar

  subroutine dsp_fc(this, nodes, amatsln, idxglo)
! ******************************************************************************
! dsp_fc -- Calculate coefficients and fill amat and rhs
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use GwfNpfModule, only: thksatnm
    ! -- dummy
    class(GwtDspType) :: this
    integer, intent(in) :: nodes
    real(DP), dimension(:), intent(inout) :: amatsln
    integer(I4B), intent(in), dimension(:) :: idxglo
    ! -- local
    integer(I4B) :: n, m, idiag, ipos
    real(DP) :: clnm, clmn, anm, wt, dstar, dnm
! ------------------------------------------------------------------------------
    !
    ! -- loop through and calculate dispersion contribution
    do n = 1, nodes
      if(this%ibound(n) == 0) cycle
      idiag = this%dis%con%ia(n)
      do ipos = this%dis%con%ia(n) + 1, this%dis%con%ia(n + 1) - 1
        m = this%dis%con%ja(ipos)
        if(this%ibound(m) == 0) cycle
        
        clnm = this%dis%con%cl1(this%dis%con%jas(ipos))
        clmn = this%dis%con%cl2(this%dis%con%jas(ipos))
        
        !TODO: need real saturated thickness        
        !anm = thksatnm(ibdn, ibdm, ictn, ictm, inwtup, ihc, iusg,              &
        !               hn, hm, satn, satm, topn, topm, botn, botm, satomega)
        anm = 1.d0
        
        dstar = wt * this%porosity(n) * this%diffc(n) + &
                (DONE - wt) * this%porosity(m) * this%diffc(m)
        dnm = dstar * anm / (clnm + clmn)
        
        amatsln(idxglo(ipos)) = amatsln(idxglo(ipos)) + dnm
        amatsln(idxglo(idiag)) = amatsln(idxglo(idiag)) - dnm

      enddo
    enddo
    !
    ! -- Return
    return
  end subroutine dsp_fc
  
  subroutine allocate_arrays(this, nodes)
! ******************************************************************************
! allocate_arrays
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryManagerModule, only: mem_allocate
    use ConstantsModule, only: DZERO
    ! -- dummy
    class(GwtDspType) :: this
    integer(I4B), intent(in) :: nodes
    ! -- local
    integer(I4B) :: n
! ------------------------------------------------------------------------------
    !
    ! -- Allocate
    call mem_allocate(this%diffc, nodes, 'DIFFC', this%origin)
    !
    ! -- Initialize
    do n = 1, nodes
      this%diffc(n) = DZERO
    enddo
    !
    ! -- Return
    return
  end subroutine allocate_arrays

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
    class(GwtDspType) :: this
    ! -- local
    character(len=LINELENGTH) :: errmsg, keyword
    integer(I4B) :: ierr
    logical :: isfound, endOfBlock
    ! -- formats
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
          case default
            write(errmsg,'(4x,a,a)')'****ERROR. UNKNOWN DISPERSION OPTION: ',  &
                                     trim(keyword)
            call store_error(errmsg)
            call ustop()
        end select
      end do
      write(this%iout,'(1x,a)')'END OF DISPERSION OPTIONS'
    end if
    !
    ! -- Return
    return
  end subroutine read_options

  subroutine read_data(this)
! ******************************************************************************
! read_data -- read the dispersion data
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use ConstantsModule,   only: LINELENGTH
    use SimModule,         only: ustop, store_error, count_errors
    ! -- dummy
    class(GwtDsptype) :: this
    ! -- local
    character(len=LINELENGTH) :: line, errmsg, keyword
    integer(I4B) :: istart, istop, lloc, ierr
    logical :: isfound, endOfBlock
    character(len=24), dimension(1) :: aname
    ! -- formats
    ! -- data
    data aname(1) /'               DIFFUSION'/
! ------------------------------------------------------------------------------
    !
    ! -- initialize
    isfound = .false.
    !
    ! -- get griddata block
    call this%parser%GetBlock('GRIDDATA', isfound, ierr)
    if(isfound) then
      write(this%iout,'(1x,a)')'PROCESSING GRIDDATA'
      do
        call this%parser%GetNextLine(endOfBlock)
        if (endOfBlock) exit
        call this%parser%GetStringCaps(keyword)
        call this%parser%GetRemainingLine(line)
        lloc = 1
        select case (keyword)
          case ('DIFFUSION')
            call this%dis%read_grid_array(line, lloc, istart, istop, this%iout,&
                                         this%parser%iuactive, this%diffc,     &
                                         aname(1))
          case default
            write(errmsg,'(4x,a,a)')'ERROR. UNKNOWN GRIDDATA TAG: ',           &
                                     trim(keyword)
            call store_error(errmsg)
            call this%parser%StoreErrorUnit()
            call ustop()
        end select
      end do
      write(this%iout,'(1x,a)')'END PROCESSING GRIDDATA'
    else
      write(errmsg,'(1x,a)')'ERROR.  REQUIRED GRIDDATA BLOCK NOT FOUND.'
      call store_error(errmsg)
      call this%parser%StoreErrorUnit()
      call ustop()
    end if
    !
    ! -- terminate if errors
    if(count_errors() > 0) then
      call this%parser%StoreErrorUnit()
      call ustop()
    endif
    !
    ! -- Return
    return
  end subroutine read_data
 
end module GwtDspModule
  