module EvtModule
  !
  use KindModule, only: DP, I4B
  use ConstantsModule, only: DZERO, DONE, LENFTYPE, LENPACKAGENAME, MAXCHARLEN
  use BndModule, only: BndType
  use SimModule, only: store_error, store_error_unit, ustop
  use ObsModule, only: DefaultObsIdProcessor
  use TimeArraySeriesLinkModule, only: TimeArraySeriesLinkType
  use TimeSeriesLinkModule, only: TimeSeriesLinkType, &
                                  GetTimeSeriesLinkFromList
  use BlockParserModule, only: BlockParserType
  !
  implicit none
  !
  private
  public :: evt_create
  !
  character(len=LENFTYPE)       :: ftype = 'EVT'
  character(len=LENPACKAGENAME) :: text  = '             EVT'
  !
  type, extends(BndType) :: EvtType
    ! -- logicals
    logical, private               :: segsdefined = .true.
    logical, private               :: fixed_cell = .false.
    logical, private               :: read_as_arrays = .false.
    logical, private               :: surfratespecified = .false.
    ! -- integers
    integer(I4B), pointer               :: inievt => null()
    integer(I4B), pointer, private      :: nseg => null()
    ! -- arrays
    integer(I4B), pointer, dimension(:) :: nodesontop => null()
  contains
    procedure :: evt_allocate_scalars
    procedure :: bnd_options         => evt_options
    procedure :: read_dimensions     => evt_read_dimensions
    procedure :: read_initial_attr   => evt_read_initial_attr
    procedure :: bnd_rp              => evt_rp
    procedure :: set_nodesontop
    procedure :: bnd_cf              => evt_cf
    procedure :: bnd_fc              => evt_fc
    procedure :: bnd_da              => evt_da
    procedure :: define_listlabel    => evt_define_listlabel
    procedure, private :: evt_rp_array
    procedure, private :: evt_rp_list
    procedure, private :: default_nodelist
    ! -- for observations
    procedure, public :: bnd_obs_supported  => evt_obs_supported
    procedure, public :: bnd_df_obs => evt_df_obs
    ! -- for time series
    procedure, public :: bnd_rp_ts => evt_rp_ts
  end type EvtType

  ! EVT uses BndType%bound array columns:
  ! Index                  Description                   old name  Keyword
  !  (1,n)                 ET Surface elevation            ETSS    SURFACE
  !  (2,n)                 Max ET Rate                     ETSR    RATE
  !  (3,n)                 Extinction Depth                ETSX    DEPTH
  ! Used only if nseg > 1 and surfratespecified is false:
  !   4->2+nseg            Proportion of Extinction Depth  PXDP    PXDP
  !   3+nseg->3+2(nseg-1)  Proportion of Max ET Rate       PETM    PETM
  ! If nseg > 1 and surfratespecified is true:
  !   4->3+nseg            Proportion of Extinction Depth  PXDP    PXDP
  !   4+nseg->3+2(nseg)    Proportion of Max ET Rate       PETM    PETM

  contains

  subroutine evt_create(packobj, id, ibcnum, inunit, iout, namemodel, pakname)
! ******************************************************************************
! evt_create -- Create a new Evapotranspiration Segments Package
! Subroutine: (1) create new-style package
!             (2) point packobj to the new package
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(BndType), pointer :: packobj
    integer(I4B),intent(in) :: id
    integer(I4B),intent(in) :: ibcnum
    integer(I4B),intent(in) :: inunit
    integer(I4B),intent(in) :: iout
    character(len=*), intent(in) :: namemodel
    character(len=*), intent(in) :: pakname
    ! -- local
    type(EvtType), pointer :: evtobj
! ------------------------------------------------------------------------------
    !
    ! -- allocate evt object and scalar variables
    allocate(evtobj)
    packobj => evtobj
    !
    ! -- create name and origin
    call packobj%set_names(ibcnum, namemodel, pakname, ftype)
    packobj%text = text
    !
    ! -- allocate scalars
    call evtobj%evt_allocate_scalars()
    !
    ! -- initialize package
    call packobj%pack_initialize()

    packobj%inunit = inunit
    packobj%iout = iout
    packobj%id = id
    packobj%ibcnum = ibcnum
    packobj%ncolbnd = 3 ! Assumes NSEG = 1
    packobj%iscloc = 2  ! sfac applies to max. ET rate
    ! indxconvertflux is Column index of bound that will be multiplied by
    ! cell area to convert flux rates to flow rates
    packobj%indxconvertflux = 1
    packobj%AllowTimeArraySeries = .true.
    !
    ! -- return
    return
  end subroutine evt_create

  subroutine evt_da(this)
! ******************************************************************************
! evt_da -- deallocate
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryManagerModule, only: mem_deallocate
    ! -- dummy
    class(EvtType) :: this
! ------------------------------------------------------------------------------
    !
    ! -- arrays
    if(associated(this%nodesontop)) deallocate(this%nodesontop)
    !
    ! -- scalars
    call mem_deallocate(this%inievt)
    call mem_deallocate(this%nseg)
    !
    ! -- Deallocate parent package
    call this%BndType%bnd_da()
    !
    ! -- return
    return
  end subroutine evt_da

  subroutine evt_allocate_scalars(this)
    ! **************************************************************************
    ! allocate_scalars -- allocate scalar members
    ! **************************************************************************
    !
    !    SPECIFICATIONS:
    ! --------------------------------------------------------------------------
    use MemoryManagerModule, only: mem_allocate
    ! -- dummy
    class(EvtType),   intent(inout) :: this
    ! --------------------------------------------------------------------------
    !
    ! -- call standard BndType allocate scalars
    call this%BndType%allocate_scalars()
    !
    ! -- allocate the object and assign values to object variables
    call mem_allocate(this%inievt, 'INIEVT', this%origin)
    call mem_allocate(this%nseg, 'NSEG', this%origin)
    !
    ! -- Set values
    this%inievt = 0
    this%nseg = 1
    this%fixed_cell = .false.
    !
    ! -- return
    return
  end subroutine evt_allocate_scalars

  subroutine evt_options(this, option, found)
    ! **************************************************************************
    ! evt_options -- set options specific to EvtType
    !
    ! evt_options overrides BndType%bnd_options
    ! **************************************************************************
    !
    !    SPECIFICATIONS:
    ! --------------------------------------------------------------------------
    implicit none
    ! -- dummy
    class(EvtType),   intent(inout) :: this
    character(len=*), intent(inout) :: option
    logical,          intent(inout) :: found
    ! -- local
    character(len=MAXCHARLEN) :: ermsg
    ! -- formats
    character(len=*),parameter :: fmtihact = &
      "(4x, 'EVAPOTRANSPIRATION WILL BE APPLIED TO HIGHEST ACTIVE CELL.')"
    character(len=*),parameter :: fmtfixedcell = &
      "(4x, 'EVAPOTRANSPIRATION WILL BE APPLIED TO SPECIFIED CELL.')"
    character(len=*), parameter :: fmtreadasarrays = &
      "(4x, 'EVAPOTRANSPIRATION INPUT WILL BE READ AS ARRAYS.')"
    character(len=*), parameter :: fmtsrz = &
      "(4x, 'ET RATE AT SURFACE WILL BE ZERO.')"
    character(len=*), parameter :: fmtsrs = &
      "(4x, 'ET RATE AT SURFACE WILL BE AS SPECIFIED BY PETM0.')"
    ! --------------------------------------------------------------------------
    !
    ! -- Check for FIXED_CELL AND LAYERED
    select case (option)
    case ('FIXED_CELL')
      this%fixed_cell = .true.
      write(this%iout, fmtfixedcell)
      found = .true.
    case ('SURF_RATE_SPECIFIED')
      this%surfratespecified = .true.
      write(this%iout, fmtsrs)
      found = .true.
      !
      if (this%read_as_arrays) then
        ermsg = 'READASARRAYS option is not compatible with the' // &
                ' SURF_RATE_SPECIFIED option.'
        call store_error(ermsg)
        call this%parser%StoreErrorUnit()
        call ustop()
      endif
    case ('READASARRAYS')
      if (this%dis%supports_layers()) then
        this%read_as_arrays = .true.
      else
        ermsg = 'READASARRAYS option is not compatible with selected' // &
                ' discretization type.'
        call store_error(ermsg)
        call this%parser%StoreErrorUnit()
        call ustop()
      endif
      !
      if (this%surfratespecified) then
        ermsg = 'READASARRAYS option is not compatible with the' // &
                ' SURF_RATE_SPECIFIED option.'
        call store_error(ermsg)
        call this%parser%StoreErrorUnit()
        call ustop()
      endif
      !
      ! -- Write option
      write(this%iout, fmtreadasarrays)
      !
      found = .true.
    case default
      !
      ! -- No options found
      found = .false.
    end select
    !
    ! -- return
    return
  end subroutine evt_options

  subroutine evt_read_dimensions(this)
! ******************************************************************************
! bnd_read_dimensions -- Read the dimensions for this package
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use ConstantsModule, only: LINELENGTH
    use SimModule, only: ustop, store_error, store_error_unit
    ! -- dummy
    class(EvtType),intent(inout) :: this
    ! -- local
    character(len=LINELENGTH) :: errmsg, keyword
    integer(I4B) :: ierr
    logical :: isfound, endOfBlock
    ! -- format
    character(len=*), parameter :: fmtnsegerr = &
        "('Error: In EVT, NSEG must be > 0 but is specified as ',i0)"
! ------------------------------------------------------------------------------
    !
    ! Dimensions block is not required if:
    !   (1) discretization is DIS or DISV, and
    !   (2) READASARRAYS option has been specified.
    if (this%read_as_arrays) then
      this%maxbound = this%dis%get_ncpl()
    else
      ! -- get dimensions block
      call this%parser%GetBlock('DIMENSIONS', isfound, ierr, &
                                supportOpenClose=.true.)
      !
      ! -- parse dimensions block if detected
      if (isfound) then
        write(this%iout,'(/1x,a)')'PROCESSING '//trim(adjustl(this%text))// &
          ' DIMENSIONS'
        do
          call this%parser%GetNextLine(endOfBlock)
          if (endOfBlock) exit
          call this%parser%GetStringCaps(keyword)
          select case (keyword)
          case ('MAXBOUND')
            if (this%read_as_arrays) then
              errmsg = 'When READASARRAYS option is used for the selected' // &
                       ' discretization package, MAXBOUND may not be specified.'
              call store_error(errmsg)
              call this%parser%StoreErrorUnit()
              call ustop()
            else
              this%maxbound = this%parser%GetInteger()
              write(this%iout,'(4x,a,i7)') 'MAXBOUND = ', this%maxbound
            endif
          case ('NSEG')
            this%nseg = this%parser%GetInteger()
            write(this%iout,'(4x,a,i0)') 'NSEG = ', this%nseg
            if (this%nseg < 1) then
              write(errmsg,fmtnsegerr)this%nseg
              call store_error(errmsg)
              call this%parser%StoreErrorUnit()
              call ustop()
            elseif (this%nseg > 1) then
              ! NSEG>1 is supported only if readasarrays is false
              if (this%read_as_arrays) then
                errmsg = 'In the EVT package, NSEG cannot be greater than 1' // &
                         ' when READASARRAYS is used.'
                call store_error(errmsg)
                call this%parser%StoreErrorUnit()
                call ustop()
              endif
              ! -- Recalculate number of columns required in bound array.
              if (this%surfratespecified) then
                this%ncolbnd = 4 + 2*(this%nseg-1)
              else
                this%ncolbnd = 3 + 2*(this%nseg-1)
              endif
            endif
          case default
            write(errmsg,'(4x,a,a)') &
              '****ERROR. UNKNOWN '//trim(this%text)//' DIMENSION: ', &
                                      trim(keyword)
            call store_error(errmsg)
            call this%parser%StoreErrorUnit()
            call ustop()
          end select
        end do
        !
        write(this%iout,'(1x,a)')'END OF '//trim(adjustl(this%text))//' DIMENSIONS'
      else
        call store_error('ERROR.  REQUIRED DIMENSIONS BLOCK NOT FOUND.')
        call this%parser%StoreErrorUnit()
        call ustop()
      endif
    endif
    !
    ! -- verify dimensions were set
    if(this%maxbound <= 0) then
      write(errmsg, '(1x,a)') &
        'ERROR.  MAXBOUND MUST BE AN INTEGER GREATER THAN ZERO.'
      call store_error(errmsg)
      call this%parser%StoreErrorUnit()
      call ustop()
    endif
    !
    ! -- Call define_listlabel to construct the list label that is written
    !    when PRINT_INPUT option is used.
    call this%define_listlabel()
    !
    ! -- return
    return
  end subroutine evt_read_dimensions

  subroutine evt_read_initial_attr(this)
! ******************************************************************************
! evt_read_initial_attr -- Part of allocate and read
! If READASARRAYS has been specified, assign default IEVT = 1
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    implicit none
    ! -- dummy
    class(EvtType),intent(inout) :: this
    !
    if (this%read_as_arrays) then
      call this%default_nodelist()
    endif
    !
    return
  end subroutine evt_read_initial_attr

  subroutine evt_rp(this)
    ! **************************************************************************
    ! evt_rp -- Read and Prepare
    ! Subroutine: (1) read itmp
    !             (2) read new boundaries if itmp>0
    ! **************************************************************************
    !
    !    SPECIFICATIONS:
    ! --------------------------------------------------------------------------
    use ConstantsModule, only: LINELENGTH
    use TdisModule, only: kper, nper
    use SimModule, only: ustop, store_error
    use ArrayHandlersModule, only: ifind
    implicit none
    ! -- dummy
    class(EvtType),intent(inout) :: this
    ! -- local
    integer(I4B) :: ierr
    integer(I4B) :: node, n
    integer(I4B) :: inievt, inrate, insurf, indepth
    integer(I4B) :: kpxdp, kpetm
    logical :: isfound, supportopenclose
    character(len=LINELENGTH) :: line, msg, errmsg
    ! -- formats
    character(len=*),parameter :: fmtblkerr = &
      "('Error.  Looking for BEGIN PERIOD iper.  Found ', a, ' instead.')"
    character(len=*),parameter :: fmtlsp = &
      "(1X,/1X,'REUSING ',A,'S FROM LAST STRESS PERIOD')"
    character(len=*), parameter :: fmtnbd = &
      "(1X,/1X,'THE NUMBER OF ACTIVE ',A,'S (',I6," // &
       "') IS GREATER THAN MAXIMUM(',I6,')')"
    ! --------------------------------------------------------------------------
    !
    ! -- Set ionper to the stress period number for which a new block of data
    !    will be read.
    if(this%inunit == 0) return
    !
    ! -- get stress period data
    if (this%ionper < kper) then
      !
      ! -- get period block
      supportopenclose = .not. this%read_as_arrays
      ! When reading a list, OPEN/CLOSE is handled by list reader,
      ! so supportOpenClose needs to be false in call the GetBlock.
      ! When reading as arrays, set supportOpenClose as desired.
      call this%parser%GetBlock('PERIOD', isfound, ierr)
      if(isfound) then
        !
        ! -- save last value and read period number
        this%lastonper = this%ionper
        this%ionper = this%parser%GetInteger()
        !
        ! -- check to make sure period blocks are increasing
        if (this%ionper < this%lastonper) then
          write(errmsg, '(a, i0)') &
            'ERROR IN STRESS PERIOD ', kper
          call store_error(errmsg)
          write(errmsg, '(a, i0)') &
            'PERIOD NUMBERS NOT INCREASING.  FOUND ', this%ionper
          call store_error(errmsg)
          write(errmsg, '(a, i0)') &
            'BUT LAST PERIOD BLOCK WAS ASSIGNED ', this%lastonper
          call store_error(errmsg)
          call this%parser%StoreErrorUnit()
          call ustop()
        endif
      else
        !
        ! -- PERIOD block not found
        if (ierr < 0) then
          ! -- End of file found; data applies for remainder of simulation.
          this%ionper = nper + 1
        else
          ! -- Found invalid block
          write(errmsg, fmtblkerr) adjustl(trim(line))
          call store_error(errmsg)
          call this%parser%StoreErrorUnit()
          call ustop()
        end if
      endif
    end if
    !
    ! -- Read data if ionper == kper
    inrate = 0
    insurf = 0
    indepth = 0
    inievt = 0
    if (this%ionper==kper) then
      !
      ! -- Remove all time-series links associated with this package
      call this%TsManager%Reset(this%name)
      call this%TasManager%Reset(this%name)
      !
      ! -- Read IEVT, SURFACE, RATE, DEPTH, PXDP, PETM, and AUX
      !    variables, if any
      kpetm = 0
      kpxdp = 0
      !
      if (.not. this%read_as_arrays) then
        ! -- Read EVT input as a list
        call this%evt_rp_list(inrate)
      else
        ! -- Read Evt input as arrays
        call this%evt_rp_array(line, inrate, insurf, indepth, &
                               kpxdp, kpetm, ierr)
      endif
      !
      ! -- Ensure that all required PXDP and PETM arrays
      !    have been defined or redefined.
      if (this%surfratespecified) then
        if (kpxdp == this%nseg .and. kpxdp == this%nseg) then
          this%segsdefined = .true.
        endif
      else
        if (kpxdp == this%nseg-1 .and. kpxdp == this%nseg-1) then
          this%segsdefined = .true.
        endif
      endif
      if (.not. this%segsdefined) then
        msg = 'Error in EVT input: Definition of PXDP or PETM is incomplete.'
        call store_error(msg)
        call this%parser%StoreErrorUnit()
        call ustop()
      endif
    else
      write(this%iout,fmtlsp) trim(this%filtyp)
    endif
    !
    ! -- If rate was read, then multiply by cell area
    if (inrate /= 0) then
      do n = 1, this%nbound
        node = this%nodelist(n)
        this%bound(2, n) = this%bound(2, n) * this%dis%get_area(node)
      enddo
    endif
    !
    ! -- return
    return
  end subroutine evt_rp

  subroutine set_nodesontop(this)
    ! **************************************************************************
    ! set_nodesontop -- store nodelist in nodesontop
    ! **************************************************************************
    !
    !    SPECIFICATIONS:
    ! --------------------------------------------------------------------------
    implicit none
    ! -- dummy
    class(EvtType),intent(inout) :: this
    ! -- local
    integer(I4B) :: n
    ! -- formats
    ! --------------------------------------------------------------------------
    !
    ! -- allocate if necessary
    if(.not. associated(this%nodesontop)) then
      allocate(this%nodesontop(this%maxbound))
    endif
    !
    ! -- copy nodelist into nodesontop
    do n = 1, this%nbound
      this%nodesontop(n) = this%nodelist(n)
    enddo
    !
    ! -- return
    return
  end subroutine set_nodesontop

  subroutine evt_cf(this)
    ! **************************************************************************
    ! evt_cf -- Formulate the HCOF and RHS terms
    ! Subroutine: (1) skip if no ET
    !             (2) calculate hcof and rhs
    ! **************************************************************************
    !
    !    SPECIFICATIONS:
    ! --------------------------------------------------------------------------
    implicit none
    ! -- dummy
    class(EvtType) :: this
    ! -- local
    integer(I4B) :: i, iseg, node
    integer(I4B) :: idxdepth, idxrate
    !real(DP), parameter :: ONE = 1.d0
    !real(DP), parameter :: ZERO = 0.d0
    real(DP) :: c, d, h, s, x
    real(DP) :: petm0
    real(DP) :: petm1, petm2, pxdp1, pxdp2, thcof, trhs
    ! --------------------------------------------------------------------------
    !
    ! -- Return if no ET nodes
    if (this%nbound == 0) return
    !
    ! -- Calculate hcof and rhs for each ET node
    do i=1,this%nbound
      !
      ! -- Find the node number
      if (this%fixed_cell) then
        node = this%nodelist(i)
      else
        node = this%nodesontop(i)
        if (this%ibound(node) == 0) &
          call this%dis%highest_active(node, this%ibound)
        this%nodelist(i) = node
      endif
      !
      ! -- set rhs and hcof to zero
      this%rhs(i) = DZERO
      this%hcof(i) = DZERO
      !
      ! -- if ibound is positive, then add terms
      if (this%ibound(node) > 0) then
        !
        c = this%bound(2,i) ! RATE -- max. ET rate
        s = this%bound(1,i) ! SURFACE -- ET surface elevation
        h = this%xnew(node)
        if (this%surfratespecified) then
          petm0 = this%bound(4+2*(this%nseg-1),i) ! PETM0
        endif
        !
        ! -- If head in cell is greater than or equal to SURFACE, ET is constant
        if (h >= s) then
          if (this%surfratespecified) then
            ! -- Subtract -PETM0 from RHS
            this%rhs(i) = this%rhs(i) + petm0
          else
            ! -- Subtract -RATE from RHS
            this%rhs(i) = this%rhs(i) + c
          endif
        else
          ! -- If depth to water >= extinction depth, then ET is 0
          d = S - h
          x = this%bound(3,i) ! DEPTH -- extinction depth
          if (d < x) then
            ! -- Variable range. add ET terms to both RHS and HCOF.
            if (this%nseg > 1) then
              ! -- Determine which segment applies based on head, and
              !    calculate terms to add to RHS and HCOF
              !
              ! -- Set proportions corresponding to surface elevation
              !    and initial indices for depth and rate.
              ! -- Idxdepth will point to the elements of bound containing
              !    proportion of depth at the bottom of each segment.
              !    Idxrate will point to the elements of bound containing
              !    proportion of ET rate at the bottom of each segment.
              !    If surfratespecified is true, skip over the elements
              !    containing pxdp0 (=0.0) and petm0.
              pxdp1 = DZERO
              if (this%surfratespecified) then
                petm1 = petm0
              else
                petm1 = DONE
              endif
              ! -- Initialize indices to point to elements preceding
              !    pxdp1 and petm1 (values for lower end of segment 1).
              idxdepth = 3
              idxrate = 2 + this%nseg
              ! -- Iterate through segments to find segment that contains
              !    current depth of head below ET surface.
              segloop: do iseg=1,this%nseg
                ! -- Set proportions corresponding to lower end of
                !    segment
                if (iseg < this%nseg) then
                  ! -- Increment the indices for depth and rate
                  idxdepth = idxdepth + 1
                  idxrate = idxrate + 1
                  ! -- Get proportions for lower end of segment
                  pxdp2 = this%bound(idxdepth,i)
                  petm2 = this%bound(idxrate,i)
                else
                  pxdp2 = DONE
                  petm2 = DZERO
                endif
                if (d <= pxdp2*x) then
                  ! -- head is in domain of this segment
                  exit segloop
                endif
                ! -- Proportions at lower end of segment will be for
                !    upper end of segment next time through loop
                pxdp1 = pxdp2
                petm1 = petm2
              enddo segloop
              ! -- Calculate terms to add to RHS and HCOF based on
              !    segment that applies at head elevation
              thcof = -(abs(petm1-petm2))*c/((pxdp2-pxdp1)*x)
              trhs = thcof*(s-pxdp1*x) + petm1*c
            else
              ! -- Calculate terms to add to RHS and HCOF based on simple
              !    linear relation of ET vs. head for single segment
              trhs = c - c*s/x
              thcof = -c/x
            endif
            this%rhs(i) = this%rhs(i) + trhs
            this%hcof(i) = this%hcof(i) + thcof
          endif
        endif
      endif
      !
    enddo
    !
    ! -- return
    return
  end subroutine evt_cf

  subroutine evt_fc(this, rhs, ia, idxglo, amatsln)
! **************************************************************************
! evt_fc -- Copy rhs and hcof into solution rhs and amat
! **************************************************************************
!
!    SPECIFICATIONS:
! --------------------------------------------------------------------------
    ! -- dummy
    class(EvtType) :: this
    real(DP), dimension(:), intent(inout) :: rhs
    integer(I4B), dimension(:), intent(in) :: ia
    integer(I4B), dimension(:), intent(in) :: idxglo
    real(DP), dimension(:), intent(inout) :: amatsln
    ! -- local
    integer(I4B) :: i, n, ipos
! --------------------------------------------------------------------------
    !
    ! -- Copy package rhs and hcof into solution rhs and amat
    do i = 1, this%nbound
      n = this%nodelist(i)
      ! -- reset hcof and rhs for excluded cells
      if (this%ibound(n) == 10000) then
        this%hcof(i) = DZERO
        this%rhs(i) = DZERO
        cycle
      end if
      rhs(n) = rhs(n) + this%rhs(i)
      ipos = ia(n)
      amatsln(idxglo(ipos)) = amatsln(idxglo(ipos)) + this%hcof(i)
    enddo
    !
    ! -- return
    return
  end subroutine evt_fc

  subroutine evt_rp_array(this, line, inrate, insurf, indepth, &
                          kpxdp, kpetm, ierr)
    ! **************************************************************************
    ! evt_rp_array -- Read and Prepare EVT as arrays
    ! **************************************************************************
    !
    !    SPECIFICATIONS:
    ! --------------------------------------------------------------------------
    use ConstantsModule, only: LENTIMESERIESNAME, LINELENGTH
    use SimModule, only: ustop, store_error
    use ArrayHandlersModule, only: ifind
    implicit none
    ! -- dummy
    class(EvtType),            intent(inout) :: this
    character(len=LINELENGTH), intent(inout) :: line
    integer(I4B),                   intent(inout) :: inrate
    integer(I4B),                   intent(inout) :: insurf
    integer(I4B),                   intent(inout) :: indepth
    integer(I4B),                   intent(inout) :: kpxdp
    integer(I4B),                   intent(inout) :: kpetm
    integer(I4B),                   intent(inout) :: ierr
    ! -- local
    integer(I4B) :: indx, ipos, istart, istop
    integer(I4B) :: jcol, jauxcol, lpos
    character(len=LENTIMESERIESNAME) :: tasName
    character(len=24) ::  atemp
    character(len=24), dimension(6) :: aname
    character(len=100) :: ermsg, keyword
    logical :: found, endOfBlock
    logical :: convertFlux = .true.
    real(DP), dimension(:), pointer :: bndArrayPtr => null()
    real(DP), dimension(:), pointer :: auxArrayPtr => null()
    real(DP), dimension(:), pointer :: auxMultArray => null()
    type(TimeArraySeriesLinkType),  pointer :: tasLink => null()
    ! data
    data aname(1) /'     LAYER OR NODE INDEX'/
    data aname(2) /'              ET SURFACE'/
    data aname(3) /' EVAPOTRANSPIRATION RATE'/
    data aname(4) /'        EXTINCTION DEPTH'/
    data aname(5) /'EXTINCT. DEP. PROPORTION'/
    data aname(6) /'      ET RATE PROPORTION'/
    !
    ! --------------------------------------------------------------------------
    !
    jauxcol = 0
    ! -- Read IEVT, SURFACE, RATE, DEPTH, PXDP, PETM, and AUX
    !    as arrays
    kpetm = 0
    kpxdp = 0
    do
      call this%parser%GetNextLine(endOfBlock)
      if (endOfBlock) exit
      call this%parser%GetStringCaps(keyword)
      !
      ! -- Parse the keywords
      select case (keyword)
      case ('IEVT')
        !
        ! -- Read the IEVT array
        call this%dis%nlarray_to_nodelist(this%nodelist, this%maxbound, &
                 this%nbound, aname(1), this%parser%iuactive, this%iout)
        !
        ! -- set flag to indicate that IEVT has been read
        this%inievt = 1
        !
        ! -- if highest_active option set, then need to store nodelist
        !    in the nodesontop array
        if (.not. this%fixed_cell) call this%set_nodesontop()
        !
      case ('SURFACE')
        !
        if (this%inievt == 0) then
          call store_error('Error.  IEVT must be read at least once ')
          call store_error('prior to reading the SURFACE array.')
          call this%parser%StoreErrorUnit()
          call ustop()
        endif
        !
        ! -- Read the surface array, then indicate
        !    that surface array was read by setting insurf
        call this%dis%read_layer_array(this%nodelist, this%bound, &
              this%ncolbnd, this%maxbound, 1, aname(2), this%parser%iuactive, &
              this%iout)
        insurf = 1
        !
      case ('RATE')
        !
        if (this%read_as_arrays) then
          if (this%inievt == 0) then
            call store_error('Error.  IEVT must be read at least once ')
            call store_error('prior to reading the RATE array.')
            call this%parser%StoreErrorUnit()
            call ustop()
          endif
        endif
        !
        ! -- Look for keyword TIMEARRAYSERIES and time-array series
        !    name on line, following RATE
        call this%parser%GetStringCaps(keyword)
        if (keyword == 'TIMEARRAYSERIES') then
          ! -- Get time-array series name
          call this%parser%GetStringCaps(tasName)
          ! -- Ensure that time-array series has been defined and that name
          !    of time-array series is valid.
          jcol = 2  ! for max ET rate
          bndArrayPtr => this%bound(jcol,:)
          ! Make a time-array-series link and add it to the list of links
          ! contained in the TimeArraySeriesManagerType object.
          call this%TasManager%MakeTasLink(this%name, bndArrayPtr, &
                                  this%iprpak, this%TimeArraySeriesFiles, &
                                  tasName, this%dis, 'RATE', convertFlux, &
                                  this%nodelist, this%parser%iuactive)
          lpos = this%TasManager%CountLinks()
          tasLink => this%TasManager%GetLink(lpos)
        else
          !
          ! -- Read the Max. ET Rate array, then indicate
          !    that rate array was read by setting inrate
          call this%dis%read_layer_array(this%nodelist, this%bound, &
            this%ncolbnd, this%maxbound, 2, aname(3), this%parser%iuactive, &
            this%iout)
        endif
        inrate = 1
        !
      case ('DEPTH')
        !
        if (this%inievt == 0) then
          call store_error('Error.  IEVT must be read at least once ')
          call store_error('prior to reading the DEPTH array.')
          call this%parser%StoreErrorUnit()
          call ustop()
        endif
        !
        ! -- Read the extinction-depth array, then indicate
        !    that depth array was read by setting indepth
        call this%dis%read_layer_array(this%nodelist, this%bound, &
          this%ncolbnd, this%maxbound, 3, aname(4), this%parser%iuactive, &
          this%iout)
        indepth = 1
        !
      case ('PXDP')
        if (this%nseg < 2) then
          ermsg = 'Error in EVT input: PXDP cannot be specified when NSEG < 2'
          call store_error(ermsg)
          call this%parser%StoreErrorUnit()
          call ustop()
        endif
        !
        if (this%inievt == 0) then
          call store_error('Error.  IEVT must be read at least once ')
          call store_error('prior to reading any PXDP array.')
          call this%parser%StoreErrorUnit()
          call ustop()
        endif
        !
        ! -- Assign column for this PXDP vector in bound array
        kpxdp = kpxdp + 1
        if (kpxdp < this%nseg-1) this%segsdefined = .false.
        if (kpxdp > this%nseg-1) then
          ermsg = 'Error in EVT: Number of PXDP arrays exceeds NSEG-1.'
          call store_error(ermsg)
          call this%parser%StoreErrorUnit()
          call ustop()
        endif
        indx = 3 + kpxdp
        !
        ! -- Read the PXDP array
        call this%dis%read_layer_array(this%nodelist, this%bound, &
            this%ncolbnd, this%maxbound, indx, aname(5), &
            this%parser%iuactive, this%iout)
        !
      case ('PETM')
        if (this%nseg < 2) then
          ermsg = 'Error in EVT input: PETM cannot be specified when NSEG < 2'
          call store_error(ermsg)
          call this%parser%StoreErrorUnit()
          call ustop()
        endif
        !
        if (this%inievt == 0) then
          call store_error('Error.  IEVT must be read at least once ')
          call store_error('prior to reading any PETM array.')
          call this%parser%StoreErrorUnit()
          call ustop()
        endif
        !
        ! -- Assign column for this PETM vector in bound array
        kpetm = kpetm + 1
        if (kpetm < this%nseg-1) this%segsdefined = .false.
        if (kpetm > this%nseg-1) then
          ermsg = 'Error in EVT: Number of PETM arrays exceeds NSEG-1.'
          call store_error(ermsg)
          call this%parser%StoreErrorUnit()
          call ustop()
        endif
        indx = 3 + this%nseg - 1 + kpetm
        !
        ! -- Read the PETM array
        call this%dis%read_layer_array(this%nodelist, this%bound, &
            this%ncolbnd, this%maxbound, indx, aname(6), &
            this%parser%iuactive, this%iout)
        !
      case default
        !
        ! -- Check for auxname, and if found, then read into auxvar array
        found = .false.
        ipos = ifind(this%auxname, line(istart:istop))
        if (ipos > 0) then
          found = .true.
          atemp = line(istart:istop)
          !
          ! -- Look for keyword TIMEARRAYSERIES and time-array series
          !    name on line, following auxname
          call this%parser%GetStringCaps(keyword)
          if (keyword == 'TIMEARRAYSERIES') then
            ! -- Get time-array series name
            call this%parser%GetStringCaps(tasName)
            jauxcol = jauxcol + 1
            auxArrayPtr => this%auxvar(jauxcol,:)
            ! Make a time-array-series link and add it to the list of links
            ! contained in the TimeArraySeriesManagerType object.
            call this%TasManager%MakeTasLink(this%name, auxArrayPtr, &
                                    this%iprpak, this%TimeArraySeriesFiles, &
                                    tasName, this%dis, this%auxname(ipos), &
                                    convertFlux, this%nodelist, &
                                    this%parser%iuactive)
          else
            !
            ! -- Read the aux variable array
            call this%dis%read_layer_array(this%nodelist, this%auxvar, &
              this%naux,  this%maxbound, ipos, atemp, this%parser%iuactive, &
              this%iout)
          endif
        endif
        !
        ! -- Nothing found
        if (.not. found) then
          call this%parser%GetCurrentLine(line)
          call store_error('****ERROR. LOOKING FOR END.  FOUND: ')
          call store_error(line)
          call this%parser%StoreErrorUnit()
          call ustop()
        endif
        !
        ! If this aux variable has been designated as a multiplier array
        ! by presence of AUXMULTNAME, set local pointer appropriately.
        if (this%iauxmultcol > 0 .and. this%iauxmultcol == ipos) then
          auxMultArray => this%auxvar(this%iauxmultcol,:)
        endif
      end select
    end do
    !
    ! -- Ensure that all required PXDP and PETM arrays
    !    have been defined or redefined.
    if (kpxdp == this%nseg-1 .and. kpxdp == this%nseg-1) then
      this%segsdefined = .true.
    endif
    if (.not. this%segsdefined) then
      ermsg = 'Error in EVT input: Definition of PXDP or PETM is incomplete.'
      call store_error(ermsg)
      call this%parser%StoreErrorUnit()
      call ustop()
    endif
    !
    ! If the multiplier-array pointer has been assigned and
    ! stress is controlled by a time-array series, assign
    ! multiplier-array pointer in time-array series link.
    if (associated(auxMultArray)) then
      if (associated(tasLink)) then
        tasLink%RMultArray => auxMultArray
      endif
    endif
    !
    return
  end subroutine evt_rp_array

  subroutine evt_rp_list(this, inrate)
! ******************************************************************************
! evt_rp_list -- Read and Prepare EVT as a list
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    implicit none
    ! -- dummy
    class(EvtType), intent(inout) :: this
    integer(I4B),        intent(inout) :: inrate
    ! -- local
    integer(I4B) :: maxboundorig, nlist
    !
! ------------------------------------------------------------------------------
    !
    nlist = -1
    maxboundorig = this%maxbound
    call this%dis%read_list(this%parser%iuactive, this%iout, this%iprpak,      &
                             nlist, this%inamedbound, this%iauxmultcol,        &
                             this%nodelist,                                    &
                             this%bound, this%auxvar, this%auxname,            &
                             this%boundname, this%listlabel,                   &
                             this%name, this%tsManager, this%iscloc)
    this%nbound = nlist
    if (nlist > this%kbound) this%kbound = nlist
    if (this%maxbound > maxboundorig) then
      ! -- The arrays that belong to BndType have been extended.
      ! Now, EVT array nodesontop needs to be recreated.
      if (associated(this%nodesontop)) then
        deallocate(this%nodesontop)
      endif
    endif
    if (.not. this%fixed_cell) call this%set_nodesontop()
    inrate = 1
    return
  end subroutine evt_rp_list

  subroutine evt_define_listlabel(this)
! ******************************************************************************
! define_listlabel -- Define the list heading that is written to iout when
!   PRINT_INPUT option is used.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    class(EvtType), intent(inout) :: this
    integer(I4B) :: nsegm1, i
! ------------------------------------------------------------------------------
    !
    ! -- create the header list label
    this%listlabel = trim(this%filtyp) // ' NO.'
    if(this%dis%ndim == 3) then
      write(this%listlabel, '(a, a7)') trim(this%listlabel), 'LAYER'
      write(this%listlabel, '(a, a7)') trim(this%listlabel), 'ROW'
      write(this%listlabel, '(a, a7)') trim(this%listlabel), 'COL'
    elseif(this%dis%ndim == 2) then
      write(this%listlabel, '(a, a7)') trim(this%listlabel), 'LAYER'
      write(this%listlabel, '(a, a7)') trim(this%listlabel), 'CELL2D'
    else
      write(this%listlabel, '(a, a7)') trim(this%listlabel), 'NODE'
    endif
    write(this%listlabel, '(a, a16)') trim(this%listlabel), 'SURFACE'
    write(this%listlabel, '(a, a16)') trim(this%listlabel), 'MAX. RATE'
    write(this%listlabel, '(a, a16)') trim(this%listlabel), 'EXT. DEPTH'
    !
    ! -- add headings for as many PXDP and PETM columns as needed
    nsegm1 = this%nseg - 1
    if (nsegm1 > 0) then
      do i = 1,nsegm1
        write(this%listlabel, '(a, a16)') trim(this%listlabel), 'PXDP'
      enddo
      do i = 1,nsegm1
        write(this%listlabel, '(a, a16)') trim(this%listlabel), 'PETM'
      enddo
    endif
    !
    ! -- PETM0, if SURF_RATE_SPECIFIED is used
    if (this%surfratespecified) then
      write(this%listlabel, '(a, a16)') trim(this%listlabel), 'PETM0'
    endif
    !
!    ! -- multiplier
!    if(this%multindex > 0) &
!      write(this%listlabel, '(a, a16)') trim(this%listlabel), 'MULTIPLIER'
    !
    ! -- boundary name
    if(this%inamedbound == 1) then
      write(this%listlabel, '(a, a16)') trim(this%listlabel), 'BOUNDARY NAME'
    endif
    !
    ! -- return
    return
  end subroutine evt_define_listlabel

  subroutine default_nodelist(this)
! ******************************************************************************
! default_nodelist -- Assign default nodelist when READASARRAYS is specified.
!                     Equivalent to reading IEVT as CONSTANT 1
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use InputOutputModule, only: get_node
    use SimModule, only: ustop, store_error
    use ConstantsModule, only: LINELENGTH
    ! -- dummy
    class(EvtType) :: this
    ! -- local
    integer(I4B) :: il, ir, ic, ncol, nrow, nlay, nodeu, noder, ipos
! ------------------------------------------------------------------------------
    !
    ! -- set variables
    if(this%dis%ndim == 3) then
      nlay = this%dis%mshape(1)
      nrow = this%dis%mshape(2)
      ncol = this%dis%mshape(3)
    elseif(this%dis%ndim == 2) then
      nlay = this%dis%mshape(1)
      nrow = 1
      ncol = this%dis%mshape(2)
    endif
    !
    ! -- Populate nodelist
    ipos = 1
    il = 1
    do ir = 1, nrow
      do ic = 1, ncol
        nodeu = get_node(il, ir, ic, nlay, nrow, ncol)
        noder = this%dis%get_nodenumber(nodeu, 0)
        if(noder > 0) then
          this%nodelist(ipos) = noder
          ipos = ipos + 1
        endif
      enddo
    enddo
    !
    ! Set flag that indicates IEVT has been assigned, and assign nbound.
    this%inievt = 1
    this%nbound = ipos - 1
    !
    ! -- if fixed_cell option not set, then need to store nodelist
    !    in the nodesontop array
    if(.not. this%fixed_cell) call this%set_nodesontop()
    !
    ! -- return
  end subroutine default_nodelist

  ! -- Procedures related to observations

  logical function evt_obs_supported(this)
! ******************************************************************************
! evt_obs_supported
!   -- Return true because EVT package supports observations.
!   -- Overrides BndType%bnd_obs_supported()
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    implicit none
    class(EvtType) :: this
! ------------------------------------------------------------------------------
    evt_obs_supported = .true.
    !
    ! -- return
    return
  end function evt_obs_supported

  subroutine evt_df_obs(this)
! ******************************************************************************
! evt_df_obs (implements bnd_df_obs)
!   -- Store observation type supported by EVT package.
!   -- Overrides BndType%bnd_df_obs
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    implicit none
    ! -- dummy
    class(EvtType) :: this
    ! -- local
    integer(I4B) :: indx
! ------------------------------------------------------------------------------
    call this%obs%StoreObsType('evt', .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => DefaultObsIdProcessor
    !
    ! -- return
    return
  end subroutine evt_df_obs

  ! -- Procedure related to time series

  subroutine evt_rp_ts(this)
    ! -- Assign tsLink%Text appropriately for
    !    all time series in use by package.
    !    In EVT package the SURFACE, RATE, DEPTH, PXDP, and PETM variables
    !    can be controlled by time series.
    !    Define Text only when time series is used for SURFACE, RATE, or DEPTH.
    ! -- dummy
    class(EvtType), intent(inout) :: this
    ! -- local
    integer(I4B) :: i, nlinks
    type(TimeSeriesLinkType), pointer :: tslink => null()
    !
    nlinks = this%TsManager%boundtslinks%Count()
    do i=1,nlinks
      tslink => GetTimeSeriesLinkFromList(this%TsManager%boundtslinks, i)
      if (associated(tslink)) then
        select case (tslink%JCol)
        case (1)
          tslink%Text = 'SURFACE'
        case (2)
          tslink%Text = 'RATE'
        case (3)
          tslink%Text = 'DEPTH'
        end select
      endif
    enddo
    !
    return
  end subroutine evt_rp_ts

end module EvtModule

