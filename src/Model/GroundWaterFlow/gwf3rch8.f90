module RchModule
  !
  use KindModule, only: DP, I4B
  use ConstantsModule, only: DZERO, LENFTYPE, LENPACKAGENAME, MAXCHARLEN
  use MemoryHelperModule, only: create_mem_path
  use BndModule, only: BndType
  use SimModule, only: store_error, store_error_unit
  use SimVariablesModule, only: errmsg
  use ObsModule, only: DefaultObsIdProcessor
  use TimeArraySeriesLinkModule, only: TimeArraySeriesLinkType
  use TimeSeriesLinkModule, only: TimeSeriesLinkType, &
                                  GetTimeSeriesLinkFromList
  use BlockParserModule, only: BlockParserType
  !
  implicit none
  !
  private
  public :: rch_create
  !
  character(len=LENFTYPE) :: ftype = 'RCH'
  character(len=LENPACKAGENAME) :: text = '             RCH'
  character(len=LENPACKAGENAME) :: texta = '            RCHA'
  !
  type, extends(BndType) :: RchType
    integer(I4B), pointer :: inirch => NULL()
    integer(I4B), dimension(:), pointer, contiguous :: nodesontop => NULL() ! User provided cell numbers; nodelist is cells where recharge is applied)
    logical, private :: fixed_cell = .false.
    logical, private :: read_as_arrays = .false.
  contains
    procedure :: rch_allocate_scalars
    procedure :: bnd_options => rch_options
    procedure :: read_dimensions => rch_read_dimensions
    procedure :: read_initial_attr => rch_read_initial_attr
    procedure :: bnd_rp => rch_rp
    procedure :: set_nodesontop
    procedure :: bnd_cf => rch_cf
    procedure :: bnd_fc => rch_fc
    procedure :: bnd_da => rch_da
    procedure :: define_listlabel => rch_define_listlabel
    procedure, public :: bnd_rp_ts => rch_rp_ts
    procedure, private :: rch_rp_array
    procedure, private :: rch_rp_list
    procedure, private :: default_nodelist
    ! -- for observations
    procedure, public :: bnd_obs_supported => rch_obs_supported
    procedure, public :: bnd_df_obs => rch_df_obs
  end type RchType

contains

  subroutine rch_create(packobj, id, ibcnum, inunit, iout, namemodel, pakname)
! ******************************************************************************
! rch_create -- Create a New Recharge Package
! Subroutine: (1) create new-style package
!             (2) point packobj to the new package
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(BndType), pointer :: packobj
    integer(I4B), intent(in) :: id
    integer(I4B), intent(in) :: ibcnum
    integer(I4B), intent(in) :: inunit
    integer(I4B), intent(in) :: iout
    character(len=*), intent(in) :: namemodel
    character(len=*), intent(in) :: pakname
    ! -- local
    type(rchtype), pointer :: rchobj
! ------------------------------------------------------------------------------
    !
    ! -- allocate recharge object and scalar variables
    allocate (rchobj)
    packobj => rchobj
    !
    ! -- create name and memory path
    call packobj%set_names(ibcnum, namemodel, pakname, ftype)
    packobj%text = text
    !
    ! -- allocate scalars
    call rchobj%rch_allocate_scalars()
    !
    ! -- initialize package
    call packobj%pack_initialize()

    packobj%inunit = inunit
    packobj%iout = iout
    packobj%id = id
    packobj%ibcnum = ibcnum
    packobj%ncolbnd = 1
    packobj%iscloc = 1 ! sfac applies to recharge rate
    packobj%ictMemPath = create_mem_path(namemodel, 'NPF')
    ! indxconvertflux is Column index of bound that will be multiplied by
    ! cell area to convert flux rates to flow rates
    packobj%indxconvertflux = 1
    packobj%AllowTimeArraySeries = .true.
    !
    ! -- return
    return
  end subroutine rch_create

  subroutine rch_allocate_scalars(this)
! ******************************************************************************
! allocate_scalars -- allocate scalar members
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryManagerModule, only: mem_allocate
    ! -- dummy
    class(RchType), intent(inout) :: this
! ------------------------------------------------------------------------------
    !
    ! -- call standard BndType allocate scalars
    call this%BndType%allocate_scalars()
    !
    ! -- allocate the object and assign values to object variables
    call mem_allocate(this%inirch, 'INIRCH', this%memoryPath)
    !
    ! -- Set values
    this%inirch = 0
    this%fixed_cell = .false.
    !
    ! -- return
    return
  end subroutine rch_allocate_scalars

  subroutine rch_options(this, option, found)
! ******************************************************************************
! rch_options -- set options specific to RchType
!
! rch_options overrides BndType%bnd_options
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use ConstantsModule, only: DZERO
    use SimModule, only: store_error
    implicit none
    ! -- dummy
    class(RchType), intent(inout) :: this
    character(len=*), intent(inout) :: option
    logical, intent(inout) :: found
    ! -- local
    character(len=MAXCHARLEN) :: ermsg
    ! -- formats
    character(len=*), parameter :: fmtihact = &
      &"(4x, 'RECHARGE WILL BE APPLIED TO HIGHEST ACTIVE CELL.')"
    character(len=*), parameter :: fmtfixedcell = &
      &"(4x, 'RECHARGE WILL BE APPLIED TO SPECIFIED CELL.')"
    character(len=*), parameter :: fmtreadasarrays = &
      &"(4x, 'RECHARGE INPUT WILL BE READ AS ARRAY(S).')"
! ------------------------------------------------------------------------------
    !
    ! -- Check for FIXED_CELL and READASARRAYS
    select case (option)
    case ('FIXED_CELL')
      this%fixed_cell = .true.
      write (this%iout, fmtfixedcell)
      found = .true.
    case ('READASARRAYS')
      if (this%dis%supports_layers()) then
        this%read_as_arrays = .true.
        this%text = texta
      else
        ermsg = 'READASARRAYS option is not compatible with selected'// &
                ' discretization type.'
        call store_error(ermsg)
        call this%parser%StoreErrorUnit()
      end if
      !
      ! -- Write option
      write (this%iout, fmtreadasarrays)
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
  end subroutine rch_options

  subroutine rch_read_dimensions(this)
! ******************************************************************************
! bnd_read_dimensions -- Read the dimensions for this package
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use ConstantsModule, only: LINELENGTH
    use SimModule, only: store_error, store_error_unit
    ! -- dummy
    class(RchType), intent(inout) :: this
    ! -- local
    character(len=LINELENGTH) :: keyword
    integer(I4B) :: ierr
    logical :: isfound, endOfBlock
    ! -- format
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
        write (this%iout, '(/1x,a)') 'PROCESSING '//trim(adjustl(this%text))// &
          ' DIMENSIONS'
        do
          call this%parser%GetNextLine(endOfBlock)
          if (endOfBlock) exit
          call this%parser%GetStringCaps(keyword)
          select case (keyword)
          case ('MAXBOUND')
            this%maxbound = this%parser%GetInteger()
            write (this%iout, '(4x,a,i7)') 'MAXBOUND = ', this%maxbound
          case default
            write (errmsg, '(4x,a,a)') &
              'Unknown '//trim(this%text)//' DIMENSION: ', trim(keyword)
            call store_error(errmsg)
            call this%parser%StoreErrorUnit()
          end select
        end do
        !
        write (this%iout, '(1x,a)') &
          'END OF '//trim(adjustl(this%text))//' DIMENSIONS'
      else
        call store_error('Required DIMENSIONS block not found.')
        call this%parser%StoreErrorUnit()
      end if
    end if
    !
    ! -- verify dimensions were set
    if (this%maxbound <= 0) then
      write (errmsg, '(1x,a)') &
        'MAXBOUND must be an integer greater than zero.'
      call store_error(errmsg)
      call this%parser%StoreErrorUnit()
    end if
    !
    ! -- Call define_listlabel to construct the list label that is written
    !    when PRINT_INPUT option is used.
    call this%define_listlabel()
    !
    ! -- return
    return
  end subroutine rch_read_dimensions

  subroutine rch_read_initial_attr(this)
! ******************************************************************************
! rch_read_initial_attr -- Part of allocate and read
! If READASARRAYS has been specified, assign default IRCH = 1
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(RchType), intent(inout) :: this
! ------------------------------------------------------------------------------
    !
    if (this%read_as_arrays) then
      call this%default_nodelist()
    end if
    !
    return
  end subroutine rch_read_initial_attr

  subroutine rch_rp(this)
! ******************************************************************************
! rch_rp -- Read and Prepare
! Subroutine: (1) read itmp
!             (2) read new boundaries if itmp>0
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use ConstantsModule, only: LINELENGTH
    use TdisModule, only: kper, nper
    use SimModule, only: store_error
    implicit none
    ! -- dummy
    class(RchType), intent(inout) :: this
    ! -- local
    integer(I4B) :: ierr
    integer(I4B) :: node, n
    integer(I4B) :: inirch, inrech
    logical :: isfound
    logical :: supportopenclose
    character(len=LINELENGTH) :: line
    ! -- formats
    character(len=*), parameter :: fmtblkerr = &
      &"('Looking for BEGIN PERIOD iper.  Found ', a, ' instead.')"
    character(len=*), parameter :: fmtlsp = &
      &"(1X,/1X,'REUSING ',A,'S FROM LAST STRESS PERIOD')"
    character(len=*), parameter :: fmtnbd = &
      "(1X,/1X,'THE NUMBER OF ACTIVE ',A,'S (',I6, &
      &') IS GREATER THAN MAXIMUM(',I6,')')"
    character(len=*), parameter :: fmtdimlayered = &
      "('When READASARRAYS is specified for the selected discretization &
      &package, DIMENSIONS block must be omitted.')"
! ------------------------------------------------------------------------------
    !
    if (this%inunit == 0) return
    !
    ! -- Set ionper to the stress period number for which a new block of data
    !    will be read.
    if (this%ionper < kper) then
      !
      ! -- get period block
      supportopenclose = .not. this%read_as_arrays
      ! When reading a list, OPEN/CLOSE is handled by list reader,
      ! so supportOpenClose needs to be false in call the GetBlock.
      ! When reading as arrays, set supportOpenClose as desired.
      call this%parser%GetBlock('PERIOD', isfound, ierr)
      if (isfound) then
        !
        ! -- read ionper and check for increasing period numbers
        call this%read_check_ionper()
      else
        !
        ! -- PERIOD block not found
        if (ierr < 0) then
          ! -- End of file found; data applies for remainder of simulation.
          this%ionper = nper + 1
        else
          ! -- Found invalid block
          call this%parser%GetCurrentLine(line)
          write (errmsg, fmtblkerr) adjustl(trim(line))
          call store_error(errmsg)
          if (this%read_as_arrays) then
            write (errmsg, fmtdimlayered)
            call store_error(errmsg)
          end if
          call this%parser%StoreErrorUnit()
        end if
      end if
    end if
    !
    ! -- Read data if ionper == kper
    inrech = 0
    inirch = 0
    if (this%ionper == kper) then
      !
      ! -- Remove all time-series links associated with this package
      call this%TsManager%Reset(this%packName)
      call this%TasManager%Reset(this%packName)
      !
      if (.not. this%read_as_arrays) then
        ! -- Read RECHARGE and other input as a list
        call this%rch_rp_list(inrech)
        call this%bnd_rp_ts()
      else
        ! -- Read RECHARGE, IRCH, and AUX variables as arrays
        call this%rch_rp_array(line, inrech)
      end if
      !
    else
      write (this%iout, fmtlsp) trim(this%filtyp)
    end if
    !
    ! -- If recharge was read, then multiply by cell area.  If inrech = 2, then
    !    recharge is begin managed as a time series, and the time series object
    !    will multiply the recharge rate by the cell area.
    if (inrech == 1) then
      do n = 1, this%nbound
        node = this%nodelist(n)
        if (node > 0) then
          this%bound(1, n) = this%bound(1, n) * this%dis%get_area(node)
        end if
      end do
    end if
    !
    ! -- return
    return
  end subroutine rch_rp

  subroutine rch_rp_array(this, line, inrech)
! ******************************************************************************
! rch_rp_array -- Read and Prepare Recharge as arrays
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use ConstantsModule, only: LENTIMESERIESNAME, LINELENGTH
    use SimModule, only: store_error
    use ArrayHandlersModule, only: ifind
    implicit none
    ! -- dummy
    class(RchType), intent(inout) :: this
    character(len=LINELENGTH), intent(inout) :: line
    integer(I4B), intent(inout) :: inrech
    ! -- local
    integer(I4B) :: n
    integer(I4B) :: ipos
    integer(I4B) :: jcol, jauxcol, lpos, ivarsread
    character(len=LENTIMESERIESNAME) :: tasName
    character(len=24), dimension(2) :: aname
    character(len=LINELENGTH) :: keyword, atemp
    logical :: found, endOfBlock
    logical :: convertFlux
    !
    ! -- these time array series pointers need to be non-contiguous
    !    beacuse a slice of bound is passed
    real(DP), dimension(:), pointer :: bndArrayPtr => null()
    real(DP), dimension(:), pointer :: auxArrayPtr => null()
    real(DP), dimension(:), pointer :: auxMultArray => null()
    type(TimeArraySeriesLinkType), pointer :: tasLink => null()
    ! -- formats
    character(len=*), parameter :: fmtrchauxmult = &
      "(4x, 'THE RECHARGE ARRAY IS BEING MULTIPLED BY THE AUXILIARY ARRAY WITH &
        &THE NAME: ', A)"
    ! -- data
    data aname(1)/'     LAYER OR NODE INDEX'/
    data aname(2)/'                RECHARGE'/
    !
! ------------------------------------------------------------------------------
    !
    ! -- Initialize
    jauxcol = 0
    ivarsread = 0
    !
    ! -- Read RECHARGE, IRCH, and AUX variables as arrays
    do
      call this%parser%GetNextLine(endOfBlock)
      if (endOfBlock) exit
      call this%parser%GetStringCaps(keyword)
      !
      ! -- Parse the keywords
      select case (keyword)
      case ('RECHARGE')
        !
        ! -- Look for keyword TIMEARRAYSERIES and time-array series
        !    name on line, following RECHARGE
        call this%parser%GetStringCaps(keyword)
        if (keyword == 'TIMEARRAYSERIES') then
          ! -- Get time-array series name
          call this%parser%GetStringCaps(tasName)
          jcol = 1 ! for recharge rate
          bndArrayPtr => this%bound(jcol, :)
          ! Make a time-array-series link and add it to the list of links
          ! contained in the TimeArraySeriesManagerType object.
          convertflux = .true.
          call this%TasManager%MakeTasLink(this%packName, bndArrayPtr, &
                                           this%iprpak, tasName, 'RECHARGE', &
                                           convertFlux, this%nodelist, &
                                           this%parser%iuactive)
          lpos = this%TasManager%CountLinks()
          tasLink => this%TasManager%GetLink(lpos)
          inrech = 2
        else
          !
          ! -- Read the recharge array, then indicate
          !    that recharge was read by setting inrech
          call this%dis%read_layer_array(this%nodelist, this%bound, &
                                         this%ncolbnd, this%maxbound, 1, &
                                         aname(2), this%parser%iuactive, &
                                         this%iout)
          inrech = 1
        end if
        !
      case ('IRCH')
        !
        ! -- Check to see if other variables have already been read.  If so,
        !    then terminate with an error that IRCH must be read first.
        if (ivarsread > 0) then
          call store_error('IRCH IS NOT FIRST VARIABLE IN &
            &PERIOD BLOCK OR IT IS SPECIFIED MORE THAN ONCE.')
          call this%parser%StoreErrorUnit()
        end if
        !
        ! -- Read the IRCH array
        call this%dis%nlarray_to_nodelist(this%nodelist, this%maxbound, &
                                          this%nbound, aname(1), &
                                          this%parser%iuactive, this%iout)
        !
        ! -- set flag to indicate that irch array has been read
        this%inirch = 1
        !
        ! -- if fixed_cell option not set, then need to store nodelist
        !    in the nodesontop array
        if (.not. this%fixed_cell) call this%set_nodesontop()
        !
      case default
        !
        ! -- Check for auxname, and if found, then read into auxvar array
        found = .false.
        ipos = ifind(this%auxname, keyword)
        if (ipos > 0) then
          found = .true.
          atemp = keyword
          !
          ! -- Look for keyword TIMEARRAYSERIES and time-array series
          !    name on line, following auxname
          call this%parser%GetStringCaps(keyword)
          if (keyword == 'TIMEARRAYSERIES') then
            ! -- Get time-array series name
            call this%parser%GetStringCaps(tasName)
            jauxcol = jauxcol + 1
            auxArrayPtr => this%auxvar(jauxcol, :)
            ! Make a time-array-series link and add it to the list of links
            ! contained in the TimeArraySeriesManagerType object.
            convertflux = .false.
            call this%TasManager%MakeTasLink(this%packName, auxArrayPtr, &
                                             this%iprpak, tasName, &
                                             this%auxname(ipos), convertFlux, &
                                             this%nodelist, &
                                             this%parser%iuactive)
          else
            !
            ! -- Read the aux variable array
            call this%dis%read_layer_array(this%nodelist, this%auxvar, &
                                           this%naux, this%maxbound, ipos, &
                                           atemp, this%parser%iuactive, this%iout)
          end if
        end if
        !
        ! -- Nothing found
        if (.not. found) then
          call this%parser%GetCurrentLine(line)
          errmsg = 'LOOKING FOR VALID VARIABLE NAME.  FOUND: '//trim(line)
          call store_error(errmsg)
          call this%parser%StoreErrorUnit()
        end if
        !
        ! -- If this aux variable has been designated as a multiplier array
        !    by presence of AUXMULTNAME, set local pointer appropriately.
        if (this%iauxmultcol > 0 .and. this%iauxmultcol == ipos) then
          auxMultArray => this%auxvar(this%iauxmultcol, :)
        end if
      end select
      !
      ! -- Increment the number of variables read
      ivarsread = ivarsread + 1
      !
    end do
    !
    ! -- If the multiplier-array pointer has been assigned and
    !    stress is controlled by a time-array series, assign
    !    multiplier-array pointer in time-array series link.
    if (associated(auxMultArray)) then
      if (associated(tasLink)) then
        tasLink%RMultArray => auxMultArray
      end if
    end if
    !
    ! -- If recharge was read and auxmultcol was specified, then multiply
    !    the recharge rate by the multplier column
    if (inrech == 1 .and. this%iauxmultcol > 0) then
      write (this%iout, fmtrchauxmult) this%auxname(this%iauxmultcol)
      do n = 1, this%nbound
        this%bound(this%iscloc, n) = this%bound(this%iscloc, n) * &
                                     this%auxvar(this%iauxmultcol, n)
      end do
    end if
    !
    return
  end subroutine rch_rp_array

  subroutine rch_rp_list(this, inrech)
! ******************************************************************************
! rch_rp_list -- Read and Prepare Recharge as a list
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    implicit none
    ! -- dummy
    class(RchType), intent(inout) :: this
    integer(I4B), intent(inout) :: inrech
    ! -- local
    integer(I4B) :: maxboundorig, nlist
    !
! ------------------------------------------------------------------------------
    !
    ! -- initialize
    nlist = -1
    maxboundorig = this%maxbound
    !
    ! -- read the list of recharge values; scale the recharge by auxmultcol
    !    if it is specified.
    call this%dis%read_list(this%parser%iuactive, this%iout, this%iprpak, &
                            nlist, this%inamedbound, this%iauxmultcol, &
                            this%nodelist, this%bound, this%auxvar, &
                            this%auxname, this%boundname, this%listlabel, &
                            this%packName, this%tsManager, this%iscloc, &
                            this%indxconvertflux)
    this%nbound = nlist
    if (this%maxbound > maxboundorig) then
      ! -- The arrays that belong to BndType have been extended.
      ! Now, RCH array nodesontop needs to be recreated.
      if (associated(this%nodesontop)) then
        deallocate (this%nodesontop)
      end if
    end if
    if (.not. this%fixed_cell) call this%set_nodesontop()
    inrech = 1
    !
    ! -- terminate the period block
    call this%parser%terminateblock()
    !
    return
  end subroutine rch_rp_list

  subroutine set_nodesontop(this)
! ******************************************************************************
! set_nodesontop -- store nodelist in nodesontop
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    implicit none
    ! -- dummy
    class(RchType), intent(inout) :: this
    ! -- local
    integer(I4B) :: n
    ! -- formats
! ------------------------------------------------------------------------------
    !
    ! -- allocate if necessary
    if (.not. associated(this%nodesontop)) then
      allocate (this%nodesontop(this%maxbound))
    end if
    !
    ! -- copy nodelist into nodesontop
    do n = 1, this%nbound
      this%nodesontop(n) = this%nodelist(n)
    end do
    !
    ! -- return
    return
  end subroutine set_nodesontop

  subroutine rch_cf(this, reset_mover)
! ******************************************************************************
! rch_cf -- Formulate the HCOF and RHS terms
! Subroutine: (1) skip if no recharge
!             (2) calculate hcof and rhs
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(rchtype) :: this
    logical, intent(in), optional :: reset_mover
    ! -- local
    integer(I4B) :: i, node
! ------------------------------------------------------------------------------
    !
    ! -- Return if no recharge
    if (this%nbound == 0) return
    !
    ! -- Calculate hcof and rhs for each recharge entry
    do i = 1, this%nbound
      !
      ! -- Find the node number
      if (this%fixed_cell) then
        node = this%nodelist(i)
      else
        node = this%nodesontop(i)
      end if
      !
      ! -- cycle if nonexistent bound
      if (node <= 0) then
        this%hcof(i) = DZERO
        this%rhs(i) = DZERO
        cycle
      end if
      !
      ! -- reset nodelist to highest active
      if (.not. this%fixed_cell) then
        if (this%ibound(node) == 0) &
          call this%dis%highest_active(node, this%ibound)
        this%nodelist(i) = node
      end if
      !
      ! -- Set rhs and hcof
      this%hcof(i) = DZERO
      this%rhs(i) = -this%bound(1, i)
      if (this%ibound(node) <= 0) then
        this%rhs(i) = DZERO
        cycle
      end if
      if (this%ibound(node) == 10000) then
        this%rhs(i) = DZERO
        cycle
      end if
    end do
    !
    ! -- return
    return
  end subroutine rch_cf

  subroutine rch_fc(this, rhs, ia, idxglo, amatsln)
! **************************************************************************
! rch_fc -- Copy rhs and hcof into solution rhs and amat
! **************************************************************************
!
!    SPECIFICATIONS:
! --------------------------------------------------------------------------
    ! -- dummy
    class(RchType) :: this
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
      if (n <= 0) cycle
      ! -- reset hcof and rhs for excluded cells
      if (this%ibound(n) == 10000) then
        this%hcof(i) = DZERO
        this%rhs(i) = DZERO
        cycle
      end if
      rhs(n) = rhs(n) + this%rhs(i)
      ipos = ia(n)
      amatsln(idxglo(ipos)) = amatsln(idxglo(ipos)) + this%hcof(i)
    end do
    !
    ! -- return
    return
  end subroutine rch_fc

  subroutine rch_da(this)
! ******************************************************************************
! rch_da -- deallocate
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryManagerModule, only: mem_deallocate
    ! -- dummy
    class(RchType) :: this
! ------------------------------------------------------------------------------
    !
    ! -- Deallocate parent package
    call this%BndType%bnd_da()
    !
    ! -- scalars
    call mem_deallocate(this%inirch)
    !
    ! -- arrays
    if (associated(this%nodesontop)) deallocate (this%nodesontop)
    !
    ! -- return
    return
  end subroutine rch_da

  subroutine rch_define_listlabel(this)
! ******************************************************************************
! define_listlabel -- Define the list heading that is written to iout when
!   PRINT_INPUT option is used.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    class(RchType), intent(inout) :: this
! ------------------------------------------------------------------------------
    !
    ! -- create the header list label
    this%listlabel = trim(this%filtyp)//' NO.'
    if (this%dis%ndim == 3) then
      write (this%listlabel, '(a, a7)') trim(this%listlabel), 'LAYER'
      write (this%listlabel, '(a, a7)') trim(this%listlabel), 'ROW'
      write (this%listlabel, '(a, a7)') trim(this%listlabel), 'COL'
    elseif (this%dis%ndim == 2) then
      write (this%listlabel, '(a, a7)') trim(this%listlabel), 'LAYER'
      write (this%listlabel, '(a, a7)') trim(this%listlabel), 'CELL2D'
    else
      write (this%listlabel, '(a, a7)') trim(this%listlabel), 'NODE'
    end if
    write (this%listlabel, '(a, a16)') trim(this%listlabel), 'RECHARGE'
!    if(this%multindex > 0) &
!      write(this%listlabel, '(a, a16)') trim(this%listlabel), 'MULTIPLIER'
    if (this%inamedbound == 1) then
      write (this%listlabel, '(a, a16)') trim(this%listlabel), 'BOUNDARY NAME'
    end if
    !
    ! -- return
    return
  end subroutine rch_define_listlabel

  subroutine default_nodelist(this)
! ******************************************************************************
! default_nodelist -- Assign default nodelist when READASARRAYS is specified.
!                     Equivalent to reading IRCH as CONSTANT 1
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use InputOutputModule, only: get_node
    use SimModule, only: store_error
    use ConstantsModule, only: LINELENGTH
    ! -- dummy
    class(RchType) :: this
    ! -- local
    integer(I4B) :: il, ir, ic, ncol, nrow, nlay, nodeu, noder, ipos
! ------------------------------------------------------------------------------
    !
    ! -- set variables
    if (this%dis%ndim == 3) then
      nlay = this%dis%mshape(1)
      nrow = this%dis%mshape(2)
      ncol = this%dis%mshape(3)
    elseif (this%dis%ndim == 2) then
      nlay = this%dis%mshape(1)
      nrow = 1
      ncol = this%dis%mshape(2)
    end if
    !
    ! -- Populate nodelist
    ipos = 1
    il = 1
    do ir = 1, nrow
      do ic = 1, ncol
        nodeu = get_node(il, ir, ic, nlay, nrow, ncol)
        noder = this%dis%get_nodenumber(nodeu, 0)
        this%nodelist(ipos) = noder
        ipos = ipos + 1
      end do
    end do
    !
    ! Set flag that indicates IRCH has been assigned, and assign nbound.
    this%inirch = 1
    this%nbound = ipos - 1
    !
    ! -- if fixed_cell option not set, then need to store nodelist
    !    in the nodesontop array
    if (.not. this%fixed_cell) call this%set_nodesontop()
    !
    ! -- return
  end subroutine default_nodelist

  ! -- Procedures related to observations
  logical function rch_obs_supported(this)
    ! ******************************************************************************
    ! rch_obs_supported
    !   -- Return true because RCH package supports observations.
    !   -- Overrides BndType%bnd_obs_supported()
    ! ******************************************************************************
    !
    !    SPECIFICATIONS:
    ! ------------------------------------------------------------------------------
    implicit none
    class(RchType) :: this
    ! ------------------------------------------------------------------------------
    rch_obs_supported = .true.
    !
    ! -- return
    return
  end function rch_obs_supported

  subroutine rch_df_obs(this)
    ! ******************************************************************************
    ! rch_df_obs (implements bnd_df_obs)
    !   -- Store observation type supported by RCH package.
    !   -- Overrides BndType%bnd_df_obs
    ! ******************************************************************************
    !
    !    SPECIFICATIONS:
    ! ------------------------------------------------------------------------------
    implicit none
    ! -- dummy
    class(RchType) :: this
    ! -- local
    integer(I4B) :: indx
    ! ------------------------------------------------------------------------------
    call this%obs%StoreObsType('rch', .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => DefaultObsIdProcessor
    !
    ! -- return
    return
  end subroutine rch_df_obs

  !
  ! -- Procedure related to time series
  subroutine rch_rp_ts(this)
    ! -- Assign tsLink%Text appropriately for
    !    all time series in use by package.
    !    In RCH package only the RECHARGE variable
    !    can be controlled by time series.
    ! -- dummy
    class(RchType), intent(inout) :: this
    ! -- local
    integer(I4B) :: i, nlinks
    type(TimeSeriesLinkType), pointer :: tslink => null()
    !
    nlinks = this%TsManager%boundtslinks%Count()
    do i = 1, nlinks
      tslink => GetTimeSeriesLinkFromList(this%TsManager%boundtslinks, i)
      if (associated(tslink)) then
        select case (tslink%JCol)
        case (1)
          tslink%Text = 'RECHARGE'
        end select
      end if
    end do
    !
    return
  end subroutine rch_rp_ts

end module RchModule

