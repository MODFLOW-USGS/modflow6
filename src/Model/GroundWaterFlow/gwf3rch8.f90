module RchModule
  !
  use KindModule, only: DP, I4B, LGP
  use ConstantsModule, only: DZERO, LENFTYPE, LENPACKAGENAME, MAXCHARLEN, &
                             IWETLAKE, LINELENGTH
  use MemoryHelperModule, only: create_mem_path
  use BndModule, only: BndType
  use BndExtModule, only: BndExtType
  use SimModule, only: store_error, store_error_filename
  use SimVariablesModule, only: errmsg
  use ObsModule, only: DefaultObsIdProcessor
  use TimeArraySeriesLinkModule, only: TimeArraySeriesLinkType
  use BlockParserModule, only: BlockParserType
  use CharacterStringModule, only: CharacterStringType
  use MatrixBaseModule
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
  type, extends(BndExtType) :: RchType
    real(DP), dimension(:), pointer, contiguous :: recharge => null() !< boundary recharge array
    integer(I4B), dimension(:), pointer, contiguous :: nodesontop => NULL() ! User provided cell numbers; nodelist is cells where recharge is applied)
    logical, pointer, private :: fixed_cell
    logical, pointer, private :: read_as_arrays
  contains
    procedure :: rch_allocate_scalars
    procedure :: allocate_arrays => rch_allocate_arrays
    procedure :: source_options => rch_source_options
    procedure :: source_dimensions => rch_source_dimensions
    procedure :: log_rch_options
    procedure :: read_initial_attr => rch_read_initial_attr
    procedure :: bnd_rp => rch_rp
    procedure :: bnd_cf => rch_cf
    procedure :: bnd_fc => rch_fc
    procedure :: bnd_da => rch_da
    procedure :: set_nodesontop
    procedure :: define_listlabel => rch_define_listlabel
    procedure :: bound_value => rch_bound_value
    procedure, private :: default_nodelist
    ! -- for observations
    procedure, public :: bnd_obs_supported => rch_obs_supported
    procedure, public :: bnd_df_obs => rch_df_obs
  end type RchType

contains

  subroutine rch_create(packobj, id, ibcnum, inunit, iout, namemodel, pakname, &
                        mempath)
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
    character(len=*), intent(in) :: mempath
    ! -- local
    type(rchtype), pointer :: rchobj
! ------------------------------------------------------------------------------
    !
    ! -- allocate recharge object and scalar variables
    allocate (rchobj)
    packobj => rchobj
    !
    ! -- create name and memory path
    call packobj%set_names(ibcnum, namemodel, pakname, ftype, mempath)
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
    !
    ! -- return
    return
  end subroutine rch_create

  subroutine rch_allocate_scalars(this)
! ******************************************************************************
! rch_allocate_scalars -- allocate scalar members
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(RchType), intent(inout) :: this
! ------------------------------------------------------------------------------
    !
    ! -- allocate base scalars
    call this%BndExtType%allocate_scalars()
    !
    ! -- allocate internal members
    allocate (this%fixed_cell)
    allocate (this%read_as_arrays)
    !
    ! -- Set values
    this%fixed_cell = .false.
    this%read_as_arrays = .false.
    !
    ! -- return
    return
  end subroutine rch_allocate_scalars

  subroutine rch_allocate_arrays(this, nodelist, auxvar)
! ******************************************************************************
! rch_allocate_arrays -- allocate arrays
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryManagerModule, only: mem_setptr, mem_checkin
    ! -- dummy
    class(RchType) :: this
    integer(I4B), dimension(:), pointer, contiguous, optional :: nodelist
    real(DP), dimension(:, :), pointer, contiguous, optional :: auxvar
    ! -- local
! ------------------------------------------------------------------------------
    !
    ! -- allocate base arrays
    call this%BndExtType%allocate_arrays(nodelist, auxvar)
    !
    ! -- set recharge input context pointer
    call mem_setptr(this%recharge, 'RECHARGE', this%input_mempath)
    !
    ! -- checkin recharge input context pointer
    call mem_checkin(this%recharge, 'RECHARGE', this%memoryPath, &
                     'RECHARGE', this%input_mempath)
    !
    ! -- return
    return
  end subroutine rch_allocate_arrays

  subroutine rch_source_options(this)
! ******************************************************************************
! rch_source_options -- source options specific to RchType
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use MemoryManagerExtModule, only: mem_set_value
    use IdmGwfDfnSelectorModule, only: GwfParamFoundType
    implicit none
    ! -- dummy
    class(RchType), intent(inout) :: this
    ! -- local
    type(GwfParamFoundType) :: found
! ------------------------------------------------------------------------------
    !
    ! -- source common bound options
    call this%BndExtType%source_options()
    !
    ! -- update defaults with idm sourced values
    call mem_set_value(this%fixed_cell, 'FIXED_CELL', this%input_mempath, &
                       found%fixed_cell)
    call mem_set_value(this%read_as_arrays, 'READASARRAYS', this%input_mempath, &
                       found%readasarrays)
    !
    if (found%readasarrays) then
      if (this%dis%supports_layers()) then
        this%text = texta
      else
        errmsg = 'READASARRAYS option is not compatible with selected'// &
                 ' discretization type.'
        call store_error(errmsg)
        call store_error_filename(this%input_fname)
      end if
    end if
    !
    ! -- log rch params
    call this%log_rch_options(found)
    !
    ! -- return
    return
  end subroutine rch_source_options

  subroutine log_rch_options(this, found)
! ******************************************************************************
! log_rch_options -- log options specific to RchType
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use IdmGwfDfnSelectorModule, only: GwfParamFoundType
    implicit none
    ! -- dummy
    class(RchType), intent(inout) :: this
    type(GwfParamFoundType), intent(in) :: found
    ! -- local
    ! -- formats
    character(len=*), parameter :: fmtfixedcell = &
      &"(4x, 'RECHARGE WILL BE APPLIED TO SPECIFIED CELL.')"
    character(len=*), parameter :: fmtreadasarrays = &
      &"(4x, 'RECHARGE INPUT WILL BE READ AS ARRAY(S).')"
! ------------------------------------------------------------------------------
    !
    ! -- log found options
    write (this%iout, '(/1x,a)') 'PROCESSING '//trim(adjustl(this%text)) &
      //' OPTIONS'
    !
    if (found%fixed_cell) then
      write (this%iout, fmtfixedcell)
    end if
    !
    if (found%readasarrays) then
      write (this%iout, fmtreadasarrays)
    end if
    !
    ! -- close logging block
    write (this%iout, '(1x,a)') &
      'END OF '//trim(adjustl(this%text))//' OPTIONS'
    !
    ! -- return
    return
  end subroutine log_rch_options

  subroutine rch_source_dimensions(this)
! ******************************************************************************
! rch_source_dimensions -- Source the dimensions for this package
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(RchType), intent(inout) :: this
    ! -- local
    ! -- format
! ------------------------------------------------------------------------------
    !
    ! Dimensions block is not required if:
    !   (1) discretization is DIS or DISV, and
    !   (2) READASARRAYS option has been specified.
    if (this%read_as_arrays) then
      this%maxbound = this%dis%get_ncpl()
      !
      ! -- verify dimensions were set
      if (this%maxbound <= 0) then
        write (errmsg, '(a)') &
          'MAXBOUND must be an integer greater than zero.'
        call store_error(errmsg)
        call store_error_filename(this%input_fname)
      end if
      !
    else
      !
      ! -- source maxbound
      call this%BndExtType%source_dimensions()
    end if
    !
    ! -- Call define_listlabel to construct the list label that is written
    !    when PRINT_INPUT option is used.
    call this%define_listlabel()
    !
    ! -- return
    return
  end subroutine rch_source_dimensions

  subroutine rch_read_initial_attr(this)
! ******************************************************************************
! rch_read_initial_attr -- Part of allocate and read
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
    use TdisModule, only: kper
    implicit none
    ! -- dummy
    class(RchType), intent(inout) :: this
    ! -- local
! ------------------------------------------------------------------------------
    !
    if (this%iper /= kper) return
    !
    if (this%read_as_arrays) then
      !
      ! -- update nodelist based on IRCH input
      call nodelist_update(this%nodelist, this%nbound, this%maxbound, &
                           this%dis, this%input_mempath)
      !
    else
      !
      call this%BndExtType%bnd_rp()
      !
    end if
    !
    ! -- copy nodelist to nodesontop if not fixed cell
    if (.not. this%fixed_cell) call this%set_nodesontop()
    !
    ! -- Write the list to iout if requested
    if (this%iprpak /= 0) then
      call this%write_list()
    end if
    !
    ! -- return
    return
  end subroutine rch_rp

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

  subroutine rch_cf(this)
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
      if (this%iauxmultcol > 0) then
        this%rhs(i) = -this%recharge(i) * this%dis%get_area(node) * &
                      this%auxvar(this%iauxmultcol, i)
      else
        this%rhs(i) = -this%recharge(i) * this%dis%get_area(node)
      end if
      if (this%ibound(node) <= 0) then
        this%rhs(i) = DZERO
        cycle
      end if
      if (this%ibound(node) == IWETLAKE) then
        this%rhs(i) = DZERO
        cycle
      end if
    end do
    !
    ! -- return
    return
  end subroutine rch_cf

  subroutine rch_fc(this, rhs, ia, idxglo, matrix_sln)
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
    class(MatrixBaseType), pointer :: matrix_sln
    ! -- local
    integer(I4B) :: i, n, ipos
! --------------------------------------------------------------------------
    !
    ! -- Copy package rhs and hcof into solution rhs and amat
    do i = 1, this%nbound
      n = this%nodelist(i)
      if (n <= 0) cycle
      ! -- reset hcof and rhs for excluded cells
      if (this%ibound(n) == IWETLAKE) then
        this%hcof(i) = DZERO
        this%rhs(i) = DZERO
        cycle
      end if
      rhs(n) = rhs(n) + this%rhs(i)
      ipos = ia(n)
      call matrix_sln%add_value_pos(idxglo(ipos), this%hcof(i))
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
    call this%BndExtType%bnd_da()
    !
    ! -- scalars
    deallocate (this%fixed_cell)
    deallocate (this%read_as_arrays)
    !
    ! -- arrays
    if (associated(this%nodesontop)) deallocate (this%nodesontop)
    call mem_deallocate(this%recharge, 'RECHARGE', this%memoryPath)
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
    ! -- Assign nbound
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

! ******************************************************************************
! rch_bound_value -- return requested boundary value
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
  function rch_bound_value(this, col, row) result(bndval)
    ! -- modules
    use ConstantsModule, only: DZERO
    ! -- dummy variables
    class(RchType), intent(inout) :: this !< BndExtType object
    integer(I4B), intent(in) :: col
    integer(I4B), intent(in) :: row
    ! -- result
    real(DP) :: bndval
    !
    select case (col)
    case (1)
      if (this%iauxmultcol > 0) then
        bndval = this%recharge(row) * this%auxvar(this%iauxmultcol, row)
      else
        bndval = this%recharge(row)
      end if
    case default
      errmsg = 'Programming error. RCH bound value requested column '&
               &'outside range of ncolbnd (1).'
      call store_error(errmsg)
      call store_error_filename(this%input_fname)
    end select
    !
    ! -- return
    return
  end function rch_bound_value

  !> @brief Update the nodelist based on IRCH input
  !!
  !! This is a module scoped routine to check for IRCH
  !! input. If array input was provided, INIRCH and IRCH
  !! will be allocated in the input context.  If the read
  !! state variable INIRCH is set to 1 during this period
  !! update, IRCH input was read and is used here to update
  !! the nodelist.
  !!
  !<
  subroutine nodelist_update(nodelist, nbound, maxbound, &
                             dis, input_mempath)
    ! -- modules
    use MemoryManagerModule, only: mem_setptr
    use BaseDisModule, only: DisBaseType
    ! -- dummy
    integer(I4B), dimension(:), contiguous, &
      pointer, intent(inout) :: nodelist
    class(DisBaseType), pointer, intent(in) :: dis
    character(len=*), intent(in) :: input_mempath
    integer(I4B), intent(inout) :: nbound
    integer(I4B), intent(in) :: maxbound
    character(len=24) :: aname = '     LAYER OR NODE INDEX'
    ! -- local
    integer(I4B), dimension(:), contiguous, &
      pointer :: irch => null()
    integer(I4B), pointer :: inirch => NULL()
    !
    ! -- set pointer to input context INIRCH
    call mem_setptr(inirch, 'INIRCH', input_mempath)
    !
    ! -- check INIRCH read state
    if (inirch == 1) then
      ! -- irch was read this period
      !
      ! -- set pointer to input context IRCH
      call mem_setptr(irch, 'IRCH', input_mempath)
      !
      ! -- update nodelist
      call dis%nlarray_to_nodelist(irch, nodelist, &
                                   maxbound, nbound, aname)
    end if
    !
    ! -- return
    return
  end subroutine nodelist_update

end module RchModule

