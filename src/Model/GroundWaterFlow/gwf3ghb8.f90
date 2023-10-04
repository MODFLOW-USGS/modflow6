module ghbmodule
  use KindModule, only: DP, I4B
  use ConstantsModule, only: DZERO, LENFTYPE, LENPACKAGENAME
  use SimVariablesModule, only: errmsg
  use SimModule, only: count_errors, store_error, store_error_filename
  use MemoryHelperModule, only: create_mem_path
  use BndModule, only: BndType
  use BndExtModule, only: BndExtType
  use ObsModule, only: DefaultObsIdProcessor
  use MatrixBaseModule
  !
  implicit none
  !
  private
  public :: ghb_create
  public :: GhbType
  !
  character(len=LENFTYPE) :: ftype = 'GHB'
  character(len=LENPACKAGENAME) :: text = '             GHB'
  !
  type, extends(BndExtType) :: GhbType
    real(DP), dimension(:), pointer, contiguous :: bhead => null() !< GHB boundary head
    real(DP), dimension(:), pointer, contiguous :: cond => null() !< GHB hydraulic conductance
  contains
    procedure :: allocate_arrays => ghb_allocate_arrays
    procedure :: source_options => ghb_options
    procedure :: log_ghb_options
    procedure :: bnd_rp => ghb_rp
    procedure :: bnd_ck => ghb_ck
    procedure :: bnd_cf => ghb_cf
    procedure :: bnd_fc => ghb_fc
    procedure :: bnd_da => ghb_da
    procedure :: define_listlabel
    procedure :: bound_value => ghb_bound_value
    procedure :: cond_mult
    ! -- methods for observations
    procedure, public :: bnd_obs_supported => ghb_obs_supported
    procedure, public :: bnd_df_obs => ghb_df_obs
    procedure, public :: ghb_store_user_cond
  end type GhbType

contains

  subroutine ghb_create(packobj, id, ibcnum, inunit, iout, namemodel, pakname, &
                        mempath)
! ******************************************************************************
! ghb_create -- Create a New Ghb Package
! Subroutine: (1) create new-style package
!             (2) point bndobj to the new package
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
    type(GhbType), pointer :: ghbobj
! ------------------------------------------------------------------------------
    !
    ! -- allocate the object and assign values to object variables
    allocate (ghbobj)
    packobj => ghbobj
    !
    ! -- create name and memory path
    call packobj%set_names(ibcnum, namemodel, pakname, ftype, mempath)
    packobj%text = text
    !
    ! -- allocate scalars
    call packobj%allocate_scalars()
    !
    ! -- initialize package
    call packobj%pack_initialize()
    !
    packobj%inunit = inunit
    packobj%iout = iout
    packobj%id = id
    packobj%ibcnum = ibcnum
    packobj%ncolbnd = 2
    packobj%iscloc = 2
    packobj%ictMemPath = create_mem_path(namemodel, 'NPF')
    !
    ! -- return
    return
  end subroutine ghb_create

  subroutine ghb_da(this)
! ******************************************************************************
! ghb_da -- deallocate
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryManagerModule, only: mem_deallocate
    ! -- dummy
    class(GhbType) :: this
! ------------------------------------------------------------------------------
    !
    ! -- Deallocate parent package
    call this%BndExtType%bnd_da()
    !
    ! -- arrays
    call mem_deallocate(this%bhead, 'BHEAD', this%memoryPath)
    call mem_deallocate(this%cond, 'COND', this%memoryPath)
    !
    ! -- return
    return
  end subroutine ghb_da

  subroutine ghb_options(this)
! ******************************************************************************
! ghb_options -- set options specific to GhbType
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use MemoryManagerExtModule, only: mem_set_value
    use CharacterStringModule, only: CharacterStringType
    use GwfGhbInputModule, only: GwfGhbParamFoundType
    ! -- dummy
    class(GhbType), intent(inout) :: this
    ! -- local
    type(GwfGhbParamFoundType) :: found
! ------------------------------------------------------------------------------
    !
    ! -- source base class options
    call this%BndExtType%source_options()
    !
    ! -- source options from input context
    call mem_set_value(this%imover, 'MOVER', this%input_mempath, found%mover)
    !
    ! -- log ghb specific options
    call this%log_ghb_options(found)
    !
    ! -- return
    return
  end subroutine ghb_options

  subroutine log_ghb_options(this, found)
! ******************************************************************************
! log_ghb_options -- log options specific to GhbType
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use GwfGhbInputModule, only: GwfGhbParamFoundType
    ! -- dummy variables
    class(GhbType), intent(inout) :: this !< BndExtType object
    type(GwfGhbParamFoundType), intent(in) :: found
    ! -- local variables
    ! -- format
    !
    ! -- log found options
    write (this%iout, '(/1x,a)') 'PROCESSING '//trim(adjustl(this%text)) &
      //' OPTIONS'
    !
    if (found%mover) then
      write (this%iout, '(4x,A)') 'MOVER OPTION ENABLED'
    end if
    !
    ! -- close logging block
    write (this%iout, '(1x,a)') &
      'END OF '//trim(adjustl(this%text))//' OPTIONS'
    !
    ! -- return
    return
  end subroutine log_ghb_options

  subroutine ghb_allocate_arrays(this, nodelist, auxvar)
! ******************************************************************************
! ghb_allocate_arrays -- allocate arrays
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryManagerModule, only: mem_allocate, mem_setptr, mem_checkin
    ! -- dummy
    class(GhbType) :: this
    integer(I4B), dimension(:), pointer, contiguous, optional :: nodelist
    real(DP), dimension(:, :), pointer, contiguous, optional :: auxvar
    ! -- local
! ------------------------------------------------------------------------------
    !
    ! -- call base type allocate arrays
    call this%BndExtType%allocate_arrays(nodelist, auxvar)
    !
    ! -- set ghb input context pointers
    call mem_setptr(this%bhead, 'BHEAD', this%input_mempath)
    call mem_setptr(this%cond, 'COND', this%input_mempath)
    !
    ! --checkin ghb input context pointers
    call mem_checkin(this%bhead, 'BHEAD', this%memoryPath, &
                     'BHEAD', this%input_mempath)
    call mem_checkin(this%cond, 'COND', this%memoryPath, &
                     'COND', this%input_mempath)
    !
    ! -- return
    return
  end subroutine ghb_allocate_arrays

  subroutine ghb_rp(this)
! ******************************************************************************
! ghb_rp -- Read and prepare
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use TdisModule, only: kper
    ! -- dummy
    class(GhbType), intent(inout) :: this
    ! -- local
! ------------------------------------------------------------------------------
    if (this%iper /= kper) return
    !
    ! -- Call the parent class read and prepare
    call this%BndExtType%bnd_rp()
    !
    ! -- store user cond
    if (this%ivsc == 1) then
      call this%ghb_store_user_cond()
    end if
    !
    ! -- Write the list to iout if requested
    if (this%iprpak /= 0) then
      call this%write_list()
    end if
    !
    ! -- return
    return
  end subroutine ghb_rp

  subroutine ghb_ck(this)
! ******************************************************************************
! ghb_ck -- Check ghb boundary condition data
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use ConstantsModule, only: LINELENGTH
    use SimModule, only: store_error, count_errors, store_error_unit
    ! -- dummy
    class(GhbType), intent(inout) :: this
    ! -- local
    character(len=LINELENGTH) :: errmsg
    integer(I4B) :: i
    integer(I4B) :: node
    real(DP) :: bt
    ! -- formats
    character(len=*), parameter :: fmtghberr = &
      "('GHB BOUNDARY (',i0,') HEAD (',f10.3,') IS LESS THAN CELL &
      &BOTTOM (',f10.3,')')"
! ------------------------------------------------------------------------------
    !
    ! -- check stress period data
    do i = 1, this%nbound
      node = this%nodelist(i)
      bt = this%dis%bot(node)
      ! -- accumulate errors
      if (this%bhead(i) < bt .and. this%icelltype(node) /= 0) then
        write (errmsg, fmt=fmtghberr) i, this%bhead(i), bt
        call store_error(errmsg)
      end if
    end do
    !
    !write summary of ghb package error messages
    if (count_errors() > 0) then
      call store_error_unit(this%inunit)
    end if
    !
    ! -- return
    return
  end subroutine ghb_ck

  subroutine ghb_cf(this, reset_mover)
! ******************************************************************************
! ghb_cf -- Formulate the HCOF and RHS terms
! Subroutine: (1) skip if no ghbs
!             (2) calculate hcof and rhs
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(GhbType) :: this
    logical, intent(in), optional :: reset_mover
    ! -- local
    integer(I4B) :: i, node
    logical :: lrm
! ------------------------------------------------------------------------------
    !
    ! -- Return if no ghbs
    if (this%nbound .eq. 0) return
    !
    ! -- packmvrobj cf
    lrm = .true.
    if (present(reset_mover)) lrm = reset_mover
    if (this%imover == 1 .and. lrm) then
      call this%pakmvrobj%cf()
    end if
    !
    ! -- Calculate hcof and rhs for each ghb entry
    do i = 1, this%nbound
      node = this%nodelist(i)
      if (this%ibound(node) .le. 0) then
        this%hcof(i) = DZERO
        this%rhs(i) = DZERO
        cycle
      end if
      this%hcof(i) = -this%cond_mult(i)
      this%rhs(i) = -this%cond_mult(i) * this%bhead(i)
    end do
    !
    ! -- return
    return
  end subroutine ghb_cf

  subroutine ghb_fc(this, rhs, ia, idxglo, matrix_sln)
! **************************************************************************
! ghb_fc -- Copy rhs and hcof into solution rhs and amat
! **************************************************************************
!
!    SPECIFICATIONS:
! --------------------------------------------------------------------------
    ! -- dummy
    class(GhbType) :: this
    real(DP), dimension(:), intent(inout) :: rhs
    integer(I4B), dimension(:), intent(in) :: ia
    integer(I4B), dimension(:), intent(in) :: idxglo
    class(MatrixBaseType), pointer :: matrix_sln
    ! -- local
    integer(I4B) :: i, n, ipos
    real(DP) :: cond, bhead, qghb
! --------------------------------------------------------------------------
    !
    ! -- pakmvrobj fc
    if (this%imover == 1) then
      call this%pakmvrobj%fc()
    end if
    !
    ! -- Copy package rhs and hcof into solution rhs and amat
    do i = 1, this%nbound
      n = this%nodelist(i)
      rhs(n) = rhs(n) + this%rhs(i)
      ipos = ia(n)
      call matrix_sln%add_value_pos(idxglo(ipos), this%hcof(i))
      !
      ! -- If mover is active and this boundary is discharging,
      !    store available water (as positive value).
      bhead = this%bhead(i)
      if (this%imover == 1 .and. this%xnew(n) > bhead) then
        cond = this%cond_mult(i)
        qghb = cond * (this%xnew(n) - bhead)
        call this%pakmvrobj%accumulate_qformvr(i, qghb)
      end if
    end do
    !
    ! -- return
    return
  end subroutine ghb_fc

  subroutine define_listlabel(this)
! ******************************************************************************
! define_listlabel -- Define the list heading that is written to iout when
!   PRINT_INPUT option is used.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    class(GhbType), intent(inout) :: this
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
    write (this%listlabel, '(a, a16)') trim(this%listlabel), 'STAGE'
    write (this%listlabel, '(a, a16)') trim(this%listlabel), 'CONDUCTANCE'
    if (this%inamedbound == 1) then
      write (this%listlabel, '(a, a16)') trim(this%listlabel), 'BOUNDARY NAME'
    end if
    !
    ! -- return
    return
  end subroutine define_listlabel

  ! -- Procedures related to observations

  logical function ghb_obs_supported(this)
    ! ******************************************************************************
    ! ghb_obs_supported
    !   -- Return true because GHB package supports observations.
    !   -- Overrides BndType%bnd_obs_supported()
    ! ******************************************************************************
    !
    !    SPECIFICATIONS:
    ! ------------------------------------------------------------------------------
    implicit none
    class(GhbType) :: this
    ! ------------------------------------------------------------------------------
    ghb_obs_supported = .true.
    return
  end function ghb_obs_supported

  subroutine ghb_df_obs(this)
    ! ******************************************************************************
    ! ghb_df_obs (implements bnd_df_obs)
    !   -- Store observation type supported by GHB package.
    !   -- Overrides BndType%bnd_df_obs
    ! ******************************************************************************
    !
    !    SPECIFICATIONS:
    ! ------------------------------------------------------------------------------
    implicit none
    ! -- dummy
    class(GhbType) :: this
    ! -- local
    integer(I4B) :: indx
    ! ------------------------------------------------------------------------------
    call this%obs%StoreObsType('ghb', .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => DefaultObsIdProcessor
    !
    ! -- Store obs type and assign procedure pointer
    !    for to-mvr observation type.
    call this%obs%StoreObsType('to-mvr', .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => DefaultObsIdProcessor
    !
    ! -- return
    return
  end subroutine ghb_df_obs

  subroutine ghb_store_user_cond(this)
    ! -- modules
    ! -- dummy variables
    class(GhbType), intent(inout) :: this !< BndExtType object
    ! -- local variables
    integer(I4B) :: n
    !
    ! -- store backup copy of conductance values
    do n = 1, this%nbound
      this%condinput(n) = this%cond_mult(n)
    end do
    !
    ! -- return
    return
  end subroutine ghb_store_user_cond

  function cond_mult(this, row) result(cond)
    ! -- modules
    use ConstantsModule, only: DZERO
    ! -- dummy variables
    class(GhbType), intent(inout) :: this !< BndExtType object
    integer(I4B), intent(in) :: row
    ! -- result
    real(DP) :: cond
    !
    if (this%iauxmultcol > 0) then
      cond = this%cond(row) * this%auxvar(this%iauxmultcol, row)
    else
      cond = this%cond(row)
    end if
    !
    ! -- return
    return
  end function cond_mult

! ******************************************************************************
! ghb_bound_value -- return requested boundary value
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
  function ghb_bound_value(this, col, row) result(bndval)
    ! -- modules
    use ConstantsModule, only: DZERO
    ! -- dummy variables
    class(GhbType), intent(inout) :: this !< BndExtType object
    integer(I4B), intent(in) :: col
    integer(I4B), intent(in) :: row
    ! -- result
    real(DP) :: bndval
    !
    select case (col)
    case (1)
      bndval = this%bhead(row)
    case (2)
      bndval = this%cond_mult(row)
    case default
      errmsg = 'Programming error. GHB bound value requested column '&
               &'outside range of ncolbnd (2).'
      call store_error(errmsg)
      call store_error_filename(this%input_fname)
    end select
    !
    ! -- return
    return
  end function ghb_bound_value

end module ghbmodule
