module rivmodule
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
  public :: riv_create
  public :: RivType
  !
  character(len=LENFTYPE) :: ftype = 'RIV'
  character(len=LENPACKAGENAME) :: text = '             RIV'
  !
  type, extends(BndExtType) :: RivType
    real(DP), dimension(:), pointer, contiguous :: stage => null() !< RIV head
    real(DP), dimension(:), pointer, contiguous :: cond => null() !< RIV bed hydraulic conductance
    real(DP), dimension(:), pointer, contiguous :: rbot => null() !< RIV bed bottom elevation
  contains
    procedure :: allocate_arrays => riv_allocate_arrays
    procedure :: source_options => riv_options
    procedure :: log_riv_options
    procedure :: bnd_rp => riv_rp
    procedure :: bnd_ck => riv_ck
    procedure :: bnd_cf => riv_cf
    procedure :: bnd_fc => riv_fc
    procedure :: bnd_da => riv_da
    procedure :: define_listlabel
    procedure :: bound_value => riv_bound_value
    procedure :: cond_mult
    ! -- methods for observations
    procedure, public :: bnd_obs_supported => riv_obs_supported
    procedure, public :: bnd_df_obs => riv_df_obs
    procedure, public :: riv_store_user_cond
  end type RivType

contains

  subroutine riv_create(packobj, id, ibcnum, inunit, iout, namemodel, pakname, &
                        mempath)
! ******************************************************************************
! riv_create -- Create a New Riv Package
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
    type(RivType), pointer :: rivobj
! ------------------------------------------------------------------------------
    !
    ! -- allocate the object and assign values to object variables
    allocate (rivobj)
    packobj => rivobj
    !
    ! -- create name and memory path
    call packobj%set_names(ibcnum, namemodel, pakname, ftype, mempath)
    packobj%text = text
    !
    ! -- allocate scalars
    call rivobj%allocate_scalars()
    !
    ! -- initialize package
    call packobj%pack_initialize()

    packobj%inunit = inunit
    packobj%iout = iout
    packobj%id = id
    packobj%ibcnum = ibcnum
    packobj%ncolbnd = 3 ! stage, conductance, rbot
    packobj%iscloc = 2 !sfac applies to conductance
    packobj%ictMemPath = create_mem_path(namemodel, 'NPF')
    !
    ! -- return
    return
  end subroutine riv_create

  subroutine riv_da(this)
! ******************************************************************************
! riv_da -- deallocate
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryManagerModule, only: mem_deallocate
    ! -- dummy
    class(RivType) :: this
! ------------------------------------------------------------------------------
    !
    ! -- Deallocate parent package
    call this%BndExtType%bnd_da()
    !
    ! -- arrays
    call mem_deallocate(this%stage, 'STAGE', this%memoryPath)
    call mem_deallocate(this%cond, 'COND', this%memoryPath)
    call mem_deallocate(this%rbot, 'RBOT', this%memoryPath)
    !
    ! -- return
    return
  end subroutine riv_da

  subroutine riv_options(this)
! ******************************************************************************
! riv_options -- set options specific to RivType
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use MemoryManagerExtModule, only: mem_set_value
    use CharacterStringModule, only: CharacterStringType
    use GwfRivInputModule, only: GwfRivParamFoundType
    ! -- dummy
    class(RivType), intent(inout) :: this
    ! -- local
    type(GwfRivParamFoundType) :: found
! ------------------------------------------------------------------------------
    !
    ! -- source base class options
    call this%BndExtType%source_options()
    !
    ! -- source options from input context
    call mem_set_value(this%imover, 'MOVER', this%input_mempath, found%mover)
    !
    ! -- log riv specific options
    call this%log_riv_options(found)
    !
    ! -- return
    return
  end subroutine riv_options

  subroutine log_riv_options(this, found)
! ******************************************************************************
! log_riv_options -- log options specific to RivType
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use GwfRivInputModule, only: GwfRivParamFoundType
    ! -- dummy variables
    class(RivType), intent(inout) :: this !< BndExtType object
    type(GwfRivParamFoundType), intent(in) :: found
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
  end subroutine log_riv_options

  subroutine riv_allocate_arrays(this, nodelist, auxvar)
! ******************************************************************************
! riv_allocate_arrays -- allocate arrays
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryManagerModule, only: mem_allocate, mem_setptr, mem_checkin
    ! -- dummy
    class(RivType) :: this
    integer(I4B), dimension(:), pointer, contiguous, optional :: nodelist
    real(DP), dimension(:, :), pointer, contiguous, optional :: auxvar
    ! -- local
! ------------------------------------------------------------------------------
    !
    ! -- call base type allocate arrays
    call this%BndExtType%allocate_arrays(nodelist, auxvar)
    !
    ! -- set riv input context pointers
    call mem_setptr(this%stage, 'STAGE', this%input_mempath)
    call mem_setptr(this%cond, 'COND', this%input_mempath)
    call mem_setptr(this%rbot, 'RBOT', this%input_mempath)
    !
    ! --checkin riv input context pointers
    call mem_checkin(this%stage, 'STAGE', this%memoryPath, &
                     'STAGE', this%input_mempath)
    call mem_checkin(this%cond, 'COND', this%memoryPath, &
                     'COND', this%input_mempath)
    call mem_checkin(this%rbot, 'RBOT', this%memoryPath, &
                     'RBOT', this%input_mempath)
    !
    ! -- return
    return
  end subroutine riv_allocate_arrays

  subroutine riv_rp(this)
! ******************************************************************************
! riv_rp -- Read and prepare
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use TdisModule, only: kper
    ! -- dummy
    class(RivType), intent(inout) :: this
    ! -- local
! ------------------------------------------------------------------------------
    if (this%iper /= kper) return
    !
    ! -- Call the parent class read and prepare
    call this%BndExtType%bnd_rp()
    !
    ! -- store user cond
    if (this%ivsc == 1) then
      call this%riv_store_user_cond()
    end if
    !
    ! -- Write the list to iout if requested
    if (this%iprpak /= 0) then
      call this%write_list()
    end if
    !
    ! -- return
    return
  end subroutine riv_rp

  subroutine riv_ck(this)
! ******************************************************************************
! riv_ck -- Check river boundary condition data
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use ConstantsModule, only: LINELENGTH
    use SimModule, only: store_error, count_errors, store_error_unit
    ! -- dummy
    class(RivType), intent(inout) :: this
    ! -- local
    character(len=LINELENGTH) :: errmsg
    integer(I4B) :: i
    integer(I4B) :: node
    real(DP) :: bt
    real(DP) :: stage
    real(DP) :: rbot
    ! -- formats
    character(len=*), parameter :: fmtriverr = &
      "('RIV BOUNDARY (',i0,') RIVER BOTTOM (',f10.4,') IS LESS &
      &THAN CELL BOTTOM (',f10.4,')')"
    character(len=*), parameter :: fmtriverr2 = &
      "('RIV BOUNDARY (',i0,') RIVER STAGE (',f10.4,') IS LESS &
      &THAN RIVER BOTTOM (',f10.4,')')"
    character(len=*), parameter :: fmtriverr3 = &
      "('RIV BOUNDARY (',i0,') RIVER STAGE (',f10.4,') IS LESS &
      &THAN CELL BOTTOM (',f10.4,')')"
! ------------------------------------------------------------------------------
    !
    ! -- check stress period data
    do i = 1, this%nbound
      node = this%nodelist(i)
      bt = this%dis%bot(node)
      stage = this%stage(i)
      rbot = this%rbot(i)
      ! -- accumulate errors
      if (rbot < bt .and. this%icelltype(node) /= 0) then
        write (errmsg, fmt=fmtriverr) i, rbot, bt
        call store_error(errmsg)
      end if
      if (stage < rbot) then
        write (errmsg, fmt=fmtriverr2) i, stage, rbot
        call store_error(errmsg)
      end if
      if (stage < bt .and. this%icelltype(node) /= 0) then
        write (errmsg, fmt=fmtriverr3) i, stage, bt
        call store_error(errmsg)
      end if
    end do
    !
    ! -- write summary of river package error messages
    if (count_errors() > 0) then
      call store_error_unit(this%inunit)
    end if
    !
    ! -- return
    return
  end subroutine riv_ck

  subroutine riv_cf(this)
! ******************************************************************************
! riv_cf -- Formulate the HCOF and RHS terms
! Subroutine: (1) skip in no rivs
!             (2) calculate hcof and rhs
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(RivType) :: this
    ! -- local
    integer(I4B) :: i, node
    real(DP) :: hriv, criv, rbot
! ------------------------------------------------------------------------------
    !
    ! -- Return if no rivs
    if (this%nbound .eq. 0) return
    !
    ! -- Calculate hcof and rhs for each riv entry
    do i = 1, this%nbound
      node = this%nodelist(i)
      if (this%ibound(node) <= 0) then
        this%hcof(i) = DZERO
        this%rhs(i) = DZERO
        cycle
      end if
      hriv = this%stage(i)
      criv = this%cond_mult(i)
      rbot = this%rbot(i)
      if (this%xnew(node) <= rbot) then
        this%rhs(i) = -criv * (hriv - rbot)
        this%hcof(i) = DZERO
      else
        this%rhs(i) = -criv * hriv
        this%hcof(i) = -criv
      end if
    end do
    !
    ! -- return
    return
  end subroutine riv_cf

  subroutine riv_fc(this, rhs, ia, idxglo, matrix_sln)
! **************************************************************************
! riv_fc -- Copy rhs and hcof into solution rhs and amat
! **************************************************************************
!
!    SPECIFICATIONS:
! --------------------------------------------------------------------------
    ! -- dummy
    class(RivType) :: this
    real(DP), dimension(:), intent(inout) :: rhs
    integer(I4B), dimension(:), intent(in) :: ia
    integer(I4B), dimension(:), intent(in) :: idxglo
    class(MatrixBaseType), pointer :: matrix_sln
    ! -- local
    integer(I4B) :: i, n, ipos
    real(DP) :: cond, stage, qriv !, rbot
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
      ! -- If mover is active and this river cell is discharging,
      !    store available water (as positive value).
      stage = this%stage(i)
      if (this%imover == 1 .and. this%xnew(n) > stage) then
        cond = this%cond_mult(i)
        qriv = cond * (this%xnew(n) - stage)
        call this%pakmvrobj%accumulate_qformvr(i, qriv)
      end if
    end do
    !
    ! -- return
    return
  end subroutine riv_fc

  subroutine define_listlabel(this)
! ******************************************************************************
! define_listlabel -- Define the list heading that is written to iout when
!   PRINT_INPUT option is used.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    class(RivType), intent(inout) :: this
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
    write (this%listlabel, '(a, a16)') trim(this%listlabel), 'BOTTOM EL.'
    if (this%inamedbound == 1) then
      write (this%listlabel, '(a, a16)') trim(this%listlabel), 'BOUNDARY NAME'
    end if
    !
    ! -- return
    return
  end subroutine define_listlabel

  ! -- Procedures related to observations

  logical function riv_obs_supported(this)
! ******************************************************************************
! riv_obs_supported
!   -- Return true because RIV package supports observations.
!   -- Overrides BndType%bnd_obs_supported()
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    implicit none
    class(RivType) :: this
! ------------------------------------------------------------------------------
    riv_obs_supported = .true.
    return
  end function riv_obs_supported

  subroutine riv_df_obs(this)
    ! ******************************************************************************
    ! riv_df_obs (implements bnd_df_obs)
    !   -- Store observation type supported by RIV package.
    !   -- Overrides BndType%bnd_df_obs
    ! ******************************************************************************
    !
    !    SPECIFICATIONS:
    ! ------------------------------------------------------------------------------
    implicit none
    ! -- dummy
    class(RivType) :: this
    ! -- local
    integer(I4B) :: indx
    ! ------------------------------------------------------------------------------
    call this%obs%StoreObsType('riv', .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => DefaultObsIdProcessor
    !
    ! -- Store obs type and assign procedure pointer
    !    for to-mvr observation type.
    call this%obs%StoreObsType('to-mvr', .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => DefaultObsIdProcessor
    !
    ! -- return
    return
  end subroutine riv_df_obs

  subroutine riv_store_user_cond(this)
    ! -- modules
    ! -- dummy variables
    class(RivType), intent(inout) :: this !< BndExtType object
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
  end subroutine riv_store_user_cond

  function cond_mult(this, row) result(cond)
    ! -- modules
    use ConstantsModule, only: DZERO
    ! -- dummy variables
    class(RivType), intent(inout) :: this !< BndExtType object
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
! riv_bound_value -- return requested boundary value
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
  function riv_bound_value(this, col, row) result(bndval)
    ! -- modules
    use ConstantsModule, only: DZERO
    ! -- dummy variables
    class(RivType), intent(inout) :: this !< BndExtType object
    integer(I4B), intent(in) :: col
    integer(I4B), intent(in) :: row
    ! -- result
    real(DP) :: bndval
    !
    select case (col)
    case (1)
      bndval = this%stage(row)
    case (2)
      bndval = this%cond_mult(row)
    case (3)
      bndval = this%rbot(row)
    case default
      errmsg = 'Programming error. RIV bound value requested column '&
               &'outside range of ncolbnd (3).'
      call store_error(errmsg)
      call store_error_filename(this%input_fname)
    end select
    !
    ! -- return
    return
  end function riv_bound_value

end module rivmodule
