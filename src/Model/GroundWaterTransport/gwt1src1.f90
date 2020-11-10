module GwtSrcModule
  !
  use KindModule, only: DP, I4B
  use ConstantsModule, only: DZERO, DEM1, DONE, LENFTYPE, DP99, LENAUXNAME,    &
                             LENPAKLOC
  use SimVariablesModule, only: errmsg
  use BndModule, only: BndType
  use GwtFmiModule, only: GwtFmiType
  use GwtMstModule, only: GwtMstType
  use ObsModule, only: DefaultObsIdProcessor
  use TimeSeriesLinkModule, only: TimeSeriesLinkType, &
                                  GetTimeSeriesLinkFromList
  use BlockParserModule, only: BlockParserType
  !
  implicit none
  !
  private
  public :: src_create
  !
  character(len=LENFTYPE) :: ftype = 'SRC'
  character(len=16)       :: text  = '     MASS SOURCE'
  !
  type, extends(BndType) :: GwtSrcType
    type(GwtFmiType), pointer                        :: fmi          => null()           ! pointer to fmi object
    type(GwtMstType), pointer                        :: mst          => null()           ! pointer to mst object
    integer(I4B), pointer                            :: iauxconstr   => null()           ! auxiliary column number of concentration constraint
    real(DP), dimension(:,:), contiguous, pointer    :: qrold        => null()           ! stored q and residual iterates for constraint option
  contains
    procedure :: allocate_scalars => src_allocate_scalars
    procedure :: src_allocate_arrays
    procedure :: bnd_ar => src_ar
    procedure :: bnd_cf => src_cf
    procedure :: bnd_fc => src_fc
    procedure :: bnd_cc => src_cc
    procedure :: bnd_da => src_da
    procedure :: bnd_options => src_options
    procedure, private :: src_constrain_rate
    procedure :: define_listlabel
    ! -- methods for observations
    procedure, public :: bnd_obs_supported => src_obs_supported
    procedure, public :: bnd_df_obs => src_df_obs
    ! -- methods for time series
    procedure, public :: bnd_rp_ts => src_rp_ts
  end type GwtSrcType

contains

  subroutine src_create(packobj, id, ibcnum, inunit, iout, namemodel, pakname, &
                        fmi, mst)
! ******************************************************************************
! src_create -- Create a New Src Package
! Subroutine: (1) create new-style package
!             (2) point bndobj to the new package
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
    type(GwtMstType), pointer :: mst
    ! -- local
    type(GwtFmiType), pointer :: fmi
    type(GwtSrcType), pointer :: srcobj
! ------------------------------------------------------------------------------
    !
    ! -- allocate the object and assign values to object variables
    allocate(srcobj)
    packobj => srcobj
    !
    ! -- create name and memory path
    call packobj%set_names(ibcnum, namemodel, pakname, ftype)
    packobj%text = text
    !
    ! -- allocate scalars
    call srcobj%allocate_scalars()
    !
    ! -- initialize package
    call packobj%pack_initialize()

    packobj%inunit = inunit
    packobj%iout = iout
    packobj%id = id
    packobj%ibcnum = ibcnum
    packobj%ncolbnd = 1
    packobj%iscloc = 1
    !
    ! -- Point src specific variables
    srcobj%fmi => fmi
    srcobj%mst => mst
    !
    ! -- return
    return
  end subroutine src_create

  subroutine src_ar(this)
! ******************************************************************************
! src_ar -- Allocate and Read
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(GwtSrcType), intent(inout) :: this
    ! -- local
    ! -- formats
! ------------------------------------------------------------------------------
    !
    call this%obs%obs_ar()
    !
    ! -- Allocate arrays
    call this%src_allocate_arrays()
    !
    ! -- Return
    return
  end subroutine src_ar
  
  subroutine src_da(this)
! ******************************************************************************
! src_da -- deallocate
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryManagerModule, only: mem_deallocate
    ! -- dummy
    class(GwtSrcType) :: this
! ------------------------------------------------------------------------------
    !
    ! -- Deallocate arrays if package was active
    if(this%inunit > 0) then
      call mem_deallocate(this%qrold)
    end if
    !
    ! -- Deallocate parent package
    call this%BndType%bnd_da()
    !
    ! -- scalars
    call mem_deallocate(this%iauxconstr)
    !
    ! -- return
    return
  end subroutine src_da

  subroutine src_options(this, option, found)
! ******************************************************************************
! src_options -- set options specific to GwtSrcType
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use SimModule, only: store_error
    ! -- dummy
    class(GwtSrcType),   intent(inout) :: this
    character(len=*), intent(inout) :: option
    logical,          intent(inout) :: found
    ! -- local
    character(len=LENAUXNAME) :: constrauxname
    integer(I4B) :: n
    ! -- formats
! ------------------------------------------------------------------------------
    !
    select case (option)
      case('AUXCONSTRAINTNAME')
        call this%parser%GetStringCaps(constrauxname)
        this%iauxconstr = -1
        write(this%iout, '(4x,a,a)')                                           &
          'AUXILIARY CONCENTRATION CONSTAINT NAME: ', trim(constrauxname)
        found = .true.
      case default
        !
        ! -- No options found
        found = .false.
      end select
    !
    ! -- AUXCONSTRAINTNAME was specified, so find column of auxvar that will 
    !    be used
    if (this%iauxconstr < 0) then
      !
      ! -- Error if no aux variable specified
      if(this%naux == 0) then
        write(errmsg, '(a,2(1x,a))')                                           &
          'AUXCONSTRAINTNAME WAS SPECIFIED AS',  trim(adjustl(constrauxname)), &
          'BUT NO AUX VARIABLES SPECIFIED.'
        call store_error(errmsg)
      end if
      !
      ! -- Assign iauxconstr column
      this%iauxconstr = 0
      do n = 1, this%naux
        if(constrauxname == this%auxname(n)) then
          this%iauxconstr = n
          exit
        end if
      end do
      !
      ! -- Error if aux variable cannot be found
      if(this%iauxconstr == 0) then
        write(errmsg, '(a,2(1x,a))')                                           &
          'AUXCONSTRAINTNAME WAS SPECIFIED AS', trim(adjustl(constrauxname)),  &
          'BUT NO AUX VARIABLE FOUND WITH THIS NAME.'
        call store_error(errmsg)
      end if
    end if
    !
    ! -- Return
    return
  end subroutine src_options

  subroutine src_allocate_scalars(this)
! ******************************************************************************
! allocate_scalars -- allocate scalar members
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use MemoryManagerModule, only: mem_allocate
    ! -- dummy
    class(GwtSrcType) :: this
! ------------------------------------------------------------------------------
    !
    ! -- call standard BndType allocate scalars
    call this%BndType%allocate_scalars()
    call mem_allocate(this%iauxconstr, 'IAUXCONSTR', this%memoryPath)
    !
    ! -- allocate the object and assign values to object variables
    !
    ! -- Set values
    this%iauxconstr = 0
    !
    ! -- return
    return
  end subroutine src_allocate_scalars

  subroutine src_allocate_arrays(this)
! ******************************************************************************
! src_allocate_arrays -- allocate array members
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use MemoryManagerModule, only: mem_allocate
    ! -- dummy
    class(GwtSrcType) :: this
! ------------------------------------------------------------------------------
    !
    ! -- call standard BndType allocate arrays
    call this%BndType%allocate_arrays()
    if (this%iauxconstr == 0) then
      call mem_allocate(this%qrold, 0, 0, 'QROLD', this%memoryPath)
    else
      call mem_allocate(this%qrold, 4, this%maxbound, 'QROLD', this%memoryPath)
    end if
    !
    ! -- Set values
    !
    ! -- return
    return
  end subroutine src_allocate_arrays

  subroutine src_cf(this, kiter, reset_mover)
! ******************************************************************************
! src_cf -- Formulate the HCOF and RHS terms
! Subroutine: (1) skip if no sources
!             (2) calculate hcof and rhs
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(GwtSrcType) :: this
    integer(I4B), intent(in) :: kiter
    logical, intent(in), optional :: reset_mover
    ! -- local
    integer(I4B) :: i, node
    real(DP) :: q
    logical :: lrm
    
! ------------------------------------------------------------------------------
    !
    ! -- Return if no sources
    if(this%nbound == 0) return
    !
    ! -- pakmvrobj cf
    lrm = .true.
    if (present(reset_mover)) lrm = reset_mover
    if(this%imover == 1 .and. lrm) then
      call this%pakmvrobj%cf()
    endif
    !
    ! -- Calculate hcof and rhs for each source entry
    do i = 1, this%nbound
      node = this%nodelist(i)
      if(this%ibound(node) <= 0) then
        this%rhs(i) = DZERO
        this%hcof(i) = DZERO
        cycle
      end if
      q = this%bound(1, i)
      if (this%iauxconstr /= 0) then
        if (q /= DZERO) then
          !
          ! -- If q is negative, then use Newton to recalculate q so that the
          !    cell concentation is not less than the concentration constraint.
          !    If q is positive, then use Newton to recalculate q so that the
          !    cell concentration is not greater than the concentration 
          !    constaint.
          call this%src_constrain_rate(q, i, kiter, this%xnew(node),           &
                                       this%auxvar(this%iauxconstr, i))
        end if
      end if
      this%rhs(i) = -q
    enddo
    !
    ! -- return
    return
  end subroutine src_cf
  
  subroutine src_constrain_rate(this, q, i, kiter, cnode, constraint)
! ******************************************************************************
! src_constrain_rate -- If necessary reduce the magnitude of q so that if q is 
!   negative that the resulting cell concentration is never less than the
!   user-specified constraint.  If q is positive, then if necessary, reduce
!   q so that the cell concentration is never greater than constraint. 
!
!   If constraints are used, then need to store previous iterates of q and the
!     the residual in order to use Newton's method.  These are saved in qrold.
!   qrold(1, :) is q from the last iteration
!   qrold(2, :) is q from two iterations prior
!   qrold(3, :) is residual (c-constraint) from the last iteration
!   qrold(4, :) is residual (c-constraint) from two iterations ago
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(GwtSrcType) :: this
    real(DP), intent(inout) :: q
    integer(I4B), intent(in) :: i
    integer(I4B), intent(in) :: kiter
    real(DP), intent(in) :: cnode
    real(DP), intent(in) :: constraint
    ! -- local
    real(DP) :: dq
! ------------------------------------------------------------------------------
    !
    ! -- initialize
    this%qrold(3, i) = cnode - constraint
    if (kiter == 1) then
      ! -- for first iteration leave q at user specified rate,
      !    and initialize q1 to zero
      this%qrold(1, i) = DZERO
    else if (kiter == 2) then
      ! - for second iteration, perturb q by one percent
      q = q * DP99
    else
      ! -- for third and up, use newton update and then constrain
      !    to not exceed user-specified q
      dq = -this%qrold(3, i) * (this%qrold(1, i) - this%qrold(2, i)) /         &
                               (this%qrold(3, i) - this%qrold(4, i))
      if (q < DZERO) then
        q = max(this%qrold(1, i) + dq, q)
      else
        q = min(this%qrold(1, i) + dq, q)
      endif
    end if
    !
    ! -- store iterates
    this%qrold(2, i) = this%qrold(1, i)
    this%qrold(1, i) = q
    this%qrold(4, i) = this%qrold(3, i)
    !
    ! -- return
    return
  end subroutine src_constrain_rate

  subroutine src_fc(this, rhs, ia, idxglo, amatsln)
! ******************************************************************************
! src_fc -- Copy rhs and hcof into solution rhs and amat
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(GwtSrcType) :: this
    real(DP), dimension(:), intent(inout) :: rhs
    integer(I4B), dimension(:), intent(in) :: ia
    integer(I4B), dimension(:), intent(in) :: idxglo
    real(DP), dimension(:), intent(inout) :: amatsln
    ! -- local
    integer(I4B) :: i, n, ipos
! ------------------------------------------------------------------------------
    !
    ! -- pakmvrobj fc
    if(this%imover == 1) then
      call this%pakmvrobj%fc()
    endif
    !
    ! -- Copy package rhs and hcof into solution rhs and amat
    do i = 1, this%nbound
      n = this%nodelist(i)
      rhs(n) = rhs(n) + this%rhs(i)
      ipos = ia(n)
      amatsln(idxglo(ipos)) = amatsln(idxglo(ipos)) + this%hcof(i)
      !
      ! -- If mover is active and mass is being withdrawn,
      !    store available mass (as positive value).
      if(this%imover == 1 .and. this%rhs(i) > DZERO) then
        call this%pakmvrobj%accumulate_qformvr(i, this%rhs(i))
      endif
    enddo
    !
    ! -- return
    return
  end subroutine src_fc

  subroutine src_cc(this, innertot, kiter, iend, icnvgmod, cpak, ipak, dpak)
! ******************************************************************************
! src_cc -- additional convergence check for advanced packages
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(GwtSrcType), intent(inout) :: this
    integer(I4B), intent(in) :: innertot
    integer(I4B), intent(in) :: kiter
    integer(I4B),intent(in) :: iend
    integer(I4B), intent(in) :: icnvgmod
    character(len=LENPAKLOC), intent(inout) :: cpak
    integer(I4B), intent(inout) :: ipak
    real(DP), intent(inout) :: dpak
    ! -- local
    character(len=LENPAKLOC) :: cloc
! ------------------------------------------------------------------------------
    !
    !
    if (this%iauxconstr /= 0) then
      cpak = ''
      ipak = 0
      dpak = DZERO
      if (kiter < 3) then
        !
        ! -- If constraints are being used, then three iterations are required
        write(cloc, "(a,'-',a)") trim(this%packName), 'RATE'
        cpak = trim(cloc)
        ipak = 1
        dpak = DZERO
      end if
    end if
    !
    ! -- No addition convergence check for boundary conditions
    !
    ! -- return
    return
  end subroutine src_cc

  subroutine define_listlabel(this)
! ******************************************************************************
! define_listlabel -- Define the list heading that is written to iout when
!   PRINT_INPUT option is used.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    class(GwtSrcType), intent(inout) :: this
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
    write(this%listlabel, '(a, a16)') trim(this%listlabel), 'STRESS RATE'
    if(this%inamedbound == 1) then
      write(this%listlabel, '(a, a16)') trim(this%listlabel), 'BOUNDARY NAME'
    endif
    !
    ! -- return
    return
  end subroutine define_listlabel

  ! -- Procedures related to observations
  logical function src_obs_supported(this)
  ! ******************************************************************************
  ! src_obs_supported
  !   -- Return true because SRC package supports observations.
  !   -- Overrides BndType%bnd_obs_supported()
  ! ******************************************************************************
  !
  !    SPECIFICATIONS:
  ! ------------------------------------------------------------------------------
    implicit none
    class(GwtSrcType) :: this
  ! ------------------------------------------------------------------------------
    src_obs_supported = .true.
    return
  end function src_obs_supported

  subroutine src_df_obs(this)
  ! ******************************************************************************
  ! src_df_obs (implements bnd_df_obs)
  !   -- Store observation type supported by SRC package.
  !   -- Overrides BndType%bnd_df_obs
  ! ******************************************************************************
  !
  !    SPECIFICATIONS:
  ! ------------------------------------------------------------------------------
    implicit none
    ! -- dummy
    class(GwtSrcType) :: this
    ! -- local
    integer(I4B) :: indx
  ! ------------------------------------------------------------------------------
    call this%obs%StoreObsType('src', .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => DefaultObsIdProcessor
    !
    ! -- Store obs type and assign procedure pointer
    !    for to-mvr observation type.
    call this%obs%StoreObsType('to-mvr', .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => DefaultObsIdProcessor
    !
    ! -- return
    return
  end subroutine src_df_obs

  ! -- Procedure related to time series

  subroutine src_rp_ts(this)
    ! -- Assign tsLink%Text appropriately for
    !    all time series in use by package.
    !    In the SRC package only the SMASSRATE variable
    !    can be controlled by time series.
    ! -- dummy
    class(GwtSrcType), intent(inout) :: this
    ! -- local
    integer(I4B) :: i, nlinks
    type(TimeSeriesLinkType), pointer :: tslink => null()
    !
    nlinks = this%TsManager%boundtslinks%Count()
    do i=1,nlinks
      tslink => GetTimeSeriesLinkFromList(this%TsManager%boundtslinks, i)
      if (associated(tslink)) then
        if (tslink%JCol==1) then
          tslink%Text = 'SMASSRATE'
        endif
      endif
    enddo
    !
    return
  end subroutine src_rp_ts

end module GwtSrcModule
