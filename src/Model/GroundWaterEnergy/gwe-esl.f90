module GweEslModule
  !
  use KindModule, only: DP, I4B
  use ConstantsModule, only: DZERO, DEM1, DONE, LENFTYPE
  use BndModule, only: BndType
  use ObsModule, only: DefaultObsIdProcessor
  use TimeSeriesLinkModule, only: TimeSeriesLinkType, &
                                  GetTimeSeriesLinkFromList
  use BlockParserModule, only: BlockParserType
  use GweInputDataModule, only: GweInputDataType
  use MatrixBaseModule
  !
  implicit none
  !
  private
  public :: esl_create
  !
  character(len=LENFTYPE) :: ftype = 'ESL'
  character(len=16) :: text = '             ESL'
  !
  type, extends(BndType) :: GweEslType

    type(GweInputDataType), pointer :: gwecommon => null() !< pointer to shared gwe data used by multiple packages but set in mst

  contains

    procedure :: allocate_scalars => esl_allocate_scalars
    procedure :: bnd_cf => esl_cf
    procedure :: bnd_fc => esl_fc
    procedure :: bnd_da => esl_da
    procedure :: define_listlabel
    ! -- methods for observations
    procedure, public :: bnd_obs_supported => esl_obs_supported
    procedure, public :: bnd_df_obs => esl_df_obs
    ! -- methods for time series
    procedure, public :: bnd_rp_ts => esl_rp_ts

  end type GweEslType

contains

  !> @brief Create an energy source loading package
  !!
  !! This subroutine points bndobj to the newly created package
  !<
  subroutine esl_create(packobj, id, ibcnum, inunit, iout, namemodel, pakname, &
                        gwecommon)
    ! -- dummy
    class(BndType), pointer :: packobj
    integer(I4B), intent(in) :: id
    integer(I4B), intent(in) :: ibcnum
    integer(I4B), intent(in) :: inunit
    integer(I4B), intent(in) :: iout
    character(len=*), intent(in) :: namemodel
    character(len=*), intent(in) :: pakname
    type(GweInputDataType), intent(in), target :: gwecommon !< shared data container for use by multiple GWE packages
    ! -- local
    type(GweEslType), pointer :: eslobj
    !
    ! -- Allocate the object and assign values to object variables
    allocate (eslobj)
    packobj => eslobj
    !
    ! -- Create name and memory path
    call packobj%set_names(ibcnum, namemodel, pakname, ftype)
    packobj%text = text
    !
    ! -- Allocate scalars
    call eslobj%allocate_scalars()
    !
    ! -- Initialize package
    call packobj%pack_initialize()
    !
    packobj%inunit = inunit
    packobj%iout = iout
    packobj%id = id
    packobj%ibcnum = ibcnum
    packobj%ncolbnd = 1
    packobj%iscloc = 1
    !
    ! -- Store pointer to shared data module for accessing cpw, rhow
    !    for the budget calculations, and for accessing the latent heat of
    !    vaporization for evaporative cooling.
    eslobj%gwecommon => gwecommon
  end subroutine esl_create

  !> @brief Deallocate memory
  !<
  subroutine esl_da(this)
    ! -- modules
    use MemoryManagerModule, only: mem_deallocate
    ! -- dummy
    class(GweEslType) :: this
    !
    ! -- Deallocate parent package
    call this%BndType%bnd_da()
  end subroutine esl_da

  !> @brief Allocate scalars
  !!
  !! Allocate scalars specific to this energy source loading package
  !<
  subroutine esl_allocate_scalars(this)
    ! -- modules
    use MemoryManagerModule, only: mem_allocate
    ! -- dummy
    class(GweEslType) :: this
    !
    ! -- call standard BndType allocate scalars
    call this%BndType%allocate_scalars()
    !
    ! -- allocate the object and assign values to object variables
    !
    ! -- Set values
  end subroutine esl_allocate_scalars

  !> @brief Formulate the HCOF and RHS terms
  !!
  !! This subroutine:
  !!   - calculates hcof and rhs terms
  !!   - skip if no sources
  !<
  subroutine esl_cf(this)
    ! -- dummy
    class(GweEslType) :: this
    ! -- local
    integer(I4B) :: i, node
    real(DP) :: q
    !
    ! -- Return if no sources
    if (this%nbound == 0) return
    !
    ! -- Calculate hcof and rhs for each source entry
    do i = 1, this%nbound
      node = this%nodelist(i)
      this%hcof(i) = DZERO
      if (this%ibound(node) <= 0) then
        this%rhs(i) = DZERO
        cycle
      end if
      q = this%bound(1, i)
      this%rhs(i) = -q
    end do
  end subroutine esl_cf

  !> @brief Add matrix terms related to specified energy source loading
  !!
  !! Copy rhs and hcof into solution rhs and amat
  !<
  subroutine esl_fc(this, rhs, ia, idxglo, matrix_sln)
    ! -- dummy
    class(GweEslType) :: this
    real(DP), dimension(:), intent(inout) :: rhs
    integer(I4B), dimension(:), intent(in) :: ia
    integer(I4B), dimension(:), intent(in) :: idxglo
    class(MatrixBaseType), pointer :: matrix_sln
    ! -- local
    integer(I4B) :: i, n, ipos
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
      ! -- If mover is active and mass is being withdrawn,
      !    store available mass (as positive value).
      if (this%imover == 1 .and. this%rhs(i) > DZERO) then
        call this%pakmvrobj%accumulate_qformvr(i, this%rhs(i))
      end if
    end do
  end subroutine esl_fc

  !> @brief Define list labels
  !!
  !! Define the list heading that is written to iout when
  !! PRINT_INPUT option is used.
  !<
  subroutine define_listlabel(this)
    ! -- dummy
    class(GweEslType), intent(inout) :: this
    !
    ! -- Create the header list label
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
    write (this%listlabel, '(a, a16)') trim(this%listlabel), 'STRESS RATE'
    if (this%inamedbound == 1) then
      write (this%listlabel, '(a, a16)') trim(this%listlabel), 'BOUNDARY NAME'
    end if
  end subroutine define_listlabel

  ! -- Procedures related to observations

  !> @brief Support function for specified energy source loading observations
  !!
  !! This function:
  !!   - returns true because ESL package supports observations.
  !!   - overrides BndType%bnd_obs_supported()
  !<
  logical function esl_obs_supported(this)
    implicit none
    ! -- dummy
    class(GweEslType) :: this
    !
    esl_obs_supported = .true.
  end function esl_obs_supported

  !> @brief Define observations
  !!
  !! This subroutine:
  !!   - stores observation types supported by ESL package.
  !!   - overrides BndType%bnd_df_obs
  !<
  subroutine esl_df_obs(this)
    implicit none
    ! -- dummy
    class(GweEslType) :: this
    ! -- local
    integer(I4B) :: indx
    !
    call this%obs%StoreObsType('esl', .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => DefaultObsIdProcessor
    !
    ! -- Store obs type and assign procedure pointer
    !    for to-mvr observation type.
    call this%obs%StoreObsType('to-mvr', .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => DefaultObsIdProcessor
  end subroutine esl_df_obs

  !> @brief Procedure related to time series
  !!
  !! Assign tsLink%Text appropriately for all time series in use by package.
  !! In the ESL package only the SENERRATE variable can be controlled by time
  !! series.
  !<
  subroutine esl_rp_ts(this)
    ! -- dummy
    class(GweEslType), intent(inout) :: this
    ! -- local
    integer(I4B) :: i, nlinks
    type(TimeSeriesLinkType), pointer :: tslink => null()
    !
    nlinks = this%TsManager%boundtslinks%Count()
    do i = 1, nlinks
      tslink => GetTimeSeriesLinkFromList(this%TsManager%boundtslinks, i)
      if (associated(tslink)) then
        if (tslink%JCol == 1) then
          tslink%Text = 'SMASSRATE'
        end if
      end if
    end do
  end subroutine esl_rp_ts

end module GweEslModule
