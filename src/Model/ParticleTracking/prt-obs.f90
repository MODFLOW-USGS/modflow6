module PrtObsModule

  use KindModule, only: DP, I4B
  use ConstantsModule, only: LINELENGTH, MAXOBSTYPES
  use BaseDisModule, only: DisBaseType
  use ObserveModule, only: ObserveType
  use ObsModule, only: ObsType
  use SimModule, only: count_errors, store_error, &
                       store_error_unit
  implicit none

  private
  public :: PrtObsType, prt_obs_cr

  type, extends(ObsType) :: PrtObsType
    ! -- Private members
    real(DP), dimension(:), pointer, contiguous, private :: x => null() !< concentration
    real(DP), dimension(:), pointer, contiguous, private :: flowja => null() !< intercell flows
  contains
    ! -- Public procedures
    procedure, public :: prt_obs_ar
    procedure, public :: obs_bd => prt_obs_bd
    procedure, public :: obs_df => prt_obs_df
    procedure, public :: obs_rp => prt_obs_rp
    procedure, public :: obs_da => prt_obs_da
    ! -- Private procedures
    procedure, private :: set_pointers
  end type PrtObsType

contains

  !> @brief Create a new PrtObsType object
  subroutine prt_obs_cr(obs, inobs)
    ! -- dummy
    type(PrtObsType), pointer, intent(out) :: obs
    integer(I4B), pointer, intent(in) :: inobs
    !
    allocate (obs)
    call obs%allocate_scalars()
    obs%active = .false.
    obs%inputFilename = ''
    obs%inUnitObs => inobs

  end subroutine prt_obs_cr

  !> @brief Allocate and read
  subroutine prt_obs_ar(this, x, flowja)
    ! -- dummy
    class(PrtObsType), intent(inout) :: this
    real(DP), dimension(:), pointer, contiguous, intent(in) :: x
    real(DP), dimension(:), pointer, contiguous, intent(in) :: flowja
    !
    ! Call ar method of parent class
    call this%obs_ar()
    !
    ! set pointers
    call this%set_pointers(x, flowja)

  end subroutine prt_obs_ar

  !> @brief Define package
  subroutine prt_obs_df(this, iout, pkgname, filtyp, dis)
    ! -- dummy
    class(PrtObsType), intent(inout) :: this
    integer(I4B), intent(in) :: iout
    character(len=*), intent(in) :: pkgname
    character(len=*), intent(in) :: filtyp
    class(DisBaseType), pointer :: dis
    ! -- local
    integer(I4B) :: indx
    !
    ! Call overridden method of parent class
    call this%ObsType%obs_df(iout, pkgname, filtyp, dis)
    !
    ! -- StoreObsType arguments are: (ObserveType, cumulative, indx);
    !    indx is returned.
    !
    ! -- Store obs type and assign procedure pointer for head observation type
    call this%StoreObsType('concentration', .false., indx)
    this%obsData(indx)%ProcessIdPtr => prt_process_concentration_obs_id
    !
    ! -- Store obs type and assign procedure pointer for flow-ja-face observation type
    call this%StoreObsType('flow-ja-face', .true., indx)
    this%obsData(indx)%ProcessIdPtr => prt_process_intercell_obs_id

  end subroutine prt_obs_df

  !> @brief Save observations
  subroutine prt_obs_bd(this)
    ! -- dummy
    class(PrtObsType), intent(inout) :: this
    ! -- local
    integer(I4B) :: i, jaindex, nodenumber
    character(len=100) :: msg
    class(ObserveType), pointer :: obsrv => null()
    !
    call this%obs_bd_clear()
    !
    ! -- iterate through all PRT observations
    if (this%npakobs > 0) then
      do i = 1, this%npakobs
        obsrv => this%pakobs(i)%obsrv
        nodenumber = obsrv%NodeNumber
        jaindex = obsrv%JaIndex
        select case (obsrv%ObsTypeId)
        case ('CONCENTRATION')
          call this%SaveOneSimval(obsrv, this%x(nodenumber))
        case ('FLOW-JA-FACE')
          call this%SaveOneSimval(obsrv, this%flowja(jaindex))
        case default
          msg = 'Error: Unrecognized observation type: '//trim(obsrv%ObsTypeId)
          call store_error(msg)
          call store_error_unit(this%inUnitObs)
        end select
      end do
    end if

  end subroutine prt_obs_bd

  !> @brief Read and prepare
  subroutine prt_obs_rp(this)
    class(PrtObsType), intent(inout) :: this
    ! Do PRT observations need any checking? If so, add checks here
  end subroutine prt_obs_rp

  !> @brief Deallocate
  subroutine prt_obs_da(this)
    ! -- dummy
    class(PrtObsType), intent(inout) :: this
    !
    nullify (this%x)
    nullify (this%flowja)
    call this%ObsType%obs_da()

  end subroutine prt_obs_da

  !> @brief Set pointers
  subroutine set_pointers(this, x, flowja)
    ! -- dummy
    class(PrtObsType), intent(inout) :: this
    real(DP), dimension(:), pointer, contiguous, intent(in) :: x
    real(DP), dimension(:), pointer, contiguous, intent(in) :: flowja
    !
    this%x => x
    this%flowja => flowja

  end subroutine set_pointers

  ! -- Procedures related to GWF observations (NOT type-bound)

  subroutine prt_process_concentration_obs_id(obsrv, dis, inunitobs, iout)
    ! -- dummy
    type(ObserveType), intent(inout) :: obsrv
    class(DisBaseType), intent(in) :: dis
    integer(I4B), intent(in) :: inunitobs
    integer(I4B), intent(in) :: iout
    ! -- local
    integer(I4B) :: nn1
    integer(I4B) :: icol, istart, istop
    character(len=LINELENGTH) :: ermsg, strng
    !
    ! -- Initialize variables
    strng = obsrv%IDstring
    icol = 1
    !
    ! Get node number, with option for ID string to be either node
    ! number or lay, row, column (when dis is structured).
    nn1 = dis%noder_from_string(icol, istart, istop, inunitobs, &
                                iout, strng, .false.)
    !
    if (nn1 > 0) then
      obsrv%NodeNumber = nn1
    else
      ermsg = 'Error reading data from ID string'
      call store_error(ermsg)
      call store_error_unit(inunitobs)
    end if

  end subroutine prt_process_concentration_obs_id

  subroutine prt_process_intercell_obs_id(obsrv, dis, inunitobs, iout)
    ! -- dummy
    type(ObserveType), intent(inout) :: obsrv
    class(DisBaseType), intent(in) :: dis
    integer(I4B), intent(in) :: inunitobs
    integer(I4B), intent(in) :: iout
    ! -- local
    integer(I4B) :: nn1, nn2
    integer(I4B) :: icol, istart, istop, jaidx
    character(len=LINELENGTH) :: ermsg, strng
    ! formats
70  format('Error: No connection exists between cells identified in text: ', a)
    !
    ! -- Initialize variables
    strng = obsrv%IDstring
    icol = 1
    !
    ! Get node number, with option for ID string to be either node
    ! number or lay, row, column (when dis is structured).
    nn1 = dis%noder_from_string(icol, istart, istop, inunitobs, &
                                iout, strng, .false.)
    !
    if (nn1 > 0) then
      obsrv%NodeNumber = nn1
    else
      ermsg = 'Error reading data from ID string: '//strng(istart:istop)
      call store_error(ermsg)
    end if
    !
    ! Get node number, with option for ID string to be either node
    ! number or lay, row, column (when dis is structured).
    nn2 = dis%noder_from_string(icol, istart, istop, inunitobs, &
                                iout, strng, .false.)
    if (nn2 > 0) then
      obsrv%NodeNumber2 = nn2
    else
      ermsg = 'Error reading data from ID string: '//strng(istart:istop)
      call store_error(ermsg)
    end if
    !
    ! -- store JA index
    jaidx = dis%con%getjaindex(nn1, nn2)
    if (jaidx == 0) then
      write (ermsg, 70) trim(strng)
      call store_error(ermsg)
    end if
    obsrv%JaIndex = jaidx
    !
    if (count_errors() > 0) then
      call store_error_unit(inunitobs)
    end if

  end subroutine prt_process_intercell_obs_id

end module PrtObsModule
