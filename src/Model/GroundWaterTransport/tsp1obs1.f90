module TspObsModule

  use KindModule, only: DP, I4B
  use ConstantsModule, only: LINELENGTH, MAXOBSTYPES
  use BaseDisModule, only: DisBaseType
  use TspIcModule, only: TspIcType
  use ObserveModule, only: ObserveType
  use ObsModule, only: ObsType
  use SimModule, only: count_errors, store_error, &
                       store_error_unit
  implicit none

  private
  public :: TspObsType, tsp_obs_cr

  type, extends(ObsType) :: TspObsType
    ! -- Private members
    type(TspIcType), pointer, private :: ic => null() ! initial conditions
    real(DP), dimension(:), pointer, contiguous, private :: x => null() ! concentration
    real(DP), dimension(:), pointer, contiguous, private :: flowja => null() ! intercell flows
  contains
    ! -- Public procedures
    procedure, public :: tsp_obs_ar
    procedure, public :: obs_bd => tsp_obs_bd
    procedure, public :: obs_df => tsp_obs_df
    procedure, public :: obs_rp => tsp_obs_rp
    procedure, public :: obs_da => tsp_obs_da
    ! -- Private procedures
    procedure, private :: set_pointers
  end type TspObsType

contains

  subroutine tsp_obs_cr(obs, inobs)
! ******************************************************************************
! tsp_obs_cr -- Create a new TspObsType object
! Subroutine: (1) creates object
!             (2) allocates pointers
!             (3) initializes values
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    type(TspObsType), pointer, intent(out) :: obs
    integer(I4B), pointer, intent(in) :: inobs
! ------------------------------------------------------------------------------
    !
    allocate (obs)
    call obs%allocate_scalars()
    obs%active = .false.
    obs%inputFilename = ''
    obs%inUnitObs => inobs
    !
    return
  end subroutine tsp_obs_cr

  subroutine tsp_obs_ar(this, ic, x, flowja)
! ******************************************************************************
! tsp_obs_ar -- allocate and read
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(TspObsType), intent(inout) :: this
    type(TspIcType), pointer, intent(in) :: ic
    real(DP), dimension(:), pointer, contiguous, intent(in) :: x
    real(DP), dimension(:), pointer, contiguous, intent(in) :: flowja
! ------------------------------------------------------------------------------
    !
    ! Call ar method of parent class
    call this%obs_ar()
    !
    ! set pointers
    call this%set_pointers(ic, x, flowja)
    !
    return
  end subroutine tsp_obs_ar

  subroutine tsp_obs_df(this, iout, pkgname, filtyp, dis)
! ******************************************************************************
! tsp_obs_df -- define
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(TspObsType), intent(inout) :: this
    integer(I4B), intent(in) :: iout
    character(len=*), intent(in) :: pkgname
    character(len=*), intent(in) :: filtyp
    class(DisBaseType), pointer :: dis
    ! -- local
    integer(I4B) :: indx
! ------------------------------------------------------------------------------
    !
    ! Call overridden method of parent class
    call this%ObsType%obs_df(iout, pkgname, filtyp, dis)
    !
    ! -- StoreObsType arguments are: (ObserveType, cumulative, indx);
    !    indx is returned.
    !
    ! -- Store obs type and assign procedure pointer for head observation type
    call this%StoreObsType('concentration', .false., indx)
    this%obsData(indx)%ProcessIdPtr => gwt_process_concentration_obs_id
    !
    ! -- Store obs type and assign procedure pointer for flow-ja-face observation type
    call this%StoreObsType('flow-ja-face', .true., indx)
    this%obsData(indx)%ProcessIdPtr => tsp_process_intercell_obs_id
    !
    return
  end subroutine tsp_obs_df

  subroutine tsp_obs_bd(this)
! ******************************************************************************
! tsp_obs_bd -- save obs
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(TspObsType), intent(inout) :: this
    ! -- local
    integer(I4B) :: i, jaindex, nodenumber
    character(len=100) :: msg
    class(ObserveType), pointer :: obsrv => null()
! ------------------------------------------------------------------------------
    !
    call this%obs_bd_clear()
    !
    ! -- iterate through all GWT observations
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
    !
    return
  end subroutine tsp_obs_bd

  subroutine tsp_obs_rp(this)
! ******************************************************************************
! tsp_obs_rp
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    class(TspObsType), intent(inout) :: this
! ------------------------------------------------------------------------------
    !
    ! Do GWT observations need any checking? If so, add checks here
    return
  end subroutine tsp_obs_rp

  subroutine tsp_obs_da(this)
! ******************************************************************************
! tsp_obs_da
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(TspObsType), intent(inout) :: this
! ------------------------------------------------------------------------------
    !
    nullify (this%ic)
    nullify (this%x)
    nullify (this%flowja)
    call this%ObsType%obs_da()
    !
    return
  end subroutine tsp_obs_da

  subroutine set_pointers(this, ic, x, flowja)
! ******************************************************************************
! set_pointers
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(TspObsType), intent(inout) :: this
    type(TspIcType), pointer, intent(in) :: ic
    real(DP), dimension(:), pointer, contiguous, intent(in) :: x
    real(DP), dimension(:), pointer, contiguous, intent(in) :: flowja
! ------------------------------------------------------------------------------
    !
    this%ic => ic
    this%x => x
    this%flowja => flowja
    !
    return
  end subroutine set_pointers

  ! -- Procedures related to GWF observations (NOT type-bound)

  subroutine gwt_process_concentration_obs_id(obsrv, dis, inunitobs, iout)
! ******************************************************************************
! gwt_process_concentration_obs_id
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    type(ObserveType), intent(inout) :: obsrv
    class(DisBaseType), intent(in) :: dis
    integer(I4B), intent(in) :: inunitobs
    integer(I4B), intent(in) :: iout
    ! -- local
    integer(I4B) :: nn1
    integer(I4B) :: icol, istart, istop
    character(len=LINELENGTH) :: ermsg, strng
! ------------------------------------------------------------------------------
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
    !
    return
  end subroutine gwt_process_concentration_obs_id

  subroutine tsp_process_intercell_obs_id(obsrv, dis, inunitobs, iout)
! ******************************************************************************
! tsp_process_intercell_obs_id
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
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
! ------------------------------------------------------------------------------
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
    !
    return
  end subroutine tsp_process_intercell_obs_id

end module TspObsModule