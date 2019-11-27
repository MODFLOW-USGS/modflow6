! This module defines type ObsType, which is the highest-level
! derived type for implementing observations. All objects derived from
! NumericalModelType or BndType already contain an ObsType member.
!
! Examples:
!   NumericalModelType.obs
!   BndType.obs
!
! Similarly, an ObsType member could be added to, say,
! NumericalExchangeType or any other type that has DF, AR, RP, AD, BD, and OT
! routines.
!
! ------------------------------------------------------------------------------
! IMPLEMENTATION OF OBSERVATIONS IN A MODEL OR PACKAGE
!
! For simple boundary packages like RIV and DRN, only steps 1-6 are
! needed. For models and advanced packages like MAW and SFR, additional
! steps are needed.
!
! 1. (package only) Override BndType.bnd_obs_supported to return true.
!    bnd_obs_supported is called from various places in code.
!
! 2. (optional) Write a subroutine that implements abstract interface
!    ObserveModule.ProcessIdSub. (Not needed if IDstring, which identifies
!    location in model to be observed, is either a single node number or
!    a single {lay, row, col} set of indices).
!
!    Examples:
!    gwf_process_head_drawdown_obs_id, gwf_process_intercell_obs_id
!
!    A package can allow IDstring to be a boundary name.
!    Example: ObsModule.DefaultObsIdProcessor
!
! 3. Override BndType.bnd_df_obs() to define string(s) to be
!    recognized as observation type(s) and (optional) assign ProcessIdPtr
!    (not needed  if IDstring is either a node number or a {lay, row, col}
!    set of indices).
!
!    Examples: gwf_df_obs, drn_df_obs
!
!    When boundary names are allowed and developer wants simulated value
!    to be cumulative (flow, for example) if user specifies multiple
!    boundaries with the same BOUNDNAME, in bnd_df_obs call to
!    ObsPackage.StoreObsType, provide cumulative argument as true.
!    Otherwise, simulated values are not cumulative.
!
! 4. In DF routine: Call bnd_df_obs
!
! 5. In AR routine: Call ObsType.obs_ar. This reads the OBS input
!    file.
!    Example (gwf_ar): call this%obs%obs_ar()
!    Example (lak_ar): call this%obs%obs_ar()
!
! 6. Override BndType.bnd_rp_obs for any package that needs to
!    check user input or process observation input in any special way.
!    If no special processing is needed, BndType.bnd_rp_obs can
!    be used.  This routine also expands the ObserveType%indxbnds array for
!    each observation in a package. ObserveType%indxbnds is used to sum
!    simulated values from multiple boundaries when BOUNDNAMES is used.
!    Equivalent routine may or may not be needed for model observations.
!    If needed, call it from bottom of RP routine.
!
!    Examples:
!        BndType.bnd_rp_obs, which is called from gwf_rp
!
! 7. In AD routine: Call ObsType.obs_ad
!    Example: gwf_ad
!
! 8. Write a *_bd_obs routine. This is the routine that actually
!    calculates the simulated value for each observation type supported
!    by the model/package.  Call *_bd_obs from the bottom of the
!    _bd routine.
!     *_bd_obs needs to:
!         Call ObsType.obs_bd_clear
!         For each observation:
!              Calculate the simulated value
!              Call ObsType.SaveOneSimval
!     Examples: gwf_bd_obs, maw_bd_obs, lak_bd_obs
!
! 9. In BD routine:
!          Call BndType.bnd_bd_obs
!     Examples: BndType.bnd_bd calls bnd_bd_obs
!               GwfModelType.gwf_bd calls gwf_bd_obs
!               MawType.maw_bd calls maw_bd_obs
!               LakType.lak_bd calls lak_bd_obs
!
! 10. Ensure that ObsType.obs_ot is called. For packages, obs_ot is called
!     from the model _ot procedure.  The model _ot procedure should also call
!     obs_ot for its own observations.  Do not call obs_ot from a package _ot
!     procedure because the package _ot procedure may not be called, depending
!     on Output Control settings (ibudfl).
!
!     Note: BndType.bnd_ot_obs calls:
!               ObsType.obs_ot
!
!     Note: ObsType.obs_ot calls:
!               store_all_simvals
!               write_continuous_simvals
!               obsOutputList.WriteOutputLines
!
! BINARY OUTPUT:
!
! When observation-output files are written, the user has the option to have
! output written to a binary file.  Binary obs output files start with a
! 100-byte header structured as follows:
!
! bytes 1-4   (ascii): Observation type contained in file; options are:
!                        "sngl" -- Single observations
!                        "cont" -- Continuous observations
! byte 5: blank
! bytes 6-11  (ascii): Precision of all floating-point values; options are:
!                        "single" -- Single precision
!                        "double" -- Double precision
! bytes 12-15 (ascii): LENOBSNAME (integer; length of observation names,
!                                  in bytes)
! bytes 16-100: blank
!
! IN A FILE OF CONTINUOUS OBSERVATIONS:
!
! The 100-byte header is followed by:
! NOBS (4-byte integer) -- Number of observations.
! NOBS repetitions of OBSNAME (ascii, LENOBSNAME bytes each).
! Any number of repetitions of:
! TIME SIMVAL-1 SIMVAL-2 ... SIMVAL-NOBS  (floating point)
!
!-------------------------------------------------------------------------------
module ObsModule

  use KindModule, only: DP, I4B
  use ArrayHandlersModule, only: ExpandArray
  use BaseDisModule,       only: DisBaseType
  use BlockParserModule,   only: BlockParserType
  use ConstantsModule,     only: LENBIGLINE, LENFTYPE, LENOBSNAME, &
                                 LENOBSTYPE, LENPACKAGENAME, &
                                 LINELENGTH, NAMEDBOUNDFLAG, MAXCHARLEN, &
                                 MAXOBSTYPES, LENHUGELINE, DNODATA
  use InputOutputModule,   only: UPCASE, openfile, GetUnit, GetFileFromPath
  use ListModule,          only: ListType
  use ObsContainerModule,  only: ObsContainerType
  use ObserveModule,       only: ConstructObservation, ObsDataType, &
                                 ObserveType, GetObsFromList, &
                                 AddObsToList
  use ObsOutputListModule, only: ObsOutputListType
  use ObsOutputModule,     only: ObsOutputType
  use ObsUtilityModule,    only: write_fmtd_cont, write_unfmtd_cont
  use OpenSpecModule,      only: ACCESS, FORM
  use SimModule,           only: count_errors, store_error, store_error_unit, &
                                 ustop
  use StringListModule,    only: AddStringToList, GetStringFromList
  use TdisModule,          only: totim

  implicit none

  private
  public :: ObsType, DefaultObsIdProcessor,  obs_cr

  type :: ObsType
    ! -- Public members
    integer(I4B), public :: iout = 0
    integer(I4B), public :: npakobs = 0
    integer(I4B), pointer, public :: inUnitObs => null()
    character(len=LINELENGTH), pointer, public :: inputFilename => null()
    character(len=2*LENPACKAGENAME+4), public :: pkgName = ''
    character(len=LENFTYPE), public :: filtyp = ''
    logical, pointer, public :: active => null()
    type(ObsContainerType), dimension(:), pointer, public :: pakobs => null()
    type(ObsDataType), dimension(:), pointer, public :: obsData => null()
    ! -- Private members
    integer(I4B), private :: iprecision = 2                                      ! 2=double; 1=single
    integer(I4B), private :: idigits = 5
    character(len=LINELENGTH), private :: outputFilename = ''
    character(len=LINELENGTH), private :: blockTypeFound = ''
    character(len=20), private:: obsfmtcont = ''
    logical, private :: echo = .false.
    logical, private :: more
    type(ListType), private :: obsList
    type(ObsOutputListType), pointer, private :: obsOutputList => null()
    class(DisBaseType), pointer, private :: dis => null()
    type(BlockParserType), private :: parser
  contains
    ! -- Public procedures
    procedure, public  :: obs_df
    procedure, public  :: obs_ar
    procedure, public  :: obs_ad
    procedure, public  :: obs_bd_clear
    procedure, public  :: obs_ot
    procedure, public  :: obs_da
    procedure, public  :: SaveOneSimval
    procedure, public  :: StoreObsType
    procedure, public  :: allocate_scalars
    ! -- Private procedures
    procedure, private :: build_headers
    procedure, private :: define_fmts
    procedure, private :: get_num
    procedure, private :: get_obs
    procedure, private :: get_obs_array
    procedure, private :: get_obs_datum
    procedure, private :: obs_ar1
    procedure, private :: obs_ar2
    procedure, private :: populate_obs_array
    procedure, private :: read_observations
    procedure, private :: read_obs_blocks
    procedure, private :: read_obs_options
    procedure, private :: write_continuous_simvals
  end type ObsType

contains

  ! Non-type-bound procedures

  subroutine obs_cr(obs, inobs)
! ******************************************************************************
! obs_cr -- Create a new ObsType object
! Subroutine: (1) creates object
!             (2) allocates pointers
!             (3) initializes values
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    implicit none
    ! -- dummy
    type(ObsType), pointer, intent(out)   :: obs
    integer(I4B), pointer, intent(in) :: inobs
    !
    allocate(obs)
    call obs%allocate_scalars()
    obs%inUnitObs => inobs
    !
    return
  end subroutine obs_cr

  subroutine DefaultObsIdProcessor(obsrv, dis, inunitobs, iout)
! ******************************************************************************
! DefaultObsIdProcessor -- Process IDstring provided for each observation. The
! IDstring identifies the location in the model of the node(s) or feature(s)
! where the simulated value is to be extracted and recorded.
! Subroutine: (1) interprets the IDstring
!             (2) stores the location of interest in the ObserveType object that
!                 contains information about the observation
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    implicit none
    ! -- dummy
    type(ObserveType),  intent(inout) :: obsrv
    class(DisBaseType), intent(in)    :: dis
    integer(I4B),            intent(in)    :: inunitobs
    integer(I4B),            intent(in)    :: iout
    ! -- local
    integer(I4B) :: n
    integer(I4B) :: icol, istart, istop
    character(len=LINELENGTH) :: ermsg, strng
    logical :: flag_string
    !
    ! -- Initialize variables
    strng = obsrv%IDstring
    icol = 1
    flag_string = .true.   ! Allow strng to contain a boundary name
    !
    n = dis%noder_from_string(icol, istart, istop, inunitobs, &
                                   iout, strng, flag_string)
    !
    if (n > 0) then
      obsrv%NodeNumber = n
    elseif (n == -2) then
      ! Integer can't be read from strng; it's presumed to be a boundary
      ! name (already converted to uppercase)
      obsrv%FeatureName = strng(istart:istop)
      ! -- Observation may require summing rates from multiple boundaries,
      !    so assign NodeNumber as a value that indicates observation
      !    is for a named boundary or group of boundaries.
      obsrv%NodeNumber = NAMEDBOUNDFLAG
    else
      ermsg = 'Error reading data from ID string'
      call store_error(ermsg)
      call store_error_unit(inunitobs)
      call ustop()
    endif
    !
    return
  end subroutine DefaultObsIdProcessor

  ! Type-bound public procedures

  subroutine obs_df(this, iout, pkgname, filtyp, dis)
! ******************************************************************************
! obs_df -- Define some members of an ObsType object
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    implicit none
    ! -- dummy
    class(ObsType), intent(inout) :: this
    integer(I4B), intent(in) :: iout
    character(len=*), intent(in) :: pkgname
    character(len=*), intent(in) :: filtyp
    class(DisBaseType), pointer  :: dis
    !
    this%iout = iout
    this%pkgName = pkgname
    this%filtyp = filtyp
    this%dis => dis
    !
    ! -- Initialize block parser
    call this%parser%Initialize(this%inUnitObs, this%iout)
    !
    return
  end subroutine obs_df

  subroutine obs_ar(this)
! ******************************************************************************
! obs_ar -- ObsType Allocate and Read
! Subroutine: (1) reads OPTIONS block of OBS input file
!             (2) reads CONTINUOUS blocks of OBS input file
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    implicit none
    ! -- dummy
    class(ObsType) :: this
    !
    call this%obs_ar1(this%pkgName)
    if (this%active) then
      call this%obs_ar2(this%dis)
    endif
    !
    return
  end subroutine obs_ar

  subroutine obs_ad(this)
! ******************************************************************************
! obs_ad -- Observation Time Step Advance
! Subroutine: (1) For each observation, resets "current" value
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    implicit none
    ! -- dummy
    class(ObsType) :: this
    ! -- local
    integer(I4B) :: i, n
    class(ObserveType), pointer :: obsrv => null()
    !
    n = this%get_num()
    do i=1,n
      obsrv => this%get_obs(i)
      call obsrv%ResetCurrent()
    enddo
    !
    return
  end subroutine obs_ad

  subroutine obs_bd_clear(this)
! **************************************************************************
! obs_bd_clear -- Clear output lines in preparation for new rows of
!                 continuous observations
! Subroutine: (1) Clears contents of all lineout members of obsOutputList
!                 at start of a new time step
! **************************************************************************
!
!    SPECIFICATIONS:
! --------------------------------------------------------------------------
    implicit none
    ! -- dummy
    class(ObsType), target :: this
    !
    call this%obsOutputList%ClearOutputLines()
    !
    return
  end subroutine obs_bd_clear

  subroutine obs_ot(this)
! ******************************************************************************
! obs_ot -- Observation Output
! Subroutine: (1) stores each simulated value into its ObserveType object
!             (2) writes each simulated value to it ObsOutputList object
!             (3) writes contents of ObsOutputList to output file
! Note: This procedure should NOT be called from a package's _ot procedure
!       because the package _ot procedure may not be called every time step.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    implicit none
    ! -- dummy
    class(ObsType), intent(inout) :: this
    !
    if (this%npakobs > 0) then
      call this%write_continuous_simvals()
      call this%obsOutputList%WriteOutputLines()
    endif
    !
    return
  end subroutine obs_ot

  subroutine obs_da(this)
! ******************************************************************************
! obs_da -- Observation Output
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(ObsType), intent(inout) :: this
    ! -- local
    integer(I4B) :: i
    !
    deallocate(this%active)
    deallocate(this%inputFilename)
    deallocate(this%obsData)
    !
    ! -- deallocate pakobs components and pakobs
    if (associated(this%pakobs)) then
      do i = 1, this%npakobs
        if (allocated(this%pakobs(i)%obsrv%indxbnds)) then
          deallocate(this%pakobs(i)%obsrv%indxbnds)
        end if
        !
        ! -- nullify pointer to this%pakobs(i)%obsrv
        !    deallocate does not work in gfortran-8 since no
        !    allocatable variables in obsrv except for indxbnds
        nullify(this%pakobs(i)%obsrv)
      end do
      deallocate(this%pakobs)
    end if
    !
    ! -- deallocate obsOutputList
    call this%obsOutputList%DeallocObsOutputList()
    deallocate(this%obsOutputList)
    !
    ! -- deallocate obslist
    call this%obslist%Clear()
    !
    ! -- nullify
    nullify(this%inUnitObs)
    !
    return
  end subroutine obs_da

  subroutine SaveOneSimval(this, obsrv, simval)
! **************************************************************************
! SaveOneSimval
! Subroutine: (1) saves or accumulates a simulated value to its ObserveType
!                 object
! **************************************************************************
!
!    SPECIFICATIONS:
! --------------------------------------------------------------------------
    implicit none
    ! -- dummy
    class(ObsType)         :: this
    class(ObserveType), intent(inout) :: obsrv
    real(DP), intent(in)  :: simval
    ! -- local
    character(len=LENOBSTYPE) :: obsTypeID
    type(ObsDataType), pointer :: obsDatum => null()
    !
    ! -- initialize variables
    obsTypeID = obsrv%ObsTypeId
    obsDatum => this%get_obs_datum(obsTypeID)
    !
    ! -- save current simulation time
    obsrv%CurrentTimeStepEndTime = totim
    !
    ! -- assign or accumulate simulated value
    if (obsDatum%Cumulative .and. simval /= DNODATA) then
      obsrv%CurrentTimeStepEndValue = obsrv%CurrentTimeStepEndValue + simval
    else
      obsrv%CurrentTimeStepEndValue = simval
    endif
    !
    return
  end subroutine SaveOneSimval

  subroutine StoreObsType(this, obsrvType, cumulative, indx)
! **************************************************************************
! StoreObsType
! Subroutine: (1) stores type name and related information for an
!                 observation type that belongs to a package or model in
!                 the obsData array
! **************************************************************************
!
!    SPECIFICATIONS:
! --------------------------------------------------------------------------
    implicit none
    ! -- dummy
    class(ObsType), intent(inout) :: this
    character(len=*), intent(in)  :: obsrvType
    ! cumulative:  Accumulate simulated values for multiple boundaries
    logical,          intent(in)  :: cumulative
    integer(I4B),     intent(out) :: indx
    ! -- local
    integer(I4B) :: i
    character(len=LENOBSTYPE)  :: obsTypeUpper
    character(len=100) :: msg
    !
    ! -- Ensure that obsrvType is not blank
    if (obsrvType=='') then
      msg = 'Programmer error: Invalid argument in store_obs_type.'
      call store_error(msg)
      call ustop()
    endif
    !
    ! -- Find first unused element
    indx = -1
    do i=1,MAXOBSTYPES
      if (this%obsData(i)%ObsTypeID /= '') cycle
      indx = i
      exit
    enddo
    !
    ! -- Ensure that array size is not exceeded
    if (indx == -1) then
      msg = 'Size of obsData array is insufficient; ' &
             // 'need to increase MAXOBSTYPES.'
      call store_error(msg)
      call store_error_unit(this%inUnitObs)
      call ustop()
    endif
    !
    ! -- Convert character argument to upper case
    obsTypeUpper = obsrvType
    call upcase(obsTypeUpper)
    !
    ! -- Assign members
    this%obsData(indx)%ObsTypeID = obsTypeUpper
    this%obsData(indx)%Cumulative = cumulative
    !
    return
  end subroutine StoreObsType

  ! Type-bound private procedures

  subroutine allocate_scalars(this)
! ******************************************************************************
! allocate_scalars -- Allocate memory for non-allocatable members
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(ObsType) :: this
! ------------------------------------------------------------------------------
    !
    allocate(this%active)
    allocate(this%inputFilename)
    allocate(this%obsOutputList)
    allocate(this%obsData(MAXOBSTYPES))
    !
    ! -- Initialize
    this%active = .false.
    this%inputFilename = ''
    !
    ! -- Return
    return
  end subroutine allocate_scalars

  subroutine obs_ar1(this, pkgname)
! **************************************************************************
! obs_ar1
!   -- read OPTIONS block of OBS input file and define output formats.
! **************************************************************************
!
!    SPECIFICATIONS:
! --------------------------------------------------------------------------
    implicit none
    ! -- dummy
    class(ObsType), intent(inout) :: this
    character(len=*), intent(in) :: pkgname
    ! -- formats
    10  format(/,'The observation utility is active for "',a,'"')
! ------------------------------------------------------------------------------
    !
    if (this%inUnitObs > 0) then
      this%active = .true.
      !
      ! -- Indicate that OBS is active
      write(this%iout,10)trim(pkgname)
      !
      ! -- Read Options block
      call this%read_obs_options()
      !
      ! -- define output formats
      call this%define_fmts()
    endif
    !
    return
  end subroutine obs_ar1

  subroutine obs_ar2(this, dis)
! **************************************************************************
! obs_ar2
!   -- Call procedure provided by package to interpret IDstring and
!      store required data.
! **************************************************************************
!
!    SPECIFICATIONS:
! --------------------------------------------------------------------------
    implicit none
    ! -- dummy
    class(ObsType), intent(inout) :: this
    class(DisBaseType)            :: dis
    ! -- local
    integer(I4B) :: i
    type(ObsDataType), pointer  :: obsDat => null()
    character(len=LENOBSTYPE)   :: obsTypeID
    class(ObserveType), pointer :: obsrv => null()
    !
    call this%read_observations()
    ! -- allocate and populate observations array
    call this%get_obs_array(this%npakobs, this%pakobs)
    !
    do i=1,this%npakobs
      obsrv => this%pakobs(i)%obsrv
      ! -- Call IDstring processor procedure provided by package
      obsTypeID = obsrv%ObsTypeId
      obsDat => this%get_obs_datum(obsTypeID)
      if (associated(obsDat%ProcessIdPtr)) then
        call obsDat%ProcessIdPtr(obsrv, dis, &
                this%inUnitObs, this%iout)
      else
        call DefaultObsIdProcessor(obsrv, dis, &
                this%inUnitObs, this%iout)
      endif
    enddo
    !
    if (count_errors() > 0) then
      call store_error_unit(this%inunitobs)
      call ustop()
    end if
    !
    return
  end subroutine obs_ar2

  subroutine read_obs_options(this)
! **************************************************************************
! read_obs_options
!   -- read OPTIONS block of OBS input file
! **************************************************************************
!
!    SPECIFICATIONS:
! --------------------------------------------------------------------------
    implicit none
    ! -- dummy
    class(ObsType) :: this
    ! -- local
    integer(I4B) :: iin
    integer(I4B) :: ierr
    integer(I4B) :: localprecision
    integer(I4B) :: localdigits
    character(len=40) :: keyword
    character(len=LINELENGTH) :: ermsg
    character(len=LINELENGTH) :: errormessage, fname
    type(ListType), pointer :: lineList => null()
    logical :: continueread, found, endOfBlock
    ! -- formats
10  format('No options block found in OBS input. Defaults will be used.')
40  format('Text output number of digits of precision set to: ',i2)
60  format(/,'Processing observation options:',/)
    !
    localprecision = 0
    localdigits = 0
    lineList => null()
    !
    ! -- Find and store file name
    iin = this%inUnitObs
    inquire(unit=iin, name=fname)
    this%inputFilename = fname
    !
    ! -- Read Options block
    continueread = .false.
    ierr = 0
    !
    ! -- get BEGIN line of OPTIONS block
    call this%parser%GetBlock('OPTIONS', found, ierr, &
      supportOpenClose=.true., blockRequired=.false.)
    if (ierr /= 0) then
      ! end of file
      ermsg = 'End-of-file encountered while searching for' // &
              ' OPTIONS in OBS ' // &
              'input file "' // trim(this%inputFilename) // '"'
      call store_error(ermsg)
      call this%parser%StoreErrorUnit()
      call ustop()
    elseif (.not. found) then
      this%blockTypeFound = ''
      if (this%iout>0) write(this%iout,10)
    endif
    !
    ! -- parse OPTIONS entries
    if (found) then
      write(this%iout,60)
      readblockoptions: do
        call this%parser%GetNextLine(endOfBlock)
        if (endOfBlock) exit
        call this%parser%GetStringCaps(keyword)
        select case (keyword)
        case ('DIGITS')
          ! -- Specifies number of significant digits used writing simulated
          !    values to a text file. Default is 5 digits.
          if (localdigits==0) then
            localdigits = this%parser%GetInteger()
            if (localdigits < 1) then
              errormessage = 'Error in OBS input: Invalid value for DIGITS option'
              call store_error(errormessage)
              exit readblockoptions
            endif
            if (localdigits < 2) localdigits = 2
            if (localdigits > 16) localdigits = 16
            write(this%iout,40)localdigits
          else
            errormessage = 'Error in OBS input: DIGITS has already been defined'
            call store_error(errormessage)
            exit readblockoptions
          endif
        case ('PRINT_INPUT')
          this%echo = .true.
          write(this%iout,'(a)')'The PRINT_INPUT option has been specified.'
        case default
          errormessage = 'Error in OBS input: Unrecognized option: ' // &
                         trim(keyword)
          call store_error(errormessage)
          exit readblockoptions
        end select
      enddo readblockoptions
    endif
    !
    if (count_errors()>0) then
      call this%parser%StoreErrorUnit()
      call ustop()
    endif
    !
    write(this%iout,'(1x)')
    !
    ! -- Assign type variables
    if (localprecision>0) this%iprecision = localprecision
    if (localdigits>0) this%idigits = localdigits
    !
    return
  end subroutine read_obs_options

  subroutine define_fmts(this)
! **************************************************************************
! define_fmts
!   -- define output formats for single and continuous observations
! **************************************************************************
!
!    SPECIFICATIONS:
! --------------------------------------------------------------------------
    implicit none
    ! -- dummy
    class(ObsType) :: this
    ! formats
    50 format('(g',i2.2,'.',i2.2,')')
    !
    write(this%obsfmtcont,50)this%idigits+7, this%idigits
    return
  end subroutine define_fmts

  subroutine read_observations(this)
! **************************************************************************
! read_observations
!   -- read CONTINUOUS blocks from OBS input file
! **************************************************************************
!
!    SPECIFICATIONS:
! --------------------------------------------------------------------------
    implicit none
    ! -- dummy
    class(ObsType) :: this
    ! -- local
    !
    ! -- Read CONTINUOUS blocks and store observations
    call this%read_obs_blocks(this%outputFilename)
    !
    ! -- build headers
    call this%build_headers()
    !
    return
  end subroutine read_observations

  function get_num(this)
! **************************************************************************
! get_num
!   -- Return the number of observations contained in this ObsType object
! **************************************************************************
!
!    SPECIFICATIONS:
! --------------------------------------------------------------------------
    implicit none
    ! -- return
    integer(I4B) :: get_num
    ! -- dummy
    class(ObsType) :: this
    get_num = this%obsList%Count()
    return
  end function get_num

  subroutine build_headers(this)
! **************************************************************************
! build_headers
! -- Build headers for CSV-formatted and unformatted continuous-observation
! output files and write them to those files.
! Each formatted header will have the form: "time,obsname-1,obsname-2, ..."
! **************************************************************************
!
!    SPECIFICATIONS:
! --------------------------------------------------------------------------
    use iso_fortran_env, only: int32
    implicit none
    ! -- dummy
    class(ObsType), target :: this
    ! -- local
    integer(I4B) :: i, ii, idx, indx, iu, num, nunit
    integer(int32) :: nobs
    character(len=LENOBSNAME), pointer :: headr => null()
    character(len=LENOBSNAME)          :: nam
    character(len=4)                   :: clenobsname
    type(ObserveType),         pointer :: obsrv => null()
    type(ObsOutputType),       pointer :: obsOutput => null()
    !
    ! --
    num = this%obsList%Count()
    ! -- Cycle through observations to build the header(s)
    if (num>0) then
      do i=1,num
        obsrv => this%get_obs(i)
        ! -- header for file of continuous observations
        indx = obsrv%indxObsOutput
        obsOutput => this%obsOutputList%Get(indx)
        headr => obsOutput%header
        if (headr == '') then
          headr = 'time'
        endif
        nam = obsrv%Name
        call ExpandArray(obsOutput%obsnames)
        idx = size(obsOutput%obsnames)
        obsOutput%obsnames(idx) = nam
      enddo
    endif
    !
    ! -- Cycle through ObsOutputList to write headers
    !    to formatted and unformatted file(s).
    num = this%obsOutputList%Count()
    do i=1,num
      obsOutput => this%obsOutputList%Get(i)
      if (obsOutput%FormattedOutput) then
        ! -- write header to formatted file
        headr => obsOutput%header
        if (headr /= '') then
          nobs = obsOutput%nobs
          iu = obsOutput%nunit
          write(iu, '(a)', advance='NO') 'time'
          do ii = 1,nobs
            write(iu, '(a,a)', advance='NO') ',', trim(obsOutput%obsnames(ii))
          enddo
          write(iu, '(a)', advance='YES') ''
        endif
      else
        ! -- write header to unformatted file
        !    First 11 bytes are obs type and precision
        nunit = obsOutput%nunit
        if (this%iprecision==1) then
          ! -- single precision output
          write(nunit)'cont single'
        elseif (this%iprecision==2) then
          ! -- double precision output
          write(nunit)'cont double'
        endif
        ! -- write LENOBSNAME to bytes 12-15
        write(clenobsname,'(i4)')LENOBSNAME
        write(nunit)clenobsname
        ! -- write blanks to complete 100-byte header
        do ii=16,100
          write(nunit)' '
        enddo
        ! -- write NOBS
        nobs = obsOutput%nobs
        write(nunit)nobs
        ! -- write NOBS * (LENOBSNAME-character observation name)
        do ii=1,nobs
          write(nunit)obsOutput%obsnames(ii)
        enddo
      endif
    enddo
    !
    return
  end subroutine build_headers

  subroutine get_obs_array(this, nObs, obsArray)
! **************************************************************************
! get_obs_array
!   -- Get an array containing all observations in this ObsType object
! **************************************************************************
!
!    SPECIFICATIONS:
! --------------------------------------------------------------------------
    implicit none
    ! -- dummy
    class(ObsType), intent(inout) :: this
    integer(I4B), intent(out)   :: nObs
    type(ObsContainerType), dimension(:), pointer, intent(inout) :: obsArray
    ! -- local
    !
    nObs = this%get_num()
    if (associated(obsArray)) deallocate(obsArray)
    allocate(obsArray(nObs))
    !
    ! Get observations
    if (nObs > 0) then
      call this%populate_obs_array(nObs, obsArray)
    endif
    !
    return
  end subroutine get_obs_array

  function get_obs_datum(this, obsTypeID) result(obsDatum)
! **************************************************************************
! get_obs_datum
!   -- Return an ObsDataType object for the specified observation type
! **************************************************************************
!
!    SPECIFICATIONS:
! --------------------------------------------------------------------------
    implicit none
    ! -- dummy
    class(ObsType) :: this
    character(len=*), intent(in) :: obsTypeID
    type(ObsDataType), pointer :: obsDatum
    ! -- local
    integer(I4B) :: i
    character( len=MAXCHARLEN) :: ermsg
    !
    obsDatum => null()
    do i=1,MAXOBSTYPES
      if (this%obsData(i)%ObsTypeID == obsTypeID) then
        obsDatum => this%obsData(I)
        exit
      endif
    enddo
    !
    if (.not. associated(obsDatum)) then
      ermsg = 'Observation type not found: ' // trim(obsTypeID)
      call store_error(ermsg)
      call store_error_unit(this%inUnitObs)
      call ustop()
    endif
    !
    return
  end function get_obs_datum

  subroutine populate_obs_array(this, nObs, obsArray)
! **************************************************************************
! populate_obs_array
!   -- Populate obsArray with observations for specified package
! **************************************************************************
!
!    SPECIFICATIONS:
! --------------------------------------------------------------------------
    implicit none
    ! -- dummy
    class(ObsType), intent(inout) :: this
    integer(I4B),               intent(in)    :: nObs
    type(ObsContainerType), dimension(nObs), intent(inout) :: obsArray
    !
    ! -- local
    integer(I4B) :: i, n
    type(ObserveType), pointer :: obsrv => null()
    !
    n = this%get_num()
    do i=1,n
      obsrv => this%get_obs(i)
      obsArray(i)%obsrv => obsrv
    enddo
    !
    return
  end subroutine populate_obs_array

  function get_obs(this, indx) result(obsrv)
! **************************************************************************
! get_obs
!   -- Return the specified ObserveType object from the list of observations
! **************************************************************************
!
!    SPECIFICATIONS:
! --------------------------------------------------------------------------
    implicit none
    ! -- dummy
    class(ObsType) :: this
    integer(I4B), intent(in)   :: indx
    class(ObserveType), pointer :: obsrv
    ! -- local
    !
    obsrv => GetObsFromList(this%obsList, indx)
    !
    return
  end function get_obs

  subroutine read_obs_blocks(this, fname)
! **************************************************************************
! read_obs_blocks
!   -- read CONTINUOUS blocks from the OBS input file
! **************************************************************************
!
!    SPECIFICATIONS:
! --------------------------------------------------------------------------
    implicit none
    ! -- dummy
    class(ObsType), intent(inout) :: this
    character(len=*),      intent(inout) :: fname
    ! -- local
    integer(I4B) :: ierr, indexobsout, numspec
    logical :: fmtd, found, endOfBlock
    character(len=LENBIGLINE) :: pnamein, fnamein
    character(len=LENHUGELINE) :: line
    character(len=LINELENGTH) :: btagfound, ermsg, message, word
    character(len=20) :: accarg, bin, fmtarg
    type(ObserveType),     pointer :: obsrv => null()
    type(ObsOutputType),   pointer :: obsOutput => null()
    ! formats
    50 format(/,'Observations read from file "',a,'":',/, &
         'Name',38x,'Type',29x,'Time',9x,'Location data',/, &
         '----------------------------------------  -------------------------------', &
         '  -----------  --------------------------'  )
    !
    numspec = -1
    ermsg = ''
    !
    inquire(unit=this%parser%iuactive, name=pnamein)
    call GetFileFromPath(pnamein, fnamein)
    !
    if (this%echo) write(this%iout,50)trim(fnamein)
    !
    found = .true.
    readblocks: do
      if (.not. found) exit
      !
      call this%parser%GetBlock('*', found, ierr, .true., .false., btagfound)
      if (.not. found) then
        exit readblocks
      endif
      this%blockTypeFound = btagfound
      !
      ! Get keyword, which should be FILEOUT
      call this%parser%GetStringCaps(word)
      !
      ! -- get name of output file
      call this%parser%GetString(fname)
      ! Fname is the output file name defined in the BEGIN line of the block.
      if (fname == '') then
        message = 'Error reading OBS input file, likely due to bad' // &
                  ' block or missing file name.'
        call store_error(message)
        call this%parser%StoreErrorUnit()
        call ustop()
      elseif  (this%obsOutputList%ContainsFile(fname)) then
        ermsg = 'OBS outfile "' // trim(fname) // &
                '" is provided more than once.'
        call store_error(ermsg)
        call this%parser%StoreErrorUnit()
        call ustop()
      endif
      !
      ! -- look for BINARY option
      call this%parser%GetStringCaps(bin)
      if (bin == 'BINARY') then
        fmtarg = FORM
        accarg = ACCESS
        fmtd = .false.
      else
        fmtarg = 'FORMATTED'
        accarg = 'SEQUENTIAL'
        fmtd = .true.
      endif
      !
      ! -- open the output file
      numspec = 0
      call openfile(numspec, 0, fname, 'OBS OUTPUT', fmtarg, &
                    accarg, 'REPLACE')
      !
      ! -- add output file to list of output files and assign its
      !    FormattedOutput member appropriately
      call this%obsOutputList%Add(fname,numspec)
      indexobsout = this%obsOutputList%Count()
      obsOutput => this%obsOutputList%Get(indexobsout)
      obsOutput%FormattedOutput = fmtd
      !
      ! -- process lines defining observations
      select case (btagfound)
      case ('CONTINUOUS')
        if (word /= 'FILEOUT') then
          call store_error('CONTINUOUS keyword must be followed by ' //    &
            '"FILEOUT" then by filename.')
          call this%parser%StoreErrorUnit()
          call ustop()
        endif
        ! -- construct a continuous observation from each line in the block
        readblockcontinuous: do
          call this%parser%GetNextLine(endOfBlock)
          if (endOfBlock) exit
          call this%parser%GetCurrentLine(line)
          call ConstructObservation(obsrv, line, numspec, fmtd, &
                                    indexobsout, this%obsData, &
                                    this%parser%iuactive)
          ! -- increment number of observations
          !    to be written to this output file.
          obsOutput => this%obsOutputList%Get(indexobsout)
          obsOutput%nobs = obsOutput%nobs + 1
          call AddObsToList(this%obsList, obsrv)
          if (this%echo) then
            call obsrv%WriteTo(this%iout)
          endif
        enddo readblockcontinuous
      case default
        ermsg = 'Error: Observation type not recognized: '// &
                       trim(btagfound)
        call store_error(ermsg)
        call this%parser%StoreErrorUnit()
        call ustop()
      end select
    enddo readblocks
    !
    return
  end subroutine read_obs_blocks

  subroutine write_continuous_simvals(this)
! **************************************************************************
! write_continuous_simvals
! Subroutine: (1) for each continuous observation, writes value to output
! **************************************************************************
!
!    SPECIFICATIONS:
! --------------------------------------------------------------------------
    implicit none
    ! -- dummy
    class(ObsType), intent(inout) :: this
    ! -- local
    integer(I4B)                :: i, iprec, numobs
    character(len=20)           :: fmtc
    real(DP)                    :: simval
    class(ObserveType), pointer :: obsrv => null()
    !---------------------------------------------------------------------------
    !
    ! Write simulated values for observations
    iprec = this%iprecision
    fmtc = this%obsfmtcont
    ! -- iterate through all observations
    numobs = this%obsList%Count()
    do i=1,numobs
      obsrv => this%get_obs(i)
      ! -- continuous observation
      simval = obsrv%CurrentTimeStepEndValue
      if (obsrv%FormattedOutput) then
        call write_fmtd_cont(fmtc, obsrv, this%obsOutputList, simval)
      else
        call write_unfmtd_cont(obsrv, iprec, this%obsOutputList, simval)
      endif
    enddo
    !
    return
  end subroutine write_continuous_simvals

end module ObsModule
