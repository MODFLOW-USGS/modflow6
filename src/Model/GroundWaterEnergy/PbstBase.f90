!> @brief This module contains common process-based stream temperature functionality
!!
!! This module contains methods for implementing functionality associated with
!! heat fluxes to a stream reach.  Four sources of thermal energy commonly 
!! accounted for in process-based stream temperature modeling include short-
!! wave radiation, long-wave radiation, sensible heat flux, and latent heat 
!! flux.  
!<
module PbstBaseModule
  use ConstantsModule, only: LINELENGTH, MAXCHARLEN, DZERO, LGP
  use KindModule, only: I4B, DP
  use NumericalPackageModule, only: NumericalPackageType
  use SimModule, only: count_errors, store_error, ustop
  use SimVariablesModule, only: errmsg
  use TdisModule, only: kper, nper, kstp
  use TimeSeriesLinkModule, only: TimeSeriesLinkType
  use TimeSeriesManagerModule, only: TimeSeriesManagerType, tsmanager_cr, &
                                     read_value_or_time_series_adv

  implicit none

  private
  
  public :: PbstBaseType
  public :: pbstbase_da
  
  type, abstract, extends(NumericalPackageType) :: PbstBaseType
    
    integer(I4B), pointer :: ncv => null() !< number of control volumes
    logical, pointer, public :: active => null() !< logical indicating if a sensible heat flux object is active
    character(len=LINELENGTH), pointer, public :: inputFilename => null() !< a particular pbst input file name, could be for sensible heat flux or latent heat flux subpackages, for example
    type(TimeSeriesManagerType), pointer, private :: tsmanager => null()
  
  contains

    procedure :: init
    procedure :: ar
    procedure, private :: read_options
    procedure :: pbst_options
    procedure(read_option), deferred :: read_option
    procedure, private :: pbstbase_allocate_scalars
    procedure :: da => pbstbase_da
  
  end type PbstBaseType
  
  abstract interface
  
    !> @brief Announce package and set pointers to variables
    !!
    !! Deferred procedure called by the PbstBaseType code to process a single
    !! keyword from the OPTIONS block of the package input file.
    !<
    function read_option(this, keyword) result(success)
      ! -- modules
      import PbstBaseType
      ! -- dummy
      class(PbstBaseType) :: this
      character(len=*), intent(in) :: keyword
      ! -- return
      logical :: success
    end function
  
  end interface
  
contains

  !> @brief Initialize the PbstBaseType object
  !!
  !! Allocate and initialize data members of the object.
  !<
  subroutine init(this, name_model, pakname, ftype, inunit, iout)
    ! -- dummy
    class(PbstBaseType) :: this
    character(len=*), intent(in) :: name_model
    character(len=*), intent(in) :: pakname
    character(len=*), intent(in) :: ftype
    integer(I4B), intent(in) :: inunit
    integer(I4B), intent(in) :: iout
    !
    call this%set_names(1, name_model, pakname, ftype)
    call this%pbstbase_allocate_scalars()
    this%inunit = inunit
    this%iout = iout
    call this%parser%Initialize(this%inunit, this%iout)
  end subroutine init
  
  !> @brief Allocate and read
  !!
  !!  Method to allocate and read static data for the SHF package
  !<
  subroutine ar(this)
    ! -- dummy
    class(PbstBaseType) :: this !< ShfType object
    !
    ! -- Create time series manager
    call tsmanager_cr(this%tsmanager, this%iout, &
                      removeTsLinksOnCompletion=.true., &
                      extendTsToEndOfSimulation=.true.)
    !
    ! -- Read options
    call this%read_options()
  end subroutine ar


  !> @brief Read the SHF-specific options from the OPTIONS block
  !<
  subroutine read_options(this)
    ! -- dummy
    class(PbstBaseType) :: this
    ! -- local
    character(len=LINELENGTH) :: keyword
    character(len=MAXCHARLEN) :: fname
    logical :: isfound
    logical :: endOfBlock
    logical(LGP) :: foundchildclassoption
    integer(I4B) :: ierr
    ! -- formats
    character(len=*), parameter :: fmtts = &
      &"(4x, 'TIME-SERIES DATA WILL BE READ FROM FILE: ', a)"
    !
    ! -- Get options block
    call this%parser%GetBlock('OPTIONS', isfound, ierr, &
                              blockRequired=.false., supportOpenClose=.true.)
    !
    ! -- Parse options block if detected
    if (isfound) then
      write (this%iout, '(1x,a)') &
        'PROCESSING '//trim(adjustl(this%packName))//' OPTIONS'
      do
        call this%parser%GetNextLine(endOfBlock)
        if (endOfBlock) then
          exit
        end if
        call this%parser%GetStringCaps(keyword)
        select case (keyword)
        case ('PRINT_INPUT')
          this%iprpak = 1
          write (this%iout, '(4x,a)') 'TIME-VARYING INPUT WILL BE PRINTED.'
        case ('TS6')
          !
          ! -- Add a time series file
          call this%parser%GetStringCaps(keyword)
          if (trim(adjustl(keyword)) /= 'FILEIN') then
            errmsg = &
              'TS6 keyword must be followed by "FILEIN" then by filename.'
            call store_error(errmsg)
            call this%parser%StoreErrorUnit()
            call ustop()
          end if
          call this%parser%GetString(fname)
          write (this%iout, fmtts) trim(fname)
          call this%tsmanager%add_tsfile(fname, this%inunit)
        case default
          !
          ! -- Check for child class options
          call this%pbst_options(keyword, foundchildclassoption)
          !
          ! -- Defer to subtype to read the option;
          ! -- if the subtype can't handle it, report an error
          if (.not. this%read_option(keyword)) then
            write (errmsg, '(a,3(1x,a),a)') &
              'Unknown', trim(adjustl(this%packName)), "option '", &
              trim(keyword), "'."
            call store_error(errmsg)
          end if
        end select
      end do
      write (this%iout, '(1x,a)') &
        'END OF '//trim(adjustl(this%packName))//' OPTIONS'
    end if
  end subroutine read_options

  !> @ brief Read additional options for sub-package
  !!
  !!  Read additional options for the SFE boundary package. This method should
  !!  be overridden by option-processing routine that is in addition to the 
  !!  base options available for all PbstBase packages.
  !<
  subroutine pbst_options(this, option, found)
    ! -- dummy
    class(PbstBaseType), intent(inout) :: this !< PbstBaseType object
    character(len=*), intent(inout) :: option !< option keyword string
    logical(LGP), intent(inout) :: found !< boolean indicating if the option was found
    !
    ! Return with found = .false.
    found = .false.
  end subroutine pbst_options

  !> @brief Allocate scalar variables
  !!
  !! Allocate scalar data members of the object.
  !<
  subroutine pbstbase_allocate_scalars(this)
    ! -- modules
    use MemoryManagerModule, only: mem_allocate
    ! -- dummy
    class(PbstBaseType) :: this
    !
    allocate (this%active)
    allocate (this%inputFilename)
    !
    ! -- initialize
    this%active = .false.
    this%inputFilename = ''
    !
    ! -- call standard NumericalPackageType allocate scalars
    call this%NumericalPackageType%allocate_scalars()
    !
    ! -- allocate
    call mem_allocate(this%ncv, 'NCV', this%memoryPath)
    !
    ! -- initialize
    this%ncv = 0
    !
    ! -- allocate time series manager
    allocate (this%tsmanager)
  end subroutine pbstbase_allocate_scalars

  !> @brief Deallocate package memory
  !!
  !! Deallocate package scalars and arrays.
  !<
  subroutine pbstbase_da(this)
    ! -- modules
    use MemoryManagerModule, only: mem_deallocate
    ! -- dummy
    class(PbstBaseType) :: this
    !
    deallocate (this%active)
    deallocate (this%inputFilename)
    !
    ! -- Deallocate time series manager
    deallocate (this%tsmanager)
    ! 
    ! -- deallocate scalars
    call mem_deallocate(this%ncv)
    !
    ! -- Deallocate parent
    call this%NumericalPackageType%da()
  end subroutine pbstbase_da

end module PbstBaseModule