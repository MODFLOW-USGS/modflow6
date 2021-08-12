!> @brief This module contains the GwtSsmInput Module 
!!
!! This module contains the code for reading and storing a
!! generic input file of source and sink concentrations.
!!
!<
module GwtSsmInputModule
  
  use KindModule,                   only: DP, LGP, I4B
  use ConstantsModule,              only: LENPACKAGENAME, LENMODELNAME, &
                                          LENMEMPATH, DZERO, LENFTYPE,  &
                                          LINELENGTH, TABLEFT, TABCENTER
  use SimVariablesModule,           only: errmsg
  use SimModule,                    only: store_error, count_errors
  use MemoryHelperModule,           only: create_mem_path
  use BlockParserModule,            only: BlockParserType
  use TimeSeriesManagerModule,      only: TimeSeriesManagerType, tsmanager_cr
  use TableModule,                  only: TableType, table_cr

  implicit none
  private
  public :: GwtSsmInputType

  character(len=LENFTYPE)       :: ftype = 'SSMI'
  character(len=LENPACKAGENAME) :: text  = '       SSM INPUT'

  !> @brief Derived type for managing SSMI input 
  !!
  !! This derived type will read and process an SSMI input file,
  !! make time series interpolations, and provide concentrations to
  !! the SSM package that correspond to an individual GWF stress
  !! package.
  !!
  !<
  type :: GwtSsmInputType
    
    character(len=LENMODELNAME)                 :: name_model      = ''         !< the name of the model that contains this package
    character(len=LENPACKAGENAME)               :: packName        = ''         !< name of the package
    character(len=LENMEMPATH)                   :: memoryPath      = ''         !< the location in the memory manager where the variables are stored
    integer(I4B), pointer                       :: id => null()                 !< id number for this ssmi package
    integer(I4B), pointer                       :: inunit => null()             !< unit number for input
    integer(I4B), pointer                       :: iout => null()               !< unit number for output
    integer(I4B), pointer                       :: maxbound => null()           !< length of dblvec
    integer(I4B), pointer                       :: ionper => null()             !< stress period for next data
    integer(I4B), pointer                       :: lastonper => null()          !< last value of ionper (for checking)
    integer(I4B), pointer                       :: iprpak => null()             !< flag for printing input
    real(DP), dimension(:), pointer, contiguous :: dblvec => null()             !< vector of floats read from file
    type(BlockParserType)                       :: parser                       !< parser object for reading blocks of information
    type(TimeSeriesManagerType), pointer        :: TsManager => null()          !< time series manager
    type(TableType), pointer                    :: inputtab => null()           !< input table object 
    
  contains

    procedure :: initialize
    procedure :: allocate_scalars
    procedure :: read_options
    procedure :: read_dimensions
    procedure :: allocate_arrays
    procedure :: get_value
    procedure :: set_value
    procedure :: ssmi_rp
    procedure :: ssmi_ad
    procedure :: ssmi_da
    procedure :: read_check_ionper
    
  end type GwtSsmInputType
  
  contains
  
  !> @ brief Initialize the SSMI type
  !!
  !! Initialize the SSMI object by setting up the parser,
  !! and time series manager, reading options and dimensions,
  !! and allocating memory.
  !!
  !<
  subroutine initialize(this, id, inunit, iout, name_model)
    ! -- dummy variables
    class(GwtSsmInputType) :: this              !<  GwtSsmInputType
    integer(I4B), intent(in) :: id              !<  id number for this ssmi package
    integer(I4B), intent(in) :: inunit          !<  unit number for input
    integer(I4B), intent(in) :: iout            !<  unit number for output
    character(len=*), intent(in) :: name_model  !<  character string containing model name
    ! -- local
    !
    ! -- construct the memory path
    write(this%packName,'(a, i0)') 'SSMI' // '-', id
    this%name_model = name_model
    this%memoryPath = create_mem_path(this%name_model, this%packName)
    !
    ! -- allocate scalar variables
    call this%allocate_scalars()
    !
    ! -- assign member values
    this%id = id
    this%inunit = inunit
    this%iout = iout
    !
    ! -- Setup the parser
    call this%parser%Initialize(this%inunit, this%iout)
    !
    ! -- Setup the time series manager
    call tsmanager_cr(this%TsManager, this%iout)
    !
    ! -- read options
    call this%read_options()
    !
    ! -- read dimensions
    call this%read_dimensions()
    !
    ! -- allocate arrays
    call this%allocate_arrays()
    !
    ! -- Now that time series are read, call define
    call this%tsmanager%tsmanager_df()
    !
    ! -- return
    return
  end subroutine initialize
  
  !> @ brief Allocate package scalars
  !!
  !!  Allocate and initialize package scalars. 
  !!
  !<
  subroutine allocate_scalars(this)
    ! -- modules
    use MemoryManagerModule, only: mem_allocate
    ! -- dummy variables
    class(GwtSsmInputType) :: this  !< GwtSsmInputType object
    !
    ! -- allocate scalars in memory manager
    call mem_allocate(this%id, 'ID', this%memoryPath)
    call mem_allocate(this%inunit, 'INUNIT', this%memoryPath)
    call mem_allocate(this%iout, 'IOUT', this%memoryPath)
    call mem_allocate(this%maxbound, 'MAXBOUND', this%memoryPath)
    call mem_allocate(this%ionper, 'IONPER', this%memoryPath)
    call mem_allocate(this%lastonper, 'LASTONPER', this%memoryPath)
    call mem_allocate(this%iprpak, 'IPRPAK', this%memoryPath)
    !
    ! -- allocate special derived types
    allocate(this%TsManager)
    !
    ! -- initialize
    this%id = 0
    this%inunit = 0
    this%iout = 0
    this%maxbound = 0
    this%ionper = 0
    this%lastonper = 0
    this%iprpak = 0
    !
    ! -- return
    return
  end subroutine allocate_scalars

  !> @ brief Read options for package
  !!
  !!  Read options for this package. 
  !!
  !<
  subroutine read_options(this)
    ! -- modules
    ! -- dummy
    class(GwtSsmInputType) :: this
    ! -- local
    character(len=LINELENGTH) :: keyword, fname
    integer(I4B) :: ierr
    logical :: isfound, endOfBlock
    ! -- formats
    character(len=*), parameter :: fmtiprpak =                                 &
      "(4x,'SSMI INFORMATION WILL BE PRINTED TO LISTING FILE.')"
    character(len=*), parameter :: fmtts = &
      "(4x, 'TIME-SERIES DATA WILL BE READ FROM FILE: ', a)"
    !
    ! -- get options block
    call this%parser%GetBlock('OPTIONS', isfound, ierr, blockRequired=.false., &
                              supportOpenClose=.true.)
    !
    ! -- parse options block if detected
    if (isfound) then
      write(this%iout,'(1x,a)')'PROCESSING SSMI OPTIONS'
      do
        call this%parser%GetNextLine(endOfBlock)
        if (endOfBlock) exit
        call this%parser%GetStringCaps(keyword)
        select case (keyword)
          case ('PRINT_INPUT')
            this%iprpak = 1
            write(this%iout, fmtiprpak)
          case ('TS6')
            call this%parser%GetStringCaps(keyword)
            if(trim(adjustl(keyword)) /= 'FILEIN') then
              errmsg = 'TS6 keyword must be followed by "FILEIN" ' //          &
                      'then by filename.'
              call store_error(errmsg)
            endif
            call this%parser%GetString(fname)
            write(this%iout,fmtts)trim(fname)
            call this%TsManager%add_tsfile(fname, this%inunit)
          case default
            write(errmsg,'(4x,a,a)') 'UNKNOWN SSMI OPTION: ', trim(keyword)
            call store_error(errmsg)
            call this%parser%StoreErrorUnit()
        end select
      end do
      write(this%iout,'(1x,a)')'END OF SSMI OPTIONS'
    end if
    !
    ! -- Return
    return
  end subroutine read_options

  !> @ brief Read dimensions for package
  !!
  !!  Read dimensions for this package. 
  !!
  !<
  subroutine read_dimensions(this)
    ! -- dummy variables
    class(GwtSsmInputType),intent(inout) :: this  !< BndType object
    ! -- local variables
    character(len=LINELENGTH) :: keyword
    logical(LGP) :: isfound
    logical(LGP) :: endOfBlock
    integer(I4B) :: ierr
    !
    ! -- get dimensions block
    call this%parser%GetBlock('DIMENSIONS', isfound, ierr, &
                              supportOpenClose=.true.)
    !
    ! -- parse dimensions block if detected
    if (isfound) then
      write(this%iout,'(/1x,a)')'PROCESSING '//trim(adjustl(text))// &
        ' DIMENSIONS'
      do
        call this%parser%GetNextLine(endOfBlock)
        if (endOfBlock) exit
        call this%parser%GetStringCaps(keyword)
        select case (keyword)
          case ('MAXBOUND')
            this%maxbound = this%parser%GetInteger()
            write(this%iout,'(4x,a,i7)') 'MAXBOUND = ', this%maxbound
          case default
            write(errmsg,'(a,3(1x,a))') &
              'UNKNOWN', trim(text), 'DIMENSION:', trim(keyword)
            call store_error(errmsg)
        end select
      end do
      !
      write(this%iout,'(1x,a)')'END OF '//trim(adjustl(text))//' DIMENSIONS'
    else
      call store_error('REQUIRED DIMENSIONS BLOCK NOT FOUND.')
      call this%parser%StoreErrorUnit()
    end if
    !
    ! -- verify dimensions were set
    if(this%maxbound <= 0) then
      write(errmsg, '(a)') 'MAXBOUND MUST BE AN INTEGER GREATER THAN ZERO.'
      call store_error(errmsg)
    end if
    !
    ! -- terminate if there are errors
    if (count_errors() > 0) then
      call this%parser%StoreErrorUnit()
    end if
    !
    ! -- return
    return
  end subroutine read_dimensions

  !> @ brief Allocate package arrays
  !!
  !!  Allocate and initialize package arrays. 
  !!
  !<
  subroutine allocate_arrays(this)
    ! -- modules
    use MemoryManagerModule, only: mem_allocate
    ! -- dummy variables
    class(GwtSsmInputType) :: this      !< GwtSsmInputType object
    ! -- local
    integer(I4B) :: i
    !
    ! -- allocate array
    call mem_allocate(this%dblvec, this%maxbound, 'DBLVEC', this%memoryPath)
    !
    ! -- initialize dblvec to zero
    do i = 1, this%maxbound
      this%dblvec(i) = DZERO
    end do
    !
    ! -- return
    return
  end subroutine allocate_arrays
  
  !> @ brief Get the data value from this package
  !!
  !!  Get the floating point value from the dblvec array. 
  !!
  !<
  function get_value(this, ientry) result(value)
    class(GwtSsmInputType) :: this      !< GwtSsmInputType object
    integer(I4B), intent(in) :: ientry  !< index of the data to return
    real(DP) :: value
    value = this%dblvec(ientry)
    return
  end function get_value

  !> @ brief Read and prepare
  !!
  !!  Read and prepare the period data block and fill dblvec
  !!  if the next period block corresponds to this time step. 
  !!
  !<
  subroutine ssmi_rp(this)
    ! -- modules
    use TdisModule, only: kper, nper
    ! -- dummy
    class(GwtSsmInputType),intent(inout) :: this  !< GwtSsmInputType object
    ! -- local
    character(len=LINELENGTH) :: title
    character(len=LINELENGTH) :: line
    character(len=LINELENGTH) :: tabletext
    logical :: isfound
    logical :: endOfBlock
    integer(I4B) :: ierr
    integer(I4B) :: ival
    ! -- formats
    character(len=*),parameter :: fmtblkerr = &
      "('Looking for BEGIN PERIOD iper.  Found ', a, ' instead.')"
    character(len=*),parameter :: fmtlsp = &
      "(1X,/1X,'REUSING ',A,'S FROM LAST STRESS PERIOD')"
    !
    ! -- Set ionper to the stress period number for which a new block of data
    !    will be read.
    if(this%inunit == 0) return
    !
    ! -- get stress period data
    if (this%ionper < kper) then
      !
      ! -- get period block
      call this%parser%GetBlock('PERIOD', isfound, ierr, &
                                supportOpenClose=.true.)
      if(isfound) then
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
          write(errmsg, fmtblkerr) adjustl(trim(line))
          call store_error(errmsg, terminate=.TRUE.)
        end if
      end if
    end if
    !
    ! -- Read data if ionper == kper
    if(this%ionper == kper) then
      !
      ! -- Remove all time-series and time-array-series links associated with
      !    this package.
      !    Do not reset as we are using a "settings" approach here in which the
      !    settings remain the same until the user changes them.
      ! call this%TsManager%Reset(this%packName)
      !
      ! -- setup table for period data
      if (this%iprpak /= 0) then
        !
        ! -- reset the input table object
        title = trim(adjustl(text)) // ' PACKAGE (' //                      &
                'SSMI' //') DATA FOR PERIOD'
        write(title, '(a,1x,i6)') trim(adjustl(title)), kper
        call table_cr(this%inputtab, ftype, title)
        call this%inputtab%table_df(1, 3, this%iout, finalize=.FALSE.)
        tabletext = 'NUMBER'
        call this%inputtab%initialize_column(tabletext, 10, alignment=TABCENTER)
        tabletext = 'DATA TYPE'
        call this%inputtab%initialize_column(tabletext, 20, alignment=TABLEFT)
        write(tabletext, '(a,1x,i6)') 'VALUE'
        call this%inputtab%initialize_column(tabletext, 15, alignment=TABCENTER)
      end if
      !
      ! -- read data
      do
        call this%parser%GetNextLine(endOfBlock)
        if (endOfBlock) exit

        ival = this%parser%GetInteger()
        if (ival < 1 .or. ival > this%maxbound) then
          write(errmsg,'(2(a,1x),i0,a)')                                        &
            'IVAL must be greater than 0 and',                                  &
            'less than or equal to ', this%maxbound, '.'
          call store_error(errmsg)
          cycle
        end if
        !
        ! -- set stress period data
        call this%set_value(ival)
        !
        ! -- write line to table
        if (this%iprpak /= 0) then
          call this%parser%GetCurrentLine(line)
          call this%inputtab%line_to_columns(line)
        end if
      end do
      if (this%iprpak /= 0) then
        call this%inputtab%finalize_table()
      end if
    !
    ! -- using data from the last stress period
    else
      write(this%iout,fmtlsp) trim(ftype)
    end if
    !
    ! -- write summary of maw well stress period error messages
    if (count_errors() > 0) then
      call this%parser%StoreErrorUnit()
    end if
    !
    ! -- return
    return
  end subroutine ssmi_rp

  !> @ brief Advance
  !!
  !!  Call the advance method on the time series so that new values
  !!  are interpolated and entered into dblvec
  !!
  !<
  subroutine ssmi_ad(this)
    ! -- modules
    ! -- dummy
    class(GwtSsmInputType),intent(inout) :: this  !< GwtSsmInputType object
    ! -- local
    !
    ! -- Advance the time series
    call this%TsManager%ad()
    !
    ! -- return
    return
  end subroutine ssmi_ad
    
  !> @ brief Deallocate variables
  !!
  !!  Deallocate and nullify package variables. 
  !!
  !<
  subroutine ssmi_da(this)
    ! -- modules
    use MemoryManagerModule, only: mem_deallocate
    ! -- dummy variables
    class(GwtSsmInputType) :: this  !< GwtSsmInputType object
    !
    ! -- deallocate arrays in memory manager
    call mem_deallocate(this%dblvec)
    !
    ! -- deallocate scalars in memory manager
    call mem_deallocate(this%id)
    call mem_deallocate(this%inunit)
    call mem_deallocate(this%iout)
    call mem_deallocate(this%maxbound)
    call mem_deallocate(this%ionper)
    call mem_deallocate(this%lastonper)
    call mem_deallocate(this%iprpak)
    !
    ! -- deallocate derived types
    call this%TsManager%da()
    deallocate(this%TsManager)
    nullify(this%TsManager)
    !
    ! -- return
    return
  end subroutine ssmi_da

  !> @ brief Check ionper
  !!
  !!  Generic method to read and check ionperiod, which is used to determine 
  !!  if new period data should be read from the input file. The check of 
  !!  ionperiod also makes sure periods are increasing in subsequent period
  !!  data blocks.  Copied from NumericalPackage
  !!
  !<
  subroutine read_check_ionper(this)
    ! -- modules
    use TdisModule, only: kper
    ! -- dummy variables
    class(GwtSsmInputType), intent(inout) :: this  !< GwtSsmInputType object
    !
    ! -- save last value and read period number
    this%lastonper = this%ionper
    this%ionper = this%parser%GetInteger()
    !
    ! -- make check
    if (this%ionper <= this%lastonper) then
      write(errmsg, '(a, i0)') &
        'ERROR IN STRESS PERIOD ', kper
      call store_error(errmsg)
      write(errmsg, '(a, i0)') &
        'PERIOD NUMBERS NOT INCREASING.  FOUND ', this%ionper
      call store_error(errmsg)
      write(errmsg, '(a, i0)') &
        'BUT LAST PERIOD BLOCK WAS ASSIGNED ', this%lastonper
      call store_error(errmsg)
      call this%parser%StoreErrorUnit()
    endif
    !
    ! -- return
    return
  end subroutine read_check_ionper
  
  !> @ brief Set the data value from the input file
  !!
  !!  Set the floating point value in the dblvec array using strings
  !!  returned from the parser.  Allow for time series names.
  !!
  !<
  subroutine set_value(this, ival)
    ! -- modules
    use TimeSeriesManagerModule, only: read_value_or_time_series_adv
    ! -- dummy
    class(GwtSsmInputType), intent(inout) :: this  !< GwtSsmInputType object
    integer(I4B), intent(in) :: ival
    ! -- local
    character(len=LINELENGTH) :: keyword
    integer(I4B) :: jj
    real(DP), pointer :: bndElem => null()
    !
    ! -- read remainder of variables on the line
    call this%parser%GetStringCaps(keyword)
    select case(keyword)
    case('CONCENTRATION')
      call this%parser%GetString(text)
      jj = 1    ! For CONCENTRATION
      bndElem => this%dblvec(ival)
      call read_value_or_time_series_adv(text, ival, jj, bndElem, this%packName,   &
                                          'BND', this%tsManager, this%iprpak,   &
                                          'CONCENTRATION')
      
    end select
    return
  end subroutine set_value

  
end module GwtSsmInputModule