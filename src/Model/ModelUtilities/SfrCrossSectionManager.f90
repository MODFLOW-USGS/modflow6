module sfrCrossSectionManager

  use KindModule, only: DP, I4B, LGP
  use ConstantsModule, only: DZERO, DONE, &
                             LINELENGTH  

  use SimVariablesModule, only: errmsg, warnmsg
  use TableModule, only: TableType, table_cr

  implicit none
  
  public :: SfrCrossSection
  public :: cross_section_cr

  type :: SfrCrossSectionType
    integer(I4B), pointer :: npoints
    real(DP), pointer, dimension(:), contiguous :: station => null()
    real(DP), pointer, dimension(:), contiguous :: depth => null()
    logical(LGP), pointer, dimension(:), contiguous :: valid => null()
  end type SfrCrossSectionType

  type :: SfrCrossSection
    integer(I4B), pointer :: iout => null()
    integer(I4B), pointer :: iprpak => null()
    integer(I4B), pointer :: nreaches => null()
    integer(I4B), pointer :: invalid => null()
    character(len=LINELENGTH), dimension(:), allocatable :: filenames
    integer(I4B), pointer, dimension(:), contiguous :: npoints => null()
    type(SfrCrossSectionType), pointer, dimension(:), contiguous :: cross_sections => null()
    type(TableType), pointer :: inputtab => null()

    
    contains
  
    !
    ! -- public procedures
    procedure, public :: initialize
    procedure, public :: read_table
    procedure, public :: get_ncrossptstot
    procedure, public :: output
    procedure, public :: pack
    procedure, public :: destroy
    !
    ! -- private procedures
    procedure, private :: validate

  end type SfrCrossSection
  
  contains

  !> @brief Create a cross-section object
  !!
  !! Subroutine to calculate the maximum top width for a reach using the
  !! cross-section station data.
  !<
  subroutine cross_section_cr(this, iout, iprpak, nreaches)
    ! -- dummy variables
    type(SfrCrossSection), pointer :: this         !< SfrCrossSection object
    integer(I4B), pointer, intent(in) :: iout      !< model listing file 
    integer(I4B), pointer, intent(in) :: iprpak    !< flag for printing table input data
    integer(I4B), pointer, intent(in) :: nreaches  !< number of reaches
    !
    ! -- check if table already associated and reset if necessary
    if (associated(this)) then
      call this%destroy()
      deallocate(this)
      nullify(this)
    end if
    !
    ! -- Create the object
    allocate(this)
    !
    ! -- initialize scalars
    this%iout => iout
    this%iprpak => iprpak
    this%nreaches => nreaches
    !
    ! -- Return
    return
  end subroutine cross_section_cr

  !> @brief Initialize a cross-section object
  !!
  !! Subroutine to inititialize the cross-section object with the current 
  !! data.
  !!
  !<
  subroutine initialize(this, ncrossptstot, ncrosspts, iacross, station, depth)
    ! -- dummy variables
    class(SfrCrossSection) :: this                                   !< SfrCrossSection object
    integer(I4B), intent(in) :: ncrossptstot                         !< total number of cross-section points
    integer(I4B), dimension(this%nreaches), intent(in) :: ncrosspts  !< pointers to cross-section data in data vector
    integer(I4B), dimension(this%nreaches+1), intent(in) :: iacross  !< pointers to cross-section data in data vector
    real(DP), dimension(ncrossptstot), intent(in) :: station         !< cross-section station data
    real(DP), dimension(ncrossptstot), intent(in) :: depth           !< cross-section depth data
    ! -- local variables
    integer(I4B) :: i
    integer(I4B) :: n
    integer(I4B) :: npoints
    integer(I4B) :: i0
    integer(I4B) :: i1
    integer(I4B) :: ipos
    !
    ! -- allocate scalars
    allocate(this%invalid)
    !
    ! -- initialize scalars
    this%invalid = 0
    !
    ! -- create cross-section container
    allocate(this%filenames(this%nreaches))
    allocate(this%npoints(this%nreaches))
    allocate(this%cross_sections(this%nreaches))
    do n = 1, this%nreaches
      npoints = ncrosspts(n)
      allocate(this%cross_sections(n)%npoints)
      allocate(this%cross_sections(n)%station(npoints))
      allocate(this%cross_sections(n)%depth(npoints))
      allocate(this%cross_sections(n)%valid(npoints))
    end do
    !
    ! -- fill cross-section container with current values
    do n = 1, this%nreaches
      this%filenames(n) = 'NONE'
      this%cross_sections(n)%npoints = ncrosspts(n)
      this%npoints(n) = ncrosspts(n)
      i0 = iacross(n)
      i1 = iacross(n + 1) - 1
      ipos = 1
      do i = i0, i1
        this%cross_sections(n)%station(ipos) = station(i)
        this%cross_sections(n)%depth(ipos) = depth(i)
        this%cross_sections(n)%valid(ipos) = .TRUE.
        ipos = ipos + 1
      end do
    end do
    !
    ! -- return
    return
  end subroutine initialize

  !> @brief Read a cross-section table
  !!
  !! Subroutine to read a cross-section table file for a reach.
  !!
  !<
  subroutine read_table(this, irch, width, filename)
    use ConstantsModule, only: IUOC
    use InputOutputModule, only: openfile
    use SimModule, only: store_error
    use BlockParserModule, only: BlockParserType
    ! -- dummy variables
    class(SfrCrossSection) :: this             !< SfrCrossSection object
    integer(I4B), intent(in) :: irch           !< current reach
    real(DP), intent(in) :: width              !< reach width
    character(len=*), intent(in) :: filename   !< table file with station depth data
    ! -- local variables
    character(len=LINELENGTH) :: tag
    character(len=LINELENGTH) :: keyword
    integer(I4B) :: ierr
    logical :: isfound
    logical :: endOfBlock
    integer(I4B) :: iu
    integer(I4B) :: n
    integer(I4B) :: j
    integer(I4B) :: ipos
    integer(I4B) :: jmin
    type(BlockParserType) :: parser
    !
    ! -- initialize local variables
    j = 0
    n = 0
    jmin = 2
    !
    ! -- create a tag with the file name and reach number
    write(tag, "('Reach',1x,i0,1x,'(',a, ')')") &
      irch, trim(adjustl(filename)) 
    !
    ! -- open the table file
    iu = IUOC
    call openfile(iu, this%iout, filename, 'SFR TABLE')
    call parser%Initialize(iu, this%iout)
    !
    ! -- get dimensions block
    call parser%GetBlock('DIMENSIONS', isfound, ierr, supportOpenClose=.true.)
    !
    ! -- parse table dimensions block if detected
    if (isfound) then
      !
      ! -- process the table dimension data
      if (this%iprpak /= 0) then
        write(this%iout,'(/1x,a)')                                               &
          'PROCESSING ' // trim(adjustl(tag)) // ' DIMENSIONS'
      end if
      readdims: do
        call parser%GetNextLine(endOfBlock)
        if (endOfBlock) exit
        call parser%GetStringCaps(keyword)
        select case (keyword)
          case ('NROW')
            n = parser%GetInteger()
            if (n < 1) then
              write(errmsg,'(a)') 'TABLE NROW MUST BE > 0'
              call store_error(errmsg)
            end if
          case ('NCOL')
            j = parser%GetInteger()
            jmin = 2
            if (j < jmin) then
              write(errmsg,'(a,1x,i0)') 'TABLE NCOL MUST BE >= ', jmin
              call store_error(errmsg)
            end if
          case default
            write(errmsg,'(a,a)') &
              'UNKNOWN '//trim(adjustl(tag))//' DIMENSIONS KEYWORD: ', trim(keyword)
            call store_error(errmsg)
        end select
      end do readdims
      if (this%iprpak /= 0) then
        write(this%iout,'(1x,a)') &
          'END OF ' // trim(adjustl(tag)) // ' DIMENSIONS'
      end if
    else
      call store_error('REQUIRED DIMENSIONS BLOCK NOT FOUND.')
    end if
    !
    ! -- check that ncol and nrow have been specified
    if (n < 1) then
      write(errmsg,'(a)') &
        'NROW NOT SPECIFIED IN THE TABLE DIMENSIONS BLOCK'
      call store_error(errmsg)
    end if
    if (j < 1) then
      write(errmsg,'(a)') &
        'NCOL NOT SPECIFIED IN THE TABLE DIMENSIONS BLOCK'
      call store_error(errmsg)
    end if
    !
    ! -- only read the table data if n and j are specified to be greater
    !    than zero - an error condition exists if n * j = 0
    if (n * j > 0) then
      !
      ! -- set the filename and reset the number of points
      this%filenames(irch) = filename
      this%npoints(irch) = n 
      !
      ! -- deallocate
      deallocate(this%cross_sections(irch)%npoints)
      deallocate(this%cross_sections(irch)%station)
      deallocate(this%cross_sections(irch)%depth)
      deallocate(this%cross_sections(irch)%valid)
      !
      ! -- reallocate
      allocate(this%cross_sections(irch)%npoints)
      allocate(this%cross_sections(irch)%station(n))
      allocate(this%cross_sections(irch)%depth(n))
      allocate(this%cross_sections(irch)%valid(n))
      !
      ! -- initialize
      this%cross_sections(irch)%npoints = n
      !
      ! -- get table block
      call parser%GetBlock('TABLE', isfound, ierr, supportOpenClose=.true.)
      !
      ! -- parse well_connections block if detected
      if (isfound) then

        ! -- process the table data
        if (this%iprpak /= 0) then
          write(this%iout,'(/1x,a)')                                            &
            'PROCESSING '//trim(adjustl(tag))//' TABLE'
        end if
        ipos = 0
        readtabledata: do
          call parser%GetNextLine(endOfBlock)
          if (endOfBlock) exit
          ipos = ipos + 1
          if (ipos > this%npoints(irch)) then
            cycle readtabledata
          end if
          this%cross_sections(irch)%station(ipos) = parser%GetDouble() * width
          this%cross_sections(irch)%depth(ipos) = parser%GetDouble()
          this%cross_sections(irch)%valid(ipos) = .TRUE.
        end do readtabledata
        
        if (this%iprpak /= 0) then
          write(this%iout,'(1x,a)')                                              &
            'END OF '//trim(adjustl(tag))//' TABLE'
        end if
      else
        call store_error('REQUIRED TABLE BLOCK NOT FOUND.')
      end if
      !
      ! -- error condition if number of rows read are not equal to nrow
      if (ipos /= this%npoints(irch)) then
        write(errmsg,'(a,1x,i0,1x,a,1x,i0,1x,a)')                                &
          'NROW SET TO', this%npoints(irch), 'BUT', ipos, 'ROWS WERE READ'
        call store_error(errmsg)
      end if    
    end if
    !
    ! -- close the open table file
    if (iu > 0) then
      close(iu)
    end if
    !
    ! -- validate the table
    call this%validate(irch)
    !
    ! -- return
    return
  end subroutine read_table
  
  !> @brief Validate cross-section tables
  !!
  !! Subroutine to validate a cross-section table.
  !!
  !<
  subroutine validate(this, irch)
    use ConstantsModule, only: DEM6, DTWOTHIRDS
    use SimModule, only: store_error
    use SortModule, only: unique_values
    use GwfSfrCrossSectionUtilsModule, only: get_cross_section_area, &
                                             get_hydraulic_radius
    ! -- dummy variables
    class(SfrCrossSection) :: this    !< SfrCrossSection object
    integer(I4B), intent(in) :: irch  !< current reach
    ! -- local variables
    logical(LGP) :: station_error
    logical(LGP) :: depth_error
    character(len=LINELENGTH) :: filename   
    integer(I4B) :: npts
    integer(I4B) :: n
    integer(I4B) :: i
    integer(I4B) :: ipos
    real(DP) :: station
    real(DP) :: depth
    real(DP) :: aw
    real(DP) :: rh
    real(DP) :: dc0
    real(DP) :: dc1
    real(DP), dimension(:), allocatable :: depths
    real(DP), dimension(:), allocatable :: unique_depths
    real(DP), dimension(3) :: factor
    !
    ! -- initialize local variables
    station_error = .FALSE.
    depth_error = .FALSE.
    npts = this%npoints(irch)
    !
    ! -- validate the station and depth data
    do n = 1, npts
      station = this%cross_sections(irch)%station(n)
      if (station < DZERO) then
        station_error = .TRUE.
      end if
      depth = this%cross_sections(irch)%depth(n)
      if (station < DZERO) then
        depth_error = .TRUE.
      end if
      if (station_error .and. depth_error) then
        exit
      end if
    end do
    !
    ! -- write error messages
    if (station_error .or. depth_error) then
      filename = this%filenames(irch)
      if (station_error) then
        write(errmsg, '(3a,1x,i0,1x,a)') &
          "All xfraction data in '", trim(adjustl(filename)), &
          "' for reach", irch, 'must be greater than or equal to zero.'
        call store_error(errmsg)
      end if
      if (depth_error) then
        write(errmsg, '(3a,1x,i0,1x,a)') &
          "All depth data in '", trim(adjustl(filename)), &
          "' for reach", irch, 'must be greater than or equal to zero.'
        call store_error(errmsg)
      end if
    end if
    !
    ! -- initialize and fill depths
    allocate(depths(npts))
    do n = 1, npts
      depths(n) = this%cross_sections(irch)%depth(n)
    end do
    !
    ! -- get unique depths
    call unique_values(depths, unique_depths)
    !
    ! -- calculate the product of the area and the hydraulic radius to 
    !    the 2/3 power
    do n = 1, size(unique_depths)
      if (unique_depths(n) <= DZERO) cycle
      ipos = 1
      do i = -1, 1, 1
        depth = unique_depths(n) + real(i, DP) * DEM6
        aw = get_cross_section_area(npts, this%cross_sections(irch)%station, &
                                    this%cross_sections(irch)%depth, depth)
        rh = get_hydraulic_radius(npts, this%cross_sections(irch)%station, &
                                  this%cross_sections(irch)%depth, depth)
        factor(ipos) = aw * rh**DTWOTHIRDS
        ipos = ipos + 1
      end do
      !
      ! -- calculate the derivative
      dc0 = (factor(2) - factor(1)) / DEM6
      dc1 = (factor(3) - factor(2)) / DEM6
      !
      ! -- evaluate the difference
      if (dc0 < DZERO .or. dc1 < DZERO) then
        this%invalid = this%invalid + 1
        depth = unique_depths(n)
        do i = 1, npts
          if (this%cross_sections(irch)%depth(i) == depth) then
            this%cross_sections(irch)%valid(i) = .FALSE.
          end if
        end do
      end if
    end do
    !
    ! -- deallocate local storage
    deallocate(depths)
    deallocate(unique_depths)
    !
    ! -- return
    return
  end subroutine validate

  !> @brief Write cross-section tables
  !!
  !! Subroutine to write the cross-section tables to the model
  !! listing file.
  !!
  !<
  subroutine output(this, widths, kstp, kper)
    use ConstantsModule, only: TABLEFT
    use SimModule, only: store_warning
    ! -- dummy variables
    class(SfrCrossSection) :: this                            !< SfrCrossSection object
    real(DP), dimension(this%nreaches), intent(in) :: widths  !< reach widths
    integer(I4B), intent(in), optional :: kstp                !< time step
    integer(I4B), intent(in), optional :: kper                !< stress period
    ! -- local variables
    character(len=LINELENGTH) :: title
    character(len=LINELENGTH) :: text
    character(len=LINELENGTH) :: filename   
    character(len=10) :: cvalid
    logical(LGP) :: transient
    integer(I4B) :: kkstp
    integer(I4B) :: kkper
    integer(I4B) :: irch
    integer(I4B) :: n
    integer(I4B) :: ntabcols
    integer(I4B) :: ninvalid_reaches
    real(DP) :: width
    real(DP) :: xfraction
    integer(I4B), dimension(this%nreaches) :: reach_fail
    !
    ! -- initialize local variables
    kkstp = 0
    kkper = 0
    !
    ! -- process optional parameters
    if (present(kstp)) then
      kkstp = kstp
    end if
    if (present(kper)) then
      kkper = kper
    end if
    !
    ! -- set transient boolean
    if (kkstp > 0 .and. kkper > 0) then
      transient = .TRUE.
    else
      transient = .FALSE.
    end if
    !
    ! -- set reach failure
    do irch = 1, this%nreaches
      filename = this%filenames(irch)
      reach_fail(irch) = 0
      !
      ! -- output cross-section data read from a file
      if (trim(adjustl(filename)) /= 'NONE') then
        do n = 1, this%npoints(irch)
          if (.not. this%cross_sections(irch)%valid(n)) then
            reach_fail(irch) = reach_fail(irch) + 1
          end if
        end do
      end if
    end do
    !
    ! -- iterate over each reach
    do irch = 1, this%nreaches
      filename = this%filenames(irch)
      !
      ! -- output cross-section data read from a file
      if (trim(adjustl(filename)) /= 'NONE') then
        !
        ! -- build and write the table for a reach, if required, or
        !    the cross-section is invalid
        if (this%iprpak > 0 .or. reach_fail(irch) > 0) then
          !
          ! -- calculate the number of table columns
          if (reach_fail(irch) > 0) then
            ntabcols = 4
          else
            ntabcols = 3
          end if
          !
          ! -- reset the input table object
          write(title, '(a,1x,i0,1x,3a)') &
            'CROSS_SECTION DATA FOR REACH', irch, "FROM TAB6 FILE ('", &
            trim(adjustl(filename)), "')"
          call table_cr(this%inputtab, trim(adjustl(filename)), title)
          call this%inputtab%table_df(this%npoints(irch), ntabcols, &
                                      this%iout, finalize=.FALSE., &
                                      transient=transient)
          if (transient) then
            call this%inputtab%set_kstpkper(kkstp, kkper)
          end if
          text = 'XFRACTION'
          call this%inputtab%initialize_column(text, 20, alignment=TABLEFT)
          text = 'STATION'
          call this%inputtab%initialize_column(text, 20, alignment=TABLEFT)
          text = 'DEPTH'
          call this%inputtab%initialize_column(text, 20, alignment=TABLEFT)
          if (reach_fail(irch) > 0) then
            text = 'NEEDS ADJUSTMENT'
            call this%inputtab%initialize_column(text, 10, alignment=TABLEFT)
          end if
          !
          ! -- set the width
          width = widths(irch)
          !
          ! -- fill the table
          do n = 1, this%npoints(irch)
            xfraction = this%cross_sections(irch)%station(n) / width
            call this%inputtab%add_term(xfraction)
            call this%inputtab%add_term(this%cross_sections(irch)%station(n))
            call this%inputtab%add_term(this%cross_sections(irch)%depth(n))
            if (reach_fail(irch) > 0) then
              if (this%cross_sections(irch)%valid(n)) then
                cvalid = ''
              else
                cvalid = 'TRUE'
              end if
              call this%inputtab%add_term(cvalid)
            end if 
          end do
          !
          ! -- finalize the table
          call this%inputtab%finalize_table()
        end if
      end if
    end do
    !
    ! -- save warning message and write summary information to the listing file
    if (this%invalid > 0) then
      ninvalid_reaches = 0
      do irch = 1, this%nreaches
        if (reach_fail(irch) > 0) then
          ninvalid_reaches = ninvalid_reaches + 1
        end if
      end do
      write(warnmsg, '(a,1x,i0,7(1x,a))') &
        'Cross-section data for', ninvalid_reaches, &
        'reaches include one or more points that result in a', &
        'non-unique depth-conveyance relation. This occurs when', & 
        'there are horizontal sections at non-zero depths', &
        '(for example, flat overbank sections). This can usually', &
        'be resolved by adding a small slope to these flat', &
        'sections. See the cross-section tables in the model', &
        'listing file for more information.'
      call store_warning(warnmsg)
    end if
    !
    ! -- return
    return
  end subroutine output

  !> @brief Get the total number of cross-section points
  !!
  !! Function to get the total number of cross-section points to
  !! get the new size of the station xsdepth data for all reaches.
  !!
  !<
  function get_ncrossptstot(this) result(nptstot)
    ! -- dummy variables
    class(SfrCrossSection) :: this
    ! -- local variables
    integer(I4B) :: nptstot
    integer(I4B) :: n
    !
    !
    nptstot = 0
    do n = 1, this%nreaches
      nptstot = nptstot + this%npoints(n)
    end do
    !
    ! -- return
    return
  end function get_ncrossptstot
  
  !> @brief Pack the cross-section object
  !!
  !! Subroutine to pack the cross-section object into vectors.
  !!
  !<
  subroutine pack(this, ncrossptstot, ncrosspts, iacross, station, depth)
    ! -- dummy variables
    class(SfrCrossSection) :: this                                      !< SfrCrossSection object
    integer(I4B), intent(in) :: ncrossptstot                            !< total number of cross-section points
    integer(I4B), dimension(this%nreaches), intent(inout) :: ncrosspts  !< pointers to cross-section data in data vector
    integer(I4B), dimension(this%nreaches+1), intent(inout) :: iacross  !< pointers to cross-section data in data vector
    real(DP), dimension(ncrossptstot), intent(inout) :: station         !< cross-section station data
    real(DP), dimension(ncrossptstot), intent(inout) :: depth           !< cross-section depth data
    ! -- local variables
    integer(I4B) :: i
    integer(I4B) :: n
    integer(I4B) :: npoints
    integer(I4B) :: ipos
    !
    ! -- pack the data
    ipos = 1
    iacross(1) = ipos
    do n = 1, this%nreaches
      npoints = this%npoints(n)
      ncrosspts(n) = npoints
      do i = 1, npoints
        station(ipos) = this%cross_sections(n)%station(i)
        depth(ipos) = this%cross_sections(n)%depth(i)
        ipos = ipos + 1
      end do
      iacross(n+1) = ipos
    end do
    !
    ! -- return
    return
  end subroutine pack

  !> @brief Deallocate the cross-section object
  !!
  !! Subroutine to deallocate the cross-section object.
  !!
  !<
  subroutine destroy(this)
    ! -- dummy variables
    class(SfrCrossSection) :: this   !< SfrCrossSection object
    ! -- local variables
    integer(I4B) :: n
    !
    ! -- deallocate and nullify pointers
    deallocate(this%npoints)
    nullify(this%npoints)
    do n = 1, this%nreaches
      deallocate(this%cross_sections(n)%npoints)
      nullify(this%cross_sections(n)%npoints)
      deallocate(this%cross_sections(n)%station)
      nullify(this%cross_sections(n)%station)
      deallocate(this%cross_sections(n)%depth)
      nullify(this%cross_sections(n)%depth)
      deallocate(this%cross_sections(n)%valid)
      nullify(this%cross_sections(n)%valid)
    end do
    deallocate(this%cross_sections)
    nullify(this%cross_sections)
    !
    ! -- input table
    if (associated(this%inputtab)) then
      call this%inputtab%table_da()
      deallocate(this%inputtab)
      nullify(this%inputtab)
    end if
    !
    ! -- deallocate and nullify class scalars
    deallocate(this%invalid)
    nullify(this%invalid)
    !
    ! -- nullify scalars that are pointers to external variables
    nullify(this%iout)
    nullify(this%iprpak)
    nullify(this%nreaches)
    !
    ! -- return
    return
  end subroutine destroy



end module sfrCrossSectionManager