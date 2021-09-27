module sfrCrossSectionManager

  use KindModule, only: DP, I4B
  use ConstantsModule, only: DZERO, DONE
  use SimVariablesModule, only: errmsg

  implicit none
  
  public :: SfrCrossSection
  public :: cross_section_cr

  type :: SfrCrossSectionType
    integer(I4B), pointer :: npoints
    real(DP), pointer, dimension(:), contiguous :: station => null()
    real(DP), pointer, dimension(:), contiguous :: depth => null()
  end type SfrCrossSectionType

  type :: SfrCrossSection
    integer(I4B), pointer :: iout => null()
    integer(I4B), pointer :: iprpak => null()
    integer(I4B), pointer :: nreaches => null()
    integer(I4B), pointer, dimension(:), contiguous :: npoints => null()
    type(SfrCrossSectionType), pointer, dimension(:), contiguous :: cross_sections => null()

    
    contains
  
    procedure :: initialize
    procedure :: read_table
    procedure :: get_ncrossptstot
    procedure :: pack
    procedure :: destroy


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
    ! -- create cross-section container
    allocate(this%npoints(this%nreaches))
    allocate(this%cross_sections(this%nreaches))
    do n = 1, this%nreaches
      npoints = ncrosspts(n)
      allocate(this%cross_sections(n)%npoints)
      allocate(this%cross_sections(n)%station(npoints))
      allocate(this%cross_sections(n)%depth(npoints))
    end do
    !
    ! -- fill cross-section container with current values
    do n = 1, this%nreaches
      this%cross_sections(n)%npoints = ncrosspts(n)
      this%npoints(n) = ncrosspts(n)
      i0 = iacross(n)
      i1 = iacross(n + 1) - 1
      ipos = 1
      do i = i0, i1
        this%cross_sections(n)%station(ipos) = station(i)
        this%cross_sections(n)%depth(ipos) = depth(i)
        ipos = ipos + 1
      end do
    end do
    !
    ! -- return
    return
  end subroutine initialize

  subroutine read_table(this, irch, width, filename)
    use ConstantsModule, only: LINELENGTH, IUOC
    use InputOutputModule, only: openfile
    use SimModule, only: store_error, count_errors
    use BlockParserModule, only: BlockParserType
    ! -- dummy variables
    class(SfrCrossSection) :: this
    integer(I4B), intent(in) :: irch
    real(DP), intent(in) :: width
    character(len=*), intent(in) :: filename
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
      ! -- reset the number of points
      this%npoints(irch) = n 
      !
      ! -- deallocate
      deallocate(this%cross_sections(irch)%npoints)
      deallocate(this%cross_sections(irch)%station)
      deallocate(this%cross_sections(irch)%depth)
      !
      ! -- reallocate
      allocate(this%cross_sections(irch)%npoints)
      allocate(this%cross_sections(irch)%station(n))
      allocate(this%cross_sections(irch)%depth(n))
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
    ! -- return
    return
  end subroutine read_table

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

  subroutine destroy(this)
    ! -- dummy variables
    class(SfrCrossSection) :: this
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
    end do
    deallocate(this%cross_sections)
    nullify(this%cross_sections)
    !
    ! -- nullify scalars
    nullify(this%iout)
    nullify(this%iprpak)
    nullify(this%nreaches)
    !
    ! -- return
    return
  end subroutine destroy



end module sfrCrossSectionManager