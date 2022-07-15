module ZoneModule

  use KindModule
  use SimVariablesModule, only: iout
  use SimModule, only: store_error, store_error_unit, ustop
  use ConstantsModule, only: LINELENGTH, DEP20
  use BlockParserModule, only: BlockParserType
  use SortModule, only: unique_values

  implicit none
  private
  public :: zone_init
  public :: clear_accumulators
  public :: flowja_accumulate
  public :: flowiaja_accumulate
  public :: flow_accumulate
  public :: flowch_setich
  public :: flowch_accumulate
  public :: zone_finalize
  public :: maxzone
  public :: iuniqzone
  public :: nmznfl, vbvl, vbznfl

  integer(I4B) :: ncells
  integer(I4B) :: maxzone
  integer(I4B), dimension(:), allocatable :: izoneuser
  integer(I4B), dimension(:), allocatable :: iuniqzone
  integer(I4B), dimension(:), allocatable :: izone
  integer(I4B), dimension(:), allocatable :: ich
  integer(I4B), dimension(:, :), allocatable :: nmznfl
  real(DP), dimension(:, :, :), allocatable :: vbznfl
  real(DP), dimension(:, :, :), allocatable :: vbvl
  character(len=LINELENGTH) :: errmsg, keyword

contains

  subroutine zone_init(inunit, nbudterms, ncr, mshape)
! ******************************************************************************
! zone_init
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use ArrayReadersModule, only: ReadArray
    type(BlockParserType) :: parser
    integer(I4B), intent(in) :: inunit
    integer(I4B), intent(in) :: nbudterms
    integer(I4B), intent(inout) :: ncr
    integer(I4B), dimension(:), intent(in) :: mshape
    integer(I4B) :: nlay, ncpl, istart, istop, k
    character(len=24) :: aname = '                   IZONE'
    integer(I4B) :: ierr
    integer(I4B) :: i
    integer(I4B) :: n
    integer(I4B) :: iminval
    integer(I4B) :: imaxval
    integer(I4B), dimension(:), allocatable :: izonecount
    logical :: isfound, endOfBlock
! ------------------------------------------------------------------------------
    !
    ! -- Read DIMENSIONS block, set NCELLS
    call parser%Initialize(inunit, iout)
    call parser%GetBlock('DIMENSIONS', isfound, ierr)
    if (isfound) then
      write (iout, '(/, a)') 'Processing zone dimensions'
      do
        call parser%GetNextLine(endOfBlock)
        if (endOfBlock) exit
        call parser%GetStringCaps(keyword)
        select case (keyword)
        case ('NCELLS')
          ncells = parser%GetInteger()
          write (iout, '(4x,a,i0)') 'NCELLS = ', ncells
        case default
          write (errmsg, '(4x,a,a)') 'ERROR. UNKNOWN DIMENSIONS ENTRY: ', &
            trim(keyword)
          call store_error(errmsg)
          call parser%StoreErrorUnit()
        end select
      end do
      write (iout, '(a)') 'End processing zone dimensions'
    else
      write (errmsg, '(1x,a)') 'ERROR.  REQUIRED ZONE DIMENSIONS BLOCK NOT FOUND.'
      call store_error(errmsg)
      call parser%StoreErrorUnit()
    end if
    !
    ! -- Validate size and allocate arrays
    if (ncr > 0) then
      if (ncells /= ncr) then
        write (errmsg, '(a,i0)') 'GRB FILE INDICATES NUMBER OF CELLS OR '// &
          'REACHES IS ', ncr
        call store_error(errmsg)
        write (errmsg, '(a,i0)') 'INSTEAD ZONE ARRAY SPECIFIED AS SIZE ', ncells
        call store_error(errmsg)
        write (errmsg, '(a,i0)') 'CHANGE SIZE OF ZONE ARRAY TO  ', ncr
        call store_error(errmsg)
        call store_error_unit(inunit)
      end if
    else
      ! -- Number of cells/reaches not available in grb or no grb specified
      !    Setting ncr to ncells
      ncr = ncells
    end if
    allocate (izoneuser(ncells))
    allocate (izone(ncells))
    allocate (ich(ncells))
    !
    ! -- get griddata block
    call parser%GetBlock('GRIDDATA', isfound, ierr)
    if (isfound) then
      write (iout, '(/, a)') 'Processing zone griddata'
      do
        call parser%GetNextLine(endOfBlock)
        if (endOfBlock) exit
        call parser%GetStringCaps(keyword)
        select case (keyword)
        case ('IZONE')
          call parser%GetStringCaps(keyword)
          if (keyword .EQ. 'LAYERED') then
            if (size(mshape) > 1) then
              nlay = mshape(1)
            else
              write (errmsg, '(4x,a)') 'ERROR. LAYERED INPUT NOT SUPPORTED &
                &FOR IZONE.  LAYERED INPUT CAN ONLY BE USED FOR IZONE &
                &WHEN A BINARY GRID FILE IS PROVIDED AND THE MODEL IS LAYERED'
              call store_error(errmsg)
              call parser%StoreErrorUnit()
            end if
            ncpl = ncells / nlay
            write (iout, '(4x, a, i0)') 'LAYERED detected.  Using NLAY = ', nlay
            write (iout, '(4x, a, i0, a)') 'Reading ', ncpl, ' values per layer'
            istart = 1
            istop = ncpl
            do k = 1, nlay
              call ReadArray(inunit, izoneuser(istart:istop), aname, 1, ncpl, &
                             iout, k)
              istart = istop + 1
              istop = istart + ncpl - 1
            end do
          else
            call ReadArray(inunit, izoneuser, aname, 1, ncells, iout, 0)
          end if
        case default
          write (errmsg, '(4x,a,a)') 'ERROR. UNKNOWN ZONE GRIDDATA TAG: ', &
            trim(keyword)
          call store_error(errmsg)
          call parser%StoreErrorUnit()
        end select
      end do
    else
      call store_error('ERROR.  REQUIRED GRIDDATA BLOCK NOT FOUND.')
      call parser%StoreErrorUnit()
    end if
    !
    ! -- Write messages
    close (inunit)
    !
    ! -- Find max and min values
    iminval = HUGE(iminval)
    imaxval = -HUGE(imaxval)
    do n = 1, size(izoneuser)
      izone(n) = izoneuser(n)
      if (izoneuser(n) /= 0) then
        if (izoneuser(n) < iminval) then
          iminval = izoneuser(n)
        end if
        if (izoneuser(n) > imaxval) then
          imaxval = izoneuser(n)
        end if
      end if
    end do
    !
    ! -- write minimum and maximum zone numbers
    write (iout, '(/, 4x, a, i0)') &
      'Successfully read zone array with NCELLS = ', ncells
    write (iout, '(4x, a, i0)') 'Minimum user-specified zone number is ', iminval
    write (iout, '(4x, a, i0)') 'Maximum user-specified zone number is ', imaxval
    !
    ! -- find unique zones
    call unique_values(izone, iuniqzone)
    !
    ! -- pop off a zero zone value
    call pop_zero_zone(iuniqzone)
    !
    ! -- set max zone number
    maxzone = size(iuniqzone)
    !
    ! -- allocate and initialize izonecount
    allocate (izonecount(0:maxzone))
    do i = 0, maxzone
      izonecount(i) = 0
    end do
    !
    ! -- fill izonemap with uniqzone number
    do n = 1, size(izone)
      if (izoneuser(n) == 0) then
        izone(n) = 0
        izonecount(0) = izonecount(0) + 1
      else
        do i = 1, maxzone
          if (izoneuser(n) == iuniqzone(i)) then
            izone(n) = i
            izonecount(i) = izonecount(i) + 1
            exit
          end if
        end do
      end if
    end do
    !
    ! -- write zone mapping
    write (iout, '(//,4x,3(a20,1x))') 'USER ZONE', 'ZONE NUMBER', 'CELL COUNT'
    write (iout, '(4x,62("-"))')
    write (iout, '(4x,3(i20,1x))') 0, 0, izonecount(0)
    do i = 1, maxzone
      write (iout, '(4x,3(i20,1x))') iuniqzone(i), i, izonecount(i)
    end do
    write (iout, '(4x,62("-"),/)')

    !
    !maxzone = maxval(izone)
    write (iout, '(a)') 'End processing zone griddata'
    !
    ! -- nmznfl is map showing connections between two zones.  If 1, then
    !    there is flow between zones, and zone to zone flow will be written.
    allocate (nmznfl(0:maxzone, 0:maxzone))
    allocate (vbznfl(2, 0:maxzone, 0:maxzone))
    allocate (vbvl(2, 0:maxzone, nbudterms))
    !
    ! -- deallocate local variables
    deallocate (izonecount)
    !
    ! -- close the zone file
    close (inunit)
    !
    ! -- return
    return
  end subroutine zone_init

  subroutine clear_accumulators()
! ******************************************************************************
! clear_accumulators
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
! ------------------------------------------------------------------------------
    ich(:) = 0
    nmznfl(:, :) = 0
    vbvl(:, :, :) = 0.d0
    vbznfl(:, :, :) = 0.d0
    !
    ! -- return
    return
  end subroutine clear_accumulators

  subroutine flowja_accumulate(nodesrc, nodedst, flowdata)
! ******************************************************************************
! flowja_accumulate
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    integer(I4B), dimension(:), intent(in) :: nodesrc
    integer(I4B), dimension(:), intent(in) :: nodedst
    real(DP), dimension(:, :), intent(in) :: flowdata
    ! -- local
    integer(I4B) :: i, n, m, iz1, iz2
    real(DP) :: q
! ------------------------------------------------------------------------------
    !
    ! -- add up flowja terms
    do i = 1, size(nodesrc)
      n = nodesrc(i)
      m = nodedst(i)
      q = flowdata(1, i)
      iz1 = izone(n)
      iz2 = izone(m)
      nmznfl(iz1, iz2) = 1
      if (q < 0.d0) then
        vbznfl(2, iz1, iz2) = vbznfl(2, iz1, iz2) - q
      else
        vbznfl(1, iz1, iz2) = vbznfl(1, iz1, iz2) + q
      end if
    end do
    !
    ! -- return
    return
  end subroutine flowja_accumulate

  subroutine flowiaja_accumulate(ia, ja, flowja)
! ******************************************************************************
! flowiaja_accumulate
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    integer(I4B), dimension(:), intent(in) :: ia
    integer(I4B), dimension(:), intent(in) :: ja
    real(DP), dimension(:), intent(in) :: flowja
    ! -- local
    integer(I4B) :: ipos, n, m, iz1, iz2
    real(DP) :: q
! ------------------------------------------------------------------------------
    !
    ! -- add up flowja terms
    do n = 1, ncells
      do ipos = ia(n), ia(n + 1) - 1
        m = ja(ipos)
        if (n == m) cycle
        q = flowja(ipos)
        iz1 = izone(n)
        iz2 = izone(m)
        nmznfl(iz1, iz2) = 1
        if (q < 0.d0) then
          vbznfl(2, iz1, iz2) = vbznfl(2, iz1, iz2) - q
        else
          vbznfl(1, iz1, iz2) = vbznfl(1, iz1, iz2) + q
        end if
      end do
    end do
    !
    ! -- return
    return
  end subroutine flowiaja_accumulate

  subroutine flow_accumulate(ibudterm, nodesrc, flowdata)
! ******************************************************************************
! flow_accumulate
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    integer(I4B), intent(in) :: ibudterm
    integer(I4B), dimension(:), intent(in) :: nodesrc
    real(DP), dimension(:, :), intent(in) :: flowdata
    ! -- local
    integer(I4B) :: i, n, iz1
    real(DP) :: q
! ------------------------------------------------------------------------------
    !
    ! -- accumulate flow terms
    do i = 1, size(nodesrc)
      n = nodesrc(i)
      q = flowdata(1, i)
      iz1 = izone(n)
      if (q < 0.d0) then
        vbvl(2, iz1, ibudterm) = vbvl(2, iz1, ibudterm) - q
      else
        vbvl(1, iz1, ibudterm) = vbvl(1, iz1, ibudterm) + q
      end if
    end do
    !
    ! -- return
    return
  end subroutine flow_accumulate

  subroutine flowch_setich(ibudterm, nodesrc)
! ******************************************************************************
! Set ICH equal to ibudterm for all constant head cells
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    integer(I4B), intent(in) :: ibudterm
    integer(I4B), dimension(:), intent(in) :: nodesrc
    ! -- local
    integer(I4B) :: i, n
! ------------------------------------------------------------------------------
    !
    ! -- accumulate flow terms
    do i = 1, size(nodesrc)
      n = nodesrc(i)
      ich(n) = ibudterm
    end do
    !
    ! -- return
    return
  end subroutine flowch_setich

  subroutine flowch_accumulate(ia, ja, flowja)
! ******************************************************************************
! flowiaja_accumulate
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    integer(I4B), dimension(:), intent(in) :: ia
    integer(I4B), dimension(:), intent(in) :: ja
    real(DP), dimension(:), intent(in) :: flowja
    ! -- local
    integer(I4B) :: ipos, n, m, iz, ibudterm
    real(DP) :: q
! ------------------------------------------------------------------------------
    !
    ! -- add up flowja terms
    do n = 1, ncells
      do ipos = ia(n), ia(n + 1) - 1
        !
        ! -- skip if cell is not constant head
        ibudterm = ich(n)
        if (ibudterm == 0) cycle
        !
        ! -- skip if adjacent cell is a constant head cell
        m = ja(ipos)
        if (n == m) cycle
        if (ich(m) > 0) cycle
        !
        ! -- accumulate constant head flows
        q = flowja(ipos)
        iz = izone(n)
        if (q < 0.d0) then
          vbvl(2, iz, ibudterm) = vbvl(2, iz, ibudterm) - q
        else
          vbvl(1, iz, ibudterm) = vbvl(1, iz, ibudterm) + q
        end if
      end do
    end do
    !
    ! -- return
    return
  end subroutine flowch_accumulate

  subroutine pop_zero_zone(ia)
    ! -- dummy arguments
    integer(I4B), dimension(:), allocatable, intent(inout) :: ia
    ! -- local variables
    integer(I4B) :: i
    integer(I4B) :: n
    integer(I4B) :: nlen
    integer(I4B) :: ipop
    integer(I4B), dimension(:), allocatable :: ib
! ------------------------------------------------------------------------------
    nlen = size(ia)
    ipop = 0
    !
    ! -- determine if there is a zero value in the integer attay
    do i = 1, nlen
      if (ia(i) == 0) then
        ipop = i
        exit
      end if
    end do
    !
    ! -- remove zero value from ia
    if (ipop /= 0) then
      !
      ! -- allocate ib
      allocate (ib(nlen - 1))
      !
      ! -- fill itmp with everything in ia except 0
      n = 1
      do i = 1, nlen
        if (i == ipop) then
          cycle
        end if
        ib(n) = ia(i)
        n = n + 1
      end do
      !
      ! -- deallocate ia
      deallocate (ia)
      !
      ! -- allocate ia and fill with ib
      allocate (ia(size(ib)))
      do i = 1, size(ib)
        ia(i) = ib(i)
      end do
      !
      ! -- deallocate ib
      deallocate (ib)
    end if
    !
    ! -- return
    return
  end subroutine pop_zero_zone

  subroutine zone_finalize()
! ******************************************************************************
! zone_finalize
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
! ------------------------------------------------------------------------------
    deallocate (izone)
    deallocate (ich)
    deallocate (nmznfl)
    deallocate (vbznfl)
    deallocate (vbvl)
    !
    ! -- return
    return
  end subroutine zone_finalize

end module ZoneModule

