module ZoneModule

  use SimVariablesModule, only: iout
  use SimModule, only: store_error, store_error_unit, ustop
  use ConstantsModule, only: LINELENGTH
  use BlockParserModule, only: BlockParserType

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
  public :: nmznfl, vbvl, vbznfl
  
  integer :: ncells
  integer :: maxzone
  integer, dimension(:), allocatable :: izone
  integer, dimension(:), allocatable :: ich
  integer, dimension(:, :), allocatable :: nmznfl
  double precision, dimension(:, :, :), allocatable :: vbznfl
  double precision, dimension(:, :, :), allocatable :: vbvl
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
    integer, intent(in) :: inunit
    integer, intent(in) :: nbudterms
    integer, intent(inout) :: ncr
    integer, dimension(:), intent(in) :: mshape
    integer :: nlay, ncpl, istart, istop, k
    character(len=24) :: aname = '                   IZONE'
    integer :: ierr
    logical :: isfound, endOfBlock
! ------------------------------------------------------------------------------
    !
    ! -- Read DIMENSIONS block, set NCELLS
    call parser%Initialize(inunit, iout)
    call parser%GetBlock('DIMENSIONS', isfound, ierr)
    if(isfound) then
      write(iout,'(/, a)') 'Processing zone dimensions'
      do
        call parser%GetNextLine(endOfBlock)
        if (endOfBlock) exit
        call parser%GetStringCaps(keyword)
        select case (keyword)
          case ('NCELLS')
            ncells = parser%GetInteger()
            write(iout,'(4x,a,i0)') 'NCELLS = ', ncells
          case default
            write(errmsg,'(4x,a,a)')'ERROR. UNKNOWN DIMENSIONS ENTRY: ',       &
                                      trim(keyword)
            call store_error(errmsg)
            call parser%StoreErrorUnit()
            call ustop()
          end select
      end do
      write(iout,'(a)') 'End processing zone dimensions'
    else
      write(errmsg,'(1x,a)')'ERROR.  REQUIRED ZONE DIMENSIONS BLOCK NOT FOUND.'
      call store_error(errmsg)
      call parser%StoreErrorUnit()
      call ustop()
    end if
    !
    ! -- Validate size and allocate arrays
    if (ncr > 0) then
      if (ncells /= ncr) then
        write(errmsg, '(a,i0)') 'GRB FILE INDICATES NUMBER OF CELLS OR ' //   &
          'REACHES IS ', ncr
        call store_error(errmsg)
        write(errmsg, '(a,i0)') 'INSTEAD ZONE ARRAY SPECIFIED AS SIZE ', ncells
        call store_error(errmsg)
        write(errmsg, '(a,i0)') 'CHANGE SIZE OF ZONE ARRAY TO  ', ncr
        call store_error(errmsg)
        call store_error_unit(inunit)
        call ustop()
      endif
    else
      ! -- Number of cells/reaches not available in grb or no grb specified
      !    Setting ncr to ncells
      ncr = ncells
    endif
    allocate(izone(ncells))
    allocate(ich(ncells))
    !
    ! -- get griddata block
    call parser%GetBlock('GRIDDATA', isfound, ierr)
    if(isfound) then
      write(iout,'(/, a)') 'Processing zone griddata'
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
                write(errmsg,'(4x,a)') 'ERROR. LAYERED INPUT NOT SUPPORTED &
                  &FOR IZONE.  LAYERED INPUT CAN ONLY BE USED FOR IZONE &
                  &WHEN A BINARY GRID FILE IS PROVIDED AND THE MODEL IS LAYERED'
                call store_error(errmsg)
                call parser%StoreErrorUnit()
                call ustop()
              endif
              ncpl = ncells / nlay
              write(iout, '(4x, a, i0)') 'LAYERED detected.  Using NLAY = ', nlay
              write(iout, '(4x, a, i0, a)') 'Reading ', ncpl, ' values per layer'
              istart = 1
              istop = ncpl
              do k = 1, nlay
                call ReadArray(inunit, izone(istart:istop), aname, 1, ncpl, iout, k)
                istart = istop + 1
                istop = istart + ncpl - 1
              enddo
            else
              call ReadArray(inunit, izone, aname, 1, ncells, iout, 0)
            endif
          case default
            write(errmsg,'(4x,a,a)')'ERROR. UNKNOWN ZONE GRIDDATA TAG: ',      &
                                     trim(keyword)
            call store_error(errmsg)
            call parser%StoreErrorUnit()
            call ustop()
        end select
      end do
    else
      call store_error('ERROR.  REQUIRED GRIDDATA BLOCK NOT FOUND.')
      call parser%StoreErrorUnit()
      call ustop()
    end if
    !
    ! -- Write messages
    close(inunit)
    maxzone = maxval(izone)
    write(iout, '(/, 4x, a, i0)') 'Successfully read zone array with NCELLS = ', ncells
    write(iout, '(4x, a, i0)') 'Maximum zone number is ', maxzone
    write(iout,'(a)') 'End processing zone griddata'
    !
    ! -- nmznfl is map showing connections between two zones.  If 1, then
    !    there is flow between zones, and zone to zone flow will be written.
    allocate(nmznfl(0:maxzone, 0:maxzone))
    allocate(vbznfl(2, 0:maxzone, 0:maxzone))
    allocate(vbvl(2, 0:maxzone, nbudterms))
    !
    ! -- close the zone file
    close(inunit)
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
    integer, dimension(:), intent(in) :: nodesrc
    integer, dimension(:), intent(in) :: nodedst
    double precision, dimension(:, :), intent(in) :: flowdata
    ! -- local
    integer :: i, n, m, iz1, iz2
    double precision :: q
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
      endif
    enddo
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
    integer, dimension(:), intent(in) :: ia
    integer, dimension(:), intent(in) :: ja
    double precision, dimension(:), intent(in) :: flowja
    ! -- local
    integer :: ipos, n, m, iz1, iz2
    double precision :: q
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
        endif
      enddo
    enddo
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
    integer, intent(in) :: ibudterm
    integer, dimension(:), intent(in) :: nodesrc
    double precision, dimension(:, :), intent(in) :: flowdata
    ! -- local
    integer :: i, n, iz1
    double precision :: q
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
      endif
    enddo
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
    integer, intent(in) :: ibudterm
    integer, dimension(:), intent(in) :: nodesrc
    ! -- local
    integer :: i, n
! ------------------------------------------------------------------------------
    !
    ! -- accumulate flow terms
    do i = 1, size(nodesrc)
      n = nodesrc(i)
      ich(n) = ibudterm
    enddo
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
    integer, dimension(:), intent(in) :: ia
    integer, dimension(:), intent(in) :: ja
    double precision, dimension(:), intent(in) :: flowja
    ! -- local
    integer :: ipos, n, m, iz, ibudterm
    double precision :: q
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
        endif
      enddo
    enddo
    !
    ! -- return
    return
  end subroutine flowch_accumulate

  subroutine zone_finalize()
! ******************************************************************************
! zone_finalize
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
! ------------------------------------------------------------------------------
    deallocate(izone)
    deallocate(ich)
    deallocate(nmznfl)
    deallocate(vbznfl)
    deallocate(vbvl)
    !
    ! -- return
    return
  end subroutine zone_finalize
  
end module ZoneModule

