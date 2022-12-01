module GrbModule

  use KindModule
  use SimVariablesModule, only: iout
  use SimModule, only: store_error, store_error_unit, ustop
  implicit none
  private
  public :: read_grb

contains

  subroutine read_grb(inunit, ia, ja, mshape)
! ******************************************************************************
! read_grb
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use InputOutputModule, only: urword
    ! -- dummy
    integer(I4B), intent(in) :: inunit
    integer(I4B), allocatable, dimension(:), intent(out) :: ia
    integer(I4B), allocatable, dimension(:), intent(out) :: ja
    integer(I4B), allocatable, dimension(:), intent(out) :: mshape
    ! -- local
    character(len=50) :: hdrtxt
    integer(I4B) :: lloc, istart, istop
    character(len=50) :: dataname
    character(len=50) :: datatype
    integer(I4B) :: ntxt, lentxt, ndim, i, j, n, nval
    integer(I4B) :: nja, ncells
    real(DP) :: r, d
    character(len=:), allocatable :: line
    character(len=:), allocatable :: dfntxt
    integer(I4B), dimension(:), allocatable :: ishape
    integer(I4B), dimension(:), allocatable :: itmp
    real(DP), dimension(:), allocatable :: dtmp
! ------------------------------------------------------------------------------
    !
    ! -- message
    write (iout, '(/,a)') 'Processing Binary Grid File'
    ! -- grid keyword
    read (inunit) hdrtxt
    lloc = 1
    call urword(hdrtxt, lloc, istart, istop, 0, i, r, iout, inunit)
    if (hdrtxt(istart:istop) /= 'GRID') then
      call store_error('GRB FILE MUST BEGIN WITH WORD GRID.  FOUND: '// &
                       hdrtxt(istart:istop))
      call store_error_unit(inunit)
    end if
    !
    ! -- grid type, allocate mshape accordingly
    call urword(hdrtxt, lloc, istart, istop, 0, i, r, iout, inunit)
    if (hdrtxt(istart:istop) == 'DIS') then
      write (iout, '(2x, a)') 'Detected regular MODFLOW grid (DIS)'
      allocate (mshape(3))
    elseif (hdrtxt(istart:istop) == 'DISV') then
      write (iout, '(2x, a)') 'Detected Discretization by Vertices grid (DISV)'
      allocate (mshape(2))
    elseif (hdrtxt(istart:istop) == 'DISU') then
      write (iout, '(2x, a)') 'Detected unstructured grid (DISU)'
      allocate (mshape(1))
    else
      call store_error('UNKNOWN GRID TYPE IN GRB FILE: '//hdrtxt(istart:istop))
      call store_error_unit(inunit)
    end if
    mshape(:) = 0
    !
    ! -- version
    read (inunit) hdrtxt
    write (iout, '(2x, a, a)') 'Detected ', trim(hdrtxt(1:49))
    !
    ! -- ntxt
    read (inunit) hdrtxt
    write (iout, '(2x, a, a)') 'Detected ', trim(hdrtxt(1:49))
    lloc = 1
    call urword(hdrtxt, lloc, istart, istop, 0, i, r, iout, inunit)
    call urword(hdrtxt, lloc, istart, istop, 2, ntxt, r, iout, inunit)

    ! -- lentxt
    read (inunit) hdrtxt
    write (iout, '(2x, a, a)') 'Detected ', trim(hdrtxt(1:49))
    lloc = 1
    call urword(hdrtxt, lloc, istart, istop, 0, i, r, iout, inunit)
    call urword(hdrtxt, lloc, istart, istop, 2, lentxt, r, iout, inunit)
    !
    ! -- read txt definitions
    allocate (character(len=lentxt) :: line)
    allocate (character(len=lentxt*ntxt) :: dfntxt)
    read (inunit) dfntxt
    ! -- read each data record
    do n = 1, lentxt * ntxt, lentxt
      line = dfntxt(n:n + lentxt - 1)
      lloc = 1
      call urword(line, lloc, istart, istop, 0, i, r, iout, inunit)
      dataname = line(istart:istop)
      call urword(line, lloc, istart, istop, 0, i, r, iout, inunit)
      datatype = line(istart:istop)
      call urword(line, lloc, istart, istop, 0, i, r, iout, inunit)
      call urword(line, lloc, istart, istop, 2, ndim, r, iout, inunit)
      allocate (ishape(ndim))
      do j = 1, ndim
        call urword(line, lloc, istart, istop, 2, ishape(j), r, iout, inunit)
      end do
      select case (trim(datatype))
      case ('INTEGER')
        if (ndim == 0) then
          read (inunit) i
          write (iout, '(2x, a, a, a, i0)') &
            'Detected ', trim(dataname), ' = ', i
          if (trim(dataname) == 'NLAY') mshape(1) = i
          if (trim(dataname) == 'NROW') mshape(2) = i
          if (trim(dataname) == 'NCOL') mshape(3) = i
          if (trim(dataname) == 'NCPL') mshape(2) = i
          if (trim(dataname) == 'NJA') nja = i
          if (trim(dataname) == 'NCELLS') ncells = i
          if (trim(dataname) == 'NODES') then
            ncells = i
            mshape(1) = i
          end if
        else
          write (iout, '(2x, a, a)') 'Detected integer array ', trim(dataname)
          nval = 1
          do j = 1, ndim
            nval = nval * ishape(j)
          end do
          allocate (itmp(nval))
          read (inunit) itmp
          if (trim(dataname) == 'IA') then
            allocate (ia(ncells + 1))
            ia = itmp
          elseif (trim(dataname) == 'JA') then
            allocate (ja(nja))
            ja = itmp
          end if
          deallocate (itmp)
        end if
      case ('DOUBLE')
        if (ndim == 0) then
          read (inunit) d
          write (iout, '(2x, a, a, a, G0)') &
            'Detected ', trim(dataname), ' = ', d
        else
          write (iout, '(2x, a, a)') 'Detected double array ', trim(dataname)
          nval = 1
          do j = 1, ndim
            nval = nval * ishape(j)
          end do
          allocate (dtmp(nval))
          read (inunit) dtmp
          deallocate (dtmp)
        end if
      end select
      deallocate (ishape)
    end do
    close (inunit)
    write (iout, '(a)') 'Done processing Binary Grid File'
    !
    ! -- deallocate local storage
    deallocate (line)
    deallocate (dfntxt)
    !
    ! -- return
    return
  end subroutine read_grb

end module GrbModule
