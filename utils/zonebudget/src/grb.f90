module GrbModule

  use SimVariablesModule, only: iout
  implicit none
  private
  public :: read_grb
  
  contains
  
  subroutine read_grb(inunit, ia, ja)
! ******************************************************************************
! read_grb
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use InputOutputModule, only: urword
    ! -- dummy
    integer, intent(in) :: inunit
    integer, allocatable, dimension(:), intent(out) :: ia
    integer, allocatable, dimension(:), intent(out) :: ja
    ! -- local
    integer :: iout
    character(len=50) :: hdrtxt
    integer :: lloc, istart, istop
    character(len=50) :: dataname
    character(len=50) :: datatype
    integer :: ntxt, lentxt, ndim, i, j, n, nval
    integer :: nja, ncells, nlay
    double precision :: r, d
    character(len=:), dimension(:), allocatable :: dfntxt
    integer, dimension(:), allocatable :: ishape
    integer, dimension(:), allocatable :: itmp
    double precision, dimension(:), allocatable :: dtmp
! ------------------------------------------------------------------------------
    !
    ! -- grid type
    read(inunit) hdrtxt
    !
    ! -- version
    read(inunit) hdrtxt
    !
    ! -- ntxt
    read(inunit) hdrtxt    
    lloc=1
    call urword(hdrtxt, lloc, istart, istop, 0, i, r, iout, inunit)
    call urword(hdrtxt, lloc, istart, istop, 2, ntxt, r, iout, inunit)
    
    ! -- lentxt
    read(inunit) hdrtxt
    lloc=1
    call urword(hdrtxt, lloc, istart, istop, 0, i, r, iout, inunit)
    call urword(hdrtxt, lloc, istart, istop, 2, lentxt, r, iout, inunit)
    !
    ! -- read txt definitions
    allocate(character(len=lentxt)::dfntxt(ntxt))
    read(inunit) dfntxt
    ! -- read each data record
    do n = 1, ntxt
      lloc = 1
      call urword(dfntxt(n), lloc, istart, istop, 0, i, r, iout, inunit)
      dataname = dfntxt(n)(istart:istop)
      call urword(dfntxt(n), lloc, istart, istop, 0, i, r, iout, inunit)
      datatype = dfntxt(n)(istart:istop)
      call urword(dfntxt(n), lloc, istart, istop, 0, i, r, iout, inunit)
      call urword(dfntxt(n), lloc, istart, istop, 2, ndim, r, iout, inunit)
      allocate(ishape(ndim))
      do j = 1, ndim
        call urword(dfntxt(n), lloc, istart, istop, 2, ishape(j), r, iout, inunit)
      enddo
      select case (trim(datatype))
        case('INTEGER')
          if(ndim == 0)then
            read(inunit) i
            if(trim(dataname) == 'NLAY') nlay = i
            if(trim(dataname) == 'NJA') nja = i
            if(trim(dataname) == 'NCELLS') ncells = i
            if(trim(dataname) == 'NODES') ncells = i
          else
            nval = 1
            do j = 1, ndim
              nval = nval * ishape(j)
            enddo
            allocate(itmp(nval))
            read(inunit) itmp
            if(trim(dataname) == 'IA') then
              allocate (ia(ncells + 1))
              ia = itmp
            elseif(trim(dataname) == 'JA') then
              allocate (ja(nja))
              ja = itmp
            endif
            deallocate(itmp)
          endif
        case('DOUBLE')
          if(ndim == 0)then
            read(inunit) d
          else
            nval = 1
            do j = 1, ndim
              nval = nval * ishape(j)
            enddo
            allocate(dtmp(nval))
            read(inunit) dtmp
            deallocate(dtmp)
          endif
      end select
      deallocate(ishape)
    enddo
    close(inunit)
    !
    ! -- return
    return
  end subroutine read_grb
  
end module GrbModule
