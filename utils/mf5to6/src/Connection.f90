module ConnectionModule
  
  use ConstantsModule, only: DZERO
  
  type :: ConnectionType
    integer :: noden ! node number of cell in parent where ghost node resides
    integer :: nodem ! node number of the connecting cell to which flow occurs from the ghost node
    integer :: kc, ic, jc, kp, ip, jp  ! layer, row, column indices of child and parent nodes
    ! ihc = 0 -- connection is vertical
    ! ihc = 1 -- connection is horizontal
    ! ihc = 2 -- connection is horizontal for vert. staggered grid
    integer :: ihc
    ! cl1: Distance between the center of cell noden and its 
    !      shared face with nodem.
    double precision :: cl1 
    ! cl2: Distance between the center of cell nodem and its 
    !      shared face with noden.
    double precision :: cl2
    double precision :: hwva ! horizontal width or area of vertical connection
    ! for ghost-node correction
    ! NODEJ1 is contributing (adjacent) node in parent grid used for
    ! head interpolation in direction 1 from cell where ghost node resides.
    ! NODEJ2 is contributing (adjacent) node in parent grid used for
    ! head interpolation in direction 2 from cell where ghost node resides.
    integer :: nodej1 = 0
    integer :: k1, i1, j1  ! layer, row, column indices of contributing node nodej1
    integer :: nodej2 = 0
    integer :: k2, i2, j2  ! layer, row, column indices of contributing node nodej2
    integer :: nodej12 = 0 ! for the off-corner cell
    integer :: k12, i12, j12  ! layer, row, column indices of contributing node nodej12
    ! ALPHAJ1 and ALPHAJ2 are contributing factors (portion of head 
    ! attributable to contributing node) for contributing cells 1 and 2.
    double precision :: alphaj1 = DZERO
    double precision :: alphaj2 = DZERO
    double precision :: alphaj12 = DZERO ! for the off-corner cell
  contains
    procedure, public :: WriteConnection
    procedure, public :: WriteGhostNodeCorrection
  end type ConnectionType
  
  contains
  
  subroutine WriteConnection(this, iu)
    implicit none
    ! dummy
    class(ConnectionType) :: this
    integer, intent(in)   :: iu
    ! format
    20 format(1x,3(1x,i0),1x,3(1x,i0),2x,i0,3(2x,g0))
    !
    write(iu,20)this%kp, this%ip, this%jp, this%kc, this%ic, this%jc, &
                this%ihc, this%cl1, this%cl2, this%hwva
    !
    return
  end subroutine WriteConnection
  
  subroutine WriteGhostNodeCorrection(this, iu, numalphaj)
    implicit none
    ! dummy
    class(ConnectionType) :: this
    integer, intent(in)   :: iu
    integer, intent(in)   :: numalphaj
    ! format 
    10 format(3(1x,3(1x,i0)),2x,g0)
    20 format(4(1x,3(1x,i0)),2(2x,g0))
    30 format(5(1x,3(1x,i0)),3(2x,g0))
    !
    select case (numalphaj)
    case (1)
      if (this%alphaj1 == 0) then
        this%k1 = 0
        this%i1 = 0
        this%j1 = 0
      end if
      write(iu,10)this%kp, this%ip, this%jp, this%kc, this%ic, this%jc, &
                  this%k1, this%i1, this%j1, this%alphaj1
    case (2)
      if (this%alphaj1 == 0.) then
        this%k1 = 0
        this%i1 = 0
        this%j1 = 0
      end if
      if (this%alphaj2 == 0.) then
        this%k2 = 0
        this%i2 = 0
        this%j2 = 0
      end if
      write(iu,20)this%kp, this%ip, this%jp, this%kc, this%ic, this%jc, &
                  this%k1, this%i1, this%j1, this%k2, this%i2, this%j2, &
                  this%alphaj1, this%alphaj2
    case (3)
      if (this%alphaj1 == 0.) then
        this%k1 = 0
        this%i1 = 0
        this%j1 = 0
      end if
      if (this%alphaj2 == 0.) then
        this%k2 = 0
        this%i2 = 0
        this%j2 = 0
      end if
      if (this%alphaj12 == 0.) then
        this%k12 = 0
        this%i12 = 0
        this%j12 = 0
      end if
      write(iu,30)this%kp, this%ip, this%jp, this%kc, this%ic, this%jc, &
                  this%k1, this%i1, this%j1, this%k2, this%i2, this%j2, &
                  this%k12, this%i12, this%j12, this%alphaj1, this%alphaj2, &
                  this%alphaj12
    end select
    !
    return
  end subroutine WriteGhostNodeCorrection
  
end module ConnectionModule
