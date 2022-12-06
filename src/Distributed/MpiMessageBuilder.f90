module MpiMessageBuilderModule
  use KindModule, only: I4B
  use VirtualDataContainerModule, only: VirtualDataContainerType  
  use mpi
  implicit none
  private

  type, public :: MpiMessageBuilderType
  contains
    procedure :: create_header_in => mb_create_header_in
    procedure :: create_header_out => mb_create_header_out
    ! private
    procedure, private :: create_snd_header
    procedure, private :: create_rcv_header
  end type

contains

  subroutine mb_create_header_out(this, rank, stage, hdr_rcv_type)
    class(MpiMessageBuilderType) :: this
    integer :: rank
    integer(I4B) :: stage    
    integer :: hdr_rcv_type
    

  end subroutine mb_create_header_out

  subroutine mb_create_header_in(this, rank, stage, hdr_snd_type)
    class(MpiMessageBuilderType) :: this
    integer :: rank
    integer :: stage    
    integer :: hdr_snd_type


  end subroutine mb_create_header_in

  subroutine create_snd_header(this, vdc, stage, hdr_snd_type)
    class(MpiMessageBuilderType) :: this
    class(VirtualDataContainerType), pointer :: vdc
    integer(I4B) :: stage
    integer, intent(out) :: hdr_snd_type ! the created MPI datatype, uncommitted
    ! local
    integer :: ierr
    integer(kind=MPI_ADDRESS_KIND), dimension(2) :: displ

    call MPI_Get_address(vdc%container_type, displ(1), ierr)
    call MPI_Get_address(vdc%id, displ(2), ierr)
    call MPI_Type_create_hindexed_block(2, 1, displ, MPI_INTEGER, hdr_snd_type, ierr)

  end subroutine create_snd_header

  subroutine create_rcv_header(this, stage, hdr_rcv_type)
    class(MpiMessageBuilderType) :: this
    integer(I4B) :: stage
    integer, intent(out) :: hdr_rcv_type !< the created MPI datatype, uncommitted
    ! local
    integer :: ierr

    call MPI_Type_contiguous(2, MPI_INTEGER, hdr_rcv_type, ierr)

  end subroutine create_rcv_header

end module MpiMessageBuilderModule