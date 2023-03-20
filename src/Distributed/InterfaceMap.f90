module InterfaceMapModule
  use KindModule, only: I4B
  use ConstantsModule, only: LENMODELNAME, LENEXCHANGENAME
  use IndexMapModule

  implicit none
  private

  type, public :: InterfaceMapType
    integer(I4B) :: nr_models
    integer(I4B), dimension(:), pointer, contiguous :: model_ids => null()
    character(len=LENMODELNAME), dimension(:), &
      pointer, contiguous :: model_names => null()
    integer(I4B) :: nr_exchanges
    integer(I4B), dimension(:), pointer, contiguous :: exchange_ids => null()
    character(len=LENEXCHANGENAME), dimension(:), &
      pointer, contiguous :: exchange_names => null()
    integer(I4B) :: prim_exg_idx
    type(IndexMapType), dimension(:), pointer :: node_map => null()
    type(IndexMapType), dimension(:), pointer :: connection_map => null()
    type(IndexMapSgnType), dimension(:), pointer :: exchange_map => null()
  contains
    procedure :: init
    procedure :: add
    procedure :: destroy
    procedure :: print_interface
  end type InterfaceMapType

contains

  subroutine init(this, nr_models, nr_exchanges)
    class(InterfaceMapType) :: this
    integer(I4B) :: nr_models
    integer(I4B) :: nr_exchanges

    this%nr_models = nr_models
    this%nr_exchanges = nr_exchanges

    allocate (this%model_ids(nr_models))
    allocate (this%model_names(nr_models))
    allocate (this%exchange_ids(nr_exchanges))
    allocate (this%exchange_names(nr_exchanges))

    allocate (this%node_map(nr_models))
    allocate (this%connection_map(nr_models))
    allocate (this%exchange_map(nr_exchanges))

    ! model id == -1 when not set
    this%model_ids = -1
    this%exchange_ids = -1

  end subroutine init

  !> @ Adds a map, either by extending the existing map
  !! for a certain model or exchange, or by assigning
  !! the map to an empty slot.
  !!
  !! The map to which is added, should be properly
  !< initialized beforehand
  subroutine add(this, map_to_add)
    use ArrayHandlersModule, only: ExtendPtrArray, ifind
    class(InterfaceMapType) :: this
    class(InterfaceMapType) :: map_to_add
    ! local
    integer(I4B) :: im, ie
    integer(I4B) :: m_id, m_index
    integer(I4B) :: e_id, e_index

    ! add models
    do im = 1, map_to_add%nr_models
      m_id = map_to_add%model_ids(im)
      m_index = ifind(this%model_ids, m_id)
      if (m_index > 0) then
        ! extend existing index map
        call this%node_map(m_index)%add(map_to_add%node_map(im))
        call this%connection_map(m_index)%add(map_to_add%connection_map(im))
      else
        ! place in first empty spot
        m_index = ifind(this%model_ids, -1)
        this%model_ids(m_index) = m_id
        this%model_names(m_index) = map_to_add%model_names(im)
        call this%node_map(m_index)%copy(map_to_add%node_map(im))
        call this%connection_map(m_index)%copy(map_to_add%connection_map(im))
      end if
    end do

    ! add exchanges
    do ie = 1, map_to_add%nr_exchanges
      e_id = map_to_add%exchange_ids(ie)
      e_index = ifind(this%exchange_ids, e_id)
      if (e_index > 0) then
        ! extend existing index map
        call this%exchange_map(e_index)%add(map_to_add%exchange_map(ie))
      else
        ! place in first empty spot
        e_index = ifind(this%exchange_ids, -1)
        this%exchange_ids(e_index) = e_id
        this%exchange_names(e_index) = map_to_add%exchange_names(ie)
        call this%exchange_map(e_index)%copy(map_to_add%exchange_map(ie))
      end if
    end do

  end subroutine add

  !> @brief Dumps interface data to the screen
  !<
  subroutine print_interface(this, outunit)
    class(InterfaceMapType) :: this
    integer(I4B) :: outunit
    ! local
    integer(I4B) :: i, n

    write(outunit,'(a,i0)') "nr. models: ", this%nr_models
    write(outunit,'(a,i0)') "nr. exchanges: ", this%nr_exchanges
    do i = 1, this%nr_models
      write(outunit,'(3a,i0,a)') "model: ", trim(this%model_names(i)), &
                       "[", this%model_ids(i), "]"
      write(outunit,*) "node map:"
      do n = 1, size(this%node_map(i)%src_idx)      
        write(outunit,'(i7,a,i7)') this%node_map(i)%src_idx(n), &
                                 " ", this%node_map(i)%tgt_idx(n)
      end do
      write(outunit,*) "connection map:"
      do n = 1, size(this%connection_map(i)%src_idx)      
        write(outunit,'(i7,a,i7)') this%connection_map(i)%src_idx(n), &
                         " ", this%connection_map(i)%tgt_idx(n)
      end do      
    end do

    do i = 1, this%nr_exchanges
      write(outunit,'(3a,i0,a)') "exchange: ", trim(this%exchange_names(i)), &
                       "[", this%exchange_ids(i), "]"
      write(outunit,*) "exchange map:"
      do n = 1, size(this%exchange_map(i)%src_idx)      
        write(outunit,'(i7,a,i7,a,i7)') this%exchange_map(i)%src_idx(n), &
                         " ", this%exchange_map(i)%tgt_idx(n), &
                         " ", this%exchange_map(i)%sign(n)
      end do
    end do

  end subroutine print_interface

  subroutine destroy(this)
    class(InterfaceMapType) :: this
    ! local
    integer(I4B) :: i

    deallocate (this%model_ids)
    deallocate (this%model_names)
    deallocate (this%exchange_ids)
    deallocate (this%exchange_names)

    do i = 1, this%nr_models
      deallocate (this%node_map(i)%src_idx)
      deallocate (this%node_map(i)%tgt_idx)
      deallocate (this%connection_map(i)%src_idx)
      deallocate (this%connection_map(i)%tgt_idx)
    end do
    deallocate (this%node_map)
    deallocate (this%connection_map)

    do i = 1, this%nr_exchanges
      deallocate (this%exchange_map(i)%src_idx)
      deallocate (this%exchange_map(i)%tgt_idx)
      deallocate (this%exchange_map(i)%sign)
    end do
    deallocate (this%exchange_map)

  end subroutine destroy

end module InterfaceMapModule
