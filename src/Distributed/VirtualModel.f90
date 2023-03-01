module VirtualModelModule
  use VirtualBaseModule
  use VirtualDataContainerModule
  use ConstantsModule, only: LENMEMPATH
  use KindModule, only: I4B, LGP
  use ListModule, only: ListType
  use SimStagesModule
  use NumericalModelModule, only: NumericalModelType
  implicit none
  private

  public :: cast_as_virtual_model
  public :: get_virtual_model_from_list
  public :: get_virtual_model

  type, public, extends(VirtualDataContainerType) :: VirtualModelType
    class(NumericalModelType), pointer :: local_model
    ! CON
    type(VirtualInt1dType), pointer :: con_ia => null()
    type(VirtualInt1dType), pointer :: con_ja => null()
    type(VirtualInt1dType), pointer :: con_jas => null()
    type(VirtualInt1dType), pointer :: con_ihc => null()
    type(VirtualDbl1dType), pointer :: con_hwva => null()
    type(VirtualDbl1dType), pointer :: con_cl1 => null()
    type(VirtualDbl1dType), pointer :: con_cl2 => null()
    type(VirtualDbl1dType), pointer :: con_anglex => null()
    ! DIS
    type(VirtualIntType), pointer :: dis_ndim => null()
    type(VirtualIntType), pointer :: dis_nodes => null()
    type(VirtualIntType), pointer :: dis_nodesuser => null()
    type(VirtualInt1dType), pointer :: dis_nodeuser => null()
    type(VirtualIntType), pointer :: dis_nja => null()
    type(VirtualIntType), pointer :: dis_njas => null()
    type(VirtualDblType), pointer :: dis_xorigin => null()
    type(VirtualDblType), pointer :: dis_yorigin => null()
    type(VirtualDblType), pointer :: dis_angrot => null()
    type(VirtualDbl1dType), pointer :: dis_xc => null()
    type(VirtualDbl1dType), pointer :: dis_yc => null()
    type(VirtualDbl1dType), pointer :: dis_top => null()
    type(VirtualDbl1dType), pointer :: dis_bot => null()
    type(VirtualDbl1dType), pointer :: dis_area => null()
    ! Numerical Model fields
    type(VirtualIntType), pointer :: moffset => null()
    type(VirtualDbl1dType), pointer :: x => null()
    type(VirtualDbl1dType), pointer :: x_old => null()
    type(VirtualInt1dType), pointer :: ibound => null()
  contains
    ! public
    procedure :: create => vm_create
    procedure :: prepare_stage => vm_prepare_stage
    procedure :: destroy => vm_destroy
    generic :: operator(==) => eq_virtual_model, eq_numerical_model

    procedure :: dis_get_nodeuser
    procedure :: dis_noder_to_string

    ! private
    procedure, private :: create_virtual_fields
    procedure, private :: deallocate_data
    procedure, private :: eq_virtual_model
    procedure, private :: eq_numerical_model
  end type VirtualModelType

contains

  subroutine vm_create(this, name, id, model)
    class(VirtualModelType) :: this
    character(len=*) :: name
    integer(I4B) :: id
    class(NumericalModelType), pointer :: model
    ! local
    logical(LGP) :: is_local

    is_local = associated(model)
    call this%VirtualDataContainerType%vdc_create(name, id, is_local)

    this%local_model => model

    ! allocate fields
    call this%create_virtual_fields()

  end subroutine vm_create

  subroutine create_virtual_fields(this)
    class(VirtualModelType) :: this

    ! CON
    allocate (this%con_ia)
    call this%create_field(this%con_ia%to_base(), 'IA', 'CON')
    allocate (this%con_ja)
    call this%create_field(this%con_ja%to_base(), 'JA', 'CON')
    allocate (this%con_jas)
    call this%create_field(this%con_jas%to_base(), 'JAS', 'CON')
    allocate (this%con_ihc)
    call this%create_field(this%con_ihc%to_base(), 'IHC', 'CON')
    allocate (this%con_hwva)
    call this%create_field(this%con_hwva%to_base(), 'HWVA', 'CON')
    allocate (this%con_cl1)
    call this%create_field(this%con_cl1%to_base(), 'CL1', 'CON')
    allocate (this%con_cl2)
    call this%create_field(this%con_cl2%to_base(), 'CL2', 'CON')
    allocate (this%con_anglex)
    call this%create_field(this%con_anglex%to_base(), 'ANGLEX', 'CON')
    ! DIS
    allocate (this%dis_ndim)
    call this%create_field(this%dis_ndim%to_base(), 'NDIM', 'DIS')
    allocate (this%dis_nodes)
    call this%create_field(this%dis_nodes%to_base(), 'NODES', 'DIS')
    allocate (this%dis_nodesuser)
    call this%create_field(this%dis_nodesuser%to_base(), 'NODESUSER', 'DIS')
    allocate (this%dis_nodeuser)
    call this%create_field(this%dis_nodeuser%to_base(), 'NODEUSER', 'DIS')
    allocate (this%dis_nja)
    call this%create_field(this%dis_nja%to_base(), 'NJA', 'DIS')
    allocate (this%dis_njas)
    call this%create_field(this%dis_njas%to_base(), 'NJAS', 'DIS')
    allocate (this%dis_xorigin)
    call this%create_field(this%dis_xorigin%to_base(), 'XORIGIN', 'DIS')
    allocate (this%dis_yorigin)
    call this%create_field(this%dis_yorigin%to_base(), 'YORIGIN', 'DIS')
    allocate (this%dis_angrot)
    call this%create_field(this%dis_angrot%to_base(), 'ANGROT', 'DIS')
    allocate (this%dis_xc)
    call this%create_field(this%dis_xc%to_base(), 'XC', 'DIS')
    allocate (this%dis_yc)
    call this%create_field(this%dis_yc%to_base(), 'YC', 'DIS')
    allocate (this%dis_top)
    call this%create_field(this%dis_top%to_base(), 'TOP', 'DIS')
    allocate (this%dis_bot)
    call this%create_field(this%dis_bot%to_base(), 'BOT', 'DIS')
    allocate (this%dis_area)
    call this%create_field(this%dis_area%to_base(), 'AREA', 'DIS')
    ! Numerical model
    allocate (this%moffset)
    call this%create_field(this%moffset%to_base(), 'MOFFSET', '')
    allocate (this%x)
    call this%create_field(this%x%to_base(), 'X', '')
    allocate (this%x_old)
    call this%create_field(this%x_old%to_base(), 'XOLD', '')
    allocate (this%ibound)
    call this%create_field(this%ibound%to_base(), 'IBOUND', '')

  end subroutine create_virtual_fields

  subroutine vm_prepare_stage(this, stage)
    class(VirtualModelType) :: this
    integer(I4B) :: stage
    ! local
    integer(I4B) :: nodes, nodesuser, nja, njas
    logical(LGP) :: is_reduced

    if (stage == STG_AFTER_MDL_DF) then

      call this%map(this%dis_ndim%to_base(), &
                    (/STG_AFTER_MDL_DF/), MAP_ALL_TYPE)
      call this%map(this%dis_nodes%to_base(), &
                    (/STG_AFTER_MDL_DF/), MAP_ALL_TYPE)
      call this%map(this%dis_nodesuser%to_base(), &
                    (/STG_AFTER_MDL_DF/), MAP_ALL_TYPE)
      call this%map(this%dis_nja%to_base(), &
                    (/STG_AFTER_MDL_DF/), MAP_ALL_TYPE)
      call this%map(this%dis_njas%to_base(), &
                    (/STG_AFTER_MDL_DF/), MAP_ALL_TYPE)

    else if (stage == STG_BEFORE_AC) then

      nodes = this%dis_nodes%get()
      nodesuser = this%dis_nodesuser%get()
      is_reduced = (nodes /= nodesuser)
      call this%map(this%moffset%to_base(), &
                    (/STG_BEFORE_AC/), MAP_ALL_TYPE)
      if (is_reduced) then
        call this%map(this%dis_nodeuser%to_base(), nodes, &
                      (/STG_BEFORE_AC/), MAP_ALL_TYPE)
      else
        ! no reduction, zero sized array, never synchronize
        call this%map(this%dis_nodeuser%to_base(), 0, &
                      (/STG_NEVER/), MAP_ALL_TYPE)
      end if
    else if (stage == STG_BEFORE_DF) then

      nodes = this%dis_nodes%get()
      nja = this%dis_nja%get()
      njas = this%dis_njas%get()
      ! DIS
      call this%map(this%dis_xorigin%to_base(), &
                    (/STG_BEFORE_DF/), MAP_ALL_TYPE)
      call this%map(this%dis_yorigin%to_base(), &
                    (/STG_BEFORE_DF/), MAP_ALL_TYPE)
      call this%map(this%dis_angrot%to_base(), &
                    (/STG_BEFORE_DF/), MAP_ALL_TYPE)
      call this%map(this%dis_xc%to_base(), nodes, &
                    (/STG_BEFORE_DF/), MAP_ALL_TYPE)
      call this%map(this%dis_yc%to_base(), nodes, &
                    (/STG_BEFORE_DF/), MAP_ALL_TYPE)
      call this%map(this%dis_top%to_base(), nodes, &
                    (/STG_BEFORE_DF/), MAP_ALL_TYPE)
      call this%map(this%dis_bot%to_base(), nodes, &
                    (/STG_BEFORE_DF/), MAP_ALL_TYPE)
      call this%map(this%dis_area%to_base(), nodes, &
                    (/STG_BEFORE_DF/), MAP_ALL_TYPE)
      ! CON
      call this%map(this%con_ia%to_base(), nodes + 1, &
                    (/STG_BEFORE_DF/), MAP_ALL_TYPE)
      call this%map(this%con_ja%to_base(), nja, &
                    (/STG_BEFORE_DF/), MAP_ALL_TYPE)
      call this%map(this%con_jas%to_base(), nja, &
                    (/STG_BEFORE_DF/), MAP_ALL_TYPE)
      call this%map(this%con_ihc%to_base(), njas, &
                    (/STG_BEFORE_DF/), MAP_ALL_TYPE)
      call this%map(this%con_hwva%to_base(), njas, &
                    (/STG_BEFORE_DF/), MAP_ALL_TYPE)
      call this%map(this%con_cl1%to_base(), njas, &
                    (/STG_BEFORE_DF/), MAP_ALL_TYPE)
      call this%map(this%con_cl2%to_base(), njas, &
                    (/STG_BEFORE_DF/), MAP_ALL_TYPE)
      call this%map(this%con_anglex%to_base(), njas, &
                    (/STG_BEFORE_DF/), MAP_ALL_TYPE)
    end if

  end subroutine vm_prepare_stage

  !> @brief Get user node number from reduced number
  !<
  function dis_get_nodeuser(this, node_reduced) result(node_user)
    class(VirtualModelType) :: this !< this virtual model
    integer(I4B), intent(in) :: node_reduced !< the reduced node number
    integer(I4B) :: node_user !< the returned user node number

    if (this%dis_nodes%get() < this%dis_nodesuser%get()) then
      node_user = this%dis_nodeuser%get(node_reduced)
    else
      node_user = node_reduced
    end if

  end function dis_get_nodeuser

  subroutine dis_noder_to_string(this, node_reduced, node_str)
    class(VirtualModelType) :: this !< this virtual model
    integer(I4B), intent(in) :: node_reduced !< reduced node number
    character(len=*), intent(inout) :: node_str !< the string representative of the user node number
    ! local
    character(len=11) :: nr_str

    if (this%is_local) then
      call this%local_model%dis%noder_to_string(node_reduced, node_str)
    else
      ! for now this will look like: (102r)
      write (nr_str, '(i0)') node_reduced
      node_str = '('//trim(adjustl(nr_str))//'r)'
    end if

  end subroutine dis_noder_to_string

  subroutine vm_destroy(this)
    class(VirtualModelType) :: this

    call this%VirtualDataContainerType%destroy()
    call this%deallocate_data()

  end subroutine vm_destroy

  subroutine deallocate_data(this)
    class(VirtualModelType) :: this

    ! CON
    deallocate (this%con_ia)
    deallocate (this%con_ja)
    deallocate (this%con_jas)
    deallocate (this%con_ihc)
    deallocate (this%con_hwva)
    deallocate (this%con_cl1)
    deallocate (this%con_cl2)
    deallocate (this%con_anglex)
    ! DIS
    deallocate (this%dis_ndim)
    deallocate (this%dis_nodes)
    deallocate (this%dis_nodeuser)
    deallocate (this%dis_nja)
    deallocate (this%dis_njas)
    deallocate (this%dis_xorigin)
    deallocate (this%dis_yorigin)
    deallocate (this%dis_angrot)
    deallocate (this%dis_xc)
    deallocate (this%dis_yc)
    deallocate (this%dis_top)
    deallocate (this%dis_bot)
    deallocate (this%dis_area)
    ! Numerical model
    deallocate (this%moffset)
    deallocate (this%x)
    deallocate (this%x_old)
    deallocate (this%ibound)

  end subroutine deallocate_data

  function get_virtual_model_from_list(model_list, idx) result(v_model)
    type(ListType) :: model_list
    integer(I4B) :: idx
    class(VirtualModelType), pointer :: v_model
    ! local
    class(*), pointer :: obj_ptr

    obj_ptr => model_list%GetItem(idx)
    v_model => cast_as_virtual_model(obj_ptr)
    return

  end function get_virtual_model_from_list

  function cast_as_virtual_model(obj_ptr) result(v_model)
    class(*), pointer :: obj_ptr
    class(VirtualModelType), pointer :: v_model

    v_model => null()
    select type (obj_ptr)
    class is (VirtualModelType)
      v_model => obj_ptr
    end select

  end function cast_as_virtual_model

  function eq_virtual_model(this, v_model) result(is_equal)
    class(VirtualModelType), intent(in) :: this
    class(VirtualModelType), intent(in) :: v_model
    logical(LGP) :: is_equal

    is_equal = (this%id == v_model%id)

  end function eq_virtual_model

  function eq_numerical_model(this, num_model) result(is_equal)
    class(VirtualModelType), intent(in) :: this
    class(NumericalModelType), intent(in) :: num_model
    logical(LGP) :: is_equal

    is_equal = (this%id == num_model%id)

  end function eq_numerical_model

!> @brief Returns a virtual model with the specified id
!< from the global list
  function get_virtual_model(model_id) result(virtual_model)
    use VirtualDataListsModule, only: virtual_model_list
    integer(I4B) :: model_id
    class(VirtualModelType), pointer :: virtual_model
    ! local
    integer(I4B) :: i
    class(*), pointer :: vm

    virtual_model => null()
    do i = 1, virtual_model_list%Count()
      vm => virtual_model_list%GetItem(i)
      select type (vm)
      class is (VirtualModelType)
        if (vm%id == model_id) then
          virtual_model => vm
          return
        end if
      end select
    end do

  end function get_virtual_model

end module VirtualModelModule
