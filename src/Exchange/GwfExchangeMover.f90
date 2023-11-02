module GwfExgMoverModule
  use KindModule, only: I4B, DP
  use ConstantsModule, only: LENMODELNAME, LENPACKAGENAME
  use MemoryManagerModule, only: mem_allocate, mem_deallocate
  use MemoryHelperModule, only: split_mem_path  
  use VirtualModelModule
  use BaseDisModule
  use GwfMvrModule
  implicit none
  private

  public :: exg_mvr_cr

  type, public, extends(GwfMvrType) :: GwfExgMoverType
    real(DP), dimension(:), pointer, contiguous :: qpactual_m1 => null()
    real(DP), dimension(:), pointer, contiguous :: qpactual_m2 => null()
  contains
    procedure :: mvr_da => xmvr_da
    procedure :: xmvr_cf
    procedure :: mvr_fc => xmvr_fc
    procedure :: check_packages => xmvr_check_packages
    procedure :: assign_packagemovers => xmvr_assign_packagemovers
    procedure :: reinitialize_movers => xmvr_reinitialize_movers
    procedure :: allocate_arrays => xmvr_allocate_arrays
  end type

contains

  subroutine exg_mvr_cr(exg_mvr, name_parent, inunit, iout, dis)
    class(GwfExgMoverType), pointer :: exg_mvr
    character(len=*), intent(in) :: name_parent
    integer(I4B), intent(in) :: inunit
    integer(I4B), intent(in) :: iout
    class(DisBaseType), pointer :: dis
    
    allocate (exg_mvr)
    
    ! Init through base
    call exg_mvr%mvr_init(name_parent, inunit, iout, dis, 1)

  end subroutine exg_mvr_cr

  subroutine xmvr_check_packages(this)
    use ConstantsModule, only: LINELENGTH
    use MemoryManagerModule, only: mem_setptr
    use SimModule, only: store_error, count_errors, store_error_unit
    class(GwfExgMoverType), intent(inout) :: this
    ! local
    !character(len=LINELENGTH) :: errmsg
    !integer(I4B) :: i
    !integer(I4B), pointer :: imover_ptr

    return
    ! TODO_MJR:
    ! do i = 1, size(this%pckMemPaths)
    !   imover_ptr => null()
    !   call mem_setptr(imover_ptr, 'IMOVER', trim(this%pckMemPaths(i)))
    !   if (imover_ptr == 0) then
    !     write (errmsg, '(a, a, a)') &
    !       'ERROR.  MODEL AND PACKAGE "', &
    !       trim(this%pckMemPaths(i)), &
    !       '" DOES NOT HAVE MOVER SPECIFIED IN OPTIONS BLOCK.'
    !     call store_error(errmsg)
    !   end if
    ! end do

    ! if (count_errors() > 0) then
    !   call this%parser%StoreErrorUnit()
    ! end if

  end subroutine xmvr_check_packages

  subroutine xmvr_assign_packagemovers(this)
    use PackageMoverModule, only: set_packagemover_pointer
    class(GwfExgMoverType), intent(inout) :: this
    ! local
    integer(I4B) :: i
    character(len=LENMODELNAME) :: mname
    character(len=LENPACKAGENAME) :: pname
    class(VirtualModelType), pointer :: vm

    do i = 1, size(this%pckMemPaths)
      if (this%pakmovers(i)%memoryPath == '') then
        ! is it local?
        call split_mem_path(this%pckMemPaths(i), mname, pname)
        vm => get_virtual_model(mname)
        if(vm%is_local) then
          ! yes, we need the pointers
          call set_packagemover_pointer(this%pakmovers(i), &
                                        trim(this%pckMemPaths(i)))
        end if
      end if
    end do
  end subroutine xmvr_assign_packagemovers

  subroutine xmvr_reinitialize_movers(this, nr_active_movers)
    class(GwfExgMoverType) :: this
    integer(I4B) :: nr_active_movers
    ! local
    integer(I4B) :: i
    character(len=LENMODELNAME) :: mname
    character(len=LENPACKAGENAME) :: pname
    class(VirtualModelType), pointer :: vm

    call this%GwfMvrType%reinitialize_movers(nr_active_movers)

    ! deactivate remote parts
    do i = 1, nr_active_movers
      call split_mem_path(this%mvr(i)%pckNameSrc, mname, pname)
      vm => get_virtual_model(mname)
      this%mvr(i)%is_provider_active = vm%is_local
      call split_mem_path(this%mvr(i)%pckNameTgt, mname, pname)
      vm => get_virtual_model(mname)
      this%mvr(i)%is_receiver_active = vm%is_local
    end do

  end subroutine xmvr_reinitialize_movers

  subroutine xmvr_cf(this)
    class(GwfExgMoverType) :: this
    ! local
    integer(I4B) :: i

    ! do i = 1, this%nmvr
    !   call this%mvr(i)%update_provider()
    ! end do

  end subroutine xmvr_cf

  subroutine xmvr_fc(this)
    class(GwfExgMoverType) :: this
    ! local
    integer(I4B) :: i

    do i = 1, this%nmvr
      call this%mvr(i)%update_provider()
    end do
    do i = 1, this%nmvr
      call this%mvr(i)%update_receiver()
    end do

  end subroutine xmvr_fc

  subroutine xmvr_allocate_arrays(this)
    class(GwfExgMoverType) :: this

    call this%GwfMvrType%allocate_arrays()
    call mem_allocate(this%qpactual_m1, this%maxmvr, 'QPACTUAL_M1', this%memoryPath)
    call mem_allocate(this%qpactual_m2, this%maxmvr, 'QPACTUAL_M2', this%memoryPath)

  end subroutine xmvr_allocate_arrays

  subroutine xmvr_da(this)
    class(GwfExgMoverType) :: this

    call this%GwfMvrType%mvr_da()
    call mem_deallocate(this%qpactual_m1)
    call mem_deallocate(this%qpactual_m2)

  end subroutine xmvr_da

end module GwfExgMoverModule