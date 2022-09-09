module IdmModule

  use KindModule, only: I4B, LGP
  use SimVariablesModule, only: errmsg
  use SimModule, only: store_error
  use IdmFileModflowModule, only: load_from_modflow_inputs

  implicit none
  private
  public :: IdmType
  public :: Idm
  public :: idm_load

  integer(I4B), parameter :: idm_input_modflow_t=1_4, idm_input_netcdf4_t=2_4

  type IdmType
    integer(I4B) :: input_type
    integer(I4B) :: iunit_lst
  contains
    procedure :: idm_load
  end type IdmType

  contains

  function Idm(iunit_lst) result(pIdmIf)
    !TODO: expose key/type param to set input_type
    integer(I4B), intent(in) :: iunit_lst
    type(IdmType), pointer :: pIdmIf

    allocate(pIdmIf)
    pIdmIf%input_type = idm_input_modflow_t
    pIdmIf%iunit_lst = iunit_lst
  end function Idm

  subroutine idm_load(this, namfile)
    class(IdmType) :: this
    character(len=*), intent(in) :: namfile

    select case (this%input_type)
    case(idm_input_modflow_t)
      call load_from_modflow_inputs(this%iunit_lst, namfile)
    case default
      write(errmsg, '(a)') 'IDM valid input source type not selected'
      call store_error(errmsg, terminate=.true.)
    end select
  end subroutine idm_load

end module IdmModule
