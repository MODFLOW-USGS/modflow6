!> @brief This module contains the InputModelContextodule
!!
!!
!<
module InputModelContextModule

  use KindModule, only: DP, I4B, LGP
  use ConstantsModule, only: LINELENGTH, LENMODELNAME, LENTIMESERIESNAME
  use SimModule, only: store_error, store_error_filename
  use SimVariablesModule, only: errmsg
  use ListModule, only: ListType
  use InputDefinitionModule, only: InputParamDefinitionType
  use NC4ModelInputsModule, only: NC4ModelInputsType

  implicit none
  private
  public :: ModelContextType
  public :: AddModelNC4Context
  public :: GetModelNC4Context
  public :: ModelContextDestroy

  type(ListType) :: model_context_list

  !> @brief type for storing model context
  !!
  !! This type is used to store a list of context objects
  !! associated with a model. Add additional context types as
  !! appropriate.
  !!
  !<
  type :: ModelContextType
    character(len=LENMODELNAME) :: modelname !< name of model
    character(len=LINELENGTH) :: modelfname !< name of model input file
    type(NC4ModelInputsType), pointer :: nc4_context
  contains
    procedure :: init => modelctx_init
    procedure :: destroy => modelctx_destroy
  end type ModelContextType

contains

  !> @brief model context init
  !!
  !<
  subroutine modelctx_init(this, modelname, modelfname)
    class(ModelContextType), intent(inout) :: this
    character(len=*), intent(in) :: modelname
    character(len=*), intent(in) :: modelfname
    !
    this%modelname = modelname
    this%modelfname = modelfname
    !
    nullify (this%nc4_context)
    !
    return
  end subroutine modelctx_init

  !> @brief destroy model context object
  !!
  !<
  subroutine modelctx_destroy(this)
    class(ModelContextType), intent(inout) :: this
    !
    if (associated(this%nc4_context)) then
      call this%nc4_context%destroy()
    end if
    !
    return
  end subroutine modelctx_destroy

  !> @brief add model context object to list
  !!
  !<
  subroutine AddModelContext(model_context)
    ! -- dummy variables
    class(ModelContextType), pointer, intent(inout) :: model_context
    ! -- local variables
    class(*), pointer :: obj
    !
    obj => model_context
    call model_context_list%Add(obj)
    !
    ! -- return
    return
  end subroutine AddModelContext

  !> @brief get model context object from list
  !!
  !<
  function GetModelContext(modelname) result(res)
    ! -- dummy variables
    character(len=*), intent(in) :: modelname
    class(ModelContextType), pointer :: res
    ! -- local variables
    class(*), pointer :: obj
    class(ModelContextType), pointer :: ctx
    integer(I4B) :: n
    !
    ! -- initialize res
    res => null()
    !
    ! -- get the object from the list
    do n = 1, model_context_list%Count()
      obj => model_context_list%GetItem(n)
      if (associated(obj)) then
        select type (obj)
        class is (ModelContextType)
          ctx => obj
          if (ctx%modelname == modelname) then
            res => obj
            exit
          end if
        end select
      end if
    end do
    !
    ! -- return
    return
  end function GetModelContext

  !> @brief cleanup model context objects
  !!
  !<
  subroutine ModelContextDestroy()
    ! -- dummy variables
    ! -- local variables
    class(*), pointer :: obj
    class(ModelContextType), pointer :: ctx
    integer(I4B) :: n
    !
    ! -- get the object from the list
    do n = 1, model_context_list%Count()
      obj => model_context_list%GetItem(n)
      if (associated(obj)) then
        select type (obj)
        class is (ModelContextType)
          ctx => obj
          call ctx%destroy()
          deallocate (ctx)
          nullify (ctx)
        end select
      end if
    end do
    !
    call model_context_list%clear()
    !
    ! -- return
    return
  end subroutine ModelContextDestroy

  !> @brief get model context object from list
  !!
  !<
  subroutine AddModelNC4Context(modelname, modelfname, nc4_context)
    ! -- dummy variables
    character(len=*), intent(in) :: modelname
    character(len=*), intent(in) :: modelfname
    type(NC4ModelInputsType), pointer, intent(in) :: nc4_context
    ! -- local variables
    class(ModelContextType), pointer :: ctx
    !
    ! -- initialize
    nullify (ctx)
    !
    ctx => GetModelContext(modelname)
    !
    if (associated(ctx)) then
      ctx%nc4_context => nc4_context
    else
      allocate (ctx)
      call ctx%init(modelname, modelfname)
      ctx%nc4_context => nc4_context
      call AddModelContext(ctx)
    end if
    !
    ! -- return
    return
  end subroutine AddModelNC4Context

  !> @brief get model context object from list
  !!
  !<
  function GetModelNC4Context(modelname) result(nc4_context)
    ! -- dummy variables
    character(len=*), intent(in) :: modelname
    ! -- result
    type(NC4ModelInputsType), pointer :: nc4_context
    ! -- local variables
    class(*), pointer :: obj
    class(ModelContextType), pointer :: ctx
    integer(I4B) :: n
    !
    ! -- initialize res
    nc4_context => null()
    !
    ! -- get the object from the list
    do n = 1, model_context_list%Count()
      obj => model_context_list%GetItem(n)
      if (associated(obj)) then
        select type (obj)
        class is (ModelContextType)
          ctx => obj
          if (ctx%modelname == modelname) then
            nc4_context => ctx%nc4_context
            exit
          end if
        end select
      end if
    end do
    !
    ! -- set error if not found
    if (.not. associated(nc4_context)) then
      errmsg = 'Programming error. NC4 Model context not found. &
               &Model='//trim(modelname)//'.'
      call store_error(errmsg, .true.)
    end if
    !
    ! -- return
    return
  end function GetModelNC4Context

end module InputModelContextModule
