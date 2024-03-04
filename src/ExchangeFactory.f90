module ExchangeFactoryModule
  use KindModule, only: I4B, LGP
  use ConstantsModule, only: LENMEMPATH, LINELENGTH
  use SimModule, only: store_error
  use CharacterStringModule, only: CharacterStringType
  use ArrayHandlersModule, only: ifind
  use ExchangeRegistrarModule
  ! TODO BEGIN TEMPLATING
  use GwfGwfExchangeModule, only: register_gwfgwf
  use GwfGwtExchangeModule, only: register_gwfgwt
  use GwfGweExchangeModule, only: register_gwfgwe
  use GwfPrtExchangeModule, only: register_gwfprt
  use GwtGwtExchangeModule, only: register_gwtgwt
  use GweGweExchangeModule, only: register_gwegwe
  use SwfGwfExchangeModule, only: register_swfgwf
  use VirtualGwfExchangeModule, only: register_virtual_gwfgwf
  use VirtualGwtExchangeModule, only: register_virtual_gwtgwt
  use VirtualGweExchangeModule, only: register_virtual_gwegwe
  use VirtualPrtExchangeModule, only: register_virtual_prtprt
  ! TODO END TEMPLATING

  implicit none
  private
  public :: create_exchanges

contains

  subroutine create_exchanges( &
    types, &
    files, &
    mempaths, &
    model1_names, &
    model2_names)
    ! dummy
    type(CharacterStringType), dimension(:), contiguous, &
      pointer, intent(in) :: types
    type(CharacterStringType), dimension(:), contiguous, &
      pointer, intent(in) :: files
    type(CharacterStringType), dimension(:), contiguous, &
      pointer, intent(in) :: mempaths
    type(CharacterStringType), dimension(:), contiguous, &
      pointer, intent(in) :: model1_names
    type(CharacterStringType), dimension(:), contiguous, &
      pointer, intent(in) :: model2_names
    ! local
    integer(I4B) :: n
    integer(I4B) :: exchange_id
    character(len=LINELENGTH) :: exchange_name
    character(len=LINELENGTH) :: exchange_type
    character(len=LINELENGTH) :: exchange_file
    character(len=LINELENGTH) :: exchange_mempath
    character(len=LINELENGTH) :: model1_name
    character(len=LINELENGTH) :: model2_name
    character(len=LINELENGTH) :: errmsg
    procedure(register_actual_exchange), pointer :: register_actual
    procedure(register_virtual_exchange), pointer :: register_virtual

    exchange_id = 0
    do n = 1, size(types)
      exchange_id = exchange_id + 1
      exchange_type = types(n)
      exchange_file = files(n)
      exchange_mempath = mempaths(n)
      model1_name = model1_names(n)
      model2_name = model2_names(n)

      ! pick the registration procedures for this exchange type
      select case (exchange_type)
        ! TODO BEGIN TEMPLATING
      case ('GWF6-GWF6')
        write (exchange_name, '(a,i0)') 'GWF-GWF_', exchange_id
        register_actual => register_gwfgwf
        register_virtual => register_virtual_gwfgwf
      case ('GWT6-GWT6')
        write (exchange_name, '(a,i0)') 'GWT-GWT_', exchange_id
        register_actual => register_gwtgwt
        register_virtual => register_virtual_gwtgwt
      case ('GWE6-GWE6')
        write (exchange_name, '(a,i0)') 'GWE-GWE_', exchange_id
        register_actual => register_gwegwe
        register_virtual => register_virtual_gwegwe
      case ('GWF6-GWT6')
        write (exchange_name, '(a,i0)') 'GWF-GWT_', exchange_id
        register_actual => register_gwfgwt
        register_virtual => null()
      case ('GWF6-GWE6')
        write (exchange_name, '(a,i0)') 'GWF-GWE_', exchange_id
        register_actual => register_gwfgwe
        register_virtual => null()
      case ('GWF6-PRT6')
        write (exchange_name, '(a,i0)') 'GWF-PRT_', exchange_id
        register_actual => register_gwfprt
        register_virtual => null()
      case ('SWF6-GWF6')
        write (exchange_name, '(a,i0)') 'SWF-GWF_', exchange_id
        register_actual => register_swfgwf
        register_virtual => null()
        ! TODO END TEMPLATING
      case default
        write (errmsg, '(a,a)') &
          'Unknown simulation exchange type: ', trim(exchange_type)
        call store_error(errmsg, terminate=.true.)
      end select

      ! register the exchange
      call register_exchange( &
        register_actual, &
        register_virtual, &
        exchange_id, &
        exchange_name, &
        exchange_type, &
        exchange_file, &
        exchange_mempath, &
        model1_name, &
        model2_name)
    end do
  end subroutine create_exchanges

end module ExchangeFactoryModule
