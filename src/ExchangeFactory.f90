module ExchangeFactoryModule
  use KindModule, only: I4B, LGP
  use ConstantsModule, only: LENMEMPATH, LINELENGTH, LENEXCHANGENAME
  use SimModule, only: store_error, count_errors
  use SimVariablesModule, only: iout, idm_context, model_names, model_loc_idx
  use MemoryHelperModule, only: create_mem_path
  use MemoryManagerModule, only: mem_setptr, mem_allocate
  use CharacterStringModule, only: CharacterStringType
  use ArrayHandlersModule, only: ifind

  implicit none
  private
  public :: create_exchanges

contains

  subroutine create_gwfgwf_exchange( &
    fname, &
    exg_id, &
    both_local, &
    both_remote, &
    m1_id, m2_id, &
    exg_mempath)
    ! -- modules
    use GwfGwfExchangeModule, only: gwfgwf_cr
    use VirtualGwfExchangeModule, only: add_virtual_gwf_exchange
    ! -- dummy
    character(len=LINELENGTH), intent(in) :: fname
    integer(I4B), intent(in) :: exg_id
    logical(LGP), intent(in) :: both_local
    logical(LGP), intent(in) :: both_remote
    integer(I4B), intent(in) :: m1_id
    integer(I4B), intent(in) :: m2_id
    character(len=LENMEMPATH), intent(in) :: exg_mempath
    ! -- local
    character(len=LENEXCHANGENAME) :: exg_name

    write (exg_name, '(a,i0)') 'GWF-GWF_', exg_id
    if (.not. both_remote) &
      call gwfgwf_cr( &
      fname, &
      exg_name, &
      exg_id, &
      m1_id, &
      m2_id, &
      exg_mempath)
    call add_virtual_gwf_exchange( &
      exg_name, &
      exg_id, &
      m1_id, &
      m2_id)
  end subroutine create_gwfgwf_exchange

  subroutine create_gwfgwt_exchange( &
    fname, &
    exg_id, &
    both_local, &
    both_remote, &
    m1_id, m2_id, &
    exg_mempath)
    ! -- modules
    use GwfGwtExchangeModule, only: gwfgwt_cr
    ! -- dummy
    character(len=LINELENGTH), intent(in) :: fname
    integer(I4B), intent(in) :: exg_id
    logical(LGP), intent(in) :: both_local
    logical(LGP), intent(in) :: both_remote
    integer(I4B), intent(in) :: m1_id
    integer(I4B), intent(in) :: m2_id
    character(len=LENMEMPATH), intent(in) :: exg_mempath
    ! -- local
    character(len=LENEXCHANGENAME) :: exg_name

    write (exg_name, '(a,i0)') 'GWF-GWT_', exg_id
    if (both_local) &
      call gwfgwt_cr( &
      fname, &
      exg_name, &
      exg_id, &
      m1_id, &
      m2_id, &
      exg_mempath)
  end subroutine create_gwfgwt_exchange

  subroutine create_gwtgwt_exchange( &
    fname, &
    exg_id, &
    both_local, &
    both_remote, &
    m1_id, m2_id, &
    exg_mempath)
    ! -- modules
    use GwtGwtExchangeModule, only: gwtgwt_cr
    use VirtualGwtExchangeModule, only: add_virtual_gwt_exchange
    ! -- dummy
    character(len=LINELENGTH), intent(in) :: fname
    integer(I4B), intent(in) :: exg_id
    logical(LGP), intent(in) :: both_local
    logical(LGP), intent(in) :: both_remote
    integer(I4B), intent(in) :: m1_id
    integer(I4B), intent(in) :: m2_id
    character(len=LENMEMPATH), intent(in) :: exg_mempath
    ! -- local
    character(len=LENEXCHANGENAME) :: exg_name

    write (exg_name, '(a,i0)') 'GWT-GWT_', exg_id
    if (.not. both_remote) &
      call gwtgwt_cr( &
      fname, &
      exg_name, &
      exg_id, &
      m1_id, &
      m2_id, &
      exg_mempath)
    call add_virtual_gwt_exchange( &
      exg_name, &
      exg_id, &
      m1_id, &
      m2_id)
  end subroutine create_gwtgwt_exchange

  subroutine create_gwfgwe_exchange( &
    fname, &
    exg_id, &
    both_local, &
    both_remote, &
    m1_id, m2_id, &
    exg_mempath)
    ! -- modules
    use GwfGweExchangeModule, only: gwfgwe_cr
    ! -- dummy
    character(len=LINELENGTH), intent(in) :: fname
    integer(I4B), intent(in) :: exg_id
    logical(LGP), intent(in) :: both_local
    logical(LGP), intent(in) :: both_remote
    integer(I4B), intent(in) :: m1_id
    integer(I4B), intent(in) :: m2_id
    character(len=LENMEMPATH), intent(in) :: exg_mempath
    ! -- local
    character(len=LENEXCHANGENAME) :: exg_name

    write (exg_name, '(a,i0)') 'GWF-GWE_', exg_id
    if (.not. both_remote) &
      call gwfgwe_cr( &
      fname, &
      exg_name, &
      exg_id, &
      m1_id, &
      m2_id, &
      exg_mempath)
  end subroutine create_gwfgwe_exchange

  subroutine create_gwegwe_exchange( &
    fname, &
    exg_id, &
    both_local, &
    both_remote, &
    m1_id, m2_id, &
    exg_mempath)
    ! -- modules
    use GweGweExchangeModule, only: gwegwe_cr
    use VirtualGweExchangeModule, only: add_virtual_gwe_exchange
    ! -- dummy
    character(len=LINELENGTH), intent(in) :: fname
    integer(I4B), intent(in) :: exg_id
    logical(LGP), intent(in) :: both_local
    logical(LGP), intent(in) :: both_remote
    integer(I4B), intent(in) :: m1_id
    integer(I4B), intent(in) :: m2_id
    character(len=LENMEMPATH), intent(in) :: exg_mempath
    ! -- local
    character(len=LENEXCHANGENAME) :: exg_name

    write (exg_name, '(a,i0)') 'GWE-GWE_', exg_id
    if (.not. both_remote) &
      call gwegwe_cr( &
      fname, &
      exg_name, &
      exg_id, &
      m1_id, &
      m2_id, &
      exg_mempath)
    call add_virtual_gwe_exchange( &
      exg_name, &
      exg_id, &
      m1_id, &
      m2_id)
  end subroutine create_gwegwe_exchange

  subroutine create_gwfprt_exchange( &
    fname, &
    exg_id, &
    both_local, &
    both_remote, &
    m1_id, m2_id, &
    exg_mempath)
    ! -- modules
    use GwfPrtExchangeModule, only: gwfprt_cr
    ! -- dummy
    character(len=LINELENGTH), intent(in) :: fname
    integer(I4B), intent(in) :: exg_id
    logical(LGP), intent(in) :: both_local
    logical(LGP), intent(in) :: both_remote
    integer(I4B), intent(in) :: m1_id
    integer(I4B), intent(in) :: m2_id
    character(len=LENMEMPATH), intent(in) :: exg_mempath
    ! -- local
    character(len=LENEXCHANGENAME) :: exg_name

    write (exg_name, '(a,i0)') 'GWF-PRT_', exg_id
    if (.not. both_remote) &
      call gwfprt_cr( &
      fname, &
      exg_name, &
      exg_id, &
      m1_id, &
      m2_id, &
      exg_mempath)
  end subroutine create_gwfprt_exchange

  subroutine create_swfgwf_exchange( &
    fname, &
    exg_id, &
    both_local, &
    both_remote, &
    m1_id, m2_id, &
    exg_mempath)
    ! -- modules
    use SwfGwfExchangeModule, only: swfgwf_cr
    ! -- dummy
    character(len=LINELENGTH), intent(in) :: fname
    integer(I4B), intent(in) :: exg_id
    logical(LGP), intent(in) :: both_local
    logical(LGP), intent(in) :: both_remote
    integer(I4B), intent(in) :: m1_id
    integer(I4B), intent(in) :: m2_id
    character(len=LENMEMPATH), intent(in) :: exg_mempath
    ! -- local
    character(len=LENEXCHANGENAME) :: exg_name

    write (exg_name, '(a,i0)') 'SWF-GWF_', exg_id
    if (.not. both_remote) &
      call swfgwf_cr( &
      fname, &
      exg_name, &
      exg_id, &
      m1_id, &
      m2_id, &
      exg_mempath)
  end subroutine create_swfgwf_exchange

  subroutine create_exchanges(etypes, efiles, emnames_a, emnames_b, emempaths)
    ! -- dummy
    type(CharacterStringType), dimension(:), contiguous, &
      pointer, intent(in) :: etypes !< exg types
    type(CharacterStringType), dimension(:), contiguous, &
      pointer, intent(in) :: efiles !< exg file names
    type(CharacterStringType), dimension(:), contiguous, &
      pointer, intent(in) :: emnames_a !< model a names
    type(CharacterStringType), dimension(:), contiguous, &
      pointer, intent(in) :: emnames_b !< model b names
    type(CharacterStringType), dimension(:), contiguous, &
      pointer, intent(in) :: emempaths
    ! -- local
    integer(I4B) :: exg_id, n
    integer(I4B) :: m1_id, m2_id
    logical(LGP) :: both_remote, both_local
    character(len=LINELENGTH) :: fname, name1, name2
    character(len=LENMEMPATH) :: exg_mempath
    character(len=LINELENGTH) :: errmsg, exgtype
    ! -- formats
    character(len=*), parameter :: fmtmerr = "('Error in simulation control ', &
      &'file.  Could not find model: ', a)"

    exg_id = 0
    do n = 1, size(etypes)
      exgtype = etypes(n)
      fname = efiles(n)
      name1 = emnames_a(n)
      name2 = emnames_b(n)
      exg_mempath = emempaths(n)
      exg_id = exg_id + 1

      ! find model index in list
      m1_id = ifind(model_names, name1)
      if (m1_id < 0) then
        write (errmsg, fmtmerr) trim(name1)
        call store_error(errmsg, terminate=.true.)
      end if
      m2_id = ifind(model_names, name2)
      if (m2_id < 0) then
        write (errmsg, fmtmerr) trim(name2)
        call store_error(errmsg, terminate=.true.)
      end if

      ! both models on other process? then don't create it here...
      both_remote = (model_loc_idx(m1_id) == -1 .and. &
                     model_loc_idx(m2_id) == -1)
      both_local = (model_loc_idx(m1_id) > 0 .and. &
                    model_loc_idx(m2_id) > 0)
      if (.not. both_remote) write (iout, '(4x,a,a,i0,a,i0,a,i0)') &
        trim(exgtype), ' exchange ', exg_id, &
        ' will be created to connect model ', m1_id, &
        ' with model ', m2_id

      select case (exgtype)
      case ('GWF6-GWF6')
        call create_gwfgwf_exchange( &
          fname, &
          exg_id, &
          both_local, &
          both_remote, &
          m1_id, &
          m2_id, &
          exg_mempath)
      case ('GWF6-GWT6')
        call create_gwfgwt_exchange( &
          fname, &
          exg_id, &
          both_local, &
          both_remote, &
          m1_id, &
          m2_id, &
          exg_mempath)
      case ('GWT6-GWT6')
        call create_gwtgwt_exchange( &
          fname, &
          exg_id, &
          both_local, &
          both_remote, &
          m1_id, &
          m2_id, &
          exg_mempath)
      case ('GWE6-GWE6')
        call create_gwegwe_exchange( &
          fname, &
          exg_id, &
          both_local, &
          both_remote, &
          m1_id, &
          m2_id, &
          exg_mempath)
      case ('GWF6-GWE6')
        call create_gwfgwe_exchange( &
          fname, &
          exg_id, &
          both_local, &
          both_remote, &
          m1_id, &
          m2_id, &
          exg_mempath)
      case ('GWF6-PRT6')
        call create_gwfprt_exchange( &
          fname, &
          exg_id, &
          both_local, &
          both_remote, &
          m1_id, &
          m2_id, &
          exg_mempath)
      case ('SWF6-GWF6')
        call create_swfgwf_exchange( &
          fname, &
          exg_id, &
          both_local, &
          both_remote, &
          m1_id, &
          m2_id, &
          exg_mempath)
      case default
        write (errmsg, '(a,a)') &
          'Unknown simulation exchange type: ', trim(exgtype)
        call store_error(errmsg, terminate=.true.)
      end select
    end do
  end subroutine create_exchanges

end module ExchangeFactoryModule
