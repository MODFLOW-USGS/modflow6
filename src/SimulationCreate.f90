module SimulationCreateModule

  use KindModule, only: DP, I4B, LGP, write_kindinfo
  use ConstantsModule, only: LINELENGTH, LENMODELNAME, LENBIGLINE, LENMEMPATH, &
                             LENPACKAGENAME, DZERO
  use SimVariablesModule, only: iout
  use GenericUtilitiesModule, only: sim_message, write_centered
  use SimModule, only: store_error, count_errors, &
                       store_error_unit, MaxErrors
  use VersionModule, only: write_listfile_header
  use InputOutputModule, only: getunit, urword, openfile
  use ArrayHandlersModule, only: expandarray, ifind
  use BaseModelModule, only: BaseModelType
  use BaseSolutionModule, only: BaseSolutionType, AddBaseSolutionToList, &
                                GetBaseSolutionFromList
  use SolutionGroupModule, only: SolutionGroupType, AddSolutionGroupToList
  use BaseExchangeModule, only: BaseExchangeType, GetBaseExchangeFromList
  use DistributedModelModule, only: add_dist_model
  use ListsModule, only: basesolutionlist, basemodellist, &
                         solutiongrouplist, baseexchangelist
  use BaseModelModule, only: GetBaseModelFromList
  use ListModule, only: ListType

  implicit none
  private
  public :: simulation_cr
  public :: simulation_da

  character(len=LENMODELNAME), allocatable, dimension(:) :: modelname

contains

  !> @brief Source the simulation nam input context and initialize the models, exchanges
  !<
  subroutine simulation_cr()
    ! -- modules
    ! -- local
! ------------------------------------------------------------------------------
    !
    ! -- Source simulation nam input context and create objects
    call source_simulation_nam()
    !
    ! -- Return
    return
  end subroutine simulation_cr

  !> @brief Deallocate simulation variables
  !<
  subroutine simulation_da()
    ! -- modules
    use MemoryManagerExtModule, only: memorylist_remove
    use SimVariablesModule, only: idm_context
    ! -- local
! ------------------------------------------------------------------------------
    !
    ! -- Deallocate input memory
    call memorylist_remove('SIM', 'NAM', idm_context)
    !
    ! -- variables
    deallocate (modelname)
    !
    ! -- Return
    return
  end subroutine simulation_da

  !> @brief Read the simulation name file
  !!
  !! Source the simulation nam input context and initialize the models,
  !! exchanges, solutions, solutions groups.  Then add the exchanges to
  !! the appropriate olutions.
  !!
  !<
  subroutine source_simulation_nam()
    ! -- dummy
    ! -- local
! ------------------------------------------------------------------------------
    !
    ! -- Process OPTIONS block from input context
    call options_create()
    !
    ! -- Process TIMING block from input context
    call timing_create()
    !
    ! -- Process MODELS block from input context
    call models_create()
    !
    ! -- Process EXCHANGES block from input context
    call exchanges_create()
    !
    ! -- Process SOLUTION_GROUPS blocks from input context
    call solution_groups_create()
    !
    ! -- Go through each model and make sure that it has been assigned to
    !    a solution.
    call check_model_assignment()
    !
    ! -- Go through each solution and assign exchanges accordingly
    call assign_exchanges()
    !
    ! -- Return
    return
  end subroutine source_simulation_nam

  !> @brief Set the simulation options
  !<
  subroutine options_create()
    ! -- modules
    use MemoryHelperModule, only: create_mem_path
    use MemoryManagerModule, only: mem_setptr
    use SimVariablesModule, only: idm_context
    use MemoryManagerModule, only: mem_set_print_option
    use SimVariablesModule, only: isimcontinue, isimcheck
    ! -- dummy
    ! -- locals
    character(len=LENMEMPATH) :: idmMemoryPath
    integer(I4B), pointer :: simcontinue, nocheck, maxerror
    character(len=:), pointer :: prmem
    character(len=LINELENGTH) :: errmsg
    !
    ! -- set input memory path
    idmMemoryPath = create_mem_path('SIM', 'NAM', idm_context)
    !
    ! -- set pointers to input context option params
    call mem_setptr(simcontinue, 'CONTINUE', idmMemoryPath)
    call mem_setptr(nocheck, 'NOCHECK', idmMemoryPath)
    call mem_setptr(maxerror, 'MAXERRORS', idmMemoryPath)
    call mem_setptr(prmem, 'PRMEM', idmMemoryPath)
    !
    ! -- update sim options
    isimcontinue = simcontinue
    isimcheck = nocheck
    call MaxErrors(maxerror)
    if (prmem /= '') then
      errmsg = ''
      call mem_set_print_option(iout, prmem, errmsg)
      if (errmsg /= '') then
        call store_error(errmsg, .true.)
      end if
    end if
    !
    ! -- log values to list file
    if (iout > 0) then
      write (iout, '(/1x,a)') 'READING SIMULATION INPUT OPTIONS'
      !
      if (isimcontinue == 1) then
        write (iout, '(4x, a)') &
          'SIMULATION WILL CONTINUE EVEN IF THERE IS NONCONVERGENCE.'
      end if
      !
      if (isimcheck == 0) then
        write (iout, '(4x, a)') &
          'MODEL DATA WILL NOT BE CHECKED FOR ERRORS.'
      end if
      !
      write (iout, '(4x, a, i0)') &
        'MAXIMUM NUMBER OF ERRORS THAT WILL BE STORED IS ', maxerror
      !
      if (prmem /= '') then
        write (iout, '(4x, a, a, a)') &
          'MEMORY_PRINT_OPTION SET TO "', trim(prmem), '".'
      end if
      !
      write (iout, '(1x,a)') 'END OF SIMULATION INPUT OPTIONS'
    end if
    !
    ! -- return
    return
  end subroutine options_create

  !> @brief Set the timing module to be used for the simulation
  !<
  subroutine timing_create()
    ! -- modules
    use MemoryHelperModule, only: create_mem_path
    use MemoryManagerModule, only: mem_setptr
    use SimVariablesModule, only: idm_context
    use TdisModule, only: tdis_cr
    ! -- dummy
    ! -- locals
    character(len=LENMEMPATH) :: idmMemoryPath
    character(len=:), pointer :: tdis6
    logical :: terminate = .true.
    !
    ! -- set input memory path
    idmMemoryPath = create_mem_path('SIM', 'NAM', idm_context)
    !
    ! -- set pointers to input context timing params
    call mem_setptr(tdis6, 'TDIS6', idmMemoryPath)
    !
    ! -- create timing
    if (tdis6 /= '') then
      call tdis_cr(tdis6)
    else
      call store_error('****ERROR. TIMING block input variable TDIS6 is unset.', &
                       terminate)
    end if
    !
    ! -- return
    return
  end subroutine timing_create

  !> @brief Set the models to be used for the simulation
  !<
  subroutine models_create()
    ! -- modules
    use MemoryHelperModule, only: create_mem_path
    use MemoryManagerModule, only: mem_setptr
    use CharacterStringModule, only: CharacterStringType
    use SimVariablesModule, only: idm_context
    use GwfModule, only: gwf_cr
    use GwtModule, only: gwt_cr
    ! -- dummy
    ! -- locals
    character(len=LENMEMPATH) :: idmMemoryPath
    type(CharacterStringType), dimension(:), pointer, contiguous :: mtypes !< model types
    type(CharacterStringType), dimension(:), pointer, contiguous :: mfnames !< model file names
    type(CharacterStringType), dimension(:), pointer, contiguous :: mnames !< model names
    character(len=LINELENGTH) :: mtype, mfname
    character(len=LENMODELNAME) :: mname
    character(len=LINELENGTH) :: errmsg
    integer(I4B) :: n
    logical :: terminate = .true.
    !
    ! -- set input memory path
    idmMemoryPath = create_mem_path('SIM', 'NAM', idm_context)
    !
    ! -- set pointers to input context model attribute arrays
    call mem_setptr(mtypes, 'MTYPE', idmMemoryPath)
    call mem_setptr(mfnames, 'MFNAME', idmMemoryPath)
    call mem_setptr(mnames, 'MNAME', idmMemoryPath)
    !
    ! -- open model logging block
    write (iout, '(/1x,a)') 'READING MODEL SIMULATION INPUT'
    !
    ! -- create models
    do n = 1, size(mtypes)
      !
      ! -- attributes for this model
      mtype = mtypes(n)
      mfname = mfnames(n)
      mname = mnames(n)
      !
      ! -- create appropriate models and update modelname
      select case (mtype)
      case ('GWF6')
        call add_model(n, 'GWF6', mname)
        call gwf_cr(mfname, n, mname)
      case ('GWT6')
        call add_model(n, 'GWT6', mname)
        call gwt_cr(mfname, n, mname)
      case default
        write (errmsg, '(4x,a,a)') &
          '****ERROR. UNKNOWN SIMULATION MODEL: ', &
          trim(mtype)
        call store_error(errmsg, terminate)
      end select
      !
      ! -- add distributed model
      call add_dist_model(n)
    end do
    !
    ! -- close model logging block
    write (iout, '(4x,a,i0)') 'NUMBER OF MODELS CREATED: ', size(modelname)
    write (iout, '(1x,a)') 'END OF MODEL SIMULATION INPUT'
    !
    ! -- return
    return
  end subroutine models_create

  !> @brief Set the exchanges to be used for the simulation
  !<
  subroutine exchanges_create()
    ! -- modules
    use MemoryHelperModule, only: create_mem_path
    use MemoryManagerModule, only: mem_setptr
    use CharacterStringModule, only: CharacterStringType
    use SimVariablesModule, only: idm_context
    use GwfGwfExchangeModule, only: gwfexchange_create
    use GwfGwtExchangeModule, only: gwfgwt_cr
    use GwtGwtExchangeModule, only: gwtexchange_create
    ! -- dummy
    ! -- locals
    character(len=LENMEMPATH) :: idmMemoryPath
    type(CharacterStringType), dimension(:), pointer, contiguous :: etypes !< exg types
    type(CharacterStringType), dimension(:), pointer, contiguous :: efiles !< exg file names
    type(CharacterStringType), dimension(:), pointer, contiguous :: emnames_a !< model a names
    type(CharacterStringType), dimension(:), pointer, contiguous :: emnames_b !< model b names
    character(len=LINELENGTH) :: exgtype, exgfile
    character(len=LENMODELNAME) :: mname_a, mname_b
    character(len=LINELENGTH) :: errmsg
    integer(I4B) :: m1, m2, n
    logical :: terminate = .true.
    ! -- formats
    character(len=*), parameter :: fmtmerr = "('Error in simulation control ', &
      &'file.  Could not find model: ', a)"
    !
    ! -- set input memory path
    idmMemoryPath = create_mem_path('SIM', 'NAM', idm_context)
    !
    ! -- set pointers to input context exchange attribute arrays
    call mem_setptr(etypes, 'EXGTYPE', idmMemoryPath)
    call mem_setptr(efiles, 'EXGFILE', idmMemoryPath)
    call mem_setptr(emnames_a, 'EXGMNAMEA', idmMemoryPath)
    call mem_setptr(emnames_b, 'EXGMNAMEB', idmMemoryPath)
    !
    ! -- open exchange logging block
    write (iout, '(/1x,a)') 'READING EXCHANGE SIMULATION INPUT'
    !
    ! -- create exchanges
    do n = 1, size(etypes)
      !
      ! -- attributes for this exchange
      exgtype = etypes(n)
      exgfile = efiles(n)
      mname_a = emnames_a(n)
      mname_b = emnames_b(n)
      !
      ! -- set model 1 and 2 id's
      m1 = ifind(modelname, mname_a)
      if (m1 < 0) then
        write (errmsg, fmtmerr) trim(mname_a)
        call store_error(errmsg, terminate)
      end if
      !
      m2 = ifind(modelname, mname_b)
      if (m2 < 0) then
        write (errmsg, fmtmerr) trim(mname_b)
        call store_error(errmsg, terminate)
      end if
      !
      ! -- log exchange
      write (iout, '(4x,a,a,i0,a,i0,a,i0)') trim(exgtype), ' exchange ', &
        n, ' will be created to connect model ', m1, ' with model ', m2
      !
      ! -- create exchanges
      select case (exgtype)
      case ('GWF6-GWF6')
        call gwfexchange_create(exgfile, n, m1, m2)
      case ('GWF6-GWT6')
        call gwfgwt_cr(exgfile, n, m1, m2)
      case ('GWT6-GWT6')
        call gwtexchange_create(exgfile, n, m1, m2)
      case default
        write (errmsg, '(4x,a,a)') &
          '****ERROR. UNKNOWN SIMULATION EXCHANGES: ', &
          trim(exgtype)
        call store_error(errmsg, terminate)
      end select
    end do
    !
    ! -- close exchange logging block
    write (iout, '(4x,a,i0)') 'NUMBER OF EXCHANGES CREATED: ', n - 1
    write (iout, '(1x,a)') 'END OF EXCHANGE SIMULATION INPUT'
    !
    ! -- return
    return
  end subroutine exchanges_create

  !> @brief Set the solution_groups to be used for the simulation
  !<
  subroutine solution_groups_create()
    ! -- modules
    use MemoryManagerModule, only: mem_setptr
    use CharacterStringModule, only: CharacterStringType
    use MemoryHelperModule, only: create_mem_path
    use SimVariablesModule, only: idm_context
    use SolutionGroupModule, only: SolutionGroupType, &
                                   solutiongroup_create
    use BaseSolutionModule, only: BaseSolutionType
    use BaseModelModule, only: BaseModelType
    use BaseExchangeModule, only: BaseExchangeType
    use NumericalSolutionModule, only: solution_create
    use InputOutputModule, only: parseline, upcase
    ! -- dummy
    ! -- local
    character(len=LENMEMPATH) :: idmMemoryPath
    character(len=LINELENGTH) :: errmsg
    type(CharacterStringType), dimension(:), contiguous, pointer :: slntype
    type(CharacterStringType), dimension(:), contiguous, pointer :: slnfname
    type(CharacterStringType), dimension(:), contiguous, pointer :: slnmnames
    integer(I4B), dimension(:), contiguous, pointer :: blocknum
    character(len=LINELENGTH) :: stype, fname, mnames
    type(SolutionGroupType), pointer :: sgp
    class(BaseSolutionType), pointer :: sp
    class(BaseModelType), pointer :: mp
    integer(I4B) :: isoln
    integer(I4B) :: isgpsoln
    integer(I4B) :: sgid
    integer(I4B) :: mid
    integer(I4B) :: i, j, istat, mxiter
    integer(I4B) :: nwords
    character(len=LENMODELNAME), dimension(:), allocatable :: words
    character(len=:), allocatable :: parse_str
    logical :: terminate = .true.
    ! -- formats
    character(len=*), parameter :: fmterrmxiter = &
      "('ERROR. MXITER IS SET TO ', i0, ' BUT THERE IS ONLY ONE SOLUTION', &
      &' IN SOLUTION GROUP ', i0, '. SET MXITER TO 1 IN SIMULATION CONTROL', &
      &' FILE.')"
! ------------------------------------------------------------------------------
    !
    ! -- set memory path
    idmMemoryPath = create_mem_path('SIM', 'NAM', idm_context)
    !
    ! -- set pointers to input context solution attribute arrays
    call mem_setptr(slntype, 'SLNTYPE', idmMemoryPath)
    call mem_setptr(slnfname, 'SLNFNAME', idmMemoryPath)
    call mem_setptr(slnmnames, 'SLNMNAMES', idmMemoryPath)
    call mem_setptr(blocknum, 'SOLUTIONGROUP#', idmMemoryPath)
    !
    ! -- open solution group logging block
    write (iout, '(/1x,a)') 'READING SOLUTIONGROUP SIMULATION INPUT'
    !
    ! -- initialize
    sgid = 0 ! integer id of soln group, tracks with blocknum
    isoln = 0 ! cumulative solution number
    !
    ! -- create solution groups
    do i = 1, size(blocknum)
      !
      ! -- attributes for this solution
      stype = slntype(i)
      fname = slnfname(i)
      mnames = slnmnames(i)

      if (blocknum(i) /= sgid) then
        !
        ! -- check for new soln group
        if (blocknum(i) == sgid + 1) then
          !
          ! -- error check completed group
          if (sgid > 0) then
            !
            ! -- Make sure there is a solution in this solution group
            if (isgpsoln == 0) then
              write (errmsg, '(4x,a,i0)') &
                'ERROR. THERE ARE NO SOLUTIONS FOR SOLUTION GROUP ', sgid
              call store_error(errmsg, terminate)
            end if
            !
            ! -- If there is only one solution then mxiter should be 1.
            if (isgpsoln == 1 .and. sgp%mxiter > 1) then
              write (errmsg, fmterrmxiter) sgp%mxiter, isgpsoln
              call store_error(errmsg, terminate)
            end if
          end if
          !
          ! -- reinitialize
          nullify (sgp)
          isgpsoln = 0 ! solution counter for this solution group
          !
          ! -- set sgid
          sgid = blocknum(i)
          !
          ! -- create new soln group and add to global list
          call solutiongroup_create(sgp, sgid)
          call AddSolutionGroupToList(solutiongrouplist, sgp)
        else
          write (errmsg, '(a,i0,a,i0,a)') &
            'Solution group blocks are not listed consecutively. Found ', &
            blocknum(i), ' when looking for ', sgid + 1, '.'
          call store_error(errmsg, terminate)
        end if
      end if
      !
      ! --
      select case (stype)
        !
      case ('MXITER')
        read (fname, *, iostat=istat) mxiter
        if (istat == 0) then
          sgp%mxiter = mxiter
        end if
      case ('IMS6')
        !
        ! -- increment solution counters
        isoln = isoln + 1
        isgpsoln = isgpsoln + 1
        !
        ! -- create soln and add to group
        call solution_create(fname, isoln)
        sp => GetBaseSolutionFromList(basesolutionlist, isoln)
        call sgp%add_solution(isoln, sp)
        !
        ! -- parse model names
        parse_str = trim(mnames)//' '
        call parseline(parse_str, nwords, words)
        !
        ! -- Find each model id and get model
        do j = 1, nwords
          call upcase(words(j))
          mid = ifind(modelname, words(j))
          if (mid <= 0) then
            write (errmsg, '(a,a)') 'Error.  Unrecognized modelname: ', &
              trim(words(j))
            call store_error(errmsg, terminate)
          end if
          mp => GetBaseModelFromList(basemodellist, mid)
          !
          ! -- Add the model to the solution
          call sp%add_model(mp)
          mp%idsoln = isoln
        end do
      case default
      end select
    end do
    !
    ! -- close exchange logging block
    write (iout, '(4x,a,i0)') 'NUMBER OF SOLUTIONGROUPS CREATED: ', isoln
    write (iout, '(1x,a)') 'END OF SOLUTIONGROUP SIMULATION INPUT'
    !
    ! -- Check and make sure at least one solution group was found
    if (solutiongrouplist%Count() == 0) then
      call store_error('ERROR.  THERE ARE NO SOLUTION GROUPS.', terminate)
    end if
    !
    ! -- return
    return
  end subroutine solution_groups_create

  !> @brief Check for dangling models, and break with
  !! error when found
  !<
  subroutine check_model_assignment()
    character(len=LINELENGTH) :: errmsg
    class(BaseModelType), pointer :: mp
    integer(I4B) :: im

    do im = 1, basemodellist%Count()
      mp => GetBaseModelFromList(basemodellist, im)
      if (mp%idsoln == 0) then
        write (errmsg, '(a,a)') &
          '****ERROR.  Model was not assigned to a solution: ', mp%name
        call store_error(errmsg, .true.)
      end if
    end do
  end subroutine check_model_assignment

  !> @brief Assign exchanges to solutions
  !!
  !! This assigns NumericalExchanges to NumericalSolutions,
  !! based on the link between the models in the solution and
  !! those exchanges. The BaseExchange%connects_model() function
  !! should be overridden to indicate if such a link exists.
  !<
  subroutine assign_exchanges()
    ! -- local
    class(BaseSolutionType), pointer :: sp
    class(BaseExchangeType), pointer :: ep
    class(BaseModelType), pointer :: mp
    type(ListType), pointer :: models_in_solution
    integer(I4B) :: is, ie, im

    do is = 1, basesolutionlist%Count()
      sp => GetBaseSolutionFromList(basesolutionlist, is)
      !
      ! -- now loop over exchanges
      do ie = 1, baseexchangelist%Count()
        ep => GetBaseExchangeFromList(baseexchangelist, ie)
        !
        ! -- and add when it affects (any model in) the solution matrix
        models_in_solution => sp%get_models()
        do im = 1, models_in_solution%Count()
          mp => GetBaseModelFromList(models_in_solution, im)
          if (ep%connects_model(mp)) then
            !
            ! -- add to solution (and only once)
            call sp%add_exchange(ep)
            exit
          end if
        end do
      end do
    end do
  end subroutine assign_exchanges

  !> @brief Add the model to the list of modelnames, check that the model name is valid
  !<
  subroutine add_model(im, mtype, mname)
    ! -- dummy
    integer, intent(in) :: im
    character(len=*), intent(in) :: mtype
    character(len=*), intent(in) :: mname
    ! -- local
    integer :: ilen
    integer :: i
    character(len=LINELENGTH) :: errmsg
    logical :: terminate = .true.
    ! ------------------------------------------------------------------------------
    call expandarray(modelname)
    ilen = len_trim(mname)
    if (ilen > LENMODELNAME) then
      write (errmsg, '(4x,a,a)') &
        'ERROR. INVALID MODEL NAME: ', trim(mname)
      call store_error(errmsg)
      write (errmsg, '(4x,a,i0,a,i0)') &
        'NAME LENGTH OF ', ilen, ' EXCEEDS MAXIMUM LENGTH OF ', &
        LENMODELNAME
      call store_error(errmsg, terminate)
    end if
    do i = 1, ilen
      if (mname(i:i) == ' ') then
        write (errmsg, '(4x,a,a)') &
          'ERROR. INVALID MODEL NAME: ', trim(mname)
        call store_error(errmsg)
        write (errmsg, '(4x,a)') &
          'MODEL NAME CANNOT HAVE SPACES WITHIN IT.'
        call store_error(errmsg, terminate)
      end if
    end do
    modelname(im) = mname
    write (iout, '(4x,a,i0)') mtype//' model '//trim(mname)// &
      ' will be created as model ', im
    !
    ! -- return
    return
  end subroutine add_model
end module SimulationCreateModule
