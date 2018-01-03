module SimulationCreateModule

  use KindModule,             only: DP, I4B
  use ConstantsModule,        only: LINELENGTH, LENMODELNAME, LENBIGLINE, DZERO
  use SimVariablesModule,     only: iout
  use SimModule,              only: ustop, store_error, count_errors,          &
                                    store_error_unit
  use InputOutputModule,      only: getunit, urword, openfile
  use ArrayHandlersModule,    only: expandarray, ifind
  use BaseModelModule,        only: BaseModelType
  use BaseSolutionModule,     only: BaseSolutionType, AddBaseSolutionToList,   &
                                    GetBaseSolutionFromList
  use SolutionGroupModule,    only: SolutionGroupType, AddSolutionGroupToList
  use BaseExchangeModule,     only: BaseExchangeType
  use ListsModule,            only: basesolutionlist, basemodellist,           &
                                    solutiongrouplist
  use BaseModelModule,        only: GetBaseModelFromList
  use BlockParserModule,      only: BlockParserType

  implicit none
  private
  public :: simulation_cr
  public :: simulation_da

  integer(I4B) :: inunit = 0
  character(len=LENMODELNAME), allocatable, dimension(:) :: modelname
  type(BlockParserType) :: parser

  contains

  subroutine simulation_cr()
! ******************************************************************************
! Read the simulation name file and initialize the models, exchanges
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- local
    character(len=LINELENGTH) :: simfile
    character(len=LINELENGTH) :: simlstfile
! ------------------------------------------------------------------------------
    !
    ! -- set default simfile and simlstfile
    simfile    = 'mfsim.nam'
    simlstfile = 'mfsim.lst'
    iout = 0
    !
    ! -- Open simulation list file
    iout = getunit()
    call openfile(iout, 0, simlstfile, 'LIST', filstat_opt='REPLACE')
    write(*,'(A,A)') ' Writing simulation list file: ', &
                     trim(adjustl(simlstfile))
    call write_simulation_header()
    !
    ! -- Read the simulation name file and create objects
    call read_simulation_namefile(trim(adjustl(simfile)))
    !
    ! -- Return
    return
  end subroutine simulation_cr

  subroutine simulation_da()
! ******************************************************************************
! Deallocate simulation variables
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- local
! ------------------------------------------------------------------------------
    !
    ! -- variables
    deallocate(modelname)
    !
    ! -- Return
    return
  end subroutine simulation_da

  subroutine write_simulation_header()
! ******************************************************************************
! Write header information for the simulation
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use ConstantsModule,        only: MFVNAM, VERSION, MFTITLE, FMTDISCLAIMER, &
                                      LENBIGLINE, IDEVELOPMODE
    use CompilerVersion
    use InputOutputModule,      only: write_centered
    ! -- dummy
    ! -- local
    character(len=LENBIGLINE) :: syscmd
    character(len=80) :: compiler
! ------------------------------------------------------------------------------
    !
    ! -- Write header lines to simulation list file.
    call write_centered('MODFLOW'//MFVNAM, iout, 80)
    call write_centered(MFTITLE, iout, 80)
    call write_centered('VERSION '//VERSION, iout, 80)
    !
    ! -- Write if develop mode
    if (IDEVELOPMODE == 1) call write_centered('***DEVELOP MODE***', iout, 80)
    !
    ! -- Write compiler version
    call get_compiler(compiler)
    call write_centered(' ', iout, 80)
    call write_centered(trim(adjustl(compiler)), iout, 80)
    !
    ! -- Write disclaimer
    write(iout, FMTDISCLAIMER)
    !
    ! -- Write the system command used to initiate simulation
    call GET_COMMAND(syscmd)
    write(iout, '(/,a,/,a)') 'System command used to initiate simulation:',    &
                             trim(syscmd)
    !
    ! -- Write precision of real variables
    write(iout, '(/,a)') 'MODFLOW was compiled using uniform precision.'
    write(iout, '(a,i0)') 'Precision of REAL variables: ', precision(DZERO)
    write(iout, '(a,i0)') 'Fortran KIND value for REAL variables: ', DP
    write(iout, '(a,i0,/)') 'Fortran KIND value for INTEGER variables: ', I4B
    !
    ! -- Return
    return
  end subroutine write_simulation_header

  subroutine read_simulation_namefile(simfile)
! ******************************************************************************
! Read the simulation name file and initialize the models, exchanges,
! solutions, solutions groups.  Then add the exchanges to the appropriate
! solutions.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    character(len=*),intent(in) :: simfile
    ! -- local
    character(len=LINELENGTH) :: errmsg
    class(BaseSolutionType), pointer :: sp
    class(BaseModelType), pointer :: mp
    integer(I4B) :: is, im
! ------------------------------------------------------------------------------
    !
    ! -- Open simulation name file
    inunit = getunit()
    call openfile(inunit, iout, simfile, 'NAM')
    write(*,'(A,A)') ' Using Simulation name file: ', simfile
    !
    ! -- Initialize block parser
    call parser%Initialize(inunit, iout)
    !
    ! -- Process OPTIONS block in simfile
    call options_create()
    !
    ! -- Process TIMING block in simfile
    call timing_create()
    !
    ! -- Process MODELS block in simfile
    call models_create()
    !
    ! -- Process EXCHANGES block in simfile
    call exchanges_create()
    !
    ! -- Process SOLUTION_GROUPS blocks in simfile
    call solution_groups_create()
    !
    ! -- Go through each model and make sure that it has been assigned to
    !    a solution.
    do im = 1, basemodellist%Count()
      mp => GetBaseModelFromList(basemodellist, im)
      if (mp%idsoln == 0) then
        write(errmsg, '(a,a)') &
           '****ERROR.  Model was not assigned to a solution: ', mp%name
        call store_error(errmsg)
      endif
    enddo
    if (count_errors() > 0) then
      call store_error_unit(inunit)
      call ustop()
    endif
    !
    ! -- Close the input file
    close(inunit)
    !
    ! -- Go through each solution and assign exchanges accordingly
    do is = 1, basesolutionlist%Count()
      sp => GetBaseSolutionFromList(basesolutionlist, is)
      call sp%slnassignexchanges()
    enddo
    !
    ! -- Return
    return
  end subroutine read_simulation_namefile

  subroutine options_create()
! ******************************************************************************
! Set the simulation options
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryManagerModule, only: mem_set_print_option
    use SimVariablesModule, only: isimcontinue, isimcheck
    ! -- local
    integer(I4B) :: ierr
    logical :: isfound, endOfBlock
    character(len=LINELENGTH) :: errmsg
    character(len=LINELENGTH) :: keyword
! ------------------------------------------------------------------------------
    !
    ! -- Process OPTIONS block
    call parser%GetBlock('OPTIONS', isfound, ierr, blockRequired=.false.)
    if (isfound) then
      write(iout,'(/1x,a)')'READING SIMULATION OPTIONS'
      do
        call parser%GetNextLine(endOfBlock)
        if (endOfBlock) exit
        call parser%GetStringCaps(keyword)
        select case (keyword)
          case ('CONTINUE')
            isimcontinue = 1
            write(iout, '(4x, a)')                                             &
                  'SIMULATION WILL CONTINUE EVEN IF THERE IS NONCONVERGENCE.'
          case ('NOCHECK')
            isimcheck = 0
            write(iout, '(4x, a)')                                             &
                  'MODEL DATA WILL NOT BE CHECKED FOR ERRORS.'
          case ('MEMORY_PRINT_OPTION')
            errmsg = ''
            call parser%GetStringCaps(keyword)
            call mem_set_print_option(iout, keyword, errmsg)
            if (errmsg /= ' ') then
              call store_error(errmsg)
              call parser%StoreErrorUnit()
              call ustop()
            endif
          case default
            write(errmsg, '(4x,a,a)') &
                  '****ERROR. UNKNOWN SIMULATION OPTION: ',                    &
                  trim(keyword)
            call store_error(errmsg)
            call parser%StoreErrorUnit()
            call ustop()
        end select
      end do
      write(iout,'(1x,a)')'END OF SIMULATION OPTIONS'
    end if
    !
    ! -- return
    return
  end subroutine options_create

  subroutine timing_create()
! ******************************************************************************
! Set the timing module to be used for the simulation
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use TdisModule, only: tdis_cr
    ! -- dummy
    ! -- local
    integer(I4B) :: ierr
    logical :: isfound, endOfBlock
    character(len=LINELENGTH) :: errmsg
    character(len=LINELENGTH) :: line, keyword
    logical :: found_tdis
! ------------------------------------------------------------------------------
    !
    ! -- Initialize
    found_tdis = .false.
    !
    ! -- Process TIMING block
    call parser%GetBlock('TIMING', isfound, ierr)
    if (isfound) then
      write(iout,'(/1x,a)')'READING SIMULATION TIMING'
      do
        call parser%GetNextLine(endOfBlock)
        if (endOfBlock) exit
        call parser%GetStringCaps(keyword)
        select case (keyword)
          case ('TDIS6')
            found_tdis = .true.
            call parser%GetString(line)
            call tdis_cr(line)
          case default
            write(errmsg, '(4x,a,a)') &
                  '****ERROR. UNKNOWN SIMULATION TIMING: ', &
                  trim(keyword)
            call store_error(errmsg)
            call parser%StoreErrorUnit()
            call ustop()
        end select
      end do
      write(iout,'(1x,a)')'END OF SIMULATION TIMING'
    else
      call store_error('****ERROR.  Did not find TIMING block in simulation'// &
                       ' control file.')
      call parser%StoreErrorUnit()
      call ustop()
    end if
    !
    ! -- Ensure that TDIS was found
    if(.not. found_tdis) then
      call store_error('****ERROR. TDIS not found in TIMING block.')
      call parser%StoreErrorUnit()
      call ustop()
    endif
    !
    ! -- return
    return
  end subroutine timing_create

  subroutine models_create()
! ******************************************************************************
! Set the models to be used for the simulation
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use GwfModule,              only: gwf_cr
    use ConstantsModule,        only: LENMODELNAME
    ! -- dummy
    ! -- local
    integer(I4B) :: ierr
    logical :: isfound, endOfBlock
    integer(I4B) :: im
    character(len=LINELENGTH) :: errmsg
    character(len=LINELENGTH) :: keyword
    character(len=LINELENGTH) :: fname, mname
! ------------------------------------------------------------------------------
    !
    ! -- Process MODELS block
    call parser%GetBlock('MODELS', isfound, ierr)
    if (isfound) then
      write(iout,'(/1x,a)')'READING SIMULATION MODELS'
      im = 0
      do
        call parser%GetNextLine(endOfBlock)
        if (endOfBlock) exit
        call parser%GetStringCaps(keyword)
        select case (keyword)
          case ('GWF6')
            call parser%GetString(fname)
            call add_model(im, 'GWF6', mname)
            call gwf_cr(fname, im, modelname(im))
          case default
            write(errmsg, '(4x,a,a)') &
                  '****ERROR. UNKNOWN SIMULATION MODEL: ',                     &
                  trim(keyword)
            call store_error(errmsg)
            call parser%StoreErrorUnit()
            call ustop()
        end select
      end do
      write(iout,'(1x,a)')'END OF SIMULATION MODELS'
    else
      call store_error('****ERROR.  Did not find MODELS block in simulation'// &
                       ' control file.')
      call parser%StoreErrorUnit()
      call ustop()
    end if
    !
    ! -- return
    return
  end subroutine models_create

  subroutine exchanges_create()
! ******************************************************************************
! Set the exchanges to be used for the simulation
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use GwfGwfExchangeModule,    only: gwfexchange_create
    ! -- dummy
    ! -- local
    integer(I4B) :: ierr
    logical :: isfound, endOfBlock
    integer(I4B) :: id
    integer(I4B) :: m1
    integer(I4B) :: m2
    character(len=LINELENGTH) :: errmsg
    character(len=LINELENGTH) :: keyword
    character(len=LINELENGTH) :: fname, name1, name2
    ! -- formats
    character(len=*), parameter :: fmtmerr = "('Error in simulation control ', &
      &'file.  Could not find model: ', a)"
! ------------------------------------------------------------------------------
    call parser%GetBlock('EXCHANGES', isfound, ierr)
    if (isfound) then
      write(iout,'(/1x,a)')'READING SIMULATION EXCHANGES'
      id = 0
      do
        call parser%GetNextLine(endOfBlock)
        if (endOfBlock) exit
        call parser%GetStringCaps(keyword)
        select case (keyword)
          case ('GWF6-GWF6')
            id = id + 1
            !
            ! -- get filename
            call parser%GetString(fname)
            !
            ! -- get first modelname and then model id
            call parser%GetStringCaps(name1)
            m1 = ifind(modelname, name1)
            if(m1 < 0) then
              write(errmsg, fmtmerr) trim(name1)
              call store_error(errmsg)
              call parser%StoreErrorUnit()
              call ustop()
            endif
            !
            ! -- get second modelname and then model id
            call parser%GetStringCaps(name2)
            m2 = ifind(modelname, name2)
            if(m2 < 0) then
              write(errmsg, fmtmerr) trim(name2)
              call store_error(errmsg)
              call parser%StoreErrorUnit()
              call ustop()
            endif
            !
            ! -- Create the exchange object.
            write(iout, '(4x,a,i0,a,i0,a,i0)') 'GWF6-GWF6 exchange ', id,      &
              ' will be created to connect model ', m1, ' with model ', m2
            call gwfexchange_create(fname, id, m1, m2)
          case default
            write(errmsg, '(4x,a,a)') &
                  '****ERROR. UNKNOWN SIMULATION EXCHANGES: ',                 &
                  trim(keyword)
            call store_error(errmsg)
            call parser%StoreErrorUnit()
            call ustop()
        end select
      end do
      write(iout,'(1x,a)')'END OF SIMULATION EXCHANGES'
    else
      call store_error('****ERROR.  Did not find EXCHANGES block in '//        &
                       'simulation control file.')
      call parser%StoreErrorUnit()
      call ustop()
    end if
    !
    ! -- return
    return
  end subroutine exchanges_create

  subroutine solution_groups_create()
! ******************************************************************************
! Set the solution_groups to be used for the simulation
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use SolutionGroupModule,        only: SolutionGroupType,                   &
                                          solutiongroup_create
    use BaseSolutionModule,         only: BaseSolutionType
    use BaseModelModule,            only: BaseModelType
    use BaseExchangeModule,         only: BaseExchangeType
    use NumericalSolutionModule,    only: solution_create
    ! -- dummy
    ! -- local
    type(SolutionGroupType), pointer  :: sgp
    class(BaseSolutionType), pointer  :: sp
    class(BaseModelType), pointer     :: mp
    integer(I4B) :: ierr
    logical :: isfound, endOfBlock
    integer(I4B) :: isoln
    integer(I4B) :: isgp
    integer(I4B) :: isgpsoln
    integer(I4B) :: sgid
    integer(I4B) :: mid
    character(len=LINELENGTH) :: errmsg
    character(len=LENBIGLINE) :: keyword
    character(len=LINELENGTH) :: fname, mname
    ! -- formats
    character(len=*), parameter :: fmterrmxiter = &
      "('ERROR. MXITER IS SET TO ', i0, ' BUT THERE IS ONLY ONE SOLUTION',     &
        &' IN SOLUTION GROUP ', i0, '. SET MXITER TO 1 IN SIMULATION CONTROL', &
        &' FILE.')"
! ------------------------------------------------------------------------------
    !
    ! -- isoln is the cumulative solution number, isgp is the cumulative
    !    solution group number.
    isoln = 0
    isgp = 0
    !
    !Read through the simulation name file and process each SOLUTION_GROUP
    sgploop: do
      !
      call parser%GetBlock('SOLUTIONGROUP', isfound, ierr)
      if(ierr /= 0) exit sgploop
      if (.not. isfound) exit sgploop
      isgp = isgp + 1
      !
      ! -- Get the solutiongroup id and check that it is listed consecutively.
      sgid = parser%GetInteger()
      if(isgp /= sgid) then
        write(errmsg, '(a)') 'Solution groups are not listed consecutively.'
        call store_error(errmsg)
        write(errmsg, '(a,i0,a,i0)' ) 'Found ', sgid, ' when looking for ',isgp
        call store_error(errmsg)
        call parser%StoreErrorUnit()
        call ustop()
      endif
      !
      ! -- Create the solutiongroup and add it to the solutiongrouplist
      call solutiongroup_create(sgp, sgid)
      call AddSolutionGroupToList(solutiongrouplist, sgp)
      !
      ! -- Begin processing the solution group
      write(iout,'(/1x,a)')'READING SOLUTIONGROUP'
      !
      ! -- Initialize isgpsoln to 0.  isgpsoln is the solution counter for this
      !    particular solution group.  It goes from 1 to the number of solutions
      !    in this group.
      isgpsoln = 0
      do
        call parser%GetNextLine(endOfBlock)
        if (endOfBlock) exit
        call parser%GetStringCaps(keyword)
        select case (keyword)
          !
          case ('MXITER')
            sgp%mxiter = parser%GetInteger()
          !
          case ('IMS6')
            !
            ! -- Initialize and increment counters
            isoln = isoln + 1
            isgpsoln = isgpsoln + 1
            !
            ! -- Create the solution, retrieve from the list, and add to sgp
            call parser%GetString(fname)
            call solution_create(fname, isoln)
            sp => GetBaseSolutionFromList(basesolutionlist, isoln)
            call sgp%add_solution(isoln, sp)
            !
            ! -- Add all of the models that are listed on this line to
            !    the current solution (sp)
            do
              !
              ! -- Set istart and istop to encompass model name. Exit this
              !    loop if there are no more models.
              call parser%GetStringCaps(mname)
              if (mname == '') exit
              !
              ! -- Find the model id, and then get model
              mid = ifind(modelname, mname)
              if(mid <= 0) then
                write(errmsg, '(a,a)') 'Error.  Invalid modelname: ', &
                  trim(mname)
                call store_error(errmsg)
                call parser%StoreErrorUnit()
                call ustop()
              endif
              mp => GetBaseModelFromList(basemodellist, mid)
              !
              ! -- Add the model to the solution
              call sp%addmodel(mp)
              mp%idsoln = isoln
              !
            enddo
          !
          case default
            write(errmsg, '(4x,a,a)') &
                  '****ERROR. UNKNOWN SOLUTIONGROUP ENTRY: ', &
                  trim(keyword)
            call store_error(errmsg)
            call parser%StoreErrorUnit()
            call ustop()
        end select
      end do
      !
      ! -- Make sure there is a solution in this solution group
      if(isgpsoln == 0) then
        write(errmsg, '(4x,a,i0)') &
          'ERROR. THERE ARE NO SOLUTIONS FOR SOLUTION GROUP ', isgp
        call store_error(errmsg)
        call parser%StoreErrorUnit()
        call ustop()
      endif
      !
      ! -- If there is only one solution then mxiter should be 1.
      if(isgpsoln == 1 .and. sgp%mxiter > 1) then
        write(errmsg, fmterrmxiter) sgp%mxiter, isgpsoln
        call store_error(errmsg)
        call parser%StoreErrorUnit()
        call ustop()
      endif
      !
      ! -- todo: more error checking?
      !
      write(iout,'(1x,a)')'END OF SIMULATION SOLUTIONGROUP'
      !
    enddo sgploop
    !
    ! -- Check and make sure at least one solution group was found
    if(solutiongrouplist%Count() == 0) then
      call store_error('ERROR.  THERE ARE NO SOLUTION GROUPS.')
      call parser%StoreErrorUnit()
      call ustop()
    endif
    !
    ! -- return
    return
  end subroutine solution_groups_create

  subroutine add_model(im, mtype, mname)
! ******************************************************************************
! Add the model to the list of modelnames, check that the model name is valid.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    integer, intent(inout) :: im
    character(len=*), intent(in) :: mtype
    character(len=*), intent(inout) :: mname
    ! -- local
    integer :: ilen
    integer :: i
    character(len=LINELENGTH) :: errmsg
! ------------------------------------------------------------------------------
    im = im + 1
    call expandarray(modelname)
    call parser%GetStringCaps(mname)
    ilen = len_trim(mname)
    if (ilen > LENMODELNAME) then
      write(errmsg, '(4x,a,a)')                                                &
            'ERROR. INVALID MODEL NAME: ', trim(mname)
      call store_error(errmsg)
      write(errmsg, '(4x,a,i0,a,i0)')                                          &
            'NAME LENGTH OF ', ilen, ' EXCEEDS MAXIMUM LENGTH OF ',            &
            LENMODELNAME
      call store_error(errmsg)
      call parser%StoreErrorUnit()
      call ustop()
    endif
    do i = 1, ilen
      if (mname(i:i) == ' ') then
        write(errmsg, '(4x,a,a)')                                              &
              'ERROR. INVALID MODEL NAME: ', trim(mname)
        call store_error(errmsg)
        write(errmsg, '(4x,a)')                                                &
              'MODEL NAME CANNOT HAVE SPACES WITHIN IT.'
        call store_error(errmsg)
        call parser%StoreErrorUnit()
        call ustop()
      endif
    enddo
    modelname(im) = mname
    write(iout, '(4x,a,i0)') mtype // ' model ' // trim(mname) //              &
      ' will be created as model ', im  
    !
    ! -- return
    return
  end subroutine add_model
  
end module SimulationCreateModule
