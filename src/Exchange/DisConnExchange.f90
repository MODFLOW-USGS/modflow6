module DisConnExchangeModule
  use KindModule, only: I4B, DP, LGP
  use SimVariablesModule, only: errmsg
  use ConstantsModule, only: LENAUXNAME, LENBOUNDNAME, LINELENGTH
  use ListModule, only: ListType
  use MemoryManagerModule, only: mem_allocate, mem_reallocate
  use BlockParserModule, only: BlockParserType
  use NumericalModelModule, only: NumericalModelType
  use DistributedModelModule, only: DistributedModelType
  use NumericalExchangeModule, only: NumericalExchangeType
  implicit none

  private
  public :: DisConnExchangeType
  public :: CastAsDisConnExchangeClass, AddDisConnExchangeToList, &
            GetDisConnExchangeFromList

  !> Exchange based on connection between discretizations of DisBaseType.
  !! The data specifies the connections, similar to the information stored
  !! in the connections object: DisBaseType%con
  !<
  type, extends(NumericalExchangeType) :: DisConnExchangeType
    character(len=LINELENGTH), pointer :: filename => null() !< name of the input file

    class(NumericalModelType), pointer :: model1 => null() !< model 1
    class(NumericalModelType), pointer :: model2 => null() !< model 2
    class(DistributedModelType), pointer :: dmodel1 => null() !< distributed model 1
    class(DistributedModelType), pointer :: dmodel2 => null() !< distributed model 2

    integer(I4B), pointer :: nexg => null() !< number of exchanges
    integer(I4B), dimension(:), pointer, contiguous :: nodem1 => null() !< node numbers in model 1
    integer(I4B), dimension(:), pointer, contiguous :: nodem2 => null() !< node numbers in model 2
    integer(I4B), dimension(:), pointer, contiguous :: ihc => null() !< horizontal connection indicator array, size: nexg
    real(DP), dimension(:), pointer, contiguous :: cl1 => null() !< connection length 1, size: nexg
    real(DP), dimension(:), pointer, contiguous :: cl2 => null() !< connection length 2, size: nexg
    real(DP), dimension(:), pointer, contiguous :: hwva => null() !< horizontal widths, vertical flow areas, size: nexg
    integer(I4B), pointer :: naux => null() !< number of auxiliary variables
    character(len=LENBOUNDNAME), dimension(:), &
      pointer, contiguous :: boundname => null() !< boundnames

    character(len=LENAUXNAME), dimension(:), &
      pointer, contiguous :: auxname => null() !< vector of auxname
    real(DP), dimension(:, :), pointer, contiguous :: auxvar => null() !< array of auxiliary variable values
    integer(I4B), pointer :: ianglex => null() !< flag indicating anglex was read, if read, ianglex is index in auxvar
    integer(I4B), pointer :: icdist => null() !< flag indicating cdist was read, if read, icdist is index in auxvar
    integer(I4B), pointer :: iprpak => null() !< print input flag
    integer(I4B), pointer :: inamedbound => null() !< flag to read boundnames

    integer(I4B), pointer :: ixt3d => null() !< flag indicating if XT3D should be applied on the interface: 0 = off, 1 = lhs, 2 = rhs
    logical(LGP) :: dev_ifmod_on !< development option, forces interface model for this exchange

    type(BlockParserType) :: parser !< block parser for input file (controlled from derived type)

  contains

    procedure :: allocate_scalars
    procedure :: allocate_arrays
    procedure :: disconnex_da
    procedure :: use_interface_model

    ! protected
    procedure, pass(this) :: parse_option
    procedure, pass(this) :: read_dimensions
    procedure, pass(this) :: read_data

  end type DisConnExchangeType

contains

  !> @brief Parse option from exchange file
  !<
  function parse_option(this, keyword, iout) result(parsed)
    use ArrayHandlersModule, only: ifind
    use InputOutputModule, only: urdaux
    class(DisConnExchangeType) :: this !< instance of exchange object
    character(len=LINELENGTH), intent(in) :: keyword !< the option name
    integer(I4B), intent(in) :: iout !< for logging
    logical(LGP) :: parsed !< true when parsed
    ! local
    integer(I4B) :: istart
    integer(I4B) :: istop
    integer(I4B) :: lloc
    integer(I4B) :: n
    integer(I4B) :: ival

    character(len=:), allocatable :: line
    character(len=LENAUXNAME), dimension(:), allocatable :: caux

    parsed = .true.

    select case (keyword)
    case ('AUXILIARY')
      call this%parser%GetRemainingLine(line)
      lloc = 1
      call urdaux(this%naux, this%parser%iuactive, iout, lloc, istart, &
                  istop, caux, line, 'GWF_GWF_Exchange')
      call mem_reallocate(this%auxname, LENAUXNAME, this%naux, &
                          'AUXNAME', trim(this%memoryPath))
      do n = 1, this%naux
        this%auxname(n) = caux(n)
      end do
      deallocate (caux)
      !
      ! -- If ANGLDEGX is an auxiliary variable, then anisotropy can be
      !    used in either model.  Store ANGLDEGX position in this%ianglex
      ival = ifind(this%auxname, 'ANGLDEGX')
      if (ival > 0) then
        this%ianglex = ival
      end if
      ival = ifind(this%auxname, 'CDIST')
      if (ival > 0) then
        this%icdist = ival
      end if
    case ('PRINT_INPUT')
      this%iprpak = 1
      write (iout, '(4x,a)') &
        'THE LIST OF EXCHANGES WILL BE PRINTED.'
    case ('XT3D')
      this%ixt3d = 1
      write (iout, '(4x,a)') 'XT3D WILL BE APPLIED ON THE INTERFACE'
    case ('BOUNDNAMES')
      this%inamedbound = 1
      write (iout, '(4x,a)') 'EXCHANGE BOUNDARIES HAVE NAMES IN LAST COLUMN'
    case ('DEV_INTERFACEMODEL_ON')
      call this%parser%DevOpt()
      this%dev_ifmod_on = .true.
      write (iout, '(4x,2a)') 'Interface model coupling approach manually &
        &activated for ', trim(this%name)
    case default
      ! not parsed here, assuming it is in derived type
      parsed = .false.
    end select

  end function parse_option

  !> @brief Read dimensions from file
  !<
  subroutine read_dimensions(this, iout)
    use ConstantsModule, only: LINELENGTH
    use SimModule, only: store_error
    class(DisConnExchangeType) :: this !< instance of exchange object
    integer(I4B), intent(in) :: iout !< output file unit
    ! local
    character(len=LINELENGTH) :: keyword
    integer(I4B) :: ierr
    logical :: isfound, endOfBlock

    ! get dimensions block
    call this%parser%GetBlock('DIMENSIONS', isfound, ierr, &
                              supportOpenClose=.true.)

    ! parse NEXG
    if (isfound) then
      write (iout, '(1x,a)') 'PROCESSING EXCHANGE DIMENSIONS'
      do
        call this%parser%GetNextLine(endOfBlock)
        if (endOfBlock) exit
        call this%parser%GetStringCaps(keyword)
        select case (keyword)
        case ('NEXG')
          this%nexg = this%parser%GetInteger()
          write (iout, '(4x,a,i0)') 'NEXG = ', this%nexg
        case default
          errmsg = "Unknown dimension '"//trim(keyword)//"'."
          call store_error(errmsg)
          call this%parser%StoreErrorUnit()
        end select
      end do
      write (iout, '(1x,a)') 'END OF EXCHANGE DIMENSIONS'
    else
      call store_error('Required dimensions block not found.')
      call this%parser%StoreErrorUnit()
    end if

    return
  end subroutine read_dimensions

  !> @brief Read exchange data block from file
  !<
  subroutine read_data(this, iout)
    use ConstantsModule, only: LINELENGTH
    use SimModule, only: store_error, store_error_unit, count_errors
    class(DisConnExchangeType) :: this !< instance of exchange object
    integer(I4B), intent(in) :: iout !< the output file unit
    ! local
    character(len=20) :: cellid1, cellid2
    character(len=2) :: cnfloat
    integer(I4B) :: lloc, ierr, nerr, iaux
    integer(I4B) :: iexg, nodem1, nodem2
    logical :: isfound, endOfBlock

    character(len=*), parameter :: fmtexglabel = "(5x, 3a10, 50(a16))"
    character(len=*), parameter :: fmtexgdata = &
                                   "(5x, a, 1x, a ,I10, 50(1pg16.6))"
    character(len=40) :: fmtexgdata2

    ! get data block
    call this%parser%GetBlock('EXCHANGEDATA', isfound, ierr, &
                              supportOpenClose=.true.)
    if (isfound) then
      write (iout, '(1x,a)') 'PROCESSING EXCHANGEDATA'
      if (this%iprpak /= 0) then
        if (this%inamedbound == 0) then
          write (iout, fmtexglabel) 'NODEM1', 'NODEM2', 'IHC', &
            'CL1', 'CL2', 'HWVA', (adjustr(this%auxname(iaux)), &
                                   iaux=1, this%naux)
        else
          write (iout, fmtexglabel) 'NODEM1', 'NODEM2', 'IHC', 'CL1', 'CL2', &
            'HWVA', (adjustr(this%auxname(iaux)), iaux=1, this%naux), &
            ' BOUNDNAME      '
          ! Define format suitable for writing input data,
          ! any auxiliary variables, and boundname.
          write (cnfloat, '(i0)') 3 + this%naux
          fmtexgdata2 = '(5x, a, 1x, a, i10, '//trim(cnfloat)// &
                        '(1pg16.6), 1x, a)'
        end if
      end if
      do iexg = 1, this%nexg
        call this%parser%GetNextLine(endOfBlock)
        lloc = 1
        !
        ! -- Read and check node 1
        call this%parser%GetCellid(this%model1%dis%ndim, cellid1, &
                                   flag_string=.true.)
        nodem1 = this%model1%dis%noder_from_cellid(cellid1, &
                                                   this%parser%iuactive, &
                                                   iout, flag_string=.true.)
        this%nodem1(iexg) = nodem1
        !
        ! -- Read and check node 2
        call this%parser%GetCellid(this%model2%dis%ndim, cellid2, &
                                   flag_string=.true.)
        nodem2 = this%model2%dis%noder_from_cellid(cellid2, &
                                                   this%parser%iuactive, &
                                                   iout, flag_string=.true.)
        this%nodem2(iexg) = nodem2
        !
        ! -- Read rest of input line
        this%ihc(iexg) = this%parser%GetInteger()
        this%cl1(iexg) = this%parser%GetDouble()
        this%cl2(iexg) = this%parser%GetDouble()
        this%hwva(iexg) = this%parser%GetDouble()
        do iaux = 1, this%naux
          this%auxvar(iaux, iexg) = this%parser%GetDouble()
        end do
        if (this%inamedbound == 1) then
          call this%parser%GetStringCaps(this%boundname(iexg))
        end if
        !
        ! -- Write the data to listing file if requested
        if (this%iprpak /= 0) then
          if (this%inamedbound == 0) then
            write (iout, fmtexgdata) trim(cellid1), trim(cellid2), &
              this%ihc(iexg), this%cl1(iexg), this%cl2(iexg), &
              this%hwva(iexg), &
              (this%auxvar(iaux, iexg), iaux=1, this%naux)
          else
            write (iout, fmtexgdata2) trim(cellid1), trim(cellid2), &
              this%ihc(iexg), this%cl1(iexg), this%cl2(iexg), &
              this%hwva(iexg), &
              (this%auxvar(iaux, iexg), iaux=1, this%naux), &
              trim(this%boundname(iexg))
          end if
        end if
        !
        ! -- Check to see if nodem1 is outside of active domain
        if (nodem1 <= 0) then
          write (errmsg, *) &
            trim(adjustl(this%model1%name))// &
            ' Cell is outside active grid domain ('// &
            trim(adjustl(cellid1))//').'
          call store_error(errmsg)
        end if
        !
        ! -- Check to see if nodem2 is outside of active domain
        if (nodem2 <= 0) then
          write (errmsg, *) &
            trim(adjustl(this%model2%name))// &
            ' Cell is outside active grid domain ('// &
            trim(adjustl(cellid2))//').'
          call store_error(errmsg)
        end if
      end do
      !
      ! -- Stop if errors
      nerr = count_errors()
      if (nerr > 0) then
        call store_error('Errors encountered in exchange input file.')
        call this%parser%StoreErrorUnit()
      end if
      !
      write (iout, '(1x,a)') 'END OF EXCHANGEDATA'
    else
      errmsg = 'Required exchangedata block not found.'
      call store_error(errmsg)
      call this%parser%StoreErrorUnit()
    end if
    !
    ! -- return
    return
  end subroutine read_data

  !> @brief Allocate scalars and initialize to defaults
  !<
  subroutine allocate_scalars(this)
    use MemoryManagerModule, only: mem_allocate
    class(DisConnExchangeType) :: this !< instance of exchange object

    allocate (this%filename)
    this%filename = ''

    call mem_allocate(this%nexg, 'NEXG', this%memoryPath)
    call mem_allocate(this%naux, 'NAUX', this%memoryPath)
    call mem_allocate(this%ianglex, 'IANGLEX', this%memoryPath)
    call mem_allocate(this%icdist, 'ICDIST', this%memoryPath)
    call mem_allocate(this%ixt3d, 'IXT3D', this%memoryPath)
    call mem_allocate(this%iprpak, 'IPRPAK', this%memoryPath)
    call mem_allocate(this%inamedbound, 'INAMEDBOUND', this%memoryPath)

    call mem_allocate(this%auxname, LENAUXNAME, 0, &
                      'AUXNAME', trim(this%memoryPath))

    this%nexg = 0
    this%naux = 0
    this%ianglex = 0
    this%icdist = 0
    this%ixt3d = 0
    this%inamedbound = 0

    this%dev_ifmod_on = .false.

  end subroutine allocate_scalars

  !> @brief Allocate array data, using the number of
  !! connected nodes @param nexg
  !<
  subroutine allocate_arrays(this)
    class(DisConnExchangeType) :: this !< instance of exchange object

    call mem_allocate(this%nodem1, this%nexg, 'NODEM1', this%memoryPath)
    call mem_allocate(this%nodem2, this%nexg, 'NODEM2', this%memoryPath)
    call mem_allocate(this%ihc, this%nexg, 'IHC', this%memoryPath)
    call mem_allocate(this%cl1, this%nexg, 'CL1', this%memoryPath)
    call mem_allocate(this%cl2, this%nexg, 'CL2', this%memoryPath)
    call mem_allocate(this%hwva, this%nexg, 'HWVA', this%memoryPath)
    ! NB: auxname array is allocated while parsing
    call mem_allocate(this%auxvar, this%naux, this%nexg, &
                      'AUXVAR', this%memoryPath)

    ! allocate boundname
    if (this%inamedbound == 1) then
      allocate (this%boundname(this%nexg))
    else
      allocate (this%boundname(1))
    end if
    this%boundname(:) = ''

  end subroutine allocate_arrays

  !> @brief Should interface model be used to handle these
  !! exchanges, to be overridden for inheriting types
  !<
  function use_interface_model(this) result(useIM)
    class(DisConnExchangeType) :: this !< instance of exchange object
    logical(LGP) :: useIM !< flag whether interface model should be used
                                     !! for this exchange instead

    useIM = .false.

  end function use_interface_model

  !> @brief Clean up all scalars and arrays
  !<
  subroutine disconnex_da(this)
    use MemoryManagerModule, only: mem_deallocate
    class(DisConnExchangeType) :: this !< instance of exchange object

    ! arrays
    call mem_deallocate(this%nodem1)
    call mem_deallocate(this%nodem2)
    call mem_deallocate(this%ihc)
    call mem_deallocate(this%cl1)
    call mem_deallocate(this%cl2)
    call mem_deallocate(this%hwva)
    call mem_deallocate(this%auxvar)

    deallocate (this%boundname)

    ! scalars
    call mem_deallocate(this%nexg)
    call mem_deallocate(this%naux)
    call mem_deallocate(this%auxname, 'AUXNAME', trim(this%memoryPath))
    call mem_deallocate(this%ianglex)
    call mem_deallocate(this%icdist)
    call mem_deallocate(this%ixt3d)
    call mem_deallocate(this%iprpak)
    call mem_deallocate(this%inamedbound)

  end subroutine disconnex_da

  function CastAsDisConnExchangeClass(obj) result(res)
    implicit none
    class(*), pointer, intent(inout) :: obj
    class(DisConnExchangeType), pointer :: res
    !
    res => null()
    if (.not. associated(obj)) return
    !
    select type (obj)
    class is (DisConnExchangeType)
      res => obj
    end select
    return
  end function CastAsDisConnExchangeClass

  subroutine AddDisConnExchangeToList(list, exchange)
    implicit none
    ! -- dummy
    type(ListType), intent(inout) :: list
    class(DisConnExchangeType), pointer, intent(in) :: exchange
    ! -- local
    class(*), pointer :: obj
    !
    obj => exchange
    call list%Add(obj)
    !
    return
  end subroutine AddDisConnExchangeToList

  function GetDisConnExchangeFromList(list, idx) result(res)
    implicit none
    ! -- dummy
    type(ListType), intent(inout) :: list
    integer(I4B), intent(in) :: idx
    class(DisConnExchangeType), pointer :: res
    ! -- local
    class(*), pointer :: obj
    !
    obj => list%GetItem(idx)
    res => CastAsDisConnExchangeClass(obj)
    !
    return
  end function GetDisConnExchangeFromList

end module DisConnExchangeModule
