module ArrayReaderBaseModule

  use KindModule, only: DP, I4B, LGP
  use ConstantsModule, only: MAXCHARLEN
  use BlockParserModule, only: BlockParserType
  use SimVariablesModule, only: errmsg
  use SimModule, only: store_error
  use InputOutputModule, only: openfile

  implicit none
  private
  public :: ArrayReaderBaseType

  type ArrayReaderBaseType

    type(BlockParserType), pointer :: parser => null()
    integer(I4B) :: iout = 0
    integer(I4B) :: input_unit = 0
    character(len=:), allocatable :: array_name
    character(len=:), allocatable :: filename
    integer(I4B) :: iprn = 0
    logical(LGP) :: isConstant = .false.
    logical(LGP) :: isInternal = .false.
    logical(LGP) :: isOpenClose = .false.
    logical(LGP) :: isBinary = .false.

  contains

    procedure :: read_array
    procedure :: reset_reader
    procedure :: read_control_record
    procedure :: set_constant ! must be overridden
    procedure :: fill_constant ! must be overridden
    procedure :: fill_internal
    procedure :: fill_open_close
    procedure :: read_ascii ! must be overridden
    procedure :: read_binary ! must be overridden
    procedure :: set_factor ! must be overridden
    procedure :: apply_factor ! must be overridden
    procedure :: open_file

  end type ArrayReaderBaseType

contains

  subroutine read_array(this)
    class(ArrayReaderBaseType) :: this

    ! read control record
    call this%read_control_record()

    ! fill array
    if (this%isConstant) then
      call this%fill_constant()
    else if (this%isInternal) then
      call this%fill_internal()
    else if (this%isOpenClose) then
      call this%fill_open_close()
    end if

  end subroutine read_array

  subroutine reset_reader(this)
    class(ArrayReaderBaseType) :: this
    this%iprn = 0
    this%isConstant = .false.
    this%isInternal = .false.
    this%isOpenClose = .false.
    this%isBinary = .false.
  end subroutine reset_reader

  subroutine read_control_record(this)
    class(ArrayReaderBaseType) :: this
    logical(LGP) :: endOfBlock
    character(len=100) :: keyword
    character(len=MAXCHARLEN) :: string

    ! read the array input style
    call this%parser%GetNextLine(endOfBlock)
    call this%parser%GetStringCaps(keyword)

    ! load array based on the different styles
    select case (keyword)
    case ('CONSTANT')
      this%isConstant = .true.
      call this%set_constant()
    case ('INTERNAL')
      this%isInternal = .true.
    case ('OPEN/CLOSE')
      this%isOpenClose = .true.
      call this%parser%GetString(string)
      this%filename = trim(string)
    case default
      write (errmsg, *) 'Error reading control record for '// &
                        trim(adjustl(this%array_name))//'.  &
                        & Use CONSTANT, INTERNAL, or OPEN/CLOSE.'
      call store_error(errmsg)
      call this%parser%StoreErrorUnit()
    end select

    ! if INTERNAL or OPEN/CLOSE then look for FACTOR and IPRN
    if (this%isInternal .or. this%isOpenClose) then
      do
        call this%parser%GetStringCaps(keyword)
        if (keyword == '') exit
        select case (keyword)
        case ('FACTOR')
          call this%set_factor()
        case ('IPRN')
          this%iprn = this%parser%GetInteger()
        case ('(BINARY)')
          this%isBinary = .true.
        end select
      end do
    end if

  end subroutine read_control_record

  subroutine set_constant(this)
    class(ArrayReaderBaseType) :: this
    errmsg = 'Programming error in ArrayReader'
    call store_error(errmsg, terminate=.true.)
  end subroutine set_constant

  subroutine fill_constant(this)
    class(ArrayReaderBaseType) :: this
    errmsg = 'Programming error in ArrayReader'
    call store_error(errmsg, terminate=.true.)
  end subroutine fill_constant

  subroutine fill_internal(this)
    class(ArrayReaderBaseType) :: this
    this%input_unit = this%parser%iuactive
    call this%read_ascii()
    call this%apply_factor()
  end subroutine fill_internal

  subroutine fill_open_close(this)
    class(ArrayReaderBaseType) :: this
    this%input_unit = 0
    call this%open_file()
    if (this%isBinary) then
      call this%read_binary()
    else
      call this%read_ascii()
    end if
    close (this%input_unit)
    call this%apply_factor()
  end subroutine fill_open_close

  subroutine read_ascii(this)
    class(ArrayReaderBaseType) :: this
    errmsg = 'Programming error in ArrayReader'
    call store_error(errmsg, terminate=.true.)
  end subroutine read_ascii

  subroutine read_binary(this)
    class(ArrayReaderBaseType) :: this
    errmsg = 'Programming error in ArrayReader'
    call store_error(errmsg, terminate=.true.)
  end subroutine read_binary

  subroutine set_factor(this)
    class(ArrayReaderBaseType) :: this
    errmsg = 'Programming error in ArrayReader'
    call store_error(errmsg, terminate=.true.)
  end subroutine set_factor

  subroutine apply_factor(this)
    class(ArrayReaderBaseType) :: this
    errmsg = 'Programming error in ArrayReader'
    call store_error(errmsg, terminate=.true.)
  end subroutine apply_factor

  subroutine open_file(this)
    use OpenSpecModule, only: FORM, ACCESS
    class(ArrayReaderBaseType) :: this
    if (this%isBinary) then
      call openfile(this%input_unit, this%iout, this%filename, &
                    'OPEN/CLOSE', fmtarg_opt=FORM, accarg_opt=ACCESS)
    else
      call openfile(this%input_unit, this%iout, this%filename, 'OPEN/CLOSE')
    end if
  end subroutine open_file

end module ArrayReaderBaseModule
