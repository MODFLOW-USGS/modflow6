module StructArrayModule

  use KindModule, only: I4B, DP, LGP
  use ConstantsModule, only: DNODATA, MAXCHARLEN, LINELENGTH
  use StructVectorModule, only: StructVectorType
  use MemoryManagerModule, only: mem_allocate
  use CharacterStringModule, only: CharacterStringType
  implicit none
  private
  public :: StructArrayType
  public :: constructStructArray

  type StructArrayType

    integer(I4B) :: ncol
    integer(I4B) :: nrow
    type(StructVectorType), dimension(:), allocatable :: struct_vector_1d

  contains
    procedure :: mem_create_vector
    procedure :: add_vector_int1d
    procedure :: add_vector_dbl1d
    procedure :: add_vector_str1d
    procedure :: read_from_parser

  end type StructArrayType

contains

  function constructStructArray(ncol, nrow) result(struct_array)
    integer(I4B), intent(in) :: ncol
    integer(I4B), intent(in) :: nrow
    type(StructArrayType), pointer :: struct_array

    allocate (struct_array)
    struct_array%ncol = ncol
    struct_array%nrow = nrow
    allocate (struct_array%struct_vector_1d(ncol))

  end function constructStructArray

  subroutine mem_create_vector(this, icol, vartype, nrow, name, memoryPath, &
                               preserve_case)
    class(StructArrayType) :: this
    integer(I4B), intent(in) :: icol
    integer(I4B), intent(in) :: nrow
    character(len=*), intent(in) :: vartype
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: memoryPath
    logical(LGP), optional, intent(in) :: preserve_case
    integer(I4B), dimension(:), pointer, contiguous :: int1d
    real(DP), dimension(:), pointer, contiguous :: dbl1d
    type(CharacterStringType), dimension(:), pointer, contiguous :: cstr1d
    integer(I4B) :: j
    integer(I4B) :: inodata = 999 !todo: create INODATA in constants?

    select case (vartype)
    case ('INTEGER1D')
      call mem_allocate(int1d, nrow, name, memoryPath)
      do j = 1, nrow
        int1d(j) = inodata
      end do
      call this%add_vector_int1d(name, memoryPath, icol, int1d)
    case ('DOUBLE1D')
      call mem_allocate(dbl1d, nrow, name, memoryPath)
      do j = 1, nrow
        dbl1d(j) = DNODATA
      end do
      call this%add_vector_dbl1d(icol, dbl1d)
    case ('STRING')
      call mem_allocate(cstr1d, LINELENGTH, nrow, name, memoryPath)
      do j = 1, nrow
        cstr1d(j) = ''
      end do
      call this%add_vector_str1d(icol, cstr1d, preserve_case)
    end select

    return
  end subroutine mem_create_vector

  subroutine add_vector_int1d(this, varname, memoryPath, icol, int1d)
    class(StructArrayType) :: this
    character(len=*), intent(in) :: varname
    character(len=*), intent(in) :: memoryPath
    integer(I4B), intent(in) :: icol
    integer(I4B), dimension(:), pointer, contiguous, intent(in) :: int1d
    type(StructVectorType) :: sv
    sv%varname = varname
    sv%memoryPath = memoryPath
    sv%memtype = 1
    sv%int1d => int1d
    this%struct_vector_1d(icol) = sv
    return
  end subroutine add_vector_int1d

  subroutine add_vector_dbl1d(this, icol, dbl1d)
    class(StructArrayType) :: this
    integer(I4B), intent(in) :: icol
    real(DP), dimension(:), pointer, contiguous, intent(in) :: dbl1d
    type(StructVectorType) :: sv
    sv%memtype = 2
    sv%dbl1d => dbl1d
    this%struct_vector_1d(icol) = sv
    return
  end subroutine add_vector_dbl1d

  subroutine add_vector_str1d(this, icol, str1d, preserve_case)
    class(StructArrayType) :: this
    integer(I4B), intent(in) :: icol
    type(CharacterStringType), dimension(:), pointer, contiguous, intent(in) :: &
      str1d
    logical(LGP), intent(in) :: preserve_case
    type(StructVectorType) :: sv
    sv%memtype = 3
    sv%preserve_case = preserve_case
    sv%str1d => str1d
    this%struct_vector_1d(icol) = sv
    return
  end subroutine add_vector_str1d

  subroutine read_from_parser(this, parser, iout)
    use BlockParserModule, only: BlockParserType
    use IdmLoggerModule, only: idm_log_var
    class(StructArrayType) :: this
    type(BlockParserType) :: parser
    integer(I4B), intent(in) :: iout
    logical(LGP) :: endOfBlock
    integer(I4B) :: i
    integer(I4B) :: j
    character(len=LINELENGTH) :: str1d
    do i = 1, this%nrow
      call parser%GetNextLine(endOfBlock)
      if (endOfBlock) exit
      do j = 1, this%ncol
        select case (this%struct_vector_1d(j)%memtype)
        case (1)
          this%struct_vector_1d(j)%int1d(i) = parser%GetInteger()
        case (2)
          this%struct_vector_1d(j)%dbl1d(i) = parser%GetDouble()
        case (3)
          call parser%GetString(str1d, &
                                (.not. this%struct_vector_1d(j)%preserve_case))
          this%struct_vector_1d(j)%str1d(i) = str1d
        end select
      end do
    end do
    do j = 1, this%ncol
      select case (this%struct_vector_1d(j)%memtype)
      case (1)
        call idm_log_var(this%struct_vector_1d(j)%int1d, &
                         this%struct_vector_1d(j)%varname, &
                         this%struct_vector_1d(j)%memoryPath, iout)
      case (2)
      case (3)
      end select
    end do
    return
  end subroutine read_from_parser

end module StructArrayModule
