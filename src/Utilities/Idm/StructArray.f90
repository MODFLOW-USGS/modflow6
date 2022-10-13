!> @brief This module contains the StructArrayModule
!!
!! This module contains the routines for reading a
!! structured list, which consists of a separate vector
!! for each column in the list.
!!
!<
module StructArrayModule

  use KindModule, only: I4B, DP, LGP
  use ConstantsModule, only: DNODATA, LINELENGTH
  use StructVectorModule, only: StructVectorType
  use MemoryManagerModule, only: mem_allocate
  use CharacterStringModule, only: CharacterStringType
  use VectorIntModule, only: VectorInt
  use IdmLoggerModule, only: idm_log_var
  use MemoryManagerModule, only: mem_setptr
  use BlockParserModule, only: BlockParserType

  implicit none
  private
  public :: StructArrayType
  public :: constructStructArray, destructStructArray

  !> @brief derived type for structured array
  !!
  !! This derived type is used to read and store a
  !! list that consists of multiple one-dimensional
  !! vectors.
  !!
  !<
  type StructArrayType
    integer(I4B) :: ncol
    integer(I4B) :: nrow
    type(StructVectorType), dimension(:), allocatable :: struct_vector_1d
  contains
    procedure :: mem_create_vector
    procedure :: add_vector_int1d
    procedure :: add_vector_dbl1d
    procedure :: add_vector_str1d
    procedure :: add_vector_intvector
    procedure :: read_from_parser
    procedure :: load_intvector
    procedure :: log_structarray_vars

  end type StructArrayType

contains

  !> @brief constructor for a struct_array
  !<
  function constructStructArray(ncol, nrow) result(struct_array)
    integer(I4B), intent(in) :: ncol !< number of columns in the StructArrayType
    integer(I4B), intent(in) :: nrow !< number of rows in the StructArrayType
    type(StructArrayType), pointer :: struct_array !< new StructArrayType

    allocate (struct_array)
    struct_array%ncol = ncol
    struct_array%nrow = nrow
    allocate (struct_array%struct_vector_1d(ncol))
  end function constructStructArray

  !> @brief destructor for a struct_array
  !<
  subroutine destructStructArray(struct_array)
    type(StructArrayType), pointer, intent(inout) :: struct_array !< StructArrayType to destroy

    deallocate (struct_array%struct_vector_1d)
    deallocate (struct_array)
    nullify (struct_array)
  end subroutine destructStructArray

  !> @brief create new vector in StructArrayType
  !<
  subroutine mem_create_vector(this, icol, vartype, name, memoryPath, &
                               varname_shape, preserve_case)
    class(StructArrayType) :: this !< StructArrayType
    integer(I4B), intent(in) :: icol !< column to create
    character(len=*), intent(in) :: vartype !< type of column to create
    character(len=*), intent(in) :: name !< name of the column to create
    character(len=*), intent(in) :: memoryPath !< memory path for storing the vector
    character(len=*), intent(in) :: varname_shape !< shape
    logical(LGP), optional, intent(in) :: preserve_case !< flag indicating whether or not to preserve case
    integer(I4B), dimension(:), pointer, contiguous :: int1d
    real(DP), dimension(:), pointer, contiguous :: dbl1d
    type(CharacterStringType), dimension(:), pointer, contiguous :: cstr1d
    type(VectorInt), pointer :: intvector
    integer(I4B) :: j
    integer(I4B) :: inodata = 999 !todo: create INODATA in constants?

    select case (vartype)
    case ('INTEGER1D')
      allocate (intvector)
      call this%add_vector_intvector(name, memoryPath, varname_shape, icol, &
                                     intvector)
    case ('INTEGER')
      call mem_allocate(int1d, this%nrow, name, memoryPath)
      do j = 1, this%nrow
        int1d(j) = inodata
      end do
      call this%add_vector_int1d(name, memoryPath, icol, int1d)
    case ('DOUBLE')
      call mem_allocate(dbl1d, this%nrow, name, memoryPath)
      do j = 1, this%nrow
        dbl1d(j) = DNODATA
      end do
      call this%add_vector_dbl1d(name, memoryPath, icol, dbl1d)
    case ('STRING')
      call mem_allocate(cstr1d, LINELENGTH, this%nrow, name, memoryPath)
      do j = 1, this%nrow
        cstr1d(j) = ''
      end do
      call this%add_vector_str1d(icol, cstr1d, preserve_case)
    end select

    return
  end subroutine mem_create_vector

  !> @brief add int1d to StructArrayType
  !<
  subroutine add_vector_int1d(this, varname, memoryPath, icol, int1d)
    class(StructArrayType) :: this !< StructArrayType
    character(len=*), intent(in) :: varname !< name of the variable
    character(len=*), intent(in) :: memoryPath !< memory path to vector
    integer(I4B), intent(in) :: icol !< column of the vector
    integer(I4B), dimension(:), pointer, contiguous, intent(in) :: int1d !< vector to add
    type(StructVectorType) :: sv
    sv%varname = varname
    sv%memoryPath = memoryPath
    sv%memtype = 1
    sv%int1d => int1d
    this%struct_vector_1d(icol) = sv
    return
  end subroutine add_vector_int1d

  !> @brief add dbl1d to StructArrayType
  !<
  subroutine add_vector_dbl1d(this, varname, memoryPath, icol, dbl1d)
    class(StructArrayType) :: this !< StructArrayType
    character(len=*), intent(in) :: varname !< name of the variable
    character(len=*), intent(in) :: memoryPath !< memory path to vector
    integer(I4B), intent(in) :: icol !< column of the vector
    real(DP), dimension(:), pointer, contiguous, intent(in) :: dbl1d !< vector to add
    type(StructVectorType) :: sv
    sv%varname = varname
    sv%memoryPath = memoryPath
    sv%memtype = 2
    sv%dbl1d => dbl1d
    this%struct_vector_1d(icol) = sv
    return
  end subroutine add_vector_dbl1d

  !> @brief add str1d to StructArrayType
  !<
  subroutine add_vector_str1d(this, icol, str1d, preserve_case)
    class(StructArrayType) :: this !< StructArrayType
    integer(I4B), intent(in) :: icol !< column of the vector
    type(CharacterStringType), dimension(:), pointer, contiguous, intent(in) :: &
      str1d !< vector to add
    logical(LGP), intent(in) :: preserve_case
    type(StructVectorType) :: sv
    sv%memtype = 3
    sv%preserve_case = preserve_case
    sv%str1d => str1d
    this%struct_vector_1d(icol) = sv
    return
  end subroutine add_vector_str1d

  !> @brief add VectorInt to StructArrayType
  !<
  subroutine add_vector_intvector(this, varname, memoryPath, varname_shape, &
                                  icol, intvector)
    class(StructArrayType) :: this !< StructArrayType
    character(len=*), intent(in) :: varname !< name of the variable
    character(len=*), intent(in) :: memoryPath !< memory path to vector
    character(len=*), intent(in) :: varname_shape !< shape of variable
    integer(I4B), intent(in) :: icol !< column of the vector
    type(VectorInt), pointer, intent(in) :: intvector !< vector to add
    type(StructVectorType) :: sv

    call intvector%init()
    call mem_setptr(sv%intvector_shape, varname_shape, memoryPath)

    sv%varname = varname
    sv%memoryPath = memoryPath
    sv%memtype = 4
    sv%intvector => intvector
    this%struct_vector_1d(icol) = sv
    return
  end subroutine add_vector_intvector

  !> @brief load integer vector into StructArrayType
  !<
  subroutine load_intvector(this)
    class(StructArrayType) :: this !< StructArrayType
    integer(I4B) :: i, j
    integer(I4B), dimension(:), pointer, contiguous :: p_intvector
    ! -- if an allocatable vector has been read, add to MemoryManager
    do i = 1, this%ncol
      if (this%struct_vector_1d(i)%memtype == 4) then
        call this%struct_vector_1d(i)%intvector%shrink_to_fit()
        call mem_allocate(p_intvector, this%struct_vector_1d(i)%intvector%size, &
                          this%struct_vector_1d(i)%varname, &
                          this%struct_vector_1d(i)%memoryPath)
        do j = 1, this%struct_vector_1d(i)%intvector%size
          p_intvector(j) = this%struct_vector_1d(i)%intvector%at(j)
        end do
        call this%struct_vector_1d(i)%intvector%destroy()
        deallocate (this%struct_vector_1d(i)%intvector)
        nullify (this%struct_vector_1d(i)%intvector_shape)
      end if
    end do
    return
  end subroutine load_intvector

  !> @brief log information about the StructArrayType
  !<
  subroutine log_structarray_vars(this, iout)
    class(StructArrayType) :: this !< StructArrayType
    integer(I4B), intent(in) :: iout !< unit number for output
    integer(I4B) :: j
    integer(I4B), dimension(:), pointer, contiguous :: int1d
    !
    ! -- idm variable logging
    do j = 1, this%ncol
      select case (this%struct_vector_1d(j)%memtype)
      case (1)
        call idm_log_var(this%struct_vector_1d(j)%int1d, &
                         this%struct_vector_1d(j)%varname, &
                         this%struct_vector_1d(j)%memoryPath, iout)
      case (2)
        call idm_log_var(this%struct_vector_1d(j)%dbl1d, &
                         this%struct_vector_1d(j)%varname, &
                         this%struct_vector_1d(j)%memoryPath, iout)
      case (4)
        call mem_setptr(int1d, this%struct_vector_1d(j)%varname, &
                        this%struct_vector_1d(j)%memoryPath)
        call idm_log_var(int1d, this%struct_vector_1d(j)%varname, &
                         this%struct_vector_1d(j)%memoryPath, iout)

      end select
    end do
    return
  end subroutine log_structarray_vars

  !> @brief read from the block parser to fill the StructArrayType
  !<
  subroutine read_from_parser(this, parser, iout)
    class(StructArrayType) :: this !< StructArrayType
    type(BlockParserType) :: parser !< block parser to read from
    integer(I4B), intent(in) :: iout !< unit number for output
    logical(LGP) :: endOfBlock
    integer(I4B) :: i, j, k
    integer(I4B) :: intval, numval
    character(len=LINELENGTH) :: str1d
    !
    ! -- read block
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
        case (4)
          numval = this%struct_vector_1d(j)%intvector_shape(i)
          do k = 1, numval
            intval = parser%GetInteger()
            call this%struct_vector_1d(j)%intvector%push_back(intval)
          end do
        end select
      end do
    end do
    !
    ! -- if jagged array was read, load to input path
    call this%load_intvector()
    !
    ! -- log loaded variables
    call this%log_structarray_vars(iout)

  end subroutine read_from_parser

end module StructArrayModule
