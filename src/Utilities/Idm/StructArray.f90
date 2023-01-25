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
  use SimVariablesModule, only: errmsg
  use SimModule, only: store_error
  use StructVectorModule, only: StructVectorType
  use MemoryManagerModule, only: mem_allocate, mem_reallocate, mem_setptr
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
    integer(I4B) :: blocknum
    logical(LGP) :: deferred_shape = .false.
    integer(I4B) :: deferred_size_init = 5
    type(StructVectorType), dimension(:), allocatable :: struct_vector_1d
  contains
    procedure :: mem_create_vector
    procedure :: add_vector_int1d
    procedure :: add_vector_dbl1d
    procedure :: add_vector_charstr1d
    procedure :: add_vector_intvector
    procedure :: read_from_parser
    procedure :: load_deferred_vectors
    procedure :: log_structarray_vars
    procedure :: check_reallocate

  end type StructArrayType

contains

  !> @brief constructor for a struct_array
  !<
  function constructStructArray(ncol, nrow, blocknum) result(struct_array)
    integer(I4B), intent(in) :: ncol !< number of columns in the StructArrayType
    integer(I4B), pointer, intent(in) :: nrow !< number of rows in the StructArrayType
    integer(I4B), intent(in) :: blocknum !< if block variable, then number, else 0
    type(StructArrayType), pointer :: struct_array !< new StructArrayType

    allocate (struct_array)
    struct_array%ncol = ncol
    if (associated(nrow)) then
      struct_array%nrow = nrow
    else
      struct_array%nrow = 0
      struct_array%deferred_shape = .true.
    end if
    if (blocknum > 0) then
      struct_array%blocknum = blocknum
    else
      struct_array%blocknum = 0
    end if
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
    type(CharacterStringType), dimension(:), pointer, contiguous :: charstr1d
    type(VectorInt), pointer :: intvector
    integer(I4B) :: j
    integer(I4B) :: inodata = 999 !todo: create INODATA in constants?

    select case (vartype)
    case ('INTEGER1D')
      allocate (intvector)
      call this%add_vector_intvector(name, memoryPath, varname_shape, icol, &
                                     intvector)
    case ('INTEGER')
      if (this%deferred_shape) then
        allocate (int1d(this%deferred_size_init))
      else
        call mem_allocate(int1d, this%nrow, name, memoryPath)
      end if
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
    case ('STRING', 'KEYWORD')
      if (this%deferred_shape) then
        allocate (charstr1d(this%deferred_size_init))
      else
        call mem_allocate(charstr1d, LINELENGTH, this%nrow, name, memoryPath)
      end if
      do j = 1, this%nrow
        charstr1d(j) = ''
      end do
      call this%add_vector_charstr1d(name, memoryPath, icol, charstr1d, &
                                     varname_shape, preserve_case)
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
    sv%shape_varname = ''
    sv%memoryPath = memoryPath
    sv%memtype = 1
    sv%int1d => int1d
    if (this%deferred_shape) then
      sv%size = this%deferred_size_init
    else
      sv%size = this%nrow
    end if
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
    sv%shape_varname = ''
    sv%memoryPath = memoryPath
    sv%memtype = 2
    sv%dbl1d => dbl1d
    sv%size = this%nrow
    this%struct_vector_1d(icol) = sv
    return
  end subroutine add_vector_dbl1d

  !> @brief add charstr1d to StructArrayType
  !<
  subroutine add_vector_charstr1d(this, varname, memoryPath, icol, charstr1d, &
                                  varname_shape, preserve_case)
    class(StructArrayType) :: this !< StructArrayType
    integer(I4B), intent(in) :: icol !< column of the vector
    character(len=*), intent(in) :: varname !< name of the variable
    character(len=*), intent(in) :: memoryPath !< memory path to vector
    type(CharacterStringType), dimension(:), pointer, contiguous, intent(in) :: &
      charstr1d !< vector to add
    character(len=*), intent(in) :: varname_shape !< shape of variable
    logical(LGP), intent(in) :: preserve_case
    type(StructVectorType) :: sv
    sv%varname = varname
    sv%shape_varname = varname_shape
    sv%memoryPath = memoryPath
    sv%memtype = 3
    sv%preserve_case = preserve_case
    sv%charstr1d => charstr1d
    if (this%deferred_shape) then
      sv%size = this%deferred_size_init
    else
      sv%size = this%nrow
    end if
    this%struct_vector_1d(icol) = sv
    return
  end subroutine add_vector_charstr1d

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
    sv%shape_varname = varname_shape
    sv%memoryPath = memoryPath
    sv%memtype = 4
    sv%intvector => intvector
    sv%size = -1
    this%struct_vector_1d(icol) = sv
    return
  end subroutine add_vector_intvector

  !> @brief load deferred vectors into managed memory
  !<
  subroutine load_deferred_vectors(this)
    use MemoryManagerModule, only: get_isize
    class(StructArrayType) :: this !< StructArrayType
    integer(I4B) :: i, j, isize
    integer(I4B), dimension(:), pointer, contiguous :: p_intvector
    integer(I4B), dimension(:), pointer, contiguous :: p_int1d
    real(DP), dimension(:), pointer, contiguous :: p_dbl1d
    type(CharacterStringType), dimension(:), pointer, contiguous :: p_charstr1d
    do j = 1, this%ncol
      !
      ! -- jagged arrays always need to be loaded
      if (this%struct_vector_1d(j)%memtype == 4) then
        !
        ! -- size intvector to number of values read
        call this%struct_vector_1d(j)%intvector%shrink_to_fit()
        !
        ! -- allocate memory manager vector
        call mem_allocate(p_intvector, this%struct_vector_1d(j)%intvector%size, &
                          this%struct_vector_1d(j)%varname, &
                          this%struct_vector_1d(j)%memoryPath)
        !
        ! -- load local vector to managed memory
        do i = 1, this%struct_vector_1d(j)%intvector%size
          p_intvector(i) = this%struct_vector_1d(j)%intvector%at(i)
        end do
        !
        ! -- cleanup local memory
        call this%struct_vector_1d(j)%intvector%destroy()
        deallocate (this%struct_vector_1d(j)%intvector)
        nullify (this%struct_vector_1d(j)%intvector_shape)
        !
        ! -- check if shape wasn't known
      else if (this%deferred_shape) then
        !
        ! -- check if already mem managed variable
        call get_isize(this%struct_vector_1d(j)%varname, &
                       this%struct_vector_1d(j)%memoryPath, isize)
        !
        ! -- allocate and load based on memtype
        select case (this%struct_vector_1d(j)%memtype)
          !
          ! -- memtype integer
        case (1)
          !
          ! -- variable exists, reallocate and append
          if (isize > 0) then
            call mem_setptr(p_int1d, this%struct_vector_1d(j)%varname, &
                            this%struct_vector_1d(j)%memoryPath)
            call mem_reallocate(p_int1d, this%nrow + isize, &
                                this%struct_vector_1d(j)%varname, &
                                this%struct_vector_1d(j)%memoryPath)

            do i = 1, this%nrow
              p_int1d(isize + i) = this%struct_vector_1d(j)%int1d(i)
            end do
          else
            !
            ! -- allocate memory manager vector
            call mem_allocate(p_int1d, this%nrow, &
                              this%struct_vector_1d(j)%varname, &
                              this%struct_vector_1d(j)%memoryPath)
            !
            ! -- load local vector to managed memory
            do i = 1, this%nrow
              p_int1d(i) = this%struct_vector_1d(j)%int1d(i)
            end do
          end if
          !
          ! -- deallocate local memory
          deallocate (this%struct_vector_1d(j)%int1d)
          !
          ! -- update structvector
          this%struct_vector_1d(j)%int1d => p_int1d
          this%struct_vector_1d(j)%size = this%nrow
          !
          ! -- memtype real
        case (2)
          !
          call mem_allocate(p_dbl1d, this%nrow, &
                            this%struct_vector_1d(j)%varname, &
                            this%struct_vector_1d(j)%memoryPath)
          !
          do i = 1, this%nrow
            p_dbl1d(i) = this%struct_vector_1d(j)%dbl1d(i)
          end do
          !
          deallocate (this%struct_vector_1d(j)%dbl1d)
          !
          ! --
          this%struct_vector_1d(j)%dbl1d => p_dbl1d
          this%struct_vector_1d(j)%size = this%nrow
          !
          ! -- memtype charstring
        case (3)
          if (isize > 0) then
            call mem_setptr(p_charstr1d, this%struct_vector_1d(j)%varname, &
                            this%struct_vector_1d(j)%memoryPath)
            call mem_reallocate(p_charstr1d, LINELENGTH, this%nrow + isize, &
                                this%struct_vector_1d(j)%varname, &
                                this%struct_vector_1d(j)%memoryPath)

            do i = 1, this%nrow
              p_charstr1d(isize + i) = this%struct_vector_1d(j)%charstr1d(i)
            end do
          else
            !
            call mem_allocate(p_charstr1d, LINELENGTH, this%nrow, &
                              this%struct_vector_1d(j)%varname, &
                              this%struct_vector_1d(j)%memoryPath)
            !
            do i = 1, this%nrow
              p_charstr1d(i) = this%struct_vector_1d(j)%charstr1d(i)
            end do
          end if
          !
          deallocate (this%struct_vector_1d(j)%charstr1d)
          !
          ! -- memtype intvector
        case (4) ! no-op
        case default ! no-op
        end select

      end if
    end do
    return
  end subroutine load_deferred_vectors

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
      !
      ! -- log based on memtype
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

  !> @brief reallocate local memory for deferred vectors if necessary
  !<
  subroutine check_reallocate(this)
    class(StructArrayType) :: this !< StructArrayType
    integer(I4B) :: i, j, newsize
    integer(I4B), dimension(:), pointer, contiguous :: p_int1d
    real(DP), dimension(:), pointer, contiguous :: p_dbl1d
    type(CharacterStringType), dimension(:), pointer, contiguous :: p_charstr1d
    character(len=LINELENGTH) :: errmsg
    integer(I4B) :: reallocate_mult
    !
    ! -- set growth rate
    reallocate_mult = 2
    !
    ! -- if shape wasn't known check to see if vector needs to grow
    if (this%deferred_shape) then
      do j = 1, this%ncol
        !
        ! -- reallocate based on memtype
        select case (this%struct_vector_1d(j)%memtype)
          !
          ! -- memtype integer
        case (1)
          !
          ! -- check if more space needed
          if (this%nrow > this%struct_vector_1d(j)%size) then
            !
            ! -- calculate new size
            newsize = this%struct_vector_1d(j)%size * reallocate_mult
            !
            ! -- allocate new vector
            allocate (p_int1d(newsize))
            !
            ! -- copy from old to new
            do i = 1, this%struct_vector_1d(j)%size
              p_int1d(i) = this%struct_vector_1d(j)%int1d(i)
            end do
            !
            ! -- deallocate old vector
            deallocate (this%struct_vector_1d(j)%int1d)
            !
            ! -- update struct array object
            this%struct_vector_1d(j)%int1d => p_int1d
            this%struct_vector_1d(j)%size = newsize
          end if
          !
          ! -- memtype real
        case (2)
          if (this%nrow > this%struct_vector_1d(j)%size) then
            !
            newsize = this%struct_vector_1d(j)%size * reallocate_mult
            !
            allocate (p_dbl1d(newsize))
            !
            do i = 1, this%struct_vector_1d(j)%size
              p_dbl1d(i) = this%struct_vector_1d(j)%dbl1d(i)
            end do
            !
            deallocate (this%struct_vector_1d(j)%dbl1d)
            !
            this%struct_vector_1d(j)%dbl1d => p_dbl1d
            this%struct_vector_1d(j)%size = newsize
          end if
          !
          ! -- memtype charstring
        case (3)
          if (this%nrow > this%struct_vector_1d(j)%size) then
            !
            newsize = this%struct_vector_1d(j)%size * reallocate_mult
            !
            allocate (p_charstr1d(newsize))
            !
            do i = 1, this%struct_vector_1d(j)%size
              p_charstr1d(i) = this%struct_vector_1d(j)%charstr1d(i)
            end do
            !
            deallocate (this%struct_vector_1d(j)%charstr1d)
            !
            this%struct_vector_1d(j)%charstr1d => p_charstr1d
            this%struct_vector_1d(j)%size = newsize
          end if
        case default
        end select
      end do
      !
      ! -- if known shape throw error if vector undersized
    else
      do j = 1, this%ncol
        !
        ! -- intvector doesn't track size
        if (this%struct_vector_1d(j)%memtype /= 4) then
          !
          ! -- check allocation based on defined shape
          if (this%nrow > this%struct_vector_1d(j)%size) then
            errmsg = 'Input vector capacity exceeded for variable "'// &
                     trim(this%struct_vector_1d(j)%varname)//'"'
            call store_error(errmsg, terminate=.true.)
          end if
        end if
      end do
    end if
    return
  end subroutine check_reallocate

  !> @brief read from the block parser to fill the StructArrayType
  !<
  subroutine read_from_parser(this, parser, iout)
    class(StructArrayType) :: this !< StructArrayType
    type(BlockParserType) :: parser !< block parser to read from
    integer(I4B), intent(in) :: iout !< unit number for output
    logical(LGP) :: endOfBlock
    integer(I4B) :: irow, j, k
    integer(I4B) :: intval, numval
    character(len=LINELENGTH) :: str
    character(len=:), allocatable :: line
    !
    ! -- initialize index irow
    irow = 0
    !
    ! -- read entire block
    do
      !
      ! -- read next line
      call parser%GetNextLine(endOfBlock)
      !
      ! -- no more lines
      if (endOfBlock) then
        exit
        !
        ! -- shape unknown, track lines read
      else if (this%deferred_shape) then
        this%nrow = this%nrow + 1
      end if
      !
      ! -- check and update memory allocation
      call this%check_reallocate()
      !
      ! -- update irow index
      irow = irow + 1
      !
      ! -- handle line reads by column memtype
      do j = 1, this%ncol
        !
        select case (this%struct_vector_1d(j)%memtype)
          !
          ! -- memtype integer
        case (1)
          !
          ! -- if reloadable block and first col, store blocknum
          if (this%blocknum > 0) then
            if (j == 1) then
              this%struct_vector_1d(j)%int1d(irow) = this%blocknum
            end if
            !
            ! -- else read int
          else
            this%struct_vector_1d(j)%int1d(irow) = parser%GetInteger()
          end if
          !
          ! -- memtype real
        case (2)
          this%struct_vector_1d(j)%dbl1d(irow) = parser%GetDouble()
          !
          ! -- memtype charstring
        case (3)
          !
          ! -- if last column with any shape, store rest of line
          !if (this%struct_vector_1d(j)%shape_varname == ':') then
          if (this%struct_vector_1d(j)%shape_varname /= '') then
            if (j == this%ncol) then
              call parser%GetRemainingLine(line)
              this%struct_vector_1d(j)%charstr1d(irow) = line
              deallocate (line)
            end if
            !
            ! -- else read string token
          else
            call parser%GetString(str, &
                                  (.not. this%struct_vector_1d(j)%preserve_case))
            this%struct_vector_1d(j)%charstr1d(irow) = str
          end if
          !
          ! -- memtype intvector
        case (4)
          !
          ! -- get shape for this row
          numval = this%struct_vector_1d(j)%intvector_shape(irow)
          !
          ! -- read and store row values
          do k = 1, numval
            intval = parser%GetInteger()
            call this%struct_vector_1d(j)%intvector%push_back(intval)
          end do
        end select
      end do
    end do
    !
    ! -- if deferred shape vectors were read, load to input path
    call this%load_deferred_vectors()
    !
    ! -- log loaded variables
    call this%log_structarray_vars(iout)

  end subroutine read_from_parser

end module StructArrayModule
