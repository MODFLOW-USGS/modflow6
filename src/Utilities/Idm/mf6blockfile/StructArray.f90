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
  use STLVecIntModule, only: STLVecInt
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
    procedure :: memload_vectors
    procedure :: load_deferred_vector
    procedure :: log_structarray_vars
    procedure :: check_reallocate

  end type StructArrayType

contains

  !> @brief constructor for a struct_array
  !<
  function constructStructArray(ncol, nrow, blocknum) result(struct_array)
    integer(I4B), intent(in) :: ncol !< number of columns in the StructArrayType
    integer(I4B), pointer, intent(in) :: nrow !< number of rows in the StructArrayType
    integer(I4B), intent(in) :: blocknum !< valid block number or 0
    type(StructArrayType), pointer :: struct_array !< new StructArrayType
    !
    ! -- allocate StructArrayType
    allocate (struct_array)
    !
    ! -- set number of arrays
    struct_array%ncol = ncol
    !
    ! -- set rows if known or set deferred
    if (associated(nrow)) then
      struct_array%nrow = nrow
    else
      struct_array%nrow = 0
      struct_array%deferred_shape = .true.
    end if
    !
    ! -- set blocknum
    if (blocknum > 0) then
      struct_array%blocknum = blocknum
    else
      struct_array%blocknum = 0
    end if
    !
    ! -- allocate StructVectorType objects
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
  subroutine mem_create_vector(this, icol, vartype, name, tagname, memoryPath, &
                               varname_shape, preserve_case)
    class(StructArrayType) :: this !< StructArrayType
    integer(I4B), intent(in) :: icol !< column to create
    character(len=*), intent(in) :: vartype !< type of column to create
    character(len=*), intent(in) :: name !< name of the column to create
    character(len=*), intent(in) :: tagname
    character(len=*), intent(in) :: memoryPath !< memory path for storing the vector
    character(len=*), intent(in) :: varname_shape !< shape
    logical(LGP), optional, intent(in) :: preserve_case !< flag indicating whether or not to preserve case
    integer(I4B), dimension(:), pointer, contiguous :: int1d
    real(DP), dimension(:), pointer, contiguous :: dbl1d
    type(CharacterStringType), dimension(:), pointer, contiguous :: charstr1d
    type(STLVecInt), pointer :: intvector
    integer(I4B) :: j
    integer(I4B) :: inodata = 999 !todo: create INODATA in constants?
    !
    ! -- allocate array memory for StructVectorType
    select case (vartype)
      !
    case ('INTEGER1D')
      !
      ! -- allocate intvector object
      allocate (intvector)
      !
      ! -- initialize StructVector and add to StructArray
      call this%add_vector_intvector(name, tagname, memoryPath, varname_shape, &
                                     icol, intvector)
      !
    case ('INTEGER')
      !
      if (this%deferred_shape) then
        ! -- shape not known, allocate locally
        allocate (int1d(this%deferred_size_init))
      else
        ! -- shape known, allocate in managed memory
        call mem_allocate(int1d, this%nrow, name, memoryPath)
      end if
      !
      ! -- initialize vector values
      do j = 1, this%nrow
        int1d(j) = inodata
      end do
      !
      ! -- initialize StructVector and add to StructArray
      call this%add_vector_int1d(name, tagname, memoryPath, icol, int1d)
      !
    case ('DOUBLE')
      !
      call mem_allocate(dbl1d, this%nrow, name, memoryPath)
      !
      do j = 1, this%nrow
        dbl1d(j) = DNODATA
      end do
      !
      call this%add_vector_dbl1d(name, tagname, memoryPath, icol, dbl1d)
      !
    case ('STRING', 'KEYWORD')
      !
      if (this%deferred_shape) then
        allocate (charstr1d(this%deferred_size_init))
      else
        call mem_allocate(charstr1d, LINELENGTH, this%nrow, name, memoryPath)
      end if
      !
      do j = 1, this%nrow
        charstr1d(j) = ''
      end do
      !
      call this%add_vector_charstr1d(name, tagname, memoryPath, icol, charstr1d, &
                                     varname_shape, preserve_case)
    end select

    return
  end subroutine mem_create_vector

  !> @brief add int1d to StructArrayType
  !<
  subroutine add_vector_int1d(this, varname, tagname, memoryPath, icol, int1d)
    class(StructArrayType) :: this !< StructArrayType
    character(len=*), intent(in) :: varname !< name of the variable
    character(len=*), intent(in) :: tagname
    character(len=*), intent(in) :: memoryPath !< memory path to vector
    integer(I4B), intent(in) :: icol !< column of the vector
    integer(I4B), dimension(:), pointer, contiguous, intent(in) :: int1d !< vector to add
    type(StructVectorType) :: sv
    !
    ! -- initialize StructVectorType
    sv%varname = varname
    sv%tagname = tagname
    sv%shapevar = ''
    sv%mempath = memoryPath
    sv%memtype = 1
    sv%int1d => int1d
    !
    ! -- set size
    if (this%deferred_shape) then
      sv%size = this%deferred_size_init
    else
      sv%size = this%nrow
    end if
    !
    ! -- set the object in the Struct Array
    this%struct_vector_1d(icol) = sv
    !
    ! -- return
    return
  end subroutine add_vector_int1d

  !> @brief add dbl1d to StructArrayType
  !<
  subroutine add_vector_dbl1d(this, varname, tagname, memoryPath, icol, dbl1d)
    class(StructArrayType) :: this !< StructArrayType
    character(len=*), intent(in) :: varname !< name of the variable
    character(len=*), intent(in) :: tagname
    character(len=*), intent(in) :: memoryPath !< memory path to vector
    integer(I4B), intent(in) :: icol !< column of the vector
    real(DP), dimension(:), pointer, contiguous, intent(in) :: dbl1d !< vector to add
    type(StructVectorType) :: sv
    !
    ! -- initialize StructVectorType
    sv%varname = varname
    sv%tagname = tagname
    sv%shapevar = ''
    sv%mempath = memoryPath
    sv%memtype = 2
    sv%dbl1d => dbl1d
    sv%size = this%nrow
    !
    ! -- set the object in the Struct Array
    this%struct_vector_1d(icol) = sv
    !
    ! -- return
    return
  end subroutine add_vector_dbl1d

  !> @brief add charstr1d to StructArrayType
  !<
  subroutine add_vector_charstr1d(this, varname, tagname, memoryPath, icol, &
                                  charstr1d, varname_shape, preserve_case)
    class(StructArrayType) :: this !< StructArrayType
    integer(I4B), intent(in) :: icol !< column of the vector
    character(len=*), intent(in) :: varname !< name of the variable
    character(len=*), intent(in) :: tagname
    character(len=*), intent(in) :: memoryPath !< memory path to vector
    type(CharacterStringType), dimension(:), pointer, contiguous, intent(in) :: &
      charstr1d !< vector to add
    character(len=*), intent(in) :: varname_shape !< shape of variable
    logical(LGP), intent(in) :: preserve_case
    type(StructVectorType) :: sv
    !
    ! -- initialize StructVectorType
    sv%varname = varname
    sv%tagname = tagname
    sv%shapevar = varname_shape
    sv%mempath = memoryPath
    sv%memtype = 3
    sv%preserve_case = preserve_case
    sv%charstr1d => charstr1d
    !
    ! -- set size
    if (this%deferred_shape) then
      sv%size = this%deferred_size_init
    else
      sv%size = this%nrow
    end if
    !
    ! -- set the object in the Struct Array
    this%struct_vector_1d(icol) = sv
    !
    ! -- return
    return
  end subroutine add_vector_charstr1d

  !> @brief add STLVecInt to StructArrayType
  !<
  subroutine add_vector_intvector(this, varname, tagname, memoryPath, &
                                  varname_shape, icol, intvector)
    class(StructArrayType) :: this !< StructArrayType
    character(len=*), intent(in) :: varname !< name of the variable
    character(len=*), intent(in) :: tagname
    character(len=*), intent(in) :: memoryPath !< memory path to vector
    character(len=*), intent(in) :: varname_shape !< shape of variable
    integer(I4B), intent(in) :: icol !< column of the vector
    type(STLVecInt), pointer, intent(in) :: intvector !< vector to add
    type(StructVectorType) :: sv
    !
    ! -- initialize STLVecInt
    call intvector%init()
    !
    ! -- set pointer to dynamic shape
    call mem_setptr(sv%intvector_shape, varname_shape, memoryPath)
    !
    ! -- initialize StructVectorType
    sv%varname = varname
    sv%tagname = tagname
    sv%shapevar = varname_shape
    sv%mempath = memoryPath
    sv%memtype = 4
    sv%intvector => intvector
    sv%size = -1
    !
    ! -- set the object in the Struct Array
    this%struct_vector_1d(icol) = sv
    !
    ! -- return
    return
  end subroutine add_vector_intvector

  subroutine load_deferred_vector(this, icol)
    use MemoryManagerModule, only: get_isize
    class(StructArrayType) :: this !< StructArrayType
    integer(I4B), intent(in) :: icol
    integer(I4B) :: i, isize
    integer(I4B), dimension(:), pointer, contiguous :: p_int1d
    real(DP), dimension(:), pointer, contiguous :: p_dbl1d
    type(CharacterStringType), dimension(:), pointer, contiguous :: p_charstr1d
    !
    ! -- check if already mem managed variable
    call get_isize(this%struct_vector_1d(icol)%varname, &
                   this%struct_vector_1d(icol)%mempath, isize)
    !
    ! -- allocate and load based on memtype
    select case (this%struct_vector_1d(icol)%memtype)
      !
    case (1) ! -- memtype integer
      !
      if (isize > 0) then
        ! -- variable exists, reallocate and append
        call mem_setptr(p_int1d, this%struct_vector_1d(icol)%varname, &
                        this%struct_vector_1d(icol)%mempath)
        ! -- Currently deferred vectors are appended to managed
        !    memory vectors when they are already allocated
        !    (e.g. SIMNAM SolutionGroup)
        call mem_reallocate(p_int1d, this%nrow + isize, &
                            this%struct_vector_1d(icol)%varname, &
                            this%struct_vector_1d(icol)%mempath)

        do i = 1, this%nrow
          p_int1d(isize + i) = this%struct_vector_1d(icol)%int1d(i)
        end do
      else
        !
        ! -- allocate memory manager vector
        call mem_allocate(p_int1d, this%nrow, &
                          this%struct_vector_1d(icol)%varname, &
                          this%struct_vector_1d(icol)%mempath)
        !
        ! -- load local vector to managed memory
        do i = 1, this%nrow
          p_int1d(i) = this%struct_vector_1d(icol)%int1d(i)
        end do
      end if
      !
      ! -- deallocate local memory
      deallocate (this%struct_vector_1d(icol)%int1d)
      !
      ! -- update structvector
      this%struct_vector_1d(icol)%int1d => p_int1d
      this%struct_vector_1d(icol)%size = this%nrow
      !
    case (2) ! -- memtype real
      !
      call mem_allocate(p_dbl1d, this%nrow, &
                        this%struct_vector_1d(icol)%varname, &
                        this%struct_vector_1d(icol)%mempath)
      !
      do i = 1, this%nrow
        p_dbl1d(i) = this%struct_vector_1d(icol)%dbl1d(i)
      end do
      !
      deallocate (this%struct_vector_1d(icol)%dbl1d)
      !
      ! --
      this%struct_vector_1d(icol)%dbl1d => p_dbl1d
      this%struct_vector_1d(icol)%size = this%nrow
      !
    case (3) ! -- memtype charstring
      if (isize > 0) then
        call mem_setptr(p_charstr1d, this%struct_vector_1d(icol)%varname, &
                        this%struct_vector_1d(icol)%mempath)
        call mem_reallocate(p_charstr1d, LINELENGTH, this%nrow + isize, &
                            this%struct_vector_1d(icol)%varname, &
                            this%struct_vector_1d(icol)%mempath)

        do i = 1, this%nrow
          p_charstr1d(isize + i) = this%struct_vector_1d(icol)%charstr1d(i)
        end do
      else
        !
        call mem_allocate(p_charstr1d, LINELENGTH, this%nrow, &
                          this%struct_vector_1d(icol)%varname, &
                          this%struct_vector_1d(icol)%mempath)
        !
        do i = 1, this%nrow
          p_charstr1d(i) = this%struct_vector_1d(icol)%charstr1d(i)
        end do
      end if
      !
      deallocate (this%struct_vector_1d(icol)%charstr1d)
      !
    case (4) ! -- memtype intvector
      ! no-op
    case default
    end select
    !
    ! -- return
    return
  end subroutine load_deferred_vector

  !> @brief load deferred vectors into managed memory
  !<
  subroutine memload_vectors(this)
    class(StructArrayType) :: this !< StructArrayType
    integer(I4B) :: icol, j
    integer(I4B), dimension(:), pointer, contiguous :: p_intvector
    !
    do icol = 1, this%ncol
      !
      if (this%struct_vector_1d(icol)%memtype == 4) then
        ! -- intvectors always need to be loaded
        !
        ! -- size intvector to number of values read
        call this%struct_vector_1d(icol)%intvector%shrink_to_fit()
        !
        ! -- allocate memory manager vector
        call mem_allocate(p_intvector, &
                          this%struct_vector_1d(icol)%intvector%size, &
                          this%struct_vector_1d(icol)%varname, &
                          this%struct_vector_1d(icol)%mempath)
        !
        ! -- load local vector to managed memory
        do j = 1, this%struct_vector_1d(icol)%intvector%size
          p_intvector(j) = this%struct_vector_1d(icol)%intvector%at(j)
        end do
        !
        ! -- cleanup local memory
        call this%struct_vector_1d(icol)%intvector%destroy()
        deallocate (this%struct_vector_1d(icol)%intvector)
        nullify (this%struct_vector_1d(icol)%intvector_shape)
        !
      else if (this%deferred_shape) then
        !
        ! -- load as shape wasn't known
        call this%load_deferred_vector(icol)
      end if
    end do
    !
    ! -- return
    return
  end subroutine memload_vectors

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
        !
      case (1) ! -- memtype integer
        !
        call idm_log_var(this%struct_vector_1d(j)%int1d, &
                         this%struct_vector_1d(j)%tagname, &
                         this%struct_vector_1d(j)%mempath, iout)
        !
      case (2) ! -- memtype real
        !
        call idm_log_var(this%struct_vector_1d(j)%dbl1d, &
                         this%struct_vector_1d(j)%tagname, &
                         this%struct_vector_1d(j)%mempath, iout)
        !
      case (4) ! -- memtype intvector
        !
        call mem_setptr(int1d, this%struct_vector_1d(j)%varname, &
                        this%struct_vector_1d(j)%mempath)
        !
        call idm_log_var(int1d, this%struct_vector_1d(j)%tagname, &
                         this%struct_vector_1d(j)%mempath, iout)
        !
      end select
      !
    end do
    !
    ! -- return
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
    integer(I4B) :: reallocate_mult
    !
    ! -- set growth rate
    reallocate_mult = 2
    !
    do j = 1, this%ncol
      !
      ! -- reallocate based on memtype
      select case (this%struct_vector_1d(j)%memtype)
        !
      case (1) ! -- memtype integer
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
      case (2) ! -- memtype real
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
      case (3) ! -- memtype charstring
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
    ! -- return
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
      if (endOfBlock) then
        ! -- no more lines
        exit
        !
      else if (this%deferred_shape) then
        !
        ! -- shape unknown, track lines read
        this%nrow = this%nrow + 1
        !
        ! -- check and update memory allocation
        call this%check_reallocate()
      end if
      !
      ! -- update irow index
      irow = irow + 1
      !
      ! -- handle line reads by column memtype
      do j = 1, this%ncol
        !
        select case (this%struct_vector_1d(j)%memtype)
          !
        case (1) ! -- memtype integer
          !
          ! -- if reloadable block and first col, store blocknum
          if (j == 1 .and. this%blocknum > 0) then
            ! -- store blocknum
            this%struct_vector_1d(j)%int1d(irow) = this%blocknum
          else
            ! -- read and store int
            this%struct_vector_1d(j)%int1d(irow) = parser%GetInteger()
          end if
          !
        case (2) ! -- memtype real
          !
          this%struct_vector_1d(j)%dbl1d(irow) = parser%GetDouble()
          !
        case (3) ! -- memtype charstring
          !
          !if (this%struct_vector_1d(j)%shapevar == ':') then
          if (this%struct_vector_1d(j)%shapevar /= '') then
            ! -- if last column with any shape, store rest of line
            if (j == this%ncol) then
              call parser%GetRemainingLine(line)
              this%struct_vector_1d(j)%charstr1d(irow) = line
              deallocate (line)
            end if
          else
            !
            ! -- read string token
            call parser%GetString(str, &
                                  (.not. this%struct_vector_1d(j)%preserve_case))
            this%struct_vector_1d(j)%charstr1d(irow) = str
          end if
          !
        case (4) ! -- memtype intvector
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
    call this%memload_vectors()
    !
    ! -- log loaded variables
    call this%log_structarray_vars(iout)

  end subroutine read_from_parser

end module StructArrayModule
