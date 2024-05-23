module SparseMatrixModule
  use KindModule, only: I4B, DP
  use ConstantsModule, only: DZERO
  use MatrixBaseModule
  use VectorBaseModule
  use SeqVectorModule
  use SparseModule, only: sparsematrix
  use MemoryManagerModule, only: mem_allocate, mem_deallocate
  implicit none
  private

  type, public, extends(MatrixBaseType) :: SparseMatrixType
    integer(I4B), pointer :: nrow
    integer(I4B), pointer :: ncol
    integer(I4B), pointer :: nja
    integer(I4B), dimension(:), pointer, contiguous :: ia !< indexes into ja for columns, sorted: diagonal element first
    integer(I4B), dimension(:), pointer, contiguous :: ja
    real(DP), dimension(:), pointer, contiguous :: amat
  contains
    procedure :: init => spm_init
    procedure :: destroy => spm_destroy
    procedure :: create_vec_mm => spm_create_vec_mm
    procedure :: create_vec => spm_create_vec

    procedure :: get_value_pos => spm_get_value_pos
    procedure :: get_diag_value => spm_get_diag_value

    procedure :: set_diag_value => spm_set_diag_value
    procedure :: set_value_pos => spm_set_value_pos
    procedure :: add_value_pos => spm_add_value_pos
    procedure :: add_diag_value => spm_add_diag_value
    procedure :: zero_entries => spm_zero_entries
    procedure :: zero_row_offdiag => spm_zero_row_offdiag

    procedure :: get_first_col_pos => spm_get_first_col_pos
    procedure :: get_last_col_pos => spm_get_last_col_pos
    procedure :: get_column => spm_get_column
    procedure :: get_position => spm_get_position
    procedure :: get_position_diag => spm_get_position_diag
    procedure :: get_aij => spm_get_aij
    procedure :: get_row_offset => spm_get_row_offset

    procedure :: multiply => spm_multiply

    procedure :: allocate_scalars
    procedure :: allocate_arrays

  end type SparseMatrixType

contains

  !> @brief Initialize sparse matrix from passed
  !< sparsity structure
  subroutine spm_init(this, sparse, mem_path)
    class(SparseMatrixType) :: this
    type(sparsematrix) :: sparse
    character(len=*) :: mem_path
    ! local
    integer(I4B) :: ierror

    this%memory_path = mem_path

    call this%allocate_scalars()

    this%nrow = sparse%nrow
    this%ncol = sparse%ncol
    this%nja = sparse%nnz

    call this%allocate_arrays()

    call sparse%filliaja(this%ia, this%ja, ierror, sort=.false.)
    call this%zero_entries()

  end subroutine spm_init

  subroutine spm_destroy(this)
    class(SparseMatrixType) :: this

    call mem_deallocate(this%nrow)
    call mem_deallocate(this%ncol)
    call mem_deallocate(this%nja)

    call mem_deallocate(this%ia)
    call mem_deallocate(this%ja)
    call mem_deallocate(this%amat)

  end subroutine spm_destroy

  function spm_create_vec_mm(this, n, name, mem_path) result(vec)
    class(SparseMatrixType) :: this ! this sparse matrix
    integer(I4B) :: n !< the nr. of elements in the vector
    character(len=*) :: name !< the variable name (for access through memory manager)
    character(len=*) :: mem_path !< memory path for storing the underlying memory items
    class(VectorBaseType), pointer :: vec ! the vector to create
    ! local
    class(SeqVectorType), pointer :: seq_vec

    allocate (seq_vec)
    call seq_vec%create_mm(n, name, mem_path)
    vec => seq_vec

  end function spm_create_vec_mm

  function spm_create_vec(this, n) result(vec)
    class(SparseMatrixType) :: this ! this sparse matrix
    integer(I4B) :: n !< the nr. of elements in the vector
    class(VectorBaseType), pointer :: vec ! the vector to create
    ! local
    class(SeqVectorType), pointer :: seq_vec

    allocate (seq_vec)
    call seq_vec%create(n)
    vec => seq_vec

  end function spm_create_vec

  function spm_get_value_pos(this, ipos) result(value)
    class(SparseMatrixType) :: this
    integer(I4B) :: ipos
    real(DP) :: value

    value = this%amat(ipos)

  end function spm_get_value_pos

  function spm_get_diag_value(this, irow) result(diag_value)
    class(SparseMatrixType) :: this
    integer(I4B) :: irow
    real(DP) :: diag_value

    diag_value = this%amat(this%ia(irow))

  end function spm_get_diag_value

  subroutine spm_set_diag_value(this, irow, diag_value)
    class(SparseMatrixType) :: this
    integer(I4B) :: irow
    real(DP) :: diag_value

    this%amat(this%ia(irow)) = diag_value

  end subroutine spm_set_diag_value

  subroutine spm_set_value_pos(this, ipos, value)
    class(SparseMatrixType) :: this
    integer(I4B) :: ipos
    real(DP) :: value

    this%amat(ipos) = value

  end subroutine spm_set_value_pos

  subroutine spm_add_value_pos(this, ipos, value)
    class(SparseMatrixType) :: this
    integer(I4B) :: ipos
    real(DP) :: value

    this%amat(ipos) = this%amat(ipos) + value

  end subroutine spm_add_value_pos

  subroutine spm_add_diag_value(this, irow, value)
    class(SparseMatrixType) :: this
    integer(I4B) :: irow
    real(DP) :: value

    this%amat(this%ia(irow)) = this%amat(this%ia(irow)) + value

  end subroutine spm_add_diag_value

  function spm_get_first_col_pos(this, irow) result(first_col_pos)
    class(SparseMatrixType) :: this
    integer(I4B) :: irow
    integer(I4B) :: first_col_pos

    first_col_pos = this%ia(irow)

  end function spm_get_first_col_pos

  function spm_get_last_col_pos(this, irow) result(last_col_pos)
    class(SparseMatrixType) :: this
    integer(I4B) :: irow
    integer(I4B) :: last_col_pos

    last_col_pos = this%ia(irow + 1) - 1

  end function spm_get_last_col_pos

  function spm_get_column(this, ipos) result(icol)
    class(SparseMatrixType) :: this
    integer(I4B) :: ipos
    integer(I4B) :: icol

    icol = this%ja(ipos)

  end function spm_get_column

  !> @brief Return position index for (irow,icol) element
  !! in the matrix. This index can be used in other
  !! routines for direct access.
  !< Returns -1 when not found.
  function spm_get_position(this, irow, icol) result(ipos)
    class(SparseMatrixType) :: this
    integer(I4B) :: irow
    integer(I4B) :: icol
    integer(I4B) :: ipos
    ! local
    integer(I4B) :: i, icol_s, icol_e

    ipos = -1
    icol_s = this%get_first_col_pos(irow)
    icol_e = this%get_last_col_pos(irow)
    do i = icol_s, icol_e
      if (this%ja(i) == icol) then
        ipos = i
        return
      end if
    end do

  end function spm_get_position

  function spm_get_position_diag(this, irow) result(ipos_diag)
    class(SparseMatrixType) :: this
    integer(I4B) :: irow
    integer(I4B) :: ipos_diag

    ipos_diag = this%ia(irow)

  end function spm_get_position_diag

  subroutine allocate_scalars(this)
    class(SparseMatrixType) :: this

    call mem_allocate(this%nrow, 'NROW', this%memory_path)
    call mem_allocate(this%ncol, 'NCOL', this%memory_path)
    call mem_allocate(this%nja, 'NJA', this%memory_path)

  end subroutine allocate_scalars

  subroutine allocate_arrays(this)
    class(SparseMatrixType) :: this

    call mem_allocate(this%ia, this%nrow + 1, 'IA', this%memory_path)
    call mem_allocate(this%ja, this%nja, 'JA', this%memory_path)
    call mem_allocate(this%amat, this%nja, 'AMAT', this%memory_path)

  end subroutine allocate_arrays

  !> @brief Set all entries in the matrix to zero
  !<
  subroutine spm_zero_entries(this)
    class(SparseMatrixType) :: this
    ! local
    integer(I4B) :: i

    do i = 1, this%nja
      this%amat(i) = DZERO
    end do

  end subroutine spm_zero_entries

  !> @brief Set all off-diagonal entries in the matrix to zero
  !<
  subroutine spm_zero_row_offdiag(this, irow)
    class(SparseMatrixType) :: this
    integer(I4B) :: irow
    ! local
    integer(I4B) :: ipos

    do ipos = this%ia(irow) + 1, this%ia(irow + 1) - 1
      this%amat(ipos) = DZERO
    end do

  end subroutine spm_zero_row_offdiag

  subroutine spm_get_aij(this, ia, ja, amat)
    class(SparseMatrixType) :: this
    integer(I4B), dimension(:), pointer, contiguous :: ia
    integer(I4B), dimension(:), pointer, contiguous :: ja
    real(DP), dimension(:), pointer, contiguous :: amat

    ia => this%ia
    ja => this%ja
    amat => this%amat

  end subroutine spm_get_aij

  function spm_get_row_offset(this) result(offset)
    class(SparseMatrixType) :: this
    integer(I4B) :: offset

    offset = 0

  end function spm_get_row_offset

  !> @brief Calculates the matrix vector product y = A*x
  !<
  subroutine spm_multiply(this, vec_x, vec_y)
    class(SparseMatrixType) :: this
    class(VectorBaseType), pointer :: vec_x
    class(VectorBaseType), pointer :: vec_y
    ! local
    integer(I4B) :: irow, icol, ipos
    real(DP), dimension(:), pointer, contiguous :: x, y

    x => vec_x%get_array()
    y => vec_y%get_array()
    do irow = 1, this%nrow
      y(irow) = DZERO
      do ipos = this%ia(irow), this%ia(irow + 1) - 1
        icol = this%ja(ipos)
        y(irow) = y(irow) + this%amat(ipos) * x(icol)
      end do
    end do

  end subroutine spm_multiply

end module SparseMatrixModule
