module AttributesModule
  use KindModule,              only: DP, I4B, LGP
  use ConstantsModule,         only: LENATTRNAME, NATTRS
  use HashTableModule,         only: HashTableType, hash_table_cr, hash_table_da

  ! This class is realy a shorehorn to go from a long vector of
  ! attributes which is human readable to setting these attributes in then
  ! memory object.

  ! JLM: should the keys be included in the class? or maybe the keys separated from the values
  ! JLM: in the vector...
  ! Define a base set of attributes to be used everywhere. These could be extended/subclassed.
  ! https://xarray.pydata.org/en/stable/generated/xarray.DataArray.html
  ! Examples of metadata to include:
  !   varname, longname, units, dimensions, gridname, pro/dia-gnosticity,
  !   public/private, internal/external, type, ...
  ! JLM set or check the keys/columns somewhere with this?
  character(len=LENATTRNAME), dimension(NATTRS), parameter :: attrs_keys = &
       [character(len=LENATTRNAME) :: 'variable name', 'long name', 'units']

  character(len=LENATTRNAME), dimension(NATTRS), parameter :: empty_var_attrs = ''

  ! JLM Document
  type :: Attrs
    integer(I4B) :: n_vars
    character(len=LENATTRNAME), pointer :: vector(:) => null()
    type(HashTableType), pointer, private :: hash    => null()
  contains
    procedure :: df
    procedure :: da
    procedure :: get_var_vec
  end type Attrs

contains

  ! JLM: DOCUMENT
  subroutine df(this, attrs_vector)
    class(Attrs), intent(inout) :: this                                !< Attrs instance
    character(len=LENATTRNAME), intent(in), target :: attrs_vector(:)  !< attribtes str array
    ! --- local
    integer(I4B) :: attrs_vec_len, vv, kk
    character(len=LENATTRNAME) :: the_key
    character(len=LENATTRNAME), dimension(NATTRS) :: keys_passed
    ! ---
    attrs_vec_len = size(attrs_vector((NATTRS+1):))
    write(*,*) 'attrs_vec_len: ', attrs_vec_len
    this%n_vars = attrs_vec_len / NATTRS  !! n_vars, JLM: ensure remainder is zero
    this%vector => attrs_vector((NATTRS+1):)

    call hash_table_cr(this%hash)

    keys_passed = attrs_vector(1:NATTRS)
    do kk = 1, size(keys_passed)
      write(*,*) 'assert: ', keys_passed(kk) == attrs_keys(kk)
    end do

    do vv = 1, this%n_vars
      ! Always calculate the index outside the hash
      the_key = this%vector(((vv - 1) * NATTRS) + 1)
      call this%hash%add_entry(the_key, vv)
    end do

    ! write(*, *) 'attrs%df internal: ', this%vector  ! just verifying, JLM: use for test?

    return
  end subroutine df

  ! JLM DOCUMENT
  function get_var_vec(this, varname) result(res)
    class(Attrs), intent(in) :: this                       !< Attrs instance
    character(len=*), intent(in) :: varname                !< variable name for which to get attrs vector
    character(len=LENATTRNAME), dimension(NATTRS) :: res   !< vector of attrs (copy)
    ! --- local
    integer(I4B) :: indx, start, end
    !
    res(:) = ''  ! default is all zero-len stringsm JLM: good choice for no matching key? use empty_var_attrs?
    indx = this%hash%get_index(varname)
    ! Always calculate the index outside the hash
    start = ((indx - 1) * NATTRS) + 1
    end = start + (NATTRS) - 1
    if (indx > 0) then
      res = this%vector(start:end)
    endif

    ! write(*, *) 'attrs%get_var_vec internal: ', res  ! just verifying, JLM: use for test?  ! JLM: remove

    return
  end function get_var_vec

  ! JLM DOCUMENT
  subroutine da(this)
    class(Attrs), intent(inout) :: this                           !< Attrs instance
    ! ---
    ! if (allocated(this%vector)) deallocate(this%vector)
    if (associated(this%vector)) nullify(this%vector)
    call hash_table_da(this%hash)

    return
  end subroutine da

end module AttributesModule
