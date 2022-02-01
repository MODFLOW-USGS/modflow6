module AttributesModule
  use KindModule,              only: DP, I4B, LGP
  use ConstantsModule,         only: LENATTRNAME, NATTRS
  use HashTableModule,         only: HashTableType, hash_table_cr, hash_table_da  

  ! JLM: should this be included in the class?
  ! Can define a base set of attributes to be used everywhere.
  ! These could be extended via subclass.
  ! https://xarray.pydata.org/en/stable/generated/xarray.DataArray.html
  ! Examples of metadata to include:
  !   varname, longname, units, dimensions, gridname, pro/dia-gnosticity,
  !   public/private, internal/external, ...
  ! JLM set or check the keys/columns somewhere with this?
  character(len=LENATTRNAME), dimension(NATTRS), parameter :: &
       attr_keys = [character(len=LENATTRNAME) :: 'varname', 'longname', 'units']
  
  type :: Attrs
    integer(I4B) :: n_vars
    character(len=LENATTRNAME), allocatable :: vector(:)
    type(HashTableType), pointer, private :: hash
  contains
    procedure :: df
    procedure :: get_var_vec
    ! procedure :: get_var_key
  end type Attrs

contains

  ! JLM: DOCUMENT
  subroutine df(this, attrs_vector)
    class(Attrs), intent(inout) :: this                        !< Attrs instance
    character(len=LENATTRNAME), intent(in) :: attrs_vector(:)  !< attribtes str array
    ! --- local
    integer(I4B) :: attrs_vec_len, vv
    character(len=LENATTRNAME) :: the_key
    ! ---
    attrs_vec_len = size(attrs_vector)
    this%n_vars = attrs_vec_len / NATTRS / 2  !! n_vars, JLM: ensure remainder is zero
    allocate(this%vector(attrs_vec_len))
    this%vector = attrs_vector  ! i suppose this could be a pointer?
    write(*,*) 'attrs_vec_len: ', attrs_vec_len
    write(*,*) 'this%n_vars: ', this%n_vars

    call hash_table_cr(this%hash)

    do vv = 1, this%n_vars
      ! Always calculate the index outside the hash
      ! *2 is because of the key:val pairs
      ! +2 is the value for varname:value
      the_key = this%vector(((vv - 1) * 2 * NATTRS) + 2)  
      write(*, *) 'the_key:>', the_key, '<'
      call this%hash%add_entry(the_key, vv)
    end do
    
    write(*, *) 'attrs%df internal: ', this%vector  ! just verifying, JLM: use for test?
    
    return
  end subroutine df

  function get_var_vec(this, varname) result(res)
    class(Attrs), intent(in) :: this                           !< Attrs instance
    character(len=*), intent(in) :: varname                    !< variable name for which to get attrs vector
    character(len=LENATTRNAME), dimension(2 * NATTRS) :: res   !< vector of attrs (copy)
    ! --- local
    integer(I4B) :: indx, start, end
    !
    res(:) = ''  ! default is all zero-len stringsm JLM: good choice for no matching key?
    indx = this%hash%get_index(varname)
    ! Always calculate the index outside the hash
    start = ((indx - 1) * 2 * NATTRS) + 1
    end = start + (2 * NATTRS) - 1
    if (indx > 0) then
      res = this%vector(start:end)
    endif

    write(*, *) 'attrs%get_var_vec internal: ', res  ! just verifying, JLM: use for test?
    
    return
  end function get_var_vec
  
end module AttributesModule
