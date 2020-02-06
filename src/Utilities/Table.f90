!Module for storing and printing a model table.
!
!New entries can be added for each time step, however, the same number of
!entries must be provided, and they must be provided in the same order.  If not,
!the module will terminate with an error.
!
!vbvl(1, :) contains cumulative rate in
!vbvl(2, :) contains cumulative rate out
!vbvl(3, :) contains rate in
!vbvl(4, :) contains rate out
!vbnm(:)    contains a LENPACKAGENAME character text string for each entry
!rowlabel(:) contains a LENPACKAGENAME character text string to write as a label for each entry

module TableModule

  use KindModule, only: DP, I4B
  use SimModule,       only: store_error, count_errors, ustop
  use ConstantsModule, only: LINELENGTH, LENBUDTXT, LENPACKAGENAME
  
  implicit none
  private
  public TableType, table_cr

  type TableType
    character(len=LINELENGTH), pointer :: title => null()
    logical, pointer :: transient => null()
    integer(I4B), pointer :: msum => null()
    integer(I4B), pointer :: maxsize => null()
    real(DP), pointer :: budperc => null()
    logical, pointer :: written_once => null()
  contains
    procedure :: table_df
    procedure :: table_ot
    procedure :: table_da
    procedure :: reset
    procedure :: add_single_entry
    procedure :: add_multi_entry
    generic :: addentry => add_single_entry, add_multi_entry
    ! -- private
    procedure          :: allocate_scalars
    procedure, private :: allocate_arrays
  end type TableType

  contains

  subroutine table_cr(this, name_model, title, transient)
! ******************************************************************************
! table_cr -- Create a new table object
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    type(TableType), pointer :: this
    character(len=*), intent(in) :: name_model
    character(len=LINELENGTH), intent(in) :: title
    logical, intent(in) :: transient
! ------------------------------------------------------------------------------
    !
    ! -- Create the object
    allocate(this)
    !
    ! -- Allocate scalars
    call this%allocate_scalars(name_model, title, transient)
    !
    ! -- Return
    return
  end subroutine table_cr

  subroutine table_df(this, maxsize)
! ******************************************************************************
! table_df -- Define the Table Object
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    class(TableType) :: this
    integer(I4B), intent(in) :: maxsize
    !character(len=*), optional :: bdtype
    !character(len=*), optional :: bddim
    !character(len=*), optional :: labeltitle
    !character(len=*), optional :: bdzone
! ------------------------------------------------------------------------------
    !
    ! -- Set values
    this%maxsize = maxsize
    !
    ! -- Allocate arrays
    call this%allocate_arrays()
    !!
    !! -- Set the table name
    !if(present(bdtype)) then
    !  this%bdtype = bdtype
    !else
    !  this%bdtype = 'VOLUME'
    !endif
    !!
    !! -- Set the table dimension
    !if(present(bddim)) then
    !  this%bddim = bddim
    !else
    !  this%bddim = 'L**3'
    !endif
    !!
    !! -- Set the table zone
    !if(present(bdzone)) then
    !  this%bdzone = bdzone
    !else
    !  this%bdzone = 'ENTIRE MODEL'
    !endif
    !!
    !! -- Set the label title
    !if(present(labeltitle)) then
    !  this%labeltitle = labeltitle
    !else
    !  this%labeltitle = 'PACKAGE NAME'
    !endif
    !
    ! -- Return
    return
  end subroutine table_df

  subroutine table_ot(this, kstp, kper, iout)
! ******************************************************************************
! table_ot -- Output the Table
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    class(TableType) :: this
    integer(I4B),intent(in) :: kstp
    integer(I4B),intent(in) :: kper
    integer(I4B),intent(in) :: iout
    character(len=17) :: val1,val2
    integer(I4B) :: msum1,l
    real(DP) :: zero,two,hund,bigvl1,bigvl2,small,                     &
                        totrin,totrot,totvin,totvot,diffr,adiffr,              &
                        pdiffr,pdiffv,avgrat,diffv,adiffv,avgvol
! ------------------------------------------------------------------------------
    !
    ! -- Return
    return
  end subroutine table_ot

  subroutine table_da(this)
! ******************************************************************************
! table_da -- Deallocate table variables
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    class(TableType) :: this
! ------------------------------------------------------------------------------
    !
    ! -- Scalars
    deallocate(this%title)
    deallocate(this%transient)
    deallocate(this%msum)
    deallocate(this%maxsize)
    deallocate(this%written_once)
    !
    ! -- Arrays
    !
    ! -- Return
    return
  end subroutine table_da

  subroutine reset(this)
! ******************************************************************************
! initialize -- Reset all of the table rates to zero, and set msum to 1.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use ConstantsModule, only: DZERO
    ! -- dummy
    class(TableType) :: this
    ! -- local
    integer(I4B) :: i
! ------------------------------------------------------------------------------
    !
    this%msum = 1
    !
    ! -- Return
    return
  end subroutine reset

  subroutine add_single_entry(this, rin, rout, delt, text,                     &
                              isupress_accumulate, rowlabel)
! ******************************************************************************
! add_single_entry -- Add Table Entry
!    rin the inflow rate
!    rout is the outflow rate
!    delt is the time step length
!    text is the name of the entry
!    isupress_accumulate is an optional flag.  If specified as 1, then
!      the volume is NOT added to the accumulators on vbvl(1, :) and vbvl(2, :).
!    rowlabel is a LENPACKAGENAME character text entry that is written to the 
!      right of the table.  It can be used for adding package names to table 
!      entries.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(TableType) :: this
    real(DP), intent(in) :: rin
    real(DP), intent(in) :: rout
    real(DP), intent(in) :: delt
    character(len=LENBUDTXT), intent(in) :: text
    integer(I4B), optional, intent(in) :: isupress_accumulate
    character(len=LENPACKAGENAME), optional, intent(in) :: rowlabel
    ! -- local
    character(len=LINELENGTH) :: errmsg
    character(len=*), parameter :: fmtbuderr = &
      "('Error in MODFLOW 6.', 'Entries do not match: ', (a), (a) )"
    integer(I4B) :: iscv
! ------------------------------------------------------------------------------
    !
    !
    ! -- Return
    return
  end subroutine add_single_entry

  subroutine add_multi_entry(this, budterm, delt, budtxt,                     &
                             isupress_accumulate, rowlabel)
! ******************************************************************************
! add_multi_entry -- Add multiple table entries
!    budterm is an array with inflow in column 1 and outflow in column 2
!    delt is the time step length
!    budtxt is the name of the entries.  It should have one entry for each
!      row in budterm
!    isupress_accumulate is an optional flag.  If specified as 1, then
!      the volume is NOT added to the accumulators on vbvl(1, :) and vbvl(2, :).
!    rowlabel is a LENPACKAGENAME character text entry that is written to the 
!      right of the table.  It can be used for adding package names to table 
!      entries. For multiple entries, the same rowlabel is used for each entry.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(TableType) :: this
    real(DP), dimension(:, :), intent(in) :: budterm
    real(DP), intent(in) :: delt
    character(len=LENBUDTXT), dimension(:), intent(in) :: budtxt
    integer(I4B), optional, intent(in) :: isupress_accumulate
    character(len=LENPACKAGENAME), optional, intent(in) :: rowlabel
    ! -- local
    character(len=LINELENGTH) :: errmsg
    character(len=*), parameter :: fmtbuderr = &
      "('Error in MODFLOW 6.', 'Entries do not match: ', (a), (a) )"
    integer(I4B) :: iscv, i
! ------------------------------------------------------------------------------
    !
    iscv = 0
    if(present(isupress_accumulate)) then
      iscv = isupress_accumulate
    endif
    !
    do i = 1, size(budtxt)
      !
      ! -- If table has been written at least once, then make sure that the present
      !    text entry matches the last text entry
      !
      !if(iscv == 0) then
      !  this%vbvl(1, this%msum)=this%vbvl(1,this%msum) + budterm(1, i) * delt
      !  this%vbvl(2, this%msum)=this%vbvl(2,this%msum) + budterm(2, i) * delt
      !endif
      !!
      !this%vbvl(3, this%msum) = budterm(1, i)
      !this%vbvl(4, this%msum) = budterm(2, i)
      !this%vbnm(this%msum) = adjustr(budtxt(i))
      !if(present(rowlabel)) then
      !  this%rowlabel(this%msum) = adjustl(rowlabel)
      !  this%labeled = .true.
      !endif
      !this%msum = this%msum + 1
      !
    enddo
    !
    ! -- Check for errors
    if(count_errors() > 0) then
      call ustop()
    endif
    !
    ! -- Return
    return
  end subroutine add_multi_entry

  subroutine allocate_scalars(this, name_model, title, transient)
! ******************************************************************************
! allocate_scalars -- allocate table scalar variables
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    class(TableType) :: this
    character(len=*), intent(in) :: name_model
    character(len=*), intent(in) :: title
    logical, intent(in) :: transient
! ------------------------------------------------------------------------------
    !
    allocate(this%title)
    allocate(this%transient)
    allocate(this%msum)
    allocate(this%maxsize)
    allocate(this%budperc)
    allocate(this%written_once)
    !allocate(this%labeled)
    !allocate(this%bdtype)
    !allocate(this%bddim)
    !allocate(this%labeltitle)
    !allocate(this%bdzone)
    !
    ! -- Initialize values
    this%title = title
    this%transient = this%transient
    this%msum = 0
    this%maxsize = 0
    this%written_once = .false.
    !this%labeled = .false.
    !this%bdtype = ''
    !this%bddim = ''
    !this%labeltitle = ''
    !this%bdzone = ''
    !
    return
  end subroutine allocate_scalars

  subroutine allocate_arrays(this)
! ******************************************************************************
! allocate_arrays -- allocate table arrays
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use ConstantsModule, only: DZERO
    ! -- dummy
    class(TableType) :: this
! ------------------------------------------------------------------------------
    !
    ! -- If redefining, then need to deallocate/reallocate
    !if(associated(this%vbvl)) then
    !  deallocate(this%vbvl)
    !  nullify(this%vbvl)
    !endif
    !if(associated(this%vbnm)) then
    !  deallocate(this%vbnm)
    !  nullify(this%vbnm)
    !endif
    !if(associated(this%rowlabel)) then
    !  deallocate(this%rowlabel)
    !  nullify(this%rowlabel)
    !endif
    !!
    !! -- Allocate
    !allocate(this%vbvl(4, this%maxsize))
    !allocate(this%vbnm(this%maxsize))
    !allocate(this%rowlabel(this%maxsize))
    !!
    !! -- Initialize values
    !this%vbvl(:, :) = DZERO
    !this%vbnm(:) = ''
    !this%rowlabel(:) = ''
    !
    return
  end subroutine allocate_arrays

end module TableModule
