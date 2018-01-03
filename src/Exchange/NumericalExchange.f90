module NumericalExchangeModule

  use KindModule,            only: DP, I4B
  use BaseExchangeModule,    only: BaseExchangeType
  use NumericalModelModule,  only: NumericalModelType
  use BaseExchangeModule,    only: BaseExchangeType, AddBaseExchangeToList
  use ConstantsModule,       only: LINELENGTH, DZERO
  use ListModule,            only: ListType
  use BlockParserModule,     only: BlockParserType

  implicit none

  private
  public :: NumericalExchangeType,                                             &
            AddNumericalExchangeToList, GetNumericalExchangeFromList

  type, extends(BaseExchangeType) :: NumericalExchangeType
    character(len=LINELENGTH), pointer           :: filename                    !name of the input file
    character(len=7), pointer                    :: typename                    !name of the type (e.g., 'NM-NM')
    logical, pointer                             :: implicit                    !logical flag to indicate implicit or explict exchange
    integer(I4B), pointer                        :: iprpak                      !print input flag
    integer(I4B), pointer                        :: iprflow                     !print flag for cell by cell flows
    integer(I4B), pointer                        :: ipakcb                      !save flag for cell by cell flows
    integer(I4B), pointer                        :: nexg                        !number of exchanges
    integer(I4B), dimension(:), pointer          :: nodem1                      !node numbers in model 1
    integer(I4B), dimension(:), pointer          :: nodem2                      !node numbers in model 2
    real(DP), pointer, dimension(:)              :: cond                        !conductance
    integer(I4B), dimension(:), pointer          :: idxglo                      !pointer to solution amat for each connection
    integer(I4B), dimension(:), pointer          :: idxsymglo                   !pointer to symmetric amat position for each connection
    class(NumericalModelType), pointer           :: m1                          !pointer to model 1
    class(NumericalModelType), pointer           :: m2                          !pointer to model 2
    integer(I4B), pointer                        :: naux                        !number of auxiliary variables
    character(len=16), allocatable, dimension(:) :: auxname                     !array of auxiliary variable names
    real(DP), pointer, dimension(:, :)           :: auxvar                      !array of auxiliary variable values
    type(BlockParserType)                        :: parser                      !block parser
  contains
    procedure :: exg_df
    procedure :: exg_ac
    procedure :: exg_mc
    procedure :: exg_ar
    !procedure :: exg_rp (not needed yet; base exg_rp does nothing)
    procedure :: exg_ad
    procedure :: exg_cf
    procedure :: exg_fc
    procedure :: exg_nr
    procedure :: exg_cc
    procedure :: exg_cq
    procedure :: exg_bd
    procedure :: exg_cnvg
    procedure :: exg_ot
    procedure :: exg_da
    procedure :: allocate_scalars
    procedure :: allocate_arrays
    procedure :: read_options
    procedure :: read_dimensions
    procedure :: get_iasym
  end type NumericalExchangeType

contains

  subroutine exg_df(this)
! ******************************************************************************
! exg_df -- define the exchange
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use BaseModelModule, only: BaseModelType
    use InputOutputModule, only: getunit, openfile
    ! -- dummy
    class(NumericalExchangeType) :: this
    ! -- local
! ------------------------------------------------------------------------------
    !
    !
    ! -- return
    return
  end subroutine exg_df

  subroutine exg_ac(this, sparse)
! ******************************************************************************
! exg_ac -- If an implicit exchange then add connections to sparse
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use SparseModule, only:sparsematrix
    ! -- dummy
    class(NumericalExchangeType) :: this
    type(sparsematrix), intent(inout) :: sparse
    ! -- local
    integer(I4B) :: n, iglo, jglo
! ------------------------------------------------------------------------------
    !
    if(this%implicit) then
      do n = 1, this%nexg
        iglo = this%nodem1(n) + this%m1%moffset
        jglo = this%nodem2(n) + this%m2%moffset
        call sparse%addconnection(iglo, jglo, 1)
        call sparse%addconnection(jglo, iglo, 1)
      enddo
    endif
    !
    ! -- return
    return
  end subroutine exg_ac

  subroutine exg_mc(this, iasln, jasln)
! ******************************************************************************
! exg_mc -- Map the connections in the global matrix
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- module
    use SparseModule, only:sparsematrix
    ! -- dummy
    class(NumericalExchangeType) :: this
    integer(I4B), dimension(:), intent(in) :: iasln
    integer(I4B), dimension(:), intent(in) :: jasln
    ! -- local
    integer(I4B) :: n, iglo, jglo, ipos
! ------------------------------------------------------------------------------
    !
    if(this%implicit) then
      do n = 1, this%nexg
        iglo = this%nodem1(n)+this%m1%moffset
        jglo = this%nodem2(n)+this%m2%moffset
        ! -- find jglobal value in row iglo and store in idxglo
        do ipos = iasln(iglo), iasln(iglo + 1) - 1
          if(jglo == jasln(ipos)) then
            this%idxglo(n) = ipos
            exit
          endif
        enddo
        ! -- find and store symmetric location
        do ipos = iasln(jglo), iasln(jglo + 1) - 1
          if(iglo == jasln(ipos)) then
            this%idxsymglo(n) = ipos
            exit
          endif
        enddo
      enddo
    endif
    !
    ! -- Return
    return
  end subroutine exg_mc

  subroutine exg_ar(this)
! ******************************************************************************
! exg_ar -- Allocate and read
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    class(NumericalExchangeType) :: this
! ------------------------------------------------------------------------------
    !
    ! -- return
    return
  end subroutine exg_ar

  subroutine exg_ad(this, isolnid, kpicard, isubtime)
! ******************************************************************************
! exg_ad -- Advance
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(NumericalExchangeType) :: this
    integer(I4B), intent(in) :: isolnid
    integer(I4B), intent(in) :: kpicard
    integer(I4B), intent(in) :: isubtime
    ! -- local
! ------------------------------------------------------------------------------
    !
    !
    ! -- return
    return
  end subroutine exg_ad

  subroutine exg_cf(this, kiter)
! ******************************************************************************
! exg_cf -- Calculate conductance, and for explicit exchanges, set the
!   conductance in the boundary package.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(NumericalExchangeType) :: this
    integer(I4B),intent(in) :: kiter
    ! -- local
! ------------------------------------------------------------------------------
    !
    !
    ! -- return
    return
  end subroutine exg_cf

  subroutine exg_fc(this, kiter, iasln, amatsln, inwtflag)
! ******************************************************************************
! exg_fc -- Fill the matrix
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(NumericalExchangeType) :: this
    integer(I4B), intent(in) :: kiter
    integer(I4B), dimension(:), intent(in) :: iasln
    real(DP), dimension(:), intent(inout) :: amatsln
    integer(I4B), optional, intent(in) :: inwtflag
    ! -- local
    integer(I4B) :: i, nodem1sln, nodem2sln, idiagsln
! ------------------------------------------------------------------------------
    !
    if(this%implicit) then
      do i = 1, this%nexg
        amatsln(this%idxglo(i)) = this%cond(i)
        amatsln(this%idxsymglo(i)) = this%cond(i)
        nodem1sln = this%nodem1(i) + this%m1%moffset
        nodem2sln = this%nodem2(i) + this%m2%moffset
        idiagsln = iasln(nodem1sln)
        amatsln(idiagsln) = amatsln(idiagsln) - this%cond(i)
        idiagsln = iasln(nodem2sln)
        amatsln(idiagsln) = amatsln(idiagsln) - this%cond(i)
      enddo
    else
      ! -- nothing to do here
    endif
    !
    ! -- return
    return
  end subroutine exg_fc

  subroutine exg_nr(this, kiter, iasln, amatsln, inwtflag)
! ******************************************************************************
! exg_nr -- Add Newton-Raphson terms to the solution
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(NumericalExchangeType) :: this
    integer(I4B), intent(in) :: kiter
    integer(I4B), dimension(:), intent(in) :: iasln
    real(DP), dimension(:), intent(inout) :: amatsln
    integer(I4B), optional, intent(in) :: inwtflag
    ! -- local
! ------------------------------------------------------------------------------
    !
    ! -- return
    return
  end subroutine exg_nr

  subroutine exg_cc(this, icnvg)
! ******************************************************************************
! exg_cc -- Additional convergence check
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(NumericalExchangeType) :: this
    integer(I4B), intent(inout) :: icnvg
    ! -- local
! ------------------------------------------------------------------------------
    !
    ! -- return
    return
  end subroutine exg_cc

  subroutine exg_cq(this, icnvg, isuppress_output, isolnid)
! ******************************************************************************
! exg_cq -- Calculate flow
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use ConstantsModule, only: LENBUDTXT
    ! -- dummy
    class(NumericalExchangeType) :: this
    integer(I4B), intent(inout) :: icnvg
    integer(I4B), intent(in) :: isuppress_output
    integer(I4B), intent(in) :: isolnid
    ! -- local
! ------------------------------------------------------------------------------
    !
    ! -- return
    return
  end subroutine exg_cq

  subroutine exg_bd(this, icnvg, isuppress_output, isolnid)
! ******************************************************************************
! exg_bd -- Exchange budget
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use ConstantsModule, only: LENBUDTXT
    ! -- dummy
    class(NumericalExchangeType) :: this
    integer(I4B), intent(inout) :: icnvg
    integer(I4B), intent(in) :: isuppress_output
    integer(I4B), intent(in) :: isolnid
    ! -- local
! ------------------------------------------------------------------------------
    !
    ! -- return
    return
  end subroutine exg_bd

  subroutine exg_cnvg(this, isolnid, icnvg)
! ******************************************************************************
! exg_cnvg -- Check for convergence for explicit exchange
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use ConstantsModule, only: DZERO
    ! -- dummy
    class(NumericalExchangeType) :: this
    integer(I4B), intent(in) :: isolnid
    integer(I4B), intent(inout) :: icnvg
    ! -- local
! ------------------------------------------------------------------------------
    !
    !
    ! -- return
    return
  end subroutine exg_cnvg

  subroutine exg_ot(this)
! ******************************************************************************
! exg_ot
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use SimVariablesModule, only: iout
    ! -- dummy
    class(NumericalExchangeType) :: this
    ! -- local
    integer(I4B) :: iexg, n1, n2
    real(DP) :: flow
    character(len=LINELENGTH) :: node1str, node2str
    ! -- format
    character(len=*), parameter :: fmtheader =                                 &
     "(/1x, 'SUMMARY OF EXCHANGE RATES FOR EXCHANGE ', a, ' WITH ID ', i0, /,  &
       &2a16, 4a16, /, 96('-'))"
    character(len=*), parameter :: fmtdata =                                   &
     "(2a16, 4(1pg16.6))"
! ------------------------------------------------------------------------------
    !
    ! -- Write a table of exchanges
    if(this%iprflow /= 0) then
      write(iout, fmtheader) trim(adjustl(this%name)), this%id, 'NODEM1',      &
                             'NODEM2', 'COND', 'X_M1', 'X_M2', 'FLOW'
      do iexg = 1, this%nexg
        n1 = this%nodem1(iexg)
        n2 = this%nodem2(iexg)
        flow = this%cond(iexg) * (this%m2%x(n2) - this%m1%x(n1))
        call this%m1%dis%noder_to_string(n1, node1str)
        call this%m2%dis%noder_to_string(n2, node2str)
        write(iout, fmtdata) trim(adjustl(node1str)), trim(adjustl(node2str)), &
                             this%cond(iexg), this%m1%x(n1), this%m2%x(n2),    &
                             flow
      enddo
    endif
    !
    ! -- return
    return
  end subroutine exg_ot

  subroutine allocate_scalars(this)
! ******************************************************************************
! allocate_scalars
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryManagerModule, only: mem_allocate
    use ConstantsModule, only: LENORIGIN
    ! -- dummy
    class(NumericalExchangeType) :: this
    ! -- local
! ------------------------------------------------------------------------------
    !
    allocate(this%filename)
    allocate(this%typename)
    call mem_allocate(this%implicit, 'IMPLICIT', this%name)
    call mem_allocate(this%iprpak, 'IPRPAK', this%name)
    call mem_allocate(this%iprflow, 'IPRFLOW', this%name)
    call mem_allocate(this%ipakcb, 'IPAKCB', this%name)
    call mem_allocate(this%nexg, 'NEXG', this%name)
    call mem_allocate(this%naux, 'NAUX', this%name)
    allocate(this%auxname(0))
    this%filename = ''
    this%typename = ''
    this%implicit = .false.
    this%iprpak = 0
    this%iprflow = 0
    this%ipakcb = 0
    this%nexg = 0
    this%naux = 0
    !
    ! -- return
    return
  end subroutine allocate_scalars

  subroutine allocate_arrays(this)
! ******************************************************************************
! allocate_arrays
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryManagerModule, only: mem_allocate
    use ConstantsModule, only: LENORIGIN
    ! -- dummy
    class(NumericalExchangeType) :: this
    ! -- local
    character(len=LENORIGIN) :: origin
! ------------------------------------------------------------------------------
    !
    ! -- create the origin name
    origin = trim(this%name)
    !
    call mem_allocate(this%nodem1, this%nexg, 'NODEM1', origin)
    call mem_allocate(this%nodem2, this%nexg, 'NODEM2', origin)
    call mem_allocate(this%cond, this%nexg, 'COND', origin)
    call mem_allocate(this%idxglo, this%nexg, 'IDXGLO', origin)
    call mem_allocate(this%idxsymglo, this%nexg, 'IDXSYMGLO', origin)
    call mem_allocate(this%auxvar, this%naux, this%nexg, 'AUXVAR', origin)
    !
    ! -- return
    return
  end subroutine allocate_arrays

  subroutine exg_da(this)
! ******************************************************************************
! exg_da
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryManagerModule, only: mem_deallocate
    use ConstantsModule, only: LENORIGIN
    ! -- dummy
    class(NumericalExchangeType) :: this
    ! -- local
! ------------------------------------------------------------------------------
    !
    ! -- scalars
    deallocate(this%filename)
    deallocate(this%typename)
    call mem_deallocate(this%implicit)
    call mem_deallocate(this%iprpak)
    call mem_deallocate(this%iprflow)
    call mem_deallocate(this%ipakcb)
    call mem_deallocate(this%nexg)
    call mem_deallocate(this%naux)
    deallocate(this%auxname)
    !
    ! -- arrays
    call mem_deallocate(this%nodem1)
    call mem_deallocate(this%nodem2)
    call mem_deallocate(this%cond)
    call mem_deallocate(this%idxglo)
    call mem_deallocate(this%idxsymglo)
    call mem_deallocate(this%auxvar)
    !
    ! -- return
    return
  end subroutine exg_da

  subroutine read_options(this, iout)
! ******************************************************************************
! read_options -- Read Options
! Subroutine: (1) read options from input file
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use ConstantsModule, only: LINELENGTH
    use SimModule, only: store_error, ustop
    use InputOutputModule, only: urdaux
    use ArrayHandlersModule, only: expandarray
    ! -- dummy
    class(NumericalExchangeType) :: this
    integer(I4B), intent(in) :: iout
    ! -- local
    character(len=LINELENGTH) :: line, errmsg, keyword
    integer(I4B) :: istart,istop,lloc,ierr
    logical :: isfound, endOfBlock
! ------------------------------------------------------------------------------
    !
    ! -- get options block
    call this%parser%GetBlock('OPTIONS', isfound, ierr,                        &
      supportOpenClose=.true., blockRequired=.false.)
    !
    ! -- parse options block if detected
    if (isfound) then
      write(iout,'(1x,a)')'PROCESSING EXCHANGE OPTIONS'
      do
        call this%parser%GetNextLine(endOfBlock)
        if (endOfBlock) exit
        call this%parser%GetStringCaps(keyword)
        select case (keyword)
          case('AUX', 'AUXILIARY')
            call this%parser%GetRemainingLine(line)
            lloc = 1
            call urdaux(this%naux, this%parser%iuactive, iout, lloc, istart,   &
                        istop, this%auxname, line, 'NM_NM_Exchange')
          case ('PRINT_INPUT')
            this%iprpak = 1
            write(iout,'(4x,a)') &
              'THE LIST OF EXCHANGES WILL BE PRINTED.'
          case ('PRINT_FLOWS')
            this%iprflow = 1
            write(iout,'(4x,a)') &
              'EXCHANGE FLOWS WILL BE PRINTED TO LIST FILES.'
          case default
            write(errmsg,'(4x,a,a)')'****ERROR. UNKNOWN EXCHANGE OPTION: ',    &
                                     trim(keyword)
            call store_error(errmsg)
            call this%parser%StoreErrorUnit()
            call ustop()
        end select
      end do
      write(iout,'(1x,a)')'END OF EXCHANGE OPTIONS'
    end if
    !
    ! -- return
    return
  end subroutine read_options

  subroutine read_dimensions(this, iout)
! ******************************************************************************
! read_dimensions -- Read Dimensions
! Subroutine: (1) read dimensions (size of exchange list) from input file
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use ConstantsModule, only: LINELENGTH
    use SimModule, only: store_error, ustop
    implicit none
    ! -- dummy
    class(NumericalExchangeType) :: this
    integer(I4B), intent(in) :: iout
    ! -- local
    character(len=LINELENGTH) :: errmsg, keyword
    integer(I4B) :: ierr
    logical :: isfound, endOfBlock
! ------------------------------------------------------------------------------
    !
    ! -- get options block
    call this%parser%GetBlock('DIMENSIONS', isfound, ierr,                     &
      supportOpenClose=.true.)
    !
    ! -- parse options block if detected
    if (isfound) then
      write(iout,'(1x,a)')'PROCESSING EXCHANGE DIMENSIONS'
      do
        call this%parser%GetNextLine(endOfBlock)
        if (endOfBlock) exit
        call this%parser%GetStringCaps(keyword)
        select case (keyword)
          case ('NEXG')
            this%nexg = this%parser%GetInteger()
            write(iout,'(4x,a,i7)')'NEXG = ', this%nexg
          case default
            write(errmsg,'(4x,a,a)')'****ERROR. UNKNOWN DIMENSION: ',          &
                                     trim(keyword)
            call store_error(errmsg)
            call this%parser%StoreErrorUnit()
            call ustop()
        end select
      end do
      write(iout,'(1x,a)')'END OF EXCHANGE DIMENSIONS'
    else
      call store_error('ERROR.  REQUIRED DIMENSIONS BLOCK NOT FOUND.')
      call this%parser%StoreErrorUnit()
      call ustop()
    end if
    !
    ! -- return
    return
  end subroutine read_dimensions

  function get_iasym(this) result (iasym)
    class(NumericalExchangeType) :: this
    integer(I4B) :: iasym
    iasym = 0
  end function get_iasym

  function CastAsNumericalExchangeClass(obj) result (res)
    implicit none
    class(*), pointer, intent(inout) :: obj
    class(NumericalExchangeType), pointer :: res
    !
    res => null()
    if (.not. associated(obj)) return
    !
    select type (obj)
    class is (NumericalExchangeType)
      res => obj
    end select
    return
  end function CastAsNumericalExchangeClass

  subroutine AddNumericalExchangeToList(list, exchange)
    implicit none
    ! -- dummy
    type(ListType),       intent(inout) :: list
    class(NumericalExchangeType), pointer, intent(in) :: exchange
    ! -- local
    class(*), pointer :: obj
    !
    obj => exchange
    call list%Add(obj)
    !
    return
  end subroutine AddNumericalExchangeToList

  function GetNumericalExchangeFromList(list, idx) result (res)
    implicit none
    ! -- dummy
    type(ListType),            intent(inout) :: list
    integer(I4B),                   intent(in)    :: idx
    class(NumericalExchangeType), pointer    :: res
    ! -- local
    class(*), pointer :: obj
    !
    obj => list%GetItem(idx)
    res => CastAsNumericalExchangeClass(obj)
    !
    return
  end function GetNumericalExchangeFromList

end module NumericalExchangeModule
