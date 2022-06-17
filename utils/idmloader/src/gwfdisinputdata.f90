module GwfDisOptionsModule
  use KindModule, only: I4B, DP
  use ConstantsModule, only: DZERO

  implicit none
  private

  !> Data structure and helper methods for passing options
  !<
  type, public :: GwfDisOptionsType
    integer(I4B) :: lenuni     = 0      !< same as model variable
    integer(I4B) :: iwritegrb  = 1      !< same as model variable
    real(DP)     :: xorigin    = DZERO  !< same as model variable
    real(DP)     :: yorigin    = DZERO  !< same as model variable
    real(DP)     :: angrot     = DZERO  !< same as model variable
  contains
    procedure, pass(this) :: construct
    procedure, pass(this) :: destroy
    procedure, pass(this) :: set_values
  end type GwfDisOptionsType

contains

  !> @brief construct the input options, set variables to their defaults
  !<
  subroutine construct(this)
    class(GwfDisOptionsType), intent(inout) :: this  !< the options, as in the input OPTIONS block
    
    ! nothing to be done here for now...
    
    return
  end subroutine construct

  !> @brief cleans up
  !<
  subroutine destroy(this)
    class(GwfDisOptionsType), intent(inout) :: this  !< the NPF options, as in the input OPTIONS block

    ! nothing to be done here for now...

  end subroutine destroy
  
  subroutine set_values(this, lenuni, iwritegrb, xorigin, yorigin, angrot)
    class(GwfDisOptionsType), intent(inout) :: this  !< the options, as in the input OPTIONS block
    integer(I4B), intent(in), optional :: lenuni
    integer(I4B), intent(in), optional :: iwritegrb
    real(DP), intent(in), optional :: xorigin
    real(DP), intent(in), optional :: yorigin
    real(DP), intent(in), optional :: angrot
    if (present(lenuni)) this%lenuni = lenuni
    if (present(iwritegrb)) this%iwritegrb = iwritegrb
    if (present(xorigin)) this%xorigin = xorigin
    if (present(yorigin)) this%yorigin = yorigin
    if (present(angrot)) this%angrot = angrot
    return
  end subroutine set_values

  subroutine set_from_parser()
  
    return
  end subroutine set_from_parser
  
end module GwfDisOptionsModule
  
  
module GwfDisDimensionsModule
  use KindModule, only: I4B, DP
  use ConstantsModule, only: DZERO

  implicit none
  private

  !> Data structure and helper methods for passing dimensions
  !<
  type, public :: GwfDisDimensionsType
    integer(I4B) :: nlay = 0      !< same as model variable
    integer(I4B) :: nrow = 0      !< same as model variable
    integer(I4B) :: ncol = 0      !< same as model variable
  contains
    procedure, pass(this) :: construct
    procedure, pass(this) :: destroy
    procedure, pass(this) :: set_values
  end type GwfDisDimensionsType

contains

  !> @brief construct the input options, set variables to their defaults
  !<
  subroutine construct(this)
    class(GwfDisDimensionsType), intent(inout) :: this  !< the dimensions, as in the input DIMENSIONS block
    
    ! nothing to be done here for now...
    
    return
  end subroutine construct

  !> @brief cleans up
  !<
  subroutine destroy(this)
    class(GwfDisDimensionsType), intent(inout) :: this  !< the dimensions, as in the input DIMENSIONS block

    ! nothing to be done here for now...

  end subroutine destroy
  
  subroutine set_values(this, nlay, nrow, ncol)
    class(GwfDisDimensionsType), intent(inout) :: this  !< the dimensions, as in the input DIMENSIONS block
    integer(I4B), intent(in), optional :: nlay
    integer(I4B), intent(in), optional :: nrow
    integer(I4B), intent(in), optional :: ncol
    
    if (present(nlay)) this%nlay = nlay
    if (present(nrow)) this%nrow = nrow
    if (present(ncol)) this%ncol = ncol
    
    return
  end subroutine set_values

  subroutine set_from_parser()
  
    return
  end subroutine set_from_parser
  
end module GwfDisDimensionsModule

  
module GwfDisGridDataModule
  use KindModule, only: I4B, DP
  use ConstantsModule, only: DZERO, DNODATA

  implicit none
  private

  !> Data structure and helper methods for passing griddata
  !<
  type, public :: GwfDisGridDataType
    real(DP), dimension(:), allocatable           :: delr     !< same as model variable
    real(DP), dimension(:), allocatable           :: delc     !< same as model variable
    real(DP), dimension(:, :), allocatable        :: top2d      !< same as model variable
    real(DP), dimension(:, :, :), allocatable     :: bot3d     !< same as model variable
    integer(I4B), dimension(:, :, :), allocatable :: idomain  !< same as model variable
  contains
    procedure, pass(this) :: construct
    procedure, pass(this) :: destroy
    procedure, pass(this) :: set_values
  end type GwfDisGridDataType

contains

  !> @brief construct the input griddata, set variables to their defaults
  !<
  subroutine construct(this, nlay, nrow, ncol)
    class(GwfDisGridDataType), intent(inout) :: this  !< the griddata
    integer(I4B), intent(in) :: nlay
    integer(I4B), intent(in) :: nrow
    integer(I4B), intent(in) :: ncol
    integer(I4B) :: i
    integer(I4B) :: j
    integer(I4B) :: k
    
    allocate(this%delr(ncol))
    allocate(this%delc(nrow))
    allocate(this%top2d(ncol, nrow))
    allocate(this%bot3d(ncol, nrow, nlay))
    allocate(this%idomain(ncol, nrow, nlay))
    
    ! delr
    do j = 1, ncol
      this%delr(j) = DNODATA
    end do
    
    ! delc
    do i = 1, nrow
      this%delc(i) = DNODATA
    end do
    
    ! top2d
    do i = 1, nrow
      do j = 1, ncol
        this%top2d(j, i) = DNODATA
      end do
    end do
    
    ! bot3d
    do k = 1, nlay
      do i = 1, nrow
        do j = 1, ncol
          this%bot3d(j, i, k) = DNODATA
        end do
      end do
    end do
    
    ! idomain
    do k = 1, nlay
      do i = 1, nrow
        do j = 1, ncol
          this%idomain(j, i, k) = 1
        end do
      end do
    end do
    
    return
  end subroutine construct

  !> @brief cleans up
  !<
  subroutine destroy(this)
    class(GwfDisGridDataType), intent(inout) :: this  !< the griddata

    deallocate(this%delr)
    deallocate(this%delc)
    deallocate(this%top2d)
    deallocate(this%bot3d)
    deallocate(this%idomain)

    return
  end subroutine destroy
  
  subroutine set_values(this, delr, delc, top2d, bot3d, idomain)
    class(GwfDisGridDataType), intent(inout) :: this  !< the griddata
    real(DP), dimension(:), intent(in), optional :: delr
    real(DP), dimension(:), intent(in), optional :: delc
    real(DP), dimension(:, :), intent(in), optional :: top2d
    real(DP), dimension(:, :, :), intent(in), optional :: bot3d
    integer(I4B), dimension(:, :, :), intent(in), optional :: idomain
    
    if (present(delr)) this%delr(:) = delr(:)
    if (present(delc)) this%delc(:) = delc(:)
    if (present(top2d)) this%top2d(:, :) = top2d(:, :)
    if (present(bot3d)) this%bot3d(:, :, :) = bot3d(:, :, :)
    if (present(idomain)) this%idomain(:, :, :) = idomain(:, :, :)
    
    return
  end subroutine set_values

  subroutine set_from_parser()
  
    return
  end subroutine set_from_parser
  
end module GwfDisGridDataModule
  
  
module GwfDisDataLoader
  use KindModule, only: I4B, DP
  use ConstantsModule, only: LINELENGTH
  use SimModule, only: store_error
  use SimVariablesModule, only: errmsg
  use ArrayReadersModule, only: ReadArray
  use BlockParserModule, only: BlockParserType
  use GwfDisOptionsModule, only: GwfDisOptionsType
  use GwfDisDimensionsModule, only: GwfDisDimensionsType
  use GwfDisGridDataModule, only: GwfDisGridDataType

  implicit none
  private
  public :: load_from_parser
  public :: GwfDisData

  type GwfDisData
    
    type(GwfDisOptionsType) :: gwf_dis_options
    type(GwfDisDimensionsType) :: gwf_dis_dimensions
    type(GwfDisGridDataType) :: gwf_dis_griddata
    
  end type GwfDisData
  
  contains

  function load_from_parser(inunit, iout) result(gwf_dis_data)
    integer(I4B), intent(in) :: inunit
    integer(I4B), intent(in) :: iout
    type(GwfDisData) :: gwf_dis_data
    type(BlockParserType) :: parser
    call parser%Initialize(inunit, iout)
    
    ! options
    call gwf_dis_data%gwf_dis_options%construct()
    call load_options_from_parser(parser, gwf_dis_data%gwf_dis_options)
    
    ! dimensions
    call gwf_dis_data%gwf_dis_dimensions%construct()
    call load_dimensions_from_parser(parser, gwf_dis_data%gwf_dis_dimensions)
    
    ! grid data
    call gwf_dis_data%gwf_dis_griddata%construct(gwf_dis_data%gwf_dis_dimensions%nlay, &
                                                 gwf_dis_data%gwf_dis_dimensions%nrow, &
                                                 gwf_dis_data%gwf_dis_dimensions%ncol)
    call load_griddata_from_parser(parser, gwf_dis_data%gwf_dis_griddata)
    
    return    
  end function load_from_parser

  subroutine load_options_from_parser(parser, gwf_dis_options)
    type(BlockParserType), intent(inout) :: parser
    type(GwfDisOptionsType), intent(inout) :: gwf_dis_options
    character(len=LINELENGTH) :: keyword
    integer(I4B) :: ierr
    logical :: isfound, endOfBlock
    
    ! check for options
    call parser%GetBlock('OPTIONS', isfound, ierr, &
                         supportOpenClose=.true., blockRequired=.false.)
    
    ! parse
    if (isfound) then
      do
        call parser%GetNextLine(endOfBlock)
        if (endOfBlock) exit
        call parser%GetStringCaps(keyword)
        select case (keyword)
          case ('LENGTH_UNITS')
            call parser%GetStringCaps(keyword)
            if(keyword == 'FEET') then
              gwf_dis_options%lenuni = 1
            elseif(keyword == 'METERS') then
              gwf_dis_options%lenuni = 2
            elseif(keyword == 'CENTIMETERS') then
              gwf_dis_options%lenuni = 3
            endif
          case('NOGRB')
            gwf_dis_options%iwritegrb = 0
          case('XORIGIN')
            gwf_dis_options%xorigin = parser%GetDouble()
          case('YORIGIN')
            gwf_dis_options%yorigin = parser%GetDouble()
          case('ANGROT')
            gwf_dis_options%angrot = parser%GetDouble()
          case default
            write(errmsg,'(4x,a,a)')'Unknown DIS Option: ', trim(keyword)
            call store_error(errmsg)
            call parser%StoreErrorUnit()
        end select
      end do
    end if
    return
  end subroutine load_options_from_parser
  
  subroutine load_dimensions_from_parser(parser, gwf_dis_dimensions)
    type(BlockParserType), intent(inout) :: parser
    type(GwfDisDimensionsType), intent(inout) :: gwf_dis_dimensions
    character(len=LINELENGTH) :: keyword
    integer(I4B) :: ierr
    logical :: isfound, endOfBlock
    
    ! check for DIMENSIONS
    call parser%GetBlock('DIMENSIONS', isfound, ierr, &
                         supportOpenClose=.true., blockRequired=.true.)
    
    ! parse
    if (isfound) then
      do
        call parser%GetNextLine(endOfBlock)
        if  (endOfBlock) exit
        call parser%GetStringCaps(keyword)
        select case (keyword)
          case ('NLAY')
            gwf_dis_dimensions%nlay = parser%GetInteger()
          case ('NROW')
            gwf_dis_dimensions%nrow = parser%GetInteger()
          case ('NCOL')
            gwf_dis_dimensions%ncol = parser%GetInteger()
          case default
            write(errmsg,'(4x,a,a)') 'Unknown DIS DIMENSION: ', trim(keyword)
            call store_error(errmsg)
            call parser%StoreErrorUnit()
        end select
      end do
    end if
    return
  end subroutine load_dimensions_from_parser
  
  subroutine load_griddata_from_parser(parser, gwf_dis_griddata)
    type(BlockParserType), intent(inout) :: parser
    type(GwfDisGridDataType), intent(inout) :: gwf_dis_griddata

    character(len=LINELENGTH) :: keyword
    integer(I4B) :: ierr
    logical :: isfound, endOfBlock
    integer(I4B) :: nlay
    integer(I4B) :: nrow
    integer(I4B) :: ncol
    integer(I4B) :: ndim
    integer(I4B) :: nodesuser
    integer(I4B) :: iout

    ! data
    character(len=24), dimension(5) :: aname
    data aname(1) /'                    DELR'/
    data aname(2) /'                    DELC'/
    data aname(3) /'TOP ELEVATION OF LAYER 1'/
    data aname(4) /'  MODEL LAYER BOTTOM EL.'/
    data aname(5) /'                 IDOMAIN'/
    
    ! sizes
    ncol = size(gwf_dis_griddata%bot3d, dim=1)
    nrow = size(gwf_dis_griddata%bot3d, dim=2)
    nlay = size(gwf_dis_griddata%bot3d, dim=3)
    ndim = 3
    nodesuser = ncol * nrow * nlay
    iout = 0

    ! check for GRIDDATA block
    call parser%GetBlock('GRIDDATA', isfound, ierr, &
                         supportOpenClose=.false., blockRequired=.true.)
    if (isfound) then
      do
        call parser%GetNextLine(endOfBlock)
        if  (endOfBlock) exit
        call parser%GetStringCaps(keyword)
        select case (keyword)
          case ('DELR')
            call ReadArray(parser%iuactive, gwf_dis_griddata%delr, aname(1), &
                           ndim, ncol, iout, 0)
          case ('DELC')
            call ReadArray(parser%iuactive, gwf_dis_griddata%delc, aname(2), &
                            ndim, nrow, iout, 0)
          case ('TOP')
            call ReadArray(parser%iuactive, gwf_dis_griddata%top2d(:,:), aname(3),      &
                            ndim, ncol, nrow, iout, 0)
          case ('BOTM')
            call parser%GetStringCaps(keyword)
            if (keyword .EQ. 'LAYERED') then
              call ReadArray(parser%iuactive, gwf_dis_griddata%bot3d(:,:,:),            &
                              aname(4), ndim, ncol, nrow,         &
                              nlay, iout, 1, nlay)
            else
              call ReadArray(parser%iuactive, gwf_dis_griddata%bot3d(:,:,:),             &
                              aname(4), ndim, nodesuser, iout)
            end if
          case ('IDOMAIN')
            call parser%GetStringCaps(keyword)
            if (keyword .EQ. 'LAYERED') then
              call ReadArray(parser%iuactive, gwf_dis_griddata%idomain, aname(5),    &
                             ndim, ncol, nrow, nlay, iout, 1, nlay)
            else
              call ReadArray(parser%iuactive, gwf_dis_griddata%idomain, aname(5),    &
                              ndim, nodesuser, 1, 1, iout, 0, 0)
            end if
          case default
            write(errmsg, '(4x,a,a)') 'Unknown DIS GRIDDATA: ', trim(keyword)
            call store_error(errmsg)
            call parser%StoreErrorUnit()
        end select
      end do
    end if
    return
  end subroutine load_griddata_from_parser
  
end module GwfDisDataLoader
  