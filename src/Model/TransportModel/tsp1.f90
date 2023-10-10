!> @brief This module contains the base transport model type
!!
!! This module contains the base class for transport models.
!!
!<

module TransportModelModule
  use KindModule, only: DP, I4B
  use VersionModule, only: write_listfile_header
  use ConstantsModule, only: LENFTYPE, LINELENGTH, DZERO, LENPAKLOC, &
                             LENMEMPATH, LENVARNAME
  use SimVariablesModule, only: errmsg
  use NumericalModelModule, only: NumericalModelType
  use TspIcModule, only: TspIcType
  use TspFmiModule, only: TspFmiType
  use TspAdvModule, only: TspAdvType
  use BudgetModule, only: BudgetType
  use MatrixBaseModule

  implicit none

  private

  public :: TransportModelType

  type, extends(NumericalModelType) :: TransportModelType

    type(TspFmiType), pointer :: fmi => null() ! flow model interface
    type(TspAdvType), pointer :: adv => null() !< advection package
    type(TspIcType), pointer :: ic => null() !< initial conditions package
    type(BudgetType), pointer :: budget => null() !< budget object
    integer(I4B), pointer :: infmi => null() ! unit number FMI
    integer(I4B), pointer :: inadv => null() !< unit number ADV
    integer(I4B), pointer :: inic => null() !< unit number IC
    real(DP), pointer :: eqnsclfac => null() !< constant factor by which all terms in the model's governing equation are scaled (divided) for formulation and solution
    ! Labels that will be defined
    character(len=LENVARNAME) :: tsptype = '' !< "solute" or "heat"
    character(len=LENVARNAME) :: depvartype = '' !< "concentration" or "temperature"
    character(len=LENVARNAME) :: depvarunit = '' !< "mass" or "energy"
    character(len=LENVARNAME) :: depvarunitabbrev = '' !< "M" or "E"

  contains

    ! -- public
    procedure, public :: tsp_cr
    procedure, public :: tsp_df
    procedure, public :: tsp_da
    procedure, public :: tsp_ac
    procedure, public :: tsp_mc
    procedure, public :: tsp_ar
    procedure, public :: tsp_rp
    procedure, public :: tsp_ad
    procedure, public :: tsp_fc
    procedure, public :: tsp_cc
    procedure, public :: tsp_cq
    procedure, public :: tsp_bd
    procedure, public :: allocate_tsp_scalars
    procedure, public :: set_tsp_labels
    procedure, public :: ftype_check
    ! -- private
    procedure, private :: create_lstfile
    procedure, private :: create_tsp_packages
    procedure, private :: log_namfile_options

  end type TransportModelType

contains

  !> @brief Create a new generalized transport model object
  !!
  !! Create a new transport model that will be further refined into GWT or GWE
  !<
  subroutine tsp_cr(this, filename, id, modelname, indis)
    ! -- modules
    use GwfNamInputModule, only: GwfNamParamFoundType
    use BudgetModule, only: budget_cr
    ! -- dummy
    class(TransportModelType) :: this
    character(len=*), intent(in) :: filename
    integer(I4B), intent(in) :: id
    integer(I4B), intent(inout) :: indis
    character(len=*), intent(in) :: modelname
    ! -- local
    character(len=LINELENGTH) :: lst_fname
    type(GwfNamParamFoundType) :: found
    !
    ! -- create the list file
    call this%create_lstfile(lst_fname, filename, found%list)
    !
    ! -- log set options
    if (this%iout > 0) then
      call this%log_namfile_options(found)
    end if
    !
    ! -- Create utility objects
    call budget_cr(this%budget, this%name)
    !
    ! -- create model packages
    call this%create_tsp_packages(indis)
    !
    ! -- Return
    return
  end subroutine tsp_cr

  !> @brief Generalized transport model define model
  !!
  !! This subroutine extended by either GWT or GWE.  This routine calls the
  !! define (df) routines for each attached package and sets variables and
  !! pointers.
  !<
  subroutine tsp_df(this)
    ! -- dummy variables
    class(TransportModelType) :: this
    !
    ! -- Return
    return
  end subroutine tsp_df

  !> @brief Generalized transport model add connections
  !!
  !! This subroutine extended by either GWT or GWE.  This routine adds the
  !! internal connections of this model to the sparse matrix
  !<
  subroutine tsp_ac(this, sparse)
    ! -- modules
    use SparseModule, only: sparsematrix
    ! -- dummy variables
    class(TransportModelType) :: this
    type(sparsematrix), intent(inout) :: sparse
    ! -- local
! ------------------------------------------------------------------------------
    !
    ! -- Return
    return
  end subroutine tsp_ac

  !> @brief Generalized transport model map coefficients
  !!
  !! This subroutine extended by either GWT or GWE.  This routine maps the
  !! positions of this models connections in the numerical solution coefficient
  !! matrix.
  !<
  subroutine tsp_mc(this, matrix_sln)
    ! -- dummy
    class(TransportModelType) :: this
    class(MatrixBaseType), pointer :: matrix_sln !< global system matrix
    ! -- local
! ------------------------------------------------------------------------------
    !
    ! -- Return
    return
  end subroutine tsp_mc

  !> @brief Generalized transport model allocate and read
  !!
  !! This subroutine extended by either GWT or GWE.  This routine calls
  !! the allocate and reads (ar) routines of attached packages and allocates
  !! memory for arrays required by the model object.
  !<
  subroutine tsp_ar(this)
    ! -- dummy variables
    class(TransportModelType) :: this
! ------------------------------------------------------------------------------
    !
    ! -- Return
    return
  end subroutine tsp_ar

  !> @brief Generalized transport model read and prepare
  !!
  !! This subroutine extended by either GWT or GWE.  This routine calls
  !! the read and prepare (rp) routines of attached packages.
  !<
  subroutine tsp_rp(this)
    ! -- dummy variables
    class(TransportModelType) :: this
! ------------------------------------------------------------------------------
    !
    ! -- Return
    return
  end subroutine tsp_rp

  !> @brief Generalized transport model time step advance
  !!
  !! This subroutine extended by either GWT or GWE.  This routine calls
  !! the advance time step (ad) routines of attached packages.
  !<
  subroutine tsp_ad(this)
    ! -- dummy variables
    class(TransportModelType) :: this
! ------------------------------------------------------------------------------
    !
    ! -- Return
    return
  end subroutine tsp_ad

  !> @brief Generalized transport model fill coefficients
  !!
  !! This subroutine extended by either GWT or GWE.  This routine calls
  !! the fill coefficients (fc) routines of attached packages.
  !<
  subroutine tsp_fc(this, kiter, matrix_sln, inwtflag)
    ! -- dummy variables
    class(TransportModelType) :: this
    integer(I4B), intent(in) :: kiter
    class(MatrixBaseType), pointer :: matrix_sln
    integer(I4B), intent(in) :: inwtflag
! ------------------------------------------------------------------------------
    !
    ! -- Return
    return
  end subroutine tsp_fc

  !> @brief Generalized transport model final convergence check
  !!
  !! This subroutine extended by either GWT or GWE.  This routine calls
  !! the convergence check (cc) routines of attached packages.
  !<
  subroutine tsp_cc(this, innertot, kiter, iend, icnvgmod, cpak, ipak, dpak)
    ! -- dummy
    class(TransportModelType) :: this
    integer(I4B), intent(in) :: innertot
    integer(I4B), intent(in) :: kiter
    integer(I4B), intent(in) :: iend
    integer(I4B), intent(in) :: icnvgmod
    character(len=LENPAKLOC), intent(inout) :: cpak
    integer(I4B), intent(inout) :: ipak
    real(DP), intent(inout) :: dpak
    ! -- local
! ------------------------------------------------------------------------------
    !
    ! -- Return
    return
  end subroutine tsp_cc

  !> @brief Generalized transport model calculate flows
  !!
  !! This subroutine extended by either GWT or GWE.  This routine calculates
  !! intercell flows (flowja)
  !<
  subroutine tsp_cq(this, icnvg, isuppress_output)
    ! -- dummy variables
    class(TransportModelType) :: this
    integer(I4B), intent(in) :: icnvg
    integer(I4B), intent(in) :: isuppress_output
    ! -- local
! ------------------------------------------------------------------------------
    !
    ! -- Return
    return
  end subroutine tsp_cq

  !> @brief Generalized transport model budget
  !!
  !! This subroutine extended by either GWT or GWE. This routine calculates
  !! package contributions to model budget
  !<
  subroutine tsp_bd(this, icnvg, isuppress_output)
    ! -- dummy
    class(TransportModelType) :: this
    integer(I4B), intent(in) :: icnvg
    integer(I4B), intent(in) :: isuppress_output
! ------------------------------------------------------------------------------
    !
    ! -- Return
    return
  end subroutine tsp_bd

  !> @brief Allocate scalar variables for transport model
  !!
  !!  Method to allocate memory for non-allocatable members.
  !<
  subroutine allocate_tsp_scalars(this, modelname)
    ! -- modules
    use MemoryManagerModule, only: mem_allocate
    ! -- dummy
    class(TransportModelType) :: this
    character(len=*), intent(in) :: modelname
! ------------------------------------------------------------------------------
    !
    ! -- allocate members from (grand)parent class
    call this%NumericalModelType%allocate_scalars(modelname)
    !
    ! -- allocate members that are part of model class
    call mem_allocate(this%inic, 'INIC', this%memoryPath)
    call mem_allocate(this%infmi, 'INFMI', this%memoryPath)
    call mem_allocate(this%inadv, 'INADV', this%memoryPath)
    call mem_allocate(this%eqnsclfac, 'EQNSCLFAC', this%memoryPath)
    !
    this%inic = 0
    this%infmi = 0
    this%inadv = 0
    this%eqnsclfac = DZERO
    !
    ! -- Return
    return
  end subroutine allocate_tsp_scalars

  !> @brief Define the labels corresponding to the flavor of
  !! transport model
  !!
  !! Set variable names according to type of transport model
  !<
  subroutine set_tsp_labels(this, tsptype, depvartype, depvarunit, &
                            depvarunitabbrev)
    class(TransportModelType) :: this
    character(len=*), intent(in), pointer :: tsptype !< type of model, default is GWT (alternative is GWE)
    character(len=*), intent(in) :: depvartype !< dependent variable type, default is "CONCENTRATION"
    character(len=*), intent(in) :: depvarunit !< units of dependent variable for writing to list file
    character(len=*), intent(in) :: depvarunitabbrev !< abbreviation of associated units
    !
    ! -- Set the model type
    this%tsptype = tsptype
    !
    ! -- Set the type of dependent variable being solved for
    this%depvartype = depvartype
    !
    ! -- Set the units associated with the dependent variable
    this%depvarunit = depvarunit
    !
    ! -- Set the units abbreviation
    this%depvarunitabbrev = depvarunitabbrev
    !
    ! -- Return
    return
  end subroutine set_tsp_labels

  !> @brief Deallocate memory
  !!
  !! Deallocate memmory at conclusion of model run
  !<
  subroutine tsp_da(this)
    ! -- modules
    use MemoryManagerModule, only: mem_deallocate
    ! -- dummy
    class(TransportModelType) :: this
    ! -- local
    !
    ! -- Scalars
    call mem_deallocate(this%inic)
    call mem_deallocate(this%infmi)
    call mem_deallocate(this%inadv)
    call mem_deallocate(this%eqnsclfac)
    !
    ! -- Return
    return
  end subroutine tsp_da

  !> @brief Generalized tranpsort model routine
  !!
  !! Check to make sure required input files have been specified
  !<
  subroutine ftype_check(this, indis, inmst)
    ! -- modules
    use ConstantsModule, only: LINELENGTH
    use SimModule, only: store_error, count_errors, store_error_filename
    ! -- dummy
    class(TransportModelType) :: this
    integer(I4B), intent(in) :: indis
    integer(I4B), intent(in) :: inmst
    ! -- local
    character(len=LINELENGTH) :: errmsg
! ------------------------------------------------------------------------------
    !
    ! -- Check for IC6, DIS(u), and MST. Stop if not present.
    if (this%inic == 0) then
      write (errmsg, '(a)') &
        'Initial conditions (IC6) package not specified.'
      call store_error(errmsg)
    end if
    if (indis == 0) then
      write (errmsg, '(a)') &
        'Discretization (DIS6 or DISU6) package not specified.'
      call store_error(errmsg)
    end if
    if (inmst == 0) then
      write (errmsg, '(a)') 'Mass storage and transfer (MST6) &
        &package not specified.'
      call store_error(errmsg)
    end if
    !
    if (count_errors() > 0) then
      write (errmsg, '(a)') 'Required package(s) not specified.'
      call store_error(errmsg)
      call store_error_filename(this%filename)
    end if
    !
    ! -- Return
    return
  end subroutine ftype_check

  !> @brief Create listing output file
  !<
  subroutine create_lstfile(this, lst_fname, model_fname, defined)
    ! -- modules
    use KindModule, only: LGP
    use InputOutputModule, only: openfile, getunit
    ! -- dummy
    class(TransportModelType) :: this
    character(len=*), intent(inout) :: lst_fname
    character(len=*), intent(in) :: model_fname
    logical(LGP), intent(in) :: defined
    ! -- local
    integer(I4B) :: i, istart, istop
    !
    ! -- set list file name if not provided
    if (.not. defined) then
      !
      ! -- initialize
      lst_fname = ' '
      istart = 0
      istop = len_trim(model_fname)
      !
      ! -- identify '.' character position from back of string
      do i = istop, 1, -1
        if (model_fname(i:i) == '.') then
          istart = i
          exit
        end if
      end do
      !
      ! -- if not found start from string end
      if (istart == 0) istart = istop + 1
      !
      ! -- set list file name
      lst_fname = model_fname(1:istart)
      istop = istart + 3
      lst_fname(istart:istop) = '.lst'
    end if
    !
    ! -- create the list file
    this%iout = getunit()
    call openfile(this%iout, 0, lst_fname, 'LIST', filstat_opt='REPLACE')
    !
    ! -- write list file header
    call write_listfile_header(this%iout, 'GROUNDWATER TRANSPORT MODEL (GWT)')
    !
    ! -- Return
    return
  end subroutine create_lstfile

  !> @brief Write model name file options to list file
  !<
  subroutine log_namfile_options(this, found)
    use GwfNamInputModule, only: GwfNamParamFoundType
    class(TransportModelType) :: this
    type(GwfNamParamFoundType), intent(in) :: found
    !
    write (this%iout, '(1x,a)') 'NAMEFILE OPTIONS:'
    !
    if (found%newton) then
      write (this%iout, '(4x,a)') &
        'NEWTON-RAPHSON method enabled for the model.'
      if (found%under_relaxation) then
        write (this%iout, '(4x,a,a)') &
          'NEWTON-RAPHSON UNDER-RELAXATION based on the bottom ', &
          'elevation of the model will be applied to the model.'
      end if
    end if
    !
    if (found%print_input) then
      write (this%iout, '(4x,a)') 'STRESS PACKAGE INPUT WILL BE PRINTED '// &
        'FOR ALL MODEL STRESS PACKAGES'
    end if
    !
    if (found%print_flows) then
      write (this%iout, '(4x,a)') 'PACKAGE FLOWS WILL BE PRINTED '// &
        'FOR ALL MODEL PACKAGES'
    end if
    !
    if (found%save_flows) then
      write (this%iout, '(4x,a)') &
        'FLOWS WILL BE SAVED TO BUDGET FILE SPECIFIED IN OUTPUT CONTROL'
    end if
    !
    write (this%iout, '(1x,a)') 'END NAMEFILE OPTIONS:'
    !
    ! -- Return
    return
  end subroutine log_namfile_options

  !> @brief Source package info and begin to process
  !<
  subroutine create_tsp_packages(this, indis)
    ! -- modules
    use ConstantsModule, only: LINELENGTH, LENPACKAGENAME
    use CharacterStringModule, only: CharacterStringType
    use ArrayHandlersModule, only: expandarray
    use MemoryManagerModule, only: mem_setptr
    use MemoryHelperModule, only: create_mem_path
    use SimVariablesModule, only: idm_context
    use GwfDisModule, only: dis_cr
    use GwfDisvModule, only: disv_cr
    use GwfDisuModule, only: disu_cr
    use TspIcModule, only: ic_cr
    use TspFmiModule, only: fmi_cr
    use TspAdvModule, only: adv_cr
    ! -- dummy
    class(TransportModelType) :: this
    integer(I4B), intent(inout) :: indis ! DIS enabled flag
    ! -- local
    type(CharacterStringType), dimension(:), contiguous, &
      pointer :: pkgtypes => null()
    type(CharacterStringType), dimension(:), contiguous, &
      pointer :: pkgnames => null()
    type(CharacterStringType), dimension(:), contiguous, &
      pointer :: mempaths => null()
    integer(I4B), dimension(:), contiguous, &
      pointer :: inunits => null()
    character(len=LENMEMPATH) :: model_mempath
    character(len=LENFTYPE) :: pkgtype
    character(len=LENPACKAGENAME) :: pkgname
    character(len=LENMEMPATH) :: mempath
    integer(I4B), pointer :: inunit
    integer(I4B) :: n
    !
    ! -- Initialize
    indis = 0
    !
    ! -- set input memory paths, input/model and input/model/namfile
    model_mempath = create_mem_path(component=this%name, context=idm_context)
    !
    ! -- set pointers to model path package info
    call mem_setptr(pkgtypes, 'PKGTYPES', model_mempath)
    call mem_setptr(pkgnames, 'PKGNAMES', model_mempath)
    call mem_setptr(mempaths, 'MEMPATHS', model_mempath)
    call mem_setptr(inunits, 'INUNITS', model_mempath)
    !
    do n = 1, size(pkgtypes)
      !
      ! attributes for this input package
      pkgtype = pkgtypes(n)
      pkgname = pkgnames(n)
      mempath = mempaths(n)
      inunit => inunits(n)
      !
      ! -- create dis package as it is a prerequisite for other packages
      select case (pkgtype)
      case ('DIS6')
        indis = 1
        call dis_cr(this%dis, this%name, mempath, indis, this%iout)
      case ('DISV6')
        indis = 1
        call disv_cr(this%dis, this%name, mempath, indis, this%iout)
      case ('DISU6')
        indis = 1
        call disu_cr(this%dis, this%name, mempath, indis, this%iout)
      case ('IC6')
        this%inic = inunit
      case ('FMI6')
        this%infmi = inunit
      case ('ADV6')
        this%inadv = inunit
      end select
    end do
    !
    ! -- Create packages that are tied directly to model
    call ic_cr(this%ic, this%name, this%inic, this%iout, this%dis, &
               this%depvartype)
    call fmi_cr(this%fmi, this%name, this%infmi, this%iout, this%eqnsclfac, &
                this%depvartype)
    call adv_cr(this%adv, this%name, this%inadv, this%iout, this%fmi, &
                this%eqnsclfac)
    !
    ! -- Return
    return
  end subroutine create_tsp_packages

end module TransportModelModule
