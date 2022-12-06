!> @brief This module contains the GwtSsm Module
!!
!! This module contains the code for handling sources and sinks
!! associated with groundwater flow model stress packages.
!!
!! todo: need observations for SSM terms
!<
module GwtSsmModule

  use KindModule, only: DP, I4B, LGP
  use ConstantsModule, only: DONE, DZERO, LENAUXNAME, LENFTYPE, &
                             LENPACKAGENAME, LINELENGTH, &
                             TABLEFT, TABCENTER, LENBUDROWLABEL
  use SimModule, only: store_error, count_errors, store_error_unit
  use SimVariablesModule, only: errmsg
  use NumericalPackageModule, only: NumericalPackageType
  use BaseDisModule, only: DisBaseType
  use GwtFmiModule, only: GwtFmiType
  use TableModule, only: TableType, table_cr
  use GwtSpcModule, only: GwtSpcType

  implicit none
  public :: GwtSsmType
  public :: ssm_cr

  character(len=LENFTYPE) :: ftype = 'SSM'
  character(len=LENPACKAGENAME) :: text = ' SOURCE-SINK MIX'

  !> @brief Derived type for the SSM Package
  !!
  !! This derived type corresponds to the SSM Package, which adds
  !! the effects of groundwater sources and sinks to the solute transport
  !! equation.
  !!
  !<
  type, extends(NumericalPackageType) :: GwtSsmType

    integer(I4B), pointer :: nbound !< total number of flow boundaries in this time step
    integer(I4B), dimension(:), pointer, contiguous :: isrctype => null() !< source type 0 is unspecified, 1 is aux, 2 is auxmixed, 3 is ssmi, 4 is ssmimixed
    integer(I4B), dimension(:), pointer, contiguous :: iauxpak => null() !< aux col for concentration
    integer(I4B), dimension(:), pointer, contiguous :: ibound => null() !< pointer to model ibound
    real(DP), dimension(:), pointer, contiguous :: cnew => null() !< pointer to gwt%x
    type(GwtFmiType), pointer :: fmi => null() !< pointer to fmi object
    type(TableType), pointer :: outputtab => null() !< output table object
    type(GwtSpcType), dimension(:), pointer :: ssmivec => null() !< array of stress package concentration objects

  contains

    procedure :: ssm_df
    procedure :: ssm_ar
    procedure :: ssm_rp
    procedure :: ssm_ad
    procedure :: ssm_fc
    procedure :: ssm_cq
    procedure :: ssm_bd
    procedure :: ssm_ot_flow
    procedure :: ssm_da
    procedure :: allocate_scalars
    procedure, private :: ssm_term
    procedure, private :: allocate_arrays
    procedure, private :: set_package_index
    procedure, private :: set_ssm_source_type
    procedure, private :: set_ssm_file_input
    procedure, private :: source_options
    procedure, private :: source_sources
    procedure, private :: source_fileinput
    procedure, private :: log_options
    procedure, private :: pak_setup_outputtab
    procedure, private :: get_ssm_conc

  end type GwtSsmType

contains

  !> @ brief Create a new SSM package
  !!
  !!  Create a new SSM package by defining names, allocating scalars
  !!  and initializing the parser.
  !!
  !<
  subroutine ssm_cr(ssmobj, name_model, inunit, iout, fmi)
    ! -- modules
    use IdmMf6FileLoaderModule, only: input_load
    ! -- dummy
    type(GwtSsmType), pointer :: ssmobj !< GwtSsmType object
    character(len=*), intent(in) :: name_model !< name of the model
    integer(I4B), intent(in) :: inunit !< fortran unit for input
    integer(I4B), intent(in) :: iout !< fortran unit for output
    type(GwtFmiType), intent(in), target :: fmi !< GWT FMI package
    ! -- formats
    character(len=*), parameter :: fmtssm = &
      "(1x,/1x,'SSM -- SOURCE-SINK MIXING PACKAGE, VERSION 1, 8/25/2017', &
      &' INPUT READ FROM UNIT ', i0, //)"
    !
    ! -- Create the object
    allocate (ssmobj)
    !
    ! -- create name and memory path
    call ssmobj%set_names(1, name_model, 'SSM', 'SSM')
    !
    ! -- Allocate scalars
    call ssmobj%allocate_scalars()
    !
    ! -- Set variables
    ssmobj%inunit = inunit
    ssmobj%iout = iout
    ssmobj%fmi => fmi
    !
    ! -- Check if input file is open
    if (inunit > 0) then
      !
      ! -- Print a message identifying the node property flow package.
      if (iout > 0) then
        write (iout, fmtssm) inunit
      end if
      !
      ! -- Initialize block parser
      call ssmobj%parser%Initialize(ssmobj%inunit, ssmobj%iout)
      !
      ! -- Load package input context
      call input_load(ssmobj%parser, 'SSM6', 'GWT', 'SSM', ssmobj%name_model, &
                      'SSM', iout)
    end if
    !
    ! -- Return
    return
  end subroutine ssm_cr

  !> @ brief Define SSM Package
  !!
  !! This routine is called from gwt_df(), but does not do anything because
  !! df is typically used to set up dimensions.  For the ssm package, the
  !! total number of ssm entries is defined by the flow model.
  !!
  !<
  subroutine ssm_df(this)
    ! -- modules
    use MemoryManagerModule, only: mem_setptr
    ! -- dummy
    class(GwtSsmType) :: this !< GwtSsmType object
    ! -- local
    ! -- formats
    !
    ! -- Return
    return
  end subroutine ssm_df

  !> @ brief Allocate and read SSM Package
  !!
  !! This routine is called from gwt_ar().  It allocates arrays, reads
  !! options and data, and sets up the output table.
  !!
  !<
  subroutine ssm_ar(this, dis, ibound, cnew)
    ! -- modules
    use MemoryManagerModule, only: mem_setptr
    ! -- dummy
    class(GwtSsmType) :: this !< GwtSsmType object
    class(DisBaseType), pointer, intent(in) :: dis !< discretization package
    integer(I4B), dimension(:), pointer, contiguous :: ibound !< GWT model ibound
    real(DP), dimension(:), pointer, contiguous :: cnew !< GWT model dependent variable
    ! -- local
    ! -- formats
    !
    ! -- store pointers to arguments that were passed in
    this%dis => dis
    this%ibound => ibound
    this%cnew => cnew
    !
    ! -- Check to make sure that there are flow packages
    if (this%fmi%nflowpack == 0) then
      write (errmsg, '(a)') 'SSM PACKAGE DOES NOT DETECT ANY BOUNDARY FLOWS &
                            &THAT REQUIRE SSM TERMS.  ACTIVATE GWF-GWT &
                            &EXCHANGE OR ACTIVATE FMI PACKAGE AND PROVIDE A &
                            &BUDGET FILE THAT CONTAINS BOUNDARY FLOWS.  IF NO &
                            &BOUNDARY FLOWS ARE PRESENT IN CORRESPONDING GWF &
                            &MODEL THEN THIS SSM PACKAGE SHOULD BE REMOVED.'
      call store_error(errmsg)
      call this%parser%StoreErrorUnit()
    end if
    !
    ! -- Allocate arrays
    call this%allocate_arrays()
    !
    ! -- Source ssm options
    call this%source_options()
    !
    ! -- Source the data blocks
    call this%source_sources()
    call this%source_fileinput()
    !
    ! -- setup the output table
    call this%pak_setup_outputtab()
    !
    ! -- Return
    return
  end subroutine ssm_ar

  !> @ brief Read and prepare this SSM Package
  !!
  !! This routine is called from gwt_rp().  It is called at the beginning of
  !! each stress period.  If any SPC input files are used to provide source
  !! and sink concentrations, then period blocks for the current stress period
  !! are read.
  !!
  !<
  subroutine ssm_rp(this)
    ! -- modules
    ! -- dummy
    class(GwtSsmType) :: this !< GwtSsmType object
    ! -- local
    integer(I4B) :: ip
    type(GwtSpcType), pointer :: ssmiptr
    ! -- formats
    !
    ! -- Call the rp method on any ssm input files
    do ip = 1, this%fmi%nflowpack
      if (this%fmi%iatp(ip) /= 0) cycle
      if (this%isrctype(ip) == 3 .or. this%isrctype(ip) == 4) then
        ssmiptr => this%ssmivec(ip)
        call ssmiptr%spc_rp()
      end if
    end do
    !
    ! -- Return
    return
  end subroutine ssm_rp

  !> @ brief Advance the SSM Package
  !!
  !! This routine is called from gwt_ad().  It is called at the beginning of
  !! each time step.  The total number of flow boundaries is counted and stored
  !! in this%nbound.  Also, if any SPC input files are used to provide source
  !! and sink concentrations and time series are referenced in those files,
  !! then ssm concenrations must be interpolated for the time step.
  !!
  !<
  subroutine ssm_ad(this)
    ! -- modules
    ! -- dummy
    class(GwtSsmType) :: this !< GwtSsmType object
    ! -- local
    integer(I4B) :: ip
    type(GwtSpcType), pointer :: ssmiptr
    integer(I4B) :: i
    integer(I4B) :: node
! ------------------------------------------------------------------------------
    !
    ! -- Calculate total number of exising flow boundaries. It is possible
    !    that a node may equal zero.  In this case, the bound should be
    !    skipped and not written to ssm output.
    this%nbound = 0
    do ip = 1, this%fmi%nflowpack
      if (this%fmi%iatp(ip) /= 0) cycle
      do i = 1, this%fmi%gwfpackages(ip)%nbound
        node = this%fmi%gwfpackages(ip)%nodelist(i)
        if (node > 0) then
          this%nbound = this%nbound + 1
        end if
      end do
    end do
    !
    ! -- Call the ad method on any ssm input files so that values for
    !    time-series are interpolated
    do ip = 1, this%fmi%nflowpack
      if (this%fmi%iatp(ip) /= 0) cycle
      if (this%isrctype(ip) == 3 .or. this%isrctype(ip) == 4) then
        ssmiptr => this%ssmivec(ip)
        call ssmiptr%spc_ad(this%fmi%gwfpackages(ip)%nbound, &
                            this%fmi%gwfpackages(ip)%budtxt)
      end if
    end do
    !
    ! -- Return
    return
  end subroutine ssm_ad

  !> @ brief Calculate the SSM mass flow rate and hcof and rhs values
  !!
  !! This is the primary SSM routine that calculates the matrix coefficient
  !! and right-hand-side value for any package and package entry.  It returns
  !! several different optional variables that are used throughout this
  !! package to update matrix terms, budget calculations, and output tables.
  !!
  !<
  subroutine ssm_term(this, ipackage, ientry, rrate, rhsval, hcofval, &
                      cssm, qssm)
    ! -- dummy
    class(GwtSsmType) :: this !< GwtSsmType
    integer(I4B), intent(in) :: ipackage !< package number
    integer(I4B), intent(in) :: ientry !< bound number
    real(DP), intent(out), optional :: rrate !< calculated mass flow rate
    real(DP), intent(out), optional :: rhsval !< calculated rhs value
    real(DP), intent(out), optional :: hcofval !< calculated hcof value
    real(DP), intent(out), optional :: cssm !< calculated source concentration depending on flow direction
    real(DP), intent(out), optional :: qssm !< water flow rate into model cell from boundary package
    ! -- local
    logical(LGP) :: lauxmixed
    integer(I4B) :: n
    real(DP) :: qbnd
    real(DP) :: ctmp
    real(DP) :: omega
    real(DP) :: hcoftmp
    real(DP) :: rhstmp
    !
    ! -- retrieve node number, qbnd and iauxpos
    hcoftmp = DZERO
    rhstmp = DZERO
    ctmp = DZERO
    qbnd = DZERO
    n = this%fmi%gwfpackages(ipackage)%nodelist(ientry)
    !
    ! -- If cell is active (ibound > 0) then calculate values
    if (this%ibound(n) > 0) then
      !
      ! -- retrieve qbnd and iauxpos
      qbnd = this%fmi%gwfpackages(ipackage)%get_flow(ientry)
      call this%get_ssm_conc(ipackage, ientry, ctmp, lauxmixed)
      !
      ! -- assign values for hcoftmp, rhstmp, and ctmp for subsequent assigment
      !    of hcof, rhs, and rate
      if (.not. lauxmixed) then
        !
        ! -- If qbnd is positive, then concentration represents the inflow
        !    concentration.  If qbnd is negative, then the outflow concentration
        !    is set equal to the simulated cell concentration
        if (qbnd >= DZERO) then
          omega = DZERO ! rhs
        else
          ctmp = this%cnew(n)
          omega = DONE ! lhs
          if (ctmp < DZERO) then
            omega = DZERO ! concentration is negative, so set mass flux to zero
          end if
        end if
      else
        !
        ! -- lauxmixed value indicates that this is a mixed sink type where
        !    the concentration value represents the injected concentration if
        !    qbnd is positive. If qbnd is negative, then the withdrawn water
        !    is equal to the minimum of the aux concentration and the cell
        !    concentration.
        if (qbnd >= DZERO) then
          omega = DZERO ! rhs (ctmp is aux value)
        else
          if (ctmp < this%cnew(n)) then
            omega = DZERO ! rhs (ctmp is aux value)
          else
            omega = DONE ! lhs (ctmp is cell concentration)
            ctmp = this%cnew(n)
          end if
        end if
      end if
      !
      ! -- Add terms based on qbnd sign
      if (qbnd <= DZERO) then
        hcoftmp = qbnd * omega
      else
        rhstmp = -qbnd * ctmp * (DONE - omega)
      end if
      !
      ! -- end of active ibound
    end if
    !
    ! -- set requested values
    if (present(hcofval)) hcofval = hcoftmp
    if (present(rhsval)) rhsval = rhstmp
    if (present(rrate)) rrate = hcoftmp * ctmp - rhstmp
    if (present(cssm)) cssm = ctmp
    if (present(qssm)) qssm = qbnd
    !
    ! -- return
    return
  end subroutine ssm_term

  !> @ brief Provide bound concentration and mixed flag
  !!
  !! SSM concentrations can be provided in auxiliary variables or
  !! through separate SPC files.  If not provided, the default
  !! concentration is zero.  This single routine provides the SSM
  !! bound concentration based on these different approaches.
  !! The mixed flag indicates whether or not
  !!
  !<
  subroutine get_ssm_conc(this, ipackage, ientry, conc, lauxmixed)
    ! -- dummy
    class(GwtSsmType) :: this !< GwtSsmType
    integer(I4B), intent(in) :: ipackage !< package number
    integer(I4B), intent(in) :: ientry !< bound number
    real(DP), intent(out) :: conc !< user-specified concentration for this bound
    logical(LGP), intent(out) :: lauxmixed !< user-specified flag for marking this as a mixed boundary
    ! -- local
    integer(I4B) :: isrctype
    integer(I4B) :: iauxpos

    conc = DZERO
    lauxmixed = .false.
    isrctype = this%isrctype(ipackage)

    select case (isrctype)
    case (1, 2)
      iauxpos = this%iauxpak(ipackage)
      conc = this%fmi%gwfpackages(ipackage)%auxvar(iauxpos, ientry)
      if (isrctype == 2) lauxmixed = .true.
    case (3, 4)
      conc = this%ssmivec(ipackage)%get_value(ientry)
      if (isrctype == 4) lauxmixed = .true.
    end select

    return
  end subroutine get_ssm_conc

  !> @ brief Fill coefficients
  !!
  !! This routine adds the effects of the SSM to the matrix equations by
  !! updating the a matrix and right-hand side vector.
  !!
  !<
  subroutine ssm_fc(this, amatsln, idxglo, rhs)
    ! -- modules
    ! -- dummy
    class(GwtSsmType) :: this
    real(DP), dimension(:), intent(inout) :: amatsln
    integer(I4B), intent(in), dimension(:) :: idxglo
    real(DP), intent(inout), dimension(:) :: rhs
    ! -- local
    integer(I4B) :: ip
    integer(I4B) :: i
    integer(I4B) :: n
    integer(I4B) :: idiag
    integer(I4B) :: nflowpack
    integer(I4B) :: nbound
    real(DP) :: hcofval
    real(DP) :: rhsval
    !
    ! -- do for each flow package
    nflowpack = this%fmi%nflowpack
    do ip = 1, nflowpack
      if (this%fmi%iatp(ip) /= 0) cycle
      !
      ! -- do for each entry in package (ip)
      nbound = this%fmi%gwfpackages(ip)%nbound
      do i = 1, nbound
        n = this%fmi%gwfpackages(ip)%nodelist(i)
        if (n <= 0) cycle
        call this%ssm_term(ip, i, rhsval=rhsval, hcofval=hcofval)
        idiag = idxglo(this%dis%con%ia(n))
        amatsln(idiag) = amatsln(idiag) + hcofval
        rhs(n) = rhs(n) + rhsval
        !
      end do
      !
    end do
    !
    ! -- Return
    return
  end subroutine ssm_fc

  !> @ brief Calculate flow
  !!
  !! Calulate the resulting mass flow between the boundary and the connected
  !! GWT model cell.  Update the diagonal position of the flowja array so that
  !! it ultimately contains the solute balance residual.
  !!
  !<
  subroutine ssm_cq(this, flowja)
    ! -- modules
    ! -- dummy
    class(GwtSsmType) :: this !< GwtSsmType object
    real(DP), dimension(:), contiguous, intent(inout) :: flowja !< flow across each face in the model grid
    ! -- local
    integer(I4B) :: ip
    integer(I4B) :: i
    integer(I4B) :: n
    integer(I4B) :: idiag
    real(DP) :: rate
    !
    ! -- do for each flow package
    do ip = 1, this%fmi%nflowpack
      !
      ! -- cycle if package is being managed as an advanced package
      if (this%fmi%iatp(ip) /= 0) cycle
      !
      ! -- do for each boundary
      do i = 1, this%fmi%gwfpackages(ip)%nbound
        n = this%fmi%gwfpackages(ip)%nodelist(i)
        if (n <= 0) cycle
        call this%ssm_term(ip, i, rrate=rate)
        idiag = this%dis%con%ia(n)
        flowja(idiag) = flowja(idiag) + rate
        !
      end do
      !
    end do
    !
    ! -- Return
    return
  end subroutine ssm_cq

  !> @ brief Calculate the global SSM budget terms
  !!
  !! Calculate the global SSM budget terms using separate in and out entries
  !! for each flow package.
  !!
  !<
  subroutine ssm_bd(this, isuppress_output, model_budget)
    ! -- modules
    use TdisModule, only: delt
    use BudgetModule, only: BudgetType
    ! -- dummy
    class(GwtSsmType) :: this !< GwtSsmType object
    integer(I4B), intent(in) :: isuppress_output !< flag to suppress output
    type(BudgetType), intent(inout) :: model_budget !< budget object for the GWT model
    ! -- local
    character(len=LENBUDROWLABEL) :: rowlabel
    integer(I4B) :: ip
    integer(I4B) :: i
    integer(I4B) :: n
    real(DP) :: rate
    real(DP) :: rin
    real(DP) :: rout
    !
    ! -- do for each flow package, unless it is being handled by an advanced
    !    transport package
    do ip = 1, this%fmi%nflowpack
      !
      ! -- cycle if package is being managed as an advanced package
      if (this%fmi%iatp(ip) /= 0) cycle
      !
      ! -- Initialize the rate accumulators
      rin = DZERO
      rout = DZERO
      !
      ! -- do for each boundary
      do i = 1, this%fmi%gwfpackages(ip)%nbound
        n = this%fmi%gwfpackages(ip)%nodelist(i)
        if (n <= 0) cycle
        call this%ssm_term(ip, i, rrate=rate)
        if (rate < DZERO) then
          rout = rout - rate
        else
          rin = rin + rate
        end if
        !
      end do
      !
      rowlabel = 'SSM_'//adjustl(trim(this%fmi%flowpacknamearray(ip)))
      call model_budget%addentry(rin, rout, delt, &
                                 this%fmi%gwfpackages(ip)%budtxt, &
                                 isuppress_output, rowlabel=rowlabel)
    end do
    !
    ! -- Return
    return
  end subroutine ssm_bd

  !> @ brief Output flows
  !!
  !! Based on user-specified controls, print SSM mass flow rates to the GWT
  !! listing file and/or write the SSM mass flow rates to the GWT binary
  !! budget file.
  !!
  !<
  subroutine ssm_ot_flow(this, icbcfl, ibudfl, icbcun)
    ! -- modules
    use TdisModule, only: kstp, kper
    use ConstantsModule, only: LENPACKAGENAME, LENBOUNDNAME, LENAUXNAME, DZERO
    ! -- dummy
    class(GwtSsmType) :: this !< GwtSsmType object
    integer(I4B), intent(in) :: icbcfl !< flag for writing binary budget terms
    integer(I4B), intent(in) :: ibudfl !< flag for printing budget terms to list file
    integer(I4B), intent(in) :: icbcun !< fortran unit number for binary budget file
    ! -- local
    character(len=LINELENGTH) :: title
    integer(I4B) :: node, nodeu
    character(len=20) :: nodestr
    integer(I4B) :: maxrows
    integer(I4B) :: ip
    integer(I4B) :: i, n2, ibinun
    real(DP) :: rrate
    real(DP) :: qssm
    real(DP) :: cssm
    integer(I4B) :: naux
    real(DP), dimension(0, 0) :: auxvar
    character(len=LENAUXNAME), dimension(0) :: auxname
    ! -- for observations
    character(len=LENBOUNDNAME) :: bname
    ! -- formats
    character(len=*), parameter :: fmttkk = &
      &"(1X,/1X,A,'   PERIOD ',I0,'   STEP ',I0)"
    !
    ! -- set maxrows
    maxrows = 0
    if (ibudfl /= 0 .and. this%iprflow /= 0) then
      call this%outputtab%set_kstpkper(kstp, kper)
      do ip = 1, this%fmi%nflowpack
        if (this%fmi%iatp(ip) /= 0) cycle
        !
        ! -- do for each boundary
        do i = 1, this%fmi%gwfpackages(ip)%nbound
          node = this%fmi%gwfpackages(ip)%nodelist(i)
          if (node > 0) then
            maxrows = maxrows + 1
          end if
        end do
      end do
      if (maxrows > 0) then
        call this%outputtab%set_maxbound(maxrows)
      end if
      title = 'SSM PACKAGE ('//trim(this%packName)// &
              ') FLOW RATES'
      call this%outputtab%set_title(title)
    end if
    !
    ! -- Set unit number for binary output
    if (this%ipakcb < 0) then
      ibinun = icbcun
    else if (this%ipakcb == 0) then
      ibinun = 0
    else
      ibinun = this%ipakcb
    end if
    if (icbcfl == 0) ibinun = 0
    !
    ! -- If cell-by-cell flows will be saved as a list, write header.
    if (ibinun /= 0) then
      naux = 0
      call this%dis%record_srcdst_list_header(text, this%name_model, &
                                              this%name_model, this%name_model, &
                                              this%packName, naux, auxname, &
                                              ibinun, this%nbound, this%iout)
    end if
    !
    ! -- If no boundaries, skip flow calculations.
    if (this%nbound > 0) then
      !
      ! -- Loop through each boundary calculating flow.
      do ip = 1, this%fmi%nflowpack
        if (this%fmi%iatp(ip) /= 0) cycle
        !
        ! -- do for each boundary
        do i = 1, this%fmi%gwfpackages(ip)%nbound
          !
          ! -- Calculate rate for this entry
          node = this%fmi%gwfpackages(ip)%nodelist(i)
          if (node <= 0) cycle
          call this%ssm_term(ip, i, rrate=rrate, qssm=qssm, cssm=cssm)
          !
          ! -- Print the individual rates if the budget is being printed
          !    and PRINT_FLOWS was specified (this%iprflow<0)
          if (ibudfl /= 0) then
            if (this%iprflow /= 0) then
              !
              ! -- set nodestr and write outputtab table
              nodeu = this%dis%get_nodeuser(node)
              call this%dis%nodeu_to_string(nodeu, nodestr)
              bname = this%fmi%gwfpackages(ip)%name
              call this%outputtab%add_term(i)
              call this%outputtab%add_term(trim(adjustl(nodestr)))
              call this%outputtab%add_term(qssm)
              call this%outputtab%add_term(cssm)
              call this%outputtab%add_term(rrate)
              call this%outputtab%add_term(bname)
            end if
          end if
          !
          ! -- If saving cell-by-cell flows in list, write flow
          if (ibinun /= 0) then
            n2 = i
            call this%dis%record_mf6_list_entry(ibinun, node, n2, rrate, &
                                                naux, auxvar(:, i), &
                                                olconv2=.FALSE.)
          end if
          !
        end do
        !
      end do
    end if
    if (ibudfl /= 0) then
      if (this%iprflow /= 0) then
        write (this%iout, '(1x)')
      end if
    end if
    !
    ! -- return
    return
  end subroutine ssm_ot_flow

  !> @ brief Deallocate
  !!
  !! Deallocate the memory associated with this derived type
  !!
  !<
  subroutine ssm_da(this)
    ! -- modules
    use MemoryManagerModule, only: mem_deallocate
    use MemoryManagerExtModule, only: memorylist_remove
    use SimVariablesModule, only: idm_context
    ! -- dummy
    class(GwtSsmType) :: this !< GwtSsmType object
    ! -- local
    integer(I4B) :: ip
    type(GwtSpcType), pointer :: ssmiptr
    !
    ! -- Deallocate package input context
    if (this%inunit > 0) then
      call memorylist_remove(this%name_model, 'SSM', idm_context)
    end if
    !
    ! -- Deallocate the ssmi objects if package was active
    if (this%inunit > 0) then
      do ip = 1, size(this%ssmivec)
        if (this%isrctype(ip) == 3 .or. this%isrctype(ip) == 4) then
          ssmiptr => this%ssmivec(ip)
          call ssmiptr%spc_da()
        end if
      end do
      deallocate (this%ssmivec)
    end if
    !
    ! -- Deallocate arrays if package was active
    if (this%inunit > 0) then
      call mem_deallocate(this%iauxpak)
      call mem_deallocate(this%isrctype)
      this%ibound => null()
      this%fmi => null()
    end if
    !
    ! -- output table object
    if (associated(this%outputtab)) then
      call this%outputtab%table_da()
      deallocate (this%outputtab)
      nullify (this%outputtab)
    end if
    !
    ! -- Scalars
    call mem_deallocate(this%nbound)
    !
    ! -- deallocate parent
    call this%NumericalPackageType%da()
    !
    ! -- Return
    return
  end subroutine ssm_da

  !> @ brief Allocate scalars
  !!
  !! Allocate scalar variables for this derived type
  !!
  !<
  subroutine allocate_scalars(this)
    ! -- modules
    use MemoryManagerModule, only: mem_allocate, mem_setptr
    ! -- dummy
    class(GwtSsmType) :: this !< GwtSsmType object
    ! -- local
    !
    ! -- allocate scalars in NumericalPackageType
    call this%NumericalPackageType%allocate_scalars()
    !
    ! -- Allocate
    call mem_allocate(this%nbound, 'NBOUND', this%memoryPath)
    !
    ! -- Initialize
    this%nbound = 0
    !
    ! -- Return
    return
  end subroutine allocate_scalars

  !> @ brief Allocate arrays
  !!
  !! Allocate array variables for this derived type
  !!
  !<
  subroutine allocate_arrays(this)
    ! -- modules
    use MemoryManagerModule, only: mem_allocate, mem_setptr
    ! -- dummy
    class(GwtSsmType) :: this !< GwtSsmType object
    ! -- local
    integer(I4B) :: nflowpack
    integer(I4B) :: i
    !
    ! -- Allocate
    nflowpack = this%fmi%nflowpack
    call mem_allocate(this%iauxpak, nflowpack, 'IAUXPAK', this%memoryPath)
    call mem_allocate(this%isrctype, nflowpack, 'ISRCTYPE', this%memoryPath)
    !
    ! -- Initialize
    do i = 1, nflowpack
      this%iauxpak(i) = 0
      this%isrctype(i) = 0
    end do
    !
    ! -- Allocate the ssmivec array
    allocate (this%ssmivec(nflowpack))
    !
    ! -- Return
    return
  end subroutine allocate_arrays

  subroutine source_options(this)
! ******************************************************************************
! source_options -- source package options from input context
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use ConstantsModule, only: LENMEMPATH
    use MemoryHelperModule, only: create_mem_path
    use MemoryManagerExtModule, only: mem_set_value
    use SimVariablesModule, only: idm_context
    use GwtSsmInputModule, only: GwtSsmParamFoundType
    ! -- dummy
    class(GwtSsmType) :: this
    ! -- locals
    character(len=LENMEMPATH) :: idmMemoryPath
    type(GwtSsmParamFoundType) :: found
! ------------------------------------------------------------------------------
    !
    ! -- set input context memory path
    idmMemoryPath = create_mem_path(this%name_model, 'SSM', idm_context)
    !
    ! -- source from input context
    call mem_set_value(this%iprflow, 'IPRFLOW', idmMemoryPath, found%iprflow)
    call mem_set_value(this%ipakcb, 'IPAKCB', idmMemoryPath, found%ipakcb)
    !
    ! -- save flows
    if (found%ipakcb) then
      this%ipakcb = -1
    end if
    !
    ! -- log options
    if (this%iout > 0) then
      call this%log_options(found)
    end if
    !
    ! -- Return
    return
  end subroutine source_options

  !> @brief Write user options to list file
  !<
  subroutine log_options(this, found)
    use GwtSsmInputModule, only: GwtSsmParamFoundType
    class(GwtSsmType) :: this
    type(GwtSsmParamFoundType), intent(in) :: found
    ! -- formats
    character(len=*), parameter :: fmtiprflow = &
      "(4x,'SSM FLOW INFORMATION WILL BE PRINTED TO LISTING FILE &
      &WHENEVER ICBCFL IS NOT ZERO.')"
    character(len=*), parameter :: fmtisvflow = &
      "(4x,'CELL-BY-CELL FLOW INFORMATION WILL BE SAVED TO BINARY FILE &
      &WHENEVER ICBCFL IS NOT ZERO.')"

    write (this%iout, '(1x,a)') 'Setting SSM Options'

    if (found%iprflow) then
      write (this%iout, fmtiprflow)
    end if

    if (found%ipakcb) then
      write (this%iout, fmtisvflow)
    end if

    write (this%iout, '(1x,a)') 'End Setting SSM Options'
  end subroutine log_options

  subroutine set_package_index(this, pkgname, index_package)
! ******************************************************************************
! set_package_index -- identify and set package index based on name
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use CharacterStringModule, only: CharacterStringType
    ! -- dummy
    class(GwtSsmType) :: this
    character(len=LINELENGTH), intent(in) :: pkgname
    integer(I4B), intent(inout) :: index_package
    ! -- locals
    integer(I4B) :: ipkg
    logical :: pkgfound
! ------------------------------------------------------------------------------
    !
    ! -- initialization
    index_package = 0
    pkgfound = .false.
    !
    ! -- identify index of package
    do ipkg = 1, this%fmi%nflowpack
      if (trim(adjustl(this%fmi%gwfpackages(ipkg)%name)) == pkgname) then
        pkgfound = .true.
        exit
      end if
    end do
    !
    ! -- set error if package not found
    if (.not. pkgfound) then
      write (errmsg, '(1x, a, a)') 'FLOW PACKAGE CANNOT BE FOUND: ', &
        trim(pkgname)
      call store_error(errmsg)
      call this%parser%StoreErrorUnit()
      !
      ! -- Ensure package was not specified more than once in SOURCES block
    else if (this%isrctype(ipkg) /= 0) then
      write (errmsg, '(1x, a, a)') &
        'A PACKAGE CANNOT BE SPECIFIED MORE THAN ONCE IN THE SSM SOURCES &
        &BLOCK.  THE FOLLOWING PACKAGE WAS SPECIFIED MORE THAN ONCE: ', &
        trim(pkgname)
      call store_error(errmsg)
      call this%parser%StoreErrorUnit()
      !
      ! -- set valid index
    else
      index_package = ipkg
    end if
  end subroutine set_package_index

  subroutine set_ssm_source_type(this, pname, srctype, auxname)
! ******************************************************************************
! set_ssm_source_type -- set a single ssm source type
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use CharacterStringModule, only: CharacterStringType
    ! -- dummy
    class(GwtSsmType) :: this
    ! -- locals
    type(CharacterStringType), intent(in) :: pname
    type(CharacterStringType), intent(in) :: srctype
    type(CharacterStringType), intent(in) :: auxname
    character(len=LINELENGTH) :: pkgname, sourcetype, aux
    integer(I4B) :: isrctype
    integer(I4B) :: ipkg, iaux
    logical :: auxfound
! ------------------------------------------------------------------------------
    !
    ! -- assign CharacterString to string
    pkgname = pname
    sourcetype = srctype
    aux = auxname
    !
    ! -- initialize locals
    auxfound = .false.
    !
    ! -- identify index of package
    call this%set_package_index(pkgname, ipkg)
    if (ipkg == 0) then
      !
      ! -- error condition; error is set
      return
    end if
    !
    ! --
    select case (sourcetype)
    case ('AUX')
      write (this%iout, '(1x,a)') 'AUX SOURCE DETECTED.'
      isrctype = 1
    case ('AUXMIXED')
      write (this%iout, '(1x,a)') 'AUXMIXED SOURCE DETECTED.'
      isrctype = 2
    case default
      write (errmsg, '(1x, a, a)') &
        'SRCTYPE MUST BE AUX OR AUXMIXED.  FOUND: ', trim(sourcetype)
      call store_error(errmsg)
      call this%parser%StoreErrorUnit()
    end select
    !
    ! -- Store the source type (1 or 2)
    this%isrctype(ipkg) = isrctype
    !
    ! -- Find and store the auxiliary column
    do iaux = 1, this%fmi%gwfpackages(ipkg)%naux
      if (trim(this%fmi%gwfpackages(ipkg)%auxname(iaux)) == &
          trim(aux)) then
        auxfound = .true.
        exit
      end if
    end do
    !
    ! -- set error if aux not found
    if (.not. auxfound) then
      write (errmsg, '(1x, a, a)') &
        'AUXILIARY NAME CANNOT BE FOUND: ', trim(aux)
      call store_error(errmsg)
      call this%parser%StoreErrorUnit()
    end if
    !
    ! -- set iauxpak and write message
    this%iauxpak(ipkg) = iaux
    write (this%iout, '(4x, a, i0, a, a)') 'USING AUX COLUMN ', &
      iaux, ' IN PACKAGE ', trim(pkgname)
  end subroutine set_ssm_source_type

  subroutine source_sources(this)
! ******************************************************************************
! source_sources -- source ssm sources from input context
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use ConstantsModule, only: LENMEMPATH
    use MemoryHelperModule, only: create_mem_path
    use MemoryManagerModule, only: mem_setptr
    use SimVariablesModule, only: idm_context
    use CharacterStringModule, only: CharacterStringType
    use GwtSsmInputModule, only: GwtSsmParamFoundType
    ! -- dummy
    class(GwtSsmType) :: this
    ! -- locals
    character(len=LENMEMPATH) :: idmMemoryPath
    type(CharacterStringType), dimension(:), pointer, contiguous :: pname
    type(CharacterStringType), dimension(:), pointer, contiguous :: srctype
    type(CharacterStringType), dimension(:), pointer, contiguous :: auxname
    integer(I4B) :: i
! ------------------------------------------------------------------------------
    !
    ! -- set input context memory path
    idmMemoryPath = create_mem_path(this%name_model, 'SSM', idm_context)
    !
    ! -- set pointers to variable describing files
    call mem_setptr(pname, 'PNAME_SOURCES', idmMemoryPath)
    call mem_setptr(srctype, 'SRCTYPE', idmMemoryPath)
    call mem_setptr(auxname, 'AUXNAME', idmMemoryPath)
    !
    ! -- set each source type
    do i = 1, size(pname)
      call this%set_ssm_source_type(pname(i), srctype(i), auxname(i))
    end do
    !
    ! -- return
    return
  end subroutine source_sources

  subroutine set_ssm_file_input(this, pname, srctype, iotype, filename, mix)
! ******************************************************************************
! set_ssm_file_input -- set a single ssm file input
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use InputOutputModule, only: openfile, getunit
    use CharacterStringModule, only: CharacterStringType
    ! -- dummy
    class(GwtSsmType) :: this
    ! -- locals
    type(CharacterStringType), intent(in) :: pname
    type(CharacterStringType), intent(in) :: srctype
    type(CharacterStringType), intent(in) :: iotype
    type(CharacterStringType), intent(in) :: filename
    type(CharacterStringType), intent(in) :: mix
    character(len=LINELENGTH) :: pkgname, sourcetype, io, fname, mixed
    integer(I4B) :: isrctype
    integer(I4B) :: ipkg
    type(GwtSpcType), pointer :: ssmiptr
    integer(I4B) :: inunit
! ------------------------------------------------------------------------------
    !
    ! -- assign CharacterString to string
    pkgname = pname
    sourcetype = srctype
    io = iotype
    fname = filename
    mixed = mix
    !
    ! -- identify index of package
    call this%set_package_index(pkgname, ipkg)
    if (ipkg == 0) then
      !
      ! -- error condition; error is set
      return
    end if
    !
    ! --
    select case (sourcetype)
    case ('SPC6')
      write (this%iout, '(1x,a)') 'SPC6 SOURCE DETECTED.'
      isrctype = 3
      !
      ! verify filein keyword
      if (trim(adjustl(io)) /= 'FILEIN') then
        errmsg = 'SPC6 keyword must be followed by "FILEIN" '// &
                 'then by filename and optionally by <MIXED>.'
        call store_error(errmsg)
        call this%parser%StoreErrorUnit()
      end if
      !
      ! -- open the file
      inunit = getunit()
      call openfile(inunit, this%iout, fname, 'SPC', filstat_opt='OLD')
      !
      ! -- create the SPC file object
      ssmiptr => this%ssmivec(ipkg)
      call ssmiptr%initialize(this%dis, ipkg, inunit, this%iout, &
                              this%name_model, trim(pkgname))
      !
      ! -- log file and package
      write (this%iout, '(4x, a, a, a, a)') 'USING SPC INPUT FILE ', &
        trim(fname), ' TO SET CONCENTRATIONS FOR PACKAGE ', trim(pkgname)
      !
      ! -- check for optional MIXED keyword and set isrctype to 4 if found
      if (trim(mixed) == 'MIXED') then
        isrctype = 4
        write (this%iout, '(1x,a,a)') 'ASSIGNED MIXED SSM TYPE TO PACKAGE ', &
          trim(pkgname)
      end if
    case default
      write (errmsg, '(1x, a, a)') &
        'SRCTYPE MUST BE SPC6.  FOUND: ', trim(sourcetype)
      call store_error(errmsg)
      call this%parser%StoreErrorUnit()
    end select
    !
    ! -- Store the source type (3 or 4)
    this%isrctype(ipkg) = isrctype
  end subroutine set_ssm_file_input

  subroutine source_fileinput(this)
! ******************************************************************************
! source_fileinput -- source ssm fileinput from input context
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use ConstantsModule, only: LENMEMPATH
    use MemoryTypeModule, only: MemoryType
    use MemoryManagerModule, only: get_from_memorylist
    use MemoryHelperModule, only: create_mem_path
    use MemoryManagerModule, only: mem_setptr
    use SimVariablesModule, only: idm_context
    use CharacterStringModule, only: CharacterStringType
    use GwtSsmInputModule, only: GwtSsmParamFoundType
    ! -- dummy
    class(GwtSsmType) :: this
    ! -- locals
    character(len=LENMEMPATH) :: idmMemoryPath
    type(CharacterStringType), dimension(:), pointer, contiguous :: pname
    type(CharacterStringType), dimension(:), pointer, contiguous :: srctype
    type(CharacterStringType), dimension(:), pointer, contiguous :: filein
    type(CharacterStringType), dimension(:), pointer, contiguous :: fname
    type(CharacterStringType), dimension(:), pointer, contiguous :: mixed
    integer(I4B) :: i
    logical(LGP) :: found
    type(MemoryType), pointer :: mt
! ------------------------------------------------------------------------------
    !
    ! -- set input context memory path
    idmMemoryPath = create_mem_path(this%name_model, 'SSM', idm_context)
    !
    ! -- fileinput is optional block, check if PNAME_FILEINPUT in memory
    call get_from_memorylist('PNAME_FILEINPUT', idmMemoryPath, mt, found, .false.)
    !
    ! -- set pointers to variable describing files
    if (found) then
      call mem_setptr(pname, 'PNAME_FILEINPUT', idmMemoryPath)
      call mem_setptr(srctype, 'SPC6', idmMemoryPath)
      call mem_setptr(filein, 'FILEIN', idmMemoryPath)
      call mem_setptr(fname, 'SPC6_FILENAME', idmMemoryPath)
      call mem_setptr(mixed, 'MIXED', idmMemoryPath)
    else
      !
      ! -- nothing to do
      return
    end if

    do i = 1, size(pname)
      call this%set_ssm_file_input(pname(i), srctype(i), filein(i), fname(i), &
                                   mixed(i))
    end do
    !
    ! -- return
    return
  end subroutine source_fileinput

  !> @ brief Setup the output table
  !!
  !! Setup the output table by creating the column headers.
  !!
  !<
  subroutine pak_setup_outputtab(this)
    ! -- dummy
    class(GwtSsmtype), intent(inout) :: this
    ! -- local
    character(len=LINELENGTH) :: title
    character(len=LINELENGTH) :: text
    integer(I4B) :: ntabcol
    !
    ! -- allocate and initialize the output table
    if (this%iprflow /= 0) then
      !
      ! -- dimension table
      ntabcol = 6
      !if (this%inamedbound > 0) then
      !  ntabcol = ntabcol + 1
      !end if
      !
      ! -- initialize the output table object
      title = 'SSM PACKAGE ('//trim(this%packName)// &
              ') FLOW RATES'
      call table_cr(this%outputtab, this%packName, title)
      call this%outputtab%table_df(1, ntabcol, this%iout, transient=.TRUE.)
      text = 'NUMBER'
      call this%outputtab%initialize_column(text, 10, alignment=TABCENTER)
      text = 'CELLID'
      call this%outputtab%initialize_column(text, 20, alignment=TABLEFT)
      text = 'BOUND Q'
      call this%outputtab%initialize_column(text, 15, alignment=TABCENTER)
      text = 'SSM CONC'
      call this%outputtab%initialize_column(text, 15, alignment=TABCENTER)
      text = 'RATE'
      call this%outputtab%initialize_column(text, 15, alignment=TABCENTER)
      text = 'PACKAGE NAME'
      call this%outputtab%initialize_column(text, 16, alignment=TABCENTER)
      !if (this%inamedbound > 0) then
      !  text = 'NAME'
      !  call this%outputtab%initialize_column(text, 20, alignment=TABLEFT)
      !end if
    end if
    !
    ! -- return
    return
  end subroutine pak_setup_outputtab

end module GwtSsmModule
