module GwtFmiModule

  use KindModule, only: DP, I4B
  use ConstantsModule, only: DONE, DZERO, DHALF, LINELENGTH, LENBUDTXT, &
                             LENPACKAGENAME
  use SimModule, only: store_error, store_error_unit
  use SimVariablesModule, only: errmsg
  use NumericalPackageModule, only: NumericalPackageType
  use BaseDisModule, only: DisBaseType
  use ListModule, only: ListType
  use BudgetFileReaderModule, only: BudgetFileReaderType
  use HeadFileReaderModule, only: HeadFileReaderType
  use PackageBudgetModule, only: PackageBudgetType
  use BudgetObjectModule, only: BudgetObjectType, budgetobject_cr_bfr

  implicit none
  private
  public :: GwtFmiType
  public :: fmi_cr

  integer(I4B), parameter :: NBDITEMS = 2
  character(len=LENBUDTXT), dimension(NBDITEMS) :: budtxt
  data budtxt/'      FLOW-ERROR', ' FLOW-CORRECTION'/

  type :: DataAdvancedPackageType
    real(DP), dimension(:), contiguous, pointer :: concpack => null()
    real(DP), dimension(:), contiguous, pointer :: qmfrommvr => null()
  end type

  type :: BudObjPtrArray
    type(BudgetObjectType), pointer :: ptr
  end type BudObjPtrArray

  type, extends(NumericalPackageType) :: GwtFmiType

    logical, pointer :: flows_from_file => null() !< if .false., then flows come from GWF through GWF-GWT exg
    integer(I4B), dimension(:), pointer, contiguous :: iatp => null() !< advanced transport package applied to gwfpackages
    type(ListType), pointer :: gwfbndlist => null() !< list of gwf stress packages
    integer(I4B), pointer :: iflowsupdated => null() !< flows were updated for this time step
    integer(I4B), pointer :: iflowerr => null() !< add the flow error correction
    real(DP), dimension(:), pointer, contiguous :: flowcorrect => null() !< mass flow correction
    integer(I4B), dimension(:), pointer, contiguous :: ibound => null() !< pointer to GWT ibound
    real(DP), dimension(:), pointer, contiguous :: gwfflowja => null() !< pointer to the GWF flowja array
    real(DP), dimension(:, :), pointer, contiguous :: gwfspdis => null() !< pointer to npf specific discharge array
    real(DP), dimension(:), pointer, contiguous :: gwfhead => null() !< pointer to the GWF head array
    real(DP), dimension(:), pointer, contiguous :: gwfsat => null() !< pointer to the GWF saturation array
    integer(I4B), dimension(:), pointer, contiguous :: ibdgwfsat0 => null() !< mark cells with saturation = 0 to exclude from dispersion
    real(DP), dimension(:), pointer, contiguous :: gwfstrgss => null() !< pointer to flow model QSTOSS
    real(DP), dimension(:), pointer, contiguous :: gwfstrgsy => null() !< pointer to flow model QSTOSY
    integer(I4B), pointer :: igwfstrgss => null() !< indicates if gwfstrgss is available
    integer(I4B), pointer :: igwfstrgsy => null() !< indicates if gwfstrgsy is available
    integer(I4B), pointer :: iubud => null() !< unit number GWF budget file
    integer(I4B), pointer :: iuhds => null() !< unit number GWF head file
    integer(I4B), pointer :: iumvr => null() !< unit number GWF mover budget file
    integer(I4B), pointer :: nflowpack => null() !< number of GWF flow packages
    integer(I4B), dimension(:), pointer, contiguous :: igwfmvrterm => null() !< flag to indicate that gwf package is a mover term
    type(BudgetFileReaderType) :: bfr !< budget file reader
    type(HeadFileReaderType) :: hfr !< head file reader
    type(PackageBudgetType), dimension(:), allocatable :: gwfpackages !< used to get flows between a package and gwf
    type(BudgetObjectType), pointer :: mvrbudobj => null() !< pointer to the mover budget budget object
    type(DataAdvancedPackageType), &
      dimension(:), pointer, contiguous :: datp => null()
    character(len=16), dimension(:), allocatable :: flowpacknamearray !< array of boundary package names (e.g. LAK-1, SFR-3, etc.)
    type(BudObjPtrArray), dimension(:), allocatable :: aptbudobj !< flow budget objects for the advanced packages
  contains

    procedure :: fmi_df
    procedure :: fmi_ar
    procedure :: fmi_rp
    procedure :: fmi_ad
    procedure :: fmi_fc
    procedure :: fmi_cq
    procedure :: fmi_bd
    procedure :: fmi_ot_flow
    procedure :: fmi_da
    procedure :: allocate_scalars
    procedure :: allocate_arrays
    procedure :: gwfsatold
    procedure :: read_options
    procedure :: read_packagedata
    procedure :: initialize_bfr
    procedure :: advance_bfr
    procedure :: finalize_bfr
    procedure :: initialize_hfr
    procedure :: advance_hfr
    procedure :: finalize_hfr
    procedure :: initialize_gwfterms_from_bfr
    procedure :: initialize_gwfterms_from_gwfbndlist
    procedure :: allocate_gwfpackages
    procedure :: deallocate_gwfpackages
    procedure :: get_package_index
    procedure :: set_aptbudobj_pointer

  end type GwtFmiType

contains

  subroutine fmi_cr(fmiobj, name_model, inunit, iout)
! ******************************************************************************
! fmi_cr -- Create a new FMI object
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    type(GwtFmiType), pointer :: fmiobj
    character(len=*), intent(in) :: name_model
    integer(I4B), intent(in) :: inunit
    integer(I4B), intent(in) :: iout
! ------------------------------------------------------------------------------
    !
    ! -- Create the object
    allocate (fmiobj)
    !
    ! -- create name and memory path
    call fmiobj%set_names(1, name_model, 'FMI', 'FMI')
    !
    ! -- Allocate scalars
    call fmiobj%allocate_scalars()
    !
    ! -- if inunit == 0, then there is no file to read, but it still needs
    !    to be active in order to manage pointers to gwf model
    !if (inunit == 0) inunit = 1
    !
    ! -- Set variables
    fmiobj%inunit = inunit
    fmiobj%iout = iout
    !
    ! -- Initialize block parser
    call fmiobj%parser%Initialize(fmiobj%inunit, fmiobj%iout)
    !
    ! -- Return
    return
  end subroutine fmi_cr

  subroutine fmi_df(this, dis, inssm)
! ******************************************************************************
! fmi_df -- Define
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use SimModule, only: store_error
    ! -- dummy
    class(GwtFmiType) :: this
    class(DisBaseType), pointer, intent(in) :: dis
    integer(I4B), intent(in) :: inssm
    ! -- local
    ! -- formats
    character(len=*), parameter :: fmtfmi = &
      "(1x,/1x,'FMI -- FLOW MODEL INTERFACE, VERSION 1, 8/29/2017', &
      &' INPUT READ FROM UNIT ', i0, //)"
    character(len=*), parameter :: fmtfmi0 = &
      &"(1x,/1x,'FMI -- FLOW MODEL INTERFACE, VERSION 1, 8/29/2017')"
! ------------------------------------------------------------------------------
    !
    ! --print a message identifying the FMI package.
    if (this%iout > 0) then
      if (this%inunit /= 0) then
        write (this%iout, fmtfmi) this%inunit
      else
        write (this%iout, fmtfmi0)
        if (this%flows_from_file) then
          write (this%iout, '(a)') '  FLOWS ARE ASSUMED TO BE ZERO.'
        else
          write (this%iout, '(a)') '  FLOWS PROVIDED BY A GWF MODEL IN THIS &
            &SIMULATION'
        end if
      end if
    end if
    !
    ! -- store pointers to arguments that were passed in
    this%dis => dis
    !
    ! -- Read fmi options
    if (this%inunit /= 0) then
      call this%read_options()
    end if
    !
    ! -- Read packagedata options
    if (this%inunit /= 0 .and. this%flows_from_file) then
      call this%read_packagedata()
      call this%initialize_gwfterms_from_bfr()
    end if
    !
    ! -- If GWF-GWT exchange is active, then setup gwfterms from bndlist
    if (.not. this%flows_from_file) then
      call this%initialize_gwfterms_from_gwfbndlist()
    end if
    !
    ! -- Make sure that ssm is on if there are any boundary packages
    if (inssm == 0) then
      if (this%nflowpack > 0) then
        call store_error('FLOW MODEL HAS BOUNDARY PACKAGES, BUT THERE &
          &IS NO SSM PACKAGE.  THE SSM PACKAGE MUST BE ACTIVATED.', &
          terminate=.TRUE.)
      end if
    end if
    !
    ! -- Return
    return
  end subroutine fmi_df

  subroutine fmi_ar(this, ibound)
! ******************************************************************************
! fmi_ar -- Allocate and Read
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use SimModule, only: store_error
    ! -- dummy
    class(GwtFmiType) :: this
    integer(I4B), dimension(:), pointer, contiguous :: ibound
    ! -- local
    ! -- formats
! ------------------------------------------------------------------------------
    !
    ! -- store pointers to arguments that were passed in
    this%ibound => ibound
    !
    ! -- Allocate arrays
    call this%allocate_arrays(this%dis%nodes)
    !
    ! -- Return
    return
  end subroutine fmi_ar

  subroutine fmi_rp(this, inmvr)
! ******************************************************************************
! fmi_rp -- Read and prepare
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use TdisModule, only: kper, kstp
    ! -- dummy
    class(GwtFmiType) :: this
    integer(I4B), intent(in) :: inmvr
    ! -- local
    ! -- formats
! ------------------------------------------------------------------------------
    !
    ! --Check to make sure MVT Package is active if mvr flows are available.
    !   This cannot be checked until RP because exchange doesn't set a pointer
    !   to mvrbudobj until exg_ar().
    if (kper * kstp == 1) then
      if (associated(this%mvrbudobj) .and. inmvr == 0) then
        write (errmsg, '(4x,a)') 'GWF WATER MOVER IS ACTIVE BUT THE GWT MVT &
          &PACKAGE HAS NOT BEEN SPECIFIED.  ACTIVATE GWT MVT PACKAGE.'
        call store_error(errmsg, terminate=.TRUE.)
      end if
      if (.not. associated(this%mvrbudobj) .and. inmvr > 0) then
        write (errmsg, '(4x,a)') 'GWF WATER MOVER TERMS ARE NOT AVAILABLE &
          &BUT THE GWT MVT PACKAGE HAS BEEN ACTIVATED.  GWF-GWT EXCHANGE &
          &OR SPECIFY GWFMOVER IN FMI PACKAGEDATA.'
        call store_error(errmsg, terminate=.TRUE.)
      end if
    end if
    !
    ! -- Return
    return
  end subroutine fmi_rp

  subroutine fmi_ad(this, cnew)
! ******************************************************************************
! fmi_ad -- advance
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use ConstantsModule, only: DHDRY
    ! -- dummy
    class(GwtFmiType) :: this
    real(DP), intent(inout), dimension(:) :: cnew
    ! -- local
    integer(I4B) :: n
    integer(I4B) :: m
    integer(I4B) :: ipos
    real(DP) :: crewet, tflow, flownm
    character(len=15) :: nodestr
    character(len=*), parameter :: fmtdry = &
     &"(/1X,'WARNING: DRY CELL ENCOUNTERED AT ',a,';  RESET AS INACTIVE &
     &WITH DRY CONCENTRATION = ', G13.5)"
    character(len=*), parameter :: fmtrewet = &
     &"(/1X,'DRY CELL REACTIVATED AT ', a,&
     &' WITH STARTING CONCENTRATION =',G13.5)"
! ------------------------------------------------------------------------------
    !
    ! -- Set flag to indicated that flows are being updated.  For the case where
    !    flows may be reused (only when flows are read from a file) then set
    !    the flag to zero to indicated that flows were not updated
    this%iflowsupdated = 1
    !
    ! -- If reading flows from a budget file, read the next set of records
    if (this%iubud /= 0) then
      call this%advance_bfr()
    end if
    !
    ! -- If reading heads from a head file, read the next set of records
    if (this%iuhds /= 0) then
      call this%advance_hfr()
    end if
    !
    ! -- If mover flows are being read from file, read the next set of records
    if (this%iumvr /= 0) then
      call this%mvrbudobj%bfr_advance(this%dis, this%iout)
    end if
    !
    ! -- If advanced package flows are being read from file, read the next set of records
    if (this%flows_from_file .and. this%inunit /= 0) then
      do n = 1, size(this%aptbudobj)
        call this%aptbudobj(n)%ptr%bfr_advance(this%dis, this%iout)
      end do
    end if
    !
    ! -- if flow cell is dry, then set gwt%ibound = 0 and conc to dry
    do n = 1, this%dis%nodes
      !
      ! -- Calculate the ibound-like array that has 0 if saturation
      !    is zero and 1 otherwise
      if (this%gwfsat(n) > DZERO) then
        this%ibdgwfsat0(n) = 1
      else
        this%ibdgwfsat0(n) = 0
      end if
      !
      ! -- Check if active transport cell is inactive for flow
      if (this%ibound(n) > 0) then
        if (this%gwfhead(n) == DHDRY) then
          ! -- transport cell should be made inactive
          this%ibound(n) = 0
          cnew(n) = DHDRY
          call this%dis%noder_to_string(n, nodestr)
          write (this%iout, fmtdry) trim(nodestr), DHDRY
        end if
      end if
      !
      ! -- Convert dry transport cell to active if flow has rewet
      if (cnew(n) == DHDRY) then
        if (this%gwfhead(n) /= DHDRY) then
          !
          ! -- obtain weighted concentration
          crewet = DZERO
          tflow = DZERO
          do ipos = this%dis%con%ia(n) + 1, this%dis%con%ia(n + 1) - 1
            m = this%dis%con%ja(ipos)
            flownm = this%gwfflowja(ipos)
            if (flownm > 0) then
              if (this%ibound(m) /= 0) then
                crewet = crewet + cnew(m) * flownm
                tflow = tflow + this%gwfflowja(ipos)
              end if
            end if
          end do
          if (tflow > DZERO) then
            crewet = crewet / tflow
          else
            crewet = DZERO
          end if
          !
          ! -- cell is now wet
          this%ibound(n) = 1
          cnew(n) = crewet
          call this%dis%noder_to_string(n, nodestr)
          write (this%iout, fmtrewet) trim(nodestr), crewet
        end if
      end if
    end do
    !
    ! -- Return
    return
  end subroutine fmi_ad

  subroutine fmi_fc(this, nodes, cold, nja, njasln, amatsln, idxglo, rhs)
! ******************************************************************************
! fmi_fc -- Calculate coefficients and fill amat and rhs
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    !use BndModule,              only: BndType, GetBndFromList
    ! -- dummy
    class(GwtFmiType) :: this
    integer, intent(in) :: nodes
    real(DP), intent(in), dimension(nodes) :: cold
    integer(I4B), intent(in) :: nja
    integer(I4B), intent(in) :: njasln
    real(DP), dimension(njasln), intent(inout) :: amatsln
    integer(I4B), intent(in), dimension(nja) :: idxglo
    real(DP), intent(inout), dimension(nodes) :: rhs
    ! -- local
    integer(I4B) :: n, ipos, idiag
! ------------------------------------------------------------------------------
    !
    ! -- Calculate the flow imbalance error and make a correction for it
    if (this%iflowerr /= 0) then
      !
      ! -- Correct the transport solution for the flow imbalance by adding
      !    the flow residual to the diagonal
      do n = 1, nodes
        idiag = idxglo(this%dis%con%ia(n))
        ipos = this%dis%con%ia(n)
        amatsln(idiag) = amatsln(idiag) - this%gwfflowja(ipos)
      end do
    end if
    !
    ! -- Return
    return
  end subroutine fmi_fc

  subroutine fmi_cq(this, cnew, flowja)
! ******************************************************************************
! fmi_cq -- Calculate flow correction
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(GwtFmiType) :: this
    real(DP), intent(in), dimension(:) :: cnew
    real(DP), dimension(:), contiguous, intent(inout) :: flowja
    ! -- local
    integer(I4B) :: n
    integer(I4B) :: idiag
    real(DP) :: rate
! ------------------------------------------------------------------------------
    !
    ! -- If not adding flow error correction, return
    if (this%iflowerr /= 0) then
      !
      ! -- Accumulate the flow correction term
      do n = 1, this%dis%nodes
        rate = DZERO
        idiag = this%dis%con%ia(n)
        if (this%ibound(n) > 0) then
          rate = -this%gwfflowja(idiag) * cnew(n)
        end if
        this%flowcorrect(n) = rate
        flowja(idiag) = flowja(idiag) + rate
      end do
    end if
    !
    ! -- Return
    return
  end subroutine fmi_cq

  subroutine fmi_bd(this, isuppress_output, model_budget)
! ******************************************************************************
! mst_bd -- Calculate budget terms
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use TdisModule, only: delt
    use BudgetModule, only: BudgetType, rate_accumulator
    ! -- dummy
    class(GwtFmiType) :: this
    integer(I4B), intent(in) :: isuppress_output
    type(BudgetType), intent(inout) :: model_budget
    ! -- local
    real(DP) :: rin
    real(DP) :: rout
! ------------------------------------------------------------------------------
    !
    ! -- flow correction
    if (this%iflowerr /= 0) then
      call rate_accumulator(this%flowcorrect, rin, rout)
      call model_budget%addentry(rin, rout, delt, budtxt(2), isuppress_output)
    end if
    !
    ! -- Return
    return
  end subroutine fmi_bd

  subroutine fmi_ot_flow(this, icbcfl, icbcun)
! ******************************************************************************
! fmi_ot_flow -- Save budget terms
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(GwtFmiType) :: this
    integer(I4B), intent(in) :: icbcfl
    integer(I4B), intent(in) :: icbcun
    ! -- local
    integer(I4B) :: ibinun
    integer(I4B) :: iprint, nvaluesp, nwidthp
    character(len=1) :: cdatafmp = ' ', editdesc = ' '
    real(DP) :: dinact
! ------------------------------------------------------------------------------
    !
    ! -- Set unit number for binary output
    if (this%ipakcb < 0) then
      ibinun = icbcun
    elseif (this%ipakcb == 0) then
      ibinun = 0
    else
      ibinun = this%ipakcb
    end if
    if (icbcfl == 0) ibinun = 0
    !
    ! -- Do not save flow corrections if not active
    if (this%iflowerr == 0) ibinun = 0
    !
    ! -- Record the storage rates if requested
    if (ibinun /= 0) then
      iprint = 0
      dinact = DZERO
      !
      ! -- flow correction
      call this%dis%record_array(this%flowcorrect, this%iout, iprint, -ibinun, &
                                 budtxt(2), cdatafmp, nvaluesp, &
                                 nwidthp, editdesc, dinact)
    end if
    !
    ! -- Return
    return
  end subroutine fmi_ot_flow

  subroutine fmi_da(this)
! ******************************************************************************
! fmi_da -- Deallocate variables
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryManagerModule, only: mem_deallocate
    ! -- dummy
    class(GwtFmiType) :: this
! ------------------------------------------------------------------------------
    ! -- todo: finalize hfr and bfr either here or in a finalize routine
    !
    ! -- deallocate any memory stored with gwfpackages
    call this%deallocate_gwfpackages()
    !
    ! -- deallocate fmi arrays
    if (associated(this%datp)) then
      deallocate (this%datp)
      deallocate (this%gwfpackages)
      deallocate (this%flowpacknamearray)
      call mem_deallocate(this%iatp)
      call mem_deallocate(this%igwfmvrterm)
    end if

    deallocate (this%aptbudobj)
    call mem_deallocate(this%flowcorrect)
    call mem_deallocate(this%ibdgwfsat0)
    if (this%flows_from_file) then
      call mem_deallocate(this%gwfflowja)
      call mem_deallocate(this%gwfsat)
      call mem_deallocate(this%gwfhead)
      call mem_deallocate(this%gwfstrgss)
      call mem_deallocate(this%gwfstrgsy)
      call mem_deallocate(this%gwfspdis)
    end if
    !
    ! -- deallocate scalars
    call mem_deallocate(this%flows_from_file)
    call mem_deallocate(this%iflowsupdated)
    call mem_deallocate(this%iflowerr)
    call mem_deallocate(this%igwfstrgss)
    call mem_deallocate(this%igwfstrgsy)
    call mem_deallocate(this%iubud)
    call mem_deallocate(this%iuhds)
    call mem_deallocate(this%iumvr)
    call mem_deallocate(this%nflowpack)
    !
    ! -- deallocate parent
    call this%NumericalPackageType%da()
    !
    ! -- Return
    return
  end subroutine fmi_da

  subroutine allocate_scalars(this)
! ******************************************************************************
! allocate_scalars
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryManagerModule, only: mem_allocate, mem_setptr
    ! -- dummy
    class(GwtFmiType) :: this
    ! -- local
! ------------------------------------------------------------------------------
    !
    ! -- allocate scalars in NumericalPackageType
    call this%NumericalPackageType%allocate_scalars()
    !
    ! -- Allocate
    call mem_allocate(this%flows_from_file, 'FLOWS_FROM_FILE', this%memoryPath)
    call mem_allocate(this%iflowsupdated, 'IFLOWSUPDATED', this%memoryPath)
    call mem_allocate(this%iflowerr, 'IFLOWERR', this%memoryPath)
    call mem_allocate(this%igwfstrgss, 'IGWFSTRGSS', this%memoryPath)
    call mem_allocate(this%igwfstrgsy, 'IGWFSTRGSY', this%memoryPath)
    call mem_allocate(this%iubud, 'IUBUD', this%memoryPath)
    call mem_allocate(this%iuhds, 'IUHDS', this%memoryPath)
    call mem_allocate(this%iumvr, 'IUMVR', this%memoryPath)
    call mem_allocate(this%nflowpack, 'NFLOWPACK', this%memoryPath)
    !
    ! -- Although not a scalar, allocate the advanced package transport
    !    budget object to zero so that it can be dynamically resized later
    allocate (this%aptbudobj(0))
    !
    ! -- Initialize
    this%flows_from_file = .true.
    this%iflowsupdated = 1
    this%iflowerr = 0
    this%igwfstrgss = 0
    this%igwfstrgsy = 0
    this%iubud = 0
    this%iuhds = 0
    this%iumvr = 0
    this%nflowpack = 0
    !
    ! -- Return
    return
  end subroutine allocate_scalars

  subroutine allocate_arrays(this, nodes)
! ******************************************************************************
! allocate_arrays
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use MemoryManagerModule, only: mem_allocate
    !modules
    use ConstantsModule, only: DZERO
    ! -- dummy
    class(GwtFmiType) :: this
    integer(I4B), intent(in) :: nodes
    ! -- local
    integer(I4B) :: n
! ------------------------------------------------------------------------------
    !
    ! -- Allocate variables needed for all cases
    if (this%iflowerr == 0) then
      call mem_allocate(this%flowcorrect, 1, 'FLOWCORRECT', this%memoryPath)
    else
      call mem_allocate(this%flowcorrect, nodes, 'FLOWCORRECT', this%memoryPath)
    end if
    do n = 1, size(this%flowcorrect)
      this%flowcorrect(n) = DZERO
    end do
    !
    ! -- Allocate ibdgwfsat0, which is an indicator array marking cells with
    !    saturation greater than 0.0 with a value of 1
    call mem_allocate(this%ibdgwfsat0, nodes, 'IBDGWFSAT0', this%memoryPath)
    do n = 1, nodes
      this%ibdgwfsat0(n) = 1
    end do
    !
    ! -- Allocate differently depending on whether or not flows are
    !    being read from a file.
    if (this%flows_from_file) then
      call mem_allocate(this%gwfflowja, this%dis%con%nja, 'GWFFLOWJA', &
                        this%memoryPath)
      call mem_allocate(this%gwfsat, nodes, 'GWFSAT', this%memoryPath)
      call mem_allocate(this%gwfhead, nodes, 'GWFHEAD', this%memoryPath)
      call mem_allocate(this%gwfspdis, 3, nodes, 'GWFSPDIS', this%memoryPath)
      do n = 1, nodes
        this%gwfsat(n) = DONE
        this%gwfhead(n) = DZERO
        this%gwfspdis(:, n) = DZERO
      end do
      do n = 1, size(this%gwfflowja)
        this%gwfflowja(n) = DZERO
      end do
      !
      ! -- allocate and initialize storage arrays
      if (this%igwfstrgss == 0) then
        call mem_allocate(this%gwfstrgss, 1, 'GWFSTRGSS', this%memoryPath)
      else
        call mem_allocate(this%gwfstrgss, nodes, 'GWFSTRGSS', this%memoryPath)
      end if
      if (this%igwfstrgsy == 0) then
        call mem_allocate(this%gwfstrgsy, 1, 'GWFSTRGSY', this%memoryPath)
      else
        call mem_allocate(this%gwfstrgsy, nodes, 'GWFSTRGSY', this%memoryPath)
      end if
      do n = 1, size(this%gwfstrgss)
        this%gwfstrgss(n) = DZERO
      end do
      do n = 1, size(this%gwfstrgsy)
        this%gwfstrgsy(n) = DZERO
      end do
      !
      ! -- If there is no fmi package, then there are no flows at all or a
      !    connected GWF model, so allocate gwfpackages to zero
      if (this%inunit == 0) call this%allocate_gwfpackages(this%nflowpack)
    end if
    !
    ! -- Return
    return
  end subroutine allocate_arrays

  function gwfsatold(this, n, delt) result(satold)
! ******************************************************************************
! gwfsatold -- calculate the groundwater cell head saturation for the end of
!   the last time step
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(GwtFmiType) :: this
    integer(I4B), intent(in) :: n
    real(DP), intent(in) :: delt
    ! -- result
    real(DP) :: satold
    ! -- local
    real(DP) :: vcell
    real(DP) :: vnew
    real(DP) :: vold
! ------------------------------------------------------------------------------
    !
    ! -- calculate the value
    vcell = this%dis%area(n) * (this%dis%top(n) - this%dis%bot(n))
    vnew = vcell * this%gwfsat(n)
    vold = vnew
    if (this%igwfstrgss /= 0) vold = vold + this%gwfstrgss(n) * delt
    if (this%igwfstrgsy /= 0) vold = vold + this%gwfstrgsy(n) * delt
    satold = vold / vcell
    !
    ! -- Return
    return
  end function gwfsatold

  subroutine read_options(this)
! ******************************************************************************
! read_options -- Read Options
! Subroutine: (1) read options from input file
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use ConstantsModule, only: LINELENGTH, DEM6
    use InputOutputModule, only: getunit, openfile, urdaux
    use SimModule, only: store_error, store_error_unit
    ! -- dummy
    class(GwtFmiType) :: this
    ! -- local
    character(len=LINELENGTH) :: keyword
    integer(I4B) :: ierr
    logical :: isfound, endOfBlock
    character(len=*), parameter :: fmtisvflow = &
      "(4x,'CELL-BY-CELL FLOW INFORMATION WILL BE SAVED TO BINARY FILE &
      &WHENEVER ICBCFL IS NOT ZERO AND FLOW IMBALANCE CORRECTION ACTIVE.')"
    character(len=*), parameter :: fmtifc = &
      &"(4x,'MASS WILL BE ADDED OR REMOVED TO COMPENSATE FOR FLOW IMBALANCE.')"
! ------------------------------------------------------------------------------
    !
    ! -- get options block
    call this%parser%GetBlock('OPTIONS', isfound, ierr, blockRequired=.false., &
                              supportOpenClose=.true.)
    !
    ! -- parse options block if detected
    if (isfound) then
      write (this%iout, '(1x,a)') 'PROCESSING FMI OPTIONS'
      do
        call this%parser%GetNextLine(endOfBlock)
        if (endOfBlock) exit
        call this%parser%GetStringCaps(keyword)
        select case (keyword)
        case ('SAVE_FLOWS')
          this%ipakcb = -1
          write (this%iout, fmtisvflow)
        case ('FLOW_IMBALANCE_CORRECTION')
          write (this%iout, fmtifc)
          this%iflowerr = 1
        case default
          write (errmsg, '(4x,a,a)') '***ERROR. UNKNOWN FMI OPTION: ', &
            trim(keyword)
          call store_error(errmsg)
          call this%parser%StoreErrorUnit()
        end select
      end do
      write (this%iout, '(1x,a)') 'END OF FMI OPTIONS'
    end if
    !
    ! -- return
    return
  end subroutine read_options

  subroutine read_packagedata(this)
! ******************************************************************************
! read_packagedata -- Read PACKAGEDATA block
! Subroutine: (1) read packagedata block from input file
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use OpenSpecModule, only: ACCESS, FORM
    use ConstantsModule, only: LINELENGTH, DEM6, LENPACKAGENAME
    use InputOutputModule, only: getunit, openfile, urdaux
    use SimModule, only: store_error, store_error_unit
    ! -- dummy
    class(GwtFmiType) :: this
    ! -- local
    type(BudgetObjectType), pointer :: budobjptr
    character(len=LINELENGTH) :: keyword, fname
    character(len=LENPACKAGENAME) :: pname
    integer(I4B) :: i
    integer(I4B) :: ierr
    integer(I4B) :: inunit
    integer(I4B) :: iapt
    logical :: isfound, endOfBlock
    logical :: blockrequired
    logical :: exist
    type(BudObjPtrArray), dimension(:), allocatable :: tmpbudobj
! ------------------------------------------------------------------------------
    !
    ! -- initialize
    iapt = 0
    blockrequired = .true.
    !
    ! -- get options block
    call this%parser%GetBlock('PACKAGEDATA', isfound, ierr, &
                              blockRequired=blockRequired, &
                              supportOpenClose=.true.)
    !
    ! -- parse options block if detected
    if (isfound) then
      write (this%iout, '(1x,a)') 'PROCESSING FMI PACKAGEDATA'
      do
        call this%parser%GetNextLine(endOfBlock)
        if (endOfBlock) exit
        call this%parser%GetStringCaps(keyword)
        select case (keyword)
        case ('GWFBUDGET')
          call this%parser%GetStringCaps(keyword)
          if (keyword /= 'FILEIN') then
            call store_error('GWFBUDGET KEYWORD MUST BE FOLLOWED BY '// &
                             '"FILEIN" then by filename.')
            call this%parser%StoreErrorUnit()
          end if
          call this%parser%GetString(fname)
          inunit = getunit()
          inquire (file=trim(fname), exist=exist)
          if (.not. exist) then
            call store_error('Could not find file '//trim(fname))
            call this%parser%StoreErrorUnit()
          end if
          call openfile(inunit, this%iout, fname, 'DATA(BINARY)', FORM, &
                        ACCESS, 'UNKNOWN')
          this%iubud = inunit
          call this%initialize_bfr()
        case ('GWFHEAD')
          call this%parser%GetStringCaps(keyword)
          if (keyword /= 'FILEIN') then
            call store_error('GWFHEAD KEYWORD MUST BE FOLLOWED BY '// &
                             '"FILEIN" then by filename.')
            call this%parser%StoreErrorUnit()
          end if
          call this%parser%GetString(fname)
          inquire (file=trim(fname), exist=exist)
          if (.not. exist) then
            call store_error('Could not find file '//trim(fname))
            call this%parser%StoreErrorUnit()
          end if
          inunit = getunit()
          call openfile(inunit, this%iout, fname, 'DATA(BINARY)', FORM, &
                        ACCESS, 'UNKNOWN')
          this%iuhds = inunit
          call this%initialize_hfr()
        case ('GWFMOVER')
          call this%parser%GetStringCaps(keyword)
          if (keyword /= 'FILEIN') then
            call store_error('GWFMOVER KEYWORD MUST BE FOLLOWED BY '// &
                             '"FILEIN" then by filename.')
            call this%parser%StoreErrorUnit()
          end if
          call this%parser%GetString(fname)
          inunit = getunit()
          call openfile(inunit, this%iout, fname, 'DATA(BINARY)', FORM, &
                        ACCESS, 'UNKNOWN')
          this%iumvr = inunit
          call budgetobject_cr_bfr(this%mvrbudobj, 'MVT', this%iumvr, &
                                   this%iout)
          call this%mvrbudobj%fill_from_bfr(this%dis, this%iout)
        case default
          !
          ! --expand the size of aptbudobj, which stores a pointer to the budobj
          allocate (tmpbudobj(iapt))
          do i = 1, size(this%aptbudobj)
            tmpbudobj(i)%ptr => this%aptbudobj(i)%ptr
          end do
          deallocate (this%aptbudobj)
          allocate (this%aptbudobj(iapt + 1))
          do i = 1, size(tmpbudobj)
            this%aptbudobj(i)%ptr => tmpbudobj(i)%ptr
          end do
          deallocate (tmpbudobj)
          !
          ! -- Open the budget file and start filling it
          iapt = iapt + 1
          pname = keyword(1:LENPACKAGENAME)
          call this%parser%GetStringCaps(keyword)
          if (keyword /= 'FILEIN') then
            call store_error('PACKAGE NAME MUST BE FOLLOWED BY '// &
                             '"FILEIN" then by filename.')
            call this%parser%StoreErrorUnit()
          end if
          call this%parser%GetString(fname)
          inunit = getunit()
          call openfile(inunit, this%iout, fname, 'DATA(BINARY)', FORM, &
                        ACCESS, 'UNKNOWN')
          call budgetobject_cr_bfr(budobjptr, pname, inunit, &
                                   this%iout, colconv2=['GWF             '])
          call budobjptr%fill_from_bfr(this%dis, this%iout)
          this%aptbudobj(iapt)%ptr => budobjptr
        end select
      end do
      write (this%iout, '(1x,a)') 'END OF FMI PACKAGEDATA'
    end if
    !
    ! -- return
    return
  end subroutine read_packagedata

  subroutine set_aptbudobj_pointer(this, name, budobjptr)
! ******************************************************************************
! set_aptbudobj_pointer -- an advanced transport can pass in a name and a
!   pointer budget object, and this routine will look through the budget
!   objects managed by FMI and point to the one with the same name, such as
!   LAK-1, SFR-1, etc.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    class(GwtFmiType) :: this
    ! -- dumm
    character(len=*), intent(in) :: name
    type(BudgetObjectType), pointer :: budobjptr
    ! -- local
    integer(I4B) :: i
! ------------------------------------------------------------------------------
    !
    ! -- find and set the pointer
    do i = 1, size(this%aptbudobj)
      if (this%aptbudobj(i)%ptr%name == name) then
        budobjptr => this%aptbudobj(i)%ptr
        exit
      end if
    end do
    !
    ! -- return
    return
  end subroutine set_aptbudobj_pointer

  subroutine initialize_bfr(this)
! ******************************************************************************
! initialize_bfr -- initalize the budget file reader
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    class(GwtFmiType) :: this
    ! -- dummy
    integer(I4B) :: ncrbud
! ------------------------------------------------------------------------------
    !
    ! -- Initialize the budget file reader
    call this%bfr%initialize(this%iubud, this%iout, ncrbud)
    !
    ! -- todo: need to run through the budget terms
    !    and do some checking
  end subroutine initialize_bfr

  subroutine advance_bfr(this)
! ******************************************************************************
! advance_bfr -- advance the budget file reader by reading the next chunk
!   of information for the current time step and stress period
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use TdisModule, only: kstp, kper
    ! -- dummy
    class(GwtFmiType) :: this
    ! -- local
    logical :: success
    integer(I4B) :: n
    integer(I4B) :: ipos
    integer(I4B) :: nu, nr
    integer(I4B) :: ip, i
    logical :: readnext
    ! -- format
    character(len=*), parameter :: fmtkstpkper = &
      &"(1x,/1x,'FMI READING BUDGET TERMS FOR KSTP ', i0, ' KPER ', i0)"
    character(len=*), parameter :: fmtbudkstpkper = &
      "(1x,/1x, 'FMI SETTING BUDGET TERMS FOR KSTP ', i0, ' AND KPER ', &
      &i0, ' TO BUDGET FILE TERMS FROM KSTP ', i0, ' AND KPER ', i0)"
! ------------------------------------------------------------------------------
    !
    ! -- If the latest record read from the budget file is from a stress
    ! -- period with only one time step, reuse that record (do not read a
    ! -- new record) if the GWT model is still in that same stress period,
    ! -- or if that record is the last one in the budget file.
    readnext = .true.
    if (kstp * kper > 1) then
      if (this%bfr%kstp == 1) then
        if (this%bfr%kpernext == kper + 1) then
          readnext = .false.
        else if (this%bfr%endoffile) then
          readnext = .false.
        end if
      else if (this%bfr%endoffile) then
        write (errmsg, '(4x,a)') 'REACHED END OF GWF BUDGET &
          &FILE BEFORE READING SUFFICIENT BUDGET INFORMATION FOR THIS &
          &GWT SIMULATION.'
        call store_error(errmsg)
        call store_error_unit(this%iubud)
      end if
    end if
    !
    ! -- Read the next record
    if (readnext) then
      !
      ! -- Write the current time step and stress period
      write (this%iout, fmtkstpkper) kstp, kper
      !
      ! -- loop through the budget terms for this stress period
      !    i is the counter for gwf flow packages
      ip = 1
      do n = 1, this%bfr%nbudterms
        call this%bfr%read_record(success, this%iout)
        if (.not. success) then
          write (errmsg, '(4x,a)') 'GWF BUDGET READ NOT SUCCESSFUL'
          call store_error(errmsg)
          call store_error_unit(this%iubud)
        end if
        !
        ! -- Ensure kper is same between model and budget file
        if (kper /= this%bfr%kper) then
          write (errmsg, '(4x,a)') 'PERIOD NUMBER IN BUDGET FILE &
            &DOES NOT MATCH PERIOD NUMBER IN TRANSPORT MODEL.  IF THERE &
            &IS MORE THAN ONE TIME STEP IN THE BUDGET FILE FOR A GIVEN STRESS &
            &PERIOD, BUDGET FILE TIME STEPS MUST MATCH GWT MODEL TIME STEPS &
            &ONE-FOR-ONE IN THAT STRESS PERIOD.'
          call store_error(errmsg)
          call store_error_unit(this%iubud)
        end if
        !
        ! -- if budget file kstp > 1, then kstp must match
        if (this%bfr%kstp > 1 .and. (kstp /= this%bfr%kstp)) then
          write (errmsg, '(4x,a)') 'TIME STEP NUMBER IN BUDGET FILE &
            &DOES NOT MATCH TIME STEP NUMBER IN TRANSPORT MODEL.  IF THERE &
            &IS MORE THAN ONE TIME STEP IN THE BUDGET FILE FOR A GIVEN STRESS &
            &PERIOD, BUDGET FILE TIME STEPS MUST MATCH GWT MODEL TIME STEPS &
            &ONE-FOR-ONE IN THAT STRESS PERIOD.'
          call store_error(errmsg)
          call store_error_unit(this%iubud)
        end if
        !
        ! -- parse based on the type of data, and compress all user node
        !    numbers into reduced node numbers
        select case (trim(adjustl(this%bfr%budtxt)))
        case ('FLOW-JA-FACE')
          !
          ! -- bfr%flowja contains only reduced connections so there is
          !    a one-to-one match with this%gwfflowja
          do ipos = 1, size(this%bfr%flowja)
            this%gwfflowja(ipos) = this%bfr%flowja(ipos)
          end do
        case ('DATA-SPDIS')
          do i = 1, this%bfr%nlist
            nu = this%bfr%nodesrc(i)
            nr = this%dis%get_nodenumber(nu, 0)
            if (nr <= 0) cycle
            this%gwfspdis(1, nr) = this%bfr%auxvar(1, i)
            this%gwfspdis(2, nr) = this%bfr%auxvar(2, i)
            this%gwfspdis(3, nr) = this%bfr%auxvar(3, i)
          end do
        case ('DATA-SAT')
          do i = 1, this%bfr%nlist
            nu = this%bfr%nodesrc(i)
            nr = this%dis%get_nodenumber(nu, 0)
            if (nr <= 0) cycle
            this%gwfsat(nr) = this%bfr%auxvar(1, i)
          end do
        case ('STO-SS')
          do nu = 1, this%dis%nodesuser
            nr = this%dis%get_nodenumber(nu, 0)
            if (nr <= 0) cycle
            this%gwfstrgss(nr) = this%bfr%flow(nu)
          end do
        case ('STO-SY')
          do nu = 1, this%dis%nodesuser
            nr = this%dis%get_nodenumber(nu, 0)
            if (nr <= 0) cycle
            this%gwfstrgsy(nr) = this%bfr%flow(nu)
          end do
        case default
          call this%gwfpackages(ip)%copy_values( &
            this%bfr%nlist, &
            this%bfr%nodesrc, &
            this%bfr%flow, &
            this%bfr%auxvar)
          do i = 1, this%gwfpackages(ip)%nbound
            nu = this%gwfpackages(ip)%nodelist(i)
            nr = this%dis%get_nodenumber(nu, 0)
            this%gwfpackages(ip)%nodelist(i) = nr
          end do
          ip = ip + 1
        end select
      end do
    else
      !
      ! -- write message to indicate that flows are being reused
      write (this%iout, fmtbudkstpkper) kstp, kper, this%bfr%kstp, this%bfr%kper
      !
      ! -- set the flag to indicate that flows were not updated
      this%iflowsupdated = 0
    end if
  end subroutine advance_bfr

  subroutine finalize_bfr(this)
! ******************************************************************************
! finalize_bfr -- finalize the budget file reader
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    class(GwtFmiType) :: this
    ! -- dummy
! ------------------------------------------------------------------------------
    !
    ! -- Finalize the budget file reader
    call this%bfr%finalize()
    !
  end subroutine finalize_bfr

  subroutine initialize_hfr(this)
! ******************************************************************************
! initialize_hfr -- initalize the head file reader
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    class(GwtFmiType) :: this
    ! -- dummy
! ------------------------------------------------------------------------------
    !
    ! -- Initialize the budget file reader
    call this%hfr%initialize(this%iuhds, this%iout)
    !
    ! -- todo: need to run through the head terms
    !    and do some checking
  end subroutine initialize_hfr

  subroutine advance_hfr(this)
! ******************************************************************************
! advance_hfr -- advance the head file reader
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use TdisModule, only: kstp, kper
    class(GwtFmiType) :: this
    integer(I4B) :: nu, nr, i, ilay
    integer(I4B) :: ncpl
    real(DP) :: val
    logical :: readnext
    logical :: success
    character(len=*), parameter :: fmtkstpkper = &
      &"(1x,/1x,'FMI READING HEAD FOR KSTP ', i0, ' KPER ', i0)"
    character(len=*), parameter :: fmthdskstpkper = &
      "(1x,/1x, 'FMI SETTING HEAD FOR KSTP ', i0, ' AND KPER ', &
      &i0, ' TO BINARY FILE HEADS FROM KSTP ', i0, ' AND KPER ', i0)"
! ------------------------------------------------------------------------------
    !
    ! -- If the latest record read from the head file is from a stress
    ! -- period with only one time step, reuse that record (do not read a
    ! -- new record) if the GWT model is still in that same stress period,
    ! -- or if that record is the last one in the head file.
    readnext = .true.
    if (kstp * kper > 1) then
      if (this%hfr%kstp == 1) then
        if (this%hfr%kpernext == kper + 1) then
          readnext = .false.
        else if (this%hfr%endoffile) then
          readnext = .false.
        end if
      else if (this%hfr%endoffile) then
        write (errmsg, '(4x,a)') 'REACHED END OF GWF HEAD &
          &FILE BEFORE READING SUFFICIENT HEAD INFORMATION FOR THIS &
          &GWT SIMULATION.'
        call store_error(errmsg)
        call store_error_unit(this%iuhds)
      end if
    end if
    !
    ! -- Read the next record
    if (readnext) then
      !
      ! -- write to list file that heads are being read
      write (this%iout, fmtkstpkper) kstp, kper
      !
      ! -- loop through the layered heads for this time step
      do ilay = 1, this%hfr%nlay
        !
        ! -- read next head chunk
        call this%hfr%read_record(success, this%iout)
        if (.not. success) then
          write (errmsg, '(4x,a)') 'GWF HEAD READ NOT SUCCESSFUL'
          call store_error(errmsg)
          call store_error_unit(this%iuhds)
        end if
        !
        ! -- Ensure kper is same between model and head file
        if (kper /= this%hfr%kper) then
          write (errmsg, '(4x,a)') 'PERIOD NUMBER IN HEAD FILE &
            &DOES NOT MATCH PERIOD NUMBER IN TRANSPORT MODEL.  IF THERE &
            &IS MORE THAN ONE TIME STEP IN THE HEAD FILE FOR A GIVEN STRESS &
            &PERIOD, HEAD FILE TIME STEPS MUST MATCH GWT MODEL TIME STEPS &
            &ONE-FOR-ONE IN THAT STRESS PERIOD.'
          call store_error(errmsg)
          call store_error_unit(this%iuhds)
        end if
        !
        ! -- if head file kstp > 1, then kstp must match
        if (this%hfr%kstp > 1 .and. (kstp /= this%hfr%kstp)) then
          write (errmsg, '(4x,a)') 'TIME STEP NUMBER IN HEAD FILE &
            &DOES NOT MATCH TIME STEP NUMBER IN TRANSPORT MODEL.  IF THERE &
            &IS MORE THAN ONE TIME STEP IN THE HEAD FILE FOR A GIVEN STRESS &
            &PERIOD, HEAD FILE TIME STEPS MUST MATCH GWT MODEL TIME STEPS &
            &ONE-FOR-ONE IN THAT STRESS PERIOD.'
          call store_error(errmsg)
          call store_error_unit(this%iuhds)
        end if
        !
        ! -- fill the head array for this layer and
        !    compress into reduced form
        ncpl = size(this%hfr%head)
        do i = 1, ncpl
          nu = (ilay - 1) * ncpl + i
          nr = this%dis%get_nodenumber(nu, 0)
          val = this%hfr%head(i)
          if (nr > 0) this%gwfhead(nr) = val
        end do
      end do
    else
      write (this%iout, fmthdskstpkper) kstp, kper, this%hfr%kstp, this%hfr%kper
    end if
  end subroutine advance_hfr

  subroutine finalize_hfr(this)
! ******************************************************************************
! finalize_hfr -- finalize the head file reader
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    class(GwtFmiType) :: this
    ! -- dummy
! ------------------------------------------------------------------------------
    !
    ! -- Finalize the head file reader
    close (this%iuhds)
    !
  end subroutine finalize_hfr

  subroutine initialize_gwfterms_from_bfr(this)
! ******************************************************************************
! initialize_gwfterms_from_bfr -- initalize terms and figure out how many
!   different terms and packages are contained within the file
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryManagerModule, only: mem_allocate
    use SimModule, only: store_error, store_error_unit, count_errors
    ! -- dummy
    class(GwtFmiType) :: this
    ! -- local
    integer(I4B) :: nflowpack
    integer(I4B) :: i, ip
    integer(I4B) :: naux
    logical :: found_flowja
    logical :: found_dataspdis
    logical :: found_datasat
    logical :: found_stoss
    logical :: found_stosy
    integer(I4B), dimension(:), allocatable :: imap
! ------------------------------------------------------------------------------
    !
    ! -- Calculate the number of gwf flow packages
    allocate (imap(this%bfr%nbudterms))
    imap(:) = 0
    nflowpack = 0
    found_flowja = .false.
    found_dataspdis = .false.
    found_datasat = .false.
    found_stoss = .false.
    found_stosy = .false.
    do i = 1, this%bfr%nbudterms
      select case (trim(adjustl(this%bfr%budtxtarray(i))))
      case ('FLOW-JA-FACE')
        found_flowja = .true.
      case ('DATA-SPDIS')
        found_dataspdis = .true.
      case ('DATA-SAT')
        found_datasat = .true.
      case ('STO-SS')
        found_stoss = .true.
        this%igwfstrgss = 1
      case ('STO-SY')
        found_stosy = .true.
        this%igwfstrgsy = 1
      case default
        nflowpack = nflowpack + 1
        imap(i) = 1
      end select
    end do
    !
    ! -- allocate gwfpackage arrays (gwfpackages, iatp, datp, ...)
    call this%allocate_gwfpackages(nflowpack)
    !
    ! -- Copy the package name and aux names from budget file reader
    !    to the gwfpackages derived-type variable
    ip = 1
    do i = 1, this%bfr%nbudterms
      if (imap(i) == 0) cycle
      call this%gwfpackages(ip)%set_name(this%bfr%dstpackagenamearray(i), &
                                         this%bfr%budtxtarray(i))
      naux = this%bfr%nauxarray(i)
      call this%gwfpackages(ip)%set_auxname(naux, this%bfr%auxtxtarray(1:naux, i))
      ip = ip + 1
    end do
    !
    ! -- Copy just the package names for the boundary packages into
    !    the flowpacknamearray
    ip = 1
    do i = 1, size(imap)
      if (imap(i) == 1) then
        this%flowpacknamearray(ip) = this%bfr%dstpackagenamearray(i)
        ip = ip + 1
      end if
    end do
    !
    ! -- Error if specific discharge, saturation or flowja not found
    if (.not. found_dataspdis) then
      write (errmsg, '(4x,a)') 'SPECIFIC DISCHARGE NOT FOUND IN &
                              &BUDGET FILE. SAVE_SPECIFIC_DISCHARGE AND &
                              &SAVE_FLOWS MUST BE ACTIVATED IN THE NPF PACKAGE.'
      call store_error(errmsg)
    end if
    if (.not. found_datasat) then
      write (errmsg, '(4x,a)') 'SATURATION NOT FOUND IN &
                              &BUDGET FILE. SAVE_SATURATION AND &
                              &SAVE_FLOWS MUST BE ACTIVATED IN THE NPF PACKAGE.'
      call store_error(errmsg)
    end if
    if (.not. found_flowja) then
      write (errmsg, '(4x,a)') 'FLOWJA NOT FOUND IN &
                              &BUDGET FILE. SAVE_FLOWS MUST &
                              &BE ACTIVATED IN THE NPF PACKAGE.'
      call store_error(errmsg)
    end if
    if (count_errors() > 0) then
      call this%parser%StoreErrorUnit()
    end if
    !
    ! -- return
    return
  end subroutine initialize_gwfterms_from_bfr

  subroutine initialize_gwfterms_from_gwfbndlist(this)
! ******************************************************************************
! initialize_gwfterms_from_gwfbndlist -- flows are coming from a gwf-gwt
!   exchange
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use BndModule, only: BndType, GetBndFromList
    ! -- dummy
    class(GwtFmiType) :: this
    ! -- local
    integer(I4B) :: ngwfpack
    integer(I4B) :: ngwfterms
    integer(I4B) :: ip
    integer(I4B) :: imover
    integer(I4B) :: ntomvr
    integer(I4B) :: iterm
    character(len=LENPACKAGENAME) :: budtxt
    class(BndType), pointer :: packobj => null()
! ------------------------------------------------------------------------------
    !
    ! -- determine size of gwf terms
    ngwfpack = this%gwfbndlist%Count()
    !
    ! -- Count number of to-mvr terms, but do not include advanced packages
    !    as those mover terms are not losses from the cell, but rather flows
    !    within the advanced package
    ntomvr = 0
    do ip = 1, ngwfpack
      packobj => GetBndFromList(this%gwfbndlist, ip)
      imover = packobj%imover
      if (packobj%isadvpak /= 0) imover = 0
      if (imover /= 0) then
        ntomvr = ntomvr + 1
      end if
    end do
    !
    ! -- Allocate arrays in fmi of size ngwfterms, which is the number of
    !    packages plus the number of packages with mover terms.
    ngwfterms = ngwfpack + ntomvr
    call this%allocate_gwfpackages(ngwfterms)
    !
    ! -- Assign values in the fmi package
    iterm = 1
    do ip = 1, ngwfpack
      !
      ! -- set and store names
      packobj => GetBndFromList(this%gwfbndlist, ip)
      budtxt = adjustl(packobj%text)
      call this%gwfpackages(iterm)%set_name(packobj%packName, budtxt)
      this%flowpacknamearray(iterm) = packobj%packName
      call this%gwfpackages(iterm)%set_auxname(packobj%naux, &
                                               packobj%auxname)
      iterm = iterm + 1
      !
      ! -- if this package has a mover associated with it, then add another
      !    term that corresponds to the mover flows
      imover = packobj%imover
      if (packobj%isadvpak /= 0) imover = 0
      if (imover /= 0) then
        budtxt = trim(adjustl(packobj%text))//'-TO-MVR'
        call this%gwfpackages(iterm)%set_name(packobj%packName, budtxt)
        this%flowpacknamearray(iterm) = packobj%packName
        call this%gwfpackages(iterm)%set_auxname(packobj%naux, &
                                                 packobj%auxname)
        this%igwfmvrterm(iterm) = 1
        iterm = iterm + 1
      end if
    end do
    return
  end subroutine initialize_gwfterms_from_gwfbndlist

  subroutine allocate_gwfpackages(this, ngwfterms)
! ******************************************************************************
! allocate_gwfpackages -- gwfpackages is an array of PackageBudget objects.
!   This routine allocates gwfpackages to the proper size and initializes some
!   member variables.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use ConstantsModule, only: LENMEMPATH
    use MemoryManagerModule, only: mem_allocate
    ! -- dummy
    class(GwtFmiType) :: this
    integer(I4B), intent(in) :: ngwfterms
    ! -- local
    integer(I4B) :: n
    character(len=LENMEMPATH) :: memPath
! ------------------------------------------------------------------------------
    !
    ! -- direct allocate
    allocate (this%gwfpackages(ngwfterms))
    allocate (this%flowpacknamearray(ngwfterms))
    allocate (this%datp(ngwfterms))
    !
    ! -- mem_allocate
    call mem_allocate(this%iatp, ngwfterms, 'IATP', this%memoryPath)
    call mem_allocate(this%igwfmvrterm, ngwfterms, 'IGWFMVRTERM', this%memoryPath)
    !
    ! -- initialize
    this%nflowpack = ngwfterms
    do n = 1, this%nflowpack
      this%iatp(n) = 0
      this%igwfmvrterm(n) = 0
      this%flowpacknamearray(n) = ''
      !
      ! -- Create a mempath for each individual flow package data set
      !    of the form, MODELNAME/FMI-FTn
      write (memPath, '(a, i0)') trim(this%memoryPath)//'-FT', n
      call this%gwfpackages(n)%initialize(memPath)
    end do
    !
    ! -- return
    return
  end subroutine allocate_gwfpackages

  subroutine deallocate_gwfpackages(this)
! ******************************************************************************
! deallocate_gwfpackages -- memory in the gwfpackages array
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(GwtFmiType) :: this
    ! -- local
    integer(I4B) :: n
! ------------------------------------------------------------------------------
    !
    ! -- initialize
    do n = 1, this%nflowpack
      call this%gwfpackages(n)%da()
    end do
    !
    ! -- return
    return
  end subroutine deallocate_gwfpackages

  subroutine get_package_index(this, name, idx)
! ******************************************************************************
! get_package_index -- find the package index for package called name
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use BndModule, only: BndType, GetBndFromList
    class(GwtFmiType) :: this
    character(len=*), intent(in) :: name
    integer(I4B), intent(inout) :: idx
    ! -- local
    integer(I4B) :: ip
! ------------------------------------------------------------------------------
    !
    ! -- Look through all the packages and return the index with name
    idx = 0
    do ip = 1, size(this%flowpacknamearray)
      if (this%flowpacknamearray(ip) == name) then
        idx = ip
        exit
      end if
    end do
    if (idx == 0) then
      call store_error('Error in get_package_index.  Could not find '//name, &
                       terminate=.TRUE.)
    end if
    !
    ! -- return
    return
  end subroutine get_package_index

end module GwtFmiModule
