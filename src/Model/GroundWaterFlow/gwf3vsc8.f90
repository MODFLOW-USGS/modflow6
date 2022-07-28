! Viscosity Package for representing variable-viscosity groundwater flow

module GwfVscModule

  use KindModule, only: DP, I4B
  use SimModule, only: store_error, count_errors
  use MemoryManagerModule, only: mem_allocate, mem_reallocate, &
                                 mem_deallocate
  use ConstantsModule, only: DHALF, DZERO, DONE, LENMODELNAME, &
                             LENAUXNAME, DHNOFLO, MAXCHARLEN, LINELENGTH
  use NumericalPackageModule, only: NumericalPackageType
  use BaseDisModule, only: DisBaseType
  use GwfNpfModule, only: GwfNpfType
  use GwfVscInputDataModule, only: GwfVscInputDataType

  implicit none

  private
  public :: GwfVscType
  public :: vsc_cr

  type :: ConcentrationPointer
    real(DP), dimension(:), pointer :: conc => null() ! pointer to concentration array
    integer(I4B), dimension(:), pointer :: icbund => null() ! store pointer to gwt ibound array
  end type ConcentrationPointer

  type, extends(NumericalPackageType) :: GwfVscType
    type(GwfNpfType), pointer :: npf => null() ! npf object
    integer(I4B), pointer :: ioutvisc => null() ! unit number for saving viscosity
    integer(I4B), pointer :: ireadconcvsc => null() ! if 1 then visc has been read from this vsc input file   ! kluge note: is this ever really used?
    integer(I4B), pointer :: iconcset => null() ! if 1 then conc is pointed to a gwt model%x
    real(DP), pointer :: viscref => null() ! reference fluid viscosity
    real(DP), dimension(:), pointer, contiguous :: visc => null() ! viscosity
    real(DP), dimension(:), pointer, contiguous :: concvsc => null() ! concentration array if specified in vsc package    ! kluge note: is this ever really used?
    integer(I4B), dimension(:), pointer :: ibound => null() ! store pointer to ibound

    integer(I4B), pointer :: nviscspecies => null() ! number of species used in viscosity equation
    real(DP), dimension(:), pointer, contiguous :: dviscdc => null() ! change in viscosity with change in concentration   ! kluge note: parameters will depend on formula; linear for now
    real(DP), dimension(:), pointer, contiguous :: cviscref => null() ! reference concentration used in viscosity equation
    real(DP), dimension(:), pointer, contiguous :: ctemp => null() ! temporary array of size (nviscspec) to pass to calcvisc
    character(len=LENMODELNAME), dimension(:), allocatable :: cmodelname ! names of gwt models used in viscosity equation
    character(len=LENAUXNAME), dimension(:), allocatable :: cauxspeciesname ! names of aux columns used in viscosity equation

    type(ConcentrationPointer), allocatable, dimension(:) :: modelconc ! concentration pointer for each transport model

  contains
    procedure :: vsc_df
    procedure :: vsc_ar
    procedure :: vsc_rp
    procedure :: vsc_ad
    procedure :: vsc_ot_dv
    procedure :: vsc_da
    procedure, private :: vsc_calcvisc
    procedure :: allocate_scalars
    procedure, private :: allocate_arrays
    procedure, private :: read_options
    procedure, private :: set_options
    procedure, private :: read_dimensions
    procedure, private :: read_packagedata
    procedure, private :: set_packagedata
    procedure :: set_concentration_pointer
  end type GwfVscType

contains

  function calcvisc(viscref, dviscdc, cviscref, conc) result(visc)
! ******************************************************************************
! calcvisc -- generic function to calculate fluid viscosity from concentration
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    real(DP), intent(in) :: viscref
    real(DP), dimension(:), intent(in) :: dviscdc
    real(DP), dimension(:), intent(in) :: cviscref
    real(DP), dimension(:), intent(in) :: conc
    ! -- return
    real(DP) :: visc
    ! -- local
    integer(I4B) :: nviscspec
    integer(I4B) :: i
! ------------------------------------------------------------------------------
    !
    nviscspec = size(dviscdc)
    visc = viscref
    do i = 1, nviscspec
      visc = visc + dviscdc(i) * (conc(i) - cviscref(i))    ! kluge note: linear for now
    end do
    !
    ! -- return
    return
  end function calcvisc

  subroutine vsc_cr(vscobj, name_model, inunit, iout)
! ******************************************************************************
! vsc_cr -- Create a new VSC object
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    type(GwfVscType), pointer :: vscobj
    character(len=*), intent(in) :: name_model
    integer(I4B), intent(in) :: inunit
    integer(I4B), intent(in) :: iout
! ------------------------------------------------------------------------------
    !
    ! -- Create the object
    allocate (vscobj)
    !
    ! -- create name and memory path
    call vscobj%set_names(1, name_model, 'VSC', 'VSC')
    !
    ! -- Allocate scalars
    call vscobj%allocate_scalars()
    !
    ! -- Set variables
    vscobj%inunit = inunit
    vscobj%iout = iout
    !
    ! -- Initialize block parser
    call vscobj%parser%Initialize(vscobj%inunit, vscobj%iout)
    !
    ! -- Return
    return
  end subroutine vsc_cr

  !> @brief Read options and package data, or set from argument
  !<
  subroutine vsc_df(this, dis, vsc_input)
! ******************************************************************************
! vsc_df -- Allocate and Read
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(GwfVscType) :: this !< this viscosity package
    class(DisBaseType), pointer, intent(in) :: dis !< pointer to discretization
    type(GwfVscInputDataType), optional, intent(in) :: vsc_input !< optional vsc input data, otherwise read from file
    ! -- local
    ! -- formats
    character(len=*), parameter :: fmtvsc = &
      "(1x,/1x,'VSC -- VISCOSITY PACKAGE, VERSION 1, 5/16/2018', &
      &' INPUT READ FROM UNIT ', i0, //)"
! ------------------------------------------------------------------------------
    !
    ! --print a message identifying the viscosity package
    write (this%iout, fmtvsc) this%inunit
    !
    ! -- store pointers to arguments that were passed in
    this%dis => dis

    if (.not. present(vsc_input)) then
      !
      ! -- Read viscosity options
      call this%read_options()
      !
      ! -- Read viscosity dimensions
      call this%read_dimensions()
    else
      ! set from input data instead
      call this%set_options(vsc_input)
      this%nviscspecies = vsc_input%nviscspecies
    end if
    !
    ! -- Allocate arrays
    call this%allocate_arrays(dis%nodes)

    if (.not. present(vsc_input)) then
      !
      ! -- Read viscosity packagedata
      call this%read_packagedata()
    else
      ! set from input data instead
      call this%set_packagedata(vsc_input)
    end if
    !
    ! -- Return
    return
  end subroutine vsc_df

  subroutine vsc_ar(this, npf, ibound)
! ******************************************************************************
! vsc_ar -- Allocate and Read
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(GwfVscType) :: this
    type(GwfNpfType), pointer, intent(in) :: npf
    integer(I4B), dimension(:), pointer :: ibound
    ! -- local
    ! -- formats
! ------------------------------------------------------------------------------
    !
    ! -- store pointers to arguments that were passed in
    this%npf => npf
    this%ibound => ibound
    !
    ! -- Return
    return
  end subroutine vsc_ar

  subroutine vsc_rp(this)
! ******************************************************************************
! vsc_rp -- Check for new vsc period data
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use TdisModule, only: kstp, kper
    ! -- dummy
    class(GwfVscType) :: this
    ! -- local
    character(len=LINELENGTH) :: errmsg
    integer(I4B) :: i
    ! -- formats
    character(len=*), parameter :: fmtc = &
      "('VISCOSITY PACKAGE DOES NOT HAVE HAVE A CONCENTRATION SET &
       &FOR SPECIES ',i0,'. ONE OR MORE MODEL NAMES MAY BE SPECIFIED &
       &INCORRECTLY IN THE PACKAGEDATA BLOCK OR A GWF-GWT EXCHANGE MAY NEED &
       &TO BE ACTIVATED.')"
! ------------------------------------------------------------------------------
    !
    ! -- Check to make sure all concentration pointers have been set
    if (kstp * kper == 1) then
      do i = 1, this%nviscspecies
        if (.not. associated(this%modelconc(i)%conc)) then
          write (errmsg, fmtc) i
          call store_error(errmsg)
        end if
      end do
      if (count_errors() > 0) then
        call this%parser%StoreErrorUnit()
      end if
    end if
    !
    ! -- return
    return
  end subroutine vsc_rp

  subroutine vsc_ad(this)
! ******************************************************************************
! vsc_ad -- Advance
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(GwfVscType) :: this
    ! -- local
! ------------------------------------------------------------------------------
    !
    ! -- update viscosity using the last concentration
    call this%vsc_calcvisc()
    !
    ! -- update kfactor    ! kluge note: need this, and also a vsc_ad_bnd subroutine to update kfactors for boundary packages
    !
    ! -- Return
    return
  end subroutine vsc_ad

  function get_bnd_viscosity(n, locvisc, locconc, viscref, dviscdc, cviscref, &
                           ctemp, auxvar) result(viscbnd)
! ******************************************************************************
! get_bnd_viscosity -- Return the viscosity of the boundary package using one of
!   several different options in the following order of priority:
!     1. Assign as aux variable in column with name 'VISCOSITY'
!     2. Calculate using viscosity equation and nviscspecies aux columns
!     3. If neither of those, then assign as viscref
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    integer(I4B), intent(in) :: n
    integer(I4B), intent(in) :: locvisc
    integer(I4B), dimension(:), intent(in) :: locconc
    real(DP), intent(in) :: viscref
    real(DP), dimension(:), intent(in) :: dviscdc
    real(DP), dimension(:), intent(in) :: cviscref
    real(DP), dimension(:), intent(inout) :: ctemp
    real(DP), dimension(:, :), intent(in) :: auxvar
    ! -- return
    real(DP) :: viscbnd
    ! -- local
    integer(I4B) :: i
! ------------------------------------------------------------------------------
    !
    ! -- assign boundary viscosity based on one of three options
    if (locvisc > 0) then
      ! -- assign viscosity to an aux column named 'VISCOSITY'
      viscbnd = auxvar(locvisc, n)
    else if (locconc(1) > 0) then
      ! -- calculate viscosity using one or more concentration auxcolumns
      do i = 1, size(locconc)
        ctemp(i) = DZERO
        if (locconc(i) > 0) then
          ctemp(i) = auxvar(locconc(i), n)
        end if
      end do
      viscbnd = calcvisc(viscref, dviscdc, cviscref, ctemp)
    else
      ! -- neither of the above, so assign as viscref
      viscbnd = viscref
    end if
    !
    ! -- return
    return
  end function get_bnd_viscosity

  subroutine vsc_ot_dv(this, idvfl)    ! kluge note: rename to _vv ?
! ******************************************************************************
! vsc_ot_dv -- Save viscosity array to binary file
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(GwfVscType) :: this
    integer(I4B), intent(in) :: idvfl
    ! -- local
    character(len=1) :: cdatafmp = ' ', editdesc = ' '
    integer(I4B) :: ibinun
    integer(I4B) :: iprint
    integer(I4B) :: nvaluesp
    integer(I4B) :: nwidthp
    real(DP) :: dinact
! ------------------------------------------------------------------------------
    !
    ! -- Set unit number for viscosity output
    if (this%ioutvisc /= 0) then
      ibinun = 1
    else
      ibinun = 0
    end if
    if (idvfl == 0) ibinun = 0
    !
    ! -- save viscosity array
    if (ibinun /= 0) then
      iprint = 0
      dinact = DHNOFLO
      !
      ! -- write viscosity to binary file
      if (this%ioutvisc /= 0) then
        ibinun = this%ioutvisc
        call this%dis%record_array(this%visc, this%iout, iprint, ibinun, &
                                   '         VISCOSITY', cdatafmp, nvaluesp, &
                                   nwidthp, editdesc, dinact)
      end if
    end if

    !
    ! -- Return
    return
  end subroutine vsc_ot_dv

  subroutine vsc_da(this)
! ******************************************************************************
! vsc_da -- Deallocate
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(GwfVscType) :: this
! ------------------------------------------------------------------------------
    !
    ! -- Deallocate arrays if package was active
    if (this%inunit > 0) then
      call mem_deallocate(this%visc)
      call mem_deallocate(this%concvsc)
      call mem_deallocate(this%dviscdc)
      call mem_deallocate(this%cviscref)
      call mem_deallocate(this%ctemp)
      deallocate (this%cmodelname)
      deallocate (this%cauxspeciesname)
      deallocate (this%modelconc)
    end if
    !
    ! -- Scalars
    call mem_deallocate(this%ioutvisc)
    call mem_deallocate(this%ireadconcvsc)
    call mem_deallocate(this%iconcset)
    call mem_deallocate(this%viscref)

    call mem_deallocate(this%nviscspecies)
    !
    ! -- deallocate parent
    call this%NumericalPackageType%da()
    !
    ! -- Return
    return
  end subroutine vsc_da

  subroutine read_dimensions(this)
! ******************************************************************************
! read_dimensions -- Read the dimensions for this package
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(GwfVscType), intent(inout) :: this
    ! -- local
    character(len=LINELENGTH) :: errmsg, keyword
    integer(I4B) :: ierr
    logical :: isfound, endOfBlock
    ! -- format
! ------------------------------------------------------------------------------
    !
    ! -- get dimensions block
    call this%parser%GetBlock('DIMENSIONS', isfound, ierr, &
                              supportOpenClose=.true.)
    !
    ! -- parse dimensions block if detected
    if (isfound) then
      write (this%iout, '(/1x,a)') 'PROCESSING VSC DIMENSIONS'
      do
        call this%parser%GetNextLine(endOfBlock)
        if (endOfBlock) exit
        call this%parser%GetStringCaps(keyword)
        select case (keyword)
        case ('NVISCSPECIES')
          this%nviscspecies = this%parser%GetInteger()
          write (this%iout, '(4x,a,i0)') 'NVISCSPECIES = ', this%nviscspecies
        case default
          write (errmsg, '(4x,a,a)') &
            'UNKNOWN VSC DIMENSION: ', trim(keyword)
          call store_error(errmsg)
          call this%parser%StoreErrorUnit()
        end select
      end do
      write (this%iout, '(1x,a)') 'END OF VSC DIMENSIONS'
    else
      call store_error('REQUIRED VSC DIMENSIONS BLOCK NOT FOUND.')
      call this%parser%StoreErrorUnit()
    end if
    !
    ! -- check dimension
    if (this%nviscspecies < 1) then
      call store_error('NVISCSPECIES MUST BE GREATER THAN ZERO.')
      call this%parser%StoreErrorUnit()
    end if
    !
    ! -- return
    return
  end subroutine read_dimensions

  subroutine read_packagedata(this)
! ******************************************************************************
! read_packagedata -- Read PACKAGEDATA block
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(GwfVscType) :: this
    ! -- local
    character(len=LINELENGTH) :: errmsg
    character(len=LINELENGTH) :: line
    integer(I4B) :: ierr
    integer(I4B) :: iviscspec
    logical :: isfound, endOfBlock
    logical :: blockrequired
    integer(I4B), dimension(:), allocatable :: itemp
    character(len=10) :: c10
    character(len=16) :: c16
    ! -- format
    character(len=*), parameter :: fmterr = &
      "('INVALID VALUE FOR IRHOSPEC (',i0,') DETECTED IN VSC PACKAGE. &
      &IRHOSPEC MUST BE > 0 AND <= NVISCSPECIES, AND DUPLICATE VALUES &
      &ARE NOT ALLOWED.')"
! ------------------------------------------------------------------------------
    !
    ! -- initialize
    allocate (itemp(this%nviscspecies))
    itemp(:) = 0
    !
    ! -- get packagedata block
    blockrequired = .true.
    call this%parser%GetBlock('PACKAGEDATA', isfound, ierr, &
                              blockRequired=blockRequired, &
                              supportOpenClose=.true.)
    !
    ! -- parse packagedata block
    if (isfound) then
      write (this%iout, '(1x,a)') 'PROCESSING VSC PACKAGEDATA'
      do
        call this%parser%GetNextLine(endOfBlock)
        if (endOfBlock) exit
        iviscspec = this%parser%GetInteger()
        if (iviscspec < 1 .or. iviscspec > this%nviscspecies) then
          write (errmsg, fmterr) iviscspec
          call store_error(errmsg)
        end if
        if (itemp(iviscspec) /= 0) then
          write (errmsg, fmterr) iviscspec
          call store_error(errmsg)
        end if
        itemp(iviscspec) = 1
        this%dviscdc(iviscspec) = this%parser%GetDouble()
        this%cviscref(iviscspec) = this%parser%GetDouble()
        call this%parser%GetStringCaps(this%cmodelname(iviscspec))
        call this%parser%GetStringCaps(this%cauxspeciesname(iviscspec))
      end do
      write (this%iout, '(1x,a)') 'END OF VSC PACKAGEDATA'
    end if
    !
    ! -- Check for errors.
    if (count_errors() > 0) then
      call this%parser%StoreErrorUnit()
    end if
    !
    ! -- write packagedata information
    write (this%iout, '(/,a)') 'SUMMARY OF SPECIES INFORMATION IN VSC PACKAGE'
    write (this%iout, '(1a11, 4a17)') &
      'SPECIES', 'DRHODC', 'CRHOREF', 'MODEL', &
      'AUXSPECIESNAME'
    do iviscspec = 1, this%nviscspecies
      write (c10, '(i0)') iviscspec
      line = ' '//adjustr(c10)
      write (c16, '(g15.6)') this%dviscdc(iviscspec)
      line = trim(line)//' '//adjustr(c16)
      write (c16, '(g15.6)') this%cviscref(iviscspec)
      line = trim(line)//' '//adjustr(c16)
      write (c16, '(a)') this%cmodelname(iviscspec)
      line = trim(line)//' '//adjustr(c16)
      write (c16, '(a)') this%cauxspeciesname(iviscspec)
      line = trim(line)//' '//adjustr(c16)
      write (this%iout, '(a)') trim(line)
    end do
    !
    ! -- deallocate
    deallocate (itemp)
    !
    ! -- return
    return
  end subroutine read_packagedata

  !> @brief Sets package data instead of reading from file
  !<
  subroutine set_packagedata(this, input_data)
    class(GwfVscType) :: this !< this vscoancy pkg
    type(GwfVscInputDataType), intent(in) :: input_data !< the input data to be set
    ! local
    integer(I4B) :: ispec

    do ispec = 1, this%nviscspecies
      this%dviscdc(ispec) = input_data%dviscdc(ispec)
      this%cviscref(ispec) = input_data%cviscref(ispec)
      this%cmodelname(ispec) = input_data%cmodelname(ispec)
      this%cauxspeciesname(ispec) = input_data%cauxspeciesname(ispec)
    end do

  end subroutine set_packagedata

  subroutine vsc_calcvisc(this)
! ******************************************************************************
! vsc_calcvisc -- calculate fluid viscosity from concentration
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(GwfVscType) :: this

    ! -- local
    integer(I4B) :: n
    integer(I4B) :: i
! ------------------------------------------------------------------------------
    !
    ! -- Calculate the viscosity using the specified concentration array
    do n = 1, this%dis%nodes
      do i = 1, this%nviscspecies
        if (this%modelconc(i)%icbund(n) == 0) then
          this%ctemp = DZERO
        else
          this%ctemp(i) = this%modelconc(i)%conc(n)
        end if
      end do
      this%visc(n) = calcvisc(this%viscref, this%dviscdc, this%cviscref, &
                               this%ctemp)
    end do
    !
    ! -- Return
    return
  end subroutine vsc_calcvisc

  subroutine allocate_scalars(this)
! ******************************************************************************
! allocate_scalars
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use ConstantsModule, only: DZERO
    ! -- dummy
    class(GwfVscType) :: this
    ! -- local
! ------------------------------------------------------------------------------
    !
    ! -- allocate scalars in NumericalPackageType
    call this%NumericalPackageType%allocate_scalars()
    !
    ! -- Allocate
    call mem_allocate(this%ioutvisc, 'IOUTVISC', this%memoryPath)
    call mem_allocate(this%ireadconcvsc, 'IREADCONCVSC', this%memoryPath)
    call mem_allocate(this%iconcset, 'ICONCSET', this%memoryPath)
    call mem_allocate(this%viscref, 'VISCREF', this%memoryPath)

    call mem_allocate(this%nviscspecies, 'NVISCSPECIES', this%memoryPath)

    !
    ! -- Initialize
    this%ioutvisc = 0
    this%iconcset = 0
    this%ireadconcvsc = 0
    this%viscref = 1000.d0

    this%nviscspecies = 0

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
    ! -- modules
    ! -- dummy
    class(GwfVscType) :: this
    integer(I4B), intent(in) :: nodes
    ! -- local
    integer(I4B) :: i
! ------------------------------------------------------------------------------
    !
    ! -- Allocate
    call mem_allocate(this%visc, nodes, 'VISC', this%memoryPath)
    call mem_allocate(this%concvsc, 0, 'CONCVSC', this%memoryPath)
    call mem_allocate(this%dviscdc, this%nviscspecies, 'DRHODC', this%memoryPath)
    call mem_allocate(this%cviscref, this%nviscspecies, 'CRHOREF', this%memoryPath)
    call mem_allocate(this%ctemp, this%nviscspecies, 'CTEMP', this%memoryPath)
    allocate (this%cmodelname(this%nviscspecies))
    allocate (this%cauxspeciesname(this%nviscspecies))
    allocate (this%modelconc(this%nviscspecies))
    !
    ! -- Initialize
    do i = 1, nodes
      this%visc(i) = this%viscref
    end do
    !
    ! -- Initialize nviscspecies arrays
    do i = 1, this%nviscspecies
      this%dviscdc(i) = DZERO
      this%cviscref(i) = DZERO
      this%ctemp(i) = DZERO
      this%cmodelname(i) = ''
      this%cauxspeciesname(i) = ''
    end do
    !
    ! -- Return
    return
  end subroutine allocate_arrays

  subroutine read_options(this)
! ******************************************************************************
! read_options
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use OpenSpecModule, only: access, form
    use InputOutputModule, only: urword, getunit, urdaux, openfile
    ! -- dummy
    class(GwfVscType) :: this
    ! -- local
    character(len=LINELENGTH) :: errmsg, keyword
    character(len=MAXCHARLEN) :: fname
    integer(I4B) :: ierr
    logical :: isfound, endOfBlock
    ! -- formats
    character(len=*), parameter :: fmtfileout = &
      "(4x, 'VSC ', 1x, a, 1x, ' WILL BE SAVED TO FILE: ', &
      &a, /4x, 'OPENED ON UNIT: ', I7)"
! ------------------------------------------------------------------------------
    !
    ! -- get options block
    call this%parser%GetBlock('OPTIONS', isfound, ierr, &
                              supportOpenClose=.true., blockRequired=.false.)
    !
    ! -- parse options block if detected
    if (isfound) then
      write (this%iout, '(1x,a)') 'PROCESSING VSC OPTIONS'
      do
        call this%parser%GetNextLine(endOfBlock)
        if (endOfBlock) exit
        call this%parser%GetStringCaps(keyword)
        select case (keyword)
        case ('VISCREF')
          this%viscref = this%parser%GetDouble()
          write (this%iout, '(4x,a,1pg15.6)') &
            'REFERENCE VISCOSITY HAS BEEN SET TO: ', &
            this%viscref
        case ('VISCOSITY')
          call this%parser%GetStringCaps(keyword)
          if (keyword == 'FILEOUT') then
            call this%parser%GetString(fname)
            this%ioutvisc = getunit()
            call openfile(this%ioutvisc, this%iout, fname, 'DATA(BINARY)', &
                          form, access, 'REPLACE')
            write (this%iout, fmtfileout) &
              'VISCOSITY', fname, this%ioutvisc
          else
            errmsg = 'OPTIONAL VISCOSITY KEYWORD MUST BE '// &
                     'FOLLOWED BY FILEOUT'
            call store_error(errmsg)
          end if
        case default
          write (errmsg, '(4x,a,a)') '****ERROR. UNKNOWN VSC OPTION: ', &
            trim(keyword)
          call store_error(errmsg)
          call this%parser%StoreErrorUnit()
        end select
      end do
      write (this%iout, '(1x,a)') 'END OF VSC OPTIONS'
    end if
    !
    ! -- Return
    return
  end subroutine read_options

  !> @brief Sets options as opposed to reading them from a file
  !<
  subroutine set_options(this, input_data)
    class(GwfVscType) :: this
    type(GwfVscInputDataType), intent(in) :: input_data !< the input data to be set

    this%viscref = input_data%viscref

  end subroutine set_options

  subroutine set_concentration_pointer(this, modelname, conc, icbund)
! ******************************************************************************
! set_concentration_pointer -- pass in a gwt model name, concentration array
!   and ibound, and store a pointer to these in the VSC package so that
!   viscosity can be calculated from them.
!   This routine is called from the gwfgwt exchange in the exg_ar() method.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(GwfVscType) :: this
    character(len=LENMODELNAME), intent(in) :: modelname
    real(DP), dimension(:), pointer :: conc
    integer(I4B), dimension(:), pointer :: icbund
    ! -- local
    integer(I4B) :: i
    logical :: found
! ------------------------------------------------------------------------------
    !
    this%iconcset = 1
    found = .false.
    do i = 1, this%nviscspecies
      if (this%cmodelname(i) == modelname) then
        this%modelconc(i)%conc => conc
        this%modelconc(i)%icbund => icbund
        found = .true.
        exit
      end if
    end do
    !
    ! -- Return
    return
  end subroutine set_concentration_pointer

end module GwfVscModule
