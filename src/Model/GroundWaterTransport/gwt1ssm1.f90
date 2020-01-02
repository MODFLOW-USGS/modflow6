module GwtSsmModule
  
  use KindModule,             only: DP, I4B
  use ConstantsModule,        only: DONE, DZERO, LENAUXNAME, LENFTYPE,         &
                                    LENPACKAGENAME
  use NumericalPackageModule, only: NumericalPackageType
  use BaseDisModule,          only: DisBaseType
  use GwtFmiModule,           only: GwtFmiType
  
  implicit none
  public :: GwtSsmType
  public :: ssm_cr

  character(len=LENFTYPE)       :: ftype = 'SSM'
  character(len=LENPACKAGENAME) :: text  = ' SOURCE-SINK MIX'

  type, extends(NumericalPackageType) :: GwtSsmType
    
    integer(I4B), pointer                              :: nbound                ! number of flow boundaries in this time step
    integer(I4B), dimension(:), pointer, contiguous    :: iauxpak => null()     ! aux col for concentration
    integer(I4B), dimension(:), pointer, contiguous    :: ibound => null()      ! pointer to model ibound
    real(DP), dimension(:), pointer, contiguous        :: cnew => null()        ! pointer to gwt%x
    type(GwtFmiType), pointer                          :: fmi => null()         ! pointer to fmi object
    
  contains
  
    procedure :: ssm_df
    procedure :: ssm_ar
    procedure :: ssm_ad
    procedure :: ssm_fc
    procedure :: ssm_bdcalc
    procedure :: ssm_bdsav
    procedure :: ssm_da
    procedure :: allocate_scalars
    procedure, private :: allocate_arrays
    procedure, private :: read_options
    procedure, private :: read_data
  
  end type GwtSsmType
  
  contains
  
  subroutine ssm_cr(ssmobj, name_model, inunit, iout, fmi)
! ******************************************************************************
! ssm_cr -- Create a new SSM object
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    type(GwtSsmType), pointer :: ssmobj
    character(len=*), intent(in) :: name_model
    integer(I4B), intent(in) :: inunit
    integer(I4B), intent(in) :: iout
    type(GwtFmiType), intent(in), target :: fmi
! ------------------------------------------------------------------------------
    !
    ! -- Create the object
    allocate(ssmobj)
    !
    ! -- create name and origin
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
    ! -- Initialize block parser
    call ssmobj%parser%Initialize(ssmobj%inunit, ssmobj%iout)
    !
    ! -- Return
    return
  end subroutine ssm_cr

  subroutine ssm_df(this)
! ******************************************************************************
! ssm_df -- Allocate and Read
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryManagerModule, only: mem_setptr
    ! -- dummy
    class(GwtSsmType) :: this
    ! -- local
    ! -- formats
! ------------------------------------------------------------------------------
    !
    ! -- Return
    return
  end subroutine ssm_df

  subroutine ssm_ar(this, dis, ibound, cnew)
! ******************************************************************************
! ssm_ar -- Allocate and Read
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryManagerModule, only: mem_setptr
    use SimModule,           only: ustop, store_error
    use ConstantsModule,   only: LINELENGTH
    ! -- dummy
    class(GwtSsmType) :: this
    class(DisBaseType), pointer, intent(in) :: dis
    integer(I4B), dimension(:), pointer, contiguous :: ibound
    real(DP), dimension(:), pointer, contiguous :: cnew
    ! -- local
    character(len=LINELENGTH) :: errmsg
    ! -- formats
    character(len=*), parameter :: fmtssm =                                    &
      "(1x,/1x,'SSM -- SOURCE-SINK MIXING PACKAGE, VERSION 1, 8/25/2017',      &
      &' INPUT READ FROM UNIT ', i0, //)"
! ------------------------------------------------------------------------------
    !
    ! --print a message identifying the storage package.
    write(this%iout, fmtssm) this%inunit
    !
    ! -- store pointers to arguments that were passed in
    this%dis     => dis
    this%ibound  => ibound
    this%cnew    => cnew
    !
    ! -- Check to make sure that there are flow packages
    if (this%fmi%nflowpack == 0) then
      write(errmsg, '(a)') '****ERROR. SSM PACKAGE DOES NOT HAVE &
                            &BOUNDARY FLOWS.  ACTIVATE GWF-GWT EXCHANGE &
                            &OR TURN ON FMI AND PROVIDE BUDGET FILE.'
      call store_error(errmsg)
      call this%parser%StoreErrorUnit()
      call ustop()
    endif
    !
    ! -- Allocate arrays
    call this%allocate_arrays()
    !
    ! -- Read ssm options
    call this%read_options()
    !
    ! -- read the data block
    call this%read_data()
    !
    ! -- Return
    return
  end subroutine ssm_ar

  subroutine ssm_ad(this)
! ******************************************************************************
! ssm_ad -- Calculate number of flow boundaries
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(GwtSsmType) :: this
    ! -- local
    integer(I4B) :: ip
! ------------------------------------------------------------------------------
    !
    ! -- Calculate total number of flow boundaries
    this%nbound = 0
    do ip = 1, this%fmi%nflowpack
      if (this%fmi%iatp(ip) /= 0) cycle 
      this%nbound = this%nbound + this%fmi%gwfpackages(ip)%nbound
    end do
    !
    ! -- Return
    return
  end subroutine ssm_ad
  
  subroutine ssm_fc(this, amatsln, idxglo, rhs)
! ******************************************************************************
! ssm_fc -- Calculate coefficients and fill amat and rhs
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(GwtSsmType) :: this
    real(DP), dimension(:), intent(inout) :: amatsln
    integer(I4B), intent(in), dimension(:) :: idxglo
    real(DP), intent(inout), dimension(:) :: rhs
    ! -- local
    integer(I4B) :: ip
    integer(I4B) :: i, n, idiag
    integer(I4B) :: iauxpos
    integer(I4B) :: nflowpack, nbound
    real(DP) :: qbnd
    real(DP) :: ctmp
! ------------------------------------------------------------------------------
    !
    ! -- do for each flow package
    nflowpack = this%fmi%nflowpack
    do ip = 1, nflowpack
      if (this%fmi%iatp(ip) /= 0) cycle
      !
      ! -- do for each boundary
      nbound = this%fmi%gwfpackages(ip)%nbound
      do i = 1, nbound
        !
        ! -- set nodenumber
        n = this%fmi%gwfpackages(ip)%nodelist(i)
        !
        ! -- skip if transport cell is inactive or constant concentration
        if (this%ibound(n) <= 0) cycle
        !
        ! -- Calculate the volumetric flow rate
        qbnd = this%fmi%gwfpackages(ip)%get_flow(i)
        !
        ! -- get the first auxiliary variable
        iauxpos = this%iauxpak(ip)
        if(iauxpos > 0) then
          ctmp = this%fmi%gwfpackages(ip)%auxvar(iauxpos, i)
        else
          ctmp = DZERO
        endif
        !
        ! -- Add terms based on qbnd sign
        if(qbnd <= DZERO) then
          idiag = idxglo(this%dis%con%ia(n))
          amatsln(idiag) = amatsln(idiag) + qbnd
        else
          rhs(n) = rhs(n) - qbnd * ctmp
        endif
        !
      enddo
      !
    enddo
    !
    ! -- Return
    return
  end subroutine ssm_fc
  
  subroutine ssm_bdcalc(this, isuppress_output, model_budget)
! ******************************************************************************
! ssm_bdcalc -- Calculate budget terms
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use TdisModule, only: delt
    use BudgetModule, only: BudgetType
    ! -- dummy
    class(GwtSsmType) :: this
    integer(I4B), intent(in) :: isuppress_output
    type(BudgetType), intent(inout) :: model_budget
    ! -- local
    integer(I4B) :: ip
    integer(I4B) :: n
    integer(I4B) :: i
    integer(I4B) :: iauxpos
    real(DP) :: rate
    real(DP) :: rin, rout
    real(DP) :: qbnd
    real(DP) :: ctmp
    real(DP) :: cbnd
! ------------------------------------------------------------------------------
    !
    ! -- initialize 
    rin = DZERO
    rout = DZERO
    !
    ! -- do for each flow package
    do ip = 1, this%fmi%nflowpack
      if (this%fmi%iatp(ip) /= 0) cycle
      !
      ! -- do for each boundary
      do i = 1, this%fmi%gwfpackages(ip)%nbound
        !
        ! -- set nodenumber
        n = this%fmi%gwfpackages(ip)%nodelist(i)
        !
        ! -- skip if transport cell is inactive or constant concentration
        if (this%ibound(n) <= 0) cycle
        !
        ! -- Calculate the volumetric flow rate
        qbnd = this%fmi%gwfpackages(ip)%get_flow(i)
        !
        ! -- get the first auxiliary variable
        iauxpos = this%iauxpak(ip)
        if(iauxpos > 0) then
          cbnd = this%fmi%gwfpackages(ip)%auxvar(iauxpos, i)
        else
          cbnd = DZERO
        endif
        !
        ! -- Add terms based on qbnd sign
        if(qbnd <= DZERO) then
          ctmp = this%cnew(n)
        else
          ctmp = cbnd
        endif
        !
        ! -- Rate is now a mass flux
        rate = qbnd * ctmp
        if(rate < DZERO) then
          rout = rout - rate
        else
          rin = rin + rate
        endif
        !
      enddo
      !
    enddo
    !
    ! -- Add contributions to model budget
    call model_budget%addentry(rin, rout, delt, text, isuppress_output)
    !
    ! -- Return
    return
  end subroutine ssm_bdcalc

  subroutine ssm_bdsav(this, icbcfl, ibudfl, icbcun, iprobs,                   &
                       isuppress_output, imap)
! ******************************************************************************
! ssm_bdsav -- Calculate SSM Budget
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use TdisModule, only: kstp, kper
    use ConstantsModule, only: LENPACKAGENAME, LENBOUNDNAME, LENAUXNAME, DZERO
    use BudgetModule, only: BudgetType
    ! -- dummy
    class(GwtSsmType) :: this
    integer(I4B), intent(in) :: icbcfl
    integer(I4B), intent(in) :: ibudfl
    integer(I4B), intent(in) :: icbcun
    integer(I4B), intent(in) :: iprobs
    integer(I4B), intent(in) :: isuppress_output
    integer(I4B), dimension(:), optional, intent(in) :: imap
    ! -- local
    integer(I4B) :: ip
    integer(I4B) :: n
    integer(I4B) :: iauxpos
    integer(I4B) :: i, n2, ibinun
    real(DP) :: qbnd
    real(DP) :: cbnd, ctmp
    real(DP) :: rrate
    integer(I4B) :: ibdlbl, naux
    real(DP), dimension(0,0) :: auxvar
    character(len=LENAUXNAME), dimension(0) :: auxname
    ! -- for observations
    character(len=LENBOUNDNAME) :: bname
    ! -- formats
    character(len=*), parameter :: fmttkk = &
      "(1X,/1X,A,'   PERIOD ',I0,'   STEP ',I0)"
! ------------------------------------------------------------------------------
    !
    !
    ! -- Initialize
    ibdlbl = 0
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
    if (isuppress_output /= 0) ibinun = 0
    !
    ! -- If cell-by-cell flows will be saved as a list, write header.
    if(ibinun /= 0) then
      naux = 0
      call this%dis%record_srcdst_list_header(text, this%name_model,      &
                  this%name_model, this%name_model, this%name, naux,           &
                  auxname, ibinun, this%nbound, this%iout)
    endif
    !
    ! -- If no boundaries, skip flow calculations.
    if(this%nbound > 0) then
      !
      ! -- Loop through each boundary calculating flow.
      do ip = 1, this%fmi%nflowpack
        if (this%fmi%iatp(ip) /= 0) cycle
        !
        ! -- do for each boundary
        do i = 1, this%fmi%gwfpackages(ip)%nbound
          !
          ! -- set nodenumber and initialize
          n = this%fmi%gwfpackages(ip)%nodelist(i)
          rrate = DZERO
          !
          ! -- skip if transport cell is inactive or constant concentration
          if (this%ibound(n) <= 0) cycle
          !
          ! -- Calculate the volumetric flow rate
          qbnd = this%fmi%gwfpackages(ip)%get_flow(i)
          !
          ! -- get the first auxiliary variable
          iauxpos = this%iauxpak(ip)
          if(iauxpos > 0) then
            cbnd = this%fmi%gwfpackages(ip)%auxvar(iauxpos, i)
          else
            cbnd = DZERO
          endif
          !
          ! -- Add terms based on qbnd sign
          if(qbnd <= DZERO) then
            ctmp = this%cnew(n)
          else
            ctmp = cbnd
          endif
          !
          ! -- Rate is now a mass flux
          rrate = qbnd * ctmp
          !
          ! -- Print the individual rates if the budget is being printed
          !    and PRINT_FLOWS was specified (this%iprflow<0)
          if(ibudfl /= 0) then
            if(this%iprflow /= 0) then
              if(ibdlbl == 0) write(this%iout,fmttkk)                        &
                text // ' (' // trim(this%name) // ')', kper, kstp
              bname = this%fmi%gwfpackages(ip)%name
              call this%dis%print_list_entry(i, n, rrate, this%iout,      &
                      bname)
              ibdlbl=1
            endif
          endif
          !
          ! -- If saving cell-by-cell flows in list, write flow
          if (ibinun /= 0) then
            n2 = i
            if (present(imap)) n2 = imap(i)
            call this%dis%record_mf6_list_entry(ibinun, n, n2, rrate,          &
                                                    naux, auxvar(:,i),         &
                                                    olconv2=.FALSE.)
          end if
          !
          ! -- Save simulated value to simvals array.
          !this%simvals(i) = rrate
          !
        enddo
        !
      enddo
    endif
    if (ibudfl /= 0) then
      if (this%iprflow /= 0) then
          write(this%iout,'(1x)')
      end if
    end if
    !
    ! -- Save the simulated values to the ObserveType objects
    !if (iprobs /= 0 .and. this%obs%npakobs > 0) then
    !  call this%bnd_bd_obs()
    !endif
    !
    ! -- return
    return
  end subroutine ssm_bdsav

  subroutine ssm_da(this)
! ******************************************************************************
! ssm_da -- Deallocate variables
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryManagerModule, only: mem_deallocate
    ! -- dummy
    class(GwtSsmType) :: this
! ------------------------------------------------------------------------------
    !
    ! -- Deallocate arrays if package was active
    if(this%inunit > 0) then
      call mem_deallocate(this%iauxpak)
      this%ibound => null()
      this%fmi => null()
    endif
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
    class(GwtSsmType) :: this
    ! -- local
! ------------------------------------------------------------------------------
    !
    ! -- allocate scalars in NumericalPackageType
    call this%NumericalPackageType%allocate_scalars()
    !
    ! -- Allocate
    call mem_allocate(this%nbound, 'NBOUND', this%origin)
    !
    ! -- Initialize
    this%nbound = 0
    !
    ! -- Return
    return
  end subroutine allocate_scalars

  subroutine allocate_arrays(this)
! ******************************************************************************
! allocate_scalars
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryManagerModule, only: mem_allocate, mem_setptr
    ! -- dummy
    class(GwtSsmType) :: this
    ! -- local
    integer(I4B) :: nflowpack
! ------------------------------------------------------------------------------
    !    
    ! -- Allocate
    nflowpack = this%fmi%nflowpack
    call mem_allocate(this%iauxpak, nflowpack, 'IAUXPAK',                      &
                      this%origin)
    !
    ! -- Initialize
    this%iauxpak(:) = 0
    !
    ! -- Return
    return
  end subroutine allocate_arrays
  
  subroutine read_options(this)
! ******************************************************************************
! read_options -- Allocate and Read
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    !modules
    use ConstantsModule,   only: LINELENGTH
    use SimModule,         only: ustop, store_error
    ! -- dummy
    class(GwtSSMType) :: this
    ! -- local
    character(len=LINELENGTH) :: errmsg, keyword
    integer(I4B) :: ierr
    logical :: isfound, endOfBlock
    ! -- formats
    character(len=*), parameter :: fmtiprflow =                                &
      "(4x,'SSM FLOW INFORMATION WILL BE PRINTED TO LISTING FILE " // &
      "WHENEVER ICBCFL IS NOT ZERO.')"
    character(len=*), parameter :: fmtisvflow =                                &
      "(4x,'CELL-BY-CELL FLOW INFORMATION WILL BE SAVED TO BINARY FILE " //    &
      "WHENEVER ICBCFL IS NOT ZERO.')"
! ------------------------------------------------------------------------------
    !
    ! -- get options block
    call this%parser%GetBlock('OPTIONS', isfound, ierr, blockRequired=.false.)
    !
    ! -- parse options block if detected
    if (isfound) then
      write(this%iout,'(1x,a)')'PROCESSING SSM OPTIONS'
      do
        call this%parser%GetNextLine(endOfBlock)
        if (endOfBlock) exit
        call this%parser%GetStringCaps(keyword)
        select case (keyword)
          case ('PRINT_FLOWS')
            this%iprflow = 1
            write(this%iout, fmtiprflow)
          case ('SAVE_FLOWS')
            this%ipakcb = -1
            write(this%iout, fmtisvflow)
          case default
            write(errmsg,'(4x,a,a)')'****ERROR. UNKNOWN SSM OPTION: ',         &
                                     trim(keyword)
            call store_error(errmsg)
            call this%parser%StoreErrorUnit()
            call ustop()
        end select
      end do
      write(this%iout,'(1x,a)')'END OF SSM OPTIONS'
    end if
    !
    ! -- Return
    return
  end subroutine read_options

  subroutine read_data(this)
! ******************************************************************************
! read_data -- read source data
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use ConstantsModule,   only: LINELENGTH
    use SimModule,         only: ustop, store_error, count_errors
    ! -- dummy
    class(GwtSsmtype) :: this
    ! -- local
    character(len=LINELENGTH) :: errmsg, keyword
    character(len=LENAUXNAME) :: auxname
    character(len=3) :: srctype
    integer(I4B) :: ierr
    integer(I4B) :: ip
    integer(I4B) :: iaux
    integer(I4B) :: nflowpack
    logical :: isfound, endOfBlock
    logical :: pakfound
    ! -- formats
    ! -- data
! ------------------------------------------------------------------------------
    !
    ! -- initialize
    isfound = .false.
    nflowpack = this%fmi%nflowpack
    !
    ! -- get sources block
    call this%parser%GetBlock('SOURCES', isfound, ierr)
    if(isfound) then
      write(this%iout,'(1x,a)')'PROCESSING SOURCES'
      do
        call this%parser%GetNextLine(endOfBlock)
        if (endOfBlock) exit
        !
        ! -- read package name and make sure it can be found
        call this%parser%GetStringCaps(keyword)
        pakfound = .false.
        do ip = 1, nflowpack
          if (trim(adjustl(this%fmi%gwfpackages(ip)%name)) == keyword) then
            pakfound = .true.
            exit
          endif
        enddo
        if (.not. pakfound) then
          write(errmsg,'(1x, a, a)') 'ERROR.  PACKAGE CANNOT BE FOUND: ',      &
                                      trim(keyword)
          call store_error(errmsg)
          call this%parser%StoreErrorUnit()
          call ustop()
        endif
        !
        ! -- read the source type
        call this%parser%GetStringCaps(srctype)
        select case(srctype)
        case('AUX')
          write(this%iout,'(1x,a)') 'AUX SOURCE DETECTED.'
        case default
          write(errmsg,'(1x, a, a)')                                          &
            'ERROR.  SRCTYPE MUST BE AUX OR LKT ', srctype
          call store_error(errmsg)
          call this%parser%StoreErrorUnit()
          call ustop()
        end select
        !
        ! -- read name of auxiliary column
        call this%parser%GetStringCaps(auxname)
        pakfound = .false.
        do iaux = 1, this%fmi%gwfpackages(ip)%naux
          if (trim(this%fmi%gwfpackages(ip)%auxname(iaux)) ==                  &
              trim(auxname)) then
            pakfound = .true.
            exit
          endif
        enddo
        if (.not. pakfound) then
          write(errmsg,'(1x, a, a)')                                           &
            'ERROR.  AUXILIARY NAME CANNOT BE FOUND: ', trim(auxname)
          call store_error(errmsg)
          call this%parser%StoreErrorUnit()
          call ustop()
        endif
        !
        ! -- Set the column position in iauxpak
        if (this%iauxpak(ip) /= 0) then
          write(errmsg,'(1x, a, a, i0, a)')                                    &
            'ERROR.  PACKAGE and AUXNAME ALREADY SPECIFIED: ',                 &
            trim(keyword), trim(auxname)
          call store_error(errmsg)
          call this%parser%StoreErrorUnit()
          call ustop()
        endif
        this%iauxpak(ip) = iaux
        write(this%iout, '(4x, a, i0, a, a)') 'USING AUX COLUMN ',      &
          iaux, ' IN PACKAGE ', trim(keyword)
        !
      end do
      write(this%iout,'(1x,a)')'END PROCESSING SOURCES'
    else
      write(errmsg,'(1x,a)')'ERROR.  REQUIRED SOURCES BLOCK NOT FOUND.'
      call store_error(errmsg)
      call this%parser%StoreErrorUnit()
      call ustop()
    end if
    !
    ! -- terminate if erros
    if(count_errors() > 0) then
      call this%parser%StoreErrorUnit()
      call ustop()
    endif
    !
    ! -- Return
    return
  end subroutine read_data
  
end module GwtSsmModule