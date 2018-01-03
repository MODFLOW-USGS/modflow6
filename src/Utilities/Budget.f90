!Module for storing and printing a model budget.
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

module BudgetModule

  use KindModule, only: DP, I4B
  use SimModule,       only: store_error, count_errors, ustop
  use ConstantsModule, only: LINELENGTH, LENBUDTXT, LENPACKAGENAME
  
  implicit none
  private
  public BudgetType, budget_cr

  type BudgetType
    integer(I4B), pointer                            :: msum => null()
    integer(I4B), pointer                            :: maxsize => null()
    real(DP), pointer                                :: budperc => null()
    logical, pointer                                 :: written_once => null()
    real(DP), dimension(:,:), pointer                :: vbvl => null()
    character(len=LENBUDTXT), dimension(:), pointer  :: vbnm => null()
    character(len=20), pointer                       :: bdtype => null()
    character(len=5), pointer                        :: bddim => null()
    character(len=LENPACKAGENAME), dimension(:), pointer  :: rowlabel => null()
    character(len=16), pointer                       :: labeltitle => null()
    character(len=20), pointer                       :: bdzone => null()
    logical, pointer                                 :: labeled => null()
  contains
    procedure :: budget_df
    procedure :: budget_ot
    procedure :: budget_da
    procedure :: reset
    procedure :: add_single_entry
    procedure :: add_multi_entry
    generic :: addentry => add_single_entry, add_multi_entry
    ! -- private
    procedure          :: allocate_scalars
    procedure, private :: allocate_arrays
  end type BudgetType

  contains

  subroutine budget_cr(this, name_model)
! ******************************************************************************
! budget_cr -- Create a new budget object
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    type(BudgetType), pointer :: this
    character(len=*), intent(in) :: name_model
! ------------------------------------------------------------------------------
    !
    ! -- Create the object
    allocate(this)
    !
    ! -- Allocate scalars
    call this%allocate_scalars(name_model)
    !
    ! -- Return
    return
  end subroutine budget_cr

  subroutine budget_df(this, maxsize, bdtype, bddim, labeltitle, bdzone)
! ******************************************************************************
! budget_df -- Define the Budget Object
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    class(BudgetType) :: this
    integer(I4B),intent(in) :: maxsize
    character(len=*), optional :: bdtype
    character(len=*), optional :: bddim
    character(len=*), optional :: labeltitle
    character(len=*), optional :: bdzone
! ------------------------------------------------------------------------------
    !
    ! -- Set values
    this%maxsize = maxsize
    !
    ! -- Allocate arrays
    call this%allocate_arrays()
    !
    ! -- Set the budget name
    if(present(bdtype)) then
      this%bdtype = bdtype
    else
      this%bdtype = 'VOLUME'
    endif
    !
    ! -- Set the budget dimension
    if(present(bddim)) then
      this%bddim = bddim
    else
      this%bddim = 'L**3'
    endif
    !
    ! -- Set the budget zone
    if(present(bdzone)) then
      this%bdzone = bdzone
    else
      this%bdzone = 'ENTIRE MODEL'
    endif
    !
    ! -- Set the label title
    if(present(labeltitle)) then
      this%labeltitle = labeltitle
    else
      this%labeltitle = 'PACKAGE NAME'
    endif
    !
    ! -- Return
    return
  end subroutine budget_df

  subroutine budget_ot(this, kstp, kper, iout)
! ******************************************************************************
! budget_ot -- Output the Budget
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    class(BudgetType) :: this
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
    ! -- Set constants
    zero = 0.d0
    two = 2.d0
    hund = 100.d0
    bigvl1 = 9.99999d11
    bigvl2 = 9.99999d10
    small = 0.1d0
    !
    ! -- Determine number of individual budget entries.
    this%budperc = zero
    msum1 = this%msum-1
    if(msum1 <= 0) return
    !
    ! -- Clear rate and volume accumulators.
    totrin=zero
    totrot=zero
    totvin=zero
    totvot=zero
    !
    ! -- Add rates and volumes (in and out) to accumulators.
    do l=1,msum1
      totrin=totrin+this%vbvl(3,l)
      totrot=totrot+this%vbvl(4,l)
      totvin=totvin+this%vbvl(1,l)
      totvot=totvot+this%vbvl(2,l)
    enddo
    !
    ! -- Print time step number and stress period number.
    if(this%labeled) then
      write(iout,261) trim(adjustl(this%bdtype)), trim(adjustl(this%bdzone)),    &
                      kstp, kper
      write(iout,266) trim(adjustl(this%bdtype)), trim(adjustl(this%bddim)),   &
                      trim(adjustl(this%bddim)),this%labeltitle
    else
      write(iout,260) trim(adjustl(this%bdtype)), trim(adjustl(this%bdzone)),    &
                      kstp, kper
      write(iout,265) trim(adjustl(this%bdtype)), trim(adjustl(this%bddim)),   &
                      trim(adjustl(this%bddim))
    endif
    !
    ! -- Print individual inflow rates and volumes and their totals.
    do l=1,msum1
      if(this%vbvl(1,l).ne.zero .and. &
              (this%vbvl(1,l).ge.bigvl1 .or. this%vbvl(1,l).lt.small)) then
          write(val1,'(1pe17.4)') this%vbvl(1,l)
      else
          write(val1,'(f17.4)') this%vbvl(1,l)
      end if
      if(this%vbvl(3,l).ne.zero .and. &
              (this%vbvl(3,l).ge.bigvl1 .or. this%vbvl(3,l).lt.small)) then
          write(val2,'(1pe17.4)') this%vbvl(3,l)
      else
          write(val2,'(f17.4)') this%vbvl(3,l)
      end if
      if(this%labeled) then
        write(iout,276) this%vbnm(l),val1,this%vbnm(l),val2,this%rowlabel(l)
      else
        write(iout,275) this%vbnm(l),val1,this%vbnm(l),val2
      endif
    enddo
    if(totvin.ne.zero .and. &
           (totvin.ge.bigvl1 .or. totvin.lt.small)) then
        write(val1,'(1pe17.4)') totvin
    else
        write(val1,'(f17.4)') totvin
    end if
    if(totrin.ne.zero .and. &
           (totrin.ge.bigvl1 .or. totrin.lt.small)) then
        write(val2,'(1pe17.4)') totrin
    else
        write(val2,'(f17.4)') totrin
    end if
    write(iout,286) val1,val2
    !
    ! -- Print individual outflow rates and volumes and their totals.
    write(iout,287)
    do l=1,msum1
      if(this%vbvl(2,l).ne.zero .and. &
              (this%vbvl(2,l).ge.bigvl1 .or. this%vbvl(2,l).lt.small)) then
          write(val1,'(1pe17.4)') this%vbvl(2,l)
      else
          write(val1,'(f17.4)') this%vbvl(2,l)
      end if
      if(this%vbvl(4,l).ne.zero .and. &
              (this%vbvl(4,l).ge.bigvl1 .or. this%vbvl(4,l).lt.small)) then
          write(val2,'(1pe17.4)') this%vbvl(4,l)
      else
          write(val2,'(f17.4)') this%vbvl(4,l)
      end if
      if(this%labeled) then
        write(iout,276) this%vbnm(l),val1,this%vbnm(l),val2,this%rowlabel(l)
      else
        write(iout,275) this%vbnm(l),val1,this%vbnm(l),val2
      endif
    enddo
    if(totvot.ne.zero .and. &
           (totvot.ge.bigvl1 .or. totvot.lt.small)) then
        write(val1,'(1pe17.4)') totvot
    else
        write(val1,'(f17.4)') totvot
    end if
    if(totrot.ne.zero .and. &
           (totrot.ge.bigvl1 .or. totrot.lt.small)) then
        write(val2,'(1pe17.4)') totrot
    else
        write(val2,'(f17.4)') totrot
    end if
    write(iout,298) val1,val2
    !
    ! -- Calculate the difference between inflow and outflow.
    !
    ! -- Calculate difference between rate in and rate out.
    diffr=totrin-totrot
    adiffr=abs(diffr)
    !
    ! -- Calculate percent difference between rate in and rate out.
    pdiffr=zero
    avgrat=(totrin+totrot)/two
    if(avgrat.ne.zero) pdiffr=hund*diffr/avgrat
    this%budperc=pdiffr
    !
    ! -- Calculate difference between volume in and volume out.
    diffv=totvin-totvot
    adiffv=abs(diffv)
    !
    ! -- Get percent difference between volume in and volume out.
    pdiffv=zero
    avgvol=(totvin+totvot)/two
    if(avgvol.ne.zero) pdiffv=hund*diffv/avgvol
    !
    ! -- Print differences and percent differences between input
    ! -- and output rates and volumes.
    if(adiffv.ne.zero .and. &
           (adiffv.ge.bigvl2 .or. adiffv.lt.small)) then
        write(val1,'(1pe17.4)') diffv
    else
        write(val1,'(f17.4)') diffv
    end if
    if(adiffr.ne.zero .and. &
           (adiffr.ge.bigvl2 .or. adiffr.lt.small)) then
        write(val2,'(1pe17.4)') diffr
    else
        write(val2,'(f17.4)') diffr
    end if
    write(iout,299) val1,val2
    write(iout,300) pdiffv,pdiffr
    !
    ! -- set written_once to .true.
    this%written_once = .true.
    !
    ! -- formats
    260 FORMAT(//2X,a,' BUDGET FOR ',a,' AT END OF'                            &
        ,' TIME STEP',I5,', STRESS PERIOD',I4/2X,78('-'))
    261 FORMAT(//2X,a,' BUDGET FOR ',a,' AT END OF'                            &
        ,' TIME STEP',I5,', STRESS PERIOD',I4/2X,99('-'))
    265 FORMAT(1X,/5X,'CUMULATIVE ',a,6X,a,7X                                  &
        ,'RATES FOR THIS TIME STEP',6X,a,'/T'/5X,18('-'),17X,24('-')           &
        //11X,'IN:',38X,'IN:'/11X,'---',38X,'---')
    266 FORMAT(1X,/5X,'CUMULATIVE ',a,6X,a,7X                                  &
        ,'RATES FOR THIS TIME STEP',6X,a,'/T',10X,A16,                         &
        /5X,18('-'),17X,24('-'),21X,16('-')                                    &
        //11X,'IN:',38X,'IN:'/11X,'---',38X,'---')
    275 FORMAT(1X,3X,A16,' =',A17,6X,A16,' =',A17)
    276 FORMAT(1X,3X,A16,' =',A17,6X,A16,' =',A17,5X,A16)
    286 FORMAT(1X,/12X,'TOTAL IN =',A,14X,'TOTAL IN =',A)
    287 FORMAT(1X,/10X,'OUT:',37X,'OUT:'/10X,4('-'),37X,4('-'))
    298 FORMAT(1X,/11X,'TOTAL OUT =',A,13X,'TOTAL OUT =',A)
    299 FORMAT(1X,/12X,'IN - OUT =',A,14X,'IN - OUT =',A)
    300 FORMAT(1X,/1X,'PERCENT DISCREPANCY =',F15.2                            &
        ,5X,'PERCENT DISCREPANCY =',F15.2/)
    !
    ! -- Return
    return
  end subroutine budget_ot

  subroutine budget_da(this)
! ******************************************************************************
! budget_da -- Deallocate budget variables
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    class(BudgetType) :: this
! ------------------------------------------------------------------------------
    !
    ! -- Scalars
    deallocate(this%msum)
    deallocate(this%maxsize)
    deallocate(this%budperc)
    deallocate(this%written_once)
    deallocate(this%labeled)
    deallocate(this%bdtype)
    deallocate(this%bddim)
    deallocate(this%labeltitle)
    deallocate(this%bdzone)
    !
    ! -- Arrays
    deallocate(this%vbvl)
    deallocate(this%vbnm)
    deallocate(this%rowlabel)
    !
    ! -- Return
    return
  end subroutine budget_da

  subroutine reset(this)
! ******************************************************************************
! initialize -- Reset all of the budget rates to zero, and set msum to 1.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use ConstantsModule, only: DZERO
    ! -- dummy
    class(BudgetType) :: this
    ! -- local
    integer(I4B) :: i
! ------------------------------------------------------------------------------
    !
    this%msum = 1
    !
    do i = 1, this%maxsize
      this%vbvl(3, i) = DZERO
      this%vbvl(4, i) = DZERO
    enddo
    !
    ! -- Return
    return
  end subroutine reset

  subroutine add_single_entry(this, rin, rout, delt, text,                     &
                              isupress_accumulate, rowlabel)
! ******************************************************************************
! add_single_entry -- Add Budget Entry
!    rin the inflow rate
!    rout is the outflow rate
!    delt is the time step length
!    text is the name of the entry
!    isupress_accumulate is an optional flag.  If specified as 1, then
!      the volume is NOT added to the accumulators on vbvl(1, :) and vbvl(2, :).
!    rowlabel is a LENPACKAGENAME character text entry that is written to the 
!      right of the table.  It can be used for adding package names to budget 
!      entries.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(BudgetType) :: this
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
    iscv = 0
    if(present(isupress_accumulate)) then
      iscv = isupress_accumulate
    endif
    !
    ! -- If budget has been written at least once, then make sure that the present
    !    text entry matches the last text entry
    if(this%written_once) then
      if(trim(adjustl(this%vbnm(this%msum))) /= trim(adjustl(text))) then
        write(errmsg, fmtbuderr) trim(adjustl(this%vbnm(this%msum))), &
                                 trim(adjustl(text))
        call store_error(errmsg)
        call ustop()
      endif
    endif
    !
    if(iscv == 0) then
      this%vbvl(1, this%msum)=this%vbvl(1,this%msum) + rin * delt
      this%vbvl(2, this%msum)=this%vbvl(2,this%msum) + rout * delt
    endif
    !
    this%vbvl(3, this%msum) = rin
    this%vbvl(4, this%msum) = rout
    this%vbnm(this%msum) = adjustr(text)
    if(present(rowlabel)) then
      this%rowlabel(this%msum) = adjustl(rowlabel)
      this%labeled = .true.
    endif
    this%msum = this%msum + 1
    !
    ! -- Return
    return
  end subroutine add_single_entry

  subroutine add_multi_entry(this, budterm, delt, budtxt,                     &
                             isupress_accumulate, rowlabel)
! ******************************************************************************
! add_multi_entry -- Add multiple budget entries
!    budterm is an array with inflow in column 1 and outflow in column 2
!    delt is the time step length
!    budtxt is the name of the entries.  It should have one entry for each
!      row in budterm
!    isupress_accumulate is an optional flag.  If specified as 1, then
!      the volume is NOT added to the accumulators on vbvl(1, :) and vbvl(2, :).
!    rowlabel is a LENPACKAGENAME character text entry that is written to the 
!      right of the table.  It can be used for adding package names to budget 
!      entries. For multiple entries, the same rowlabel is used for each entry.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(BudgetType) :: this
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
      ! -- If budget has been written at least once, then make sure that the present
      !    text entry matches the last text entry
      if(this%written_once) then
          if(trim(adjustl(this%vbnm(this%msum))) /=                            &
              trim(adjustl(budtxt(i)))) then
            write(errmsg, fmtbuderr) trim(adjustl(this%vbnm(this%msum))),      &
                                     trim(adjustl(budtxt(i)))
            call store_error(errmsg)
          endif
      endif
      !
      if(iscv == 0) then
        this%vbvl(1, this%msum)=this%vbvl(1,this%msum) + budterm(1, i) * delt
        this%vbvl(2, this%msum)=this%vbvl(2,this%msum) + budterm(2, i) * delt
      endif
      !
      this%vbvl(3, this%msum) = budterm(1, i)
      this%vbvl(4, this%msum) = budterm(2, i)
      this%vbnm(this%msum) = adjustr(budtxt(i))
      if(present(rowlabel)) then
        this%rowlabel(this%msum) = adjustl(rowlabel)
        this%labeled = .true.
      endif
      this%msum = this%msum + 1
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

  subroutine allocate_scalars(this, name_model)
! ******************************************************************************
! allocate_scalars -- allocate budget scalar variables
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    class(BudgetType) :: this
    character(len=*), intent(in) :: name_model
! ------------------------------------------------------------------------------
    !
    allocate(this%msum)
    allocate(this%maxsize)
    allocate(this%budperc)
    allocate(this%written_once)
    allocate(this%labeled)
    allocate(this%bdtype)
    allocate(this%bddim)
    allocate(this%labeltitle)
    allocate(this%bdzone)
    !
    ! -- Initialize values
    this%msum = 0
    this%maxsize = 0
    this%written_once = .false.
    this%labeled = .false.
    this%bdtype = ''
    this%bddim = ''
    this%labeltitle = ''
    this%bdzone = ''
    !
    return
  end subroutine allocate_scalars

  subroutine allocate_arrays(this)
! ******************************************************************************
! allocate_arrays -- allocate budget arrays
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use ConstantsModule, only: DZERO
    ! -- dummy
    class(BudgetType) :: this
! ------------------------------------------------------------------------------
    !
    ! -- If redefining, then need to deallocate/reallocate
    if(associated(this%vbvl)) then
      deallocate(this%vbvl)
      nullify(this%vbvl)
    endif
    if(associated(this%vbnm)) then
      deallocate(this%vbnm)
      nullify(this%vbnm)
    endif
    if(associated(this%rowlabel)) then
      deallocate(this%rowlabel)
      nullify(this%rowlabel)
    endif
    !
    ! -- Allocate
    allocate(this%vbvl(4, this%maxsize))
    allocate(this%vbnm(this%maxsize))
    allocate(this%rowlabel(this%maxsize))
    !
    ! -- Initialize values
    this%vbvl(:, :) = DZERO
    this%vbnm(:) = ''
    this%rowlabel(:) = ''
    !
    return
  end subroutine allocate_arrays

end module BudgetModule
