module ZoneOutputModule

  use KindModule
  use ConstantsModule, only: LINELENGTH
  use BudgetModule, only: BudgetType, budget_cr
  use ZoneModule, only: iuniqzone
  implicit none
  private
  public :: zoneoutput_init
  public :: zoneoutput_write
  public :: zoneoutput_finalize

  integer(I4B) :: iout
  integer(I4B) :: ioutcsv = 0
  type(BudgetType), dimension(:), allocatable :: budobj

contains

  subroutine zoneoutput_init(iunit_out, iunit_csv, maxzone, nbudterms)
! ******************************************************************************
! zoneoutput_init
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    integer(I4B), intent(in) :: iunit_out
    integer(I4B), intent(in) :: iunit_csv
    integer(I4B), intent(in) :: maxzone
    integer(I4B), intent(in) :: nbudterms
    ! -- local
    integer(I4B) :: izone
    character(len=LINELENGTH) :: bdzone
! ------------------------------------------------------------------------------
    iout = iunit_out
    ioutcsv = iunit_csv
    !
    ! -- Create the budget objects to that budget tables can be
    !    written to list file.
    allocate (budobj(maxzone))
    do izone = 1, maxzone
      call budobj(izone)%allocate_scalars('ZONEBUDGET')
      write (bdzone, '(a,i0)') 'ZONE ', iuniqzone(izone)
      call budobj(izone)%budget_df(nbudterms + maxzone, &
                                   labeltitle='PACKAGE/MODEL', bdzone=bdzone)
    end do
    !
    ! -- Return
    return
  end subroutine zoneoutput_init

  subroutine zoneoutput_write(itime, kstp, kper, delt, totim, nbudterms, &
                              nmznfl, vbvl, vbznfl, packagenamearray, &
                              budtxtarray, internalflow)
! ******************************************************************************
! zoneoutput_write
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use ZoneModule, only: maxzone, iuniqzone
    ! -- dummy
    integer(I4B), intent(in) :: itime
    integer(I4B), intent(in) :: kstp
    integer(I4B), intent(in) :: kper
    real(DP), intent(in) :: delt
    real(DP), intent(in) :: totim
    integer(I4B), intent(in) :: nbudterms
    integer(I4B), dimension(0:maxzone, 0:maxzone), intent(in) :: nmznfl
    real(DP), dimension(2, 0:maxzone, nbudterms), intent(in) :: vbvl
    real(DP), dimension(2, 0:maxzone, 0:maxzone), intent(in) :: vbznfl
    character(len=16), dimension(:), intent(in) :: packagenamearray
    character(len=16), dimension(:), intent(in) :: budtxtarray
    integer(I4B), dimension(:), intent(in) :: internalflow
    ! -- local
    character(len=500) :: txt
    integer(I4B) :: ibudterm, izone, iinout, iz2, j
    integer(I4B) :: izv
    real(DP) :: val, rin, rout
    character(len=16), dimension(:), allocatable :: spntmp
! ------------------------------------------------------------------------------
    !
    ! -- If this is the first time, then write the CSV header, but skip
    !    FLOW-JA-FACE as that is only used for zone to zone flow.
    if (itime == 1) then
      !
      ! -- Because there can be more than one package of the same type, need
      !    to add package name to CSV titles, but only if necessary.  Put
      !    packagename into spntmp if there are multiple butxt entries of the
      !    same type.
      allocate (spntmp(nbudterms))
      spntmp(:) = ''
      do ibudterm = 1, nbudterms
        do j = 1, nbudterms
          if (j == ibudterm) cycle
          if (budtxtarray(ibudterm) == budtxtarray(j)) then
            spntmp(ibudterm) = packagenamearray(ibudterm)
          end if
        end do
      end do
      !
      ! -- Write time and zone information to CSV header
      write (ioutcsv, '(a)', advance='no') 'totim,'
      write (ioutcsv, '(a)', advance='no') 'kstp,'
      write (ioutcsv, '(a)', advance='no') 'kper,'
      write (ioutcsv, '(a)', advance='no') 'zone,'
      do ibudterm = 1, nbudterms
        if (internalflow(ibudterm) == 1) cycle
        txt = ''
        if (spntmp(ibudterm) /= '') then
          txt = trim(adjustl(spntmp(ibudterm)))//'-'
        end if
        write (txt, '(a, a, a)') trim(txt), &
          trim(adjustl(budtxtarray(ibudterm))), &
          '-IN,'
        write (ioutcsv, '(a)', advance='no') trim(txt)
      end do
      !
      ! -- Write budget terms to CSV header
      do ibudterm = 1, nbudterms
        if (internalflow(ibudterm) == 1) cycle
        txt = ''
        if (spntmp(ibudterm) /= '') then
          txt = trim(adjustl(spntmp(ibudterm)))//'-'
        end if
        write (txt, '(a, a, a)') trim(txt), &
          trim(adjustl(budtxtarray(ibudterm))), &
          '-OUT,'
        write (ioutcsv, '(a)', advance='no') trim(txt)
      end do
      !
      ! -- Write zone to zone flow names to CSV header
      do izone = 0, maxzone
        if (izone == 0) then
          izv = izone
        else
          izv = iuniqzone(izone)
        end if
        write (ioutcsv, '(a, i0)', advance='no') 'FROM ZONE ', izv
        write (ioutcsv, '(a)', advance='no') ','
      end do
      do izone = 0, maxzone
        if (izone == 0) then
          izv = izone
        else
          izv = iuniqzone(izone)
        end if
        write (ioutcsv, '(a, i0)', advance='no') 'TO ZONE ', izv
        if (izone < maxzone) write (ioutcsv, '(a)', advance='no') ','
      end do
      write (ioutcsv, *)
    end if
    !
    ! -- Write a line of CSV entries for each zone
    zoneloop: do izone = 1, maxzone
      !
      ! -- Time and zone information
      write (txt, '(G0)') totim
      write (ioutcsv, '(a)', advance='no') trim(adjustl(txt))//','
      write (txt, '(i0)') kstp
      write (ioutcsv, '(a)', advance='no') trim(adjustl(txt))//','
      write (txt, '(i0)') kper
      write (ioutcsv, '(a)', advance='no') trim(adjustl(txt))//','
      write (txt, '(i0)') iuniqzone(izone)
      write (ioutcsv, '(a)', advance='no') trim(adjustl(txt))//','
      !
      ! -- CSV budget ins and outs
      do iinout = 1, 2
        do ibudterm = 1, nbudterms
          if (internalflow(ibudterm) == 1) cycle
          write (txt, '(G0)') vbvl(iinout, izone, ibudterm)
          write (ioutcsv, '(a)', advance='no') trim(adjustl(txt))
          write (ioutcsv, '(a)', advance='no') ','
        end do
      end do
      !
      ! -- CSV file zone to zone flow in and out
      do iz2 = 0, maxzone
        val = vbznfl(1, izone, iz2)
        if (izone == iz2) val = 0.d0
        write (txt, '(G0)') val
        write (ioutcsv, '(a)', advance='no') trim(adjustl(txt))
        write (ioutcsv, '(a)', advance='no') ','
      end do
      do iz2 = 0, maxzone
        val = vbznfl(2, izone, iz2)
        if (izone == iz2) val = 0.d0
        write (txt, '(G0)') val
        write (ioutcsv, '(a)', advance='no') trim(adjustl(txt))
        if (iz2 < maxzone) write (ioutcsv, '(a)', advance='no') ','
      end do
      !
      ! -- LST file ins and outs
      call budobj(izone)%reset()
      do ibudterm = 1, size(budtxtarray)
        if (internalflow(ibudterm) == 1) cycle
        call budobj(izone)%addentry(vbvl(1, izone, ibudterm), &
                                    vbvl(2, izone, ibudterm), &
                                    delt, budtxtarray(ibudterm), &
                                    rowlabel=packagenamearray(ibudterm))
      end do
      !
      ! -- LST file zone to zone
      do iz2 = 0, maxzone
        if (izone == iz2) cycle
        if (nmznfl(izone, iz2) == 1) then
          rin = vbznfl(1, izone, iz2)
          rout = vbznfl(2, izone, iz2)
          if (iz2 == 0) then
            izv = iz2
          else
            izv = iuniqzone(iz2)
          end if
          write (txt, '(a,i0)') 'ZONE ', izv
          call budobj(izone)%addentry(rin, rout, delt, txt)
        end if
      end do
      call budobj(izone)%finalize_step(delt)
      call budobj(izone)%budget_ot(kstp, kper, iout)
      !
      ! -- write line ending after each zone
      write (ioutcsv, *)
    end do zoneloop
    !
    ! -- Return
    return
  end subroutine zoneoutput_write

  subroutine zoneoutput_finalize()
! ******************************************************************************
! zoneoutput_finalize
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
! ------------------------------------------------------------------------------
    !
    close (ioutcsv)
    !
    ! -- Return
    return
  end subroutine zoneoutput_finalize

end module ZoneOutputModule
