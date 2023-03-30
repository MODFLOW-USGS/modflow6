module ChdObsWriterModule
  use ChdModule, only: ChdType, AddChdToList, GetChdFromList, CellInChdList
  use ConstantsModule, only: LENOBSNAME, LENOBSTYPE, LINELENGTH
  use ConstantsPHMFModule, only: FCINPUT
  use FileTypeModule, only: FileType
  use GLOBAL, only: IUNIT, NLAY, NROW, NCOL
  use ListModule, only: ListType
  use InputOutputModule, only: same_word
  use OBSCHDMODULE, only: IUCHOBSV, NQCLCH, NQOBCH, otimech=>OTIME, &
                          qcellch=>QCELL, NQCH
  use ObsWriterModule, only: ObsWriterType
  use utl7module, only: assign_ncharsizes_flow, build_obsname

  private
  public :: ChdObsWriterType, createChdObsWriter

  type, extends(ObsWriterType) :: ChdObsWriterType
    type(ListType), pointer :: PkgIbChdList => null()
    type(ListType), pointer :: PkgTvChdList => null()
    type(ListType), pointer :: PkgCnstChdList => null()
  contains
    procedure :: InitializeObs => initialize_obs
    procedure :: WriteContinuous
    procedure :: WriteContinuousIb
    procedure :: WriteContinuousChd
  end type ChdObsWriterType

contains

  subroutine createChdObsWriter(newChdObsWriter, basename, iuChdObs)
    implicit none
    type(ChdObsWriterType), pointer, intent(out) :: newChdObsWriter
    character(len=*), intent(in) :: basename
    integer, intent(in) :: iuChdObs
    !
    allocate(newChdObsWriter)
    call newChdObsWriter%InitializeObs(basename)
    newChdObsWriter%IuObs = IuChdObs
    !
    return
  end subroutine createChdObsWriter

  subroutine initialize_obs(this, basename, modifier)
    implicit none
    ! dummy
    class(ChdObsWriterType), intent(inout) :: this
    character(len=*), intent(in) :: basename
    character(len=*), optional, intent(in) :: modifier
    ! local
    character(len=4) :: ftype
    character(len=LINELENGTH) :: fname
    !
    this%basename = basename
    this%PkgType = 'CHD'
    this%Active = .true.
    ftype = 'OBS6'
    fname = trim(basename) // '.chd.obs'
    ! Invoke superclass initializer
    call this%FileWriterType%InitializeFile(fname, ftype)
    this%FileWriterType%fileobj%FCode = FCINPUT
    !
    return
  end subroutine initialize_obs

  subroutine WriteContinuous(this, igrid)
    implicit none
    ! dummy
    class(ChdObsWriterType) :: this
    integer, intent(in) :: igrid
    !
    if (this%Source == 'IBOUND') then
      call this%WriteContinuousIb(igrid)
    elseif (this%Source == 'CHD') then
      call this%WriteContinuousChd(igrid)
    endif
    !
    return
  end subroutine WriteContinuous

  subroutine WriteContinuousIb(this, igrid)
    implicit none
    ! dummy
    class(ChdObsWriterType) :: this
    integer, intent(in) :: igrid
    ! local
    integer :: ic, igrp, iu, iuob, kc, &
               kob, nc, j, jl, jr, jc
    integer :: ilay, irow, icol
    integer :: ncharid, nchd
    real :: rnchd
    character(len=LENOBSNAME) :: oname
    character(len=LINELENGTH) :: outfilename
    character(len=LENOBSTYPE) :: otype, otypename
    logical :: killonfailure
    ! formats
    1 format()
    10 format(a,1x,a)
    20 format(2x,a,2x,a)
    30 format(2x,a,2x,a,3(2x,i0))
    !
    otype = 'CHD'
    otypename = 'CHDIB'
    ! Assign ncharid as width sufficient to support integers
    ! up to number of CHDs from IBOUND.
    ncharid = 1
    nchd = this%PkgIbChdList%Count()
    rnchd = real(nchd)
    if (nchd > 0) then
      ncharid = int(log10(rnchd)) + 1
    endif
    !
    ! Ned todo (all flow observation packages): when #cells in group = 1, use original obsnam
    !
    iu = this%fileobj%IUnit
    killonfailure = .false.
    iuob = iunit(38)  ! CHD obs input unit
    write(*,*)'Processing CHOB input for boundaries specified in IBOUND...'
    if (associated(NQCH)) then
      call OBS2CHD7DA(igrid)
    endif
    call OBS2CHD7AR(iuob, igrid)
    rewind(iuob)
    outfilename = trim(this%basename) // '_chdobs_out.csv'
    !
    write(iu,1)
    write(iu,10)'#','Constant-head observations for CHDs from IBOUND'
    write(iu,10)'BEGIN CONTINUOUS FILEOUT',trim(outfilename)
    !
    ! Initialize counters
    kob = 0
    kc = 0
    ! Iterate through cell groups
    do igrp=1,NQCH
      ! Number of cells in this cell group
      nc = NQCLCH(igrp)
      ! For continuous observations, no need to iterate through times
      ! Iterate through cells for this observation time for this cell group
      cellloop: do ic=1,nc
        kc = kc + 1
        ilay = int(qcellch(1,kc))
        irow = int(qcellch(2,kc))
        icol = int(qcellch(3,kc))
        !
        ! If cell is not in IbChdList, ignore
        if (.not. CellInChdList(this%PkgIbChdList, ilay, irow, icol)) then
          cycle cellloop
        endif
        !
        ! Don't duplicate cells
        if (kc > 1) then
          do j=1,kc-1
            jl = int(qcellch(1,j))
            jr = int(qcellch(2,j))
            jc = int(qcellch(3,j))
            if (jl == ilay .and. jr == irow .and. jc == icol) then
              cycle cellloop
            endif
          enddo
        endif
        !
        kob = kob + 1
        oname = build_obsname(otypename, kob, ncharid)
        write(iu,30)trim(oname), trim(otype), ilay, irow, icol
      enddo cellloop
    enddo
    !
    write(iu,10)'END','CONTINUOUS'
    !
    return
  end subroutine WriteContinuousIb

  subroutine WriteContinuousChd(this, igrid)
    implicit none
    ! dummy
    class(ChdObsWriterType) :: this
    integer, intent(in) :: igrid
    ! local
    integer :: ic, igrp, iu, iuob, kc, &
               kob, nc, j, jl, jr, jc
    integer :: ilay, irow, icol
    integer :: ncharid, nchd
    real :: rnchd
    character(len=LENOBSNAME) :: oname
    character(len=LINELENGTH) :: outfilename
    character(len=LENOBSTYPE) :: otype, otypename
    logical :: killonfailure, cnstChd, tvChd
    ! formats
    1 format()
    10 format(a,1x,a)
    20 format(2x,a,2x,a)
    30 format(2x,a,2x,a,3(2x,i0))
    !
    otype = 'CHD'
    otypename = 'CHD'
    ! Assign ncharid as width sufficient to support integers
    ! up to number of CHDs from IBOUND.
    ncharid = 1
    nchd = this%PkgTvChdList%Count() + this%PkgCnstChdList%Count()
    rnchd = real(nchd)
    if (nchd > 0) then
      ncharid = int(log10(rnchd)) + 1
    endif
    !
    ! Ned todo (all flow observation packages): when #cells in group = 1, use original obsnam
    !
    iu = this%fileobj%IUnit
    killonfailure = .false.
    iuob = iunit(38)  ! CHD obs input unit
    write(*,*)'Processing CHOB input for CHD package data...'
    if (associated(NQCH)) then
      call OBS2CHD7DA(igrid)
    endif
    call OBS2CHD7AR(iuob, igrid)
    rewind(iuob)
    outfilename = trim(this%basename) // '_chdobs_out.csv'
    !
    write(iu,1)
    write(iu,10)'#','Constant-head observations for CHDs from CHD package'
    write(iu,10)'BEGIN CONTINUOUS FILEOUT',trim(outfilename)
    !
    ! Initialize counters
    kob = 0
    kc = 0
    ! Iterate through cell groups
    do igrp=1,NQCH
      ! Number of cells in this cell group
      nc = NQCLCH(igrp)
      ! For continuous observations, no need to iterate through times
      ! Iterate through cells for this observation time for this cell group
      cellloop: do ic=1,nc
        kc = kc + 1
        ilay = int(qcellch(1,kc))
        irow = int(qcellch(2,kc))
        icol = int(qcellch(3,kc))
        !
        ! If cell is not in either TvChdList or CnstChdList, ignore it.
        cnstChd = CellInChdList(this%PkgCnstChdList, ilay, irow, icol)
        tvChd = CellInChdList(this%PkgTvChdList, ilay, irow, icol)
        if (.not. cnstChd .and. .not. tvChd) then
          cycle cellloop
        endif
        !
        ! Don't duplicate cells
        if (kc > 1) then
          do j=1,kc-1
            jl = int(qcellch(1,j))
            jr = int(qcellch(2,j))
            jc = int(qcellch(3,j))
            if (jl == ilay .and. jr == irow .and. jc == icol) then
              cycle cellloop
            endif
          enddo
        endif
        !
        kob = kob + 1
        oname = build_obsname(otypename, kob, ncharid)
        write(iu,30)trim(oname), trim(otype), ilay, irow, icol
      enddo cellloop
    enddo
    !
    write(iu,10)'END','CONTINUOUS'
    !
    return
  end subroutine WriteContinuousChd

end module ChdObsWriterModule
