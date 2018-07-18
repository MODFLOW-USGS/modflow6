module GhbObsWriterModule
  use ConstantsModule, only: LENOBSNAME, LENOBSTYPE, LINELENGTH
  use ConstantsPHMFModule, only: FCINPUT
  use FileTypeModule, only: FileType
  use GLOBAL, only: IUNIT, NLAY, NROW, NCOL
  use InputOutputModule, only: same_word
  use OBSGHBMODULE, only: IUGBOBSV, NQGB, NQCLGB, NQOBGB, otimegb=>OTIME, &
                          qcellgb=>QCELL, NQCGB
  use ObsWriterModule, only: ObsWriterType
  use utl7module, only: assign_ncharsizes_flow, build_obsname

  private
  public :: GhbObsWriterType, createGhbObsWriter

  type, extends(ObsWriterType) :: GhbObsWriterType
  contains
    procedure :: InitializeObs => initialize_obs
    procedure :: WriteContinuous => WriteContinuousGhb
  end type GhbObsWriterType

contains

  subroutine createGhbObsWriter(newGhbObsWriter, basename, iuGhbObs)
    implicit none
    type(GhbObsWriterType), pointer, intent(out) :: newGhbObsWriter
    character(len=*), intent(in) :: basename
    integer, intent(in) :: iuGhbObs
    !
    allocate(newGhbObsWriter)
    call newGhbObsWriter%InitializeObs(basename)
    newGhbObsWriter%IuObs = IuGhbObs
    !
    return
  end subroutine createGhbObsWriter

  subroutine initialize_obs(this, basename, modifier)
    implicit none
    ! dummy
    class(GhbObsWriterType), intent(inout) :: this
    character(len=*), intent(in) :: basename
    character(len=*), optional, intent(in) :: modifier
    ! local
    character(len=4) :: ftype
    character(len=LINELENGTH) :: fname
    !
    this%basename = basename
    this%PkgType = 'GHB'
    this%Active = .true.
    ftype = 'OBS6'
    fname = trim(basename) // '.ghb.obs'
    ! Invoke superclass initializer
    call this%FileWriterType%InitializeFile(fname, ftype)
    this%FileWriterType%fileobj%FCode = FCINPUT
    !
    return
  end subroutine initialize_obs

  subroutine WriteContinuousGhb(this, igrid)
    implicit none
    ! dummy
    class(GhbObsWriterType) :: this
    integer, intent(in) :: igrid
    ! local
    integer :: ic, igrp, iu, iuob, j, jc, jl, jr, kc, kc1, kc2, &
               kob, nc
    integer :: iub, ncharid, ilay, irow, icol
    real :: rngbobs
    character(len=LENOBSNAME) :: oname
    character(len=LINELENGTH) :: outfilename
    character(len=LENOBSTYPE) :: otype, otypename
    type(FileType), pointer :: outfile => null()
    logical :: killonfailure
    ! formats
    1 format()
    10 format(a,1x,a)
    20 format(2x,a,2x,a)
    30 format(2x,a,2x,a,3(2x,i0))
    !
    otype = 'GHB'
    otypename = 'GHB'
    !
    iu = this%fileobj%IUnit
    killonfailure = .false.
    iub = iunit(7)    ! GHB package input unit
    iuob = iunit(35)  ! GHB obs input unit
    write(*,*)'Processing GBOB input...'
    call OBS2GHB7AR(iuob, iub, igrid)
    !
    ! Assign ncharid as width sufficient to support integers
    ! up to max number of GHB observation cells.
    ncharid = 1
    rngbobs = real(NQCGB)
    if (NQCGB > 0) then
      ncharid = int(log10(rngbobs)) + 1
    endif
    !
    outfile => this%Mf6Files%GetFileByUnit(IUGBOBSV, killonfailure)
    if (associated(outfile)) then
      outfilename = outfile%FName
    else
      outfilename = trim(this%basename) // '_ghbobs_out.csv'
    endif
    !
    write(iu,1)
    write(iu,10)'#','GHB package observations'
    write(iu,10)'BEGIN CONTINUOUS FILEOUT',trim(outfilename)
    !
    ! Initialize counters
    kob = 0
    kc1 = 1
    ! Iterate through cell groups
    do igrp=1,nqgb
      ! Number of cells in this cell group
      nc = nqclgb(igrp)
      kc2 = kc1 + nc - 1
      ! Iterate through cells for this observation time for this cell group
      kc = kc1 - 1
      cellloop: do ic=1,nc
        kc = kc + 1
        ilay = int(qcellgb(1,kc))
        irow = int(qcellgb(2,kc))
        icol = int(qcellgb(3,kc))
        !
        ! Do not duplicate cells
        if (kc > 1) then
          do j=1,kc-1
            jl = int(qcellgb(1,j))
            jr = int(qcellgb(2,j))
            jc = int(qcellgb(3,j))
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
      kc1 = kc1 + nc
    enddo
    !
    write(iu,10)'END','CONTINUOUS'
    !
    return
  end subroutine WriteContinuousGhb

end module GhbObsWriterModule
