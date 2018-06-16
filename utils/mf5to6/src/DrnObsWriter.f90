module DrnObsWriterModule
  use ConstantsModule, only: LENOBSNAME, LENOBSTYPE, LINELENGTH
  use ConstantsPHMFModule, only: FCINPUT
  use FileTypeModule, only: FileType
  use GLOBAL, only: IUNIT, NLAY, NROW, NCOL
  use InputOutputModule, only: same_word
  use OBSDRNMODULE, only: IUDROBSV, NQCLDR, NQOBDR, otimedr=>OTIME, &
                          qcelldr=>QCELL, NQDR, NQCDR
  use ObsWriterModule, only: ObsWriterType
  use utl7module, only: assign_ncharsizes_flow, build_obsname

  private
  public :: DrnObsWriterType, createDrnObsWriter

  type, extends(ObsWriterType) :: DrnObsWriterType
  contains
    procedure :: InitializeObs => initialize_obs
    procedure :: WriteContinuous => WriteContinuousDrn
  end type DrnObsWriterType

contains

  subroutine createDrnObsWriter(newDrnObsWriter, basename, iuDrnObs)
    implicit none
    type(DrnObsWriterType), pointer, intent(out) :: newDrnObsWriter
    character(len=*), intent(in) :: basename
    integer, intent(in) :: iuDrnObs
    !
    allocate(newDrnObsWriter)
    call newDrnObsWriter%InitializeObs(basename)
    newDrnObsWriter%IuObs = IuDrnObs
    !
    return
  end subroutine createDrnObsWriter

  subroutine initialize_obs(this, basename, modifier)
    implicit none
    ! dummy
    class(DrnObsWriterType), intent(inout) :: this
    character(len=*), intent(in) :: basename
    character(len=*), optional, intent(in) :: modifier
    ! local
    character(len=4) :: ftype
    character(len=LINELENGTH) :: fname
    !
    this%basename = basename
    this%PkgType = 'DRN'
    this%Active = .true.
    ftype = 'OBS6'
    fname = trim(basename) // '.drn.obs'
    ! Invoke superclass initializer
    call this%FileWriterType%InitializeFile(fname, ftype)
    this%FileWriterType%fileobj%FCode = FCINPUT
    !
    return
  end subroutine initialize_obs

  subroutine WriteContinuousDrn(this, igrid)
    implicit none
    ! dummy
    class(DrnObsWriterType) :: this
    integer, intent(in)     :: igrid
    ! local
    integer :: ic, igrp, iu, iuob, j, jc, jl, jr, kc, kc1, kc2, &
               kob, nc
    integer :: ilay, irow, icol, iub, ncharid
    real :: rndrobs
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
    otype = 'DRN'
    otypename = 'DRN'
    !
    iu = this%fileobj%IUnit
    killonfailure = .false.
    iub = iunit(3)    ! DRN package input unit
    iuob = iunit(33)  ! DRN obs input unit
    write(*,*)'Processing DROB input...'
    call OBS2DRN7AR(iuob, iub, igrid)
    !
    ! Assign ncharid as width sufficient to support integers
    ! up to max number of DRN observation cells.
    ncharid = 1
    rndrobs = real(NQCDR)
    if (NQCDR > 0) then
      ncharid = int(log10(rndrobs)) + 1
    endif
    !
    outfile => this%Mf6Files%GetFileByUnit(IUDROBSV, killonfailure)
    if (associated(outfile)) then
      outfilename = outfile%FName
    else
      outfilename = trim(this%basename) // '_drnobs_out.csv'
    endif
    !
    write(iu,1)
    write(iu,10)'#','Drain package observations'
    write(iu,10)'BEGIN CONTINUOUS FILEOUT',trim(outfilename)
    !
    ! Initialize counters
    kob = 0
    kc1 = 1
    ! Iterate through cell groups
    do igrp=1,NQDR
      ! Number of cells in this cell group
      nc = nqcldr(igrp)
      kc2 = kc1 + nc - 1
      ! Iterate through cells of this cell group
      kc = kc1 - 1
      cellloop: do ic=1,nc
        kc = kc + 1
        ilay = int(qcelldr(1,kc))
        irow = int(qcelldr(2,kc))
        icol = int(qcelldr(3,kc))
        !
        ! Do not duplicate cells
        if (kc > 1) then
          do j=1,kc-1
            jl = int(qcelldr(1,j))
            jr = int(qcelldr(2,j))
            jc = int(qcelldr(3,j))
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
  end subroutine WriteContinuousDrn

end module DrnObsWriterModule
