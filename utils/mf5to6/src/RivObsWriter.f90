module RivObsWriterModule
  use ConstantsModule, only: LENOBSNAME, LENOBSTYPE, LINELENGTH
  use ConstantsPHMFModule, only: FCINPUT
  use FileTypeModule, only: FileType
  use GLOBAL, only: IUNIT, NLAY, NROW, NCOL
  use InputOutputModule, only: same_word
  use OBSRIVMODULE, only: IURVOBSV, NQRV, NQCLRV, NQOBRV, otimerv=>OTIME, &
                          qcellrv=>QCELL, NQCRV
  use ObsWriterModule, only: ObsWriterType
  use utl7module, only: assign_ncharsizes_flow, build_obsname

  private
  public :: RivObsWriterType, createRivObsWriter

  type, extends(ObsWriterType) :: RivObsWriterType
  contains
    procedure :: InitializeObs => initialize_obs
    procedure :: WriteContinuous => WriteContinuousRiv
  end type RivObsWriterType

contains

  subroutine createRivObsWriter(newRivObsWriter, basename, iuRivObs)
    implicit none
    type(RivObsWriterType), pointer, intent(out) :: newRivObsWriter
    character(len=*), intent(in) :: basename
    integer, intent(in) :: iuRivObs
    !
    allocate(newRivObsWriter)
    call newRivObsWriter%InitializeObs(basename)
    newRivObsWriter%IuObs = IuRivObs
    !
    return
  end subroutine createRivObsWriter

  subroutine initialize_obs(this, basename, modifier)
    implicit none
    ! dummy
    class(RivObsWriterType), intent(inout) :: this
    character(len=*), intent(in) :: basename
    character(len=*), optional, intent(in) :: modifier
    ! local
    character(len=4) :: ftype
    character(len=LINELENGTH) :: fname
    !
    this%basename = basename
    this%PkgType = 'RIV'
    this%Active = .true.
    ftype = 'OBS6'
    fname = trim(basename) // '.riv.obs'
    ! Invoke superclass initializer
    call this%FileWriterType%InitializeFile(fname, ftype)
    this%FileWriterType%fileobj%FCode = FCINPUT
    !
    return
  end subroutine initialize_obs

  subroutine WriteContinuousRiv(this, igrid)
    implicit none
    ! dummy
    class(RivObsWriterType) :: this
    integer, intent(in) :: igrid
    ! local
    integer :: ic, igrp, iu, iurvob, j, jc, jl, jr, kc, kc1, kc2, &
               kob, nc
    integer :: iub, ncharid, ilay, irow, icol
    real :: rnrvobs
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
    otype = 'RIV'
    otypename = 'RIV'
    !
    iu = this%fileobj%IUnit ! Ned todo: this needs to be the unit for the RivObs input file
    killonfailure = .false.
    iub = iunit(4)      ! RIV package input unit
    iurvob = iunit(34)  ! RIV obs input unit
    write(*,*)'Processing RVOB input...'
    call OBS2RIV7AR(iurvob, iub, igrid)
    !
    ! Assign ncharid as width sufficient to support integers
    ! up to max number of RIV observation cells.
    ncharid = 1
    rnrvobs = real(NQCRV)
    if (NQCRV > 0) then
      ncharid = int(log10(rnrvobs)) + 1
    endif
    !
    outfile => this%Mf6Files%GetFileByUnit(IURVOBSV, killonfailure)
    if (associated(outfile)) then
      outfilename = outfile%FName
    else
      outfilename = trim(this%basename) // '_rivobs_out.csv'
    endif
    !
    write(iu,1)
    write(iu,10)'#','River package observations'
    write(iu,10)'BEGIN CONTINUOUS FILEOUT',trim(outfilename)
    !
    ! Initialize counters
    kob = 0
    kc1 = 1
    ! Iterate through cell groups
    do igrp=1,nqrv
      ! Number of cells in this cell group
      nc = nqclrv(igrp)
      kc2 = kc1 + nc - 1
      ! Iterate through cells of cell group
      kc = kc1 - 1
      cellloop: do ic=1,nc
        kc = kc + 1
        ilay = int(qcellrv(1,kc))
        irow = int(qcellrv(2,kc))
        icol = int(qcellrv(3,kc))
        !
        ! Do not duplicate cells
        if (kc > 1) then
          do j=1,kc-1
            jl = int(qcellrv(1,j))
            jr = int(qcellrv(2,j))
            jc = int(qcellrv(3,j))
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
  end subroutine WriteContinuousRiv

end module RivObsWriterModule
