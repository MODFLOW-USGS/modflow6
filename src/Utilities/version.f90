module VersionModule
  use KindModule
  public
  ! -- modflow 6 version
  integer(I4B), parameter :: IDEVELOPMODE = 1
  character(len=40), parameter :: VERSION = '6.0.5 03/31/2019'
  character(len=10), parameter :: MFVNAM = ' 6'
  character(len=*), parameter  :: MFTITLE =                                     &
    'U.S. GEOLOGICAL SURVEY MODULAR HYDROLOGIC MODEL'
  character(len=*), parameter :: FMTTITLE =                                     &
    "(/,34X,'MODFLOW',A,/,                                                      &
    &16X,'U.S. GEOLOGICAL SURVEY MODULAR HYDROLOGIC MODEL',                     &
    &/,23X,'Version ',A/)"
  ! -- disclaimer must be appropriate for version (release or release candidate)
  character(len=*), parameter :: FMTDISCLAIMER =                                &
    "(/,                                                                        &
    &'This software is preliminary or provisional and is subject to ',/,        &
    &'revision. It is being provided to meet the need for timely best ',/,      &
    &'science. The software has not received final approval by the U.S. ',/,    &
    &'Geological Survey (USGS). No warranty, expressed or implied, is made ',/, &
    &'by the USGS or the U.S. Government as to the functionality of the ',/,    &
    &'software and related material nor shall the fact of release ',/,          &
    &'constitute any such warranty. The software is provided on the ',/,        &
    &'condition that neither the USGS nor the U.S. Government shall be held ',/,&
    &'liable for any damages resulting from the authorized or unauthorized ',/, &
    &'use of the software.',/)"
end module VersionModule

