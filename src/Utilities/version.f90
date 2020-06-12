module VersionModule
  use KindModule
  public
  ! -- modflow 6 version
  integer(I4B), parameter :: IDEVELOPMODE = 0
  character(len=40), parameter :: VERSION = '6.1.1 06/12/2020'
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
    &'This software has been approved for release by the U.S. Geological ',/,   &
    &'Survey (USGS). Although the software has been subjected to rigorous ',/,  &
    &'review, the USGS reserves the right to update the software as needed ',/, &
    &'pursuant to further analysis and review. No warranty, expressed or ',/,   &
    &'implied, is made by the USGS or the U.S. Government as to the ',/,        &
    &'functionality of the software and related material nor shall the ',/,     &
    &'fact of release constitute any such warranty. Furthermore, the ',/,       &
    &'software is released on condition that neither the USGS nor the U.S. ',/, &
    &'Government shall be held liable for any damages resulting from its ',/,   &
    &'authorized or unauthorized use. Also refer to the USGS Water ',/,         &
    &'Resources Software User Rights Notice for complete use, copyright, ',/,   &
    &'and distribution information.',/)"
end module VersionModule

