module OpenSpecModule
!  Code in this file defines values for OPEN-statement specifiers.  Some
!  of the values are extensions to ANSI Fortran 90 and 95.  One of the
!  specifiers is not included in ANSI FORTRAN 77. The included
!  specifiers are ACCESS, FORM and ACTION.
!
  CHARACTER(len=20) :: ACCESS, FORM, ACTION(2)
!
!
!  Specifiers for OPEN statements for unformatted files, which are
!  sometimes compiler specific.
!  The included specifiers are ACCESS and FORM.
!
!  ACCESS specifier --
!
!    Standard Fortran -- Use unless there is a reason to do otherwise.
!      DATA ACCESS/'SEQUENTIAL'/
  DATA ACCESS/'STREAM'/
!
!
!  FORM specifier --
!
!    Standard Fortran, which results in vendor dependent (non-portable)
!    files.  Use unless there is a reason to do otherwise.
  DATA FORM/'UNFORMATTED'/
!
!    Non-standard Fortran that causes code compiled by Compaq (Digital)
!    Fortran on personal computers to use unstructured non-formatted
!    files.  This may make it possible for the non-formatted files used
!    by MODFLOW to be used with programs that are compiled by other
!    compilers.
!      DATA FORM/'BINARY'/
!
!
!  OPEN-statement specifiers related to file-sharing.
!
!  ACTION specifier --
!
!    Standard FORTRAN 77 -- Eliminate the ACTION= specifier from all
!    OPEN statements in the source-code files.
!
!    Standard Fortran 90 and 95 -- Use unless there is a reason to do
!    otherwise.
  DATA(ACTION(IACT), IACT=1, 2)/'READ', 'READWRITE'/
!
!    Non-standard Fortran that causes code compiled by the Lahey LF90
!    compiler to create files that can be shared.  For use when parallel
!    processing is used or to enable an editor to view output files
!    while the program is running.
!      DATA (ACTION(I),I=1,2)/'READ,DENYWRITE','READWRITE,DENYNONE'/
!
end module OpenSpecModule
