! -- MODFLOW 6 utility routines.
!
module InputOutputModule

  use KindModule, only: DP, I4B, I8B
  use SimVariablesModule, only: iunext, isim_mode, errmsg
  use SimModule, only: store_error, store_error_unit
  use ConstantsModule, only: IUSTART, IULAST,                                  &
                             LINELENGTH, LENBIGLINE, LENBOUNDNAME,             &
                             NAMEDBOUNDFLAG, MAXCHARLEN,                       &
                             TABLEFT, TABCENTER, TABRIGHT,                     &
                             TABSTRING, TABUCSTRING, TABINTEGER, TABREAL,      &
                             DZERO
  use GenericUtilitiesModule, only: is_same, sim_message
  private
  public :: GetUnit,                                                           &
            UPCASE, URWORD, ULSTLB, UBDSV4,                                    &
            ubdsv06, UBDSVB, UCOLNO, ULAPRW,                                   &
            ULASAV, ubdsv1, ubdsvc, ubdsvd, UWWORD,                            &
            same_word, get_node, get_ijk, padl, unitinquire,                   &
            ParseLine, ulaprufw, openfile,                                     &
            linear_interpolate, lowcase,                                       &
            read_line,                                                         &
            GetFileFromPath, extract_idnum_or_bndname, urdaux,                 &
            get_jk, print_format, BuildFixedFormat,                            &
            BuildFloatFormat, BuildIntFormat, fseek_stream,                    &
            get_nwords, u9rdcom,                                               &
            append_processor_id

  contains

  !> @brief Open a file
  !!
  !! Subroutine to open a file using the specified arguments
  !!
  !<
  subroutine openfile(iu, iout, fname, ftype, fmtarg_opt, accarg_opt,          &
                      filstat_opt, mode_opt)
    ! -- modules
    use OpenSpecModule, only: action
    implicit none
    ! -- dummy variables
    integer(I4B), intent(inout)       :: iu                 !< unit number
    integer(I4B), intent(in)          :: iout               !< output unit number to write a message (iout=0 does not print)
    character(len=*), intent(in) :: fname                   !< name of the file
    character(len=*), intent(in) :: ftype                   !< file type (e.g. WEL)
    character(len=*), intent(in), optional :: fmtarg_opt    !< file format, default is 'formatted'
    character(len=*), intent(in), optional :: accarg_opt    !< file access, default is 'sequential'
    character(len=*), intent(in), optional :: filstat_opt   !< file status, default is 'old'. Use 'REPLACE' for output file.
    integer(I4B), intent(in), optional :: mode_opt          !< simulation mode that is evaluated to determine if the file should be opened
    ! -- local variables
    character(len=20) :: fmtarg
    character(len=20) :: accarg
    character(len=20) :: filstat
    character(len=20) :: filact
    integer(I4B) :: imode
    integer(I4B) :: iflen
    integer(I4B) :: ivar
    integer(I4B) :: iuop
    ! -- formats
50  FORMAT(1X,/1X,'OPENED ',A,/                                                &
                 1X,'FILE TYPE:',A,'   UNIT ',I4,3X,'STATUS:',A,/              &
                 1X,'FORMAT:',A,3X,'ACCESS:',A/                                &
                 1X,'ACTION:',A/)
60  FORMAT(1X,/1X,'DID NOT OPEN ',A,/)
    !
    ! -- process mode_opt
    if (present(mode_opt)) then
      imode = mode_opt
    else
      imode = isim_mode
    end if
    !
    ! -- evaluate if the file should be opened
    if (isim_mode < imode) then
      if(iout > 0) then
        write(iout, 60) trim(fname)
      end if
    else
      !
      ! -- Default is to read an existing text file
      fmtarg = 'FORMATTED'
      accarg = 'SEQUENTIAL'
      filstat = 'OLD'
      !
      ! -- Override defaults
      if(present(fmtarg_opt)) then
        fmtarg = fmtarg_opt
        call upcase(fmtarg)
      endif
      if(present(accarg_opt)) then
        accarg = accarg_opt
        call upcase(accarg)
      endif
      if(present(filstat_opt)) then
        filstat = filstat_opt
        call upcase(filstat)
      endif
      if(filstat == 'OLD') then
        filact = action(1)
      else
        filact = action(2)
      endif
      !
      ! -- size of fname
      iflen = len_trim(fname)
      !
      ! -- Get a free unit number
      if(iu <= 0) then
        call freeunitnumber(iu)
      endif
      !
      ! -- Check to see if file is already open, if not then open the file
      inquire(file=fname(1:iflen), number=iuop)
      if(iuop > 0) then
        ivar = -1
      else
        open(unit=iu, file=fname(1:iflen), form=fmtarg, access=accarg,           &
           status=filstat, action=filact, iostat=ivar)
      endif
      !
      ! -- Check for an error
      if(ivar /= 0) then
        write (errmsg, '(3a,1x,i0,a)') &
          'Could not open "', fname(1:iflen), '" on unit', iu, '.'
        if(iuop > 0) then
          write (errmsg, '(a,1x,a,1x,i0,a)') &
            trim(errmsg), 'File already open on unit', iuop, '.'
        endif
        write (errmsg, '(a,1x,a,1x,a,a)') &
          trim(errmsg), 'Specified file status', trim(filstat), '.'
        write (errmsg, '(a,1x,a,1x,a,a)') &
          trim(errmsg), 'Specified file format', trim(fmtarg), '.'
        write (errmsg, '(a,1x,a,1x,a,a)') &
          trim(errmsg), 'Specified file access', trim(accarg), '.'
        write (errmsg, '(a,1x,a,1x,a,a)') &
          trim(errmsg), 'Specified file action', trim(filact), '.'
        write (errmsg, '(a,1x,a,1x,i0,a)') &
          trim(errmsg), 'IOSTAT error number', ivar, '.'
        write (errmsg, '(a,1x,a)') &
          trim(errmsg), 'STOP EXECUTION in subroutine openfile().'
        call store_error(errmsg, terminate=.TRUE.)
      endif
      !
      ! -- Write a message
      if(iout > 0) then
        write(iout, 50) fname(1:iflen),                                          &
                       ftype, iu, filstat,                                       &
                       fmtarg, accarg,                                           &
                       filact
      end if
    end if
    !
    ! -- return
    return
  end subroutine openfile

  !> @brief Assign a free unopened unit number
  !!
  !! Subroutine to assign a free unopened unit number to the iu dummy argument
  !!
  !<
  subroutine freeunitnumber(iu)
    ! -- modules
    implicit none
    ! -- dummy variables
    integer(I4B),intent(inout) :: iu  !< next free file unit number
    ! -- local variables
    integer(I4B) :: i
    logical :: opened
    !
    ! -- code
    do i = iunext, iulast
      inquire(unit=i, opened=opened)
      if(.not. opened) exit
    enddo
    iu = i
    iunext = iu + 1
    !
    ! -- return
    return
  end subroutine freeunitnumber

  !> @brief Get a free unit number
  !!
  !! Function to get a free unit number that hasn't been used
  !!
  !<
  function getunit()
    ! -- modules
    implicit none
    ! -- return
    integer(I4B) :: getunit  !< free unit number
    ! -- local variables
    integer(I4B) :: iunit
    !
    ! -- code
    call freeunitnumber(iunit)
    getunit = iunit
    !
    ! -- Return
    return
  end function getunit
  
  !> @brief Convert to upper case
  !!
  !! Subroutine to convert a character string to upper case.
  !!
  !<
  subroutine upcase(word)
    implicit none
    ! -- dummy variables  
    character (len=*), intent(inout) :: word  !< word to convert to upper case
    ! -- local variables
    integer(I4B) :: l
    integer(I4B) :: idiff
    integer(I4B) :: k
    !
    ! -- Compute the difference between lowercase and uppercase.
    l = len(word)
    idiff = ichar('a') - ichar('A')
    !
    ! -- Loop through the string and convert any lowercase characters.
    do k = 1, l
      IF (word(k:k) >= 'a' .and. word(k:k) <= 'z') &
        word(k:k) = char(ichar(word(k:k)) - idiff)
    end do
    !
    ! -- return.
    return
    end subroutine upcase

  !> @brief Convert to lower case
  !!
  !! Subroutine to convert a character string to lower case.
  !!
  !<
  subroutine lowcase(word)
    implicit none
    ! -- dummy variables
    character(len=*) :: word  !< 
    ! -- local variables
    integer(I4B) :: idiff, k, l
    !
    ! -- compute the difference between lowercase and uppercase.
    l = len(word)
    idiff = ichar('a') - ichar('A')
    !
    ! -- loop through the string and convert any uppercase characters.
    do k = 1, l
      if(word(k:k) >= 'A' .and. word(k:k) <= 'Z') then
        word(k:k)=char(ichar(word(k:k))+idiff)
      endif
    enddo
    !
    ! -- return.
    return
  end subroutine lowcase

  !> @brief Append processor id to a string
  !!
  !! Subroutine to append the processor id to a string 
  !! before the file extension (extension is the 
  !! string after the last '.' in the string. If there
  !! is no '.' in the string the processor id is appended 
  !! to the end of the string.
  !!
  !<
  subroutine append_processor_id(name, proc_id)
    ! -- dummy variables
    character(len=linelength), intent(inout) :: name  !< file name
    integer(I4B), intent(in) :: proc_id  !< processor id
    ! -- local variables
    character(len=linelength) :: name_local
    character(len=linelength) :: extension_local
    integer(I4B) :: ipos0
    integer(I4B) :: ipos1
    !
    name_local = name
    call lowcase(name_local)
    ipos0 = index(name_local, ".", back=.TRUE.)
    ipos1 = len_trim(name)
    if (ipos0 > 0) then
      write(extension_local, '(a)') name(ipos0:ipos1)
    else
      ipos0 = ipos1
      extension_local = ''
    end if
    write(name, '(a,a,i0,a)') &
      name(1:ipos0-1), '.p', proc_id, trim(adjustl(extension_local))
    !
    ! -- return
    return
  end subroutine append_processor_id

  !> @brief Create a formatted line
  !!
  !! Subroutine to create a formatted line with specified alignment
  !! and column separators. Like URWORD, UWWORD works with strings,
  !! integers, and floats. Can pass an optional format statement,
  !! alignment, and column separator.
  !!
  !<
  subroutine UWWORD(LINE,ICOL,ILEN,NCODE,C,N,R,FMT,ALIGNMENT,SEP)
    implicit none
    ! -- dummy variables
    character (len=*), intent(inout) :: LINE         !< line
    integer(I4B), intent(inout) :: ICOL              !< column to write to line
    integer(I4B), intent(in) :: ILEN                 !< current length of line
    integer(I4B), intent(in) :: NCODE                !< code for data type to write
    character (len=*), intent(in) :: C               !< character data type
    integer(I4B), intent(in) :: N                    !< integer data type
    real(DP), intent(in) :: R                        !< float data type
    character (len=*), optional, intent(in) :: FMT   !< format statement
    integer(I4B), optional, intent(in) :: ALIGNMENT  !< alignment specifier
    character (len=*), optional, intent(in) :: SEP   !< column separator
    ! -- local variables
    character (len=16) :: cfmt
    character (len=16) :: cffmt
    character (len=ILEN) :: cval
    integer(I4B) :: ialign
    integer(I4B) :: i
    integer(I4B) :: ispace
    integer(I4B) :: istop
    integer(I4B) :: ipad
    integer(I4B) :: ireal
    ! -- code
    !
    ! -- initialize locals
    ipad = 0
    ireal = 0
    !
    ! -- process dummy variables
    if (present(FMT)) then
      CFMT = FMT
    else
      select case(NCODE)
        case(TABSTRING, TABUCSTRING)
          write(cfmt, '(A,I0,A)') '(A', ILEN, ')'
        case(TABINTEGER)
          write(cfmt, '(A,I0,A)') '(I', ILEN, ')'
        case(TABREAL)
          ireal = 1
          i = ILEN - 7
          write(cfmt, '(A,I0,A,I0,A)') '(1PG', ILEN, '.', i, ')'
          if (R >= DZERO) then
            ipad = 1
          end if
      end select
    end if
    write(cffmt, '(A,I0,A)') '(A', ILEN, ')'

    if (present(ALIGNMENT)) then
      ialign = ALIGNMENT
    else
      ialign = TABRIGHT
    end if
    !
    ! -- 
    if (NCODE == TABSTRING .or. NCODE == TABUCSTRING) then
      cval = C
      if (NCODE == TABUCSTRING) then
        call UPCASE(cval)
      end if
    else if (NCODE == TABINTEGER) then
      write(cval, cfmt) N
    else if (NCODE == TABREAL) then
      write(cval, cfmt) R
    end if
    !
    ! -- apply alignment to cval
    if (len_trim(adjustl(cval)) > ILEN) then
      cval = adjustl(cval)
    else
      cval = trim(adjustl(cval))
    end if
    if (ialign == TABCENTER) then
      i = len_trim(cval)
      ispace = (ILEN - i) / 2
      if (ireal > 0) then
        if (ipad > 0) then
          cval = ' ' //trim(adjustl(cval))
        else
          cval = trim(adjustl(cval))
        end if
      else
        cval = repeat(' ', ispace) // trim(cval)
      end if
    else if (ialign == TABLEFT) then
      cval = trim(adjustl(cval))
      if (ipad > 0) then
        cval = ' ' //trim(adjustl(cval))
      end if
    else
      cval = adjustr(cval)
    end if
    if (NCODE == TABUCSTRING) then
      call UPCASE(cval)
    end if
    !
    ! -- increment istop to the end of the column
    istop = ICOL + ILEN - 1
    !
    ! -- write final string to line
    write(LINE(ICOL:istop), cffmt) cval

    ICOL = istop + 1

    if (present(SEP)) then
      i = len(SEP)
      istop = ICOL + i
      write(LINE(ICOL:istop), '(A)') SEP
      ICOL = istop
    end if
    !
    !-- return
    return
  end subroutine UWWORD

  !> @brief Extract a word from a string
  !!
  !! Subroutine to extract a word from a line of text, and optionally
  !! convert the word to a number. The last character in the line is 
  !! set to blank so that if any problems occur with finding a word, 
  !! istart and istop will point to this blank character. Thus, a word 
  !! will always be returned unless there is a numeric conversion error.
  !! Be sure that the last character in line is not an important character
  !! because it will always be set to blank.
  !!
  !! A word starts with the first character that is not a space or
  !! comma, and ends when a subsequent character that is a space
  !! or comma. Note that these parsing rules do not treat two
  !! commas separated by one or more spaces as a null word.
  !!
  !! For a word that begins with "'" or '"', the word starts with
  !! the character after the quote and ends with the character preceding 
  !! a subsequent quote. Thus, a quoted word can include spaces and commas.
  !! The quoted word cannot contain a quote character of the same type 
  !! within the word but can contain a different quote character. For 
  !! example, "WORD'S" or 'WORD"S'.
  !!
  !! Number conversion error is written to unit iout if iout is positive; 
  !! error is written to default output if iout is 0; no error message is 
  !! written if iout is negative.
  !!
  !<
  SUBROUTINE URWORD(LINE,ICOL,ISTART,ISTOP,NCODE,N,R,IOUT,IN)
    ! -- dummy variables  
    character(len=*) :: LINE                 !< line to parse
    integer(I4B), intent(inout) :: icol      !< current column in line
    integer(I4B), intent(inout) :: istart    !< starting character position of the word
    integer(I4B), intent(inout) :: istop     !< ending character position of the word
    integer(I4B), intent(in) :: ncode        !< word conversion flag (1) upper case, (2) integer, (3) real number
    integer(I4B), intent(inout) :: n         !< integer data type
    real(DP), intent(inout) :: r             !< float data type
    integer(I4B), intent(in) :: iout         !< output listing file unit
    integer(I4B), intent(in) :: in           !< input file unit number
    ! -- local variables
    CHARACTER(len=20) STRING                
    CHARACTER(len=30) RW
    CHARACTER(len=1) TAB
    CHARACTER(len=1) CHAREND
    character(len=200) :: msg
    character(len=LINELENGTH) :: msg_line
    !
    ! -- code
    TAB=CHAR(9)
    !
    ! -- Set last char in LINE to blank and set ISTART and ISTOP to point
    !    to this blank as a default situation when no word is found.  If
    !    starting location in LINE is out of bounds, do not look for a word.
    LINLEN=LEN(LINE)
    LINE(LINLEN:LINLEN)=' '
    ISTART=LINLEN
    ISTOP=LINLEN
    LINLEN=LINLEN-1
    IF(ICOL.LT.1 .OR. ICOL.GT.LINLEN) GO TO 100
    !
    ! -- Find start of word, which is indicated by first character that
    !    is not a blank, a comma, or a tab.
    DO 10 I=ICOL,LINLEN
      IF(LINE(I:I).NE.' ' .AND. LINE(I:I).NE.',' .AND. &
         LINE(I:I).NE.TAB) GO TO 20
10  CONTINUE
    ICOL=LINLEN+1
    GO TO 100
    !
    ! -- Found start of word.  Look for end.
    !    When word is quoted, only a quote can terminate it.
    !    SEARCH FOR A SINGLE (CHAR(39)) OR DOUBLE (CHAR(34)) QUOTE
20  IF(LINE(I:I).EQ.CHAR(34) .OR. LINE(I:I).EQ.CHAR(39)) THEN
      IF (LINE(I:I).EQ.CHAR(34)) THEN
        CHAREND = CHAR(34)
      ELSE
        CHAREND = CHAR(39)
      END IF
      I=I+1
      IF(I.LE.LINLEN) THEN
        DO 25 J=I,LINLEN
          IF(LINE(J:J).EQ.CHAREND) GO TO 40
25      CONTINUE
      END IF
    !
    ! -- When word is not quoted, space, comma, or tab will terminate.
    ELSE
      DO 30 J=I,LINLEN
        IF(LINE(J:J).EQ.' ' .OR. LINE(J:J).EQ.',' .OR. &
           LINE(J:J).EQ.TAB) GO TO 40
30    CONTINUE
    END IF
    !
    ! -- End of line without finding end of word; set end of word to
    !    end of line.
    J=LINLEN+1
    !
    ! -- Found end of word; set J to point to last character in WORD and
    !    set ICOL to point to location for scanning for another word.
40  ICOL=J+1
    J=J-1
    IF(J.LT.I) GO TO 100
    ISTART=I
    ISTOP=J
    !
    ! -- Convert word to upper case and RETURN if NCODE is 1.
    IF(NCODE.EQ.1) THEN
      IDIFF=ICHAR('a')-ICHAR('A')
      DO 50 K=ISTART,ISTOP
        IF(LINE(K:K).GE.'a' .AND. LINE(K:K).LE.'z') &
           LINE(K:K)=CHAR(ICHAR(LINE(K:K))-IDIFF)
50    CONTINUE
      RETURN
    END IF
    !
    ! -- Convert word to a number if requested.
100 IF(NCODE.EQ.2 .OR. NCODE.EQ.3) THEN
      RW=' '
      L=30-ISTOP+ISTART
      IF(L.LT.1) GO TO 200
      RW(L:30)=LINE(ISTART:ISTOP)
      IF(NCODE.EQ.2) READ(RW,'(I30)',ERR=200) N
      IF(NCODE.EQ.3) READ(RW,'(F30.0)',ERR=200) R
    END IF
    RETURN
    !
    ! -- Number conversion error.
200 IF(NCODE.EQ.3) THEN
      STRING= 'A REAL NUMBER'
      L=13
    ELSE
      STRING= 'AN INTEGER'
      L=10
    END IF
    !
    ! -- If output unit is negative, set last character of string to 'E'.
    IF(IOUT.LT.0) THEN
      N=0
      R=0.
      LINE(LINLEN+1:LINLEN+1)='E'
      RETURN
    !
    ! -- If output unit is positive; write a message to output unit.
    ELSE IF(IOUT.GT.0) THEN
      IF(IN.GT.0) THEN
        write(msg_line,201) IN,LINE(ISTART:ISTOP),STRING(1:L)
      ELSE
        WRITE(msg_line,202) LINE(ISTART:ISTOP),STRING(1:L)
      END IF
      call sim_message(msg_line, iunit=IOUT, skipbefore=1)
      call sim_message(LINE, iunit=IOUT, fmt='(1x,a)')
201   FORMAT(1X,'FILE UNIT ',I4,' : ERROR CONVERTING "',A, &
             '" TO ',A,' IN LINE:')
202   FORMAT(1X,'KEYBOARD INPUT : ERROR CONVERTING "',A, &
             '" TO ',A,' IN LINE:')
    !
    ! -- If output unit is 0; write a message to default output.
    ELSE
      IF(IN.GT.0) THEN
        write(msg_line,201) IN,LINE(ISTART:ISTOP),STRING(1:L)
      ELSE
        WRITE(msg_line,202) LINE(ISTART:ISTOP),STRING(1:L)
      END IF
      call sim_message(msg_line, iunit=IOUT, skipbefore=1)
      call sim_message(LINE, iunit=IOUT, fmt='(1x,a)')
    END IF
    !
    ! -- STOP after storing error message.
    call lowcase(string)
    if (in > 0) then
      write(msg,205) in,line(istart:istop),trim(string)
    else
      write(msg,207) line(istart:istop),trim(string)
    endif
205 format('File unit ',I0,': Error converting "',A, &
           '" to ',A,' in following line:')
207 format('Keyboard input: Error converting "',A, &
           '" to ',A,' in following line:')
    call store_error(msg)
    call store_error(trim(line))
    call store_error_unit(in)
    !
    ! -- return
END SUBROUTINE URWORD

      SUBROUTINE ULSTLB(IOUT,LABEL,CAUX,NCAUX,NAUX)
!C     ******************************************************************
!C     PRINT A LABEL FOR A LIST
!C     ******************************************************************
!C
!C        SPECIFICATIONS:
!C     ------------------------------------------------------------------
      CHARACTER(len=*) LABEL
      CHARACTER(len=16) CAUX(NCAUX)
      CHARACTER(len=400) BUF
      CHARACTER(len=1) DASH(400)
      DATA DASH/400*'-'/
!C     ------------------------------------------------------------------
!C
!C1------Construct the complete label in BUF.  Start with BUF=LABEL.
      BUF=LABEL
!C
!C2------Add auxiliary data names if there are any.
      NBUF=LEN(LABEL)+9
      IF(NAUX.GT.0) THEN
         DO 10 I=1,NAUX
         N1=NBUF+1
         NBUF=NBUF+16
         BUF(N1:NBUF)=CAUX(I)
10       CONTINUE
      END IF
!C
!C3------Write the label.
      WRITE(IOUT,103) BUF(1:NBUF)
  103 FORMAT(1X,A)
!C
!C4------Add a line of dashes.
      WRITE(IOUT,104) (DASH(J),J=1,NBUF)
  104 FORMAT(1X,400A)
!C
!C5------Return.
      RETURN
      END SUBROUTINE ULSTLB
!

      SUBROUTINE UBDSV4(KSTP,KPER,TEXT,NAUX,AUXTXT,IBDCHN, &
     &          NCOL,NROW,NLAY,NLIST,IOUT,DELT,PERTIM,TOTIM)
!C     ******************************************************************
!C     WRITE HEADER RECORDS FOR CELL-BY-CELL FLOW TERMS FOR ONE COMPONENT
!C     OF FLOW PLUS AUXILIARY DATA USING A LIST STRUCTURE.  EACH ITEM IN
!C     THE LIST IS WRITTEN BY MODULE UBDSVB
!C     ******************************************************************
!C
!C        SPECIFICATIONS:
!C     ------------------------------------------------------------------
      CHARACTER(len=16) :: TEXT
      character(len=16), dimension(:) :: AUXTXT
      real(DP),intent(in) :: delt,pertim,totim
      character(len=*), parameter :: fmt = &
      "(1X,'UBDSV4 SAVING ',A16,' ON UNIT',I7,' AT TIME STEP',I7,"// &
      "', STRESS PERIOD',I7)"
!C     ------------------------------------------------------------------
!C
!C1------WRITE UNFORMATTED RECORDS IDENTIFYING DATA.
      IF(IOUT.GT.0) WRITE(IOUT,fmt) TEXT,IBDCHN,KSTP,KPER
      WRITE(IBDCHN) KSTP,KPER,TEXT,NCOL,NROW,-NLAY
      WRITE(IBDCHN) 5,DELT,PERTIM,TOTIM
      WRITE(IBDCHN) NAUX+1
      IF(NAUX.GT.0) WRITE(IBDCHN) (AUXTXT(N),N=1,NAUX)
      WRITE(IBDCHN) NLIST
!C
!C2------RETURN
      RETURN
      END SUBROUTINE UBDSV4

      SUBROUTINE UBDSVB(IBDCHN,ICRL,Q,VAL,NVL,NAUX,LAUX)
!C     ******************************************************************
!C     WRITE ONE VALUE OF CELL-BY-CELL FLOW PLUS AUXILIARY DATA USING
!C     A LIST STRUCTURE.
!C     ******************************************************************
!C
!C        SPECIFICATIONS:
!C     ------------------------------------------------------------------
      real(DP), DIMENSION(nvl) :: VAL
      real(DP) :: q
!C     ------------------------------------------------------------------
!C
!C1------WRITE CELL NUMBER AND FLOW RATE
      IF(NAUX.GT.0) THEN
         N2=LAUX+NAUX-1
         WRITE(IBDCHN) ICRL,Q,(VAL(N),N=LAUX,N2)
      ELSE
         WRITE(IBDCHN) ICRL,Q
      END IF
!C
!C2------RETURN
      RETURN
      END SUBROUTINE UBDSVB

  SUBROUTINE UCOLNO(NLBL1,NLBL2,NSPACE,NCPL,NDIG,IOUT)
!C     ******************************************************************
!C     OUTPUT COLUMN NUMBERS ABOVE A MATRIX PRINTOUT
!C        NLBL1 IS THE START COLUMN LABEL (NUMBER)
!C        NLBL2 IS THE STOP COLUMN LABEL (NUMBER)
!C        NSPACE IS NUMBER OF BLANK SPACES TO LEAVE AT START OF LINE
!C        NCPL IS NUMBER OF COLUMN NUMBERS PER LINE
!C        NDIG IS NUMBER OF CHARACTERS IN EACH COLUMN FIELD
!C        IOUT IS OUTPUT CHANNEL
!C     ******************************************************************
!C
!C        SPECIFICATIONS:
!C     ------------------------------------------------------------------
      CHARACTER(len=1) DOT,SPACE,DG,BF
      DIMENSION BF(1000),DG(10)
!C
      DATA DG(1),DG(2),DG(3),DG(4),DG(5),DG(6),DG(7),DG(8),DG(9),DG(10)/ &
     &         '0','1','2','3','4','5','6','7','8','9'/
      DATA DOT,SPACE/'.',' '/
!C     ------------------------------------------------------------------
!C
!C1------CALCULATE # OF COLUMNS TO BE PRINTED (NLBL), WIDTH
!C1------OF A LINE (NTOT), NUMBER OF LINES (NWRAP).
      if (iout<=0) return
      WRITE(IOUT,1)
    1 FORMAT(1X)
      NLBL=NLBL2-NLBL1+1
      N=NLBL
      IF(NLBL.GT.NCPL) N=NCPL
      NTOT=NSPACE+N*NDIG
      IF(NTOT.GT.1000) GO TO 50
      NWRAP=(NLBL-1)/NCPL + 1
      J1=NLBL1-NCPL
      J2=NLBL1-1
!C
!C2------BUILD AND PRINT EACH LINE
      DO 40 N=1,NWRAP
!C
!C3------CLEAR THE BUFFER (BF).
      DO 20 I=1,1000
      BF(I)=SPACE
   20 CONTINUE
      NBF=NSPACE
!C
!C4------DETERMINE FIRST (J1) AND LAST (J2) COLUMN # FOR THIS LINE.
      J1=J1+NCPL
      J2=J2+NCPL
      IF(J2.GT.NLBL2) J2=NLBL2
!C
!C5------LOAD THE COLUMN #'S INTO THE BUFFER.
      DO 30 J=J1,J2
      NBF=NBF+NDIG
      I2=J/10
      I1=J-I2*10+1
      BF(NBF)=DG(I1)
      IF(I2.EQ.0) GO TO 30
      I3=I2/10
      I2=I2-I3*10+1
      BF(NBF-1)=DG(I2)
      IF(I3.EQ.0) GO TO 30
      I4=I3/10
      I3=I3-I4*10+1
      BF(NBF-2)=DG(I3)
      IF(I4.EQ.0) GO TO 30
      IF(I4.GT.9) THEN
!C5A-----If more than 4 digits, use "X" for 4th digit.
         BF(NBF-3)='X'
      ELSE
         BF(NBF-3)=DG(I4+1)
      END IF
   30 CONTINUE
!C
!C6------PRINT THE CONTENTS OF THE BUFFER (I.E. PRINT THE LINE).
      WRITE(IOUT,31) (BF(I),I=1,NBF)
   31 FORMAT(1X,1000A1)
!C
   40 CONTINUE
!C
!C7------PRINT A LINE OF DOTS (FOR AESTHETIC PURPOSES ONLY).
   50 NTOT=NTOT
      IF(NTOT.GT.1000) NTOT=1000
      WRITE(IOUT,51) (DOT,I=1,NTOT)
   51 FORMAT(1X,1000A1)
!C
!C8------RETURN
      RETURN
      END SUBROUTINE UCOLNO

      SUBROUTINE ULAPRW(BUF,TEXT,KSTP,KPER,NCOL,NROW,ILAY,IPRN,IOUT)
!C     ******************************************************************
!C     PRINT 1 LAYER ARRAY
!C     ******************************************************************
!C
!C        SPECIFICATIONS:
!C     ------------------------------------------------------------------
      CHARACTER(len=16) TEXT
      real(DP),dimension(ncol,nrow) :: buf
!C     ------------------------------------------------------------------
!C
      if (iout<=0) return
!C1------PRINT A HEADER DEPENDING ON ILAY
      IF(ILAY.GT.0) THEN
         WRITE(IOUT,1) TEXT,ILAY,KSTP,KPER
    1    FORMAT('1',/2X,A,' IN LAYER ',I3,' AT END OF TIME STEP ',I3, &
     &     ' IN STRESS PERIOD ',I4/2X,75('-'))
      ELSE IF(ILAY.LT.0) THEN
         WRITE(IOUT,2) TEXT,KSTP,KPER
    2    FORMAT('1',/1X,A,' FOR CROSS SECTION AT END OF TIME STEP',I3, &
     &     ' IN STRESS PERIOD ',I4/1X,79('-'))
      END IF
!C
!C2------MAKE SURE THE FORMAT CODE (IP OR IPRN) IS
!C2------BETWEEN 1 AND 21.
      IP=IPRN
      IF(IP.LT.1 .OR. IP.GT.21) IP=12
!C
!C3------CALL THE UTILITY MODULE UCOLNO TO PRINT COLUMN NUMBERS.
      IF(IP.EQ.1) CALL UCOLNO(1,NCOL,0,11,11,IOUT)
      IF(IP.EQ.2) CALL UCOLNO(1,NCOL,0,9,14,IOUT)
      IF(IP.GE.3 .AND. IP.LE.6) CALL UCOLNO(1,NCOL,3,15,8,IOUT)
      IF(IP.GE.7 .AND. IP.LE.11) CALL UCOLNO(1,NCOL,3,20,6,IOUT)
      IF(IP.EQ.12) CALL UCOLNO(1,NCOL,0,10,12,IOUT)
      IF(IP.GE.13 .AND. IP.LE.18) CALL UCOLNO(1,NCOL,3,10,7,IOUT)
      IF(IP.EQ.19) CALL UCOLNO(1,NCOL,0,5,13,IOUT)
      IF(IP.EQ.20) CALL UCOLNO(1,NCOL,0,6,12,IOUT)
      IF(IP.EQ.21) CALL UCOLNO(1,NCOL,0,7,10,IOUT)
!C
!C4------LOOP THROUGH THE ROWS PRINTING EACH ONE IN ITS ENTIRETY.
      DO I=1,NROW
      SELECT CASE(IP)

      CASE(1)
!C------------ FORMAT 11G10.3
      WRITE(IOUT,11) I,(BUF(J,I),J=1,NCOL)
11    FORMAT(1X,I3,2X,1PG10.3,10(1X,G10.3):/(5X,11(1X,G10.3)))

      CASE(2)
!C------------ FORMAT 9G13.6
      WRITE(IOUT,21) I,(BUF(J,I),J=1,NCOL)
21    FORMAT(1X,I3,2X,1PG13.6,8(1X,G13.6):/(5X,9(1X,G13.6)))

      CASE(3)
!C------------ FORMAT 15F7.1
      WRITE(IOUT,31) I,(BUF(J,I),J=1,NCOL)
31    FORMAT(1X,I3,1X,15(1X,F7.1):/(5X,15(1X,F7.1)))

      CASE(4)
!C------------ FORMAT 15F7.2
      WRITE(IOUT,41) I,(BUF(J,I),J=1,NCOL)
41    FORMAT(1X,I3,1X,15(1X,F7.2):/(5X,15(1X,F7.2)))

      CASE(5)
!C------------ FORMAT 15F7.3
      WRITE(IOUT,51) I,(BUF(J,I),J=1,NCOL)
51    FORMAT(1X,I3,1X,15(1X,F7.3):/(5X,15(1X,F7.3)))

      CASE(6)
!C------------ FORMAT 15F7.4
      WRITE(IOUT,61) I,(BUF(J,I),J=1,NCOL)
61    FORMAT(1X,I3,1X,15(1X,F7.4):/(5X,15(1X,F7.4)))

      CASE(7)
!C------------ FORMAT 20F5.0
      WRITE(IOUT,71) I,(BUF(J,I),J=1,NCOL)
71    FORMAT(1X,I3,1X,20(1X,F5.0):/(5X,20(1X,F5.0)))

      CASE(8)
!C------------ FORMAT 20F5.1
      WRITE(IOUT,81) I,(BUF(J,I),J=1,NCOL)
81    FORMAT(1X,I3,1X,20(1X,F5.1):/(5X,20(1X,F5.1)))

      CASE(9)
!C------------ FORMAT 20F5.2
      WRITE(IOUT,91) I,(BUF(J,I),J=1,NCOL)
91    FORMAT(1X,I3,1X,20(1X,F5.2):/(5X,20(1X,F5.2)))

      CASE(10)
!C------------ FORMAT 20F5.3
      WRITE(IOUT,101) I,(BUF(J,I),J=1,NCOL)
101   FORMAT(1X,I3,1X,20(1X,F5.3):/(5X,20(1X,F5.3)))

      CASE(11)
!C------------ FORMAT 20F5.4
      WRITE(IOUT,111) I,(BUF(J,I),J=1,NCOL)
111   FORMAT(1X,I3,1X,20(1X,F5.4):/(5X,20(1X,F5.4)))

      CASE(12)
!C------------ FORMAT 10G11.4
      WRITE(IOUT,121) I,(BUF(J,I),J=1,NCOL)
121   FORMAT(1X,I3,2X,1PG11.4,9(1X,G11.4):/(5X,10(1X,G11.4)))

      CASE(13)
!C------------ FORMAT 10F6.0
      WRITE(IOUT,131) I,(BUF(J,I),J=1,NCOL)
131   FORMAT(1X,I3,1X,10(1X,F6.0):/(5X,10(1X,F6.0)))

      CASE(14)
!C------------ FORMAT 10F6.1
      WRITE(IOUT,141) I,(BUF(J,I),J=1,NCOL)
141   FORMAT(1X,I3,1X,10(1X,F6.1):/(5X,10(1X,F6.1)))

      CASE(15)
!C------------ FORMAT 10F6.2
      WRITE(IOUT,151) I,(BUF(J,I),J=1,NCOL)
151   FORMAT(1X,I3,1X,10(1X,F6.2):/(5X,10(1X,F6.2)))

      CASE(16)
!C------------ FORMAT 10F6.3
      WRITE(IOUT,161) I,(BUF(J,I),J=1,NCOL)
161   FORMAT(1X,I3,1X,10(1X,F6.3):/(5X,10(1X,F6.3)))

      CASE(17)
!C------------ FORMAT 10F6.4
      WRITE(IOUT,171) I,(BUF(J,I),J=1,NCOL)
171   FORMAT(1X,I3,1X,10(1X,F6.4):/(5X,10(1X,F6.4)))

      CASE(18)
!C------------ FORMAT 10F6.5
      WRITE(IOUT,181) I,(BUF(J,I),J=1,NCOL)
181   FORMAT(1X,I3,1X,10(1X,F6.5):/(5X,10(1X,F6.5)))

      CASE(19)
!C------------FORMAT 5G12.5
      WRITE(IOUT,191) I,(BUF(J,I),J=1,NCOL)
191   FORMAT(1X,I3,2X,1PG12.5,4(1X,G12.5):/(5X,5(1X,G12.5)))

      CASE(20)
!C------------FORMAT 6G11.4
      WRITE(IOUT,201) I,(BUF(J,I),J=1,NCOL)
201   FORMAT(1X,I3,2X,1PG11.4,5(1X,G11.4):/(5X,6(1X,G11.4)))

      CASE(21)
!C------------FORMAT 7G9.2
      WRITE(IOUT,211) I,(BUF(J,I),J=1,NCOL)
211   FORMAT(1X,I3,2X,1PG9.2,6(1X,G9.2):/(5X,7(1X,G9.2)))

      END SELECT
      END DO
      !
      ! -- flush file
      flush(IOUT)
      !
      ! -- return
      RETURN
      END SUBROUTINE ULAPRW

     SUBROUTINE ULASAV(BUF,TEXT,KSTP,KPER,PERTIM,TOTIM,NCOL, &
     &                   NROW,ILAY,ICHN)
!C     ******************************************************************
!C     SAVE 1 LAYER ARRAY ON DISK
!C     ******************************************************************
!C
!C        SPECIFICATIONS:
!C     ------------------------------------------------------------------
      CHARACTER(len=16) TEXT
      real(DP),dimension(ncol,nrow) :: buf
      real(DP) :: pertim,totim
!C     ------------------------------------------------------------------
!C
!C1------WRITE AN UNFORMATTED RECORD CONTAINING IDENTIFYING
!C1------INFORMATION.
      WRITE(ICHN) KSTP,KPER,PERTIM,TOTIM,TEXT,NCOL,NROW,ILAY
!C
!C2------WRITE AN UNFORMATTED RECORD CONTAINING ARRAY VALUES
!C2------THE ARRAY IS DIMENSIONED (NCOL,NROW)
      WRITE(ICHN) ((BUF(IC,IR),IC=1,NCOL),IR=1,NROW)
      !
      ! -- flush file
      flush(ICHN)
!C
!C3------RETURN
      RETURN
     END SUBROUTINE ULASAV

  subroutine ubdsv1(kstp, kper, text, ibdchn, buff, ncol, nrow, nlay, iout, &
                    delt, pertim, totim)
! ******************************************************************************
! Record cell-by-cell flow terms for one component of flow as a 3-D array with
!   extra record to indicate delt, pertim, and totim
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    implicit none
    integer(I4B), intent(in) :: kstp
    integer(I4B), intent(in) :: kper
    character(len=*), intent(in) :: text
    integer(I4B), intent(in) :: ibdchn
    real(DP), dimension(:), intent(in) :: buff
    integer(I4B), intent(in) :: ncol
    integer(I4B), intent(in) :: nrow
    integer(I4B), intent(in) :: nlay
    integer(I4B), intent(in) :: iout
    real(DP), intent(in) :: delt
    real(DP), intent(in) :: pertim
    real(DP), intent(in) :: totim
    ! -- format
    character(len=*), parameter :: fmt = &
      "(1X,'UBDSV1 SAVING ',A16,' ON UNIT',I7,' AT TIME STEP',I7,"// &
      "', STRESS PERIOD',I7)"
! ------------------------------------------------------------------------------
    !
    ! -- Write records
    if(iout > 0) write(iout, fmt) text, ibdchn, kstp, kper
    write(ibdchn) kstp,kper,text,ncol,nrow,-nlay
    write(ibdchn) 1,delt,pertim,totim
    write(ibdchn) buff
    !
    ! -- flush file
    flush(ibdchn)
    !
    ! -- return
    return
  end subroutine ubdsv1

  subroutine ubdsv06(kstp,kper,text,                                 &
                     modelnam1,paknam1,modelnam2,paknam2,            &
                     ibdchn,naux,auxtxt,                             &
                     ncol,nrow,nlay,nlist,iout,delt,pertim,totim)
! ******************************************************************
! write header records for cell-by-cell flow terms for one component
! of flow.  each item in the list is written by module ubdsvc
! ******************************************************************
!
!     specifications:
! ------------------------------------------------------------------
    implicit none
    integer(I4B), intent(in) :: kstp
    integer(I4B), intent(in) :: kper
    character(len=*), intent(in) :: text
    character(len=*), intent(in) :: modelnam1
    character(len=*), intent(in) :: paknam1
    character(len=*), intent(in) :: modelnam2
    character(len=*), intent(in) :: paknam2
    integer(I4B), intent(in) :: naux
    character(len=16), dimension(:), intent(in) :: auxtxt
    integer(I4B), intent(in) :: ibdchn
    integer(I4B), intent(in) :: ncol
    integer(I4B), intent(in) :: nrow
    integer(I4B), intent(in) :: nlay
    integer(I4B), intent(in) :: nlist
    integer(I4B), intent(in) :: iout
    real(DP), intent(in) :: delt
    real(DP), intent(in) :: pertim
    real(DP), intent(in) :: totim
    ! -- local variables
    integer(I4B) :: n
    ! -- format
    character(len=*), parameter :: fmt = &
      "(1X,'UBDSV06 SAVING ',A16,' IN MODEL ',A16,' PACKAGE ',A16,"//&
      "'CONNECTED TO MODEL ',A16,' PACKAGE ',A16,"//                 &
      "' ON UNIT',I7,' AT TIME STEP',I7,', STRESS PERIOD',I7)"
! ------------------------------------------------------------------
!
! write unformatted records identifying data.
    if (iout > 0) write(iout,fmt) text, modelnam1, paknam1,          &
                                  modelnam2, paknam2,                &
                                  ibdchn, kstp, kper
    write(ibdchn) kstp,kper,text,ncol,nrow,-nlay
    write(ibdchn) 6,delt,pertim,totim
    write(ibdchn) modelnam1
    write(ibdchn) paknam1
    write(ibdchn) modelnam2
    write(ibdchn) paknam2
    write(ibdchn) naux+1
    if (naux > 0) write(ibdchn) (auxtxt(n),n=1,naux)
    write(ibdchn) nlist
    !
    ! -- return
    return
  end subroutine ubdsv06

  subroutine ubdsvc(ibdchn, n, q, naux, aux)
! ******************************************************************************
! Write one value of cell-by-cell flow using a list structure. From node (n)
! and to node (n2) are written to the file
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    implicit none
    integer(I4B), intent(in) :: ibdchn
    integer(I4B), intent(in) :: n
    real(DP), intent(in) :: q
    integer(I4B), intent(in) :: naux
    real(DP), dimension(naux), intent(in) :: aux
    ! -- local variables
    integer(I4B) :: nn
! ------------------------------------------------------------------------------
    !
    ! -- Write record
    if (naux > 0) then
        write(ibdchn) n,q,(aux(nn),nn=1,naux)
    else
        write(ibdchn) n,q
    end if
    !
    ! -- return
    return
  end subroutine ubdsvc

  subroutine ubdsvd(ibdchn, n, n2, q, naux, aux)
! ******************************************************************************
! Write one value of cell-by-cell flow using a list structure. From node (n)
! and to node (n2) are written to the file
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    implicit none
    integer(I4B), intent(in) :: ibdchn
    integer(I4B), intent(in) :: n
    integer(I4B), intent(in) :: n2
    real(DP), intent(in) :: q
    integer(I4B), intent(in) :: naux
    real(DP), dimension(naux), intent(in) :: aux
    ! -- local variables
    integer(I4B) :: nn
! ------------------------------------------------------------------------------
    !
    ! -- Write record
    if (naux > 0) then
        write(ibdchn) n,n2,q,(aux(nn),nn=1,naux)
    else
        write(ibdchn) n,n2,q
    end if
    !
    ! -- return
    return
  end subroutine ubdsvd

  logical function same_word(word1, word2)
    ! Perform a case-insensitive comparison of two words
    implicit none
    ! -- dummy variables variables
    character(len=*), intent(in) :: word1, word2
    ! -- local variables
    character(len=200) :: upword1, upword2
    !
    upword1 = word1
    call upcase(upword1)
    upword2 = word2
    call upcase(upword2)
    same_word = (upword1==upword2)
    return
  end function same_word

  function get_node(ilay, irow, icol, nlay, nrow, ncol)
    ! Return node number, given layer, row, and column indices
    ! for a structured grid.  If any argument is invalid,
    ! return -1.
    implicit none
    ! -- return
    integer(I4B) :: get_node
    ! -- dummy variables
    integer(I4B), intent(in) :: ilay, irow, icol, nlay, nrow, ncol
    !
    if (nlay>0 .and. nrow>0 .and. ncol>0) then
      if (ilay>0 .and. ilay<=nlay) then
        if (irow>0 .and. irow<=nrow) then
          if (icol>0 .and. icol<=ncol) then
            get_node = icol + ncol*(irow-1) + (ilay-1)*nrow*ncol
            return
          endif
        endif
      endif
    endif
    get_node = -1
    return
  end function get_node

  subroutine get_ijk(nodenumber, nrow, ncol, nlay, irow, icol, ilay)
    ! Calculate irow, icol, and ilay from the nodenumber and grid
    ! dimensions.  If nodenumber is invalid, set irow, icol, and
    ! ilay to -1
    implicit none
    ! -- dummy variables
    integer(I4B), intent(in) :: nodenumber
    integer(I4B), intent(in) :: nrow
    integer(I4B), intent(in) :: ncol
    integer(I4B), intent(in) :: nlay
    integer(I4B), intent(out) :: irow
    integer(I4B), intent(out) :: icol
    integer(I4B), intent(out) :: ilay
    ! -- local variables
    integer(I4B) :: nodes
    integer(I4B) :: ij
    !
    nodes = nlay * nrow * ncol
    if(nodenumber < 1 .or. nodenumber > nodes) then
      irow = -1
      icol = -1
      ilay = -1
    else
      ilay = (nodenumber - 1) / (ncol * nrow) + 1
      ij = nodenumber - (ilay - 1) * ncol * nrow
      irow = (ij - 1) / ncol + 1
      icol = ij - (irow - 1) * ncol
    endif
    !
    return
  end subroutine get_ijk
  
  !> @brief Function for string manipulation
  !<
  function padl(str, width) result(res)
    ! -- local
    character(len=*), intent(in) :: str
    integer, intent(in) :: width
    ! -- Return
    character(len=max(len_trim(str), width)) :: res
! ------------------------------------------------------------------------------
    res = str
    res = adjustr(res)
    !
    ! -- Return
    return
  end function

  subroutine get_jk(nodenumber, ncpl, nlay, icpl, ilay)
    ! Calculate icpl, and ilay from the nodenumber and grid
    ! dimensions.  If nodenumber is invalid, set irow, icol, and
    ! ilay to -1
    implicit none
    ! -- dummy variables
    integer(I4B), intent(in) :: nodenumber
    integer(I4B), intent(in) :: ncpl
    integer(I4B), intent(in) :: nlay
    integer(I4B), intent(out) :: icpl
    integer(I4B), intent(out) :: ilay
    ! -- local variables
    integer(I4B) :: nodes
    !
    nodes = ncpl * nlay
    if(nodenumber < 1 .or. nodenumber > nodes) then
      icpl = -1
      ilay = -1
    else
      ilay = (nodenumber - 1) / ncpl + 1
      icpl = nodenumber - (ilay - 1) * ncpl
    endif
    !
    return
  end subroutine get_jk

  subroutine unitinquire(iu)
    ! -- dummy variables
    integer(I4B) :: iu
    ! -- local variables
    character(len=LINELENGTH) :: line
    character(len=100) :: fname, ac, act, fm, frm, seq, unf
    ! -- format
    character(len=*), parameter :: fmta =                                        &
       &"('unit:',i4,'  name:',a,'  access:',a,'  action:',a)"                                  
    character(len=*), parameter :: fmtb =                                        &
       &"('    formatted:',a,'  sequential:',a,'  unformatted:',a,'  form:',a)"                                  
    ! -- code
    !
    ! -- set strings using inquire statement
    inquire(unit=iu, name=fname, access=ac, action=act, formatted=fm,            &
            sequential=seq, unformatted=unf, form=frm)
    !
    ! -- write the results of the inquire statement
    write(line,fmta) iu, trim(fname), trim(ac), trim(act)
    call sim_message(line)
    write(line,fmtb) trim(fm), trim(seq), trim(unf), trim(frm)
    call sim_message(line)
    !
    ! -- return
    return
  end subroutine unitinquire

  subroutine ParseLine(line, nwords, words, inunit, filename)
    ! Parse a line into words. Blanks and commas are recognized as
    ! delimiters. Multiple blanks between words is OK, but multiple
    ! commas between words is treated as an error. Quotation marks
    ! are not recognized as delimiters.
    use ConstantsModule, only: LINELENGTH
    implicit none
    ! -- dummy variables
    character(len=*), intent(in) :: line
    integer(I4B), intent(inout) :: nwords
    character(len=*), allocatable, dimension(:), intent(inout) :: words
    integer(I4B), intent(in), optional :: inunit
    character(len=*), intent(in), optional :: filename
    ! -- local variables
    integer(I4B) :: i, idum, istart, istop, linelen, lloc
    real(DP) :: rdum
    !
    nwords = 0
    if (allocated(words)) then
      deallocate(words)
    endif
    linelen = len(line)
    !
    ! -- get the number of words in a line and allocate words array
    nwords = get_nwords(line)
    allocate(words(nwords))
    !
    ! -- Populate words array and return
    lloc = 1
    do i = 1, nwords
      call URWORD(line, lloc, istart, istop, 0, idum, rdum, 0, 0)
      words(i) = line(istart:istop)
    end do
    !
    ! -- return
    return
  end subroutine ParseLine

  subroutine ulaprufw(ncol, nrow, kstp, kper, ilay, iout, buf, text, userfmt, &
                      nvalues, nwidth, editdesc)
    ! **************************************************************************
    ! Print 1 layer array with user formatting in wrap format
    ! **************************************************************************
    !
    !    Specifications:
    ! --------------------------------------------------------------------------
    implicit none
    ! -- dummy variables
    integer(I4B), intent(in) :: ncol, nrow, kstp, kper, ilay, iout
    real(DP),dimension(ncol,nrow), intent(in) :: buf
    character(len=*), intent(in) :: text
    character(len=*), intent(in) :: userfmt
    integer(I4B), intent(in) :: nvalues, nwidth
    character(len=1), intent(in) :: editdesc
    ! -- local variables
    integer(I4B) :: i, j, nspaces
    ! formats
    1 format('1',/2X,A,' IN LAYER ',I3,' AT END OF TIME STEP ',I3, &
          ' IN STRESS PERIOD ',I4/2X,75('-'))
    2 format('1',/1X,A,' FOR CROSS SECTION AT END OF TIME STEP',I3, &
          ' IN STRESS PERIOD ',I4/1X,79('-'))
    ! ------------------------------------------------------------------
    !
    if (iout<=0) return
    ! -- Print a header depending on ILAY
    if (ilay > 0) then
       write(iout,1) trim(text), ilay, kstp, kper
    else if(ilay < 0) then
       write(iout,2) trim(text), kstp, kper
    end if
    !
    ! -- Print column numbers.
    nspaces = 0
    if (editdesc == 'F') nspaces = 3
    call ucolno(1, ncol, nspaces, nvalues, nwidth+1, iout)
    !
    ! -- Loop through the rows, printing each one in its entirety.
    do i=1,nrow
      write(iout,userfmt) i,(buf(j,i),j=1,ncol)
    end do
    !
    ! -- flush file
    flush(IOUT)
    !
    ! -- return
    return
  end subroutine ulaprufw

  function read_line(iu, eof) result (astring)
    ! This function reads a line of arbitrary length and returns
    ! it.  The returned string can be stored in a deferred-length
    ! character variable, for example:
    !
    !    integer(I4B) :: iu
    !    character(len=:), allocatable :: my_string
    !    logical :: eof
    !    iu = 8
    !    open(iu,file='my_file')
    !    my_string = read_line(iu, eof)
    !
    implicit none
    ! -- dummy variables
    integer(I4B), intent(in)           :: iu
    logical, intent(out)          :: eof
    character(len=:), allocatable :: astring
    ! -- local variables
    integer(I4B)        :: isize, istat
    character(len=256)  :: buffer
    character(len=1000) :: ermsg, fname
    character(len=7)    :: fmtd
    logical             :: lop
    ! -- format
20  format('Error in read_line: File ',i0,' is not open.')
30  format('Error in read_line: Attempting to read text ' // &
              'from unformatted file: "',a,'"')
40  format('Error reading from file "',a,'" opened on unit ',i0, &
              ' in read_line.')
    !
    astring = ''
    eof = .false.
    do
      read(iu, '(a)', advance='NO', iostat=istat, size=isize, end=99) buffer
      if (istat > 0) then
        ! Determine error if possible, report it, and stop.
        if (iu <= 0) then
          ermsg = 'Programming error in call to read_line: ' // &
                  'Attempt to read from unit number <= 0'
        else
          inquire(unit=iu,opened=lop,name=fname,formatted=fmtd)
          if (.not. lop) then
            write(ermsg,20) iu
          elseif (fmtd == 'NO' .or. fmtd == 'UNKNOWN') then
            write(ermsg, 30) trim(fname)
          else
            write(ermsg,40) trim(fname), iu
          endif
        endif
        call store_error(ermsg)
        call store_error_unit(iu)
      endif
      astring = astring // buffer(:isize)
      ! An end-of-record condition stops the loop.
      if (istat < 0) then
        return
      endif
    enddo
    !
    return
99  continue
    ! An end-of-file condition returns an empty string.
    eof = .true.
    return
    !
  end function read_line

  subroutine GetFileFromPath(pathname, filename)
    implicit none
    ! -- dummy variables
    character(len=*), intent(in) :: pathname
    character(len=*), intent(out) :: filename
    ! -- local variables
    integer(I4B) :: i, istart, istop, lenpath
    character(len=1) :: fs = '/'
    character(len=1) :: bs = '\'
    !
    filename = ''
    lenpath = len_trim(pathname)
    istart = 1
    istop = lenpath
    loop: do i=lenpath,1,-1
      if (pathname(i:i) == fs .or. pathname(i:i) == bs) then
        if (i == istop) then
          istop = istop - 1
        else
          istart = i + 1
          exit loop
        endif
      endif
    enddo loop
    if (istop >= istart) then
      filename = pathname(istart:istop)
    endif
    !
    return
  end subroutine GetFileFromPath

  subroutine extract_idnum_or_bndname(line, icol, istart, istop, idnum, bndname)
    ! Starting at position icol, define string as line(istart:istop).
    ! If string can be interpreted as an integer(I4B), return integer in idnum argument.
    ! If token is not an integer(I4B), assume it is a boundary name, return NAMEDBOUNDFLAG
    ! in idnum, convert string to uppercase and return it in bndname.
    implicit none
    ! -- dummy variables
    character(len=*),            intent(inout) :: line
    integer(I4B),                     intent(inout) :: icol, istart, istop
    integer(I4B),                     intent(out)   :: idnum
    character(len=LENBOUNDNAME), intent(out)   :: bndname
    ! -- local variables
    integer(I4B) :: istat, ndum, ncode=0
    real(DP) :: rdum
    !
    call urword(line, icol, istart, istop, ncode, ndum, rdum, 0, 0)
    read(line(istart:istop),*,iostat=istat) ndum
    if (istat == 0) then
      idnum = ndum
      bndname = ''
    else
      idnum = NAMEDBOUNDFLAG
      bndname = line(istart:istop)
      call upcase(bndname)
    endif
    !
    return
  end subroutine extract_idnum_or_bndname

  subroutine urdaux(naux, inunit, iout, lloc, istart, istop, auxname, line, text)
! ******************************************************************************
! Read auxiliary variables from an input line
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use ArrayHandlersModule, only: ExpandArray
    use ConstantsModule,     only: LENAUXNAME
    ! -- implicit
    implicit none
    ! -- dummy variables
    integer(I4B), intent(inout) :: naux
    integer(I4B), intent(in) :: inunit
    integer(I4B), intent(in) :: iout
    integer(I4B), intent(inout) :: lloc
    integer(I4B), intent(inout) :: istart
    integer(I4B), intent(inout) :: istop
    character(len=LENAUXNAME), allocatable, dimension(:), intent(inout) :: auxname
    character(len=*), intent(inout) :: line
    character(len=*), intent(in) :: text
    ! -- local variables
    integer(I4B) :: n, linelen
    integer(I4B) :: iauxlen
    real(DP) :: rval
! ------------------------------------------------------------------------------
    linelen = len(line)
    if(naux > 0) then
      write(errmsg,'(a)') 'Auxiliary variables already specified. Auxiliary ' // &
        'variables must be specified on one line in the options block.'
      call store_error(errmsg)
      call store_error_unit(inunit)
    endif
    auxloop: do
      call urword(line, lloc, istart, istop, 1, n, rval, iout, inunit)
      if (istart >= linelen) exit auxloop
      iauxlen = istop - istart + 1
      if (iauxlen > LENAUXNAME) then
        write (errmsg, '(a, a, a, i0, a, i0, a)') &
          'Found auxiliary variable (', line(istart:istop), &
          ') with a name of size ', iauxlen, &
          '. Auxiliary variable names must be len than or equal&
          & to ', LENAUXNAME, ' characters.'
        call store_error(errmsg)
        call store_error_unit(inunit)
      end if      
      naux = naux + 1
      call ExpandArray(auxname)
      auxname(naux) = line(istart:istop)
      if(iout > 0) then
        write(iout, "(4X,'AUXILIARY ',a,' VARIABLE: ',A)")                     &
          trim(adjustl(text)), auxname(naux)
      endif
    enddo auxloop
    !
    return
  end subroutine urdaux

  subroutine print_format(linein, cdatafmp, editdesc, nvaluesp, nwidthp, inunit)
! ******************************************************************************
! print_format -- define the print or save format
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
! Define cdatafmp as a Fortran output format based on user input.  Also define
! nvalues, nwidth, and editdesc.
!
!   Syntax for linein:
!     COLUMNS nval WIDTH nwid [DIGITS ndig [options]]
!
! Where:
!     nval = Number of values per line.
!     nwid = Number of character places to be used for each value.
!     ndig = Number of digits to the right of the decimal point (required
!            for real array).
!     options are:
!            editoption: One of [EXPONENTIAL, FIXED, GENERAL, SCIENTIFIC]
! A default value should be passed in for editdesc as G, I, E, F, or S.
! If I is passed in, then the fortran format will be for an integer variable.
! ------------------------------------------------------------------------------
    ! -- dummy variables
    character(len=*), intent(in) :: linein
    character(len=*), intent(inout) :: cdatafmp
    character(len=*), intent(inout) :: editdesc
    integer(I4B), intent(inout) :: nvaluesp
    integer(I4B), intent(inout) :: nwidthp
    integer(I4B), intent(in) :: inunit
    ! -- local variables
    character(len=len(linein)) :: line
    character(len=20), dimension(:), allocatable :: words
    character(len=100) :: ermsg
    integer(I4B) :: ndigits=0, nwords=0
    integer(I4B) :: i, ierr
    logical :: isint
! ------------------------------------------------------------------------------
    !
    ! -- Parse line and initialize values
    line(:) = linein(:)
    call ParseLine(line, nwords, words, inunit)
    ierr = 0
    i = 0
    isint = .false.
    if(editdesc == 'I') isint = .true.
    !
    ! -- Check array name
    if (nwords < 1) then
      ermsg = 'Could not build PRINT_FORMAT from line' // trim(line)
      call store_error(trim(ermsg))
      ermsg = 'Syntax is: COLUMNS <columns> WIDTH <width> DIGITS &
              &<digits> <format>'
      call store_error(trim(ermsg))
      call store_error_unit(inunit)
    endif
    !
    ermsg = 'Error setting PRINT_FORMAT. Syntax is incorrect in line:'
    if (nwords >= 4) then
      if (.not. same_word(words(1), 'COLUMNS')) ierr = 1
      if (.not. same_word(words(3), 'WIDTH')) ierr = 1
      ! -- Read nvalues and nwidth
      if (ierr == 0) then
        read(words(2), *, iostat=ierr) nvaluesp
      endif
      if (ierr == 0) then
        read(words(4), *, iostat=ierr) nwidthp
      endif
    else
      ierr = 1
    endif
    if (ierr /= 0) then
      call store_error(ermsg)
      call store_error(line)
      ermsg = 'Syntax is: COLUMNS <columns> WIDTH <width> &
              &DIGITS <digits> <format>'
      call store_error(trim(ermsg))
      call store_error_unit(inunit)
    endif
    i = 4
    !
    if (.not. isint) then
      ! -- Check for DIGITS specification
      if (nwords >= 5) then
        if (.not. same_word(words(5), 'DIGITS')) ierr = 1
        ! -- Read ndigits
        read(words(6), *, iostat=ierr) ndigits
      else
        ierr = 1
      endif
      i = i + 2
    endif
    !
    ! -- Check for EXPONENTIAL | FIXED | GENERAL | SCIENTIFIC option.
    ! -- Check for LABEL, WRAP, and STRIP options.
    do
      i = i + 1
      if (i <= nwords) then
        call upcase(words(i))
        select case (words(i))
        case ('EXPONENTIAL')
          editdesc = 'E'
          if (isint) ierr = 1
        case ('FIXED')
          editdesc = 'F'
          if (isint) ierr = 1
        case ('GENERAL')
          editdesc = 'G'
          if (isint) ierr = 1
        case ('SCIENTIFIC')
          editdesc = 'S'
          if (isint) ierr = 1
        case default
          ermsg = 'Error in format specification. Unrecognized option: ' // words(i)
          call store_error(ermsg)
          ermsg = 'Valid values are EXPONENTIAL, FIXED, GENERAL, or SCIENTIFIC.'
          call store_error(ermsg)
          call store_error_unit(inunit)
        end select
      else
        exit
      endif
    enddo
    if (ierr /= 0) then
      call store_error(ermsg)
      call store_error(line)
      call store_error_unit(inunit)
    endif
    !
    ! -- Build the output format.
    select case (editdesc)
    case ('I')
      call BuildIntFormat(nvaluesp, nwidthp, cdatafmp)
    case ('F')
      call BuildFixedFormat(nvaluesp, nwidthp, ndigits, cdatafmp)
    case ('E', 'G', 'S')
      call BuildFloatFormat(nvaluesp, nwidthp, ndigits, editdesc, cdatafmp)
    end select
    !
    return
  end subroutine print_format

  subroutine BuildFixedFormat(nvalsp, nwidp, ndig, outfmt, prowcolnum)
    ! Build a fixed format for printing or saving a real array
    implicit none
    ! -- dummy variables
    integer(I4B), intent(in) :: nvalsp, nwidp, ndig
    character(len=*), intent(inout) :: outfmt
    logical, intent(in), optional :: prowcolnum  ! default true
    ! -- local variables
    character(len=8)   :: cvalues, cwidth, cdigits
    character(len=60)  :: ufmt
    logical :: prowcolnumlocal
    ! formats
    10 format(i8)
    !
    if (present(prowcolnum)) then
      prowcolnumlocal = prowcolnum
    else
      prowcolnumlocal = .true.
    endif
    !
    ! -- Convert integers to characters and left-adjust
    write(cdigits,10) ndig
    cdigits = adjustl(cdigits)
    !
    ! -- Build format for printing to the list file in wrap format
    write(cvalues,10) nvalsp
    cvalues = adjustl(cvalues)
    write(cwidth,10) nwidp
    cwidth = adjustl(cwidth)
    if (prowcolnumlocal) then
      ufmt = '(1x,i3,1x,'
    else
      ufmt = '(5x,'
    endif
    ufmt = trim(ufmt) // cvalues
    ufmt = trim(ufmt) // '(1x,f'
    ufmt = trim(ufmt) // cwidth
    ufmt = trim(ufmt) // '.'
    ufmt = trim(ufmt) // cdigits
    ufmt = trim(ufmt) // '):/(5x,'
    ufmt = trim(ufmt) // cvalues
    ufmt = trim(ufmt) // '(1x,f'
    ufmt = trim(ufmt) // cwidth
    ufmt = trim(ufmt) // '.'
    ufmt = trim(ufmt) // cdigits
    ufmt = trim(ufmt) // ')))'
    outfmt = ufmt
    !
    return
  end subroutine BuildFixedFormat

  subroutine BuildFloatFormat(nvalsp, nwidp, ndig, editdesc, outfmt, prowcolnum)
    ! Build a floating-point format for printing or saving a real array
    implicit none
    ! -- dummy variables
    integer(I4B), intent(in) :: nvalsp, nwidp, ndig
    character(len=*), intent(in) :: editdesc
    character(len=*), intent(inout) :: outfmt
    logical, intent(in), optional :: prowcolnum  ! default true
    ! -- local variables
    character(len=8)   :: cvalues,  cwidth, cdigits
    character(len=60)  :: ufmt
    logical :: prowcolnumlocal
    ! formats
    10 format(i8)
    !
    if (present(prowcolnum)) then
      prowcolnumlocal = prowcolnum
    else
      prowcolnumlocal = .true.
    endif
    !
    ! -- Build the format
    write(cdigits,10) ndig
    cdigits = adjustl(cdigits)
    ! -- Convert integers to characters and left-adjust
    write(cwidth,10) nwidp
    cwidth = adjustl(cwidth)
    ! -- Build format for printing to the list file
    write(cvalues, 10) (nvalsp - 1)
    cvalues = adjustl(cvalues)
    if (prowcolnumlocal) then
      ufmt = '(1x,i3,2x,1p,' // editdesc
    else
      ufmt = '(6x,1p,' // editdesc
    endif
    ufmt = trim(ufmt) // cwidth
    ufmt = trim(ufmt) // '.'
    ufmt = trim(ufmt) // cdigits
    if (nvalsp>1) then
      ufmt = trim(ufmt) // ','
      ufmt = trim(ufmt) // cvalues
      ufmt = trim(ufmt) // '(1x,'
      ufmt = trim(ufmt) // editdesc
      ufmt = trim(ufmt) // cwidth
      ufmt = trim(ufmt) // '.'
      ufmt = trim(ufmt) // cdigits
      ufmt = trim(ufmt) // ')'
    endif
    ufmt = trim(ufmt) // ':/(5x,'
    write(cvalues, 10) nvalsp
    cvalues = adjustl(cvalues)
    ufmt = trim(ufmt) // cvalues
    ufmt = trim(ufmt) // '(1x,'
    ufmt = trim(ufmt) // editdesc
    ufmt = trim(ufmt) // cwidth
    ufmt = trim(ufmt) // '.'
    ufmt = trim(ufmt) // cdigits
    ufmt = trim(ufmt) // ')))'
    outfmt = ufmt
    !
    return
  end subroutine BuildFloatFormat

  subroutine BuildIntFormat(nvalsp, nwidp, outfmt, prowcolnum)
    ! Build a format for printing or saving an integer array
    implicit none
    ! -- dummy variables
    integer(I4B), intent(in) :: nvalsp, nwidp
    character(len=*), intent(inout) :: outfmt
    logical, intent(in), optional :: prowcolnum  ! default true
    ! -- local variables
    character(len=8)   :: cvalues, cwidth
    character(len=60)  :: ufmt
    logical :: prowcolnumlocal
    ! formats
    10 format(i8)
    !
    if (present(prowcolnum)) then
      prowcolnumlocal = prowcolnum
    else
      prowcolnumlocal = .true.
    endif
    !
    ! -- Build format for printing to the list file in wrap format
    write(cvalues,10)nvalsp
    cvalues = adjustl(cvalues)
    write(cwidth,10)nwidp
    cwidth = adjustl(cwidth)
    if (prowcolnumlocal) then
      ufmt = '(1x,i3,1x,'
    else
      ufmt = '(5x,'
    endif
    ufmt = trim(ufmt) // cvalues
    ufmt = trim(ufmt) // '(1x,i'
    ufmt = trim(ufmt) // cwidth
    ufmt = trim(ufmt) // '):/(5x,'
    ufmt = trim(ufmt) // cvalues
    ufmt = trim(ufmt) // '(1x,i'
    ufmt = trim(ufmt) // cwidth
    ufmt = trim(ufmt) // ')))'
    outfmt = ufmt
    !
    return
  end subroutine BuildIntFormat


  !> @brief Get the number of words in a string
  !!
  !! Function to get the number of words in a string
  !!
  !<
  function get_nwords(line)
    ! -- return variable
    integer(I4B) :: get_nwords            !< number of words in a string
    ! -- dummy variables
    character(len=*), intent(in) :: line  !< line
    ! -- local variables
    integer(I4B) :: linelen
    integer(I4B) :: lloc
    integer(I4B) :: istart
    integer(I4B) :: istop
    integer(I4B) :: idum
    real(DP) :: rdum
    !
    ! -- initialize variables
    get_nwords = 0
    linelen = len(line)
    !
    ! -- Count words in line and allocate words array
    lloc = 1
    do
      call URWORD(line, lloc, istart, istop, 0, idum, rdum, 0, 0)
      if (istart == linelen) exit
      get_nwords = get_nwords + 1
    end do
    !
    ! -- return
    return
  end function get_nwords

  subroutine fseek_stream(iu, offset, whence, status)
! ******************************************************************************
! Move the file pointer.  Patterned after fseek, which is not 
! supported as part of the fortran standard.  For this subroutine to work
! the file must have been opened with access='stream' and action='readwrite'.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    integer(I4B), intent(in) :: iu
    integer(I4B), intent(in) :: offset
    integer(I4B), intent(in) :: whence
    integer(I4B), intent(inout) :: status
    integer(I8B) :: ipos
! ------------------------------------------------------------------------------
    !
    inquire(unit=iu, size=ipos)
    
    select case(whence)
    case(0)
      !
      ! -- whence = 0, offset is relative to start of file
      ipos = 0 + offset
    case(1)
      !
      ! -- whence = 1, offset is relative to current pointer position
      inquire(unit=iu, pos=ipos)
      ipos = ipos + offset
    case(2)
      !
      ! -- whence = 2, offset is relative to end of file
      inquire(unit=iu, size=ipos)
      ipos = ipos + offset
    end select
    !
    ! -- position the file pointer to ipos
    write(iu, pos=ipos, iostat=status)
    inquire(unit=iu, pos=ipos)
    !
    ! -- return
    return
  end subroutine fseek_stream
  
  subroutine u9rdcom(iin, iout, line, ierr)
! ******************************************************************************
! Read until non-comment line found and then return line.  Different from
! u8rdcom in that line is a deferred length character string, which allows
! any length lines to be read using the get_line subroutine.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use, intrinsic :: iso_fortran_env, only: IOSTAT_END
    implicit none
    ! -- dummy variables
    integer(I4B),         intent(in) :: iin
    integer(I4B),         intent(in) :: iout
    character (len=:), allocatable, intent(inout) :: line
    integer(I4B),        intent(out) :: ierr
    ! -- local variables
    character (len=:), allocatable :: linetemp
    character (len=2), parameter :: comment = '//'
    character(len=1), parameter  :: tab = CHAR(9)
    logical :: iscomment
    integer(I4B) :: i, j, l, istart, lsize
! ------------------------------------------------------------------------------
    !code
    !
    !readerrmsg = ''
    line = comment
    pcomments: do
      call get_line(iin, line, ierr)
      if (ierr == IOSTAT_END) then
        ! -- End of file reached. Return with ierr = IOSTAT_END
        !    and line as an empty string
        line = ' '
        exit pcomments
      elseif (ierr /= 0) then
        ! -- Other error...report it
        call unitinquire(iin)
        write(errmsg, *) 'u9rdcom: Could not read from unit: ',iin
        call store_error(errmsg, terminate=.TRUE.)
      endif
      if (len_trim(line).lt.1) then
        line = comment
        cycle
      end if
      !
      ! Ensure that any initial tab characters are treated as spaces
      cleartabs: do
        !
        ! -- adjustl manually to avoid stack overflow
        lsize = len(line)
        istart = 1
        allocate(character(len=lsize) :: linetemp)
        do j = 1, lsize
          if (line(j:j) /= ' ' .and. line(j:j) /= ',' .and. line(j:j) /= char(9)) then
            istart = j
            exit
          end if
        end do
        linetemp(:) = ' '
        linetemp(:) = line(istart:)
        line(:) = linetemp(:)
        deallocate(linetemp)
        !
        ! -- check for comment
        iscomment = .false.
        select case (line(1:1))
          case ('#')
            iscomment = .true.
            exit cleartabs
          case ('!')
            iscomment = .true.
            exit cleartabs
          case (tab)
            line(1:1) = ' '
            cycle cleartabs
          case default
            if (line(1:2).eq.comment) iscomment = .true.
            if (len_trim(line) < 1) iscomment = .true.
            exit cleartabs
        end select
      end do cleartabs
      !
      if (.not.iscomment) then
        exit pcomments
      else
        if (iout > 0) then
          !find the last non-blank character.
          l=len(line)
          do i = l, 1, -1
            if(line(i:i).ne.' ') then
              exit
            end if
          end do
          !print the line up to the last non-blank character.
          write(iout,'(1x,a)') line(1:i)
        end if
      end if
    end do pcomments
    return
  end subroutine u9rdcom

  subroutine get_line(lun, line, iostat)
! ******************************************************************************
! Read an unlimited length line from unit number lun into a deferred-length
! character string (line).  Tack on a single space to the end so that 
! routines like URWORD continue to function as before.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy variables
    integer(I4B), intent(in) :: lun
    character(len=:), intent(out), allocatable :: line
    integer(I4B), intent(out) :: iostat
    ! -- local variables
    integer(I4B), parameter :: buffer_len = MAXCHARLEN
    character(len=buffer_len) :: buffer
    character(len=:), allocatable :: linetemp
    integer(I4B) :: size_read, linesize
! ------------------------------------------------------------------------------
    !
    ! -- initialize
    line = ''
    linetemp = ''
    !
    ! -- process
    do
      read ( lun, '(A)',  &
          iostat = iostat,  &
          advance = 'no',  &
          size = size_read ) buffer
      if (is_iostat_eor(iostat)) then
        linesize = len(line)
        deallocate(linetemp)
        allocate(character(len=linesize) :: linetemp)
        linetemp(:) = line(:)
        deallocate(line)
        allocate(character(len=linesize + size_read + 1) :: line)
        line(:) = linetemp(:)
        line(linesize+1:) = buffer(:size_read)
        linesize = len(line)
        line(linesize:linesize) = ' '
        iostat = 0
        exit
      else if (iostat == 0) then
        linesize = len(line)
        deallocate(linetemp)
        allocate(character(len=linesize) :: linetemp)
        linetemp(:) = line(:)
        deallocate(line)
        allocate(character(len=linesize + size_read) :: line)
        line(:) = linetemp(:)
        line(linesize+1:) = buffer(:size_read)
      else
        exit
      end if
    end do
  end subroutine get_line  

END MODULE InputOutputModule
