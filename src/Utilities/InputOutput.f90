! -- MODFLOW 6 utility routines.
!
module InputOutputModule

  use KindModule, only: DP, I4B, I8B
  use SimVariablesModule, only: iunext, isim_mode, errmsg
  use SimModule, only: store_error, store_error_unit
  use ConstantsModule, only: IUSTART, IULAST, LINELENGTH, LENBIGLINE, &
                             LENBOUNDNAME, NAMEDBOUNDFLAG, MAXCHARLEN, &
                             TABLEFT, TABCENTER, TABRIGHT, TABSTRING, &
                             TABUCSTRING, TABINTEGER, TABREAL, DZERO
  use MessageModule, only: write_message
  private
  public :: GetUnit, UPCASE, URWORD, ULSTLB, UBDSV4, ubdsv06, UBDSVB, UCOLNO, &
            ULAPRW, ULASAV, ubdsv1, ubdsvc, ubdsvd, UWWORD, same_word, &
            str_pad_left, unitinquire, ParseLine, ulaprufw, openfile, &
            linear_interpolate, lowcase, read_line, GetFileFromPath, &
            extract_idnum_or_bndname, urdaux, print_format, BuildFixedFormat, &
            BuildFloatFormat, BuildIntFormat, fseek_stream, get_nwords, &
            u9rdcom, append_processor_id

  contains

  !> @brief Open a file
  !!
  !! Subroutine to open a file using the specified arguments
  !<
  subroutine openfile(iu, iout, fname, ftype, fmtarg_opt, accarg_opt, &
                      filstat_opt, mode_opt)
    ! -- modules
    use OpenSpecModule, only: action
    implicit none
    ! -- dummy
    integer(I4B), intent(inout) :: iu !< unit number
    integer(I4B), intent(in) :: iout !< output unit number to write a message (iout=0 does not print)
    character(len=*), intent(in) :: fname !< name of the file
    character(len=*), intent(in) :: ftype !< file type (e.g. WEL)
    character(len=*), intent(in), optional :: fmtarg_opt !< file format, default is 'formatted'
    character(len=*), intent(in), optional :: accarg_opt !< file access, default is 'sequential'
    character(len=*), intent(in), optional :: filstat_opt !< file status, default is 'old'. Use 'REPLACE' for output file.
    integer(I4B), intent(in), optional :: mode_opt !< simulation mode that is evaluated to determine if the file should be opened
    ! -- local
    character(len=20) :: fmtarg
    character(len=20) :: accarg
    character(len=20) :: filstat
    character(len=20) :: filact
    integer(I4B) :: imode
    integer(I4B) :: iflen
    integer(I4B) :: ivar
    integer(I4B) :: iuop
    ! -- formats
50  format(1x,/1x,'OPENED ',a,/ &
                 1x,'FILE TYPE:',a,'   UNIT ',I4,3x,'STATUS:',a,/ &
                 1x,'FORMAT:',a,3x,'ACCESS:',a/ &
                 1x,'ACTION:',a/)
60  format(1x,/1x,'DID NOT OPEN ',a,/)
    !
    ! -- Process mode_opt
    if (present(mode_opt)) then
      imode = mode_opt
    else
      imode = isim_mode
    end if
    !
    ! -- Evaluate if the file should be opened
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
      if (iout > 0) then
        write(iout, 50) fname(1:iflen), ftype, iu, filstat, fmtarg, accarg, &
                        filact
      end if
    end if
    !
    ! -- Return
    return
  end subroutine openfile

  !> @brief Assign a free unopened unit number
  !!
  !! Subroutine to assign a free unopened unit number to the iu dummy argument
  !<
  subroutine freeunitnumber(iu)
    ! -- modules
    implicit none
    ! -- dummy
    integer(I4B), intent(inout) :: iu  !< next free file unit number
    ! -- local
    integer(I4B) :: i
    logical :: opened
    !
    do i = iunext, iulast
      inquire(unit=i, opened=opened)
      if (.not. opened) exit
    enddo
    iu = i
    iunext = iu + 1
    !
    ! -- Return
    return
  end subroutine freeunitnumber

  !> @brief Get a free unit number
  !!
  !! Function to get a free unit number that hasn't been used
  !<
  function getunit()
    ! -- modules
    implicit none
    ! -- return
    integer(I4B) :: getunit  !< free unit number
    ! -- local
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
  !<
  subroutine upcase(word)
    implicit none
    ! -- dummy  
    character (len=*), intent(inout) :: word  !< word to convert to upper case
    ! -- local
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
    ! -- Return
    return
  end subroutine upcase

  !> @brief Convert to lower case
  !!
  !! Subroutine to convert a character string to lower case.
  !<
  subroutine lowcase(word)
    implicit none
    ! -- dummy
    character(len=*) :: word
    ! -- local
    integer(I4B) :: idiff, k, l
    !
    ! -- Compute the difference between lowercase and uppercase.
    l = len(word)
    idiff = ichar('a') - ichar('A')
    !
    ! -- Loop through the string and convert any uppercase characters.
    do k = 1, l
      if(word(k:k) >= 'A' .and. word(k:k) <= 'Z') then
        word(k:k)=char(ichar(word(k:k))+idiff)
      endif
    enddo
    !
    ! -- Return
    return
  end subroutine lowcase

  !> @brief Append processor id to a string
  !!
  !! Subroutine to append the processor id to a string before the file extension
  !! (extension is the string after the last '.' in the string. If there is
  !! no '.' in the string the processor id is appended to the end of the string.
  !<
  subroutine append_processor_id(name, proc_id)
    ! -- dummy
    character(len=LINELENGTH), intent(inout) :: name  !< file name
    integer(I4B), intent(in) :: proc_id  !< processor id
    ! -- local
    character(len=LINELENGTH) :: name_local
    character(len=LINELENGTH) :: name_processor
    character(len=LINELENGTH) :: extension_local
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
    write(name_processor, '(a,a,i0,a)') &
      name(1:ipos0-1), '.p', proc_id, trim(adjustl(extension_local))
    name = name_processor
    !
    ! -- Return
    return
  end subroutine append_processor_id

  !> @brief Create a formatted line
  !!
  !! Subroutine to create a formatted line with specified alignment and column
  !! separators. Like URWORD, UWWORD works with strings, integers, and floats. 
  !! Can pass an optional format statement, alignment, and column separator.
  !<
  subroutine UWWORD(line, icol, ilen, ncode, c, n, r, fmt, alignment, sep)
    implicit none
    ! -- dummy
    character (len=*), intent(inout) :: line !< line
    integer(I4B), intent(inout) :: icol !< column to write to line
    integer(I4B), intent(in) :: ilen !< current length of line
    integer(I4B), intent(in) :: ncode !< code for data type to write
    character (len=*), intent(in) :: c !< character data type
    integer(I4B), intent(in) :: n !< integer data type
    real(DP), intent(in) :: r !< float data type
    character (len=*), optional, intent(in) :: fmt !< format statement
    integer(I4B), optional, intent(in) :: alignment !< alignment specifier
    character (len=*), optional, intent(in) :: sep !< column separator
    ! -- local 
    character (len=16) :: cfmt
    character (len=16) :: cffmt
    character (len=ILEN) :: cval
    integer(I4B) :: ialign
    integer(I4B) :: i
    integer(I4B) :: ispace
    integer(I4B) :: istop
    integer(I4B) :: ipad
    integer(I4B) :: ireal
    !
    ! -- initialize locals
    ipad = 0
    ireal = 0
    !
    ! -- process dummy variables
    if (present(fmt)) then
      cfmt = fmt
    else
      select case(ncode)
        case(TABSTRING, TABUCSTRING)
          write(cfmt, '(a,I0,a)') '(a', ilen, ')'
        case(TABINTEGER)
          write(cfmt, '(a,I0,a)') '(I', ilen, ')'
        case(TABREAL)
          ireal = 1
          i = ilen - 7
          write(cfmt, '(a,I0,a,I0,a)') '(1PG', ilen, '.', i, ')'
          if (R >= DZERO) then
            ipad = 1
          end if
      end select
    end if
    write(cffmt, '(a,I0,a)') '(a', ilen, ')'
    !
    if (present(alignment)) then
      ialign = alignment
    else
      ialign = TABRIGHT
    end if
    !
    ! -- 
    if (ncode == TABSTRING .or. ncode == TABUCSTRING) then
      cval = C
      if (ncode == TABUCSTRING) then
        call UPcase(cval)
      end if
    else if (ncode == TABINTEGER) then
      write(cval, cfmt) n
    else if (ncode == TABREAL) then
      write(cval, cfmt) r
    end if
    !
    ! -- Apply alignment to cval
    if (len_trim(adjustl(cval)) > ilen) then
      cval = adjustl(cval)
    else
      cval = trim(adjustl(cval))
    end if
    if (ialign == TABCENTER) then
      i = len_trim(cval)
      ispace = (ilen - i) / 2
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
    if (ncode == TABUCSTRING) then
      call UPcase(cval)
    end if
    !
    ! -- Increment istop to the end of the column
    istop = icol + ilen - 1
    !
    ! -- Write final string to line
    write(line(icol:istop), cffmt) cval
    !
    icoL = istop + 1
    !
    if (present(sep)) then
      i = len(sep)
      istop = icol + i
      write(line(icol:istop), '(a)') sep
      icol = istop
    end if
    !
    ! -- Return
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
  subroutine URWORD(line, icol, istart, istop, ncode, n, r, iout, in)
    ! -- dummy  
    character(len=*) :: line !< line to parse
    integer(I4B), intent(inout) :: icol !< current column in line
    integer(I4B), intent(inout) :: istart !< starting character position of the word
    integer(I4B), intent(inout) :: istop !< ending character position of the word
    integer(I4B), intent(in) :: ncode !< word conversion flag (1) upper case, (2) integer, (3) real number
    integer(I4B), intent(inout) :: n !< integer data type
    real(DP), intent(inout) :: r !< float data type
    integer(I4B), intent(in) :: iout !< output listing file unit
    integer(I4B), intent(in) :: in !< input file unit number
    ! -- local
    character(len=20) string                
    character(len=30) rw
    character(len=1) tab
    character(len=1) charend
    character(len=200) :: msg
    character(len=linelength) :: msg_line
    !
    tab=char(9)
    !
    ! -- Set last char in LINE to blank and set ISTART and ISTOP to point
    !    to this blank as a default situation when no word is found.  If
    !    starting location in LINE is out of bounds, do not look for a word.
    linlen = len(line)
    line(linlen:linlen) = ' '
    istart = linlen
    istop = linlen
    linlen = linlen - 1
    if(icol < 1 .or. icol > linlen) go to 100
    !
    ! -- Find start of word, which is indicated by first character that
    !    is not a blank, a comma, or a tab.
    do 10 i=icol, linlen
      if(line(i:i) /= ' ' .and. line(i:i) /= ',' .and. &
         line(i:i) /= tab) go to 20
10  continue
    icol = linlen + 1
    go to 100
    !
    ! -- Found start of word.  Look for end.
    !    When word is quoted, only a quote can terminate it.
    !    search for a single (char(39)) or double (char(34)) quote
20  if(line(i:i)==char(34) .or. line(i:i)==char(39)) then
      if (line(i:i) == char(34)) then
        charend = char(34)
      else
        charend = char(39)
      end if
      i = i + 1
      if(i <= linlen) then
        do 25 j=i, linlen
          if(line(j:j)==charend) go to 40
25      continue
      end if
    !
    ! -- When word is not quoted, space, comma, or tab will terminate.
    else
      do 30 j=i, linlen
        if(line(j:j)==' ' .or. line(j:j)==',' .or. &
           line(j:j)==tab) go to 40
30    continue
    end if
    !
    ! -- End of line without finding end of word; set end of word to
    !    end of line.
    j = linlen + 1
    !
    ! -- Found end of word; set J to point to last character in WORD and
    !    set ICOL to point to location for scanning for another word.
40  icol = j + 1
    j = j - 1
    if(j < i) go to 100
    istart = i
    istop = j
    !
    ! -- Convert word to upper case and RETURN if NCODE is 1.
    if(ncode==1) then
      idiff = ichar('a') - ichar('A')
      do 50 k=istart, istop
        if(line(k:k) > 'a' .and. line(k:k) < 'z') &
           line(k:k) = char(ichar(line(k:k)) - idiff)
50    continue
      return
    end if
    !
    ! -- Convert word to a number if requested.
100 if(ncode==2 .or. ncode==3) then
      rw = ' '
      l = 30-istop+istart
      if (l < 1) go to 200
      rw(l:30) = line(istart:istop)
      if(ncode == 2) read(rw,'(i30)',err=200) n
      if(ncode == 3) read(rw,'(f30.0)',err=200) r
    end if
    return
    !
    ! -- Number conversion error.
200 if(ncode == 3) then
      string = 'a real number'
      l = 13
    else
      string = 'an integer'
      l = 10
    end if
    !
    ! -- If output unit is negative, set last character of string to 'E'.
    if (iout < 0) then
      n = 0
      r = 0.
      line(linlen+1:linlen+1) = 'E'
      return
    !
    ! -- If output unit is positive; write a message to output unit.
    else if(iout==0) then
      if(in > 0) then
        write(msg_line,201) in, line(istart:istop), string(1:l)
      else
        write(msg_line,202) line(istart:istop), string(1:l)
      end if
      call write_message(msg_line, iunit=IOUT, skipbefore=1)
      call write_message(line, iunit=IOUT, fmt='(1x,a)')
201   format(1x,'FILE UNIT ',I4,' : ERROR CONVERTING "',a, &
             '" TO ',a,' IN LINE:')
202   format(1x,'KEYBOARD INPUT : ERROR CONVERTING "',a, &
             '" TO ',a,' IN LINE:')
    !
    ! -- If output unit is 0; write a message to default output.
    else
      if(in > 0) then
        write(msg_line,201) in, line(istart:istop), string(1:l)
      else
        write(msg_line,202) line(istart:istop), string(1:l)
      end if
      call write_message(msg_line, iunit=iout, skipbefore=1)
      call write_message(LINE, iunit=iout, fmt='(1x,a)')
    end if
    !
    ! -- STOP after storing error message.
    call lowcase(string)
    if (in > 0) then
      write(msg,205) in,line(istart:istop), trim(string)
    else
      write(msg,207) line(istart:istop), trim(string)
    endif
205 format('File unit ',I0,': Error converting "',a, &
           '" to ',A,' in following line:')
207 format('Keyboard input: Error converting "',a, &
           '" to ',A,' in following line:')
    call store_error(msg)
    call store_error(trim(line))
    call store_error_unit(in)
    !
    ! -- Return
    return
  end subroutine URWORD

  !> @brief Print a label for a list
  !<
  subroutine ULSTLB(iout, label, caux, ncaux, naux)
    ! -- dummy
    character(len=*) :: label
    character(len=16) :: caux(ncaux)
    ! -- local
    character(len=400) buf
    ! -- constant
    character(len=1) DASH(400)
    data DASH/400*'-'/
    !
    ! -- Construct the complete label in BUF.  Start with BUF=LABEL.
    buf=label
    !
    ! -- Add auxiliary data names if there are any.
    nbuf = len(label) + 9
    if(naux > 0) then
       do 10 i=1, naux
         n1 = nbuf + 1
         nbuf = nbuf + 16
         buf(n1:nbuf) = caux(i)
10     continue
    end if
    !
    ! -- Write the label.
    write(iout, 103) buf(1:nbuf)
103 format(1x, a)
    !
    ! -- Add a line of dashes.
    write(iout, 104) (DASH(j), j=1, nbuf)
104 format(1x,400a)
    !
    ! -- Return
    return
  end subroutine ULSTLB

  !> @brief Write header records for cell-by-cell flow terms for one component
  !! of flow plus auxiliary data using a list structure
  !!
  !! Each item in the list is written by module UBDSVB
  !<
  subroutine UBDSV4(kstp, kper, text, naux, auxtxt, ibdchn, &
     &              ncol, nrow, nlay, nlist, iout, delt, pertim, totim)
    ! -- dummy 
    character(len=16) :: text
    character(len=16), dimension(:) :: auxtxt
    real(DP),intent(in) :: delt, pertim, totim
    ! -- formats
    character(len=*), parameter :: fmt = &
      "(1X,'UBDSV4 SAVING ',A16,' ON UNIT',I7,' AT TIME STEP',I7,"// &
      "', STRESS PERIOD',I7)"
    !
    ! -- Write unformatted records identifying data
    if(iout > 0) write(iout, fmt) text, ibdchn, kstp, kper
    write(ibdchn) kstp, kper, text, ncol, nrow, -nlay
    write(ibdchn) 5, delt, pertim, totim
    write(ibdchn) naux + 1
    if(naux > 0) write(ibdchn) (auxtxt(n), n=1, naux)
    write(ibdchn) nlist
    !
    ! -- Return
    return
  end subroutine UBDSV4

  !> @brief Write one value of cell-by-cell flow plus auxiliary data using a
  !! list structure
  !< 
  subroutine UBDSVB(ibdchn, icrl, q, val, nvl, naux, laux)
    ! -- dummy
    real(DP), dimension(nvl) :: val
    real(DP) :: q
    !
    ! -- Write cell number and flow rate
    IF(naux > 0) then
      n2 = laux + naux - 1
      write(ibdchn) icrl, q, (val(n), n=laux, n2)
    else
      write(ibdchn) icrl, q
    end if
    !
    ! -- Return
    return
  end subroutine UBDSVB

  !> @brief Output column numbers above a matrix printout
  !!
  !! nlbl1 is the start column label (number)
  !! nlbl2 is the stop column label (number)
  !! nspace is number of blank spaces to leave at start of line
  !! ncpl is number of column numbers per line
  !! ndig is number of characters in each column field
  !! iout is output channel
  !<
  subroutine UCOLNO(nlbl1, nlbl2, nspace, ncpl, ndig, iout)
    ! -- local
    character(len=1) :: DOT, SPACE, DG, BF
    dimension :: BF(1000), DG(10)
    !
    data DG(1),DG(2),DG(3),DG(4),DG(5),DG(6),DG(7),DG(8),DG(9),DG(10)/ &
       & '0','1','2','3','4','5','6','7','8','9'/
    data DOT,SPACE/'.',' '/
    !
    ! -- Calculate # of columns to be printed (nlbl), width
    !    of a line (ntot), number of lines (nwrap).
    if (iout <= 0) return
    write(iout, 1)
  1 format(1x)
    nlbl = nlbl2 - nlbl1 + 1
    n = nlbl
    !
    if(nlbl < ncpl) n = ncpl
    ntot = nspace + n * ndig
    !
    if(ntot > 1000) go to 50
    nwrap = (nlbl-1) / ncpl + 1
    j1 = nlbl1 - ncpl
    j2 = nlbl1 - 1
    !
    ! -- Build and print each line
    do 40 n=1, nwrap
      !
      ! -- Clear the buffer (BF)
      do 20 i=1, 1000
        BF(i) = SPACE
 20   continue
      nbf = nspace
      !
      ! -- Determine first (j1) and last (j2) column # for this line.
      j1 = j1 + ncpl
      j2 = j2 + ncpl
      if (j2 > nlbl2) j2 = nlbl2
      !
      !-- Load the column #'s into the buffer.
      do 30 j=j1, j2
        nbf = nbf + ndig
        i2 = j / 10
        i1 = j - i2 * 10 + 1
        BF(nbf) = DG(i1)
        if(i2 == 0) go to 30
        i3 = i2 / 10
        i2 = i2 - i3 * 10 + 1
        BF(nbf-1) = DG(i2)
        if(i3 == 0) go to 30
        i4 = i3 / 10
        i3 = i3 - i4 * 10 + 1
        BF(nbf-2) = DG(i3)
        if (I4 == 0) go to 30
        if (I4 > 9) then
          ! -- If more than 4 digits, use "X" for 4th digit.
          BF(nbf-3) = 'X'
        else
          BF(nbf-3) = DG(i4+1)
        end if
 30   continue
      !
      ! -- Print the contents of the buffer (i.e. print the line).
      write(iout, 31) (BF(i), i=1, nbf)
 31   format(1x,1000A1)
      !
 40 continue
    !
    ! -- Print a line of dots (for aesthetic purposes only).
 50 ntot = ntot
    if (ntot > 1000) ntot=1000
    write(iout, 51) (DOT,i=1,ntot)
 51 format(1x,1000A1)
    !
    ! -- Return
    return
  end subroutine UCOLNO

  !> @brief Print 1 layer array
  !<
  subroutine ULAPRW(buf,text,kstp,kper,ncol,nrow,ilay,iprn,iout)
    ! -- dummy
    character(len=16) :: text
    real(DP), dimension(ncol,nrow) :: buf
    !
    if (iout <= 0) return
    ! -- Print a header depending on ilay
    if (ilay > 0) then
      write(iout, 1) text, ilay, kstp, kper
    1 format('1', /2x, a, ' IN LAYER ',I3,' AT END OF TIME STEP ',I3, &
            & ' IN STRESS PERIOD ',I4/2x,75('-'))
      else if (ilay < 0) then
        write(iout,2) text, kstp, kper
    2    format('1',/1x,A,' FOR CROSS SECTION AT END OF TIME STEP',I3, &
            & ' IN STRESS PERIOD ',I4/1x,79('-'))
    end if
    !
    ! -- Make sure the format code (ip or iprn) is between 1 and 21
    ip = iprn
    if (ip < 1 .or. ip > 21) ip = 12
    !
    ! -- Call the utility module ucolno to print column numbers.
    if (ip == 1) call ucolno(1, ncol, 0, 11, 11, iout)
    if (ip == 2) call ucolno(1, ncol, 0, 9, 14, iout)
    if (ip >= 3 .and. ip <= 6) call ucolno(1, ncol, 3, 15, 8, iout)
    if (ip >= 7 .and. ip <= 11) call ucolno(1, ncol, 3, 20, 6, iout)
    if (ip == 12) call ucolno(1, ncol, 0, 10, 12, iout)
    if (ip >= 13 .and. ip <= 18) call ucolno(1, ncol, 3, 10, 7, iout)
    if (ip == 19) call ucolno(1, ncol, 0, 5, 13, iout)
    if (ip == 20) call ucolno(1, ncol, 0, 6, 12, iout)
    if (ip == 21) call ucolno(1, ncol, 0, 7, 10, iout)
    ! 
    ! -- Loop through the rows printing each one in its entirety.
    do i=1,nrow
      select case(ip)
      !
      case(1)
      ! -- format 11G10.3
      write(iout, 11) i,(buf(j, i), j=1, ncol)
11    format(1X,I3,2X,1PG10.3,10(1X,G10.3):/(5X,11(1X,G10.3)))
      !
      case(2)
      ! -- format 9G13.6
      write(iout, 21) i, (buf(j, i), j=1, ncol)
21    format(1x,I3,2x,1PG13.6,8(1x,G13.6):/(5x,9(1x,G13.6)))
      !
      case(3)
      ! -- format 15F7.1
      write(iout, 31) i, (buf(j, i), j=1, ncol)
31    format(1x,I3,1x,15(1x,F7.1):/(5x,15(1x,F7.1)))
      !
      case(4)
      ! -- format 15F7.2
      write(iout,41) i,(buf(j,i),j=1,ncol)
41    format(1x,I3,1x,15(1x,F7.2):/(5x,15(1x,F7.2)))
      !
      case(5)
      ! -- format 15F7.3
      write(iout, 51) i, (buf(j, i), j=1, ncol)
51    format(1x,I3,1x,15(1x,F7.3):/(5x,15(1x,F7.3)))
      !
      case(6)
      ! -- format 15F7.4
      write(iout, 61) i, (buf(j, i), j=1, ncol)
61    format(1x,I3,1x,15(1x,F7.4):/(5x,15(1x,F7.4)))
      !
      case(7)
      ! -- format 20F5.0
      write(iout, 71) i, (buf(j, i), j=1, ncol)
71    format(1x,I3,1x,20(1x,F5.0):/(5x,20(1x,F5.0)))
      !
      case(8)
      ! -- format 20F5.1
      write(iout, 81) i, (buf(j, i), j=1, ncol)
81    format(1x,I3,1x,20(1x,F5.1):/(5x,20(1x,F5.1)))
      !
      case(9)
      ! -- format 20F5.2
      write(iout, 91) i, (buf(j, i), j=1, ncol)
91    format(1x,I3,1x,20(1x,F5.2):/(5x,20(1x,F5.2)))
      !
      case(10)
      ! -- format 20F5.3
      write(iout, 101) i, (buf(j, i), j=1, ncol)
101   format(1x,I3,1x,20(1x,F5.3):/(5x,20(1x,F5.3)))
      !
      case(11)
      ! -- format 20F5.4
      write(iout, 111) i, (buf(j, i), j=1, ncol)
111   format(1x,I3,1x,20(1x,F5.4):/(5x,20(1x,F5.4)))
      !
      case(12)
      ! -- format 10G11.4
      write(iout,121) i, (buf(j, i), j=1, ncol)
121   format(1x,I3,2x,1PG11.4,9(1x,G11.4):/(5x,10(1x,G11.4)))
      !
      case(13)
      ! -- format 10F6.0
      write(iout, 131) i, (buf(j, i), j=1, ncol)
131   format(1x,I3,1x,10(1x,F6.0):/(5X,10(1x,F6.0)))
      !
      case(14)
      ! -- format 10F6.1
      write(iout, 141) i, (buf(j, i), j=1, ncol)
141   format(1x,I3,1x,10(1x,F6.1):/(5x,10(1x,F6.1)))
      !
      case(15)
      ! -- format 10F6.2
      write(iout, 151) i, (buf(j, i), j=1, ncol)
151   format(1x,I3,1x,10(1x,F6.2):/(5x,10(1x,F6.2)))
      !
      case(16)
      ! -- format 10F6.3
      write(iout, 161) i, (buf(j, i), j=1, ncol)
161   format(1x,I3,1x,10(1x,F6.3):/(5x,10(1x,F6.3)))
      !
      case(17)
      ! -- format 10F6.4
      write(iout, 171) i, (buf(j, i), j=1, ncol)
171   format(1x,I3,1x,10(1x,F6.4):/(5x,10(1x,F6.4)))
      !
      case(18)
      ! -- format 10F6.5
      write(iout, 181) i, (buf(j, i), j=1, ncol)
181   format(1x,I3,1x,10(1x,F6.5):/(5x,10(1x,F6.5)))
      !
      case(19)
      ! -- format 5G12.5
      write(iout, 191) i, (buf(j, i), j=1, ncol)
191   format(1x,I3,2x,1PG12.5,4(1x,G12.5):/(5x,5(1x,G12.5)))
      !
      case(20)
      ! -- format 6G11.4
      write(iout, 201) i, (buf(j, i), j=1, ncol)
201   format(1x,I3,2x,1PG11.4,5(1x,G11.4):/(5x,6(1x,G11.4)))
      !
      case(21)
      ! -- format 7G9.2
      write(iout, 211) i, (buf(j, i), j=1, ncol)
211   format(1x,I3,2x,1PG9.2,6(1x,G9.2):/(5x,7(1x,G9.2)))
      !
      end select
    end do
    !
    ! -- Flush file
    flush(iout)
    !
    ! -- Return
    return
  end subroutine ULAPRW

  !> @brief Save 1 layer array on disk
  !<
  subroutine ulasav(buf, text, kstp, kper, pertim, totim, ncol, nrow, &
                     ilay, ichn)
    ! -- dummy
    character(len=16) :: text
    real(DP), dimension(ncol, nrow) :: buf
    real(DP) :: pertim, totim
    !
    ! -- Write an unformatted record containing identifying information
    write(ichn) kstp, kper, pertim, totim, text, ncol, nrow, ilay
    !
    ! -- Write an unformatted record containing array values. The array is
    !    dimensioned (ncol,nrow)
    write(ichn) ((buf(ic, ir), ic=1, ncol), ir=1, nrow)
    !
    ! -- flush file
    flush(ICHN)
    !
    ! -- Return
    return
  end subroutine ulasav

  !> @brief Record cell-by-cell flow terms for one component of flow as a 3-D 
  !! array with extra record to indicate delt, pertim, and totim
  !<
  subroutine ubdsv1(kstp, kper, text, ibdchn, buff, ncol, nrow, nlay, iout, &
                    delt, pertim, totim)
    implicit none
    ! -- dummy
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
    ! -- Return
    return
  end subroutine ubdsv1

  !> @brief Write header records for cell-by-cell flow terms for one component
  !! of flow.  
  !!
  !! Each item in the list is written by module ubdsvc
  !<
  subroutine ubdsv06(kstp, kper, text, modelnam1, paknam1, modelnam2, paknam2, &
                     ibdchn, naux, auxtxt, ncol, nrow, nlay, nlist, iout, &
                     delt, pertim, totim)
    implicit none
    ! -- dummy
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
    ! -- local
    integer(I4B) :: n
    ! -- format
    character(len=*), parameter :: fmt = &
      "(1X,'UBDSV06 SAVING ',A16,' IN MODEL ',A16,' PACKAGE ',A16,"//&
      "'CONNECTED TO MODEL ',A16,' PACKAGE ',A16,"//&
      "' ON UNIT',I7,' AT TIME STEP',I7,', STRESS PERIOD',I7)"
    !
    ! -- Write unformatted records identifying data.
    if (iout > 0) write(iout,fmt) text, modelnam1, paknam1, modelnam2, paknam2,&
                                  modelnam2, paknam2, ibdchn, kstp, kper
    write(ibdchn) kstp, kper, text, ncol, nrow, -nlay
    write(ibdchn) 6, delt, pertim, totim
    write(ibdchn) modelnam1
    write(ibdchn) paknam1
    write(ibdchn) modelnam2
    write(ibdchn) paknam2
    write(ibdchn) naux+1
    if (naux > 0) write(ibdchn) (auxtxt(n),n=1,naux)
    write(ibdchn) nlist
    !
    ! -- Return
    return
  end subroutine ubdsv06

  !> @brief Write one value of cell-by-cell flow using a list structure.
  !!
  !! From node (n) and to node (n2) are written to the file
  !<
  subroutine ubdsvc(ibdchn, n, q, naux, aux)
    implicit none
    ! -- dummy
    integer(I4B), intent(in) :: ibdchn
    integer(I4B), intent(in) :: n
    real(DP), intent(in) :: q
    integer(I4B), intent(in) :: naux
    real(DP), dimension(naux), intent(in) :: aux
    ! -- local
    integer(I4B) :: nn
    !
    ! -- Write record
    if (naux > 0) then
        write(ibdchn) n,q,(aux(nn),nn=1,naux)
    else
        write(ibdchn) n,q
    end if
    !
    ! -- Return
    return
  end subroutine ubdsvc

  !> @brief Write one value of cell-by-cell flow using a list structure.
  !!
  !! From node (n) and to node (n2) are written to the file
  !<
  subroutine ubdsvd(ibdchn, n, n2, q, naux, aux)
    implicit none
    ! -- dummy
    integer(I4B), intent(in) :: ibdchn
    integer(I4B), intent(in) :: n
    integer(I4B), intent(in) :: n2
    real(DP), intent(in) :: q
    integer(I4B), intent(in) :: naux
    real(DP), dimension(naux), intent(in) :: aux
    ! -- local
    integer(I4B) :: nn
    !
    ! -- Write record
    if (naux > 0) then
        write(ibdchn) n,n2,q,(aux(nn),nn=1,naux)
    else
        write(ibdchn) n,n2,q
    end if
    !
    ! -- Return
    return
  end subroutine ubdsvd

  !> @brief Perform a case-insensitive comparison of two words
  !<
  logical function same_word(word1, word2) 
    implicit none
    ! -- dummy
    character(len=*), intent(in) :: word1, word2
    ! -- local
    character(len=200) :: upword1, upword2
    !
    upword1 = word1
    call upcase(upword1)
    upword2 = word2
    call upcase(upword2)
    same_word = (upword1==upword2)
    !
    ! -- Return
    return
  end function same_word

  !> @brief Function for string manipulation
  !<
  function str_pad_left(str, width) result(res)
    ! -- local
    character(len=*), intent(in) :: str
    integer, intent(in) :: width
    ! -- Return
    character(len=max(len_trim(str), width)) :: res
    !
    res = str
    res = adjustr(res)
    !
    ! -- Return
    return
  end function

  subroutine unitinquire(iu)
    ! -- dummy
    integer(I4B) :: iu
    ! -- local
    character(len=LINELENGTH) :: line
    character(len=100) :: fname, ac, act, fm, frm, seq, unf
    ! -- format
    character(len=*), parameter :: fmta = &
       &"('unit:',i4,'  name:',a,'  access:',a,'  action:',a)"
    character(len=*), parameter :: fmtb = &
       &"('    formatted:',a,'  sequential:',a,'  unformatted:',a,'  form:',a)"
    !
    ! -- set strings using inquire statement
    inquire(unit=iu, name=fname, access=ac, action=act, formatted=fm, &
            sequential=seq, unformatted=unf, form=frm)
    !
    ! -- write the results of the inquire statement
    write(line,fmta) iu, trim(fname), trim(ac), trim(act)
    call write_message(line)
    write(line,fmtb) trim(fm), trim(seq), trim(unf), trim(frm)
    call write_message(line)
    !
    ! -- Return
    return
  end subroutine unitinquire

  !> @brief Parse a line into words. 
  !!
  !! Blanks and commas are recognized as delimiters. Multiple blanks between 
  !! words is OK, but multiple commas between words is treated as an error. 
  !! Quotation marks are not recognized as delimiters.
  !< 
  subroutine ParseLine(line, nwords, words, inunit, filename)
    ! -- modules 
    use ConstantsModule, only: LINELENGTH
    implicit none
    ! -- dummy
    character(len=*), intent(in) :: line
    integer(I4B), intent(inout) :: nwords
    character(len=*), allocatable, dimension(:), intent(inout) :: words
    integer(I4B), intent(in), optional :: inunit
    character(len=*), intent(in), optional :: filename
    ! -- local
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
    ! -- Return
    return
  end subroutine ParseLine

  !> @brief Print 1 layer array with user formatting in wrap format
  !<
  subroutine ulaprufw(ncol, nrow, kstp, kper, ilay, iout, buf, text, userfmt, &
                      nvalues, nwidth, editdesc)
    implicit none
    ! -- dummy
    integer(I4B), intent(in) :: ncol, nrow, kstp, kper, ilay, iout
    real(DP),dimension(ncol,nrow), intent(in) :: buf
    character(len=*), intent(in) :: text
    character(len=*), intent(in) :: userfmt
    integer(I4B), intent(in) :: nvalues, nwidth
    character(len=1), intent(in) :: editdesc
    ! -- local
    integer(I4B) :: i, j, nspaces
    ! -- formats
    1 format('1',/2X,A,' IN LAYER ',I3,' AT END OF TIME STEP ',I3, &
          ' IN STRESS PERIOD ',I4/2X,75('-'))
    2 format('1',/1X,A,' FOR CROSS SECTION AT END OF TIME STEP',I3, &
          ' IN STRESS PERIOD ',I4/1X,79('-'))
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
    ! -- Return
    return
  end subroutine ulaprufw

  !> @breif This function reads a line of arbitrary length and returns it.
  !!
  !! The returned string can be stored in a deferred-length character variable, 
  !! for example:
  !!
  !!   integer(I4B) :: iu
  !!   character(len=:), allocatable :: my_string
  !!   logical :: eof
  !!   iu = 8
  !!   open(iu,file='my_file')
  !!   my_string = read_line(iu, eof)
  !<
  function read_line(iu, eof) result (astring)
    !
    implicit none
    ! -- dummy
    integer(I4B), intent(in) :: iu
    logical, intent(out) :: eof
    character(len=:), allocatable :: astring
    ! -- local
    integer(I4B) :: isize, istat
    character(len=256) :: buffer
    character(len=1000) :: ermsg, fname
    character(len=7) :: fmtd
    logical :: lop
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
      ! -- An end-of-record condition stops the loop.
      if (istat < 0) then
        return
      endif
    enddo
    !
    return
99  continue
    !
    ! An end-of-file condition returns an empty string.
    eof = .true.
    !
    ! -- Return
    return
  end function read_line

  subroutine GetFileFromPath(pathname, filename)
    implicit none
    ! -- dummy
    character(len=*), intent(in) :: pathname
    character(len=*), intent(out) :: filename
    ! -- local
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
    ! -- Return
    return
  end subroutine GetFileFromPath

  !> @brief Starting at position icol, define string as line(istart:istop).
  !!
  !! If string can be interpreted as an integer(I4B), return integer in idnum 
  !! argument. If token is not an integer(I4B), assume it is a boundary name, 
  !! return NAMEDBOUNDFLAG in idnum, convert string to uppercase and return it
  !! in bndname.
  !<
  subroutine extract_idnum_or_bndname(line, icol, istart, istop, idnum, bndname)
    implicit none
    ! -- dummy
    character(len=*), intent(inout) :: line
    integer(I4B), intent(inout) :: icol, istart, istop
    integer(I4B), intent(out) :: idnum
    character(len=LENBOUNDNAME), intent(out) :: bndname
    ! -- local 
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
    ! -- Return
    return
  end subroutine extract_idnum_or_bndname

  !> @brief Read auxiliary variables from an input line
  !<
  subroutine urdaux(naux, inunit, iout, lloc, istart, istop, auxname, line, text)
    ! -- modules
    use ArrayHandlersModule, only: ExpandArray
    use ConstantsModule,     only: LENAUXNAME
    ! -- implicit
    implicit none
    ! -- dummy
    integer(I4B), intent(inout) :: naux
    integer(I4B), intent(in) :: inunit
    integer(I4B), intent(in) :: iout
    integer(I4B), intent(inout) :: lloc
    integer(I4B), intent(inout) :: istart
    integer(I4B), intent(inout) :: istop
    character(len=LENAUXNAME), allocatable, dimension(:), intent(inout) :: auxname
    character(len=*), intent(inout) :: line
    character(len=*), intent(in) :: text
    ! -- local
    integer(I4B) :: n, linelen
    integer(I4B) :: iauxlen
    real(DP) :: rval
    !
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
    ! -- Return
    return
  end subroutine urdaux

  !> @brief Define the print or save format
  !!
  !! Define cdatafmp as a Fortran output format based on user input. Also define
  !! nvalues, nwidth, and editdesc.
  !!
  !! Syntax for linein:
  !!   COLUMNS nval WIDTH nwid [DIGITS ndig [options]]
  !!
  !! Where:
  !!   nval = Number of values per line.
  !!   nwid = Number of character places to be used for each value.
  !!   ndig = Number of digits to the right of the decimal point (required
  !!          for real array).
  !!   options are:
  !!          editoption: One of [EXPONENTIAL, FIXED, GENERAL, SCIENTIFIC]
  !! A default value should be passed in for editdesc as G, I, E, F, or S.
  !! If I is passed in, then the fortran format will be for an integer variable.
  !<
  subroutine print_format(linein, cdatafmp, editdesc, nvaluesp, nwidthp, inunit)
    ! -- dummy
    character(len=*), intent(in) :: linein
    character(len=*), intent(inout) :: cdatafmp
    character(len=*), intent(inout) :: editdesc
    integer(I4B), intent(inout) :: nvaluesp
    integer(I4B), intent(inout) :: nwidthp
    integer(I4B), intent(in) :: inunit
    ! -- local
    character(len=len(linein)) :: line
    character(len=20), dimension(:), allocatable :: words
    character(len=100) :: ermsg
    integer(I4B) :: ndigits=0, nwords=0
    integer(I4B) :: i, ierr
    logical :: isint
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
    ! -- Return
    return
  end subroutine print_format

  !> @brief Build a fixed format for printing or saving a real array
  !<
  subroutine BuildFixedFormat(nvalsp, nwidp, ndig, outfmt, prowcolnum)
    implicit none
    ! -- dummy
    integer(I4B), intent(in) :: nvalsp, nwidp, ndig
    character(len=*), intent(inout) :: outfmt
    logical, intent(in), optional :: prowcolnum  ! default true
    ! -- local
    character(len=8)   :: cvalues, cwidth, cdigits
    character(len=60)  :: ufmt
    logical :: prowcolnumlocal
    ! -- formats
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
    !
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
    ! -- Return
    return
  end subroutine BuildFixedFormat

  !> @brief Build a floating-point format for printing or saving a real array
  !<
  subroutine BuildFloatFormat(nvalsp, nwidp, ndig, editdesc, outfmt, prowcolnum)
    implicit none
    ! -- dummy
    integer(I4B), intent(in) :: nvalsp, nwidp, ndig
    character(len=*), intent(in) :: editdesc
    character(len=*), intent(inout) :: outfmt
    logical, intent(in), optional :: prowcolnum  ! default true
    ! -- local
    character(len=8)   :: cvalues,  cwidth, cdigits
    character(len=60)  :: ufmt
    logical :: prowcolnumlocal
    ! -- formats
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
    !
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
    ! -- Return
    return
  end subroutine BuildFloatFormat

  !> @brief Build a format for printing or saving an integer array
  !<
  subroutine BuildIntFormat(nvalsp, nwidp, outfmt, prowcolnum)
    implicit none
    ! -- dummy
    integer(I4B), intent(in) :: nvalsp, nwidp
    character(len=*), intent(inout) :: outfmt
    logical, intent(in), optional :: prowcolnum  ! default true
    ! -- local
    character(len=8)   :: cvalues, cwidth
    character(len=60)  :: ufmt
    logical :: prowcolnumlocal
    ! -- formats
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
    ! -- Return
    return
  end subroutine BuildIntFormat

  !> @brief Get the number of words in a string
  !<
  function get_nwords(line)
    ! -- return
    integer(I4B) :: get_nwords !< number of words in a string
    ! -- dummy
    character(len=*), intent(in) :: line  !< line
    ! -- local
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
    ! -- Return
    return
  end function get_nwords

  !> @brief Move the file pointer.
  !!
  !! Patterned after fseek, which is not supported as part of the fortran 
  !! standard.  For this subroutine to work the file must have been opened with
  !! access='stream' and action='readwrite'.
  !<
  subroutine fseek_stream(iu, offset, whence, status)
    ! -- dummy
    integer(I4B), intent(in) :: iu
    integer(I4B), intent(in) :: offset
    integer(I4B), intent(in) :: whence
    integer(I4B), intent(inout) :: status
    ! -- local
    integer(I8B) :: ipos
    !
    inquire(unit=iu, size=ipos)
    !
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
    ! -- Return
    return
  end subroutine fseek_stream

  !> @brief Read until non-comment line found and then return line. 
  !!
  !! Different from u8rdcom in that line is a deferred length character string,
  !! which allows any length lines to be read using the get_line subroutine.
  !<
  subroutine u9rdcom(iin, iout, line, ierr)
    ! -- module
    use, intrinsic :: iso_fortran_env, only: IOSTAT_END
    implicit none
    ! -- dummy
    integer(I4B),         intent(in) :: iin
    integer(I4B),         intent(in) :: iout
    character (len=:), allocatable, intent(inout) :: line
    integer(I4B),        intent(out) :: ierr
    ! -- local
    character (len=:), allocatable :: linetemp
    character (len=2), parameter :: comment = '//'
    character(len=1), parameter  :: tab = CHAR(9)
    logical :: iscomment
    integer(I4B) :: i, j, l, istart, lsize
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
      if (len_trim(line) < 1) then
        line = comment
        cycle
      end if
      !
      ! -- Ensure that any initial tab characters are treated as spaces
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
            if (line(1:2) == comment) iscomment = .true.
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
          l = len(line)
          do i = l, 1, -1
            if(line(i:i) /= ' ') then
              exit
            end if
          end do
          ! -- print the line up to the last non-blank character.
          write(iout,'(1x,a)') line(1:i)
        end if
      end if
    end do pcomments
    !
    ! -- Return
    return
  end subroutine u9rdcom

  !> @brief Read an unlimited length line from unit number lun into a deferred-
  !! length character string (line).  
  !!
  !! Tack on a single space to the end so that routines like URWORD continue to 
  !! function as before.
  !<
  subroutine get_line(lun, line, iostat)
    ! -- dummy
    integer(I4B), intent(in) :: lun
    character(len=:), intent(out), allocatable :: line
    integer(I4B), intent(out) :: iostat
    ! -- local
    integer(I4B), parameter :: buffer_len = MAXCHARLEN
    character(len=buffer_len) :: buffer
    character(len=:), allocatable :: linetemp
    integer(I4B) :: size_read, linesize
    !
    ! -- initialize
    line = ''
    linetemp = ''
    !
    ! -- process
    do
      read (lun, '(A)', iostat=iostat, advance='no', size=size_read) buffer
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

end module InputOutputModule
