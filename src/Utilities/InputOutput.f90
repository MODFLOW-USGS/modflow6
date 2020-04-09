! -- MODFLOW 6 utility routines.
!
module InputOutputModule

  use KindModule, only: DP, I4B
  use SimModule, only: store_error, ustop, store_error_unit,                   &
                       store_error_filename
  use ConstantsModule, only: LINELENGTH, LENBIGLINE, LENBOUNDNAME,             &
                             NAMEDBOUNDFLAG, LINELENGTH, MAXCHARLEN
  private
  public :: dclosetest, GetUnit, u8rdcom, uget_block,                          &
            uterminate_block, UPCASE, URWORD, ULSTLB, UBDSV4,                  &
            ubdsv06, UBDSVB, UCOLNO, ULAPRW,                                   &
            ULASAV, ubdsv1, ubdsvc, ubdsvd, UWWORD,                            &
            same_word, get_node, get_ijk, unitinquire,                         &
            ParseLine, ulaprufw, write_centered, openfile,                     &
            linear_interpolate, lowcase,                                       &
            read_line, uget_any_block,                                         &
            GetFileFromPath, extract_idnum_or_bndname, urdaux,                 &
            get_jk, uget_block_line, print_format, BuildFixedFormat,           &
            BuildFloatFormat, BuildIntFormat

  contains

  logical function dclosetest(a,b,eps)
! ******************************************************************************
! Check and see if two doubles are close enough to be considered equal
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    implicit none
    ! -- dummy
    real(DP), intent(in) :: a
    real(DP), intent(in) :: b
    real(DP), intent(in), optional :: eps
    ! -- local
    real(DP) :: epslocal, absval
! ------------------------------------------------------------------------------
    !
    if (present(eps)) then
      epslocal = eps
    else
      epslocal = 1.2d-7
    endif
    dclosetest=.true.
    if(a.gt.b) then
      absval = abs(a)
      if((a-b) .le. absval*epslocal) return
    else
      absval = abs(b)
      if((b-a) .le. absval*epslocal) return
    end if
    dclosetest=.false.
    !
    ! -- Return
    return
  end function dclosetest

  subroutine openfile(iu, iout, fname, ftype, fmtarg_opt, accarg_opt,          &
                      filstat_opt)
! ******************************************************************************
! openfile -- Open a file using the specified arguments.
!
!   iu is the unit number
!   iout is the output unit number to write a message (iout=0 does not print)
!   fname is the name of the file
!   ftype is the type of the file (e.g. WEL)
!   fmtarg_opt is the format, default is 'formatted'
!   accarg_opt is the access, default is 'sequential'
!   filstat_opt is the file status, default is 'old'.  Use 'REPLACE' for an
!     output file.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use OpenSpecModule, only: action
    implicit none
    ! -- dummy
    integer(I4B), intent(inout)       :: iu
    integer(I4B), intent(in)          :: iout
    character(len=*), intent(in) :: fname
    character(len=*), intent(in) :: ftype
    character(len=*), intent(in), optional :: fmtarg_opt
    character(len=*), intent(in), optional :: accarg_opt
    character(len=*), intent(in), optional :: filstat_opt
    ! -- local
    character(len=20) :: fmtarg
    character(len=20) :: accarg
    character(len=20) :: filstat
    character(len=20) :: filact
    integer(I4B) :: iflen
    integer(I4B) :: ivar
    integer(I4B) :: iuop
    character(len=LINELENGTH) :: errmsg
    ! -- formats
50  FORMAT(1X,/1X,'OPENED ',A,/                                                &
                 1X,'FILE TYPE:',A,'   UNIT ',I4,3X,'STATUS:',A,/              &
                 1X,'FORMAT:',A,3X,'ACCESS:',A/                                &
                 1X,'ACTION:',A/)
2011  FORMAT('*** ERROR OPENING FILE "',A,'" ON UNIT ',I0)
2017  format('*** FILE ALREADY OPEN ON UNIT: ',I0)
2012  format('       SPECIFIED FILE STATUS: ',A)
2013  format('       SPECIFIED FILE FORMAT: ',A)
2014  format('       SPECIFIED FILE ACCESS: ',A)
2015  format('       SPECIFIED FILE ACTION: ',A)
2016  format('         IOSTAT ERROR NUMBER: ',I0)
2018  format('  -- STOP EXECUTION (openfile)')
! ------------------------------------------------------------------------------
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
      write(errmsg,2011) fname(1:iflen), iu
      call store_error(errmsg)
      if(iuop > 0) then
        write(errmsg, 2017) iuop
        call store_error(errmsg)
      endif
      write(errmsg,2012) filstat
      call store_error(errmsg)
      write(errmsg,2013) fmtarg
      call store_error(errmsg)
      write(errmsg,2014) accarg
      call store_error(errmsg)
      write(errmsg,2015) filact
      call store_error(errmsg)
      write(errmsg,2016) ivar
      call store_error(errmsg)
      write(errmsg,2018)
      call store_error(errmsg)
      call ustop()
    endif
    !
    ! -- Write a message
    if(iout > 0) then
      write(iout, 50) fname(1:iflen),                                         &
                     ftype, iu, filstat,                                      &
                     fmtarg, accarg,                                          &
                     filact
    endif
    !
    ! -- return
    return
  end subroutine openfile

  subroutine freeunitnumber(iu)
! ******************************************************************************
! Assign a free unopened unit number to the iu dummy argument.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    implicit none
    ! -- dummy
    integer(I4B),intent(inout) :: iu
    ! -- local
    integer(I4B) :: lastunitnumber
    parameter(lastunitnumber=10000)
    integer(I4B), save :: nextunitnumber=1000
    integer(I4B) :: i
    logical :: opened
! ------------------------------------------------------------------------------
  !
    do i = nextunitnumber, lastunitnumber
      inquire(unit=i, opened=opened)
      if(.not. opened) exit
    enddo
    iu = i
    nextunitnumber = iu + 1
    !
    ! -- return
    return
  end subroutine freeunitnumber

  function getunit()
! ******************************************************************************
! Get a free unit number that hasn't been used yet.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    implicit none
    ! -- return
    integer(I4B) :: getunit
    ! -- local
    integer(I4B) :: iunit
! ------------------------------------------------------------------------------
    !
    call freeunitnumber(iunit)
    getunit = iunit
    !
    ! -- Return
    return
  end function getunit

  subroutine u8rdcom(iin, iout, line, ierr)
! ******************************************************************************
! Read until non-comment line found and then return line
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use ConstantsModule, only: LINELENGTH
    use, intrinsic :: iso_fortran_env, only: IOSTAT_END
    implicit none
    ! -- dummy
    integer(I4B),         intent(in) :: iin
    integer(I4B),         intent(in) :: iout
    character (len=*), intent(inout) :: line
    integer(I4B),        intent(out) :: ierr
    ! -- local definitions
    character (len=2), parameter :: comment = '//'
    character(len=LINELENGTH)    :: errmsg
    character(len=1), parameter  :: tab = CHAR(9)
    logical :: iscomment
    integer(I4B) :: i, l
! ------------------------------------------------------------------------------
    !code
    !
    !readerrmsg = ''
    line = comment
    pcomments: do
      read (iin,'(a)', iostat=ierr) line
      !read (iin,'(a)', iostat=ierr, iomsg=readerrmsg) line
      if (ierr == IOSTAT_END) then
        ! -- End of file reached.
        ! -- Backspace is needed for gfortran.
        backspace(iin)
        line = ' '
        exit pcomments
      elseif (ierr /= 0) then
        ! -- Other error...report it
        call store_error('******Error in u8rdcom.')
        write(errmsg, *) 'Could not read from unit: ',iin
        call store_error(errmsg)
        !write(errmsg,*)'Error reported as: ',trim(readerrmsg)
        !call store_error(errmsg)
        call unitinquire(iin)
        call ustop()
      endif
      if (len_trim(line).lt.1) then
        line = comment
        cycle
      end if
      !
      ! Ensure that any initial tab characters are treated as spaces
      cleartabs: do
        line = trim(adjustl(line))
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
  end subroutine u8rdcom

  subroutine uget_block_line(iu, iuext, iout, line, lloc, istart, istop)
! ******************************************************************************
! Read and return line read from an external file or from within a block.
! The line is read from an external file if iu is not equal to iuext
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    implicit none
    ! -- dummy
    integer(I4B), intent(in) :: iu
    integer(I4B), intent(in) :: iuext
    integer(I4B), intent(in) :: iout
    character (len=*), intent(inout) :: line
    integer(I4B), intent(inout) :: lloc
    integer(I4B), intent(inout) :: istart
    integer(I4B), intent(inout) :: istop
    ! -- local definitions
    integer(I4B) :: ierr
    integer(I4B) :: ival
    real(DP) :: rval
! ------------------------------------------------------------------------------
    lloc = 1
    call u8rdcom(iuext, iout, line, ierr)
    call urword(line, lloc, istart, istop, 1, ival, rval, iout, iuext)
    ! -- determine if an empty string is returned
    !    condition occurs if the end of the file has been read
    if (len_trim(line) < 1) then
      ! -- if external file, read line from package unit (iu)
      if (iuext /= iu) then
        lloc = 1
        call u8rdcom(iu, iout, line, ierr)
        call urword(line, lloc, istart, istop, 1, ival, rval, iout, iu)
      end if
    end if
    return
  end subroutine uget_block_line


  subroutine uget_block(iin, iout, ctag, ierr, isfound, lloc, line, iuext,     &
                        blockRequired, supportopenclose)
! ******************************************************************************
! Read until the ctag block is found.  Return isfound with true, if found.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    implicit none
    ! -- dummy
    integer(I4B),         intent(in) :: iin
    integer(I4B),         intent(in) :: iout
    character (len=*),    intent(in) :: ctag
    integer(I4B),        intent(out) :: ierr
    logical,           intent(inout) :: isfound
    integer(I4B),      intent(inout) :: lloc
    character (len=*), intent(inout) :: line
    integer(I4B),      intent(inout) :: iuext
    logical, optional,    intent(in) :: blockRequired
    logical, optional,    intent(in) :: supportopenclose
    ! -- local
    integer(I4B) :: istart
    integer(I4B) :: istop
    integer(I4B) :: ival
    integer(I4B) :: lloc2
    real(DP) :: rval
    character(len=LINELENGTH) :: fname, line2
    character(len=MAXCHARLEN) :: ermsg
    logical :: supportoc, blockRequiredLocal
! ------------------------------------------------------------------------------
    !code
    if (present(blockRequired)) then
      blockRequiredLocal = blockRequired
    else
      blockRequiredLocal = .true.
    endif
    supportoc = .false.
    if (present(supportopenclose)) then
      supportoc = supportopenclose
    endif
    iuext = iin
    isfound = .false.
    mainloop: do
      lloc = 1
      call u8rdcom(iin, iout, line, ierr)
      if (ierr < 0) exit
      call urword(line, lloc, istart, istop, 1, ival, rval, iin, iout)
      if (line(istart:istop) == 'BEGIN') then
        call urword(line, lloc, istart, istop, 1, ival, rval, iin, iout)
        if (line(istart:istop) == ctag) then
          isfound = .true.
          if (supportoc) then
            ! Look for OPEN/CLOSE on 1st line after line starting with BEGIN
            call u8rdcom(iin,iout,line2,ierr)
            if (ierr < 0) exit
            lloc2 = 1
            call urword(line2, lloc2, istart, istop, 1, ival, rval, iin, iout)
            if (line2(istart:istop) == 'OPEN/CLOSE') then
              ! -- Get filename and preserve case
              call urword(line2, lloc2, istart, istop, 0, ival, rval, iin, iout)
              fname = line2(istart:istop)
              ! If line contains '(BINARY)' or 'SFAC', handle this block elsewhere
              chk: do
                call urword(line2, lloc2, istart, istop, 1, ival, rval, iin, iout)
                if (line2(istart:istop) == '') exit chk
                if (line2(istart:istop) == '(BINARY)' .or. &
                    line2(istart:istop) == 'SFAC') then
                  backspace(iin)
                  exit mainloop
                end if
              end do chk
              iuext = GetUnit()
              call openfile(iuext,iout,fname,'OPEN/CLOSE')
            else
              backspace(iin)
            end if
          end if
        else
          if (blockRequiredLocal) then
            ermsg = 'Error: Required block "' // trim(ctag) // &
                    '" not found. Found block "' // line(istart:istop) // &
                    '" instead.'
            call store_error(ermsg)
            call store_error_unit(iuext)
            call ustop()
          else
            backspace(iin)
          endif
        end if
        exit mainloop
      else if (line(istart:istop) == 'END') then
        call urword(line, lloc, istart, istop, 1, ival, rval, iin, iout)
        if (line(istart:istop) == ctag) then
          ermsg = 'Error: Looking for BEGIN ' // trim(ctag) // &
                  ' but found END ' // line(istart:istop) // &
                  ' instead.'
          call store_error(ermsg)
          call store_error_unit(iuext)
          call ustop()
        endif
      end if
    end do mainloop
    return
  end subroutine uget_block

  subroutine uget_any_block(iin,iout,isfound,lloc,line,ctagfound,iuext)
! ******************************************************************************
! Read until any block is found. If found, return isfound as true and
! return block name in ctagfound.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    implicit none
    ! -- dummy
    integer(I4B), intent(in) :: iin
    integer(I4B), intent(in) :: iout
    logical, intent(inout) :: isfound
    integer(I4B), intent(inout) :: lloc
    character (len=*), intent(inout) :: line
    character(len=*), intent(out) :: ctagfound
    integer(I4B), intent(inout) :: iuext
    ! -- local
    integer(I4B) :: ierr, istart, istop
    integer(I4B) :: ival, lloc2
    real(DP) :: rval
    character(len=100) :: ermsg
    character(len=LINELENGTH) :: line2, fname
! ------------------------------------------------------------------------------
    !code
    isfound = .false.
    ctagfound = ''
    iuext = iin
    do
      lloc = 1
      call u8rdcom(iin,iout,line,ierr)
      if (ierr < 0) exit
      call urword(line, lloc, istart, istop, 1, ival, rval, iin, iout)
      if (line(istart:istop) == 'BEGIN') then
        call urword(line, lloc, istart, istop, 1, ival, rval, iin, iout)
        if (line(istart:istop) /= '') then
          isfound = .true.
          ctagfound = line(istart:istop)
          call u8rdcom(iin,iout,line2,ierr)
          if (ierr < 0) exit
          lloc2 = 1
          call urword(line2,lloc2,istart,istop,1,ival,rval,iout,iin)
          if (line2(istart:istop) == 'OPEN/CLOSE') then
            iuext = GetUnit()
            call urword(line2,lloc2,istart,istop,0,ival,rval,iout,iin)
            fname = line2(istart:istop)
            call openfile(iuext,iout,fname,'OPEN/CLOSE')
          else
            backspace(iin)
          endif
        else
          ermsg  = 'Block name missing in file.'
          call store_error(ermsg)
          call store_error_unit(iin)
          call ustop()
        end if
        exit
      end if
    end do
    return
  end subroutine uget_any_block

  subroutine uterminate_block(iin,iout,key,ctag,lloc,line,ierr,iuext)
! ******************************************************************************
! Possible abnormal block termination.  Terminate if 'begin' found or if
! 'end' encountered with incorrect tag.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    implicit none
    ! -- dummy
    integer(I4B), intent(in) :: iin
    integer(I4B), intent(in) :: iout
    character (len=*), intent(in) :: key
    character (len=*), intent(in) :: ctag
    integer(I4B), intent(inout) :: lloc
    character (len=*), intent(inout) :: line
    integer(I4B), intent(inout) :: ierr
    integer(I4B), intent(inout) :: iuext
    ! -- local
    character(len=LENBIGLINE) :: ermsg
    integer(I4B) :: istart
    integer(I4B) :: istop
    integer(I4B) :: ival
    real(DP) :: rval
    ! -- format
1   format('ERROR. "',A,'" DETECTED WITHOUT "',A,'". ','"END',1X,A, &
      '" MUST BE USED TO END ',A,'.')
2   format('ERROR. "',A,'" DETECTED BEFORE "END',1X,A,'". ','"END',1X,A, &
        '" MUST BE USED TO END ',A,'.')
! ------------------------------------------------------------------------------
    !code
    ierr = 1
    select case(key)
      case ('END')
        call urword(line, lloc, istart, istop, 1, ival, rval, iout, iin)
        if (line(istart:istop).ne.ctag) then
          write(ermsg, 1) trim(key), trim(ctag), trim(ctag), trim(ctag)
          call store_error(ermsg)
          call store_error_unit(iin)
          call ustop()
        else
          ierr = 0
          if (iuext /= iin) then
            ! close external file
            close(iuext)
            iuext = iin
          endif
        end if
      case ('BEGIN')
        write(ermsg, 2) trim(key), trim(ctag), trim(ctag), trim(ctag)
        call store_error(ermsg)
        call store_error_unit(iin)
        call ustop()
    end select
    return
  end subroutine uterminate_block

      SUBROUTINE UPCASE(WORD)
!C     ******************************************************************
!C     CONVERT A CHARACTER STRING TO ALL UPPER CASE
!C     ******************************************************************
!C       SPECIFICATIONS:
!C     ------------------------------------------------------------------
      CHARACTER WORD*(*)
!C
!C1------Compute the difference between lowercase and uppercase.
      L = LEN(WORD)
      IDIFF=ICHAR('a')-ICHAR('A')
!C
!C2------Loop through the string and convert any lowercase characters.
      DO 10 K=1,L
      IF(WORD(K:K).GE.'a' .AND. WORD(K:K).LE.'z') &
     &   WORD(K:K)=CHAR(ICHAR(WORD(K:K))-IDIFF)
10    CONTINUE
!C
!C3------return.
      RETURN
      END SUBROUTINE upcase

      subroutine lowcase(word)
!     ******************************************************************
!     Convert a character string to all lower case
!     ******************************************************************
!       specifications:
!     ------------------------------------------------------------------
      implicit none
      ! -- dummy
      character(len=*) :: word
      ! -- local
      integer(I4B) :: idiff, k, l
!
!------compute the difference between lowercase and uppercase.
      l = len(word)
      idiff=ichar('a')-ichar('A')
!
!------loop through the string and convert any uppercase characters.
      do k=1,l
        if(word(k:k).ge.'A' .and. word(k:k).le.'Z') then
          word(k:k)=char(ichar(word(k:k))+idiff)
        endif
      enddo
!
!------return.
      return
      end subroutine lowcase

      subroutine UWWORD(LINE,ICOL,ILEN,NCODE,C,N,R,FMT,CENTER,LEFT,SEP)
      implicit none
      ! -- dummy
      character (len=*), intent(inout) :: LINE
      integer(I4B), intent(inout) :: ICOL
      integer(I4B), intent(in) :: ILEN
      integer(I4B), intent(in) :: NCODE
      character (len=*), intent(in) :: C
      integer(I4B), intent(in) :: N
      real(DP), intent(in) :: R
      character (len=*), optional, intent(in) :: FMT
      logical, optional, intent(in) :: CENTER
      logical, optional, intent(in) :: LEFT
      character (len=*), optional, intent(in) :: SEP
      ! -- local
      character (len=16) :: cfmt
      character (len=ILEN) :: cval
      logical :: lcenter
      logical :: lleft
      integer(I4B) :: i
      integer(I4B) :: ispace
      integer(I4B) :: istop
      ! -- code
      if (present(FMT)) then
        CFMT = FMT
      else
        select case(NCODE)
          case(0, 1)
            write(cfmt, '(A,I0,A)') '(A', ILEN, ')'
          case(2)
            write(cfmt, '(A,I0,A)') '(I', ILEN, ')'
          case(3)
            i = ILEN - 7
            write(cfmt, '(A,I0,A,I0,A)') '(G', ILEN, '.', i, ')'
        end select
      end if

      if (present(CENTER)) then
        lcenter = CENTER
      else
        lcenter = .FALSE.
      end if

      if (present(LEFT)) then
        lleft = LEFT
      else
        lleft = .FALSE.
      end if

      if (NCODE == 0 .or. NCODE == 1) then
        if (len_trim(adjustl(C)) > ILEN) then
          cval = adjustl(C)
        else
          cval = trim(adjustl(C))
        end if
        if (lcenter) then
          i = len_trim(cval)
          ispace = (ILEN - i) / 2
          cval = repeat(' ', ispace) // trim(cval)
        else if (lleft) then
          cval = trim(adjustl(cval))
        else
          cval = adjustr(cval)
        end if
        if (NCODE == 1) then
          call UPCASE(cval)
        end if
      end if

      istop = ICOL + ILEN

      select case(NCODE)
        case(0, 1)
          write(LINE(ICOL:istop), cfmt) cval
        case(2)
          write(LINE(ICOL:istop), cfmt) N
        case(3)
          write(LINE(ICOL:istop), cfmt) R
      end select

      ICOL = istop

      if (present(SEP)) then
        i = len(SEP)
        istop = ICOL + i
        write(LINE(ICOL:istop), '(A)') SEP
        ICOL = istop
      end if
!
!------return.
      return
      end subroutine UWWORD

      SUBROUTINE URWORD(LINE,ICOL,ISTART,ISTOP,NCODE,N,R,IOUT,IN)
!C     ******************************************************************
!C     ROUTINE TO EXTRACT A WORD FROM A LINE OF TEXT, AND OPTIONALLY
!C     CONVERT THE WORD TO A NUMBER.
!C        ISTART AND ISTOP WILL BE RETURNED WITH THE STARTING AND
!C          ENDING CHARACTER POSITIONS OF THE WORD.
!C        THE LAST CHARACTER IN THE LINE IS SET TO BLANK SO THAT IF ANY
!C          PROBLEMS OCCUR WITH FINDING A WORD, ISTART AND ISTOP WILL
!C          POINT TO THIS BLANK CHARACTER.  THUS, A WORD WILL ALWAYS BE
!C          RETURNED UNLESS THERE IS A NUMERIC CONVERSION ERROR.  BE SURE
!C          THAT THE LAST CHARACTER IN LINE IS NOT AN IMPORTANT CHARACTER
!C          BECAUSE IT WILL ALWAYS BE SET TO BLANK.
!C        A WORD STARTS WITH THE FIRST CHARACTER THAT IS NOT A SPACE OR
!C          COMMA, AND ENDS WHEN A SUBSEQUENT CHARACTER THAT IS A SPACE
!C          OR COMMA.  NOTE THAT THESE PARSING RULES DO NOT TREAT TWO
!C          COMMAS SEPARATED BY ONE OR MORE SPACES AS A NULL WORD.
!C        FOR A WORD THAT BEGINS WITH "'", THE WORD STARTS WITH THE
!C          CHARACTER AFTER THE QUOTE AND ENDS WITH THE CHARACTER
!C          PRECEDING A SUBSEQUENT QUOTE.  THUS, A QUOTED WORD CAN
!C          INCLUDE SPACES AND COMMAS.  THE QUOTED WORD CANNOT CONTAIN
!C          A QUOTE CHARACTER.
!C        IF NCODE IS 1, THE WORD IS CONVERTED TO UPPER CASE.
!C        IF NCODE IS 2, THE WORD IS CONVERTED TO AN INTEGER.
!C        IF NCODE IS 3, THE WORD IS CONVERTED TO A REAL NUMBER.
!C        NUMBER CONVERSION ERROR IS WRITTEN TO UNIT IOUT IF IOUT IS
!C          POSITIVE; ERROR IS WRITTEN TO DEFAULT OUTPUT IF IOUT IS 0;
!C          NO ERROR MESSAGE IS WRITTEN IF IOUT IS NEGATIVE.
!C     ******************************************************************
!C
!C        SPECIFICATIONS:
!C     ------------------------------------------------------------------
      integer(I4B), intent(inout) :: n
      real(DP),intent(inout) :: r
      CHARACTER(len=*) LINE
      CHARACTER(len=20) STRING
      CHARACTER(len=30) RW
      CHARACTER(len=1) TAB
      character(len=200) :: msg
!C     ------------------------------------------------------------------
      TAB=CHAR(9)
!C
!C1------Set last char in LINE to blank and set ISTART and ISTOP to point
!C1------to this blank as a default situation when no word is found.  If
!C1------starting location in LINE is out of bounds, do not look for a
!C1------word.
      LINLEN=LEN(LINE)
      LINE(LINLEN:LINLEN)=' '
      ISTART=LINLEN
      ISTOP=LINLEN
      LINLEN=LINLEN-1
      IF(ICOL.LT.1 .OR. ICOL.GT.LINLEN) GO TO 100
!C
!C2------Find start of word, which is indicated by first character that
!C2------is not a blank, a comma, or a tab.
      DO 10 I=ICOL,LINLEN
      IF(LINE(I:I).NE.' ' .AND. LINE(I:I).NE.',' &
     &    .AND. LINE(I:I).NE.TAB) GO TO 20
10    CONTINUE
      ICOL=LINLEN+1
      GO TO 100
!C
!C3------Found start of word.  Look for end.
!C3A-----When word is quoted, only a quote can terminate it.
20    IF(LINE(I:I).EQ.'''') THEN
         I=I+1
         IF(I.LE.LINLEN) THEN
            DO 25 J=I,LINLEN
            IF(LINE(J:J).EQ.'''') GO TO 40
25          CONTINUE
         END IF
!C
!C3B-----When word is not quoted, space, comma, or tab will terminate.
      ELSE
         DO 30 J=I,LINLEN
         IF(LINE(J:J).EQ.' ' .OR. LINE(J:J).EQ.',' &
     &    .OR. LINE(J:J).EQ.TAB) GO TO 40
30       CONTINUE
      END IF
!C
!C3C-----End of line without finding end of word; set end of word to
!C3C-----end of line.
      J=LINLEN+1
!C
!C4------Found end of word; set J to point to last character in WORD and
!C-------set ICOL to point to location for scanning for another word.
40    ICOL=J+1
      J=J-1
      IF(J.LT.I) GO TO 100
      ISTART=I
      ISTOP=J
!C
!C5------Convert word to upper case and RETURN if NCODE is 1.
      IF(NCODE.EQ.1) THEN
         IDIFF=ICHAR('a')-ICHAR('A')
         DO 50 K=ISTART,ISTOP
            IF(LINE(K:K).GE.'a' .AND. LINE(K:K).LE.'z') &
     &             LINE(K:K)=CHAR(ICHAR(LINE(K:K))-IDIFF)
50       CONTINUE
         RETURN
      END IF
!C
!C6------Convert word to a number if requested.
100   IF(NCODE.EQ.2 .OR. NCODE.EQ.3) THEN
         RW=' '
         L=30-ISTOP+ISTART
         IF(L.LT.1) GO TO 200
         RW(L:30)=LINE(ISTART:ISTOP)
         IF(NCODE.EQ.2) READ(RW,'(I30)',ERR=200) N
         IF(NCODE.EQ.3) READ(RW,'(F30.0)',ERR=200) R
      END IF
      RETURN
!C
!C7------Number conversion error.
200   IF(NCODE.EQ.3) THEN
         STRING= 'A REAL NUMBER'
         L=13
      ELSE
         STRING= 'AN INTEGER'
         L=10
      END IF
!C
!C7A-----If output unit is negative, set last character of string to 'E'.
      IF(IOUT.LT.0) THEN
         N=0
         R=0.
         LINE(LINLEN+1:LINLEN+1)='E'
         RETURN
!C
!C7B-----If output unit is positive; write a message to output unit.
      ELSE IF(IOUT.GT.0) THEN
         IF(IN.GT.0) THEN
            WRITE(IOUT,201) IN,LINE(ISTART:ISTOP),STRING(1:L),LINE
         ELSE
            WRITE(IOUT,202) LINE(ISTART:ISTOP),STRING(1:L),LINE
         END IF
201      FORMAT(1X,/1X,'FILE UNIT ',I4,' : ERROR CONVERTING "',A, &
     &       '" TO ',A,' IN LINE:',/1X,A)
202      FORMAT(1X,/1X,'KEYBOARD INPUT : ERROR CONVERTING "',A, &
     &       '" TO ',A,' IN LINE:',/1X,A)
!C
!C7C-----If output unit is 0; write a message to default output.
      ELSE
         IF(IN.GT.0) THEN
            WRITE(*,201) IN,LINE(ISTART:ISTOP),STRING(1:L),LINE
         ELSE
            WRITE(*,202) LINE(ISTART:ISTOP),STRING(1:L),LINE
         END IF
      END IF
!C
!C7D-----STOP after storing error message.
      call lowcase(string)
      if (in > 0) then
        write(msg,205)in,line(istart:istop),trim(string)
      else
        write(msg,207)line(istart:istop),trim(string)
      endif
205   format('File unit ',I0,': Error converting "',A, &
     &       '" to ',A,' in following line:')
207   format('Keyboard input: Error converting "',A, &
     &       '" to ',A,' in following line:')
      call store_error(msg)
      call store_error(trim(line))
      call store_error_unit(in)
      call ustop()
      !
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
    1 FORMAT(1X,'UBDSV4 SAVING "',A16,'" ON UNIT',I4, &
     &     ' AT TIME STEP',I3,', STRESS PERIOD',I4)
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
    5 IP=IPRN
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
      DO 1000 I=1,NROW
      GO TO(10,20,30,40,50,60,70,80,90,100,110,120,130,140,150,160,170, &
     &      180,190,200,210), IP
!C
!C------------ FORMAT 11G10.3
   10 WRITE(IOUT,11) I,(BUF(J,I),J=1,NCOL)
   11 FORMAT(1X,I3,2X,1PG10.3,10(1X,G10.3):/(5X,11(1X,G10.3)))
      GO TO 1000
!C
!C------------ FORMAT 9G13.6
   20 WRITE(IOUT,21) I,(BUF(J,I),J=1,NCOL)
   21 FORMAT(1X,I3,2X,1PG13.6,8(1X,G13.6):/(5X,9(1X,G13.6)))
      GO TO 1000
!C
!C------------ FORMAT 15F7.1
   30 WRITE(IOUT,31) I,(BUF(J,I),J=1,NCOL)
   31 FORMAT(1X,I3,1X,15(1X,F7.1):/(5X,15(1X,F7.1)))
      GO TO 1000
!C
!C------------ FORMAT 15F7.2
   40 WRITE(IOUT,41) I,(BUF(J,I),J=1,NCOL)
   41 FORMAT(1X,I3,1X,15(1X,F7.2):/(5X,15(1X,F7.2)))
      GO TO 1000
!C
!C------------ FORMAT 15F7.3
   50 WRITE(IOUT,51) I,(BUF(J,I),J=1,NCOL)
   51 FORMAT(1X,I3,1X,15(1X,F7.3):/(5X,15(1X,F7.3)))
      GO TO 1000
!C
!C------------ FORMAT 15F7.4
   60 WRITE(IOUT,61) I,(BUF(J,I),J=1,NCOL)
   61 FORMAT(1X,I3,1X,15(1X,F7.4):/(5X,15(1X,F7.4)))
      GO TO 1000
!C
!C------------ FORMAT 20F5.0
   70 WRITE(IOUT,71) I,(BUF(J,I),J=1,NCOL)
   71 FORMAT(1X,I3,1X,20(1X,F5.0):/(5X,20(1X,F5.0)))
      GO TO 1000
!C
!C------------ FORMAT 20F5.1
   80 WRITE(IOUT,81) I,(BUF(J,I),J=1,NCOL)
   81 FORMAT(1X,I3,1X,20(1X,F5.1):/(5X,20(1X,F5.1)))
      GO TO 1000
!C
!C------------ FORMAT 20F5.2
   90 WRITE(IOUT,91) I,(BUF(J,I),J=1,NCOL)
   91 FORMAT(1X,I3,1X,20(1X,F5.2):/(5X,20(1X,F5.2)))
      GO TO 1000
!C
!C------------ FORMAT 20F5.3
  100 WRITE(IOUT,101) I,(BUF(J,I),J=1,NCOL)
  101 FORMAT(1X,I3,1X,20(1X,F5.3):/(5X,20(1X,F5.3)))
      GO TO 1000
!C
!C------------ FORMAT 20F5.4
  110 WRITE(IOUT,111) I,(BUF(J,I),J=1,NCOL)
  111 FORMAT(1X,I3,1X,20(1X,F5.4):/(5X,20(1X,F5.4)))
      GO TO 1000
!C
!C------------ FORMAT 10G11.4
  120 WRITE(IOUT,121) I,(BUF(J,I),J=1,NCOL)
  121 FORMAT(1X,I3,2X,1PG11.4,9(1X,G11.4):/(5X,10(1X,G11.4)))
      GO TO 1000
!C
!C------------ FORMAT 10F6.0
  130 WRITE(IOUT,131) I,(BUF(J,I),J=1,NCOL)
  131 FORMAT(1X,I3,1X,10(1X,F6.0):/(5X,10(1X,F6.0)))
      GO TO 1000
!C
!C------------ FORMAT 10F6.1
  140 WRITE(IOUT,141) I,(BUF(J,I),J=1,NCOL)
  141 FORMAT(1X,I3,1X,10(1X,F6.1):/(5X,10(1X,F6.1)))
      GO TO 1000
!C
!C------------ FORMAT 10F6.2
  150 WRITE(IOUT,151) I,(BUF(J,I),J=1,NCOL)
  151 FORMAT(1X,I3,1X,10(1X,F6.2):/(5X,10(1X,F6.2)))
      GO TO 1000
!C
!C------------ FORMAT 10F6.3
  160 WRITE(IOUT,161) I,(BUF(J,I),J=1,NCOL)
  161 FORMAT(1X,I3,1X,10(1X,F6.3):/(5X,10(1X,F6.3)))
      GO TO 1000
!C
!C------------ FORMAT 10F6.4
  170 WRITE(IOUT,171) I,(BUF(J,I),J=1,NCOL)
  171 FORMAT(1X,I3,1X,10(1X,F6.4):/(5X,10(1X,F6.4)))
      GO TO 1000
!C
!C------------ FORMAT 10F6.5
  180 WRITE(IOUT,181) I,(BUF(J,I),J=1,NCOL)
  181 FORMAT(1X,I3,1X,10(1X,F6.5):/(5X,10(1X,F6.5)))
      GO TO 1000
!C
!C------------FORMAT 5G12.5
  190 WRITE(IOUT,191) I,(BUF(J,I),J=1,NCOL)
  191 FORMAT(1X,I3,2X,1PG12.5,4(1X,G12.5):/(5X,5(1X,G12.5)))
      GO TO 1000
!C
!C------------FORMAT 6G11.4
  200 WRITE(IOUT,201) I,(BUF(J,I),J=1,NCOL)
  201 FORMAT(1X,I3,2X,1PG11.4,5(1X,G11.4):/(5X,6(1X,G11.4)))
      GO TO 1000
!C
!C------------FORMAT 7G9.2
  210 WRITE(IOUT,211) I,(BUF(J,I),J=1,NCOL)
  211 FORMAT(1X,I3,2X,1PG9.2,6(1X,G9.2):/(5X,7(1X,G9.2)))
!C
 1000 CONTINUE
!C
!C5------RETURN
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
    ! -- local
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
    ! -- local
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
    ! -- local
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
    ! -- dummy variables
    character(len=*), intent(in) :: word1, word2
    ! -- local
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
    ! -- dummy
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
    ! -- dummy
    integer(I4B), intent(in) :: nodenumber
    integer(I4B), intent(in) :: nrow
    integer(I4B), intent(in) :: ncol
    integer(I4B), intent(in) :: nlay
    integer(I4B), intent(out) :: irow
    integer(I4B), intent(out) :: icol
    integer(I4B), intent(out) :: ilay
    ! -- local
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

  subroutine get_jk(nodenumber, ncpl, nlay, icpl, ilay)
    ! Calculate icpl, and ilay from the nodenumber and grid
    ! dimensions.  If nodenumber is invalid, set irow, icol, and
    ! ilay to -1
    implicit none
    ! -- dummy
    integer(I4B), intent(in) :: nodenumber
    integer(I4B), intent(in) :: ncpl
    integer(I4B), intent(in) :: nlay
    integer(I4B), intent(out) :: icpl
    integer(I4B), intent(out) :: ilay
    ! -- local
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
    integer(I4B) :: iu
    character(len=100) :: fname,ac,act,fm,frm,seq,unf
    inquire(unit=iu,name=fname,access=ac,action=act,formatted=fm, &
    sequential=seq,unformatted=unf,form=frm)

    10 format('unit:',i4,'  name:',a,'  access:',a,'  action:',a,/, &
    '    formatted:',a, &
    '  sequential:',a,'  unformatted:',a,'  form:',a)

    write(*,10)iu,trim(fname),trim(ac),trim(act),trim(fm),trim(seq), &
    trim(unf),trim(frm)
    return
  end subroutine unitinquire

  subroutine ParseLine(line, nwords, words, inunit, filename)
    ! Parse a line into words. Blanks and commas are recognized as
    ! delimiters. Multiple blanks between words is OK, but multiple
    ! commas between words is treated as an error. Quotation marks
    ! are not recognized as delimiters.
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
    ! -- Count words in line and allocate words array
    lloc = 1
    do
      call URWORD(line, lloc, istart, istop, 0, idum, rdum, 0, 0)
      if (istart == linelen) exit
      nwords = nwords + 1
    enddo
    allocate(words(nwords))
    !
    ! -- Populate words array and return
    lloc = 1
    do i = 1, nwords
      call URWORD(line, lloc, istart, istop, 0, idum, rdum, 0, 0)
      words(i) = line(istart:istop)
    enddo
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
    ! -- dummy
    integer(I4B), intent(in) :: ncol, nrow, kstp, kper, ilay, iout
    real(DP),dimension(ncol,nrow), intent(in) :: buf
    character(len=*), intent(in) :: text
    character(len=*), intent(in) :: userfmt
    integer(I4B), intent(in) :: nvalues, nwidth
    character(len=1), intent(in) :: editdesc
    ! -- local
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
    enddo
    !
    return
  end subroutine ulaprufw

  subroutine write_centered(text, iout, linelen)
    ! Write text to unit iout centered in width defined by linelen
    ! Left-pad with blanks as needed.
    use ConstantsModule, only: LINELENGTH
    implicit none
    ! -- dummy
    character(len=*), intent(in) :: text
    integer(I4B), intent(in) :: iout
    integer(I4B), intent(in) :: linelen
    ! -- local
    integer(I4B) :: loc1, loc2, lentext, nspaces
    character(len=LINELENGTH) :: newline, textleft
    !
    if (iout<=0) return
    textleft = adjustl(text)
    lentext = len_trim(textleft)
    nspaces = linelen - lentext
    loc1 = (nspaces / 2) + 1
    loc2 = loc1 + lentext - 1
    newline = ' '
    newline(loc1:loc2) = textleft
    write(iout,'(a)')trim(newline)
    !
    return
  end subroutine write_centered

  function linear_interpolate(t0, t1, y0, y1, t) result(y)
    implicit none
    ! -- dummy
    real(DP), intent(in) :: t, t0, t1, y0, y1
    real(DP)             :: y
    ! -- local
    real(DP) :: delt, dely, slope
    character(len=100) :: msg
    !
    ! -- don't get bitten by rounding errors or divide-by-zero
    if (dclosetest(t0, t1) .or. dclosetest(t, t1)) then
      y = y1
    elseif (t == t0) then
      y = y0
    elseif ((t0 < t .and. t < t1) .or. (t1 < t .and. t < t0)) then
      ! -- perform linear interpolation
      delt = t1 - t0
      dely = y1 - y0
      slope = dely / delt
      y = y0 + slope * (t - t0)
    else
      ! -- t is outside range t0 to t1
      msg = 'Error: in linear_interpolate, t is outside range t0 to t1'
      call store_error(msg)
      call ustop()
    endif
    !
    return
  end function linear_interpolate

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
    ! -- dummy
    integer(I4B), intent(in)           :: iu
    logical, intent(out)          :: eof
    character(len=:), allocatable :: astring
    ! -- local
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
        call ustop()
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
    return
  end subroutine GetFileFromPath

  subroutine extract_idnum_or_bndname(line, icol, istart, istop, idnum, bndname)
    ! Starting at position icol, define string as line(istart:istop).
    ! If string can be interpreted as an integer(I4B), return integer in idnum argument.
    ! If token is not an integer(I4B), assume it is a boundary name, return NAMEDBOUNDFLAG
    ! in idnum, convert string to uppercase and return it in bndname.
    implicit none
    ! -- dummy
    character(len=*),            intent(inout) :: line
    integer(I4B),                     intent(inout) :: icol, istart, istop
    integer(I4B),                     intent(out)   :: idnum
    character(len=LENBOUNDNAME), intent(out)   :: bndname
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
    return
  end subroutine extract_idnum_or_bndname

  subroutine urdaux(naux, inunit, iout, lloc, istart, istop, auxname, line,  &
                    text)
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
    real(DP) :: rval
    character(len=LINELENGTH) :: errmsg
! ------------------------------------------------------------------------------
    linelen = len(line)
    if(naux > 0) then
      write(errmsg,'(a)') '****ERROR. AUXILIARY VARIABLES ' //         &
        'ALREADY SPECIFIED. AUXILIARY VARIABLES MUST BE SPECIFIED '//  &
        'ON ONE LINE IN THE OPTIONS BLOCK.'
      call store_error(errmsg)
      call store_error_unit(inunit)
      call ustop()
    endif
    auxloop: do
      call urword(line, lloc, istart, istop, 1, n, rval, iout, inunit)
      if(lloc >= linelen) exit auxloop
      naux = naux + 1
      call ExpandArray(auxname)
      auxname(naux) = line(istart:istop)
      if(iout > 0) then
        write(iout, "(4X,'AUXILIARY ',a,' VARIABLE: ',A)")                     &
          trim(adjustl(text)), auxname(naux)
      endif
    enddo auxloop

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
      call store_error_unit(inunit)
      call ustop()
    endif
    !
    ermsg = 'Error setting PRINT_FORMAT. Syntax is incorrect in line:'
    if (nwords >= 4) then
      if (.not. same_word(words(1), 'COLUMNS')) ierr = 1
      if (.not. same_word(words(3), 'WIDTH')) ierr = 1
      ! -- Read nvalues and nwidth
      read(words(2), *, iostat=ierr) nvaluesp
      if (ierr == 0) then
        read(words(4), *, iostat=ierr) nwidthp
      endif
    else
      ierr = 1
    endif
    if (ierr /= 0) then
      call store_error(ermsg)
      call store_error(line)
      call store_error_unit(inunit)
      call ustop()
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
          ermsg = 'Error in Output Control: Unrecognized option: ' // words(i)
          call store_error(ermsg)
          call store_error_unit(inunit)
          call ustop()
        end select
      else
        exit
      endif
    enddo
    if (ierr /= 0) then
      call store_error(ermsg)
      call store_error(line)
      call store_error_unit(inunit)
      call ustop()
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
    ! -- dummy
    integer(I4B), intent(in) :: nvalsp, nwidp, ndig
    character(len=*), intent(inout) :: outfmt
    logical, intent(in), optional :: prowcolnum  ! default true
    ! -- local
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
    ! -- dummy
    integer(I4B), intent(in) :: nvalsp, nwidp, ndig
    character(len=*), intent(in) :: editdesc
    character(len=*), intent(inout) :: outfmt
    logical, intent(in), optional :: prowcolnum  ! default true
    ! -- local
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
    ! -- dummy
    integer(I4B), intent(in) :: nvalsp, nwidp
    character(len=*), intent(inout) :: outfmt
    logical, intent(in), optional :: prowcolnum  ! default true
    ! -- local
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

END MODULE InputOutputModule
