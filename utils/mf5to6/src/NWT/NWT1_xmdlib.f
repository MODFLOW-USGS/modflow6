!
!                        libxmd
!                      version 2.1.2
!
!                 (c) 2011   M. Ibaraki
!
!        version 2.0     April 30, 2011
!                    include realloc and ialloc with arrays for semi- list.append() function in python
!                    must be replaced with c program
!        version 2.1     May 8, 2011
!                    no scaling is required for red-black ordering (reduced system)
!        version 2.1.1   May 12, 2011
!                    minor changes
!        version 2.1.2   January 23, 2012
!                    resolved cgstab and orthomin initialization issues
!
!
      module xmdalloc
      contains
      subroutine realloc(a, n)
!         a: array
!         n: new size
        implicit none
        double precision, allocatable, dimension(:) :: a, tmp
        integer :: n, nold, ierr
        nold = size(a)
        if (nold > n) return
        allocate (tmp(nold+200000), stat=ierr)
        if (ierr /= 0) stop "allocate error"
        tmp(1:nold) = a(1:nold)
        deallocate (a)
        allocate (a(n+200000), stat=ierr)
        if (ierr /= 0) stop "allocate error"
        a(1:nold) = tmp(1:nold)
      end subroutine realloc

      subroutine ialloc(a, n)
!         a: array
!         n: new size
        implicit none
        integer, allocatable, dimension(:) :: a, tmp
        integer :: n, nold, ierr
        nold = size(a)
        if (nold > n) return
        allocate (tmp(nold+200000), stat=ierr)
        if (ierr /= 0) stop "allocate error"
        tmp(1:nold) = a(1:nold)
        deallocate (a)
        allocate (a(n+200000), stat=ierr)
        if (ierr /= 0) stop "allocate error"
        a(1:nold) = tmp(1:nold)
      end subroutine ialloc

      end module xmdalloc





      module xmdcmn
c
c     miunit      unit number for output
c     miout       switch for information message
c                  <  0 nothing printed
c                  >= 0 print all
c
      integer ::  miunit, miout

      end module xmdcmn



      module xmdmatrix

!     icolour()        nn       poiter array for red-black ordered matrix
!     RBorder()       nn        Red-Black ordering vector
!     iblackend()       nn      end markder for black nodes in a row
!     iaf()        nblack+1     YSMP array for factored matrix
!     jaf()         njaf        YSMP array for factored matrix                   
!     idiagf()     nblack       diagonal location for factored matrix
!     af()          njaf        factored matrix (each row of af contains a row L\U)
!                               where A = LU
!     lorder()      nn          ordering vector  lorder(new_number) = old_number
!     njaf                      size of jaf() and af()
!     nblack                    number of black nodes

      integer, dimension(:), allocatable :: icolour, RBorder, iblackend,
     [                                      iaf, idiagf, lorder
      integer :: njaf, nblack

      integer, dimension(:), allocatable :: jaf
      double precision, dimension(:), allocatable :: af

      end module xmdmatrix





      module xmdSymFacLev
      contains
      subroutine xmdsfacl(iaf, jafwk, idiagf, ia, ja,
     [                    icolour, RBorder, iblackend,
     [                    n, nja, njaf, level, nblack, ierr)

      use xmdcmn
      use xmdalloc

      implicit none
      integer :: ia(n+1), ja(nja), idiagf(nblack), iaf(nblack+1), 
     [        icolour(n), RBorder(n), iblackend(n),
     [        n, nja, njaf, ierr, level, nblack

c     integer, dimension(:), allocatable :: jaf
c
c
c     incomplete LU factorization
c
c     jaf() is wk array, on exit first iaf(n+1)-1 entries
c     are ja() factors 
c
c
c     output: iaf, jaf, idiagf, njaf
c
c
c     lorder(new_order)=old_order
c
c     brute force factor
c
c     local variables
c
c
      integer :: itemp, ii, first, iold, iend, num, next,
     [           iblck, id, kk, nred, ierror
      integer :: maxint = 999999

      integer, dimension(:), allocatable :: levptr1, list

      integer, dimension(:), allocatable :: jafwk, levptr, jafwk0
      if (allocated(jafwk)) deallocate (jafwk)
      allocate( jafwk(1), levptr(1), stat = ierror )
      if (ierror /= 0) stop "== not enough memory (xmdsfacl) =="

      allocate( levptr1(n), list(nblack), stat = ierror )
      if (ierror /= 0) stop "== not enough memory (xmdsfacl) =="

c     initialize arrays

      iaf(1:nblack+1) = 0
      list(1:nblack) = 0
      idiagf(1:nblack) = 0
      levptr1(1:n) = maxint

      iaf(1) = 1

      itemp = 0                    ! current position in jaf

      nred = n - nblack

      BlackNodes: do iblck = 1, nblack       ! do it in NEW number!!

        iold = RBorder(iblck)       ! black node number in old numbering

c     load row i of L/U into list  -- black node

        iend = itemp

c     diagonal is black

        id = iblck
        iend = iend + 1

        call ialloc(jafwk, iend)

        jafwk(iend) = id

c       off-diagonals - black node

        do ii = ia(iold)+1, iblackend(iold)
          id = icolour(ja(ii))
          iend = iend + 1
          call ialloc(jafwk, iend)
          jafwk(iend) = id
        enddo

c     load row i of L/U into list  -- red node elimination

        do ii = iblackend(iold)+1, ia(iold+1)-1 ! off-diagonals and red

          id = ja(ii)

c       ignore black node or diagonal node

          do kk = ia(id)+1, ia(id+1)-1   ! off-diagonals

            iend = iend + 1
            call ialloc(jafwk, iend)
            jafwk(iend) = icolour(ja(kk))

          enddo

        enddo

        num = iend-itemp

c       sort entries

        call xmdshell(jafwk(itemp+1), num)

        do ii = itemp+1, iend
          levptr1( jafwk(ii) ) = 0

c      new fill-in caused by red node elimination -> level = 1

          if (icolour(jafwk(ii)) < 0) levptr1( jafwk(ii) ) = 1
        enddo


        first = jafwk(itemp+1)
        do ii = itemp+1, iend-1
          list(jafwk(ii)) = jafwk(ii+1)   ! link list containing next column number
        enddo
        list(jafwk(iend)) = n+1         ! end marker

        call xmdmrgl(iblck, iaf, jafwk, idiagf, list, size(jafwk),
     [               nblack,
     [               first, level, levptr, levptr1, size(levptr))

        next = first
  100   continue
        if (next /= n+1) then
          itemp = itemp+1
          call ialloc(jafwk, itemp)
          jafwk(itemp) = next

          call ialloc(levptr, itemp)
          levptr(itemp) = levptr1( next ) 

          levptr1( next ) = maxint

          if (next == iblck) idiagf(iblck) = itemp

          next = list(next)   ! link list
          goto 100
        endif
        iaf(iblck+1) = itemp+1
        if (idiagf(iblck) == 0) then
          ierr  =  3
          write (miunit,200) iblck
          return
        endif

      enddo BlackNodes


  200 format ('  error in xmdsfacl'/
     [        '    no diagonal in L\U:  row number',i8)

c     minjaf = iaf(nblack+1)-1   ! size of af and jaf

      njaf = itemp
c     print *, 'actual size of af, jaf arrays (njaf): ', njaf

c     the size of jafwk is bigger than njaf...

      allocate (jafwk0(njaf))
      jafwk0(1:njaf) = jafwk(1:njaf)
      deallocate (jafwk)
      allocate (jafwk(njaf))

      jafwk(1:njaf) = jafwk0(1:njaf)

c     deallocate(levptr, levptr1, list, jafwk0)
c     nullify(jafwk0, levptr)

      end subroutine xmdsfacl
      end module xmdSymFacLev 







      module xmdSymFacDrop
      contains

      subroutine xmdsfacd(a, b, afwk, epsrn, iaf, jafwk, idiagf,
     [                    ia, ja, icolour, RBorder, iblackend,
     [                    n, nja, njaf, level, nblack, ierr)


      use xmdcmn
      use xmdalloc

      integer ia(n+1), ja(nja), idiagf(nblack),
     [        iaf(nblack+1),
     [        icolour(n), RBorder(n), iblackend(n),
     [        n, nja, njaf, ierr, level, nblack

      double precision a(nja), b(n), epsrn
c
c
c     incomplete LU factorization which uses the combination of
c     level and drop tolerance
c
c
c     input:
c       a()            coefficient matrix
c       b()            RHS vector
c       epsrn          drop tolerance
c       ia(), ja()     YSMP pointers for a
c       icolour()        pointer for node attribute
c                > 0  -> black node number
c                < 0  -> red   node number
c       iblackend()       pointer to indicate the end of black node in a row
c       RBorder()       permutation vector for black-red
c          - RBorder(1:nblack)
c             original_node_number = RBorder(black_node_number)
c
c          - RBorder(nblack+1:neq)
c             original_node_number = RBorder(red_node_number)
c                                  note: decrease in numbers
c       n              total number of nodes
c       nja            size of ja()
c       njaf           size of af() and jaf()
c       lsize          size of levptr ( >= njaf+nblack )
c       level          level of ILU
c       nblack         number of black nodes
c     output:
c       af()           factored coefficient matrix
c       iaf(), jaf()   YSMP pointers for af
c       idiagf()       diagonal pointer for af
c       ierr           error flag
c     temporary:
c       row()          real temp work array
c       list()         integer temp work array
c       levptr()       integer temp work array and devide this as
c                      [njaf][n]
c                       ^^^^  ^
c                [level in af][level in row i]
c
c
c     jaf() is wk array, on exit first iaf(n+1)-1 entries
c     are ja() factors 
c
c     brute force factor
c
c     local variables
c
      integer :: i, itemp, ii, first, iold, iend, num, next,
     [        maxint, iblck, id, kk, idk, idkk
      double precision temp, epsmin

      double precision, dimension(:), allocatable :: row
      integer, dimension(:), allocatable :: list, levptr1
      parameter (maxint = 999999)

      integer, dimension(:), allocatable :: jafwk, levptr, jafwk0
      double precision, dimension(:), allocatable :: afwk, afwk0

      allocate( levptr(1) )
      if (allocated(jafwk)) deallocate (jafwk)
      if (allocated(afwk)) deallocate (afwk)
      allocate( jafwk(1),  afwk(1) , stat = ierror )
      if (ierror /= 0) stop "== not enough memory (xmdsfacd) =="


c     lsize = njaf+nblack

      allocate( row(nblack), stat = ierror )
      if (ierror /= 0) stop "== not enough memory (xmdsfacd) =="

      allocate( list(nblack), levptr1(nblack), stat = ierror )
      if (ierror /= 0) stop "== not enough memory (xmdsfacd) =="

      epsmin = 1.0d-300

c     initialize arrays

      levptr1(1:nblack) = maxint
      iaf(1:nblack+1) = 0
      list(1:nblack) = 0
      idiagf(nblack) = 0
      row(1:nblack) = 0.0d0

      iaf(nblack+1) = 0
      iaf(1) = 1
      itemp = 0                    ! current position in jaf

      do 4 iblck = 1, nblack       ! do it in NEW number!!

        iold = RBorder(iblck)       ! black node number in old numbering

c     load row i of L/U into list  -- black node

        iend = itemp

c     diagonal is black

        id = iblck
        iend = iend + 1
        call ialloc(jafwk, iend)
        jafwk(iend) = id

        row(id) = a( ia(iold) )

c       off-diagonals - black node

        do ii = ia(iold)+1, iblackend(iold)
          id = icolour(ja(ii))
          iend = iend + 1
          call ialloc(jafwk, iend)
          jafwk(iend) = id
          row(id) = row(id) + a(ii)
        enddo

c     load row i of L/U into list  -- red node elimination

        do ii = iblackend(iold)+1, ia(iold+1)-1 ! off-diagonals and red

          id = ja(ii)

c       ignore black node or diagonal node

          do kk = ia(id)+1, ia(id+1)-1   ! off-diagonals

            iend = iend + 1
            call ialloc(jafwk, iend)
            jafwk(iend) = icolour(ja(kk))

            idk = ja(kk)
            idkk = icolour(idk)

            row(idkk) = row(idkk) - a(ii) * a(kk) / a( ia(id) )

          enddo

c      modify {b}

          b(iold) = b(iold) - a(ii)*b(id) / a( ia(id) )

        enddo

        num = iend-itemp

c       sort entries

        call xmdshell(jafwk(itemp+1), num)

        do ii = itemp+1, iend

          levptr1(jafwk(ii)) = 0

c      new fill-in caused by red node elimination -> level = 1

          if (icolour(jafwk(ii)) < 0) levptr1(jafwk(ii)) = 1

        enddo


        first = jafwk(itemp+1)
        do ii = itemp+1, iend-1
          list(jafwk(ii)) = jafwk(ii+1)   ! link list containing next column number
        enddo
        list(jafwk(iend)) = n+1         ! end marker


        call xmdmrgd(a, afwk, row, epsrn, iblck, nblack, ia, iaf,
     [              jafwk, idiagf, list, RBorder, nja, size(jafwk),
     [               n, first, level, levptr, levptr1, size(levptr))

        next = first
  100   continue
        if (next /= n+1) then
          itemp = itemp+1
          call ialloc(jafwk, itemp)
          jafwk(itemp) = next

          call ialloc(levptr, itemp)
          levptr(itemp) = levptr1(next)
          levptr1(next) = maxint

          if (next == iblck) idiagf(iblck) = itemp

          next = list(next)
          goto 100
        endif
        iaf(iblck+1) = itemp+1
        if (idiagf(iblck) == 0) then
          ierr  =  3
          write (miunit,200) iblck
          return
        endif

c     inverse of diagonal to avoid small pivots

        temp = row(iblck)
        ii = idiagf(iblck)

        call realloc(afwk, ii)
        afwk(ii) = 1.0d0/(temp + epsmin)

c
c     gathering full length array
c
        do ii = iaf(iblck), iaf(iblck+1)-1

        call realloc(afwk, ii)
        afwk(ii) = row(jafwk(ii)) 

          row(jafwk(ii)) = 0.0d0
          list(jafwk(ii)) = 0
        enddo

    4 continue

  200 format ('  error in xmdsfacd'/
     [        '    no diagonal in L\U:  row number',i8)

c     minjaf = iaf(nblack+1)-1   ! size of af and jaf

      njaf = itemp

c     print *, 'actual size of af, jaf arrays (njaf): ', njaf

c     the size of jafwk, afwk is bigger than njaf...

c     jafwk0 => jafwk   ! ....wk0 are tmp arrays
c     afwk0 => afwk

      allocate (jafwk0(njaf), afwk0(njaf))
      jafwk0(1:njaf) = jafwk(1:njaf)
      afwk0(1:njaf) = afwk(1:njaf)

      deallocate (jafwk, afwk)
      allocate (jafwk(njaf), afwk(njaf))
      jafwk(1:njaf) = jafwk0(1:njaf)
      afwk(1:njaf) = afwk0(1:njaf)

      deallocate(row, list, levptr, levptr1, jafwk0, afwk0)
c     nullify(jafwk0, afwk0, levptr)

      end subroutine xmdsfacd

      end module xmdSymFacDrop









      subroutine xmdordng(ia, ja, lorder, neq, nja, norder, ierr)

      use xmdcmn
c
c     compute ordering 
c
c     input:
c        ia(), ja()    YSMP arrays
c        nja           dim of ja()
c        neq           number of nodes
c        norder        ordering scheme
c                      = 0  natural ordering
c                      = 1  RCM ordering
c                      = 2  minimum degree
c     output:
c        lorder()      ordering vector
c                         lorder(new_number) = old_number
c        ierr          error flag
c
      integer :: ia(neq+1), ja(nja), lorder(neq), 
     [         neq, norder, nja, ierr
c
c     local variables
c
      integer :: i, nsp, iflag, ierror
      integer, dimension(:), allocatable :: iwork0, iwork1

      allocate( iwork0(neq), iwork1(neq+1), stat = ierror )
      if (ierror /= 0) stop "== not enough memory (xmdordng) =="

      ierr = 0

c  ---------------
c     get lorder
c  ---------------
      if (norder == 0) then
c
c     natural ordering
c
        do i = 1, neq
          lorder(i) = i
        enddo

      elseif (norder == 1) then
c
c     RCM ordering
c
        call genrcm(neq, nja, ia, ja,
     [              lorder, iwork0, iwork1)

c       call genrcm(neq, nja, ia, ja, lorder)

      elseif (norder == 2) then
        deallocate( iwork1 )
        nsp = 3*neq + 4*nja
        allocate( iwork1(nsp), stat = ierror )
        if (ierror /= 0) stop "== not enough memory (xmdordng) =="
c
c     use odrv code to get minimum degree ordering
c
        call odrv(ia, ja, lorder, iwork0, iwork1,
     [            neq, nja, nsp, iflag)

        if (iflag /= 0) then
          write (miunit, 100) iflag
          ierr = 3
          return
        endif
      endif

  100 format ('  error in min. degree ordering'/
     [        '    error flag',i8)

c     deallocate( iwork0, iwork1 )
      end subroutine xmdordng




      subroutine xmdRedBlack(ia, ja, lorder, icolour, RBorder,
     [                 iblackend, neq, nja, nblack, ierr, redcdsys)
c
c     red-black ordering setup - compute red-black ordering vectors
c
c     input:
c           neq      number of equations
c           ia()     YSMP array
c           lorder() ordering vector
c                      lorder(new_number) = old_number
c           nja      size of ja array
c           redcdsys = .true.      reduced system
c                    = .false.     full system
c     output:
c           nblack   number of black node
c           icolour()  > 0  -> black node number
c                    < 0  -> red   node number
c           RBorder()
c              - RBorder(1:nblack)
c                 original_node_number = RBorder(black_node_number)
c            
c              - RBorder(nblack+1:neq)
c                 original_node_number = RBorder(red_node_number) 
c
c           iblackend()   end marker for black nodes in a row
c           ierr     error flag
c     in-output:
c           ja()     YSMP array  - order of entries in a row is changed 
c
c
c     temp:
c           ilist()
c
      use xmdcmn

      implicit none
      integer :: ia(neq+1), ja(nja), lorder(neq), nblack, neq, nja, ierr
      integer :: icolour(neq), RBorder(neq), iblackend(neq)
      logical :: redcdsys
c
c     locall variables
c
      integer :: ibf, nred, ii, idf, i, iHalf, iBlack,iRed, ierror, k
      integer, allocatable, dimension(:) :: ilist
      allocate( ilist(neq) , stat = ierror )
      if (ierror /= 0) stop "== not enough memory (xmdRedBlack) =="

      ierr = 0
      nred = 0
      nblack = 0
c
c  determine red or black nodes accoring to their connections
c
c     ilist(ibk) = 0 : undetermined
c                = 1 : red node
c                = -1 : black node - neighbour of a red node
c
      if (.not.redcdsys) then    ! full system
        ilist(1:neq) = -1           ! all nodes are black
        do i  = 1, neq
          ibf = lorder(i)
          call xmdBlackCounter(icolour, RBorder, neq, nblack, ibf)
        enddo
      else                  ! reduced system
        ilist(1:neq) = 0            ! set all nodes are undetermined
c
c  check all nodes to find red nodes (surrounded by black nodes)
c
        do i  = 1, neq

          ibf = lorder(i)

          if (ilist(ibf) == -1) then  ! ibf is a black node
            call xmdBlackCounter(icolour, RBorder, neq, nblack, ibf)
            cycle
          endif

c  check ibf's neighbour, 

          do ii = ia(ibf), ia(ibf+1)-1
            if (ilist( ja(ii) ) == 1) then ! found a red node in neighbour. ibf should be a black node
              call xmdBlackCounter(icolour, RBorder, neq, nblack, ibf)
              cycle
            endif
          enddo

c  this is a red node!

          ilist(ibf) = 1

c    neighbouring nodes should be black

          do ii = ia(ibf), ia(ibf+1)-1
            if (ja(ii)  /=  ibf) ilist(ja(ii)) = -1
          enddo

c    count red node

          nred = nred + 1
          icolour(ibf) = -nred  ! negative # for red node
          RBorder( neq +1 -nred) = ibf  ! store red node info into RBorder

        enddo

      endif


      if (nblack + nred  /=  neq) then
        ierr = 3
        write (miunit,100)
        return
      endif

  100 format ('  error in subroutine xmdRedBlack'/
     [        '   sum of n_black and n_red <> n')
  200 format ('  need more space in integer temp. array')
  300 format ('  error in red-black ordering'/
     [        '    total number of nodes',i8/
     [        '    number of red node',i8/
     [        '    number of black node',i8)






c
c     rearrange column numbers 
c
      iHalf = neq/2
      do i = 1, neq

        iBlack = 0
        iRed = 0
        do ii = ia(i)+1, ia(i+1)-1  ! check neighbours

          ibf = ja(ii)

          if (icolour( ibf ) > 0) then        ! black node
            iBlack = iBlack + 1
            if (iBlack > iHalf) then
              ierr = 2
              write (miunit,200)
              return
            endif
            ilist(iBlack) = ibf
          else
            iRed = iRed + 1
            if (iRed > iHalf) then
              ierr = 2
              write (miunit,200)
              return
            endif
            ilist(iHalf + iRed) = ibf
          endif
        enddo


c     check number of entries

        k = ia(i+1) - ia(i) - 1
        if (k /= iRed+iBlack) then
          ierr = 3
          write (miunit,300) k, iRed, iBlack
          return
        endif

c     sort entries

        call xmdshell(ilist(1), iBlack)
        call xmdshell(ilist(iHalf+1), iRed)

        k = ia(i)
        do ii = 1, iBlack
          k = k + 1
          ja(k) = ilist(ii)
        enddo

        iblackend(i) = k  ! end marker for black nodes

        do ii = 1, iRed
          k = k + 1
          ja(k) = ilist(iHalf+ii)
        enddo
c
      enddo

c     deallocate( ilist )
      end subroutine xmdRedBlack







      subroutine xmdBlackCounter(icolour, RBorder, neq, nblack, ibf)
c
c     count black node
c
      implicit none
      integer :: icolour(neq), RBorder(neq), neq, nblack, ibf

      nblack = nblack+1
      icolour(ibf) = nblack   ! positive # for black node
      RBorder(nblack) = ibf

      end subroutine xmdBlackCounter



      subroutine xmdshell(x,n)
c
c        shell sort
c
      implicit none
      integer n, x(n)
c
c
c       return x(i), i=1,..n
c         sorted in increasing value
c
c       local vars
c
      integer m, maxi, k, j, itemp, xert
c
      m = n
   10 continue
      m = m/2
      if( m  ==  0) return
      maxi = n - m
      do 20 j=1, maxi
        do k = j, 1, -m
          xert = x(k+m)
          if( xert  >=  x(k) ) go to 20
          itemp = x(k+m)
          x(k+m) = x(k)
          x(k) = itemp
        enddo
   20 continue
      goto 10
      end subroutine xmdshell






      subroutine xmdcgstb(a, b, x, af, soln,
     [                    ctol, rrctol, 
     [                    ia, ja, iaf, jaf, idiagf, RBorder, nblack,
     [                    nred, n, nja, njaf, nitmax, north, ierr)


      implicit none
      double precision :: a(nja), b(n), af(njaf), x(n), soln(nblack),
     [                 ctol, rrctol


      integer :: ia(n+1), ja(nja), iaf(nblack+1), jaf(njaf),
     [        idiagf(nblack), RBorder(n), n, nja, njaf, nitmax, nblack,
     [        nred, north, ierr
c
c     local variables
c
c     work arrays
c        q()     nblack
c        qb()    nblack
c        aqb()   nblack
c        reso()  nblack
c        res()   nblack
c        t()     nblack
c        s()     nblack
c        sb()    nblack
c       total     9*nblack

      integer :: i, iter, irpnt, ierror
      double precision :: res0, omega, omegah, beta, betah, alpha,
     [                 resmax, temp1, temp2, temp, verysmall
      logical :: conv, rescal

      double precision, dimension(:), allocatable :: q, qb, aqb, reso,
     [                                             res, t, s, sb

      allocate( q(nblack), qb(nblack), aqb(nblack), reso(nblack),
     [          res(nblack), t(nblack), s(nblack),
     [          sb(nblack), stat = ierror )
      if (ierror /= 0) stop "== not enough memory (xmdcgstb) =="

      verysmall = 1.0d-300

      q(1:nblack) = 0.0d0
      aqb(1:nblack) = 0.0d0

      irpnt = nblack+1             ! pointer for REDorder
      if (nred < 1) irpnt = 1

c     get [a]{x} -- note: use t as tmp

      call xmdrbmtv(a, x, t, ia, ja, RBorder(1), RBorder(irpnt),
     [              n, nja, nblack, nred)

      res0 = 0.0d0
      do i = 1, nblack
        temp = b( RBorder(i) ) - t(i)
        t(i) = 0.0d0
        res(i) = temp
        reso(i) = res(i)
        res0 = res0 + temp * temp
      enddo

      res0 = dsqrt(res0)
      omegah = 1.0d0
      beta = 1.0d0
      alpha = 1.0d0

c     save x into soln and use x as temporary(n) array

c      do i = 1, nblack
c        soln(i) = x( RBorder(i) )
c      enddo

      soln(1:nblack) = x( RBorder(1:nblack) )

c
c     iteration loop
c
      do iter = 0, nitmax

        betah = 0.0d+0
        do i = 1, nblack
          betah = betah + reso(i)*res(i)
        enddo

!       omega = (betah/beta)*(omegah/alpha)
        omega = betah/(beta+sign(verysmall, beta)) *
     [          omegah/(alpha+sign(verysmall, alpha))
        beta = betah
        do i = 1, nblack
          q(i) = res(i)+omega*(q(i)-alpha*aqb(i))
        enddo

        call xmdilusl(qb, q, af, iaf, jaf, idiagf, njaf,
     [                nblack)
c
c     CGSTAB acceleration
c
        do i = 1, nblack
          x(RBorder(i)) = qb(i)
        enddo
        call xmdrbmtv(a, x, aqb, ia, ja, RBorder(1), RBorder(irpnt),
     [                n, nja, nblack, nred)

        temp1 = 0.0d+0
        do i = 1, nblack
          temp1 = temp1 + reso(i)*aqb(i)
        enddo

!       omegah = betah/temp1
        omegah = betah/(temp1+sign(verysmall, temp1))

        do i = 1, nblack
          s(i) = res(i) - omegah*aqb(i)
        enddo

        call xmdilusl(sb, s, af, iaf, jaf, idiagf, njaf, nblack)

        do i = 1, nblack
          x(RBorder(i)) = sb(i)
        enddo
        call xmdrbmtv(a, x, t, ia, ja, RBorder(1), RBorder(irpnt),
     [                n, nja, nblack, nred)

        temp1 = 0.0d+0
        temp2 = 0.0d+0
        do i = 1, nblack
          temp1 = temp1 + t(i)*t(i)
          temp2 = temp2 + t(i)*s(i)
        enddo

!       alpha = temp2/temp1
        alpha = temp2/(temp1+sign(verysmall, temp1))

        resmax = 0.0d+0
        temp1 = 0.0d+0
        conv = .false.

        do i = 1, nblack
          soln(i) = soln(i) + omegah*qb(i) + alpha*sb(i)
          res(i) = s(i) - alpha*t(i)
          temp1 = dmax1( dabs(omegah*qb(i)), dabs(qb(i)),
     [                   dabs(alpha*sb(i)), dabs(sb(i)),
     [                   dabs(omegah*qb(i) + alpha*sb(i)), temp1 )
          resmax = resmax + res(i)*res(i)
        enddo

        resmax = dsqrt(resmax)

        conv = temp1 < ctol
        conv = conv.or.(resmax < rrctol*res0)

        if (conv) then
          nitmax = iter+1
c         deallocate( q, qb, aqb, reso, res, t, s, sb )
          return
        endif
c
c     explicit calculation of residual at every ``north'' step
c
        rescal = mod(iter+1,north) == 0

        if (rescal) then
          do i = 1, nblack
            x(RBorder(i)) = soln(i)
          enddo
          call xmdrbmtv(a, x, qb, ia, ja, RBorder(1), RBorder(irpnt),
     [                  n, nja, nblack, nred)
          do i = 1, nblack
            res(i) = b( RBorder(i) ) - qb(i)
          enddo 
        endif

      enddo

c     no convergence

      ierr = -1
c     deallocate( q, qb, aqb, reso, res, t, s, sb )

      end subroutine xmdcgstb



      subroutine xmdcnjgd(a, b, x, af, soln,
     [                    ctol, rrctol, 
     [                    ia, ja, iaf, jaf, idiagf, RBorder, nblack,
     [                    nred, n, nja, njaf, nitmax, ierr)


      implicit none
      double precision :: a(nja), b(n), af(njaf), x(n), soln(nblack),
     [                    ctol, rrctol


      integer :: ia(n+1), ja(nja), iaf(nblack+1), jaf(njaf),
     [           idiagf(nblack), RBorder(n), n, nja, njaf, nitmax,
     [           nblack, nred, ierr
c
c     local variables
c
      integer :: i, iter, irpnt, ierror
      double precision :: temp, res0, rvkm, rvk, aconj, omega, resmax
      logical conv
c
c     work arrays
c      avk()      nblack
c      q()        nblack
c      aq()       nblack
c      res()      nblack
c      v()        nblack
c      total      5*nblack

      double precision, dimension(:), allocatable :: avk, q, aq, res, v

      allocate( avk(nblack), q(nblack), aq(nblack), res(nblack),
     [          v(nblack), stat = ierror )
      if (ierror /= 0) stop "== not enough memory (xmdcnjgd) =="


c     get [a]{x} and calculate {res} -- note: use {soln} as tmp

      irpnt = nblack+1             ! pointer for REDorder
      if (nred < 1) irpnt = 1


c     get [a]{x}      note: use soln as tmp

      call xmdrbmtv(a, x, soln, ia, ja, RBorder(1), RBorder(irpnt),
     [              n, nja, nblack, nred)

      res0 = 0.0d0
      do i = 1, nblack
        temp = b( RBorder(i) ) - soln(i)
        res(i) = temp
        res0 = res0 + temp * temp
      enddo


      res0 = dsqrt(res0)

c     save x into soln and use x as temporary(n) array

      do i = 1, nblack
        soln(i) = x( RBorder(i) )
      enddo

c
c     iteration loop
c

      rvkm = 0.0d0
      do iter = 0, nitmax

        call xmdilusl(v, res, af, iaf, jaf, idiagf, njaf, nblack)
c
c      conjugate gradient acceleration
c
        do i = 1, nblack
          x(RBorder(i)) = v(i)
        enddo
        call xmdrbmtv(a, x, avk, ia, ja, RBorder(1), RBorder(irpnt),
     [                n, nja, nblack, nred)

        rvk = 0.0d+0
        do i = 1, nblack
          rvk = rvk + res(i)*v(i)
        enddo
      
c       {q} <- {v} and {aq} <- {avk}
      
        aconj = 0.0d0
        if (iter > 0) aconj = rvk/rvkm
      
        do i = 1, nblack
          q(i) = v(i) + aconj * q(i)
          aq(i) = avk(i) + aconj * aq(i)
        enddo

        omega = 0.0d+0
        do i = 1, nblack
          omega = omega + q(i)*aq(i)
        enddo
  
        omega = rvk/omega
        rvkm = rvk
  
        resmax = 0.0d+0
        temp = 0.0d+0
        conv = .false.

        do i = 1, nblack
          soln(i) = soln(i) + omega*q(i)
          res(i) = res(i) - omega*aq(i)
          temp = dmax1(dabs(q(i)), dabs(omega*q(i)), temp)
          resmax = resmax + res(i) * res(i)
        enddo

        resmax = dsqrt(resmax)
        conv = temp < ctol
        conv = conv.or.(resmax < rrctol*res0)

        if (conv) go to 890

      enddo

c     no convergence

      ierr = -1
c     deallocate ( avk, q, aq, res, v )
      return

  890 continue
      nitmax = iter+1

c     deallocate ( avk, q, aq, res, v )
      end subroutine xmdcnjgd





      subroutine xmdgtred(a, xx, b, ia, ja, REDorder, nja, n, nred)
c
c     update red node solution using black node
c
      implicit none
      double precision :: a(nja), xx(n), b(n)
      integer :: ia(n+1), ja(nja), REDorder(nred), n, nja, nred
c
c     local variables
c
      integer :: ired, kk, iold

      do ired = 1, nred

        iold = REDorder(ired)

c     assume diagonal is 1

        xx(iold) = b(iold)

        do kk = ia(iold)+1, ia(iold+1)-1    ! skip diagonal
          xx(iold) = xx(iold) - a(kk) * xx(ja(kk))
        enddo

        xx(iold) = xx(iold) / a( ia(iold) )
      enddo

      end subroutine xmdgtred










      subroutine xmdilusl(temp, b, af, iaf, jaf, idiagf, njaf,
     [                    nblack)
c
c     input: b, af, iaf, jaf, idiagf, njaf, temp, nblack
c     output: x
c
      implicit none
      integer :: njaf, iaf(nblack+1), jaf(njaf), idiagf(nblack), nblack
      double precision :: b(nblack), af(njaf), temp(nblack)
c
c     local vars
c
      integer i, k
c
c     forward solve:  Lz=b
c        (L has unit diagonal)
c
      do i = 1, nblack
        temp(i) = b( i )
      enddo

      do i = 1, nblack
        do k = iaf(i), idiagf(i)-1
          temp(i) = temp(i) - temp( jaf(k) )*af(k)
        enddo
      enddo
c
c        back solve Ux = z
c           (U does not have unit diag)
c
      do i = nblack, 1, -1
        do k = idiagf(i)+1, iaf(i+1)-1
          temp(i) = temp(i)-temp( jaf(k) )*af(k)
        enddo
        temp(i) = temp(i)/af( idiagf(i) )
      enddo

      end subroutine xmdilusl



      subroutine xmdmrgl(i, iaf, jaf, idiagf, list, njaf, n, first,
     [                   level, levptr, levptr1, nlevptr)
c
c      level with drop tolerance
c          input: levptr   level pointer
c
      implicit none
      integer :: njaf, n, i, iaf(n+1), jaf(njaf), idiagf(n), list(n),
     [        first, level, levptr(nlevptr), levptr1(n), nlevptr
c
c     local 
c
      integer :: irow, next, oldlst, ii, nxtlst, ilevel1, ilevel2
c
c     first -> first non-zero in row i of L\U
c     i = nzero in row k of L\U
c     list(i) position to next nonzero column
c
c     list(last) = n+1
c
      next = first
   10 continue
      if (next < i) then
        oldlst = next         ! current position
        nxtlst = list(next)   ! next column number in link list
        irow = next
c
c       scan row "row" of U
c
        do ii = idiagf(irow)+1, iaf(irow+1)-1
  100     continue
          ilevel1 = levptr(ii)+levptr1(irow)+1
          ilevel2 = levptr1(jaf(ii))
          ilevel1 = min0(ilevel1,ilevel2)
          if (ilevel1 <= level) then
            if (jaf(ii) < nxtlst) then
c
c     add this index to list()
c 
              list(oldlst) = jaf(ii)
              list(jaf(ii)) = nxtlst
              oldlst = jaf(ii)
              levptr1(jaf(ii)) = ilevel1
            elseif (jaf(ii) == nxtlst) then

              levptr1(jaf(ii)) = ilevel1
              oldlst = jaf(ii)
              nxtlst = list(oldlst)

            elseif (jaf(ii) > nxtlst) then
              oldlst = nxtlst
              nxtlst = list(oldlst)
              goto 100
            endif
          endif
        enddo

        next = list(next)
c
c       get next element of l/u
c
        goto 10
      endif

      end subroutine xmdmrgl




      subroutine xmdnfctr(a, b, ia, ja, nja, n, ierr)
c
c     driver for xmdnfac
c
c     input:
c       a()            coefficient matrix
c       b()            RHS vector
c       rwork()        temp real*8 work array
c       msindx()       index array for integer work array iwork()
c       ia(), ja()     YSMP pointers for a
c       nja            size of a() and ja()
c       njaf           size of af() and jaf()
c       n              total number of nodes
c       nblack         number of black nodes
c
c     output:
c       iwork()        integer work array
c       af()           factored coefficient matrix
c       ierr           error flag
c
      use xmdcmn
      use xmdmatrix
      implicit none
      integer :: ia(n+1), ja(nja), n, nja, ierr
      double precision :: a(nja), b(n)

c
c     local variables
c
       integer :: ierror

      if (allocated(af)) deallocate (af)
      allocate( af(njaf), stat = ierror )
      if (ierror /= 0) stop "== not enough memory (xmdnfctr) =="

      call xmdnfac(ia, ja, af, n, nja, njaf, a, b, idiagf, iaf, jaf,
     [             nblack, icolour, RBorder, iblackend)

      if (ierr /= 0) then
        write (miunit,200) ierr
        return
      endif
  200 format ('  error in xmdnfctr (xmbnfac)'/'    error flag',i8)

      end subroutine xmdnfctr




      subroutine xmdnfac(ia, ja, af, n, nja, njaf, a,
     [                   b, idiagf, iaf, jaf, nblack,
     [                   icolour, RBorder, iblackend)
c
      implicit none
      integer :: n, nja, njaf, nblack
      integer :: ia(n+1), ja(nja), idiagf(nblack), iaf(nblack+1),
     [           jaf(njaf), icolour(n), RBorder(n), iblackend(n)
      double precision :: a(nja), af(njaf), b(n)
c
c
c       factors PAP^t  (re-ordered matrix)
c 
c
c     local variables
c
      integer :: iold, id, ii, idd, iii, kk, idkk, iblck, idk, ierror
      double precision :: mult, temp, epsmin

      double precision, dimension(:), allocatable :: row
      integer, dimension(:), allocatable :: list

      allocate( row(nblack), list(nblack), stat = ierror )
      if (ierror /= 0) stop "== not enough memory (xmdnfac) =="

      epsmin = 1.0d-300

      row(1:nblack) = 0.0d0
      list(1:nblack) = 0
c
c     perform red-black elimination and actural numerical factorization
c
c   /                 \ /    \   /    \           /                 \ /    \   /    \
c   |        :        | |    |   |    | eliminate |        :        | |    |   |    |
c   |   Dr   :  Arb   | | Xr |   | Br | red nodes |   Dr   :  Arb   | | Xr |   | Br |
c   |        :        | |    |   |    |  exactly  |        :        | |    |   |    |
c   |--------:--------| |----| = |----|   =>      |--------:--------| |----| = |----|
c   |        :        | |    |   |    |           |        :        | |    |   |    |
c   |  Abr   :  Abb   | | Xb |   | Bb |           |   0    :   R    | | Xb |   | C  |
c   |        :        | |    |   |    |           |        :        | |    |   |    |
c   \                 / \    /   \    /           \                 / \    /   \    /
c
c      R = Abb - Abr Dr^{-1} Arb    (1)
c      C = Bb - Abr Dr^{-1} Br      (2)
c
      blacknodes: do iblck = 1, nblack
c
c     scatter packed form of row i
c
        iold = RBorder(iblck)
        id = iblck
        row(id) = a( ia(iold) )

c       off-diagonals - black nodes

        do ii = ia(iold)+1, iblackend(iold)
          id = icolour(ja(ii))
          row(id) = row(id) + a(ii) ! Abb  in (1)
        enddo
c
c     off-diagonals - red nodes
c              perform red nodes elimination

        do ii = iblackend(iold)+1, ia(iold+1)-1

          id = ja(ii)  ! red node
          do kk = ia(id)+1, ia(id+1)-1
            idk = ja(kk)
            idkk = icolour(idk)
            row(idkk) = row(idkk) - a(ii) * a(kk) / a( ia(id) )  ! Abr Dr^{-1} Arb  in (1)
          enddo

c      modify {b}       Bb - Abr Dr^{-1} Br`    in (2)

          b(iold) = b(iold) - a(ii)*b(id) / a( ia(id) )

        enddo
c
c     set maker
c
        do ii = iaf(iblck), iaf(iblck+1)-1
          id = jaf(ii)
          list(id) = 1
        enddo
c
c     elimination
c
        do ii = iaf(iblck), idiagf(iblck)-1

          id = jaf(ii)
          mult = row(id)/af(idiagf(id))
          row(id) = mult

          do iii = idiagf(id)+1, iaf(id+1)-1
            idd = jaf(iii)
            if (list(idd) > 0) then
              row(idd) = row(idd) - mult*af(iii)
            endif
          enddo

        enddo

c     inverse of diagonal to avoid small pivots

        temp = row(iblck)
        af(idiagf(iblck)) = 1.0d0/(temp + epsmin)
c
c     gathering full length array
c
        do ii = iaf(iblck), iaf(iblck+1)-1
          id = jaf(ii)
          af(ii) = row(id)
          row(id) = 0.0d0
          list(id) = 0
        enddo

      enddo blacknodes

c     deallocate( row, list )
      end subroutine xmdnfac






      subroutine xmdorthmn(a, b, x, af, soln,
     [                  ctol, rrctol, 
     [                  ia, ja, iaf, jaf, idiagf, RBorder,
     [                  nblack, nred, n, nja, njaf, north, nitmax,
     [                  ierr)


      implicit none
      double precision :: a(nja), b(n), x(n), af(njaf),
     [                 soln(nblack), ctol, rrctol


      integer :: ia(n+1), ja(nja), iaf(nblack+1), jaf(njaf),
     [        idiagf(nblack), RBorder(n),
     [        n, nja, njaf, north, nitmax, nblack, nred, ierr
c
c     local variables
c
c     work arrays
c        avk()   nblack
c        aqaq()  north
c        q()     (north+1)*nblack
c        aq()    (north+1)*nblack
c        res()   nblack
c
c       total    2*nblack + 2*(north+1)*nblack + north


      integer :: i, k, iter, j, i2, irpnt, ipnt, ierror
      double precision :: temp, res0, aqr, alpha, omega, resmax, avkaq
      logical :: conv

      double precision, dimension(:), allocatable :: avk, aqaq, q,
     [                  aq, res

      allocate( avk(nblack), aqaq(north),
     [          q((north+1)*nblack), aq((north+1)*nblack),
     [          res(nblack), stat = ierror )
      if (ierror /= 0) stop "== not enough memory (xmdorthmn) =="

c
c     intilaize all vectors
c
      k = (north+1)*nblack
      do i = 1, k
        q(i) = 0.0d0
        aq(i) = 0.0d0
      enddo

      do i = 1, nblack
c       v(i) = 0.0d0
        avk(i) = 0.0d0
      enddo

      aqaq(1:north) = 0.0d0

      avk = 0.0d0

      irpnt = nblack+1             ! pointer for REDorder
      if (nred < 1) irpnt = 1

c     get [a]{x} and calculate {res} -- note: use {soln} as tmp

      call xmdrbmtv(a, x, soln, ia, ja, RBorder(1), RBorder(irpnt),
     [              n, nja, nblack, nred)

      res0 = 0.0d0
      do i = 1, nblack
        temp = b( RBorder(i) ) - soln(i)
        res(i) = temp
        res0 = res0 + temp * temp
      enddo

      res0 = dsqrt(res0)

c     save x into soln and use x as temporary(n) array

      do i = 1, nblack
        soln(i) = x( RBorder(i) )
      enddo

c
c     iteration loop
c

      do iter = 0, nitmax

        k = mod(iter,north) + 1
        ipnt = (k-1)*nblack

c     calculate v and substitute into q(ipnt+i)

        call xmdilusl(q(ipnt+1), res, af, iaf, jaf, idiagf, njaf,
     [                nblack)

        alpha = 0.0d0
c
c     product of a and v at k
c
        do i = 1, nblack
          x(RBorder(i)) = q(ipnt+i)
        enddo

        call xmdrbmtv(a, x, avk, ia, ja, RBorder(1), RBorder(irpnt),
     [                n, nja, nblack, nred)


c     {q} <- {v} and {aq} <- {avk}
c        note: we already did {q} <- {v}


        do i = 1, nblack
c         q(ipnt+i) = v(i)
          aq(ipnt+i) = avk(i)
        enddo

        do j = 1, k-1
          i2 = (j-1)*nblack
          avkaq = 0.0d+0
          do i = 1, nblack
            avkaq = avkaq + avk(i)*aq(i2+i)
          enddo
          alpha = -avkaq/aqaq(j)
          do i = 1, nblack
            q(ipnt+i) = q(ipnt+i) + alpha*q(i2+i)
            aq(ipnt+i) = aq(ipnt+i) + alpha*aq(i2+i)
          enddo
        enddo

        aqr = 0.0d+0
        temp = 0.0d+0
        do i = 1, nblack
          aqr = aqr + aq(ipnt+i)*res(i)
          temp = temp + aq(ipnt+i)*aq(ipnt+i)
        enddo
        aqaq(k) = temp
       
        omega = aqr/aqaq(k)

        resmax = 0.0d+0
        temp = 0.0d+0
        conv = .false.

        do i = 1, nblack
          k = RBorder(i)
          soln(i) = soln(i) + omega*q(ipnt+i)
          res(i) = res(i) - omega*aq(ipnt+i)
          temp = dmax1(dabs(omega*q(ipnt+i)), dabs(q(ipnt+i)), temp)
          resmax = resmax + res(i)*res(i)
        enddo

        resmax = dsqrt(resmax)
  
        conv = temp < ctol
        conv = conv.or.(resmax < rrctol*res0)

        if (conv) go to 890
      enddo

c     no convergence

      ierr = -1
      return

  890 continue
      nitmax = iter+1
c     deallocate ( avk, aqaq, q, aq, res )

      end subroutine xmdorthmn






      subroutine xmdrbmtv(a, xx, amltx, ia, ja, RBorder, REDorder,
     [                      n, nja, nblack, nred)

c
c      multiply [a] and {x} in reduced form
c
c      input:
c             ia(), ja()      YSMP pointers
c             a()             matrix
c             xx()            solution vector
c             n               number of unknowns
c             nblack          number of black nodes
c             nred            number of red nodes
c             nja             size of ja()
c             RBorder(1:nblack) black node pointer
c                 original_node_number = RBorder(black_node_number)
c             REDorder(1:nred)   red node pointer
c                 original_node_number = REDorder(red_node_number)
c
c      output:
c             amltx()         mulitiplication of [a]{x}in reduced system, i.e., [Abb']{Xb}
c
      implicit none
      integer :: n, nja, nblack, nred, ia(n+1), ja(nja),
     [        RBorder(nblack), REDorder(nred)
      double precision :: a(nja), amltx(nblack), xx(n)
c
c     /                 \ /    \
c     |        :        | |    |
c     |   Dr   :  Arb   | | Xr |  after elimination red nodes
c     |        :        | |    |
c     |--------:--------| |----|  Abb' = Abb_{diag} + Abb_{off_diag} - Abr * Arb
c     |        :        | |    |                    
c     |  Abr   :  Abb   | | Xb |
c     |        :        | |    |
c     \                 / \    /
c
c      let's Xr = -Dr^{-1} * Arb * Xb then
c
c      Abb' * Xb = Abb_{diag} * Xb + { Abb_{off_diag} - Abr * Dr^{-1} * Arb } Xb
c                = Abb_{diag} * Xb + ( Abb Abr )_{off_diag} * { Xr+Xb }
c
c     local variables
c
      integer iold, kk, iblck, ired

c     calculate Xr = -Dr^{-1} * Arb * Xb

      do ired = 1, nred

        iold = REDorder(ired)
        xx(iold) = 0.0d0  ! use red xx(iold) as tmp.

        do kk = ia(iold)+1, ia(iold+1)-1
          xx(iold) = xx(iold) - a(kk) * xx(ja(kk))
        enddo

        xx(iold) = xx(iold) / a( ia(iold) )

      enddo

c     calculate   Abb' * Xb
c                  = Abb_{diag} * Xb + ( Abb Abr )_{off_diag} * { Xr+Xb }

      do iblck = 1, nblack

        iold = RBorder(iblck)

c     obatain Abb_{diag} * Xb
        amltx(iblck) = xx(iold) * a( ia(iold) )

c     calculate  ( Abb Abr )_{off_diag} * { Xr+Xb } and add to the above
        do kk = ia(iold)+1, ia(iold+1)-1
          amltx(iblck) = amltx(iblck) + a(kk)* xx( ja(kk) )
        enddo

      enddo

      end subroutine xmdrbmtv




c              -------------------------------
c                   Column Number Rearrange
c              -------------------------------
c
c                      xmdrowrg.f
c                      version 1.1
c             last revision: March 29, 2011
c
c                (c) 2011   M. Ibaraki
c
c     This program rearrange the rows of a matrix so that the diagonal
c     of each row appear as the first element of each row and
c     sort row entries in increasing column order.
c
c
c
c     variable definitions:
c
c     ia,ja      YSMP ia,ja data structure
c     n          total number of unknowns
c     nja        size of ja array
c     ierr       error flag
c
c     input      ia, n, nja
c     in-output  ja
c     output     ierr
c
c     required subroutine: xmd_v1.3/xmdshell
c

      subroutine xmdrowrg(ia, ja, n, nja, ierr)

      use xmdcmn
      implicit none
      integer :: n, nja, ierr, ia(n+1), ja(nja)
c
c     local variables
c
      integer :: i, j, node, nentry
      logical :: found

      ierr = 0
      do i = 1, n
        found = .false.
        do j = ia(i), ia(i+1)-1
          if (ja(j) == i) then
            node = ja(ia(i))
            ja(ia(i)) = ja(j)
            ja(j) = node
            found = .true.
          endif
        enddo
        if (.not.found) then
          write (miunit,*) 'error in data structure!!'
          write (miunit,*) 'on the row of ',i
          write (miunit,*) 'the diagonal of this row is missing'
          ierr = 3
          return
        endif
        nentry = ia(i+1)-ia(i)-1
        call xmdshell(ja(ia(i)+1),nentry)
      enddo

      end subroutine xmdrowrg


c      -------------------------------------------------------------
c        Sparse Matrix Solver for Symmetric and UNSymmetric Matrix
c      -------------------------------------------------------------
c
c                       xmdsolv.f
c                      version 2.0
c              last revision:  Feb 22, 2011
c
c             (c)copyright 2011   M. Ibaraki
c
c     This program solves  [a] x = b  using preconditioned
c     conjugate gradient type methods.
c     current version has:
c                          conjugate gradient
c                          ORTHOMIN
c                          CGSTAB
c
c     The sparsity pattern of the matrix is stored using ia, ja
c     data structure.
c
c     This subroutine contains acceleration part only
c
c
c     variable definitions:
c
c      iacl             choice of acceleration method
c                       0 = conjugate gradient
c                       1 = ORTHOMIN
c                       2 = CGSTAB
c      a                matrix stored as linear array
c      af               matrix factors (each row of af
c                       contains a row L\U)
c                       where A = LU
c      aq               A * q^{k}
c      avk              A * v^{k}
c      b                right hand side
c      ctol             convergence tolerance
c      ia,ja            usual ia, ja arrays
c      conv             flag for convergence check
c      iaf, jaf         "ia, ja" arrays for af
c      idiagf           points to diagonal of af array,
c                       i.e. af( idiag( i) ) is the
c                       diagonal element of U, assuming
c                       A = LU, where L is unit lower triangular
c      north            number of orthogonalizaion
c
c      invord           inverse of lorder:  invord( lorder(i) ) = i
c
c      lorder           ordering vector: lorder( new_order ) = old_order
c      n                number of unknowns
c      nitmax           max numvber of iterations
c      nja              size of ja, a, arrays
c      njaf             size of jaf, af arrays
c      q                search vector
c      aqaq             product of aq and aq
c      res              residual
c      rwork            temporary n-length vector wkspace
c      v                (LU)^{-1} r^{k}
c      x                solution (original ordering)
c
c      input        : a,b,ia,ja,iaf,jaf,idiagf,af,n,nja,njaf,lorder
c                     ctol,rrctol,north
c      input-output : x,nitmax
c      temp         : avk,aqaq,q,aq,res,v,rwork
c
c
c
c     for xmdilusl:
c       input: res,af,iaf,jaf,idiagf,n,njaf,rwork,lorder
c              (rwork is a temp vector)
c       output: v  
c
c     for xmdcnjgd:
c       input: ctol,rrctol,a,v,ia,ja,nja,n,iter,invord,lorder
c       temps: avk
c       input-output: q,aq,res,x,rvkm 
c       output: conv
c
c
c
c     for xmdorthmnmn:
c       input: ctol,rrctol,a,v,ia,ja,nja,n,iter,invord,lorder,north
c       temps: avk
c       input-output: q,aq,res,x,aqaq 
c       output: conv
c

      subroutine xmdsolv(a, b, x, ctol, rrctol, ia, ja,
     [                   nja, n, north, nitmax, iacl, ierr)

      use xmdcmn
      use xmdmatrix

      implicit none
      integer :: ia(n+1), ja(nja), n, nja, north, nitmax, iacl, ierr

      double precision :: a(nja), b(n), x(n), ctol, rrctol
c
c     local variables
c
      integer :: ierror, i, nred

      double precision, dimension(:), allocatable :: soln

      allocate( soln(nblack), stat = ierror )
      if (ierror /= 0) stop "== not enough memory (xmdsolv) =="


      nred = n - nblack
c
c     conjugate gradient
c
      if (iacl == 0) then 

        call xmdcnjgd(a, b, x, af, soln,
     [                ctol, rrctol, 
     [                ia, ja, iaf, jaf, idiagf,
     [                RBorder, nblack,
     [                nred, n, nja, njaf, nitmax, ierr)
c
c     ORTHOMIN
c
      elseif (iacl == 1) then             !  required size

        call xmdorthmn(a, b, x, af, soln,
     [              ctol, rrctol, 
     [              ia, ja, iaf, jaf, idiagf,
     [              RBorder,
     [              nblack, nred, n, nja, njaf, north, nitmax,
     [              ierr)
c
c     CGSTAB
c
      elseif (iacl == 2) then         !  required size

        call xmdcgstb(a, b, x, af, soln,
     [                ctol, rrctol, 
     [                ia, ja, iaf, jaf, idiagf,
     [                RBorder, nblack,
     [                nred, n, nja, njaf, nitmax, north, ierr)

      endif

      if (ierr == -1) then
!        write (*,*) 'too many iterations!!'
        ierr = 0
      endif

c
c     scatter solution
c
c      do i = 1, nblack
c        x( RBorder(i) ) = soln(i)
c      enddo
       x( RBorder(1:nblack) ) = soln(1:nblack)

c     recover red nodes

      if (nred > 0) then
        call xmdgtred(a, x, b, ia, ja, RBorder(nblack+1), nja, n, nred)
      endif

c     deallocate ( soln )
      end subroutine xmdsolv



      subroutine xmdmrgd(a, af, row, epsrn, i, nblack, ia, iaf, jaf,
     [                   idiagf, list, RBorder, nja, njaf, n, first,
     [                   level, levptr, levptr1, nlevptr)

c
c      level with drop tolerance
c
      implicit none
      double precision :: a(nja), af(njaf), row(nblack), epsrn
      integer :: nja, njaf, n, i, nlevptr,
     [        ia(n+1), iaf(nblack+1), jaf(njaf),
     [        idiagf(nblack), list(nblack), RBorder(n),
     [        first, level, levptr(nlevptr), levptr1(nblack), nblack
c
c     local 
c
      integer :: irow, next, oldlst, ii, nxtlst, id1, id2,
     [           ilevel1, ilevel2
      double precision mult, tmp1, tmp2
c
c     first -> first non-zero in row i of L\U
c     i = nzero in row k of L\U
c     list(i) position to next nonzero column
c
c     list(last) = n+1
c
      next = first
   10 continue
      if (next < i) then
        oldlst = next         ! current position
        nxtlst = list(next)   ! next column number in link list
        irow = next
c
c       scan row "row" of U
c
        mult = row(irow)/af(idiagf(irow))
        row(irow) = mult

        do ii = idiagf(irow)+1, iaf(irow+1)-1
  100     continue
          ilevel1 = levptr(ii)+levptr1(irow)+1
          ilevel2 = levptr1(jaf(ii))
          ilevel1 = min0(ilevel1,ilevel2)
          if (ilevel1 <= level) then
            if (jaf(ii) < nxtlst) then

              id1 = ia(RBorder(i))
              id2 = ia(RBorder(jaf(ii)))
              tmp1 = dabs(mult*af(ii))
              tmp2 = epsrn * dsqrt(dabs(a(id1)*a(id2)))

              if (tmp1 > tmp2) then
c
c     add this index to list()
c 
                list(oldlst) = jaf(ii)
                list(jaf(ii)) = nxtlst
                oldlst = jaf(ii)
                levptr1(jaf(ii)) = ilevel1
                row(jaf(ii)) = row(jaf(ii)) - mult*af(ii)
              endif
            elseif (jaf(ii) == nxtlst) then

              levptr1(jaf(ii)) = ilevel1
              oldlst = jaf(ii)
              nxtlst = list(oldlst)

              row(jaf(ii)) = row(jaf(ii)) - mult*af(ii)
            elseif (jaf(ii) > nxtlst) then
              oldlst = nxtlst
              nxtlst = list(oldlst)
              goto 100
            endif
          endif
        enddo

        next = list(next)
c
c       get next element of l/u
c
        goto 10
      endif

      end subroutine xmdmrgd











      subroutine xmdprpc(ia, ja, nja, nn, norder, ierr, redsys)

c
c     assign pointers for arrays and ordering vector
c
c     input:
c       ia(), ja()     YSMP pointers for a
c       iaf(), jaf()   YSMP pointers for af
c       liwrk          size of integer temp work array iwork()
c       nja            size of ja()
c       njaf           size of jaf()
c       nn             total number of nodes
c       norder         switch for ordering scheme
c                        = 0; original ordering
c                        = 1; RCM ordering
c                        = 2; Minimum Degree ordering
c       redsys         flag for reduced system
c                        = .true.  perform red-black ordering
c                        = .false. full system treatment
c     output:
c       lorder()       ordering vector: lorder( new_order ) = old_order
c       nblack         number of black nodes
c       ierr           error flag
c
      use xmdcmn
      use xmdmatrix

      implicit none
      integer :: ia(nn+1), ja(nja), nja, nn, norder, ierr
      logical :: redsys
c
c     local variables
c
      integer ierror


      allocate(icolour(nn), RBorder(nn), iblackend(nn), lorder(nn),
     [         stat = ierror )
      if (ierror /= 0) stop "== not enough memory (xmdprpc) =="


c     row number rearrange

      call xmdrowrg(ia, ja, nn, nja, ierr)

      if (ierr /= 0) then
        write (miunit,150) ierr
        return
      endif
c
c     set ordering
c
      call xmdordng(ia, ja, lorder, nn, nja, norder, ierr)
      if (ierr /= 0) then
        write (miunit,100) ierr
        return
      endif
c
c     get red-black ordering
c

      call xmdRedBlack(ia, ja, lorder, icolour, RBorder,
     [               iblackend, nn, nja, nblack, ierr, redsys)

      allocate(iaf(nblack+1), idiagf(nblack), stat = ierror )
      if (ierror /= 0) stop "== not enough memory (xmdprpc - af) =="

  100 format ('  error in xmdprpc (xmdordng)'/'    error flag',i8)
  150 format ('  error in xmdprpc (xmdrowrg)'/'    error flag',i8)

      end subroutine xmdprpc




      subroutine xmdprecl(ia, ja, level, nja, nn, ierr)
c
c     assign arrays (ordering, and red-black ordering)
c     and perform symbolic factorization
c
c
c     input:
c       ia(), ja()     YSMP pointers for a
c       iaf(), jaf()   YSMP pointers for af
c       idiagf()       diagonal pointer for af
c       iwork()        integer temp work array
c       level          level of ILU
c       liwrk          size of integer temp work array iwork()
c       nblack         number of black nodes
c       nja            size of ja()
c       njaf           size of jaf()
c       njaf           size of af()
c       nn             total number of nodes
c       redsys         flag for reduced system
c                        = .true.  perform red-black ordering
c                        = .false. full system treatment
c     output:
c       ierr           error flag
c

      use xmdcmn
      use xmdSymFacLev
      use xmdmatrix

      implicit none
      integer :: ia(nn+1), ja(nja), level, nja, nn
c
c     local variable
c
      integer :: ierr
      ierr = 0

      call xmdsfacl(iaf, jaf, idiagf, ia, ja,
     [              icolour, RBorder, iblackend,
     [              nn, nja, njaf, level, nblack, ierr)


      if (ierr /= 0) then
        write (miunit,300) ierr
        return
      endif

  300 format ('  error in xmdprecl (xmdsfacl)'/'    error flag',i8)

      end subroutine xmdprecl







      subroutine xmdprecd(a, b, epsrn, ia, ja, nja, nn, level, ierr)
c
c
c     assign arrays (ordering, and red-black ordering)
c     and perform symbolic factorization
c
c
c     input:
c       af()           factored coefficient matrix
c                      -- note: this array is used as integer work array
c                               with the size of 2*njaf
c                      for xmdsfac
c       ia(), ja()     YSMP pointers for a
c       iaf(), jaf()   YSMP pointers for af
c       idiagf()       diagonal pointer for af
c       iwork()        integer temp work array
c       level          level of ILU
c       liwrk          size of integer temp work array iwork()
c       nblack         number of black nodes
c       nja            size of ja()
c       njaf           size of jaf()
c       njaf           size of af()
c       nn             total number of nodes
c     output:
c       ierr           error flag
c
      use xmdcmn
      use xmdmatrix
      use xmdSymFacDrop

      implicit none
      double precision :: a(nja), b(nn), epsrn
      integer :: ia(nn+1), ja(nja), level, nja, nn
c
c     local variables
c
      integer :: ierr
      ierr = 0
c
c   jaf, af will be calculated
c
        call xmdsfacd(a, b, af, epsrn, iaf, jaf, idiagf, ia, ja,
     [                icolour, RBorder, iblackend, nn, nja, njaf, level,
     [                nblack, ierr)

c     print *, 'jaf size ', size(jaf)

      if (ierr /= 0) then
        write (miunit,300) ierr
        return
      endif

  300 format ('  error in xmdprecd (xmdsfacd)'/'    error flag',i8)

      end subroutine xmdprecd





      subroutine xmdcheck(ia, ja, n, nja, ierr)

c     check the sizes of arrays
c
      use xmdcmn
      use xmdmatrix

      implicit none
      integer :: ia(n+1), ja(nja), n, nja, ierr
c
c     local variables
c
      integer :: i, iold, id, ii, idd, kk, idkk, iblck, idk, ibk, k

      ierr = 0
c
c     check ia, ja data structure
c
c
c     check diag entry in the first column
c
      do ibk = 1, n
        i = ja( ia(ibk) )
        if (i /= ibk) then
          write (miunit,110) ibk, i
          ierr = 50
          return
        endif
      enddo

  110 format ('error in xmdcheck'/
     [        '  diagonal entry is not placed at the first column'/
     [        '  row number:', i5/
     [        '  column number in the first entry:', i5)

c
c     check symmetric structure
c
      do ibk = 1, n
        do k = ia(ibk), ia(ibk+1)-1

          id = ja(k)
          do kk = ia(id), ia(id+1)-1
            if (ja(kk) == ibk) goto 101
          enddo
          write (miunit,105) ibk, id
          ierr = -2

  101     continue
        enddo
      enddo

  105 format ('warning in xmdcheck'/
     [        '  ia,ja data structure shows non-symmetric structure'/
     [        '  row number:', i5/
     [        '  column number which does not have symmetric part:',
     [        i5)
c
c     check red-black ordering connectoins
c
      blacknodes: do iblck = 1, nblack
        iold = RBorder(iblck)

        offdiagonals: do ii = ia(iold)+1, ia(iold+1)-1
          id = ja(ii)
          idd = icolour(id)
          if (idd < 0) then  !  red node check
            do kk = ia(id)+1, ia(id+1)-1
              idk = ja(kk)
              idkk = icolour(idk)
              if (idkk < 0) then
                write (miunit,200) id, idk, idkk ; ierr = 3
                return
              endif
            enddo
          endif
        enddo offdiagonals

      enddo blacknodes

  200 format ('error in xmdcheck'/
     [        '  error in red-black ordering'/
     [        '  red node should have black node connections only'/
     [        '  row number:', i5/
     [        '  column number:', i5/
     [        '  column number attribute:',i5)

      end subroutine xmdcheck










c----- subroutine genrcm
c***************************************************************
c***************************************************************
c********   genrcm ..... general Reverse Cuthill Mckee   *******
c***************************************************************
c***************************************************************
c
c     purpose - genrcm finds the reverse cuthill-mckee
c        ordering for a general graph. for each connected
c        component in the graph, genrcm obtains the ordering
c        by calling the subroutine rcm.
c
c     input parameters -
c        neqns - number of equations
c        (xadj, adjncy) - array pair containing the adjacency
c               structure of the graph of the matrix.
c
c     output parameter -
c        perm - vector that contains the rcm ordering.
c
c     working parameters -
c        mask - is used to mark variables that have been
c               numbered during the ordering process. it is
c               initialized to 1, and set to zero as each node
c               is numbered.
c        xls - the index vector for a level structure.  the
c               level structure is stored in the currently
c               unused spaces in the permutation vector perm.
c
c     program subroutines -
c        fnroot, rcm.
c
c***************************************************************
c
      subroutine  genrcm ( neqns, nja, xadj, adjncy, perm, 
     1                     mask, xls )
c
c***************************************************************
c
         integer nja, lperm
         integer adjncy(nja), mask(neqns), perm(neqns), 
     1           xls(neqns+1)
         integer xadj(neqns+1), ccsize, i, neqns, nlvl,
     1           num, root
c
c***************************************************************
c
         do 100 i = 1, neqns
            mask(i) = 1
  100    continue
         num = 1
         do 200 i = 1, neqns
c           ---------------------------------------
c           for each masked connected component ...
c           ---------------------------------------
            if (mask(i) .eq. 0) go to 200
               root = i
c              -----------------------------------------
c              first find a pseudo-peripheral node root.
c              note that the level structure found by
c              fnroot is stored starting at perm(num).
c              then rcm is called to order the component
c              using root as the starting node.
c              -----------------------------------------
cmi
               lperm = neqns - num +1
cmi
               call  fnroot ( lperm, neqns, nja, root, xadj, adjncy,
     1                        mask, nlvl, xls, perm(num) )
               call     rcm ( lperm, neqns, nja, root, xadj, adjncy,
     1                        mask, perm(num), ccsize, xls )
               num = num + ccsize
               if (num .gt. neqns) return
  200    continue
         return
      end
c----- subroutine fnroot
c***************************************************************
c***************************************************************
c*******     fnroot ..... find pseudo-peripheral node    *******
c***************************************************************
c***************************************************************
c
c    purpose - fnroot implements a modified version of the
c       scheme by gibbs, poole, and stockmeyer to find pseudo-
c       peripheral nodes.  it determines such a node for the
c       section subgraph specified by mask and root.
c
c    input parameters -
c       (xadj, adjncy) - adjacency structure pair for the graph.
c       mask - specifies a section subgraph. nodes for which
c              mask is zero are ignored by fnroot.
c
c    updated parameter -
c       root - on input, it (along with mask) defines the
c              component for which a pseudo-peripheral node is
c              to be found. on output, it is the node obtained.
c
c    output parameters -
c       nlvl - is the number of levels in the level structure
c              rooted at the node root.
c       (xls,ls) - the level structure array pair containing
c                  the level structure found.
c
c    program subroutines -
c       rootls.
c
c***************************************************************
c
      subroutine  fnroot ( lls, neqns, nja, root, xadj, adjncy, mask,
     1                     nlvl, xls, ls )
c
c***************************************************************
c
         integer neqns, nja, lls
         integer adjncy(nja), ls(lls), mask(neqns), xls(neqns+1)
         integer xadj(neqns+1), ccsize, j, jstrt, k, kstop, kstrt,
     1           mindeg, nabor, ndeg, nlvl, node, nunlvl,
     1           root
c
c***************************************************************
c
c        ---------------------------------------------
c        determine the level structure rooted at root.
c        ---------------------------------------------
         call  rootls ( lls, neqns, nja,
     1                  root, xadj, adjncy, mask, nlvl, xls, ls )
         ccsize = xls(nlvl+1) - 1
         if ( nlvl .eq. 1 .or. nlvl .eq. ccsize ) return
c        ----------------------------------------------------
c        pick a node with minimum degree from the last level.
c        ----------------------------------------------------
  100    jstrt = xls(nlvl)
         mindeg = ccsize
         root = ls(jstrt)
         if ( ccsize .eq. jstrt )  go to 400
            do 300 j = jstrt, ccsize
               node = ls(j)
               ndeg = 0
               kstrt = xadj(node)
               kstop = xadj(node+1) - 1
               do 200 k = kstrt, kstop
                  nabor = adjncy(k)
                  if ( mask(nabor) .gt. 0 )  ndeg = ndeg + 1
  200          continue
               if ( ndeg .ge. mindeg ) go to 300
                  root = node
                  mindeg = ndeg
  300       continue
c        ----------------------------------------
c        and generate its rooted level structure.
c        ----------------------------------------
  400    call  rootls ( lls, neqns, nja,
     1                 root, xadj, adjncy, mask, nunlvl, xls, ls )
         if (nunlvl .le. nlvl)  return
            nlvl = nunlvl
            if ( nlvl .lt. ccsize )  go to 100
            return
      end
c----- subroutine rcm
c***************************************************************
c***************************************************************
c********     rcm ..... reverse cuthill-mckee ordering   *******
c***************************************************************
c***************************************************************
c
c     purpose - rcm numbers a connected component specified by
c        mask and root, using the rcm algorithm.
c        the numbering is to be started at the node root.
c
c     input parameters -
c        root - is the node that defines the connected
c               component and it is used as the starting
c               node for the rcm ordering.
c        (xadj, adjncy) - adjacency structure pair for
c               the graph.
c
c     updated parameters -
c        mask - only those nodes with nonzero input mask
c               values are considered by the routine.  the
c               nodes numbered by rcm will have their
c               mask values set to zero.
c
c     output parameters -
c        perm - will contain the rcm ordering.
c        ccsize - is the size of the connected component
c               that has been numbered by rcm.
c
c     working parameter -
c        deg - is a temporary vector used to hold the degree
c               of the nodes in the section graph specified
c               by mask and root.
c
c     program subroutines -
c        degree.
c
c***************************************************************
c
      subroutine  rcm ( llperm, neqns, nja, root, xadj, adjncy, 
     1                  mask, perm, ccsize, deg )
c
c***************************************************************
c
         integer neqns, nja, llperm
         integer adjncy(nja), deg(neqns), mask(neqns), 
     1           perm(llperm)
         integer xadj(neqns+1), ccsize, fnbr, i, j, jstop,
     1           jstrt, k, l, lbegin, lnbr, lperm,
     1           lvlend, nbr, node, root
c
c***************************************************************
c
c        -------------------------------------
c        find the degrees of the nodes in the
c        component specified by mask and root.
c        -------------------------------------
         call  degree ( llperm, neqns, nja,
     1                  root, xadj, adjncy, mask, deg,
     1                  ccsize, perm )
         mask(root) = 0
         if ( ccsize .le. 1 ) return
         lvlend = 0
         lnbr = 1
c        --------------------------------------------
c        lbegin and lvlend point to the beginning and
c        the end of the current level respectively.
c        --------------------------------------------
  100    lbegin = lvlend + 1
         lvlend = lnbr
         do 600 i = lbegin, lvlend
c           ----------------------------------
c           for each node in current level ...
c           ----------------------------------
            node = perm(i)
            jstrt = xadj(node)
            jstop = xadj(node+1) - 1
c           ------------------------------------------------
c           find the unnumbered neighbors of node.
c           fnbr and lnbr point to the first and last
c           unnumbered neighbors respectively of the current
c           node in perm.
c           ------------------------------------------------
            fnbr = lnbr + 1
            do 200 j = jstrt, jstop
               nbr = adjncy(j)
               if ( mask(nbr) .eq. 0 )  go to 200
                  lnbr = lnbr + 1
                  mask(nbr) = 0
                  perm(lnbr) = nbr
  200       continue
            if ( fnbr .ge. lnbr )  go to 600
c              ------------------------------------------
c              sort the neighbors of node in increasing
c              order by degree. linear insertion is used.
c              ------------------------------------------
               k = fnbr
  300          l = k
                  k = k + 1
                  nbr = perm(k)
  400             if ( l .lt. fnbr )  go to 500
                     lperm = perm(l)
                     if ( deg(lperm) .le. deg(nbr) )  go to 500
                        perm(l+1) = lperm
                        l = l - 1
                        go to 400
  500             perm(l+1) = nbr
                  if ( k .lt. lnbr )  go to 300
  600    continue
         if (lnbr .gt. lvlend) go to 100
c        ---------------------------------------
c        we now have the cuthill mckee ordering.
c        reverse it below ...
c        ---------------------------------------
         k = ccsize/2
         l = ccsize
         do 700 i = 1, k
            lperm = perm(l)
            perm(l) = perm(i)
            perm(i) = lperm
            l = l - 1
  700    continue
         return
      end
c----- subroutine degree
c***************************************************************
c***************************************************************
c********     degree ..... degree in masked component   ********
c***************************************************************
c***************************************************************
c
c     purpose - this routine computes the degrees of the nodes
c        in the connected component specified by mask and root.
c        nodes for which mask is zero are ignored.
c
c     input parameter -
c        root - is the input node that defines the component.
c        (xadj, adjncy) - adjacency structure pair.
c        mask - specifies a section subgraph.
c
c     output parameters -
c        deg - array containing the degrees of the nodes in
c              the component.
c        ccsize-size of the component specifed by mask and root
c
c     working parameter -
c        ls - a temporary vector used to store the nodes of the
c               component level by level.
c
c***************************************************************
c
      subroutine  degree ( lls, neqns, nja, root, xadj, adjncy, mask,
     1                     deg, ccsize, ls )
c
c***************************************************************
c
         integer neqns, nja, lls
         integer adjncy(nja), deg(neqns), ls(lls), mask(neqns)
         integer xadj(neqns+1), ccsize, i, ideg, j, jstop, jstrt,
     1           lbegin, lvlend, lvsize, nbr, node, root
c
c***************************************************************
c
c        -------------------------------------------------
c        initialization ...
c        the array xadj is used as a temporary marker to
c        indicate which nodes have been considered so far.
c        -------------------------------------------------
         ls(1) = root
         xadj(root) = -xadj(root)
         lvlend = 0
         ccsize = 1
c        -----------------------------------------------------
c        lbegin is the pointer to the beginning of the current
c        level, and lvlend points to the end of this level.
c        -----------------------------------------------------
  100    lbegin = lvlend + 1
         lvlend = ccsize
c        -----------------------------------------------
c        find the degrees of nodes in the current level,
c        and at the same time, generate the next level.
c        -----------------------------------------------
         do 400 i = lbegin, lvlend
            node = ls(i)
            jstrt = -xadj(node)
            jstop = iabs(xadj(node + 1)) - 1
            ideg = 0
            if ( jstop .lt. jstrt ) go to 300
               do 200 j = jstrt, jstop
                  nbr = adjncy(j)
                  if ( mask(nbr) .eq. 0 )  go to  200
                     ideg = ideg + 1
                     if ( xadj(nbr) .lt. 0 ) go to 200
                        xadj(nbr) = -xadj(nbr)
                        ccsize = ccsize + 1
                        ls(ccsize) = nbr
  200          continue
  300       deg(node) = ideg
  400    continue
c        ------------------------------------------
c        compute the current level width.
c        if it is nonzero , generate another level.
c        ------------------------------------------
         lvsize = ccsize - lvlend
         if ( lvsize .gt. 0 ) go to 100
c        ------------------------------------------
c        reset xadj to its correct sign and return.
c        ------------------------------------------
         do 500 i = 1, ccsize
            node = ls(i)
            xadj(node) = -xadj(node)
  500    continue
         return
      end
c----- subroutine rootls
c***************************************************************
c***************************************************************
c********     rootls ..... rooted level structure      *********
c***************************************************************
c***************************************************************
c
c     purpose - rootls generates the level structure rooted
c        at the input node called root. only those nodes for
c        which mask is nonzero will be considered.
c
c     input parameters -
c        root - the node at which the level structure is to
c               be rooted.
c        (xadj, adjncy) - adjacency structure pair for the
c               given graph.
c        mask - is used to specify a section subgraph. nodes
c               with mask(i)=0 are ignored.
c
c     output parameters -
c        nlvl - is the number of levels in the level structure.
c        (xls, ls) - array pair for the rooted level structure.
c
c***************************************************************
c
      subroutine  rootls ( lls, neqns, nja, root, xadj, adjncy, mask,
     1                     nlvl, xls, ls )
c
c***************************************************************
c
         integer neqns, nja, lls
         integer adjncy(nja), ls(lls), mask(neqns), xls(neqns+1)
         integer xadj(neqns+1), i, j, jstop, jstrt, lbegin,
     1           ccsize, lvlend, lvsize, nbr, nlvl,
     1           node, root
c
c***************************************************************
c
c        ------------------
c        initialization ...
c        ------------------
         mask(root) = 0
         ls(1) = root
         nlvl = 0
         lvlend = 0
         ccsize = 1
c        -----------------------------------------------------
c        lbegin is the pointer to the beginning of the current
c        level, and lvlend points to the end of this level.
c        -----------------------------------------------------
  200    lbegin = lvlend + 1
         lvlend = ccsize
         nlvl = nlvl + 1
         xls(nlvl) = lbegin
c        -------------------------------------------------
c        generate the next level by finding all the masked
c        neighbors of nodes in the current level.
c        -------------------------------------------------
         do 400 i = lbegin, lvlend
            node = ls(i)
            jstrt = xadj(node)
            jstop = xadj(node + 1) - 1
            if ( jstop .lt. jstrt )  go to 400
               do 300 j = jstrt, jstop
                  nbr = adjncy(j)
                  if (mask(nbr) .eq. 0) go to 300
                     ccsize = ccsize + 1
                     ls(ccsize) = nbr
                     mask(nbr) = 0
  300          continue
  400    continue
c        ------------------------------------------
c        compute the current level width.
c        if it is nonzero, generate the next level.
c        ------------------------------------------
         lvsize = ccsize - lvlend
         if (lvsize .gt. 0 ) go to 200
c        -------------------------------------------------------
c        reset mask to one for the nodes in the level structure.
c        -------------------------------------------------------
         xls(nlvl+1) = lvlend + 1
         do 500 i = 1, ccsize
            node = ls(i)
            mask(node) = 1
  500    continue
         return
      end
















































      subroutine odrv(ia, ja, p, ip, isp, n, nja, nsp, flag)



cmi   subroutine odrv(n, ia,ja,a, p,ip, nsp,isp, path, flag)
c
c                                                                3/12/82
c***********************************************************************
c  odrv -- driver for sparse matrix reordering routines
c***********************************************************************
c
c  description
c
c    odrv finds a minimum degree ordering of the rows and columns
c    of a matrix m stored in (ia,ja,a) format (see below).  for the
c    reordered matrix, the work and storage required to perform
c    gaussian elimination is (usually) significantly less.
c
c    note.. odrv and its subordinate routines have been modified to
c    compute orderings for general matrices, not necessarily having any
c    symmetry.  the miminum degree ordering is computed for the
c    structure of the symmetric matrix  m + m-transpose.
c    modifications to the original odrv module have been made in
c    the coding in subroutine mdi, and in the initial comments in
c    subroutines odrv and md.
c
c    if only the nonzero entries in the upper triangle of m are being
c    stored, then odrv symmetrically reorders (ia,ja,a), (optionally)
c    with the diagonal entries placed first in each row.  this is to
c    ensure that if m(i,j) will be in the upper triangle of m with
c    respect to the new ordering, then m(i,j) is stored in row i (and
c    thus m(j,i) is not stored),  whereas if m(i,j) will be in the
c    strict lower triangle of m, then m(j,i) is stored in row j (and
c    thus m(i,j) is not stored).
c
c
c  storage of sparse matrices
c
c    the nonzero entries of the matrix m are stored row-by-row in the
c    array a.  to identify the individual nonzero entries in each row,
c    we need to know in which column each entry lies.  these column
c    indices are stored in the array ja.  i.e., if  a(k) = m(i,j),  then
c    ja(k) = j.  to identify the individual rows, we need to know where
c    each row starts.  these row pointers are stored in the array ia.
c    i.e., if m(i,j) is the first nonzero entry (stored) in the i-th row
c    and  a(k) = m(i,j),  then  ia(i) = k.  moreover, ia(n+1) points to
c    the first location following the last element in the last row.
c    thus, the number of entries in the i-th row is  ia(i+1) - ia(i),
c    the nonzero entries in the i-th row are stored consecutively in
c
c            a(ia(i)),  a(ia(i)+1),  ..., a(ia(i+1)-1),
c
c    and the corresponding column indices are stored consecutively in
c
c            ja(ia(i)), ja(ia(i)+1), ..., ja(ia(i+1)-1).
c
c    since the coefficient matrix is symmetric, only the nonzero entries
c    in the upper triangle need be stored.  for example, the matrix
c
c             ( 1  0  2  3  0 )
c             ( 0  4  0  0  0 )
c         m = ( 2  0  5  6  0 )
c             ( 3  0  6  7  8 )
c             ( 0  0  0  8  9 )
c
c    could be stored as
c
c            - 1  2  3  4  5  6  7  8  9 10 11 12 13
c         ---+--------------------------------------
c         ia - 1  4  5  8 12 14
c         ja - 1  3  4  2  1  3  4  1  3  4  5  4  5
c          a - 1  2  3  4  2  5  6  3  6  7  8  8  9
c
c    or (symmetrically) as
c
c            - 1  2  3  4  5  6  7  8  9
c         ---+--------------------------
c         ia - 1  4  5  7  9 10
c         ja - 1  3  4  2  3  4  4  5  5
c          a - 1  2  3  4  5  6  7  8  9          .
c
c
c  parameters
c
c    n    - order of the matrix
c
c    ia   - integer one-dimensional array containing pointers to delimit
c           rows in ja and a.  dimension = n+1
c
c    ja   - integer one-dimensional array containing the column indices
c           corresponding to the elements of a.  dimension = number of
c           nonzero entries in (the upper triangle of) m
c
c    a    - real one-dimensional array containing the nonzero entries in
c           (the upper triangle of) m, stored by rows.  dimension =
c           number of nonzero entries in (the upper triangle of) m
c
c    p    - integer one-dimensional array used to return the permutation
c           of the rows and columns of m corresponding to the minimum
c           degree ordering.  dimension = n
c
c    ip   - integer one-dimensional array used to return the inverse of
c           the permutation returned in p.  dimension = n
c
c    nsp  - declared dimension of the one-dimensional array isp.  nsp
c           must be at least  3n+4k,  where k is the number of nonzeroes
c           in the strict upper triangle of m
c
c    isp  - integer one-dimensional array used for working storage.
c           dimension = nsp
c
c    path - integer path specification.  values and their meanings are -
c             1  find minimum degree ordering only
c             2  find minimum degree ordering and reorder symmetrically
c                  stored matrix (used when only the nonzero entries in
c                  the upper triangle of m are being stored)
c             3  reorder symmetrically stored matrix as specified by
c                  input permutation (used when an ordering has already
c                  been determined and only the nonzero entries in the
c                  upper triangle of m are being stored)
c             4  same as 2 but put diagonal entries at start of each row
c             5  same as 3 but put diagonal entries at start of each row
c
c    flag - integer error flag.  values and their meanings are -
c               0    no errors detected
c              9n+k  insufficient storage in md
c             10n+1  insufficient storage in odrv
c             11n+1  illegal path specification
c
c
c  conversion from real to double precision
c
c    change the real declarations in odrv and sro to double precision
c    declarations.
c
c-----------------------------------------------------------------------
c
      integer  ia(n+1), ja(nja),  p(n), ip(n),  isp(nsp),  flag,
     *   v, l, head,  n, nsp, mmax, next, nja

cmi
cmi   original
cmi
cmi   integer  ia(*), ja(*),  p(*), ip(*),  isp(*),  path,  flag,
cmi  *   v, l, head,  tmp, q, n, nsp, mmax, next
cmi   double precision  a(*)
cmi   logical  dflag

      integer path
cmi
cmi   set for finding ordering only
cmi
      path = 1
cmi
c
c----initialize error flag and validate path specification
      flag = 0
      if (path.lt.1 .or. 5.lt.path)  go to 111


c
c----allocate storage and find minimum degree ordering
cmi   if ((path-1) * (path-2) * (path-4) .ne. 0)  go to 1


        mmax = (nsp-n)/2
        v    = 1
        l    = v     +  mmax
        head = l     +  mmax
        next = head  +  n
        if (mmax.lt.n)  go to 110
c
        call md(n, nja, ia,ja, mmax,isp(v),isp(l), isp(head),p,ip,
     [      isp(v), flag)
        if (flag.ne.0)  go to 100
cmi
cmic
cmic----allocate storage and symmetrically reorder matrix
cmi   1  if ((path-2) * (path-3) * (path-4) * (path-5) .ne. 0)  go to 2
cmi        tmp = (nsp+1) -      n
cmi        q   = tmp     - (ia(n+1)-1)
cmi        if (q.lt.1)  go to 110
cmic
cmi        dflag = path.eq.4 .or. path.eq.5
cmi        call sro
cmi     *     (n,  ip,  ia, ja, a,  isp(tmp),  isp(q),  dflag)
cmi
c
   2  return
c
c ** error -- error detected in md
c             flag = 9 * n + vi from routine mdi.
c
 100  return
c ** error -- insufficient storage
 110  flag = 10*n + 1
      return
c ** error -- illegal path specified
 111  flag = 11*n + 1
      return
      end



      subroutine md(n, nja, ia,ja, mmax, v,l, head,last,next,
     [              mark, flag)
c
c***********************************************************************
c  md -- minimum degree algorithm (based on element model)
c***********************************************************************
c
c  description
c
c    md finds a minimum degree ordering of the rows and columns of a
c    general sparse matrix m stored in (ia,ja,a) format.
c    when the structure of m is nonsymmetric, the ordering is that
c    obtained for the symmetric matrix  m + m-transpose.
c
c
c  additional parameters
c
c    mmax  - declared dimension of the one-dimensional arrays v and l.
c           mmax must be at least  n+2k,  where k is the number of
c           nonzeroes in the strict upper triangle of m
c
c    v    - integer one-dimensional work array.  dimension = mmax
c
c    l    - integer one-dimensional work array.  dimension = mmax
c
c    head - integer one-dimensional work array.  dimension = n
c
c    last - integer one-dimensional array used to return the permutation
c           of the rows and columns of m corresponding to the minimum
c           degree ordering.  dimension = n
c
c    next - integer one-dimensional array used to return the inverse of
c           the permutation returned in last.  dimension = n
c
c    mark - integer one-dimensional work array (may be the same as v).
c           dimension = n
c
c    flag - integer error flag.  values and their meanings are -
c             0      no errors detected
c             11n+1  insufficient storage in md
c
c
c  definitions of internal parameters
c
c    ---------+---------------------------------------------------------
c    v(s)     - value field of list entry
c    ---------+---------------------------------------------------------
c    l(s)     - link field of list entry  (0 =) end of list)
c    ---------+---------------------------------------------------------
c    l(vi)    - pointer to element list of uneliminated vertex vi
c    ---------+---------------------------------------------------------
c    l(ej)    - pointer to boundary list of active element ej
c    ---------+---------------------------------------------------------
c    head(d)  - vj =) vj head of d-list d
c             -  0 =) no vertex in d-list d
c
c
c             -                  vi uneliminated vertex
c             -          vi in ek           -       vi not in ek
c    ---------+-----------------------------+---------------------------
c    next(vi) - undefined but nonnegative   - vj =) vj next in d-list
c             -                             -  0 =) vi tail of d-list
c    ---------+-----------------------------+---------------------------
c    last(vi) - (not set until mdp)         - -d =) vi head of d-list d
c             --vk =) compute degree        - vj =) vj last in d-list
c             - ej =) vi prototype of ej    -  0 =) vi not in any d-list
c             -  0 =) do not compute degree -
c    ---------+-----------------------------+---------------------------
c    mark(vi) - mark(vk)                    - nonneg. tag .lt. mark(vk)
c
c
c             -                   vi eliminated vertex
c             -      ei active element      -           otherwise
c    ---------+-----------------------------+---------------------------
c    next(vi) - -j =) vi was j-th vertex    - -j =) vi was j-th vertex
c             -       to be eliminated      -       to be eliminated
c    ---------+-----------------------------+---------------------------
c    last(vi) -  m =) size of ei = m        - undefined
c    ---------+-----------------------------+---------------------------
c    mark(vi) - -m =) overlap count of ei   - undefined
c             -       with ek = m           -
c             - otherwise nonnegative tag   -
c             -       .lt. mark(vk)         -
c
c-----------------------------------------------------------------------
c
      integer  ia(n+1), ja(nja),  v(mmax), l(mmax),  head(n), last(n),
     [         next(n), mark(n),  flag,  tag, dmin, vk,ek, tail,
     [         n, mmax, k, nja

      equivalence  (vk,ek)
c
c----initialization
      tag = 0
      call  mdi(n, nja, ia,ja, mmax,v,l, head,last,next,
     [          mark,tag, flag)
      if (flag.ne.0)  return
c
      k = 0
      dmin = 1
c
c----while  k .lt. n  do
   1  if (k.ge.n)  go to 4
c
c------search for vertex of minimum degree
   2    if (head(dmin).gt.0)  go to 3
          dmin = dmin + 1
          go to 2
c
c------remove vertex vk of minimum degree from degree list
   3    vk = head(dmin)
        head(dmin) = next(vk)
        if (head(dmin).gt.0)  last(head(dmin)) = -dmin
c
c------number vertex vk, adjust tag, and tag vk
        k = k+1
        next(vk) = -k
        last(ek) = dmin - 1
        tag = tag + last(ek)
        mark(vk) = tag
c
c------form element ek from uneliminated neighbors of vk
        call  mdm(n, mmax, vk,tail, v,l, last,next, mark)
c
c------purge inactive elements and do mass elimination
        call  mdp(n, mmax, k,ek,tail, v,l, head,last,next, mark)
c
c------update degrees of uneliminated vertices in ek
        call  mdu(n, mmax, ek,dmin, v,l, head,last,next, mark)
c
        go to 1
c
c----generate inverse permutation from permutation
   4  do 5 k=1,n
        next(k) = -next(k)
   5    last(next(k)) = k
c
      return
      end


      subroutine mdi(n, nja, ia,ja, mmax,v,l, head,last,next,
     [               mark,tag, flag)
c
c***********************************************************************
c  mdi -- initialization
c***********************************************************************
      integer  ia(n+1), ja(nja),  v(mmax), l(mmax),  head(n), last(n),
     [         next(n),
     *   mark(n), tag,  flag,  sfs, vi,dvi, vj, n, mmax, jmin, jmax, j,
     *   lvk, kmax, k, nextvi, nja
c
c----initialize degrees, element lists, and degree lists
      do 1 vi=1,n
        mark(vi) = 1
        l(vi) = 0
   1    head(vi) = 0
      sfs = n+1
c
c----create nonzero structure
c----for each nonzero entry a(vi,vj)
      do 6 vi=1,n
        jmin = ia(vi)
        jmax = ia(vi+1) - 1
        if (jmin.gt.jmax)  go to 6
        do 5 j=jmin,jmax
          vj = ja(j)
          if (vj-vi) 2, 5, 4
c
c------if a(vi,vj) is in strict lower triangle
c------check for previous occurrence of a(vj,vi)
   2      lvk = vi
          kmax = mark(vi) - 1
          if (kmax .eq. 0) go to 4
          do 3 k=1,kmax
            lvk = l(lvk)
            if (v(lvk).eq.vj) go to 5
   3        continue
c----for unentered entries a(vi,vj)
   4        if (sfs.ge.mmax)  go to 101
c
c------enter vj in element list for vi
            mark(vi) = mark(vi) + 1
            v(sfs) = vj
            l(sfs) = l(vi)
            l(vi) = sfs
            sfs = sfs+1
c
c------enter vi in element list for vj
            mark(vj) = mark(vj) + 1
            v(sfs) = vi
            l(sfs) = l(vj)
            l(vj) = sfs
            sfs = sfs+1
   5      continue
   6    continue
c
c----create degree lists and initialize mark vector
      do 7 vi=1,n
        dvi = mark(vi)
        next(vi) = head(dvi)
        head(dvi) = vi
        last(vi) = -dvi
        nextvi = next(vi)
        if (nextvi.gt.0)  last(nextvi) = vi
   7    mark(vi) = tag
c
      return
c
c ** error-  insufficient storage
 101  flag = 9*n + vi
      return
      end



      subroutine mdm(n, mmax, vk,tail, v,l, last,next, mark)
c
c***********************************************************************
c  mdm -- form element from uneliminated neighbors of vk
c***********************************************************************
      integer  vk, tail,  v(mmax), l(mmax),   last(n), next(n),
     [         mark(n),
     *   tag, s,ls,vs,es, b,lb,vb, blp,blpmax, n, mmax
      equivalence  (vs, es)
c
c----initialize tag and list of uneliminated neighbors
      tag = mark(vk)
      tail = vk
c
c----for each vertex/element vs/es in element list of vk
      ls = l(vk)
   1  s = ls
      if (s.eq.0)  go to 5
        ls = l(s)
        vs = v(s)
        if (next(vs).lt.0)  go to 2
c
c------if vs is uneliminated vertex, then tag and append to list of
c------uneliminated neighbors
          mark(vs) = tag
          l(tail) = s
          tail = s
          go to 4
c
c------if es is active element, then ...
c--------for each vertex vb in boundary list of element es
   2      lb = l(es)
          blpmax = last(es)
          do 3 blp=1,blpmax
            b = lb
            lb = l(b)
            vb = v(b)
c
c----------if vb is untagged vertex, then tag and append to list of
c----------uneliminated neighbors
            if (mark(vb).ge.tag)  go to 3
              mark(vb) = tag
              l(tail) = b
              tail = b
   3        continue
c
c--------mark es inactive
          mark(es) = tag
c
   4    go to 1
c
c----terminate list of uneliminated neighbors
   5  l(tail) = 0
c
      return
      end


      subroutine mdp(n, mmax, k,ek,tail, v,l, head,last,next, mark)
c
c***********************************************************************
c  mdp -- purge inactive elements and do mass elimination
c***********************************************************************
      integer  ek, tail,  v(mmax), l(mmax),  head(n), last(n), next(n),
     *   mark(n),  tag, free, li,vi,lvi,evi, s,ls,es, ilp, ilpmax,
     [   k, i, n, mmax
c
c----initialize tag
      tag = mark(ek)
c
c----for each vertex vi in ek
      li = ek
      ilpmax = last(ek)
      if (ilpmax.le.0)  go to 12
      do 11 ilp=1,ilpmax
        i = li
        li = l(i)
        vi = v(li)
c
c------remove vi from degree list
        if (last(vi).eq.0)  go to 3
          if (last(vi).gt.0)  go to 1
            head(-last(vi)) = next(vi)
            go to 2
   1        next(last(vi)) = next(vi)
   2      if (next(vi).gt.0)  last(next(vi)) = last(vi)
c
c------remove inactive items from element list of vi
   3    ls = vi
   4    s = ls
        ls = l(s)
        if (ls.eq.0)  go to 6
          es = v(ls)
          if (mark(es).lt.tag)  go to 5
            free = ls
            l(s) = l(ls)
            ls = s
   5      go to 4
c
c------if vi is interior vertex, then remove from list and eliminate
   6    lvi = l(vi)
        if (lvi.ne.0)  go to 7
          l(i) = l(li)
          li = i
c
          k = k+1
          next(vi) = -k
          last(ek) = last(ek) - 1
          go to 11
c
c------else ...
c--------classify vertex vi
   7      if (l(lvi).ne.0)  go to 9
            evi = v(lvi)
            if (next(evi).ge.0)  go to 9
              if (mark(evi).lt.0)  go to 8
c
c----------if vi is prototype vertex, then mark as such, initialize
c----------overlap count for corresponding element, and move vi to end
c----------of boundary list
                last(vi) = evi
                mark(evi) = -1
                l(tail) = li
                tail = li
                l(i) = l(li)
                li = i
                go to 10
c
c----------else if vi is duplicate vertex, then mark as such and adjust
c----------overlap count for corresponding element
   8            last(vi) = 0
                mark(evi) = mark(evi) - 1
                go to 10
c
c----------else mark vi to compute degree
   9            last(vi) = -ek
c
c--------insert ek in element list of vi
  10      v(free) = ek
          l(free) = l(vi)
          l(vi) = free
  11    continue
c
c----terminate boundary list
  12  l(tail) = 0
c
      return
      end



      subroutine mdu(n, mmax, ek,dmin, v,l, head,last,next, mark)
c
c***********************************************************************
c  mdu -- update degrees of uneliminated vertices in ek
c***********************************************************************
      integer  ek, dmin,  v(mmax), l(mmax),  head(n), last(n), next(n),
     *   mark(n),  tag, vi,evi,dvi, s,vs,es, b,vb, ilp,ilpmax,
     *   blp,blpmax, i, n, mmax

      equivalence  (vs, es)
c
c----initialize tag
      tag = mark(ek) - last(ek)
c
c----for each vertex vi in ek
      i = ek
      ilpmax = last(ek)
      if (ilpmax.le.0)  go to 11
      do 10 ilp=1,ilpmax
        i = l(i)
        vi = v(i)
        if (last(vi))  1, 10, 8
c
c------if vi neither prototype nor duplicate vertex, then merge elements
c------to compute degree
   1      tag = tag + 1
          dvi = last(ek)
c
c--------for each vertex/element vs/es in element list of vi
          s = l(vi)
   2      s = l(s)
          if (s.eq.0)  go to 9
            vs = v(s)
            if (next(vs).lt.0)  go to 3
c
c----------if vs is uneliminated vertex, then tag and adjust degree
              mark(vs) = tag
              dvi = dvi + 1
              go to 5
c
c----------if es is active element, then expand
c------------check for outmatched vertex
   3          if (mark(es).lt.0)  go to 6
c
c------------for each vertex vb in es
              b = es
              blpmax = last(es)
              do 4 blp=1,blpmax
                b = l(b)
                vb = v(b)
c
c--------------if vb is untagged, then tag and adjust degree
                if (mark(vb).ge.tag)  go to 4
                  mark(vb) = tag
                  dvi = dvi + 1
   4            continue
c
   5        go to 2
c
c------else if vi is outmatched vertex, then adjust overlaps but do not
c------compute degree
   6      last(vi) = 0
          mark(es) = mark(es) - 1
   7      s = l(s)
          if (s.eq.0)  go to 10
            es = v(s)
            if (mark(es).lt.0)  mark(es) = mark(es) - 1
            go to 7
c
c------else if vi is prototype vertex, then calculate degree by
c------inclusion/exclusion and reset overlap count
   8      evi = last(vi)
          dvi = last(ek) + last(evi) + mark(evi)
          mark(evi) = 0
c
c------insert vi in appropriate degree list
   9    next(vi) = head(dvi)
        head(dvi) = vi
        last(vi) = -dvi
        if (next(vi).gt.0)  last(next(vi)) = vi
        if (dvi.lt.dmin)  dmin = dvi
c
  10    continue
c
  11  return
      end























