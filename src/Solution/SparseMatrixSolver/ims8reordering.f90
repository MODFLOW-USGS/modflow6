  MODULE IMSReorderingModule
    use KindModule, only: DP, I4B
    private
    public :: ims_genrcm, ims_odrv, ims_dperm, ims_vperm
  contains
  
      !----- subroutine ims_genrcm
      !
      !     purpose - ims_genrcm finds the reverse cuthill-mckee
      !        ordering for a general graph. for each connected
      !        component in the graph, ims_genrcm obtains the ordering
      !        by calling the subroutine ims_rcm.
      !
      !     input parameters -
      !        neqns - number of equations
      !        (xadj0, adjncy) - array pair containing the adjacency
      !               structure of the graph of the matrix.
      !
      !     output parameter -
      !        perm - vector that contains the rcm ordering.
      !
      !     working parameters -
      !        xadj - working ia of the matrix 
      !        mask - is used to mark variables that have been
      !               numbered during the ordering process. it is
      !               initialized to 1, and set to zero as each node
      !               is numbered.
      !        xls - the index vector for a level structure.  the
      !               level structure is stored in the currently
      !               unused spaces in the permutation vector perm.
      !
      !     program subroutines -
      !        ims_fnroot, ims_rcm.
      !
      !***************************************************************
      !
      subroutine ims_genrcm(neqns, nja, xadj0, adjncy, perm, mask, xls)
        !
        !***************************************************************
        !
        implicit none
    
        ! -- dummy variables
        integer(I4B), intent(in) :: neqns, nja
        integer(I4B), dimension(neqns+1), intent(in) :: xadj0
        integer(I4B), dimension(nja), intent(in) :: adjncy
        integer(I4B), dimension(neqns), intent(inout) :: perm
        integer(I4B), dimension(neqns), intent(inout) :: mask
        integer(I4B), dimension(neqns+1), intent(inout) :: xls
  
        ! -- locals
        integer(I4B) :: i
        integer(I4B) :: ccsize
        integer(I4B) :: lperm
        integer(I4B) :: nlvl
        integer(I4B) :: num
        integer(I4B) :: root
        integer(I4B), allocatable, dimension(:) :: xadj
        !
        !***************************************************************
        !
        ! allocate local storage
        allocate(xadj(neqns+1))
        !
        ! initialize mask and working xadj
        do i = 1, neqns
          mask(i) = 1
          xadj(i) = xadj0(i)
        end do
        xadj(neqns+1) = xadj0(neqns+1)

        num = 1
        louter: do i = 1, neqns
          !
          !for each masked connected component
          if (mask(i) == 0) cycle
          root = i
          !
          ! first find a pseudo-peripheral node root.
          ! note that the level structure found by
          ! ims_fnroot is stored starting at perm(num).
          ! then ims_rcm is called to order the component
          ! using root as the starting node.
          !
          ! mi
          lperm = neqns - num + 1
          ! mi
          call ims_fnroot(lperm, neqns, nja, root, xadj, adjncy, mask,          &
                          nlvl, xls, perm(num))
          call ims_rcm(lperm, neqns, nja, root, xadj, adjncy, mask,             &
                       perm(num), ccsize, xls )
          num = num + ccsize
          if (num > neqns) exit louter
        end do louter
        !
        ! allocate local storage
        deallocate(xadj)
        
        return
      end subroutine  ims_genrcm

      ! subroutine ims_fnroot
      !
      ! find pseudo-peripheral node 
      !
      !    purpose - ims_fnroot implements a modified version of the
      !       scheme by gibbs, poole, and stockmeyer to find pseudo-
      !       peripheral nodes.  it determines such a node for the
      !       section subgraph specified by mask and root.
      !
      !    input parameters -
      !       (xadj, adjncy) - adjacency structure pair for the graph.
      !       mask - specifies a section subgraph. nodes for which
      !              mask is zero are ignored by ims_fnroot.
      !
      !    updated parameter -
      !       root - on input, it (along with mask) defines the
      !              component for which a pseudo-peripheral node is
      !              to be found. on output, it is the node obtained.
      !
      !    output parameters -
      !       nlvl - is the number of levels in the level structure
      !              rooted at the node root.
      !       (xls,ls) - the level structure array pair containing
      !                  the level structure found.
      !
      !    program subroutines -
      !       ims_rootls.
      !
      !***************************************************************
      !
      subroutine ims_fnroot (lls, neqns, nja, root, xadj, adjncy, mask,         &
                             nlvl, xls, ls )
         implicit none
     
         ! -- dummy variables
         integer(I4B), intent(in) :: lls
         integer(I4B), intent(in) :: neqns
         integer(I4B), intent(in) :: nja
         integer(I4B), intent(inout) :: root
         integer(I4B), dimension(neqns+1), intent(in) :: xadj
         integer(I4B), dimension(nja), intent(in) :: adjncy
         integer(I4B), dimension(neqns), intent(inout) :: mask
         integer(I4B), intent(inout) :: nlvl
         integer(I4B), dimension(neqns+1), intent(inout) :: xls
         integer(I4B), dimension(lls), intent(inout) :: ls 
     
         ! -- local
         integer(I4B) :: ccsize
         integer(I4B) :: j
         integer(I4B) :: k
         integer(I4B) :: jstrt
         integer(I4B) :: kstrt
         integer(I4B) :: kstop
         integer(I4B) :: mindeg
         integer(I4B) :: nabor
         integer(I4B) :: ndeg
         integer(I4B) :: node
         integer(I4B) :: nunlvl
         !
         ! determine the level structure rooted at root.
         call ims_rootls(lls, neqns, nja, root, xadj, adjncy, mask,             &
                         nlvl, xls, ls)
         ccsize = xls(nlvl+1) - 1
         if ( nlvl == 1 .or. nlvl == ccsize ) return
         !
         ! pick a node with minimum degree from the last level.
    100  jstrt = xls(nlvl)
         mindeg = ccsize
         root = ls(jstrt)
         if ( ccsize == jstrt ) go to 400
         louter: do j = jstrt, ccsize
           node = ls(j)
           ndeg = 0
           kstrt = xadj(node)
           kstop = xadj(node+1) - 1
           linner: do k = kstrt, kstop
             nabor = adjncy(k)
             if (mask(nabor) > 0)  ndeg = ndeg + 1
           end do linner
           if (ndeg >= mindeg) cycle louter
           root = node
           mindeg = ndeg
         end do louter
         !
         ! and generate its rooted level structure.
    400  call ims_rootls(lls, neqns, nja, root, xadj, adjncy, mask,             &
                         nunlvl, xls, ls)
         if (nunlvl <= nlvl)  return
         nlvl = nunlvl
         if (nlvl < ccsize) go to 100
         return
      end subroutine  ims_fnroot


      ! subroutine ims_rcm
      !
      ! reverse cuthill-mckee ordering
      !
      !     purpose - rcm numbers a connected component specified by
      !        mask and root, using the rcm algorithm.
      !        the numbering is to be started at the node root.
      !
      !     input parameters -
      !        root - is the node that defines the connected
      !               component and it is used as the starting
      !               node for the rcm ordering.
      !        (xadj, adjncy) - adjacency structure pair for
      !               the graph.
      !
      !     updated parameters -
      !        mask - only those nodes with nonzero input mask
      !               values are considered by the routine.  the
      !               nodes numbered by rcm will have their
      !               mask values set to zero.
      !
      !     output parameters -
      !        perm - will contain the rcm ordering.
      !        ccsize - is the size of the connected component
      !               that has been numbered by rcm.
      !
      !     working parameter -
      !        deg - is a temporary vector used to hold the degree
      !               of the nodes in the section graph specified
      !               by mask and root.
      !
      !     program subroutines -
      !        ims_degree.
      !
      !***************************************************************
      !
      subroutine ims_rcm(llperm, neqns, nja, root, xadj, adjncy,                &
                         mask, perm, ccsize, deg)
      !
         implicit none
         
         ! -- dummy variables
         integer(I4B), intent(in) :: llperm
         integer(I4B), intent(in) :: neqns
         integer(I4B), intent(in) :: nja
         integer(I4B), intent(in) :: root
         integer(I4B), dimension(neqns+1), intent(inout) :: xadj
         integer(I4B), dimension(nja), intent(in) :: adjncy
         integer(I4B), dimension(neqns), intent(inout) :: mask
         integer(I4B), dimension(llperm), intent(inout) :: perm
         integer(I4B), intent(inout) :: ccsize
         integer(I4B), dimension(neqns), intent(inout) :: deg
         
         ! -- local
         integer(I4B) :: fnbr
         integer(I4B) :: i
         integer(I4B) :: j
         integer(I4B) :: jstop
         integer(I4B) :: jstrt
         integer(I4B) :: k
         integer(I4B) :: l
         integer(I4B) :: lbegin
         integer(I4B) :: lnbr
         integer(I4B) :: lperm
         integer(I4B) :: lvlend
         integer(I4B) :: nbr
         integer(I4B) :: node
         ! code
         ! find the degrees of the nodes in the
         ! component specified by mask and root.
         call ims_degree(llperm, neqns, nja, root, xadj, adjncy, mask,          &
                         deg, ccsize, perm)
         mask(root) = 0
         if (ccsize <= 1) return
         lvlend = 0
         lnbr = 1
         !
         ! lbegin and lvlend point to the beginning and
         ! the end of the current level respectively.
100      lbegin = lvlend + 1
         lvlend = lnbr
         lbegend: do i = lbegin, lvlend
           !
           ! for each node in current level ...
           node = perm(i)
           jstrt = xadj(node)
           jstop = xadj(node+1) - 1
           !
           ! find the unnumbered neighbors of node.
           ! fnbr and lnbr point to the first and last
           ! unnumbered neighbors respectively of the current
           ! node in perm.
           fnbr = lnbr + 1
           lunn: do j = jstrt, jstop
              nbr = adjncy(j)
              if (mask(nbr) == 0)  cycle lunn
              lnbr = lnbr + 1
              mask(nbr) = 0
              perm(lnbr) = nbr
           end do lunn
           if (fnbr >= lnbr)  cycle lbegend
           !
           ! sort the neighbors of node in increasing
           ! order by degree. linear insertion is used.
           k = fnbr
300        l = k
           k = k + 1
           nbr = perm(k)
400        if (l < fnbr)  go to 500
           lperm = perm(l)
           if (deg(lperm) <= deg(nbr))  go to 500
           perm(l+1) = lperm
           l = l - 1
           go to 400
500        perm(l+1) = nbr
           if (k < lnbr)  go to 300
         end do lbegend
         if (lnbr > lvlend) go to 100
         !
         ! we now have the cuthill mckee ordering.
         ! reverse it below ...
         k = ccsize/2
         l = ccsize
         do i = 1, k
           lperm = perm(l)
           perm(l) = perm(i)
           perm(i) = lperm
           l = l - 1
         end do
         return
      end subroutine  ims_rcm


      !----- subroutine ims_degree
      ! degree in masked component   ********
      !
      !     purpose - this routine computes the degrees of the nodes
      !        in the connected component specified by mask and root.
      !        nodes for which mask is zero are ignored.
      !
      !     input parameter -
      !        root - is the input node that defines the component.
      !        (xadj, adjncy) - adjacency structure pair.
      !        mask - specifies a section subgraph.
      !
      !     output parameters -
      !        deg - array containing the degrees of the nodes in
      !              the component.
      !        ccsize-size of the component specified by mask and root
      !
      !     working parameter -
      !        ls - a temporary vector used to store the nodes of the
      !               component level by level.
      !
      !***************************************************************
      !
      subroutine ims_degree(lls, neqns, nja, root, xadj, adjncy, mask,          &
                            deg, ccsize, ls)
      !
      !***************************************************************
      !
        implicit none     
      
        ! -- dummy variables
        integer(I4B), intent(in) :: lls
        integer(I4B), intent(in) :: neqns
        integer(I4B), intent(in) :: nja
        integer(I4B), intent(in) :: root
        integer(I4B), dimension(neqns+1), intent(inout) :: xadj
        integer(I4B), dimension(nja), intent(in) :: adjncy
        integer(I4B), dimension(neqns), intent(in) :: mask
        integer(I4B), dimension(neqns), intent(inout) :: deg
        integer(I4B), intent(inout) :: ccsize
        integer(I4B), dimension(lls), intent(inout) :: ls
         
        ! -- local
        integer(I4B) :: i
        integer(I4B) :: ideg
        integer(I4B) :: j
        integer(I4B) :: jstop
        integer(I4B) :: jstrt
        integer(I4B) :: lbegin
        integer(I4B) :: lvlend
        integer(I4B) :: lvsize
        integer(I4B) :: nbr
        integer(I4B) :: node
        
        ! code
        !
        ! initialization ...
        ! the array xadj is used as a temporary marker to
        ! indicate which nodes have been considered so far.
        ls(1) = root
        xadj(root) = -xadj(root)
        lvlend = 0
        ccsize = 1
        !
        ! lbegin is the pointer to the beginning of the current
        ! level, and lvlend points to the end of this level.
100     lbegin = lvlend + 1
        lvlend = ccsize
        ! 
        ! find the degrees of nodes in the current level,
        ! and at the same time, generate the next level.
        louter: do i = lbegin, lvlend
          node = ls(i)
          jstrt = -xadj(node)
          jstop = iabs(xadj(node + 1)) - 1
          ideg = 0
          if (jstop < jstrt) go to 300
          linner: do j = jstrt, jstop
            nbr = adjncy(j)
            if (mask(nbr) == 0)  cycle linner
            ideg = ideg + 1
            if (xadj(nbr) < 0) cycle linner
            xadj(nbr) = -xadj(nbr)
            ccsize = ccsize + 1
            ls(ccsize) = nbr
          end do linner
300       deg(node) = ideg
        end do louter
        ! 
        ! compute the current level width.
        ! if it is nonzero , generate another level.
        lvsize = ccsize - lvlend
        if (lvsize > 0) go to 100
        !
        ! reset xadj to its correct sign and return.
        do i = 1, ccsize
          node = ls(i)
          xadj(node) = -xadj(node)
        end do
        return
      end subroutine ims_degree


      ! subroutine ims_rootls
      !
      ! rooted level structure
      !
      !     purpose - ims_rootls generates the level structure rooted
      !        at the input node called root. only those nodes for
      !        which mask is nonzero will be considered.
      !
      !     input parameters -
      !        root - the node at which the level structure is to
      !               be rooted.
      !        (xadj, adjncy) - adjacency structure pair for the
      !               given graph.
      !        mask - is used to specify a section subgraph. nodes
      !               with mask(i)=0 are ignored.
      !
      !     output parameters -
      !        nlvl - is the number of levels in the level structure.
      !        (xls, ls) - array pair for the rooted level structure.
      !
      !***************************************************************
      !
      subroutine ims_rootls(lls, neqns, nja, root, xadj, adjncy, mask,          &
                            nlvl, xls, ls )

        implicit none
       
        ! -- dummy variables
        integer(I4B), intent(in) :: lls
        integer(I4B), intent(in) :: neqns
        integer(I4B), intent(in) :: nja
        integer(I4B), intent(in) :: root
        integer(I4B), dimension(neqns+1), intent(in) :: xadj
        integer(I4B), dimension(nja), intent(in) :: adjncy
        integer(I4B), dimension(neqns), intent(inout) :: mask
        integer(I4B), intent(inout) :: nlvl
        integer(I4B), dimension(neqns+1), intent(inout) :: xls
        integer(I4B), dimension(lls), intent(inout) :: ls
         
        ! -- local
        integer(I4B) :: i
        integer(I4B) :: j
        integer(I4B) :: jstop
        integer(I4B) :: jstrt
        integer(I4B) :: lbegin
        integer(I4B) :: ccsize
        integer(I4B) :: lvlend
        integer(I4B) :: lvsize
        integer(I4B) :: nbr
        integer(I4B) :: node
        !
        ! code
        !
        ! initialization ...
        mask(root) = 0
        ls(1) = root
        nlvl = 0
        lvlend = 0
        ccsize = 1
        !
        ! lbegin is the pointer to the beginning of the current
        ! level, and lvlend points to the end of this level.
200     lbegin = lvlend + 1
        lvlend = ccsize
        nlvl = nlvl + 1
        xls(nlvl) = lbegin
        !
        ! generate the next level by finding all the masked
        ! neighbors of nodes in the current level.
        louter: do i = lbegin, lvlend
          node = ls(i)
          jstrt = xadj(node)
          jstop = xadj(node + 1) - 1
          if (jstop < jstrt)  cycle louter
          linner: do j = jstrt, jstop
            nbr = adjncy(j)
            if (mask(nbr) == 0) cycle linner
            ccsize = ccsize + 1
            ls(ccsize) = nbr
            mask(nbr) = 0
          end do linner
        end do louter
        !
        ! compute the current level width.
        ! if it is nonzero, generate the next level.
        lvsize = ccsize - lvlend
        if (lvsize > 0 ) go to 200
        !
        ! reset mask to one for the nodes in the level structure.
        xls(nlvl+1) = lvlend + 1
        do i = 1, ccsize
          node = ls(i)
          mask(node) = 1
        end do
        return
      end subroutine ims_rootls



      subroutine ims_odrv(n, nja, nsp, ia, ja, p, ip, isp, flag)
        !
        !                                                                3/12/82
        !***********************************************************************
        !  odrv -- driver for sparse matrix reordering routines
        !***********************************************************************
        !
        !  description
        !
        !    odrv finds a minimum degree ordering of the rows and columns
        !    of a matrix m stored in (ia,ja,a) format (see below).  for the
        !    reordered matrix, the work and storage required to perform
        !    gaussian elimination is (usually) significantly less.
        !
        !    note.. odrv and its subordinate routines have been modified to
        !    compute orderings for general matrices, not necessarily having any
        !    symmetry.  the minimum degree ordering is computed for the
        !    structure of the symmetric matrix  m + m-transpose.
        !    modifications to the original odrv module have been made in
        !    the coding in subroutine mdi, and in the initial comments in
        !    subroutines odrv and md.
        !
        !    if only the nonzero entries in the upper triangle of m are being
        !    stored, then odrv symmetrically reorders (ia,ja,a), (optionally)
        !    with the diagonal entries placed first in each row.  this is to
        !    ensure that if m(i,j) will be in the upper triangle of m with
        !    respect to the new ordering, then m(i,j) is stored in row i (and
        !    thus m(j,i) is not stored),  whereas if m(i,j) will be in the
        !    strict lower triangle of m, then m(j,i) is stored in row j (and
        !    thus m(i,j) is not stored).
        !
        !
        !  storage of sparse matrices
        !
        !    the nonzero entries of the matrix m are stored row-by-row in the
        !    array a.  to identify the individual nonzero entries in each row,
        !    we need to know in which column each entry lies.  these column
        !    indices are stored in the array ja.  i.e., if  a(k) = m(i,j),  then
        !    ja(k) = j.  to identify the individual rows, we need to know where
        !    each row starts.  these row pointers are stored in the array ia.
        !    i.e., if m(i,j) is the first nonzero entry (stored) in the i-th row
        !    and  a(k) = m(i,j),  then  ia(i) = k.  moreover, ia(n+1) points to
        !    the first location following the last element in the last row.
        !    thus, the number of entries in the i-th row is  ia(i+1) - ia(i),
        !    the nonzero entries in the i-th row are stored consecutively in
        !
        !            a(ia(i)),  a(ia(i)+1),  ..., a(ia(i+1)-1),
        !
        !    and the corresponding column indices are stored consecutively in
        !
        !            ja(ia(i)), ja(ia(i)+1), ..., ja(ia(i+1)-1).
        !
        !    since the coefficient matrix is symmetric, only the nonzero entries
        !    in the upper triangle need be stored.  for example, the matrix
        !
        !             ( 1  0  2  3  0 )
        !             ( 0  4  0  0  0 )
        !         m = ( 2  0  5  6  0 )
        !             ( 3  0  6  7  8 )
        !             ( 0  0  0  8  9 )
        !
        !    could be stored as
        !
        !            - 1  2  3  4  5  6  7  8  9 10 11 12 13
        !         ---+--------------------------------------
        !         ia - 1  4  5  8 12 14
        !         ja - 1  3  4  2  1  3  4  1  3  4  5  4  5
        !          a - 1  2  3  4  2  5  6  3  6  7  8  8  9
        !
        !    or (symmetrically) as
        !
        !            - 1  2  3  4  5  6  7  8  9
        !         ---+--------------------------
        !         ia - 1  4  5  7  9 10
        !         ja - 1  3  4  2  3  4  4  5  5
        !          a - 1  2  3  4  5  6  7  8  9          .
        !
        !
        !  parameters
        !
        !    n    - order of the matrix
        !
        !    nja  - number of nonzeroes in the matrix
        !
        !    nsp  - declared dimension of the one-dimensional array isp.  nsp
        !           must be at least  3n+4k,  where k is the number of nonzeroes
        !           in the strict upper triangle of m
        !
        !    ia   - integer one-dimensional array containing pointers to delimit
        !           rows in ja and a.  dimension = n+1
        !
        !    ja   - integer one-dimensional array containing the column indices
        !           corresponding to the elements of a.  dimension = number of
        !           nonzero entries in (the upper triangle of) m
        !
        !    a    - real one-dimensional array containing the nonzero entries in
        !           (the upper triangle of) m, stored by rows.  dimension =
        !           number of nonzero entries in (the upper triangle of) m
        !
        !    p    - integer one-dimensional array used to return the permutation
        !           of the rows and columns of m corresponding to the minimum
        !           degree ordering.  dimension = n
        !
        !    ip   - integer one-dimensional array used to return the inverse of
        !           the permutation returned in p.  dimension = n
        !
        !    isp  - integer one-dimensional array used for working storage.
        !           dimension = nsp
        !
        !    path - integer path specification.  values and their meanings are -
        !             1  find minimum degree ordering only
        !             2  find minimum degree ordering and reorder symmetrically
        !                  stored matrix (used when only the nonzero entries in
        !                  the upper triangle of m are being stored)
        !             3  reorder symmetrically stored matrix as specified by
        !                  input permutation (used when an ordering has already
        !                  been determined and only the nonzero entries in the
        !                  upper triangle of m are being stored)
        !             4  same as 2 but put diagonal entries at start of each row
        !             5  same as 3 but put diagonal entries at start of each row
        !
        !    flag - integer error flag.  values and their meanings are -
        !               0    no errors detected
        !              9n+k  insufficient storage in md
        !             10n+1  insufficient storage in odrv
        !             11n+1  illegal path specification
        !
        !
        !  conversion from real to double precision
        !
        !    change the real declarations in odrv and sro to double precision
        !    declarations.
        !
        !-----------------------------------------------------------------------
        !
        implicit none
        
        ! -- dummy variables
        integer(I4B), intent(in) :: n
        integer(I4B), intent(in) :: nja
        integer(I4B), intent(in) :: nsp
        integer(I4B), dimension(n+1), intent(in) :: ia
        integer(I4B), dimension(nja), intent(in) :: ja
        integer(I4B), dimension(n), intent(inout) :: p
        integer(I4B), dimension(n), intent(inout) :: ip
        integer(I4B), dimension(nsp), intent(inout) :: isp
        integer(I4B), intent(inout) :: flag

        ! -- local
        integer(I4B) :: v
        integer(I4B) :: l
        integer(I4B) :: head
        integer(I4B) :: mmax
        integer(I4B) :: next
        integer(I4B) :: path
        !
        ! set path for finding ordering only
        !
        path = 1
        !
        !
        ! initialize error flag and validate path specification
        flag = 0
        if (path < 1 .or. 5 < path)  go to 111
        !
        ! find minimum degree ordering
        mmax = (nsp-n)/2
        v    = 1
        l    = v     +  mmax
        head = l     +  mmax
        next = head  +  n
        if (mmax < n)  go to 110
        !
        call ims_md(n, nja, ia, ja, mmax, isp(v), isp(l), isp(head), p, &
                    ip, isp(v), flag)
        if (flag.ne.0)  go to 100
        !
2       return
        !
        ! ** error -- error detected in md
        !             flag = 9 * n + vi from routine mdi.
        !
100     return
        ! ** error -- insufficient storage
110     flag = 10*n + 1
        return
        ! ** error -- illegal path specified
111     flag = 11*n + 1
        return
      end subroutine ims_odrv



      subroutine ims_md(n, nja, ia, ja, mmax, v, l, head, last, next,           &
                        mark, flag)
        !
        !*****************************************************************
        !  ims_md -- minimum degree algorithm (based on element model)
        !*****************************************************************
        !
        !  description
        !
        !    ims_md finds a minimum degree ordering of the rows and 
        !    columns of a general sparse matrix m stored in (ia,ja,a) 
        !    format. when the structure of m is nonsymmetric, the ordering 
        !    is that obtained for the symmetric matrix  m + m-transpose.
        !
        !
        !  additional parameters
        !
        !    mmax  - declared dimension of the one-dimensional arrays v and l.
        !           mmax must be at least  n+2k,  where k is the number of
        !           nonzeroes in the strict upper triangle of m
        !
        !    v    - integer one-dimensional work array.  dimension = mmax
        !
        !    l    - integer one-dimensional work array.  dimension = mmax
        !
        !    head - integer one-dimensional work array.  dimension = n
        !
        !    last - integer one-dimensional array used to return the permutation
        !           of the rows and columns of m corresponding to the minimum
        !           degree ordering.  dimension = n
        !
        !    next - integer one-dimensional array used to return the inverse of
        !           the permutation returned in last.  dimension = n
        !
        !    mark - integer one-dimensional work array (may be the same as v).
        !           dimension = n
        !
        !    flag - integer error flag.  values and their meanings are -
        !             0      no errors detected
        !             11n+1  insufficient storage in md
        !
        !
        !  definitions of internal parameters
        !
        !    ---------+---------------------------------------------------------
        !    v(s)     - value field of list entry
        !    ---------+---------------------------------------------------------
        !    l(s)     - link field of list entry  (0 =) end of list)
        !    ---------+---------------------------------------------------------
        !    l(vi)    - pointer to element list of uneliminated vertex vi
        !    ---------+---------------------------------------------------------
        !    l(ej)    - pointer to boundary list of active element ej
        !    ---------+---------------------------------------------------------
        !    head(d)  - vj =) vj head of d-list d
        !             -  0 =) no vertex in d-list d
        !
        !
        !             -                  vi uneliminated vertex
        !             -          vi in ek           -       vi not in ek
        !    ---------+-----------------------------+---------------------------
        !    next(vi) - undefined but nonnegative   - vj =) vj next in d-list
        !             -                             -  0 =) vi tail of d-list
        !    ---------+-----------------------------+---------------------------
        !    last(vi) - (not set until mdp)         - -d =) vi head of d-list d
        !             --vk =) compute degree        - vj =) vj last in d-list
        !             - ej =) vi prototype of ej    -  0 =) vi not in any d-list
        !             -  0 =) do not compute degree -
        !    ---------+-----------------------------+---------------------------
        !    mark(vi) - mark(vk)                    - nonneg. tag .lt. mark(vk)
        !
        !
        !             -                   vi eliminated vertex
        !             -      ei active element      -           otherwise
        !    ---------+-----------------------------+---------------------------
        !    next(vi) - -j =) vi was j-th vertex    - -j =) vi was j-th vertex
        !             -       to be eliminated      -       to be eliminated
        !    ---------+-----------------------------+---------------------------
        !    last(vi) -  m =) size of ei = m        - undefined
        !    ---------+-----------------------------+---------------------------
        !    mark(vi) - -m =) overlap count of ei   - undefined
        !             -       with ek = m           -
        !             - otherwise nonnegative tag   -
        !             -       .lt. mark(vk)         -
        !
        !-----------------------------------------------------------------------
        !
        implicit none
        
        ! -- dummy variables
        integer(I4B), intent(in) :: n
        integer(I4B), intent(in) :: nja
        integer(I4B), dimension(n+1), intent(in) :: ia
        integer(I4B), dimension(nja), intent(in) :: ja
        integer(I4B), intent(in) :: mmax
        integer(I4B), dimension(mmax), intent(inout) :: v
        integer(I4B), dimension(mmax), intent(inout) :: l
        integer(I4B), dimension(n), intent(inout) :: head
        integer(I4B), dimension(n), intent(inout) :: last
        integer(I4B), dimension(n), intent(inout) :: next
        integer(I4B), dimension(n), intent(inout) :: mark
        integer(I4B), intent(inout) :: flag

        ! -- local
        integer(I4B) :: tag
        integer(I4B) :: dmin
        integer(I4B) :: vk
        integer(I4B) :: ek
        integer(I4B) :: tail
        integer(I4B) :: k

        equivalence(vk, ek)
        !
        ! initialization
        tag = 0
        call ims_mdi(n, nja, ia, ja, mmax ,v, l, head, last, next,              &
                     mark, tag, flag)
        if (flag.ne.0)  return
        !
        k = 0
        dmin = 1
        !
        ! while  k .lt. n  do
1       if (k >= n)  go to 4
        !
        ! search for vertex of minimum degree
2       if (head(dmin) > 0)  go to 3
        dmin = dmin + 1
        go to 2
        !
        ! remove vertex vk of minimum degree from degree list
3       vk = head(dmin)
        head(dmin) = next(vk)
        if (head(dmin) > 0)  last(head(dmin)) = -dmin
        !
        ! number vertex vk, adjust tag, and tag vk
        k = k+1
        next(vk) = -k
        last(ek) = dmin - 1
        tag = tag + last(ek)
        mark(vk) = tag
        !
        ! form element ek from uneliminated neighbors of vk
        call ims_mdm(n, mmax, vk, tail, v, l, last, next, mark)
        !
        ! purge inactive elements and do mass elimination
        call ims_mdp(n, mmax, k, ek, tail, v, l, head, last, next, mark)
        !
        ! update degrees of uneliminated vertices in ek
        call ims_mdu(n, mmax, ek, dmin, v, l, head, last, next, mark)
        !
        go to 1
        !
        ! generate inverse permutation from permutation
4       do k = 1, n
          next(k) = -next(k)
          last(next(k)) = k
        end do
        !
        return
      end subroutine ims_md


      subroutine ims_mdi(n, nja, ia, ja, mmax, v, l, head, last, next,          &
                         mark, tag, flag)
        !
        !***********************************************************************
        !  ims_mdi -- initialization
        !***********************************************************************
        implicit none
        
        ! -- dummy variables
        integer(I4B), intent(in) :: n
        integer(I4B), intent(in) :: nja
        integer(I4B), dimension(n+1), intent(in) :: ia
        integer(I4B), dimension(nja), intent(in) :: ja
        integer(I4B), intent(in) :: mmax
        integer(I4B), dimension(mmax), intent(inout) :: v
        integer(I4B), dimension(mmax), intent(inout) :: l
        integer(I4B), dimension(n), intent(inout) :: head
        integer(I4B), dimension(n), intent(inout) :: last
        integer(I4B), dimension(n), intent(inout) :: next
        integer(I4B), dimension(n), intent(inout) :: mark
        integer(I4B), intent(in) :: tag
        integer(I4B), intent(inout) :: flag

        ! -- local
        integer(I4B) :: sfs
        integer(I4B) :: vi
        integer(I4B) :: dvi
        integer(I4B) :: vj
        integer(I4B) :: jmin
        integer(I4B) :: jmax
        integer(I4B) :: j
        integer(I4B) :: lvk
        integer(I4B) :: kmax
        integer(I4B) :: k
        integer(I4B) :: nextvi
        integer(I4B) :: ieval
        !
        ! initialize degrees, element lists, and degree lists
        do vi = 1, n
          mark(vi) = 1
          l(vi) = 0
          head(vi) = 0
        end do
        sfs = n + 1
        !
        ! create nonzero structure
        ! for each nonzero entry a(vi,vj)
        louter: do vi = 1, n
          jmin = ia(vi)
          jmax = ia(vi+1) - 1
          if (jmin > jmax)  cycle louter
          linner1: do j = jmin, jmax !5
            vj = ja(j)
            !if (vj-vi) 2, 5, 4
            ieval = vj - vi
            if (ieval == 0) cycle linner1 !5
            if (ieval > 0) go to 4
            !
            ! if a(vi,vj) is in strict lower triangle
            ! check for previous occurrence of a(vj,vi)
2           lvk = vi
            kmax = mark(vi) - 1
            if (kmax == 0) go to 4
            linner2: do k = 1, kmax
              lvk = l(lvk)
              if (v(lvk) == vj) cycle linner1 !5
            end do linner2
            ! for unentered entries a(vi,vj)
4           if (sfs >= mmax)  go to 101
            !
            ! enter vj in element list for vi
            mark(vi) = mark(vi) + 1
            v(sfs) = vj
            l(sfs) = l(vi)
            l(vi) = sfs
            sfs = sfs+1
            !
            ! enter vi in element list for vj
            mark(vj) = mark(vj) + 1
            v(sfs) = vi
            l(sfs) = l(vj)
            l(vj) = sfs
            sfs = sfs + 1
          end do linner1
        end do louter
        !
        ! create degree lists and initialize mark vector
        do vi = 1, n
          dvi = mark(vi)
          next(vi) = head(dvi)
          head(dvi) = vi
          last(vi) = -dvi
          nextvi = next(vi)
          if (nextvi > 0)  last(nextvi) = vi
          mark(vi) = tag
        end do
        !
        return
        !
        ! ** error-  insufficient storage
101     flag = 9*n + vi
        return
      end subroutine ims_mdi



      subroutine ims_mdm(n, mmax, vk, tail, v, l, last, next, mark)
      !
      !***********************************************************************
      !  ims_mdm -- form element from uneliminated neighbors of vk
      !***********************************************************************
        implicit none
        
        ! -- dummy variables
        integer(I4B), intent(in) :: n
        integer(I4B), intent(in) :: mmax
        integer(I4B), intent(in) :: vk
        integer(I4B), intent(inout) :: tail
        integer(I4B), dimension(mmax), intent(inout) :: v
        integer(I4B), dimension(mmax), intent(inout) :: l
        integer(I4B), dimension(n), intent(inout) :: last
        integer(I4B), dimension(n), intent(inout) :: next
        integer(I4B), dimension(n), intent(inout) :: mark

        ! -- local
        integer(I4B) :: tag
        integer(I4B) :: s
        integer(I4B) :: ls
        integer(I4B) :: vs
        integer(I4B) :: es
        integer(I4B) :: b
        integer(I4B) :: lb
        integer(I4B) :: vb
        integer(I4B) :: blp
        integer(I4B) :: blpmax
        
        equivalence  (vs, es)
        !
        ! initialize tag and list of uneliminated neighbors
        tag = mark(vk)
        tail = vk
        !
        ! for each vertex/element vs/es in element list of vk
        ls = l(vk)
1       s = ls
        if (s == 0)  go to 5
        ls = l(s)
        vs = v(s)
        if (next(vs) < 0)  go to 2
        !
        ! if vs is uneliminated vertex, then tag and append to list of
        ! uneliminated neighbors
        mark(vs) = tag
        l(tail) = s
        tail = s
        go to 4
        !
        ! if es is active element, then ...
        ! for each vertex vb in boundary list of element es
2       lb = l(es)
        blpmax = last(es)
        louter: do blp = 1, blpmax !3
          b = lb
          lb = l(b)
          vb = v(b)
          !
          ! if vb is untagged vertex, then tag and append to list of
          ! uneliminated neighbors
          if (mark(vb) >= tag)  cycle louter !3
          mark(vb) = tag
          l(tail) = b
          tail = b
        end do louter
        !
        ! mark es inactive
        mark(es) = tag
        !
4       go to 1
        !
        ! terminate list of uneliminated neighbors
5       l(tail) = 0
        !
        return
      end subroutine ims_mdm


      subroutine ims_mdp(n, mmax, k, ek, tail, v, l, head, last, next, mark)
        !
        !***********************************************************************
        !  ims_mdp -- purge inactive elements and do mass elimination
        !***********************************************************************
        implicit none
        
        ! -- dummy variables
        integer(I4B), intent(in) :: n
        integer(I4B), intent(in) :: mmax
        integer(I4B), intent(inout) :: k
        integer(I4B), intent(in) :: ek
        integer(I4B), intent(inout) :: tail
        integer(I4B), dimension(mmax), intent(inout) :: v
        integer(I4B), dimension(mmax), intent(inout) :: l
        integer(I4B), dimension(n), intent(inout) :: head
        integer(I4B), dimension(n), intent(inout) :: last
        integer(I4B), dimension(n), intent(inout) :: next
        integer(I4B), dimension(n), intent(inout) :: mark

        ! -- local
        integer(I4B) :: tag
        integer(I4B) :: free
        integer(I4B) :: li
        integer(I4B) :: vi
        integer(I4B) :: lvi
        integer(I4B) :: evi
        integer(I4B) :: s
        integer(I4B) :: ls
        integer(I4B) :: es
        integer(I4B) :: ilp
        integer(I4B) :: ilpmax
        integer(I4B) :: i
        !
        ! initialize tag
        tag = mark(ek)
        !
        ! for each vertex vi in ek
        li = ek
        ilpmax = last(ek)
        if (ilpmax <= 0)  go to 12
        louter: do ilp = 1, ilpmax !11
          i = li
          li = l(i)
          vi = v(li)
          !
          ! remove vi from degree list
          if (last(vi) == 0)  go to 3
          if (last(vi) > 0)  go to 1
          head(-last(vi)) = next(vi)
          go to 2
1         next(last(vi)) = next(vi)
2         if (next(vi) > 0)  last(next(vi)) = last(vi)
          !
          ! remove inactive items from element list of vi
3         ls = vi
4         s = ls
          ls = l(s)
          if (ls == 0)  go to 6
          es = v(ls)
          if (mark(es) < tag)  go to 5
          free = ls
          l(s) = l(ls)
          ls = s
5         go to 4
          !
          ! if vi is interior vertex, then remove from list and eliminate

6         lvi = l(vi)
          if (lvi.ne.0)  go to 7
          l(i) = l(li)
          li = i
          !
          k = k + 1
          next(vi) = -k
          last(ek) = last(ek) - 1
          cycle louter !11
          !
          ! else ...
          ! classify vertex vi
7         if (l(lvi).ne.0)  go to 9
          evi = v(lvi)
          if (next(evi) >= 0)  go to 9
          if (mark(evi) < 0)  go to 8
          !
          ! if vi is prototype vertex, then mark as such, initialize
          ! overlap count for corresponding element, and move vi to end
          ! of boundary list
          last(vi) = evi
          mark(evi) = -1
          l(tail) = li
          tail = li
          l(i) = l(li)
          li = i
          go to 10
          !
          ! else if vi is duplicate vertex, then mark as such and adjust
          ! overlap count for corresponding element
8         last(vi) = 0
          mark(evi) = mark(evi) - 1
          go to 10
          !
          ! else mark vi to compute degree
9         last(vi) = -ek
          !
          ! insert ek in element list of vi
10        v(free) = ek
          l(free) = l(vi)
          l(vi) = free
        end do louter !11
        !
        ! terminate boundary list
12      l(tail) = 0
        !
        return
      end subroutine ims_mdp



      subroutine ims_mdu(n, mmax, ek, dmin, v, l, head, last, next, mark)
        !
        !***********************************************************************
        !  ims_mdu -- update degrees of uneliminated vertices in ek
        !***********************************************************************
        implicit none
        
        ! -- dummy variables
        integer(I4B), intent(in) :: n
        integer(I4B), intent(in) :: mmax
        integer(I4B), intent(in) :: ek
        integer(I4B), intent(inout) :: dmin
        integer(I4B), dimension(mmax), intent(inout) :: v
        integer(I4B), dimension(mmax), intent(inout) :: l
        integer(I4B), dimension(n), intent(inout) :: head
        integer(I4B), dimension(n), intent(inout) :: last
        integer(I4B), dimension(n), intent(inout) :: next
        integer(I4B), dimension(n), intent(inout) :: mark

        ! -- local
        integer(I4B) :: tag
        integer(I4B) :: vi
        integer(I4B) :: evi
        integer(I4B) :: dvi
        integer(I4B) :: s
        integer(I4B) :: vs
        integer(I4B) :: es
        integer(I4B) :: b
        integer(I4B) :: vb
        integer(I4B) :: ilp
        integer(I4B) :: ilpmax
        integer(I4B) :: blp
        integer(I4B) :: blpmax
        integer(I4B) :: i

        equivalence  (vs, es)
        !
        ! initialize tag
        tag = mark(ek) - last(ek)
        !
        ! for each vertex vi in ek
        i = ek
        ilpmax = last(ek)
        if (ilpmax <= 0)  go to 11
        louter: do ilp = 1, ilpmax !10
          i = l(i)
          vi = v(i)
          !if (last(vi))  1, 10, 8
          if (last(vi) == 0) cycle louter !10
          if (last(vi) > 0) goto 8
          !
          ! if vi neither prototype nor duplicate vertex, then merge elements
          ! to compute degree
1         tag = tag + 1
          dvi = last(ek)
          !
          ! for each vertex/element vs/es in element list of vi
          s = l(vi)
2         s = l(s)
          if (s == 0)  go to 9
          vs = v(s)
          if (next(vs) < 0)  go to 3
          !
          ! if vs is uneliminated vertex, then tag and adjust degree
          mark(vs) = tag
          dvi = dvi + 1
          go to 5
          !
          ! if es is active element, then expand
          ! check for outmatched vertex
3         if (mark(es) < 0)  go to 6
          !
          ! for each vertex vb in es
          b = es
          blpmax = last(es)
          linner: do blp = 1, blpmax !4
            b = l(b)
            vb = v(b)
            !
            ! if vb is untagged, then tag and adjust degree
            if (mark(vb) >= tag)  cycle linner !4
            mark(vb) = tag
            dvi = dvi + 1
          end do linner !4
          !
5         go to 2
          !
          ! else if vi is outmatched vertex, then adjust overlaps but do not
          ! compute degree
6         last(vi) = 0
          mark(es) = mark(es) - 1
7         s = l(s)
          if (s == 0)  cycle louter !10
          es = v(s)
          if (mark(es) < 0)  mark(es) = mark(es) - 1
          go to 7
          !
          ! else if vi is prototype vertex, then calculate degree by
          ! inclusion/exclusion and reset overlap count
8         evi = last(vi)
          dvi = last(ek) + last(evi) + mark(evi)
          mark(evi) = 0
          !
          ! insert vi in appropriate degree list
9         next(vi) = head(dvi)
          head(dvi) = vi
          last(vi) = -dvi
          if (next(vi) > 0)  last(next(vi)) = vi
          if (dvi < dmin)  dmin = dvi
          !
        end do louter !10
        !
11      return
      end subroutine ims_mdu

      !
      ! ROUTINES FROM SPARSKIT TO PERMUTATE A LINEAR SYSTEM OF EQUATIONS
      ! IN ORDER TO REORDER THE MATRIX TO MINIMIZE THE BANDWIDTH USING
      ! THE REVERSE CUTHILL MCKEE OR MINIMUM DEGREE ORDERING ALGORITHMS
      !
      subroutine ims_dperm(nrow, nja, a, ja, ia, ao, jao, iao,                  & 
                           perm, qperm, job)
        implicit none
      
        ! -- dummy variables
        integer(I4B), intent(in) :: nrow
        integer(I4B), intent(in) :: nja
        real(DP), dimension(nja), intent(in) :: a
        integer(I4B), dimension(nja), intent(in) :: ja
        integer(I4B), dimension(nrow+1), intent(in) :: ia
        real(DP), dimension(nja), intent(inout) :: ao
        integer(I4B), dimension(nja), intent(inout) :: jao
        integer(I4B), dimension(nrow+1), intent(inout) :: iao
        integer(I4B), dimension(nrow), intent(inout) :: perm
        integer(I4B), dimension(nrow), intent(inout) :: qperm
        integer(I4B), intent(in) :: job
        !-----------------------------------------------------------------------
        ! This routine permutes the rows and columns of a matrix stored in CSR
        ! format. i.e., it computes P A Q, where P, Q are permutation matrices.
        ! P maps row i into row perm(i) and Q maps column j into column qperm(j)
        !      a(i,j)    becomes   a(perm(i),qperm(j)) in new matrix
        ! In the particular case where Q is the transpose of P (symmetric
        ! permutation of A) then qperm is not needed.
        ! note that qperm should be of length ncol (number of columns) but this
        ! is not checked.
        !-----------------------------------------------------------------------
        ! Y. Saad, Sep. 21 1989 / recoded Jan. 28 1991.
        !-----------------------------------------------------------------------
        ! on entry:
        !----------
        ! n       = dimension of the matrix
        ! a, ja,
        !    ia = input matrix in a, ja, ia format
        ! perm       = integer array of length n containing the permutation arra
        !        for the rows: perm(i) is the destination of row i in the
        !         permuted matrix -- also the destination of column i in case
        !         permutation is symmetric (job .le. 2)
        !
        ! qperm      = same thing for the columns. This should be provided only
        !         if job=3 or job=4, i.e., only in the case of a nonsymmetric
        !        permutation of rows and columns. Otherwise qperm is a dummy
        !
        ! job      = integer indicating the work to be done:
        ! * job = 1,2 permutation is symmetric  Ao :== P * A * transp(P)
        !             job = 1      permute a, ja, ia into ao, jao, iao
        !             job = 2 permute matrix ignoring real values.
        ! * job = 3,4 permutation is non-symmetric  Ao :== P * A * Q
        !             job = 3      permute a, ja, ia into ao, jao, iao
        !             job = 4 permute matrix ignoring real values.
        !
        ! on return:
        !-----------
        ! ao, jao, iao = input matrix in a, ja, ia format
        !
        ! in case job .eq. 2 or job .eq. 4, a and ao are never referred to
        ! and can be dummy arguments.
        ! Notes:
        !-------
        !  1) algorithm is in place
        !  2) column indices may not be sorted on return eventhough they may be
        !     on entry.
        !----------------------------------------------------------------------
        ! -- local
        integer(I4B) :: locjob, mod
        !
        !     locjob indicates whether or not real values must be copied.
        !
        locjob = mod(job,2)
        !
        ! permute rows first
        !
        call ims_rperm(nrow, nja, a, ja, ia, ao, jao, iao, perm, locjob)
        !
        ! then permute columns
        !
        locjob = 0
        !
        if (job .le. 2) then
          call ims_cperm(nrow, nja, ao, jao, iao, ao, jao, iao, perm, locjob)
        else
          call ims_cperm(nrow, nja, ao, jao, iao, ao, jao, iao, qperm, locjob)
        endif
        !
        return
      !-------end-of-ims_dperm----------------------------------------------------
      end subroutine ims_dperm


      !-----------------------------------------------------------------------
      subroutine ims_rperm (nrow, nja, a, ja, ia, ao, jao, iao, perm, job)
        implicit none
        
        ! -- dummy variables
        integer(I4B), intent(in) :: nrow
        integer(I4B), intent(in) :: nja
        real(DP), dimension(nja), intent(in) :: a
        integer(I4B), dimension(nja), intent(in) :: ja
        integer(I4B), dimension(nrow+1), intent(in) :: ia
        real(DP), dimension(nja), intent(inout) :: ao
        integer(I4B), dimension(nja), intent(inout) :: jao
        integer(I4B), dimension(nrow+1), intent(inout) :: iao
        integer(I4B), dimension(nrow), intent(inout) :: perm
        integer(I4B), intent(in) :: job
        !-----------------------------------------------------------------------
        ! this subroutine permutes the rows of a matrix in CSR format.
        ! ims_rperm  computes B = P A  where P is a permutation matrix.
        ! the permutation P is defined through the array perm: for each j,
        ! perm(j) represents the destination row number of row number j.
        ! Youcef Saad -- recoded Jan 28, 1991.
        !-----------------------------------------------------------------------
        ! on entry:
        !----------
        ! n       = dimension of the matrix
        ! a, ja, ia = input matrix in csr format
        ! perm       = integer array of length nrow containing the permutation a
        !        for the rows: perm(i) is the destination of row i in the
        !         permuted matrix.
        !         ---> a(i,j) in the original matrix becomes a(perm(i),j)
        !         in the output  matrix.
        !
        ! job      = integer indicating the work to be done:
        !             job = 1      permute a, ja, ia into ao, jao, iao
        !                       (including the copying of real values ao and
        !                       the array iao).
        !             job .ne. 1 :  ignore real values.
        !                     (in which case arrays a and ao are not needed nor
        !                      used).
        !
        !------------
        ! on return:
        !------------
        ! ao, jao, iao = input matrix in a, ja, ia format
        ! note :
        !        if (job.ne.1)  then the arrays a and ao are not used.
        !----------------------------------------------------------------------c
        !           Y. Saad, May  2, 1990                                      c
        !----------------------------------------------------------------------c
        ! -- local
        logical :: values
        integer(I4B) :: i
        integer(I4B) :: j
        integer(I4B) :: k
        integer(I4B) :: ii
        integer(I4B) :: ko
        
        values = (job .eq. 1)
        !
        !     determine pointers for output matrix.
        !
        do j=1,nrow
          i = perm(j)
          iao(i+1) = ia(j+1) - ia(j)
        end do
        !
        ! get pointers from lengths
        !
        iao(1) = 1
        do j=1,nrow
          iao(j+1) = iao(j+1) + iao(j)
        end do
        !
        ! copying
        !
        do ii=1,nrow
          !
          ! old row = ii  -- new row = iperm(ii) -- ko = new pointer
          !
          ko = iao(perm(ii))
          do k = ia(ii), ia(ii+1)-1
            jao(ko) = ja(k)
            if (values) ao(ko) = a(k)
            ko = ko+1
          end do
        end do
        !
        return
      !---------end-of-ims_rperm -------------------------------------------------
      !-----------------------------------------------------------------------
      end subroutine ims_rperm



      !-----------------------------------------------------------------------
      subroutine ims_cperm (nrow, nja, a, ja, ia, ao, jao, iao, perm, job)
        implicit none

        ! -- dummy variables
        integer(I4B), intent(in) :: nrow
        integer(I4B), intent(in) :: nja
        real(DP), dimension(nja), intent(in) :: a
        integer(I4B), dimension(nja), intent(in) :: ja
        integer(I4B), dimension(nrow+1), intent(in) :: ia
        real(DP), dimension(nja), intent(inout) :: ao
        integer(I4B), dimension(nja), intent(inout) :: jao
        integer(I4B), dimension(nrow+1), intent(inout) :: iao
        integer(I4B), dimension(nrow), intent(inout) :: perm
        integer(I4B), intent(in) :: job

        !-----------------------------------------------------------------------
        ! this subroutine permutes the columns of a matrix a, ja, ia.
        ! the result is written in the output matrix  ao, jao, iao.
        ! cperm computes B = A P, where  P is a permutation matrix
        ! that maps column j into column perm(j), i.e., on return
        !      a(i,j) becomes a(i,perm(j)) in new matrix
        ! Y. Saad, May 2, 1990 / modified Jan. 28, 1991.
        !-----------------------------------------------------------------------
        ! on entry:
        !----------
        ! nrow       = row dimension of the matrix
        !
        ! a, ja, ia = input matrix in csr format.
        !
        ! perm      = integer array of length ncol (number of columns of A
        !         containing the permutation array  the columns:
        !         a(i,j) in the original matrix becomes a(i,perm(j))
        !         in the output matrix.
        !
        ! job      = integer indicating the work to be done:
        !             job = 1      permute a, ja, ia into ao, jao, iao
        !                       (including the copying of real values ao and
        !                       the array iao).
        !             job .ne. 1 :  ignore real values ao and ignore iao.
        !
        !------------
        ! on return:
        !------------
        ! ao, jao, iao = input matrix in a, ja, ia format (array ao not needed)
        !
        ! Notes:
        !-------
        ! 1. if job=1 then ao, iao are not used.
        ! 2. This routine is in place: ja, jao can be the same.
        ! 3. If the matrix is initially sorted (by increasing column number)
        !    then ao,jao,iao  may not be on return.
        !
        !----------------------------------------------------------------------c
        ! -- local
        integer(I4B) :: k, i
        !
        do k=1, nja
          jao(k) = perm(ja(k))
        end do
        !
        ! done with ja array. return if no need to touch values.
        !
        if (job .ne. 1) return
        !
        ! else get new pointers -- and copy values too.
        !
        do i=1, nrow+1
          iao(i) = ia(i)
        end do
        !
        do k=1, nja
          ao(k) = a(k)
        end do
        !
        return
        !---------end-of-ims_cperm--------------------------------------------------
        !-----------------------------------------------------------------------
      end subroutine ims_cperm


      !-----------------------------------------------------------------------
      subroutine ims_vperm (n, x, perm)
        implicit none
        
        ! -- dummy variables
        integer(I4B), intent(in) :: n
        integer(I4B), dimension(n), intent(inout) :: perm
        real(DP), dimension(n), intent(inout) :: x
        !-----------------------------------------------------------------------
        ! this subroutine performs an in-place permutation of a real vector x
        ! according to the permutation array perm(*), i.e., on return,
        ! the vector x satisfies,
        !
        ! x(perm(j)) :== x(j), j=1,2,.., n
        !
        !-----------------------------------------------------------------------
        ! on entry:
        !---------
        ! n = length of vector x.
        ! perm = integer array of length n containing the permutation  array.
        ! x = input vector
        !
        ! on return:
        !----------
        ! x = vector x permuted according to x(perm(*)) :=  x(*)
        !
        !----------------------------------------------------------------------c
        !           Y. Saad, Sep. 21 1989                                      c
        !----------------------------------------------------------------------c
        ! -- local
        integer(I4B) :: j
        integer(I4B) :: k
        integer(I4B) :: ii
        integer(I4B) :: init
        integer(I4B) :: next
        real(DP) :: tmp, tmp1
        !
        init      = 1
        tmp       = x(init)
        ii        = perm(init)
        perm(init)= -perm(init)
        k         = 0
        !
        ! loop
        !
6       k = k + 1
        !
        ! save the chased element --
        !
        tmp1      = x(ii)
        x(ii)     = tmp
        next      = perm(ii)
        if (next < 0 ) goto 65
        !
        ! test for end
        !
        if (k > n) go to 101
        tmp       = tmp1
        perm(ii)  = -perm(ii)
        ii        = next
        !
        ! end loop
        !
        go to 6
        !
        ! reinitialize cycle --
        !
65      init = init + 1
        if (init > n) go to 101
        if (perm(init) < 0) go to 65
        tmp       =  x(init)
        ii        =  perm(init)
        perm(init)= -perm(init)
        go to 6
        !
101     continue
        do j = 1, n
          perm(j) = -perm(j)
        end do
        !
        return
        !-------------------end-of-ims_vperm---------------------------------------
        !-----------------------------------------------------------------------
      end subroutine ims_vperm
  
  end module IMSReorderingModule