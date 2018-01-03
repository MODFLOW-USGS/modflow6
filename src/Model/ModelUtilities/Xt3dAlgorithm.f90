module Xt3dAlgorithmModule
!
! -- Mathematical core of the XT3D method.
!
      contains
!
      subroutine qconds(nnbrmx,nnbr0,inbr0,il01,vc0,vn0,dl0,dl0n,ck0,     &
         nnbr1,inbr1,il10,vc1,vn1,dl1,dl1n,ck1,ar01,ar10,                 &
         vcthresh,allhc0,allhc1,chat01,chati0,chat1j)
      use KindModule, only: DP, I4B
      implicit real(DP) (a-h,o-z)
      implicit integer(I4B) (i-n)
      logical allhc0,allhc1
      dimension inbr0(nnbrmx)
      dimension vc0(nnbrmx,3),vn0(nnbrmx,3)
      dimension dl0(nnbrmx),dl0n(nnbrmx)
      dimension ck0(3,3)
      dimension inbr1(nnbrmx)
      dimension vc1(nnbrmx,3),vn1(nnbrmx,3)
      dimension dl1(nnbrmx),dl1n(nnbrmx)
      dimension ck1(3,3)
      dimension bhat0(nnbrmx),bhat1(nnbrmx)
      dimension chati0(nnbrmx),chat1j(nnbrmx)
!
!.....Compute the "conductances" in the normal-flux expression for an
!        interface (modflow-usg version).  The cell on one side of
!        the interface is "cell 0", and the one on the other side is
!        "cell 1".
!
!        nnbrmx = maximum number of neighbors allowed for a cell.
!        nnbr0 = number of neighbors (local connections) for cell 0.
!        inbr0 = array with the list of neighbors for cell 0.
!        il01 = local node number of cell 1 with respect to cell 0.
!        vc0 = array of connection unit-vectors for cell 0 with its
!           neighbors.
!        vn0 = array of unit normal vectors for cell 0's interfaces.
!        dl0 = array of lengths contributed by cell 0 to its
!           connections with its neighbors.
!        dl0n = array of lengths contributed by cell 0's neighbors to
!           their connections with cell 0.
!        ck0 = conductivity tensor for cell 0.
!        nnbr1 = number of neighbors (local connections) for cell 1.
!        inbr1 = array with the list of neighbors for cell 1.
!        il10 = local node number of cell 0 with respect to cell 1.
!        vc1 = array of connection unit-vectors for cell 1 with its
!           neighbors.
!        vn1 = array of unit normal vectors for cell 1's interfaces.
!        dl1 = array of lengths contributed by cell 1 to its
!           connections with its neighbors.
!        dl1n = array of lengths contributed by cell 1's neighbors to
!           their connections with cell 1.
!        ck1 = conductivity tensor for cell1.
!        ar01 = area of interface (0,1).
!        ar10 = area of interface (1,0).
!        chat01 = "conductance" for connection (0,1).
!        chati0 = array of "conductances" for connections of cell 0.
!           (zero for connection with cell 1, as this connection is
!           already covered by chat01.)
!        chat1j = array of "conductances" for connections of cell 1.
!           (zero for connection with cell 0., as this connection is
!           already covered by chat01.)
!
!.....Set the global cell number for cell 1, as found in the neighbor
!        list for cell 0.  It is assumed that cells 0 and 1 are both
!        active, or else this subroutine would not have been called.
      i1 = inbr0(il01)
!
!.....If area ar01 is zero (in which case ar10 is also zero, since
!        this can only happen here in the case of Newton), then the
!        "conductances" are all zero.
      if (ar01.eq.0d0) then
         chat01 = 0d0
         do i=1,nnbrmx
          chati0(i) = 0d0
          chat1j(i) = 0d0
         enddo
!.....Else compute "conductances."
      else
!........Compute contributions from cell 0.
         call abhats(nnbrmx,nnbr0,inbr0,il01,vc0,vn0,dl0,dl0n,ck0,        &
                     vcthresh,allhc0,ar01,ahat0,bhat0)
!........Compute contributions from cell 1.
         call abhats(nnbrmx,nnbr1,inbr1,il10,vc1,vn1,dl1,dl1n,ck1,        &
                     vcthresh,allhc1,ar10,ahat1,bhat1)
!........Compute "conductances" based on the two flux estimates.
         wght1 = ahat0/(ahat0 + ahat1)
         wght0 = 1d0 - wght1
         chat01 = wght1*ahat1
         do i=1,nnbrmx
          chati0(i) = wght0*bhat0(i)
          chat1j(i) = wght1*bhat1(i)
         enddo
      end if
!
      return
      end subroutine qconds

      subroutine abhats(nnbrmx,nnbr,inbr,il01,vc,vn,dl0,dln,ck,    &
         vcthresh,allhc,ar01,ahat,bhat)
      use KindModule, only: DP, I4B
      implicit real(DP) (a-h,o-z)
      implicit integer(I4B) (i-n)
      logical allhc,iscomp
      dimension vc(nnbrmx,3),vccde(nnbrmx,3),vn(nnbrmx,3)
      dimension dl0(nnbrmx),dln(nnbrmx)
      dimension inbr(nnbrmx),ck(3,3)
      dimension rmat(3,3),sigma(3),bhat(nnbrmx)
      dimension bd(nnbrmx),be(nnbrmx),betad(nnbrmx),betae(nnbrmx)
!
!.....Compute "ahat" and "bhat" coefficients for one side of an
!        interface.
!
!.....Determine the basis vectors for local "(c, d, e)" coordinates
!        associated with the connection between cells 0 and 1, and
!        set the rotation matrix that transforms vectors from model
!        coordinates to (c, d, e) coordinates.  (If no active
!        connection is found that has a non-negligible component
!        perpendicular to the primary connection, ilmo=0 is returned.)
      call getrot(nnbrmx,nnbr,inbr,vc,il01,rmat,iml0)
!
!.....If no active connection with a non-negligible perpendicular
!        component, assume no perpendicular gradient and base gradient
!        solely on the primary connection.  Otherwise, proceed with
!        basing weights on information from neighboring connections.
      if (iml0.eq.0) then
!
!........Compute ahat and bhat coefficients assuming perpendicular
!           components of gradient are zero.
         sigma(1) = dot_product(vn(il01,:), matmul(ck, rmat(:,1)))
         ahat = sigma(1)/dl0(il01)
         bhat = 0d0
!
      else
!
!........Transform local connection unit-vectors from model coordinates
!           to "(c, d, e)" coordinates associated with the connection
!           between cells 0 and 1.
         call tranvc(nnbrmx,nnbr,rmat,vc,vccde)
!
!........Get "a" and "b" weights for first perpendicular direction.
         call abwts(nnbrmx,nnbr,inbr,il01,2,vccde,           &
            vcthresh,dl0,dln,acd,add,aed,bd)
!
!........If all neighboring connections are user-designated as
!           horizontal, or if none have a non-negligible component in
!           the second perpendicular direction, assume zero gradient in
!           the second perpendicular direction.  Otherwise, get "a" and
!           "b" weights for second perpendicular direction based on
!           neighboring connections.
         if (allhc) then                   ! kluge note - does not handle the case of a x-sec model (old comment?)
            ace = 0d0
            aee = 1d0
            ade = 0d0
            be = 0d0
         else
            iscomp = .false.
            do 200 il=1,nnbr
               if ((il.eq.il01).or.(inbr(il).eq.0)) then
                  cycle
               else if (dabs(vccde(il,3)).gt.1d-10) then
                  iscomp = .true.
                  exit
               end if
  200       continue
            if (iscomp) then
               call abwts(nnbrmx,nnbr,inbr,il01,3,vccde,    &
                  vcthresh,dl0,dln,ace,aee,ade,be)
            else
               ace = 0d0
               aee = 1d0
               ade = 0d0
               be = 0d0
            end if
         end if
!
!........Compute alpha and beta coefficients.
         determ = add*aee - ade*aed
         oodet = 1d0/determ
         alphad = (acd*aee - ace*aed)*oodet
         alphae = (ace*add - acd*ade)*oodet
         betad = 0d0
         betae = 0d0
         do 300 il=1,nnbr
!...........If this is connection (0,1) or inactive, skip.
            if ((il.eq.il01).or.(inbr(il).eq.0)) cycle
            betad(il) = (bd(il)*aee - be(il)*aed)*oodet
            betae(il) = (be(il)*add - bd(il)*ade)*oodet
  300    continue
!
!........Compute sigma coefficients.
         sigma = matmul(vn(il01,:), matmul(ck, rmat))
!
!........Compute ahat and bhat coefficients.
         ahat = (sigma(1) - sigma(2)*alphad - sigma(3)*alphae)/dl0(il01)
         bhat = 0d0
         do 400 il=1,nnbr
!...........If this is connection (0,1) or inactive, skip.
            if ((il.eq.il01).or.(inbr(il).eq.0)) cycle
            dl0il = dl0(il) + dln(il)
            bhat(il) = (sigma(2)*betad(il) + sigma(3)*betae(il))/dl0il
  400    continue
!........Set the bhat for connection (0,1) to zero here, since we have
!           been skipping it in our do loops to avoiding explicitly
!           computing it.  This will carry through to the corresponding
!           chati0 and chat1j value, so that they too are zero.
         bhat(il01) = 0d0
!
      end if
!
!.....Multiply by area.
      ahat = ahat*ar01
      bhat = bhat*ar01
!
      return
      end subroutine abhats

      subroutine getrot(nnbrmx,nnbr,inbr,vc,il01,rmat,iml0)
      use KindModule, only: DP, I4B
      implicit real(DP) (a-h,o-z)
      implicit integer(I4B) (i-n)
      dimension inbr(nnbrmx)
      dimension vc(nnbrmx,3),vcc(3),vcd(3),vce(3),vcmax(3),rmat(3,3)
!
!.....Compute the matrix that rotates the model-coordinate axes to
!     the "(c, d, e)-coordinate" axes associated with a connection.
!     This is also the matrix that transforms the components of a vector
!     from (c, d, e) coordinates to model coordinates.  [Its transpose
!     transforms from model to (c, d, e) coordinates.]
!
!        vcc = unit vector for the primary connection, in model
!           coordinates.
!        vcd = unit vector for the first perpendicular direction,
!           in model coordinates.
!        vce = unit vector for the second perpendicular direction,
!           in model coordinates.
!        vcmax = unit vector for the connection with the maximum
!           component perpendicular to the primary connection,
!           in model coordinates.
!        rmat = rotation matrix from model to (c, d, e) coordinates.
!
!.....set vcc.
      vcc(:) = vc(il01,:)
!
!.....Set vcmax.  (If no connection has a perpendicular component
!        greater than some tiny threshold, return with iml0=0 and
!        the first column of rmat set to vcc -- the other columns
!        are not needed.)
      acmpmn = 1d0 - 1d-10
      iml0 = 0
      do 200 il=1,nnbr
         if ((il.eq.il01).or.(inbr(il).eq.0)) then
            cycle
         else
            cmp = dot_product(vc(il,:), vcc)
            acmp = dabs(cmp)
            if (acmp.lt.acmpmn) then
               cmpmn = cmp
               acmpmn = acmp
               iml0 = il
            end if
         end if
  200 continue
      if (iml0.eq.0) then
         rmat(:,1) = vcc(:)
         goto 999
      else
         vcmax(:) = vc(iml0,:)
      end if
!
!.....Set the first perpendicular direction as the direction that is
!        coplanar with vcc and vcmax and perpendicular to vcc.
      vcd = vcmax - cmpmn*vcc
      vcd = vcd/dsqrt(1d0 - cmpmn*cmpmn)
!
!.....Set the second perpendicular direction as the cross product of
!        the primary and first-perpendicular directions.
      vce(1) = vcc(2)*vcd(3) - vcc(3)*vcd(2)
      vce(2) = vcc(3)*vcd(1) - vcc(1)*vcd(3)
      vce(3) = vcc(1)*vcd(2) - vcc(2)*vcd(1)
!
!.....Set the rotation matrix as the matrix with vcc, vcd, and vce
!        as its columns.
      rmat(:,1) = vcc(:)
      rmat(:,2) = vcd(:)
      rmat(:,3) = vce(:)
!
  999 return
      end subroutine getrot

      subroutine tranvc(nnbrmx,nnbrs,rmat,vc,vccde)
      use KindModule, only: DP, I4B
      implicit real(DP) (a-h,o-z)
      implicit integer(I4B) (i-n)
      dimension rmat(3,3)
      dimension vc(nnbrmx,3),vccde(nnbrmx,3)
!
!.....Transform local connection unit-vectors from model coordinates
!        to "(c, d, e)" coordinates associated with a connection.
!
!        nnbrs = number of neighbors (local connections)
!        rmat = rotation matrix from (c, d, e) to model coordinates.
!        vc = array of connection unit-vectors with respect to model
!           coordinates.
!        vccde = array of connection unit-vectors with respect to
!           (c, d, e) coordinates.
!
!.....Loop over the local connections, transforming the unit vectors.
!        Note that we are multiplying by the transpose of the
!        rotation matrix so that the transformation is from model
!        to (c, d, e) coordinates.
      do 200 il=1,nnbrs
         vccde(il,:) = matmul(transpose(rmat), vc(il,:))
  200 continue
!
      return
      end subroutine tranvc

      subroutine abwts(nnbrmx,nnbr,inbr,il01,nde1,vccde,    &
         vcthresh,dl0,dln,acd,add,aed,bd)
      use KindModule, only: DP, I4B
      implicit real(DP) (a-h,o-z)
      implicit integer(I4B) (i-n)
      dimension inbr(nnbrmx)
      dimension vccde(nnbrmx,3)
      dimension dl0(nnbrmx), dln(nnbrmx)
      dimension omwt(nnbrmx), bd(nnbrmx)
!
!.....Compute "a" and "b" weights for the local connections with respect
!        to the perpendicular direction of primary interest.
!
!        nde1 = number that indicates the perpendicular direction of
!           primary interest on this call: "d" (2) or "e" (3).
!        vccde = array of connection unit-vectors with respect to
!           (c, d, e) coordinates.
!        bd = array of "b" weights.
!        aed = "a" weight that goes on the matrix side of the 2x2
!           problem.
!        acd = "a" weight that goes on the right-hand side of the
!           2x2 problem.
!
!.....Set the perpendicular direction of secondary interest.
      nde2 = 5 - nde1
!
!.....Begin computing "omega" weights.
      omwt = 0d0
      dsum = 0d0
      vcmx = 0d0
      do 200 il=1,nnbr
!........if this is connection (0,1) or inactive, skip.
         if ((il.eq.il01).or.(inbr(il).eq.0)) cycle
         vcmx = max(dabs(vccde(il,nde1)), vcmx)
         dlm = 5d-1*(dl0(il) + dln(il))
!...........Distance-based weighting.  dl4wt is the distance between
!              the point supplying the gradient information and the
!              point at which the flux is being estimated.  Could be
!              coded as a special case of resistance-based weighting
!              (by setting the conductivity matrix to be the identity
!              matrix), but this is more efficient.
          cosang = vccde(il,1)
          dl4wt = dsqrt(dlm*dlm + dl0(il01)*dl0(il01)                          &
                  - 2d0*dlm*dl0(il01)*cosang)
         omwt(il) = dabs(vccde(il,nde1))*dl4wt
         dsum = dsum + omwt(il)
  200 continue
!
!.....Finish computing non-normalized "omega" weights.  [Add a
!        tiny bit to dsum so that the normalized omega weight later
!        evaluates to (essentially) 1 in the case of a single relevant
!        connection, avoiding 0/0.]
      dsum = dsum + 1d-10*dsum
      do 250 il=1,nnbr
!........If this is connection (0,1) or inactive, skip.
         if ((il.eq.il01).or.(inbr(il).eq.0)) cycle
         fact = dsum - omwt(il)
         omwt(il) = fact*dabs(vccde(il,nde1))
  250 continue
!
!.....Compute "b" weights.
      bd = 0d0
      dsum = 0d0
      do 300 il=1,nnbr
!........If this is connection (0,1) or inactive, skip.
         if ((il.eq.il01).or.(inbr(il).eq.0)) cycle
         bd(il) = omwt(il)*sign(1d0,vccde(il,nde1))
         dsum = dsum + omwt(il)*dabs(vccde(il,nde1))
  300 continue
      oodsum = 1d0/dsum
      do 350 il=1,nnbr
!........If this is connection (0,1) or inactive, skip.
         if ((il.eq.il01).or.(inbr(il).eq.0)) cycle
         bd(il) = bd(il)*oodsum
  350 continue
!
!.....Compute "a" weights.
      add = 1d0
      acd = 0d0
      aed = 0d0
      do 400 il=1,nnbr
!........If this is connection (0,1) or inactive, skip.
         if ((il.eq.il01).or.(inbr(il).eq.0)) cycle
         acd = acd + bd(il)*vccde(il,1)
         aed = aed + bd(il)*vccde(il,nde2)
  400 continue
!
!.....Apply attenuation function to acd, aed, and bd.
      if (vcmx.lt.vcthresh) then
         fatten = vcmx/vcthresh
         acd = acd*fatten
         aed = aed*fatten
         bd = bd*fatten
      end if
!
  999 return
      end subroutine abwts
!
end module Xt3dAlgorithmModule
