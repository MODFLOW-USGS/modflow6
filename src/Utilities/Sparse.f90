      module SparseModule
      !cdl (8/12/2013) sparse matrix module for managing the structure
      !of a matrix.  Module uses FORTRAN 2003 extensions to manage
      !the data structures in an object oriented fashion.

          use KindModule, only: DP, I4B
          implicit none
          
          type rowtype
              integer(I4B) :: nnz                                               ! number of nonzero entries in the row
              integer(I4B),allocatable,dimension(:) :: icolarray                ! array of column numbers
          end type rowtype

          type, public :: sparsematrix
              integer(I4B) :: nrow                                              ! number of rows in the matrix
              integer(I4B) :: ncol                                              ! number of columns in the matrix
              integer(I4B) :: nnz                                               ! number of nonzero matrix entries
              type(rowtype),allocatable,dimension(:) :: row                     ! one rowtype for each matrix row
              contains
                procedure :: init => initialize
                procedure :: addconnection
                procedure :: filliaja
                procedure :: sort
                procedure :: destroy
          end type sparsematrix
          
          contains

          subroutine destroy(this)
              implicit none
              class(sparsematrix), intent(inout) :: this
              deallocate(this%row)
          end subroutine destroy

          subroutine initialize(this,nrow,ncol,rowmaxnnz)
              !initial the sparse matrix.  This subroutine
              !acts a method for a sparse matrix by initializing
              !the row data.  It presently requires one maximum
              !value for all rows, however, this can be changed
              !to a vector of maximum values with one value for
              !each row.
              implicit none
              class(sparsematrix), intent(inout) :: this
              integer(I4B),intent(in) :: nrow,ncol
              integer(I4B),intent(in),dimension(nrow) :: rowmaxnnz
              integer(I4B) :: i
              this%nrow = nrow
              this%ncol = ncol
              this%nnz = 0
              allocate(this%row(nrow))
              do i=1,nrow
                  allocate(this%row(i)%icolarray(rowmaxnnz(i)))
                  this%row(i)%icolarray=0
                  this%row(i)%nnz=0
              enddo
          end subroutine initialize

          subroutine filliaja(this,ia,ja,ierror)
              !allocate and fill the ia and ja arrays using information
              !from the sparsematrix.
              !ierror is returned as:
              !  0 if no error
              !  1 if ia is not the correct size
              !  2 if ja is not the correct size
              !  3 if both ia and ja are not correct size
              implicit none
              class(sparsematrix), intent(inout) :: this
              integer(I4B),dimension(:),intent(inout) :: ia,ja
              integer(I4B),intent(inout) :: ierror
              integer(I4B) :: i,j,ipos
              ierror=0
              if(ubound(ia,dim=1)/=this%nrow+1) ierror=1
              if(ubound(ja,dim=1)/=this%nnz) ierror=ierror+2
              if(ierror/=0) return
              ipos=1
              ia(1)=ipos
              do i=1,this%nrow
                  do j=1,this%row(i)%nnz
                      ja(ipos)=this%row(i)%icolarray(j)
                      ipos=ipos+1
                  enddo
                  ia(i+1)=ipos
              enddo
          end subroutine filliaja

          subroutine addconnection(this,i,j,inodup,iaddop)
              !add a connection to the sparsematrix.  if inodup
              !(for no duplicates) is 1, then j is added only
              !if it is unique.
              implicit none
              class(sparsematrix), intent(inout) :: this
              integer(I4B),intent(in) :: i,j,inodup
              integer(I4B),optional,intent(inout) :: iaddop
              integer(I4B) :: iadded
              call insert(i, j, this%row(i), inodup, iadded)
              this%nnz=this%nnz+iadded
              if (present(iaddop)) iaddop = iadded
          end subroutine addconnection

          subroutine insert(i, j, thisrow, inodup, iadded)
              !insert j into the icolarray for row i
              !inodup=1 means do not include duplicate connections
              !iadded is 1 if a new entry was added (meaning that nnz for the row was increased)
              !iadded is 0 if duplicate and not added.  Used to track total number of connections
              implicit none
              integer(I4B),intent(in) :: i,j,inodup
              type(rowtype),intent(inout) :: thisrow
              integer(I4B),allocatable,dimension(:) :: iwk
              integer(I4B),intent(inout) :: iadded
              integer(I4B) :: jj,maxnnz
              iadded=0
              maxnnz=ubound(thisrow%icolarray,dim=1)
              if (thisrow%icolarray(1) == 0) then
                  thisrow%icolarray(1) = j
                  thisrow%nnz = thisrow%nnz + 1
                  iadded=1
                  return
              endif
              if (thisrow%nnz == maxnnz) then
                  !increase size of the row
                  allocate(iwk(thisrow%nnz))
                  iwk=thisrow%icolarray
                  deallocate(thisrow%icolarray)
                  !Specify how to increase the size of the icolarray.  Adding 1
                  !will be most memory conservative, but may be a little slower
                  !due to frequent allocate/deallocate.  Another option would be
                  !to double the size: maxnnz=maxnnz*2
                  maxnnz=maxnnz+1
                  allocate(thisrow%icolarray(maxnnz))
                  thisrow%icolarray(1:thisrow%nnz)=iwk(1:thisrow%nnz)
                  thisrow%icolarray(thisrow%nnz+1:maxnnz)=0
              endif
              if(inodup==1) then
                  do jj=1,thisrow%nnz
                      if(thisrow%icolarray(jj)==j) return
                  enddo
              endif
              !add the connection to end
              thisrow%nnz=thisrow%nnz+1
              thisrow%icolarray(thisrow%nnz)=j
              iadded=1
          end subroutine insert

          subroutine sort(this)
              !sort the icolarray for each row, but do not include
              !the diagonal position in the sort so that it stays in front
              implicit none
              class(sparsematrix), intent(inout) :: this
              integer(I4B) :: i,nval
              do i=1,this%nrow
                  nval=this%row(i)%nnz
                  call sortintarray(nval-1, &
                      this%row(i)%icolarray(2:nval)) 
              enddo
          end subroutine sort

          subroutine sortintarray(nval,iarray)
              !simple subroutine for sorting an array
              !in place.  It is not the fastest sort function
              !but should suffice for relatively short nodelists.
              implicit none
              integer(I4B),intent(in) :: nval
              integer(I4B),intent(inout),dimension(nval) :: iarray
              integer(I4B) :: i,j,itemp
              do i=1,nval-1
                  do j=i+1,nval
                      if(iarray(i)>iarray(j)) then
                          itemp=iarray(j)
                          iarray(j)=iarray(i)
                          iarray(i)=itemp
                      endif
                  enddo
              enddo
          end subroutine sortintarray

      end module SparseModule