module Xt3dModule
  
  use KindModule,              only: DP, I4B
  use ConstantsModule,         only: DZERO, DHALF, DONE, LENORIGIN
  use BaseDisModule,           only: DisBaseType
  
  implicit none

  public Xt3dType
  public :: xt3d_cr
  
  type Xt3dType
    integer(I4B), pointer                           :: inunit      => null()
    integer(I4B), pointer                           :: iout        => null()
    character(len=LENORIGIN), pointer               :: origin      => null()     !origin name of this package (e.g. 'GWF_1 NPF')
    integer(I4B), dimension(:), pointer, contiguous :: ibound      => null()     !pointer to model ibound
    integer(I4B),dimension(:), pointer, contiguous  :: iax         => null()     !ia array for extended neighbors used by xt3d
    integer(I4B),dimension(:), pointer, contiguous  :: jax         => null()     !ja array for extended neighbors used by xt3d
    integer(I4B),dimension(:), pointer, contiguous  :: idxglox     => null()     !mapping array for extended neighbors used by xt3d
    integer(I4B), pointer                           :: numextnbrs  => null()     !dimension of jax array
    integer(I4B), pointer                           :: ixt3d       => null()     !xt3d flag (0 is off, 1 is lhs, 2 is rhs)
    logical, pointer                                :: nozee       => null()     !nozee flag
    real(DP), pointer                               :: vcthresh    => null()     !attenuation function threshold
    real(DP), dimension(:,:), pointer, contiguous   :: rmatck      => null()     !rotation matrix for the conductivity tensor
    real(DP), dimension(:,:), pointer, contiguous   :: vecc        => null()     !connection vectors
    real(DP), dimension(:,:), pointer, contiguous   :: vecn        => null()     !interface normals
    real(DP), dimension(:), pointer, contiguous     :: conlen      => null()     !direct connection lengths
    real(DP), dimension(:), pointer, contiguous     :: qsat        => null()     !saturated flow saved for Newton
    real(DP), dimension(:), pointer, contiguous     :: qrhs        => null()     !rhs part of flow saved for Newton
    integer(I4B), pointer                           :: nbrmax      => null()     !maximum number of neighbors for any cell
    real(DP), dimension(:), pointer, contiguous     :: amatpc      => null()     !saved contributions to amat from permanently confined connections, direct neighbors
    real(DP), dimension(:), pointer, contiguous     :: amatpcx     => null()     !saved contributions to amat from permanently confined connections, extended neighbors
    integer(I4B), dimension(:), pointer, contiguous :: iallpc      => null()     !indicates for each node whether all connections processed by xt3d are permanently confined (0 no, 1 yes)
    logical, pointer                                :: lamatsaved  => null()     !indicates whether amat has been saved for permanently confined connections
    class(DisBaseType), pointer                     :: dis         => null()     !discretization object
    ! pointers to npf variables
    real(DP), dimension(:), pointer, contiguous     :: k11         => null()     !horizontal hydraulic conductivity
    real(DP), dimension(:),pointer, contiguous      :: k22         => null()     !minor axis of horizontal hydraulic conductivity ellipse
    real(DP), dimension(:), pointer, contiguous     :: k33         => null()     !vertical hydraulic conductivity
    integer(I4B), pointer                           :: ik22        => null()     !flag indicates K22 was read
    integer(I4B), pointer                           :: ik33        => null()     !flag indicates K33 was read
    real(DP), dimension(:), pointer, contiguous     :: sat         => null()     !saturation (0. to 1.) for each cell
    integer(I4B), pointer                           :: inewton     => null()     !Newton flag
    integer(I4B), dimension(:), pointer, contiguous :: icelltype   => null()     !cell type (confined or unconfined)
    integer(I4B), pointer                           :: iangle1     => null()     !flag to indicate angle1 was read
    integer(I4B), pointer                           :: iangle2     => null()     !flag to indicate angle2 was read
    integer(I4B), pointer                           :: iangle3     => null()     !flag to indicate angle3 was read
    real(DP), dimension(:), pointer, contiguous     :: angle1      => null()     !k ellipse rotation in xy plane around z axis (yaw)
    real(DP), dimension(:), pointer, contiguous     :: angle2      => null()     !k ellipse rotation up from xy plane around y axis (pitch)
    real(DP), dimension(:), pointer, contiguous     :: angle3      => null()     !k tensor rotation around x axis (roll)
    logical, pointer                                :: ldispersion => null()     !flag to indicate dispersion
  contains
    procedure :: xt3d_df
    procedure :: xt3d_ac
    procedure :: xt3d_mc
    procedure :: xt3d_ar
    procedure :: xt3d_fc
    procedure :: xt3d_fcpc
    procedure :: xt3d_fhfb
    procedure :: xt3d_flowjahfb
    procedure :: xt3d_fn
    procedure :: xt3d_flowja
    procedure :: xt3d_da
    procedure, private :: allocate_scalars
    procedure, private :: allocate_arrays
    procedure, private :: xt3d_load
    procedure, private :: xt3d_load_inbr
    procedure, private :: xt3d_indices
    procedure, private :: xt3d_areas
    procedure, private :: xt3d_amat_nbrs
    procedure, private :: xt3d_amatpc_nbrs
    procedure, private :: xt3d_amat_nbrnbrs
    procedure, private :: xt3d_amatpcx_nbrnbrs
    procedure, private :: xt3d_iallpc
    procedure, private :: xt3d_get_iinm
    procedure, private :: xt3d_get_iinmx
    procedure, private :: xt3d_rhs
    procedure, private :: xt3d_fillrmatck
    procedure, private :: xt3d_qnbrs
  end type Xt3dType
  
  contains

    subroutine xt3d_cr(xt3dobj, name_model, inunit, iout, ldispopt)
! ******************************************************************************
! xt3d_cr -- Create a new xt3d object
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    type(Xt3dType), pointer :: xt3dobj
    character(len=*), intent(in) :: name_model
    integer(I4B), intent(in) :: inunit
    integer(I4B), intent(in) :: iout
    logical, optional, intent(in) :: ldispopt
! ------------------------------------------------------------------------------
    !
    ! -- Create the object
    allocate(xt3dobj)
    !
    ! -- Allocate scalars
    call xt3dobj%allocate_scalars(name_model)
    !
    ! -- Set variables
    xt3dobj%inunit = inunit
    xt3dobj%iout   = iout
    if (present(ldispopt)) xt3dobj%ldispersion = ldispopt
    !
    ! -- Return
    return
    end subroutine xt3d_cr

  subroutine xt3d_df(this, dis)
! ******************************************************************************
! xt3d_df -- define the xt3d object
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(Xt3dType) :: this
    class(DisBaseType), pointer, intent(inout) :: dis
! ------------------------------------------------------------------------------
    !
    this%dis => dis
    !
    ! -- Return
    return
  end subroutine xt3d_df
  
  subroutine xt3d_ac(this, moffset, sparse)
! ******************************************************************************
! xt3d_ac -- Add connections for extended neighbors to the sparse matrix
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use SparseModule, only: sparsematrix
    ! -- dummy
    class(Xt3dType) :: this
    integer(I4B), intent(in) :: moffset
    type(sparsematrix), intent(inout) :: sparse
    ! -- local
    integer(I4B) :: i, j, k, jj, kk, iglo, kglo, iadded
! ------------------------------------------------------------------------------
    !
    ! -- If not rhs, add connections
    if (this%ixt3d == 1) then
       ! -- loop over nodes
       do i = 1, this%dis%nodes
         iglo = i + moffset
         ! -- loop over neighbors
         do jj = this%dis%con%ia(i), this%dis%con%ia(i+1) - 1
           j = this%dis%con%ja(jj)
           ! -- loop over neighbors of neighbors
           do kk = this%dis%con%ia(j), this%dis%con%ia(j+1) - 1
             k = this%dis%con%ja(kk)
             kglo = k + moffset
             call sparse%addconnection(iglo, kglo, 1, iadded)
             this%numextnbrs = this%numextnbrs + iadded
            enddo
         enddo
       enddo
    endif
    !
    ! -- Return
    return
  end subroutine xt3d_ac
  
  subroutine xt3d_mc(this, moffset, iasln, jasln, inewton)
! ******************************************************************************
! xt3d_mc -- Map connections and construct iax, jax, and idxglox
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryManagerModule, only: mem_allocate
    ! -- dummy
    class(Xt3dType) :: this
    integer(I4B), intent(in) :: moffset
    integer(I4B), dimension(:), intent(in) :: iasln
    integer(I4B), dimension(:), intent(in) :: jasln
    ! -- local
    integer(I4B) :: i, j, jj, iglo, jglo, jjg, niax, njax, ipos, inewton
    integer(I4B) :: igfirstnod, iglastnod
    logical :: isextnbr
! ------------------------------------------------------------------------------
    !
    ! -- If not rhs, map connections for extended neighbors and construct iax,
    ! -- jax, and idxglox
    if (this%ixt3d == 1) then
      !
      ! -- calculate the first node for the model and the last node in global
      !    numbers
      igfirstnod = moffset + 1
      iglastnod = moffset + this%dis%nodes
      !
      ! -- allocate iax, jax, and idxglox
      niax = this%dis%nodes + 1
      njax = this%numextnbrs ! + 1
      call mem_allocate(this%iax, niax, 'IAX', trim(this%origin))
      call mem_allocate(this%jax, njax, 'JAX', trim(this%origin))
      call mem_allocate(this%idxglox, njax, 'IDXGLOX', trim(this%origin))
      !
      ! -- load first iax entry
      ipos = 1
      this%iax(1) = ipos
      !
      ! -- loop over nodes
      do i = 1, this%dis%nodes
        !
        ! -- calculate global node number
        iglo = i + moffset
        !
        ! -- loop over neighbors in global matrix
        do jjg = iasln(iglo), iasln(iglo + 1) - 1
          !
          ! -- if jglo is in a different model, then it cannot be an extended
          !    neighbor, so skip over it
          jglo = jasln(jjg)
          if (jglo < igfirstnod .or. jglo > iglastnod) then
            cycle
          endif
          !
          ! -- determine whether this neighbor is an extended neighbor
          !    by searching the original neighbors
          isextnbr = .true.
          searchloop: do jj = this%dis%con%ia(i), this%dis%con%ia(i+1) - 1
            j = this%dis%con%ja(jj)
            jglo = j + moffset
            !
            ! -- if an original neighbor, note that and end the search
            if(jglo == jasln(jjg)) then
              isextnbr = .false.
              exit searchloop
            endif
          enddo searchloop
          !
          ! -- if an extended neighbor, add it to jax and idxglox
          if (isextnbr) then
            this%jax(ipos) = jasln(jjg) - moffset
            this%idxglox(ipos) = jjg
            ipos = ipos + 1
          endif
        enddo
        ! -- load next iax entry
        this%iax(i+1) = ipos
      enddo
      !
    else
      !
      call mem_allocate(this%iax, 0, 'IAX', trim(this%origin))
      call mem_allocate(this%jax, 0, 'JAX', trim(this%origin))
      call mem_allocate(this%idxglox, 0, 'IDXGLOX', trim(this%origin))
      !
    endif
    !
    ! -- Return
    return
  end subroutine xt3d_mc
  
  subroutine xt3d_ar(this, ibound, k11, ik33, k33, sat, ik22, k22,             &
    inewton, icelltype, iangle1, iangle2, iangle3, angle1, angle2, angle3)
! ******************************************************************************
! xt3d_ar -- Allocate and Read
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use SimModule, only: store_error, ustop
    ! -- dummy
    class(Xt3dType) :: this
    integer(I4B), dimension(:), pointer, contiguous, intent(inout) :: ibound
    real(DP), dimension(:), intent(in), pointer, contiguous :: k11
    integer(I4B), intent(in), pointer :: ik33
    real(DP), dimension(:), intent(in), pointer, contiguous :: k33
    real(DP), dimension(:), intent(in), pointer, contiguous :: sat
    integer(I4B), intent(in), pointer :: ik22
    real(DP), dimension(:), intent(in), pointer, contiguous :: k22
    integer(I4B), intent(in), pointer :: inewton
    integer(I4B), dimension(:), intent(in), pointer, contiguous :: icelltype
    integer(I4B), intent(in), pointer :: iangle1
    integer(I4B), intent(in), pointer :: iangle2
    integer(I4B), intent(in), pointer :: iangle3
    real(DP), dimension(:), intent(in), pointer, contiguous :: angle1
    real(DP), dimension(:), intent(in), pointer, contiguous :: angle2
    real(DP), dimension(:), intent(in), pointer, contiguous :: angle3
    ! -- local
    integer(I4B) :: n, nnbrs
    ! -- formats
    character(len=*), parameter :: fmtheader =                                 &
      "(1x, /1x, 'XT3D is active.'//)"
    ! -- data
! ------------------------------------------------------------------------------
    !
    ! -- Print a message identifying the xt3d module.
    write(this%iout, fmtheader)
    !
    ! -- Store pointers to arguments that were passed in
    this%ibound  => ibound
    this%k11 => k11
    this%ik33 => ik33
    this%k33 => k33
    this%sat => sat
    this%ik22 => ik22
    this%k22 => k22
    this%inewton => inewton
    this%icelltype => icelltype
    this%iangle1 => iangle1
    this%iangle2 => iangle2
    this%iangle3 => iangle3
    this%angle1 => angle1
    this%angle2 => angle2
    this%angle3 => angle3
    !
    ! -- If angle1 and angle2 were not specified, then there is no z
    !    component in the xt3d formulation for horizontal connections.
    if(this%iangle2 == 0) this%nozee = .true.
    !
    ! -- Determine the maximum number of neighbors for any cell.
    this%nbrmax = 0
    do n = 1, this%dis%nodes
      nnbrs = this%dis%con%ia(n+1) - this%dis%con%ia(n) - 1
      this%nbrmax = max(nnbrs, this%nbrmax)
    end do
    !
    ! -- Check to make sure dis package can calculate connection direction info
    if (this%dis%icondir == 0) then
      call store_error('Error. Vertices not specified for discretization ' // &
        'package, but XT3D is active: '// trim(adjustl(this%origin)) //       &
        '. Vertices must be specified in discretization package in order ' // &
        'to use XT3D.')
      call ustop()
    endif
    !
    ! -- Check to make sure ANGLEDEGX is available for interface normals
    if (this%dis%con%ianglex == 0) then
      call store_error('Error. ANGLDEGX is not specified in the DIS ' // &
        'package, but XT3D is active: '// trim(adjustl(this%origin)) //       &
        '. ANGLDEGX must be provided in discretization package in order ' // &
        'to use XT3D.')
      call ustop()
    endif
    !
    ! -- allocate arrays
    call this%allocate_arrays()
    !
    ! -- If not Newton and not rhs, calculate amatpc and amatpcx for permanently
    ! -- confined connections
    if(this%lamatsaved) call this%xt3d_fcpc(this%dis%nodes)
    !
    ! -- Return
    return
  end subroutine xt3d_ar

  subroutine xt3d_fc(this, kiter, njasln, amat, idxglo, rhs, hnew)
! ******************************************************************************
! xt3d_fc -- Formulate
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use ConstantsModule, only: DONE
    use Xt3dAlgorithmModule, only: qconds
    ! -- dummy
    class(Xt3dType) :: this
    integer(I4B) :: kiter
    integer(I4B),intent(in) :: njasln
    real(DP),dimension(njasln),intent(inout) :: amat
    integer(I4B),intent(in),dimension(:) :: idxglo
    real(DP),intent(inout),dimension(:) :: rhs
    real(DP),intent(inout),dimension(:) :: hnew
    ! -- local
    integer(I4B) :: nodes, nja
    integer(I4B) :: n, m, ipos
    !
    logical :: allhc0, allhc1
    integer(I4B) :: nnbr0, nnbr1
    integer(I4B) :: il0, ii01, jjs01, il01, il10, ii00, ii11, ii10
    integer(I4B) :: i
    integer(I4B),dimension(this%nbrmax) :: inbr0, inbr1
    real(DP) :: ar01, ar10
    real(DP),dimension(this%nbrmax,3) :: vc0, vn0, vc1, vn1
    real(DP),dimension(this%nbrmax) :: dl0, dl0n, dl1, dl1n
    real(DP),dimension(3,3) :: ck0, ck1
    real(DP) :: chat01
    real(DP),dimension(this%nbrmax) :: chati0, chat1j
    real(DP) :: qnm, qnbrs
! ------------------------------------------------------------------------------
    !
    ! -- Calculate xt3d conductance-like coefficients and put into amat and rhs
    ! -- as appropriate
    !
    nodes = this%dis%nodes
    nja = this%dis%con%nja
    if (this%lamatsaved) then
      do i = 1, this%dis%con%nja
        amat(idxglo(i)) = amat(idxglo(i)) + this%amatpc(i)
      end do
      do i = 1, this%numextnbrs
        amat(this%idxglox(i)) = amat(this%idxglox(i)) + this%amatpcx(i)
      end do
    end if
    !
    do n = 1, nodes
      ! -- Skip if inactive.
      if (this%ibound(n).eq.0) cycle
      ! -- Skip if all connections are permanently confined
      if (this%lamatsaved) then
        if (this%iallpc(n) == 1) cycle
      end if
      nnbr0 = this%dis%con%ia(n+1) - this%dis%con%ia(n) - 1
      ! -- Load conductivity and connection info for cell 0.
      call this%xt3d_load(nodes, n, nnbr0, inbr0, vc0, vn0, dl0, dl0n,   &
        ck0, allhc0)
      ! -- Loop over active neighbors of cell 0 that have a higher
      ! -- cell number (taking advantage of reciprocity).
      do il0 = 1,nnbr0
        ipos = this%dis%con%ia(n) + il0
        if (this%dis%con%mask(ipos) == 0) cycle
        
        m = inbr0(il0)
        ! -- Skip if neighbor is inactive or has lower cell number.
        if ((m.eq.0).or.(m.lt.n)) cycle
        nnbr1 = this%dis%con%ia(m+1) - this%dis%con%ia(m) - 1
         ! -- Load conductivity and connection info for cell 1.
        call this%xt3d_load(nodes, m, nnbr1, inbr1, vc1, vn1, dl1, dl1n,    &
          ck1, allhc1)
        ! -- Set various indices.
        call this%xt3d_indices(n, m, il0, ii01, jjs01, il01, il10,          &
          ii00, ii11, ii10)
        ! -- Compute areas.
        if (this%inewton /= 0) then
          ar01 = DONE
          ar10 = DONE
        else
          call this%xt3d_areas(nodes, n, m, jjs01, .false., ar01, ar10, hnew)
        end if
        ! -- Compute "conductances" for interface between
        ! -- cells 0 and 1.
        call qconds(this%nbrmax, nnbr0, inbr0, il01, vc0, vn0, dl0, dl0n,    &
          ck0, nnbr1, inbr1, il10, vc1, vn1, dl1, dl1n, ck1, ar01, ar10,     &
          this%vcthresh, allhc0, allhc1, chat01, chati0, chat1j)
        ! -- If Newton, compute and save saturated flow, then scale
        ! -- conductance-like coefficients by the actual area for
        ! -- subsequent amat and rhs assembly.
        if (this%inewton /= 0) then
          ! -- Contribution to flow from primary connection.
          qnm = chat01*(hnew(m) - hnew(n))
          ! -- Contribution from immediate neighbors of node 0.
          call this%xt3d_qnbrs(nodes, n, m, nnbr0, inbr0, chati0, hnew, qnbrs)
          qnm = qnm + qnbrs
          ! -- Contribution from immediate neighbors of node 1.
          call this%xt3d_qnbrs(nodes, m, n, nnbr1, inbr1, chat1j, hnew, qnbrs)
          qnm = qnm - qnbrs
          ! -- Multiply by saturated area and save in qsat.
          call this%xt3d_areas(nodes, n, m, jjs01, .true., ar01, ar10, hnew)
          this%qsat(ii01) = qnm*ar01
          ! -- Scale coefficients by actual area.  If RHS
          ! -- formulation, also compute and save qrhs.
          call this%xt3d_areas(nodes, n, m, jjs01, .false., ar01, ar10, hnew)
          if (this%ixt3d == 2) then
            this%qrhs(ii01) = -qnbrs*ar01
          end if
          chat01 = chat01*ar01
          chati0 = chati0*ar01
          chat1j = chat1j*ar01
        end if
        ! -- Contribute to rows for cells 0 and 1.
        amat(idxglo(ii00)) = amat(idxglo(ii00)) - chat01
        amat(idxglo(ii01)) = amat(idxglo(ii01)) + chat01
        amat(idxglo(ii11)) = amat(idxglo(ii11)) - chat01
        amat(idxglo(ii10)) = amat(idxglo(ii10)) + chat01
        if (this%ixt3d == 1) then
           call this%xt3d_amat_nbrs(nodes, n, ii00, nnbr0, nja, njasln,        &
             inbr0, amat, idxglo, chati0)
           call this%xt3d_amat_nbrnbrs(nodes, n, m, ii01, nnbr1, nja, njasln,  &
             inbr1, amat, idxglo, chat1j)
           call this%xt3d_amat_nbrs(nodes, m, ii11, nnbr1, nja, njasln,        &
             inbr1, amat, idxglo, chat1j)
           call this%xt3d_amat_nbrnbrs(nodes, m, n, ii10, nnbr0, nja, njasln,  &
             inbr0, amat, idxglo, chati0)
        else
           call this%xt3d_rhs(nodes, n, m, nnbr0, inbr0, chati0, hnew, rhs)
           call this%xt3d_rhs(nodes, m, n, nnbr1, inbr1, chat1j, hnew, rhs)
        endif
        !
      enddo
    enddo
    !
    ! -- Return
    return
  end subroutine xt3d_fc

  subroutine xt3d_fcpc(this, nodes)
! ******************************************************************************
! xt3d_fcpc -- Formulate for permanently confined connections and save in
!              amatpc and amatpcx
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use ConstantsModule, only: DONE
    use Xt3dAlgorithmModule, only: qconds
    ! -- dummy
    class(Xt3dType) :: this
    integer(I4B) :: nodes
    ! -- local
    integer(I4B) :: n, m, ipos
    !
    logical :: allhc0, allhc1
    integer(I4B) :: nnbr0, nnbr1
    integer(I4B) :: il0, ii01, jjs01, il01, il10, ii00, ii11, ii10
    integer(I4B),dimension(this%nbrmax) :: inbr0, inbr1
    real(DP) :: ar01, ar10
    real(DP),dimension(this%nbrmax,3) :: vc0, vn0, vc1, vn1
    real(DP),dimension(this%nbrmax) :: dl0, dl0n, dl1, dl1n
    real(DP),dimension(3,3) :: ck0, ck1
    real(DP) :: chat01
    real(DP),dimension(this%nbrmax) :: chati0, chat1j
! ------------------------------------------------------------------------------
    !
    ! -- Calculate xt3d conductance-like coefficients for permanently confined
    ! -- connections and put into amatpc and amatpcx as appropriate
    do n = 1, nodes
      ! -- Skip if not iallpc.
      if (this%iallpc(n) == 0) cycle
      nnbr0 = this%dis%con%ia(n+1) - this%dis%con%ia(n) - 1
      ! -- Load conductivity and connection info for cell 0.
      call this%xt3d_load(nodes, n, nnbr0, inbr0, vc0, vn0, dl0, dl0n,     &
         ck0, allhc0)
      ! -- Loop over active neighbors of cell 0 that have a higher
      ! -- cell number (taking advantage of reciprocity).
      do il0 = 1,nnbr0
        ipos = this%dis%con%ia(n) + il0
        if (this%dis%con%mask(ipos) == 0) cycle
        
        m = inbr0(il0)
        ! -- Skip if neighbor has lower cell number.
        if (m.lt.n) cycle
        nnbr1 = this%dis%con%ia(m+1) - this%dis%con%ia(m) - 1
        ! -- Load conductivity and connection info for cell 1.
        call this%xt3d_load(nodes, m, nnbr1, inbr1, vc1, vn1, dl1, dl1n,    &
          ck1, allhc1)
        ! -- Set various indices.
        call this%xt3d_indices(n, m, il0, ii01, jjs01, il01, il10,          &
          ii00, ii11, ii10)
        ! -- Compute confined areas.
        call this%xt3d_areas(nodes, n, m, jjs01, .true., ar01, ar10)
        ! -- Compute "conductances" for interface between
        ! -- cells 0 and 1.
        call qconds(this%nbrmax, nnbr0, inbr0, il01, vc0, vn0, dl0, dl0n,    &
          ck0, nnbr1, inbr1, il10, vc1, vn1, dl1, dl1n, ck1, ar01, ar10,     &
          this%vcthresh, allhc0, allhc1, chat01, chati0, chat1j)
        ! -- Contribute to rows for cells 0 and 1.
        this%amatpc(ii00) = this%amatpc(ii00) - chat01
        this%amatpc(ii01) = this%amatpc(ii01) + chat01
        this%amatpc(ii11) = this%amatpc(ii11) - chat01
        this%amatpc(ii10) = this%amatpc(ii10) + chat01
        call this%xt3d_amatpc_nbrs(nodes, n, ii00, nnbr0, inbr0, chati0)
        call this%xt3d_amatpcx_nbrnbrs(nodes, n, m, ii01, nnbr1, inbr1, chat1j)
        call this%xt3d_amatpc_nbrs(nodes, m, ii11, nnbr1, inbr1, chat1j)
        call this%xt3d_amatpcx_nbrnbrs(nodes, m, n, ii10, nnbr0, inbr0, chati0)
      enddo
    enddo
    !
    ! -- Return
    return
  end subroutine xt3d_fcpc

  subroutine xt3d_fhfb(this, kiter, nodes, nja, njasln, amat, idxglo, rhs, hnew, &
    n, m, condhfb)
! ******************************************************************************
! xt3d_fhfb -- Formulate HFB correction
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use ConstantsModule, only: DONE
    use Xt3dAlgorithmModule, only: qconds
    ! -- dummy
    class(Xt3dType) :: this
    integer(I4B) :: kiter
    integer(I4B),intent(in) :: nodes
    integer(I4B),intent(in) :: nja
    integer(I4B),intent(in) :: njasln
    integer(I4B) :: n, m
    real(DP),dimension(njasln),intent(inout) :: amat
    integer(I4B),intent(in),dimension(nja) :: idxglo
    real(DP),intent(inout),dimension(nodes) :: rhs
    real(DP),intent(inout),dimension(nodes) :: hnew
    real(DP) :: condhfb
    ! -- local
    !
    logical :: allhc0, allhc1
    integer(I4B) :: nnbr0, nnbr1
    integer(I4B) :: il0, ii01, jjs01, il01, il10, ii00, ii11, ii10, il
    integer(I4B),dimension(this%nbrmax) :: inbr0, inbr1
    real(DP) :: ar01, ar10
    real(DP),dimension(this%nbrmax,3) :: vc0, vn0, vc1, vn1
    real(DP),dimension(this%nbrmax) :: dl0, dl0n, dl1, dl1n
    real(DP),dimension(3,3) :: ck0, ck1
    real(DP) :: chat01
    real(DP),dimension(this%nbrmax) :: chati0, chat1j
    real(DP) :: qnm, qnbrs
    real(DP) :: term
! ------------------------------------------------------------------------------
    !
    ! -- Calculate hfb corrections to xt3d conductance-like coefficients and
    ! -- put into amat and rhs as appropriate
    !
    nnbr0 = this%dis%con%ia(n+1) - this%dis%con%ia(n) - 1
    ! -- Load conductivity and connection info for cell 0.
    call this%xt3d_load(nodes, n, nnbr0, inbr0, vc0, vn0, dl0, dl0n,    &
      ck0, allhc0)
    ! -- Find local neighbor number of cell 1.
    do il = 1,nnbr0
      if (inbr0(il).eq.m) then
        il0 = il
        exit
      end if
    end do
    nnbr1 = this%dis%con%ia(m+1) - this%dis%con%ia(m) - 1
    ! -- Load conductivity and connection info for cell 1.
    call this%xt3d_load(nodes, m, nnbr1, inbr1, vc1, vn1, dl1, dl1n,    &
      ck1, allhc1)
    ! -- Set various indices.
    call this%xt3d_indices(n, m, il0, ii01, jjs01, il01, il10,          &
      ii00, ii11, ii10)
    ! -- Compute areas.
    if (this%inewton /= 0) then
      ar01 = DONE
      ar10 = DONE
    else
      call this%xt3d_areas(nodes, n, m, jjs01, .false., ar01, ar10, hnew)
    end if
    ! -- Compute "conductances" for interface between
    ! -- cells 0 and 1.
    call qconds(this%nbrmax, nnbr0, inbr0, il01, vc0, vn0, dl0, dl0n,    &
      ck0, nnbr1, inbr1, il10, vc1, vn1, dl1, dl1n, ck1, ar01, ar10,     &
      this%vcthresh, allhc0, allhc1, chat01, chati0, chat1j)
    ! -- Apply scale factor to compute "conductances" for hfb correction
    if(condhfb > DZERO) then
      term = chat01/(chat01 + condhfb)
    else
      term = -condhfb
    endif
    chat01 = -chat01*term
    chati0 = -chati0*term
    chat1j = -chat1j*term
    ! -- If Newton, compute and save saturated flow, then scale
    ! -- conductance-like coefficients by the actual area for
    ! -- subsequent amat and rhs assembly.
    if (this%inewton /= 0) then
      ! -- Contribution to flow from primary connection.
      qnm = chat01*(hnew(m) - hnew(n))
      ! -- Contribution from immediate neighbors of node 0.
      call this%xt3d_qnbrs(nodes, n, m, nnbr0, inbr0, chati0, hnew, qnbrs)
      qnm = qnm + qnbrs
      ! -- Contribution from immediate neighbors of node 1.
      call this%xt3d_qnbrs(nodes, m, n, nnbr1, inbr1, chat1j, hnew, qnbrs)
      qnm = qnm - qnbrs
      ! -- Multiply by saturated area and add correction to qsat.
      call this%xt3d_areas(nodes, n, m, jjs01, .true., ar01, ar10, hnew)
      this%qsat(ii01) = this%qsat(ii01) + qnm*ar01
      ! -- Scale coefficients by actual area.  If RHS
      ! -- formulation, also compute and add correction to qrhs.
      call this%xt3d_areas(nodes, n, m, jjs01, .false., ar01, ar10, hnew)
      if (this%ixt3d == 2) then
        this%qrhs(ii01) = this%qrhs(ii01) - qnbrs*ar01
      end if
      chat01 = chat01*ar01
      chati0 = chati0*ar01
      chat1j = chat1j*ar01
    end if
    ! -- Contribute to rows for cells 0 and 1.
    amat(idxglo(ii00)) = amat(idxglo(ii00)) - chat01
    amat(idxglo(ii01)) = amat(idxglo(ii01)) + chat01
    amat(idxglo(ii11)) = amat(idxglo(ii11)) - chat01
    amat(idxglo(ii10)) = amat(idxglo(ii10)) + chat01
    if (this%ixt3d == 1) then
       call this%xt3d_amat_nbrs(nodes, n, ii00, nnbr0, nja, njasln,        &
         inbr0, amat, idxglo, chati0)
       call this%xt3d_amat_nbrnbrs(nodes, n, m, ii01, nnbr1, nja, njasln,  &
         inbr1, amat, idxglo, chat1j)
       call this%xt3d_amat_nbrs(nodes, m, ii11, nnbr1, nja, njasln,        &
         inbr1, amat, idxglo, chat1j)
       call this%xt3d_amat_nbrnbrs(nodes, m, n, ii10, nnbr0, nja, njasln,  &
         inbr0, amat, idxglo, chati0)
    else
       call this%xt3d_rhs(nodes, n, m, nnbr0, inbr0, chati0, hnew, rhs)
       call this%xt3d_rhs(nodes, m, n, nnbr1, inbr1, chat1j, hnew, rhs)
    endif
    !
    ! -- Return
    return
  end subroutine xt3d_fhfb

  subroutine xt3d_fn(this, kiter, nodes, nja, njasln, amat, idxglo, rhs, hnew)
! ******************************************************************************
! xt3d_fn -- Fill Newton terms for xt3d
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use ConstantsModule, only: DONE
    use SmoothingModule, only: sQuadraticSaturationDerivative
    ! -- dummy
    class(Xt3dType) :: this
    integer(I4B) :: kiter
    integer(I4B),intent(in) :: nodes
    integer(I4B),intent(in) :: nja
    integer(I4B),intent(in) :: njasln
    real(DP),dimension(njasln),intent(inout) :: amat
    integer(I4B),intent(in),dimension(nja) :: idxglo
    real(DP),intent(inout),dimension(nodes) :: rhs
    real(DP),intent(inout),dimension(nodes) :: hnew
    ! -- local
    integer(I4B) :: n, m, ipos
    !
    integer(I4B) :: nnbr0
    integer(I4B) :: il0, ii01, jjs01, il01, il10, ii00, ii11, ii10
    integer(I4B),dimension(this%nbrmax) :: inbr0
    integer(I4B) :: iups, idn
    real(DP) :: topup, botup, derv, term, termrhs
! ------------------------------------------------------------------------------
    !
    ! -- Update amat and rhs with Newton terms
    do n = 1, nodes
      ! -- Skip if inactive.
      if (this%ibound(n).eq.0) cycle
      ! -- No Newton correction if amat saved (which implies no rhs option)
      ! -- and all connections for the cell are permanently confined.
      if (this%lamatsaved) then
        if (this%iallpc(n) == 1) cycle
      end if
      nnbr0 = this%dis%con%ia(n+1) - this%dis%con%ia(n) - 1
      ! -- Load neighbors of cell. Set cell numbers for inactive
      ! -- neighbors to zero.
      call this%xt3d_load_inbr(n, nnbr0, inbr0)
      ! -- Loop over active neighbors of cell 0 that have a higher
      ! -- cell number (taking advantage of reciprocity).
      do il0 = 1,nnbr0
        ipos = this%dis%con%ia(n) + il0
        if (this%dis%con%mask(ipos) == 0) cycle
        
        m = inbr0(il0)
        ! -- Skip if neighbor is inactive or has lower cell number.
        if ((inbr0(il0).eq.0).or.(m.lt.n)) cycle
        ! -- Set various indices.
        call this%xt3d_indices(n, m, il0, ii01, jjs01, il01, il10,          &
          ii00, ii11, ii10)
        ! determine upstream node
        iups = m
        if (hnew(m) < hnew(n)) iups = n
        idn = n
        if (iups == n) idn = m
        ! -- no Newton terms if upstream cell is confined
        ! -- and no rhs option
        if ((this%icelltype(iups) == 0).and.(this%ixt3d.eq.1)) cycle
        ! -- Set the upstream top and bot, and then recalculate for a
        !    vertically staggered horizontal connection
        topup = this%dis%top(iups)
        botup = this%dis%bot(iups)
        if(this%dis%con%ihc(jjs01) == 2) then
          topup = min(this%dis%top(n), this%dis%top(m))
          botup = max(this%dis%bot(n), this%dis%bot(m))
        endif
        ! derivative term
        derv = sQuadraticSaturationDerivative(topup, botup, hnew(iups))
        term = this%qsat(ii01) * derv
        if (this%ixt3d == 1) then
           termrhs = term
        else
           termrhs = term - this%qrhs(ii01)
        endif
        ! fill Jacobian for n being the upstream node
        if (iups == n) then
          ! fill in row of n
          amat(idxglo(ii00)) = amat(idxglo(ii00)) + term
          rhs(n) = rhs(n) + termrhs * hnew(n)
          ! fill in row of m
          amat(idxglo(ii10)) = amat(idxglo(ii10)) - term
          rhs(m) = rhs(m) - termrhs * hnew(n)
        ! fill Jacobian for m being the upstream node
        else
          ! fill in row of n
          amat(idxglo(ii01)) = amat(idxglo(ii01)) + term
          rhs(n) = rhs(n) + termrhs * hnew(m)
          ! fill in row of m
          amat(idxglo(ii11)) = amat(idxglo(ii11)) - term
          rhs(m) = rhs(m) - termrhs * hnew(m)
        end if
      enddo
    enddo
    !
    ! -- Return
    return
  end subroutine xt3d_fn

  subroutine xt3d_flowja(this, hnew, flowja)
! ******************************************************************************
! xt3d_flowja -- Budget
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use Xt3dAlgorithmModule, only: qconds
    ! -- dummy
    class(Xt3dType) :: this
    real(DP),intent(inout),dimension(:) :: hnew
    real(DP),intent(inout),dimension(:) :: flowja
    ! -- local
    integer(I4B) :: n, ipos, m, nodes
    real(DP) :: qnm, qnbrs
    logical :: allhc0, allhc1
    integer(I4B) :: nnbr0, nnbr1
    integer(I4B) :: il0, ii01, jjs01, il01, il10, ii00, ii11, ii10
    integer(I4B),dimension(this%nbrmax) :: inbr0, inbr1
    real(DP) :: ar01, ar10
    real(DP),dimension(this%nbrmax,3) :: vc0, vn0, vc1, vn1
    real(DP),dimension(this%nbrmax) :: dl0, dl0n, dl1, dl1n
    real(DP),dimension(3,3) :: ck0, ck1
    real(DP) :: chat01
    real(DP),dimension(this%nbrmax) :: chati0, chat1j
! ------------------------------------------------------------------------------
    !
    ! -- Calculate the flow across each cell face and store in flowja
    nodes = this%dis%nodes
    do n = 1, nodes
      ! -- Skip if inactive.
      if (this%ibound(n).eq.0) cycle
      nnbr0 = this%dis%con%ia(n+1) - this%dis%con%ia(n) - 1
      ! -- Load conductivity and connection info for cell 0.
      call this%xt3d_load(nodes, n, nnbr0, inbr0, vc0, vn0, dl0, dl0n,      &
        ck0, allhc0)
      ! -- Loop over active neighbors of cell 0 that have a higher
      ! -- cell number (taking advantage of reciprocity).
      do il0 = 1,nnbr0
        m = inbr0(il0)
        ! -- Skip if neighbor is inactive or has lower cell number.
        if ((inbr0(il0).eq.0).or.(m.lt.n)) cycle
        nnbr1 = this%dis%con%ia(m+1) - this%dis%con%ia(m) - 1
        ! -- Load conductivity and connection info for cell 1.
        call this%xt3d_load(nodes, m, nnbr1, inbr1, vc1, vn1, dl1, dl1n,     &
          ck1, allhc1)
        ! -- Set various indices.
        call this%xt3d_indices(n, m, il0, ii01, jjs01, il01, il10,           &
          ii00, ii11, ii10)
        ! -- Compute areas.
        if (this%inewton /= 0)                                               &
          call this%xt3d_areas(nodes, n, m, jjs01, .true., ar01, ar10, hnew)
        call this%xt3d_areas(nodes, n, m, jjs01, .false., ar01, ar10, hnew)
        ! -- Compute "conductances" for interface between
        ! -- cells 0 and 1.
        call qconds(this%nbrmax, nnbr0, inbr0, il01, vc0, vn0, dl0, dl0n,    &
          ck0, nnbr1, inbr1, il10, vc1, vn1, dl1, dl1n, ck1, ar01, ar10,     &
          this%vcthresh, allhc0, allhc1, chat01, chati0, chat1j)
        ! -- Contribution to flow from primary connection.
        qnm = chat01*(hnew(m) - hnew(n))
        ! -- Contribution from immediate neighbors of node 0.
        call this%xt3d_qnbrs(nodes, n, m, nnbr0, inbr0, chati0, hnew, qnbrs)
        qnm = qnm + qnbrs
        ! -- Contribution from immediate neighbors of node 1.
        call this%xt3d_qnbrs(nodes, m, n, nnbr1, inbr1, chat1j, hnew, qnbrs)
        qnm = qnm - qnbrs
        ipos = ii01
        flowja(ipos) = qnm
        flowja(this%dis%con%isym(ipos)) = -qnm        
      enddo
    enddo
    !
    ! -- Return
    return
  end subroutine xt3d_flowja

  subroutine xt3d_flowjahfb(this, n, m, hnew, flowja, condhfb)
! ******************************************************************************
! xt3d_flowjahfb -- hfb contribution to flowja when xt3d is used
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    use ConstantsModule, only: DONE
    use Xt3dAlgorithmModule, only: qconds
    ! -- dummy
    class(Xt3dType) :: this
    integer(I4B) :: n, m
    real(DP),intent(inout),dimension(:) :: hnew
    real(DP),intent(inout),dimension(:) :: flowja
    real(DP) :: condhfb
    ! -- local
    !
    integer(I4B) :: nodes
    logical :: allhc0, allhc1
!!!    integer(I4B), parameter :: nbrmax = 10
    integer(I4B) :: nnbr0, nnbr1
    integer(I4B) :: il0, ii01, jjs01, il01, il10, ii00, ii11, ii10, il
    integer(I4B),dimension(this%nbrmax) :: inbr0, inbr1
    integer(I4B) :: ipos
    real(DP) :: ar01, ar10
    real(DP),dimension(this%nbrmax,3) :: vc0, vn0, vc1, vn1
    real(DP),dimension(this%nbrmax) :: dl0, dl0n, dl1, dl1n
    real(DP),dimension(3,3) :: ck0, ck1
    real(DP) :: chat01
    real(DP),dimension(this%nbrmax) :: chati0, chat1j
    real(DP) :: qnm, qnbrs
    real(DP) :: term
! ------------------------------------------------------------------------------
    !
    ! -- Calculate hfb corrections to xt3d conductance-like coefficients and
    ! -- put into amat and rhs as appropriate
    !
    nodes = this%dis%nodes
    nnbr0 = this%dis%con%ia(n+1) - this%dis%con%ia(n) - 1
    ! -- Load conductivity and connection info for cell 0.
    call this%xt3d_load(nodes, n, nnbr0, inbr0, vc0, vn0, dl0, dl0n,    &
      ck0, allhc0)
    ! -- Find local neighbor number of cell 1.
    do il = 1,nnbr0
      if (inbr0(il).eq.m) then
        il0 = il
        exit
      end if
    end do
    nnbr1 = this%dis%con%ia(m+1) - this%dis%con%ia(m) - 1
    ! -- Load conductivity and connection info for cell 1.
    call this%xt3d_load(nodes, m, nnbr1, inbr1, vc1, vn1, dl1, dl1n,    &
      ck1, allhc1)
    ! -- Set various indices.
    call this%xt3d_indices(n, m, il0, ii01, jjs01, il01, il10,          &
      ii00, ii11, ii10)
    ! -- Compute areas.
    if (this%inewton /= 0) then
      ar01 = DONE
      ar10 = DONE
    else
      call this%xt3d_areas(nodes, n, m, jjs01, .false., ar01, ar10, hnew)
    end if
    ! -- Compute "conductances" for interface between
    ! -- cells 0 and 1.
    call qconds(this%nbrmax, nnbr0, inbr0, il01, vc0, vn0, dl0, dl0n,    &
      ck0, nnbr1, inbr1, il10, vc1, vn1, dl1, dl1n, ck1, ar01, ar10,     &
      this%vcthresh, allhc0, allhc1, chat01, chati0, chat1j)
    ! -- Apply scale factor to compute "conductances" for hfb correction
    if(condhfb > DZERO) then
      term = chat01/(chat01 + condhfb)
    else
      term = -condhfb
    endif
    chat01 = -chat01*term
    chati0 = -chati0*term
    chat1j = -chat1j*term
    ! -- Contribution to flow from primary connection.
    qnm = chat01*(hnew(m) - hnew(n))
    ! -- Contribution from immediate neighbors of node 0.
    call this%xt3d_qnbrs(nodes, n, m, nnbr0, inbr0, chati0, hnew, qnbrs)
    qnm = qnm + qnbrs
    ! -- Contribution from immediate neighbors of node 1.
    call this%xt3d_qnbrs(nodes, m, n, nnbr1, inbr1, chat1j, hnew, qnbrs)
    qnm = qnm - qnbrs
    ! -- If Newton, scale conductance-like coefficients by the 
    ! -- actual area.
    if (this%inewton /= 0) then
      call this%xt3d_areas(nodes, n, m, jjs01, .true., ar01, ar10, hnew)
      call this%xt3d_areas(nodes, n, m, jjs01, .false., ar01, ar10, hnew)
      qnm = qnm*ar01
    end if
    ipos = ii01
    flowja(ipos) = flowja(ipos) + qnm
    flowja(this%dis%con%isym(ipos)) = flowja(this%dis%con%isym(ipos)) - qnm
    !
    ! -- Return
    return
  end subroutine xt3d_flowjahfb

  subroutine xt3d_da(this)
! ******************************************************************************
! xt3d_da -- Deallocate variables
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryManagerModule, only: mem_deallocate
    ! -- dummy
    class(Xt3dType) :: this
! ------------------------------------------------------------------------------
    !  
    ! -- Deallocate arrays
    if (this%ixt3d /= 0) then
      call mem_deallocate(this%iax)
      call mem_deallocate(this%jax)
      call mem_deallocate(this%idxglox)
      call mem_deallocate(this%rmatck)
      call mem_deallocate(this%vecc)
      call mem_deallocate(this%conlen)
      call mem_deallocate(this%vecn)
      call mem_deallocate(this%qsat)
      call mem_deallocate(this%qrhs)
      call mem_deallocate(this%amatpc)
      call mem_deallocate(this%amatpcx)
      call mem_deallocate(this%iallpc)
    endif
    !
    ! -- Strings
    deallocate(this%origin)
    !
    ! -- Scalars
    call mem_deallocate(this%ixt3d)
    call mem_deallocate(this%inunit)
    call mem_deallocate(this%iout)
    call mem_deallocate(this%numextnbrs)
    call mem_deallocate(this%nozee)
    call mem_deallocate(this%vcthresh)
    call mem_deallocate(this%lamatsaved)
    call mem_deallocate(this%nbrmax)
    call mem_deallocate(this%ldispersion)
    !
    ! -- Return
    return
  end subroutine xt3d_da

  subroutine allocate_scalars(this, name_model)
! ******************************************************************************
! allocate_scalars -- Allocate scalar pointer variables
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryManagerModule, only: mem_allocate
    ! -- dummy
    class(Xt3dType) :: this
    character(len=*) :: name_model
! ------------------------------------------------------------------------------
    !
    ! -- Allocate and assign origin
    allocate(this%origin)
    this%origin = trim(adjustl(name_model)) // ' XT3D'
    !
    ! -- Allocate scalars
    call mem_allocate(this%ixt3d, 'IXT3D', this%origin)
    call mem_allocate(this%nbrmax, 'NBRMAX', this%origin)
    call mem_allocate(this%inunit, 'INUNIT', this%origin)
    call mem_allocate(this%iout, 'IOUT', this%origin)
    call mem_allocate(this%numextnbrs, 'NUMEXTNBRS', this%origin)
    call mem_allocate(this%nozee, 'NOZEE', this%origin)
    call mem_allocate(this%vcthresh, 'VCTHRESH', this%origin)
    call mem_allocate(this%lamatsaved, 'LAMATSAVED', this%origin)
    call mem_allocate(this%ldispersion, 'LDISPERSION', this%origin)
    !  
    ! -- Initialize value
    this%ixt3d = 0
    this%nbrmax = 0
    this%inunit = 0
    this%iout = 0
    this%numextnbrs = 0
    this%nozee = .false.
    this%vcthresh = 1.d-10
    this%lamatsaved = .false.
    this%ldispersion = .false.
    !
    ! -- Return
    return
  end subroutine allocate_scalars

  subroutine allocate_arrays(this)
! ******************************************************************************
! allocate_arrays -- Allocate xt3d arrays
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryManagerModule, only: mem_allocate
    ! -- dummy
    class(Xt3dType) :: this
    ! -- local
    integer(I4B) :: njax
! ------------------------------------------------------------------------------
    !
    call mem_allocate(this%rmatck, 3, 3, 'RMATCK', this%origin)
    !
    if (this%inewton /= 0) then
      call mem_allocate(this%qsat, this%dis%nja, 'QSAT', this%origin)
      if (this%ixt3d == 1) then
        call mem_allocate(this%qrhs, 0, 'QRHS', this%origin)
      else
        call mem_allocate(this%qrhs, this%dis%nja, 'QRHS', this%origin)
      end if
      call mem_allocate(this%amatpc, 0, 'AMATPC', this%origin)
      call mem_allocate(this%amatpcx, 0, 'AMATPCX', this%origin)
      call mem_allocate(this%iallpc, 0, 'IALLPC', this%origin)
    else
      call mem_allocate(this%qsat, 0, 'QSAT', this%origin)
      call mem_allocate(this%qrhs, 0, 'QRHS', this%origin)
    end if
    !
    call this%xt3d_iallpc()
    !
    if (this%lamatsaved) then
      call mem_allocate(this%amatpc, this%dis%nja, 'AMATPC', this%origin)
      njax = this%numextnbrs ! + 1
      call mem_allocate(this%amatpcx, njax, 'AMATPCX', this%origin)
    else
      call mem_allocate(this%amatpc, 0, 'AMATPC', this%origin)
      call mem_allocate(this%amatpcx, 0, 'AMATPCX', this%origin)
    end if
    call mem_allocate(this%vecc, 0, 3, 'VECC', this%origin)
    call mem_allocate(this%conlen, 0, 'CONLEN', this%origin)
    call mem_allocate(this%vecn, 0, 3, 'VECN', this%origin)
    !
    this%rmatck = 0.d0
    if (this%inewton /= 0) then
      this%qsat = 0.d0
      if (this%ixt3d == 2) this%qrhs = 0.d0
    else if (this%lamatsaved) then
      this%amatpc = 0.d0
      this%amatpcx = 0.d0
    end if
    this%vecc = 0.d0
    this%conlen = 0.d0
    this%vecn = 0.d0
    !
    ! -- Return
    return
  end subroutine allocate_arrays

  subroutine xt3d_iallpc(this)
! ******************************************************************************
! xt3d_iallpc -- Allocate and populate iallpc array. Set lamatsaved.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryManagerModule, only: mem_allocate, mem_deallocate
    ! -- dummy
    class(Xt3dType) :: this
    ! -- local
    integer(I4B) :: n, m, mm, il0, il1
    integer(I4B) :: nnbr0, nnbr1
    integer(I4B),dimension(this%nbrmax) :: inbr0, inbr1
! ------------------------------------------------------------------------------
    !
    if(this%ixt3d == 2 .or. this%ldispersion) then
      this%lamatsaved = .false.
      call mem_allocate(this%iallpc, 0, 'IALLPC', this%origin)
    else
      call mem_allocate(this%iallpc, this%dis%nodes, 'IALLPC', this%origin)
      this%iallpc = 1
      do n = 1, this%dis%nodes
        if (this%icelltype(n) /= 0) then
          this%iallpc(n) = 0
          cycle
        end if
        nnbr0 = this%dis%con%ia(n+1) - this%dis%con%ia(n) - 1
        call this%xt3d_load_inbr(n, nnbr0, inbr0)
        do il0 = 1,nnbr0
          m = inbr0(il0)
          if (m.lt.n) cycle
          if (this%icelltype(m) /= 0) then
            this%iallpc(n) = 0
            this%iallpc(m) = 0
            cycle
          end if
          nnbr1 = this%dis%con%ia(m+1) - this%dis%con%ia(m) - 1
          call this%xt3d_load_inbr(m, nnbr1, inbr1)
          do il1 = 1,nnbr1
            mm = inbr1(il1)
!!!            if (mm.lt.m) cycle
            if (this%icelltype(mm) /= 0) then
              this%iallpc(n) = 0
              this%iallpc(m) = 0
              this%iallpc(mm) = 0
            end if
          enddo
        enddo
      enddo
      this%lamatsaved = .false.
      do n = 1, this%dis%nodes
        if (this%iallpc(n) == 1) then
          this%lamatsaved = .true.
          exit
        end if
      enddo
    end if
    !
    if (.not.this%lamatsaved) then
      call mem_deallocate(this%iallpc)       ! kluge: ok to do this???
      call mem_allocate(this%iallpc, 0, 'IALLPC', this%origin)
    end if
    !
    ! -- Return
    return
  end subroutine xt3d_iallpc
  
  subroutine xt3d_indices(this, n, m, il0, ii01, jjs01, il01, il10,          &
    ii00, ii11, ii10)
! ******************************************************************************
! xt3d_indices -- Set various indices for XT3D.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- module
    ! -- dummy
    class(Xt3dType) :: this
    integer(I4B) :: n, m, il0, ii01, jjs01, il01, il10, ii00, ii11, ii10
    ! -- local
    integer(I4B) :: iinm
! ------------------------------------------------------------------------------
    !
    ! -- Set local number of node 0-1 connection (local cell number of cell 1
    ! -- in cell 0's neighbor list).
    il01 = il0
    ! -- Set local number of node 1-0 connection (local cell number of cell 0
    ! -- in cell 1's neighbor list).
    call this%xt3d_get_iinm(m, n, iinm)
    il10 = iinm - this%dis%con%ia(m)
    ! -- Set index of node 0 diagonal in the ja array.
    ii00 = this%dis%con%ia(n)
    ! -- Set index of node 0-1 connection in the ja array.
    ii01 = ii00 + il01
    ! -- Set symmetric index of node 0-1 connection.
    jjs01 = this%dis%con%jas(ii01)
    ! -- Set index of node 1 diagonal in the ja array.
    ii11 = this%dis%con%ia(m)
    ! -- Set index of node 1-0 connection in the ja array.
    ii10 = ii11 + il10
    !
    return
  end subroutine xt3d_indices

  subroutine xt3d_load(this, nodes, n, nnbr, inbr, vc, vn, dl, dln, ck, allhc)
! ******************************************************************************
! xt3d_load -- Load conductivity and connection info for a cell into arrays
!              used by XT3D.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- module
    use ConstantsModule, only: DZERO, DHALF, DONE
    ! -- dummy
    class(Xt3dType) :: this
    logical :: allhc
    integer(I4B),intent(in) :: nodes
    integer(I4B) :: n, nnbr
    integer(I4B),dimension(this%nbrmax) :: inbr
    real(DP),dimension(this%nbrmax,3) :: vc, vn
    real(DP),dimension(this%nbrmax) :: dl, dln
    real(DP),dimension(3,3) :: ck
    ! -- local
    integer(I4B) :: il, ii, jj, jjs
    integer(I4B) :: ihcnjj
    real(DP) :: satn, satjj
    real(DP) :: cl1njj, cl2njj, dltot, ooclsum
! ------------------------------------------------------------------------------
    !
    ! -- Set conductivity tensor for cell.
    ck = DZERO
    ck(1,1) = this%k11(n)
    if(this%ik22 == 0) then
      ck(2,2) = ck(1,1)
    else
      ck(2,2) = this%k22(n)
    end if
    if(this%ik33 == 0) then
      ck(3,3) = ck(1,1)
    else
      ck(3,3) = this%k33(n)
    endif
    call this%xt3d_fillrmatck(n)
    ck = matmul(this%rmatck, ck)
    ck = matmul(ck, transpose(this%rmatck))
    !
    ! -- Load neighbors of cell. Set cell numbers for inactive
    ! -- neighbors to zero so xt3d knows to ignore them. Compute
    ! -- direct connection lengths from perpendicular connection
    ! -- lengths. Also determine if all active connections are
    ! -- horizontal.
    allhc = .true.
    do il = 1,nnbr
      ii = il + this%dis%con%ia(n)
      jj = this%dis%con%ja(ii)
      jjs = this%dis%con%jas(ii)
      if (this%ibound(jj).ne.0) then
        inbr(il) = jj
        satn = this%sat(n)
        satjj = this%sat(jj)
        ! -- DISV and DIS
        ihcnjj = this%dis%con%ihc(jjs)
        call this%dis%connection_normal(n, jj, ihcnjj,                     &
          vn(il, 1), vn(il, 2), vn(il, 3), ii)
        call this%dis%connection_vector(n, jj, this%nozee, satn, satjj,    &
          ihcnjj, vc(il, 1), vc(il, 2), vc(il, 3), dltot)
        if(jj > n) then
          cl1njj = this%dis%con%cl1(jjs)
          cl2njj = this%dis%con%cl2(jjs)
        else
          cl1njj = this%dis%con%cl2(jjs)
          cl2njj = this%dis%con%cl1(jjs)
        endif
        ooclsum = 1d0/(cl1njj + cl2njj)
        dl(il) = dltot*cl1njj*ooclsum
        dln(il) = dltot*cl2njj*ooclsum
        if (this%dis%con%ihc(jjs).eq.0) allhc = .false.
      else
        inbr(il) = 0
      end if
    end do
    !
    return
  end subroutine xt3d_load
  
  subroutine xt3d_load_inbr(this, n, nnbr, inbr)
! ******************************************************************************
! xt3d_load_inbr -- Load neighbor list for a cell.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- module
    ! -- dummy
    class(Xt3dType) :: this
    integer(I4B) :: n, nnbr
    integer(I4B),dimension(this%nbrmax) :: inbr
    ! -- local
    integer(I4B) :: il, ii, jj
! ------------------------------------------------------------------------------
    !
    ! -- Load neighbors of cell. Set cell numbers for inactive
    ! -- neighbors to zero so xt3d knows to ignore them.
    do il = 1,nnbr
      ii = il + this%dis%con%ia(n)
      jj = this%dis%con%ja(ii)
      if (this%ibound(jj).ne.0) then
        inbr(il) = jj
      else
        inbr(il) = 0
      end if
    end do
    !
    return
  end subroutine xt3d_load_inbr

  subroutine xt3d_areas(this, nodes, n, m, jjs01, lsat, ar01, ar10, hnew)
! ******************************************************************************
! xt3d_areas -- Compute interfacial areas.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- module
    use ConstantsModule, only: DZERO, DONE
    use SmoothingModule, only: sQuadraticSaturation                     ! kluge debug
    ! -- dummy
    class(Xt3dType) :: this
    logical :: lsat
    integer(I4B) :: nodes, n, m, jjs01
    real(DP) :: ar01, ar10
    real(DP),intent(inout),dimension(:), optional :: hnew
    ! -- local
    real(DP) :: topn, botn, topm, botm, thksatn, thksatm
    real(DP) :: sill_top, sill_bot, tpn, tpm
    real(DP) :: hn, hm                                          ! kluge debug
    real(DP) :: satups
! ------------------------------------------------------------------------------
    !
    ! -- Compute area depending on connection type
    if (this%dis%con%ihc(jjs01).eq.0) then
      ! -- vertical connection
      ar01 = this%dis%con%hwva(jjs01)
      ar10 = ar01
    else if (this%inewton /= 0) then
      if (lsat) then
        topn = this%dis%top(n)
        botn = this%dis%bot(n)
        topm = this%dis%top(m)
        botm = this%dis%bot(m)
        thksatn = topn - botn
        thksatm = topm - botm
        if (this%dis%con%ihc(jjs01).eq.2) then
          ! -- vertically staggered
          sill_top = min(topn, topm)
          sill_bot = max(botn, botm)
          tpn = botn + thksatn
          tpm = botm + thksatm
          thksatn = max(min(tpn, sill_top) - sill_bot, DZERO)
          thksatm = max(min(tpm, sill_top) - sill_bot, DZERO)
        end if
        ar01 = this%dis%con%hwva(jjs01)*DHALF*(thksatn + thksatm)
      else
        ! -- If Newton and lsat=.false., it is assumed that the fully saturated
        ! -- areas have already been calculated and are being passed in through
        ! -- ar01 and ar10. The actual areas are obtained simply by scaling by
        ! -- the upstream saturation.
        if (hnew(m) < hnew(n)) then
          if (this%icelltype(n) == 0) then
            satups = DONE
          else
            topn = this%dis%top(n)
            botn = this%dis%bot(n)
            hn = hnew(n)                                           ! kluge
            satups = sQuadraticSaturation(topn, botn, hn)
          end if
        else
          if (this%icelltype(m) == 0) then
            satups = DONE
          else
            topm = this%dis%top(m)
            botm = this%dis%bot(m)
            hm = hnew(m)                                           ! kluge
            satups = sQuadraticSaturation(topm, botm, hm)
          end if
        end if
        ar01 = ar01*satups
      end if
      ar10 = ar01
    else
      topn = this%dis%top(n)
      botn = this%dis%bot(n)
      topm = this%dis%top(m)
      botm = this%dis%bot(m)
      if (lsat.or.(this%icelltype(n) == 0)) then
        thksatn = topn - botn
      else
        thksatn = this%sat(n)*(topn - botn)
      end if
      if (lsat.or.(this%icelltype(m) == 0)) then
        thksatm = topm - botm
      else
        thksatm = this%sat(m)*(topm - botm)
      end if
      if (this%dis%con%ihc(jjs01).eq.2) then
        ! -- vertically staggered
        sill_top = min(topn, topm)
        sill_bot = max(botn, botm)
        tpn = botn + thksatn
        tpm = botm + thksatm
        thksatn = max(min(tpn, sill_top) - sill_bot, DZERO)
        thksatm = max(min(tpm, sill_top) - sill_bot, DZERO)
      end if
      ar01 = this%dis%con%hwva(jjs01)*thksatn
      ar10 = this%dis%con%hwva(jjs01)*thksatm
    endif
    !
    return
  end subroutine xt3d_areas

  subroutine xt3d_amat_nbrs(this, nodes, n, idiag, nnbr, nja,          &
      njasln, inbr, amat, idxglo, chat)
! ******************************************************************************
! xt3d_amat_nbrs -- Add contributions from neighbors to amat.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- module
    ! -- dummy
    class(Xt3dType) :: this
    integer(I4B),intent(in) :: nodes
    integer(I4B) :: n, idiag, nnbr, nja, njasln
    integer(I4B),dimension(this%nbrmax) :: inbr
    integer(I4B),intent(in),dimension(nja) :: idxglo
    real(DP),dimension(njasln),intent(inout) :: amat
    real(DP),dimension(this%nbrmax) :: chat
    ! -- local
    integer(I4B) :: iil, iii
! ------------------------------------------------------------------------------
    !
    do iil = 1,nnbr
      if (inbr(iil).ne.0) then
        iii = this%dis%con%ia(n) + iil
        amat(idxglo(idiag)) = amat(idxglo(idiag)) - chat(iil)
        amat(idxglo(iii)) = amat(idxglo(iii)) + chat(iil)            
      endif
    enddo
    !
    return
  end subroutine xt3d_amat_nbrs

  subroutine xt3d_amat_nbrnbrs(this, nodes, n, m, ii01, nnbr, nja,   &
      njasln, inbr, amat, idxglo, chat)
! ******************************************************************************
! xt3d_amat_nbrnbrs -- Add contributions from neighbors of neighbor to amat.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- module
    ! -- dummy
    class(Xt3dType) :: this
    integer(I4B),intent(in) :: nodes
    integer(I4B) :: n, m, ii01, nnbr, nja, njasln
    integer(I4B),dimension(this%nbrmax) :: inbr
    integer(I4B),intent(in),dimension(nja) :: idxglo
    real(DP),dimension(njasln),intent(inout) :: amat
    real(DP),dimension(this%nbrmax) :: chat
    ! -- local
    integer(I4B) :: iil, iii, jjj, iixjjj, iijjj
! ------------------------------------------------------------------------------
    !
    do iil = 1,nnbr
      if (inbr(iil).ne.0) then
        amat(idxglo(ii01)) = amat(idxglo(ii01)) + chat(iil)
        iii = this%dis%con%ia(m) + iil
        jjj = this%dis%con%ja(iii)
        call this%xt3d_get_iinmx(n, jjj, iixjjj)
        if (iixjjj.ne.0) then
          amat(this%idxglox(iixjjj)) = amat(this%idxglox(iixjjj)) - chat(iil)
        else
          call this%xt3d_get_iinm(n, jjj, iijjj)
          amat(idxglo(iijjj)) = amat(idxglo(iijjj)) - chat(iil)
        endif
      endif
    enddo
    !
    return
  end subroutine xt3d_amat_nbrnbrs

  subroutine xt3d_amatpc_nbrs(this, nodes, n, idiag, nnbr, inbr, chat)
! ******************************************************************************
! xt3d_amatpc_nbrs -- Add contributions from neighbors to amatpc.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- module
    ! -- dummy
    class(Xt3dType) :: this
    integer(I4B),intent(in) :: nodes
    integer(I4B) :: n, idiag, nnbr
    integer(I4B),dimension(this%nbrmax) :: inbr
    real(DP),dimension(this%nbrmax) :: chat
    ! -- local
    integer(I4B) :: iil, iii
! ------------------------------------------------------------------------------
    !
    do iil = 1,nnbr
      iii = this%dis%con%ia(n) + iil
      this%amatpc(idiag) = this%amatpc(idiag) - chat(iil)
      this%amatpc(iii) = this%amatpc(iii) + chat(iil)            
    enddo
    !
    return
  end subroutine xt3d_amatpc_nbrs

  subroutine xt3d_amatpcx_nbrnbrs(this, nodes, n, m, ii01, nnbr, inbr, chat)
! ******************************************************************************
! xt3d_amatpcx_nbrnbrs -- Add contributions from neighbors of neighbor to
!   amatpc and amatpcx.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- module
    ! -- dummy
    class(Xt3dType) :: this
    integer(I4B),intent(in) :: nodes
    integer(I4B) :: n, m, ii01, nnbr
    integer(I4B),dimension(this%nbrmax) :: inbr
    real(DP),dimension(this%nbrmax) :: chat
    ! -- local
    integer(I4B) :: iil, iii, jjj, iixjjj, iijjj
! ------------------------------------------------------------------------------
    !
    do iil = 1,nnbr
      this%amatpc(ii01) = this%amatpc(ii01) + chat(iil)
      iii = this%dis%con%ia(m) + iil
      jjj = this%dis%con%ja(iii)
      call this%xt3d_get_iinmx(n, jjj, iixjjj)
      if (iixjjj.ne.0) then
        this%amatpcx(iixjjj) = this%amatpcx(iixjjj) - chat(iil)
      else
        call this%xt3d_get_iinm(n, jjj, iijjj)
        this%amatpc(iijjj) = this%amatpc(iijjj) - chat(iil)
      endif
    enddo
    !
    return
  end subroutine xt3d_amatpcx_nbrnbrs

  subroutine xt3d_get_iinm(this, n, m, iinm)
! ******************************************************************************
! xt3d_get_iinm -- Get position of n-m connection in ja array (return 0 if
!   not connected).
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- module
    ! -- dummy
    class(Xt3dType) :: this
    integer(I4B) :: n, m, iinm
    ! -- local
    integer(I4B) :: ii, jj
! ------------------------------------------------------------------------------
    !
    iinm = 0
    do ii = this%dis%con%ia(n), this%dis%con%ia(n+1)-1
      jj = this%dis%con%ja(ii)
      if (jj.eq.m) then
        iinm = ii
        exit
      endif
    enddo
    !
    return
  end subroutine xt3d_get_iinm

  subroutine xt3d_get_iinmx(this, n, m, iinmx)
! ******************************************************************************
! xt3d_get_iinmx -- Get position of n-m "extended connection" in jax array
!   (return 0 if not connected).
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- module
    ! -- dummy
    class(Xt3dType) :: this
    integer(I4B) :: n, m, iinmx
    ! -- local
    integer(I4B) :: iix, jjx
! ------------------------------------------------------------------------------
    !
    iinmx = 0
    do iix = this%iax(n), this%iax(n+1)-1
      jjx = this%jax(iix)
      if (jjx.eq.m) then
        iinmx = iix
        exit
      endif
    enddo
    !
    return
  end subroutine xt3d_get_iinmx

  subroutine xt3d_rhs(this, nodes, n, m, nnbr, inbr, chat, hnew,     &
      rhs)
! ******************************************************************************
! xt3d_rhs -- Add contributions to rhs.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- module
    ! -- dummy
    class(Xt3dType) :: this
    integer(I4B),intent(in) :: nodes
    integer(I4B) :: n, m, nnbr
    integer(I4B),dimension(this%nbrmax) :: inbr
    real(DP),dimension(this%nbrmax) :: chat
    real(DP),intent(inout),dimension(nodes) :: hnew, rhs
    ! -- local
    integer(I4B) :: iil, iii, jjj
    real(DP) :: term
! ------------------------------------------------------------------------------
    !
    do iil = 1,nnbr
      if (inbr(iil).ne.0) then
        iii = iil + this%dis%con%ia(n)
        jjj = this%dis%con%ja(iii)
        term = chat(iil)*(hnew(jjj)-hnew(n))
        rhs(n) = rhs(n) - term
        rhs(m) = rhs(m) + term
      endif
    enddo              
    !
    return
  end subroutine xt3d_rhs

  subroutine xt3d_qnbrs(this, nodes, n, m, nnbr, inbr, chat, hnew,   &
      qnbrs)
! ******************************************************************************
! xt3d_qnbrs -- Add contributions to flow from neighbors.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- module
    ! -- dummy
    class(Xt3dType) :: this
    integer(I4B),intent(in) :: nodes
    integer(I4B) :: n, m, nnbr
    integer(I4B),dimension(this%nbrmax) :: inbr
    real(DP) :: qnbrs
    real(DP),dimension(this%nbrmax) :: chat
    real(DP),intent(inout),dimension(nodes) :: hnew
    ! -- local
    integer(I4B) :: iil, iii, jjj
    real(DP) :: term
! ------------------------------------------------------------------------------
    !
    qnbrs = 0d0
    do iil = 1,nnbr
      if (inbr(iil).ne.0) then
        iii = iil + this%dis%con%ia(n)
        jjj = this%dis%con%ja(iii)
        term = chat(iil)*(hnew(jjj)-hnew(n))
        qnbrs = qnbrs + term
      endif
    enddo              
    !
    return
  end subroutine xt3d_qnbrs

  subroutine xt3d_fillrmatck(this, n)
! ******************************************************************************
! xt3d_fillrmatck -- Fill rmat array for cell n.
!   angle1, 2, and 3 must be in radians.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- module
    ! -- dummy
    class(Xt3dType) :: this
    integer(I4B), intent(in) :: n
    ! -- local
    real(DP) :: ang1, ang2, ang3, ang2d, ang3d
    real(DP) :: s1, c1, s2, c2, s3, c3
! ------------------------------------------------------------------------------
    !
    if (this%nozee) then
      ang2d = 0d0
      ang3d = 0d0
      ang1 = this%angle1(n)
      ang2 = 0d0
      ang3 = 0d0
    else
      ang1 = this%angle1(n)
      ang2 = this%angle2(n)
      ang3 = this%angle3(n)
    endif
    s1 = sin(ang1)
    c1 = cos(ang1)
    s2 = sin(ang2)
    c2 = cos(ang2)
    s3 = sin(ang3)
    c3 = cos(ang3)
    this%rmatck(1,1) = c1*c2
    this%rmatck(1,2) = c1*s2*s3 - s1*c3
    this%rmatck(1,3) = -c1*s2*c3 - s1*s3
    this%rmatck(2,1) = s1*c2
    this%rmatck(2,2) = s1*s2*s3 + c1*c3
    this%rmatck(2,3) = -s1*s2*c3 + c1*s3
    this%rmatck(3,1) = s2
    this%rmatck(3,2) = -c2*s3
    this%rmatck(3,3) = c2*c3
    !
    return
  end subroutine xt3d_fillrmatck
  
end module Xt3dModule
