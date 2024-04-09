module VectorInterpolationModule

  use KindModule, only: I4B, DP
  use ConstantsModule, only: DZERO, DONE, DEM10
  use BaseDisModule, only: DisBaseType

  implicit none

  public :: vector_interpolation_2d

contains

  !> @brief Interpolate 2D vector components at cell center
  !!
  !! Given a component of a vector on the edges of all cells, assumed to be
  !! normal to the edge, interpolate a 2d vector using an XT3D-like
  !! interpolation.  Can be used to calculate a cell-center velocity given
  !! face flows.  Routine can optionally divide the edge flow by area,
  !! if provided, to interpolate velocity, for example.  Sign on flowja is
  !! assumed to be positive into the cell.
  !!
  !! Routine requires that dis%connection_vector() and dis%connection_normal()
  !! are available.
  !<
  subroutine vector_interpolation_2d(dis, flowja, nedges, nodedge, &
                                     propsedge, vcomp, vmag, flowareaja)
    ! modules
    ! dummy
    class(DisBaseType), intent(in) :: dis !< discretization package object
    real(DP), intent(in), dimension(:) :: flowja !< flow across each face in model grid (size njas)
    integer(I4B), intent(in), optional :: nedges !< number of external edges for which a flow is provided
    integer(I4B), dimension(:), intent(in), optional :: nodedge !< array of node numbers that have edges
    real(DP), dimension(:, :), intent(in), optional :: propsedge !< edge properties (Q, area, nx, ny, distance)
    real(DP), dimension(:, :), intent(inout), optional :: vcomp !< vector components: vx, vy, vz (nodes, 3)
    real(DP), dimension(:), intent(inout), optional :: vmag !< vector magnitude (nodes)
    real(DP), intent(in), dimension(:), optional :: flowareaja !< flow area across each face in model grid
    ! local
    integer(I4B) :: n
    integer(I4B) :: m
    integer(I4B) :: ipos
    integer(I4B) :: isympos
    integer(I4B) :: ihc
    integer(I4B) :: ic
    integer(I4B) :: iz
    integer(I4B) :: nc
    real(DP) :: vx
    real(DP) :: vy
    real(DP) :: vz
    real(DP) :: xn
    real(DP) :: yn
    real(DP) :: zn
    real(DP) :: xc
    real(DP) :: yc
    real(DP) :: zc
    real(DP) :: cl1
    real(DP) :: cl2
    real(DP) :: dltot
    real(DP) :: ooclsum
    real(DP) :: q
    real(DP) :: dsumx
    real(DP) :: dsumy
    real(DP) :: dsumz
    real(DP) :: denom
    real(DP) :: area
    real(DP) :: axy
    real(DP) :: ayx
    real(DP), allocatable, dimension(:) :: vi
    real(DP), allocatable, dimension(:) :: di
    real(DP), allocatable, dimension(:) :: viz
    real(DP), allocatable, dimension(:) :: diz
    real(DP), allocatable, dimension(:) :: nix
    real(DP), allocatable, dimension(:) :: niy
    real(DP), allocatable, dimension(:) :: wix
    real(DP), allocatable, dimension(:) :: wiy
    real(DP), allocatable, dimension(:) :: wiz
    real(DP), allocatable, dimension(:) :: bix
    real(DP), allocatable, dimension(:) :: biy
    logical :: nozee = .true.
    !
    ! -- Ensure dis has necessary information
    ! todo: do we need this?  this needs to be done outside of this routine
    ! if (this%icalcvelocity /= 0 .and. this%dis%con%ianglex == 0) then
    !   call store_error('Error.  ANGLDEGX not provided in '// &
    !                    'discretization file.  ANGLDEGX required for '// &
    !                    'calculation of velocity.', terminate=.TRUE.)
    ! end if
    !
    ! -- Find max number of connections and allocate weight arrays
    nc = 0
    do n = 1, dis%nodes
      !
      ! -- Count internal model connections
      ic = dis%con%ia(n + 1) - dis%con%ia(n) - 1
      !
      ! -- Count edge connections
      if (present(nedges)) then
        do m = 1, nedges
          if (nodedge(m) == n) then
            ic = ic + 1
          end if
        end do
      end if
      !
      ! -- Set max number of connections for any cell
      if (ic > nc) nc = ic
    end do
    !
    ! -- Allocate storage arrays needed for cell-centered calculation
    allocate (vi(nc))
    allocate (di(nc))
    allocate (viz(nc))
    allocate (diz(nc))
    allocate (nix(nc))
    allocate (niy(nc))
    allocate (wix(nc))
    allocate (wiy(nc))
    allocate (wiz(nc))
    allocate (bix(nc))
    allocate (biy(nc))
    !
    ! -- Go through each cell and calculate specific discharge
    do n = 1, dis%nodes
      !
      ! -- first calculate geometric properties for x and y directions and
      !    the specific discharge at a face (vi)
      ic = 0
      iz = 0
      vi(:) = DZERO
      di(:) = DZERO
      viz(:) = DZERO
      diz(:) = DZERO
      nix(:) = DZERO
      niy(:) = DZERO
      do ipos = dis%con%ia(n) + 1, dis%con%ia(n + 1) - 1
        m = dis%con%ja(ipos)
        isympos = dis%con%jas(ipos)
        ihc = 1
        ic = ic + 1

        ! Find the normal components (xn, yn, zn) for this connection
        call dis%connection_normal(n, m, ihc, xn, yn, zn, ipos)

        ! Find the straight-line distance between n and m and put
        ! in dltot
        call dis%connection_vector(n, m, nozee, DONE, DONE, &
                                   ihc, xc, yc, zc, dltot)

        ! Find the connection lengths, cl1 and cl2, adjusting for the
        ! n, m symmetry
        cl1 = dis%con%cl1(isympos)
        cl2 = dis%con%cl2(isympos)
        if (m < n) then
          cl1 = dis%con%cl2(isympos)
          cl2 = dis%con%cl1(isympos)
        end if

        ! Set the normal components and distance for this connection
        ooclsum = DONE / (cl1 + cl2)
        nix(ic) = -xn
        niy(ic) = -yn
        di(ic) = dltot * cl1 * ooclsum

        ! Set vi to flow area and divide by area if present
        q = flowja(isympos)
        if (n < m) then
          vi(ic) = q
        else
          vi(ic) = -q
        end if
        if (present(flowareaja)) then
          area = flowareaja(isympos)
          if (area > DZERO) then
            vi(ic) = vi(ic) / area
          else
            vi(ic) = DZERO
          end if
        end if

      end do
      !
      ! -- Look through edge flows that may have been provided by an exchange
      !    and incorporate them into the averaging arrays
      if (present(nedges)) then
        do m = 1, nedges
          if (nodedge(m) == n) then
            ic = ic + 1
            nix(ic) = -propsedge(3, m)
            niy(ic) = -propsedge(4, m)
            di(ic) = propsedge(5, m)
            vi(ic) = propsedge(1, m)
            if (present(flowareaja)) then
              area = propsedge(2, m)
              if (area > DZERO) then
                vi(ic) = vi(ic) / area
              else
                vi(ic) = DZERO
              end if
            end if
          end if
        end do
      end if

      ! Set vz to zero
      vz = DZERO

      ! -- distance-based weighting
      nc = ic
      dsumx = DZERO
      dsumy = DZERO
      dsumz = DZERO
      do ic = 1, nc
        wix(ic) = di(ic) * abs(nix(ic))
        wiy(ic) = di(ic) * abs(niy(ic))
        dsumx = dsumx + wix(ic)
        dsumy = dsumy + wiy(ic)
      end do
      !
      ! -- Finish computing omega weights.  Add a tiny bit
      !    to dsum so that the normalized omega weight later
      !    evaluates to (essentially) 1 in the case of a single
      !    relevant connection, avoiding 0/0.
      dsumx = dsumx + DEM10 * dsumx
      dsumy = dsumy + DEM10 * dsumy
      do ic = 1, nc
        wix(ic) = (dsumx - wix(ic)) * abs(nix(ic))
        wiy(ic) = (dsumy - wiy(ic)) * abs(niy(ic))
      end do
      !
      ! -- compute B weights
      dsumx = DZERO
      dsumy = DZERO
      do ic = 1, nc
        bix(ic) = wix(ic) * sign(DONE, nix(ic))
        biy(ic) = wiy(ic) * sign(DONE, niy(ic))
        dsumx = dsumx + wix(ic) * abs(nix(ic))
        dsumy = dsumy + wiy(ic) * abs(niy(ic))
      end do
      if (dsumx > DZERO) dsumx = DONE / dsumx
      if (dsumy > DZERO) dsumy = DONE / dsumy
      axy = DZERO
      ayx = DZERO
      do ic = 1, nc
        bix(ic) = bix(ic) * dsumx
        biy(ic) = biy(ic) * dsumy
        axy = axy + bix(ic) * niy(ic)
        ayx = ayx + biy(ic) * nix(ic)
      end do
      !
      ! -- Calculate vector components at cell center. The divide by zero checking
      !    below is problematic for cells with only one flow, such as can happen
      !    with triangular cells in corners.  In this case, the resulting
      !    cell velocity will be calculated as zero.  The method should be
      !    improved so that edge flows of zero are included in these
      !    calculations.  But this needs to be done with consideration for LGR
      !    cases in which flows are submitted from an exchange.
      vx = DZERO
      vy = DZERO
      do ic = 1, nc
        vx = vx + (bix(ic) - axy * biy(ic)) * vi(ic)
        vy = vy + (biy(ic) - ayx * bix(ic)) * vi(ic)
      end do
      denom = DONE - axy * ayx
      if (denom /= DZERO) then
        vx = vx / denom
        vy = vy / denom
      end if

      ! Store velocity components
      if (present(vcomp)) then
        vcomp(1, n) = vx
        vcomp(2, n) = vy
        vcomp(3, n) = vz
      end if

      ! Store velocity magnitude
      if (present(vmag)) then
        vmag(n) = sqrt(vx**2 + vy**2 + vz**2)
      end if

    end do

    ! cleanup
    deallocate (vi)
    deallocate (di)
    deallocate (nix)
    deallocate (niy)
    deallocate (wix)
    deallocate (wiy)
    deallocate (wiz)
    deallocate (bix)
    deallocate (biy)

  end subroutine vector_interpolation_2d

end module VectorInterpolationModule
