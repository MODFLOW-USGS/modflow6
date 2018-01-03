MODULE GWFFHBMODULE
  INTEGER, SAVE, POINTER :: NBDTIM
  INTEGER, SAVE, POINTER :: NFLW => null()
  INTEGER, SAVE, POINTER :: NHED => null()
  INTEGER, SAVE, POINTER :: IFHBCB,NFHBX1, &
                            NFHBX2,IFHBSS
  INTEGER, SAVE, POINTER, DIMENSION(:,:)  ::IFLLOC
  INTEGER, SAVE, POINTER, DIMENSION(:,:)  ::IHDLOC
  double precision, SAVE, POINTER, DIMENSION(:)    ::BDTIM
  double precision, SAVE, POINTER, DIMENSION(:,:)  ::FLWRAT
  double precision, SAVE, POINTER, DIMENSION(:,:)  ::SBHED
  REAL,    SAVE, POINTER, DIMENSION(:,:)  ::BDFV
  REAL,    SAVE, POINTER, DIMENSION(:,:)  ::BDHV
  double precision, SAVE, POINTER, DIMENSION(:)  ::FHBXWT
  CHARACTER(LEN=16), SAVE, POINTER, DIMENSION(:) ::FHBXNM
  double precision, save, pointer :: cnstmf, cnstmh
  
  TYPE GWFFHBTYPE
    INTEGER, POINTER ::NBDTIM,NFLW,NHED,IFHBCB,NFHBX1, &
                             NFHBX2,IFHBSS
    INTEGER, POINTER, DIMENSION(:,:)  ::IFLLOC
    INTEGER, POINTER, DIMENSION(:,:)  ::IHDLOC
    double precision, POINTER, DIMENSION(:)    ::BDTIM
    double precision, POINTER, DIMENSION(:,:)  ::FLWRAT
    double precision, POINTER, DIMENSION(:,:)  ::SBHED
    REAL,    POINTER, DIMENSION(:,:)  ::BDFV
    REAL,    POINTER, DIMENSION(:,:)  ::BDHV
    double precision, POINTER, DIMENSION(:)  ::FHBXWT
    CHARACTER(LEN=16), POINTER, DIMENSION(:) ::FHBXNM
    double precision, pointer :: cnstmf, cnstmh
  END TYPE
  
  TYPE(GWFFHBTYPE), SAVE  ::GWFFHBDAT(10)
  
CONTAINS

  SUBROUTINE GWF2FHB7DA(IGRID)
!  Deallocate FHB DATA
!
    DEALLOCATE(GWFFHBDAT(IGRID)%NBDTIM)
    DEALLOCATE(GWFFHBDAT(IGRID)%NFLW)
    DEALLOCATE(GWFFHBDAT(IGRID)%NHED)
    DEALLOCATE(GWFFHBDAT(IGRID)%IFHBCB)
    DEALLOCATE(GWFFHBDAT(IGRID)%NFHBX1)
    DEALLOCATE(GWFFHBDAT(IGRID)%NFHBX2)
    DEALLOCATE(GWFFHBDAT(IGRID)%IFHBSS)
    DEALLOCATE(GWFFHBDAT(IGRID)%FHBXWT)
    DEALLOCATE(GWFFHBDAT(IGRID)%FHBXNM)
    DEALLOCATE(GWFFHBDAT(IGRID)%BDTIM)
    DEALLOCATE(GWFFHBDAT(IGRID)%FLWRAT)
    DEALLOCATE(GWFFHBDAT(IGRID)%BDFV)
    DEALLOCATE(GWFFHBDAT(IGRID)%IFLLOC)
    DEALLOCATE(GWFFHBDAT(IGRID)%SBHED)
    DEALLOCATE(GWFFHBDAT(IGRID)%BDHV)
    DEALLOCATE(GWFFHBDAT(IGRID)%IHDLOC)
    DEALLOCATE(GWFFHBDAT(IGRID)%cnstmf)
    DEALLOCATE(GWFFHBDAT(IGRID)%cnstmh)
!
    RETURN
  END SUBROUTINE GWF2FHB7DA
      
  SUBROUTINE SGWF2FHB7PNT(IGRID)
!  Set pointers to FHB data for grid.
!
    NBDTIM=>GWFFHBDAT(IGRID)%NBDTIM
    NFLW=>GWFFHBDAT(IGRID)%NFLW
    NHED=>GWFFHBDAT(IGRID)%NHED
    IFHBCB=>GWFFHBDAT(IGRID)%IFHBCB
    NFHBX1=>GWFFHBDAT(IGRID)%NFHBX1
    NFHBX2=>GWFFHBDAT(IGRID)%NFHBX2
    IFHBSS=>GWFFHBDAT(IGRID)%IFHBSS
    FHBXWT=>GWFFHBDAT(IGRID)%FHBXWT
    FHBXNM=>GWFFHBDAT(IGRID)%FHBXNM
    BDTIM=>GWFFHBDAT(IGRID)%BDTIM
    FLWRAT=>GWFFHBDAT(IGRID)%FLWRAT
    BDFV=>GWFFHBDAT(IGRID)%BDFV
    IFLLOC=>GWFFHBDAT(IGRID)%IFLLOC
    SBHED=>GWFFHBDAT(IGRID)%SBHED
    BDHV=>GWFFHBDAT(IGRID)%BDHV
    IHDLOC=>GWFFHBDAT(IGRID)%IHDLOC
    cnstmf => gwffhbdat(igrid)%cnstmf
    cnstmh => gwffhbdat(igrid)%cnstmh
!
    RETURN
  END SUBROUTINE SGWF2FHB7PNT
      
  SUBROUTINE SGWF2FHB7PSV(IGRID)
!  Save pointers to FHB data for grid.
!
    GWFFHBDAT(IGRID)%NBDTIM=>NBDTIM
    GWFFHBDAT(IGRID)%NFLW=>NFLW
    GWFFHBDAT(IGRID)%NHED=>NHED
    GWFFHBDAT(IGRID)%IFHBCB=>IFHBCB
    GWFFHBDAT(IGRID)%NFHBX1=>NFHBX1
    GWFFHBDAT(IGRID)%NFHBX2=>NFHBX2
    GWFFHBDAT(IGRID)%IFHBSS=>IFHBSS
    GWFFHBDAT(IGRID)%FHBXWT=>FHBXWT
    GWFFHBDAT(IGRID)%FHBXNM=>FHBXNM
    GWFFHBDAT(IGRID)%BDTIM=>BDTIM
    GWFFHBDAT(IGRID)%FLWRAT=>FLWRAT
    GWFFHBDAT(IGRID)%BDFV=>BDFV
    GWFFHBDAT(IGRID)%IFLLOC=>IFLLOC
    GWFFHBDAT(IGRID)%SBHED=>SBHED
    GWFFHBDAT(IGRID)%BDHV=>BDHV
    GWFFHBDAT(IGRID)%IHDLOC=>IHDLOC
    GWFFHBDAT(IGRID)%cnstmf => cnstmf
    GWFFHBDAT(IGRID)%cnstmh => cnstmh
!
    RETURN
  END SUBROUTINE SGWF2FHB7PSV

END MODULE

! NBDTIM = number of times
! NFLW   = number of flow cells
! NHED   = number of head cells
! IFHBSS = steady-state option (ignored if simulation is transient);
!          applies when simulation includes multiple steady-state stress periods.
!    = 0: First value in FLWRAT, SBHED is used.
!   /= 0: Values of flow, head are interpolated as for transient simulations
! IFHBCB = flag and unit number for cell-by-cell flows
! NFHBX1 = number of flow auxiliary variables
! NFHBX2 = number of head auxiliary variables
! IFLLOC = cell indices (and IAUX) for flow cells
! IHDLOC = cell indices (and IAUX) for head cells
! BDTIM  = times at which flows and heads are specified
! FLWRAT = flow rate at times provided in BDTIM
! SBHED  = head at times provided in BDTIM
! BDFV   = interpolated flow rates for all flow cells and values of all 
!          aux variable associated with each flow cell (unneeded by mf5to6)
! BDHV   = interpolated heads for all head cells and values of all 
!          aux variable associated with each head cell (unneeded by mf5to6)
! FHBXWT = auxiliary variable weighting factors
! FHBXNM = auxiliary variable names

