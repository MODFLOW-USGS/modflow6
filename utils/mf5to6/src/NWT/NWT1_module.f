      MODULE GWFNWTMODULE
      USE machine_constants, ONLY: kdp
      IMPLICIT NONE                                                     
      DOUBLE PRECISION, PARAMETER :: HEPS = 1.0E-7                      
      DOUBLE PRECISION, PARAMETER :: CLOSEZERO = 1.0E-15
      DOUBLE PRECISION,PARAMETER :: BIG = 1.0D20 
      DOUBLE PRECISION,PARAMETER :: SMALL = 1.0D-5              
      DOUBLE PRECISION, SAVE, DIMENSION(:), POINTER :: A
      DOUBLE PRECISION, SAVE, DIMENSION(:,:), POINTER :: Dc
      DOUBLE PRECISION, SAVE, DIMENSION(:, :, :), POINTER :: Hiter      
      DOUBLE PRECISION, SAVE, DIMENSION(:), POINTER :: BB, Hchange
      DOUBLE PRECISION, SAVE, DIMENSION(:), POINTER :: Hchold, Wsave    
      DOUBLE PRECISION, SAVE, POINTER :: Cvm1, Hvm1                     
      DOUBLE PRECISION, SAVE, POINTER :: W, Theta                       
      DOUBLE PRECISION, SAVE, POINTER :: Akappa, Gamma, Amomentum       
      DOUBLE PRECISION, SAVE, POINTER :: Hvp1, Crm1                     
      DOUBLE PRECISION, SAVE, POINTER :: Hrm1, Hrp1, Ccm1               
      DOUBLE PRECISION, SAVE, POINTER :: Hcm1, Hcp1                     
      DOUBLE PRECISION, SAVE, POINTER :: Ccc, Crr, Cvv, H               
      DOUBLE PRECISION, SAVE, POINTER :: Hcoff, Rhss, Fflux, Fhead
      DOUBLE PRECISION, SAVE, POINTER :: Fheadsave
      INTEGER, SAVE, POINTER :: Numnonzero, II, Itreal, Ibt, NJA
      INTEGER, SAVE, POINTER :: IFDPARAM, ICNVGFLG
      INTEGER, SAVE, POINTER :: Btrack, Iierr
      DOUBLE PRECISION, SAVE, POINTER :: Tol, Ftol, RELAX, RMS2, RMS1
      DOUBLE PRECISION, SAVE, POINTER :: Thickfact, Breduc, Btol, RMSAVE
      INTEGER, SAVE, POINTER :: Numactive, Numcell   
      INTEGER, SAVE, POINTER :: Nonmeth
      INTEGER, SAVE, POINTER :: Linmeth
      INTEGER, SAVE, POINTER :: IPRNWT
      INTEGER, SAVE, POINTER :: IBOTAV
      INTEGER, SAVE, POINTER :: ITER1,Numtrack 
      INTEGER, SAVE, DIMENSION(:), POINTER :: IA, JA
      INTEGER, SAVE, DIMENSION(:, :), POINTER :: Diag
      INTEGER, SAVE, DIMENSION(:, :, :), POINTER :: Icell
      ! added for MF5to6
      integer, save, pointer :: MxIterInner => null()
      double precision, save, pointer :: HCloseLinear => null()
      double precision, save, pointer :: RCloseLinear => null()
      integer, save, pointer :: iaclNwt => null()
      integer, save, pointer :: norderNwt => null()
      integer, save, pointer :: levelNwt => null()
      integer, save, pointer :: northNwt => null()
      integer, save, pointer :: MaxBackIterNwt => null()
      integer, save, pointer :: IRedSysNwt => null()
      integer, save, pointer :: IDropTolNwt => null()
      integer, save, pointer :: BackFlagNwt => null()
      double precision, save, pointer :: BackTolNwt => null()
      double precision, save, pointer :: BackReduceNwt => null()
      double precision, save, pointer :: EpsrnNwt => null()
      double precision, save, pointer :: RrctolNwt => null()
      TYPE GWFNWTTYPE                                                   
        DOUBLE PRECISION, DIMENSION(:), POINTER :: A
        DOUBLE PRECISION, DIMENSION(:,:), POINTER :: Dc
        DOUBLE PRECISION, DIMENSION(:, :, :), POINTER :: Hiter  
        DOUBLE PRECISION, DIMENSION(:), POINTER :: BB, Hchange
        DOUBLE PRECISION, DIMENSION(:), POINTER :: Hchold, Wsave        
        DOUBLE PRECISION, POINTER :: Cvm1, Hvm1                         
        DOUBLE PRECISION, POINTER :: Hvp1, Crm1                         
        DOUBLE PRECISION, POINTER :: Hrm1, Hrp1, Ccm1                   
        DOUBLE PRECISION, POINTER :: Hcm1, Hcp1                         
        DOUBLE PRECISION, POINTER :: Ccc, Crr, Cvv, H                   
        DOUBLE PRECISION, POINTER :: W, Theta                           
        DOUBLE PRECISION, POINTER :: Akappa, Gamma, Amomentum           
        DOUBLE PRECISION, POINTER :: Hcoff, Rhss, Fflux, Fhead
        DOUBLE PRECISION, POINTER :: Fheadsave
        INTEGER, POINTER :: Numnonzero, II, Itreal, Ibt, NJA
        INTEGER, POINTER :: IFDPARAM, ICNVGFLG
        DOUBLE PRECISION, POINTER :: Tol, Ftol, RELAX, RMS2, RMS1
        DOUBLE PRECISION, POINTER :: Thickfact, Breduc, Btol, RMSAVE
        INTEGER, POINTER :: Numactive, Numcell
        INTEGER, POINTER :: Nonmeth
        INTEGER, POINTER :: Linmeth
        INTEGER, POINTER :: IPRNWT
        INTEGER, POINTER :: IBOTAV
        INTEGER, POINTER :: Btrack, Iierr                 
        INTEGER, POINTER :: ITER1,Numtrack
        INTEGER, DIMENSION(:), POINTER :: IA, JA                   
        INTEGER, DIMENSION(:, :), POINTER :: Diag
        INTEGER, DIMENSION(:, :, :), POINTER :: Icell
        ! added for MF5to6
        integer, pointer :: MxIterInner
        double precision, pointer :: HCloseLinear
        double precision, pointer :: RCloseLinear
        integer, pointer :: iaclNwt
        integer, pointer :: norderNwt
        integer, pointer :: levelNwt
        integer, pointer :: northNwt
        integer, pointer :: MaxBackIterNwt
        integer, pointer :: IRedSysNwt
        integer, pointer :: IDropTolNwt
        integer, pointer :: BackFlagNwt
        double precision, pointer :: BackTolNwt
        double precision, pointer :: BackReduceNwt
        double precision, pointer :: EpsrnNwt
        double precision, pointer :: RrctolNwt
      END TYPE GWFNWTTYPE                                               
      TYPE (GWFNWTTYPE) , SAVE::Gwfnwtdat(10)                           
!
      contains
!
      SUBROUTINE GWF2NWT1DA(Igrid)
      IMPLICIT NONE
!     ------------------------------------------------------------------
!     SPECIFICATIONS:
!     ------------------------------------------------------------------
!     ARGUMENTS
!     ------------------------------------------------------------------
      INTEGER Igrid    
!     ------------------------------------------------------------------
! Deallocate NWT data.
      DEALLOCATE (Gwfnwtdat(Igrid)%IA)
      DEALLOCATE (Gwfnwtdat(Igrid)%JA)
      DEALLOCATE (Gwfnwtdat(Igrid)%Diag)
      DEALLOCATE (Gwfnwtdat(Igrid)%Icell)      
      DEALLOCATE (Gwfnwtdat(Igrid)%A)
      DEALLOCATE (Gwfnwtdat(Igrid)%Dc)
      DEALLOCATE (Gwfnwtdat(Igrid)%BB)
      DEALLOCATE (Gwfnwtdat(Igrid)%Hchange)
      DEALLOCATE (Gwfnwtdat(Igrid)%Numnonzero)
      DEALLOCATE (Gwfnwtdat(Igrid)%NJA)
      DEALLOCATE (Gwfnwtdat(Igrid)%Itreal)
      DEALLOCATE (Gwfnwtdat(Igrid)%Ibt)
      DEALLOCATE (Gwfnwtdat(Igrid)%II)
      DEALLOCATE (Gwfnwtdat(Igrid)%IFDPARAM) 
      DEALLOCATE (Gwfnwtdat(Igrid)%ICNVGFLG)
      DEALLOCATE (Gwfnwtdat(Igrid)%Tol)
      DEALLOCATE (Gwfnwtdat(Igrid)%Ftol)
      deallocate (Gwfnwtdat(Igrid)%RELAX)
      DEALLOCATE (Gwfnwtdat(Igrid)%ITER1)
      DEALLOCATE (Gwfnwtdat(Igrid)%Cvm1)
      DEALLOCATE (Gwfnwtdat(Igrid)%Hvm1)
      DEALLOCATE (Gwfnwtdat(Igrid)%Hvp1)
      DEALLOCATE (Gwfnwtdat(Igrid)%Crm1)
      DEALLOCATE (Gwfnwtdat(Igrid)%Hrm1)
      DEALLOCATE (Gwfnwtdat(Igrid)%Hrp1)
      DEALLOCATE (Gwfnwtdat(Igrid)%Ccm1)
      DEALLOCATE (Gwfnwtdat(Igrid)%Hcm1)
      DEALLOCATE (Gwfnwtdat(Igrid)%Hcp1)
      DEALLOCATE (Gwfnwtdat(Igrid)%Ccc)
      DEALLOCATE (Gwfnwtdat(Igrid)%Crr)
      DEALLOCATE (Gwfnwtdat(Igrid)%Cvv)
      DEALLOCATE (Gwfnwtdat(Igrid)%H)
      DEALLOCATE (Gwfnwtdat(Igrid)%Hcoff)
      DEALLOCATE (Gwfnwtdat(Igrid)%Rhss)
      DEALLOCATE (Gwfnwtdat(Igrid)%Fhead)
      DEALLOCATE (Gwfnwtdat(Igrid)%Fheadsave)
      DEALLOCATE (Gwfnwtdat(Igrid)%Fflux)
      DEALLOCATE (Gwfnwtdat(Igrid)%Hchold)
      DEALLOCATE (Gwfnwtdat(Igrid)%Numactive)
      DEALLOCATE (Gwfnwtdat(Igrid)%Numcell)
      DEALLOCATE (Gwfnwtdat(Igrid)%W)
      DEALLOCATE (Gwfnwtdat(Igrid)%Hiter)
      DEALLOCATE (Gwfnwtdat(Igrid)%Wsave)
      DEALLOCATE (Gwfnwtdat(Igrid)%Theta)
      DEALLOCATE (Gwfnwtdat(Igrid)%Akappa)
      DEALLOCATE (Gwfnwtdat(Igrid)%Gamma)
      DEALLOCATE (Gwfnwtdat(Igrid)%Amomentum)
      DEALLOCATE (Gwfnwtdat(Igrid)%Btrack)
      DEALLOCATE (Gwfnwtdat(Igrid)%Btol)
      DEALLOCATE (Gwfnwtdat(Igrid)%RMSAVE)
      DEALLOCATE (Gwfnwtdat(Igrid)%Thickfact)
      DEALLOCATE (Gwfnwtdat(Igrid)%Numtrack)
      DEALLOCATE (Gwfnwtdat(Igrid)%RMS2) 
      DEALLOCATE (Gwfnwtdat(Igrid)%RMS1) 
      DEALLOCATE (Gwfnwtdat(Igrid)%Iierr)
      DEALLOCATE (Gwfnwtdat(Igrid)%Nonmeth)
      DEALLOCATE (Gwfnwtdat(Igrid)%Linmeth)
      DEALLOCATE (Gwfnwtdat(Igrid)%IPRNWT)
      DEALLOCATE (Gwfnwtdat(Igrid)%IBOTAV)
      deallocate (Gwfnwtdat(igrid)%MxIterInner)
      deallocate (Gwfnwtdat(Igrid)%HCloseLinear)
      deallocate (Gwfnwtdat(Igrid)%RCloseLinear)
      deallocate (Gwfnwtdat(Igrid)%iaclNwt)
      deallocate (Gwfnwtdat(Igrid)%norderNwt)
      deallocate (Gwfnwtdat(Igrid)%levelNwt)
      deallocate (Gwfnwtdat(Igrid)%northNwt)
      deallocate (Gwfnwtdat(Igrid)%MaxBackIterNwt)
      deallocate (Gwfnwtdat(Igrid)%IRedSysNwt)
      deallocate (Gwfnwtdat(Igrid)%IDropTolNwt)
      deallocate (Gwfnwtdat(Igrid)%BackFlagNwt)
      deallocate (Gwfnwtdat(Igrid)%BackTolNwt)
      deallocate (Gwfnwtdat(Igrid)%BackReduceNwt)
      deallocate (Gwfnwtdat(Igrid)%EpsrnNwt)
      deallocate (Gwfnwtdat(Igrid)%RrctolNwt)
      END SUBROUTINE GWF2NWT1DA
 
 
 
      SUBROUTINE SGWF2NWT1PNT(Igrid)
      IMPLICIT NONE
!     ------------------------------------------------------------------
!     SPECIFICATIONS:
!     ------------------------------------------------------------------
!     ARGUMENTS
!     ------------------------------------------------------------------
      INTEGER Igrid   
!     ------------------------------------------------------------------
! Set NWT pointers for grid.
      IA => Gwfnwtdat(Igrid)%IA
      JA => Gwfnwtdat(Igrid)%JA
      Diag => Gwfnwtdat(Igrid)%Diag
      Icell => Gwfnwtdat(Igrid)%Icell     
      A => Gwfnwtdat(Igrid)%A
      Dc => Gwfnwtdat(Igrid)%Dc
      BB => Gwfnwtdat(Igrid)%BB
      Hchange => Gwfnwtdat(Igrid)%Hchange
      Numnonzero => Gwfnwtdat(Igrid)%Numnonzero
      NJA => Gwfnwtdat(Igrid)%NJA
      Itreal => Gwfnwtdat(Igrid)%Itreal
      Ibt => Gwfnwtdat(Igrid)%Ibt
      II => Gwfnwtdat(Igrid)%II
      IFDPARAM => Gwfnwtdat(Igrid)%IFDPARAM   
      ICNVGFLG => Gwfnwtdat(Igrid)%ICNVGFLG
      Tol => Gwfnwtdat(Igrid)%Tol
      Ftol => Gwfnwtdat(Igrid)%Ftol
      RELAX => Gwfnwtdat(Igrid)%RELAX
      ITER1 => Gwfnwtdat(Igrid)%ITER1
      Cvm1 => Gwfnwtdat(Igrid)%Cvm1
      Hvm1 => Gwfnwtdat(Igrid)%Hvm1
      Hvp1 => Gwfnwtdat(Igrid)%Hvp1
      Crm1 => Gwfnwtdat(Igrid)%Crm1
      Hrm1 => Gwfnwtdat(Igrid)%Hrm1
      Hrp1 => Gwfnwtdat(Igrid)%Hrp1
      Ccm1 => Gwfnwtdat(Igrid)%Ccm1
      Hcm1 => Gwfnwtdat(Igrid)%Hcm1
      Hcp1 => Gwfnwtdat(Igrid)%Hcp1
      Ccc => Gwfnwtdat(Igrid)%Ccc
      Crr => Gwfnwtdat(Igrid)%Crr
      Cvv => Gwfnwtdat(Igrid)%Cvv
      H => Gwfnwtdat(Igrid)%H
      Hcoff => Gwfnwtdat(Igrid)%Hcoff
      Rhss => Gwfnwtdat(Igrid)%Rhss
      Fhead => Gwfnwtdat(Igrid)%Fhead
      Fheadsave => Gwfnwtdat(Igrid)%Fheadsave
      Fflux => Gwfnwtdat(Igrid)%Fflux
      Hchold => Gwfnwtdat(Igrid)%Hchold
      Numactive => Gwfnwtdat(Igrid)%Numactive
      Numcell => Gwfnwtdat(Igrid)%Numcell
      W => Gwfnwtdat(Igrid)%W
      Hiter => Gwfnwtdat(Igrid)%Hiter
      Wsave => Gwfnwtdat(Igrid)%Wsave
      Theta => Gwfnwtdat(Igrid)%Theta
      Akappa => Gwfnwtdat(Igrid)%Akappa
      Gamma => Gwfnwtdat(Igrid)%Gamma
      Amomentum => Gwfnwtdat(Igrid)%Amomentum
      Btrack => Gwfnwtdat(Igrid)%Btrack
      Btol => Gwfnwtdat(Igrid)%Btol
      RMSAVE => Gwfnwtdat(Igrid)%RMSAVE
      Thickfact => Gwfnwtdat(Igrid)%Thickfact
      Numtrack => Gwfnwtdat(Igrid)%Numtrack
      RMS2 => Gwfnwtdat(Igrid)%RMS2 
      RMS1 => Gwfnwtdat(Igrid)%RMS1   
      Iierr => Gwfnwtdat(Igrid)%Iierr
      Nonmeth => Gwfnwtdat(Igrid)%Nonmeth
      Linmeth => Gwfnwtdat(Igrid)%Linmeth 
      IPRNWT => Gwfnwtdat(Igrid)%IPRNWT
      IBOTAV => Gwfnwtdat(Igrid)%IBOTAV  
      MxIterInner => Gwfnwtdat(Igrid)%MxIterInner
      HCloseLinear => Gwfnwtdat(Igrid)%HCloseLinear
      RCloseLinear => Gwfnwtdat(Igrid)%RCloseLinear
      iaclNwt => Gwfnwtdat(Igrid)%iaclNwt
      norderNwt => Gwfnwtdat(Igrid)%norderNwt
      levelNwt => Gwfnwtdat(Igrid)%levelNwt
      northNwt => Gwfnwtdat(Igrid)%northNwt
      MaxBackIterNwt => Gwfnwtdat(Igrid)%MaxBackIterNwt
      IRedSysNwt => Gwfnwtdat(Igrid)%IRedSysNwt
      IDropTolNwt => Gwfnwtdat(Igrid)%IDropTolNwt
      BackFlagNwt => Gwfnwtdat(Igrid)%BackFlagNwt
      BackTolNwt => Gwfnwtdat(Igrid)%BackTolNwt
      BackReduceNwt => Gwfnwtdat(Igrid)%BackReduceNwt
      EpsrnNwt => Gwfnwtdat(Igrid)%EpsrnNwt
      RrctolNwt => Gwfnwtdat(Igrid)%RrctolNwt
      END SUBROUTINE SGWF2NWT1PNT
!
      SUBROUTINE SGWF2NWT1PSV(Igrid)
      IMPLICIT NONE
!     ------------------------------------------------------------------
!     SPECIFICATIONS:
!     ------------------------------------------------------------------
!     ARGUMENTS
!     ------------------------------------------------------------------
      INTEGER Igrid
!     ------------------------------------------------------------------
! Save NWT pointers for grid.
!
      Gwfnwtdat(Igrid)%IA => IA
      Gwfnwtdat(Igrid)%JA => JA
      Gwfnwtdat(Igrid)%Diag => Diag
      Gwfnwtdat(Igrid)%Icell => Icell      
      Gwfnwtdat(Igrid)%A => A
      Gwfnwtdat(Igrid)%Dc => Dc
      Gwfnwtdat(Igrid)%BB => BB
      Gwfnwtdat(Igrid)%Hchange => Hchange
      Gwfnwtdat(Igrid)%Numnonzero => Numnonzero
      Gwfnwtdat(Igrid)%NJA => NJA
      Gwfnwtdat(Igrid)%Itreal => Itreal
      Gwfnwtdat(Igrid)%Ibt => Ibt
      Gwfnwtdat(Igrid)%II => II
      Gwfnwtdat(Igrid)%IFDPARAM => IFDPARAM   
      Gwfnwtdat(Igrid)%ICNVGFLG => ICNVGFLG
      Gwfnwtdat(Igrid)%Tol => Tol
      Gwfnwtdat(Igrid)%Ftol => Ftol
      Gwfnwtdat(Igrid)%RELAX => RELAX
      Gwfnwtdat(Igrid)%ITER1 => ITER1
      Gwfnwtdat(Igrid)%Cvm1 => Cvm1
      Gwfnwtdat(Igrid)%Hvm1 => Hvm1
      Gwfnwtdat(Igrid)%Hvp1 => Hvp1
      Gwfnwtdat(Igrid)%Crm1 => Crm1
      Gwfnwtdat(Igrid)%Hrm1 => Hrm1
      Gwfnwtdat(Igrid)%Hrp1 => Hrp1
      Gwfnwtdat(Igrid)%Ccm1 => Ccm1
      Gwfnwtdat(Igrid)%Hcm1 => Hcm1
      Gwfnwtdat(Igrid)%Hcp1 => Hcp1
      Gwfnwtdat(Igrid)%Ccc => Ccc
      Gwfnwtdat(Igrid)%Crr => Crr
      Gwfnwtdat(Igrid)%Cvv => Cvv
      Gwfnwtdat(Igrid)%H => H
      Gwfnwtdat(Igrid)%Hcoff => Hcoff
      Gwfnwtdat(Igrid)%Rhss => Rhss
      Gwfnwtdat(Igrid)%Fhead => Fhead
      Gwfnwtdat(Igrid)%Fheadsave => Fheadsave
      Gwfnwtdat(Igrid)%Fflux => Fflux
      Gwfnwtdat(Igrid)%Hchold => Hchold
      Gwfnwtdat(Igrid)%Numactive => Numactive
      Gwfnwtdat(Igrid)%Numcell => Numcell
      Gwfnwtdat(Igrid)%W => W
      Gwfnwtdat(Igrid)%Hiter => Hiter
      Gwfnwtdat(Igrid)%Wsave => Wsave
      Gwfnwtdat(Igrid)%Theta => Theta
      Gwfnwtdat(Igrid)%Akappa => Akappa
      Gwfnwtdat(Igrid)%Gamma => Gamma
      Gwfnwtdat(Igrid)%Amomentum => Amomentum
      Gwfnwtdat(Igrid)%Btrack => Btrack
      Gwfnwtdat(Igrid)%Btol => Btol
      Gwfnwtdat(Igrid)%RMSAVE => RMSAVE
      Gwfnwtdat(Igrid)%Thickfact => Thickfact
      Gwfnwtdat(Igrid)%Numtrack => Numtrack
      Gwfnwtdat(IGRID)%RMS2=>RMS2
      Gwfnwtdat(IGRID)%RMS1=>RMS1
      Gwfnwtdat(IGRID)%Iierr=>Iierr
      Gwfnwtdat(IGRID)%Nonmeth=>Nonmeth
      Gwfnwtdat(IGRID)%Linmeth=>Linmeth
      Gwfnwtdat(IGRID)%IPRNWT=>IPRNWT
      Gwfnwtdat(IGRID)%IBOTAV=>IBOTAV
      Gwfnwtdat(IGRID)%MxIterInner => MxIterInner
      Gwfnwtdat(IGRID)%HCloseLinear => HCloseLinear
      Gwfnwtdat(IGRID)%RCloseLinear => RCloseLinear
      Gwfnwtdat(IGRID)%iaclNwt => iaclNwt
      Gwfnwtdat(Igrid)%norderNwt => norderNwt
      Gwfnwtdat(IGRID)%levelNwt => levelNwt
      Gwfnwtdat(IGRID)%northNwt => northNwt
      Gwfnwtdat(IGRID)%MaxBackIterNwt => MaxBackIterNwt
      Gwfnwtdat(IGRID)%IRedSysNwt => IRedSysNwt
      Gwfnwtdat(IGRID)%IDropTolNwt => IDropTolNwt
      Gwfnwtdat(IGRID)%BackFlagNwt => BackFlagNwt
      Gwfnwtdat(IGRID)%BackTolNwt => BackTolNwt
      Gwfnwtdat(IGRID)%BackReduceNwt => BackReduceNwt
      Gwfnwtdat(IGRID)%EpsrnNwt => EpsrnNwt
      Gwfnwtdat(IGRID)%RrctolNwt => RrctolNwt
!
      END SUBROUTINE SGWF2NWT1PSV

      END MODULE GWFNWTMODULE 
