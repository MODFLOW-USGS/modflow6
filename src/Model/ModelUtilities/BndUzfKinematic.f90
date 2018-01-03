module UzfKinematicModule
  
  use KindModule, only: DP, I4B
  use ConstantsModule, only: DZERO, DEM30, DEM20, DEM15, DEM14, DEM12, DEM10,   &
                             DEM9, DEM7, DEM6, DEM5, DEM4, DEM3, DHALF, DONE,   &
                             DTWO, DTHREE, DEP20
  use SmoothingModule
  use TdisModule, only: ITMUNI, delt, kper

  implicit none
  private
  public :: UzfKinematicType
  
  type :: UzfKinematicType
      real(DP), pointer :: thtr                      => null()
      real(DP), pointer :: thts                      => null()
      real(DP), pointer :: thti                      => null()
      real(DP), pointer :: eps                       => null()
      real(DP), pointer :: extwc                     => null()
      real(DP), pointer :: ha                        => null()
      real(DP), pointer :: hroot                     => null()
      real(DP), pointer :: rootact                   => null()
      real(DP), pointer :: etact                     => null()
      real(DP), dimension(:), pointer :: uzspst      => null()
      real(DP), dimension(:), pointer :: uzthst      => null()
      real(DP), dimension(:), pointer :: uzflst      => null()
      real(DP), dimension(:), pointer :: uzdpst      => null()
      integer(I4B), pointer :: nwavst                => null()
      real(DP), pointer :: uzolsflx                  => null()
      real(DP), pointer :: uzstor                    => null()
      real(DP), pointer :: delstor                   => null()
      real(DP), pointer :: totflux                   => null()
      real(DP), pointer :: vflow                     => null()
      integer(I4B), pointer :: nwav, ntrail          => null()
      real(DP), pointer :: sinf                      => null()
      real(DP), pointer :: finf                      => null()
      real(DP), pointer :: pet                       => null()
      real(DP), pointer :: petmax                    => null()
      real(DP), pointer :: extdp                     => null()
      real(DP), pointer :: extdpuz                   => null()
      real(DP), pointer :: finf_rej                  => null()
      real(DP), pointer :: gwet                      => null()
      real(DP), pointer :: uzfarea                   => null()
      real(DP), pointer :: cellarea                  => null()
      real(DP), pointer :: celtop                    => null()
      real(DP), pointer :: celbot                    => null()
      real(DP), pointer :: landtop                   => null()
      real(DP), pointer :: cvlm1                     => null()
      real(DP), pointer :: watab                     => null()
      real(DP), pointer :: watabold                  => null()
      real(DP), pointer :: vks                       => null()
      real(DP), pointer :: surfdep                   => null()
      real(DP), pointer :: surflux                   => null()
      real(DP), pointer :: surfluxbelow              => null()
      real(DP), pointer :: surfseep                  => null()
      real(DP), pointer :: gwpet                     => null()
      integer(I4B), pointer :: landflag              => null()
      integer(I4B), pointer :: ivertcon               => null()
  contains
      procedure :: init
      procedure :: setdata
      procedure :: setdatauzfarea
      procedure :: setdatafinf
      procedure :: setdataet
      procedure :: setdataetwc
      procedure :: setdataetha
      procedure :: setwaves
      procedure :: wave_shift
      procedure :: routewaves
      procedure :: uzflow
      procedure :: addrech
      procedure :: factors
      procedure :: trailwav
      procedure :: leadwav
      procedure :: leadspeed
      procedure :: advance
      procedure :: formulate
      procedure :: budget
      procedure :: unsat_stor
      procedure :: update_wav
      procedure :: simgwet
      procedure :: caph
      procedure :: rate_et_z
      procedure :: uzet
      procedure :: uz_rise
      procedure :: vertcellflow
      procedure :: etfunc_nlin
      procedure :: etfunc_lin
      procedure :: rejfinf
      procedure :: gwseep
      procedure :: setbelowpet
      procedure :: dealloc
    end type UzfKinematicType
!  
    contains
!
! ------------------------------------------------------------------------------
   
  subroutine init(this, ipos, nwav)
! ******************************************************************************
! init -- allocate and set uzf object variables
!
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
   !modules
   !arguments
   class(UzfKinematicType) :: this
   integer(I4B), intent(in) :: ipos
   integer(I4B), intent(in) :: nwav
! ------------------------------------------------------------------------------
    allocate(this%uzdpst(nwav))
    allocate(this%uzthst(nwav))
    allocate(this%uzflst(nwav))
    allocate(this%uzspst(nwav))
    allocate(this%nwavst)
    allocate(this%uzolsflx)
    allocate(this%thtr)
    allocate(this%thts)
    allocate(this%thti)
    allocate(this%eps)
    allocate(this%ha)
    allocate(this%hroot)
    allocate(this%rootact)
    allocate(this%extwc)
    allocate(this%etact)
    allocate(this%nwav)
    allocate(this%ntrail)
    allocate(this%uzstor)
    allocate(this%delstor)
    allocate(this%totflux)
    allocate(this%vflow)
    allocate(this%sinf)
    allocate(this%finf)
    allocate(this%finf_rej)
    allocate(this%gwet)
    allocate(this%uzfarea)
    allocate(this%cellarea)
    allocate(this%celtop)
    allocate(this%celbot)
    allocate(this%landtop)
    allocate(this%cvlm1)
    allocate(this%watab)
    allocate(this%watabold)
    allocate(this%surfdep)
    allocate(this%vks)
    allocate(this%surflux)
    allocate(this%surfluxbelow)
    allocate(this%surfseep)
    allocate(this%gwpet)
    allocate(this%pet)
    allocate(this%petmax)
    allocate(this%extdp)
    allocate(this%extdpuz)
    allocate(this%landflag) 
    allocate(this%ivertcon)
    this%uzdpst = DZERO
    this%uzthst = DZERO
    this%uzflst = DZERO
    this%uzspst = DZERO
    this%nwavst = 1
    this%uzolsflx = DZERO
    this%thtr = DZERO
    this%thts = DZERO
    this%thti = DZERO
    this%eps = DZERO
    this%ha = DZERO
    this%hroot = DZERO
    this%rootact = DZERO
    this%extwc = DZERO
    this%etact = DZERO
    this%nwav = nwav
    this%ntrail = 0
    this%uzstor = DZERO
    this%delstor = DZERO
    this%totflux = DZERO
    this%vflow = DZERO
    this%sinf = DZERO
    this%finf = DZERO
    this%finf_rej = DZERO
    this%gwet = DZERO
    this%uzfarea = DZERO
    this%cellarea = DZERO
    this%celtop = DZERO
    this%celbot = DZERO
    this%landtop = DZERO
    this%cvlm1 = DZERO
    this%watab = DZERO
    this%watabold = DZERO
    this%surfdep = DZERO
    this%vks = DZERO
    this%surflux = DZERO
    this%surfluxbelow = DZERO
    this%surfseep = DZERO
    this%gwpet = DZERO
    this%pet = DZERO
    this%petmax = DZERO
    this%extdp = DZERO
    this%extdpuz = DZERO
    this%landflag = 0
    this%ivertcon = 0
  end subroutine init
  !
  !
  subroutine dealloc(this)
! ******************************************************************************
! dealloc -- deallocate uzf object variables
!
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
   ! -- modules
   ! -- dummy
   class(UzfKinematicType) :: this
   ! -- locals
! ------------------------------------------------------------------------------
    deallocate(this%uzdpst)
    deallocate(this%uzthst)
    deallocate(this%uzflst)
    deallocate(this%uzspst)
    deallocate(this%nwavst)
    deallocate(this%uzolsflx)
    deallocate(this%thtr)
    deallocate(this%thts)
    deallocate(this%thti)
    deallocate(this%eps)
    deallocate(this%ha)
    deallocate(this%hroot)
    deallocate(this%rootact)
    deallocate(this%extwc)
    deallocate(this%etact)
    deallocate(this%nwav)
    deallocate(this%ntrail)
    deallocate(this%uzstor)
    deallocate(this%delstor)
    deallocate(this%totflux)
    deallocate(this%vflow)
    deallocate(this%sinf)
    deallocate(this%finf)
    deallocate(this%finf_rej)
    deallocate(this%gwet)
    deallocate(this%uzfarea)
    deallocate(this%cellarea)
    deallocate(this%celtop)
    deallocate(this%celbot)
    deallocate(this%landtop)
    deallocate(this%cvlm1)
    deallocate(this%watab)
    deallocate(this%watabold)
    deallocate(this%surfdep)
    deallocate(this%vks)
    deallocate(this%surflux)
    deallocate(this%surfluxbelow)
    deallocate(this%surfseep)
    deallocate(this%gwpet)
    deallocate(this%pet)
    deallocate(this%petmax)
    deallocate(this%extdp)
    deallocate(this%extdpuz)
    deallocate(this%landflag) 
    deallocate(this%ivertcon)
    !
    ! -- return
    return
  end subroutine dealloc
!
! ------------------------------------------------------------------------------
  
  subroutine setdata(this,ipos,area,top,bot,surfdep,     &
                     vks,thtr,thts,thti,eps,             &
                     ntrail,landflag,ivertcon,hgwf)
! ******************************************************************************
! setdata -- set uzf object material properties
!
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    !modules
    !arguments
    class(UzfKinematicType) :: this
    integer(I4B), intent(in) :: ipos, ntrail, landflag, ivertcon
    real(DP), intent(in) :: area
    real(DP), intent(in) :: top
    real(DP), intent(in) :: bot
    real(DP), intent(in) :: surfdep
    real(DP), intent(in) :: vks
    real(DP), intent(in) :: thtr
    real(DP), intent(in) :: thts
    real(DP), intent(in) :: thti
    real(DP), intent(in) :: eps
    real(DP), intent(in) :: hgwf 
! ------------------------------------------------------------------------------
    this%landflag = landflag
    this%ivertcon = ivertcon
    this%surfdep = surfdep
    this%uzfarea = area
    this%cellarea = area
    if ( this%landflag == 1 ) then
      this%celtop = top - DHALF*this%surfdep
    else
      this%celtop = top
    end if
    this%celbot = bot
    this%vks = vks
      this%watab = this%celbot
      if ( hgwf > this%celbot ) this%watab = hgwf
      if ( this%watab > this%celtop ) this%watab = this%celtop
    this%watabold = this%watab
    this%thtr = thtr
    this%thts = thts
    this%thti = thti
    this%eps = eps
    this%ntrail = ntrail
    this%pet = DZERO
    this%extdp = DZERO
    this%extwc = DZERO
    this%ha = DZERO
    this%hroot = DZERO
    end subroutine setdata
!
! ------------------------------------------------------------------------------                     
   subroutine setdatafinf(this,finf)
! ******************************************************************************
! setdatafinf -- set infiltration
!
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    !modules
    !arguments
    class(UzfKinematicType) :: this
    real(DP), intent(in) :: finf
! ------------------------------------------------------------------------------
    if (this%landflag == 1) then
      this%sinf = finf
      this%finf = finf
    else
      this%sinf = DZERO
      this%finf = DZERO
    end if
    this%finf_rej = DZERO
    this%surflux = DZERO
    this%surfluxbelow = DZERO
   end subroutine setdatafinf      
! ------------------------------------------------------------------------------
!
! ------------------------------------------------------------------------------                     
   subroutine setdatauzfarea(this,areamult)
! ******************************************************************************
! setdatauzfarea -- set uzfarea using cellarea and areamult
!
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! modules
    ! -- dummy
    class(UzfKinematicType) :: this
    real(DP), intent(in) :: areamult
! ------------------------------------------------------------------------------
    this%uzfarea = this%cellarea * areamult
    !
    ! -- return
    return
   end subroutine setdatauzfarea
   
! ------------------------------------------------------------------------------
!  
   subroutine setdataet(this,thisbelow,jbelow,pet,extdp)
! ******************************************************************************
! setdataet -- set unsat. et variables
!
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    !modules
    !arguments
    class(UzfKinematicType) :: this
    type(UzfKinematicType) :: thisbelow
    integer(I4B), intent(in) :: jbelow
    real(DP), intent(in) :: pet, extdp
    ! -- dummy
    real(DP) :: thick
! ------------------------------------------------------------------------------
    if (this%landflag == 1) then
      this%pet = pet
      this%gwpet = pet
    else
      this%pet = DZERO
      this%gwpet = DZERO
    end if
    thick = this%celtop - this%celbot
    this%extdp = extdp
    if ( this%landflag > 0 ) then
        this%landtop = this%celtop
        this%petmax = this%pet
    end if
    !
    ! set uz extinction depth
    if ( this%landtop - this%extdp < this%celbot ) then
      this%extdpuz = thick
    else
      this%extdpuz = this%celtop - (this%landtop - this%extdp)
    end if
    if ( this%extdpuz < DZERO ) this%extdpuz = DZERO
    if ( this%extdpuz > DEM7 .and. this%extdp < DEM7 ) this%extdp = this%extdpuz
    !
    ! set pet for underlying cell
    if ( jbelow > 0 ) then
      thisbelow%landtop = this%landtop
      thisbelow%petmax = this%petmax
    end if
   end subroutine setdataet   
!
! ------------------------------------------------------------------------------   
   
    subroutine setbelowpet(this,thisbelow,aet)
! ******************************************************************************
! setbelowpet -- subtract aet from pet to calculate residual et 
!                for deeper cells
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    !modules
    !arguments
    class(UzfKinematicType) :: this
    type(UzfKinematicType) :: thisbelow
    real(DP), intent(in) :: aet
    ! -- dummy
    real(DP) :: pet
! ------------------------------------------------------------------------------ 
    pet = DZERO
    if ( thisbelow%extdpuz > DEM3 ) then
       pet = this%petmax - aet
       if ( pet < DZERO ) pet  = DZERO
    end if
    thisbelow%pet = pet
    end subroutine setbelowpet  
!
! ------------------------------------------------------------------------------  
    
   subroutine setdataetwc(this,thisbelow,jbelow,extwc)
! ******************************************************************************
! setdataetwc -- set extinction water content 
!
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    !arguments
    class(UzfKinematicType) :: this
    type(UzfKinematicType) :: thisbelow
    real(DP), intent(in) :: extwc
    integer(I4B), intent(in) :: jbelow
! ------------------------------------------------------------------------------
    this%extwc = extwc
    if ( jbelow > 0 ) thisbelow%extwc = extwc
   end subroutine setdataetwc
!
! ------------------------------------------------------------------------------  
   
   subroutine setdataetha(this,thisbelow,jbelow,ha,hroot,rootact)
! ******************************************************************************
! setdataetha -- set variables for head-based unsat. flow
!
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    !arguments
    class(UzfKinematicType) :: this
    type(UzfKinematicType) :: thisbelow
    real(DP), intent(in) :: ha,hroot,rootact
    integer(I4B), intent(in) :: jbelow
! ------------------------------------------------------------------------------
    this%ha = ha
    this%hroot = hroot
    this%rootact = rootact
    if ( jbelow > 0 ) then
        thisbelow%ha = ha
        thisbelow%hroot = hroot
        thisbelow%rootact = rootact
    end if   
   end subroutine setdataetha
!
! ------------------------------------------------------------------------------
      
    subroutine advance(this)
! ******************************************************************************
! advance -- set variables to advance to new time step. nothing yet.
!
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    class(UzfKinematicType) :: this
! ------------------------------------------------------------------------------
    this%surfseep = DZERO
    end subroutine advance
!
! ------------------------------------------------------------------------------

    subroutine formulate(this,thiswork,thisbelow,ipos,totfluxtot,ietflag,     &
                         issflag,iseepflag,trhs,thcof,hgwf,                   &
                         hgwfml1,cvv,deriv,qfrommvr,qformvr,ierr,sumaet,      &
                         ivertflag)
! ******************************************************************************
! formulate -- formulate the unsaturated flow object, calculate terms for 
!              gwf equation            
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    !modules
    use TdisModule, only: delt
    !arguments
    class(UzfKinematicType) :: this
    type(UzfKinematicType) :: thiswork
    type(UzfKinematicType) :: thisbelow
    integer(I4B), intent(in) :: ipos,ietflag,iseepflag,issflag,ivertflag
    integer(I4B), intent(inout) :: ierr
    real(DP), intent(in) :: hgwf,hgwfml1,cvv,qfrommvr
    real(DP), intent(inout) :: trhs,thcof,qformvr,sumaet
    real(DP), intent(inout) :: totfluxtot
    real(DP), intent(inout) :: deriv
    ! -- dummy
    real(DP) :: test,scale,seep,finfact,derivfinf
    real(DP) :: trhsfinf,thcoffinf,trhsseep,thcofseep,deriv1,deriv2
! ------------------------------------------------------------------------------
    totfluxtot = DZERO
    trhsfinf = DZERO
    thcoffinf = DZERO
    trhsseep = DZERO
    thcofseep = DZERO
    this%finf_rej = DZERO
    this%surflux = this%finf + qfrommvr / this%uzfarea 
    this%surfseep = DZERO
    seep = DZERO
    finfact = DZERO
    deriv1 = DZERO
    deriv2 = DZERO
    derivfinf = DZERO
    this%watab = hgwf
    this%etact = DZERO
    this%surfluxbelow = DZERO
    !
    ! set pet for gw when there is no UZ. 
    this%gwpet = this%pet
    if( ivertflag > 0 ) then
      thisbelow%finf = DZERO
    end if
    !
    ! save wave states for resetting after iteration.
    this%watab = hgwf
    call thiswork%wave_shift(this,0,1,this%nwavst,1)
    if ( this%watab > this%celtop ) this%watab = this%celtop
    !
    if ( this%ivertcon > 0 ) then
      if ( this%watab < this%celbot ) this%watab = this%celbot
    end if
    !
    ! add water from mover to applied infiltration.
    !this%surflux = this%surflux 
    if ( this%surflux > this%vks ) then
      this%surflux = this%vks
    end if
    !
    ! saturation excess rejected infiltration 
      if ( this%landflag==1 ) then
        call this%rejfinf(ipos,deriv1,hgwf,trhsfinf,thcoffinf,finfact)
        this%surflux = finfact
      end if
    !
    ! calculate rejected infiltration
      this%finf_rej =  this%finf + (qfrommvr / this%uzfarea) - this%surflux
      if ( iseepflag > 0 .and. this%landflag==1) then
    !
    ! calculate groundwater discharge
        call this%gwseep(ipos,deriv2,scale,hgwf,trhsseep,thcofseep,seep)
        this%surfseep = seep        
      end if
    !
    ! route water through unsat zone, calc. storage change and recharge
    !
    test = this%watab
    if ( this%watabold - test < -DEM15 ) test = this%watabold
    if ( this%celtop - test > DEM15 ) then
      if ( issflag == 0 ) then
        call this%routewaves(totfluxtot,delt,ietflag,ipos,ierr)  
        if ( ierr > 0 ) return
        call this%uz_rise(totfluxtot)
        this%totflux = totfluxtot
        if( ietflag > 0 .and. this%ivertcon > 0 ) then
            thisbelow%pet = thisbelow%pet - this%etact
            if ( thisbelow%pet < DEM15 ) thisbelow%pet = DEM15
        end if
        if ( this%ivertcon > 0 ) then
          call this%addrech(thisbelow,hgwf,trhsfinf,thcoffinf,derivfinf,delt,0)
        end if
      else
        this%totflux = this%surflux*delt
        totfluxtot = this%surflux*delt
      end if
      thcoffinf = DZERO
      trhsfinf = this%totflux*this%uzfarea/delt
    else
      this%totflux = this%surflux*delt
      totfluxtot = this%surflux*delt
    end if
    deriv =  deriv1 + deriv2 + derivfinf
    trhs = trhsfinf + trhsseep
    thcof = thcoffinf + thcofseep
    !
    ! add spring flow and rejected infiltration to mover
    qformvr = this%surfseep + this%finf_rej*this%uzfarea
    !
    ! reset waves to previous state for next iteration  
    call this%wave_shift(thiswork,0,1,thiswork%nwavst,1)  
    !
    ! distribute PET to deeper cells
    sumaet = sumaet + this%etact
    if( this%ivertcon > 0 ) then
      if ( ietflag > 0 ) then
        call this%setbelowpet(thisbelow,sumaet)
      end if
    end if
    end subroutine formulate 
!
! ------------------------------------------------------------------------------
  
    subroutine budget(this,thisbelow,ipos,totfluxtot,rfinf,rin,rout,rsto,       &
                      ret,retgw,rgwseep,rvflux,ietflag,iseepflag,               &
                      issflag,hgwf,hgwfml1,cvv,numobs,obs_num,                  &
                      obs_depth,obs_theta,qfrommvr,qformvr,qgwformvr,sumaet,    &
                      ierr)
! ******************************************************************************
! budget -- save unsat. conditions at end of time step, calculate budget 
!           terms            
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    !modules
    use TdisModule, only: delt, kper
    !arguments
    class(UzfKinematicType) :: this
    type(UzfKinematicType) :: thisbelow
    integer(I4B), intent(in) :: ipos,ietflag,iseepflag,issflag
    integer(I4B), intent(inout) :: ierr
    integer(I4B), intent(in) :: numobs
    integer(I4B), dimension(:),intent(in) :: obs_num
    real(DP),dimension(:),intent(in) :: obs_depth
    real(DP),dimension(:),intent(inout) :: obs_theta
    real(DP), intent(in) :: hgwf,hgwfml1,cvv,qfrommvr
    real(DP), intent(inout) :: rfinf
    real(DP), intent(inout) :: rin,qformvr,sumaet
    real(DP), intent(inout) :: qgwformvr
    real(DP), intent(inout) :: rout
    real(DP), intent(inout) :: rsto
    real(DP), intent(inout) :: ret,retgw,rgwseep
    real(DP), intent(inout) :: rvflux
    real(DP), intent(inout) :: totfluxtot
    ! -- dummy
    real(DP) :: test, deriv,scale,seep,finfact
    real(DP) :: f1,f2,d1,d2
    real(DP) :: trhsfinf,thcoffinf,trhsseep,thcofseep
    integer(I4B) :: i, j
! ------------------------------------------------------------------------------
    totfluxtot = DZERO
    trhsfinf = DZERO
    thcoffinf = DZERO
    trhsseep = DZERO
    thcofseep = DZERO
    this%finf_rej = DZERO
    this%surflux = this%finf + qfrommvr / this%uzfarea
    this%watab = hgwf
    this%vflow = DZERO
    this%surfseep = DZERO
    seep = DZERO
    finfact = DZERO
    this%etact = DZERO
    this%surfluxbelow = DZERO
    sumaet = DZERO
    !
    ! set pet for gw when there is no UZ. 
    this%gwpet = this%pet
    if ( this%ivertcon > 0 ) then
      thisbelow%finf = dzero
      if ( this%watab < this%celbot ) this%watab = this%celbot
    end if
    if ( this%watab > this%celtop ) this%watab = this%celtop
    if ( this%surflux > this%vks ) then
      this%surflux = this%vks
    end if
    !
    ! infiltration excess -- rejected infiltration 
    if ( this%landflag==1 ) then
      call rejfinf(this,ipos,deriv,hgwf,trhsfinf,thcoffinf,finfact)
      this%surflux = finfact
      if (finfact<this%finf)then
          this%surflux = finfact
      end if
    end if
    !
    ! calculate rejected infiltration
    this%finf_rej =  this%finf  + (qfrommvr / this%uzfarea) - this%surflux
    !
    ! groundwater discharge
    if ( iseepflag > 0 .and. this%landflag == 1 ) then
      call this%gwseep(ipos,deriv,scale,hgwf,trhsseep,thcofseep,seep)
      this%surfseep = seep
      rgwseep = rgwseep + this%surfseep
    end if
    !
    ! sat. to unsat. zone exchange.
    !if ( this%landflag == 0 .and. issflag == 0 ) then
    !  call this%vertcellflow(ipos,ttrhs,hgwf,hgwfml1,cvv)
    !end if
    !rvflux = rvflux + this%vflow
    !
    !route unsaturated flow, calc. storage change and recharge
    test = this%watab
    if ( this%watabold - test < -DEM15 ) test = this%watabold
    if ( this%celtop - test > DEM15 ) then
      if ( issflag == 0 ) then
        call this%routewaves(totfluxtot,delt,ietflag,ipos,ierr) 
        if ( ierr > 0 ) return
        call this%uz_rise(totfluxtot)
        this%totflux = totfluxtot  
        if ( this%ivertcon > 0 ) then
          call this%addrech(thisbelow,hgwf,trhsfinf,thcoffinf,deriv,delt,1)
        end if
      else 
        this%totflux = this%surflux*delt
        totfluxtot = this%surflux*delt  
      end if
      thcoffinf = dzero
      trhsfinf = this%totflux*this%uzfarea/delt
      call this%update_wav(ipos,delt,rout,rsto,ret,ietflag,issflag,0)
    else
      call this%update_wav(ipos,delt,rout,rsto,ret,ietflag,issflag,1)
      totfluxtot = this%surflux*delt
      this%totflux = this%surflux*delt
    end if
    rfinf = rfinf + this%sinf * this%uzfarea
    rin = rin + this%surflux*this%uzfarea - this%surfluxbelow*this%uzfarea
    !
    ! add spring flow and rejected infiltration to mover
    !qformvr = this%surfseep + this%finf_rej*this%uzfarea
    qformvr = this%finf_rej*this%uzfarea
    qgwformvr = this%surfseep
    !
    ! process for observations
    do i = 1, numobs
      j = obs_num(i)
      if (this%watab < this%celtop) then
        if (this%celtop - obs_depth(j) > this%watab) then
          d1 = obs_depth(j) - DEM3
          d2 = obs_depth(j) + DEM3
          f1 = unsat_stor(this, d1)
          f2 = unsat_stor(this, d2)
          obs_theta(j) = this%thtr + (f2 - f1)/(d2 - d1)
        else 
          obs_theta(j) = this%thts
        end if
      else
        obs_theta(j) = this%thts
      end if
    end do
    !
    ! distribute residual PET to deeper cells
    sumaet = sumaet + this%etact
    if (this%ivertcon > 0) then
      if (ietflag > 0) then
        call this%setbelowpet(thisbelow, sumaet)
      end if
    end if
    end subroutine budget
!
! ------------------------------------------------------------------------------
                      
  subroutine vertcellflow(this,ipos,trhs,hgwf,hgwfml1,cvv)
! ******************************************************************************
! vertcellflow -- calculate exchange from sat. to unsat. zones
!                 subroutine not used until sat to unsat flow is supported
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------      
    !modules
    !arguments
    class(UzfKinematicType) :: this
    integer(I4B), intent(in) :: ipos
    real(DP), intent(in) :: hgwf,hgwfml1,cvv
    real(DP), intent(inout) :: trhs
    ! -- dummy
    real(DP) :: Qv,maxvflow,h1,h2,test
! ------------------------------------------------------------------------------
    this%vflow = DZERO
    this%finf = DZERO
!    UZfact(ic,ir,il) = 1.0
    trhs = DZERO
    h1 = hgwfml1
    h2 = hgwf
    test = this%watab
    if ( this%watabold - test < -DEM30 ) test = this%watabold
    if ( this%celtop - test > DEM30 ) then
      !
      ! calc. downward flow using GWF heads and conductance
      Qv = cvv*(h1-h2)
      if ( Qv > DEM30 ) then
        this%vflow = Qv
        this%surflux = this%vflow/this%uzfarea
!          UZfact(ic,ir,il) = dzero
        maxvflow = this%vks*this%uzfarea
        if ( this%vflow - maxvflow > DEM9 ) then
          this%surflux = this%vks
          trhs = this%vflow - maxvflow
          this%vflow = maxvflow
        end if
      end if  
    end if
    return
    end subroutine vertcellflow
!
! ------------------------------------------------------------------------------
    

  subroutine addrech(this,thisbelow,hgwf,trhs,thcof,deriv,delt,it)
! ******************************************************************************
! addrech -- add recharge or infiltration to cells
!
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    !modules
    !arguments
    class(UzfKinematicType) :: this
    type(UzfKinematicType) :: thisbelow
    integer(I4B), intent(in) :: it
    real(DP), intent(inout) :: trhs,thcof,deriv
    real(DP), intent(in) :: delt,hgwf
    ! -- dummy
    real(DP) :: fcheck
    real(DP) :: x,scale,range
! ------------------------------------------------------------------------------ 
    range = DEM5
    deriv = DZERO
    thcof = DZERO
    trhs = this%uzfarea*this%totflux/delt
    if ( this%totflux < DEM14 ) return
    scale = DONE
    !
    ! smoothly reduce flow between cells when head close to cell top 
    x = (hgwf-(this%celbot-range))  
    call sSCurve(x,range,deriv,scale)
    deriv = this%uzfarea*deriv*this%totflux/delt
    thisbelow%finf = (DONE-scale)*this%totflux/delt 
    fcheck = thisbelow%finf - thisbelow%vks
    !
    ! reduce flow between cells when vks is too small
    if ( fcheck < DEM14 ) fcheck = DZERO
    thisbelow%finf = thisbelow%finf - fcheck
    this%surfluxbelow = thisbelow%finf
    this%totflux = scale*this%totflux + fcheck*delt
    trhs = this%uzfarea*this%totflux/delt
    end subroutine addrech
!
! ------------------------------------------------------------------------------
    subroutine rejfinf(this,ipos,deriv,hgwf,trhs,thcof,finfact)
! ******************************************************************************
! rejfinf -- reject applied infiltration due to low vks
!
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
      !modules
      !arguments
      class(UzfKinematicType) :: this
      integer(I4B), intent(in) :: ipos
      real(DP), intent(inout) :: deriv,finfact,thcof,trhs
      real(DP), intent(in) :: hgwf
      ! -- dummy
      real(DP) :: x,range,scale,q 
! ------------------------------------------------------------------------------
      range = this%surfdep
      q = (this%surflux)
      finfact = q
      trhs = finfact*this%uzfarea
      x = (this%celtop-hgwf)
      call sLinear(x,range,deriv,scale)
      deriv = -q*deriv*this%uzfarea*scale
      if ( scale < DONE ) then
        finfact = q*scale
        trhs = finfact*this%uzfarea*this%celtop/range
        thcof = finfact*this%uzfarea/range
      end if
    end subroutine rejfinf
!
! ------------------------------------------------------------------------------
  
    subroutine gwseep(this,ipos,deriv,scale,hgwf,trhs,thcof,seep)
! ******************************************************************************
! gwseep -- calc. groudwater discharge to land surface
!
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
      !modules
      !argument
      class(UzfKinematicType) :: this
      integer(I4B), intent(in) :: ipos
      real(DP), intent(inout) :: deriv,trhs,thcof,seep
      real(DP), intent(out) :: scale
      real(DP), intent(in) :: hgwf
      ! -- dummy
      real(DP) :: x,range,y,deriv1,d1,d2,Q
! ------------------------------------------------------------------------------
      seep = DZERO
      deriv = DZERO
      deriv1 = DZERO
      d1 = DZERO
      d2 = DZERO
      scale = DZERO
      Q = this%uzfarea*this%vks
      range = this%surfdep
      x = (hgwf-this%celtop)
      call sCubicLinear(x,range,deriv1,y)
      scale = y
      seep = scale*Q*(hgwf-this%celtop)/range
      trhs = scale*Q*this%celtop/range
      thcof = -scale*Q/range
      d1 = -deriv1*Q*x/range
      d2 = -scale*Q/range
      deriv = d1 + d2
      if ( seep < DZERO ) then
        seep = DZERO
        deriv = DZERO
        trhs = DZERO
        thcof = DZERO
      end if
    end subroutine gwseep
!
! ------------------------------------------------------------------------------

    subroutine simgwet(this,igwetflag,ipos,hgwf,trhs,thcof,et,det)
! ******************************************************************************
! simgwet -- calc. gwf et using residual uzf pet
!
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
      !modules
      !arguments
      class(UzfKinematicType) :: this
      integer(I4B), intent(in) :: igwetflag,ipos
      real(DP), intent(in) :: hgwf
      real(DP), intent(inout) :: trhs,thcof,det,et
      ! -- dummy
      real(DP) :: s,x,c
      external :: etfunc_lin, etfunc_nlin
      real(DP) :: etfunc_lin, etfunc_nlin
! ------------------------------------------------------------------------------      
      this%gwet = DZERO
      s = this%landtop
      x = this%extdp
      c = this%gwpet
      if ( x < DEM6 ) return
      if ( igwetflag==1 ) then
        et = this%etfunc_lin(s,x,c,det,trhs,thcof,hgwf)
      else if ( igwetflag==2 ) then
        et = this%etfunc_nlin(s,x,c,det,trhs,thcof,hgwf)
      end if
      this%gwet = et*this%uzfarea
      trhs = -trhs*this%uzfarea
      thcof = thcof*this%uzfarea
      return
      end subroutine simgwet
!
! ------------------------------------------------------------------------------      
      
      function etfunc_lin(this,s,x,c,det,trhs,thcof,hgwf)
! ******************************************************************************
! etfunc_lin -- calc. gwf et using linear ET function from mf-2005
!
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------    
      !modules
      ! -- return
      real(DP) :: etfunc_lin
      ! -- dummy
      class(UzfKinematicType) :: this
      real(DP) :: etgw
      real(DP) :: range
      real(DP) :: depth,scale,thick
      ! -- local
      real(DP), intent(inout) :: det
      real(DP), intent(in) :: s,x,c
      real(DP), intent(inout) :: trhs,thcof
      real(DP), intent(in) :: hgwf
! ------------------------------------------------------------------------------
      !
      ! Between ET surface and extinction depth
      trhs = DZERO
      thcof = DZERO
      det = DZERO
      if ( hgwf > (s-x) .and. hgwf < s ) THEN
        etgw = (c*(hgwf-(s-x))/x)
        if ( etgw > c ) then
          etgw = c
        else
          trhs = c - c*s/x
          thcof = -c/x
          etgw = trhs-(thcof*hgwf)
        end if
      !
      ! Above land surface
      else if ( hgwf >= s ) then           
        trhs = c
        etgw = c
      !
      ! Below extinction depth
      else
        etgw = DZERO
      end if
      depth = hgwf - (s - x)
      thick = this%celtop-this%celbot
      if (depth > thick ) depth = thick
      if ( depth < dzero ) depth = dzero
      range = DEM4*x
      call sCubic(depth,range,det,scale)
      etgw = scale*etgw
      det = -det*etgw   
      etfunc_lin = etgw
      end function etfunc_lin
!
! ------------------------------------------------------------------------------

      function etfunc_nlin(this,s,x,c,det,trhs,thcof,hgwf)
! ******************************************************************************
! etfunc_nlin -- Square-wave ET function with smoothing at extinction depth
!
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
! ------------------------------------------------------------------------------
      ! -- return
      real(DP) :: etfunc_nlin
      ! -- dummy
      class(UzfKinematicType) :: this
      real(DP), intent(inout) :: det
      real(DP), intent(in) :: s,x,c,hgwf
      ! -- local
      real(DP), intent(inout) :: trhs,thcof
      real(DP) :: etgw
      real(DP) :: range
      real(DP) :: depth,scale
! ------------------------------------------------------------------------------
      det = DZERO 
      trhs = DZERO
      thcof = DZERO
      depth = hgwf - (s - x)
      if ( depth < DZERO ) depth = DZERO
      etgw = c
      range = DEM3*x
      call sCubic(depth,range,det,scale)
      etgw = etgw*scale
      trhs = -etgw
      det = -det*etgw
      etfunc_nlin = etgw
      return
      end function etfunc_nlin
!
! ------------------------------------------------------------------------------
  
    subroutine uz_rise(this,totfluxtot)
! ******************************************************************************
! uz_rise -- calculate recharge due to a rise in the gwf head
!
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
! ------------------------------------------------------------------------------
    class(UzfKinematicType) :: this
    real(DP), intent(inout) :: totfluxtot
    real(DP) :: fm1,fm2,d1
! ------------------------------------------------------------------------------
    !
    ! additional recharge from a rising water table
    if ( this%watab-this%watabold > DEM30 ) then
      d1 = this%celtop-this%watabold
      fm1 = this%unsat_stor(d1)
      d1 = this%celtop-this%watab
      fm2 = this%unsat_stor(d1)
      totfluxtot = totfluxtot + (fm1-fm2)
    end if
    end subroutine uz_rise
!
! ------------------------------------------------------------------------------
    
    subroutine setwaves(this,ipos)
! ******************************************************************************
! setwaves -- reset waves to default values at start of simulation
!
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------   
! ------------------------------------------------------------------------------
    class(UzfKinematicType) :: this
    integer(I4B), intent(in) :: ipos
    real(DP) :: bottom, top
    integer(I4B) :: jk
    real(DP) :: thick
! ------------------------------------------------------------------------------
    this%uzstor = DZERO
    this%delstor = DZERO
    this%totflux = DZERO
    this%nwavst = 1
    this%uzdpst = DZERO
    thick = this%celtop - this%watab
    do jk = 1, this%nwav
      this%uzthst(jk) = this%thtr
    end do
    !
    ! initialize waves for first stress period
    if ( thick > DZERO ) then
      this%uzdpst(1) = thick
      this%uzthst(1) = this%thti
      top = this%uzthst(1) - this%thtr
      if ( top < DZERO ) top = DZERO
      bottom = this%thts - this%thtr
      if ( bottom < DZERO ) bottom = DZERO
      this%uzflst(1) = this%vks*(top/bottom)**this%eps
      if ( this%uzthst(1) < this%thtr ) this%uzthst(1) = this%thtr
      !
      ! calculate water stored in the unsaturated zone
      if ( top > DZERO ) then
        this%uzstor = this%uzdpst(1)*top*this%uzfarea
        this%uzspst(1) = DZERO
        this%uzolsflx = this%uzflst(1)
      else
        this%uzstor = DZERO
        this%uzflst(1) = DZERO
        this%uzspst(1) = DZERO
        this%uzolsflx = DZERO
      end if
      !
      ! no unsaturated zone
    else
      this%uzflst(1) = DZERO
      this%uzdpst(1) = DZERO
      this%uzspst(1) = DZERO
      this%uzthst(1) = this%thtr
      this%uzstor = DZERO   
      this%uzolsflx = this%finf
    end if
    return
    end subroutine
!
! ------------------------------------------------------------------------------
    
    subroutine routewaves(this,totfluxtot,delt,ietflag,ipos,ierr)
! ******************************************************************************
! routewaves -- prepare and route waves over time step
!
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
! ------------------------------------------------------------------------------
    class(UzfKinematicType) :: this
    real(DP), intent(inout) :: totfluxtot
    real(DP), intent(in) :: delt
    integer(I4B), intent(in) :: ietflag
    integer(I4B), intent(in) :: ipos
    integer(I4B), intent(inout) :: ierr
    real(DP) :: thick, thickold
    integer(I4B) :: idelt, iwav, ik
! ------------------------------------------------------------------------------
      this%totflux = DZERO
      this%etact = DZERO
      thick = this%celtop - this%watab
      thickold =   this%celtop - this%watabold
      !
      ! no uz, clear waves
      if ( thickold < DZERO ) then
        do iwav = 1, 6
          this%uzthst(iwav) = this%thtr
          this%uzdpst(iwav) = DZERO
          this%uzspst(iwav) = DZERO
          this%uzflst(iwav) = DZERO
          this%nwavst = 1
        end do
      end if
      idelt = 1
      do ik = 1, idelt
       call this%uzflow(thick,thickold,delt,ietflag,ipos,ierr)
       if ( ierr > 0 ) return
        totfluxtot = totfluxtot + this%totflux
      end do
    ! set residual pet after uz et    
    this%gwpet = this%pet - this%etact/delt
    if ( this%gwpet < DZERO ) this%gwpet = DZERO
    return
    end subroutine routewaves
!
! ------------------------------------------------------------------------------
  
      subroutine wave_shift(this1,this2,shft,strt,stp,cntr)
! ******************************************************************************
! wave_shift -- copy waves or shift waves in arrays
!
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
      class (UzfKinematicType) :: this1
      type (UzfKinematicType) :: this2
      integer(I4B) :: j,shft,strt,stp,cntr
! ------------------------------------------------------------------------------
      do j = strt, stp, cntr
        this1%uzthst(j) = this2%uzthst(j+shft)
        this1%uzdpst(j) = this2%uzdpst(j+shft)
        this1%uzflst(j) = this2%uzflst(j+shft)
        this1%uzspst(j) = this2%uzspst(j+shft)
      end do
      this1%nwavst = this2%nwavst
      return
      end subroutine
!
! ------------------------------------------------------------------------------
      
      subroutine uzflow(this,thick,thickold,delt,ietflag,ipos,ierr)
!     ******************************************************************
!     uzflow----moc solution for kinematic wave equation
!     ******************************************************************
!     SPECIFICATIONS:
! ------------------------------------------------------------------------------
      !modules
      ! -- dummy
      class (UzfKinematicType) :: this
      real(DP), intent(inout) :: thickold
      real(DP), intent(inout) :: thick
      real(DP), intent(in) :: delt
      integer(I4B), intent(in) :: ietflag
      integer(I4B), intent(in) :: ipos
      integer(I4B), intent(inout) :: ierr
      real(DP) :: ffcheck,time,feps1,feps2
      real(DP) :: thetadif,thetab,fluxb,oldsflx
      integer(I4B) :: itrailflg,itester
!     ------------------------------------------------------------------
      time = DZERO
      this%totflux = DZERO      
      itrailflg = 0
      oldsflx = this%uzflst(this%nwavst)
      call this%factors(feps1,feps2)
      !check for falling or rising water table
      if ( (thick-thickold) > feps1 ) then
        thetadif = abs(this%uzthst(1)-this%thtr)
        if ( thetadif > DEM6 ) then
          call this%wave_shift(this,-1,this%nwavst+1,2,-1)
          if ( this%uzdpst(2) < DEM30 ) this%uzdpst(2) = (this%ntrail+DTWO)*DEM6
          if ( this%uzthst(2) > this%thtr ) then
            this%uzspst(2) = this%uzflst(2)/(this%uzthst(2)-this%thtr)
          else
            this%uzspst(2) = DZERO
          end if
          this%uzthst(1) = this%thtr
          this%uzflst(1) = DZERO
          this%uzspst(1) = DZERO
          this%uzdpst(1) = thick
          this%nwavst = this%nwavst + 1
          if ( this%nwavst.GE.this%nwav ) then
            !too many waves error
            ierr = 1
            return
          end if
        else
          this%uzdpst(1) = thick
        end if
      end if
      thetab = this%uzthst(1)
      fluxb = this%uzflst(1)
      this%totflux = DZERO
      itester = 0
      ffcheck = (this%surflux-this%uzflst(this%nwavst))
      !
      !crease new waves in infiltration changes
      if ( ffcheck > feps2 .OR. ffcheck < -feps2 ) then
        this%nwavst = this%nwavst + 1
        if ( this%nwavst.GE.this%nwav ) then
          !
          !too many waves error
          ierr = 1
          return
        end if
      else if ( this%nwavst.EQ.1 ) then
        itester = 1
      end if
      if ( this%nwavst > 1 ) then
        IF ( ffcheck < -feps2) THEN
          call this%trailwav(ierr)
          if ( ierr > 0 ) return
          itrailflg = 1
        end if
        call this%leadwav(time,itester,itrailflg,thetab,fluxb,ffcheck,feps2,delt,ipos)
      end if
      if ( itester.EQ.1 ) then
        this%totflux = this%totflux + (delt-time)*this%uzflst(1)
        time = DZERO
        itester = 0
      end if
      !
      !simulate et
      if ( ietflag > 0  ) call this%uzet(delt,ietflag,ierr)
      if ( ierr > 0 ) return
      return
      end subroutine uzflow
!
! ------------------------------------------------------------------------------
      
    subroutine factors(this,feps1,feps2)
! ******************************************************************************
!     factors----calculate unit specific tolerances
! ******************************************************************************
!     SPECIFICATIONS:
! ------------------------------------------------------------------------------
    !modules
    ! -- dummy
    class (UzfKinematicType) :: this
    real(DP), intent(out) :: feps1
    real(DP), intent(out) :: feps2
    real(DP) :: factor1
    real(DP) :: factor2
    ! calculate constants for uzflow
      factor1 = DONE
      factor2 = DONE
      feps1 = DEM9
      feps2 = DEM9
      if ( ITMUNI.EQ.1 ) then
        factor1 = DONE/86400.D0
      else if ( ITMUNI.EQ.2 ) then
        factor1 = DONE/1440.D0
      else if ( ITMUNI.EQ.3 ) then
        factor1 = DONE/24.0D0 
      else if ( ITMUNI.EQ.5 ) then
        factor1 = 365.0D0
      end if    
      factor2 = DONE/0.3048
      feps1 = feps1*factor1*factor2
      feps2 = feps2*factor1*factor2
    end subroutine factors
!
! ------------------------------------------------------------------------------
    
      subroutine trailwav(this,ierr)
! ******************************************************************************
!     trailwav----create and set trail waves
! ******************************************************************************
!     SPECIFICATIONS:
! ------------------------------------------------------------------------------
      !modules
      !arguments
      integer(I4B), intent(inout) :: ierr
      ! -- dummy
      class (UzfKinematicType) :: this
      real(DP) :: smoist, smoistinc, ftrail, eps_m1
      real(DP) :: thtsrinv
      real(DP) :: flux1,flux2,theta1,theta2
      real(DP) :: fnuminc
      integer(I4B) :: j,jj,jk,nwavstm1
!     ------------------------------------------------------------------
      eps_m1 = dble(this%eps) - DONE
      thtsrinv = DONE/(this%thts-this%thtr)
      nwavstm1 = this%nwavst - 1
      !initialize trailwaves
      smoist = (((this%surflux/this%vks)**(DONE/this%eps))*     &
                 (this%thts-this%thtr)) + this%thtr
      if ( this%uzthst(nwavstm1)-smoist > DEM9 ) then
        fnuminc = DZERO
        do jk = 1, this%ntrail
          fnuminc = fnuminc + float(jk)
        end do
        smoistinc = (this%uzthst(nwavstm1)-smoist)/(fnuminc-DONE)
        jj = this%ntrail
        ftrail = dble(this%ntrail) + DONE
        do j = this%nwavst, this%nwavst + this%ntrail - 1
          if ( j > this%nwav ) then
            ! too many waves error
            ierr = 1
            return
          end if
          if( j > this%nwavst ) then
            this%uzthst(j) = this%uzthst(j-1)                             &
                              - ((ftrail-float(jj))*smoistinc)
          else
            this%uzthst(j) = this%uzthst(j-1) - DEM9
          end if
          jj = jj - 1
          if ( this%uzthst(j).LE.this%thtr+DEM9 ) this%uzthst(j)         &
               = this%thtr + DEM9
          this%uzflst(j) = this%vks*(((this%uzthst(j)-this%thtr)          &
                           *thtsrinv)**this%eps)
          theta2 = this%uzthst(j-1)
          flux2 = this%uzflst(j-1)
          flux1 = this%uzflst(j)
          theta1 = this%uzthst(j)
          this%uzspst(j) = this%leadspeed(theta1,theta2,flux1,flux2)
          this%uzdpst(j) = DZERO
          if ( j==this%nwavst ) then
            this%uzdpst(j) = this%uzdpst(j) + (this%ntrail+1)*DEM9
          else
            this%uzdpst(j) = this%uzdpst(j-1) - DEM9
          end if
        end do
        this%nwavst = this%nwavst + this%ntrail - 1
        if ( this%nwavst.GE.this%nwav ) then
          !too many waves error
          ierr = 1
          return
        end if
      else
        this%uzdpst(this%nwavst) = DZERO
        this%uzflst(this%nwavst) = this%vks*(((this%uzthst(this%nwavst)-   &
                           this%thtr)*thtsrinv)**this%eps)
        this%uzthst(this%nwavst) = smoist
        theta2 = this%uzthst(this%nwavst-1)
        flux2 = this%uzflst(this%nwavst-1)
        flux1 = this%uzflst(this%nwavst)
        theta1 = this%uzthst(this%nwavst)
        this%uzspst(this%nwavst) = this%leadspeed(theta1,theta2,flux1,flux2)
      end if
      return
      end subroutine trailwav
      
      
      subroutine leadwav(this,time,itester,itrailflg,thetab,fluxb,  &
                         ffcheck,feps2,delt,ipos)
!     ******************************************************************
!     leadwav----create a lead wave and route over time step
!     ******************************************************************
!     SPECIFICATIONS:
! ----------------------------------------------------------------------
      !modules
      !arguments
      class (UzfKinematicType) :: this
      real(DP), intent(inout) :: thetab
      real(DP), intent(inout) :: fluxb
      real(DP), intent(in) :: feps2
      real(DP), intent(inout) :: time
      integer(I4B), intent(inout) :: Itester, Itrailflg
      real(DP), intent(inout) :: ffcheck
      real(DP), intent(in) :: delt
      integer(I4B), intent(in) :: ipos
      ! -- dummy
      real(DP) :: bottomtime,shortest,fcheck
      real(DP) :: eps_m1,timenew,bottom,timedt
      real(DP) :: thtsrinv,diff,fluxhld2
      real(DP) :: flux1,flux2,theta1,theta2,ftest
      real(DP), allocatable, dimension(:) :: checktime
      integer(I4B) :: iflx,iremove,j,l
      integer(I4B) :: nwavp1, jshort
      integer(I4B), allocatable, dimension(:) :: more
!     ------------------------------------------------------------------
      allocate(checktime(this%nwavst))
      allocate(more(this%nwavst))
      ftest = DZERO
      eps_m1 = dble(this%eps) - DONE
      thtsrinv = DONE/(this%thts-this%thtr)
      !
      !initialize new wave
      if ( Itrailflg.EQ.0 ) then
        if ( ffcheck > feps2 ) then
          this%uzflst(this%nwavst) = this%surflux
          IF ( this%uzflst(this%nwavst) < DEM30 )                &
               this%uzflst(this%nwavst) = DZERO
          this%uzthst(this%nwavst) =                             &
                     (((this%uzflst(this%nwavst)/this%vks)**     &
                     (DONE/this%eps))*(this%thts-this%thtr))     &
                      + this%thtr
          theta2 = this%uzthst(this%nwavst)
          flux2 = this%uzflst(this%nwavst)
          flux1 = this%uzflst(this%nwavst-1)
          theta1 = this%uzthst(this%nwavst-1)
          this%uzspst(this%nwavst) = this%leadspeed(theta1,theta2,flux1,flux2)  
          this%uzdpst(this%nwavst) = DZERO
        end if
      end if
      !
      !route all waves and interception of waves over times step
      diff = DONE
      timedt = DZERO
      iflx = 0
      fluxhld2 = this%uzflst(1)
      if ( this%nwavst.EQ.0 ) Itester = 1
      if ( Itester.NE.1 ) then
      do while ( diff > DEM6 )
        nwavp1 = this%nwavst + 1
        timedt = delt - Time
        do j = 1, this%nwavst
          checktime(j) = DEP20
          more(j) = 0
        end do
        shortest = timedt
        if ( this%nwavst > 2 ) then
          j = 2
          !
          !calculate time until wave overtakes wave ahead
          nwavp1 = this%nwavst + 1
          do while ( j < nwavp1 )
            ftest = this%uzspst(j-1)-this%uzspst(j)
            if ( abs(ftest) > DEM30 ) then
              checktime(j) = (this%uzdpst(j)-this%uzdpst(j-1))/(ftest)
              IF ( checktime(j) < DEM30 ) checktime(j) = DEP20
            end if
            j = j + 1
          end do
        end if
        !
        !calc time until wave reaches bottom of cell
        bottomtime = DEP20
        if ( this%nwavst > 1 ) then
          if ( this%uzspst(2) > DZERO ) then
            bottom = this%uzspst(2)
            if ( bottom < DEM15 ) bottom = DEM15
            bottomtime = (this%uzdpst(1)-this%uzdpst(2))/bottom
            if ( bottomtime < DZERO ) bottomtime = DEM12
          end if
        end if
        !
        !calc time for wave interception
        jshort = 0
        do j = this%nwavst, 3, -1
          if ( shortest-checktime(j) > -DEM9 ) then
            more(j) = 1
            jshort = j
            shortest = checktime(j)
          end if
        end do
        do j = 3, this%nwavst
          if ( shortest-checktime(j) < DEM9 ) then
              if ( j.ne.jshort) more(j) = 0
          end if
        end do
        !
        !what happens first, waves hits bottom or interception
        iremove = 0
        timenew = Time
        fcheck = (Time+shortest) - delt
        if ( shortest < DEM7 ) fcheck = -DONE
        if ( bottomtime < shortest .AND. Time+bottomtime < delt ) then
          j = 2
          do while ( j < nwavp1 )
            !
            !route waves
            this%uzdpst(j) = this%uzdpst(j) + this%uzspst(j)             &
                                *bottomtime
            j = j + 1
          end do
          fluxb = this%uzflst(2)
          thetab = this%uzthst(2)
          iflx = 1
          call this%wave_shift(this,1,1,this%nwavst-1,1)
          iremove = 1
          timenew = Time + bottomtime
          this%uzspst(1) = DZERO
          !
          !do waves intercept before end of time step
        else if ( fcheck < DZERO .AND. this%nwavst > 2 ) then
          j = 2
          do while ( j < nwavp1 )
            this%uzdpst(j) = this%uzdpst(j) + this%uzspst(j)             &
                                *shortest
            j = j + 1
          end do
          !
          !combine waves that intercept, remove a wave
          j = 3
          l = j
          do while ( j < this%nwavst+1 )          
            if ( more(j).EQ.1 ) then
              l = j
              theta2 = this%uzthst(j)
              flux2 = this%uzflst(j)
              if ( j.EQ.3 ) then
                flux1 = fluxb
                theta1 = thetab
              else
                flux1 = this%uzflst(j-2)
                theta1 = this%uzthst(j-2)
              end if
              this%uzspst(j) = this%leadspeed(theta1,theta2,flux1,flux2)
              !
              !update waves.
              call this%wave_shift(this,1,l-1,this%nwavst-1,1)
              l = this%nwavst + 1
              iremove = iremove + 1
            end if
            j = j + 1
          end do
          timenew = timenew + shortest
          !
          !calc. total flux to bottom during remaining time in step
        else
          j = 2
          do while ( j < nwavp1 )
            this%uzdpst(j) = this%uzdpst(j) + this%uzspst(j)        &
                                *timedt
            j = j + 1
          end do
          timenew = delt
        end if
        this%totflux = this%totflux + fluxhld2*(timenew-time)
        if ( iflx.EQ.1 ) then
          fluxhld2 = this%uzflst(1)
          iflx = 0
        end if
        !
        !remove dead waves
        this%nwavst = this%nwavst - iremove       
        Time = timenew
        diff = delt - Time
        if ( this%nwavst.EQ.1 ) then
          Itester = 1
          exit
        end if
      end do
      end if
      deallocate(checktime)
      deallocate(more)
      return
      end subroutine leadwav
!
                         
! 
      function leadspeed(this,theta1,theta2,flux1,flux2)
!     ******************************************************************
!     leadspeed----calculates waves speed from dflux/dtheta
!     ******************************************************************
!     SPECIFICATIONS:
! ----------------------------------------------------------------------
      !modules
      !arguments
      class (UzfKinematicType) :: this
      real(DP), intent(in) :: theta1
      real(DP), intent(in) :: theta2
      real(DP), intent(in) :: flux1
      real(DP), intent(inout) :: flux2
      ! -- dummy
      real(DP) :: comp1, comp2, thsrinv, epsfksths
      real(DP) :: eps_m1, fhold, comp3
      real(DP) :: leadspeed
! ----------------------------------------------------------------------
      eps_m1 = dble(this%eps) - DONE
      thsrinv = DONE/(this%thts-this%thtr)
      epsfksths = this%eps*this%vks*thsrinv
      comp1 = theta2-theta1
      comp2 = abs(flux2-flux1)
      comp3 = theta1 - this%thtr
      if ( comp2 < DEM15 ) flux2 = flux1 + DEM15
      if ( abs(comp1) < DEM30 ) then
        if ( comp3 > DEM30 ) fhold = ((comp3)*thsrinv)**this%eps
        if ( fhold < DEM30 ) fhold = DEM30
        leadspeed = epsfksths * (fhold**eps_m1)    
      else
        leadspeed = (flux2-flux1)/(theta2-theta1) 
      end if
      if ( leadspeed < DEM30 ) leadspeed = DEM30
      end function leadspeed
!
! ----------------------------------------------------------------------
    
      function unsat_stor(this,d1)
!     ******************************************************************
!     unsat_stor---- sums up mobile water over depth interval
!     ******************************************************************
!     SPECIFICATIONS:
! ----------------------------------------------------------------------
      !modules
      !arguments
      class (UzfKinematicType) :: this
      real(DP), intent(inout) :: d1
      ! -- dummy
      real(DP) :: fm, unsat_stor
      integer(I4B) :: j, k,nwavm1,jj
! ----------------------------------------------------------------------
      fm = DZERO
      j = this%nwavst + 1
      k = this%nwavst
      nwavm1 = k-1
      if ( d1 > this%uzdpst(1) ) d1 = this%uzdpst(1)
      !
      !find deepest wave above depth d1, counter held as j
      do while ( k > 0 )
        if ( this%uzdpst(k) - d1 < -DEM30) j = k
          k = k - 1
      end do
      if ( j > this%nwavst ) then
        fm = fm + (this%uzthst(this%nwavst)-this%thtr)*d1
      elseif ( this%nwavst > 1 ) then
        if ( j > 1 ) then
          fm = fm + (this%uzthst(j-1)-this%thtr)              &
                     *(d1-this%uzdpst(j))
        end if
        do jj = j, nwavm1
          fm = fm + (this%uzthst(jj)-this%thtr)               &
                     *(this%uzdpst(jj)                        &
                      -this%uzdpst(jj+1))
        end do
        fm = fm + (this%uzthst(this%nwavst)-this%thtr)        &
                    *(this%uzdpst(this%nwavst))
      else
        fm = fm + (this%uzthst(1)-this%thtr)*d1
      end if
      unsat_stor = fm
      end function unsat_stor
!
! ----------------------------------------------------------------------
    
      subroutine update_wav(this,ipos,delt,rout,rsto,ret,etflg,iss,itest)
!     ******************************************************************
!     update_wav---- update to new state of uz at end of time step
!     ******************************************************************
!     SPECIFICATIONS:
! ----------------------------------------------------------------------
      !modules
      !arguments
      class (UzfKinematicType) :: this
      integer(I4B), intent(in) :: ipos,etflg,itest,iss
      real(DP), intent(in) :: delt
      real(DP), intent(inout) :: rout
      real(DP), intent(inout) :: rsto
      real(DP), intent(inout) :: ret
      ! -- dummy
      real(DP) :: uzstorhold,bot,fm,depthsave,top
      real(DP) :: thick,thtsrinv
      integer(I4B) :: nwavhld, k,j
! ----------------------------------------------------------------------
!
      bot = this%watab
      top = this%celtop
      thick = top-bot
      nwavhld = this%nwavst      
      if ( itest == 1 ) then
!        this%uzflst(1) = this%surflux   !rgn 5/25/17
        this%uzflst(1) = DZERO
        this%uzthst(1) = this%thtr
        this%delstor = - this%uzstor
        this%uzstor = DZERO
        uzstorhold = DZERO
        rout = rout + this%totflux*this%uzfarea/delt
        return
      end if
      if ( iss == 1 ) then          
        if ( this%thts-this%thtr < DEM7 ) then
          thtsrinv = DONE/DEM7
        else
          thtsrinv = DONE/(this%thts-this%thtr) 
        end if
        this%totflux = this%surflux*delt
        this%watabold = this%watab
        this%uzthst(1) = this%thti
        this%uzflst(1) = this%vks*(((this%uzthst(1)-this%thtr)          &
                           *thtsrinv)**this%eps)
        this%uzdpst(1) = thick
        this%uzspst(1) = thick
        this%nwavst = 1
        this%uzstor = thick*(this%thti-this%thtr)*this%uzfarea
        this%delstor = DZERO
        rout = rout + this%totflux*this%uzfarea/delt
      else
        !
        !water table rises through waves      
        if ( this%watab - this%watabold > DEM30 ) then
          depthsave = this%uzdpst(1)
          j = 0
          k = this%nwavst
          do while ( k > 0 )
            if ( this%uzdpst(k) - thick < -DEM30) j = k
            k = k - 1
          end do
          this%uzdpst(1) = thick
          if ( j > 1 ) then    
            this%uzspst(1) = dzero
            this%nwavst = this%nwavst - j + 2
            this%uzthst(1) = this%uzthst(j-1)
            this%uzflst(1) = this%uzflst(j-1)
            if ( j > 2 ) call this%wave_shift(this,j-2,2,nwavhld-(j-2),1)      
          elseif ( j == 0 ) then
            this%uzspst(1) = dzero
            this%uzthst(1) = this%uzthst(this%nwavst)
            this%uzflst(1) = this%uzflst(this%nwavst) 
            this%nwavst = 1
          end if
        end if    
        !
        !calculate new unsat. storage 
        if ( thick > DZERO ) then
          fm = this%unsat_stor(thick)
          uzstorhold = this%uzstor
          this%uzstor = fm*this%uzfarea
          this%delstor = this%uzstor - uzstorhold
        else
          this%uzspst(1) = DZERO
          this%nwavst = 1
          this%uzthst(1) = this%thtr
          this%uzflst(1) = DZERO
          this%delstor = - this%uzstor
          this%uzstor = DZERO
          uzstorhold = DZERO
        end if
        this%watabold = this%watab
        rout = rout + this%totflux*this%uzfarea/delt
        rsto = rsto + this%delstor/delt
        if ( etflg > 0 ) ret = ret + this%etact*this%uzfarea/delt
      end if
      end subroutine
      
      subroutine uzet(this,delt,ietflag,ierr)
!     ******************************************************************
!     uzet---- remove water from uz due to et
!     ******************************************************************
!     SPECIFICATIONS:
! ----------------------------------------------------------------------
      !modules
      ! -- dummy
      class (UzfKinematicType) :: this
      real(DP), intent(in) :: delt
      integer(I4B), intent(in) :: ietflag
      integer(I4B), intent(inout) :: ierr
      ! -- local
      type(UzfKinematicType), pointer :: uzfktemp
      real(DP) :: diff,thetaout,fm,st
      real(DP) :: thtsrinv,epsfksthts,fmp
      real(DP) :: fktho,theta1,theta2,flux1,flux2
      real(DP) :: hcap,ha,factor,tho,depth
      real(DP) :: extwc1,petsub
      integer(I4B) :: i,j,jhold,jk,kj,kk,numadd,k,nwv,itest
!     ------------------------------------------------------------------
    this%etact = DZERO
    if ( this%extdpuz < DEM7 ) return
    petsub = this%rootact*this%pet*this%extdpuz/this%extdp
    thetaout = delt*petsub/this%extdp
    if ( ietflag==1 ) thetaout = delt*this%pet/this%extdp
    if ( thetaout < DEM10 ) return
    depth = this%uzdpst(1)
    st = this%Unsat_stor(depth)
    if ( st < DEM4 ) return
    !
    !allocate temporary wave storage.      
    allocate(uzfktemp)
    allocate(uzfktemp%uzdpst(this%nwavst))
    allocate(uzfktemp%uzthst(this%nwavst))
    allocate(uzfktemp%uzflst(this%nwavst))
    allocate(uzfktemp%uzspst(this%nwavst))
    allocate(uzfktemp%nwavst)
    ha = this%ha
    nwv = this%nwavst
    itest = 0
    !
    ! store original wave characteristics
    call uzfktemp%wave_shift(this,0,1,Nwv,1)
    factor = 1.0d0
    this%etact = dzero
    if ( this%thts-this%thtr < DEM7 ) then
      thtsrinv = 1.0/DEM7
    else
      thtsrinv = DONE/(this%thts-this%thtr) 
    end if
    epsfksthts = this%eps*this%vks*thtsrinv
    this%Etact = DZERO
    fmp = DZERO
    extwc1 = this%extwc - this%thtr
    if ( extwc1 < DEM6 ) extwc1 = DEM7
    numadd = 0
    fm = st
    k = 0
    !loop for reducing aet to pet when et is head dependent
    do while ( itest == 0 )
      k = k + 1
      if ( k > 1 .AND. ABS(fmp-petsub) > DEM5*petsub) factor = factor/(fm/petsub)
      !
      !one wave shallower than extdp
      if ( this%nwavst == 1 .AND. this%uzdpst(1) <= this%extdpuz ) then
        if ( ietflag == 2 ) then
          tho = this%uzthst(1)
          fktho = this%uzflst(1)
          hcap = this%caph(tho)
          thetaout = this%rate_et_z(factor,fktho,hcap)
        end if 
        if ( (this%uzthst(1)-thetaout) > this%thtr+extwc1 ) then
          this%uzthst(1) = this%uzthst(1) - thetaout
          this%uzflst(1) = this%vks*(((this%uzthst(1)-this%thtr)*thtsrinv)**this%eps)
        else if ( this%uzthst(1) > this%thtr+extwc1 ) then
          this%uzthst(1) = this%thtr + extwc1
          this%uzflst(1) = this%vks*(((this%uzthst(1)-this%thtr)*thtsrinv)**this%eps)
        end if
        !
        !all waves shallower than extinction depth
      else if ( this%nwavst > 1 .AND. this%uzdpst(this%nwavst) > this%extdpuz) then
        if ( ietflag == 2 ) then
          tho = this%uzthst(this%nwavst)
          fktho = this%uzflst(this%nwavst)
          hcap = this%caph(tho)
          thetaout = this%rate_et_z(factor,fktho,hcap)
        end if 
        if ( this%uzthst(this%nwavst)-thetaout > this%thtr+extwc1 ) then
          this%uzthst(this%nwavst+1) = this%uzthst(this%nwavst) - thetaout
          numadd = 1
        else if ( this%uzthst(this%nwavst) > this%thtr+extwc1 ) then
          this%uzthst(this%nwavst+1) = this%thtr + extwc1
          numadd = 1
        end if
        if ( numadd == 1 ) then
          this%uzflst(this%nwavst+1) = this%vks*                              &
                              (((this%uzthst(this%nwavst+1)-                  &
                              this%thtr)*thtsrinv)**this%eps)
          theta2 = this%uzthst(this%nwavst+1)
          flux2 = this%uzflst(this%nwavst+1)
          flux1 = this%uzflst(this%nwavst)
          theta1 = this%uzthst(this%nwavst)
          this%uzspst(this%nwavst+1) = this%leadspeed(theta1,theta2,flux1,flux2)  
          this%uzdpst(this%nwavst+1) = this%extdpuz
          this%nwavst = this%nwavst + 1
          if ( this%nwavst > this%nwav ) then
          !
          !too many waves error, deallocate temp arrays and return
            ierr = 1
            goto 500
          end if
        else
          numadd = 0
        end if
      !
      !one wave below extinction depth
      else if ( this%nwavst == 1 ) then
        if ( ietflag == 2 ) then
          tho = this%uzthst(1)
          fktho = this%uzflst(1)
          hcap = this%caph(tho)
          thetaout = this%rate_et_z(factor,fktho,hcap)
        end if
        if ( (this%uzthst(1)-thetaout) > this%thtr+extwc1 ) then
          if ( thetaout > DEM30 ) then
            this%uzthst(2) = this%uzthst(1) - thetaout
            this%uzflst(2) = this%vks*(((this%uzthst(2)-this%thtr)*    &
                               thtsrinv)**this%eps)
            this%uzdpst(2) = this%extdpuz            
            theta2 = this%uzthst(2)
            flux2 = this%uzflst(2)
            flux1 = this%uzflst(1)
            theta1 = this%uzthst(1)
            this%uzspst(2) = this%leadspeed(theta1,theta2,flux1,flux2)               
            this%nwavst = this%nwavst + 1
            if ( this%nwavst > this%nwav ) then
              !
              !too many waves error
              ierr = 1
              goto 500
            end if
          end if
        else if ( this%uzthst(1) > this%thtr+extwc1 ) then
          if ( thetaout > DEM30 ) then
            this%uzthst(2) = this%thtr + extwc1
            this%uzflst(2) = this%vks*(((this%uzthst(2)-                 &
                             this%thtr)*thtsrinv)**this%eps)  
            this%uzdpst(2) = this%extdpuz
            theta2 = this%uzthst(2)
            flux2 = this%uzflst(2)
            flux1 = this%uzflst(1)
            theta1 = this%uzthst(1)
            this%uzspst(2) = this%leadspeed(theta1,theta2,flux1,flux2)                
            this%nwavst = this%nwavst + 1
            if ( this%nwavst > this%nwav ) then
              !too many waves error
              ierr = 1
              goto 500
            end if
          end if
        end if
      else
        !
        !extinction depth splits waves
        if ( this%uzdpst(1)-this%extdpuz > DEM7 ) then
          j = 2
          jk = 0
          !
          !locate extinction depth between waves
          do while ( jk == 0 )
            diff = this%uzdpst(j) - this%extdpuz
            if ( diff > dzero ) then
              j = j + 1
            else
              jk = 1
            end if
          end do
          kk = j
          if ( this%uzthst(j) > this%thtr+extwc1 ) then
            !
            !create a wave at extinction depth
            if ( abs(diff) > DEM5 ) then
              call this%wave_shift(this,-1,this%nwavst+1,j,-1)
              this%uzdpst(j) = this%extdpuz
              this%nwavst = this%nwavst + 1
              if ( this%nwavst > this%nwav ) then
                !
                !too many waves error
                ierr = 1
                goto 500
              end if               
            end if
            kk = j
          else
            jhold = this%nwavst
            i = j + 1
            do while ( i < this%nwavst )
              if ( this%uzthst(i) > this%thtr+extwc1 ) then
                jhold = i
                i = this%nwavst + 1
              end if
              i = i + 1
            end do
            j = jhold
            kk = jhold
          end if
        else
          kk = 1
        end if
        !
        !all waves above extinction depth
        do while ( kk.LE.this%nwavst)
          if ( ietflag==2 ) then
            tho = this%uzthst(kk)
            fktho = this%uzflst(kk)
            hcap = this%caph(tho)
            thetaout = this%rate_et_z(factor,fktho,hcap)
          end if
          if ( this%uzthst(kk) > this%thtr+extwc1 ) then
            if ( this%uzthst(kk)-thetaout > this%thtr+extwc1 ) then
              this%uzthst(kk) = this%uzthst(kk) - thetaout
            else if ( this%uzthst(kk) > this%thtr+extwc1 ) then
              this%uzthst(kk) = this%thtr + extwc1
            end if
            if ( kk.EQ.1 ) then
              this%uzflst(kk) = this%vks*(((this%uzthst(kk)-this%thtr)*thtsrinv)**this%eps)
            end if
            if ( kk > 1 ) then
              flux1 = this%vks*((this%uzthst(kk-1)-this%thtr)*thtsrinv)**this%eps
              flux2 = this%vks*((this%uzthst(kk)-this%thtr)*thtsrinv)**this%eps
              this%uzflst(kk) = flux2
              theta2 = this%uzthst(kk)
              theta1 = this%uzthst(kk-1)
              this%uzspst(kk) = this%leadspeed(theta1,theta2,flux1,flux2)    
            end if
          end if
          kk = kk + 1
        end do
      end if
      !
      !calculate aet
      kj = 1
      do while ( kj.LE.this%nwavst-1 )
        if ( abs(this%uzthst(kj)-this%uzthst(kj+1)) < DEM6 ) then
          call this%wave_shift(this,1,kj+1,this%nwavst-1,1)
          kj = kj - 1
          this%nwavst = this%nwavst - 1
        end if
        kj = kj + 1
      end do
      depth = this%uzdpst(1)
      fm = this%Unsat_stor(depth)
      this%etact = st - fm
      fm = this%Etact/delt
      if ( this%Etact < dzero ) then
        call this%wave_shift(uzfktemp,0,1,Nwv,1)
        this%nwavst = Nwv
        this%Etact = dzero
      elseif ( petsub-fm < -DEM15 .AND. ietflag==2 ) then
        ! aet greater than pet, reset and try again
        call this%wave_shift(uzfktemp,0,1,Nwv,1)
        this%nwavst = Nwv
        this%Etact = dzero
      else
        itest = 1
      end if
      !end aet-pet loop for head dependent et
      fmp = fm
      if ( k > 100 ) then
        itest = 1
      elseif ( ietflag < 2 ) then
        fmp = petsub
        itest = 1
      end if
    end do
500 deallocate(uzfktemp%uzdpst)
    deallocate(uzfktemp%uzthst)
    deallocate(uzfktemp%uzflst)
    deallocate(uzfktemp%uzspst)
    deallocate(uzfktemp%nwavst)
    deallocate(uzfktemp)
    return
      end subroutine uzet
!
! ----------------------------------------------------------------------

      function caph(this,tho)
!     ******************************************************************
!     caph---- calculate capillary pressure head from B-C equation
!     ******************************************************************
!     SPECIFICATIONS:
! ----------------------------------------------------------------------
      !modules
      class (UzfKinematicType) :: this
      real(DP), intent(in) :: tho
      ! -- dummy
      real(DP) :: caph,lambda,star
! ----------------------------------------------------------------------
      caph = -DEM6
      star = (tho-this%thtr)/(this%thts-this%thtr) 
      if ( star < DEM15 ) star = DEM15
      lambda = DTWO/(this%eps-DTHREE)   
      if ( star > DEM15 ) then
        if ( tho-this%thts < DEM15 ) then
          caph = this%ha*star**(-DONE/lambda)
        else
          caph = DZERO
        end if
      end if
      end function caph
      
      function rate_et_z(this,factor,fktho,h)
!     ******************************************************************
!     rate_et_z---- capillary pressure based uz et
!     ******************************************************************
!     SPECIFICATIONS:
! ----------------------------------------------------------------------
      !modules
      !arguments
      class (UzfKinematicType) :: this
      real(DP), intent(in) :: factor,fktho,h
      ! -- dummy
      real(DP) :: rate_et_z
! ----------------------------------------------------------------------
      rate_et_z = factor*fktho*(h-this%hroot)
      if ( rate_et_z < DZERO ) rate_et_z = DZERO
      end function rate_et_z
!
! ------------------------------------------------------------------------------
! end of BndUzfKinematic object
end module UzfKinematicModule