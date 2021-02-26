module LakeModule
  
  use ConstantsModule, only: DHALF, DZERO, LINELENGTH, MAXCHARLEN
  use GLOBAL, only: NCOL, NROW, NLAY, IBOUND, DELC, DELR
  use GWFBASMODULE, only: SGWF2BAS7PNT
  use GWFLAKMODULE, only: LKARR1, BDLKN1, DEPTHTABLE, VOLUMETABLE, &
                          AREATABLE, SGWF2LAK7PNT
  use InputOutputModule, only: GetUnit, openfile
  use LakeConnectionModule, only: LakeConnectionType, &
                                  ConstructLakeConnection, &
                                  AddLakeConnectionToList, &
                                  GetConnectionFromList
  use LakeOutletModule, only: LakeOutletType
  use ListModule, only: ListType
  
  implicit none
  
  private
  public :: LakeType, ConstructLakeType, CastAsLakeType, &
            AddLakeToList, GetLakeFromList
  
  type LakeType
    integer          :: LakeNum = 0
    integer          :: Igrid = 0
    double precision :: Strt = DZERO
    character(len=LINELENGTH) :: TableFile = ''
    character(len=MAXCHARLEN) :: ModelBasename = ''
    type(ListType), pointer :: Connections => null()
  contains
    procedure, public :: AddConnection
    procedure, public :: GetConnection
    procedure, public :: DefineConnections
    procedure, public :: WriteBathFile
  end type LakeType
  
contains

  ! Type-bound procedures

  subroutine AddConnection(this, connection)
    implicit none
    ! dummy
    class(LakeType) :: this
    type(LakeConnectionType), pointer :: connection
    !
    call AddLakeConnectionToList(this%Connections, connection)
    !
    return
  end subroutine AddConnection
  
  function GetConnection(this, idx) result (conn)
    ! dummy
    class(LakeType), intent(inout) :: this
    integer, intent(in) :: idx
    type(LakeConnectionType), pointer :: conn
    !
    conn => GetConnectionFromList(this%Connections, idx)
    !
    return
  end function GetConnection

  subroutine DefineConnections(this)
    implicit none
    ! dummy
    class(LakeType) :: this
    ! local
    integer :: i, ip1, im1, j, jp1, jm1, k, kcon, km1, lakenum
    type(LakeConnectionType), pointer :: newConn => null()
    !
    call SGWF2BAS7PNT(this%Igrid)
    lakenum = this%LakeNum
    kcon = 0
    ! Iterate through cells of LKARR1 array and define horizontal connections
    do k=1,NLAY
      if (k > 1) then
        km1 = k - 1
      else
        km1 = 0
      endif
      do i=1,NROW
        if (i > 1) then
          im1 = i - 1
        else
          im1 = 0
        endif
        if (i < NROW) then
          ip1 = i + 1
        else
          ip1 = 0
        endif
        do j=1,NCOL
          if (j > 1) then
            jm1 = j - 1
          else
            jm1 = 0
          endif
          if (j < NCOL) then
            jp1 = j + 1
          else
            jp1 = 0
          endif
          if (IBOUND(j,i,k) /= 0) then
            ! Check surrounding cells (same layer) for lake of interest
            if (jm1 /= 0) then
              if (LKARR1(jm1,i,k) == lakenum) then
                ! Create a horizontal connection (row direction) and
                ! add it to list of connections.
                kcon = kcon + 1
                call ConstructLakeConnection(newConn, lakenum, kcon, i, j, &
                                             k, 'HORIZONTAL')
                newConn%BedLeak = BDLKN1(jm1,i,k)
                newConn%ConnLen = DHALF * DELR(j)
                newConn%ConnWidth =DELC(i)
                call this%AddConnection(newConn)
              endif
            endif
            if (jp1 /= 0) then
              if (LKARR1(jp1,i,k) == lakenum) then
                ! Create a horizontal connection (row direction) and
                ! add it to list of connections.
                kcon = kcon + 1
                call ConstructLakeConnection(newConn, lakenum, kcon, i, j, &
                                             k, 'HORIZONTAL')
                newConn%BedLeak = BDLKN1(jp1,i,k)
                newConn%ConnLen = DHALF * DELR(j)
                newConn%ConnWidth =DELC(i)
                call this%AddConnection(newConn)
              endif
            endif
            if (im1 /= 0) then
              if (LKARR1(j,im1,k) == lakenum) then
                ! Create a horizontal connection (column direction) and
                ! add it to list of connections.
                kcon = kcon + 1
                call ConstructLakeConnection(newConn, lakenum, kcon, i, j, &
                                             k, 'HORIZONTAL')
                newConn%BedLeak = BDLKN1(j,im1,k)
                newConn%ConnLen = DHALF * DELC(i)
                newConn%ConnWidth  = DELR(j)
                call this%AddConnection(newConn)
              endif
            endif
            if (ip1 /= 0) then
              if (LKARR1(j,ip1,k) == lakenum) then
                ! Create a horizontal connection (column direction) and 
                ! add it to list of connections.
                kcon = kcon + 1
                call ConstructLakeConnection(newConn, lakenum, kcon, i, j, &
                                             k, 'HORIZONTAL')
                newConn%BedLeak = BDLKN1(j,ip1,k)
                newConn%ConnLen = DHALF * DELC(i)
                newConn%ConnWidth  = DELR(j)
                call this%AddConnection(newConn)
              endif
            endif
            if (km1 /= 0) then
              ! Check overlying cell for lake of interest
              if (LKARR1(j,i,km1) == lakenum) then
                ! Create a vertical connection and add it
                ! to list of connections.
                kcon = kcon + 1
                call ConstructLakeConnection(newConn, lakenum, kcon, i, j, &
                                             k, 'VERTICAL')
                newConn%BedLeak = BDLKN1(j,i,km1)
                ! ConnLen and ConnWid not needed for vertical connection
                call this%AddConnection(newConn)
              endif
            endif
          endif
        enddo
      enddo
    enddo
    !
    return
  end subroutine DefineConnections

  subroutine WriteBathFile(this)
    ! dummy
    class(LakeType) :: this
    ! local
    integer :: i, iu, lakenum, ndim
    ! format
    1 format()
    10 format(a,a,i0,a)
    20 format(2x,g15.8,2x,g15.8,2x,g15.8)
    30 format(a)
    40 format(2x,a,2x,i0)
    !
    call SGWF2BAS7PNT(this%Igrid)
    ndim = size(VOLUMETABLE, 1)
    lakenum = this%LakeNum
    write(this%TableFile,10)trim(this%ModelBasename),'_lake_', &
                            lakenum,'_bathymetry.txt'
    !
    ! Open and write bathymetry table
    iu = GetUnit()
    call openfile(iu, 0, this%TableFile, 'BATHYMETRY', filstat_opt='REPLACE')
    !
    write(iu,30)'BEGIN DIMENSIONS'
    write(iu,40)'NROW',ndim
    write(iu,40)'NCOL',3
    write(iu,30)'END DIMENSIONS'
    write(iu,1)
    !
    write(iu,30)'BEGIN TABLE'
    write(iu,30)'#     stage            volume           sarea'
    do i=1,ndim
      write(iu,20)DEPTHTABLE(i,lakenum), VOLUMETABLE(i,lakenum), AREATABLE(i,lakenum)
    enddo
    write(iu,30)'END TABLE'
    !
    close(iu)
    !
    return
  end subroutine WriteBathFile

  ! Non-type-bound procedures
  
  subroutine ConstructLakeType(newLake, Igrid)
    ! dummy
    type(LakeType), pointer, intent(inout) :: newLake
    integer, intent(in) :: Igrid
    !
    allocate(newLake)
    allocate(newLake%Connections)
    newLake%Igrid = Igrid
    !
    return
  end subroutine ConstructLakeType

  function CastAsLakeType(obj) result (res)
    implicit none
    class(*), pointer, intent(inout) :: obj
    type(LakeType), pointer :: res
    !
    res => null()
    if (.not. associated(obj)) return
    !
    select type (obj)
    type is (LakeType)
      res => obj
    end select
    return
  end function CastAsLakeType

  subroutine AddLakeToList(list, lake)
    implicit none
    ! dummy
    type(ListType), pointer :: list
    type(LakeType), pointer :: lake
    ! local
    class(*), pointer :: obj => null()
    !
    obj => lake
    call list%Add(obj)
    !
    return
  end subroutine AddLakeToList

  function GetLakeFromList(list, idx) result (res)
    implicit none
    ! dummy
    type(ListType), pointer :: list
    integer, intent(in) :: idx
    type(LakeType), pointer :: res
    ! local
    class(*), pointer :: obj => null()
    !
    obj => list%GetItem(idx)
    res => CastAsLakeType(obj)
    !
    return
  end function GetLakeFromList

end module LakeModule
