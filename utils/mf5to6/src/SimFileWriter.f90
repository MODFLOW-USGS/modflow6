module SimFileWriterModule

  use ConstantsModule, only: MAXCHARLEN
  use ExchangeModule, only: ExchangeType
  use ExchangeWriterModule, only: ExchangeWriterType
  use InputOutputModule, only: GetUnit
  use ListModule, only: ListType
  use ModelConverterModule, only: ModelConverterType
  use ModelModule, only: ModelType
  use MoverModule, only: MoverType, AddMoverToList
  use MvrPackageWriterModule, only: MvrPackageWriterType
  use SimListVariablesModule, only: SimMovers

  implicit none

  type SimFileWriterType
    type(MvrPackageWriterType), pointer :: MvrWriter => null()
    character(len=MAXCHARLEN) :: BaseName = ''
    type(ListType), pointer :: SimMovers => null()
  contains
    procedure, public :: AddMover
    procedure, public :: InitializeSimWriter
    procedure, public :: WriteMoverFile
    generic, public :: WriteSimFile => WriteSimFileSingle, WriteSimFileMultiple
    procedure, private :: WriteSimFileSingle
    procedure, private :: WriteSimFileMultiple
  end type SimFileWriterType

contains

  subroutine InitializeSimWriter(this)
    ! dummy
    class(SimFileWriterType) :: this
    ! local
    !
    allocate(this%MvrWriter)
    this%MvrWriter%WriteModelNames = .true.
    call this%MvrWriter%InitializeMvrWriter(SimMovers)
    !
    return
  end subroutine InitializeSimWriter

  subroutine WriteSimFileSingle(this, modelConverter)
    ! Write a MODFLOW 6 simulation file for a single model
    implicit none
    ! dummy
    class(SimFileWriterType) :: this
    type(ModelConverterType), intent(inout) :: modelConverter
    ! local
    integer :: iu
    character(len=10) :: SimNameFile = 'mfsim.nam'
    character(len=MAXCHARLEN) :: GwfNameFile, ImsFile, TdisFile
    character(len=MAXCHARLEN) :: ModelName
    type(ModelType), pointer  :: model => null()
    ! formats
    1 format()
    10 format(a)
    20 format(2x,a,2x,a)
    30 format(2x,a,2x,a,2x,a)
    40 format(2x,a,2x,i0)
    !
    model => modelConverter%model
    !
    ! store some strings in local variables
    TdisFile = model%TdisWriter%fileobj%FName
    GwfNameFile = model%NameFile2015
    ImsFile = model%ImsWriter%fileobj%FName
    ModelName = model%BaseName
    !
    ! open simulation name file and write a comment at top
    iu = GetUnit()
    open(iu,file=SimNameFile,status='REPLACE')
    write(iu,10)'# Simulation name file for MODFLOW 6 prepared by MF5to6'
    !
    ! write Options block
    write(iu,1)
    write(iu,10)'BEGIN Options'
    write(iu,10)'END Options'
    !
    ! write Timing block
    write(iu,1)
    write(iu,10)'BEGIN TIMING'
    write(iu,20)'TDIS6',trim(TdisFile)
    write(iu,10)'END TIMING'
    !
    ! write Models block
    write(iu,1)
    write(iu,10)'BEGIN MODELS'
    write(iu,30)'GWF6', trim(GwfNameFile), trim(ModelName)
    write(iu,10)'END MODELS'
    !
    ! write empty Exchanges block
    write(iu,1)
    write(iu,10)'BEGIN EXCHANGES'
    write(iu,10)'END EXCHANGES'
    !
    ! write Solution_Group block
    write(iu,1)
    write(iu,10)'BEGIN SOLUTIONGROUP 1'
    write(iu,40)'MXITER',1
    write(iu,30)'IMS6',trim(ImsFile),trim(ModelName)
    write(iu,10)'END SOLUTIONGROUP'
    !
    ! Close simulation name file
    close(iu)
    !
    return
  end subroutine WriteSimFileSingle

  subroutine WriteSimFileMultiple(this, modelConverters, &
                                  exchangeWriter)
    ! Write a MODFLOW 6 simulation file for a single model
    implicit none
    ! dummy
    class(SimFileWriterType) :: this
    type(ListType), intent(inout) :: modelConverters
    type(ExchangeWriterType), intent(inout) :: exchangeWriter
    ! local
    integer :: i, iu, nexg, ngrids
    character(len=10) :: SimNameFile = 'mfsim.nam'
    character(len=MAXCHARLEN) :: GwfNameFile, ImsFile, TdisFile, ExgFile
    character(len=MAXCHARLEN) :: ModelName, name1, name2, exgtype, line
    type(ModelConverterType), pointer :: childConverter
    type(ModelConverterType), pointer :: parentConverter
    type(ModelType), pointer  :: parent => null()
    type(ModelType), pointer  :: child => null()
    type(ExchangeType), pointer :: exchange => null()
    class(*), pointer :: obj => null()
    ! formats
    1 format()
    10 format(a)
    20 format(2x,a,2x,a)
    30 format(2x,a,2x,a,2x,a)
    40 format(2x,a,2x,i0)
    50 format(2x,a,2x,a,2x,a,2x,a)
    60 format(2x,a)
    !
    exchangeWriter%MvrWriter => this%MvrWriter
    !
    ngrids = modelConverters%Count()
    !
    ! Get parent converter, which is the first one in the list.
    obj => modelConverters%GetItem(1)
    select type(obj)
    type is (ModelConverterType)
      parentConverter => obj
    end select
    !
    parent => parentConverter%model
    !
    ! store some strings in local variables
    TdisFile = parent%TdisWriter%fileobj%FName
    GwfNameFile = parent%NameFile2015
    ImsFile = parent%ImsWriter%fileobj%FName
    ModelName = parent%BaseName
    !
    ! open simulation name file and write a comment at top
    iu = GetUnit()
    open(iu,file=SimNameFile,status='REPLACE')
    write(iu,10)'# Simulation name file for MODFLOW 6 prepared by MF5to6'
    !
    ! write Options block
    write(iu,1)
    write(iu,10)'BEGIN OPTIONS'
    write(iu,10)'END OPTIONS'
    !
    ! write Timing block
    write(iu,1)
    write(iu,10)'BEGIN TIMING'
    write(iu,20)'TDIS6',trim(TdisFile)
    write(iu,10)'END TIMING'
    !
    ! write Models block
    write(iu,1)
    write(iu,10)'BEGIN MODELS'
    write(iu,30)'GWF6', trim(GwfNameFile), trim(ModelName)
    do i=2,ngrids
      obj => modelConverters%GetItem(i)
      select type(obj)
      type is (ModelConverterType)
        childConverter => obj
      end select
      child => childConverter%model
      GwfNameFile = child%NameFile2015
      ModelName = child%BaseName
      write(iu,30)'GWF6', trim(GwfNameFile), trim(ModelName)
    enddo
    write(iu,10)'END MODELS'
    !
    ! write Exchanges block
    write(iu,1)
    write(iu,10)'BEGIN EXCHANGES'
    nexg = exchangeWriter%Exchanges%Count()
    if (nexg > 0) then
      do i=1,nexg
        obj => exchangeWriter%Exchanges%GetItem(i)
        select type(obj)
        type is (ExchangeType)
          exchange => obj
        end select
        exgtype = exchange%ExgType
        ExgFile = exchange%FileWriter%fileobj%FName
        name1 = exchange%Model1%BaseName
        name2 = exchange%Model2%BaseName
        write(iu,50)trim(exgtype), trim(ExgFile), trim(name1), trim(name2)
      enddo
    endif
    write(iu,10)'END EXCHANGES'
    !
    ! write SOLUTIONGROUP block
    write(iu,1)
    write(iu,10)'BEGIN SOLUTIONGROUP 1'
    write(iu,40)'MXITER',1
    line = 'IMS6  ' // trim(ImsFile) // '  ' // trim(parent%ModelName)
    do i=2,ngrids
      obj => modelConverters%GetItem(i)
      select type(obj)
      type is (ModelConverterType)
        childConverter => obj
      end select
      child => childConverter%model
      ModelName = child%BaseName
      line = trim(line) // '  ' // trim(ModelName)
    enddo
    write(iu,60)trim(line)
    write(iu,10)'END SOLUTIONGROUP'
    !
    ! Close simulation name file
    close(iu)
    !
    return
  end subroutine WriteSimFileMultiple

  subroutine AddMover(this, mover)
    implicit none
    ! dummy
    class(SimFileWriterType) :: this
    type(MoverType), pointer :: mover
    !
!    call this%MvrWriter%AddMover(mover)
    call AddMoverToList(this%SimMovers, mover)
    !
    return
  end subroutine AddMover

  subroutine WriteMoverFile(this)
    implicit none
    ! dummy
    class(SimFileWriterType) :: this
    ! local
    character(len=MAXCHARLEN) :: fname
    !
    if (SimMovers%Count() > 0) then
      this%MvrWriter%Active = .true.
      call this%MvrWriter%AllocatePointers()
      fname = trim(this%BaseName) // '.mvr6'
      call this%MvrWriter%InitializeFile(fname, 'MVR6')
      this%MvrWriter%PrintInput = .true.
      this%MvrWriter%PrintFlows = .true.
!      this%MvrWriter%PackageName = 'MVR'
      call this%MvrWriter%WriteMvrFile()
    endif
    !
    return
  end subroutine WriteMoverFile

end module SimFileWriterModule
