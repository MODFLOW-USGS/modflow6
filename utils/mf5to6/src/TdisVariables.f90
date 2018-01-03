module TdisVariablesModule
  
  use TdisWriterModule, only: TdisWriterType
  
  type(TdisWriterType), pointer :: GlobalTdisWriter => null()
  
end module TdisVariablesModule
