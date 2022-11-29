# makefile created by pymake (version 1.2.5) for the 'mf6' executable.


include ./makedefaults

# Define the source file directories
SOURCEDIR1=../src
SOURCEDIR2=../src/Exchange
SOURCEDIR3=../src/Model
SOURCEDIR4=../src/Model/Geometry
SOURCEDIR5=../src/Model/ModelUtilities
SOURCEDIR6=../src/Model/Connection
SOURCEDIR7=../src/Model/GroundWaterTransport
SOURCEDIR8=../src/Model/GroundWaterFlow
SOURCEDIR9=../src/Solution
SOURCEDIR10=../src/Solution/LinearMethods
SOURCEDIR11=../src/Timing
SOURCEDIR12=../src/Utilities
SOURCEDIR13=../src/Utilities/TimeSeries
SOURCEDIR14=../src/Utilities/Libraries
SOURCEDIR15=../src/Utilities/Libraries/rcm
SOURCEDIR16=../src/Utilities/Libraries/sparsekit
SOURCEDIR17=../src/Utilities/Libraries/sparskit2
SOURCEDIR18=../src/Utilities/Libraries/blas
SOURCEDIR19=../src/Utilities/Libraries/daglib
SOURCEDIR20=../src/Utilities/Idm
SOURCEDIR21=../src/Utilities/Matrix
SOURCEDIR22=../src/Utilities/Observation
SOURCEDIR23=../src/Utilities/OutputControl
SOURCEDIR24=../src/Utilities/Memory
SOURCEDIR25=../src/Utilities/ArrayRead

VPATH = \
${SOURCEDIR1} \
${SOURCEDIR2} \
${SOURCEDIR3} \
${SOURCEDIR4} \
${SOURCEDIR5} \
${SOURCEDIR6} \
${SOURCEDIR7} \
${SOURCEDIR8} \
${SOURCEDIR9} \
${SOURCEDIR10} \
${SOURCEDIR11} \
${SOURCEDIR12} \
${SOURCEDIR13} \
${SOURCEDIR14} \
${SOURCEDIR15} \
${SOURCEDIR16} \
${SOURCEDIR17} \
${SOURCEDIR18} \
${SOURCEDIR19} \
${SOURCEDIR20} \
${SOURCEDIR21} \
${SOURCEDIR22} \
${SOURCEDIR23} \
${SOURCEDIR24} \
${SOURCEDIR25} 

.SUFFIXES: .f90 .F90 .o

OBJECTS = \
$(OBJDIR)/rcm.o \
$(OBJDIR)/CharString.o \
$(OBJDIR)/OpenSpec.o \
$(OBJDIR)/kind.o \
$(OBJDIR)/InputDefinition.o \
$(OBJDIR)/sparsekit.o \
$(OBJDIR)/ilut.o \
$(OBJDIR)/CsrUtils.o \
$(OBJDIR)/gwf3dis8idm.o \
$(OBJDIR)/BaseGeometry.o \
$(OBJDIR)/Constants.o \
$(OBJDIR)/SimVariables.o \
$(OBJDIR)/SfrCrossSectionUtils.o \
$(OBJDIR)/ims8misc.o \
$(OBJDIR)/GwtAdvOptions.o \
$(OBJDIR)/HashTable.o \
$(OBJDIR)/SmoothingFunctions.o \
$(OBJDIR)/ims8reordering.o \
$(OBJDIR)/gwf3npf8idm.o \
$(OBJDIR)/Sparse.o \
$(OBJDIR)/blas1_d.o \
$(OBJDIR)/gwt1dspidm.o \
$(OBJDIR)/IdmLogger.o \
$(OBJDIR)/dag_module.o \
$(OBJDIR)/GwfNpfOptions.o \
$(OBJDIR)/gwf3disv8idm.o \
$(OBJDIR)/GwtDspOptions.o \
$(OBJDIR)/gwf3disu8idm.o \
$(OBJDIR)/InterfaceMap.o \
$(OBJDIR)/Xt3dAlgorithm.o \
$(OBJDIR)/GwfBuyInputData.o \
$(OBJDIR)/compilerversion.o \
$(OBJDIR)/GwfStorageUtils.o \
$(OBJDIR)/defmacro.o \
$(OBJDIR)/genericutils.o \
$(OBJDIR)/MatrixBase.o \
$(OBJDIR)/version.o \
$(OBJDIR)/Timer.o \
$(OBJDIR)/ArrayHandlers.o \
$(OBJDIR)/List.o \
$(OBJDIR)/mf6lists.o \
$(OBJDIR)/StringList.o \
$(OBJDIR)/ObsOutput.o \
$(OBJDIR)/TimeSeriesRecord.o \
$(OBJDIR)/Message.o \
$(OBJDIR)/Sim.o \
$(OBJDIR)/sort.o \
$(OBJDIR)/VectorInt.o \
$(OBJDIR)/InputOutput.o \
$(OBJDIR)/InputDefinitionSelector.o \
$(OBJDIR)/comarg.o \
$(OBJDIR)/Iunit.o \
$(OBJDIR)/BudgetFileReader.o \
$(OBJDIR)/MemoryHelper.o \
$(OBJDIR)/ObsOutputList.o \
$(OBJDIR)/HeadFileReader.o \
$(OBJDIR)/DisvGeom.o \
$(OBJDIR)/PrintSaveManager.o \
$(OBJDIR)/ModflowInput.o \
$(OBJDIR)/ArrayReaders.o \
$(OBJDIR)/CircularGeometry.o \
$(OBJDIR)/StructVector.o \
$(OBJDIR)/RectangularGeometry.o \
$(OBJDIR)/BlockParser.o \
$(OBJDIR)/Budget.o \
$(OBJDIR)/NameFile.o \
$(OBJDIR)/ArrayReaderBase.o \
$(OBJDIR)/ims8base.o \
$(OBJDIR)/Double1dReader.o \
$(OBJDIR)/TimeSeries.o \
$(OBJDIR)/TableTerm.o \
$(OBJDIR)/Double2dReader.o \
$(OBJDIR)/TimeSeriesFileList.o \
$(OBJDIR)/TimeSeriesLink.o \
$(OBJDIR)/Integer2dReader.o \
$(OBJDIR)/Table.o \
$(OBJDIR)/ListReader.o \
$(OBJDIR)/SfrCrossSectionManager.o \
$(OBJDIR)/Integer1dReader.o \
$(OBJDIR)/Memory.o \
$(OBJDIR)/LayeredArrayReader.o \
$(OBJDIR)/MemoryList.o \
$(OBJDIR)/MemoryManager.o \
$(OBJDIR)/GwfMvrPeriodData.o \
$(OBJDIR)/SparseMatrix.o \
$(OBJDIR)/ims8linear.o \
$(OBJDIR)/ats.o \
$(OBJDIR)/MemoryManagerExt.o \
$(OBJDIR)/MappedVariable.o \
$(OBJDIR)/MemorySetHandler.o \
$(OBJDIR)/tdis.o \
$(OBJDIR)/UzfCellGroup.o \
$(OBJDIR)/BaseModel.o \
$(OBJDIR)/TimeSeriesManager.o \
$(OBJDIR)/DistributedData.o \
$(OBJDIR)/PackageBudget.o \
$(OBJDIR)/PackageMover.o \
$(OBJDIR)/StructArray.o \
$(OBJDIR)/Connections.o \
$(OBJDIR)/BaseExchange.o \
$(OBJDIR)/LoadMf6FileType.o \
$(OBJDIR)/DiscretizationBase.o \
$(OBJDIR)/Mover.o \
$(OBJDIR)/BaseSolution.o \
$(OBJDIR)/Xt3dInterface.o \
$(OBJDIR)/Observe.o \
$(OBJDIR)/InputDefinition.o \
$(OBJDIR)/ArrayReaderBase.o \
$(OBJDIR)/TimeArraySeriesLink.o \
$(OBJDIR)/ObsUtility.o \
$(OBJDIR)/ObsContainer.o \
$(OBJDIR)/VectorInt.o \
$(OBJDIR)/gwt1dspidm.o \
$(OBJDIR)/gwf3npf8idm.o \
$(OBJDIR)/gwf3disv8idm.o \
$(OBJDIR)/gwf3disu8idm.o \
$(OBJDIR)/gwf3dis8idm.o \
$(OBJDIR)/Integer2dReader.o \
$(OBJDIR)/BudgetFileReader.o \
$(OBJDIR)/TimeArraySeriesManager.o \
$(OBJDIR)/PackageMover.o \
$(OBJDIR)/Obs3.o \
$(OBJDIR)/NumericalPackage.o \
$(OBJDIR)/Budget.o \
$(OBJDIR)/StructVector.o \
$(OBJDIR)/IdmLogger.o \
$(OBJDIR)/InputDefinitionSelector.o \
$(OBJDIR)/Integer1dReader.o \
$(OBJDIR)/Double2dReader.o \
$(OBJDIR)/Double1dReader.o \
$(OBJDIR)/sort.o \
$(OBJDIR)/SfrCrossSectionUtils.o \
$(OBJDIR)/BudgetTerm.o \
$(OBJDIR)/BoundaryPackage.o \
$(OBJDIR)/BaseModel.o \
$(OBJDIR)/StructArray.o \
$(OBJDIR)/ModflowInput.o \
$(OBJDIR)/LayeredArrayReader.o \
$(OBJDIR)/SfrCrossSectionManager.o \
$(OBJDIR)/dag_module.o \
$(OBJDIR)/BudgetObject.o \
$(OBJDIR)/NumericalModel.o \
$(OBJDIR)/mf6lists.o \
$(OBJDIR)/PackageBudget.o \
$(OBJDIR)/HeadFileReader.o \
$(OBJDIR)/PrintSaveManager.o \
$(OBJDIR)/Xt3dAlgorithm.o \
$(OBJDIR)/gwf3tvbase8.o \
$(OBJDIR)/LoadMf6FileType.o \
$(OBJDIR)/gwf3sfr8.o \
$(OBJDIR)/gwf3riv8.o \
$(OBJDIR)/gwf3maw8.o \
$(OBJDIR)/gwf3lak8.o \
$(OBJDIR)/GwfVscInputData.o \
$(OBJDIR)/gwf3ghb8.o \
$(OBJDIR)/gwf3drn8.o \
$(OBJDIR)/DistributedModel.o \
$(OBJDIR)/BaseExchange.o \
$(OBJDIR)/UzfCellGroup.o \
$(OBJDIR)/gwt1fmi1.o \
$(OBJDIR)/OutputControlData.o \
$(OBJDIR)/TimeArraySeries.o \
$(OBJDIR)/TimeArraySeriesLink.o \
$(OBJDIR)/BudgetTerm.o \
$(OBJDIR)/TimeArraySeriesManager.o \
$(OBJDIR)/gwf3ic8.o \
$(OBJDIR)/Xt3dInterface.o \
$(OBJDIR)/gwf3tvk8.o \
$(OBJDIR)/MemoryManagerExt.o \
$(OBJDIR)/IdmMf6FileLoader.o \
$(OBJDIR)/gwf3vsc8.o \
$(OBJDIR)/GwfNpfOptions.o \
$(OBJDIR)/CellWithNbrs.o \
$(OBJDIR)/NumericalExchange.o \
$(OBJDIR)/Iunit.o \
$(OBJDIR)/gwf3uzf8.o \
$(OBJDIR)/gwt1apt1.o \
$(OBJDIR)/GwtSpc.o \
$(OBJDIR)/OutputControl.o \
$(OBJDIR)/gwt1ic1.o \
$(OBJDIR)/gwt1mst1.o \
$(OBJDIR)/GwtDspOptions.o \
$(OBJDIR)/gwf3npf8.o \
$(OBJDIR)/GwtAdvOptions.o \
$(OBJDIR)/gwf3tvs8.o \
$(OBJDIR)/GwfStorageUtils.o \
$(OBJDIR)/Mover.o \
$(OBJDIR)/GwfMvrPeriodData.o \
$(OBJDIR)/ims8misc.o \
$(OBJDIR)/GwfBuyInputData.o \
$(OBJDIR)/InterfaceMap.o \
$(OBJDIR)/gwf3disu8.o \
$(OBJDIR)/GridSorting.o \
$(OBJDIR)/DisConnExchange.o \
$(OBJDIR)/CsrUtils.o \
$(OBJDIR)/MappedVariable.o \
$(OBJDIR)/TransportModel.o \
$(OBJDIR)/NameFile.o \
$(OBJDIR)/gwt1uzt1.o \
$(OBJDIR)/gwt1ssm1.o \
$(OBJDIR)/gwt1src1.o \
$(OBJDIR)/gwt1sft1.o \
$(OBJDIR)/gwt1oc1.o \
$(OBJDIR)/gwt1obs1.o \
$(OBJDIR)/gwt1mwt1.o \
$(OBJDIR)/gwt1mvt1.o \
$(OBJDIR)/gwt1lkt1.o \
$(OBJDIR)/gwt1ist1.o \
$(OBJDIR)/gwt1dsp.o \
$(OBJDIR)/gwt1cnc1.o \
$(OBJDIR)/gwt1adv1.o \
$(OBJDIR)/gwf3disv8.o \
$(OBJDIR)/gwf3dis8.o \
$(OBJDIR)/gwf3api8.o \
$(OBJDIR)/gwf3wel8.o \
$(OBJDIR)/gwf3rch8.o \
$(OBJDIR)/gwf3sto8.o \
$(OBJDIR)/OutputControl.o \
$(OBJDIR)/GwtSpc.o \
$(OBJDIR)/BudgetObject.o \
$(OBJDIR)/Obs3.o \
$(OBJDIR)/gwf3oc8.o \
$(OBJDIR)/gwt1oc1.o \
$(OBJDIR)/gwf3obs8.o \
$(OBJDIR)/gwf3mvr8.o \
$(OBJDIR)/gwf3npf8.o \
$(OBJDIR)/gwf3csub8.o \
$(OBJDIR)/gwf3buy8.o \
$(OBJDIR)/GhostNode.o \
$(OBJDIR)/gwf3evt8.o \
$(OBJDIR)/gwf3chd8.o \
$(OBJDIR)/gwf3maw8.o \
$(OBJDIR)/NumericalModel.o \
$(OBJDIR)/gwt1dsp.o \
$(OBJDIR)/gwf3drn8.o \
$(OBJDIR)/gwf3riv8.o \
$(OBJDIR)/gwt1mvt1.o \
$(OBJDIR)/gwf3evt8.o \
$(OBJDIR)/gwf3rch8.o \
$(OBJDIR)/TransportModel.o \
$(OBJDIR)/gwt1apt1.o \
$(OBJDIR)/gwt1ssm1.o \
$(OBJDIR)/gwt1mst1.o \
$(OBJDIR)/gwt1uzt1.o \
$(OBJDIR)/gwt1adv1.o \
$(OBJDIR)/DistributedModel.o \
$(OBJDIR)/gwt1ist1.o \
$(OBJDIR)/gwt1lkt1.o \
$(OBJDIR)/NumericalExchange.o \
$(OBJDIR)/gwt1sft1.o \
$(OBJDIR)/DisConnExchange.o \
$(OBJDIR)/GhostNode.o \
$(OBJDIR)/gwf3buy8.o \
$(OBJDIR)/NumericalSolution.o \
$(OBJDIR)/gwt1mwt1.o \
$(OBJDIR)/CellWithNbrs.o \
$(OBJDIR)/GridSorting.o \
$(OBJDIR)/gwt1.o \
$(OBJDIR)/gwf3.o \
$(OBJDIR)/GwtGwtExchange.o \
$(OBJDIR)/GwfGwfExchange.o \
$(OBJDIR)/GridConnection.o \
$(OBJDIR)/GwtInterfaceModel.o \
$(OBJDIR)/SpatialModelConnection.o \
$(OBJDIR)/GwfInterfaceModel.o \
$(OBJDIR)/GwtGwtConnection.o \
$(OBJDIR)/GwfGwfConnection.o \
$(OBJDIR)/GwfGwtExchange.o \
$(OBJDIR)/ConnectionBuilder.o \
$(OBJDIR)/SimulationCreate.o \
$(OBJDIR)/mf6core.o \
$(OBJDIR)/mf6.o

# Define the objects that make up the program
$(PROGRAM) : $(OBJECTS)
	-$(FC) $(OPTLEVEL) -o $@ $(OBJECTS) $(LDFLAGS)

$(OBJDIR)/%.o : %.f90
	@mkdir -p $(@D)
	$(FC) $(OPTLEVEL) $(FFLAGS) -c $< -o $@ $(INCSWITCH) $(MODSWITCH)

$(OBJDIR)/%.o : %.F90
	@mkdir -p $(@D)
	$(FC) $(OPTLEVEL) $(FFLAGS) -c $< -o $@ $(INCSWITCH) $(MODSWITCH)

