# makefile created by pymake (version 1.2.7) for the 'mf6' executable.


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
SOURCEDIR9=../src/Distributed
SOURCEDIR10=../src/Solution
SOURCEDIR11=../src/Solution/PETSc
SOURCEDIR12=../src/Solution/LinearMethods
SOURCEDIR13=../src/Timing
SOURCEDIR14=../src/Utilities
SOURCEDIR15=../src/Utilities/TimeSeries
SOURCEDIR16=../src/Utilities/Libraries
SOURCEDIR17=../src/Utilities/Libraries/rcm
SOURCEDIR18=../src/Utilities/Libraries/sparsekit
SOURCEDIR19=../src/Utilities/Libraries/sparskit2
SOURCEDIR20=../src/Utilities/Libraries/blas
SOURCEDIR21=../src/Utilities/Libraries/daglib
SOURCEDIR22=../src/Utilities/Idm
SOURCEDIR23=../src/Utilities/Matrix
SOURCEDIR24=../src/Utilities/Vector
SOURCEDIR25=../src/Utilities/Observation
SOURCEDIR26=../src/Utilities/OutputControl
SOURCEDIR27=../src/Utilities/Memory
SOURCEDIR28=../src/Utilities/ArrayRead

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
${SOURCEDIR25} \
${SOURCEDIR26} \
${SOURCEDIR27} \
${SOURCEDIR28} 

.SUFFIXES: .f90 .F90 .o

OBJECTS = \
$(OBJDIR)/sparsekit.o \
$(OBJDIR)/ilut.o \
$(OBJDIR)/blas1_d.o \
$(OBJDIR)/kind.o \
$(OBJDIR)/Sparse.o \
$(OBJDIR)/dag_module.o \
$(OBJDIR)/OpenSpec.o \
$(OBJDIR)/GwtAdvOptions.o \
$(OBJDIR)/InputDefinition.o \
$(OBJDIR)/rcm.o \
$(OBJDIR)/VectorBase.o \
$(OBJDIR)/gwf3dis8idm.o \
$(OBJDIR)/CharString.o \
$(OBJDIR)/ims8reordering.o \
$(OBJDIR)/Constants.o \
$(OBJDIR)/BaseGeometry.o \
$(OBJDIR)/compilerversion.o \
$(OBJDIR)/gwf3disu8idm.o \
$(OBJDIR)/GwfBuyInputData.o \
$(OBJDIR)/HashTable.o \
$(OBJDIR)/SimStages.o \
$(OBJDIR)/defmacro.o \
$(OBJDIR)/CsrUtils.o \
$(OBJDIR)/SimVariables.o \
$(OBJDIR)/genericutils.o \
$(OBJDIR)/gwf3npf8idm.o \
$(OBJDIR)/gwt1dspidm.o \
$(OBJDIR)/Xt3dAlgorithm.o \
$(OBJDIR)/Timer.o \
$(OBJDIR)/SfrCrossSectionUtils.o \
$(OBJDIR)/List.o \
$(OBJDIR)/ims8misc.o \
$(OBJDIR)/MatrixBase.o \
$(OBJDIR)/LinearSolverBase.o \
$(OBJDIR)/gwf3disv8idm.o \
$(OBJDIR)/GwtDspOptions.o \
$(OBJDIR)/IdmLogger.o \
$(OBJDIR)/SmoothingFunctions.o \
$(OBJDIR)/VirtualDataLists.o \
$(OBJDIR)/StringList.o \
$(OBJDIR)/version.o \
$(OBJDIR)/GwfStorageUtils.o \
$(OBJDIR)/ArrayHandlers.o \
$(OBJDIR)/Message.o \
$(OBJDIR)/GwfVscInputData.o \
$(OBJDIR)/mf6lists.o \
$(OBJDIR)/IndexMap.o \
$(OBJDIR)/GwfNpfOptions.o \
$(OBJDIR)/ObsOutput.o \
$(OBJDIR)/TimeSeriesRecord.o \
$(OBJDIR)/Sim.o \
$(OBJDIR)/InterfaceMap.o \
$(OBJDIR)/MemoryHelper.o \
$(OBJDIR)/DistributedVariable.o \
$(OBJDIR)/sort.o \
$(OBJDIR)/InputOutput.o \
$(OBJDIR)/CircularGeometry.o \
$(OBJDIR)/ObsOutputList.o \
$(OBJDIR)/Iunit.o \
$(OBJDIR)/RectangularGeometry.o \
$(OBJDIR)/DisvGeom.o \
$(OBJDIR)/ArrayReaders.o \
$(OBJDIR)/BlockParser.o \
$(OBJDIR)/Budget.o \
$(OBJDIR)/BudgetFileReader.o \
$(OBJDIR)/PrintSaveManager.o \
$(OBJDIR)/STLVecInt.o \
$(OBJDIR)/comarg.o \
$(OBJDIR)/HeadFileReader.o \
$(OBJDIR)/InputDefinitionSelector.o \
$(OBJDIR)/TableTerm.o \
$(OBJDIR)/ModflowInput.o \
$(OBJDIR)/ArrayReaderBase.o \
$(OBJDIR)/NameFile.o \
$(OBJDIR)/StructVector.o \
$(OBJDIR)/Double2dReader.o \
$(OBJDIR)/Integer2dReader.o \
$(OBJDIR)/TimeSeries.o \
$(OBJDIR)/TimeSeriesFileList.o \
$(OBJDIR)/ims8base.o \
$(OBJDIR)/TimeSeriesLink.o \
$(OBJDIR)/Table.o \
$(OBJDIR)/Double1dReader.o \
$(OBJDIR)/Integer1dReader.o \
$(OBJDIR)/LayeredArrayReader.o \
$(OBJDIR)/ListReader.o \
$(OBJDIR)/SfrCrossSectionManager.o \
$(OBJDIR)/Memory.o \
$(OBJDIR)/MemoryList.o \
$(OBJDIR)/MemoryManager.o \
$(OBJDIR)/Connections.o \
$(OBJDIR)/ats.o \
$(OBJDIR)/MemoryManagerExt.o \
$(OBJDIR)/MemorySetHandler.o \
$(OBJDIR)/tdis.o \
$(OBJDIR)/ims8linear.o \
$(OBJDIR)/SeqVector.o \
$(OBJDIR)/PackageBudget.o \
$(OBJDIR)/StructArray.o \
$(OBJDIR)/MappedMemory.o \
$(OBJDIR)/BaseModel.o \
$(OBJDIR)/SparseMatrix.o \
$(OBJDIR)/GwfMvrPeriodData.o \
$(OBJDIR)/VirtualBase.o \
$(OBJDIR)/VirtualDataContainer.o \
$(OBJDIR)/BaseExchange.o \
$(OBJDIR)/PackageMover.o \
$(OBJDIR)/TimeSeriesManager.o \
$(OBJDIR)/DiscretizationBase.o \
$(OBJDIR)/UzfCellGroup.o \
$(OBJDIR)/BaseSolution.o \
$(OBJDIR)/OutputControlData.o \
$(OBJDIR)/LoadMf6FileType.o \
$(OBJDIR)/TimeArray.o \
$(OBJDIR)/BudgetTerm.o \
$(OBJDIR)/ImsLinearSolver.o \
$(OBJDIR)/Mover.o \
$(OBJDIR)/LinearSolverFactory.o \
$(OBJDIR)/VirtualSolution.o \
$(OBJDIR)/NumericalPackage.o \
$(OBJDIR)/Xt3dInterface.o \
$(OBJDIR)/Observe.o \
$(OBJDIR)/IdmMf6FileLoader.o \
$(OBJDIR)/SolutionGroup.o \
$(OBJDIR)/OutputControl.o \
$(OBJDIR)/gwf3tvbase8.o \
$(OBJDIR)/gwf3ic8.o \
$(OBJDIR)/BudgetObject.o \
$(OBJDIR)/gwf3dis8.o \
$(OBJDIR)/gwf3tvs8.o \
$(OBJDIR)/TimeArraySeries.o \
$(OBJDIR)/gwf3tvk8.o \
$(OBJDIR)/RouterBase.o \
$(OBJDIR)/TimeArraySeriesLink.o \
$(OBJDIR)/gwf3disu8.o \
$(OBJDIR)/ObsContainer.o \
$(OBJDIR)/gwf3mvr8.o \
$(OBJDIR)/TimeArraySeriesManager.o \
$(OBJDIR)/gwf3disv8.o \
$(OBJDIR)/ObsUtility.o \
$(OBJDIR)/GwtSpc.o \
$(OBJDIR)/gwt1ic1.o \
$(OBJDIR)/gwf3oc8.o \
$(OBJDIR)/gwt1oc1.o \
$(OBJDIR)/gwf3sto8.o \
$(OBJDIR)/SerialRouter.o \
$(OBJDIR)/RouterFactory.o \
$(OBJDIR)/Obs3.o \
$(OBJDIR)/gwf3obs8.o \
$(OBJDIR)/BoundaryPackage.o \
$(OBJDIR)/gwf3csub8.o \
$(OBJDIR)/NumericalModel.o \
$(OBJDIR)/gwt1cnc1.o \
$(OBJDIR)/gwf3api8.o \
$(OBJDIR)/GhostNode.o \
$(OBJDIR)/gwf3rch8.o \
$(OBJDIR)/gwf3chd8.o \
$(OBJDIR)/gwf3riv8.o \
$(OBJDIR)/gwt1fmi1.o \
$(OBJDIR)/gwf3wel8.o \
$(OBJDIR)/NumericalExchange.o \
$(OBJDIR)/gwt1ssm1.o \
$(OBJDIR)/gwt1src1.o \
$(OBJDIR)/NumericalSolution.o \
$(OBJDIR)/gwf3sfr8.o \
$(OBJDIR)/SolutionFactory.o \
$(OBJDIR)/gwf3drn8.o \
$(OBJDIR)/gwt1obs1.o \
$(OBJDIR)/gwf3uzf8.o \
$(OBJDIR)/gwf3evt8.o \
$(OBJDIR)/gwf3ghb8.o \
$(OBJDIR)/gwt1mst1.o \
$(OBJDIR)/gwt1adv1.o \
$(OBJDIR)/gwt1apt1.o \
$(OBJDIR)/gwf3lak8.o \
$(OBJDIR)/gwt1mvt1.o \
$(OBJDIR)/gwf3maw8.o \
$(OBJDIR)/VirtualModel.o \
$(OBJDIR)/TransportModel.o \
$(OBJDIR)/DisConnExchange.o \
$(OBJDIR)/gwf3vsc8.o \
$(OBJDIR)/VirtualGwtModel.o \
$(OBJDIR)/gwt1ist1.o \
$(OBJDIR)/CellWithNbrs.o \
$(OBJDIR)/gwt1sft1.o \
$(OBJDIR)/gwt1uzt1.o \
$(OBJDIR)/GridSorting.o \
$(OBJDIR)/VirtualExchange.o \
$(OBJDIR)/gwf3hfb8.o \
$(OBJDIR)/VirtualGwfExchange.o \
$(OBJDIR)/gwt1mwt1.o \
$(OBJDIR)/VirtualGwfModel.o \
$(OBJDIR)/gwt1lkt1.o \
$(OBJDIR)/VirtualGwtExchange.o \
$(OBJDIR)/gwf3npf8.o \
$(OBJDIR)/gwt1dsp.o \
$(OBJDIR)/gwf3buy8.o \
$(OBJDIR)/GridConnection.o \
$(OBJDIR)/SpatialModelConnection.o \
$(OBJDIR)/gwt1.o \
$(OBJDIR)/gwf3.o \
$(OBJDIR)/GwfGwfExchange.o \
$(OBJDIR)/Mapper.o \
$(OBJDIR)/GwtGwtExchange.o \
$(OBJDIR)/VirtualDataManager.o \
$(OBJDIR)/GwfInterfaceModel.o \
$(OBJDIR)/GwtInterfaceModel.o \
$(OBJDIR)/RunControl.o \
$(OBJDIR)/RunControlFactory.o \
$(OBJDIR)/GwfGwfConnection.o \
$(OBJDIR)/GwtGwtConnection.o \
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

