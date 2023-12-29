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
SOURCEDIR22=../src/Idm
SOURCEDIR23=../src/Idm/selector
SOURCEDIR24=../src/Idm/mf6blockfile
SOURCEDIR25=../src/Utilities/Matrix
SOURCEDIR26=../src/Utilities/Vector
SOURCEDIR27=../src/Utilities/Observation
SOURCEDIR28=../src/Utilities/OutputControl
SOURCEDIR29=../src/Utilities/Memory
SOURCEDIR30=../src/Utilities/ArrayRead

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
${SOURCEDIR28} \
${SOURCEDIR29} \
${SOURCEDIR30} 

.SUFFIXES: .f90 .F90 .o

OBJECTS = \
$(OBJDIR)/ilut.o \
$(OBJDIR)/kind.o \
$(OBJDIR)/VectorBase.o \
$(OBJDIR)/IdmLogger.o \
$(OBJDIR)/BaseGeometry.o \
$(OBJDIR)/InputDefinition.o \
$(OBJDIR)/SimStages.o \
$(OBJDIR)/GwtDspOptions.o \
$(OBJDIR)/gwf3npf8idm.o \
$(OBJDIR)/Sparse.o \
$(OBJDIR)/GwtAdvOptions.o \
$(OBJDIR)/gwt1disv1idm.o \
$(OBJDIR)/gwt1dis1idm.o \
$(OBJDIR)/gwf3dis8idm.o \
$(OBJDIR)/simnamidm.o \
$(OBJDIR)/gwf3disu8idm.o \
$(OBJDIR)/CsrUtils.o \
$(OBJDIR)/gwt1idm.o \
$(OBJDIR)/gwf3disv8idm.o \
$(OBJDIR)/blas1_d.o \
$(OBJDIR)/gwt1dsp1idm.o \
$(OBJDIR)/CharString.o \
$(OBJDIR)/OpenSpec.o \
$(OBJDIR)/dag_module.o \
$(OBJDIR)/ims8reordering.o \
$(OBJDIR)/Constants.o \
$(OBJDIR)/rcm.o \
$(OBJDIR)/HashTable.o \
$(OBJDIR)/sparsekit.o \
$(OBJDIR)/gwt1disu1idm.o \
$(OBJDIR)/gwf3idm.o \
$(OBJDIR)/SfrCrossSectionUtils.o \
$(OBJDIR)/MatrixBase.o \
$(OBJDIR)/compilerversion.o \
$(OBJDIR)/defmacro.o \
$(OBJDIR)/SimVariables.o \
$(OBJDIR)/SmoothingFunctions.o \
$(OBJDIR)/GwfVscInputData.o \
$(OBJDIR)/GwfStorageUtils.o \
$(OBJDIR)/Xt3dAlgorithm.o \
$(OBJDIR)/GwfNpfOptions.o \
$(OBJDIR)/GwfBuyInputData.o \
$(OBJDIR)/ims8misc.o \
$(OBJDIR)/LinearSolverBase.o \
$(OBJDIR)/ArrayHandlers.o \
$(OBJDIR)/IndexMap.o \
$(OBJDIR)/version.o \
$(OBJDIR)/InterfaceMap.o \
$(OBJDIR)/Message.o \
$(OBJDIR)/List.o \
$(OBJDIR)/Sim.o \
$(OBJDIR)/Timer.o \
$(OBJDIR)/MemoryHelper.o \
$(OBJDIR)/sort.o \
$(OBJDIR)/StringList.o \
$(OBJDIR)/mf6lists.o \
$(OBJDIR)/IdmGwfDfnSelector.o \
$(OBJDIR)/IdmGwtDfnSelector.o \
$(OBJDIR)/ObsOutput.o \
$(OBJDIR)/Budget.o \
$(OBJDIR)/IdmSimDfnSelector.o \
$(OBJDIR)/DistributedVariable.o \
$(OBJDIR)/InputOutput.o \
$(OBJDIR)/VirtualDataLists.o \
$(OBJDIR)/DisvGeom.o \
$(OBJDIR)/Iunit.o \
$(OBJDIR)/TimeSeriesRecord.o \
$(OBJDIR)/TableTerm.o \
$(OBJDIR)/HeadFileReader.o \
$(OBJDIR)/PrintSaveManager.o \
$(OBJDIR)/IdmDfnSelector.o \
$(OBJDIR)/DefinitionSelect.o \
$(OBJDIR)/ArrayReaders.o \
$(OBJDIR)/comarg.o \
$(OBJDIR)/STLVecInt.o \
$(OBJDIR)/BlockParser.o \
$(OBJDIR)/CircularGeometry.o \
$(OBJDIR)/BudgetFileReader.o \
$(OBJDIR)/RectangularGeometry.o \
$(OBJDIR)/ObsOutputList.o \
$(OBJDIR)/ims8base.o \
$(OBJDIR)/TimeSeries.o \
$(OBJDIR)/TimeSeriesFileList.o \
$(OBJDIR)/ArrayReaderBase.o \
$(OBJDIR)/ModflowInput.o \
$(OBJDIR)/TimeSeriesLink.o \
$(OBJDIR)/Double1dReader.o \
$(OBJDIR)/Double2dReader.o \
$(OBJDIR)/Table.o \
$(OBJDIR)/Integer2dReader.o \
$(OBJDIR)/StructVector.o \
$(OBJDIR)/SfrCrossSectionManager.o \
$(OBJDIR)/ListReader.o \
$(OBJDIR)/Memory.o \
$(OBJDIR)/MemoryList.o \
$(OBJDIR)/MemoryManager.o \
$(OBJDIR)/ats.o \
$(OBJDIR)/Integer1dReader.o \
$(OBJDIR)/StructArray.o \
$(OBJDIR)/MemorySetHandler.o \
$(OBJDIR)/MappedMemory.o \
$(OBJDIR)/ModelPackageInputs.o \
$(OBJDIR)/BaseModel.o \
$(OBJDIR)/GwfMvrPeriodData.o \
$(OBJDIR)/ims8linear.o \
$(OBJDIR)/MemoryManagerExt.o \
$(OBJDIR)/Connections.o \
$(OBJDIR)/SeqVector.o \
$(OBJDIR)/tdis.o \
$(OBJDIR)/PackageMover.o \
$(OBJDIR)/PackageBudget.o \
$(OBJDIR)/TimeSeriesManager.o \
$(OBJDIR)/Mover.o \
$(OBJDIR)/VirtualBase.o \
$(OBJDIR)/VirtualDataContainer.o \
$(OBJDIR)/BaseExchange.o \
$(OBJDIR)/SparseMatrix.o \
$(OBJDIR)/DiscretizationBase.o \
$(OBJDIR)/UzfCellGroup.o \
$(OBJDIR)/BudgetTerm.o \
$(OBJDIR)/Observe.o \
$(OBJDIR)/OutputControlData.o \
$(OBJDIR)/gwf3dis8.o \
$(OBJDIR)/LayeredArrayReader.o \
$(OBJDIR)/TimeArray.o \
$(OBJDIR)/NumericalPackage.o \
$(OBJDIR)/LoadMf6File.o \
$(OBJDIR)/ExplicitModel.o \
$(OBJDIR)/BaseSolution.o \
$(OBJDIR)/gwf3disu8.o \
$(OBJDIR)/TimeArraySeries.o \
$(OBJDIR)/SolutionGroup.o \
$(OBJDIR)/ObsContainer.o \
$(OBJDIR)/ExplicitSolution.o \
$(OBJDIR)/TimeArraySeriesLink.o \
$(OBJDIR)/Xt3dInterface.o \
$(OBJDIR)/gwf3disv8.o \
$(OBJDIR)/ImsLinearSolver.o \
$(OBJDIR)/TimeArraySeriesManager.o \
$(OBJDIR)/BudgetObject.o \
$(OBJDIR)/ObsUtility.o \
$(OBJDIR)/gwf3tvbase8.o \
$(OBJDIR)/gwf3tvs8.o \
$(OBJDIR)/IdmMf6File.o \
$(OBJDIR)/OutputControl.o \
$(OBJDIR)/LinearSolverFactory.o \
$(OBJDIR)/gwf3ic8.o \
$(OBJDIR)/Obs3.o \
$(OBJDIR)/gwf3tvk8.o \
$(OBJDIR)/GwtSpc.o \
$(OBJDIR)/IdmSimulation.o \
$(OBJDIR)/gwt1oc1.o \
$(OBJDIR)/gwf3mvr8.o \
$(OBJDIR)/gwt1ic1.o \
$(OBJDIR)/gwt1obs1.o \
$(OBJDIR)/gwf3obs8.o \
$(OBJDIR)/gwf3oc8.o \
$(OBJDIR)/gwf3sto8.o \
$(OBJDIR)/BoundaryPackage.o \
$(OBJDIR)/gwf3csub8.o \
$(OBJDIR)/gwf3uzf8.o \
$(OBJDIR)/gwt1cnc1.o \
$(OBJDIR)/gwf3lak8.o \
$(OBJDIR)/gwt1src1.o \
$(OBJDIR)/gwf3maw8.o \
$(OBJDIR)/gwt1fmi1.o \
$(OBJDIR)/gwf3rch8.o \
$(OBJDIR)/gwt1apt1.o \
$(OBJDIR)/gwf3wel8.o \
$(OBJDIR)/gwf3riv8.o \
$(OBJDIR)/gwf3drn8.o \
$(OBJDIR)/gwt1mwt1.o \
$(OBJDIR)/gwf3sfr8.o \
$(OBJDIR)/gwf3api8.o \
$(OBJDIR)/gwf3evt8.o \
$(OBJDIR)/NumericalModel.o \
$(OBJDIR)/gwf3ghb8.o \
$(OBJDIR)/gwf3chd8.o \
$(OBJDIR)/gwt1uzt1.o \
$(OBJDIR)/gwf3vsc8.o \
$(OBJDIR)/NumericalExchange.o \
$(OBJDIR)/gwt1ssm1.o \
$(OBJDIR)/NumericalSolution.o \
$(OBJDIR)/gwt1adv1.o \
$(OBJDIR)/gwt1lkt1.o \
$(OBJDIR)/SolutionFactory.o \
$(OBJDIR)/VirtualModel.o \
$(OBJDIR)/gwt1sft1.o \
$(OBJDIR)/gwt1mst1.o \
$(OBJDIR)/VirtualSolution.o \
$(OBJDIR)/VirtualGwfModel.o \
$(OBJDIR)/gwt1ist1.o \
$(OBJDIR)/gwf3npf8.o \
$(OBJDIR)/TransportModel.o \
$(OBJDIR)/DisConnExchange.o \
$(OBJDIR)/GhostNode.o \
$(OBJDIR)/gwt1mvt1.o \
$(OBJDIR)/VirtualExchange.o \
$(OBJDIR)/gwt1dsp1.o \
$(OBJDIR)/gwf3buy8.o \
$(OBJDIR)/VirtualGwfExchange.o \
$(OBJDIR)/gwf3hfb8.o \
$(OBJDIR)/VirtualGwtExchange.o \
$(OBJDIR)/VirtualGwtModel.o \
$(OBJDIR)/CellWithNbrs.o \
$(OBJDIR)/RouterBase.o \
$(OBJDIR)/gwf3.o \
$(OBJDIR)/gwt1.o \
$(OBJDIR)/GridSorting.o \
$(OBJDIR)/GwfGwfExchange.o \
$(OBJDIR)/SerialRouter.o \
$(OBJDIR)/RouterFactory.o \
$(OBJDIR)/GwtGwtExchange.o \
$(OBJDIR)/GridConnection.o \
$(OBJDIR)/SpatialModelConnection.o \
$(OBJDIR)/Mapper.o \
$(OBJDIR)/VirtualDataManager.o \
$(OBJDIR)/GwfInterfaceModel.o \
$(OBJDIR)/RunControl.o \
$(OBJDIR)/GwtInterfaceModel.o \
$(OBJDIR)/RunControlFactory.o \
$(OBJDIR)/GwfGwfConnection.o \
$(OBJDIR)/GwtGwtConnection.o \
$(OBJDIR)/ConnectionBuilder.o \
$(OBJDIR)/GwfGwtExchange.o \
$(OBJDIR)/Simulation.o \
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

