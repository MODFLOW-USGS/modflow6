# makefile created by pymake (version 1.2.9.dev0) for the 'mf6' executable.


include ./makedefaults

# Define the source file directories
SOURCEDIR1=../src
<<<<<<< HEAD
SOURCEDIR2=../src/Model
SOURCEDIR3=../src/Model/TransportModel
SOURCEDIR4=../src/Model/GroundWaterFlow
SOURCEDIR5=../src/Model/Geometry
SOURCEDIR6=../src/Model/ModelUtilities
SOURCEDIR7=../src/Model/GroundWaterTransport
SOURCEDIR8=../src/Model/Connection
SOURCEDIR9=../src/Distributed
SOURCEDIR10=../src/Utilities
SOURCEDIR11=../src/Utilities/Idm
SOURCEDIR12=../src/Utilities/Idm/mf6blockfile
SOURCEDIR13=../src/Utilities/Idm/selector
SOURCEDIR14=../src/Utilities/Vector
SOURCEDIR15=../src/Utilities/Matrix
SOURCEDIR16=../src/Utilities/Observation
SOURCEDIR17=../src/Utilities/ArrayRead
SOURCEDIR18=../src/Utilities/OutputControl
SOURCEDIR19=../src/Utilities/Libraries
SOURCEDIR20=../src/Utilities/Libraries/blas
SOURCEDIR21=../src/Utilities/Libraries/rcm
SOURCEDIR22=../src/Utilities/Libraries/sparsekit
SOURCEDIR23=../src/Utilities/Libraries/sparskit2
SOURCEDIR24=../src/Utilities/Libraries/daglib
SOURCEDIR25=../src/Utilities/Memory
SOURCEDIR26=../src/Utilities/TimeSeries
SOURCEDIR27=../src/Timing
SOURCEDIR28=../src/Solution
SOURCEDIR29=../src/Solution/PETSc
SOURCEDIR30=../src/Solution/LinearMethods
SOURCEDIR31=../src/Exchange
=======
SOURCEDIR2=../src/Exchange
SOURCEDIR3=../src/Timing
SOURCEDIR4=../src/Model
SOURCEDIR5=../src/Model/Connection
SOURCEDIR6=../src/Model/ModelUtilities
SOURCEDIR7=../src/Model/GroundWaterFlow
SOURCEDIR8=../src/Model/Geometry
SOURCEDIR9=../src/Model/TransportModel
SOURCEDIR10=../src/Model/GroundWaterTransport
SOURCEDIR11=../src/Solution
SOURCEDIR12=../src/Solution/LinearMethods
SOURCEDIR13=../src/Solution/PETSc
SOURCEDIR14=../src/Distributed
SOURCEDIR15=../src/Utilities
SOURCEDIR16=../src/Utilities/TimeSeries
SOURCEDIR17=../src/Utilities/Idm
SOURCEDIR18=../src/Utilities/Idm/selector
SOURCEDIR19=../src/Utilities/Idm/mf6blockfile
SOURCEDIR20=../src/Utilities/ArrayRead
SOURCEDIR21=../src/Utilities/Memory
SOURCEDIR22=../src/Utilities/Matrix
SOURCEDIR23=../src/Utilities/Vector
SOURCEDIR24=../src/Utilities/Observation
SOURCEDIR25=../src/Utilities/OutputControl
SOURCEDIR26=../src/Utilities/Libraries
SOURCEDIR27=../src/Utilities/Libraries/rcm
SOURCEDIR28=../src/Utilities/Libraries/sparskit2
SOURCEDIR29=../src/Utilities/Libraries/sparsekit
SOURCEDIR30=../src/Utilities/Libraries/blas
SOURCEDIR31=../src/Utilities/Libraries/daglib
>>>>>>> 73519cf4e7 (rebuild makefiles)

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
${SOURCEDIR30} \
${SOURCEDIR31} 

.SUFFIXES: .f90 .F90 .o

OBJECTS = \
$(OBJDIR)/kind.o \
$(OBJDIR)/Constants.o \
$(OBJDIR)/ErrorUtil.o \
$(OBJDIR)/SimVariables.o \
$(OBJDIR)/ArrayHandlers.o \
$(OBJDIR)/Message.o \
$(OBJDIR)/defmacro.o \
$(OBJDIR)/compilerversion.o \
$(OBJDIR)/version.o \
$(OBJDIR)/Sim.o \
$(OBJDIR)/OpenSpec.o \
$(OBJDIR)/InputOutput.o \
$(OBJDIR)/TableTerm.o \
$(OBJDIR)/Table.o \
$(OBJDIR)/MemoryHelper.o \
$(OBJDIR)/CharString.o \
$(OBJDIR)/InputDefinition.o \
$(OBJDIR)/Memory.o \
$(OBJDIR)/List.o \
$(OBJDIR)/simtdisidm.o \
$(OBJDIR)/simnamidm.o \
$(OBJDIR)/gwt1idm.o \
$(OBJDIR)/gwt1ic1idm.o \
$(OBJDIR)/gwt1dsp1idm.o \
$(OBJDIR)/gwt1disv1idm.o \
$(OBJDIR)/gwt1disu1idm.o \
$(OBJDIR)/gwt1dis1idm.o \
$(OBJDIR)/gwt1cnc1idm.o \
$(OBJDIR)/gwf3wel8idm.o \
$(OBJDIR)/gwf3riv8idm.o \
$(OBJDIR)/gwf3rch8idm.o \
$(OBJDIR)/gwf3rcha8idm.o \
$(OBJDIR)/gwf3npf8idm.o \
$(OBJDIR)/gwf3idm.o \
$(OBJDIR)/gwf3ic8idm.o \
$(OBJDIR)/gwf3ghb8idm.o \
$(OBJDIR)/gwf3evt8idm.o \
$(OBJDIR)/gwf3evta8idm.o \
$(OBJDIR)/gwf3drn8idm.o \
$(OBJDIR)/gwf3disv8idm.o \
$(OBJDIR)/gwf3disu8idm.o \
$(OBJDIR)/gwf3dis8idm.o \
$(OBJDIR)/gwf3chd8idm.o \
$(OBJDIR)/gwtgwtidm.o \
$(OBJDIR)/gwfgwtidm.o \
$(OBJDIR)/gwfgwfidm.o \
$(OBJDIR)/LongLineReader.o \
$(OBJDIR)/DevFeature.o \
$(OBJDIR)/MemoryList.o \
$(OBJDIR)/IdmSimDfnSelector.o \
$(OBJDIR)/IdmGwtDfnSelector.o \
$(OBJDIR)/IdmGwfDfnSelector.o \
$(OBJDIR)/IdmExgDfnSelector.o \
$(OBJDIR)/TimeSeriesRecord.o \
$(OBJDIR)/MathUtil.o \
$(OBJDIR)/BlockParser.o \
$(OBJDIR)/MemoryManager.o \
$(OBJDIR)/IdmDfnSelector.o \
$(OBJDIR)/TimeSeries.o \
$(OBJDIR)/SourceCommon.o \
$(OBJDIR)/MemoryManagerExt.o \
$(OBJDIR)/ats.o \
$(OBJDIR)/GeomUtil.o \
$(OBJDIR)/TimeSeriesLink.o \
$(OBJDIR)/TimeSeriesFileList.o \
$(OBJDIR)/tdis.o \
$(OBJDIR)/HashTable.o \
$(OBJDIR)/VectorBase.o \
$(OBJDIR)/Sparse.o \
$(OBJDIR)/DisvGeom.o \
$(OBJDIR)/ArrayReaders.o \
$(OBJDIR)/TimeSeriesManager.o \
$(OBJDIR)/SmoothingFunctions.o \
$(OBJDIR)/MatrixBase.o \
$(OBJDIR)/ListReader.o \
$(OBJDIR)/Connections.o \
$(OBJDIR)/TimeArray.o \
$(OBJDIR)/ObsOutput.o \
$(OBJDIR)/DiscretizationBase.o \
$(OBJDIR)/TimeArraySeries.o \
$(OBJDIR)/ObsOutputList.o \
$(OBJDIR)/Observe.o \
$(OBJDIR)/TimeArraySeriesLink.o \
$(OBJDIR)/ObsUtility.o \
$(OBJDIR)/ObsContainer.o \
$(OBJDIR)/BudgetFileReader.o \
$(OBJDIR)/TimeArraySeriesManager.o \
$(OBJDIR)/PackageMover.o \
$(OBJDIR)/Obs3.o \
$(OBJDIR)/NumericalPackage.o \
$(OBJDIR)/Budget.o \
$(OBJDIR)/BudgetTerm.o \
$(OBJDIR)/sort.o \
$(OBJDIR)/SfrCrossSectionUtils.o \
$(OBJDIR)/BoundaryPackage.o \
$(OBJDIR)/VirtualBase.o \
$(OBJDIR)/STLVecInt.o \
$(OBJDIR)/BaseModel.o \
$(OBJDIR)/PackageBudget.o \
$(OBJDIR)/HeadFileReader.o \
$(OBJDIR)/BudgetObject.o \
$(OBJDIR)/PrintSaveManager.o \
$(OBJDIR)/SfrCrossSectionManager.o \
$(OBJDIR)/dag_module.o \
$(OBJDIR)/BoundaryPackageExt.o \
$(OBJDIR)/VirtualDataLists.o \
$(OBJDIR)/VirtualDataContainer.o \
$(OBJDIR)/SimStages.o \
$(OBJDIR)/NumericalModel.o \
$(OBJDIR)/FlowModelInterface.o \
$(OBJDIR)/OutputControlData.o \
$(OBJDIR)/gwf3ic8.o \
$(OBJDIR)/Xt3dAlgorithm.o \
$(OBJDIR)/gwf3tvbase8.o \
$(OBJDIR)/gwf3sfr8.o \
$(OBJDIR)/gwf3riv8.o \
$(OBJDIR)/gwf3maw8.o \
$(OBJDIR)/mf6lists.o \
$(OBJDIR)/gwf3lak8.o \
$(OBJDIR)/GwfVscInputData.o \
$(OBJDIR)/gwf3ghb8.o \
$(OBJDIR)/gwf3drn8.o \
$(OBJDIR)/IndexMap.o \
$(OBJDIR)/VirtualModel.o \
$(OBJDIR)/BaseExchange.o \
$(OBJDIR)/tsp1fmi1.o \
$(OBJDIR)/GwtSpc.o \
$(OBJDIR)/OutputControl.o \
$(OBJDIR)/tsp1ic1.o \
$(OBJDIR)/TspAdvOptions.o \
$(OBJDIR)/UzfCellGroup.o \
$(OBJDIR)/Xt3dInterface.o \
$(OBJDIR)/gwf3tvk8.o \
$(OBJDIR)/gwf3vsc8.o \
$(OBJDIR)/GwfNpfOptions.o \
$(OBJDIR)/InterfaceMap.o \
$(OBJDIR)/SeqVector.o \
$(OBJDIR)/ImsLinearSettings.o \
$(OBJDIR)/ConvergenceSummary.o \
$(OBJDIR)/ArrayReaderBase.o \
$(OBJDIR)/CellWithNbrs.o \
$(OBJDIR)/NumericalExchange.o \
$(OBJDIR)/tsp1ssm1.o \
$(OBJDIR)/tsp1oc1.o \
$(OBJDIR)/tsp1obs1.o \
$(OBJDIR)/tsp1mvt1.o \
$(OBJDIR)/tsp1adv1.o \
$(OBJDIR)/gwf3disv8.o \
$(OBJDIR)/gwf3disu8.o \
$(OBJDIR)/gwf3dis8.o \
$(OBJDIR)/gwf3uzf8.o \
$(OBJDIR)/tsp1apt1.o \
$(OBJDIR)/gwt1mst1.o \
$(OBJDIR)/GwtDspOptions.o \
$(OBJDIR)/gwf3npf8.o \
$(OBJDIR)/gwf3tvs8.o \
$(OBJDIR)/GwfStorageUtils.o \
$(OBJDIR)/Mover.o \
$(OBJDIR)/GwfMvrPeriodData.o \
$(OBJDIR)/ims8misc.o \
$(OBJDIR)/GwfBuyInputData.o \
$(OBJDIR)/VirtualSolution.o \
$(OBJDIR)/SparseMatrix.o \
$(OBJDIR)/LinearSolverBase.o \
$(OBJDIR)/ims8reordering.o \
$(OBJDIR)/ModflowInput.o \
$(OBJDIR)/IdmLogger.o \
$(OBJDIR)/Integer2dReader.o \
$(OBJDIR)/VirtualExchange.o \
$(OBJDIR)/GridSorting.o \
$(OBJDIR)/DisConnExchange.o \
$(OBJDIR)/CsrUtils.o \
$(OBJDIR)/tsp1.o \
$(OBJDIR)/gwt1uzt1.o \
$(OBJDIR)/gwt1src1.o \
$(OBJDIR)/gwt1sft1.o \
$(OBJDIR)/gwt1mwt1.o \
$(OBJDIR)/gwt1lkt1.o \
$(OBJDIR)/gwt1ist1.o \
$(OBJDIR)/gwt1dsp1.o \
$(OBJDIR)/gwt1cnc1.o \
$(OBJDIR)/gwf3api8.o \
$(OBJDIR)/gwf3wel8.o \
$(OBJDIR)/gwf3rch8.o \
$(OBJDIR)/gwf3sto8.o \
$(OBJDIR)/gwf3oc8.o \
$(OBJDIR)/gwf3obs8.o \
$(OBJDIR)/gwf3mvr8.o \
$(OBJDIR)/gwf3hfb8.o \
$(OBJDIR)/gwf3csub8.o \
$(OBJDIR)/gwf3buy8.o \
$(OBJDIR)/GhostNode.o \
$(OBJDIR)/gwf3evt8.o \
$(OBJDIR)/gwf3chd8.o \
$(OBJDIR)/RouterBase.o \
$(OBJDIR)/ImsLinearSolver.o \
$(OBJDIR)/ims8base.o \
$(OBJDIR)/StructVector.o \
$(OBJDIR)/DefinitionSelect.o \
$(OBJDIR)/InputLoadType.o \
$(OBJDIR)/Integer1dReader.o \
$(OBJDIR)/Double2dReader.o \
$(OBJDIR)/Double1dReader.o \
$(OBJDIR)/GridConnection.o \
$(OBJDIR)/DistributedVariable.o \
$(OBJDIR)/gwt1.o \
$(OBJDIR)/gwf3.o \
$(OBJDIR)/GwfExchangeMover.o \
$(OBJDIR)/SerialRouter.o \
$(OBJDIR)/Timer.o \
$(OBJDIR)/LinearSolverFactory.o \
$(OBJDIR)/ims8linear.o \
$(OBJDIR)/BaseSolution.o \
$(OBJDIR)/StructArray.o \
$(OBJDIR)/BoundInputContext.o \
$(OBJDIR)/AsciiInputLoadType.o \
$(OBJDIR)/LayeredArrayReader.o \
$(OBJDIR)/ExplicitModel.o \
$(OBJDIR)/SpatialModelConnection.o \
$(OBJDIR)/GwtInterfaceModel.o \
$(OBJDIR)/GwtGwtExchange.o \
$(OBJDIR)/GwfInterfaceModel.o \
$(OBJDIR)/GwfGwfExchange.o \
$(OBJDIR)/RouterFactory.o \
$(OBJDIR)/NumericalSolution.o \
$(OBJDIR)/MappedMemory.o \
$(OBJDIR)/StressListInput.o \
$(OBJDIR)/StressGridInput.o \
$(OBJDIR)/LoadMf6File.o \
$(OBJDIR)/ExplicitSolution.o \
$(OBJDIR)/GwtGwtConnection.o \
$(OBJDIR)/GwfGwfConnection.o \
$(OBJDIR)/VirtualDataManager.o \
$(OBJDIR)/Mapper.o \
$(OBJDIR)/IdmMf6File.o \
$(OBJDIR)/ModelPackageInput.o \
$(OBJDIR)/VirtualGwtModel.o \
$(OBJDIR)/VirtualGwtExchange.o \
$(OBJDIR)/VirtualGwfModel.o \
$(OBJDIR)/VirtualGwfExchange.o \
$(OBJDIR)/SolutionGroup.o \
$(OBJDIR)/SolutionFactory.o \
$(OBJDIR)/GwfGwtExchange.o \
$(OBJDIR)/RunControl.o \
$(OBJDIR)/SourceLoad.o \
$(OBJDIR)/ModelPackageInputs.o \
$(OBJDIR)/SimulationCreate.o \
$(OBJDIR)/RunControlFactory.o \
$(OBJDIR)/IdmLoad.o \
$(OBJDIR)/ConnectionBuilder.o \
$(OBJDIR)/comarg.o \
$(OBJDIR)/mf6core.o \
$(OBJDIR)/BaseGeometry.o \
$(OBJDIR)/mf6.o \
$(OBJDIR)/StringList.o \
$(OBJDIR)/MemorySetHandler.o \
$(OBJDIR)/ilut.o \
$(OBJDIR)/sparsekit.o \
$(OBJDIR)/rcm.o \
$(OBJDIR)/blas1_d.o \
$(OBJDIR)/Iunit.o \
$(OBJDIR)/RectangularGeometry.o \
$(OBJDIR)/CircularGeometry.o

# Define the objects that make up the program
$(PROGRAM) : $(OBJECTS)
	-$(FC) $(OPTLEVEL) -o $@ $(OBJECTS) $(LDFLAGS)

$(OBJDIR)/%.o : %.f90
	@mkdir -p $(@D)
	$(FC) $(OPTLEVEL) $(FFLAGS) -c $< -o $@ $(INCSWITCH) $(MODSWITCH)

$(OBJDIR)/%.o : %.F90
	@mkdir -p $(@D)
	$(FC) $(OPTLEVEL) $(FFLAGS) -c $< -o $@ $(INCSWITCH) $(MODSWITCH)

