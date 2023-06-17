# makefile created by pymake (version 1.2.7) for the 'mf6' executable.


include ./makedefaults

# Define the source file directories
SOURCEDIR1=..\src
SOURCEDIR2=..\src\Distributed
SOURCEDIR3=..\src\Exchange
SOURCEDIR4=..\src\Model
SOURCEDIR5=..\src\Model\Connection
SOURCEDIR6=..\src\Model\Geometry
SOURCEDIR7=..\src\Model\GroundWaterEnergy
SOURCEDIR8=..\src\Model\GroundWaterFlow
SOURCEDIR9=..\src\Model\GroundWaterTransport
SOURCEDIR10=..\src\Model\ModelUtilities
SOURCEDIR11=..\src\Model\TransportModel
SOURCEDIR12=..\src\Solution
SOURCEDIR13=..\src\Solution\LinearMethods
SOURCEDIR14=..\src\Solution\PETSc
SOURCEDIR15=..\src\Timing
SOURCEDIR16=..\src\Utilities
SOURCEDIR17=..\src\Utilities\ArrayRead
SOURCEDIR18=..\src\Utilities\Idm
SOURCEDIR19=..\src\Utilities\Idm\mf6blockfile
SOURCEDIR20=..\src\Utilities\Idm\selector
SOURCEDIR21=..\src\Utilities\Libraries
SOURCEDIR22=..\src\Utilities\Libraries\blas
SOURCEDIR23=..\src\Utilities\Libraries\daglib
SOURCEDIR24=..\src\Utilities\Libraries\rcm
SOURCEDIR25=..\src\Utilities\Libraries\sparsekit
SOURCEDIR26=..\src\Utilities\Libraries\sparskit2
SOURCEDIR27=..\src\Utilities\Matrix
SOURCEDIR28=..\src\Utilities\Memory
SOURCEDIR29=..\src\Utilities\Observation
SOURCEDIR30=..\src\Utilities\OutputControl
SOURCEDIR31=..\src\Utilities\TimeSeries
SOURCEDIR32=..\src\Utilities\Vector

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
${SOURCEDIR31} \
${SOURCEDIR32} 

.SUFFIXES: .f90 .F90 .o

OBJECTS = \
$(OBJDIR)/kind.o \
$(OBJDIR)/Constants.o \
$(OBJDIR)/SimVariables.o \
$(OBJDIR)/genericutils.o \
$(OBJDIR)/defmacro.o \
$(OBJDIR)/compilerversion.o \
$(OBJDIR)/ArrayHandlers.o \
$(OBJDIR)/version.o \
$(OBJDIR)/Message.o \
$(OBJDIR)/Sim.o \
$(OBJDIR)/OpenSpec.o \
$(OBJDIR)/InputOutput.o \
$(OBJDIR)/TableTerm.o \
$(OBJDIR)/Table.o \
$(OBJDIR)/MemoryHelper.o \
$(OBJDIR)/CharString.o \
$(OBJDIR)/Memory.o \
$(OBJDIR)/List.o \
$(OBJDIR)/MemoryList.o \
$(OBJDIR)/TimeSeriesRecord.o \
$(OBJDIR)/BlockParser.o \
$(OBJDIR)/MemoryManager.o \
$(OBJDIR)/TimeSeries.o \
$(OBJDIR)/ats.o \
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
$(OBJDIR)/DiscretizationBase.o \
$(OBJDIR)/TimeArray.o \
$(OBJDIR)/ObsOutput.o \
$(OBJDIR)/TimeArraySeries.o \
$(OBJDIR)/ObsOutputList.o \
$(OBJDIR)/Observe.o \
$(OBJDIR)/TimeArraySeriesLink.o \
$(OBJDIR)/ObsUtility.o \
$(OBJDIR)/ObsContainer.o \
$(OBJDIR)/TspLabels.o \
$(OBJDIR)/BudgetFileReader.o \
$(OBJDIR)/TimeArraySeriesManager.o \
$(OBJDIR)/PackageMover.o \
$(OBJDIR)/Obs3.o \
$(OBJDIR)/NumericalPackage.o \
$(OBJDIR)/Budget.o \
$(OBJDIR)/SeqVector.o \
$(OBJDIR)/sort.o \
$(OBJDIR)/SfrCrossSectionUtils.o \
$(OBJDIR)/BudgetTerm.o \
$(OBJDIR)/BoundaryPackage.o \
$(OBJDIR)/BaseModel.o \
$(OBJDIR)/SparseMatrix.o \
$(OBJDIR)/LinearSolverBase.o \
$(OBJDIR)/ims8reordering.o \
$(OBJDIR)/VirtualBase.o \
$(OBJDIR)/STLVecInt.o \
$(OBJDIR)/PrintSaveManager.o \
$(OBJDIR)/InputDefinition.o \
$(OBJDIR)/SfrCrossSectionManager.o \
$(OBJDIR)/dag_module.o \
$(OBJDIR)/BudgetObject.o \
$(OBJDIR)/NumericalModel.o \
$(OBJDIR)/BaseExchange.o \
$(OBJDIR)/ImsLinearSolver.o \
$(OBJDIR)/ims8base.o \
$(OBJDIR)/VirtualDataLists.o \
$(OBJDIR)/VirtualDataContainer.o \
$(OBJDIR)/SimStages.o \
$(OBJDIR)/PackageBudget.o \
$(OBJDIR)/HeadFileReader.o \
$(OBJDIR)/OutputControlData.o \
$(OBJDIR)/gwf3ic8.o \
$(OBJDIR)/simnamidm.o \
$(OBJDIR)/gwt1idm.o \
$(OBJDIR)/gwt1dsp1idm.o \
$(OBJDIR)/gwt1disv1idm.o \
$(OBJDIR)/gwt1disu1idm.o \
$(OBJDIR)/gwt1dis1idm.o \
$(OBJDIR)/gwf3npf8idm.o \
$(OBJDIR)/gwf3idm.o \
$(OBJDIR)/gwf3disv8idm.o \
$(OBJDIR)/gwf3disu8idm.o \
$(OBJDIR)/gwf3dis8idm.o \
$(OBJDIR)/gwe1idm.o \
$(OBJDIR)/gwe1dspidm.o \
$(OBJDIR)/gwe1disv1idm.o \
$(OBJDIR)/gwe1disu1idm.o \
$(OBJDIR)/gwe1dis1idm.o \
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
$(OBJDIR)/Timer.o \
$(OBJDIR)/NumericalExchange.o \
$(OBJDIR)/LinearSolverFactory.o \
$(OBJDIR)/ims8linear.o \
$(OBJDIR)/BaseSolution.o \
$(OBJDIR)/IndexMap.o \
$(OBJDIR)/VirtualModel.o \
$(OBJDIR)/tsp1fmi1.o \
$(OBJDIR)/GwtSpc.o \
$(OBJDIR)/GweInputData.o \
$(OBJDIR)/OutputControl.o \
$(OBJDIR)/tsp1ic1.o \
$(OBJDIR)/TspAdvOptions.o \
$(OBJDIR)/MemoryManagerExt.o \
$(OBJDIR)/IdmSimDfnSelector.o \
$(OBJDIR)/IdmGwtDfnSelector.o \
$(OBJDIR)/IdmGwfDfnSelector.o \
$(OBJDIR)/IdmGweDfnSelector.o \
$(OBJDIR)/UzfCellGroup.o \
$(OBJDIR)/Xt3dInterface.o \
$(OBJDIR)/gwf3tvk8.o \
$(OBJDIR)/gwf3vsc8_memoryMng_strings.o \
$(OBJDIR)/GwfNpfOptions.o \
$(OBJDIR)/NumericalSolution.o \
$(OBJDIR)/InterfaceMap.o \
$(OBJDIR)/CellWithNbrs.o \
$(OBJDIR)/tsp1ssm1.o \
$(OBJDIR)/tsp1oc1.o \
$(OBJDIR)/tsp1obs1.o \
$(OBJDIR)/tsp1mvt1.o \
$(OBJDIR)/tsp1adv1.o \
$(OBJDIR)/gwf3disv8.o \
$(OBJDIR)/gwf3disu8.o \
$(OBJDIR)/gwf3dis8.o \
$(OBJDIR)/IdmDfnSelector.o \
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
$(OBJDIR)/GweDspOptions.o \
$(OBJDIR)/VirtualSolution.o \
$(OBJDIR)/ArrayReaderBase.o \
$(OBJDIR)/VirtualExchange.o \
$(OBJDIR)/GridSorting.o \
$(OBJDIR)/DisConnExchange.o \
$(OBJDIR)/CsrUtils.o \
$(OBJDIR)/tsp1cnc1.o \
$(OBJDIR)/tsp1.o \
$(OBJDIR)/ModelPackageInputs.o \
$(OBJDIR)/gwt1uzt1.o \
$(OBJDIR)/gwt1src1.o \
$(OBJDIR)/gwt1sft1.o \
$(OBJDIR)/gwt1mwt1.o \
$(OBJDIR)/gwt1lkt1.o \
$(OBJDIR)/gwt1ist1.o \
$(OBJDIR)/gwt1dsp1.o \
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
$(OBJDIR)/gwe1uze1.o \
$(OBJDIR)/gwe1src1.o \
$(OBJDIR)/gwe1sfe1.o \
$(OBJDIR)/gwe1mwe1.o \
$(OBJDIR)/gwe1mst1.o \
$(OBJDIR)/gwe1lke1.o \
$(OBJDIR)/gwe1dsp1.o \
$(OBJDIR)/RouterBase.o \
$(OBJDIR)/Integer2dReader.o \
$(OBJDIR)/GridConnection.o \
$(OBJDIR)/DistributedVariable.o \
$(OBJDIR)/gwt1.o \
$(OBJDIR)/gwf3.o \
$(OBJDIR)/gwe1.o \
$(OBJDIR)/SerialRouter.o \
$(OBJDIR)/StructVector.o \
$(OBJDIR)/IdmLogger.o \
$(OBJDIR)/Integer1dReader.o \
$(OBJDIR)/Double2dReader.o \
$(OBJDIR)/Double1dReader.o \
$(OBJDIR)/ExplicitModel.o \
$(OBJDIR)/SpatialModelConnection.o \
$(OBJDIR)/GwtInterfaceModel.o \
$(OBJDIR)/GwtGwtExchange.o \
$(OBJDIR)/GwfInterfaceModel.o \
$(OBJDIR)/GwfGwfExchange.o \
$(OBJDIR)/GweInterfaceModel.o \
$(OBJDIR)/GweGweExchange.o \
$(OBJDIR)/RouterFactory.o \
$(OBJDIR)/MappedMemory.o \
$(OBJDIR)/StructArray.o \
$(OBJDIR)/ModflowInput.o \
$(OBJDIR)/LayeredArrayReader.o \
$(OBJDIR)/DefinitionSelect.o \
$(OBJDIR)/ExplicitSolution.o \
$(OBJDIR)/GwtGwtConnection.o \
$(OBJDIR)/GwfGwfConnection.o \
$(OBJDIR)/GweGweConnection.o \
$(OBJDIR)/VirtualDataManager.o \
$(OBJDIR)/Mapper.o \
$(OBJDIR)/LoadMf6File.o \
$(OBJDIR)/VirtualGwtModel.o \
$(OBJDIR)/VirtualGwtExchange.o \
$(OBJDIR)/VirtualGwfModel.o \
$(OBJDIR)/VirtualGwfExchange.o \
$(OBJDIR)/VirtualGweModel.o \
$(OBJDIR)/VirtualGweExchange.o \
$(OBJDIR)/SolutionGroup.o \
$(OBJDIR)/SolutionFactory.o \
$(OBJDIR)/GwfGwtExchange.o \
$(OBJDIR)/GwfGweExchange.o \
$(OBJDIR)/RunControl.o \
$(OBJDIR)/IdmMf6File.o \
$(OBJDIR)/SimulationCreate.o \
$(OBJDIR)/RunControlFactory.o \
$(OBJDIR)/IdmSimulation.o \
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
$(OBJDIR)/LatHeatVapor.o \
$(OBJDIR)/GwtAdvOptions.o \
$(OBJDIR)/gwf3vsc8.o \
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

