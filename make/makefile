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
SOURCEDIR10=../src/Solution/PETSc
SOURCEDIR11=../src/Solution/LinearMethods
SOURCEDIR12=../src/Timing
SOURCEDIR13=../src/Utilities
SOURCEDIR14=../src/Utilities/TimeSeries
SOURCEDIR15=../src/Utilities/Libraries
SOURCEDIR16=../src/Utilities/Libraries/rcm
SOURCEDIR17=../src/Utilities/Libraries/sparsekit
SOURCEDIR18=../src/Utilities/Libraries/sparskit2
SOURCEDIR19=../src/Utilities/Libraries/blas
SOURCEDIR20=../src/Utilities/Libraries/daglib
SOURCEDIR21=../src/Utilities/Idm
SOURCEDIR22=../src/Utilities/Matrix
SOURCEDIR23=../src/Utilities/Vector
SOURCEDIR24=../src/Utilities/Observation
SOURCEDIR25=../src/Utilities/OutputControl
SOURCEDIR26=../src/Utilities/Memory
SOURCEDIR27=../src/Utilities/ArrayRead

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
${SOURCEDIR27} 

.SUFFIXES: .f90 .F90 .o

OBJECTS = \
$(OBJDIR)/CharString.o \
$(OBJDIR)/rcm.o \
$(OBJDIR)/OpenSpec.o \
$(OBJDIR)/sparsekit.o \
$(OBJDIR)/ilut.o \
$(OBJDIR)/blas1_d.o \
$(OBJDIR)/kind.o \
$(OBJDIR)/IdmLogger.o \
$(OBJDIR)/Sparse.o \
$(OBJDIR)/GwtDspOptions.o \
$(OBJDIR)/BaseGeometry.o \
$(OBJDIR)/dag_module.o \
$(OBJDIR)/InputDefinition.o \
$(OBJDIR)/gwf3disv8idm.o \
$(OBJDIR)/VectorBase.o \
$(OBJDIR)/gwf3npf8idm.o \
$(OBJDIR)/gwt1dspidm.o \
$(OBJDIR)/Constants.o \
$(OBJDIR)/gwf3disu8idm.o \
$(OBJDIR)/ims8reordering.o \
$(OBJDIR)/InterfaceMap.o \
$(OBJDIR)/compilerversion.o \
$(OBJDIR)/defmacro.o \
$(OBJDIR)/HashTable.o \
$(OBJDIR)/SimVariables.o \
$(OBJDIR)/genericutils.o \
$(OBJDIR)/CsrUtils.o \
$(OBJDIR)/GwtAdvOptions.o \
$(OBJDIR)/SmoothingFunctions.o \
$(OBJDIR)/GwfNpfOptions.o \
$(OBJDIR)/gwf3dis8idm.o \
$(OBJDIR)/version.o \
$(OBJDIR)/GwfBuyInputData.o \
$(OBJDIR)/Timer.o \
$(OBJDIR)/GwfStorageUtils.o \
$(OBJDIR)/ArrayHandlers.o \
$(OBJDIR)/List.o \
$(OBJDIR)/SfrCrossSectionUtils.o \
$(OBJDIR)/Xt3dAlgorithm.o \
$(OBJDIR)/ims8misc.o \
$(OBJDIR)/MatrixBase.o \
$(OBJDIR)/GwfVscInputData.o \
$(OBJDIR)/ObsOutput.o \
$(OBJDIR)/LinearSolverBase.o \
$(OBJDIR)/StringList.o \
$(OBJDIR)/TimeSeriesRecord.o \
$(OBJDIR)/Message.o \
$(OBJDIR)/mf6lists.o \
$(OBJDIR)/Sim.o \
$(OBJDIR)/InputOutput.o \
$(OBJDIR)/DisvGeom.o \
$(OBJDIR)/Iunit.o \
$(OBJDIR)/ObsOutputList.o \
$(OBJDIR)/HeadFileReader.o \
$(OBJDIR)/MemoryHelper.o \
$(OBJDIR)/sort.o \
$(OBJDIR)/ArrayReaders.o \
$(OBJDIR)/CircularGeometry.o \
$(OBJDIR)/BlockParser.o \
$(OBJDIR)/RectangularGeometry.o \
$(OBJDIR)/Budget.o \
$(OBJDIR)/InputDefinitionSelector.o \
$(OBJDIR)/TableTerm.o \
$(OBJDIR)/ModflowInput.o \
$(OBJDIR)/BudgetFileReader.o \
$(OBJDIR)/PrintSaveManager.o \
$(OBJDIR)/ims8base.o \
$(OBJDIR)/ArrayReaderBase.o \
$(OBJDIR)/NameFile.o \
$(OBJDIR)/Double1dReader.o \
$(OBJDIR)/STLVecInt.o \
$(OBJDIR)/StructVector.o \
$(OBJDIR)/Double2dReader.o \
$(OBJDIR)/Table.o \
$(OBJDIR)/TimeSeries.o \
$(OBJDIR)/ListReader.o \
$(OBJDIR)/Memory.o \
$(OBJDIR)/TimeSeriesFileList.o \
$(OBJDIR)/comarg.o \
$(OBJDIR)/Integer2dReader.o \
$(OBJDIR)/SfrCrossSectionManager.o \
$(OBJDIR)/TimeSeriesLink.o \
$(OBJDIR)/MemoryList.o \
$(OBJDIR)/MemoryManager.o \
$(OBJDIR)/Integer1dReader.o \
$(OBJDIR)/MappedVariable.o \
$(OBJDIR)/BaseModel.o \
$(OBJDIR)/LayeredArrayReader.o \
$(OBJDIR)/Connections.o \
$(OBJDIR)/DistributedData.o \
$(OBJDIR)/ats.o \
$(OBJDIR)/MemoryManagerExt.o \
$(OBJDIR)/PackageMover.o \
$(OBJDIR)/MemorySetHandler.o \
$(OBJDIR)/SeqVector.o \
$(OBJDIR)/tdis.o \
$(OBJDIR)/ims8linear.o \
$(OBJDIR)/PackageBudget.o \
$(OBJDIR)/StructArray.o \
$(OBJDIR)/GwfMvrPeriodData.o \
$(OBJDIR)/UzfCellGroup.o \
$(OBJDIR)/SparseMatrix.o \
$(OBJDIR)/TimeSeriesManager.o \
$(OBJDIR)/BaseExchange.o \
$(OBJDIR)/Mover.o \
$(OBJDIR)/BaseSolution.o \
$(OBJDIR)/ImsLinearSolver.o \
$(OBJDIR)/LoadMf6FileType.o \
$(OBJDIR)/DiscretizationBase.o \
$(OBJDIR)/BudgetTerm.o \
$(OBJDIR)/Xt3dInterface.o \
$(OBJDIR)/LinearSolverFactory.o \
$(OBJDIR)/NumericalPackage.o \
$(OBJDIR)/Observe.o \
$(OBJDIR)/IdmMf6FileLoader.o \
$(OBJDIR)/gwf3ic8.o \
$(OBJDIR)/SolutionGroup.o \
$(OBJDIR)/OutputControlData.o \
$(OBJDIR)/TimeArray.o \
$(OBJDIR)/BudgetObject.o \
$(OBJDIR)/TimeArraySeries.o \
$(OBJDIR)/gwf3mvr8.o \
$(OBJDIR)/TimeArraySeriesLink.o \
$(OBJDIR)/gwf3disv8.o \
$(OBJDIR)/TimeArraySeriesManager.o \
$(OBJDIR)/ObsContainer.o \
$(OBJDIR)/gwf3tvbase8.o \
$(OBJDIR)/gwf3dis8.o \
$(OBJDIR)/gwf3disu8.o \
$(OBJDIR)/ObsUtility.o \
$(OBJDIR)/gwt1ic1.o \
$(OBJDIR)/GwtSpc.o \
$(OBJDIR)/OutputControl.o \
$(OBJDIR)/gwf3oc8.o \
$(OBJDIR)/gwt1oc1.o \
$(OBJDIR)/Obs3.o \
$(OBJDIR)/gwf3tvk8.o \
$(OBJDIR)/gwf3tvs8.o \
$(OBJDIR)/gwf3csub8.o \
$(OBJDIR)/BoundaryPackage.o \
$(OBJDIR)/gwf3rch8.o \
$(OBJDIR)/NumericalModel.o \
$(OBJDIR)/gwt1src1.o \
$(OBJDIR)/GhostNode.o \
$(OBJDIR)/TransportModel.o \
$(OBJDIR)/gwf3lak8.o \
$(OBJDIR)/DistributedModel.o \
$(OBJDIR)/NumericalExchange.o \
$(OBJDIR)/DisConnExchange.o \
$(OBJDIR)/gwf3wel8.o \
$(OBJDIR)/gwt1fmi1.o \
$(OBJDIR)/gwf3obs8.o \
$(OBJDIR)/gwt1obs1.o \
$(OBJDIR)/gwf3drn8.o \
$(OBJDIR)/gwf3evt8.o \
$(OBJDIR)/gwf3sfr8.o \
$(OBJDIR)/gwf3api8.o \
$(OBJDIR)/gwf3uzf8.o \
$(OBJDIR)/gwf3ghb8.o \
$(OBJDIR)/gwf3chd8.o \
$(OBJDIR)/gwf3maw8.o \
$(OBJDIR)/gwf3sto8.o \
$(OBJDIR)/gwt1cnc1.o \
$(OBJDIR)/gwf3riv8.o \
$(OBJDIR)/CellWithNbrs.o \
$(OBJDIR)/gwt1mst1.o \
$(OBJDIR)/gwt1mvt1.o \
$(OBJDIR)/gwt1adv1.o \
$(OBJDIR)/gwt1apt1.o \
$(OBJDIR)/gwt1lkt1.o \
$(OBJDIR)/gwt1ssm1.o \
$(OBJDIR)/NumericalSolution.o \
$(OBJDIR)/gwt1ist1.o \
$(OBJDIR)/gwt1sft1.o \
$(OBJDIR)/gwf3vsc8.o \
$(OBJDIR)/GridSorting.o \
$(OBJDIR)/gwf3hfb8.o \
$(OBJDIR)/gwt1mwt1.o \
$(OBJDIR)/gwf3npf8.o \
$(OBJDIR)/gwt1uzt1.o \
$(OBJDIR)/gwt1dsp.o \
$(OBJDIR)/gwt1.o \
$(OBJDIR)/GridConnection.o \
$(OBJDIR)/gwf3buy8.o \
$(OBJDIR)/GwtInterfaceModel.o \
$(OBJDIR)/GwtGwtExchange.o \
$(OBJDIR)/SpatialModelConnection.o \
$(OBJDIR)/gwf3.o \
$(OBJDIR)/GwfInterfaceModel.o \
$(OBJDIR)/GwtGwtConnection.o \
$(OBJDIR)/GwfGwfExchange.o \
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

