# makefile created on 2021-11-21 18:38:47.864510
# by pymake (version 1.2.1) for the 'mf6' executable.


include ./makedefaults

# Define the source file directories
SOURCEDIR1=../src
SOURCEDIR2=../src/Exchange
SOURCEDIR3=../src/Solution
SOURCEDIR4=../src/Solution/SparseMatrixSolver
SOURCEDIR5=../src/Timing
SOURCEDIR6=../src/Utilities
SOURCEDIR7=../src/Utilities/TimeSeries
SOURCEDIR8=../src/Utilities/Memory
SOURCEDIR9=../src/Utilities/OutputControl
SOURCEDIR10=../src/Utilities/Libraries
SOURCEDIR11=../src/Utilities/Libraries/rcm
SOURCEDIR12=../src/Utilities/Libraries/daglib
SOURCEDIR13=../src/Utilities/Libraries/sparsekit
SOURCEDIR14=../src/Utilities/Observation
SOURCEDIR15=../src/Model
SOURCEDIR16=../src/Model/Connection
SOURCEDIR17=../src/Model/GroundWaterTransport
SOURCEDIR18=../src/Model/ModelUtilities
SOURCEDIR19=../src/Model/GroundWaterFlow
SOURCEDIR20=../src/Model/Geometry

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
${SOURCEDIR20} 

.SUFFIXES: .f90 .F90 .o

OBJECTS = \
$(OBJDIR)/sparsekit.o \
$(OBJDIR)/kind.o \
$(OBJDIR)/OpenSpec.o \
$(OBJDIR)/Sparse.o \
$(OBJDIR)/rcm.o \
$(OBJDIR)/GwfNpfGridData.o \
$(OBJDIR)/HashTable.o \
$(OBJDIR)/BaseGeometry.o \
$(OBJDIR)/dag_module.o \
$(OBJDIR)/Constants.o \
$(OBJDIR)/compilerversion.o \
$(OBJDIR)/ims8reordering.o \
$(OBJDIR)/Xt3dAlgorithm.o \
$(OBJDIR)/SfrCrossSectionUtils.o \
$(OBJDIR)/defmacro.o \
$(OBJDIR)/SimVariables.o \
$(OBJDIR)/SmoothingFunctions.o \
$(OBJDIR)/CsrUtils.o \
$(OBJDIR)/GwfStorageUtils.o \
$(OBJDIR)/PackageBudget.o \
$(OBJDIR)/GwfNpfOptions.o \
$(OBJDIR)/genericutils.o \
$(OBJDIR)/ArrayHandlers.o \
$(OBJDIR)/List.o \
$(OBJDIR)/Timer.o \
$(OBJDIR)/version.o \
$(OBJDIR)/ObsOutput.o \
$(OBJDIR)/ims8sparsekit.o \
$(OBJDIR)/mf6lists.o \
$(OBJDIR)/Message.o \
$(OBJDIR)/TimeSeriesRecord.o \
$(OBJDIR)/StringList.o \
$(OBJDIR)/Sim.o \
$(OBJDIR)/Budget.o \
$(OBJDIR)/MemoryHelper.o \
$(OBJDIR)/sort.o \
$(OBJDIR)/Iunit.o \
$(OBJDIR)/VectorInt.o \
$(OBJDIR)/InputOutput.o \
$(OBJDIR)/PrintSaveManager.o \
$(OBJDIR)/comarg.o \
$(OBJDIR)/ArrayReaders.o \
$(OBJDIR)/HeadFileReader.o \
$(OBJDIR)/BlockParser.o \
$(OBJDIR)/ObsOutputList.o \
$(OBJDIR)/BudgetFileReader.o \
$(OBJDIR)/CircularGeometry.o \
$(OBJDIR)/TableTerm.o \
$(OBJDIR)/RectangularGeometry.o \
$(OBJDIR)/NameFile.o \
$(OBJDIR)/ims8base.o \
$(OBJDIR)/TimeSeries.o \
$(OBJDIR)/DisvGeom.o \
$(OBJDIR)/TimeSeriesFileList.o \
$(OBJDIR)/Table.o \
$(OBJDIR)/TimeSeriesLink.o \
$(OBJDIR)/SfrCrossSectionManager.o \
$(OBJDIR)/ListReader.o \
$(OBJDIR)/Memory.o \
$(OBJDIR)/MemoryList.o \
$(OBJDIR)/MemoryManager.o \
$(OBJDIR)/ims8linear.o \
$(OBJDIR)/Connections.o \
$(OBJDIR)/ats.o \
$(OBJDIR)/BaseModel.o \
$(OBJDIR)/PackageMover.o \
$(OBJDIR)/tdis.o \
$(OBJDIR)/MemorySetHandler.o \
$(OBJDIR)/Mover.o \
$(OBJDIR)/TimeSeriesManager.o \
$(OBJDIR)/UzfCellGroup.o \
$(OBJDIR)/BaseExchange.o \
$(OBJDIR)/DiscretizationBase.o \
$(OBJDIR)/BaseSolution.o \
$(OBJDIR)/Xt3dInterface.o \
$(OBJDIR)/gwf3disv8.o \
$(OBJDIR)/gwf3dis8.o \
$(OBJDIR)/NumericalPackage.o \
$(OBJDIR)/TimeArray.o \
$(OBJDIR)/Observe.o \
$(OBJDIR)/gwf3hfb8.o \
$(OBJDIR)/BudgetTerm.o \
$(OBJDIR)/ObsContainer.o \
$(OBJDIR)/OutputControlData.o \
$(OBJDIR)/gwf3disu8.o \
$(OBJDIR)/SolutionGroup.o \
$(OBJDIR)/ObsUtility.o \
$(OBJDIR)/TimeArraySeries.o \
$(OBJDIR)/gwf3ic8.o \
$(OBJDIR)/gwt1ic1.o \
$(OBJDIR)/BudgetObject.o \
$(OBJDIR)/gwf3tvbase8.o \
$(OBJDIR)/OutputControl.o \
$(OBJDIR)/TimeArraySeriesLink.o \
$(OBJDIR)/Obs3.o \
$(OBJDIR)/gwf3tvk8.o \
$(OBJDIR)/gwf3tvs8.o \
$(OBJDIR)/gwf3mvr8.o \
$(OBJDIR)/gwf3obs8.o \
$(OBJDIR)/gwf3csub8.o \
$(OBJDIR)/TimeArraySeriesManager.o \
$(OBJDIR)/gwt1obs1.o \
$(OBJDIR)/gwt1oc1.o \
$(OBJDIR)/gwf3oc8.o \
$(OBJDIR)/gwf3npf8.o \
$(OBJDIR)/GwtSpc.o \
$(OBJDIR)/BoundaryPackage.o \
$(OBJDIR)/gwf3drn8.o \
$(OBJDIR)/gwf3evt8.o \
$(OBJDIR)/gwt1src1.o \
$(OBJDIR)/gwf3sfr8.o \
$(OBJDIR)/gwf3rch8.o \
$(OBJDIR)/gwf3wel8.o \
$(OBJDIR)/NumericalModel.o \
$(OBJDIR)/gwf3sto8.o \
$(OBJDIR)/gwf3uzf8.o \
$(OBJDIR)/gwf3ghb8.o \
$(OBJDIR)/gwf3maw8.o \
$(OBJDIR)/GhostNode.o \
$(OBJDIR)/gwt1cnc1.o \
$(OBJDIR)/NumericalExchange.o \
$(OBJDIR)/gwf3lak8.o \
$(OBJDIR)/gwt1fmi1.o \
$(OBJDIR)/gwf3api8.o \
$(OBJDIR)/Topology.o \
$(OBJDIR)/gwf3riv8.o \
$(OBJDIR)/gwt1adv1.o \
$(OBJDIR)/DisConnExchange.o \
$(OBJDIR)/gwt1mst1.o \
$(OBJDIR)/gwf3chd8.o \
$(OBJDIR)/gwt1ist1.o \
$(OBJDIR)/gwt1mvt1.o \
$(OBJDIR)/gwt1ssm1.o \
$(OBJDIR)/gwf3buy8.o \
$(OBJDIR)/gwt1apt1.o \
$(OBJDIR)/gwt1lkt1.o \
$(OBJDIR)/gwt1dsp.o \
$(OBJDIR)/GridSorting.o \
$(OBJDIR)/NumericalSolution.o \
$(OBJDIR)/gwt1mwt1.o \
$(OBJDIR)/gwf3.o \
$(OBJDIR)/gwt1sft1.o \
$(OBJDIR)/gwt1uzt1.o \
$(OBJDIR)/GridConnection.o \
$(OBJDIR)/SpatialModelConnection.o \
$(OBJDIR)/GwfGwfExchange.o \
$(OBJDIR)/GwfInterfaceModel.o \
$(OBJDIR)/gwt1.o \
$(OBJDIR)/GwfGwtExchange.o \
$(OBJDIR)/GwfGwfConnection.o \
$(OBJDIR)/SimulationCreate.o \
$(OBJDIR)/ConnectionBuilder.o \
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

