# makefile created by pymake (version 1.2.1) for the 'mf6' executable.


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
SOURCEDIR12=../src/Utilities/Libraries/blas
SOURCEDIR13=../src/Utilities/Libraries/sparskit2
SOURCEDIR14=../src/Utilities/Libraries/daglib
SOURCEDIR15=../src/Utilities/Libraries/sparsekit
SOURCEDIR16=../src/Utilities/Observation
SOURCEDIR17=../src/Model
SOURCEDIR18=../src/Model/Connection
SOURCEDIR19=../src/Model/GroundWaterTransport
SOURCEDIR20=../src/Model/ModelUtilities
SOURCEDIR21=../src/Model/GroundWaterFlow
SOURCEDIR22=../src/Model/Geometry

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
${SOURCEDIR22} 

.SUFFIXES: .F90 .f90 .o

OBJECTS = \
$(OBJDIR)/OpenSpec.o \
$(OBJDIR)/ilut.o \
$(OBJDIR)/dag_module.o \
$(OBJDIR)/sparsekit.o \
$(OBJDIR)/kind.o \
$(OBJDIR)/rcm.o \
$(OBJDIR)/ims8reordering.o \
$(OBJDIR)/GwfNpfGridData.o \
$(OBJDIR)/Constants.o \
$(OBJDIR)/GwfStorageUtils.o \
$(OBJDIR)/SimVariables.o \
$(OBJDIR)/defmacro.o \
$(OBJDIR)/GwfNpfOptions.o \
$(OBJDIR)/Sparse.o \
$(OBJDIR)/PackageBudget.o \
$(OBJDIR)/BaseGeometry.o \
$(OBJDIR)/HashTable.o \
$(OBJDIR)/compilerversion.o \
$(OBJDIR)/Xt3dAlgorithm.o \
$(OBJDIR)/blas1_d.o \
$(OBJDIR)/SfrCrossSectionUtils.o \
$(OBJDIR)/CsrUtils.o \
$(OBJDIR)/SmoothingFunctions.o \
$(OBJDIR)/genericutils.o \
$(OBJDIR)/version.o \
$(OBJDIR)/ArrayHandlers.o \
$(OBJDIR)/Timer.o \
$(OBJDIR)/List.o \
$(OBJDIR)/Message.o \
$(OBJDIR)/Sim.o \
$(OBJDIR)/sort.o \
$(OBJDIR)/MemoryHelper.o \
$(OBJDIR)/TimeSeriesRecord.o \
$(OBJDIR)/mf6lists.o \
$(OBJDIR)/VectorInt.o \
$(OBJDIR)/InputOutput.o \
$(OBJDIR)/ObsOutput.o \
$(OBJDIR)/TableTerm.o \
$(OBJDIR)/StringList.o \
$(OBJDIR)/ArrayReaders.o \
$(OBJDIR)/comarg.o \
$(OBJDIR)/PrintSaveManager.o \
$(OBJDIR)/CircularGeometry.o \
$(OBJDIR)/RectangularGeometry.o \
$(OBJDIR)/HeadFileReader.o \
$(OBJDIR)/Iunit.o \
$(OBJDIR)/Budget.o \
$(OBJDIR)/DisvGeom.o \
$(OBJDIR)/BudgetFileReader.o \
$(OBJDIR)/Table.o \
$(OBJDIR)/ListReader.o \
$(OBJDIR)/BlockParser.o \
$(OBJDIR)/ObsOutputList.o \
$(OBJDIR)/SfrCrossSectionManager.o \
$(OBJDIR)/TimeSeries.o \
$(OBJDIR)/TimeSeriesFileList.o \
$(OBJDIR)/Memory.o \
$(OBJDIR)/NameFile.o \
$(OBJDIR)/MemoryList.o \
$(OBJDIR)/ims8base.o \
$(OBJDIR)/TimeSeriesLink.o \
$(OBJDIR)/MemoryManager.o \
$(OBJDIR)/ims8linear.o \
$(OBJDIR)/Connections.o \
$(OBJDIR)/BaseModel.o \
$(OBJDIR)/PackageMover.o \
$(OBJDIR)/MemorySetHandler.o \
$(OBJDIR)/Mover.o \
$(OBJDIR)/ats.o \
$(OBJDIR)/tdis.o \
$(OBJDIR)/UzfCellGroup.o \
$(OBJDIR)/BaseExchange.o \
$(OBJDIR)/TimeSeriesManager.o \
$(OBJDIR)/DiscretizationBase.o \
$(OBJDIR)/Xt3dInterface.o \
$(OBJDIR)/gwf3dis8.o \
$(OBJDIR)/gwf3disv8.o \
$(OBJDIR)/BudgetTerm.o \
$(OBJDIR)/BaseSolution.o \
$(OBJDIR)/gwf3disu8.o \
$(OBJDIR)/TimeArray.o \
$(OBJDIR)/NumericalPackage.o \
$(OBJDIR)/BudgetObject.o \
$(OBJDIR)/gwf3ic8.o \
$(OBJDIR)/gwt1ic1.o \
$(OBJDIR)/gwf3mvr8.o \
$(OBJDIR)/OutputControlData.o \
$(OBJDIR)/Observe.o \
$(OBJDIR)/TimeArraySeries.o \
$(OBJDIR)/gwf3hfb8.o \
$(OBJDIR)/gwf3tvbase8.o \
$(OBJDIR)/SolutionGroup.o \
$(OBJDIR)/ObsUtility.o \
$(OBJDIR)/OutputControl.o \
$(OBJDIR)/ObsContainer.o \
$(OBJDIR)/TimeArraySeriesLink.o \
$(OBJDIR)/gwf3oc8.o \
$(OBJDIR)/gwf3tvk8.o \
$(OBJDIR)/gwf3tvs8.o \
$(OBJDIR)/gwt1oc1.o \
$(OBJDIR)/gwf3sto8.o \
$(OBJDIR)/Obs3.o \
$(OBJDIR)/gwf3obs8.o \
$(OBJDIR)/TimeArraySeriesManager.o \
$(OBJDIR)/gwf3npf8.o \
$(OBJDIR)/gwt1obs1.o \
$(OBJDIR)/gwf3csub8.o \
$(OBJDIR)/GwtSpc.o \
$(OBJDIR)/BoundaryPackage.o \
$(OBJDIR)/gwt1src1.o \
$(OBJDIR)/gwf3ghb8.o \
$(OBJDIR)/gwf3uzf8.o \
$(OBJDIR)/gwf3maw8.o \
$(OBJDIR)/gwt1cnc1.o \
$(OBJDIR)/gwf3evt8.o \
$(OBJDIR)/gwf3rch8.o \
$(OBJDIR)/gwf3wel8.o \
$(OBJDIR)/gwt1fmi1.o \
$(OBJDIR)/gwt1adv1.o \
$(OBJDIR)/gwt1mst1.o \
$(OBJDIR)/NumericalModel.o \
$(OBJDIR)/gwf3api8.o \
$(OBJDIR)/gwt1ist1.o \
$(OBJDIR)/GhostNode.o \
$(OBJDIR)/gwf3drn8.o \
$(OBJDIR)/gwf3chd8.o \
$(OBJDIR)/gwt1mvt1.o \
$(OBJDIR)/gwf3lak8.o \
$(OBJDIR)/gwf3riv8.o \
$(OBJDIR)/gwt1ssm1.o \
$(OBJDIR)/gwf3sfr8.o \
$(OBJDIR)/gwt1apt1.o \
$(OBJDIR)/gwt1dsp.o \
$(OBJDIR)/gwf3buy8.o \
$(OBJDIR)/gwt1lkt1.o \
$(OBJDIR)/Topology.o \
$(OBJDIR)/gwf3.o \
$(OBJDIR)/NumericalExchange.o \
$(OBJDIR)/gwt1sft1.o \
$(OBJDIR)/DisConnExchange.o \
$(OBJDIR)/gwt1uzt1.o \
$(OBJDIR)/gwt1mwt1.o \
$(OBJDIR)/GridSorting.o \
$(OBJDIR)/GwfGwfExchange.o \
$(OBJDIR)/NumericalSolution.o \
$(OBJDIR)/GridConnection.o \
$(OBJDIR)/gwt1.o \
$(OBJDIR)/GwfGwtExchange.o \
$(OBJDIR)/SpatialModelConnection.o \
$(OBJDIR)/SimulationCreate.o \
$(OBJDIR)/GwfInterfaceModel.o \
$(OBJDIR)/GwfGwfConnection.o \
$(OBJDIR)/ConnectionBuilder.o \
$(OBJDIR)/mf6core.o \
$(OBJDIR)/mf6.o

# Define the objects that make up the program
$(PROGRAM) : $(OBJECTS)
	-$(FC) $(OPTLEVEL) -o $@ $(OBJECTS) $(LDFLAGS)

$(OBJDIR)/%.o : %.F90
	@mkdir -p $(@D)
	$(FC) $(OPTLEVEL) $(FFLAGS) -c $< -o $@ $(INCSWITCH) $(MODSWITCH)

$(OBJDIR)/%.o : %.f90
	@mkdir -p $(@D)
	$(FC) $(OPTLEVEL) $(FFLAGS) -c $< -o $@ $(INCSWITCH) $(MODSWITCH)

