# makefile created on 2021-11-16 12:14:02.967101
# by pymake (version 1.2.1) for the 'mf6' executable 
# using the 'gfortran' fortran compiler(s).


include ./makedefaults

# Define the source file directories
SOURCEDIR1=../src
SOURCEDIR2=../src/Model
SOURCEDIR3=../src/Model/GroundWaterTransport
SOURCEDIR4=../src/Model/GroundWaterFlow
SOURCEDIR5=../src/Model/Geometry
SOURCEDIR6=../src/Model/Connection
SOURCEDIR7=../src/Model/ModelUtilities
SOURCEDIR8=../src/Utilities
SOURCEDIR9=../src/Utilities/Observation
SOURCEDIR10=../src/Utilities/TimeSeries
SOURCEDIR11=../src/Utilities/OutputControl
SOURCEDIR12=../src/Utilities/Memory
SOURCEDIR13=../src/Exchange
SOURCEDIR14=../src/Solution
SOURCEDIR15=../src/Solution/SparseMatrixSolver
SOURCEDIR16=../src/Timing

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
${SOURCEDIR16} 

.SUFFIXES: .f90 .F90 .o

OBJECTS = \
$(OBJDIR)/kind.o \
$(OBJDIR)/OpenSpec.o \
$(OBJDIR)/CsrUtils.o \
$(OBJDIR)/compilerversion.o \
$(OBJDIR)/ims8reordering.o \
$(OBJDIR)/GwfNpfGridData.o \
$(OBJDIR)/BaseGeometry.o \
$(OBJDIR)/Sparse.o \
$(OBJDIR)/Constants.o \
$(OBJDIR)/HashTable.o \
$(OBJDIR)/defmacro.o \
$(OBJDIR)/PackageBudget.o \
$(OBJDIR)/GwfStorageUtils.o \
$(OBJDIR)/SmoothingFunctions.o \
$(OBJDIR)/SimVariables.o \
$(OBJDIR)/GwfNpfOptions.o \
$(OBJDIR)/SfrCrossSectionUtils.o \
$(OBJDIR)/Xt3dAlgorithm.o \
$(OBJDIR)/genericutils.o \
$(OBJDIR)/ims8sparsekit.o \
$(OBJDIR)/ArrayHandlers.o \
$(OBJDIR)/List.o \
$(OBJDIR)/Timer.o \
$(OBJDIR)/version.o \
$(OBJDIR)/mf6lists.o \
$(OBJDIR)/StringList.o \
$(OBJDIR)/Message.o \
$(OBJDIR)/TimeSeriesRecord.o \
$(OBJDIR)/ObsOutput.o \
$(OBJDIR)/Sim.o \
$(OBJDIR)/sort.o \
$(OBJDIR)/Budget.o \
$(OBJDIR)/Iunit.o \
$(OBJDIR)/MemoryHelper.o \
$(OBJDIR)/VectorInt.o \
$(OBJDIR)/InputOutput.o \
$(OBJDIR)/ArrayReaders.o \
$(OBJDIR)/PrintSaveManager.o \
$(OBJDIR)/comarg.o \
$(OBJDIR)/BudgetFileReader.o \
$(OBJDIR)/TableTerm.o \
$(OBJDIR)/DisvGeom.o \
$(OBJDIR)/HeadFileReader.o \
$(OBJDIR)/BlockParser.o \
$(OBJDIR)/CircularGeometry.o \
$(OBJDIR)/ObsOutputList.o \
$(OBJDIR)/NameFile.o \
$(OBJDIR)/RectangularGeometry.o \
$(OBJDIR)/TimeSeries.o \
$(OBJDIR)/Table.o \
$(OBJDIR)/TimeSeriesFileList.o \
$(OBJDIR)/ims8base.o \
$(OBJDIR)/TimeSeriesLink.o \
$(OBJDIR)/ListReader.o \
$(OBJDIR)/SfrCrossSectionManager.o \
$(OBJDIR)/Memory.o \
$(OBJDIR)/MemoryList.o \
$(OBJDIR)/MemoryManager.o \
$(OBJDIR)/PackageMover.o \
$(OBJDIR)/MemorySetHandler.o \
$(OBJDIR)/BaseModel.o \
$(OBJDIR)/ats.o \
$(OBJDIR)/ims8linear.o \
$(OBJDIR)/Connections.o \
$(OBJDIR)/tdis.o \
$(OBJDIR)/Mover.o \
$(OBJDIR)/BaseExchange.o \
$(OBJDIR)/TimeSeriesManager.o \
$(OBJDIR)/UzfCellGroup.o \
$(OBJDIR)/BaseSolution.o \
$(OBJDIR)/SolutionGroup.o \
$(OBJDIR)/DiscretizationBase.o \
$(OBJDIR)/gwf3dis8.o \
$(OBJDIR)/NumericalPackage.o \
$(OBJDIR)/TimeArray.o \
$(OBJDIR)/Observe.o \
$(OBJDIR)/gwf3ic8.o \
$(OBJDIR)/gwf3disu8.o \
$(OBJDIR)/OutputControlData.o \
$(OBJDIR)/ObsUtility.o \
$(OBJDIR)/BudgetTerm.o \
$(OBJDIR)/gwt1ic1.o \
$(OBJDIR)/gwf3disv8.o \
$(OBJDIR)/Xt3dInterface.o \
$(OBJDIR)/TimeArraySeries.o \
$(OBJDIR)/OutputControl.o \
$(OBJDIR)/gwf3tvbase8.o \
$(OBJDIR)/gwf3oc8.o \
$(OBJDIR)/ObsContainer.o \
$(OBJDIR)/gwt1oc1.o \
$(OBJDIR)/TimeArraySeriesLink.o \
$(OBJDIR)/TimeArraySeriesManager.o \
$(OBJDIR)/gwf3hfb8.o \
$(OBJDIR)/BudgetObject.o \
$(OBJDIR)/gwf3tvk8.o \
$(OBJDIR)/gwf3tvs8.o \
$(OBJDIR)/Obs3.o \
$(OBJDIR)/gwf3csub8.o \
$(OBJDIR)/gwf3mvr8.o \
$(OBJDIR)/GwtSpc.o \
$(OBJDIR)/gwf3npf8.o \
$(OBJDIR)/BoundaryPackage.o \
$(OBJDIR)/gwf3obs8.o \
$(OBJDIR)/gwf3evt8.o \
$(OBJDIR)/gwf3drn8.o \
$(OBJDIR)/gwf3uzf8.o \
$(OBJDIR)/gwf3lak8.o \
$(OBJDIR)/gwt1obs1.o \
$(OBJDIR)/gwf3wel8.o \
$(OBJDIR)/NumericalModel.o \
$(OBJDIR)/NumericalExchange.o \
$(OBJDIR)/gwt1cnc1.o \
$(OBJDIR)/gwf3sto8.o \
$(OBJDIR)/gwf3riv8.o \
$(OBJDIR)/gwf3api8.o \
$(OBJDIR)/gwf3chd8.o \
$(OBJDIR)/gwt1fmi1.o \
$(OBJDIR)/gwf3ghb8.o \
$(OBJDIR)/gwf3sfr8.o \
$(OBJDIR)/Topology.o \
$(OBJDIR)/gwt1src1.o \
$(OBJDIR)/gwf3maw8.o \
$(OBJDIR)/gwf3rch8.o \
$(OBJDIR)/NumericalSolution.o \
$(OBJDIR)/gwt1dsp.o \
$(OBJDIR)/gwt1mvt1.o \
$(OBJDIR)/gwt1apt1.o \
$(OBJDIR)/GridSorting.o \
$(OBJDIR)/gwt1ssm1.o \
$(OBJDIR)/GhostNode.o \
$(OBJDIR)/gwt1adv1.o \
$(OBJDIR)/gwt1mst1.o \
$(OBJDIR)/DisConnExchange.o \
$(OBJDIR)/GridConnection.o \
$(OBJDIR)/gwt1sft1.o \
$(OBJDIR)/gwt1uzt1.o \
$(OBJDIR)/gwt1mwt1.o \
$(OBJDIR)/gwt1lkt1.o \
$(OBJDIR)/gwt1ist1.o \
$(OBJDIR)/gwf3buy8.o \
$(OBJDIR)/SpatialModelConnection.o \
$(OBJDIR)/gwf3.o \
$(OBJDIR)/GwfGwfExchange.o \
$(OBJDIR)/gwt1.o \
$(OBJDIR)/GwfGwtExchange.o \
$(OBJDIR)/GwfInterfaceModel.o \
$(OBJDIR)/SimulationCreate.o \
$(OBJDIR)/GwfGwfConnection.o \
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

