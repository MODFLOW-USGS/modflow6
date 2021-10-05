# makefile created on 2021-07-30 14:34:49.615882
# by pymake (version 1.2.1) for the 'mf6' executable 
# using the 'gfortran' fortran compiler(s).


include ./makedefaults

# Define the source file directories
SOURCEDIR1=../src
SOURCEDIR2=../src/Exchange
SOURCEDIR3=../src/Model
SOURCEDIR4=../src/Model/Geometry
SOURCEDIR5=../src/Model/GroundWaterFlow
SOURCEDIR6=../src/Model/GroundWaterTransport
SOURCEDIR7=../src/Model/ModelUtilities
SOURCEDIR8=../src/Solution
SOURCEDIR9=../src/Solution/SparseMatrixSolver
SOURCEDIR10=../src/Timing
SOURCEDIR11=../src/Utilities
SOURCEDIR12=../src/Utilities/Memory
SOURCEDIR13=../src/Utilities/Observation
SOURCEDIR14=../src/Utilities/OutputControl
SOURCEDIR15=../src/Utilities/TimeSeries

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
${SOURCEDIR15} 

.SUFFIXES: .f90 .o

OBJECTS = \
$(OBJDIR)/kind.o \
$(OBJDIR)/Sparse.o \
$(OBJDIR)/ims8reordering.o \
$(OBJDIR)/Constants.o \
$(OBJDIR)/GwfNpfGridData.o \
$(OBJDIR)/Xt3dAlgorithm.o \
$(OBJDIR)/compilerversion.o \
$(OBJDIR)/SmoothingFunctions.o \
$(OBJDIR)/PackageBudget.o \
$(OBJDIR)/GwfNpfOptions.o \
$(OBJDIR)/OpenSpec.o \
$(OBJDIR)/HashTable.o \
$(OBJDIR)/BaseGeometry.o \
$(OBJDIR)/defmacro.o \
$(OBJDIR)/SimVariables.o \
$(OBJDIR)/GwfStorageUtils.o \
$(OBJDIR)/genericutils.o \
$(OBJDIR)/List.o \
$(OBJDIR)/ObsOutput.o \
$(OBJDIR)/TimeSeriesRecord.o \
$(OBJDIR)/Timer.o \
$(OBJDIR)/mf6lists.o \
$(OBJDIR)/version.o \
$(OBJDIR)/ArrayHandlers.o \
$(OBJDIR)/ims8sparsekit.o \
$(OBJDIR)/StringList.o \
$(OBJDIR)/Message.o \
$(OBJDIR)/Sim.o \
$(OBJDIR)/InputOutput.o \
$(OBJDIR)/Budget.o \
$(OBJDIR)/Iunit.o \
$(OBJDIR)/ArrayReaders.o \
$(OBJDIR)/sort.o \
$(OBJDIR)/MemoryHelper.o \
$(OBJDIR)/BlockParser.o \
$(OBJDIR)/ims8base.o \
$(OBJDIR)/BudgetFileReader.o \
$(OBJDIR)/PrintSaveManager.o \
$(OBJDIR)/RectangularGeometry.o \
$(OBJDIR)/HeadFileReader.o \
$(OBJDIR)/NameFile.o \
$(OBJDIR)/ObsOutputList.o \
$(OBJDIR)/comarg.o \
$(OBJDIR)/TimeSeries.o \
$(OBJDIR)/CircularGeometry.o \
$(OBJDIR)/DisvGeom.o \
$(OBJDIR)/TimeSeriesLink.o \
$(OBJDIR)/TableTerm.o \
$(OBJDIR)/TimeSeriesFileList.o \
$(OBJDIR)/Table.o \
$(OBJDIR)/Memory.o \
$(OBJDIR)/ListReader.o \
$(OBJDIR)/MemoryList.o \
$(OBJDIR)/MemoryManager.o \
$(OBJDIR)/ims8linear.o \
$(OBJDIR)/PackageMover.o \
$(OBJDIR)/Mover.o \
$(OBJDIR)/Connections.o \
$(OBJDIR)/BaseModel.o \
$(OBJDIR)/MemorySetHandler.o \
$(OBJDIR)/ats.o \
$(OBJDIR)/tdis.o \
$(OBJDIR)/TimeSeriesManager.o \
$(OBJDIR)/BaseExchange.o \
$(OBJDIR)/UzfCellGroup.o \
$(OBJDIR)/DiscretizationBase.o \
$(OBJDIR)/OutputControlData.o \
$(OBJDIR)/gwf3disu8.o \
$(OBJDIR)/Observe.o \
$(OBJDIR)/BudgetTerm.o \
$(OBJDIR)/Xt3dInterface.o \
$(OBJDIR)/NumericalPackage.o \
$(OBJDIR)/ObsUtility.o \
$(OBJDIR)/BaseSolution.o \
$(OBJDIR)/gwf3dis8.o \
$(OBJDIR)/gwf3disv8.o \
$(OBJDIR)/gwf3hfb8.o \
$(OBJDIR)/SolutionGroup.o \
$(OBJDIR)/gwf3sto8.o \
$(OBJDIR)/TimeArray.o \
$(OBJDIR)/OutputControl.o \
$(OBJDIR)/ObsContainer.o \
$(OBJDIR)/BudgetObject.o \
$(OBJDIR)/gwf3ic8.o \
$(OBJDIR)/gwt1oc1.o \
$(OBJDIR)/TimeArraySeries.o \
$(OBJDIR)/Obs3.o \
$(OBJDIR)/gwf3oc8.o \
$(OBJDIR)/gwf3mvr8.o \
$(OBJDIR)/TimeArraySeriesLink.o \
$(OBJDIR)/gwf3npf8.o \
$(OBJDIR)/gwt1ic1.o \
$(OBJDIR)/TimeArraySeriesManager.o \
$(OBJDIR)/gwf3obs8.o \
$(OBJDIR)/gwf3csub8.o \
$(OBJDIR)/gwt1obs1.o \
$(OBJDIR)/BoundaryPackage.o \
$(OBJDIR)/gwf3riv8.o \
$(OBJDIR)/gwf3sfr8.o \
$(OBJDIR)/gwt1cnc1.o \
$(OBJDIR)/gwf3evt8.o \
$(OBJDIR)/gwf3ghb8.o \
$(OBJDIR)/NumericalModel.o \
$(OBJDIR)/gwf3maw8.o \
$(OBJDIR)/gwf3rch8.o \
$(OBJDIR)/gwt1src1.o \
$(OBJDIR)/GhostNode.o \
$(OBJDIR)/gwf3api8.o \
$(OBJDIR)/gwf3lak8.o \
$(OBJDIR)/gwf3buy8.o \
$(OBJDIR)/gwf3uzf8.o \
$(OBJDIR)/gwf3chd8.o \
$(OBJDIR)/gwf3wel8.o \
$(OBJDIR)/gwt1fmi1.o \
$(OBJDIR)/gwf3drn8.o \
$(OBJDIR)/gwt1ssm1.o \
$(OBJDIR)/gwt1mst1.o \
$(OBJDIR)/NumericalExchange.o \
$(OBJDIR)/gwt1mvt1.o \
$(OBJDIR)/NumericalSolution.o \
$(OBJDIR)/DisConnExchange.o \
$(OBJDIR)/gwt1ist1.o \
$(OBJDIR)/gwt1adv1.o \
$(OBJDIR)/gwt1apt1.o \
$(OBJDIR)/gwt1mwt1.o \
$(OBJDIR)/gwt1sft1.o \
$(OBJDIR)/gwt1dsp.o \
$(OBJDIR)/gwf3.o \
$(OBJDIR)/gwt1uzt1.o \
$(OBJDIR)/gwt1lkt1.o \
$(OBJDIR)/GwfGwfExchange.o \
$(OBJDIR)/gwt1.o \
$(OBJDIR)/GwfGwtExchange.o \
$(OBJDIR)/SimulationCreate.o \
$(OBJDIR)/mf6core.o \
$(OBJDIR)/mf6.o

# Define the objects that make up the program
$(PROGRAM) : $(OBJECTS)
	-$(FC) $(OPTLEVEL) -o $@ $(OBJECTS) $(LDFLAGS)

$(OBJDIR)/%.o : %.f90
	@mkdir -p $(@D)
	$(FC) $(OPTLEVEL) $(FFLAGS) -c $< -o $@ $(INCSWITCH) $(MODSWITCH)

