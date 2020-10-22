# makefile created on 2020-10-22 09:26:36.744206
# by pymake (version 1.2.0) for the 'mf6' executable 
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

.SUFFIXES: .f90 .fpp .o

OBJECTS = \
$(OBJDIR)/kind.o \
$(OBJDIR)/Sparse.o \
$(OBJDIR)/BaseGeometry.o \
$(OBJDIR)/OpenSpec.o \
$(OBJDIR)/Constants.o \
$(OBJDIR)/HashTable.o \
$(OBJDIR)/defmacro.o \
$(OBJDIR)/SimVariables.o \
$(OBJDIR)/compilerversion.o \
$(OBJDIR)/SmoothingFunctions.o \
$(OBJDIR)/ims8reordering.o \
$(OBJDIR)/PackageBudget.o \
$(OBJDIR)/genericutils.o \
$(OBJDIR)/Xt3dAlgorithm.o \
$(OBJDIR)/List.o \
$(OBJDIR)/ArrayHandlers.o \
$(OBJDIR)/ObsOutput.o \
$(OBJDIR)/TimeSeriesRecord.o \
$(OBJDIR)/Timer.o \
$(OBJDIR)/mf6lists.o \
$(OBJDIR)/StringList.o \
$(OBJDIR)/version.o \
$(OBJDIR)/Message.o \
$(OBJDIR)/Sim.o \
$(OBJDIR)/sort.o \
$(OBJDIR)/Iunit.o \
$(OBJDIR)/Budget.o \
$(OBJDIR)/MemoryHelper.o \
$(OBJDIR)/InputOutput.o \
$(OBJDIR)/comarg.o \
$(OBJDIR)/CircularGeometry.o \
$(OBJDIR)/ArrayReaders.o \
$(OBJDIR)/BlockParser.o \
$(OBJDIR)/TableTerm.o \
$(OBJDIR)/PrintSaveManager.o \
$(OBJDIR)/BudgetFileReader.o \
$(OBJDIR)/DisvGeom.o \
$(OBJDIR)/ObsOutputList.o \
$(OBJDIR)/HeadFileReader.o \
$(OBJDIR)/Table.o \
$(OBJDIR)/TimeSeries.o \
$(OBJDIR)/RectangularGeometry.o \
$(OBJDIR)/TimeSeriesLink.o \
$(OBJDIR)/Memory.o \
$(OBJDIR)/NameFile.o \
$(OBJDIR)/MemoryList.o \
$(OBJDIR)/TimeSeriesFileList.o \
$(OBJDIR)/ListReader.o \
$(OBJDIR)/MemoryManager.o \
$(OBJDIR)/tdis.o \
$(OBJDIR)/MemorySetHandler.o \
$(OBJDIR)/UzfCellGroup.o \
$(OBJDIR)/PackageMover.o \
$(OBJDIR)/Connections.o \
$(OBJDIR)/ims8linear.o \
$(OBJDIR)/BaseModel.o \
$(OBJDIR)/TimeSeriesManager.o \
$(OBJDIR)/Mover.o \
$(OBJDIR)/DiscretizationBase.o \
$(OBJDIR)/BaseSolution.o \
$(OBJDIR)/gwf3disu8.o \
$(OBJDIR)/gwf3dis8.o \
$(OBJDIR)/TimeArray.o \
$(OBJDIR)/OutputControlData.o \
$(OBJDIR)/BaseExchange.o \
$(OBJDIR)/NumericalPackage.o \
$(OBJDIR)/gwf3disv8.o \
$(OBJDIR)/BudgetTerm.o \
$(OBJDIR)/Observe.o \
$(OBJDIR)/Xt3dInterface.o \
$(OBJDIR)/gwf3sto8.o \
$(OBJDIR)/SolutionGroup.o \
$(OBJDIR)/TimeArraySeries.o \
$(OBJDIR)/ObsContainer.o \
$(OBJDIR)/ObsUtility.o \
$(OBJDIR)/gwf3hfb8.o \
$(OBJDIR)/TimeArraySeriesLink.o \
$(OBJDIR)/BudgetObject.o \
$(OBJDIR)/gwf3ic8.o \
$(OBJDIR)/OutputControl.o \
$(OBJDIR)/Obs3.o \
$(OBJDIR)/gwf3mvr8.o \
$(OBJDIR)/gwt1ic1.o \
$(OBJDIR)/gwf3npf8.o \
$(OBJDIR)/TimeArraySeriesManager.o \
$(OBJDIR)/gwf3obs8.o \
$(OBJDIR)/gwf3oc8.o \
$(OBJDIR)/gwt1oc1.o \
$(OBJDIR)/gwt1obs1.o \
$(OBJDIR)/gwf3csub8.o \
$(OBJDIR)/BoundaryPackage.o \
$(OBJDIR)/gwf3uzf8.o \
$(OBJDIR)/NumericalModel.o \
$(OBJDIR)/gwf3evt8.o \
$(OBJDIR)/gwf3riv8.o \
$(OBJDIR)/gwt1fmi1.o \
$(OBJDIR)/gwf3chd8.o \
$(OBJDIR)/gwf3lak8.o \
$(OBJDIR)/gwf3drn8.o \
$(OBJDIR)/gwt1cnc1.o \
$(OBJDIR)/gwt1mst1.o \
$(OBJDIR)/gwt1src1.o \
$(OBJDIR)/gwt1mvt1.o \
$(OBJDIR)/NumericalExchange.o \
$(OBJDIR)/gwt1ssm1.o \
$(OBJDIR)/gwf3wel8.o \
$(OBJDIR)/gwf3sfr8.o \
$(OBJDIR)/gwf3rch8.o \
$(OBJDIR)/gwf3maw8.o \
$(OBJDIR)/GhostNode.o \
$(OBJDIR)/gwt1adv1.o \
$(OBJDIR)/gwt1dsp.o \
$(OBJDIR)/gwf3ghb8.o \
$(OBJDIR)/NumericalSolution.o \
$(OBJDIR)/gwt1ist1.o \
$(OBJDIR)/gwt1apt1.o \
$(OBJDIR)/gwt1sft1.o \
$(OBJDIR)/gwf3buy8.o \
$(OBJDIR)/gwt1uzt1.o \
$(OBJDIR)/gwt1mwt1.o \
$(OBJDIR)/gwt1lkt1.o \
$(OBJDIR)/gwf3.o \
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

$(OBJDIR)/%.o : %.fpp
	@mkdir -p $(@D)
	$(FC) $(OPTLEVEL) $(FFLAGS) -c $< -o $@ $(INCSWITCH) $(MODSWITCH)

