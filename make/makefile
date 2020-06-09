# makefile created on 2020-06-08 12:35:49.625919
# by pymake (version 1.1.0)
# using the gfortran fortran and gcc c/c++ compilers.

# Define the directories for the object and module files,
# the executable, and the executable name and path.
OBJDIR = ./obj_temp
BINDIR = .
PROGRAM = mf6

SOURCEDIR1=../src
SOURCEDIR2=../src/Exchange
SOURCEDIR3=../src/Solution
SOURCEDIR4=../src/Solution/SparseMatrixSolver
SOURCEDIR5=../src/Timing
SOURCEDIR6=../src/Utilities
SOURCEDIR7=../src/Utilities/TimeSeries
SOURCEDIR8=../src/Utilities/Memory
SOURCEDIR9=../src/Utilities/OutputControl
SOURCEDIR10=../src/Utilities/Observation
SOURCEDIR11=../src/Model
SOURCEDIR12=../src/Model/ModelUtilities
SOURCEDIR13=../src/Model/GroundWaterFlow
SOURCEDIR14=../src/Model/Geometry

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
${SOURCEDIR14} 

.SUFFIXES: .c .cpp .f .f90 .F90 .fpp .o

# Define the Fortran compile flags
FC = gfortran
FFLAGS = -O2 -ffpe-summary=overflow -ffpe-trap=overflow,zero,invalid -fbacktrace -D__APPLE__ -MMD -cpp 

# Define the C compile flags
CC = gcc
CFLAGS = -O2 -D_UF -MMD -cpp 

# Define the libraries
SYSLIBS = -lc 

OBJECTS = \
$(OBJDIR)/kind.o \
$(OBJDIR)/version.o \
$(OBJDIR)/Sparse.o \
$(OBJDIR)/compilerversion.o \
$(OBJDIR)/BaseGeometry.o \
$(OBJDIR)/ims8reordering.o \
$(OBJDIR)/Constants.o \
$(OBJDIR)/HashTable.o \
$(OBJDIR)/SimVariables.o \
$(OBJDIR)/OpenSpec.o \
$(OBJDIR)/Xt3dAlgorithm.o \
$(OBJDIR)/SmoothingFunctions.o \
$(OBJDIR)/defmacro.o \
$(OBJDIR)/genericutils.o \
$(OBJDIR)/ArrayHandlers.o \
$(OBJDIR)/Timer.o \
$(OBJDIR)/List.o \
$(OBJDIR)/mf6lists.o \
$(OBJDIR)/ObsOutput.o \
$(OBJDIR)/Message.o \
$(OBJDIR)/StringList.o \
$(OBJDIR)/TimeSeriesRecord.o \
$(OBJDIR)/Sim.o \
$(OBJDIR)/InputOutput.o \
$(OBJDIR)/PrintSaveManager.o \
$(OBJDIR)/BudgetFileReader.o \
$(OBJDIR)/sort.o \
$(OBJDIR)/ArrayReaders.o \
$(OBJDIR)/comarg.o \
$(OBJDIR)/Iunit.o \
$(OBJDIR)/ObsOutputList.o \
$(OBJDIR)/DisvGeom.o \
$(OBJDIR)/TableTerm.o \
$(OBJDIR)/BlockParser.o \
$(OBJDIR)/Budget.o \
$(OBJDIR)/RectangularGeometry.o \
$(OBJDIR)/Table.o \
$(OBJDIR)/Memory.o \
$(OBJDIR)/CircularGeometry.o \
$(OBJDIR)/TimeSeries.o \
$(OBJDIR)/TimeSeriesLink.o \
$(OBJDIR)/ListReader.o \
$(OBJDIR)/NameFile.o \
$(OBJDIR)/MemoryList.o \
$(OBJDIR)/MemoryManager.o \
$(OBJDIR)/TimeSeriesFileList.o \
$(OBJDIR)/Connections.o \
$(OBJDIR)/PackageMover.o \
$(OBJDIR)/BaseModel.o \
$(OBJDIR)/tdis.o \
$(OBJDIR)/BaseSolution.o \
$(OBJDIR)/UzfCellGroup.o \
$(OBJDIR)/ims8linear.o \
$(OBJDIR)/BaseExchange.o \
$(OBJDIR)/SolutionGroup.o \
$(OBJDIR)/Mover.o \
$(OBJDIR)/TimeSeriesManager.o \
$(OBJDIR)/DiscretizationBase.o \
$(OBJDIR)/gwf3dis8.o \
$(OBJDIR)/BudgetTerm.o \
$(OBJDIR)/gwf3disv8.o \
$(OBJDIR)/OutputControlData.o \
$(OBJDIR)/NumericalPackage.o \
$(OBJDIR)/TimeArray.o \
$(OBJDIR)/gwf3disu8.o \
$(OBJDIR)/gwf3ic8.o \
$(OBJDIR)/TimeArraySeries.o \
$(OBJDIR)/Observe.o \
$(OBJDIR)/Xt3dInterface.o \
$(OBJDIR)/gwf3sto8.o \
$(OBJDIR)/ObsUtility.o \
$(OBJDIR)/BudgetObject.o \
$(OBJDIR)/gwf3npf8.o \
$(OBJDIR)/gwf3mvr8.o \
$(OBJDIR)/OutputControl.o \
$(OBJDIR)/TimeArraySeriesLink.o \
$(OBJDIR)/ObsContainer.o \
$(OBJDIR)/gwf3oc8.o \
$(OBJDIR)/gwf3hfb8.o \
$(OBJDIR)/TimeArraySeriesManager.o \
$(OBJDIR)/Obs3.o \
$(OBJDIR)/gwf3obs8.o \
$(OBJDIR)/gwf3csub8.o \
$(OBJDIR)/BoundaryPackage.o \
$(OBJDIR)/gwf3lak8.o \
$(OBJDIR)/NumericalModel.o \
$(OBJDIR)/gwf3wel8.o \
$(OBJDIR)/gwf3sfr8.o \
$(OBJDIR)/gwf3evt8.o \
$(OBJDIR)/gwf3chd8.o \
$(OBJDIR)/gwf3maw8.o \
$(OBJDIR)/NumericalExchange.o \
$(OBJDIR)/gwf3uzf8.o \
$(OBJDIR)/gwf3rch8.o \
$(OBJDIR)/gwf3riv8.o \
$(OBJDIR)/gwf3drn8.o \
$(OBJDIR)/GhostNode.o \
$(OBJDIR)/gwf3ghb8.o \
$(OBJDIR)/NumericalSolution.o \
$(OBJDIR)/gwf3.o \
$(OBJDIR)/GwfGwfExchange.o \
$(OBJDIR)/SimulationCreate.o \
$(OBJDIR)/mf6core.o \
$(OBJDIR)/mf6.o 

# Define task functions

# Create the bin directory and compile and link the program
all: makebin | $(PROGRAM)

# Make the bin directory for the executable
makebin :
	mkdir -p $(BINDIR)

# Define the objects that make up the program
$(PROGRAM) : $(OBJECTS)
	-$(FC) $(FFLAGS) -o $@ $(OBJECTS) $(SYSLIBS) -I$(OBJDIR) -J$(OBJDIR) 

$(OBJDIR)/%.o : %.f
	@mkdir -p $(@D)
	$(FC) $(FFLAGS) -c $< -o $@ -I$(OBJDIR) -J$(OBJDIR) 
	cat ./obj_temp/$*.d >> Dependencies
	rm -f $*.d

$(OBJDIR)/%.o : %.f90
	@mkdir -p $(@D)
	$(FC) $(FFLAGS) -c $< -o $@ -I$(OBJDIR) -J$(OBJDIR) 
	cat ./obj_temp/$*.d >> Dependencies
	rm -f $*.d

$(OBJDIR)/%.o : %.F90
	@mkdir -p $(@D)
	$(FC) $(FFLAGS) -c $< -o $@ -I$(OBJDIR) -J$(OBJDIR) 
	cat ./obj_temp/$*.d >> Dependencies
	rm -f $*.d

$(OBJDIR)/%.o : %.fpp
	@mkdir -p $(@D)
	$(FC) $(FFLAGS) -c $< -o $@ -I$(OBJDIR) -J$(OBJDIR) 
	cat ./obj_temp/$*.d >> Dependencies
	rm -f $*.d

$(OBJDIR)/%.o : %.c
	@mkdir -p $(@D)
	$(CC) $(CFLAGS) -c $< -o $@
	cat ./obj_temp/$*.d >> Dependencies
	rm -f $*.d

$(OBJDIR)/%.o : %.cpp
	@mkdir -p $(@D)
	$(CC) $(CFLAGS) -c $< -o $@
	cat ./obj_temp/$*.d >> Dependencies
	rm -f $*.d

# Clean the object and module files and the executable
.PHONY : clean
clean : 
	-rm -r Dependencies
	-rm -rf $(OBJDIR)
	-rm -rf $(PROGRAM)

# Clean the object and module files
.PHONY : cleanobj
cleanobj : 
	-rm -rf $(OBJDIR)

# Touch dependencies
Dependencies : 
	touch Dependencies

