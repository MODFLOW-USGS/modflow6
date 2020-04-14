# makefile created on 2020-03-23 17:29:37.182957
# by pymake (version 1.1.0)
# using the gfortran fortran and gcc c/c++ compilers.

# Define the directories for the object and module files,
# the executable, and the executable name and path.
OBJDIR = ./obj_temp
BINDIR = ../bin
PROGRAM = ../bin/mf6

SOURCEDIR1=../src
SOURCEDIR2=../src/Utilities
SOURCEDIR3=../src/Utilities/OutputControl
SOURCEDIR4=../src/Utilities/TimeSeries
SOURCEDIR5=../src/Utilities/Memory
SOURCEDIR6=../src/Utilities/Observation
SOURCEDIR7=../src/Timing
SOURCEDIR8=../src/Solution
SOURCEDIR9=../src/Solution/SparseMatrixSolver
SOURCEDIR10=../src/Model
SOURCEDIR11=../src/Model/GroundWaterFlow
SOURCEDIR12=../src/Model/Geometry
SOURCEDIR13=../src/Model/ModelUtilities
SOURCEDIR14=../src/Exchange

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
FFLAGS = -O2 -ffpe-summary=overflow -ffpe-trap=overflow,zero,invalid -fbacktrace -fno-backtrace -march=native -mtune=native -fwhole-program -fstack-arrays -flto -funroll-loops -floop-nest-optimize -fprofile-abs-path -fopenmp -ftree-vectorize 

# Define the C compile flags
CC = gcc
CFLAGS = -O2 -D_UF 

# Define the libraries
SYSLIBS = -lc 

OBJECTS = \
$(OBJDIR)/OpenSpec.o \
$(OBJDIR)/DeferredStringObject.o \
$(OBJDIR)/kind.o \
$(OBJDIR)/Xt3dAlgorithm.o \
$(OBJDIR)/HashTable.o \
$(OBJDIR)/ims8reordering.o \
$(OBJDIR)/BaseGeometry.o \
$(OBJDIR)/Constants.o \
$(OBJDIR)/Sparse.o \
$(OBJDIR)/compilerversion.o \
$(OBJDIR)/version.o \
$(OBJDIR)/Memory.o \
$(OBJDIR)/Averaging.o \
$(OBJDIR)/SmoothingFunctions.o \
$(OBJDIR)/SimVariables.o \
$(OBJDIR)/genericutils.o \
$(OBJDIR)/Timer.o \
$(OBJDIR)/List.o \
$(OBJDIR)/ArrayHandlers.o \
$(OBJDIR)/MemoryList.o \
$(OBJDIR)/ObsOutput.o \
$(OBJDIR)/TimeSeriesRecord.o \
$(OBJDIR)/mf6lists.o \
$(OBJDIR)/Sim.o \
$(OBJDIR)/StringList.o \
$(OBJDIR)/Iunit.o \
$(OBJDIR)/InputOutput.o \
$(OBJDIR)/MemoryManager.o \
$(OBJDIR)/BaseModel.o \
$(OBJDIR)/Budget.o \
$(OBJDIR)/TableTerm.o \
$(OBJDIR)/sort.o \
$(OBJDIR)/DisvGeom.o \
$(OBJDIR)/ObsOutputList.o \
$(OBJDIR)/comarg.o \
$(OBJDIR)/CircularGeometry.o \
$(OBJDIR)/BaseSolution.o \
$(OBJDIR)/ArrayReaders.o \
$(OBJDIR)/RectangularGeometry.o \
$(OBJDIR)/PrintSaveManager.o \
$(OBJDIR)/RectangularChGeometry.o \
$(OBJDIR)/ListReader.o \
$(OBJDIR)/BlockParser.o \
$(OBJDIR)/PackageMover.o \
$(OBJDIR)/NameFile.o \
$(OBJDIR)/Connections.o \
$(OBJDIR)/TimeSeries.o \
$(OBJDIR)/Mover.o \
$(OBJDIR)/ims8linear.o \
$(OBJDIR)/tdis.o \
$(OBJDIR)/TimeSeriesLink.o \
$(OBJDIR)/BaseExchange.o \
$(OBJDIR)/TimeSeriesFileList.o \
$(OBJDIR)/BudgetFileReader.o \
$(OBJDIR)/UzfCellGroup.o \
$(OBJDIR)/Table.o \
$(OBJDIR)/SolutionGroup.o \
$(OBJDIR)/TimeSeriesManager.o \
$(OBJDIR)/DiscretizationBase.o \
$(OBJDIR)/TimeArray.o \
$(OBJDIR)/BudgetTerm.o \
$(OBJDIR)/gwf3disv8.o \
$(OBJDIR)/TimeArraySeries.o \
$(OBJDIR)/Observe.o \
$(OBJDIR)/gwf3dis8.o \
$(OBJDIR)/BudgetObject.o \
$(OBJDIR)/gwf3disu8.o \
$(OBJDIR)/ObsUtility.o \
$(OBJDIR)/TimeArraySeriesLink.o \
$(OBJDIR)/OutputControlData.o \
$(OBJDIR)/NumericalPackage.o \
$(OBJDIR)/Xt3dInterface.o \
$(OBJDIR)/gwf3mvr8.o \
$(OBJDIR)/ObsContainer.o \
$(OBJDIR)/TimeArraySeriesManager.o \
$(OBJDIR)/gwf3sto8.o \
$(OBJDIR)/Obs3.o \
$(OBJDIR)/gwf3ic8.o \
$(OBJDIR)/OutputControl.o \
$(OBJDIR)/VKDProcess.o \
$(OBJDIR)/gwf3oc8.o \
$(OBJDIR)/BoundaryPackage.o \
$(OBJDIR)/gwf3rch8.o \
$(OBJDIR)/gwf3ghb8.o \
$(OBJDIR)/gwf3riv8.o \
$(OBJDIR)/gwf3lak8.o \
$(OBJDIR)/gwf3chd8.o \
$(OBJDIR)/gwf3drn8.o \
$(OBJDIR)/gwf3maw8.o \
$(OBJDIR)/gwf3hfb8.o \
$(OBJDIR)/gwf3uzf8.o \
$(OBJDIR)/NumericalModel.o \
$(OBJDIR)/GhostNode.o \
$(OBJDIR)/gwf3csub8.o \
$(OBJDIR)/gwf3wel8.o \
$(OBJDIR)/gwf3evt8.o \
$(OBJDIR)/gwf3obs8.o \
$(OBJDIR)/gwf3sfr8.o \
$(OBJDIR)/NumericalExchange.o \
$(OBJDIR)/gwf3npf8.o \
$(OBJDIR)/NumericalSolution.o \
$(OBJDIR)/gwf3.o \
$(OBJDIR)/GwfGwfExchange.o \
$(OBJDIR)/SimulationCreate.o \
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

$(OBJDIR)/%.o : %.f90
	@mkdir -p $(@D)
	$(FC) $(FFLAGS) -c $< -o $@ -I$(OBJDIR) -J$(OBJDIR) 

$(OBJDIR)/%.o : %.F90
	@mkdir -p $(@D)
	$(FC) $(FFLAGS) -c $< -o $@ -I$(OBJDIR) -J$(OBJDIR) 

$(OBJDIR)/%.o : %.fpp
	@mkdir -p $(@D)
	$(FC) $(FFLAGS) -c $< -o $@ -I$(OBJDIR) -J$(OBJDIR) 

$(OBJDIR)/%.o : %.c
	@mkdir -p $(@D)
	$(CC) $(CFLAGS) -c $< -o $@

$(OBJDIR)/%.o : %.cpp
	@mkdir -p $(@D)
	$(CC) $(CFLAGS) -c $< -o $@

# Clean the object and module files and the executable
.PHONY : clean
clean : 
	-rm -rf $(OBJDIR)
	-rm -rf $(PROGRAM)

# Clean the object and module files
.PHONY : cleanobj
cleanobj : 
	-rm -rf $(OBJDIR)

