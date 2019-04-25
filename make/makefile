# makefile created on 2019-04-03 15:40:14.628797
# by pymake (version 1.1.0)
# using the gfortran fortran and gcc c/c++ compilers.

# Define the directories for the object and module files,
# the executable, and the executable name and path.
OBJDIR = ./obj_temp
BINDIR = .
PROGRAM = ../bin/mf6

SOURCEDIR1=../src
SOURCEDIR2=../src/Timing
SOURCEDIR3=../src/Exchange
SOURCEDIR4=../src/Solution
SOURCEDIR5=../src/Solution/SparseMatrixSolver
SOURCEDIR6=../src/Utilities
SOURCEDIR7=../src/Utilities/TimeSeries
SOURCEDIR8=../src/Utilities/OutputControl
SOURCEDIR9=../src/Utilities/Memory
SOURCEDIR10=../src/Utilities/Observation
SOURCEDIR11=../src/Model
SOURCEDIR12=../src/Model/GroundWaterFlow
SOURCEDIR13=../src/Model/ModelUtilities
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
FFLAGS = -O2 -fbacktrace -ffpe-summary=overflow -ffpe-trap=overflow,zero,invalid -march=native -mtune=native -O2 -fwhole-program -fstack-arrays -flto

# Define the C compile flags
CC = gcc
CFLAGS = -O3 -D_UF 

# Define the libraries
SYSLIBS = -lc 

OBJECTS = \
$(OBJDIR)/kind.o \
$(OBJDIR)/version.o \
$(OBJDIR)/HashTable.o \
$(OBJDIR)/ims8reordering.o \
$(OBJDIR)/List.o \
$(OBJDIR)/Constants.o \
$(OBJDIR)/genericutils.o \
$(OBJDIR)/BaseGeometry.o \
$(OBJDIR)/SimVariables.o \
$(OBJDIR)/compilerversion.o \
$(OBJDIR)/OpenSpec.o \
$(OBJDIR)/Timer.o \
$(OBJDIR)/Xt3dAlgorithm.o \
$(OBJDIR)/Sparse.o \
$(OBJDIR)/SmoothingFunctions.o \
$(OBJDIR)/ObsOutput.o \
$(OBJDIR)/mf6lists.o \
$(OBJDIR)/TimeSeriesRecord.o \
$(OBJDIR)/StringList.o \
$(OBJDIR)/ArrayHandlers.o \
$(OBJDIR)/Memory.o \
$(OBJDIR)/Sim.o \
$(OBJDIR)/Budget.o \
$(OBJDIR)/MemoryList.o \
$(OBJDIR)/InputOutput.o \
$(OBJDIR)/CircularGeometry.o \
$(OBJDIR)/RectangularGeometry.o \
$(OBJDIR)/ObsOutputList.o \
$(OBJDIR)/PrintSaveManager.o \
$(OBJDIR)/DisvGeom.o \
$(OBJDIR)/comarg.o \
$(OBJDIR)/ArrayReaders.o \
$(OBJDIR)/MemoryManager.o \
$(OBJDIR)/ListReader.o \
$(OBJDIR)/sort.o \
$(OBJDIR)/BlockParser.o \
$(OBJDIR)/RectangularChGeometry.o \
$(OBJDIR)/Iunit.o \
$(OBJDIR)/BaseModel.o \
$(OBJDIR)/PackageMover.o \
$(OBJDIR)/BaseSolution.o \
$(OBJDIR)/TimeSeries.o \
$(OBJDIR)/NameFile.o \
$(OBJDIR)/Connections.o \
$(OBJDIR)/Mover.o \
$(OBJDIR)/ims8linear.o \
$(OBJDIR)/tdis.o \
$(OBJDIR)/SolutionGroup.o \
$(OBJDIR)/TimeSeriesLink.o \
$(OBJDIR)/TimeSeriesFileList.o \
$(OBJDIR)/BndUzfKinematic.o \
$(OBJDIR)/TimeSeriesManager.o \
$(OBJDIR)/DiscretizationBase.o \
$(OBJDIR)/NumericalPackage.o \
$(OBJDIR)/TimeArray.o \
$(OBJDIR)/gwf3disu8.o \
$(OBJDIR)/BaseExchange.o \
$(OBJDIR)/Averaging.o \
$(OBJDIR)/VKDProcess.o \
$(OBJDIR)/Observe.o \
$(OBJDIR)/Xt3dInterface.o \
$(OBJDIR)/gwf3mvr8.o \
$(OBJDIR)/gwf3dis8.o \
$(OBJDIR)/ObsUtility.o \
$(OBJDIR)/OutputControlData.o \
$(OBJDIR)/gwf3sto8.o \
$(OBJDIR)/OutputControl.o \
$(OBJDIR)/gwf3hfb8.o \
$(OBJDIR)/ObsContainer.o \
$(OBJDIR)/TimeArraySeries.o \
$(OBJDIR)/TimeArraySeriesLink.o \
$(OBJDIR)/Obs3.o \
$(OBJDIR)/gwf3ic8.o \
$(OBJDIR)/gwf3disv8.o \
$(OBJDIR)/gwf3npf8.o \
$(OBJDIR)/gwf3oc8.o \
$(OBJDIR)/TimeArraySeriesManager.o \
$(OBJDIR)/gwf3obs8.o \
$(OBJDIR)/BoundaryPackage.o \
$(OBJDIR)/gwf3ghb8.o \
$(OBJDIR)/gwf3sfr8.o \
$(OBJDIR)/gwf3evt8.o \
$(OBJDIR)/gwf3drn8.o \
$(OBJDIR)/gwf3wel8.o \
$(OBJDIR)/gwf3lak8.o \
$(OBJDIR)/gwf3rch8.o \
$(OBJDIR)/NumericalModel.o \
$(OBJDIR)/gwf3chd8.o \
$(OBJDIR)/gwf3maw8.o \
$(OBJDIR)/NumericalExchange.o \
$(OBJDIR)/gwf3riv8.o \
$(OBJDIR)/gwf3uzf8.o \
$(OBJDIR)/NumericalSolution.o \
$(OBJDIR)/GhostNode.o \
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

