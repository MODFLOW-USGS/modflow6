# makefile created on 2019-06-12 10:26:12.107386
# by pymake (version 1.1.0)
# using the gfortran fortran and gcc c/c++ compilers.

# Define the directories for the object and module files,
# the executable, and the executable name and path.
OBJDIR = ./obj_temp
BINDIR = ../bin
PROGRAM = ../bin/mf6

SOURCEDIR1=../src/
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
FFLAGS = -O2 -ffpe-summary=overflow -ffpe-trap=overflow,zero,invalid -fbacktrace 

# Define the C compile flags
CC = gcc
CFLAGS = -O2 -D_UF 

# Define the libraries
SYSLIBS = -lc 

OBJECTS = \
$(OBJDIR)/mf6.o \
$(OBJDIR)/OpenSpec.o \
$(OBJDIR)/cbackmod.o \
$(OBJDIR)/kind.o \
$(OBJDIR)/version.o \
$(OBJDIR)/Xt3dAlgorithm.o \
$(OBJDIR)/SimVariables.o \
$(OBJDIR)/Constants.o \
$(OBJDIR)/compilerversion.o \
$(OBJDIR)/ims8reordering.o \
$(OBJDIR)/BaseGeometry.o \
$(OBJDIR)/List.o \
$(OBJDIR)/TimeSeriesRecord.o \
$(OBJDIR)/HashTable.o \
$(OBJDIR)/SmoothingFunctions.o \
$(OBJDIR)/Sparse.o \
$(OBJDIR)/ArrayHandlers.o \
$(OBJDIR)/Memory.o \
$(OBJDIR)/MemoryList.o \
$(OBJDIR)/Timer.o \
$(OBJDIR)/StringList.o \
$(OBJDIR)/genericutils.o \
$(OBJDIR)/mf6lists.o \
$(OBJDIR)/ObsOutput.o \
$(OBJDIR)/Sim.o \
$(OBJDIR)/Budget.o \
$(OBJDIR)/sort.o \
$(OBJDIR)/Iunit.o \
$(OBJDIR)/InputOutput.o \
$(OBJDIR)/MemoryManager.o \
$(OBJDIR)/BaseModel.o \
$(OBJDIR)/RectangularGeometry.o \
$(OBJDIR)/comarg.o \
$(OBJDIR)/DisvGeom.o \
$(OBJDIR)/PackageMover.o \
$(OBJDIR)/BlockParser.o \
$(OBJDIR)/ObsOutputList.o \
$(OBJDIR)/CircularGeometry.o \
$(OBJDIR)/BaseSolution.o \
$(OBJDIR)/ArrayReaders.o \
$(OBJDIR)/ListReader.o \
$(OBJDIR)/PrintSaveManager.o \
$(OBJDIR)/RectangularChGeometry.o \
$(OBJDIR)/Connections.o \
$(OBJDIR)/TimeSeries.o \
$(OBJDIR)/Mover.o \
$(OBJDIR)/NameFile.o \
$(OBJDIR)/ims8linear.o \
$(OBJDIR)/tdis.o \
$(OBJDIR)/TimeSeriesLink.o \
$(OBJDIR)/TimeSeriesFileList.o \
$(OBJDIR)/BndUzfKinematic.o \
$(OBJDIR)/SolutionGroup.o \
$(OBJDIR)/BaseExchange.o \
$(OBJDIR)/TimeSeriesManager.o \
$(OBJDIR)/DiscretizationBase.o \
$(OBJDIR)/TimeArray.o \
$(OBJDIR)/gwf3dis8.o \
$(OBJDIR)/gwf3disu8.o \
$(OBJDIR)/Xt3dInterface.o \
$(OBJDIR)/Observe.o \
$(OBJDIR)/OutputControlData.o \
$(OBJDIR)/NumericalPackage.o \
$(OBJDIR)/TimeArraySeries.o \
$(OBJDIR)/gwf3disv8.o \
$(OBJDIR)/ObsContainer.o \
$(OBJDIR)/ObsUtility.o \
$(OBJDIR)/OutputControl.o \
$(OBJDIR)/gwf3sto8.o \
$(OBJDIR)/gwf3oc8.o \
$(OBJDIR)/TimeArraySeriesLink.o \
$(OBJDIR)/gwf3mvr8.o \
$(OBJDIR)/gwf3hfb8.o \
$(OBJDIR)/gwf3ic8.o \
$(OBJDIR)/TimeArraySeriesManager.o \
$(OBJDIR)/Obs3.o \
$(OBJDIR)/BoundaryPackage.o \
$(OBJDIR)/gwf3npf8.o \
$(OBJDIR)/gwf3lak8.o \
$(OBJDIR)/gwf3evt8.o \
$(OBJDIR)/gwf3uzf8.o \
$(OBJDIR)/gwf3sfr8.o \
$(OBJDIR)/gwf3wel8.o \
$(OBJDIR)/gwf3maw8.o \
$(OBJDIR)/gwf3rch8.o \
$(OBJDIR)/gwf3drn8.o \
$(OBJDIR)/gwf3chd8.o \
$(OBJDIR)/gwf3obs8.o \
$(OBJDIR)/gwf3ghb8.o \
$(OBJDIR)/NumericalModel.o \
$(OBJDIR)/gwf3riv8.o \
$(OBJDIR)/GhostNode.o \
$(OBJDIR)/gwf3.o \
$(OBJDIR)/NumericalExchange.o \
$(OBJDIR)/NumericalSolution.o \
$(OBJDIR)/GwfGwfExchange.o \
$(OBJDIR)/SimulationCreate.o \
$(OBJDIR)/mf6sub.o 

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

