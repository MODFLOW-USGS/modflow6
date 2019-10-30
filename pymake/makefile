# makefile created on 2019-10-30 17:37:26.525809
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
FFLAGS = -O2 -ffpe-summary=overflow -ffpe-trap=overflow,zero,invalid -fbacktrace 

# Define the C compile flags
CC = gcc
CFLAGS = -O2 -D_UF 

# Define the libraries
SYSLIBS = -lc 

OBJECTS = \
$(OBJDIR)/OpenSpec.o \
$(OBJDIR)/kind.o \
$(OBJDIR)/SimVariables.o \
$(OBJDIR)/version.o \
$(OBJDIR)/Xt3dAlgorithm.o \
$(OBJDIR)/Constants.o \
$(OBJDIR)/List.o \
$(OBJDIR)/compilerversion.o \
$(OBJDIR)/ims8reordering.o \
$(OBJDIR)/BaseGeometry.o \
$(OBJDIR)/HashTable.o \
$(OBJDIR)/Sparse.o \
$(OBJDIR)/Timer.o \
$(OBJDIR)/ArrayHandlers.o \
$(OBJDIR)/StringList.o \
$(OBJDIR)/TimeSeriesRecord.o \
$(OBJDIR)/SmoothingFunctions.o \
$(OBJDIR)/Sim.o \
$(OBJDIR)/sort.o \
$(OBJDIR)/genericutils.o \
$(OBJDIR)/ObsOutput.o \
$(OBJDIR)/mf6lists.o \
$(OBJDIR)/Memory.o \
$(OBJDIR)/Budget.o \
$(OBJDIR)/MemoryList.o \
$(OBJDIR)/Iunit.o \
$(OBJDIR)/InputOutput.o \
$(OBJDIR)/DisvGeom.o \
$(OBJDIR)/ObsOutputList.o \
$(OBJDIR)/ListReader.o \
$(OBJDIR)/ArrayReaders.o \
$(OBJDIR)/CircularGeometry.o \
$(OBJDIR)/RectangularChGeometry.o \
$(OBJDIR)/RectangularGeometry.o \
$(OBJDIR)/BlockParser.o \
$(OBJDIR)/MemoryManager.o \
$(OBJDIR)/PrintSaveManager.o \
$(OBJDIR)/comarg.o \
$(OBJDIR)/ims8linear.o \
$(OBJDIR)/tdis.o \
$(OBJDIR)/Connections.o \
$(OBJDIR)/PackageMover.o \
$(OBJDIR)/BaseModel.o \
$(OBJDIR)/TimeSeries.o \
$(OBJDIR)/BndUzfKinematic.o \
$(OBJDIR)/Mover.o \
$(OBJDIR)/NameFile.o \
$(OBJDIR)/TimeSeriesLink.o \
$(OBJDIR)/BaseSolution.o \
$(OBJDIR)/TimeSeriesFileList.o \
$(OBJDIR)/TimeSeriesManager.o \
$(OBJDIR)/SolutionGroup.o \
$(OBJDIR)/DiscretizationBase.o \
$(OBJDIR)/OutputControlData.o \
$(OBJDIR)/OutputControl.o \
$(OBJDIR)/BaseExchange.o \
$(OBJDIR)/Observe.o \
$(OBJDIR)/TimeArray.o \
$(OBJDIR)/Xt3dInterface.o \
$(OBJDIR)/gwf3oc8.o \
$(OBJDIR)/TimeArraySeries.o \
$(OBJDIR)/NumericalPackage.o \
$(OBJDIR)/gwf3disu8.o \
$(OBJDIR)/gwf3mvr8.o \
$(OBJDIR)/gwf3disv8.o \
$(OBJDIR)/gwf3dis8.o \
$(OBJDIR)/ObsUtility.o \
$(OBJDIR)/ObsContainer.o \
$(OBJDIR)/gwf3ic8.o \
$(OBJDIR)/TimeArraySeriesLink.o \
$(OBJDIR)/gwf3npf8.o \
$(OBJDIR)/gwf3sto8.o \
$(OBJDIR)/Obs3.o \
$(OBJDIR)/gwf3obs8.o \
$(OBJDIR)/gwf3hfb8.o \
$(OBJDIR)/TimeArraySeriesManager.o \
$(OBJDIR)/BoundaryPackage.o \
$(OBJDIR)/gwf3drn8.o \
$(OBJDIR)/gwf3chd8.o \
$(OBJDIR)/gwf3sfr8.o \
$(OBJDIR)/NumericalModel.o \
$(OBJDIR)/gwf3riv8.o \
$(OBJDIR)/GhostNode.o \
$(OBJDIR)/gwf3lak8.o \
$(OBJDIR)/gwf3maw8.o \
$(OBJDIR)/gwf3ghb8.o \
$(OBJDIR)/gwf3uzf8.o \
$(OBJDIR)/gwf3rch8.o \
$(OBJDIR)/gwf3wel8.o \
$(OBJDIR)/gwf3evt8.o \
$(OBJDIR)/NumericalExchange.o \
$(OBJDIR)/NumericalSolution.o \
$(OBJDIR)/gwf3.o \
$(OBJDIR)/GwfGwfExchange.o \
$(OBJDIR)/SimulationCreate.o \
$(OBJDIR)/mf6lib.o \
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

