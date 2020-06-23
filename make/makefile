# makefile created on 2020-06-11 17:47:11.819998
# by pymake (version 1.1.0)
# using the gfortran fortran and gcc c/c++ compilers.

# Define the directories for the object and module files,
# the executable, and the executable name and path.
OBJDIR = ./obj_temp
BINDIR = .
PROGRAM = mf6

SOURCEDIR1=../src
SOURCEDIR2=../src/Exchange
SOURCEDIR3=../src/Model
SOURCEDIR4=../src/Model/Geometry
SOURCEDIR5=../src/Model/GroundWaterFlow
SOURCEDIR6=../src/Model/ModelUtilities
SOURCEDIR7=../src/Solution
SOURCEDIR8=../src/Solution/SparseMatrixSolver
SOURCEDIR9=../src/Timing
SOURCEDIR10=../src/Utilities
SOURCEDIR11=../src/Utilities/Memory
SOURCEDIR12=../src/Utilities/Observation
SOURCEDIR13=../src/Utilities/OutputControl
SOURCEDIR14=../src/Utilities/TimeSeries

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
FFLAGS = -O2 -fbacktrace -Bstatic -D_WIN32 -MMD -cpp 

# Define the C compile flags
CC = gcc
CFLAGS = -O2 -Bstatic -D_UF -MMD -cpp 

# Define the libraries
SYSLIBS = -lgfortran -lm 

OBJECTS = \
$(OBJDIR)/OpenSpec.o \
$(OBJDIR)/kind.o \
$(OBJDIR)/Constants.o \
$(OBJDIR)/HashTable.o \
$(OBJDIR)/defmacro.o \
$(OBJDIR)/BaseGeometry.o \
$(OBJDIR)/SimVariables.o \
$(OBJDIR)/genericutils.o \
$(OBJDIR)/compilerversion.o \
$(OBJDIR)/ims8reordering.o \
$(OBJDIR)/Sparse.o \
$(OBJDIR)/version.o \
$(OBJDIR)/ArrayHandlers.o \
$(OBJDIR)/List.o \
$(OBJDIR)/SmoothingFunctions.o \
$(OBJDIR)/Timer.o \
$(OBJDIR)/Xt3dAlgorithm.o \
$(OBJDIR)/TimeSeriesRecord.o \
$(OBJDIR)/Message.o \
$(OBJDIR)/ObsOutput.o \
$(OBJDIR)/mf6lists.o \
$(OBJDIR)/StringList.o \
$(OBJDIR)/Sim.o \
$(OBJDIR)/Budget.o \
$(OBJDIR)/InputOutput.o \
$(OBJDIR)/sort.o \
$(OBJDIR)/BudgetFileReader.o \
$(OBJDIR)/CircularGeometry.o \
$(OBJDIR)/PrintSaveManager.o \
$(OBJDIR)/RectangularGeometry.o \
$(OBJDIR)/ArrayReaders.o \
$(OBJDIR)/Iunit.o \
$(OBJDIR)/ObsOutputList.o \
$(OBJDIR)/comarg.o \
$(OBJDIR)/TableTerm.o \
$(OBJDIR)/Table.o \
$(OBJDIR)/DisvGeom.o \
$(OBJDIR)/BlockParser.o \
$(OBJDIR)/Memory.o \
$(OBJDIR)/TimeSeries.o \
$(OBJDIR)/MemoryList.o \
$(OBJDIR)/TimeSeriesLink.o \
$(OBJDIR)/ListReader.o \
$(OBJDIR)/NameFile.o \
$(OBJDIR)/TimeSeriesFileList.o \
$(OBJDIR)/MemoryManager.o \
$(OBJDIR)/Connections.o \
$(OBJDIR)/tdis.o \
$(OBJDIR)/BaseModel.o \
$(OBJDIR)/ims8linear.o \
$(OBJDIR)/PackageMover.o \
$(OBJDIR)/TimeSeriesManager.o \
$(OBJDIR)/UzfCellGroup.o \
$(OBJDIR)/BaseSolution.o \
$(OBJDIR)/DiscretizationBase.o \
$(OBJDIR)/gwf3disu8.o \
$(OBJDIR)/gwf3dis8.o \
$(OBJDIR)/BudgetTerm.o \
$(OBJDIR)/SolutionGroup.o \
$(OBJDIR)/TimeArray.o \
$(OBJDIR)/OutputControlData.o \
$(OBJDIR)/gwf3disv8.o \
$(OBJDIR)/Observe.o \
$(OBJDIR)/Xt3dInterface.o \
$(OBJDIR)/Mover.o \
$(OBJDIR)/BaseExchange.o \
$(OBJDIR)/NumericalPackage.o \
$(OBJDIR)/BudgetObject.o \
$(OBJDIR)/TimeArraySeries.o \
$(OBJDIR)/ObsUtility.o \
$(OBJDIR)/OutputControl.o \
$(OBJDIR)/ObsContainer.o \
$(OBJDIR)/TimeArraySeriesLink.o \
$(OBJDIR)/gwf3mvr8.o \
$(OBJDIR)/gwf3hfb8.o \
$(OBJDIR)/gwf3ic8.o \
$(OBJDIR)/gwf3npf8.o \
$(OBJDIR)/gwf3sto8.o \
$(OBJDIR)/gwf3oc8.o \
$(OBJDIR)/Obs3.o \
$(OBJDIR)/gwf3obs8.o \
$(OBJDIR)/gwf3csub8.o \
$(OBJDIR)/TimeArraySeriesManager.o \
$(OBJDIR)/BoundaryPackage.o \
$(OBJDIR)/gwf3evt8.o \
$(OBJDIR)/gwf3rch8.o \
$(OBJDIR)/NumericalModel.o \
$(OBJDIR)/gwf3maw8.o \
$(OBJDIR)/gwf3riv8.o \
$(OBJDIR)/gwf3ghb8.o \
$(OBJDIR)/gwf3wel8.o \
$(OBJDIR)/gwf3chd8.o \
$(OBJDIR)/gwf3sfr8.o \
$(OBJDIR)/GhostNode.o \
$(OBJDIR)/gwf3lak8.o \
$(OBJDIR)/gwf3uzf8.o \
$(OBJDIR)/gwf3drn8.o \
$(OBJDIR)/gwf3.o \
$(OBJDIR)/NumericalExchange.o \
$(OBJDIR)/GwfGwfExchange.o \
$(OBJDIR)/NumericalSolution.o \
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

