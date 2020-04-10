# makefile created on 2019-11-01 18:38:32.979161
# by pymake (version 1.1.0)
# using the gfortran fortran and gcc c/c++ compilers.

# Define the directories for the object and module files,
# the executable, and the executable name and path.
OBJDIR = ./obj_temp
BINDIR = .
PROGRAM = mf6

VPATH = ./src_temp/Utilities ./src_temp/Model/Geometry ./src_temp/Model/ModelUtilities ./src_temp/Solution/SparseMatrixSolver ./src_temp/Utilities/TimeSeries ./src_temp ./src_temp/Utilities/Memory ./src_temp/Utilities/Observation ./src_temp/Utilities/OutputControl ./src_temp/Timing ./src_temp/Model ./src_temp/Solution ./src_temp/Model/GroundWaterFlow ./src_temp/Exchange 

.SUFFIXES: .c .cpp .f .f90 .F90 .fpp .o

# Define the Fortran compile flags
FC = gfortran
FFLAGS = -O2 -ffpe-summary=overflow -ffpe-trap=overflow,zero,invalid -fbacktrace -MMD -cpp 

# Define the C compile flags
CC = gcc
CFLAGS = -O2 -D_UF -MMD -cpp 

# Define the libraries
SYSLIBS = -lc 

OBJECTS = \
$(OBJDIR)/kind.o \
$(OBJDIR)/version.o \
$(OBJDIR)/List.o \
$(OBJDIR)/SimVariables.o \
$(OBJDIR)/compilerversion.o \
$(OBJDIR)/BaseGeometry.o \
$(OBJDIR)/HashTable.o \
$(OBJDIR)/Xt3dAlgorithm.o \
$(OBJDIR)/ims8reordering.o \
$(OBJDIR)/StringList.o \
$(OBJDIR)/OpenSpec.o \
$(OBJDIR)/Sparse.o \
$(OBJDIR)/Constants.o \
$(OBJDIR)/TimeSeriesRecord.o \
$(OBJDIR)/mf6lists.o \
$(OBJDIR)/ArrayHandlers.o \
$(OBJDIR)/Sim.o \
$(OBJDIR)/SmoothingFunctions.o \
$(OBJDIR)/Iunit.o \
$(OBJDIR)/Memory.o \
$(OBJDIR)/MemoryList.o \
$(OBJDIR)/Timer.o \
$(OBJDIR)/genericutils.o \
$(OBJDIR)/ObsOutput.o \
$(OBJDIR)/sort.o \
$(OBJDIR)/Budget.o \
$(OBJDIR)/InputOutput.o \
$(OBJDIR)/CircularGeometry.o \
$(OBJDIR)/RectangularChGeometry.o \
$(OBJDIR)/BlockParser.o \
$(OBJDIR)/ArrayReaders.o \
$(OBJDIR)/ObsOutputList.o \
$(OBJDIR)/NameFile.o \
$(OBJDIR)/ListReader.o \
$(OBJDIR)/PrintSaveManager.o \
$(OBJDIR)/RectangularGeometry.o \
$(OBJDIR)/DisvGeom.o \
$(OBJDIR)/MemoryManager.o \
$(OBJDIR)/tdis.o \
$(OBJDIR)/Connections.o \
$(OBJDIR)/comarg.o \
$(OBJDIR)/TimeSeries.o \
$(OBJDIR)/PackageMover.o \
$(OBJDIR)/TimeSeriesLink.o \
$(OBJDIR)/ims8linear.o \
$(OBJDIR)/BndUzfKinematic.o \
$(OBJDIR)/TimeSeriesFileList.o \
$(OBJDIR)/Mover.o \
$(OBJDIR)/BaseModel.o \
$(OBJDIR)/TimeSeriesManager.o \
$(OBJDIR)/BaseSolution.o \
$(OBJDIR)/DiscretizationBase.o \
$(OBJDIR)/OutputControlData.o \
$(OBJDIR)/gwf3disu8.o \
$(OBJDIR)/gwf3disv8.o \
$(OBJDIR)/SolutionGroup.o \
$(OBJDIR)/gwf3dis8.o \
$(OBJDIR)/Observe.o \
$(OBJDIR)/TimeArray.o \
$(OBJDIR)/TimeArraySeries.o \
$(OBJDIR)/NumericalPackage.o \
$(OBJDIR)/Xt3dInterface.o \
$(OBJDIR)/gwf3ic8.o \
$(OBJDIR)/BaseExchange.o \
$(OBJDIR)/OutputControl.o \
$(OBJDIR)/gwf3hfb8.o \
$(OBJDIR)/ObsContainer.o \
$(OBJDIR)/gwf3oc8.o \
$(OBJDIR)/gwf3mvr8.o \
$(OBJDIR)/gwf3npf8.o \
$(OBJDIR)/ObsUtility.o \
$(OBJDIR)/TimeArraySeriesLink.o \
$(OBJDIR)/TimeArraySeriesManager.o \
$(OBJDIR)/gwf3sto8.o \
$(OBJDIR)/Obs3.o \
$(OBJDIR)/BoundaryPackage.o \
$(OBJDIR)/NumericalModel.o \
$(OBJDIR)/gwf3chd8.o \
$(OBJDIR)/gwf3lak8.o \
$(OBJDIR)/gwf3riv8.o \
$(OBJDIR)/gwf3ghb8.o \
$(OBJDIR)/gwf3obs8.o \
$(OBJDIR)/gwf3sfr8.o \
$(OBJDIR)/NumericalExchange.o \
$(OBJDIR)/gwf3evt8.o \
$(OBJDIR)/gwf3drn8.o \
$(OBJDIR)/gwf3maw8.o \
$(OBJDIR)/gwf3wel8.o \
$(OBJDIR)/gwf3uzf8.o \
$(OBJDIR)/gwf3rch8.o \
$(OBJDIR)/GhostNode.o \
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

