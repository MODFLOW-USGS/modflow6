# makefile created on 2017-08-07 08:46:28.417044
# by pymake (version 1.1.0)
# using the gfortran fortran and gcc c/c++ compilers.

# Define the directories for the object and module files,
# the executable, and the executable name and path.
OBJDIR = ./obj_temp
BINDIR = .
PROGRAM = mf5to6

SOURCEDIR1=../src
SOURCEDIR2=../src/LGR
SOURCEDIR3=../src/MF2005
SOURCEDIR4=../src/NWT
SOURCEDIR5=../src/Preproc
SOURCEDIR6=../../../src/Utilities/Memory
SOURCEDIR7=../../../src/Utilities/TimeSeries
SOURCEDIR8=../../../src/Utilities

VPATH = \
${SOURCEDIR1} \
${SOURCEDIR2} \
${SOURCEDIR3} \
${SOURCEDIR4} \
${SOURCEDIR5} \
${SOURCEDIR6} \
${SOURCEDIR7} \
${SOURCEDIR8} 

.SUFFIXES: .c .cpp .f .f90 .F90 .fpp .o

# Define the Fortran compile flags
FC = gfortran
FFLAGS = -O2 -fbacktrace -ffpe-summary=overflow -ffpe-trap=overflow,zero,invalid 

# Define the C compile flags
CC = gcc
CFLAGS = -O3 -D_UF 

# Define the libraries
SYSLIBS = -lc 

OBJECTS = \
$(OBJDIR)/GwfUzfModule_NWT.o \
$(OBJDIR)/GwfEtsModule.o \
$(OBJDIR)/kind.o \
$(OBJDIR)/GwfLpfModule.o \
$(OBJDIR)/Constants.o \
$(OBJDIR)/GwfBcfModule.o \
$(OBJDIR)/GwfFhbModule.o \
$(OBJDIR)/GwfGhbModule.o \
$(OBJDIR)/StressPeriod.o \
$(OBJDIR)/Connection.o \
$(OBJDIR)/GwfChdModule.o \
$(OBJDIR)/OpenSpec.o \
$(OBJDIR)/GwfWelModule.o \
$(OBJDIR)/GlobalVariables.o \
$(OBJDIR)/gwf2hfb7_NWT.o \
$(OBJDIR)/ParamModule.o \
$(OBJDIR)/Global.o \
$(OBJDIR)/Memory.o \
$(OBJDIR)/NWT1_xmdlib.o \
$(OBJDIR)/ConstantsPHMF.o \
$(OBJDIR)/List.o \
$(OBJDIR)/SfrDiversion.o \
$(OBJDIR)/GwfLgrModule.o \
$(OBJDIR)/LakeTributary.o \
$(OBJDIR)/GwfLakModule.o \
$(OBJDIR)/GwfEvtModule.o \
$(OBJDIR)/GwfUpwModule.o \
$(OBJDIR)/mach_mod.o \
$(OBJDIR)/GwfDrnModule.o \
$(OBJDIR)/GlobalPHMF.o \
$(OBJDIR)/Auxiliary.o \
$(OBJDIR)/GlobalVariablesPHMF.o \
$(OBJDIR)/GwfResModule.o \
$(OBJDIR)/GwfRchModule.o \
$(OBJDIR)/GwfSfrModule.o \
$(OBJDIR)/GwfRivModule.o \
$(OBJDIR)/ConverterCommon.o \
$(OBJDIR)/GwfMnwModule.o \
$(OBJDIR)/LakeConnection.o \
$(OBJDIR)/GwfBasModule.o \
$(OBJDIR)/TimeSeriesRecord.o \
$(OBJDIR)/LakeOutlet.o \
$(OBJDIR)/NWT1_module.o \
$(OBJDIR)/pcgn2.o \
$(OBJDIR)/NWT1_ilupc_mod.o \
$(OBJDIR)/MemoryList.o \
$(OBJDIR)/SimVariables.o \
$(OBJDIR)/ArrayHandlers.o \
$(OBJDIR)/CharacterContainer.o \
$(OBJDIR)/SimPHMF.o \
$(OBJDIR)/InputOutput.o \
$(OBJDIR)/MemoryManager.o \
$(OBJDIR)/ModelPackage.o \
$(OBJDIR)/MultiLayerObsModule.o \
$(OBJDIR)/SfrReach.o \
$(OBJDIR)/SimVariablesPHMF.o \
$(OBJDIR)/BlockParser.o \
$(OBJDIR)/GwfSfrCheck.o \
$(OBJDIR)/LineList.o \
$(OBJDIR)/Lake.o \
$(OBJDIR)/GwfFhbSubs.o \
$(OBJDIR)/ObservePHMF.o \
$(OBJDIR)/Utilities.o \
$(OBJDIR)/ArrayReadersMF5.o \
$(OBJDIR)/TimeSeries.o \
$(OBJDIR)/ChdType.o \
$(OBJDIR)/GwfResSubs.o \
$(OBJDIR)/SfrSegment.o \
$(OBJDIR)/precutls.o \
$(OBJDIR)/DiscretizationBasePHMF.o \
$(OBJDIR)/File.o \
$(OBJDIR)/FileList.o \
$(OBJDIR)/utl7.o \
$(OBJDIR)/GwfEtsSubs.o \
$(OBJDIR)/Discretization3D.o \
$(OBJDIR)/obs2riv7.o \
$(OBJDIR)/parutl7.o \
$(OBJDIR)/NWT1_solver.o \
$(OBJDIR)/obs2chd7.o \
$(OBJDIR)/NWT1_xmd.o \
$(OBJDIR)/Mover.o \
$(OBJDIR)/NWT1_gmres.o \
$(OBJDIR)/GwfMnwSubs.o \
$(OBJDIR)/GwfLakSubs.o \
$(OBJDIR)/FileWriter.o \
$(OBJDIR)/GwfChdSubs.o \
$(OBJDIR)/GwfDrnSubs.o \
$(OBJDIR)/GwfGhbSubs.o \
$(OBJDIR)/GwfLgrSubs.o \
$(OBJDIR)/gwf2wel7_NWT.o \
$(OBJDIR)/sip7.o \
$(OBJDIR)/de47.o \
$(OBJDIR)/GwfEvtSubs.o \
$(OBJDIR)/obs2drn7.o \
$(OBJDIR)/obs2bas7.o \
$(OBJDIR)/GwfUzfSubs_NWT.o \
$(OBJDIR)/TdisWriter.o \
$(OBJDIR)/NpfWriter.o \
$(OBJDIR)/GwfRchSubs.o \
$(OBJDIR)/obs2ghb7.o \
$(OBJDIR)/GwfRivSubs.o \
$(OBJDIR)/GwfBasOcSubs.o \
$(OBJDIR)/GwfHfbSubs_NWT.o \
$(OBJDIR)/GwfSfrSubs.o \
$(OBJDIR)/gmg7.o \
$(OBJDIR)/TdisVariables.o \
$(OBJDIR)/ObsBlock.o \
$(OBJDIR)/pcg7.o \
$(OBJDIR)/StoWriter.o \
$(OBJDIR)/IcWriter.o \
$(OBJDIR)/gwf2mnw2i7.o \
$(OBJDIR)/DisWriter.o \
$(OBJDIR)/gwf2mnw17.o \
$(OBJDIR)/Preproc.o \
$(OBJDIR)/ObsWriter.o \
$(OBJDIR)/GhbObsWriter.o \
$(OBJDIR)/PackageWriter.o \
$(OBJDIR)/MvrPackageWriter.o \
$(OBJDIR)/MawPackageWriter.o \
$(OBJDIR)/HfbPackageWriter.o \
$(OBJDIR)/DrnObsWriter.o \
$(OBJDIR)/LakPackageWriter.o \
$(OBJDIR)/DrnPackageWriter.o \
$(OBJDIR)/ImsPackageWriter.o \
$(OBJDIR)/OutputControlWriter.o \
$(OBJDIR)/WelPackageWriter.o \
$(OBJDIR)/GhbPackageWriter.o \
$(OBJDIR)/RchPackageWriter.o \
$(OBJDIR)/RivPackageWriter.o \
$(OBJDIR)/ChdObsWriter.o \
$(OBJDIR)/RivObsWriter.o \
$(OBJDIR)/SfrPackageWriter.o \
$(OBJDIR)/EvtPackageWriter.o \
$(OBJDIR)/UzfPackageWriter.o \
$(OBJDIR)/ChdPackageWriter.o \
$(OBJDIR)/FhbPackageWriter.o \
$(OBJDIR)/Model.o \
$(OBJDIR)/GwfLpfSubs.o \
$(OBJDIR)/GwfBasSubs.o \
$(OBJDIR)/Exchange.o \
$(OBJDIR)/GwfBcfSubs.o \
$(OBJDIR)/GwfUpwSubs.o \
$(OBJDIR)/ModelConverter.o \
$(OBJDIR)/ExchangeWriter.o \
$(OBJDIR)/SimFileWriter.o \
$(OBJDIR)/mf5to6.o 

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

