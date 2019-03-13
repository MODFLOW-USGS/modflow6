# makefile created on 2019-03-07 17:10:38.840084
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
FFLAGS = -O2 -fbacktrace 

# Define the C compile flags
CC = gcc
CFLAGS = -O3 -D_UF 

# Define the libraries
SYSLIBS = 

OBJECTS = \
$(OBJDIR)/ConverterCommon.o \
$(OBJDIR)/GwfLgrModule.o \
$(OBJDIR)/NWT1_xmdlib.o \
$(OBJDIR)/GwfRivModule.o \
$(OBJDIR)/GwfMnwModule.o \
$(OBJDIR)/ConstantsPHMF.o \
$(OBJDIR)/GwfFhbModule.o \
$(OBJDIR)/gwf2hfb7_NWT.o \
$(OBJDIR)/GwfGhbModule.o \
$(OBJDIR)/kind.o \
$(OBJDIR)/GwfDrnModule.o \
$(OBJDIR)/GwfUpwModule.o \
$(OBJDIR)/OpenSpec.o \
$(OBJDIR)/GwfResModule.o \
$(OBJDIR)/GwfWelModule.o \
$(OBJDIR)/GwfChdModule.o \
$(OBJDIR)/GwfLakModule.o \
$(OBJDIR)/GwfLpfModule.o \
$(OBJDIR)/GwfRchModule.o \
$(OBJDIR)/StressPeriod.o \
$(OBJDIR)/GwfUzfModule_NWT.o \
$(OBJDIR)/ParamModule.o \
$(OBJDIR)/GwfEtsModule.o \
$(OBJDIR)/GwfEvtModule.o \
$(OBJDIR)/mach_mod.o \
$(OBJDIR)/GwfBcfModule.o \
$(OBJDIR)/version.o \
$(OBJDIR)/Constants.o \
$(OBJDIR)/GlobalVariablesPHMF.o \
$(OBJDIR)/GlobalPHMF.o \
$(OBJDIR)/Connection.o \
$(OBJDIR)/NWT1_module.o \
$(OBJDIR)/List.o \
$(OBJDIR)/TimeSeriesRecord.o \
$(OBJDIR)/genericutils.o \
$(OBJDIR)/CharacterContainer.o \
$(OBJDIR)/GlobalVariables.o \
$(OBJDIR)/GwfSfrModule.o \
$(OBJDIR)/Global.o \
$(OBJDIR)/LakeOutlet.o \
$(OBJDIR)/pcgn2.o \
$(OBJDIR)/LakeTributary.o \
$(OBJDIR)/Memory.o \
$(OBJDIR)/SfrDiversion.o \
$(OBJDIR)/Auxiliary.o \
$(OBJDIR)/GwfBasModule.o \
$(OBJDIR)/SimVariables.o \
$(OBJDIR)/LakeConnection.o \
$(OBJDIR)/ModelPackage.o \
$(OBJDIR)/MemoryList.o \
$(OBJDIR)/NWT1_ilupc_mod.o \
$(OBJDIR)/ArrayHandlers.o \
$(OBJDIR)/SimPHMF.o \
$(OBJDIR)/InputOutput.o \
$(OBJDIR)/MemoryManager.o \
$(OBJDIR)/ObservePHMF.o \
$(OBJDIR)/BlockParser.o \
$(OBJDIR)/MultiLayerObsModule.o \
$(OBJDIR)/GwfFhbSubs.o \
$(OBJDIR)/Utilities.o \
$(OBJDIR)/ArrayReadersMF5.o \
$(OBJDIR)/GwfSfrCheck.o \
$(OBJDIR)/Lake.o \
$(OBJDIR)/SfrReach.o \
$(OBJDIR)/TimeSeries.o \
$(OBJDIR)/DiscretizationBasePHMF.o \
$(OBJDIR)/precutls.o \
$(OBJDIR)/GwfResSubs.o \
$(OBJDIR)/SimVariablesPHMF.o \
$(OBJDIR)/File.o \
$(OBJDIR)/FileWriter.o \
$(OBJDIR)/ChdType.o \
$(OBJDIR)/FileList.o \
$(OBJDIR)/DisWriter.o \
$(OBJDIR)/SfrSegment.o \
$(OBJDIR)/LineList.o \
$(OBJDIR)/Discretization3D.o \
$(OBJDIR)/TdisWriter.o \
$(OBJDIR)/StoWriter.o \
$(OBJDIR)/TdisVariables.o \
$(OBJDIR)/NpfWriter.o \
$(OBJDIR)/ObsBlock.o \
$(OBJDIR)/IcWriter.o \
$(OBJDIR)/utl7.o \
$(OBJDIR)/obs2drn7.o \
$(OBJDIR)/GwfBasOcSubs.o \
$(OBJDIR)/gmg7.o \
$(OBJDIR)/pcg7.o \
$(OBJDIR)/parutl7.o \
$(OBJDIR)/GwfRchSubs.o \
$(OBJDIR)/GwfLgrSubs.o \
$(OBJDIR)/obs2bas7.o \
$(OBJDIR)/GwfUzfSubs_NWT.o \
$(OBJDIR)/GwfRivSubs.o \
$(OBJDIR)/de47.o \
$(OBJDIR)/Mover.o \
$(OBJDIR)/NWT1_gmres.o \
$(OBJDIR)/Preproc.o \
$(OBJDIR)/obs2riv7.o \
$(OBJDIR)/GwfSfrSubs.o \
$(OBJDIR)/NWT1_xmd.o \
$(OBJDIR)/NWT1_solver.o \
$(OBJDIR)/GwfLakSubs.o \
$(OBJDIR)/obs2chd7.o \
$(OBJDIR)/GwfHfbSubs_NWT.o \
$(OBJDIR)/obs2ghb7.o \
$(OBJDIR)/GwfEtsSubs.o \
$(OBJDIR)/GwfChdSubs.o \
$(OBJDIR)/GwfGhbSubs.o \
$(OBJDIR)/GwfEvtSubs.o \
$(OBJDIR)/ObsWriter.o \
$(OBJDIR)/DrnObsWriter.o \
$(OBJDIR)/GwfDrnSubs.o \
$(OBJDIR)/GhbObsWriter.o \
$(OBJDIR)/GwfMnwSubs.o \
$(OBJDIR)/gwf2wel7_NWT.o \
$(OBJDIR)/RivObsWriter.o \
$(OBJDIR)/sip7.o \
$(OBJDIR)/ChdObsWriter.o \
$(OBJDIR)/gwf2mnw2i7.o \
$(OBJDIR)/PackageWriter.o \
$(OBJDIR)/GhbPackageWriter.o \
$(OBJDIR)/RivPackageWriter.o \
$(OBJDIR)/MawPackageWriter.o \
$(OBJDIR)/DrnPackageWriter.o \
$(OBJDIR)/OutputControlWriter.o \
$(OBJDIR)/MvrPackageWriter.o \
$(OBJDIR)/SfrPackageWriter.o \
$(OBJDIR)/gwf2mnw17.o \
$(OBJDIR)/EvtPackageWriter.o \
$(OBJDIR)/RchPackageWriter.o \
$(OBJDIR)/WelPackageWriter.o \
$(OBJDIR)/UzfPackageWriter.o \
$(OBJDIR)/ChdPackageWriter.o \
$(OBJDIR)/ImsPackageWriter.o \
$(OBJDIR)/HfbPackageWriter.o \
$(OBJDIR)/LakPackageWriter.o \
$(OBJDIR)/FhbPackageWriter.o \
$(OBJDIR)/Model.o \
$(OBJDIR)/GwfUpwSubs.o \
$(OBJDIR)/GwfLpfSubs.o \
$(OBJDIR)/GwfBcfSubs.o \
$(OBJDIR)/GwfBasSubs.o \
$(OBJDIR)/ModelConverter.o \
$(OBJDIR)/Exchange.o \
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

