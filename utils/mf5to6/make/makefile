# makefile created on 2019-12-12 13:31:13.926505
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
CFLAGS = -O2 -D_UF 

# Define the libraries
SYSLIBS = 

OBJECTS = \
$(OBJDIR)/GwfUpwModule.o \
$(OBJDIR)/GwfChdModule.o \
$(OBJDIR)/ConstantsPHMF.o \
$(OBJDIR)/StressPeriod.o \
$(OBJDIR)/GwfGhbModule.o \
$(OBJDIR)/GwfRchModule.o \
$(OBJDIR)/GwfUzfModule_NWT.o \
$(OBJDIR)/GwfLpfModule.o \
$(OBJDIR)/ParamModule.o \
$(OBJDIR)/mach_mod.o \
$(OBJDIR)/OpenSpec.o \
$(OBJDIR)/GlobalPHMF.o \
$(OBJDIR)/GwfRivModule.o \
$(OBJDIR)/ConverterCommon.o \
$(OBJDIR)/GwfBcfModule.o \
$(OBJDIR)/GwfEvtModule.o \
$(OBJDIR)/GwfFhbModule.o \
$(OBJDIR)/GwfWelModule.o \
$(OBJDIR)/GwfDrnModule.o \
$(OBJDIR)/GwfEtsModule.o \
$(OBJDIR)/GwfResModule.o \
$(OBJDIR)/GwfLgrModule.o \
$(OBJDIR)/GwfLakModule.o \
$(OBJDIR)/gwf2hfb7_NWT.o \
$(OBJDIR)/NWT1_xmdlib.o \
$(OBJDIR)/kind.o \
$(OBJDIR)/GwfMnwModule.o \
$(OBJDIR)/GlobalVariablesPHMF.o \
$(OBJDIR)/List.o \
$(OBJDIR)/NWT1_module.o \
$(OBJDIR)/Constants.o \
$(OBJDIR)/Connection.o \
$(OBJDIR)/genericutils.o \
$(OBJDIR)/LakeOutlet.o \
$(OBJDIR)/Auxiliary.o \
$(OBJDIR)/version.o \
$(OBJDIR)/SfrDiversion.o \
$(OBJDIR)/TimeSeriesRecord.o \
$(OBJDIR)/Memory.o \
$(OBJDIR)/GwfSfrModule.o \
$(OBJDIR)/CharacterContainer.o \
$(OBJDIR)/Global.o \
$(OBJDIR)/LakeTributary.o \
$(OBJDIR)/GlobalVariables.o \
$(OBJDIR)/GwfBasModule.o \
$(OBJDIR)/pcgn2.o \
$(OBJDIR)/MemoryList.o \
$(OBJDIR)/SimVariables.o \
$(OBJDIR)/ArrayHandlers.o \
$(OBJDIR)/SfrReach.o \
$(OBJDIR)/LakeConnection.o \
$(OBJDIR)/NWT1_ilupc_mod.o \
$(OBJDIR)/ModelPackage.o \
$(OBJDIR)/SimPHMF.o \
$(OBJDIR)/SfrSegment.o \
$(OBJDIR)/MultiLayerObsModule.o \
$(OBJDIR)/InputOutput.o \
$(OBJDIR)/GwfFhbSubs.o \
$(OBJDIR)/SimVariablesPHMF.o \
$(OBJDIR)/Utilities.o \
$(OBJDIR)/LineList.o \
$(OBJDIR)/Lake.o \
$(OBJDIR)/MemoryManager.o \
$(OBJDIR)/GwfSfrCheck.o \
$(OBJDIR)/BlockParser.o \
$(OBJDIR)/precutls.o \
$(OBJDIR)/ObservePHMF.o \
$(OBJDIR)/ArrayReadersMF5.o \
$(OBJDIR)/File.o \
$(OBJDIR)/GwfResSubs.o \
$(OBJDIR)/FileWriter.o \
$(OBJDIR)/FileList.o \
$(OBJDIR)/TimeSeries.o \
$(OBJDIR)/DisWriter.o \
$(OBJDIR)/DiscretizationBasePHMF.o \
$(OBJDIR)/Discretization3D.o \
$(OBJDIR)/TdisWriter.o \
$(OBJDIR)/NpfWriter.o \
$(OBJDIR)/utl7.o \
$(OBJDIR)/gmg7.o \
$(OBJDIR)/StoWriter.o \
$(OBJDIR)/GwfEvtSubs.o \
$(OBJDIR)/obs2drn7.o \
$(OBJDIR)/IcWriter.o \
$(OBJDIR)/obs2riv7.o \
$(OBJDIR)/NWT1_gmres.o \
$(OBJDIR)/GwfRchSubs.o \
$(OBJDIR)/TdisVariables.o \
$(OBJDIR)/GwfBasOcSubs.o \
$(OBJDIR)/de47.o \
$(OBJDIR)/ChdType.o \
$(OBJDIR)/obs2chd7.o \
$(OBJDIR)/Mover.o \
$(OBJDIR)/gwf2wel7_NWT.o \
$(OBJDIR)/pcg7.o \
$(OBJDIR)/GwfEtsSubs.o \
$(OBJDIR)/GwfGhbSubs.o \
$(OBJDIR)/parutl7.o \
$(OBJDIR)/GwfDrnSubs.o \
$(OBJDIR)/ObsBlock.o \
$(OBJDIR)/GwfLakSubs.o \
$(OBJDIR)/sip7.o \
$(OBJDIR)/GwfRivSubs.o \
$(OBJDIR)/GwfLgrSubs.o \
$(OBJDIR)/GwfHfbSubs_NWT.o \
$(OBJDIR)/GwfUzfSubs_NWT.o \
$(OBJDIR)/GwfMnwSubs.o \
$(OBJDIR)/GwfSfrSubs.o \
$(OBJDIR)/NWT1_xmd.o \
$(OBJDIR)/Preproc.o \
$(OBJDIR)/gwf2mnw17.o \
$(OBJDIR)/GwfChdSubs.o \
$(OBJDIR)/obs2bas7.o \
$(OBJDIR)/NWT1_solver.o \
$(OBJDIR)/obs2ghb7.o \
$(OBJDIR)/gwf2mnw2i7.o \
$(OBJDIR)/ObsWriter.o \
$(OBJDIR)/ChdObsWriter.o \
$(OBJDIR)/DrnObsWriter.o \
$(OBJDIR)/RivObsWriter.o \
$(OBJDIR)/PackageWriter.o \
$(OBJDIR)/MawPackageWriter.o \
$(OBJDIR)/EvtPackageWriter.o \
$(OBJDIR)/GhbPackageWriter.o \
$(OBJDIR)/RchPackageWriter.o \
$(OBJDIR)/DrnPackageWriter.o \
$(OBJDIR)/GhbObsWriter.o \
$(OBJDIR)/RivPackageWriter.o \
$(OBJDIR)/ChdPackageWriter.o \
$(OBJDIR)/ImsPackageWriter.o \
$(OBJDIR)/WelPackageWriter.o \
$(OBJDIR)/HfbPackageWriter.o \
$(OBJDIR)/OutputControlWriter.o \
$(OBJDIR)/MvrPackageWriter.o \
$(OBJDIR)/FhbPackageWriter.o \
$(OBJDIR)/LakPackageWriter.o \
$(OBJDIR)/SfrPackageWriter.o \
$(OBJDIR)/UzfPackageWriter.o \
$(OBJDIR)/Model.o \
$(OBJDIR)/Exchange.o \
$(OBJDIR)/GwfBasSubs.o \
$(OBJDIR)/GwfLpfSubs.o \
$(OBJDIR)/GwfUpwSubs.o \
$(OBJDIR)/GwfBcfSubs.o \
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

