# makefile created on 2019-03-13 12:38:11.683334
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
$(OBJDIR)/GwfRchModule.o \
$(OBJDIR)/GwfChdModule.o \
$(OBJDIR)/GwfWelModule.o \
$(OBJDIR)/GwfUzfModule_NWT.o \
$(OBJDIR)/gwf2hfb7_NWT.o \
$(OBJDIR)/GwfLakModule.o \
$(OBJDIR)/ConverterCommon.o \
$(OBJDIR)/GwfLgrModule.o \
$(OBJDIR)/NWT1_xmdlib.o \
$(OBJDIR)/OpenSpec.o \
$(OBJDIR)/StressPeriod.o \
$(OBJDIR)/ParamModule.o \
$(OBJDIR)/GwfResModule.o \
$(OBJDIR)/GwfRivModule.o \
$(OBJDIR)/GlobalPHMF.o \
$(OBJDIR)/GwfLpfModule.o \
$(OBJDIR)/GwfBcfModule.o \
$(OBJDIR)/GwfEvtModule.o \
$(OBJDIR)/GwfFhbModule.o \
$(OBJDIR)/GwfMnwModule.o \
$(OBJDIR)/GwfGhbModule.o \
$(OBJDIR)/kind.o \
$(OBJDIR)/mach_mod.o \
$(OBJDIR)/GwfDrnModule.o \
$(OBJDIR)/ConstantsPHMF.o \
$(OBJDIR)/GwfUpwModule.o \
$(OBJDIR)/version.o \
$(OBJDIR)/GwfEtsModule.o \
$(OBJDIR)/NWT1_module.o \
$(OBJDIR)/Constants.o \
$(OBJDIR)/Connection.o \
$(OBJDIR)/Memory.o \
$(OBJDIR)/GwfSfrModule.o \
$(OBJDIR)/List.o \
$(OBJDIR)/GlobalVariablesPHMF.o \
$(OBJDIR)/genericutils.o \
$(OBJDIR)/SfrDiversion.o \
$(OBJDIR)/TimeSeriesRecord.o \
$(OBJDIR)/Global.o \
$(OBJDIR)/GlobalVariables.o \
$(OBJDIR)/Auxiliary.o \
$(OBJDIR)/CharacterContainer.o \
$(OBJDIR)/LakeTributary.o \
$(OBJDIR)/ArrayHandlers.o \
$(OBJDIR)/NWT1_ilupc_mod.o \
$(OBJDIR)/MemoryList.o \
$(OBJDIR)/LakeOutlet.o \
$(OBJDIR)/GwfBasModule.o \
$(OBJDIR)/LakeConnection.o \
$(OBJDIR)/pcgn2.o \
$(OBJDIR)/SimVariables.o \
$(OBJDIR)/SimPHMF.o \
$(OBJDIR)/SimVariablesPHMF.o \
$(OBJDIR)/ModelPackage.o \
$(OBJDIR)/SfrReach.o \
$(OBJDIR)/MultiLayerObsModule.o \
$(OBJDIR)/SfrSegment.o \
$(OBJDIR)/InputOutput.o \
$(OBJDIR)/GwfSfrCheck.o \
$(OBJDIR)/GwfFhbSubs.o \
$(OBJDIR)/LineList.o \
$(OBJDIR)/ObservePHMF.o \
$(OBJDIR)/Lake.o \
$(OBJDIR)/MemoryManager.o \
$(OBJDIR)/ArrayReadersMF5.o \
$(OBJDIR)/GwfResSubs.o \
$(OBJDIR)/BlockParser.o \
$(OBJDIR)/Utilities.o \
$(OBJDIR)/precutls.o \
$(OBJDIR)/TimeSeries.o \
$(OBJDIR)/DiscretizationBasePHMF.o \
$(OBJDIR)/File.o \
$(OBJDIR)/FileWriter.o \
$(OBJDIR)/IcWriter.o \
$(OBJDIR)/ChdType.o \
$(OBJDIR)/FileList.o \
$(OBJDIR)/utl7.o \
$(OBJDIR)/Discretization3D.o \
$(OBJDIR)/StoWriter.o \
$(OBJDIR)/GwfGhbSubs.o \
$(OBJDIR)/GwfSfrSubs.o \
$(OBJDIR)/GwfDrnSubs.o \
$(OBJDIR)/DisWriter.o \
$(OBJDIR)/GwfRchSubs.o \
$(OBJDIR)/obs2riv7.o \
$(OBJDIR)/gwf2wel7_NWT.o \
$(OBJDIR)/obs2chd7.o \
$(OBJDIR)/obs2bas7.o \
$(OBJDIR)/NWT1_solver.o \
$(OBJDIR)/parutl7.o \
$(OBJDIR)/GwfUzfSubs_NWT.o \
$(OBJDIR)/de47.o \
$(OBJDIR)/NWT1_gmres.o \
$(OBJDIR)/ObsBlock.o \
$(OBJDIR)/GwfHfbSubs_NWT.o \
$(OBJDIR)/obs2ghb7.o \
$(OBJDIR)/NWT1_xmd.o \
$(OBJDIR)/GwfMnwSubs.o \
$(OBJDIR)/obs2drn7.o \
$(OBJDIR)/TdisWriter.o \
$(OBJDIR)/GwfLgrSubs.o \
$(OBJDIR)/GwfEtsSubs.o \
$(OBJDIR)/NpfWriter.o \
$(OBJDIR)/GwfChdSubs.o \
$(OBJDIR)/GwfBasOcSubs.o \
$(OBJDIR)/GwfRivSubs.o \
$(OBJDIR)/gwf2mnw2i7.o \
$(OBJDIR)/Mover.o \
$(OBJDIR)/sip7.o \
$(OBJDIR)/gmg7.o \
$(OBJDIR)/GwfLakSubs.o \
$(OBJDIR)/GwfEvtSubs.o \
$(OBJDIR)/pcg7.o \
$(OBJDIR)/TdisVariables.o \
$(OBJDIR)/Preproc.o \
$(OBJDIR)/gwf2mnw17.o \
$(OBJDIR)/ObsWriter.o \
$(OBJDIR)/GhbObsWriter.o \
$(OBJDIR)/ChdObsWriter.o \
$(OBJDIR)/PackageWriter.o \
$(OBJDIR)/RivObsWriter.o \
$(OBJDIR)/DrnObsWriter.o \
$(OBJDIR)/WelPackageWriter.o \
$(OBJDIR)/MvrPackageWriter.o \
$(OBJDIR)/GhbPackageWriter.o \
$(OBJDIR)/SfrPackageWriter.o \
$(OBJDIR)/ImsPackageWriter.o \
$(OBJDIR)/RivPackageWriter.o \
$(OBJDIR)/LakPackageWriter.o \
$(OBJDIR)/DrnPackageWriter.o \
$(OBJDIR)/RchPackageWriter.o \
$(OBJDIR)/HfbPackageWriter.o \
$(OBJDIR)/OutputControlWriter.o \
$(OBJDIR)/MawPackageWriter.o \
$(OBJDIR)/ChdPackageWriter.o \
$(OBJDIR)/EvtPackageWriter.o \
$(OBJDIR)/UzfPackageWriter.o \
$(OBJDIR)/FhbPackageWriter.o \
$(OBJDIR)/Model.o \
$(OBJDIR)/GwfBasSubs.o \
$(OBJDIR)/GwfBcfSubs.o \
$(OBJDIR)/GwfLpfSubs.o \
$(OBJDIR)/GwfUpwSubs.o \
$(OBJDIR)/Exchange.o \
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

