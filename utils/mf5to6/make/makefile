# makefile created on 2018-08-09 13:41:31.725160
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
$(OBJDIR)/GlobalPHMF.o \
$(OBJDIR)/mach_mod.o \
$(OBJDIR)/GwfLpfModule.o \
$(OBJDIR)/GwfEvtModule.o \
$(OBJDIR)/gwf2hfb7_NWT.o \
$(OBJDIR)/GwfResModule.o \
$(OBJDIR)/GwfDrnModule.o \
$(OBJDIR)/ConstantsPHMF.o \
$(OBJDIR)/GwfEtsModule.o \
$(OBJDIR)/GwfGhbModule.o \
$(OBJDIR)/GwfLakModule.o \
$(OBJDIR)/NWT1_module.o \
$(OBJDIR)/kind.o \
$(OBJDIR)/GwfUpwModule.o \
$(OBJDIR)/NWT1_xmdlib.o \
$(OBJDIR)/OpenSpec.o \
$(OBJDIR)/GwfMnwModule.o \
$(OBJDIR)/GwfChdModule.o \
$(OBJDIR)/GwfRchModule.o \
$(OBJDIR)/GwfUzfModule_NWT.o \
$(OBJDIR)/GwfBcfModule.o \
$(OBJDIR)/ConverterCommon.o \
$(OBJDIR)/GwfFhbModule.o \
$(OBJDIR)/ParamModule.o \
$(OBJDIR)/GwfWelModule.o \
$(OBJDIR)/GwfLgrModule.o \
$(OBJDIR)/GwfRivModule.o \
$(OBJDIR)/StressPeriod.o \
$(OBJDIR)/GlobalVariablesPHMF.o \
$(OBJDIR)/version.o \
$(OBJDIR)/List.o \
$(OBJDIR)/Constants.o \
$(OBJDIR)/Connection.o \
$(OBJDIR)/TimeSeriesRecord.o \
$(OBJDIR)/CharacterContainer.o \
$(OBJDIR)/LakeOutlet.o \
$(OBJDIR)/LakeTributary.o \
$(OBJDIR)/Global.o \
$(OBJDIR)/GwfSfrModule.o \
$(OBJDIR)/LakeConnection.o \
$(OBJDIR)/Auxiliary.o \
$(OBJDIR)/GlobalVariables.o \
$(OBJDIR)/SfrDiversion.o \
$(OBJDIR)/ArrayHandlers.o \
$(OBJDIR)/Memory.o \
$(OBJDIR)/SimVariables.o \
$(OBJDIR)/MemoryList.o \
$(OBJDIR)/NWT1_ilupc_mod.o \
$(OBJDIR)/SfrReach.o \
$(OBJDIR)/pcgn2.o \
$(OBJDIR)/GwfBasModule.o \
$(OBJDIR)/SimPHMF.o \
$(OBJDIR)/SimVariablesPHMF.o \
$(OBJDIR)/SfrSegment.o \
$(OBJDIR)/GwfSfrCheck.o \
$(OBJDIR)/InputOutput.o \
$(OBJDIR)/ModelPackage.o \
$(OBJDIR)/ArrayReadersMF5.o \
$(OBJDIR)/Utilities.o \
$(OBJDIR)/precutls.o \
$(OBJDIR)/GwfFhbSubs.o \
$(OBJDIR)/MemoryManager.o \
$(OBJDIR)/MultiLayerObsModule.o \
$(OBJDIR)/Lake.o \
$(OBJDIR)/ObservePHMF.o \
$(OBJDIR)/File.o \
$(OBJDIR)/GwfResSubs.o \
$(OBJDIR)/LineList.o \
$(OBJDIR)/BlockParser.o \
$(OBJDIR)/FileWriter.o \
$(OBJDIR)/DiscretizationBasePHMF.o \
$(OBJDIR)/IcWriter.o \
$(OBJDIR)/FileList.o \
$(OBJDIR)/StoWriter.o \
$(OBJDIR)/TimeSeries.o \
$(OBJDIR)/Discretization3D.o \
$(OBJDIR)/utl7.o \
$(OBJDIR)/TdisWriter.o \
$(OBJDIR)/GwfEvtSubs.o \
$(OBJDIR)/ObsBlock.o \
$(OBJDIR)/GwfRchSubs.o \
$(OBJDIR)/gwf2wel7_NWT.o \
$(OBJDIR)/GwfChdSubs.o \
$(OBJDIR)/GwfGhbSubs.o \
$(OBJDIR)/GwfLakSubs.o \
$(OBJDIR)/DisWriter.o \
$(OBJDIR)/obs2ghb7.o \
$(OBJDIR)/Mover.o \
$(OBJDIR)/obs2bas7.o \
$(OBJDIR)/GwfSfrSubs.o \
$(OBJDIR)/NWT1_gmres.o \
$(OBJDIR)/ChdType.o \
$(OBJDIR)/sip7.o \
$(OBJDIR)/TdisVariables.o \
$(OBJDIR)/NpfWriter.o \
$(OBJDIR)/pcg7.o \
$(OBJDIR)/NWT1_xmd.o \
$(OBJDIR)/obs2chd7.o \
$(OBJDIR)/GwfUzfSubs_NWT.o \
$(OBJDIR)/GwfEtsSubs.o \
$(OBJDIR)/parutl7.o \
$(OBJDIR)/GwfRivSubs.o \
$(OBJDIR)/Preproc.o \
$(OBJDIR)/obs2riv7.o \
$(OBJDIR)/GwfHfbSubs_NWT.o \
$(OBJDIR)/GwfLgrSubs.o \
$(OBJDIR)/GwfMnwSubs.o \
$(OBJDIR)/NWT1_solver.o \
$(OBJDIR)/GwfDrnSubs.o \
$(OBJDIR)/gmg7.o \
$(OBJDIR)/de47.o \
$(OBJDIR)/GwfBasOcSubs.o \
$(OBJDIR)/obs2drn7.o \
$(OBJDIR)/gwf2mnw17.o \
$(OBJDIR)/gwf2mnw2i7.o \
$(OBJDIR)/ObsWriter.o \
$(OBJDIR)/ChdObsWriter.o \
$(OBJDIR)/RivObsWriter.o \
$(OBJDIR)/DrnObsWriter.o \
$(OBJDIR)/GhbObsWriter.o \
$(OBJDIR)/PackageWriter.o \
$(OBJDIR)/WelPackageWriter.o \
$(OBJDIR)/ChdPackageWriter.o \
$(OBJDIR)/HfbPackageWriter.o \
$(OBJDIR)/DrnPackageWriter.o \
$(OBJDIR)/MvrPackageWriter.o \
$(OBJDIR)/MawPackageWriter.o \
$(OBJDIR)/FhbPackageWriter.o \
$(OBJDIR)/OutputControlWriter.o \
$(OBJDIR)/EvtPackageWriter.o \
$(OBJDIR)/RchPackageWriter.o \
$(OBJDIR)/RivPackageWriter.o \
$(OBJDIR)/GhbPackageWriter.o \
$(OBJDIR)/ImsPackageWriter.o \
$(OBJDIR)/LakPackageWriter.o \
$(OBJDIR)/SfrPackageWriter.o \
$(OBJDIR)/UzfPackageWriter.o \
$(OBJDIR)/Model.o \
$(OBJDIR)/GwfUpwSubs.o \
$(OBJDIR)/GwfBasSubs.o \
$(OBJDIR)/GwfBcfSubs.o \
$(OBJDIR)/Exchange.o \
$(OBJDIR)/GwfLpfSubs.o \
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

