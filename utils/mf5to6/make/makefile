# makefile created on 2020-06-11 17:48:34.879702
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
FFLAGS = -O2 -fbacktrace -Bstatic -D_WIN32 -MMD -cpp 

# Define the C compile flags
CC = gcc
CFLAGS = -O2 -Bstatic -D_UF -MMD -cpp 

# Define the libraries
SYSLIBS = -lgfortran -lm 

OBJECTS = \
$(OBJDIR)/GlobalPHMF.o \
$(OBJDIR)/kind.o \
$(OBJDIR)/GwfGhbModule.o \
$(OBJDIR)/GwfEvtModule.o \
$(OBJDIR)/ConstantsPHMF.o \
$(OBJDIR)/GwfLpfModule.o \
$(OBJDIR)/GwfWelModule.o \
$(OBJDIR)/gwf2hfb7_NWT.o \
$(OBJDIR)/GwfMnwModule.o \
$(OBJDIR)/Constants.o \
$(OBJDIR)/GwfUpwModule.o \
$(OBJDIR)/version.o \
$(OBJDIR)/GwfDrnModule.o \
$(OBJDIR)/OpenSpec.o \
$(OBJDIR)/NWT1_xmdlib.o \
$(OBJDIR)/StressPeriod.o \
$(OBJDIR)/GwfLgrModule.o \
$(OBJDIR)/ConverterCommon.o \
$(OBJDIR)/ParamModule.o \
$(OBJDIR)/mach_mod.o \
$(OBJDIR)/GwfUzfModule_NWT.o \
$(OBJDIR)/GwfRivModule.o \
$(OBJDIR)/GlobalVariables.o \
$(OBJDIR)/GwfChdModule.o \
$(OBJDIR)/GwfRchModule.o \
$(OBJDIR)/Connection.o \
$(OBJDIR)/GwfEtsModule.o \
$(OBJDIR)/GlobalVariablesPHMF.o \
$(OBJDIR)/GwfLakModule.o \
$(OBJDIR)/GwfFhbModule.o \
$(OBJDIR)/GwfBcfModule.o \
$(OBJDIR)/GwfResModule.o \
$(OBJDIR)/Global.o \
$(OBJDIR)/GwfSfrModule.o \
$(OBJDIR)/NWT1_module.o \
$(OBJDIR)/ArrayHandlers.o \
$(OBJDIR)/SimVariables.o \
$(OBJDIR)/genericutils.o \
$(OBJDIR)/pcgn2.o \
$(OBJDIR)/SimPHMF.o \
$(OBJDIR)/GwfSfrCheck.o \
$(OBJDIR)/GwfBasModule.o \
$(OBJDIR)/NWT1_ilupc_mod.o \
$(OBJDIR)/InputOutput.o \
$(OBJDIR)/List.o \
$(OBJDIR)/BlockParser.o \
$(OBJDIR)/SfrDiversion.o \
$(OBJDIR)/Utilities.o \
$(OBJDIR)/SimVariablesPHMF.o \
$(OBJDIR)/GwfFhbSubs.o \
$(OBJDIR)/ArrayReadersMF5.o \
$(OBJDIR)/GwfResSubs.o \
$(OBJDIR)/LakeConnection.o \
$(OBJDIR)/CharacterContainer.o \
$(OBJDIR)/TableTerm.o \
$(OBJDIR)/Auxiliary.o \
$(OBJDIR)/ObservePHMF.o \
$(OBJDIR)/SimListVariables.o \
$(OBJDIR)/LakeOutlet.o \
$(OBJDIR)/MultiLayerObsModule.o \
$(OBJDIR)/SfrReach.o \
$(OBJDIR)/LineList.o \
$(OBJDIR)/TimeSeriesRecord.o \
$(OBJDIR)/Table.o \
$(OBJDIR)/LakeTributary.o \
$(OBJDIR)/Lake.o \
$(OBJDIR)/precutls.o \
$(OBJDIR)/ModelPackage.o \
$(OBJDIR)/Memory.o \
$(OBJDIR)/SfrSegment.o \
$(OBJDIR)/TimeSeries.o \
$(OBJDIR)/File.o \
$(OBJDIR)/FileList.o \
$(OBJDIR)/FileWriter.o \
$(OBJDIR)/utl7.o \
$(OBJDIR)/GwfSfrSubs.o \
$(OBJDIR)/GwfDrnSubs.o \
$(OBJDIR)/obs2bas7.o \
$(OBJDIR)/NpfWriter.o \
$(OBJDIR)/GwfGhbSubs.o \
$(OBJDIR)/GwfMnwSubs.o \
$(OBJDIR)/gwf2wel7_NWT.o \
$(OBJDIR)/parutl7.o \
$(OBJDIR)/obs2riv7.o \
$(OBJDIR)/NWT1_xmd.o \
$(OBJDIR)/GwfEvtSubs.o \
$(OBJDIR)/gmg7.o \
$(OBJDIR)/de47.o \
$(OBJDIR)/GwfLgrSubs.o \
$(OBJDIR)/GwfBasOcSubs.o \
$(OBJDIR)/GwfHfbSubs_NWT.o \
$(OBJDIR)/MemoryList.o \
$(OBJDIR)/GwfRchSubs.o \
$(OBJDIR)/pcg7.o \
$(OBJDIR)/TdisWriter.o \
$(OBJDIR)/obs2chd7.o \
$(OBJDIR)/StoWriter.o \
$(OBJDIR)/IcWriter.o \
$(OBJDIR)/GwfLakSubs.o \
$(OBJDIR)/NWT1_solver.o \
$(OBJDIR)/NWT1_gmres.o \
$(OBJDIR)/ChdType.o \
$(OBJDIR)/GwfRivSubs.o \
$(OBJDIR)/Mover.o \
$(OBJDIR)/obs2drn7.o \
$(OBJDIR)/DisWriter.o \
$(OBJDIR)/obs2ghb7.o \
$(OBJDIR)/TdisVariables.o \
$(OBJDIR)/GwfEtsSubs.o \
$(OBJDIR)/GwfChdSubs.o \
$(OBJDIR)/GwfUzfSubs_NWT.o \
$(OBJDIR)/sip7.o \
$(OBJDIR)/MemoryManager.o \
$(OBJDIR)/gwf2mnw17.o \
$(OBJDIR)/DiscretizationBasePHMF.o \
$(OBJDIR)/gwf2mnw2i7.o \
$(OBJDIR)/Discretization3D.o \
$(OBJDIR)/ObsBlock.o \
$(OBJDIR)/Preproc.o \
$(OBJDIR)/ObsWriter.o \
$(OBJDIR)/PackageWriter.o \
$(OBJDIR)/GhbObsWriter.o \
$(OBJDIR)/RivObsWriter.o \
$(OBJDIR)/ChdObsWriter.o \
$(OBJDIR)/MvrPackageWriter.o \
$(OBJDIR)/DrnPackageWriter.o \
$(OBJDIR)/ImsPackageWriter.o \
$(OBJDIR)/WelPackageWriter.o \
$(OBJDIR)/EvtPackageWriter.o \
$(OBJDIR)/DrnObsWriter.o \
$(OBJDIR)/LakPackageWriter.o \
$(OBJDIR)/OutputControlWriter.o \
$(OBJDIR)/MawPackageWriter.o \
$(OBJDIR)/ChdPackageWriter.o \
$(OBJDIR)/GhbPackageWriter.o \
$(OBJDIR)/RivPackageWriter.o \
$(OBJDIR)/HfbPackageWriter.o \
$(OBJDIR)/SfrPackageWriter.o \
$(OBJDIR)/RchPackageWriter.o \
$(OBJDIR)/FhbPackageWriter.o \
$(OBJDIR)/UzfPackageWriter.o \
$(OBJDIR)/Model.o \
$(OBJDIR)/GwfUpwSubs.o \
$(OBJDIR)/GwfBcfSubs.o \
$(OBJDIR)/GwfLpfSubs.o \
$(OBJDIR)/Exchange.o \
$(OBJDIR)/GwfBasSubs.o \
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

