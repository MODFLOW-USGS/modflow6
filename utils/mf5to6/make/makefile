# makefile created on 2020-06-08 12:36:37.282790
# by pymake (version 1.1.0)
# using the gfortran fortran and gcc c/c++ compilers.

# Define the directories for the object and module files,
# the executable, and the executable name and path.
OBJDIR = ./obj_temp
BINDIR = .
PROGRAM = mf5to6

SOURCEDIR1=../src
SOURCEDIR2=../src/NWT
SOURCEDIR3=../src/LGR
SOURCEDIR4=../src/Preproc
SOURCEDIR5=../src/MF2005
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
FFLAGS = -O2 -ffpe-summary=overflow -ffpe-trap=overflow,zero,invalid -fbacktrace -D__APPLE__ -MMD -cpp 

# Define the C compile flags
CC = gcc
CFLAGS = -O2 -D_UF -MMD -cpp 

# Define the libraries
SYSLIBS = -lc 

OBJECTS = \
$(OBJDIR)/GwfUzfModule_NWT.o \
$(OBJDIR)/GwfBcfModule.o \
$(OBJDIR)/GwfLgrModule.o \
$(OBJDIR)/GwfRchModule.o \
$(OBJDIR)/GwfUpwModule.o \
$(OBJDIR)/GwfWelModule.o \
$(OBJDIR)/GwfMnwModule.o \
$(OBJDIR)/GwfGhbModule.o \
$(OBJDIR)/kind.o \
$(OBJDIR)/GwfLakModule.o \
$(OBJDIR)/GwfDrnModule.o \
$(OBJDIR)/GwfEvtModule.o \
$(OBJDIR)/GwfEtsModule.o \
$(OBJDIR)/GwfChdModule.o \
$(OBJDIR)/GlobalPHMF.o \
$(OBJDIR)/GwfLpfModule.o \
$(OBJDIR)/ParamModule.o \
$(OBJDIR)/StressPeriod.o \
$(OBJDIR)/ConverterCommon.o \
$(OBJDIR)/NWT1_xmdlib.o \
$(OBJDIR)/GwfFhbModule.o \
$(OBJDIR)/version.o \
$(OBJDIR)/OpenSpec.o \
$(OBJDIR)/GwfResModule.o \
$(OBJDIR)/gwf2hfb7_NWT.o \
$(OBJDIR)/mach_mod.o \
$(OBJDIR)/ConstantsPHMF.o \
$(OBJDIR)/GwfRivModule.o \
$(OBJDIR)/GlobalVariablesPHMF.o \
$(OBJDIR)/Constants.o \
$(OBJDIR)/SimVariables.o \
$(OBJDIR)/genericutils.o \
$(OBJDIR)/Global.o \
$(OBJDIR)/GwfSfrModule.o \
$(OBJDIR)/Connection.o \
$(OBJDIR)/NWT1_module.o \
$(OBJDIR)/GlobalVariables.o \
$(OBJDIR)/ArrayHandlers.o \
$(OBJDIR)/GwfBasModule.o \
$(OBJDIR)/SimPHMF.o \
$(OBJDIR)/NWT1_ilupc_mod.o \
$(OBJDIR)/pcgn2.o \
$(OBJDIR)/List.o \
$(OBJDIR)/SfrDiversion.o \
$(OBJDIR)/GwfSfrCheck.o \
$(OBJDIR)/LakeConnection.o \
$(OBJDIR)/SimVariablesPHMF.o \
$(OBJDIR)/LakeOutlet.o \
$(OBJDIR)/InputOutput.o \
$(OBJDIR)/Auxiliary.o \
$(OBJDIR)/MultiLayerObsModule.o \
$(OBJDIR)/TimeSeriesRecord.o \
$(OBJDIR)/SimListVariables.o \
$(OBJDIR)/Lake.o \
$(OBJDIR)/ModelPackage.o \
$(OBJDIR)/Utilities.o \
$(OBJDIR)/LakeTributary.o \
$(OBJDIR)/CharacterContainer.o \
$(OBJDIR)/TableTerm.o \
$(OBJDIR)/precutls.o \
$(OBJDIR)/ObservePHMF.o \
$(OBJDIR)/GwfFhbSubs.o \
$(OBJDIR)/LineList.o \
$(OBJDIR)/BlockParser.o \
$(OBJDIR)/ArrayReadersMF5.o \
$(OBJDIR)/Table.o \
$(OBJDIR)/SfrReach.o \
$(OBJDIR)/File.o \
$(OBJDIR)/GwfResSubs.o \
$(OBJDIR)/SfrSegment.o \
$(OBJDIR)/FileWriter.o \
$(OBJDIR)/DisWriter.o \
$(OBJDIR)/TimeSeries.o \
$(OBJDIR)/StoWriter.o \
$(OBJDIR)/TdisWriter.o \
$(OBJDIR)/Memory.o \
$(OBJDIR)/TdisVariables.o \
$(OBJDIR)/FileList.o \
$(OBJDIR)/ChdType.o \
$(OBJDIR)/IcWriter.o \
$(OBJDIR)/MemoryList.o \
$(OBJDIR)/utl7.o \
$(OBJDIR)/GwfDrnSubs.o \
$(OBJDIR)/gwf2wel7_NWT.o \
$(OBJDIR)/obs2drn7.o \
$(OBJDIR)/gmg7.o \
$(OBJDIR)/NWT1_solver.o \
$(OBJDIR)/parutl7.o \
$(OBJDIR)/de47.o \
$(OBJDIR)/NpfWriter.o \
$(OBJDIR)/GwfGhbSubs.o \
$(OBJDIR)/GwfLakSubs.o \
$(OBJDIR)/GwfHfbSubs_NWT.o \
$(OBJDIR)/GwfRchSubs.o \
$(OBJDIR)/pcg7.o \
$(OBJDIR)/GwfLgrSubs.o \
$(OBJDIR)/obs2bas7.o \
$(OBJDIR)/obs2chd7.o \
$(OBJDIR)/GwfChdSubs.o \
$(OBJDIR)/GwfEtsSubs.o \
$(OBJDIR)/GwfMnwSubs.o \
$(OBJDIR)/obs2ghb7.o \
$(OBJDIR)/obs2riv7.o \
$(OBJDIR)/MemoryManager.o \
$(OBJDIR)/GwfRivSubs.o \
$(OBJDIR)/GwfUzfSubs_NWT.o \
$(OBJDIR)/GwfBasOcSubs.o \
$(OBJDIR)/GwfEvtSubs.o \
$(OBJDIR)/NWT1_xmd.o \
$(OBJDIR)/GwfSfrSubs.o \
$(OBJDIR)/Mover.o \
$(OBJDIR)/sip7.o \
$(OBJDIR)/NWT1_gmres.o \
$(OBJDIR)/DiscretizationBasePHMF.o \
$(OBJDIR)/gwf2mnw17.o \
$(OBJDIR)/gwf2mnw2i7.o \
$(OBJDIR)/Discretization3D.o \
$(OBJDIR)/ObsBlock.o \
$(OBJDIR)/Preproc.o \
$(OBJDIR)/ObsWriter.o \
$(OBJDIR)/PackageWriter.o \
$(OBJDIR)/RchPackageWriter.o \
$(OBJDIR)/RivPackageWriter.o \
$(OBJDIR)/ImsPackageWriter.o \
$(OBJDIR)/WelPackageWriter.o \
$(OBJDIR)/ChdObsWriter.o \
$(OBJDIR)/GhbPackageWriter.o \
$(OBJDIR)/HfbPackageWriter.o \
$(OBJDIR)/DrnPackageWriter.o \
$(OBJDIR)/OutputControlWriter.o \
$(OBJDIR)/GhbObsWriter.o \
$(OBJDIR)/RivObsWriter.o \
$(OBJDIR)/DrnObsWriter.o \
$(OBJDIR)/MvrPackageWriter.o \
$(OBJDIR)/EvtPackageWriter.o \
$(OBJDIR)/SfrPackageWriter.o \
$(OBJDIR)/MawPackageWriter.o \
$(OBJDIR)/ChdPackageWriter.o \
$(OBJDIR)/LakPackageWriter.o \
$(OBJDIR)/FhbPackageWriter.o \
$(OBJDIR)/UzfPackageWriter.o \
$(OBJDIR)/Model.o \
$(OBJDIR)/GwfBasSubs.o \
$(OBJDIR)/Exchange.o \
$(OBJDIR)/GwfLpfSubs.o \
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

