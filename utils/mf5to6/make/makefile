# makefile created on 2021-11-21 18:38:49.158619
# by pymake (version 1.2.1) for the 'mf5to6' executable.


include ./makedefaults

# Define the source file directories
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

.SUFFIXES: .f .f90 .F90 .o

OBJECTS = \
$(OBJDIR)/GwfEvtModule.o \
$(OBJDIR)/mach_mod.o \
$(OBJDIR)/OpenSpec.o \
$(OBJDIR)/gwf2hfb7_NWT.o \
$(OBJDIR)/GwfGhbModule.o \
$(OBJDIR)/GwfFhbModule.o \
$(OBJDIR)/GwfLgrModule.o \
$(OBJDIR)/ConverterCommon.o \
$(OBJDIR)/GwfChdModule.o \
$(OBJDIR)/ParamModule.o \
$(OBJDIR)/GwfMnwModule.o \
$(OBJDIR)/StressPeriod.o \
$(OBJDIR)/GwfWelModule.o \
$(OBJDIR)/kind.o \
$(OBJDIR)/GwfLpfModule.o \
$(OBJDIR)/GwfResModule.o \
$(OBJDIR)/GwfBcfModule.o \
$(OBJDIR)/NWT1_module.o \
$(OBJDIR)/Constants.o \
$(OBJDIR)/NWT1_xmdlib.o \
$(OBJDIR)/GwfRivModule.o \
$(OBJDIR)/GlobalPHMF.o \
$(OBJDIR)/SimVariables.o \
$(OBJDIR)/GwfUzfModule_NWT.o \
$(OBJDIR)/compilerversion.o \
$(OBJDIR)/ConstantsPHMF.o \
$(OBJDIR)/GwfDrnModule.o \
$(OBJDIR)/GwfUpwModule.o \
$(OBJDIR)/genericutils.o \
$(OBJDIR)/GwfEtsModule.o \
$(OBJDIR)/GwfLakModule.o \
$(OBJDIR)/GwfRchModule.o \
$(OBJDIR)/List.o \
$(OBJDIR)/LakeTributary.o \
$(OBJDIR)/Connection.o \
$(OBJDIR)/GlobalVariablesPHMF.o \
$(OBJDIR)/version.o \
$(OBJDIR)/GwfSfrModule.o \
$(OBJDIR)/Auxiliary.o \
$(OBJDIR)/CharacterContainer.o \
$(OBJDIR)/TimeSeriesRecord.o \
$(OBJDIR)/Global.o \
$(OBJDIR)/LakeOutlet.o \
$(OBJDIR)/SimListVariables.o \
$(OBJDIR)/SfrDiversion.o \
$(OBJDIR)/ModelPackage.o \
$(OBJDIR)/GlobalVariables.o \
$(OBJDIR)/LakeConnection.o \
$(OBJDIR)/GwfBasModule.o \
$(OBJDIR)/ArrayHandlers.o \
$(OBJDIR)/NWT1_ilupc_mod.o \
$(OBJDIR)/pcgn2.o \
$(OBJDIR)/SfrReach.o \
$(OBJDIR)/SimPHMF.o \
$(OBJDIR)/MemoryHelper.o \
$(OBJDIR)/MultiLayerObsModule.o \
$(OBJDIR)/GwfSfrCheck.o \
$(OBJDIR)/InputOutput.o \
$(OBJDIR)/SfrSegment.o \
$(OBJDIR)/SimVariablesPHMF.o \
$(OBJDIR)/GwfFhbSubs.o \
$(OBJDIR)/TableTerm.o \
$(OBJDIR)/LineList.o \
$(OBJDIR)/Utilities.o \
$(OBJDIR)/ObservePHMF.o \
$(OBJDIR)/BlockParser.o \
$(OBJDIR)/Lake.o \
$(OBJDIR)/ArrayReadersMF5.o \
$(OBJDIR)/GwfResSubs.o \
$(OBJDIR)/Table.o \
$(OBJDIR)/precutls.o \
$(OBJDIR)/Memory.o \
$(OBJDIR)/TimeSeries.o \
$(OBJDIR)/File.o \
$(OBJDIR)/ChdType.o \
$(OBJDIR)/FileWriter.o \
$(OBJDIR)/MemoryList.o \
$(OBJDIR)/FileList.o \
$(OBJDIR)/TdisWriter.o \
$(OBJDIR)/StoWriter.o \
$(OBJDIR)/MemoryManager.o \
$(OBJDIR)/TdisVariables.o \
$(OBJDIR)/DiscretizationBasePHMF.o \
$(OBJDIR)/DisWriter.o \
$(OBJDIR)/utl7.o \
$(OBJDIR)/GwfBasOcSubs.o \
$(OBJDIR)/GwfChdSubs.o \
$(OBJDIR)/GwfUzfSubs_NWT.o \
$(OBJDIR)/parutl7.o \
$(OBJDIR)/IcWriter.o \
$(OBJDIR)/GwfGhbSubs.o \
$(OBJDIR)/sip7.o \
$(OBJDIR)/GwfLgrSubs.o \
$(OBJDIR)/GwfMnwSubs.o \
$(OBJDIR)/GwfSfrSubs.o \
$(OBJDIR)/gmg7.o \
$(OBJDIR)/GwfLakSubs.o \
$(OBJDIR)/obs2chd7.o \
$(OBJDIR)/GwfDrnSubs.o \
$(OBJDIR)/NpfWriter.o \
$(OBJDIR)/GwfEvtSubs.o \
$(OBJDIR)/GwfRchSubs.o \
$(OBJDIR)/obs2ghb7.o \
$(OBJDIR)/obs2drn7.o \
$(OBJDIR)/Discretization3D.o \
$(OBJDIR)/GwfHfbSubs_NWT.o \
$(OBJDIR)/obs2riv7.o \
$(OBJDIR)/gwf2mnw2i7.o \
$(OBJDIR)/GwfRivSubs.o \
$(OBJDIR)/NWT1_gmres.o \
$(OBJDIR)/Mover.o \
$(OBJDIR)/de47.o \
$(OBJDIR)/ObsBlock.o \
$(OBJDIR)/pcg7.o \
$(OBJDIR)/Preproc.o \
$(OBJDIR)/gwf2wel7_NWT.o \
$(OBJDIR)/gwf2mnw17.o \
$(OBJDIR)/GwfEtsSubs.o \
$(OBJDIR)/NWT1_xmd.o \
$(OBJDIR)/obs2bas7.o \
$(OBJDIR)/NWT1_solver.o \
$(OBJDIR)/ObsWriter.o \
$(OBJDIR)/ChdObsWriter.o \
$(OBJDIR)/PackageWriter.o \
$(OBJDIR)/DrnPackageWriter.o \
$(OBJDIR)/RivObsWriter.o \
$(OBJDIR)/DrnObsWriter.o \
$(OBJDIR)/RchPackageWriter.o \
$(OBJDIR)/GhbObsWriter.o \
$(OBJDIR)/WelPackageWriter.o \
$(OBJDIR)/RivPackageWriter.o \
$(OBJDIR)/ImsPackageWriter.o \
$(OBJDIR)/MawPackageWriter.o \
$(OBJDIR)/EvtPackageWriter.o \
$(OBJDIR)/ChdPackageWriter.o \
$(OBJDIR)/GhbPackageWriter.o \
$(OBJDIR)/HfbPackageWriter.o \
$(OBJDIR)/MvrPackageWriter.o \
$(OBJDIR)/OutputControlWriter.o \
$(OBJDIR)/LakPackageWriter.o \
$(OBJDIR)/SfrPackageWriter.o \
$(OBJDIR)/UzfPackageWriter.o \
$(OBJDIR)/FhbPackageWriter.o \
$(OBJDIR)/Model.o \
$(OBJDIR)/Exchange.o \
$(OBJDIR)/GwfUpwSubs.o \
$(OBJDIR)/GwfLpfSubs.o \
$(OBJDIR)/GwfBasSubs.o \
$(OBJDIR)/GwfBcfSubs.o \
$(OBJDIR)/ModelConverter.o \
$(OBJDIR)/ExchangeWriter.o \
$(OBJDIR)/SimFileWriter.o \
$(OBJDIR)/mf5to6.o

# Define the objects that make up the program
$(PROGRAM) : $(OBJECTS)
	-$(FC) $(OPTLEVEL) -o $@ $(OBJECTS) $(LDFLAGS)

$(OBJDIR)/%.o : %.f
	@mkdir -p $(@D)
	$(FC) $(OPTLEVEL) $(FFLAGS) -c $< -o $@ $(INCSWITCH) $(MODSWITCH)

$(OBJDIR)/%.o : %.f90
	@mkdir -p $(@D)
	$(FC) $(OPTLEVEL) $(FFLAGS) -c $< -o $@ $(INCSWITCH) $(MODSWITCH)

$(OBJDIR)/%.o : %.F90
	@mkdir -p $(@D)
	$(FC) $(OPTLEVEL) $(FFLAGS) -c $< -o $@ $(INCSWITCH) $(MODSWITCH)

