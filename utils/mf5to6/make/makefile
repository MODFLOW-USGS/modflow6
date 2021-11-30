# makefile created on 2021-11-30 17:15:17.851751
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

.SUFFIXES: .f90 .f .F90 .o

OBJECTS = \
$(OBJDIR)/StressPeriod.o \
$(OBJDIR)/GwfWelModule.o \
$(OBJDIR)/GwfLpfModule.o \
$(OBJDIR)/mach_mod.o \
$(OBJDIR)/GwfResModule.o \
$(OBJDIR)/GwfBcfModule.o \
$(OBJDIR)/NWT1_module.o \
$(OBJDIR)/NWT1_xmdlib.o \
$(OBJDIR)/ConverterCommon.o \
$(OBJDIR)/GlobalPHMF.o \
$(OBJDIR)/GwfRivModule.o \
$(OBJDIR)/GwfUzfModule_NWT.o \
$(OBJDIR)/ConstantsPHMF.o \
$(OBJDIR)/GwfDrnModule.o \
$(OBJDIR)/GwfUpwModule.o \
$(OBJDIR)/GwfEtsModule.o \
$(OBJDIR)/OpenSpec.o \
$(OBJDIR)/GwfLakModule.o \
$(OBJDIR)/GwfRchModule.o \
$(OBJDIR)/GwfEvtModule.o \
$(OBJDIR)/kind.o \
$(OBJDIR)/gwf2hfb7_NWT.o \
$(OBJDIR)/GlobalVariablesPHMF.o \
$(OBJDIR)/GwfGhbModule.o \
$(OBJDIR)/GwfLgrModule.o \
$(OBJDIR)/GwfFhbModule.o \
$(OBJDIR)/GwfChdModule.o \
$(OBJDIR)/ParamModule.o \
$(OBJDIR)/GwfMnwModule.o \
$(OBJDIR)/Constants.o \
$(OBJDIR)/Global.o \
$(OBJDIR)/SimVariables.o \
$(OBJDIR)/genericutils.o \
$(OBJDIR)/Connection.o \
$(OBJDIR)/GwfSfrModule.o \
$(OBJDIR)/compilerversion.o \
$(OBJDIR)/GwfBasModule.o \
$(OBJDIR)/ArrayHandlers.o \
$(OBJDIR)/NWT1_ilupc_mod.o \
$(OBJDIR)/pcgn2.o \
$(OBJDIR)/List.o \
$(OBJDIR)/SimListVariables.o \
$(OBJDIR)/SfrDiversion.o \
$(OBJDIR)/LakeTributary.o \
$(OBJDIR)/LakeConnection.o \
$(OBJDIR)/LakeOutlet.o \
$(OBJDIR)/ModelPackage.o \
$(OBJDIR)/version.o \
$(OBJDIR)/TimeSeriesRecord.o \
$(OBJDIR)/CharacterContainer.o \
$(OBJDIR)/Auxiliary.o \
$(OBJDIR)/SfrReach.o \
$(OBJDIR)/GlobalVariables.o \
$(OBJDIR)/SimPHMF.o \
$(OBJDIR)/InputOutput.o \
$(OBJDIR)/GwfSfrCheck.o \
$(OBJDIR)/ArrayReadersMF5.o \
$(OBJDIR)/TableTerm.o \
$(OBJDIR)/MultiLayerObsModule.o \
$(OBJDIR)/GwfResSubs.o \
$(OBJDIR)/SfrSegment.o \
$(OBJDIR)/SimVariablesPHMF.o \
$(OBJDIR)/Table.o \
$(OBJDIR)/GwfFhbSubs.o \
$(OBJDIR)/MemoryHelper.o \
$(OBJDIR)/LineList.o \
$(OBJDIR)/Utilities.o \
$(OBJDIR)/BlockParser.o \
$(OBJDIR)/Lake.o \
$(OBJDIR)/ObservePHMF.o \
$(OBJDIR)/Memory.o \
$(OBJDIR)/TimeSeries.o \
$(OBJDIR)/ChdType.o \
$(OBJDIR)/precutls.o \
$(OBJDIR)/MemoryList.o \
$(OBJDIR)/File.o \
$(OBJDIR)/FileWriter.o \
$(OBJDIR)/MemoryManager.o \
$(OBJDIR)/DisWriter.o \
$(OBJDIR)/DiscretizationBasePHMF.o \
$(OBJDIR)/FileList.o \
$(OBJDIR)/TdisWriter.o \
$(OBJDIR)/NpfWriter.o \
$(OBJDIR)/IcWriter.o \
$(OBJDIR)/StoWriter.o \
$(OBJDIR)/TdisVariables.o \
$(OBJDIR)/Discretization3D.o \
$(OBJDIR)/utl7.o \
$(OBJDIR)/GwfEtsSubs.o \
$(OBJDIR)/obs2riv7.o \
$(OBJDIR)/GwfRivSubs.o \
$(OBJDIR)/GwfUzfSubs_NWT.o \
$(OBJDIR)/obs2chd7.o \
$(OBJDIR)/NWT1_gmres.o \
$(OBJDIR)/ObsBlock.o \
$(OBJDIR)/Mover.o \
$(OBJDIR)/de47.o \
$(OBJDIR)/GwfChdSubs.o \
$(OBJDIR)/GwfLgrSubs.o \
$(OBJDIR)/pcg7.o \
$(OBJDIR)/GwfGhbSubs.o \
$(OBJDIR)/GwfSfrSubs.o \
$(OBJDIR)/gmg7.o \
$(OBJDIR)/sip7.o \
$(OBJDIR)/GwfDrnSubs.o \
$(OBJDIR)/GwfLakSubs.o \
$(OBJDIR)/Preproc.o \
$(OBJDIR)/GwfEvtSubs.o \
$(OBJDIR)/parutl7.o \
$(OBJDIR)/gwf2wel7_NWT.o \
$(OBJDIR)/gwf2mnw17.o \
$(OBJDIR)/GwfRchSubs.o \
$(OBJDIR)/GwfMnwSubs.o \
$(OBJDIR)/GwfHfbSubs_NWT.o \
$(OBJDIR)/obs2ghb7.o \
$(OBJDIR)/NWT1_xmd.o \
$(OBJDIR)/obs2bas7.o \
$(OBJDIR)/obs2drn7.o \
$(OBJDIR)/NWT1_solver.o \
$(OBJDIR)/GwfBasOcSubs.o \
$(OBJDIR)/gwf2mnw2i7.o \
$(OBJDIR)/ObsWriter.o \
$(OBJDIR)/ChdObsWriter.o \
$(OBJDIR)/GhbObsWriter.o \
$(OBJDIR)/RivObsWriter.o \
$(OBJDIR)/DrnObsWriter.o \
$(OBJDIR)/PackageWriter.o \
$(OBJDIR)/GhbPackageWriter.o \
$(OBJDIR)/WelPackageWriter.o \
$(OBJDIR)/DrnPackageWriter.o \
$(OBJDIR)/MvrPackageWriter.o \
$(OBJDIR)/SfrPackageWriter.o \
$(OBJDIR)/HfbPackageWriter.o \
$(OBJDIR)/EvtPackageWriter.o \
$(OBJDIR)/ChdPackageWriter.o \
$(OBJDIR)/RchPackageWriter.o \
$(OBJDIR)/FhbPackageWriter.o \
$(OBJDIR)/OutputControlWriter.o \
$(OBJDIR)/ImsPackageWriter.o \
$(OBJDIR)/MawPackageWriter.o \
$(OBJDIR)/LakPackageWriter.o \
$(OBJDIR)/UzfPackageWriter.o \
$(OBJDIR)/RivPackageWriter.o \
$(OBJDIR)/Model.o \
$(OBJDIR)/GwfLpfSubs.o \
$(OBJDIR)/GwfBasSubs.o \
$(OBJDIR)/GwfUpwSubs.o \
$(OBJDIR)/GwfBcfSubs.o \
$(OBJDIR)/Exchange.o \
$(OBJDIR)/ModelConverter.o \
$(OBJDIR)/ExchangeWriter.o \
$(OBJDIR)/SimFileWriter.o \
$(OBJDIR)/mf5to6.o

# Define the objects that make up the program
$(PROGRAM) : $(OBJECTS)
	-$(FC) $(OPTLEVEL) -o $@ $(OBJECTS) $(LDFLAGS)

$(OBJDIR)/%.o : %.f90
	@mkdir -p $(@D)
	$(FC) $(OPTLEVEL) $(FFLAGS) -c $< -o $@ $(INCSWITCH) $(MODSWITCH)

$(OBJDIR)/%.o : %.f
	@mkdir -p $(@D)
	$(FC) $(OPTLEVEL) $(FFLAGS) -c $< -o $@ $(INCSWITCH) $(MODSWITCH)

$(OBJDIR)/%.o : %.F90
	@mkdir -p $(@D)
	$(FC) $(OPTLEVEL) $(FFLAGS) -c $< -o $@ $(INCSWITCH) $(MODSWITCH)

