# makefile created by pymake (version 1.2.1) for the 'mf5to6' executable.


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

.SUFFIXES: .F90 .f .f90 .o

OBJECTS = \
$(OBJDIR)/GwfGhbModule.o \
$(OBJDIR)/GwfLgrModule.o \
$(OBJDIR)/GwfFhbModule.o \
$(OBJDIR)/GwfChdModule.o \
$(OBJDIR)/GwfMnwModule.o \
$(OBJDIR)/StressPeriod.o \
$(OBJDIR)/GwfWelModule.o \
$(OBJDIR)/ConverterCommon.o \
$(OBJDIR)/GwfLpfModule.o \
$(OBJDIR)/ParamModule.o \
$(OBJDIR)/GwfResModule.o \
$(OBJDIR)/GwfBcfModule.o \
$(OBJDIR)/GlobalPHMF.o \
$(OBJDIR)/NWT1_xmdlib.o \
$(OBJDIR)/GwfRivModule.o \
$(OBJDIR)/ConstantsPHMF.o \
$(OBJDIR)/mach_mod.o \
$(OBJDIR)/GwfUzfModule_NWT.o \
$(OBJDIR)/GwfDrnModule.o \
$(OBJDIR)/GwfEtsModule.o \
$(OBJDIR)/GwfUpwModule.o \
$(OBJDIR)/GwfLakModule.o \
$(OBJDIR)/GwfRchModule.o \
$(OBJDIR)/kind.o \
$(OBJDIR)/GwfEvtModule.o \
$(OBJDIR)/OpenSpec.o \
$(OBJDIR)/GlobalVariablesPHMF.o \
$(OBJDIR)/gwf2hfb7_NWT.o \
$(OBJDIR)/NWT1_module.o \
$(OBJDIR)/Constants.o \
$(OBJDIR)/Connection.o \
$(OBJDIR)/SimVariables.o \
$(OBJDIR)/compilerversion.o \
$(OBJDIR)/genericutils.o \
$(OBJDIR)/List.o \
$(OBJDIR)/SfrDiversion.o \
$(OBJDIR)/GwfSfrModule.o \
$(OBJDIR)/version.o \
$(OBJDIR)/Auxiliary.o \
$(OBJDIR)/CharacterContainer.o \
$(OBJDIR)/TimeSeriesRecord.o \
$(OBJDIR)/GlobalVariables.o \
$(OBJDIR)/LakeTributary.o \
$(OBJDIR)/Global.o \
$(OBJDIR)/LakeOutlet.o \
$(OBJDIR)/SimListVariables.o \
$(OBJDIR)/LakeConnection.o \
$(OBJDIR)/ArrayHandlers.o \
$(OBJDIR)/GwfBasModule.o \
$(OBJDIR)/NWT1_ilupc_mod.o \
$(OBJDIR)/ModelPackage.o \
$(OBJDIR)/pcgn2.o \
$(OBJDIR)/SfrReach.o \
$(OBJDIR)/SimPHMF.o \
$(OBJDIR)/MemoryHelper.o \
$(OBJDIR)/MultiLayerObsModule.o \
$(OBJDIR)/GwfSfrCheck.o \
$(OBJDIR)/SfrSegment.o \
$(OBJDIR)/SimVariablesPHMF.o \
$(OBJDIR)/InputOutput.o \
$(OBJDIR)/Utilities.o \
$(OBJDIR)/LineList.o \
$(OBJDIR)/GwfFhbSubs.o \
$(OBJDIR)/ObservePHMF.o \
$(OBJDIR)/TableTerm.o \
$(OBJDIR)/Lake.o \
$(OBJDIR)/BlockParser.o \
$(OBJDIR)/ArrayReadersMF5.o \
$(OBJDIR)/GwfResSubs.o \
$(OBJDIR)/precutls.o \
$(OBJDIR)/Table.o \
$(OBJDIR)/Memory.o \
$(OBJDIR)/TimeSeries.o \
$(OBJDIR)/File.o \
$(OBJDIR)/ChdType.o \
$(OBJDIR)/FileList.o \
$(OBJDIR)/MemoryList.o \
$(OBJDIR)/utl7.o \
$(OBJDIR)/parutl7.o \
$(OBJDIR)/GwfLgrSubs.o \
$(OBJDIR)/GwfGhbSubs.o \
$(OBJDIR)/sip7.o \
$(OBJDIR)/gmg7.o \
$(OBJDIR)/GwfSfrSubs.o \
$(OBJDIR)/GwfMnwSubs.o \
$(OBJDIR)/FileWriter.o \
$(OBJDIR)/GwfDrnSubs.o \
$(OBJDIR)/GwfLakSubs.o \
$(OBJDIR)/obs2chd7.o \
$(OBJDIR)/GwfEvtSubs.o \
$(OBJDIR)/TdisWriter.o \
$(OBJDIR)/obs2ghb7.o \
$(OBJDIR)/MemoryManager.o \
$(OBJDIR)/GwfRchSubs.o \
$(OBJDIR)/StoWriter.o \
$(OBJDIR)/TdisVariables.o \
$(OBJDIR)/obs2drn7.o \
$(OBJDIR)/gwf2mnw2i7.o \
$(OBJDIR)/DiscretizationBasePHMF.o \
$(OBJDIR)/obs2riv7.o \
$(OBJDIR)/GwfRivSubs.o \
$(OBJDIR)/Mover.o \
$(OBJDIR)/de47.o \
$(OBJDIR)/NWT1_gmres.o \
$(OBJDIR)/pcg7.o \
$(OBJDIR)/gwf2wel7_NWT.o \
$(OBJDIR)/gwf2mnw17.o \
$(OBJDIR)/GwfHfbSubs_NWT.o \
$(OBJDIR)/GwfEtsSubs.o \
$(OBJDIR)/NWT1_xmd.o \
$(OBJDIR)/obs2bas7.o \
$(OBJDIR)/DisWriter.o \
$(OBJDIR)/NWT1_solver.o \
$(OBJDIR)/GwfBasOcSubs.o \
$(OBJDIR)/GwfUzfSubs_NWT.o \
$(OBJDIR)/GwfChdSubs.o \
$(OBJDIR)/IcWriter.o \
$(OBJDIR)/NpfWriter.o \
$(OBJDIR)/Discretization3D.o \
$(OBJDIR)/ObsBlock.o \
$(OBJDIR)/Preproc.o \
$(OBJDIR)/ObsWriter.o \
$(OBJDIR)/ChdObsWriter.o \
$(OBJDIR)/GhbObsWriter.o \
$(OBJDIR)/PackageWriter.o \
$(OBJDIR)/DrnObsWriter.o \
$(OBJDIR)/HfbPackageWriter.o \
$(OBJDIR)/RchPackageWriter.o \
$(OBJDIR)/WelPackageWriter.o \
$(OBJDIR)/ImsPackageWriter.o \
$(OBJDIR)/MawPackageWriter.o \
$(OBJDIR)/MvrPackageWriter.o \
$(OBJDIR)/RivObsWriter.o \
$(OBJDIR)/EvtPackageWriter.o \
$(OBJDIR)/ChdPackageWriter.o \
$(OBJDIR)/GhbPackageWriter.o \
$(OBJDIR)/OutputControlWriter.o \
$(OBJDIR)/LakPackageWriter.o \
$(OBJDIR)/RivPackageWriter.o \
$(OBJDIR)/DrnPackageWriter.o \
$(OBJDIR)/FhbPackageWriter.o \
$(OBJDIR)/SfrPackageWriter.o \
$(OBJDIR)/UzfPackageWriter.o \
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

$(OBJDIR)/%.o : %.F90
	@mkdir -p $(@D)
	$(FC) $(OPTLEVEL) $(FFLAGS) -c $< -o $@ $(INCSWITCH) $(MODSWITCH)

$(OBJDIR)/%.o : %.f
	@mkdir -p $(@D)
	$(FC) $(OPTLEVEL) $(FFLAGS) -c $< -o $@ $(INCSWITCH) $(MODSWITCH)

$(OBJDIR)/%.o : %.f90
	@mkdir -p $(@D)
	$(FC) $(OPTLEVEL) $(FFLAGS) -c $< -o $@ $(INCSWITCH) $(MODSWITCH)

