# makefile created on 2021-11-20 19:21:51.370270
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
$(OBJDIR)/OpenSpec.o \
$(OBJDIR)/GwfEvtModule.o \
$(OBJDIR)/gwf2hfb7_NWT.o \
$(OBJDIR)/mach_mod.o \
$(OBJDIR)/kind.o \
$(OBJDIR)/GwfGhbModule.o \
$(OBJDIR)/GwfFhbModule.o \
$(OBJDIR)/GwfLgrModule.o \
$(OBJDIR)/GwfChdModule.o \
$(OBJDIR)/GwfMnwModule.o \
$(OBJDIR)/StressPeriod.o \
$(OBJDIR)/GwfWelModule.o \
$(OBJDIR)/GwfLpfModule.o \
$(OBJDIR)/GwfResModule.o \
$(OBJDIR)/ParamModule.o \
$(OBJDIR)/GwfBcfModule.o \
$(OBJDIR)/NWT1_module.o \
$(OBJDIR)/Constants.o \
$(OBJDIR)/GlobalPHMF.o \
$(OBJDIR)/NWT1_xmdlib.o \
$(OBJDIR)/ConverterCommon.o \
$(OBJDIR)/GwfRivModule.o \
$(OBJDIR)/SimVariables.o \
$(OBJDIR)/ConstantsPHMF.o \
$(OBJDIR)/GwfDrnModule.o \
$(OBJDIR)/GwfUzfModule_NWT.o \
$(OBJDIR)/compilerversion.o \
$(OBJDIR)/GwfEtsModule.o \
$(OBJDIR)/GwfUpwModule.o \
$(OBJDIR)/genericutils.o \
$(OBJDIR)/GwfLakModule.o \
$(OBJDIR)/GwfRchModule.o \
$(OBJDIR)/List.o \
$(OBJDIR)/Connection.o \
$(OBJDIR)/LakeTributary.o \
$(OBJDIR)/version.o \
$(OBJDIR)/GlobalVariablesPHMF.o \
$(OBJDIR)/GwfSfrModule.o \
$(OBJDIR)/Auxiliary.o \
$(OBJDIR)/CharacterContainer.o \
$(OBJDIR)/TimeSeriesRecord.o \
$(OBJDIR)/Global.o \
$(OBJDIR)/LakeOutlet.o \
$(OBJDIR)/SimListVariables.o \
$(OBJDIR)/SfrDiversion.o \
$(OBJDIR)/GlobalVariables.o \
$(OBJDIR)/LakeConnection.o \
$(OBJDIR)/ArrayHandlers.o \
$(OBJDIR)/GwfBasModule.o \
$(OBJDIR)/ModelPackage.o \
$(OBJDIR)/NWT1_ilupc_mod.o \
$(OBJDIR)/pcgn2.o \
$(OBJDIR)/SfrReach.o \
$(OBJDIR)/SimPHMF.o \
$(OBJDIR)/MemoryHelper.o \
$(OBJDIR)/MultiLayerObsModule.o \
$(OBJDIR)/GwfSfrCheck.o \
$(OBJDIR)/InputOutput.o \
$(OBJDIR)/GwfFhbSubs.o \
$(OBJDIR)/SfrSegment.o \
$(OBJDIR)/TableTerm.o \
$(OBJDIR)/Utilities.o \
$(OBJDIR)/LineList.o \
$(OBJDIR)/SimVariablesPHMF.o \
$(OBJDIR)/Lake.o \
$(OBJDIR)/ObservePHMF.o \
$(OBJDIR)/BlockParser.o \
$(OBJDIR)/ArrayReadersMF5.o \
$(OBJDIR)/GwfResSubs.o \
$(OBJDIR)/Table.o \
$(OBJDIR)/precutls.o \
$(OBJDIR)/Memory.o \
$(OBJDIR)/TimeSeries.o \
$(OBJDIR)/File.o \
$(OBJDIR)/ChdType.o \
$(OBJDIR)/FileWriter.o \
$(OBJDIR)/FileList.o \
$(OBJDIR)/TdisWriter.o \
$(OBJDIR)/MemoryList.o \
$(OBJDIR)/StoWriter.o \
$(OBJDIR)/MemoryManager.o \
$(OBJDIR)/TdisVariables.o \
$(OBJDIR)/DiscretizationBasePHMF.o \
$(OBJDIR)/NpfWriter.o \
$(OBJDIR)/utl7.o \
$(OBJDIR)/GwfBasOcSubs.o \
$(OBJDIR)/GwfUzfSubs_NWT.o \
$(OBJDIR)/GwfChdSubs.o \
$(OBJDIR)/IcWriter.o \
$(OBJDIR)/parutl7.o \
$(OBJDIR)/DisWriter.o \
$(OBJDIR)/GwfLgrSubs.o \
$(OBJDIR)/GwfGhbSubs.o \
$(OBJDIR)/sip7.o \
$(OBJDIR)/GwfSfrSubs.o \
$(OBJDIR)/gmg7.o \
$(OBJDIR)/GwfMnwSubs.o \
$(OBJDIR)/GwfDrnSubs.o \
$(OBJDIR)/GwfLakSubs.o \
$(OBJDIR)/obs2chd7.o \
$(OBJDIR)/GwfEvtSubs.o \
$(OBJDIR)/obs2ghb7.o \
$(OBJDIR)/GwfRchSubs.o \
$(OBJDIR)/gwf2wel7_NWT.o \
$(OBJDIR)/Discretization3D.o \
$(OBJDIR)/obs2drn7.o \
$(OBJDIR)/gwf2mnw2i7.o \
$(OBJDIR)/obs2riv7.o \
$(OBJDIR)/GwfRivSubs.o \
$(OBJDIR)/Mover.o \
$(OBJDIR)/de47.o \
$(OBJDIR)/NWT1_gmres.o \
$(OBJDIR)/ObsBlock.o \
$(OBJDIR)/GwfHfbSubs_NWT.o \
$(OBJDIR)/pcg7.o \
$(OBJDIR)/Preproc.o \
$(OBJDIR)/NWT1_xmd.o \
$(OBJDIR)/obs2bas7.o \
$(OBJDIR)/GwfEtsSubs.o \
$(OBJDIR)/NWT1_solver.o \
$(OBJDIR)/gwf2mnw17.o \
$(OBJDIR)/ObsWriter.o \
$(OBJDIR)/GhbObsWriter.o \
$(OBJDIR)/PackageWriter.o \
$(OBJDIR)/RivObsWriter.o \
$(OBJDIR)/ChdObsWriter.o \
$(OBJDIR)/RchPackageWriter.o \
$(OBJDIR)/DrnObsWriter.o \
$(OBJDIR)/WelPackageWriter.o \
$(OBJDIR)/ImsPackageWriter.o \
$(OBJDIR)/MawPackageWriter.o \
$(OBJDIR)/HfbPackageWriter.o \
$(OBJDIR)/EvtPackageWriter.o \
$(OBJDIR)/ChdPackageWriter.o \
$(OBJDIR)/GhbPackageWriter.o \
$(OBJDIR)/MvrPackageWriter.o \
$(OBJDIR)/OutputControlWriter.o \
$(OBJDIR)/RivPackageWriter.o \
$(OBJDIR)/LakPackageWriter.o \
$(OBJDIR)/SfrPackageWriter.o \
$(OBJDIR)/DrnPackageWriter.o \
$(OBJDIR)/FhbPackageWriter.o \
$(OBJDIR)/UzfPackageWriter.o \
$(OBJDIR)/Model.o \
$(OBJDIR)/GwfBasSubs.o \
$(OBJDIR)/GwfUpwSubs.o \
$(OBJDIR)/GwfLpfSubs.o \
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

