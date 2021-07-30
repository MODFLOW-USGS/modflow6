# makefile created on 2021-07-30 14:36:43.397584
# by pymake (version 1.2.1) for the 'mf5to6' executable 
# using the 'gfortran' fortran compiler(s).


include ./makedefaults

# Define the source file directories
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

.SUFFIXES: .f .f90 .o

OBJECTS = \
$(OBJDIR)/GwfBcfModule.o \
$(OBJDIR)/GlobalPHMF.o \
$(OBJDIR)/ParamModule.o \
$(OBJDIR)/GwfUpwModule.o \
$(OBJDIR)/GwfChdModule.o \
$(OBJDIR)/GwfFhbModule.o \
$(OBJDIR)/GwfResModule.o \
$(OBJDIR)/gwf2hfb7_NWT.o \
$(OBJDIR)/StressPeriod.o \
$(OBJDIR)/kind.o \
$(OBJDIR)/GwfEtsModule.o \
$(OBJDIR)/GwfWelModule.o \
$(OBJDIR)/GwfRchModule.o \
$(OBJDIR)/ConstantsPHMF.o \
$(OBJDIR)/GwfUzfModule_NWT.o \
$(OBJDIR)/NWT1_xmdlib.o \
$(OBJDIR)/GwfLakModule.o \
$(OBJDIR)/OpenSpec.o \
$(OBJDIR)/GwfEvtModule.o \
$(OBJDIR)/GlobalVariablesPHMF.o \
$(OBJDIR)/GwfRivModule.o \
$(OBJDIR)/GwfLgrModule.o \
$(OBJDIR)/GwfDrnModule.o \
$(OBJDIR)/ConverterCommon.o \
$(OBJDIR)/mach_mod.o \
$(OBJDIR)/GwfGhbModule.o \
$(OBJDIR)/GwfLpfModule.o \
$(OBJDIR)/GwfMnwModule.o \
$(OBJDIR)/compilerversion.o \
$(OBJDIR)/Constants.o \
$(OBJDIR)/NWT1_module.o \
$(OBJDIR)/Global.o \
$(OBJDIR)/ArrayHandlers.o \
$(OBJDIR)/Connection.o \
$(OBJDIR)/NWT1_ilupc_mod.o \
$(OBJDIR)/SimVariables.o \
$(OBJDIR)/GwfSfrModule.o \
$(OBJDIR)/GwfBasModule.o \
$(OBJDIR)/pcgn2.o \
$(OBJDIR)/genericutils.o \
$(OBJDIR)/List.o \
$(OBJDIR)/LakeOutlet.o \
$(OBJDIR)/Auxiliary.o \
$(OBJDIR)/version.o \
$(OBJDIR)/SfrDiversion.o \
$(OBJDIR)/GlobalVariables.o \
$(OBJDIR)/LakeTributary.o \
$(OBJDIR)/SimPHMF.o \
$(OBJDIR)/LakeConnection.o \
$(OBJDIR)/SimVariablesPHMF.o \
$(OBJDIR)/SimListVariables.o \
$(OBJDIR)/TimeSeriesRecord.o \
$(OBJDIR)/SfrReach.o \
$(OBJDIR)/CharacterContainer.o \
$(OBJDIR)/InputOutput.o \
$(OBJDIR)/ModelPackage.o \
$(OBJDIR)/BlockParser.o \
$(OBJDIR)/GwfSfrCheck.o \
$(OBJDIR)/MemoryHelper.o \
$(OBJDIR)/MultiLayerObsModule.o \
$(OBJDIR)/GwfFhbSubs.o \
$(OBJDIR)/SfrSegment.o \
$(OBJDIR)/ObservePHMF.o \
$(OBJDIR)/Utilities.o \
$(OBJDIR)/TimeSeries.o \
$(OBJDIR)/TableTerm.o \
$(OBJDIR)/Lake.o \
$(OBJDIR)/ArrayReadersMF5.o \
$(OBJDIR)/LineList.o \
$(OBJDIR)/ChdType.o \
$(OBJDIR)/precutls.o \
$(OBJDIR)/Table.o \
$(OBJDIR)/GwfResSubs.o \
$(OBJDIR)/File.o \
$(OBJDIR)/Memory.o \
$(OBJDIR)/FileWriter.o \
$(OBJDIR)/IcWriter.o \
$(OBJDIR)/TdisWriter.o \
$(OBJDIR)/MemoryList.o \
$(OBJDIR)/StoWriter.o \
$(OBJDIR)/FileList.o \
$(OBJDIR)/NpfWriter.o \
$(OBJDIR)/DisWriter.o \
$(OBJDIR)/TdisVariables.o \
$(OBJDIR)/MemoryManager.o \
$(OBJDIR)/utl7.o \
$(OBJDIR)/NWT1_xmd.o \
$(OBJDIR)/GwfDrnSubs.o \
$(OBJDIR)/de47.o \
$(OBJDIR)/gwf2wel7_NWT.o \
$(OBJDIR)/GwfGhbSubs.o \
$(OBJDIR)/pcg7.o \
$(OBJDIR)/obs2ghb7.o \
$(OBJDIR)/GwfLgrSubs.o \
$(OBJDIR)/GwfBasOcSubs.o \
$(OBJDIR)/GwfSfrSubs.o \
$(OBJDIR)/parutl7.o \
$(OBJDIR)/DiscretizationBasePHMF.o \
$(OBJDIR)/GwfRivSubs.o \
$(OBJDIR)/GwfChdSubs.o \
$(OBJDIR)/sip7.o \
$(OBJDIR)/obs2bas7.o \
$(OBJDIR)/GwfHfbSubs_NWT.o \
$(OBJDIR)/GwfLakSubs.o \
$(OBJDIR)/NWT1_gmres.o \
$(OBJDIR)/NWT1_solver.o \
$(OBJDIR)/Mover.o \
$(OBJDIR)/GwfEtsSubs.o \
$(OBJDIR)/GwfMnwSubs.o \
$(OBJDIR)/obs2chd7.o \
$(OBJDIR)/obs2riv7.o \
$(OBJDIR)/GwfUzfSubs_NWT.o \
$(OBJDIR)/obs2drn7.o \
$(OBJDIR)/gmg7.o \
$(OBJDIR)/GwfRchSubs.o \
$(OBJDIR)/GwfEvtSubs.o \
$(OBJDIR)/gwf2mnw17.o \
$(OBJDIR)/Discretization3D.o \
$(OBJDIR)/gwf2mnw2i7.o \
$(OBJDIR)/ObsBlock.o \
$(OBJDIR)/Preproc.o \
$(OBJDIR)/ObsWriter.o \
$(OBJDIR)/ChdObsWriter.o \
$(OBJDIR)/RivObsWriter.o \
$(OBJDIR)/PackageWriter.o \
$(OBJDIR)/ChdPackageWriter.o \
$(OBJDIR)/HfbPackageWriter.o \
$(OBJDIR)/DrnObsWriter.o \
$(OBJDIR)/GhbObsWriter.o \
$(OBJDIR)/DrnPackageWriter.o \
$(OBJDIR)/EvtPackageWriter.o \
$(OBJDIR)/GhbPackageWriter.o \
$(OBJDIR)/RivPackageWriter.o \
$(OBJDIR)/WelPackageWriter.o \
$(OBJDIR)/OutputControlWriter.o \
$(OBJDIR)/RchPackageWriter.o \
$(OBJDIR)/MawPackageWriter.o \
$(OBJDIR)/MvrPackageWriter.o \
$(OBJDIR)/ImsPackageWriter.o \
$(OBJDIR)/LakPackageWriter.o \
$(OBJDIR)/SfrPackageWriter.o \
$(OBJDIR)/FhbPackageWriter.o \
$(OBJDIR)/UzfPackageWriter.o \
$(OBJDIR)/Model.o \
$(OBJDIR)/GwfBcfSubs.o \
$(OBJDIR)/GwfUpwSubs.o \
$(OBJDIR)/GwfBasSubs.o \
$(OBJDIR)/GwfLpfSubs.o \
$(OBJDIR)/Exchange.o \
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

