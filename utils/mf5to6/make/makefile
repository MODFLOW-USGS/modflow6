# makefile created on 2021-02-18 08:25:53.237196
# by pymake (version 1.2.0) for the 'mf5to6' executable 
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

.SUFFIXES: .f90 .f .fpp .o

OBJECTS = \
$(OBJDIR)/ParamModule.o \
$(OBJDIR)/GwfLakModule.o \
$(OBJDIR)/ConverterCommon.o \
$(OBJDIR)/GwfLpfModule.o \
$(OBJDIR)/mach_mod.o \
$(OBJDIR)/GwfDrnModule.o \
$(OBJDIR)/GwfGhbModule.o \
$(OBJDIR)/StressPeriod.o \
$(OBJDIR)/GwfUzfModule_NWT.o \
$(OBJDIR)/GwfEtsModule.o \
$(OBJDIR)/GwfWelModule.o \
$(OBJDIR)/GwfLgrModule.o \
$(OBJDIR)/GwfEvtModule.o \
$(OBJDIR)/NWT1_xmdlib.o \
$(OBJDIR)/gwf2hfb7_NWT.o \
$(OBJDIR)/NWT1_module.o \
$(OBJDIR)/GwfFhbModule.o \
$(OBJDIR)/GwfChdModule.o \
$(OBJDIR)/GwfUpwModule.o \
$(OBJDIR)/GlobalPHMF.o \
$(OBJDIR)/OpenSpec.o \
$(OBJDIR)/GwfMnwModule.o \
$(OBJDIR)/GwfBcfModule.o \
$(OBJDIR)/GwfResModule.o \
$(OBJDIR)/ConstantsPHMF.o \
$(OBJDIR)/GwfRchModule.o \
$(OBJDIR)/kind.o \
$(OBJDIR)/GwfRivModule.o \
$(OBJDIR)/GlobalVariablesPHMF.o \
$(OBJDIR)/Constants.o \
$(OBJDIR)/SimVariables.o \
$(OBJDIR)/compilerversion.o \
$(OBJDIR)/genericutils.o \
$(OBJDIR)/GwfSfrModule.o \
$(OBJDIR)/Global.o \
$(OBJDIR)/List.o \
$(OBJDIR)/Connection.o \
$(OBJDIR)/version.o \
$(OBJDIR)/TimeSeriesRecord.o \
$(OBJDIR)/LakeTributary.o \
$(OBJDIR)/ArrayHandlers.o \
$(OBJDIR)/LakeConnection.o \
$(OBJDIR)/LakeOutlet.o \
$(OBJDIR)/GlobalVariables.o \
$(OBJDIR)/GwfBasModule.o \
$(OBJDIR)/pcgn2.o \
$(OBJDIR)/SimPHMF.o \
$(OBJDIR)/Auxiliary.o \
$(OBJDIR)/MemoryHelper.o \
$(OBJDIR)/NWT1_ilupc_mod.o \
$(OBJDIR)/CharacterContainer.o \
$(OBJDIR)/SimListVariables.o \
$(OBJDIR)/SfrDiversion.o \
$(OBJDIR)/SfrReach.o \
$(OBJDIR)/SimVariablesPHMF.o \
$(OBJDIR)/MultiLayerObsModule.o \
$(OBJDIR)/GwfSfrCheck.o \
$(OBJDIR)/InputOutput.o \
$(OBJDIR)/SfrSegment.o \
$(OBJDIR)/Lake.o \
$(OBJDIR)/Utilities.o \
$(OBJDIR)/ModelPackage.o \
$(OBJDIR)/TableTerm.o \
$(OBJDIR)/LineList.o \
$(OBJDIR)/ObservePHMF.o \
$(OBJDIR)/precutls.o \
$(OBJDIR)/Table.o \
$(OBJDIR)/GwfFhbSubs.o \
$(OBJDIR)/BlockParser.o \
$(OBJDIR)/ArrayReadersMF5.o \
$(OBJDIR)/File.o \
$(OBJDIR)/FileList.o \
$(OBJDIR)/TimeSeries.o \
$(OBJDIR)/Memory.o \
$(OBJDIR)/GwfResSubs.o \
$(OBJDIR)/MemoryList.o \
$(OBJDIR)/ChdType.o \
$(OBJDIR)/FileWriter.o \
$(OBJDIR)/utl7.o \
$(OBJDIR)/DisWriter.o \
$(OBJDIR)/StoWriter.o \
$(OBJDIR)/GwfLgrSubs.o \
$(OBJDIR)/GwfEtsSubs.o \
$(OBJDIR)/de47.o \
$(OBJDIR)/gmg7.o \
$(OBJDIR)/GwfHfbSubs_NWT.o \
$(OBJDIR)/IcWriter.o \
$(OBJDIR)/NWT1_gmres.o \
$(OBJDIR)/obs2drn7.o \
$(OBJDIR)/parutl7.o \
$(OBJDIR)/MemoryManager.o \
$(OBJDIR)/TdisWriter.o \
$(OBJDIR)/obs2chd7.o \
$(OBJDIR)/GwfEvtSubs.o \
$(OBJDIR)/GwfLakSubs.o \
$(OBJDIR)/GwfChdSubs.o \
$(OBJDIR)/obs2riv7.o \
$(OBJDIR)/GwfRchSubs.o \
$(OBJDIR)/GwfDrnSubs.o \
$(OBJDIR)/GwfRivSubs.o \
$(OBJDIR)/NWT1_solver.o \
$(OBJDIR)/pcg7.o \
$(OBJDIR)/NpfWriter.o \
$(OBJDIR)/obs2bas7.o \
$(OBJDIR)/GwfGhbSubs.o \
$(OBJDIR)/GwfUzfSubs_NWT.o \
$(OBJDIR)/obs2ghb7.o \
$(OBJDIR)/GwfSfrSubs.o \
$(OBJDIR)/sip7.o \
$(OBJDIR)/GwfBasOcSubs.o \
$(OBJDIR)/DiscretizationBasePHMF.o \
$(OBJDIR)/Discretization3D.o \
$(OBJDIR)/gwf2mnw17.o \
$(OBJDIR)/NWT1_xmd.o \
$(OBJDIR)/GwfMnwSubs.o \
$(OBJDIR)/Mover.o \
$(OBJDIR)/gwf2wel7_NWT.o \
$(OBJDIR)/TdisVariables.o \
$(OBJDIR)/ObsBlock.o \
$(OBJDIR)/Preproc.o \
$(OBJDIR)/ObsWriter.o \
$(OBJDIR)/gwf2mnw2i7.o \
$(OBJDIR)/DrnObsWriter.o \
$(OBJDIR)/PackageWriter.o \
$(OBJDIR)/DrnPackageWriter.o \
$(OBJDIR)/WelPackageWriter.o \
$(OBJDIR)/MawPackageWriter.o \
$(OBJDIR)/GhbObsWriter.o \
$(OBJDIR)/ChdObsWriter.o \
$(OBJDIR)/OutputControlWriter.o \
$(OBJDIR)/RivPackageWriter.o \
$(OBJDIR)/EvtPackageWriter.o \
$(OBJDIR)/RivObsWriter.o \
$(OBJDIR)/ChdPackageWriter.o \
$(OBJDIR)/GhbPackageWriter.o \
$(OBJDIR)/HfbPackageWriter.o \
$(OBJDIR)/RchPackageWriter.o \
$(OBJDIR)/MvrPackageWriter.o \
$(OBJDIR)/ImsPackageWriter.o \
$(OBJDIR)/FhbPackageWriter.o \
$(OBJDIR)/SfrPackageWriter.o \
$(OBJDIR)/UzfPackageWriter.o \
$(OBJDIR)/LakPackageWriter.o \
$(OBJDIR)/Model.o \
$(OBJDIR)/Exchange.o \
$(OBJDIR)/GwfBcfSubs.o \
$(OBJDIR)/GwfLpfSubs.o \
$(OBJDIR)/GwfUpwSubs.o \
$(OBJDIR)/GwfBasSubs.o \
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

$(OBJDIR)/%.o : %.fpp
	@mkdir -p $(@D)
	$(FC) $(OPTLEVEL) $(FFLAGS) -c $< -o $@ $(INCSWITCH) $(MODSWITCH)

