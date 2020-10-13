# makefile created on 2020-10-13 14:19:13.968307
# by pymake (version 1.2.0) for the 'mf5to6' executable 
# using the 'gfortran' fortran compiler(s).


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

.SUFFIXES: .f90 .f .fpp .o

OBJECTS = \
$(OBJDIR)/GwfLakModule.o \
$(OBJDIR)/GwfRchModule.o \
$(OBJDIR)/kind.o \
$(OBJDIR)/OpenSpec.o \
$(OBJDIR)/GwfEvtModule.o \
$(OBJDIR)/gwf2hfb7_NWT.o \
$(OBJDIR)/GwfGhbModule.o \
$(OBJDIR)/GwfLgrModule.o \
$(OBJDIR)/GwfFhbModule.o \
$(OBJDIR)/GwfChdModule.o \
$(OBJDIR)/mach_mod.o \
$(OBJDIR)/StressPeriod.o \
$(OBJDIR)/GwfMnwModule.o \
$(OBJDIR)/GwfWelModule.o \
$(OBJDIR)/GwfLpfModule.o \
$(OBJDIR)/GwfResModule.o \
$(OBJDIR)/ParamModule.o \
$(OBJDIR)/GwfBcfModule.o \
$(OBJDIR)/Constants.o \
$(OBJDIR)/NWT1_module.o \
$(OBJDIR)/GlobalPHMF.o \
$(OBJDIR)/NWT1_xmdlib.o \
$(OBJDIR)/GwfRivModule.o \
$(OBJDIR)/SimVariables.o \
$(OBJDIR)/ConstantsPHMF.o \
$(OBJDIR)/GwfDrnModule.o \
$(OBJDIR)/GwfUzfModule_NWT.o \
$(OBJDIR)/compilerversion.o \
$(OBJDIR)/GwfEtsModule.o \
$(OBJDIR)/GwfUpwModule.o \
$(OBJDIR)/ConverterCommon.o \
$(OBJDIR)/GlobalVariablesPHMF.o \
$(OBJDIR)/GwfSfrModule.o \
$(OBJDIR)/genericutils.o \
$(OBJDIR)/Connection.o \
$(OBJDIR)/Global.o \
$(OBJDIR)/List.o \
$(OBJDIR)/LakeOutlet.o \
$(OBJDIR)/LakeTributary.o \
$(OBJDIR)/version.o \
$(OBJDIR)/LakeConnection.o \
$(OBJDIR)/Auxiliary.o \
$(OBJDIR)/ArrayHandlers.o \
$(OBJDIR)/GwfBasModule.o \
$(OBJDIR)/CharacterContainer.o \
$(OBJDIR)/NWT1_ilupc_mod.o \
$(OBJDIR)/SfrDiversion.o \
$(OBJDIR)/TimeSeriesRecord.o \
$(OBJDIR)/pcgn2.o \
$(OBJDIR)/SimListVariables.o \
$(OBJDIR)/GlobalVariables.o \
$(OBJDIR)/SfrReach.o \
$(OBJDIR)/SimPHMF.o \
$(OBJDIR)/MemoryHelper.o \
$(OBJDIR)/GwfSfrCheck.o \
$(OBJDIR)/MultiLayerObsModule.o \
$(OBJDIR)/ModelPackage.o \
$(OBJDIR)/InputOutput.o \
$(OBJDIR)/SfrSegment.o \
$(OBJDIR)/SimVariablesPHMF.o \
$(OBJDIR)/GwfFhbSubs.o \
$(OBJDIR)/TableTerm.o \
$(OBJDIR)/Utilities.o \
$(OBJDIR)/LineList.o \
$(OBJDIR)/Lake.o \
$(OBJDIR)/ObservePHMF.o \
$(OBJDIR)/BlockParser.o \
$(OBJDIR)/ArrayReadersMF5.o \
$(OBJDIR)/Table.o \
$(OBJDIR)/precutls.o \
$(OBJDIR)/GwfResSubs.o \
$(OBJDIR)/Memory.o \
$(OBJDIR)/TimeSeries.o \
$(OBJDIR)/File.o \
$(OBJDIR)/ChdType.o \
$(OBJDIR)/FileList.o \
$(OBJDIR)/MemoryList.o \
$(OBJDIR)/MemoryManager.o \
$(OBJDIR)/DiscretizationBasePHMF.o \
$(OBJDIR)/FileWriter.o \
$(OBJDIR)/utl7.o \
$(OBJDIR)/GwfBasOcSubs.o \
$(OBJDIR)/GwfUzfSubs_NWT.o \
$(OBJDIR)/GwfChdSubs.o \
$(OBJDIR)/TdisWriter.o \
$(OBJDIR)/parutl7.o \
$(OBJDIR)/GwfLgrSubs.o \
$(OBJDIR)/sip7.o \
$(OBJDIR)/GwfGhbSubs.o \
$(OBJDIR)/GwfSfrSubs.o \
$(OBJDIR)/gmg7.o \
$(OBJDIR)/GwfMnwSubs.o \
$(OBJDIR)/GwfDrnSubs.o \
$(OBJDIR)/IcWriter.o \
$(OBJDIR)/GwfLakSubs.o \
$(OBJDIR)/obs2chd7.o \
$(OBJDIR)/GwfEvtSubs.o \
$(OBJDIR)/obs2ghb7.o \
$(OBJDIR)/StoWriter.o \
$(OBJDIR)/GwfRchSubs.o \
$(OBJDIR)/TdisVariables.o \
$(OBJDIR)/Discretization3D.o \
$(OBJDIR)/obs2drn7.o \
$(OBJDIR)/gwf2mnw2i7.o \
$(OBJDIR)/obs2riv7.o \
$(OBJDIR)/DisWriter.o \
$(OBJDIR)/GwfRivSubs.o \
$(OBJDIR)/NpfWriter.o \
$(OBJDIR)/de47.o \
$(OBJDIR)/ObsBlock.o \
$(OBJDIR)/NWT1_gmres.o \
$(OBJDIR)/pcg7.o \
$(OBJDIR)/Mover.o \
$(OBJDIR)/Preproc.o \
$(OBJDIR)/gwf2wel7_NWT.o \
$(OBJDIR)/gwf2mnw17.o \
$(OBJDIR)/GwfHfbSubs_NWT.o \
$(OBJDIR)/NWT1_xmd.o \
$(OBJDIR)/obs2bas7.o \
$(OBJDIR)/GwfEtsSubs.o \
$(OBJDIR)/NWT1_solver.o \
$(OBJDIR)/ObsWriter.o \
$(OBJDIR)/PackageWriter.o \
$(OBJDIR)/RivPackageWriter.o \
$(OBJDIR)/HfbPackageWriter.o \
$(OBJDIR)/DrnObsWriter.o \
$(OBJDIR)/ImsPackageWriter.o \
$(OBJDIR)/GhbPackageWriter.o \
$(OBJDIR)/GhbObsWriter.o \
$(OBJDIR)/ChdObsWriter.o \
$(OBJDIR)/MawPackageWriter.o \
$(OBJDIR)/EvtPackageWriter.o \
$(OBJDIR)/RivObsWriter.o \
$(OBJDIR)/MvrPackageWriter.o \
$(OBJDIR)/RchPackageWriter.o \
$(OBJDIR)/WelPackageWriter.o \
$(OBJDIR)/OutputControlWriter.o \
$(OBJDIR)/DrnPackageWriter.o \
$(OBJDIR)/LakPackageWriter.o \
$(OBJDIR)/ChdPackageWriter.o \
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

$(OBJDIR)/%.o : %.f90
	@mkdir -p $(@D)
	$(FC) $(OPTLEVEL) $(FFLAGS) -c $< -o $@ $(INCSWITCH) $(MODSWITCH)

$(OBJDIR)/%.o : %.f
	@mkdir -p $(@D)
	$(FC) $(OPTLEVEL) $(FFLAGS) -c $< -o $@ $(INCSWITCH) $(MODSWITCH)

$(OBJDIR)/%.o : %.fpp
	@mkdir -p $(@D)
	$(FC) $(OPTLEVEL) $(FFLAGS) -c $< -o $@ $(INCSWITCH) $(MODSWITCH)

