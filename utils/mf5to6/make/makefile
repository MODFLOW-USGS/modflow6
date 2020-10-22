# makefile created on 2020-10-22 09:28:15.441302
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
$(OBJDIR)/GwfLakModule.o \
$(OBJDIR)/StressPeriod.o \
$(OBJDIR)/GwfWelModule.o \
$(OBJDIR)/ConverterCommon.o \
$(OBJDIR)/GwfMnwModule.o \
$(OBJDIR)/GwfChdModule.o \
$(OBJDIR)/GlobalPHMF.o \
$(OBJDIR)/GwfResModule.o \
$(OBJDIR)/ConstantsPHMF.o \
$(OBJDIR)/ParamModule.o \
$(OBJDIR)/GwfLpfModule.o \
$(OBJDIR)/GwfBcfModule.o \
$(OBJDIR)/kind.o \
$(OBJDIR)/GwfDrnModule.o \
$(OBJDIR)/OpenSpec.o \
$(OBJDIR)/GwfRchModule.o \
$(OBJDIR)/GwfEvtModule.o \
$(OBJDIR)/NWT1_xmdlib.o \
$(OBJDIR)/compilerversion.o \
$(OBJDIR)/gwf2hfb7_NWT.o \
$(OBJDIR)/mach_mod.o \
$(OBJDIR)/GwfUpwModule.o \
$(OBJDIR)/GwfLgrModule.o \
$(OBJDIR)/GwfGhbModule.o \
$(OBJDIR)/GwfFhbModule.o \
$(OBJDIR)/GwfRivModule.o \
$(OBJDIR)/GwfUzfModule_NWT.o \
$(OBJDIR)/GlobalVariablesPHMF.o \
$(OBJDIR)/GwfEtsModule.o \
$(OBJDIR)/Constants.o \
$(OBJDIR)/NWT1_module.o \
$(OBJDIR)/SimVariables.o \
$(OBJDIR)/genericutils.o \
$(OBJDIR)/Connection.o \
$(OBJDIR)/List.o \
$(OBJDIR)/Global.o \
$(OBJDIR)/CharacterContainer.o \
$(OBJDIR)/GwfSfrModule.o \
$(OBJDIR)/SfrDiversion.o \
$(OBJDIR)/pcgn2.o \
$(OBJDIR)/version.o \
$(OBJDIR)/Auxiliary.o \
$(OBJDIR)/GlobalVariables.o \
$(OBJDIR)/GwfBasModule.o \
$(OBJDIR)/LakeTributary.o \
$(OBJDIR)/SimListVariables.o \
$(OBJDIR)/TimeSeriesRecord.o \
$(OBJDIR)/LakeOutlet.o \
$(OBJDIR)/LakeConnection.o \
$(OBJDIR)/NWT1_ilupc_mod.o \
$(OBJDIR)/ArrayHandlers.o \
$(OBJDIR)/SfrReach.o \
$(OBJDIR)/SimPHMF.o \
$(OBJDIR)/GwfSfrCheck.o \
$(OBJDIR)/ModelPackage.o \
$(OBJDIR)/SimVariablesPHMF.o \
$(OBJDIR)/SfrSegment.o \
$(OBJDIR)/MultiLayerObsModule.o \
$(OBJDIR)/InputOutput.o \
$(OBJDIR)/BlockParser.o \
$(OBJDIR)/ArrayReadersMF5.o \
$(OBJDIR)/MemoryHelper.o \
$(OBJDIR)/LineList.o \
$(OBJDIR)/TimeSeries.o \
$(OBJDIR)/GwfResSubs.o \
$(OBJDIR)/Utilities.o \
$(OBJDIR)/GwfFhbSubs.o \
$(OBJDIR)/TableTerm.o \
$(OBJDIR)/Table.o \
$(OBJDIR)/ChdType.o \
$(OBJDIR)/Lake.o \
$(OBJDIR)/ObservePHMF.o \
$(OBJDIR)/precutls.o \
$(OBJDIR)/Memory.o \
$(OBJDIR)/MemoryList.o \
$(OBJDIR)/MemoryManager.o \
$(OBJDIR)/File.o \
$(OBJDIR)/FileList.o \
$(OBJDIR)/FileWriter.o \
$(OBJDIR)/DiscretizationBasePHMF.o \
$(OBJDIR)/Discretization3D.o \
$(OBJDIR)/StoWriter.o \
$(OBJDIR)/utl7.o \
$(OBJDIR)/GwfSfrSubs.o \
$(OBJDIR)/GwfEvtSubs.o \
$(OBJDIR)/sip7.o \
$(OBJDIR)/obs2ghb7.o \
$(OBJDIR)/NWT1_xmd.o \
$(OBJDIR)/pcg7.o \
$(OBJDIR)/TdisWriter.o \
$(OBJDIR)/GwfChdSubs.o \
$(OBJDIR)/IcWriter.o \
$(OBJDIR)/NpfWriter.o \
$(OBJDIR)/GwfEtsSubs.o \
$(OBJDIR)/parutl7.o \
$(OBJDIR)/gmg7.o \
$(OBJDIR)/obs2riv7.o \
$(OBJDIR)/GwfHfbSubs_NWT.o \
$(OBJDIR)/GwfDrnSubs.o \
$(OBJDIR)/GwfGhbSubs.o \
$(OBJDIR)/GwfLgrSubs.o \
$(OBJDIR)/DisWriter.o \
$(OBJDIR)/gwf2wel7_NWT.o \
$(OBJDIR)/GwfUzfSubs_NWT.o \
$(OBJDIR)/GwfLakSubs.o \
$(OBJDIR)/obs2drn7.o \
$(OBJDIR)/de47.o \
$(OBJDIR)/TdisVariables.o \
$(OBJDIR)/GwfRchSubs.o \
$(OBJDIR)/gwf2mnw17.o \
$(OBJDIR)/NWT1_solver.o \
$(OBJDIR)/ObsBlock.o \
$(OBJDIR)/GwfRivSubs.o \
$(OBJDIR)/GwfBasOcSubs.o \
$(OBJDIR)/obs2bas7.o \
$(OBJDIR)/NWT1_gmres.o \
$(OBJDIR)/GwfMnwSubs.o \
$(OBJDIR)/obs2chd7.o \
$(OBJDIR)/Mover.o \
$(OBJDIR)/gwf2mnw2i7.o \
$(OBJDIR)/Preproc.o \
$(OBJDIR)/ObsWriter.o \
$(OBJDIR)/RivObsWriter.o \
$(OBJDIR)/PackageWriter.o \
$(OBJDIR)/OutputControlWriter.o \
$(OBJDIR)/WelPackageWriter.o \
$(OBJDIR)/DrnObsWriter.o \
$(OBJDIR)/RchPackageWriter.o \
$(OBJDIR)/ChdObsWriter.o \
$(OBJDIR)/GhbObsWriter.o \
$(OBJDIR)/RivPackageWriter.o \
$(OBJDIR)/DrnPackageWriter.o \
$(OBJDIR)/HfbPackageWriter.o \
$(OBJDIR)/GhbPackageWriter.o \
$(OBJDIR)/MvrPackageWriter.o \
$(OBJDIR)/MawPackageWriter.o \
$(OBJDIR)/ChdPackageWriter.o \
$(OBJDIR)/ImsPackageWriter.o \
$(OBJDIR)/FhbPackageWriter.o \
$(OBJDIR)/EvtPackageWriter.o \
$(OBJDIR)/SfrPackageWriter.o \
$(OBJDIR)/LakPackageWriter.o \
$(OBJDIR)/UzfPackageWriter.o \
$(OBJDIR)/Model.o \
$(OBJDIR)/GwfBcfSubs.o \
$(OBJDIR)/GwfBasSubs.o \
$(OBJDIR)/GwfUpwSubs.o \
$(OBJDIR)/GwfLpfSubs.o \
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

$(OBJDIR)/%.o : %.fpp
	@mkdir -p $(@D)
	$(FC) $(OPTLEVEL) $(FFLAGS) -c $< -o $@ $(INCSWITCH) $(MODSWITCH)

