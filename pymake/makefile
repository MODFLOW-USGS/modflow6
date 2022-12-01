# makefile created by pymake (version 1.2.5) for the 'mf6' executable.


include ./makedefaults

# Define the source file directories
SOURCEDIR1=../src
SOURCEDIR2=../src/Exchange
SOURCEDIR3=../src/Solution
SOURCEDIR4=../src/Solution/LinearMethods
SOURCEDIR5=../src/Timing
SOURCEDIR6=../src/Utilities
SOURCEDIR7=../src/Utilities/Idm
SOURCEDIR8=../src/Utilities/TimeSeries
SOURCEDIR9=../src/Utilities/Memory
SOURCEDIR10=../src/Utilities/OutputControl
SOURCEDIR11=../src/Utilities/ArrayRead
SOURCEDIR12=../src/Utilities/Libraries
SOURCEDIR13=../src/Utilities/Libraries/rcm
SOURCEDIR14=../src/Utilities/Libraries/blas
SOURCEDIR15=../src/Utilities/Libraries/sparskit2
SOURCEDIR16=../src/Utilities/Libraries/daglib
SOURCEDIR17=../src/Utilities/Libraries/sparsekit
SOURCEDIR18=../src/Utilities/Observation
SOURCEDIR19=../src/Model
SOURCEDIR20=../src/Model/Connection
SOURCEDIR21=../src/Model/GroundWaterTransport
SOURCEDIR22=../src/Model/ModelUtilities
SOURCEDIR23=../src/Model/GroundWaterFlow
SOURCEDIR24=../src/Model/Geometry

VPATH = \
${SOURCEDIR1} \
${SOURCEDIR2} \
${SOURCEDIR3} \
${SOURCEDIR4} \
${SOURCEDIR5} \
${SOURCEDIR6} \
${SOURCEDIR7} \
${SOURCEDIR8} \
${SOURCEDIR9} \
${SOURCEDIR10} \
${SOURCEDIR11} \
${SOURCEDIR12} \
${SOURCEDIR13} \
${SOURCEDIR14} \
${SOURCEDIR15} \
${SOURCEDIR16} \
${SOURCEDIR17} \
${SOURCEDIR18} \
${SOURCEDIR19} \
${SOURCEDIR20} \
${SOURCEDIR21} \
${SOURCEDIR22} \
${SOURCEDIR23} \
${SOURCEDIR24} 

.SUFFIXES: .f90 .F90 .o

OBJECTS = \
$(OBJDIR)/kind.o \
$(OBJDIR)/Constants.o \
$(OBJDIR)/SimVariables.o \
$(OBJDIR)/genericutils.o \
$(OBJDIR)/compilerversion.o \
$(OBJDIR)/ArrayHandlers.o \
$(OBJDIR)/version.o \
$(OBJDIR)/Message.o \
$(OBJDIR)/defmacro.o \
$(OBJDIR)/Sim.o \
$(OBJDIR)/OpenSpec.o \
$(OBJDIR)/InputOutput.o \
$(OBJDIR)/TableTerm.o \
$(OBJDIR)/Table.o \
$(OBJDIR)/MemoryHelper.o \
$(OBJDIR)/CharString.o \
$(OBJDIR)/Memory.o \
$(OBJDIR)/List.o \
$(OBJDIR)/MemoryList.o \
$(OBJDIR)/TimeSeriesRecord.o \
$(OBJDIR)/BlockParser.o \
$(OBJDIR)/MemoryManager.o \
$(OBJDIR)/TimeSeries.o \
$(OBJDIR)/ats.o \
$(OBJDIR)/TimeSeriesLink.o \
$(OBJDIR)/TimeSeriesFileList.o \
$(OBJDIR)/tdis.o \
$(OBJDIR)/HashTable.o \
$(OBJDIR)/Sparse.o \
$(OBJDIR)/DisvGeom.o \
$(OBJDIR)/ArrayReaders.o \
$(OBJDIR)/TimeSeriesManager.o \
$(OBJDIR)/SmoothingFunctions.o \
$(OBJDIR)/ListReader.o \
$(OBJDIR)/Connections.o \
$(OBJDIR)/DiscretizationBase.o \
$(OBJDIR)/TimeArray.o \
$(OBJDIR)/ObsOutput.o \
$(OBJDIR)/TimeArraySeries.o \
$(OBJDIR)/ObsOutputList.o \
$(OBJDIR)/Observe.o \
$(OBJDIR)/InputDefinition.o \
$(OBJDIR)/TimeArraySeriesLink.o \
$(OBJDIR)/ObsUtility.o \
$(OBJDIR)/ObsContainer.o \
$(OBJDIR)/VectorInt.o \
$(OBJDIR)/gwt1dspidm.o \
$(OBJDIR)/gwf3npf8idm.o \
$(OBJDIR)/gwf3disv8idm.o \
$(OBJDIR)/gwf3disu8idm.o \
$(OBJDIR)/gwf3dis8idm.o \
$(OBJDIR)/ArrayReaderBase.o \
$(OBJDIR)/TimeArraySeriesManager.o \
$(OBJDIR)/PackageMover.o \
$(OBJDIR)/Obs3.o \
$(OBJDIR)/NumericalPackage.o \
$(OBJDIR)/Budget.o \
$(OBJDIR)/BudgetFileReader.o \
$(OBJDIR)/StructVector.o \
$(OBJDIR)/IdmLogger.o \
$(OBJDIR)/InputDefinitionSelector.o \
$(OBJDIR)/Integer2dReader.o \
$(OBJDIR)/Double2dReader.o \
$(OBJDIR)/BoundaryPackage.o \
$(OBJDIR)/BaseModel.o \
$(OBJDIR)/BudgetTerm.o \
$(OBJDIR)/StructArray.o \
$(OBJDIR)/ModflowInput.o \
$(OBJDIR)/Integer1dReader.o \
$(OBJDIR)/Double1dReader.o \
$(OBJDIR)/NumericalModel.o \
$(OBJDIR)/mf6lists.o \
$(OBJDIR)/PackageBudget.o \
$(OBJDIR)/HeadFileReader.o \
$(OBJDIR)/BudgetObject.o \
$(OBJDIR)/sort.o \
$(OBJDIR)/SfrCrossSectionUtils.o \
$(OBJDIR)/PrintSaveManager.o \
$(OBJDIR)/Xt3dAlgorithm.o \
$(OBJDIR)/gwf3tvbase8.o \
$(OBJDIR)/LoadMf6FileType.o \
$(OBJDIR)/DistributedModel.o \
$(OBJDIR)/BaseExchange.o \
$(OBJDIR)/UzfCellGroup.o \
$(OBJDIR)/gwt1fmi1.o \
$(OBJDIR)/SfrCrossSectionManager.o \
$(OBJDIR)/dag_module.o \
$(OBJDIR)/OutputControlData.o \
$(OBJDIR)/gwf3ic8.o \
$(OBJDIR)/Xt3dInterface.o \
$(OBJDIR)/gwf3tvk8.o \
$(OBJDIR)/MemoryManagerExt.o \
$(OBJDIR)/IdmMf6FileLoader.o \
$(OBJDIR)/GwfNpfOptions.o \
$(OBJDIR)/CellWithNbrs.o \
$(OBJDIR)/NumericalExchange.o \
$(OBJDIR)/Iunit.o \
$(OBJDIR)/gwf3uzf8.o \
$(OBJDIR)/gwt1apt1.o \
$(OBJDIR)/GwtSpc.o \
$(OBJDIR)/gwf3sfr8.o \
$(OBJDIR)/OutputControl.o \
$(OBJDIR)/gwt1ic1.o \
$(OBJDIR)/gwf3maw8.o \
$(OBJDIR)/gwf3lak8.o \
$(OBJDIR)/gwt1mst1.o \
$(OBJDIR)/GwtDspOptions.o \
$(OBJDIR)/gwf3npf8.o \
$(OBJDIR)/GwtAdvOptions.o \
$(OBJDIR)/gwf3tvs8.o \
$(OBJDIR)/GwfStorageUtils.o \
$(OBJDIR)/Mover.o \
$(OBJDIR)/GwfMvrPeriodData.o \
$(OBJDIR)/ims8misc.o \
$(OBJDIR)/GwfBuyInputData.o \
$(OBJDIR)/InterfaceMap.o \
$(OBJDIR)/gwf3disu8.o \
$(OBJDIR)/GridSorting.o \
$(OBJDIR)/DisConnExchange.o \
$(OBJDIR)/CsrUtils.o \
$(OBJDIR)/MappedVariable.o \
$(OBJDIR)/TransportModel.o \
$(OBJDIR)/NameFile.o \
$(OBJDIR)/gwt1uzt1.o \
$(OBJDIR)/gwt1ssm1.o \
$(OBJDIR)/gwt1src1.o \
$(OBJDIR)/gwt1sft1.o \
$(OBJDIR)/gwt1oc1.o \
$(OBJDIR)/gwt1obs1.o \
$(OBJDIR)/gwt1mwt1.o \
$(OBJDIR)/gwt1mvt1.o \
$(OBJDIR)/gwt1lkt1.o \
$(OBJDIR)/gwt1ist1.o \
$(OBJDIR)/gwt1dsp.o \
$(OBJDIR)/gwt1cnc1.o \
$(OBJDIR)/gwt1adv1.o \
$(OBJDIR)/gwf3disv8.o \
$(OBJDIR)/gwf3dis8.o \
$(OBJDIR)/gwf3api8.o \
$(OBJDIR)/gwf3wel8.o \
$(OBJDIR)/gwf3riv8.o \
$(OBJDIR)/gwf3rch8.o \
$(OBJDIR)/gwf3sto8.o \
$(OBJDIR)/gwf3oc8.o \
$(OBJDIR)/gwf3obs8.o \
$(OBJDIR)/gwf3mvr8.o \
$(OBJDIR)/gwf3hfb8.o \
$(OBJDIR)/gwf3csub8.o \
$(OBJDIR)/gwf3buy8.o \
$(OBJDIR)/GhostNode.o \
$(OBJDIR)/gwf3ghb8.o \
$(OBJDIR)/gwf3evt8.o \
$(OBJDIR)/gwf3drn8.o \
$(OBJDIR)/gwf3chd8.o \
$(OBJDIR)/ims8reordering.o \
$(OBJDIR)/GridConnection.o \
$(OBJDIR)/DistributedData.o \
$(OBJDIR)/gwt1.o \
$(OBJDIR)/gwf3.o \
$(OBJDIR)/ims8base.o \
$(OBJDIR)/SpatialModelConnection.o \
$(OBJDIR)/GwtInterfaceModel.o \
$(OBJDIR)/GwtGwtExchange.o \
$(OBJDIR)/GwfInterfaceModel.o \
$(OBJDIR)/GwfGwfExchange.o \
$(OBJDIR)/BaseSolution.o \
$(OBJDIR)/Timer.o \
$(OBJDIR)/ims8linear.o \
$(OBJDIR)/GwtGwtConnection.o \
$(OBJDIR)/GwfGwfConnection.o \
$(OBJDIR)/SolutionGroup.o \
$(OBJDIR)/NumericalSolution.o \
$(OBJDIR)/GwfGwtExchange.o \
$(OBJDIR)/SimulationCreate.o \
$(OBJDIR)/ConnectionBuilder.o \
$(OBJDIR)/comarg.o \
$(OBJDIR)/mf6core.o \
$(OBJDIR)/BaseGeometry.o \
$(OBJDIR)/mf6.o \
$(OBJDIR)/StringList.o \
$(OBJDIR)/MemorySetHandler.o \
$(OBJDIR)/ilut.o \
$(OBJDIR)/sparsekit.o \
$(OBJDIR)/rcm.o \
$(OBJDIR)/blas1_d.o \
$(OBJDIR)/RectangularGeometry.o \
$(OBJDIR)/CircularGeometry.o

# Define the objects that make up the program
$(PROGRAM) : $(OBJECTS)
	-$(FC) $(OPTLEVEL) -o $@ $(OBJECTS) $(LDFLAGS)

$(OBJDIR)/%.o : %.f90
	@mkdir -p $(@D)
	$(FC) $(OPTLEVEL) $(FFLAGS) -c $< -o $@ $(INCSWITCH) $(MODSWITCH)

$(OBJDIR)/%.o : %.F90
	@mkdir -p $(@D)
	$(FC) $(OPTLEVEL) $(FFLAGS) -c $< -o $@ $(INCSWITCH) $(MODSWITCH)

