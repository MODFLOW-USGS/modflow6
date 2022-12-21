# makefile created by pymake (version 1.2.7) for the 'mf6' executable.


include ./makedefaults

# Define the source file directories
SOURCEDIR1=../src
SOURCEDIR2=../src/Exchange
SOURCEDIR3=../src/Model
SOURCEDIR4=../src/Model/Geometry
SOURCEDIR5=../src/Model/ModelUtilities
SOURCEDIR6=../src/Model/Connection
SOURCEDIR7=../src/Model/GroundWaterTransport
SOURCEDIR8=../src/Model/GroundWaterFlow
SOURCEDIR9=../src/Solution
SOURCEDIR10=../src/Solution/LinearMethods
SOURCEDIR11=../src/Timing
SOURCEDIR12=../src/Utilities
SOURCEDIR13=../src/Utilities/TimeSeries
SOURCEDIR14=../src/Utilities/Libraries
SOURCEDIR15=../src/Utilities/Libraries/rcm
SOURCEDIR16=../src/Utilities/Libraries/sparsekit
SOURCEDIR17=../src/Utilities/Libraries/sparskit2
SOURCEDIR18=../src/Utilities/Libraries/blas
SOURCEDIR19=../src/Utilities/Libraries/daglib
SOURCEDIR20=../src/Utilities/Idm
SOURCEDIR21=../src/Utilities/Matrix
SOURCEDIR22=../src/Utilities/Observation
SOURCEDIR23=../src/Utilities/OutputControl
SOURCEDIR24=../src/Utilities/Memory
SOURCEDIR25=../src/Utilities/ArrayRead

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
${SOURCEDIR24} \
${SOURCEDIR25} 

.SUFFIXES: .f90 .F90 .o

OBJECTS = \
$(OBJDIR)/ilut.o \
$(OBJDIR)/blas1_d.o \
$(OBJDIR)/dag_module.o \
$(OBJDIR)/CharString.o \
$(OBJDIR)/OpenSpec.o \
$(OBJDIR)/rcm.o \
$(OBJDIR)/kind.o \
$(OBJDIR)/GwtDspOptions.o \
$(OBJDIR)/sparsekit.o \
$(OBJDIR)/HashTable.o \
$(OBJDIR)/CsrUtils.o \
$(OBJDIR)/Sparse.o \
$(OBJDIR)/GwtAdvOptions.o \
$(OBJDIR)/BaseGeometry.o \
$(OBJDIR)/IdmLogger.o \
$(OBJDIR)/Constants.o \
$(OBJDIR)/defmacro.o \
$(OBJDIR)/MatrixBase.o \
$(OBJDIR)/SimVariables.o \
$(OBJDIR)/SmoothingFunctions.o \
$(OBJDIR)/ims8misc.o \
$(OBJDIR)/Xt3dAlgorithm.o \
$(OBJDIR)/GwfNpfOptions.o \
$(OBJDIR)/InputDefinition.o \
$(OBJDIR)/SfrCrossSectionUtils.o \
$(OBJDIR)/ims8reordering.o \
$(OBJDIR)/GwfStorageUtils.o \
$(OBJDIR)/GwfBuyInputData.o \
$(OBJDIR)/InterfaceMap.o \
$(OBJDIR)/gwf3dis8idm.o \
$(OBJDIR)/compilerversion.o \
$(OBJDIR)/gwf3disu8idm.o \
$(OBJDIR)/genericutils.o \
$(OBJDIR)/gwf3npf8idm.o \
$(OBJDIR)/gwf3disv8idm.o \
$(OBJDIR)/Timer.o \
$(OBJDIR)/version.o \
$(OBJDIR)/ArrayHandlers.o \
$(OBJDIR)/gwt1dspidm.o \
$(OBJDIR)/Message.o \
$(OBJDIR)/List.o \
$(OBJDIR)/Sim.o \
$(OBJDIR)/ObsOutput.o \
$(OBJDIR)/InputOutput.o \
$(OBJDIR)/sort.o \
$(OBJDIR)/ObsOutputList.o \
$(OBJDIR)/Iunit.o \
$(OBJDIR)/HeadFileReader.o \
$(OBJDIR)/MemoryHelper.o \
$(OBJDIR)/StringList.o \
$(OBJDIR)/TimeSeriesRecord.o \
$(OBJDIR)/InputDefinitionSelector.o \
$(OBJDIR)/TableTerm.o \
$(OBJDIR)/mf6lists.o \
$(OBJDIR)/VectorInt.o \
$(OBJDIR)/comarg.o \
$(OBJDIR)/Budget.o \
$(OBJDIR)/PrintSaveManager.o \
$(OBJDIR)/BudgetFileReader.o \
$(OBJDIR)/ModflowInput.o \
$(OBJDIR)/CircularGeometry.o \
$(OBJDIR)/ArrayReaders.o \
$(OBJDIR)/RectangularGeometry.o \
$(OBJDIR)/BlockParser.o \
$(OBJDIR)/StructVector.o \
$(OBJDIR)/TimeSeries.o \
$(OBJDIR)/DisvGeom.o \
$(OBJDIR)/TimeSeriesFileList.o \
$(OBJDIR)/ims8base.o \
$(OBJDIR)/TimeSeriesLink.o \
$(OBJDIR)/Table.o \
$(OBJDIR)/ArrayReaderBase.o \
$(OBJDIR)/Double1dReader.o \
$(OBJDIR)/SfrCrossSectionManager.o \
$(OBJDIR)/NameFile.o \
$(OBJDIR)/ListReader.o \
$(OBJDIR)/Memory.o \
$(OBJDIR)/MemoryList.o \
$(OBJDIR)/Integer2dReader.o \
$(OBJDIR)/MemoryManager.o \
$(OBJDIR)/PackageMover.o \
$(OBJDIR)/PackageBudget.o \
$(OBJDIR)/GwfMvrPeriodData.o \
$(OBJDIR)/Connections.o \
$(OBJDIR)/StructArray.o \
$(OBJDIR)/Mover.o \
$(OBJDIR)/SparseMatrix.o \
$(OBJDIR)/Integer1dReader.o \
$(OBJDIR)/BaseModel.o \
$(OBJDIR)/ims8linear.o \
$(OBJDIR)/Double2dReader.o \
$(OBJDIR)/MemoryManagerExt.o \
$(OBJDIR)/MappedVariable.o \
$(OBJDIR)/MemorySetHandler.o \
$(OBJDIR)/ats.o \
$(OBJDIR)/DistributedData.o \
$(OBJDIR)/tdis.o \
$(OBJDIR)/LayeredArrayReader.o \
$(OBJDIR)/BaseExchange.o \
$(OBJDIR)/TimeSeriesManager.o \
$(OBJDIR)/UzfCellGroup.o \
$(OBJDIR)/DiscretizationBase.o \
$(OBJDIR)/BaseSolution.o \
$(OBJDIR)/OutputControlData.o \
$(OBJDIR)/LoadMf6FileType.o \
$(OBJDIR)/TimeArray.o \
$(OBJDIR)/NumericalPackage.o \
$(OBJDIR)/Xt3dInterface.o \
$(OBJDIR)/SolutionGroup.o \
$(OBJDIR)/Observe.o \
$(OBJDIR)/gwf3hfb8.o \
$(OBJDIR)/gwf3ic8.o \
$(OBJDIR)/OutputControl.o \
$(OBJDIR)/BudgetTerm.o \
$(OBJDIR)/gwt1oc1.o \
$(OBJDIR)/gwf3tvbase8.o \
$(OBJDIR)/TimeArraySeries.o \
$(OBJDIR)/gwf3tvk8.o \
$(OBJDIR)/gwt1ic1.o \
$(OBJDIR)/TimeArraySeriesLink.o \
$(OBJDIR)/gwf3tvs8.o \
$(OBJDIR)/TimeArraySeriesManager.o \
$(OBJDIR)/ObsContainer.o \
$(OBJDIR)/GwtSpc.o \
$(OBJDIR)/gwf3oc8.o \
$(OBJDIR)/BudgetObject.o \
$(OBJDIR)/ObsUtility.o \
$(OBJDIR)/IdmMf6FileLoader.o \
$(OBJDIR)/gwf3sto8.o \
$(OBJDIR)/gwf3dis8.o \
$(OBJDIR)/gwf3mvr8.o \
$(OBJDIR)/gwf3disu8.o \
$(OBJDIR)/Obs3.o \
$(OBJDIR)/gwf3npf8.o \
$(OBJDIR)/gwf3disv8.o \
$(OBJDIR)/gwf3obs8.o \
$(OBJDIR)/gwf3csub8.o \
$(OBJDIR)/gwt1obs1.o \
$(OBJDIR)/BoundaryPackage.o \
$(OBJDIR)/gwf3lak8.o \
$(OBJDIR)/gwt1fmi1.o \
$(OBJDIR)/gwf3maw8.o \
$(OBJDIR)/gwt1ssm1.o \
$(OBJDIR)/gwf3uzf8.o \
$(OBJDIR)/NumericalModel.o \
$(OBJDIR)/gwf3wel8.o \
$(OBJDIR)/gwt1src1.o \
$(OBJDIR)/gwf3api8.o \
$(OBJDIR)/gwf3drn8.o \
$(OBJDIR)/gwt1adv1.o \
$(OBJDIR)/gwf3evt8.o \
$(OBJDIR)/gwf3rch8.o \
$(OBJDIR)/gwt1apt1.o \
$(OBJDIR)/gwf3chd8.o \
$(OBJDIR)/gwf3ghb8.o \
$(OBJDIR)/gwf3riv8.o \
$(OBJDIR)/gwt1mvt1.o \
$(OBJDIR)/gwt1cnc1.o \
$(OBJDIR)/gwt1mwt1.o \
$(OBJDIR)/gwt1mst1.o \
$(OBJDIR)/gwt1ist1.o \
$(OBJDIR)/gwf3sfr8.o \
$(OBJDIR)/gwt1dsp.o \
$(OBJDIR)/gwt1lkt1.o \
$(OBJDIR)/TransportModel.o \
$(OBJDIR)/DistributedModel.o \
$(OBJDIR)/NumericalExchange.o \
$(OBJDIR)/GhostNode.o \
$(OBJDIR)/NumericalSolution.o \
$(OBJDIR)/gwt1uzt1.o \
$(OBJDIR)/gwt1sft1.o \
$(OBJDIR)/gwt1.o \
$(OBJDIR)/gwf3buy8.o \
$(OBJDIR)/DisConnExchange.o \
$(OBJDIR)/gwf3.o \
$(OBJDIR)/CellWithNbrs.o \
$(OBJDIR)/GwfGwfExchange.o \
$(OBJDIR)/GridSorting.o \
$(OBJDIR)/GwtGwtExchange.o \
$(OBJDIR)/GridConnection.o \
$(OBJDIR)/GwfInterfaceModel.o \
$(OBJDIR)/GwtInterfaceModel.o \
$(OBJDIR)/SpatialModelConnection.o \
$(OBJDIR)/GwtGwtConnection.o \
$(OBJDIR)/GwfGwfConnection.o \
$(OBJDIR)/ConnectionBuilder.o \
$(OBJDIR)/GwfGwtExchange.o \
$(OBJDIR)/SimulationCreate.o \
$(OBJDIR)/mf6core.o \
$(OBJDIR)/mf6.o

# Define the objects that make up the program
$(PROGRAM) : $(OBJECTS)
	-$(FC) $(OPTLEVEL) -o $@ $(OBJECTS) $(LDFLAGS)

$(OBJDIR)/%.o : %.f90
	@mkdir -p $(@D)
	$(FC) $(OPTLEVEL) $(FFLAGS) -c $< -o $@ $(INCSWITCH) $(MODSWITCH)

$(OBJDIR)/%.o : %.F90
	@mkdir -p $(@D)
	$(FC) $(OPTLEVEL) $(FFLAGS) -c $< -o $@ $(INCSWITCH) $(MODSWITCH)

