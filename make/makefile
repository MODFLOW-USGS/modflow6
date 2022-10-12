# makefile created by pymake (version 1.2.5) for the 'mf6' executable.


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
SOURCEDIR20=../src/Utilities/Observation
SOURCEDIR21=../src/Utilities/OutputControl
SOURCEDIR22=../src/Utilities/Memory

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
${SOURCEDIR22} 

.SUFFIXES: .f90 .F90 .o

OBJECTS = \
$(OBJDIR)/CharString.o \
$(OBJDIR)/kind.o \
$(OBJDIR)/blas1_d.o \
$(OBJDIR)/Constants.o \
$(OBJDIR)/InterfaceMap.o \
$(OBJDIR)/dag_module.o \
$(OBJDIR)/GwtDspOptions.o \
$(OBJDIR)/ims8misc.o \
$(OBJDIR)/rcm.o \
$(OBJDIR)/CsrUtils.o \
$(OBJDIR)/compilerversion.o \
$(OBJDIR)/ims8reordering.o \
$(OBJDIR)/GwfBuyInputData.o \
$(OBJDIR)/sparsekit.o \
$(OBJDIR)/GwfStorageUtils.o \
$(OBJDIR)/Xt3dAlgorithm.o \
$(OBJDIR)/BaseGeometry.o \
$(OBJDIR)/defmacro.o \
$(OBJDIR)/ilut.o \
$(OBJDIR)/SimVariables.o \
$(OBJDIR)/SmoothingFunctions.o \
$(OBJDIR)/HashTable.o \
$(OBJDIR)/Sparse.o \
$(OBJDIR)/GwtAdvOptions.o \
$(OBJDIR)/OpenSpec.o \
$(OBJDIR)/GwfNpfOptions.o \
$(OBJDIR)/SfrCrossSectionUtils.o \
$(OBJDIR)/genericutils.o \
$(OBJDIR)/List.o \
$(OBJDIR)/ObsOutput.o \
$(OBJDIR)/version.o \
$(OBJDIR)/ArrayHandlers.o \
$(OBJDIR)/StringList.o \
$(OBJDIR)/TimeSeriesRecord.o \
$(OBJDIR)/Timer.o \
$(OBJDIR)/mf6lists.o \
$(OBJDIR)/Message.o \
$(OBJDIR)/Sim.o \
$(OBJDIR)/Iunit.o \
$(OBJDIR)/VectorInt.o \
$(OBJDIR)/Budget.o \
$(OBJDIR)/sort.o \
$(OBJDIR)/MemoryHelper.o \
$(OBJDIR)/InputOutput.o \
$(OBJDIR)/CircularGeometry.o \
$(OBJDIR)/ArrayReaders.o \
$(OBJDIR)/BlockParser.o \
$(OBJDIR)/ims8base.o \
$(OBJDIR)/comarg.o \
$(OBJDIR)/BudgetFileReader.o \
$(OBJDIR)/PrintSaveManager.o \
$(OBJDIR)/DisvGeom.o \
$(OBJDIR)/NameFile.o \
$(OBJDIR)/TimeSeries.o \
$(OBJDIR)/RectangularGeometry.o \
$(OBJDIR)/TimeSeriesFileList.o \
$(OBJDIR)/ObsOutputList.o \
$(OBJDIR)/TimeSeriesLink.o \
$(OBJDIR)/HeadFileReader.o \
$(OBJDIR)/TableTerm.o \
$(OBJDIR)/Table.o \
$(OBJDIR)/ListReader.o \
$(OBJDIR)/SfrCrossSectionManager.o \
$(OBJDIR)/Memory.o \
$(OBJDIR)/MemoryList.o \
$(OBJDIR)/MemoryManager.o \
$(OBJDIR)/MemorySetHandler.o \
$(OBJDIR)/GwfMvrPeriodData.o \
$(OBJDIR)/ims8linear.o \
$(OBJDIR)/MappedVariable.o \
$(OBJDIR)/BaseModel.o \
$(OBJDIR)/PackageBudget.o \
$(OBJDIR)/PackageMover.o \
$(OBJDIR)/DistributedData.o \
$(OBJDIR)/ats.o \
$(OBJDIR)/tdis.o \
$(OBJDIR)/Connections.o \
$(OBJDIR)/TimeSeriesManager.o \
$(OBJDIR)/Mover.o \
$(OBJDIR)/UzfCellGroup.o \
$(OBJDIR)/BaseExchange.o \
$(OBJDIR)/DiscretizationBase.o \
$(OBJDIR)/NumericalPackage.o \
$(OBJDIR)/Observe.o \
$(OBJDIR)/BaseSolution.o \
$(OBJDIR)/gwf3disv8.o \
$(OBJDIR)/OutputControlData.o \
$(OBJDIR)/gwf3dis8.o \
$(OBJDIR)/TimeArray.o \
$(OBJDIR)/gwf3ic8.o \
$(OBJDIR)/gwf3tvbase8.o \
$(OBJDIR)/TimeArraySeries.o \
$(OBJDIR)/gwt1ic1.o \
$(OBJDIR)/TimeArraySeriesLink.o \
$(OBJDIR)/gwf3disu8.o \
$(OBJDIR)/BudgetTerm.o \
$(OBJDIR)/TimeArraySeriesManager.o \
$(OBJDIR)/Xt3dInterface.o \
$(OBJDIR)/ObsContainer.o \
$(OBJDIR)/SolutionGroup.o \
$(OBJDIR)/GwtSpc.o \
$(OBJDIR)/gwf3tvs8.o \
$(OBJDIR)/ObsUtility.o \
$(OBJDIR)/OutputControl.o \
$(OBJDIR)/gwf3tvk8.o \
$(OBJDIR)/gwf3hfb8.o \
$(OBJDIR)/BudgetObject.o \
$(OBJDIR)/Obs3.o \
$(OBJDIR)/gwf3npf8.o \
$(OBJDIR)/gwt1obs1.o \
$(OBJDIR)/gwf3mvr8.o \
$(OBJDIR)/gwt1oc1.o \
$(OBJDIR)/gwf3sto8.o \
$(OBJDIR)/BoundaryPackage.o \
$(OBJDIR)/gwf3oc8.o \
$(OBJDIR)/gwf3rch8.o \
$(OBJDIR)/gwf3obs8.o \
$(OBJDIR)/gwf3ghb8.o \
$(OBJDIR)/gwf3uzf8.o \
$(OBJDIR)/gwt1src1.o \
$(OBJDIR)/gwf3wel8.o \
$(OBJDIR)/gwf3lak8.o \
$(OBJDIR)/gwf3maw8.o \
$(OBJDIR)/gwf3sfr8.o \
$(OBJDIR)/gwf3api8.o \
$(OBJDIR)/gwf3riv8.o \
$(OBJDIR)/gwf3buy8.o \
$(OBJDIR)/gwf3chd8.o \
$(OBJDIR)/gwf3evt8.o \
$(OBJDIR)/gwf3csub8.o \
$(OBJDIR)/gwf3drn8.o \
$(OBJDIR)/gwt1fmi1.o \
$(OBJDIR)/gwt1cnc1.o \
$(OBJDIR)/NumericalModel.o \
$(OBJDIR)/gwt1dsp.o \
$(OBJDIR)/gwt1apt1.o \
$(OBJDIR)/TransportModel.o \
$(OBJDIR)/gwt1uzt1.o \
$(OBJDIR)/gwt1adv1.o \
$(OBJDIR)/gwt1ssm1.o \
$(OBJDIR)/gwt1mst1.o \
$(OBJDIR)/gwt1sft1.o \
$(OBJDIR)/NumericalExchange.o \
$(OBJDIR)/gwt1mwt1.o \
$(OBJDIR)/GhostNode.o \
$(OBJDIR)/gwt1mvt1.o \
$(OBJDIR)/gwt1lkt1.o \
$(OBJDIR)/gwt1ist1.o \
$(OBJDIR)/DistributedModel.o \
$(OBJDIR)/gwt1.o \
$(OBJDIR)/NumericalSolution.o \
$(OBJDIR)/DisConnExchange.o \
$(OBJDIR)/gwf3.o \
$(OBJDIR)/CellWithNbrs.o \
$(OBJDIR)/GridSorting.o \
$(OBJDIR)/GwtGwtExchange.o \
$(OBJDIR)/GwfGwfExchange.o \
$(OBJDIR)/GridConnection.o \
$(OBJDIR)/GwfInterfaceModel.o \
$(OBJDIR)/GwtInterfaceModel.o \
$(OBJDIR)/SpatialModelConnection.o \
$(OBJDIR)/GwfGwfConnection.o \
$(OBJDIR)/GwtGwtConnection.o \
$(OBJDIR)/GwfGwtExchange.o \
$(OBJDIR)/ConnectionBuilder.o \
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

