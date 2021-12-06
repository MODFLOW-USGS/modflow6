# makefile created on 2021-12-06 11:32:24.533931
# by pymake (version 1.2.1) for the 'mf6' executable 
# using the 'gfortran' fortran compiler(s).


include ./makedefaults

# Define the source file directories
SOURCEDIR1=../src
SOURCEDIR2=../src/Exchange
SOURCEDIR3=../src/Model
SOURCEDIR4=../src/Model/Connection
SOURCEDIR5=../src/Model/Geometry
SOURCEDIR6=../src/Model/GroundWaterFlow
SOURCEDIR7=../src/Model/GroundWaterTransport
SOURCEDIR8=../src/Model/ModelUtilities
SOURCEDIR9=../src/Solution
SOURCEDIR10=../src/Solution/SparseMatrixSolver
SOURCEDIR11=../src/Timing
SOURCEDIR12=../src/Utilities
SOURCEDIR13=../src/Utilities/Libraries
SOURCEDIR14=../src/Utilities/Libraries/blas
SOURCEDIR15=../src/Utilities/Libraries/daglib
SOURCEDIR16=../src/Utilities/Libraries/rcm
SOURCEDIR17=../src/Utilities/Libraries/sparsekit
SOURCEDIR18=../src/Utilities/Libraries/sparskit2
SOURCEDIR19=../src/Utilities/Memory
SOURCEDIR20=../src/Utilities/Observation
SOURCEDIR21=../src/Utilities/OutputControl
SOURCEDIR22=../src/Utilities/TimeSeries

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
$(OBJDIR)/sparsekit.o \
$(OBJDIR)/kind.o \
$(OBJDIR)/rcm.o \
$(OBJDIR)/Constants.o \
$(OBJDIR)/GwtDspGridData.o \
$(OBJDIR)/HashTable.o \
$(OBJDIR)/BaseGeometry.o \
$(OBJDIR)/SimVariables.o \
$(OBJDIR)/GwfNpfGridData.o \
$(OBJDIR)/compilerversion.o \
$(OBJDIR)/genericutils.o \
$(OBJDIR)/ilut.o \
$(OBJDIR)/GwfStorageUtils.o \
$(OBJDIR)/OpenSpec.o \
$(OBJDIR)/Sparse.o \
$(OBJDIR)/List.o \
$(OBJDIR)/version.o \
$(OBJDIR)/dag_module.o \
$(OBJDIR)/CsrUtils.o \
$(OBJDIR)/SmoothingFunctions.o \
$(OBJDIR)/SfrCrossSectionUtils.o \
$(OBJDIR)/GwtDspOptions.o \
$(OBJDIR)/blas1_d.o \
$(OBJDIR)/defmacro.o \
$(OBJDIR)/Xt3dAlgorithm.o \
$(OBJDIR)/GwfNpfOptions.o \
$(OBJDIR)/ims8reordering.o \
$(OBJDIR)/GwtAdvOptions.o \
$(OBJDIR)/ArrayHandlers.o \
$(OBJDIR)/StringList.o \
$(OBJDIR)/ObsOutput.o \
$(OBJDIR)/Timer.o \
$(OBJDIR)/mf6lists.o \
$(OBJDIR)/TimeSeriesRecord.o \
$(OBJDIR)/Message.o \
$(OBJDIR)/Sim.o \
$(OBJDIR)/sort.o \
$(OBJDIR)/MemoryHelper.o \
$(OBJDIR)/Budget.o \
$(OBJDIR)/VectorInt.o \
$(OBJDIR)/Iunit.o \
$(OBJDIR)/InputOutput.o \
$(OBJDIR)/CircularGeometry.o \
$(OBJDIR)/BlockParser.o \
$(OBJDIR)/TableTerm.o \
$(OBJDIR)/PrintSaveManager.o \
$(OBJDIR)/DisvGeom.o \
$(OBJDIR)/BudgetFileReader.o \
$(OBJDIR)/RectangularGeometry.o \
$(OBJDIR)/ims8base.o \
$(OBJDIR)/ObsOutputList.o \
$(OBJDIR)/Table.o \
$(OBJDIR)/comarg.o \
$(OBJDIR)/HeadFileReader.o \
$(OBJDIR)/ArrayReaders.o \
$(OBJDIR)/NameFile.o \
$(OBJDIR)/ListReader.o \
$(OBJDIR)/TimeSeries.o \
$(OBJDIR)/Memory.o \
$(OBJDIR)/MemoryList.o \
$(OBJDIR)/TimeSeriesFileList.o \
$(OBJDIR)/SfrCrossSectionManager.o \
$(OBJDIR)/TimeSeriesLink.o \
$(OBJDIR)/MemoryManager.o \
$(OBJDIR)/BaseModel.o \
$(OBJDIR)/ims8linear.o \
$(OBJDIR)/Connections.o \
$(OBJDIR)/ats.o \
$(OBJDIR)/PackageMover.o \
$(OBJDIR)/PackageBudget.o \
$(OBJDIR)/tdis.o \
$(OBJDIR)/TimeSeriesManager.o \
$(OBJDIR)/MemorySetHandler.o \
$(OBJDIR)/Mover.o \
$(OBJDIR)/DiscretizationBase.o \
$(OBJDIR)/Xt3dInterface.o \
$(OBJDIR)/BaseExchange.o \
$(OBJDIR)/BudgetTerm.o \
$(OBJDIR)/gwf3disu8.o \
$(OBJDIR)/UzfCellGroup.o \
$(OBJDIR)/OutputControlData.o \
$(OBJDIR)/BudgetObject.o \
$(OBJDIR)/BaseSolution.o \
$(OBJDIR)/Observe.o \
$(OBJDIR)/TimeArray.o \
$(OBJDIR)/gwf3disv8.o \
$(OBJDIR)/TimeArraySeries.o \
$(OBJDIR)/TimeArraySeriesLink.o \
$(OBJDIR)/gwf3dis8.o \
$(OBJDIR)/SolutionGroup.o \
$(OBJDIR)/OutputControl.o \
$(OBJDIR)/NumericalPackage.o \
$(OBJDIR)/TimeArraySeriesManager.o \
$(OBJDIR)/ObsUtility.o \
$(OBJDIR)/gwf3tvbase8.o \
$(OBJDIR)/gwf3mvr8.o \
$(OBJDIR)/ObsContainer.o \
$(OBJDIR)/gwt1oc1.o \
$(OBJDIR)/gwf3ic8.o \
$(OBJDIR)/gwf3hfb8.o \
$(OBJDIR)/gwf3oc8.o \
$(OBJDIR)/gwt1ic1.o \
$(OBJDIR)/gwf3tvs8.o \
$(OBJDIR)/GwtSpc.o \
$(OBJDIR)/Obs3.o \
$(OBJDIR)/gwf3tvk8.o \
$(OBJDIR)/gwf3sto8.o \
$(OBJDIR)/gwf3obs8.o \
$(OBJDIR)/gwf3npf8.o \
$(OBJDIR)/gwf3csub8.o \
$(OBJDIR)/BoundaryPackage.o \
$(OBJDIR)/gwt1obs1.o \
$(OBJDIR)/gwt1cnc1.o \
$(OBJDIR)/gwf3chd8.o \
$(OBJDIR)/gwf3maw8.o \
$(OBJDIR)/gwf3wel8.o \
$(OBJDIR)/gwf3drn8.o \
$(OBJDIR)/gwf3evt8.o \
$(OBJDIR)/gwf3rch8.o \
$(OBJDIR)/gwf3api8.o \
$(OBJDIR)/gwf3uzf8.o \
$(OBJDIR)/gwf3sfr8.o \
$(OBJDIR)/gwt1fmi1.o \
$(OBJDIR)/gwt1src1.o \
$(OBJDIR)/gwf3lak8.o \
$(OBJDIR)/gwt1mst1.o \
$(OBJDIR)/gwf3riv8.o \
$(OBJDIR)/NumericalModel.o \
$(OBJDIR)/gwt1dsp.o \
$(OBJDIR)/gwf3buy8.o \
$(OBJDIR)/gwf3ghb8.o \
$(OBJDIR)/gwt1mvt1.o \
$(OBJDIR)/gwt1adv1.o \
$(OBJDIR)/GhostNode.o \
$(OBJDIR)/Topology.o \
$(OBJDIR)/NumericalExchange.o \
$(OBJDIR)/gwt1ssm1.o \
$(OBJDIR)/gwt1apt1.o \
$(OBJDIR)/NumericalSolution.o \
$(OBJDIR)/DisConnExchange.o \
$(OBJDIR)/GridSorting.o \
$(OBJDIR)/gwt1ist1.o \
$(OBJDIR)/gwt1mwt1.o \
$(OBJDIR)/gwt1sft1.o \
$(OBJDIR)/gwt1lkt1.o \
$(OBJDIR)/gwf3.o \
$(OBJDIR)/GridConnection.o \
$(OBJDIR)/gwt1uzt1.o \
$(OBJDIR)/GwfGwfExchange.o \
$(OBJDIR)/SpatialModelConnection.o \
$(OBJDIR)/GwfInterfaceModel.o \
$(OBJDIR)/GwfGwfConnection.o \
$(OBJDIR)/gwt1.o \
$(OBJDIR)/GwtGwtExchange.o \
$(OBJDIR)/GwtInterfaceModel.o \
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

