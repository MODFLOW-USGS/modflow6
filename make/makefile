# makefile created on 2022-01-27 16:38:51.990298
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
$(OBJDIR)/ilut.o \
$(OBJDIR)/rcm.o \
$(OBJDIR)/blas1_d.o \
$(OBJDIR)/sparsekit.o \
$(OBJDIR)/dag_module.o \
$(OBJDIR)/OpenSpec.o \
$(OBJDIR)/kind.o \
$(OBJDIR)/Sparse.o \
$(OBJDIR)/BaseGeometry.o \
$(OBJDIR)/GwtDspGridData.o \
$(OBJDIR)/Constants.o \
$(OBJDIR)/HashTable.o \
$(OBJDIR)/GwtDspOptions.o \
$(OBJDIR)/ims8reordering.o \
$(OBJDIR)/SfrCrossSectionUtils.o \
$(OBJDIR)/GwfNpfGridData.o \
$(OBJDIR)/CsrUtils.o \
$(OBJDIR)/GwfBuyInputData.o \
$(OBJDIR)/GwtAdvOptions.o \
$(OBJDIR)/GwfNpfOptions.o \
$(OBJDIR)/defmacro.o \
$(OBJDIR)/GwfStorageUtils.o \
$(OBJDIR)/SmoothingFunctions.o \
$(OBJDIR)/Xt3dAlgorithm.o \
$(OBJDIR)/SimVariables.o \
$(OBJDIR)/compilerversion.o \
$(OBJDIR)/genericutils.o \
$(OBJDIR)/List.o \
$(OBJDIR)/ArrayHandlers.o \
$(OBJDIR)/mf6lists.o \
$(OBJDIR)/version.o \
$(OBJDIR)/Timer.o \
$(OBJDIR)/Message.o \
$(OBJDIR)/TimeSeriesRecord.o \
$(OBJDIR)/ObsOutput.o \
$(OBJDIR)/StringList.o \
$(OBJDIR)/Sim.o \
$(OBJDIR)/Iunit.o \
$(OBJDIR)/InputOutput.o \
$(OBJDIR)/ObsOutputList.o \
$(OBJDIR)/CircularGeometry.o \
$(OBJDIR)/HeadFileReader.o \
$(OBJDIR)/MemoryHelper.o \
$(OBJDIR)/PrintSaveManager.o \
$(OBJDIR)/RectangularGeometry.o \
$(OBJDIR)/sort.o \
$(OBJDIR)/VectorInt.o \
$(OBJDIR)/ArrayReaders.o \
$(OBJDIR)/BudgetFileReader.o \
$(OBJDIR)/comarg.o \
$(OBJDIR)/Budget.o \
$(OBJDIR)/BlockParser.o \
$(OBJDIR)/DisvGeom.o \
$(OBJDIR)/TableTerm.o \
$(OBJDIR)/ims8base.o \
$(OBJDIR)/TimeSeries.o \
$(OBJDIR)/NameFile.o \
$(OBJDIR)/TimeSeriesLink.o \
$(OBJDIR)/TimeSeriesFileList.o \
$(OBJDIR)/Table.o \
$(OBJDIR)/SfrCrossSectionManager.o \
$(OBJDIR)/Memory.o \
$(OBJDIR)/MemoryList.o \
$(OBJDIR)/ListReader.o \
$(OBJDIR)/MemoryManager.o \
$(OBJDIR)/PackageBudget.o \
$(OBJDIR)/ats.o \
$(OBJDIR)/Connections.o \
$(OBJDIR)/MemorySetHandler.o \
$(OBJDIR)/tdis.o \
$(OBJDIR)/ims8linear.o \
$(OBJDIR)/PackageMover.o \
$(OBJDIR)/UzfCellGroup.o \
$(OBJDIR)/BaseModel.o \
$(OBJDIR)/TimeSeriesManager.o \
$(OBJDIR)/BaseExchange.o \
$(OBJDIR)/DiscretizationBase.o \
$(OBJDIR)/BudgetTerm.o \
$(OBJDIR)/Mover.o \
$(OBJDIR)/gwf3dis8.o \
$(OBJDIR)/BudgetObject.o \
$(OBJDIR)/Xt3dInterface.o \
$(OBJDIR)/BaseSolution.o \
$(OBJDIR)/NumericalPackage.o \
$(OBJDIR)/SolutionGroup.o \
$(OBJDIR)/gwf3mvr8.o \
$(OBJDIR)/gwf3tvbase8.o \
$(OBJDIR)/gwf3disv8.o \
$(OBJDIR)/OutputControlData.o \
$(OBJDIR)/gwf3tvs8.o \
$(OBJDIR)/gwf3disu8.o \
$(OBJDIR)/TimeArray.o \
$(OBJDIR)/Observe.o \
$(OBJDIR)/gwf3hfb8.o \
$(OBJDIR)/gwf3tvk8.o \
$(OBJDIR)/TimeArraySeries.o \
$(OBJDIR)/gwf3ic8.o \
$(OBJDIR)/ObsContainer.o \
$(OBJDIR)/gwf3sto8.o \
$(OBJDIR)/TimeArraySeriesLink.o \
$(OBJDIR)/OutputControl.o \
$(OBJDIR)/gwt1oc1.o \
$(OBJDIR)/gwt1ic1.o \
$(OBJDIR)/gwf3npf8.o \
$(OBJDIR)/ObsUtility.o \
$(OBJDIR)/TimeArraySeriesManager.o \
$(OBJDIR)/Obs3.o \
$(OBJDIR)/gwf3oc8.o \
$(OBJDIR)/GwtSpc.o \
$(OBJDIR)/gwf3obs8.o \
$(OBJDIR)/gwt1obs1.o \
$(OBJDIR)/BoundaryPackage.o \
$(OBJDIR)/NumericalModel.o \
$(OBJDIR)/gwf3ghb8.o \
$(OBJDIR)/gwf3chd8.o \
$(OBJDIR)/gwf3csub8.o \
$(OBJDIR)/gwf3maw8.o \
$(OBJDIR)/gwf3wel8.o \
$(OBJDIR)/gwf3drn8.o \
$(OBJDIR)/gwt1fmi1.o \
$(OBJDIR)/gwf3api8.o \
$(OBJDIR)/gwf3riv8.o \
$(OBJDIR)/gwt1src1.o \
$(OBJDIR)/gwf3uzf8.o \
$(OBJDIR)/GhostNode.o \
$(OBJDIR)/gwt1ssm1.o \
$(OBJDIR)/gwf3evt8.o \
$(OBJDIR)/gwf3sfr8.o \
$(OBJDIR)/gwf3rch8.o \
$(OBJDIR)/CellWithNbrs.o \
$(OBJDIR)/gwt1apt1.o \
$(OBJDIR)/gwf3lak8.o \
$(OBJDIR)/gwt1cnc1.o \
$(OBJDIR)/NumericalExchange.o \
$(OBJDIR)/gwt1adv1.o \
$(OBJDIR)/gwt1mvt1.o \
$(OBJDIR)/NumericalSolution.o \
$(OBJDIR)/gwt1mst1.o \
$(OBJDIR)/gwt1dsp.o \
$(OBJDIR)/gwt1uzt1.o \
$(OBJDIR)/GridSorting.o \
$(OBJDIR)/gwt1sft1.o \
$(OBJDIR)/gwt1mwt1.o \
$(OBJDIR)/gwt1lkt1.o \
$(OBJDIR)/DisConnExchange.o \
$(OBJDIR)/gwf3buy8.o \
$(OBJDIR)/gwf3.o \
$(OBJDIR)/gwt1ist1.o \
$(OBJDIR)/GridConnection.o \
$(OBJDIR)/GwfGwfExchange.o \
$(OBJDIR)/gwt1.o \
$(OBJDIR)/GwtInterfaceModel.o \
$(OBJDIR)/GwtGwtExchange.o \
$(OBJDIR)/SpatialModelConnection.o \
$(OBJDIR)/GwfInterfaceModel.o \
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

