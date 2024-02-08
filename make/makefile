# makefile created by pymake (version 1.2.9.dev0) for the 'mf6' executable.


include ./makedefaults

# Define the source file directories
SOURCEDIR1=../src
SOURCEDIR2=../src/IDM
SOURCEDIR3=../src/Exchange
SOURCEDIR4=../src/Distributed
SOURCEDIR5=../src/GWT
SOURCEDIR6=../src/GWF
SOURCEDIR7=../src/Solution
SOURCEDIR8=../src/Solution/LinearMethods
SOURCEDIR9=../src/Solution/PETSc
SOURCEDIR10=../src/Timing
SOURCEDIR11=../src/Utilities
SOURCEDIR12=../src/Utilities/IDM
SOURCEDIR13=../src/Utilities/IDM/mf6blockfile
SOURCEDIR14=../src/Utilities/TimeSeries
SOURCEDIR15=../src/Utilities/Memory
SOURCEDIR16=../src/Utilities/OutputControl
SOURCEDIR17=../src/Utilities/ArrayRead
SOURCEDIR18=../src/Utilities/Libraries
SOURCEDIR19=../src/Utilities/Libraries/rcm
SOURCEDIR20=../src/Utilities/Libraries/blas
SOURCEDIR21=../src/Utilities/Libraries/sparskit2
SOURCEDIR22=../src/Utilities/Libraries/daglib
SOURCEDIR23=../src/Utilities/Libraries/sparsekit
SOURCEDIR24=../src/Utilities/TimeList
SOURCEDIR25=../src/Utilities/Vector
SOURCEDIR26=../src/Utilities/Matrix
SOURCEDIR27=../src/Utilities/Observation
SOURCEDIR28=../src/Model
SOURCEDIR29=../src/Model/Connection
SOURCEDIR30=../src/Model/ModelUtilities
SOURCEDIR31=../src/Model/Discretization
SOURCEDIR32=../src/Model/TransportModel
SOURCEDIR33=../src/Model/Geometry

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
${SOURCEDIR25} \
${SOURCEDIR26} \
${SOURCEDIR27} \
${SOURCEDIR28} \
${SOURCEDIR29} \
${SOURCEDIR30} \
${SOURCEDIR31} \
${SOURCEDIR32} \
${SOURCEDIR33} 

.SUFFIXES: .f90 .F90 .o

OBJECTS = \
$(OBJDIR)/kind.o \
$(OBJDIR)/Constants.o \
$(OBJDIR)/ErrorUtil.o \
$(OBJDIR)/SimVariables.o \
$(OBJDIR)/ArrayHandlers.o \
$(OBJDIR)/Message.o \
$(OBJDIR)/defmacro.o \
$(OBJDIR)/compilerversion.o \
$(OBJDIR)/version.o \
$(OBJDIR)/Sim.o \
$(OBJDIR)/OpenSpec.o \
$(OBJDIR)/InputOutput.o \
$(OBJDIR)/TableTerm.o \
$(OBJDIR)/Table.o \
$(OBJDIR)/MemoryHelper.o \
$(OBJDIR)/CharString.o \
$(OBJDIR)/Memory.o \
$(OBJDIR)/List.o \
$(OBJDIR)/LongLineReader.o \
$(OBJDIR)/DevFeature.o \
$(OBJDIR)/MemoryList.o \
$(OBJDIR)/TimeSeriesRecord.o \
$(OBJDIR)/MathUtil.o \
$(OBJDIR)/BlockParser.o \
$(OBJDIR)/MemoryManager.o \
$(OBJDIR)/TimeSeries.o \
$(OBJDIR)/ats.o \
$(OBJDIR)/GeomUtil.o \
$(OBJDIR)/TimeSeriesLink.o \
$(OBJDIR)/TimeSeriesFileList.o \
$(OBJDIR)/tdis.o \
$(OBJDIR)/HashTable.o \
$(OBJDIR)/VectorBase.o \
$(OBJDIR)/Sparse.o \
$(OBJDIR)/DisvGeom.o \
$(OBJDIR)/ArrayReaders.o \
$(OBJDIR)/TimeSeriesManager.o \
$(OBJDIR)/SmoothingFunctions.o \
$(OBJDIR)/MemoryManagerExt.o \
$(OBJDIR)/MatrixBase.o \
$(OBJDIR)/ListReader.o \
$(OBJDIR)/Connections.o \
$(OBJDIR)/TimeArray.o \
$(OBJDIR)/ObsOutput.o \
$(OBJDIR)/DiscretizationBase.o \
$(OBJDIR)/InputDefinition.o \
$(OBJDIR)/TimeArraySeries.o \
$(OBJDIR)/ObsOutputList.o \
$(OBJDIR)/Observe.o \
$(OBJDIR)/sim-namidm.o \
$(OBJDIR)/gwt-namidm.o \
$(OBJDIR)/gwt-icidm.o \
$(OBJDIR)/gwt-dspidm.o \
$(OBJDIR)/gwt-disvidm.o \
$(OBJDIR)/gwt-disuidm.o \
$(OBJDIR)/gwt-disidm.o \
$(OBJDIR)/gwt-cncidm.o \
$(OBJDIR)/gwf-welidm.o \
$(OBJDIR)/gwf-rividm.o \
$(OBJDIR)/gwf-rchidm.o \
$(OBJDIR)/gwf-rchaidm.o \
$(OBJDIR)/gwf-npfidm.o \
$(OBJDIR)/gwf-namidm.o \
$(OBJDIR)/gwf-icidm.o \
$(OBJDIR)/gwf-ghbidm.o \
$(OBJDIR)/gwf-evtidm.o \
$(OBJDIR)/gwf-evtaidm.o \
$(OBJDIR)/gwf-drnidm.o \
$(OBJDIR)/gwf-disvidm.o \
$(OBJDIR)/gwf-disuidm.o \
$(OBJDIR)/gwf-disidm.o \
$(OBJDIR)/gwf-chdidm.o \
$(OBJDIR)/exg-gwtgwtidm.o \
$(OBJDIR)/exg-gwfgwtidm.o \
$(OBJDIR)/exg-gwfgwfidm.o \
$(OBJDIR)/TimeArraySeriesLink.o \
$(OBJDIR)/ObsUtility.o \
$(OBJDIR)/ObsContainer.o \
$(OBJDIR)/BudgetFileReader.o \
$(OBJDIR)/IdmDfnSelector.o \
$(OBJDIR)/TimeArraySeriesManager.o \
$(OBJDIR)/PackageMover.o \
$(OBJDIR)/Obs.o \
$(OBJDIR)/NumericalPackage.o \
$(OBJDIR)/Budget.o \
$(OBJDIR)/BudgetTerm.o \
$(OBJDIR)/sort.o \
$(OBJDIR)/SfrCrossSectionUtils.o \
$(OBJDIR)/SourceCommon.o \
$(OBJDIR)/BoundaryPackage.o \
$(OBJDIR)/VirtualBase.o \
$(OBJDIR)/STLVecInt.o \
$(OBJDIR)/PackageBudget.o \
$(OBJDIR)/HeadFileReader.o \
$(OBJDIR)/BudgetObject.o \
$(OBJDIR)/PrintSaveManager.o \
$(OBJDIR)/SfrCrossSectionManager.o \
$(OBJDIR)/dag_module.o \
$(OBJDIR)/BoundaryPackageExt.o \
$(OBJDIR)/VirtualDataLists.o \
$(OBJDIR)/VirtualDataContainer.o \
$(OBJDIR)/SimStages.o \
$(OBJDIR)/BaseModel.o \
$(OBJDIR)/FlowModelInterface.o \
$(OBJDIR)/OutputControlData.o \
$(OBJDIR)/InitialConditions.o \
$(OBJDIR)/Xt3dAlgorithm.o \
$(OBJDIR)/TvBase.o \
$(OBJDIR)/gwf-sfr.o \
$(OBJDIR)/gwf-riv.o \
$(OBJDIR)/gwf-maw.o \
$(OBJDIR)/mf6lists.o \
$(OBJDIR)/gwf-lak.o \
$(OBJDIR)/GwfVscInputData.o \
$(OBJDIR)/gwf-ghb.o \
$(OBJDIR)/gwf-drn.o \
$(OBJDIR)/VirtualModel.o \
$(OBJDIR)/NumericalModel.o \
$(OBJDIR)/BaseExchange.o \
$(OBJDIR)/tsp-spc.o \
$(OBJDIR)/tsp-fmi.o \
$(OBJDIR)/OutputControl.o \
$(OBJDIR)/tsp-ic.o \
$(OBJDIR)/TspAdvOptions.o \
$(OBJDIR)/UzfCellGroup.o \
$(OBJDIR)/Xt3dInterface.o \
$(OBJDIR)/gwf-tvk.o \
$(OBJDIR)/HGeoUtil.o \
$(OBJDIR)/gwf-vsc.o \
$(OBJDIR)/GwfNpfOptions.o \
$(OBJDIR)/IndexMap.o \
$(OBJDIR)/CellWithNbrs.o \
$(OBJDIR)/NumericalExchange.o \
$(OBJDIR)/tsp-ssm.o \
$(OBJDIR)/tsp-oc.o \
$(OBJDIR)/tsp-obs.o \
$(OBJDIR)/tsp-mvt.o \
$(OBJDIR)/tsp-adv.o \
$(OBJDIR)/Disv.o \
$(OBJDIR)/Disu.o \
$(OBJDIR)/Dis.o \
$(OBJDIR)/gwf-uzf.o \
$(OBJDIR)/tsp-apt.o \
$(OBJDIR)/gwt-mst.o \
$(OBJDIR)/GwtDspOptions.o \
$(OBJDIR)/gwf-tvs.o \
$(OBJDIR)/GwfStorageUtils.o \
$(OBJDIR)/Mover.o \
$(OBJDIR)/GwfMvrPeriodData.o \
$(OBJDIR)/ImsLinearMisc.o \
$(OBJDIR)/gwf-npf.o \
$(OBJDIR)/GwfBuyInputData.o \
$(OBJDIR)/InterfaceMap.o \
$(OBJDIR)/SeqVector.o \
$(OBJDIR)/ImsLinearSettings.o \
$(OBJDIR)/ConvergenceSummary.o \
$(OBJDIR)/ArrayReaderBase.o \
$(OBJDIR)/VirtualExchange.o \
$(OBJDIR)/GridSorting.o \
$(OBJDIR)/DisConnExchange.o \
$(OBJDIR)/CsrUtils.o \
$(OBJDIR)/tsp.o \
$(OBJDIR)/gwt-uzt.o \
$(OBJDIR)/gwt-src.o \
$(OBJDIR)/gwt-sft.o \
$(OBJDIR)/gwt-mwt.o \
$(OBJDIR)/gwt-lkt.o \
$(OBJDIR)/gwt-ist.o \
$(OBJDIR)/gwt-dsp.o \
$(OBJDIR)/gwt-cnc.o \
$(OBJDIR)/gwf-api.o \
$(OBJDIR)/gwf-wel.o \
$(OBJDIR)/gwf-rch.o \
$(OBJDIR)/gwf-sto.o \
$(OBJDIR)/gwf-oc.o \
$(OBJDIR)/gwf-obs.o \
$(OBJDIR)/gwf-mvr.o \
$(OBJDIR)/gwf-hfb.o \
$(OBJDIR)/gwf-csub.o \
$(OBJDIR)/gwf-buy.o \
$(OBJDIR)/GhostNode.o \
$(OBJDIR)/gwf-evt.o \
$(OBJDIR)/gwf-chd.o \
$(OBJDIR)/VirtualSolution.o \
$(OBJDIR)/SparseMatrix.o \
$(OBJDIR)/LinearSolverBase.o \
$(OBJDIR)/ImsReordering.o \
$(OBJDIR)/ModflowInput.o \
$(OBJDIR)/IdmLogger.o \
$(OBJDIR)/Integer2dReader.o \
$(OBJDIR)/GridConnection.o \
$(OBJDIR)/DistributedVariable.o \
$(OBJDIR)/gwt.o \
$(OBJDIR)/gwf.o \
$(OBJDIR)/GwfExchangeMover.o \
$(OBJDIR)/RouterBase.o \
$(OBJDIR)/ImsLinearSolver.o \
$(OBJDIR)/ImsLinearBase.o \
$(OBJDIR)/StructVector.o \
$(OBJDIR)/DefinitionSelect.o \
$(OBJDIR)/InputLoadType.o \
$(OBJDIR)/Integer1dReader.o \
$(OBJDIR)/Double2dReader.o \
$(OBJDIR)/Double1dReader.o \
$(OBJDIR)/SpatialModelConnection.o \
$(OBJDIR)/GwtInterfaceModel.o \
$(OBJDIR)/exg-gwtgwt.o \
$(OBJDIR)/GwfInterfaceModel.o \
$(OBJDIR)/exg-gwfgwf.o \
$(OBJDIR)/SerialRouter.o \
$(OBJDIR)/Timer.o \
$(OBJDIR)/LinearSolverFactory.o \
$(OBJDIR)/ImsLinear.o \
$(OBJDIR)/BaseSolution.o \
$(OBJDIR)/StructArray.o \
$(OBJDIR)/BoundInputContext.o \
$(OBJDIR)/AsciiInputLoadType.o \
$(OBJDIR)/LayeredArrayReader.o \
$(OBJDIR)/ExplicitModel.o \
$(OBJDIR)/GwtGwtConnection.o \
$(OBJDIR)/GwfGwfConnection.o \
$(OBJDIR)/RouterFactory.o \
$(OBJDIR)/NumericalSolution.o \
$(OBJDIR)/MappedMemory.o \
$(OBJDIR)/StressListInput.o \
$(OBJDIR)/StressGridInput.o \
$(OBJDIR)/LoadMf6File.o \
$(OBJDIR)/ExplicitSolution.o \
$(OBJDIR)/VirtualGwtModel.o \
$(OBJDIR)/VirtualGwfModel.o \
$(OBJDIR)/VirtualGwtExchange.o \
$(OBJDIR)/VirtualGwfExchange.o \
$(OBJDIR)/exg-gwfgwt.o \
$(OBJDIR)/VirtualDataManager.o \
$(OBJDIR)/Mapper.o \
$(OBJDIR)/IdmMf6File.o \
$(OBJDIR)/IdmModelPackageInput.o \
$(OBJDIR)/SolutionGroup.o \
$(OBJDIR)/SolutionFactory.o \
$(OBJDIR)/ModelFactory.o \
$(OBJDIR)/ExchangeFactory.o \
$(OBJDIR)/RunControl.o \
$(OBJDIR)/SourceLoad.o \
$(OBJDIR)/ModelPackageInputs.o \
$(OBJDIR)/Simulation.o \
$(OBJDIR)/RunControlFactory.o \
$(OBJDIR)/IdmLoad.o \
$(OBJDIR)/ConnectionFactory.o \
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
$(OBJDIR)/Iunit.o \
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

