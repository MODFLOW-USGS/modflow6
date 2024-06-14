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
SOURCEDIR9=../src/Distributed
SOURCEDIR10=../src/Solution
SOURCEDIR11=../src/Solution/PETSc
SOURCEDIR12=../src/Solution/LinearMethods
SOURCEDIR13=../src/Timing
SOURCEDIR14=../src/Utilities
SOURCEDIR15=../src/Utilities/TimeSeries
SOURCEDIR16=../src/Utilities/Libraries
SOURCEDIR17=../src/Utilities/Libraries/rcm
SOURCEDIR18=../src/Utilities/Libraries/sparsekit
SOURCEDIR19=../src/Utilities/Libraries/sparskit2
SOURCEDIR20=../src/Utilities/Libraries/blas
SOURCEDIR21=../src/Utilities/Libraries/daglib
SOURCEDIR22=../src/Utilities/Idm
SOURCEDIR23=../src/Utilities/Idm/selector
SOURCEDIR24=../src/Utilities/Idm/mf6blockfile
SOURCEDIR25=../src/Utilities/Matrix
SOURCEDIR26=../src/Utilities/Vector
SOURCEDIR27=../src/Utilities/Observation
SOURCEDIR28=../src/Utilities/OutputControl
SOURCEDIR29=../src/Utilities/Memory
SOURCEDIR30=../src/Utilities/ArrayRead

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
${SOURCEDIR30} 

.SUFFIXES: .f90 .F90 .o

OBJECTS = \
$(OBJDIR)/ilut.o \
$(OBJDIR)/kind.o \
$(OBJDIR)/VectorBase.o \
$(OBJDIR)/IdmLogger.o \
$(OBJDIR)/BaseGeometry.o \
$(OBJDIR)/InputDefinition.o \
$(OBJDIR)/SimStages.o \
$(OBJDIR)/GwtDspOptions.o \
$(OBJDIR)/gwf-npfidm.o \
$(OBJDIR)/Sparse.o \
$(OBJDIR)/GwtAdvOptions.o \
$(OBJDIR)/gwt-disvidm.o \
$(OBJDIR)/gwt-disidm.o \
$(OBJDIR)/gwf-disidm.o \
$(OBJDIR)/simnamidm.o \
$(OBJDIR)/gwf-disuidm.o \
$(OBJDIR)/CsrUtils.o \
$(OBJDIR)/gwtidm.o \
$(OBJDIR)/gwf-disvidm.o \
$(OBJDIR)/blas1_d.o \
$(OBJDIR)/gwt-dspidm.o \
$(OBJDIR)/CharString.o \
$(OBJDIR)/OpenSpec.o \
$(OBJDIR)/dag_module.o \
$(OBJDIR)/ims8reordering.o \
$(OBJDIR)/Constants.o \
$(OBJDIR)/rcm.o \
$(OBJDIR)/HashTable.o \
$(OBJDIR)/sparsekit.o \
$(OBJDIR)/gwt-disuidm.o \
$(OBJDIR)/gwfidm.o \
$(OBJDIR)/SfrCrossSectionUtils.o \
$(OBJDIR)/MatrixBase.o \
$(OBJDIR)/compilerversion.o \
$(OBJDIR)/defmacro.o \
$(OBJDIR)/SimVariables.o \
$(OBJDIR)/SmoothingFunctions.o \
$(OBJDIR)/GwfVscInputData.o \
$(OBJDIR)/GwfStorageUtils.o \
$(OBJDIR)/Xt3dAlgorithm.o \
$(OBJDIR)/GwfNpfOptions.o \
$(OBJDIR)/GwfBuyInputData.o \
$(OBJDIR)/ims8misc.o \
$(OBJDIR)/LinearSolverBase.o \
$(OBJDIR)/ArrayHandlers.o \
$(OBJDIR)/IndexMap.o \
$(OBJDIR)/version.o \
$(OBJDIR)/InterfaceMap.o \
$(OBJDIR)/Message.o \
$(OBJDIR)/List.o \
$(OBJDIR)/Sim.o \
$(OBJDIR)/Timer.o \
$(OBJDIR)/MemoryHelper.o \
$(OBJDIR)/sort.o \
$(OBJDIR)/StringList.o \
$(OBJDIR)/mf6lists.o \
$(OBJDIR)/IdmGwfDfnSelector.o \
$(OBJDIR)/IdmGwtDfnSelector.o \
$(OBJDIR)/ObsOutput.o \
$(OBJDIR)/Budget.o \
$(OBJDIR)/IdmSimDfnSelector.o \
$(OBJDIR)/DistributedVariable.o \
$(OBJDIR)/InputOutput.o \
$(OBJDIR)/VirtualDataLists.o \
$(OBJDIR)/DisvGeom.o \
$(OBJDIR)/Iunit.o \
$(OBJDIR)/TimeSeriesRecord.o \
$(OBJDIR)/TableTerm.o \
$(OBJDIR)/HeadFileReader.o \
$(OBJDIR)/PrintSaveManager.o \
$(OBJDIR)/IdmDfnSelector.o \
$(OBJDIR)/DefinitionSelect.o \
$(OBJDIR)/ArrayReaders.o \
$(OBJDIR)/comarg.o \
$(OBJDIR)/STLVecInt.o \
$(OBJDIR)/BlockParser.o \
$(OBJDIR)/CircularGeometry.o \
$(OBJDIR)/BudgetFileReader.o \
$(OBJDIR)/RectangularGeometry.o \
$(OBJDIR)/ObsOutputList.o \
$(OBJDIR)/ims8base.o \
$(OBJDIR)/TimeSeries.o \
$(OBJDIR)/TimeSeriesFileList.o \
$(OBJDIR)/ArrayReaderBase.o \
$(OBJDIR)/ModflowInput.o \
$(OBJDIR)/TimeSeriesLink.o \
$(OBJDIR)/Double1dReader.o \
$(OBJDIR)/Double2dReader.o \
$(OBJDIR)/Table.o \
$(OBJDIR)/Integer2dReader.o \
$(OBJDIR)/StructVector.o \
$(OBJDIR)/SfrCrossSectionManager.o \
$(OBJDIR)/ListReader.o \
$(OBJDIR)/Memory.o \
$(OBJDIR)/MemoryStore.o \
$(OBJDIR)/MemoryManager.o \
$(OBJDIR)/ats.o \
$(OBJDIR)/Integer1dReader.o \
$(OBJDIR)/StructArray.o \
$(OBJDIR)/MemorySetHandler.o \
$(OBJDIR)/MappedMemory.o \
$(OBJDIR)/ModelPackageInputs.o \
$(OBJDIR)/BaseModel.o \
$(OBJDIR)/GwfMvrPeriodData.o \
$(OBJDIR)/ims8linear.o \
$(OBJDIR)/MemoryManagerExt.o \
$(OBJDIR)/Connections.o \
$(OBJDIR)/SeqVector.o \
$(OBJDIR)/tdis.o \
$(OBJDIR)/PackageMover.o \
$(OBJDIR)/PackageBudget.o \
$(OBJDIR)/TimeSeriesManager.o \
$(OBJDIR)/Mover.o \
$(OBJDIR)/VirtualBase.o \
$(OBJDIR)/VirtualDataContainer.o \
$(OBJDIR)/BaseExchange.o \
$(OBJDIR)/SparseMatrix.o \
$(OBJDIR)/DiscretizationBase.o \
$(OBJDIR)/UzfCellGroup.o \
$(OBJDIR)/BudgetTerm.o \
$(OBJDIR)/Observe.o \
$(OBJDIR)/OutputControlData.o \
$(OBJDIR)/gwf-dis.o \
$(OBJDIR)/LayeredArrayReader.o \
$(OBJDIR)/TimeArray.o \
$(OBJDIR)/NumericalPackage.o \
$(OBJDIR)/LoadMf6File.o \
$(OBJDIR)/ExplicitModel.o \
$(OBJDIR)/BaseSolution.o \
$(OBJDIR)/gwf-disu.o \
$(OBJDIR)/TimeArraySeries.o \
$(OBJDIR)/SolutionGroup.o \
$(OBJDIR)/ObsContainer.o \
$(OBJDIR)/ExplicitSolution.o \
$(OBJDIR)/TimeArraySeriesLink.o \
$(OBJDIR)/Xt3dInterface.o \
$(OBJDIR)/gwf-disv.o \
$(OBJDIR)/ImsLinearSolver.o \
$(OBJDIR)/TimeArraySeriesManager.o \
$(OBJDIR)/BudgetObject.o \
$(OBJDIR)/ObsUtility.o \
$(OBJDIR)/TvBase.o \
$(OBJDIR)/gwf-tvs.o \
$(OBJDIR)/IdmMf6File.o \
$(OBJDIR)/OutputControl.o \
$(OBJDIR)/LinearSolverFactory.o \
$(OBJDIR)/gwf-ic.o \
$(OBJDIR)/Obs.o \
$(OBJDIR)/gwf-tvk.o \
$(OBJDIR)/GwtSpc.o \
$(OBJDIR)/IdmSimulation.o \
$(OBJDIR)/gwt1oc1.o \
$(OBJDIR)/gwf-mvr.o \
$(OBJDIR)/gwt-ic.o \
$(OBJDIR)/gwt1obs1.o \
$(OBJDIR)/gwf-obs.o \
$(OBJDIR)/gwf-oc.o \
$(OBJDIR)/gwf-sto.o \
$(OBJDIR)/BoundaryPackage.o \
$(OBJDIR)/gwf-csub.o \
$(OBJDIR)/gwf-uzf.o \
$(OBJDIR)/gwt-cnc.o \
$(OBJDIR)/gwf-lak.o \
$(OBJDIR)/gwt-src.o \
$(OBJDIR)/gwf-maw.o \
$(OBJDIR)/gwt-fmi.o \
$(OBJDIR)/gwf-rch.o \
$(OBJDIR)/gwt-apt.o \
$(OBJDIR)/gwf-wel.o \
$(OBJDIR)/gwf-riv.o \
$(OBJDIR)/gwf-drn.o \
$(OBJDIR)/gwt-mwt.o \
$(OBJDIR)/gwf-sfr.o \
$(OBJDIR)/gwf-api.o \
$(OBJDIR)/gwf-evt.o \
$(OBJDIR)/NumericalModel.o \
$(OBJDIR)/gwf-ghb.o \
$(OBJDIR)/gwf-chd.o \
$(OBJDIR)/gwt-uzt.o \
$(OBJDIR)/gwf-vsc.o \
$(OBJDIR)/NumericalExchange.o \
$(OBJDIR)/gwt-ssm.o \
$(OBJDIR)/NumericalSolution.o \
$(OBJDIR)/gwt-adv.o \
$(OBJDIR)/gwt-lkt.o \
$(OBJDIR)/SolutionFactory.o \
$(OBJDIR)/VirtualModel.o \
$(OBJDIR)/gwt-sft.o \
$(OBJDIR)/gwt-mst.o \
$(OBJDIR)/VirtualSolution.o \
$(OBJDIR)/VirtualGwfModel.o \
$(OBJDIR)/gwt-ist.o \
$(OBJDIR)/gwf-npf.o \
$(OBJDIR)/TransportModel.o \
$(OBJDIR)/DisConnExchange.o \
$(OBJDIR)/GhostNode.o \
$(OBJDIR)/gwt-mvt.o \
$(OBJDIR)/VirtualExchange.o \
$(OBJDIR)/gwt-dsp.o \
$(OBJDIR)/gwf-buy.o \
$(OBJDIR)/VirtualGwfExchange.o \
$(OBJDIR)/gwf-hfb.o \
$(OBJDIR)/VirtualGwtExchange.o \
$(OBJDIR)/VirtualGwtModel.o \
$(OBJDIR)/CellWithNbrs.o \
$(OBJDIR)/RouterBase.o \
$(OBJDIR)/gwf.o \
$(OBJDIR)/gwt.o \
$(OBJDIR)/GridSorting.o \
$(OBJDIR)/GwfGwfExchange.o \
$(OBJDIR)/SerialRouter.o \
$(OBJDIR)/RouterFactory.o \
$(OBJDIR)/GwtGwtExchange.o \
$(OBJDIR)/GridConnection.o \
$(OBJDIR)/SpatialModelConnection.o \
$(OBJDIR)/Mapper.o \
$(OBJDIR)/VirtualDataManager.o \
$(OBJDIR)/GwfInterfaceModel.o \
$(OBJDIR)/RunControl.o \
$(OBJDIR)/GwtInterfaceModel.o \
$(OBJDIR)/RunControlFactory.o \
$(OBJDIR)/GwfGwfConnection.o \
$(OBJDIR)/GwtGwtConnection.o \
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

