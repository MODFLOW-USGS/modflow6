# makefile created by pymake (version 1.2.10.dev0) for the 'mf6' executable.


include ./makedefaults

# Define the source file directories
SOURCEDIR1=../src
SOURCEDIR2=../src/Exchange
SOURCEDIR3=../src/Distributed
SOURCEDIR4=../src/Solution
SOURCEDIR5=../src/Solution/LinearMethods
SOURCEDIR6=../src/Solution/PETSc
SOURCEDIR7=../src/Timing
SOURCEDIR8=../src/Utilities
SOURCEDIR9=../src/Utilities/Idm
SOURCEDIR10=../src/Utilities/Idm/selector
SOURCEDIR11=../src/Utilities/Idm/mf6blockfile
SOURCEDIR12=../src/Utilities/TimeSeries
SOURCEDIR13=../src/Utilities/Memory
SOURCEDIR14=../src/Utilities/OutputControl
SOURCEDIR15=../src/Utilities/ArrayRead
SOURCEDIR16=../src/Utilities/Libraries
SOURCEDIR17=../src/Utilities/Libraries/rcm
SOURCEDIR18=../src/Utilities/Libraries/blas
SOURCEDIR19=../src/Utilities/Libraries/sparskit2
SOURCEDIR20=../src/Utilities/Libraries/daglib
SOURCEDIR21=../src/Utilities/Libraries/sparsekit
SOURCEDIR22=../src/Utilities/Vector
SOURCEDIR23=../src/Utilities/Matrix
SOURCEDIR24=../src/Utilities/Observation
SOURCEDIR25=../src/Model
SOURCEDIR26=../src/Model/Connection
SOURCEDIR27=../src/Model/SurfaceWaterFlow
SOURCEDIR28=../src/Model/GroundWaterTransport
SOURCEDIR29=../src/Model/ModelUtilities
SOURCEDIR30=../src/Model/GroundWaterFlow
SOURCEDIR31=../src/Model/TransportModel
SOURCEDIR32=../src/Model/Geometry
SOURCEDIR33=../src/Model/GroundWaterEnergy

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
$(OBJDIR)/InputDefinition.o \
$(OBJDIR)/Memory.o \
$(OBJDIR)/List.o \
$(OBJDIR)/swf-zdgidm.o \
$(OBJDIR)/swf-namidm.o \
$(OBJDIR)/swf-icidm.o \
$(OBJDIR)/swf-flwidm.o \
$(OBJDIR)/swf-dislidm.o \
$(OBJDIR)/swf-dfwidm.o \
$(OBJDIR)/swf-cxsidm.o \
$(OBJDIR)/swf-chdidm.o \
$(OBJDIR)/simtdisidm.o \
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
$(OBJDIR)/gwe-namidm.o \
$(OBJDIR)/gwe-icidm.o \
$(OBJDIR)/gwe-disvidm.o \
$(OBJDIR)/gwe-disuidm.o \
$(OBJDIR)/gwe-disidm.o \
$(OBJDIR)/gwe-ctpidm.o \
$(OBJDIR)/gwe-cndidm.o \
$(OBJDIR)/exg-swfgwfidm.o \
$(OBJDIR)/exg-gwtgwtidm.o \
$(OBJDIR)/exg-gwfgwtidm.o \
$(OBJDIR)/exg-gwfgwfidm.o \
$(OBJDIR)/exg-gwfgweidm.o \
$(OBJDIR)/exg-gwegweidm.o \
$(OBJDIR)/LongLineReader.o \
$(OBJDIR)/DevFeature.o \
$(OBJDIR)/MemoryList.o \
$(OBJDIR)/IdmSwfDfnSelector.o \
$(OBJDIR)/IdmSimDfnSelector.o \
$(OBJDIR)/IdmGwtDfnSelector.o \
$(OBJDIR)/IdmGwfDfnSelector.o \
$(OBJDIR)/IdmGweDfnSelector.o \
$(OBJDIR)/IdmExgDfnSelector.o \
$(OBJDIR)/TimeSeriesRecord.o \
$(OBJDIR)/MathUtil.o \
$(OBJDIR)/BlockParser.o \
$(OBJDIR)/MemoryManager.o \
$(OBJDIR)/IdmDfnSelector.o \
$(OBJDIR)/TimeSeries.o \
$(OBJDIR)/SourceCommon.o \
$(OBJDIR)/MemoryManagerExt.o \
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
$(OBJDIR)/MatrixBase.o \
$(OBJDIR)/ListReader.o \
$(OBJDIR)/Connections.o \
$(OBJDIR)/TimeArray.o \
$(OBJDIR)/ObsOutput.o \
$(OBJDIR)/DiscretizationBase.o \
$(OBJDIR)/TimeArraySeries.o \
$(OBJDIR)/ObsOutputList.o \
$(OBJDIR)/Observe.o \
$(OBJDIR)/TimeArraySeriesLink.o \
$(OBJDIR)/ObsUtility.o \
$(OBJDIR)/ObsContainer.o \
$(OBJDIR)/BudgetFileReader.o \
$(OBJDIR)/TimeArraySeriesManager.o \
$(OBJDIR)/PackageMover.o \
$(OBJDIR)/Obs.o \
$(OBJDIR)/NumericalPackage.o \
$(OBJDIR)/Budget.o \
$(OBJDIR)/BudgetTerm.o \
$(OBJDIR)/sort.o \
$(OBJDIR)/SfrCrossSectionUtils.o \
$(OBJDIR)/BoundaryPackage.o \
$(OBJDIR)/VirtualBase.o \
$(OBJDIR)/STLVecInt.o \
$(OBJDIR)/BaseModel.o \
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
$(OBJDIR)/NumericalModel.o \
$(OBJDIR)/FlowModelInterface.o \
$(OBJDIR)/OutputControlData.o \
$(OBJDIR)/gwf-ic.o \
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
$(OBJDIR)/IndexMap.o \
$(OBJDIR)/VirtualModel.o \
$(OBJDIR)/BaseExchange.o \
$(OBJDIR)/tsp-fmi.o \
$(OBJDIR)/GwtSpc.o \
$(OBJDIR)/GweInputData.o \
$(OBJDIR)/OutputControl.o \
$(OBJDIR)/tsp-ic.o \
$(OBJDIR)/TspAdvOptions.o \
$(OBJDIR)/UzfCellGroup.o \
$(OBJDIR)/Xt3dInterface.o \
$(OBJDIR)/gwf-tvk.o \
$(OBJDIR)/gwf-vsc.o \
$(OBJDIR)/GwfNpfOptions.o \
$(OBJDIR)/InterfaceMap.o \
$(OBJDIR)/SeqVector.o \
$(OBJDIR)/ImsLinearSettings.o \
$(OBJDIR)/ConvergenceSummary.o \
$(OBJDIR)/ArrayReaderBase.o \
$(OBJDIR)/CellWithNbrs.o \
$(OBJDIR)/NumericalExchange.o \
$(OBJDIR)/tsp-ssm.o \
$(OBJDIR)/tsp-oc.o \
$(OBJDIR)/tsp-obs.o \
$(OBJDIR)/tsp-mvt.o \
$(OBJDIR)/tsp-adv.o \
$(OBJDIR)/gwf-disv.o \
$(OBJDIR)/gwf-disu.o \
$(OBJDIR)/gwf-dis.o \
$(OBJDIR)/gwf-uzf.o \
$(OBJDIR)/tsp-apt.o \
$(OBJDIR)/gwt-mst.o \
$(OBJDIR)/GwtDspOptions.o \
$(OBJDIR)/gwf-npf.o \
$(OBJDIR)/gwf-tvs.o \
$(OBJDIR)/GwfStorageUtils.o \
$(OBJDIR)/Mover.o \
$(OBJDIR)/GwfMvrPeriodData.o \
$(OBJDIR)/ImsLinearMisc.o \
$(OBJDIR)/GwfBuyInputData.o \
$(OBJDIR)/GweCndOptions.o \
$(OBJDIR)/VirtualSolution.o \
$(OBJDIR)/SparseMatrix.o \
$(OBJDIR)/LinearSolverBase.o \
$(OBJDIR)/ImsReordering.o \
$(OBJDIR)/ModflowInput.o \
$(OBJDIR)/IdmLogger.o \
$(OBJDIR)/Integer2dReader.o \
$(OBJDIR)/DislGeom.o \
$(OBJDIR)/SwfCxsUtils.o \
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
$(OBJDIR)/gwe-uze.o \
$(OBJDIR)/gwe-sfe.o \
$(OBJDIR)/gwe-mwe.o \
$(OBJDIR)/gwe-lke.o \
$(OBJDIR)/gwe-est.o \
$(OBJDIR)/gwe-esl.o \
$(OBJDIR)/gwe-ctp.o \
$(OBJDIR)/gwe-cnd.o \
$(OBJDIR)/RouterBase.o \
$(OBJDIR)/ImsLinearSolver.o \
$(OBJDIR)/ImsLinearBase.o \
$(OBJDIR)/StructVector.o \
$(OBJDIR)/DefinitionSelect.o \
$(OBJDIR)/InputLoadType.o \
$(OBJDIR)/Integer1dReader.o \
$(OBJDIR)/Double2dReader.o \
$(OBJDIR)/Double1dReader.o \
$(OBJDIR)/swf-disl.o \
$(OBJDIR)/swf-cxs.o \
$(OBJDIR)/swf-ic.o \
$(OBJDIR)/GridConnection.o \
$(OBJDIR)/DistributedVariable.o \
$(OBJDIR)/gwt.o \
$(OBJDIR)/gwf.o \
$(OBJDIR)/GwfExchangeMover.o \
$(OBJDIR)/gwe.o \
$(OBJDIR)/SerialRouter.o \
$(OBJDIR)/Timer.o \
$(OBJDIR)/LinearSolverFactory.o \
$(OBJDIR)/ImsLinear.o \
$(OBJDIR)/BaseSolution.o \
$(OBJDIR)/StructArray.o \
$(OBJDIR)/BoundInputContext.o \
$(OBJDIR)/AsciiInputLoadType.o \
$(OBJDIR)/LayeredArrayReader.o \
$(OBJDIR)/swf-zdg.o \
$(OBJDIR)/swf-sto.o \
$(OBJDIR)/swf-oc.o \
$(OBJDIR)/swf-obs.o \
$(OBJDIR)/swf-flw.o \
$(OBJDIR)/swf-dfw.o \
$(OBJDIR)/ExplicitModel.o \
$(OBJDIR)/SpatialModelConnection.o \
$(OBJDIR)/GwtInterfaceModel.o \
$(OBJDIR)/exg-gwtgwt.o \
$(OBJDIR)/GwfInterfaceModel.o \
$(OBJDIR)/exg-gwfgwf.o \
$(OBJDIR)/GweInterfaceModel.o \
$(OBJDIR)/exg-gwegwe.o \
$(OBJDIR)/RouterFactory.o \
$(OBJDIR)/NumericalSolution.o \
$(OBJDIR)/MappedMemory.o \
$(OBJDIR)/StressListInput.o \
$(OBJDIR)/StressGridInput.o \
$(OBJDIR)/LoadMf6File.o \
$(OBJDIR)/swf.o \
$(OBJDIR)/ExplicitSolution.o \
$(OBJDIR)/GwtGwtConnection.o \
$(OBJDIR)/GwfGwfConnection.o \
$(OBJDIR)/GweGweConnection.o \
$(OBJDIR)/VirtualDataManager.o \
$(OBJDIR)/Mapper.o \
$(OBJDIR)/IdmMf6File.o \
$(OBJDIR)/ModelPackageInput.o \
$(OBJDIR)/VirtualGwtModel.o \
$(OBJDIR)/VirtualGwtExchange.o \
$(OBJDIR)/VirtualGwfModel.o \
$(OBJDIR)/VirtualGwfExchange.o \
$(OBJDIR)/VirtualGweModel.o \
$(OBJDIR)/VirtualGweExchange.o \
$(OBJDIR)/exg-swfgwf.o \
$(OBJDIR)/SolutionGroup.o \
$(OBJDIR)/SolutionFactory.o \
$(OBJDIR)/exg-gwfgwt.o \
$(OBJDIR)/exg-gwfgwe.o \
$(OBJDIR)/RunControl.o \
$(OBJDIR)/SourceLoad.o \
$(OBJDIR)/ModelPackageInputs.o \
$(OBJDIR)/SimulationCreate.o \
$(OBJDIR)/RunControlFactory.o \
$(OBJDIR)/IdmLoad.o \
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
$(OBJDIR)/Iunit.o \
$(OBJDIR)/TimeSelect.o \
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

