fs = import('fs')
pymod = import('python')

# define paths
root_path = meson.project_source_root()
src_path = root_path / 'src'
idm_path = src_path / 'IDM'
dfn_path = root_path / 'doc' / 'mf6io' / 'mf6ivar' / 'dfn'

# locate programs
py = pymod.find_installation()
fypp = find_program('fypp', required : false)

# model selection
models = get_option('models')
message(f'Selected models: @models@')
if not models.contains('gwf')
  error('gwf is mandatory, other models are optional')
endif

# define exchanges
exchanges = ['gwf-gwf']
foreach m : models
  if m == 'gwf'
    continue
  endif
  exchanges += [f'gwf-@m@']
  exchanges += [f'@m@-@m@']
endforeach

# external source files
external_libraries = files(
    'Utilities' / 'Libraries' / 'daglib' / 'dag_module.f90',
    'Utilities' / 'Libraries' / 'rcm' / 'rcm.f90',
    'Utilities' / 'Libraries' / 'sparsekit' / 'sparsekit.f90',
    'Utilities' / 'Libraries' / 'blas' / 'blas1_d.f90',
    'Utilities' / 'Libraries' / 'sparskit2' / 'ilut.f90',
)

# mf6 core source files
modflow_sources = files(
    'Distributed' / 'IndexMap.f90',
    'Distributed' / 'InterfaceMap.f90',
    'Distributed' / 'Mapper.f90',
    'Distributed' / 'MappedMemory.f90',
    'Distributed' / 'RouterBase.f90',
    'Distributed' / 'RouterFactory.F90',
    'Distributed' / 'SerialRouter.f90',
    'Distributed' / 'VirtualBase.f90',
    'Distributed' / 'VirtualDataContainer.f90',
    'Distributed' / 'VirtualDataLists.f90',
    'Distributed' / 'VirtualDataManager.f90',
    'Distributed' / 'VirtualExchange.f90',    
    'Distributed' / 'VirtualModel.f90',
    'Distributed' / 'VirtualSolution.f90',
    'Exchange' / 'BaseExchange.f90',
    'Exchange' / 'DisConnExchange.f90',
    'Exchange' / 'GhostNode.f90',
    'Exchange' / 'NumericalExchange.f90',
    'Model' / 'Connection' / 'CellWithNbrs.f90',
    'Model' / 'Connection' / 'CsrUtils.f90',
    'Model' / 'Connection' / 'GridConnection.f90',
    'Model' / 'Connection' / 'GridSorting.f90',
    'Model' / 'Connection' / 'SpatialModelConnection.f90',
    'Model' / 'Connection' / 'DistributedVariable.f90',
    'Model' / 'Discretization' / 'Dis.f90',
    'Model' / 'Discretization' / 'Disu.f90',
    'Model' / 'Discretization' / 'Disv.f90',
    'Model' / 'Geometry' / 'BaseGeometry.f90',
    'Model' / 'Geometry' / 'CircularGeometry.f90',
    'Model' / 'Geometry' / 'RectangularGeometry.f90',
    'Model' / 'ModelUtilities' / 'BoundaryPackage.f90',
    'Model' / 'ModelUtilities' / 'BoundaryPackageExt.f90',
    'Model' / 'ModelUtilities' / 'Connections.f90',
    'Model' / 'ModelUtilities' / 'DiscretizationBase.f90',
    'Model' / 'ModelUtilities' / 'DisvGeom.f90',
    'Model' / 'ModelUtilities' / 'FlowModelInterface.f90',
    'Model' / 'ModelUtilities' / 'InitialConditions.f90',
    'Model' / 'ModelUtilities' / 'Mover.f90',
    'Model' / 'ModelUtilities' / 'PackageMover.f90',
    'Model' / 'ModelUtilities' / 'SfrCrossSectionManager.f90',
    'Model' / 'ModelUtilities' / 'SfrCrossSectionUtils.f90',
    'Model' / 'ModelUtilities' / 'TspAdvOptions.f90',
    'Model' / 'ModelUtilities' / 'UzfCellGroup.f90',
    'Model' / 'ModelUtilities' / 'Xt3dAlgorithm.f90',
    'Model' / 'ModelUtilities' / 'Xt3dInterface.f90',
    'Model' / 'TransportModel' / 'tsp.f90',
    'Model' / 'TransportModel' / 'tsp-adv.f90',
    'Model' / 'TransportModel' / 'tsp-apt.f90',
    'Model' / 'TransportModel' / 'tsp-fmi.f90',
    'Model' / 'TransportModel' / 'tsp-ic.f90',
    'Model' / 'TransportModel' / 'tsp-obs.f90',
    'Model' / 'TransportModel' / 'tsp-oc.f90',
    'Model' / 'TransportModel' / 'tsp-mvt.f90',
    'Model' / 'TransportModel' / 'tsp-spc.f90',
    'Model' / 'TransportModel' / 'tsp-ssm.f90',
    'Model' / 'BaseModel.f90',
    'Model' / 'ExplicitModel.f90',
    'Model' / 'NumericalModel.f90',
    'Model' / 'NumericalPackage.f90',
    'Solution' / 'ConvergenceSummary.f90',
    'Solution' / 'LinearMethods' / 'ImsLinearSettings.f90',
    'Solution' / 'LinearMethods' / 'ImsLinearBase.f90',
    'Solution' / 'LinearMethods' / 'ImsLinear.f90',
    'Solution' / 'LinearMethods' / 'ImsReordering.f90',
    'Solution' / 'LinearMethods' / 'ImsLinearMisc.f90',
    'Solution' / 'LinearMethods' / 'ImsLinearSolver.f90',
    'Solution' / 'BaseSolution.f90',
    'Solution' / 'ExplicitSolution.f90',
    'Solution' / 'LinearSolverBase.f90',
    'Solution' / 'LinearSolverFactory.F90',
    'Solution' / 'NumericalSolution.f90',
    'Solution' / 'SolutionFactory.F90',
    'Solution' / 'SolutionGroup.f90',
    'Timing' / 'ats.f90',
    'Timing' / 'tdis.f90',
    'Utilities' / 'ArrayRead' / 'ArrayReaderBase.f90',
    'Utilities' / 'ArrayRead' / 'Double1dReader.f90',
    'Utilities' / 'ArrayRead' / 'Double2dReader.f90',
    'Utilities' / 'ArrayRead' / 'Integer1dReader.f90',
    'Utilities' / 'ArrayRead' / 'Integer2dReader.f90',
    'Utilities' / 'ArrayRead' / 'LayeredArrayReader.f90',
    'Utilities' / 'IDM' / 'BoundInputContext.f90',
    'Utilities' / 'IDM' / 'DefinitionSelect.f90',
    'Utilities' / 'IDM' / 'IdmLoad.f90',
    'Utilities' / 'IDM' / 'IdmLogger.f90',
    'Utilities' / 'IDM' / 'InputDefinition.f90',
    'Utilities' / 'IDM' / 'InputLoadType.f90',
    'Utilities' / 'IDM' / 'ModelPackageInputs.f90',
    'Utilities' / 'IDM' / 'ModflowInput.f90',
    'Utilities' / 'IDM' / 'SourceCommon.f90',
    'Utilities' / 'IDM' / 'SourceLoad.F90',
    'Utilities' / 'IDM' / 'mf6blockfile' / 'AsciiInputLoadType.f90',
    'Utilities' / 'IDM' / 'mf6blockfile' / 'IdmMf6File.f90',
    'Utilities' / 'IDM' / 'mf6blockfile' / 'LoadMf6File.f90',
    'Utilities' / 'IDM' / 'mf6blockfile' / 'StressGridInput.f90',
    'Utilities' / 'IDM' / 'mf6blockfile' / 'StressListInput.f90',
    'Utilities' / 'IDM' / 'mf6blockfile' / 'StructArray.f90',
    'Utilities' / 'IDM' / 'mf6blockfile' / 'StructVector.f90',
    'Utilities' / 'Matrix' / 'MatrixBase.f90',
    'Utilities' / 'Matrix' / 'SparseMatrix.f90',
    'Utilities' / 'Memory' / 'Memory.f90',
    'Utilities' / 'Memory' / 'MemoryHelper.f90',
    'Utilities' / 'Memory' / 'MemoryList.f90',
    'Utilities' / 'Memory' / 'MemoryManager.f90',
    'Utilities' / 'Memory' / 'MemoryManagerExt.f90',
    'Utilities' / 'Memory' / 'MemorySetHandler.f90',
    'Utilities' / 'Observation' / 'Obs.f90',
    'Utilities' / 'Observation' / 'ObsContainer.f90',
    'Utilities' / 'Observation' / 'Observe.f90',
    'Utilities' / 'Observation' / 'ObsOutput.f90',
    'Utilities' / 'Observation' / 'ObsOutputList.f90',
    'Utilities' / 'Observation' / 'ObsUtility.f90',
    'Utilities' / 'OutputControl' / 'OutputControl.f90',
    'Utilities' / 'OutputControl' / 'OutputControlData.f90',
    'Utilities' / 'OutputControl' / 'PrintSaveManager.f90',
    'Utilities' / 'TimeSeries' / 'TimeArray.f90',
    'Utilities' / 'TimeSeries' / 'TimeArraySeries.f90',
    'Utilities' / 'TimeSeries' / 'TimeArraySeriesLink.f90',
    'Utilities' / 'TimeSeries' / 'TimeArraySeriesManager.f90',
    'Utilities' / 'TimeSeries' / 'TimeSeries.f90',
    'Utilities' / 'TimeSeries' / 'TimeSeriesFileList.f90',
    'Utilities' / 'TimeSeries' / 'TimeSeriesLink.f90',
    'Utilities' / 'TimeSeries' / 'TimeSeriesManager.f90',
    'Utilities' / 'TimeSeries' / 'TimeSeriesRecord.f90',
    'Utilities' / 'Vector' / 'VectorBase.f90',
    'Utilities' / 'Vector' / 'SeqVector.f90',
    'Utilities' / 'ArrayHandlers.f90',
    'Utilities' / 'ArrayReaders.f90',
    'Utilities' / 'BlockParser.f90',
    'Utilities' / 'Budget.f90',
    'Utilities' / 'BudgetFileReader.f90',
    'Utilities' / 'BudgetObject.f90',
    'Utilities' / 'BudgetTerm.f90',
    'Utilities' / 'CharString.f90',
    'Utilities' / 'comarg.f90',
    'Utilities' / 'compilerversion.F90',
    'Utilities' / 'Constants.f90',
    'Utilities' / 'defmacro.F90',
    'Utilities' / 'DevFeature.f90',
    'Utilities' / 'ErrorUtil.f90',
    'Utilities' / 'GeomUtil.f90',
    'Utilities' / 'HashTable.f90',
    'Utilities' / 'HeadFileReader.f90',
    'Utilities' / 'InputOutput.f90',
    'Utilities' / 'HGeoUtil.f90',
    'Utilities' / 'Iunit.f90',
    'Utilities' / 'kind.f90',
    'Utilities' / 'List.f90',
    'Utilities' / 'ListReader.f90',
    'Utilities' / 'LongLineReader.f90',
    'Utilities' / 'MathUtil.f90',
    'Utilities' / 'Message.f90',
    'Utilities' / 'OpenSpec.f90',
    'Utilities' / 'PackageBudget.f90',
    'Utilities' / 'Sim.f90',
    'Utilities' / 'SimStages.f90',
    'Utilities' / 'SimVariables.f90',
    'Utilities' / 'SmoothingFunctions.f90',
    'Utilities' / 'sort.f90',
    'Utilities' / 'Sparse.f90',    
    'Utilities' / 'STLVecInt.f90',
    'Utilities' / 'StringList.f90',
    'Utilities' / 'Table.f90',
    'Utilities' / 'TableTerm.f90',
    'Utilities' / 'Timer.f90',
    'Utilities' / 'version.f90',
    'mf6core.f90',
    'mf6lists.f90',
    'Simulation.f90',
    'RunControl.f90',
    'RunControlFactory.F90',
)

# model source files
foreach m : models
  subdir(m.to_upper())
endforeach

# IDM files
subdir('IDM')

# generate factories for configurable mf6 components
foreach f : ['Model', 'Exchange', 'Connection']
  input = f'@f@Factory.fypp'
  output = f'@f@Factory.f90'
  configure_file(
    command: [
      fypp,
      src_path / input,
      src_path / output,
      '-DMODELS=' + (models.length() > 0 ? ('["' + '", "'.join(models) + '"]') : '[]'),
      '-DEXCHANGES=' + (exchanges.length() > 0 ? ('["' + '", "'.join(exchanges) + '"]') : '[]')
    ],
    input: input,
    output: output
  )
  modflow_sources += files(output)
endforeach

# PETSc-related source files
if with_petsc
  modflow_sources += files(
    'Utilities' / 'Vector' / 'PetscVector.F90',
    'Utilities' / 'Matrix' / 'PetscMatrix.F90',
    'Solution' / 'PETSc' / 'PetscSolver.F90',
    'Solution' / 'PETSc' / 'PetscConvergence.F90',
    'Solution' / 'PETSc' / 'PetscImsPreconditioner.F90'
  )  
endif

# MPI-related source files
if with_mpi
  modflow_sources += files(
    'Distributed' / 'MpiMessageBuilder.f90',
    'Distributed' / 'MpiRouter.f90',
    'Distributed' / 'MpiRunControl.F90',
    'Distributed' / 'MpiWorld.f90',
    'Solution' / 'ParallelSolution.f90'
  )
endif

mf6_external = static_library('mf6_external', external_libraries)

message('MODFLOW 6 executable name: ' + buildname)

if build_machine.system() == 'windows' and with_petsc
  mf6core = static_library('mf6core', 
                           modflow_sources, 
                           dependencies:  dependencies, 
                           link_with: [mf6_external], 
                           include_directories: petsc_incdir)
  mf6exe = executable(buildname,
                      'mf6.f90', 
                      link_with: [mf6core],
                      dependencies: dependencies, 
                      install: true)
else
  mf6core = static_library('mf6core', 
                           modflow_sources,  
                           dependencies: dependencies,
                           link_with: [mf6_external])
  mf6exe = executable(buildname, 
                      'mf6.f90', 
                      link_with: [mf6core],  
                      dependencies: dependencies, 
                      install: true)
endif
