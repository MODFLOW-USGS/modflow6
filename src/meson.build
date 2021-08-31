subdir('Exchange')
subdir('Model')
subdir('Solution')
subdir('Timing')
subdir('Utilities')

modflow_sources = files(
    'mf6.f90',
    'mf6core.f90',
    'mf6lists.f90',
    'SimulationCreate.f90'
)

executable('mf6',
    exchange_sources,
    model_geometry_sources, 
    model_groundwaterflow_sources, 
    model_groundwatertransport_sources,
    model_modelutilities_sources,
    model_sources,
    solution_sparsematrixsolver_sources,
    solution_sources,
    timing_sources,
    utilities_memory_sources,
    utilities_observation_sources,
    utilities_outputcontrol_sources,
    utilities_timeseries_sources,
    utilities_sources,
    modflow_sources
)
