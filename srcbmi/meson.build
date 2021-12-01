bmi_sources = files(
    'bmi.f90',
    'mf6bmi.f90',
    'mf6bmiError.f90',
    'mf6bmiGrid.f90',
    'mf6bmiUtil.f90',
    'mf6xmi.f90',
)

library('mf6', bmi_sources, link_with: mf6core, name_prefix: 'lib', install: true)
