# MODFLOW 6 Input Data Model developer resource

This document intends to describe, from a development perspective, what the MODFLOW 6 Input Data Model (IDM) is and how to work with it. As the Input Data Model is an active area of development this document should be expected to change frequently and will be updated to track with current state.

* [Overview](#overview)
* [Terminology](#terminology)
* [IDM roadmap](#idm-roadmap)
* [Package Update Status](#package-update-status)
* [Package Update Process](#package-update-process)

## Overview
The MODFLOW 6 Input Data Model is intended to provide a common entry point for simulation input data, which can be referred to as the "input context". It is built upon the MODFLOW 6 memory management infrastructure to define variable access paths that are scoped to a particular component/subcomponent. Input variables themselves are ultimately described by MODFLOW 6 definition (dfn) files.

The Modflow6 Input Data Model is intended to decouple a simulation run from any particular, supported source of input that a user might construct. Currently, Modflow6 components read input directly from proprietary mf6 style files as required during a simulation run. The new design assigns responsibility for reading and loading data from the files to IDM proper, which builds the input context from the supported user input source (currently, mf6 input files). Simulation components then access the input context to retrieve relevant input data.

## Terminology
### dfn
MODFLOW 6 dfn file formats are described in the [dfn readme.md](../../doc/mf6io/mf6ivar/readme.md). These files describe input parameters and are used to generate documentation and auto-generated flopy classes related to package inputs. These files are also inputs to the dfn2f90.py script which generates fortran input definitions that are used by IDM to load simulation input.
### input definition
Input definitions are generated from MODFLOW 6 dfn files and describe input parameter or blocks:

```fortran
type(InputParamDefinitionType), parameter :: &
  gwtdsp_opt_xt3d_off = InputParamDefinitionType &
  ( &
  'GWT', & ! component
  'DSP', & ! subcomponent
  'OPTIONS', & ! block
  'XT3D_OFF', & ! tag name
  'XT3D_OFF', & ! fortran variable
  'KEYWORD', & ! type
  '', & ! shape
  .false., & ! required
  .false., & ! multi-record
  .false., & ! preserve case
  .false. & ! layered
  )
```    

These definitions are used by IDM input handlers to interpret user input and build the input context.

A releated set of input definitions are contained in a fortran file that is named from the related package file, e.g. [gwf3npf8idm.f90](../../src/Model/GroundWaterFlow/gwf3npf8idm.f90) contains input definitions relevant to [gwf3npf8.f90](../../src/Model/GroundWaterFlow/gwf3npf8.f90). An input definition file is organized into 3 lists, a parameter (input variable) list, an aggregate (recarray type) list, and a block list.  It also contains the definition for a found type that can be used within packages to track what input variable paths were created by IDM:

```fortran
type GwtDspParamFoundType
  logical :: opt_xt3d_off = .false.
  logical :: opt_xt3d_rhs = .false.
  logical :: grid_diffc = .false.
  logical :: grid_alh = .false.
  logical :: grid_alv = .false.
  logical :: grid_ath1 = .false.
  logical :: grid_ath2 = .false.
  logical :: grid_atv = .false.
end type GwtDspParamFoundType`
```

## IDM roadmap
Phase | Scope | Action | Status
--- | --- | --- | ---
1.0 | idm | tool to generate input definitions from dfn files | complete
1.0 | idm | support generic reading of static mf6 input | complete
1.0 | idm | evaluate and prototype support for time-varying inputs | ongoing
1.0 | model subcomponents | update packages requiring static loader | ongoing
1.1 | idm | support for dynamic loading | not started
1.1 | model subcomponents | update packages requiring dynamic loader | not started
1.2 | model subcomponents | remove blockparser from all packages | not started
1.2 | doc | plan and prioritize required documentation | not started
1.2 | test | plan and prioritize testing | not started
2.0 | idm | preparation for 2nd supported input source | future
2.1 | idm | reader/writer support for 2nd supported input source | future

## Package Update Status
FTYPE | Status | Comment
--- | --- | ---
DIS6 | complete |
DISV6 | complete |
DISU6 | complete |
IC6 | candidate | stage next set
OC6 | IDM time-varying support needed |
NPF6 | complete |
STO6 | candidate | simplified period block, good candidate for early tv support
CSUB6 | IDM time-varying support needed |
BUY6 | candidate | in progress
HFB6 | candidate | period block but no timeseries data
CHD6 | IDM time-varying support needed | BndType
WEL6 | IDM time-varying support needed | BndType
DRN6 | IDM time-varying support needed | BndType
RIV6 | IDM time-varying support needed | BndType
GHB6 | IDM time-varying support needed | BndType
RCH6 | IDM time-varying support needed | BndType
EVT6 | IDM time-varying support needed | BndType
MAW6 | IDM time-varying support needed | BndType
SFR6 | IDM time-varying support needed | BndType
LAK6 | IDM time-varying support needed | BndType
UZF6 | IDM time-varying support needed | BndType
MVR6 | IDM time-varying support needed |
GNC6 | candidate | stage next set
OBS6 | candidate | stage next set
FMI6 | candidate | in progress
ADV6 | candidate | stage next set
DSP6 | complete |
SSM6 | candidate | stage next set
MST6 | candidate | stage next set
IST6 | IDM time-varying support needed | BndType
CNC6 | IDM time-varying support needed | BndType
SRC6 | IDM time-varying support needed | BndType
LKT6 | IDM time-varying support needed | GwtAptType
SFT6 | IDM time-varying support needed | GwtAptType
MWT6 | IDM time-varying support needed | GwtAptType
UZT6 | IDM time-varying support needed | GwtAptType
MVT6 | candidate | stage next set
API6 | IDM time-varying support needed | BndType

## Package Update process
###	Update [dfn2f90.py](scripts/dfn2f90.py)
Add the package dfn file path to the `gwf_dfns` or `gwt_dfns` list structure
### Run the dfn2f90.py script
```shell
cd utils/idmloader/scripts
python dfn2f90.py
```
This will create the new Idm f90 definition file in the appropriate directory, either `src/Model/GroundWaterFlow` or `src/Model/GroundWaterTransport`
### Update [meson.build](../../src/meson.build) and [mf6core.vfproj](../../msvs/mf6core.vfproj)
Add newly created f90 definition file
### Update [InputDefinitionSelector.f90](../../src/Utilities/Idm/InputDefinitionSelector.f90)
Expose the newly generated definitions to Idm core.  This involves adding “use” statements for the new module definition types and updating 3 select statements to make the new definition lists available.
### Update the package file
#### Create and load subcomponent input context
In create, or after parser initialization, invoke IdmMf6FileLoaderModule `input_load()` interface to create and load package input data to the subcomponent input context.

```fortran
call input_load(dspobj%parser, 'DSP6', 'GWT', 'DSP', dspobj%name_model, 'DSP', iout)
```

#### Source package input data 
Replace each “read” routine that currently parses an mf6 file type block with a corresponding “source” routine.  The implementation of the source routine is dependent on the data itself but a common pattern is to use the MemoryManagerExtModule `mem_set_value()` interface to copy data from the input context to package paths.  A parameter found type, in the generated idm definition file, should be used to pass a corresponding logical to `mem_set_value()`, which sets the logical to True if the input path was found and data was copied.  When sourcing has been completed, the found type parameter logicals can be checked to determine what other actions need to be taken in response to both found or not found input data.

```fortran
character(len=LENMEMPATH) :: idmMemoryPath
type(GwtDspParamFoundType) :: found
! ------------------------------------------------------------------------------
!
! -- set memory path
idmMemoryPath = create_mem_path(this%name_model, 'DSP', idm_context)
!
! -- update defaults with idm sourced values
call mem_set_value(this%ixt3doff, 'XT3D_OFF', idmMemoryPath, found%opt_xt3d_off)
call mem_set_value(this%ixt3drhs, 'XT3D_RHS', idmMemoryPath, found%opt_xt3d_rhs)
!
! -- set xt3d state flag
if (found%opt_xt3d_off) this%ixt3d = 0
if (found%opt_xt3d_rhs) this%ixt3d = 2
```

#### Deallocate package input paths
In deallocate, add a call to MemoryManagerExtModule `memorylist_remove()` for the package.  This call will search the subcomponent input path and deallocate all memory that was allocated as part of the load process.

```fortran
! -- Deallocate input memory
call memorylist_remove(this%name_model, 'DSP', idm_context)
```

## Adding params to packages already using IDM
### Update appropriate dfn file with new param(s)
### Run the dfn2f90.py script
```shell
cd utils/idmloader/scripts
python dfn2f90.py
```

This will update the existing idm package f90 file to add the new parameter definition
#### Source package input data
Update the package source routine for the relevant block to copy the data from the input path to the package path. Take any necessary action depending on whether the data was found or not found. 
