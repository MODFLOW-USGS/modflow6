# Distributing MODFLOW 6

This folder contains scripts to automate MODFLOW 6 distribution tasks.

<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->

- [Overview](#overview)
- [Requirements](#requirements)
- [Testing](#testing)
- [Release procedures](#release-procedures)
  - [Preparing a nightly release](#preparing-a-nightly-release)
  - [Preparing an official release](#preparing-an-official-release)
    - [Updating version info](#updating-version-info)
    - [Building makefiles](#building-makefiles)
    - [Building example models](#building-example-models)
    - [Benchmarking example models](#benchmarking-example-models)
    - [Building documentation](#building-documentation)
    - [Building the distribution archive](#building-the-distribution-archive)
- [Release automation](#release-automation)
  - [Nightly builds](#nightly-builds)
  - [Official releases](#official-releases)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

## Overview

This document describes release procedures for MODFLOW 6. Manually preparing a release involves running scripts in the `distribution` directory in a particular order:

1. Update version info: `update_version.py`
2. Run benchmarks: `benchmark.py`
3. Build documentation: `build_docs.py`
4. Build distribution: `build_dist.py`

This process is automated in the `.github/workflows/release.yml` workflow.

The `build_dist.py` script is lazy &mdash; benchmarks, example models and documentation artifacts are downloaded via the GitHub API if available, and only re-created if none exist or the `--force` (`-f`) flag is provided. This allows the release workflow to consume artifacts previously created by other workflow runs, reducing the time needed to create and publish a release.

## Requirements

This document assumes a MODFLOW 6 development environment has been configured as per the [developer documentation](../DEVELOPER.md), including a Fortran compiler (either `ifort` or `gfortran`) as well as a Conda environment as specified in `environment.yml`. Official distributions are currently prepared with Intel Fortran (`ifort`).

## Testing

Each script in `distribution/` contains its own tests. To run them, run `pytest` from the `distribution/` folder. The tests will not be discovered if `pytest` is run from a different location, as the scripts in this folder are not named `test_*.py` and are only discoverable by virtue of the patterns provided in `distribution/pytest.ini`.  The tests use temporary directories where possible and revert modifications to tracked files on teardown.

**Note:** the tests clean up by reverting changes to files in the following locations:

- `doc/`
- `make`
- `utils/**/make/`

Make sure you don't have any uncommitted changes in these locations before running the tests.

**Note:** to avoid contested file access, the tests **should not be run in parallel** with `pytest-xdist`.

There is a small additional suite of tests that can be used to validate a release distribution folder after it is built: `check_dist.py`. These tests are run as part of the release workflow.

## Release procedures

MODFLOW 6 release come in two flavors:

- nightly development builds
- full/approved distributions

Development builds are created nightly from the tip of the `develop` branch and released from the [`MODFLOW-USGS/modflow6-nightly-build` repository](https://github.com/MODFLOW-USGS/modflow6-nightly-build). Development distributions contain only MODFLOW 6 input/output documentation and core executables and libraries:

- `mf6`: MODFLOW 6 executable
- `zbud6`: Zonebudget executable
- `mf5to6`: MODFLOW 5 to 6 converter executable
- `libmf6`: MODFLOW 6 dynamic library

Full distributions, on the other hand, contain the items listed above, as well as:

- Meson build files
- Fortran source code
- MODFLOW 6 example models
- MODFLOW 6 makefiles
- MODFLOW 6 Visual Studio files
- more extensive documentation, including:
  - MODFLOW 6 input/output docs
  - MODFLOW 6 example model docs
  - MODFLOW 6 release notes
  - MODFLOW 6 supplementary technical information
  - docs for various MODFLOW 6 features and packages
  - docs for `mf5to6` and `zbud6`


### Preparing a minimal development release

Development releases are built and [posted nightly on the `MODFLOW-USGS/modflow6-nightly-build` repository](https://github.com/MODFLOW-USGS/modflow6-nightly-build/releases). For a minimal release, distribution contents include:

- platform-specific distributions containing only executables `mf6`, `zbud6`, `mf5to6` and library `libmf6`
- MODFLOW 6 input/output documentation
- release notes
- `code.json` metadata

The `build_dist.py` script can be used to create both minimal and full distributions. By default, a minimal distribution is created. To create a full distribution, run the script with the `--full` flag:

The script has several other arguments:

- `--build-path`: path to the build workspace, defaults to `<project root>/builddir`
- `--output-path (-o)`: path to create a distribution zipfile, defaults to `<project root>/distribution/`
- `--examples-repo-path (-e)`: path to the [`MODFLOW-USGS/modflow6-examples`](https://github.com/MODFLOW-USGS/modflow6-examples) repository, defaults to `modflow6-examples` side-by-side with project root
- `--force (-f)`: whether to recreate and overwrite preexisting components of the distribution, if they already exist

Default paths are resolved relative to the script's location on the filesystem, *not* the current working directory, so the script can be run from `distribution/`, from the project root, or from anywhere else. (This is true of all scripts in the `distribution/` directory.)

### Preparing an official release

To prepare an official release for distribution, the steps are as follows:

#### Updating version info

Version information is stored primarily in `version.txt` in the project root, as well as in several other files in the repository.

The `update_version.py` script updates files containing version information. First a file lock is acquired, then repository files are updated, then the lock is released.

The version can be specified with the `--version` (short `-v`) option. For instance, to set version to `6.4.1`, run from the `scripts/` folder:

```shell
python update_version.py -v 6.4.1
```

If no `--version` is provided, the version is not changed, only the build timestamp.

An optional label may be appended to the release number, e.g.

```shell
python update_version.py -v 6.4.2rc
```

The label must start immediately following the patch version number, with no space in between. The label may contain numeric characters or symbols, but *must not* start with a number (otherwise there is no way to distinguish it from the patch version number).

The `--approved` (short `-a`) flag can be used to approve an official release. If the `--approved` flag is provided, disclaimer language is altered to reflect approval. If the flag is not provided, the language reflects preliminary/provisional status and `(preliminary)` is appended to version numbers.

The `--releasemode` flag can be used to control whether binaries are built in development or release mode by editing the contents of `src/Utilities/version.f90`. If the `--releasemode` flag is provided, `IDEVELOPMODE` is set to 0. If `--releasemode` is not provided, `IDEVELOPMODE` is set to 1.

#### Building makefiles

The `build_makefiles.py` script is used to rewrite makefiles after Fortran source files have been added, removed, or renamed. Up-to-date makefiles must be generated for inclusion in a distribution. To build makefiles, run:

```shell
python build_makefiles.py
```

#### Building example models

MODFLOW 6 [example models](https://github.com/MODFLOW-USGS/modflow6-examples) are bundled with official releases. Example models must be built and run to generate plots and tables before documentation can be generated. The `release.yml` workflow attempts to download the latest release from the examples repository, only re-building and re-running example models if no such release is available. See the examples repository for more information on preparing example models.

#### Benchmarking example models

MODFLOW 6 documentation includes a performance evaluation comparing the current version against the last official release. Benchmarks must run before a release can be prepared. Benchmarks run as a component of the `docs.yml` CI workflow &mdash; `release.yml` attempts to download benchmark results if available, only re-running them if necessary.

The `benchmark.py` script benchmarks the current development version of MODFLOW 6 against the latest release rebuilt in development mode, using the models from the `MODFLOW-USGS/modflow6-examples` repository. Paths to pre-built binaries for both versions can be provided via the `--current-bin-path` (short `-c`) and `--previous-bin-path` (short `-p`) command line options. If bin paths are not provided, executables are rebuilt in the default locations:

`<project root>/bin`: current development version
`<project root>/bin/rebuilt`: previous version

The examples repository must first be installed and prepared as described above. Its path may be explicitly provided with the `--examples-repo-path` (short `-e`) option. If no path is provided, the repository is assumed to be named `modflow6-examples` and live side-by-side with the `modflow6` repository on the filesystem.

The directory to write benchmark results can be specified with `--output-path` (short `-o`). If no such option is provided, results are written to the current working directory.

```shell
python benchmark.py -e ../modflow6-examples -o .benchmarks
```

The above will write results to a markdown file `.benchmarks/run-time-comparison.md` relative to the project root.

#### Building documentation

Extensive documentation is bundled with official MODFLOW 6 releases. MODFLOW 6 documentation is written in LaTeX. Some LaTeX files (in particular for MODFLOW 6 input/output documentation) is automatically generated from DFN files. The `release.yml` workflow first runs `update_version.py` to update version strings to be substituted into the docs, then runs `build_docs.py` to regenerate LaTeX files where necessary, download benchmark results (and convert the Markdown results file to LaTeX), download publications hosted on the USGS website, and finally convert LaTeX to PDFs.

Manually building MODFLOW 6 documentation requires additional Python dependencies specified in `build_rtd_docs/requirements.rtd.txt`. Styles defined in the [`MODFLOW-USGS/usgslatex`](https://github.com/MODFLOW-USGS/usgslatex) are also required. (See that repository's `README` for installation instructions or this repo's [`../.github/workflows/docs.yml](../.github/workflows/docs.yml) CI workflow for an example.)

#### Building the distribution archive

After each step above is complete, the `build_dist.py` script can be used to construct the MODFLOW 6 distribution. See [the `release.yml` workflow](../.github/workflows/release.yml) for a complete example of how to build a distribution archive.

#### Verifying the distribution archive

The `check_dist.py` script can be used to check the release distribution folder. The `--path` argument is the path to the dist folder. The `--approved` flag can be used to signal that the release is approved/official. By default the release is assumed preliminary. The script checks the version string emitted by `mf6 -v` for the presence or absence of "preliminary" depending on this flag.

## Release automation

Both nightly builds and official distributions are built automatically with GitHub Actions.

### Nightly builds

As mentioned, development releases are automatically built and posted nightly on the [`MODFLOW-USGS/modflow6-nightly-build`](https://github.com/MODFLOW-USGS/modflow6-nightly-build) repository.

### Official releases

The procedure above to prepare an official release is reproduced in `.github/workflows/release.yml`. This workflow has no triggers of its own, and must be dispatched by `.github/workflows/release_dispatch.yml`. A release can be dispatched in two ways:

- Push a release branch to the `MODFLOW-USGS/modflow6` repository.
- Manually trigger the workflow via GitHub CLI or web UI.

#### Triggering with a release branch

To release a new version of MODFLOW 6:

1. Create a release candidate branch from the tip of `develop` or `master`. The branch's name must begin with `v` followed by the version number. For an officially approved release, include *only* the version number. For a preliminary release candidate, append `rc` after the version number, e.g. `v6.4.0rc`. If the branch name does not end in `rc`, the release is assumed to be approved.
2. Push the branch to the `MODFLOW-USGS/modflow6` repository. This triggers the release workflow. If the release is still an unapproved candidate (i.e. the branch name ends with `rc`) binaries are built with `IDEVELOPMODE` set to 1, and the workflow ends after uploading binaries and documentation artifacts for inspection. If the release is approved/official, the workflow drafts a pull request against the `master` branch.
3. To continue with the release, merge the PR into `master`. This triggers another job to tag the new tip of `master` with the release number, draft a release post, upload binaries and documentation as release assets, and create another PR updating the `develop` branch from `master`, resetting version files, and setting `IDEVELOPMODE` back to 1.
4. To finalize the release, publish the release post and merge the PR into `develop`.

#### Triggering a release manually

The `workflow_dispatch` event is GitHub's mechanism for manually triggering workflows. This can be accomplished from the Actions tab in the GitHub UI.

Navigate to the Actions tab of this repository. Select the release dispatch workflow. A `Run workflow` button should be visible in an alert at the top of the list of workflow runs. Click the `Run workflow` button, selecting values for the various inputs:

- `approve`: whether the release is officially approved, or just a release candidate
- `branch`: the branch to release from
- `development`: whether to build a minimal development distribution or a full distribution
- `run_tests`: whether to run autotests after building binaries
- `version`: the version number of the release

### Release versioning

MODFLOW 6 version numbers follow the [semantic versioning](https://semver.org/) convention `major.minor.patch`. Release tags do *not* include an initial `v`, as is common in many other projects.

Patch releases should typically branch from `master`, since `develop` may contain broader changes not yet ready for release. Minor releases typically branch from `develop`. MODFLOW 6 release procedures do not currently increment the major version number.

Release notes use the following format convention:

```
This is the approved USGS MODFLOW <semver> release.

<authors>, <release year>, MODFLOW 6 Modular Hydrologic Model version <semver>: U.S. Geological Survey Software Release, <release date>, <doi link>

Visit the USGS "MODFLOW and Related Programs" site for information on MODFLOW 6 and related software: https://doi.org/10.5066/F76Q1VQV
```