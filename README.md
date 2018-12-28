
# MODFLOW 6



[USGS Release Page](https://water.usgs.gov/ogw/modflow/MODFLOW.html)

## Automated Testing Status on Travis-CI

### Version 6.0.3 develop-patch-sfrdfn &mdash; build 47
[![Build Status](https://travis-ci.org/MODFLOW-USGS/modflow6.svg?branch=develop-patch-sfrdfn)](https://travis-ci.org/MODFLOW-USGS/modflow6)

## Introduction

MODFLOW is a popular open-source groundwater flow model distributed by the U.S. Geological Survey.  For 30 years, the MODFLOW program has been widely used by academic researchers, private consultants, and government scientists to accurately, reliably, and efficiently simulate groundwater flow.  With time, growing interest in surface and groundwater interactions, local refinement with nested and unstructured grids, karst groundwater flow, solute transport, and saltwater intrusion, has led to the development of numerous MODFLOW versions.  Although these MODFLOW versions are often based on the core version (presently MODFLOW-2005), there are often incompatibilities that restrict their use with one another.  In many cases, development of these alternative versions has been challenging due to the underlying MODFLOW structure, which was designed for the simulation with a single groundwater flow model using a rectilinear grid.

MODFLOW 6 is the latest core version of MODFLOW. It synthesizes many of the capabilities available in MODFLOW-2005, MODFLOW-NWT, and MODFLOW-LGR. MODFLOW 6 was built on a new object-oriented framework that allows new packages and models to be added, and allows any number of models to be tightly coupled at the matrix level. The Groundwater Flow (GWF) Model is the first model to be released in MODFLOW 6. It supports regular MODFLOW grids consisting of layers, rows, and columns, but it also supports more flexible grids that may conform to irregular boundaries or have increased resolution in areas of interest. The GWF Model consists of the original MODFLOW stress packages (CHD, WEL, DRN, RIV, GHB, RCH, and EVT) and four advanced stress packages (MAW, SFR, LAK, and UZF), which have been distilled from their predecessors to contain the most commonly used capabilities. MODFLOW 6 contains a new Water Mover (MVR) Package that can transfer water from provider packages to receiver packages. Providers can be many of the stress and advanced stress packages; receivers can be any of the advanced stress packages. This new capability makes it possible to route water between lakes and streams, route rejected infiltration into a nearby stream, or augment lakes using groundwater pumped from wells, for example. To modernize user interaction with the program, the MODFLOW 6 input structure was redesigned. Within package input files, information is divided into blocks, and informative keywords are used to label numeric data and activate options. This new input structure was designed to make it easier for users to adjust simulation options in an intuitive manner, reduce user input errors, and allow new capabilities to be added without causing problems with backward compatibility.



## How to Cite
-----------------------------------------------

### ***Citations for MODFLOW 6:***

[Hughes, J.D., Langevin, C.D., and Banta, E.R., 2017, Documentation for the MODFLOW 6 framework: U.S. Geological Survey Techniques and Methods, book 6, chap. A57, 40 p., https://doi.org/10.3133/tm6A57.](https://doi.org/10.3133/tm6A57)

[Langevin, C.D., Hughes, J.D., Banta, E.R., Niswonger, R.G., Panday, Sorab, and Provost, A.M., 2017, Documentation for the MODFLOW 6 Groundwater Flow Model: U.S. Geological Survey Techniques and Methods, book 6, chap. A55, 197 p., https://doi.org/10.3133/tm6A55.](https://doi.org/10.3133/tm6A55)

[Provost, A.M., Langevin, C.D., and Hughes, J.D., 2017, Documentation for the "XT3D" option in the Node Property Flow (NPF) Package of MODFLOW 6: U.S. Geological Survey Techniques and Methods, book 6, chap. A56, 40 p., https://doi.org/10.3133/tm6A56.](https://doi.org/10.3133/tm6A56)

#### ***Software/Code citation for MODFLOW 6:***

[Langevin, C.D., Hughes, J.D., Banta, E.R., Provost, A.M., Niswonger, R.G., and Panday, Sorab, 2018, MODFLOW 6 Modular Hydrologic Model version 6.0.3 &mdash; develop-patch-sfrdfn: U.S. Geological Survey Software Release, 28 December 2018, https://doi.org/10.5066/F76Q1VQV](https://doi.org/10.5066/F76Q1VQV)


## Instructions for building definition files for new packages

Instructions for building definition files for new packages are summarized in [doc/mf6io/mf6ivar/readme.md](doc/mf6io/mf6ivar/readme.md).


Disclaimer
----------

This software is preliminary or provisional and is subject to revision. It is
being provided to meet the need for timely best science. The software has not
received final approval by the U.S. Geological Survey (USGS). No warranty,
expressed or implied, is made by the USGS or the U.S. Government as to the
functionality of the software and related material nor shall the fact of release
constitute any such warranty. The software is provided on the condition that
neither the USGS nor the U.S. Government shall be held liable for any damages
resulting from the authorized or unauthorized use of the software.

