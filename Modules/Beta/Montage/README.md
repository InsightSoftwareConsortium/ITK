ITKMontage
==========

[![Build Status](https://dev.azure.com/ITKMontage/ITKMontage/_apis/build/status/InsightSoftwareConsortium.ITKMontage?branchName=master)](https://dev.azure.com/ITKMontage/ITKMontage/_build/latest?definitionId=1&branchName=master)
[![PyPI](https://img.shields.io/pypi/v/itk-montage.svg)](https://pypi.python.org/pypi/itk-montage)
[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://github.com/InsightSoftwareConsortium/ITK/blob/master/LICENSE)

Montaging for microscopy imaging files.

ITK is an open-source, cross-platform library that provides developers
with an extensive suite of software tools for image analysis. Developed
through extreme programming methodologies, ITK employs leading-edge
algorithms for registering and segmenting multidimensional scientific
images.

This work is based on, in part:

    Bican, J.
    Phase Correlation Method for ITK
    The Insight Journal - 2006 July - December
    http://hdl.handle.net/1926/396

Installation
------------

To install the Python package:

    pip install itk-montage

To build the C++ module, either enable the CMake option in ITK's build
configuration:

    Module_Montage:BOOL=ON

Or, build the module as a separate project against an ITK build tree:

    git clone https://github.com/InsightSoftwareConsortium/ITKMontage
    mkdir ITKMontage-build
    cd ITKMontage-build
    cmake -DITK_DIR=/path/to/ITK-build ../ITKMontage
    cmake --build .

See Also
--------

-   <http://dream3d.bluequartz.net/>
-   <http://www.incf.org>
-   <http://www.itk.org>
-   <http://www.alleninstitute.org>

License
-------

This code is copyrighted by the Insight Software Consortium, and it is
distributed under the Apache 2 license.

Acknowledgements
----------------

The code contained herein was partially funded by the following
contracts:

-   United States Air Force Prime Contract FA8650-15-D-5231
