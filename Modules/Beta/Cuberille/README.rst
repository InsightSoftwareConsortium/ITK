ITKCuberille
============

.. image:: https://circleci.com/gh/InsightSoftwareConsortium/ITKCuberille.svg?style=shield
    :target: https://circleci.com/gh/InsightSoftwareConsortium/ITKCuberille

.. image:: https://travis-ci.org/InsightSoftwareConsortium/ITKCuberille.svg?branch=master
    :target: https://travis-ci.org/InsightSoftwareConsortium/ITKCuberille

.. image:: https://img.shields.io/appveyor/ci/itkrobot/ITKCuberille.svg
    :target: https://ci.appveyor.com/project/itkrobot/ITKCuberille


Overview
--------

This module implements cuberille implicit surface polygonization for `ITK
<https://www.itk.org>`_. This method operates by diving the surface into a
number of small cubes called cuberilles. Each cuberille is centered at a pixel
lying on the iso-surface and then quadrilaterals are generated for each face.
The original approach is improved by projecting the vertices of each cuberille
onto the implicit surface, smoothing the typical block-like resultant mesh.

A more detailed description can be found in the
`Insight Journal article <http://hdl.handle.net/10380/3186>`_::

  Mueller, D. "Cuberille Implicit Surface Polygonization for ITK"
  http://hdl.handle.net/10380/3186
  http://www.insight-journal.org/browse/publication/740
  July 20, 2010.

Installation
------------

Python
^^^^^^

Binary `Python packages <https://pypi.python.org/pypi/itk-bonemorphometry>`_
are available for Linux, macOS, and Windows. They can be installed with::

  python -m pip install --upgrade pip
  python -m pip install itk-bonemorphometry

C++
^^^

Since ITK 4.9.0, this module is available in the ITK source tree as a Remote
module.  To enable it, set::

  Module_Cuberille:BOOL=ON

in ITK's CMake build configuration.

License
-------

This software is distributed under the Apache 2.0 license. Please see
the *LICENSE* file for details.
