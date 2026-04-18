ITKCuberille
============

.. image:: https://github.com/InsightSoftwareConsortium/ITKCuberille/workflows/Build,%20test,%20package/badge.svg

.. image:: https://img.shields.io/pypi/v/itk-cuberille.svg
    :target: https://pypi.python.org/pypi/itk-cuberille
    :alt: PyPI

.. image:: https://img.shields.io/badge/License-Apache%202.0-blue.svg
    :target: https://github.com/InsightSoftwareConsortium/ITKCuberille/blob/master/LICENSE
    :alt: License

Overview
--------

This module implements cuberille implicit surface polygonization for `ITK
<https://www.itk.org>`_. This method operates by dividing the surface into a
number of small cubes called cuberilles. Each cuberille is centered at a pixel
lying on the iso-surface and then quadrilaterals are generated for each face.
The original approach is improved by projecting the vertices of each cuberille
onto the implicit surface, smoothing the typical block-like resultant mesh.

A more detailed description can be found in the
`Insight Journal article <https://hdl.handle.net/10380/3186>`_::

  Mueller, D. "Cuberille Implicit Surface Polygonization for ITK"
  https://hdl.handle.net/10380/3186
  https://www.insight-journal.org/browse/publication/740
  July 20, 2010.

Installation
------------

Python
^^^^^^

Binary `Python packages <https://pypi.python.org/pypi/itk-cuberille>`_
are available for Linux, macOS, and Windows. They can be installed with::

  python -m pip install --upgrade pip
  python -m pip install itk-cuberille

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
