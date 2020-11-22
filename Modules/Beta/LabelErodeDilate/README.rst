ITKLabelErodeDilate
===================

.. image:: https://github.com/InsightSoftwareConsortium/ITKLabelErodeDilate/workflows/Build,%20test,%20package/badge.svg

Overview
--------

ITK classes for erode and dilate operations on label images.

This module contains classes for mathematical morphology on label images using
circular/spherical/hyperspherical structuring elements. Operations are done
efficiently by using parabolic structuring functions. The module does also
handle label collisions.

For more information, see::

  Beare, R. and Jackway, P.
  Parallel Algorithms via Scaled Paraboloid Structuring Functions for
  Spatially-Variant and Label-Set Dilations and Erosions.
  2011 International Conference on Digital Image Computing Techniques and
  Applications (DICTA). 180--185. 2011. IEEE.
  
Installation
------------

::

  pip install itk-labelerodedilate

License
-------

This software is distributed under the Apache 2.0 license. Please see
the *LICENSE* file for details.
