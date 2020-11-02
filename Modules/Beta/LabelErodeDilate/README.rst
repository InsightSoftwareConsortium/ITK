ITKLabelErodeDilate
===================

.. image:: https://github.com/InsightSoftwareConsortium/ITKLabelErodeDilate/workflows/Build,%20test,%20package/badge.svg

.. |CircleCI| image:: https://circleci.com/gh/InsightSoftwareConsortium/ITKLabelErodeDilate.svg?style=shield
    :target: https://circleci.com/gh/InsightSoftwareConsortium/ITKLabelErodeDilate

.. |TravisCI| image:: https://travis-ci.org/InsightSoftwareConsortium/ITKLabelErodeDilate.svg?branch=master
    :target: https://travis-ci.org/InsightSoftwareConsortium/ITKLabelErodeDilate

.. |AppVeyor| image:: https://img.shields.io/appveyor/ci/itkrobot/itklabelerodedilate.svg
    :target: https://ci.appveyor.com/project/itkrobot/itklabelerodedilate

=========== =========== ===========
   Linux      macOS       Windows
=========== =========== ===========
|CircleCI|  |TravisCI|  |AppVeyor|
=========== =========== ===========

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

License
-------

This software is distributed under the Apache 2.0 license. Please see
the *LICENSE* file for details.
