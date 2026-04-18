ITKGrowCut
=================================

.. image:: https://github.com/InsightSoftwareConsortium/ITKGrowCut/workflows/Build,%20test,%20package/badge.svg
    :alt:    Build Status

.. image:: https://img.shields.io/pypi/v/itk-growcut.svg
    :target: https://pypi.python.org/pypi/itk-growcut
    :alt: PyPI Version

.. image:: https://img.shields.io/badge/License-Apache%202.0-blue.svg
    :target: https://github.com/InsightSoftwareConsortium/ITKGrowCut/blob/master/LICENSE
    :alt: License

Overview
--------

ITKGrowCut is a remote module for ITK. The main filter segments a 3D image from user-provided seeds.

The original idea was presented by `Vezhnevets and Konouchine
<https://www.graphicon.ru/html/2005/proceedings/papers/VezhntvetsKonushin.pdf>`_:

 | Vladimir Vezhnevets and Vadim Konouchine:
 | “GrowCut” – interactive multi-label N-D image segmentation by cellular automata.
 | In: Proc. Graphicon. (2005) 150–156

In 2011 Harini Veeraraghavan of Memorial Sloan Kettering Cancer Center provided an `implementation based on ITK
<https://github.com/Slicer/SlicerGitSVNArchive/blob/master/Libs/vtkITK/itkGrowCutSegmentationImageFilter.txx>`_.

In 2014, Zhu et al. presented an `efficient approximation
<https://robobees.seas.harvard.edu/files/nac/files/zhu-miccai2014.pdf>`_:

 | Liangjia Zhu, Ivan Kolesov, Yi Gao, Ron Kikinis, Allen Tannenbaum.
 | An Effective Interactive Medical Image Segmentation Method Using Fast GrowCut
 | International Conference on Medical Image Computing and Computer Assisted Intervention (MICCAI),
 | Interactive Medical Image Computing Workshop, 2014

Zhu et al. also provided an open source implementation as a Slicer plugin, `based on VTK
<https://github.com/ljzhu/FastGrowCut>`_.
Since then, their implementation was integrated into Slicer, `refactored and improved
<https://github.com/Slicer/Slicer/blob/1a692bf36e9c99c47661fbf5fdba0fd3c3e72f95/Modules/Loadable/Segmentations/Logic/vtkImageGrowCutSegment.cxx>`_.
In this remote module we are building upon the improved variant from Slicer.


Acknowledgements
----------------

This software was developed in part by the Center for Integrative Biomedical Computing (CIBC), the
`Scientific Computing and Imaging (SCI) Institute <https://www.sci.utah.edu/cibc>`_ and
`Kitware <https://www.kitware.com>`_.

Support came from the National Institute of General Medical Sciences (NIGMS) of the National Institutes of Health (NIH)
under grant numbers P41 GM103545 and R24 GM136986.
