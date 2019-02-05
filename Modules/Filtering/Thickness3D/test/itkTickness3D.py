#!/usr/bin/env python3
# -*- coding: utf-8 -*-
#
# title           : itkTickness3DTest.py
# description     : Test file for the ITKThickness3D module wrapping
# copyright       : Insight Software Consortium
# license         : Apache License, Version 2.0
# author(s)       : Thomas Janvier [thomas.p.janvier@gmail.com]
# creation        : 30 January 2019

import os
import sys
import itk
import warnings


def tests(source, destination=None):

    tests = [
        (itkBinaryThinningImageFilter3DTest, "skeleton.tiff"),
        (itkMedialThicknessImageFilter3DTest, "medial_thickness.tiff"),
    ]

    # check if source file exists
    if not os.path.isfile(source):
        raise IOError("Source must be an existing image file")
    # parse destination if none
    if not destination:
        destination = os.path.split(source)[0]
    # check if destination path exists
    if not os.path.isdir(destination):
        os.mkdir(destination)

    image = itk.imread(source)
    outputs = []
    for fun, args in tests:
        outputs.append(fun(image, args))

    return outputs


def itkBinaryThinningImageFilter3DTest(image, output_filename=None):
    """Basic ITK pipeline using BinaryThinningImageFilter3D

    Args:
        source {str} -- input filename
        destination {str, optional} -- output filename

    Returns:
        the skeleton image {numpy.array} -- 3D array
    """

    output = itk.BinaryThinningImageFilter3D.New(image)
    itk.imwrite(output, output_filename)
    return itk.GetArrayFromImage(output)


def itkMedialThicknessImageFilter3DTest(image, output_filename=None):
    """Basic ITK pipeline using kMedialThicknessImageFilter3D

    Args:
        source {str} -- input filename
        destination {str, optional} -- output filename

    Returns:
        the skeleton thickness image {numpy.array} -- 3D array
    """

    output = itk.MedialThicknessImageFilter3D.New(image)
    itk.imwrite(output, output_filename)
    return itk.GetArrayFromImage(output)


if __name__ == "__main__":
    """Entry point for ITKBinaryThinning3D module wrapping testing

    If no arguments are provided, use the module test data.

    First argument is treated as the input image filename.
    Second argument is treated as the output image filename (if none, replace the input).

    Example:
        $ python itkBinaryThinningImageFilter3DTest.py

    """
    if len(sys.argv) == 1:
        try:
            CWD = os.path.dirname(os.path.realpath(__file__))
        except Exception as err:
            warnings.warn(
                "Could not access `__file__` attribute, using `os.getcwd()` instead",
                RuntimeWarning,
            )
            CWD = os.getcwd()
        itkBinaryThinningImageFilter3DTest(
            os.path.join(CWD, "Input", "input.tif"), "skeleton.tif"
        )
    else:
        itkBinaryThinningImageFilter3DTest(sys.argv[1])
    sys.exit(0)
