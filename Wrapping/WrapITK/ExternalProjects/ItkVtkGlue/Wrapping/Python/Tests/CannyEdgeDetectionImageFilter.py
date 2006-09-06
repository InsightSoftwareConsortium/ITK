# This file demonstrates how to connect VTK and ITK pipelines together
# in scripted languages with the new ConnectVTKITK wrapping functionality.
# Data is loaded in with VTK, processed with ITK and written back to disc
# with  
#
# For this to work, you have to build InsightApplications/ConnectVTKITK
# as well.
#
# It also demonstrates the use of the python-specific itkPyCommand object.
#
# -- Charl P. Botha <cpbotha AT ieee.org>

import itk, itkvtk
import sys
from vtk import *

itk.auto_progress = True

# VTK will read the PNG image for us
reader = vtkPNGReader()
reader.SetFileName(sys.argv[1])

# it has to be a single component, itk::VTKImageImport doesn't support more
lum = vtkImageLuminance()
lum.SetInput(reader.GetOutput())

# let's cast the output to float
imageCast = vtkImageCast()
imageCast.SetOutputScalarTypeToFloat()
imageCast.SetInput(lum.GetOutput())

cannyImgType = itk.Image[itk.F, 2]
vtk2itk = itk.VTKImageToImageFilter[cannyImgType].New(imageCast)

canny  = itk.CannyEdgeDetectionImageFilter[cannyImgType, cannyImgType].New(vtk2itk)

writerImgType = itk.Image[itk.US, 2]
rescaler = itk.RescaleIntensityImageFilter[cannyImgType, writerImgType].New(canny)

itk2vtk = iImageToVTKImageFilter[writerImgType].New(rescaler)

# finally write the image to disk using VTK
writer = vtkPNGWriter()
writer.SetFileName(sys.argv[2])
writer.SetInput(itk2vtk.GetOutput())

# before we call Write() on the writer, it is prudent to give
# our ITK pipeline an Update() call... this is not necessary
# for normal error-less operation, but ensures that exceptions
# thrown by ITK get through to us in the case of an error;
# This is because the VTK wrapping system does not support
# C++ exceptions.
rescaler.Update()

# write the file to disk...
writer.Write()
