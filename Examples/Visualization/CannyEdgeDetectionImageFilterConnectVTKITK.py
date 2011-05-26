#==========================================================================
#
#   Copyright Insight Software Consortium
#
#   Licensed under the Apache License, Version 2.0 (the "License");
#   you may not use this file except in compliance with the License.
#   You may obtain a copy of the License at
#
#          http://www.apache.org/licenses/LICENSE-2.0.txt
#
#   Unless required by applicable law or agreed to in writing, software
#   distributed under the License is distributed on an "AS IS" BASIS,
#   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#   See the License for the specific language governing permissions and
#   limitations under the License.
#
#==========================================================================*/

# This file demonstrates how to connect VTK and ITK pipelines together
# in scripted languages with the new ConnectVTKITK wrapping functionality.
# Data is loaded in with VTK, processed with ITK and written back to disc
# with VTK.
#
# For this to work, you have to build InsightApplications/ConnectVTKITK
# as well.
#
# It also demonstrates the use of the python-specific itkPyCommand object.
#
# -- Charl P. Botha <cpbotha AT ieee.org>

import os
import sys
import InsightToolkit as itk
import ConnectVTKITKPython as CVIPy
import vtk

# VTK will read the PNG image for us
reader = vtk.vtkPNGReader()
reader.SetFileName("../../Testing/Data/Input/cthead1.png")

# it has to be a single component, itk::VTKImageImport doesn't support more
lum = vtk.vtkImageLuminance()
lum.SetInput(reader.GetOutput())

# let's cast the output to float
imageCast = vtk.vtkImageCast()
imageCast.SetOutputScalarTypeToFloat()
imageCast.SetInput(lum.GetOutput())

# the end-point of this VTK pipeline segment is a vtkImageExport
vtkExporter = vtk.vtkImageExport()
vtkExporter.SetInput(imageCast.GetOutput())

# it connects to the itk::VTKImageImport at the beginning of
# the subsequent ITK pipeline; two-dimensional float type
itkImporter = itk.itkVTKImageImportF2_New()

# Call the magic function that connects the two.  This will only be
# available if you built ITK with ITK_CSWIG_CONNECTVTKITK set to ON.
CVIPy.ConnectVTKToITKF2(vtkExporter, itkImporter.GetPointer())

# perform a canny edge detection and rescale the output
canny  = itk.itkCannyEdgeDetectionImageFilterF2F2_New()
rescaler = itk.itkRescaleIntensityImageFilterF2US2_New()
canny.SetInput(itkImporter.GetOutput())
rescaler.SetInput(canny.GetOutput())
rescaler.SetOutputMinimum(0)
rescaler.SetOutputMaximum(65535)

# this is to show off the new PyCommand functionality. :)
def progressEvent():
    print "%.0f%s done..." % (canny.GetProgress() * 100.0, '%')

pc = itk.itkPyCommand_New()
pc.SetCommandCallable(progressEvent)
canny.AddObserver(itk.itkProgressEvent(), pc.GetPointer())
# end of show-off

# this will form the end-point of the ITK pipeline segment
itkExporter = itk.itkVTKImageExportUS2_New()
itkExporter.SetInput(rescaler.GetOutput())

# the vtkImageImport will bring our data back into VTK-land
vtkImporter = vtk.vtkImageImport()
# do the magic connection call (once again: only available if you built
# ITK with ITK_CSWIG_CONNECTVTKITK set to ON)
CVIPy.ConnectITKUS2ToVTK(itkExporter.GetPointer(), vtkImporter)

# finally write the image to disk using VTK
writer = vtk.vtkPNGWriter()
writer.SetFileName('./testout.png')
writer.SetInput(vtkImporter.GetOutput())

# before we call Write() on the writer, it is prudent to give
# our ITK pipeline an Update() call... this is not necessary
# for normal error-less operation, but ensures that exceptions
# thrown by ITK get through to us in the case of an error;
# This is because the VTK wrapping system does not support
# C++ exceptions.
rescaler.Update()

# write the file to disk...
writer.Write()

print "\n\nWrote testout.png to current directory."
