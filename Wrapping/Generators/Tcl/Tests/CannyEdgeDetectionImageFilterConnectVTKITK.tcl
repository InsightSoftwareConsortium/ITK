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
# in scripted generators with the new ConnectVTKITK wrapping functionality.
# Data is loaded in with VTK, processed with ITK and written back to disc
# with VTK.
#
# For this to work, you have to build InsightApplications/ConnectVTKITK
# as well.
#
# -- Charl P. Botha <cpbotha AT ieee.org>
# -- Modified to Tcl version by H.J.Huisman (25 March 2004)
# Execute this script by:
#      tclsh CannyEdgeDetectionImageFilterConnectVTKITK.tcl
# You must set the environment variable TCLLIB_DIR to where the tcl
# libraries are on your system. For example:
#      export  TCLLIB_DIR="/data/usr/itk16/lib/InsightToolkit /data/usr/vtk422/lib/vtk /data/prog/InsightApplications-1.6.0/ConnectVTKITK/"
#
puts "Loading VTK package [package require vtk]"
puts "Loading InsightToolkit [package require InsightToolkit]"
puts "Loading ConnectVTKITK [package require ConnectVTKITK]"
wm withdraw .

# VTK will read the PNG image for us
vtkPNGReader reader
reader SetFileName "../../Testing/Data/Input/cthead1.png"

# it has to be a single component, itk::VTKImageImport doesn't support more
vtkImageLuminance lum
lum SetInput [reader GetOutput]

# let's cast the output to float
vtkImageCast imageCast
imageCast SetOutputScalarTypeToFloat
imageCast SetInput [lum GetOutput]

# the end-point of this VTK pipeline segment is a vtkImageExport
vtkImageExport vtkExporter
vtkExporter SetInput [imageCast GetOutput]

# it connects to the itk::VTKImageImport at the beginning of
# the subsequent ITK pipeline; two-dimensional float type
set itkImporter [itkVTKImageImportF2_New]

# Call the magic function that connects the two.  This will only be
# available if you built ITK with ITK_CSWIG_CONNECTVTKITK set to ON.
ConnectVTKToITKF2 vtkExporter [$itkImporter GetPointer]

# perform a canny edge detection and rescale the output
set canny [itkCannyEdgeDetectionImageFilterF2F2_New]
set rescaler [itkRescaleIntensityImageFilterF2UC2_New]
$canny SetInput [$itkImporter GetOutput]
$rescaler SetInput [$canny GetOutput]
$rescaler SetOutputMinimum 0
$rescaler SetOutputMaximum 65535

# this will form the end-point of the ITK pipeline segment
set itkExporter [itkVTKImageExportUC2_New]
$itkExporter SetInput [$rescaler GetOutput]

# the vtkImageImport will bring our data back into VTK-land
vtkImageImport vtkImporter
# do the magic connection call (once again: only available if you built
# ITK with ITK_CSWIG_CONNECTVTKITK set to ON)
ConnectITKUC2ToVTK [$itkExporter GetPointer] vtkImporter

# finally write the image to disk using VTK
vtkPNGWriter writer
writer SetFileName "testout.png"
writer SetInput [vtkImporter GetOutput]

# before we call Write() on the writer, it is prudent to give
# our ITK pipeline an Update() call... this is not necessary
# for normal error-less operation, but ensures that exceptions
# thrown by ITK get through to us in the case of an error;
# This is because the VTK wrapping system does not support
# C++ exceptions.
$rescaler Update

# write the file to disk...
writer Write

puts "\n\nWrote testout.png to current directory."
exit
