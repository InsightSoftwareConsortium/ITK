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

import itk
import sys
from vtk import *

# VTK will read the PNG image for us
reader = vtkPNGReader()
reader.SetFileName(sys.argv[1])

# it has to be a single component, itk::VTKImageImport doesn't support more
lum = vtkImageLuminance()
lum.SetInput(reader.GetOutput())

# convert from vtk to itk, and from itk to vtk
ImgType = itk.Image[itk.UC, 2]
vtk2itk = itk.VTKImageToImageFilter[ImgType].New(lum)
itk2vtk = itk.ImageToVTKImageFilter[ImgType].New(vtk2itk)

# finally write the image to disk using VTK
writer = vtkPNGWriter()
writer.SetFileName(sys.argv[2])
writer.SetInput(itk2vtk.GetOutput())

# write the file to disk...
writer.Write()
