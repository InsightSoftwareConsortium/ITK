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
