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
from sys import argv

# Input arguments
inputFilename = argv[1]
xOutputFilename = argv[2]
yOutputFilename = argv[3]
iterNum = int(argv[4])
noiseLevel = float(argv[5])
timeStep = float(argv[6])

itk.auto_progress(2)

# Define ITK types
DIM = 2

PixelType = itk.F
GradientPixelType = itk.CovariantVector[PixelType, DIM]

ImageType = itk.Image[PixelType, DIM]
GradientImageType = itk.Image[GradientPixelType, DIM]

WriterPixelType = itk.UC
WriteImageType = itk.Image[WriterPixelType, DIM]

GradientImageFilterType = itk.GradientImageFilter[
    ImageType, PixelType, PixelType]

LaplacianImageFilterType = itk.LaplacianImageFilter[
    ImageType, ImageType]

GradientVectorFlowImageFilterType = itk.GradientVectorFlowImageFilter[
    GradientImageType, GradientImageType, PixelType]

VectorIndexSelectionCastImageFilter = itk.VectorIndexSelectionCastImageFilter[
    GradientImageType, ImageType]

RescaleIntensityImageFilter = itk.RescaleIntensityImageFilter[
    ImageType, WriteImageType]

# Read image file
reader = itk.ImageFileReader[ImageType].New(FileName=inputFilename)

# Compute gradient for image
gradient = GradientImageFilterType.New()
gradient.SetInput(reader.GetOutput())

laplacian = LaplacianImageFilterType.New()

# Compute GVF for image
gvf = GradientVectorFlowImageFilterType.New()
gvf.SetInput(gradient.GetOutput())
gvf.SetLaplacianFilter(laplacian)
gvf.SetIterationNum(iterNum)
gvf.SetNoiseLevel(noiseLevel)
gvf.SetTimeStep(timeStep)
gvf.Update()

# Write vector field components to image files
for i, fileName in enumerate((xOutputFilename, yOutputFilename)):
    visc = VectorIndexSelectionCastImageFilter.New()
    visc.SetInput(gvf.GetOutput())
    visc.SetIndex(i)
    visc.Update()

    rescaler = RescaleIntensityImageFilter.New()
    rescaler.SetOutputMinimum(0)
    rescaler.SetOutputMaximum(255)
    rescaler.SetInput(visc.GetOutput())
    rescaler.Update()

    writer = itk.ImageFileWriter[WriteImageType].New(
        rescaler.GetOutput(), FileName=fileName)
    writer.Update()
