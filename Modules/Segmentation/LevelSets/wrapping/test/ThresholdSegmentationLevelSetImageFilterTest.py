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

#     INPUTS: {BrainProtonDensitySlice.png}
#     OUTPUTS: {ThresholdSegmentationLevelSetImageFilterWhiteMatter.png}
#     60 116 5 150 180

#     INPUTS: {BrainProtonDensitySlice.png}
#     OUTPUTS: {ThresholdSegmentationLevelSetImageFilterVentricle.png}
#     81 112 5 210 250

#     INPUTS: {BrainProtonDensitySlice.png}
#     OUTPUTS: {ThresholdSegmentationLevelSetImageFilterGrayMatter.png}
#     107 69 5 180  210

from __future__ import print_function

import itk
from sys import argv, stderr, exit
import os
itk.auto_progress(2)

# itk.auto_progress(1)

if len(argv) < 8:
    print((
        "Missing Parameters \n Usage: "
        "ThresholdSegmentationLevelSetImageFilter.py inputImage outputImage "
        "seedX seedY InitialDistance LowerThreshold UpperThreshold "
        "[CurvatureScaling == 1.0]"), file=stderr)
    exit(1)

InternalPixelType = itk.F
Dimension = 2
InternalImageType = itk.Image[InternalPixelType, Dimension]

OutputPixelType = itk.UC
OutputImageType = itk.Image[OutputPixelType, Dimension]

thresholder = itk.BinaryThresholdImageFilter[
    InternalImageType, OutputImageType].New()

thresholder.SetLowerThreshold(-1000.0)
thresholder.SetUpperThreshold(0.0)

thresholder.SetOutsideValue(0)
thresholder.SetInsideValue(255)

ReaderType = itk.ImageFileReader[InternalImageType]
WriterType = itk.ImageFileWriter[OutputImageType]

reader = ReaderType.New()
writer = WriterType.New()

reader.SetFileName(argv[1])
writer.SetFileName(argv[2])


FastMarchingFilterType = itk.FastMarchingImageFilter[
    InternalImageType,
    InternalImageType]
fastMarching = FastMarchingFilterType.New()

ThresholdSegLvlSetImgFilterType = itk.ThresholdSegmentationLevelSetImageFilter[
    InternalImageType,
    InternalImageType,
    InternalPixelType]
thresholdSegmentation = ThresholdSegLvlSetImgFilterType.New()
thresholdSegmentation.SetPropagationScaling(1.0)
if len(argv) > 8:
    thresholdSegmentation.SetCurvatureScaling(float(argv[8]))
else:
    thresholdSegmentation.SetCurvatureScaling(1.0)

thresholdSegmentation.SetMaximumRMSError(0.02)
thresholdSegmentation.SetNumberOfIterations(1200)

thresholdSegmentation.SetUpperThreshold(float(argv[7]))
thresholdSegmentation.SetLowerThreshold(float(argv[6]))
thresholdSegmentation.SetIsoSurfaceValue(0.0)

thresholdSegmentation.SetInput(fastMarching.GetOutput())
thresholdSegmentation.SetFeatureImage(reader.GetOutput())
thresholder.SetInput(thresholdSegmentation.GetOutput())
writer.SetInput(thresholder.GetOutput())

NodeType = itk.LevelSetNode[InternalPixelType, Dimension]
NodeContainer = itk.VectorContainer[itk.UI, NodeType]
seeds = NodeContainer.New()
seedPosition = [int(argv[3]), int(argv[4])]

initialDistance = float(argv[5])

node = NodeType()

seedValue = - initialDistance

node.SetValue(seedValue)
node.SetIndex(seedPosition)

seeds.Initialize()
seeds.InsertElement(0, node)

fastMarching.SetTrialPoints(seeds)

fastMarching.SetSpeedConstant(1.0)


reader.Update()
fastMarching.SetOutputSize(
    reader.GetOutput().GetBufferedRegion().GetSize())
writer.Update()

itk.echo(thresholdSegmentation)


InternalWriterType = itk.ImageFileWriter[InternalImageType]

outputDirectory = os.path.dirname(argv[2])
mapWriter = InternalWriterType.New()
mapWriter.SetInput(fastMarching.GetOutput())
mapWriter.SetFileName(os.path.join(outputDirectory, "fastMarchingImage.mha"))
mapWriter.Update()

speedWriter = InternalWriterType.New()
speedWriter.SetInput(thresholdSegmentation.GetSpeedImage())
speedWriter.SetFileName(os.path.join(outputDirectory, "speedTermImage.mha"))
speedWriter.Update()
