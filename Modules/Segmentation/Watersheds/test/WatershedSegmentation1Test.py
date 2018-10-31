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
itk.auto_progress(2)

PixelType = itk.ctype('float')
Dimension = 2
ImageType = itk.Image[PixelType, Dimension]
reader = itk.ImageFileReader[ImageType].New()
reader.SetFileName(sys.argv[1])

diffusion = itk.GradientAnisotropicDiffusionImageFilter.New(Input=reader.GetOutput())
diffusion.SetTimeStep(0.0625)
diffusion.SetConductanceParameter(9.0)
diffusion.SetNumberOfIterations(5)

gradient = itk.GradientMagnitudeImageFilter.New(Input=diffusion.GetOutput())

watershed = itk.WatershedImageFilter.New(Input=gradient.GetOutput())
watershed.SetThreshold(0.01)
watershed.SetLevel(0.2)

relabel = itk.RelabelComponentImageFilter.New(Input=watershed.GetOutput())

cast = itk.CastImageFilter[relabel.GetOutput().__class__,
  itk.Image[itk.ctype('unsigned char'), Dimension]].New(Input=relabel.GetOutput())

writer = itk.ImageFileWriter.New(Input=cast.GetOutput())
writer.SetFileName(sys.argv[2])
writer.Update()
