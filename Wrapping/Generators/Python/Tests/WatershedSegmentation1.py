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

reader = itk.ImageFileReader.IF2.New()
reader.SetFileName(sys.argv[1])

diffusion = itk.GradientAnisotropicDiffusionImageFilter.IF2IF2.New()
diffusion.SetInput(reader.GetOutput())
diffusion.SetTimeStep(0.0625)
diffusion.SetConductanceParameter(9.0)
diffusion.SetNumberOfIterations(5)

gradient = itk.GradientMagnitudeImageFilter.IF2IF2.New()
gradient.SetInput(diffusion.GetOutput())

watershed = itk.WatershedImageFilter.IF2.New()
watershed.SetInput(gradient.GetOutput())
watershed.SetThreshold(0.01)
watershed.SetLevel(0.2)

relabel = itk.RelabelComponentImageFilter.IUL2ISS2.New()
relabel.SetInput(watershed.GetOutput())

cast = itk.CastImageFilter.ISS2IUC2.New()
cast.SetInput(relabel.GetOutput())

writer = itk.ImageFileWriter.IUC2.New()
writer.SetFileName(sys.argv[2])
writer.SetInput(cast.GetOutput())
writer.Update()
