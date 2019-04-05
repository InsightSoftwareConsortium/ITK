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

#
#  Example on the use of the CurvatureAnisotropicDiffusionImageFilter
#


import itk
from sys import argv
itk.auto_progress(2)

dim = 2
IType = itk.Image[itk.F, dim]
OIType = itk.Image[itk.UC, dim]

reader = itk.ImageFileReader[IType].New(FileName=argv[1])
filter = itk.CurvatureAnisotropicDiffusionImageFilter[IType, IType].New(
    reader,
    NumberOfIterations=eval(
        argv[
            3]),
    TimeStep=eval(argv[4]),
    ConductanceParameter=eval(argv[5]))
cast = itk.RescaleIntensityImageFilter[IType, OIType].New(filter,
                                                          OutputMinimum=0,
                                                          OutputMaximum=255)
writer = itk.ImageFileWriter[OIType].New(cast, FileName=argv[2])

writer.Update()
