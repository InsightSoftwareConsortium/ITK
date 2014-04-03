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
#  Example on the use of the GrayscaleDilateImageFilter
#  and Box strucutring element
#

import itk
from sys import argv
itk.auto_progress(2)

dim = 2
IType = itk.Image[itk.US, dim]
OIType = itk.Image[itk.UC, dim]

reader = itk.ImageFileReader[IType].New(FileName=argv[1])
reader2 = itk.ImageFileReader[OIType].New(FileName=argv[3])
kernel = itk.FlatStructuringElement[dim].FromImageUC(reader2.GetOutput())
grayscaleFilter = itk.GrayscaleDilateImageFilter[IType, IType, kernel]
grayscaleFilter = grayscaleFilter.New(reader, Kernel=kernel)
cast = itk.CastImageFilter[IType, OIType].New(grayscaleFilter)
writer = itk.ImageFileWriter[OIType].New(cast, FileName=argv[2])

writer.Update()
