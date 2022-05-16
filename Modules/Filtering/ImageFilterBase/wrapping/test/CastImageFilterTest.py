# ==========================================================================
#
#   Copyright NumFOCUS
#
#   Licensed under the Apache License, Version 2.0 (the "License");
#   you may not use this file except in compliance with the License.
#   You may obtain a copy of the License at
#
#          https://www.apache.org/licenses/LICENSE-2.0.txt
#
#   Unless required by applicable law or agreed to in writing, software
#   distributed under the License is distributed on an "AS IS" BASIS,
#   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#   See the License for the specific language governing permissions and
#   limitations under the License.
#
# ==========================================================================*/

#
#  Example on the use of the CastImageFilter
#

import itk
from sys import argv

itk.auto_progress(2)

dim = 2
InputImageType = itk.Image[itk.F, dim]
OutputImageType = itk.Image[itk.UC, dim]

reader = itk.ImageFileReader[InputImageType].New(FileName=argv[1])
filt = itk.CastImageFilter[InputImageType, OutputImageType].New(reader)
writer = itk.ImageFileWriter[OutputImageType].New(filt, FileName=argv[2])

writer.Update()
