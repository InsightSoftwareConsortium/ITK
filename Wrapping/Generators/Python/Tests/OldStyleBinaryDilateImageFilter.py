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
#  Example on the use of the BinaryDilateImageFilter
#

from InsightToolkit import *
from sys import argv

reader = itkImageFileReaderIUC2.New()
reader.SetFileName(argv[1])
kernel = itkFlatStructuringElement2.Ball(5)
filter = itkBinaryDilateImageFilterIUC2IUC2SE2.New()
filter.SetInput(reader.GetOutput())
filter.SetDilateValue(200)
filter.SetKernel(kernel)
writer = itkImageFileWriterIUC2.New()
writer.SetInput(filter.GetOutput())
writer.SetFileName(argv[2])

writer.Update()
