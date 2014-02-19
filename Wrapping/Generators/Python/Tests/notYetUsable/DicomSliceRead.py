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
#  Example on the use of DicomImageIO for reading a single DICOM slice, rescale
#  the intensities and save it in a different file format.
#

from InsightToolkit import *

from sys import argv

#
# Reads an image in  16bits/pixel
# and save it as      8bits/pixel
#
reader = itkImageFileReaderUS2_New()
writer = itkImageFileWriterUC2_New()

dicomIO = itkDicomImageIO_New()

reader.SetImageIO(dicomIO.GetPointer())

filter = itkRescaleIntensityImageFilterUS2UC2_New()

filter.SetOutputMinimum(0)
filter.SetOutputMaximum(255)

filter.SetInput(reader.GetOutput())
writer.SetInput(filter.GetOutput())

reader.SetFileName(argv[1])
writer.SetFileName(argv[2])

writer.Update()
