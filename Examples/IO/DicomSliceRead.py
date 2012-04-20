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
#  Example on the use of ImageFileReader to reading a single slice (it will read
#  DICOM or other format), rescale the intensities and save it in a different
#  file format.
#

import itk
import sys

if len(sys.argv) < 3:
    print('Usage: ' + sys.argv[0] + ' inputFile.dcm outputFile.png')
    sys.exit(1)

#
# Reads a 2D image in with signed short (16bits/pixel) pixel type
# and save it as unsigned char (8bits/pixel) pixel type
#
InputImageType  = itk.Image.SS2
OutputImageType = itk.Image.UC2

reader = itk.ImageFileReader[InputImageType].New()
writer = itk.ImageFileWriter[OutputImageType].New()

filter = itk.RescaleIntensityImageFilter[InputImageType, OutputImageType].New()
filter.SetOutputMinimum( 0 )
filter.SetOutputMaximum(255)

filter.SetInput( reader.GetOutput() )
writer.SetInput( filter.GetOutput() )

reader.SetFileName( sys.argv[1] )
writer.SetFileName( sys.argv[2] )

writer.Update()
