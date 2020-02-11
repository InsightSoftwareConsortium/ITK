#==========================================================================
#
#   Copyright NumFOCUS
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

import sys
try:
    import numpy as np
except ImportError:
    # We don't have numpy -- bail
    sys.exit(0)
import itk

if len(sys.argv) < 2:
    print('Usage: ' + sys.argv[0] + ' <inputImage>')
    sys.exit(1)
inputImageFileName = sys.argv[1]

image = itk.imread(inputImageFileName)
array = itk.GetArrayFromImage(image)

extractor = itk.ExtractImageFilter.New(image)
extractionRegion = image.GetLargestPossibleRegion()
extractor.SetExtractionRegion(extractionRegion)

# GetArrayFromImage calls UpdateLargestPossibleRegion to ensure the image buffer
# has been populated
array = itk.GetArrayFromImage(extractor.GetOutput())

# GetArrayFromImage calls UpdateLargestPossibleRegion to ensure the image buffer
# has been populated with the correct region
extractionRegion.SetSize(10)
extractor.SetExtractionRegion(extractionRegion)
array = itk.GetArrayFromImage(extractor.GetOutput())
