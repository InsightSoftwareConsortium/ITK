# ==========================================================================
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
# ==========================================================================*/

import sys
import itk
import numpy as np

PixelType = itk.UC
Dimension = 2
ImageType = itk.Image[PixelType, Dimension]

reader = itk.ImageFileReader[ImageType].New()
reader.SetFileName(sys.argv[1])
reader.Update()

print("ready to convert image into numpy array")

arr = itk.PyBuffer[ImageType].GetArrayViewFromImage(reader.GetOutput())

print("ready to convert numpy array into image")

writer = itk.ImageFileWriter[ImageType].New()
writer.SetFileName(sys.argv[2])
writer.SetInput(itk.PyBuffer[ImageType].GetImageViewFromArray(arr))

writer.Update()
