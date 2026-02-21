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
# ==========================================================================
import itk
import numpy as np
import sys

Dimension = 2
PixelType = itk.UC

ImageType = itk.Image[PixelType, Dimension]

image_size = [10, 10]

image = ImageType.New()
image.SetRegions(image_size)
image.Allocate()
image.FillBuffer(4)

if sys.version_info >= (3, 12):
    array = np.array(image)
else:
    array = np.array(image.__buffer__())

assert array[0, 0] == 4
assert array[0, 1] == 4
assert isinstance(array, np.ndarray)

if sys.version_info >= (3, 12):
    array = np.asarray(image)
else:
    array = np.asarray(image.__buffer__())
assert array[0, 0] == 4
assert array[0, 1] == 4
assert isinstance(array, np.ndarray)
