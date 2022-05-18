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
import itk
import numpy as np

Dimension = 2
PixelType = itk.UC

ImageType = itk.Image[PixelType, Dimension]

image_size = [10, 10]

image = ImageType.New()
image.SetRegions(image_size)
image.Allocate()
image.FillBuffer(4)

array = image.__array__()
assert array[0, 0] == 4
assert array[0, 1] == 4
assert isinstance(array, np.ndarray)

array = np.asarray(image)
assert array[0, 0] == 4
assert array[0, 1] == 4
assert isinstance(array, np.ndarray)
