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
import sys
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

# --- PEP 688 buffer protocol tests ---

# Test __buffer__ on scalar image
mv = image.__buffer__()
assert isinstance(mv, memoryview)
assert mv.format == "B"  # unsigned char
assert mv.shape == (10, 10)
assert mv[0, 0] == 4
assert mv.readonly is False

# Test that memoryview shares memory (zero-copy)
image.SetPixel([3, 5], 42)
assert mv[5, 3] == 42

# Test float image
float_image = itk.Image[itk.F, 2].New()
float_image.SetRegions([8, 6])
float_image.Allocate()
float_image.FillBuffer(3.14)
fmv = float_image.__buffer__()
assert fmv.format == "f"
assert fmv.shape == (6, 8)
assert abs(fmv[0, 0] - 3.14) < 1e-5

# Test 3D image
image_3d = itk.Image[itk.SS, 3].New()
image_3d.SetRegions([4, 5, 6])
image_3d.Allocate()
image_3d.FillBuffer(-7)
mv3d = image_3d.__buffer__()
assert mv3d.format == "h"  # signed short
assert mv3d.shape == (6, 5, 4)
assert mv3d[0, 0, 0] == -7

# On Python 3.12+, memoryview() should call __buffer__ automatically
if sys.version_info >= (3, 12):
    auto_mv = memoryview(image)
    assert auto_mv.shape == (10, 10)
    assert auto_mv[0, 0] == 4

print("All buffer protocol tests passed.")
