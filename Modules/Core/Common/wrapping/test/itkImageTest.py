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

# --- Default: zero-copy (correct NumPy np.asarray semantics) ---

# __array_interface__ is active by default
ai = image.__array_interface__
assert ai["version"] == 3, f"Expected version 3, got {ai['version']}"
assert ai["shape"] == (10, 10), f"Expected (10, 10), got {ai['shape']}"
assert isinstance(ai["data"], tuple)
assert isinstance(ai["data"][0], int)  # pointer
assert ai["data"][1] is False  # not read-only

# np.asarray returns zero-copy view (correct per NumPy semantics)
arr_view = np.asarray(image)
assert arr_view.shape == (10, 10)
assert arr_view.dtype == np.uint8
image.SetPixel([1, 2], 99)
assert arr_view[2, 1] == 99  # zero-copy verified

# __array__ returns zero-copy view by default
arr_a = image.__array__()
image.SetPixel([0, 0], 77)
assert arr_a[0, 0] == 77  # zero-copy verified

# --- __array__ copy parameter (NumPy 2.0 protocol) ---

# copy=None (default): zero-copy view
arr_none = image.__array__(copy=None)
image.SetPixel([0, 0], 55)
assert arr_none[0, 0] == 55  # zero-copy

# copy=True: always returns a copy
arr_true = image.__array__(copy=True)
image.SetPixel([0, 0], 66)
assert arr_true[0, 0] == 55  # copy: still old value

# copy=False: zero-copy, succeeds when no copy is needed
arr_false = image.__array__(copy=False)
image.SetPixel([0, 0], 88)
assert arr_false[0, 0] == 88  # zero-copy

# copy=False with incompatible dtype: raises ValueError
try:
    _ = image.__array__(dtype=np.float64, copy=False)
    assert False, "copy=False with dtype conversion should raise ValueError"
except ValueError:
    pass  # expected: dtype conversion requires a copy

# copy=False with matching dtype: succeeds
arr_same_dtype = image.__array__(dtype=np.uint8, copy=False)
image.SetPixel([0, 0], 91)
assert arr_same_dtype[0, 0] == 91  # zero-copy, no dtype conversion

# --- SIMULATE_PEP688=False: revert to legacy deep-copy behavior ---
itk.SIMULATE_PEP688 = False

# __array_interface__ is disabled in legacy mode
try:
    _ = image.__array_interface__
    assert False, "__array_interface__ should raise when SIMULATE_PEP688 is False"
except AttributeError:
    pass  # expected: reverts to legacy behavior

# __array__ returns deep copy in legacy mode
arr_copy = image.__array__()
image.SetPixel([0, 0], 123)
assert arr_copy[0, 0] == 91  # deep copy: still old value

# copy=False in legacy mode: raises ValueError (copy unavoidable)
try:
    _ = image.__array__(copy=False)
    assert False, "copy=False in legacy mode should raise ValueError"
except ValueError:
    pass  # expected: legacy mode always copies

# Restore default
del itk.SIMULATE_PEP688

print("All buffer protocol tests passed.")
