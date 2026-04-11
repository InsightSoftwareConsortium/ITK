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
"""Test itk.Image numpy/buffer protocol integration.

Tests are automatically skipped when features are not available in the
current ITK build (e.g., __buffer__ requires PEP 688 support added in
ITK 6.x).  This allows cherry-picking into older ITK releases.
"""
import sys
import itk
import numpy as np

skipped = 0


def skip(name, reason):
    global skipped
    skipped += 1
    print(f"  SKIP: {name} ({reason})")


# --- Feature detection ---
_probe = itk.Image[itk.UC, 2].New()
_probe.SetRegions([2, 2])
_probe.Allocate()
HAS_BUFFER = hasattr(_probe, "__buffer__")
HAS_ARRAY_COPY_PARAM = False
try:
    _probe.__array__(copy=None)
    HAS_ARRAY_COPY_PARAM = True
except TypeError:
    pass
# Detect zero-copy np.asarray
_probe.FillBuffer(1)
_a = np.asarray(_probe)
_probe.SetPixel([0, 0], 99)
HAS_ZEROCOPY = _a[0, 0] == 99
del _a, _probe

# --- Setup ---
image = itk.Image[itk.UC, 2].New()
image.SetRegions([10, 10])
image.Allocate()
image.FillBuffer(4)

# --- Basic __array__ (all ITK versions) ---
array = image.__array__()
assert array[0, 0] == 4
assert array[0, 1] == 4
assert isinstance(array, np.ndarray)

array = np.asarray(image)
assert array[0, 0] == 4
assert array[0, 1] == 4
assert isinstance(array, np.ndarray)

# --- PEP 688 buffer protocol tests ---
if HAS_BUFFER:
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
    else:
        skip(
            "memoryview(image) auto",
            f"requires Python 3.12+, have {sys.version_info[:2]}",
        )
else:
    skip("PEP 688 __buffer__ tests", "__buffer__ not available in this ITK version")

# --- np.asarray zero-copy ---
if HAS_ZEROCOPY:
    arr_view = np.asarray(image)
    assert arr_view.shape == (10, 10)
    assert arr_view.dtype == np.uint8
    image.SetPixel([1, 2], 99)
    assert arr_view[2, 1] == 99  # zero-copy verified

    arr_a = image.__array__()
    image.SetPixel([0, 0], 77)
    assert arr_a[0, 0] == 77  # zero-copy verified
else:
    skip(
        "np.asarray zero-copy tests",
        "zero-copy np.asarray not available in this ITK version",
    )

# --- __array__ copy parameter (NumPy 2.0 protocol) ---
if HAS_ARRAY_COPY_PARAM:
    arr_none = image.__array__(copy=None)
    image.SetPixel([0, 0], 55)
    assert arr_none[0, 0] == 55  # zero-copy

    arr_true = image.__array__(copy=True)
    image.SetPixel([0, 0], 66)
    assert arr_true[0, 0] == 55  # copy: still old value

    arr_false = image.__array__(copy=False)
    image.SetPixel([0, 0], 88)
    assert arr_false[0, 0] == 88  # zero-copy

    try:
        _ = image.__array__(dtype=np.float64, copy=False)
        assert False, "copy=False with dtype conversion should raise ValueError"
    except ValueError:
        pass  # expected: dtype conversion requires a copy

    arr_same_dtype = image.__array__(dtype=np.uint8, copy=False)
    image.SetPixel([0, 0], 91)
    assert arr_same_dtype[0, 0] == 91  # zero-copy, no dtype conversion
else:
    skip(
        "__array__(copy=...) tests",
        "__array__ copy parameter not available in this ITK version",
    )

if skipped:
    print(f"({skipped} test group(s) skipped)")
print("All buffer protocol tests passed.")
