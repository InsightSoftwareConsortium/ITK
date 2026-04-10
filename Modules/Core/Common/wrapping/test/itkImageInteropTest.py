#!/usr/bin/env python
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
"""Comprehensive interop tests for itk.Image with numpy, torch, and dask.

Tests use image sizes representative of clinical imaging modalities:
  - CT:  512x512x128 signed short (int16)
  - MRI: 256x256x64  float32
  - DWI: 128x128x40  float32 vector (30 gradient directions)

Required: numpy
Optional: torch, dask (tests skipped if unavailable)
"""
import sys
import itk
import numpy as np

# --- Optional dependency detection ---
_HAVE_TORCH = False
try:
    import torch

    _HAVE_TORCH = True
except ImportError:
    pass

_HAVE_DASK = False
try:
    import dask
    import dask.array as da

    _HAVE_DASK = True
except ImportError:
    pass

passed = 0
skipped = 0
failed = 0


def check(name, condition, detail=""):
    global passed, failed
    if condition:
        passed += 1
    else:
        failed += 1
        msg = f"  FAIL: {name}"
        if detail:
            msg += f" ({detail})"
        print(msg)


def skip(name, reason):
    global skipped
    skipped += 1
    print(f"  SKIP: {name} ({reason})")


# --- Feature detection ---
_probe = itk.Image[itk.SS, 3].New()
_probe.SetRegions([2, 2, 2])
_probe.Allocate()
HAS_BUFFER = hasattr(_probe, "__buffer__")

# Detect whether __array__ accepts the copy keyword (NumPy 2.x protocol).
HAS_ARRAY_COPY_PARAM = False
try:
    _probe.__array__(copy=None)
    HAS_ARRAY_COPY_PARAM = True
except TypeError:
    pass

# Detect whether np.asarray produces a zero-copy view.
_probe.FillBuffer(0)
_a = np.asarray(_probe)
_probe.SetPixel([0, 0, 0], 999)
HAS_ZEROCOPY = _a[0, 0, 0] == 999
del _a, _probe


# ===================================================================
#  Helper: create images matching clinical modalities
# ===================================================================


def make_ct_image():
    """CT: 512x512x128, signed short (int16), ~67 MB."""
    img = itk.Image[itk.SS, 3].New()
    img.SetRegions([512, 512, 128])
    img.SetSpacing([0.5, 0.5, 1.0])
    img.Allocate()
    img.FillBuffer(-1024)  # Hounsfield air
    return img


def make_mri_image():
    """MRI T1: 256x256x64, float32, ~16 MB."""
    img = itk.Image[itk.F, 3].New()
    img.SetRegions([256, 256, 64])
    img.SetSpacing([1.0, 1.0, 2.0])
    img.Allocate()
    img.FillBuffer(500.0)  # Typical T1 signal
    return img


def make_dwi_image():
    """DWI: 128x128x40, float32, single volume, ~2.5 MB."""
    img = itk.Image[itk.F, 3].New()
    img.SetRegions([128, 128, 40])
    img.SetSpacing([2.0, 2.0, 2.5])
    img.Allocate()
    img.FillBuffer(800.0)  # B0 signal
    return img


def make_rgb_image():
    """RGB color: 256x256, unsigned char, ~192 KB."""
    img = itk.Image[itk.RGBPixel[itk.UC], 2].New()
    img.SetRegions([256, 256])
    img.Allocate()
    return img


def make_vector_image():
    """Vector image: 64x64x32, 6 components (DTI tensor), float32."""
    img = itk.VectorImage[itk.F, 3].New()
    img.SetRegions([64, 64, 32])
    img.SetNumberOfComponentsPerPixel(6)
    img.Allocate(True)  # zero-initialize
    return img


# ===================================================================
#  Section 1: NumPy interop (REQUIRED)
# ===================================================================
print("=" * 60)
print("Section 1: NumPy interop (required)")
print("=" * 60)

# -- 1.1 np.asarray basic properties --
ct = make_ct_image()
ct_arr = np.asarray(ct)
check("CT np.asarray shape", ct_arr.shape == (128, 512, 512))
check("CT np.asarray dtype", ct_arr.dtype == np.int16)
check("CT np.asarray value", ct_arr[0, 0, 0] == -1024)

# -- 1.2 np.asarray zero-copy --
if HAS_ZEROCOPY:
    # Verify zero-copy: modify through numpy, read back through ITK
    ct_arr[10, 20, 30] = 1500
    check("CT zero-copy np->itk", ct.GetPixel([30, 20, 10]) == 1500)

    # Verify zero-copy: modify through ITK, read back through numpy
    ct.SetPixel([5, 6, 7], 42)
    check("CT zero-copy itk->np", ct_arr[7, 6, 5] == 42)
else:
    skip("CT zero-copy np->itk", "zero-copy np.asarray not available")
    skip("CT zero-copy itk->np", "zero-copy np.asarray not available")

# -- 1.3 MRI image --
mri = make_mri_image()
mri_arr = np.asarray(mri)
check("MRI np.asarray shape", mri_arr.shape == (64, 256, 256))
check("MRI np.asarray dtype", mri_arr.dtype == np.float32)
check("MRI np.asarray value", abs(mri_arr[0, 0, 0] - 500.0) < 1e-5)

# Zero-copy write
if HAS_ZEROCOPY:
    mri_arr[32, 128, 128] = 999.0
    check("MRI zero-copy", mri.GetPixel([128, 128, 32]) == 999.0)
else:
    skip("MRI zero-copy", "zero-copy np.asarray not available")

# -- 1.4 DWI single volume --
dwi = make_dwi_image()
dwi_arr = np.asarray(dwi)
check("DWI np.asarray shape", dwi_arr.shape == (40, 128, 128))
check("DWI np.asarray dtype", dwi_arr.dtype == np.float32)

# -- 1.5 Multiple pixel types --
for ptype, expected_dtype, label in [
    (itk.UC, np.uint8, "UC/uint8"),
    (itk.SS, np.int16, "SS/int16"),
    (itk.SI, np.int32, "SI/int32"),
    (itk.F, np.float32, "F/float32"),
    (itk.D, np.float64, "D/float64"),
]:
    img = itk.Image[ptype, 3].New()
    img.SetRegions([4, 4, 4])
    img.Allocate()
    arr = np.asarray(img)
    check(f"{label} dtype", arr.dtype == expected_dtype, f"got {arr.dtype}")
    check(f"{label} shape", arr.shape == (4, 4, 4))

# -- 1.6 __array__ returns view (not copy) --
if HAS_ZEROCOPY:
    mri2 = make_mri_image()
    arr1 = mri2.__array__()
    check("__array__ returns ndarray", isinstance(arr1, np.ndarray))
    arr1[0, 0, 0] = 12345.0
    check("__array__ zero-copy", mri2.GetPixel([0, 0, 0]) == 12345.0)
    del mri2, arr1
else:
    skip("__array__ zero-copy", "zero-copy np.asarray not available")

# -- 1.6b __array__(copy=...) NumPy 2.x protocol --
if HAS_ARRAY_COPY_PARAM:
    img_copy_test = make_mri_image()

    # copy=None should return a view (zero-copy)
    arr_none = img_copy_test.__array__(copy=None)
    check("__array__(copy=None) is ndarray", isinstance(arr_none, np.ndarray))
    if HAS_ZEROCOPY:
        arr_none[0, 0, 0] = 77777.0
        check(
            "__array__(copy=None) zero-copy",
            img_copy_test.GetPixel([0, 0, 0]) == 77777.0,
        )

    # copy=True should return a deep copy
    img_copy_test.FillBuffer(500.0)
    arr_copy = img_copy_test.__array__(copy=True)
    arr_copy[0, 0, 0] = 99999.0
    check(
        "__array__(copy=True) is deep copy",
        img_copy_test.GetPixel([0, 0, 0]) == 500.0,
    )

    # copy=False should return a view (zero-copy, no dtype conversion)
    arr_false = img_copy_test.__array__(copy=False)
    check("__array__(copy=False) is ndarray", isinstance(arr_false, np.ndarray))
    if HAS_ZEROCOPY:
        arr_false[0, 0, 0] = 11111.0
        check(
            "__array__(copy=False) zero-copy",
            img_copy_test.GetPixel([0, 0, 0]) == 11111.0,
        )

    # copy=False with incompatible dtype should raise ValueError
    raised = False
    try:
        img_copy_test.__array__(dtype=np.float64, copy=False)
    except ValueError:
        raised = True
    check("__array__(dtype=float64, copy=False) raises ValueError", raised)

    del img_copy_test, arr_none, arr_copy, arr_false
else:
    skip("__array__(copy=...) tests", "__array__ copy param not available")

# -- 1.7 __buffer__ protocol --
if HAS_BUFFER:
    ct_mv = ct.__buffer__()
    check("CT __buffer__ type", isinstance(ct_mv, memoryview))
    check("CT __buffer__ format", ct_mv.format == "h")  # signed short
    check("CT __buffer__ shape", ct_mv.shape == (128, 512, 512))

    if sys.version_info >= (3, 12):
        auto_mv = memoryview(ct)
        check("PEP 688 auto memoryview", auto_mv.shape == (128, 512, 512))
    else:
        skip(
            "PEP 688 auto memoryview",
            f"requires Python 3.12+, have {sys.version_info[:2]}",
        )
else:
    skip("__buffer__ protocol tests", "__buffer__ not available in this ITK version")

# -- 1.8 Vector/multi-component images --
# Note: VectorImage uses array_view_from_image path (DECL_PYTHON_IMAGE_CLASS
# is only applied to itk::Image, not itk::VectorImage)
vec = make_vector_image()
vec_arr = itk.array_view_from_image(vec)
check("VectorImage shape", vec_arr.shape == (32, 64, 64, 6))
check("VectorImage dtype", vec_arr.dtype == np.float32)

# -- 1.9 np.array with dtype conversion --
ct_f32 = np.array(ct, dtype=np.float32)
check("np.array dtype conversion", ct_f32.dtype == np.float32)

# -- 1.10 Consistency: np.asarray vs __buffer__ vs array_view --
mri3 = make_mri_image()
via_asarray = np.asarray(mri3)
via_view = itk.array_view_from_image(mri3)
if HAS_BUFFER:
    via_buf = np.asarray(mri3.__buffer__())
    check(
        "Consistency: all paths same data",
        np.array_equal(via_asarray, via_view) and np.array_equal(via_asarray, via_buf),
    )
else:
    check(
        "Consistency: np.asarray vs array_view",
        np.array_equal(via_asarray, via_view),
    )

print(f"  NumPy: {passed} passed, {failed} failed")
numpy_passed = passed
numpy_failed = failed

# ===================================================================
#  Section 2: PyTorch interop (optional)
# ===================================================================
print()
print("=" * 60)
if _HAVE_TORCH:
    print(f"Section 2: PyTorch interop (torch {torch.__version__})")
else:
    print("Section 2: PyTorch interop (SKIPPED — torch not installed)")
print("=" * 60)

if _HAVE_TORCH:
    # -- 2.1 torch.as_tensor via np.asarray --
    ct2 = make_ct_image()
    ct_tensor = torch.as_tensor(np.asarray(ct2))
    check("CT torch.as_tensor shape", tuple(ct_tensor.shape) == (128, 512, 512))
    check("CT torch.as_tensor dtype", ct_tensor.dtype == torch.int16)
    check("CT torch.as_tensor value", ct_tensor[0, 0, 0].item() == -1024)

    # -- 2.2 torch.from_numpy zero-copy chain --
    if HAS_ZEROCOPY:
        mri4 = make_mri_image()
        mri_np = np.asarray(mri4)  # zero-copy via np.asarray
        mri_tensor = torch.from_numpy(mri_np)  # zero-copy numpy->torch
        check("MRI torch shape", tuple(mri_tensor.shape) == (64, 256, 256))
        check("MRI torch dtype", mri_tensor.dtype == torch.float32)

        # Zero-copy chain: ITK -> numpy -> torch
        mri4.SetPixel([100, 100, 30], 777.0)
        check(
            "MRI zero-copy itk->np->torch",
            mri_tensor[30, 100, 100].item() == 777.0,
        )
    else:
        skip("MRI torch zero-copy chain", "zero-copy np.asarray not available")

    # -- 2.3 DWI volume --
    dwi2 = make_dwi_image()
    dwi_tensor = torch.from_numpy(np.asarray(dwi2))
    check("DWI torch shape", tuple(dwi_tensor.shape) == (40, 128, 128))

    # -- 2.4 Vector image --
    vec2 = make_vector_image()
    vec_tensor = torch.from_numpy(itk.array_view_from_image(vec2))
    check("VectorImage torch shape", tuple(vec_tensor.shape) == (32, 64, 64, 6))
    check("VectorImage torch dtype", vec_tensor.dtype == torch.float32)

    # -- 2.5 dtype mapping --
    for ptype, expected_tdtype, label in [
        (itk.UC, torch.uint8, "UC->uint8"),
        (itk.SS, torch.int16, "SS->int16"),
        (itk.SI, torch.int32, "SI->int32"),
        (itk.F, torch.float32, "F->float32"),
        (itk.D, torch.float64, "D->float64"),
    ]:
        img = itk.Image[ptype, 3].New()
        img.SetRegions([4, 4, 4])
        img.Allocate()
        t = torch.from_numpy(np.asarray(img))
        check(f"torch {label}", t.dtype == expected_tdtype, f"got {t.dtype}")

    torch_passed = passed - numpy_passed
    torch_failed = failed - numpy_failed
    print(f"  PyTorch: {torch_passed} passed, {torch_failed} failed")
else:
    skip("PyTorch tests", "torch not installed")
    print("  (install torch to enable these tests)")

# ===================================================================
#  Section 3: Dask interop (optional)
# ===================================================================
print()
print("=" * 60)
if _HAVE_DASK:
    print(f"Section 3: Dask interop (dask {dask.__version__})")
else:
    print("Section 3: Dask interop (SKIPPED — dask not installed)")
print("=" * 60)

pre_dask_passed = passed
pre_dask_failed = failed

if _HAVE_DASK:
    # -- 3.1 dask.array.from_array via np.asarray --
    ct3 = make_ct_image()
    ct_darr = da.from_array(np.asarray(ct3), chunks=(32, 128, 128))
    check("CT dask shape", ct_darr.shape == (128, 512, 512))
    check("CT dask dtype", ct_darr.dtype == np.int16)

    # Compute a chunk and verify value
    chunk = ct_darr[0:32, 0:128, 0:128].compute()
    check("CT dask compute value", chunk[0, 0, 0] == -1024)
    check("CT dask compute shape", chunk.shape == (32, 128, 128))

    # -- 3.2 MRI with dask --
    mri5 = make_mri_image()
    mri_darr = da.from_array(np.asarray(mri5), chunks=(16, 64, 64))
    check("MRI dask shape", mri_darr.shape == (64, 256, 256))
    check("MRI dask dtype", mri_darr.dtype == np.float32)

    # -- 3.3 Dask reduction on clinical-size image --
    mri_mean = mri_darr.mean().compute()
    check("MRI dask mean", abs(mri_mean - 500.0) < 1e-3, f"got {mri_mean}")

    # -- 3.4 DWI with dask --
    dwi3 = make_dwi_image()
    dwi_darr = da.from_array(np.asarray(dwi3), chunks=(10, 32, 32))
    check("DWI dask shape", dwi_darr.shape == (40, 128, 128))

    # -- 3.5 Vector image with dask --
    vec3 = make_vector_image()
    vec_darr = da.from_array(itk.array_view_from_image(vec3), chunks=(8, 16, 16, 6))
    check("VectorImage dask shape", vec_darr.shape == (32, 64, 64, 6))

    dask_passed = passed - pre_dask_passed
    dask_failed = failed - pre_dask_failed
    print(f"  Dask: {dask_passed} passed, {dask_failed} failed")
else:
    skip("Dask tests", "dask not installed")
    print("  (install dask to enable these tests)")

# ===================================================================
#  Summary
# ===================================================================
print()
print("=" * 60)
print(f"Python {sys.version_info[0]}.{sys.version_info[1]}.{sys.version_info[2]}")
print(f"NumPy {np.__version__}")
print(
    f"Features: __buffer__={HAS_BUFFER}, copy_param={HAS_ARRAY_COPY_PARAM}, "
    f"zero_copy={HAS_ZEROCOPY}"
)
print(f"TOTAL: {passed} passed, {failed} failed, {skipped} skipped")
print("=" * 60)

if failed > 0:
    sys.exit(1)

print("All interop tests passed.")
