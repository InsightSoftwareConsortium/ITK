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
"""Test object lifetime, memory safety, and leak detection for itk.Image exports.

Tests are automatically skipped when features are not available in the
current ITK build.  This allows cherry-picking into older ITK releases
(e.g., v5.4.5) where PEP 688 and zero-copy np.asarray are not yet
implemented.

Features detected at runtime:
  - __buffer__:           PEP 688 buffer export (ITK 6.x)
  - __array__(copy=...):  NumPy 2.0 copy parameter (ITK 6.x)
  - Zero-copy np.asarray: mutation visible through array (ITK 6.x)
"""

import gc
import os
import sys
import weakref

import itk
import numpy as np

passed = 0
failed = 0
skipped = 0


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


def make_image(pixel_type=itk.F, dimension=3, size=None, fill=42.0):
    """Create a test image with known pixel values."""
    if size is None:
        size = [16, 16, 8]
    ImageType = itk.Image[pixel_type, dimension]
    image = ImageType.New()
    image.SetRegions(size[:dimension])
    image.Allocate()
    image.FillBuffer(fill)
    return image


def get_rss_bytes():
    """Get current resident set size in bytes (Linux/macOS)."""
    try:
        with open("/proc/self/statm") as f:
            rss_pages = int(f.read().split()[1])
        return rss_pages * os.sysconf("SC_PAGE_SIZE")
    except (OSError, ValueError):
        pass
    try:
        import resource

        rusage = resource.getrusage(resource.RUSAGE_SELF)
        if sys.platform == "darwin":
            return rusage.ru_maxrss
        return rusage.ru_maxrss * 1024
    except (ImportError, AttributeError):
        return None


# --- Feature detection ---
_probe = make_image(size=[2, 2, 2])
HAS_BUFFER = hasattr(_probe, "__buffer__")
HAS_ARRAY_COPY_PARAM = False
try:
    _probe.__array__(copy=None)
    HAS_ARRAY_COPY_PARAM = True
except TypeError:
    pass
_probe.FillBuffer(1.0)
_a = np.asarray(_probe)
_probe.SetPixel([0, 0, 0], 999.0)
HAS_ZEROCOPY = _a[0, 0, 0] == 999.0
del _a, _probe
gc.collect()

_SKIP_BUFFER = "__buffer__ not available in this ITK version"
_SKIP_ZEROCOPY = "zero-copy np.asarray not available in this ITK version"
_SKIP_COPY = "__array__(copy=) not available in this ITK version"


def run_lifetime_and_leak_tests():
    """Run all lifetime tests inside a function scope.

    All local variables are explicitly deleted at the end of each
    section, and gc.collect() is called to ensure deterministic
    cleanup.  After this function returns, the caller verifies
    that no image memory has leaked.
    """

    # ==============================================================
    #  Section 1: Deep copy paths (always safe, all versions)
    # ==============================================================
    print("=" * 60)
    print("Section 1: Deep copy paths (always safe)")
    print("=" * 60)

    if HAS_ARRAY_COPY_PARAM:
        image = make_image()
        arr = image.__array__(copy=True)
        expected = arr[0, 0, 0]
        del image
        gc.collect()
        check("__array__(copy=True) survives del image", arr[0, 0, 0] == expected)
        del arr, expected
    else:
        skip("__array__(copy=True) survives del image", _SKIP_COPY)

    image = make_image(fill=99.0)
    arr = np.array(image, copy=True)
    del image
    gc.collect()
    check("np.array(copy=True) survives del image", arr[0, 0, 0] == 99.0)
    del arr

    image = make_image(fill=77.0)
    arr = itk.array_from_image(image)
    del image
    gc.collect()
    check("itk.array_from_image survives del image", arr[0, 0, 0] == 77.0)
    del arr

    gc.collect()

    # ==============================================================
    #  Section 2: __array__ zero-copy (3.10-3.11 primary path)
    # ==============================================================
    print()
    print("=" * 60)
    print("Section 2: __array__ zero-copy (NDArrayITKBase holds ref)")
    print("=" * 60)

    if HAS_ZEROCOPY:
        image = make_image(fill=33.0)
        arr = image.__array__()
        check("__array__() value matches", arr[0, 0, 0] == 33.0)
        image.SetPixel([1, 1, 1], 123.0)
        check("__array__() zero-copy: mutation visible", arr[1, 1, 1] == 123.0)
        del image
        gc.collect()
        check(
            "__array__() survives del image",
            arr[0, 0, 0] == 33.0,
            f"expected 33.0, got {arr[0, 0, 0]}",
        )
        del arr
    else:
        skip("__array__() zero-copy tests", _SKIP_ZEROCOPY)

    image = make_image(fill=55.0)
    arr = itk.array_view_from_image(image)
    check("array_view_from_image value matches", arr[0, 0, 0] == 55.0)
    del image
    gc.collect()
    check(
        "array_view_from_image survives del image",
        arr[0, 0, 0] == 55.0,
        f"expected 55.0, got {arr[0, 0, 0]}",
    )
    del arr

    gc.collect()

    # ==============================================================
    #  Section 3: np.asarray (critical path — all versions)
    # ==============================================================
    print()
    print("=" * 60)
    print("Section 3: np.asarray lifetime (critical path)")
    print("=" * 60)

    if HAS_ZEROCOPY:
        image = make_image(fill=11.0)
        arr = np.asarray(image)
        check("np.asarray value matches", arr[0, 0, 0] == 11.0)
        image.SetPixel([0, 0, 0], 22.0)
        check("np.asarray zero-copy: mutation visible", arr[0, 0, 0] == 22.0)
        # THE CRITICAL TEST (thewtex's concern)
        del image
        gc.collect()
        check(
            "np.asarray survives del image",
            arr[0, 0, 0] == 22.0,
            f"expected 22.0, got {arr[0, 0, 0]}",
        )
        del arr

        for ptype, fill_val, dtype_name in [
            (itk.UC, 200, "uint8"),
            (itk.SS, -1000, "int16"),
            (itk.F, 3.14, "float32"),
        ]:
            image = make_image(
                pixel_type=ptype, dimension=2, size=[32, 32], fill=fill_val
            )
            arr = np.asarray(image)
            del image
            gc.collect()
            check(
                f"np.asarray({dtype_name}) survives del image",
                abs(float(arr[0, 0]) - fill_val) < 0.01,
                f"expected {fill_val}, got {arr[0, 0]}",
            )
            del arr
    else:
        skip("np.asarray zero-copy lifetime tests", _SKIP_ZEROCOPY)

    gc.collect()

    # ==============================================================
    #  Section 4: __buffer__ / memoryview (PEP 688)
    # ==============================================================
    print()
    print("=" * 60)
    print("Section 4: __buffer__ / memoryview lifetime")
    print("=" * 60)

    if HAS_BUFFER:
        image = make_image(pixel_type=itk.UC, dimension=2, size=[10, 10], fill=7)
        mv = image.__buffer__()
        check("__buffer__() value matches", mv[0, 0] == 7)
        del image
        gc.collect()
        check(
            "__buffer__() survives del image",
            mv[0, 0] == 7,
            f"expected 7, got {mv[0, 0]}",
        )
        del mv

        if sys.version_info >= (3, 12):
            image = make_image(pixel_type=itk.SS, dimension=2, size=[8, 8], fill=-5)
            mv = memoryview(image)
            check("memoryview(image) value matches", mv[0, 0] == -5)
            del image
            gc.collect()
            check(
                "memoryview(image) survives del image",
                mv[0, 0] == -5,
                f"expected -5, got {mv[0, 0]}",
            )
            del mv
        else:
            skip(
                "memoryview(image) auto",
                f"requires Python 3.12+, have {sys.version_info[:2]}",
            )

        image = make_image(pixel_type=itk.F, dimension=3, size=[4, 5, 6], fill=2.5)
        mv = image.__buffer__()
        check("__buffer__() 3D float value", abs(mv[0, 0, 0] - 2.5) < 1e-5)
        del image
        gc.collect()
        check(
            "__buffer__() 3D float survives del image",
            abs(mv[0, 0, 0] - 2.5) < 1e-5,
        )
        del mv
    else:
        skip("__buffer__ lifetime tests", _SKIP_BUFFER)

    gc.collect()

    # ==============================================================
    #  Section 5: Chained references and GC stress
    # ==============================================================
    print()
    print("=" * 60)
    print("Section 5: Chained references and GC stress")
    print("=" * 60)

    if HAS_ZEROCOPY:
        # slice chain
        image = make_image(fill=50.0)
        arr = np.asarray(image)
        sliced = arr[2:6, 2:6, 0:4]
        check("slice value before del", sliced[0, 0, 0] == 50.0)
        del image
        gc.collect()
        check("slice survives del image", sliced[0, 0, 0] == 50.0)
        del arr
        gc.collect()
        check("slice survives del arr AND del image", sliced[0, 0, 0] == 50.0)
        del sliced

        # transpose chain
        image = make_image(fill=60.0)
        arr = np.asarray(image)
        transposed = arr.T
        del image
        gc.collect()
        check("transposed view survives del image", transposed[0, 0, 0] == 60.0)
        del arr, transposed

        # ravel chain
        image = make_image(pixel_type=itk.UC, dimension=2, size=[8, 8], fill=9)
        arr = np.asarray(image)
        flat = arr.ravel()
        del image
        gc.collect()
        check("raveled view survives del image", flat[0] == 9)
        del arr, flat

        # multi-ref
        image = make_image(fill=70.0)
        arr1 = np.asarray(image)
        arr2 = np.asarray(image)
        if HAS_BUFFER:
            mv = image.__buffer__()
        del image
        gc.collect()
        check("arr1 survives (multi-ref)", arr1[0, 0, 0] == 70.0)
        check("arr2 survives (multi-ref)", arr2[0, 0, 0] == 70.0)
        if HAS_BUFFER:
            check("memoryview survives (multi-ref)", abs(mv[0, 0, 0] - 70.0) < 1e-5)
            del mv
        del arr1, arr2

        # rapid create-export-delete — crash-freedom is the assertion;
        # any use-after-free would raise or segfault inside the loop.
        for i in range(100):
            img = make_image(pixel_type=itk.UC, dimension=2, size=[4, 4], fill=i % 256)
            a = np.asarray(img)
            del a, img
        gc.collect()
        check("100x create-export-delete cycle (no crash)", True)
    else:
        skip("chained reference tests", _SKIP_ZEROCOPY)

    # ==============================================================
    #  Section 6: Filter pipeline lifetime
    # ==============================================================
    print()
    print("=" * 60)
    print("Section 6: Filter pipeline output lifetime")
    print("=" * 60)

    if HAS_ZEROCOPY:
        input_image = make_image(fill=10.0)
        median = itk.MedianImageFilter.New(input_image, Radius=1)
        median.Update()
        output = median.GetOutput()
        arr = np.asarray(output)
        check("filter output np.asarray value", abs(arr[2, 2, 2] - 10.0) < 1e-5)
        del median, input_image, output
        gc.collect()
        check(
            "filter output array survives pipeline deletion",
            abs(arr[2, 2, 2] - 10.0) < 1e-5,
        )
        del arr

        input_image = make_image(fill=20.0)
        median = itk.MedianImageFilter.New(input_image, Radius=1)
        median.Update()
        arr = np.asarray(median.GetOutput())
        del median, input_image
        gc.collect()
        check(
            "inline GetOutput().asarray survives",
            abs(arr[2, 2, 2] - 20.0) < 1e-5,
        )
        del arr
    else:
        skip("pipeline lifetime tests", _SKIP_ZEROCOPY)

    gc.collect()

    # ==============================================================
    #  Section 7: Weak reference GC verification
    # ==============================================================
    print()
    print("=" * 60)
    print("Section 7: Weak reference GC verification")
    print("=" * 60)

    if HAS_ZEROCOPY:
        # np.asarray path
        image = make_image()
        ref = weakref.ref(image)
        arr = np.asarray(image)
        del image
        gc.collect()
        check("image alive while array exists", ref() is not None)
        del arr
        gc.collect()
        check(
            "image GC'd after array deleted",
            ref() is None,
            "image leaked -- ref() still alive",
        )
    else:
        skip("np.asarray weak ref test", _SKIP_ZEROCOPY)

    if HAS_BUFFER:
        # __buffer__ path
        image = make_image()
        ref = weakref.ref(image)
        mv = image.__buffer__()
        del image
        gc.collect()
        check("image alive while memoryview exists", ref() is not None)
        del mv
        gc.collect()
        check(
            "image GC'd after memoryview deleted",
            ref() is None,
            "image leaked via __buffer__ path",
        )
    else:
        skip("__buffer__ weak ref test", _SKIP_BUFFER)

    # array_view_from_image path
    image = make_image()
    ref = weakref.ref(image)
    view = itk.array_view_from_image(image)
    del image
    gc.collect()
    check("image alive while view exists", ref() is not None)
    del view
    gc.collect()
    check(
        "image GC'd after view deleted",
        ref() is None,
        "image leaked via array_view_from_image",
    )

    # deep copy must NOT hold image reference
    image = make_image()
    ref = weakref.ref(image)
    arr_copy = itk.array_from_image(image)
    del image
    gc.collect()
    check(
        "image GC'd immediately after array_from_image",
        ref() is None,
        "deep copy is holding a reference to the image",
    )
    del arr_copy

    image = make_image()
    ref = weakref.ref(image)
    arr_copy = np.array(image, copy=True)
    del image
    gc.collect()
    check("image GC'd immediately after np.array(copy=True)", ref() is None)
    del arr_copy

    if HAS_ARRAY_COPY_PARAM:
        image = make_image()
        ref = weakref.ref(image)
        arr_copy = image.__array__(copy=True)
        del image
        gc.collect()
        check("image GC'd immediately after __array__(copy=True)", ref() is None)
        del arr_copy
    else:
        skip("__array__(copy=True) GC test", _SKIP_COPY)

    if HAS_ZEROCOPY:
        # chained slice
        image = make_image()
        ref = weakref.ref(image)
        arr = np.asarray(image)
        sliced = arr[10:14, 10:14, 0:4]
        del image, arr
        gc.collect()
        check("image alive while slice exists", ref() is not None)
        del sliced
        gc.collect()
        check(
            "image GC'd after slice deleted",
            ref() is None,
            "image leaked via slice chain",
        )

        # pipeline output
        input_image = make_image(fill=10.0)
        median = itk.MedianImageFilter.New(input_image, Radius=1)
        median.Update()
        output = median.GetOutput()
        ref = weakref.ref(output)
        arr = np.asarray(output)
        del output, median, input_image
        gc.collect()
        check("pipeline output alive while array exists", ref() is not None)
        del arr
        gc.collect()
        check(
            "pipeline output GC'd after array deleted",
            ref() is None,
            "pipeline output leaked",
        )
    else:
        skip("chained/pipeline weak ref tests", _SKIP_ZEROCOPY)

    gc.collect()

    # ==============================================================
    #  Section 8: Reference count verification
    # ==============================================================
    print()
    print("=" * 60)
    print("Section 8: Reference count verification")
    print("=" * 60)

    if HAS_ZEROCOPY:
        image = make_image()
        base_refcount = sys.getrefcount(image)

        arr = np.asarray(image)
        after_asarray = sys.getrefcount(image)
        check(
            "np.asarray adds reference",
            after_asarray > base_refcount,
            f"base={base_refcount}, after={after_asarray}",
        )

        arr2 = np.asarray(image)
        after_second = sys.getrefcount(image)
        check(
            "second np.asarray adds another reference",
            after_second > after_asarray,
            f"after_first={after_asarray}, after_second={after_second}",
        )

        del arr, arr2
        gc.collect()
        restored = sys.getrefcount(image)
        check(
            "refcount restored after del arrays",
            restored == base_refcount,
            f"base={base_refcount}, restored={restored}",
        )
    else:
        skip("np.asarray refcount tests", _SKIP_ZEROCOPY)
        image = make_image()
        base_refcount = sys.getrefcount(image)

    if HAS_BUFFER:
        mv = image.__buffer__()
        after_buffer = sys.getrefcount(image)
        check(
            "__buffer__ adds reference",
            after_buffer > base_refcount,
            f"base={base_refcount}, after={after_buffer}",
        )
        del mv
        gc.collect()
        restored2 = sys.getrefcount(image)
        check(
            "refcount restored after del memoryview",
            restored2 == base_refcount,
            f"base={base_refcount}, restored={restored2}",
        )
    else:
        skip("__buffer__ refcount tests", _SKIP_BUFFER)

    del image
    gc.collect()

    # ==============================================================
    #  Section 9: Circular reference detection
    # ==============================================================
    print()
    print("=" * 60)
    print("Section 9: Circular reference detection")
    print("=" * 60)

    image = make_image(size=[8, 8, 8])
    arr = np.asarray(image)
    gc.collect()
    garbage_before = len(gc.garbage)
    del arr, image
    gc.collect()
    check(
        "no uncollectable garbage from np.asarray",
        len(gc.garbage) == garbage_before,
        f"garbage grew from {garbage_before} to {len(gc.garbage)}",
    )

    if HAS_BUFFER:
        image = make_image(size=[8, 8, 8])
        mv = image.__buffer__()
        gc.collect()
        garbage_before = len(gc.garbage)
        del mv, image
        gc.collect()
        check(
            "no uncollectable garbage from __buffer__",
            len(gc.garbage) == garbage_before,
            f"garbage grew from {garbage_before} to {len(gc.garbage)}",
        )
    else:
        skip("__buffer__ garbage test", _SKIP_BUFFER)

    # -- end of function: all locals go out of scope --


# ==================================================================
#  Main: run tests, then verify no RSS growth (leak detection)
# ==================================================================

# Warm up ITK lazy loading before measuring
_warmup = make_image(size=[4, 4, 4])
_ = np.asarray(_warmup)
del _warmup, _
gc.collect()

# Run all lifetime and GC tests inside a function scope
run_lifetime_and_leak_tests()
gc.collect()

# After the function returns, all locals are out of scope.
# Verify no RSS growth from repeated create-export-delete cycles.
print()
print("=" * 60)
print("Section 10: RSS memory growth (leak detection)")
print("=" * 60)

rss_available = get_rss_bytes() is not None
if rss_available:
    N_ITERATIONS = 200
    IMAGE_SIZE = [64, 64, 64]

    export_paths = [
        ("np.asarray", lambda img: np.asarray(img)),
        ("array_view_from_image", lambda img: itk.array_view_from_image(img)),
        ("array_from_image", lambda img: itk.array_from_image(img)),
    ]
    if HAS_BUFFER:
        export_paths.insert(1, ("__buffer__", lambda img: img.__buffer__()))

    for path_name, export_fn in export_paths:
        gc.collect()
        rss_before = get_rss_bytes()
        for _ in range(N_ITERATIONS):
            img = make_image(size=IMAGE_SIZE)
            exported = export_fn(img)
            del exported, img
        gc.collect()
        rss_after = get_rss_bytes()
        growth_mb = (rss_after - rss_before) / (1024 * 1024)
        check(
            f"{path_name} RSS growth < 20 MB ({growth_mb:.1f} MB)",
            growth_mb < 20,
            f"{growth_mb:.1f} MB over {N_ITERATIONS} iterations",
        )
else:
    skip("RSS growth tests", "RSS measurement not available on this platform")


# ==================================================================
#  Summary
# ==================================================================
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
    print("LIFETIME TESTS FAILED!")
    sys.exit(1)
else:
    print("All lifetime and memory leak tests passed.")
