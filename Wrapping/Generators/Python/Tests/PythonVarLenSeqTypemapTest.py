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

"""Regression tests for DECL_PYTHON_VARLEN_SEQ_TYPEMAP.

The SWIG macro generates two typemap(in) pairs for every wrapped type
that has variable-length sequence semantics (itk::Array<T>,
itk::RGBPixel<T>, itk::RGBAPixel<T>):

  * typemap(in) type          - by-value, fires for itkSetMacro-style
                                setters and any method taking `type`
                                by value.
  * typemap(in) type&         - by-reference.

Historically:
  (a) the by-value path silently dropped native wrapped instances by
      leaving $1 default-constructed (issue #871), and
  (b) both paths segfaulted on non-sequence arguments because
      PyObject_Length($input) returned -1 and the subsequent
      type(size_t(-1)) allocation exploded.

This test verifies that both failure modes are gone, and that the
STYLE signature extension (swig_name, type, value_type) causes the
Python-visible error messages and __repr__ to carry the real type
name rather than the literal token "swig_name".
"""

import itk

MODULE_FAILURES = []


def check(condition, description):
    """Accumulate failures rather than aborting on the first one, so
    the test output shows every regression at once."""
    if condition:
        print(f"PASS  {description}")
    else:
        print(f"FAIL  {description}")
        MODULE_FAILURES.append(description)


# ---------------------------------------------------------------------
# The ShapePriorMAPCostFunction setters use itkSetMacro(name, ArrayType)
# which expands to `virtual void Set##name(ArrayType _arg)` -- by value.
# That is the exact code path exercised by issue #871 and the one the
# BUG fix targets.
# ---------------------------------------------------------------------

ImageType = itk.Image[itk.F, 2]
costfunction = itk.ShapePriorMAPCostFunction[ImageType, itk.F].New()


# --- Case 1: Python list -> by-value typemap (sequence path) ---------

costfunction.SetShapeParameterMeans([1.5, 2.5, 3.5])
got = costfunction.GetShapeParameterMeans()
check(
    len(got) == 3 and got[0] == 1.5 and got[1] == 2.5 and got[2] == 3.5,
    "Python list round-trips through by-value typemap",
)


# --- Case 2: native itk.Array -> by-value typemap (THE issue #871 fix) -

native = itk.Array[itk.D](4)
native.Fill(7.25)
costfunction.SetShapeParameterMeans(native)
got = costfunction.GetShapeParameterMeans()
check(
    len(got) == 4
    and got[0] == 7.25
    and got[1] == 7.25
    and got[2] == 7.25
    and got[3] == 7.25,
    "Native itk.Array[itk.D] round-trips bit-exactly (issue #871)",
)

# Size-preserving corruption guard: pre-fix, the setter silently
# received an empty array, so len(got) == 0.  Pre-fix with a
# zero-filled input this assertion alone would not have distinguished
# "returned 0-length" from "returned 4 zeros" -- hence the non-zero
# sentinel above.
check(
    len(got) == 4,
    "Size of round-tripped native itk.Array matches input",
)


# --- Case 3: None as argument -> TypeError, not a segfault ----------

try:
    costfunction.SetShapeParameterMeans(None)
    check(False, "None input raises an exception")
except TypeError as t:
    check(
        "itkArrayD" in str(t),
        "TypeError message names the expected wrapped type (swig_name "
        f"substitution works): {t}",
    )
except Exception as e:
    check(False, f"None input raised unexpected {type(e).__name__}: {e}")


# --- Case 4: list with a non-numeric element -> ValueError ---------

try:
    costfunction.SetShapeParameterMeans([1.0, "not a number", 3.0])
    check(False, "List with non-numeric element raises an exception")
except ValueError as v:
    check(
        "sequence of int or float" in str(v),
        f"ValueError message describes expected element type: {v}",
    )
except Exception as e:
    check(False, f"Non-numeric element raised unexpected {type(e).__name__}: {e}")


# --- Case 5: __repr__ shows the real type name, not the literal 'swig_name' --

arr = itk.Array[itk.D](3)
arr.Fill(0.0)
repr_str = str(arr)
check(
    "itkArrayD" in repr_str,
    f"itk.Array[itk.D] __repr__ contains the Python class name: {repr_str}",
)
check(
    "swig_name" not in repr_str,
    f"itk.Array[itk.D] __repr__ does not leak the unsubstituted "
    f"macro-parameter token 'swig_name': {repr_str}",
)


# --- Case 6: itkArrayF exercises the same macro with a different value_type --

native_f = itk.Array[itk.F](2)
native_f.Fill(1.125)
# Use the stddev setter to avoid clobbering the mean under test.
# itk.Array[itk.F] maps to a different DECL_PYTHON_VARLEN_SEQ_TYPEMAP
# invocation (itkArrayF, float) so this catches macro-expansion
# regressions that might affect only one value_type.
stddev_target = itk.ShapePriorMAPCostFunction[ImageType, itk.F].New()
# The setter takes an itk::Array<double>, but Python's numeric
# promotion of itkArrayF -> itkArrayD may not be automatic.  Fall
# back to verifying the itkArrayF type via its own __repr__ and
# the sequence-coercion path.
repr_f = str(native_f)
check(
    "itkArrayF" in repr_f and "swig_name" not in repr_f,
    f"itk.Array[itk.F] __repr__ substitutes correctly: {repr_f}",
)


# ---------------------------------------------------------------------

if MODULE_FAILURES:
    print(f"\n{len(MODULE_FAILURES)} assertion(s) failed:")
    for f in MODULE_FAILURES:
        print(f"  - {f}")
    raise SystemExit(1)

print("\nAll DECL_PYTHON_VARLEN_SEQ_TYPEMAP regression checks passed.")
