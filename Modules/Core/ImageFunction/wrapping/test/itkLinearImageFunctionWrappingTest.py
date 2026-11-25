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

# Regression test for Discourse #7495 / GitHub #5310:
#   `ImageFunction::IsInsideBuffer` has three C++ overloads (Index,
#   ContinuousIndex, Point), but in older releases SWIG's overload
#   dispatcher always selected the integer-only Index overload because
#   the typecheck typemap accepted any object satisfying
#   PySequence_Check + length == dim — which wrapped ContinuousIndex
#   and Point instances do via their __getitem__/__len__.  All of the
#   following calls must succeed, each reaching its intended overload.

import itk
import numpy as np

image = itk.GetImageFromArray(np.full((10, 10, 10), 1.0, dtype=np.float32))
interp = itk.LinearInterpolateImageFunction.New(image)

# All four documented overloads must be reachable and produce True for
# coordinates that are inside the buffer.

# 1. Raw Python list of ints  -> Index overload.
assert interp.IsInsideBuffer([1, 2, 3]) is True

# 2. Wrapped ContinuousIndex   -> ContinuousIndex overload.
cindex = itk.ContinuousIndex[itk.D, 3]()
cindex[0] = 1.5
cindex[1] = 2.5
cindex[2] = 3.5
assert interp.IsInsideBuffer(cindex) is True

# 3. Wrapped Point             -> Point overload.
point = itk.Point[itk.D, 3]()
point[0] = 1.5
point[1] = 2.5
point[2] = 3.5
assert interp.IsInsideBuffer(point) is True

# 4. Raw Python list of floats -> ContinuousIndex overload (selected
#    after the Index typecheck correctly rejects a float-element list).
assert interp.IsInsideBuffer([1.5, 2.5, 3.5]) is True

# Out-of-buffer cases must return False, not throw.
assert interp.IsInsideBuffer([100, 100, 100]) is False
out_point = itk.Point[itk.D, 3]()
for d in range(3):
    out_point[d] = 100.0
assert interp.IsInsideBuffer(out_point) is False

print("All IsInsideBuffer overload-dispatch checks passed.")
