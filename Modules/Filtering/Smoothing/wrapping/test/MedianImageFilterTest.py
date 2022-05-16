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

#
#  Example on the use of the MedianImageFilter
#

import itk
import itk.support.types as itkt
from sys import argv, version_info
import warnings

if version_info >= (3, 8):
    from typing import Sequence, TypeVar, get_type_hints, get_args, get_origin, Union
else:
    from typing import Sequence, TypeVar, Union


try:
    from numpy.typing import ArrayLike
except ImportError:
    from numpy import ndarray as ArrayLike

input_filename = argv[1]
output_filename = argv[2]
radius = int(argv[3])

reader = itk.ImageFileReader.IUC2.New(FileName=input_filename)

# test the deduction of the template parameter from the input
filt = itk.MedianImageFilter.New(reader, Radius=radius)
filt.Update()
filt_result = filt.GetOutput()
watcher = itk.XMLFilterWatcher(filt, "filter")

# test the update of the filter with the snake case function
# and the setting of parameter inside it
result_snake_case = itk.median_image_filter(reader, radius=radius)

# SetPrimaryInputName("ValidInput");
compare_filter = itk.ComparisonImageFilter.New(filt_result, TestInput=result_snake_case)
compare_filter.Update()
assert compare_filter.GetMaximumDifference() < 0.000000001

if version_info >= (3, 8):
    # Check the type hints
    type_hints = get_type_hints(itk.median_image_filter, globalns={"itk": itk})

    assert "args" in type_hints
    args_hints = type_hints["args"]
    assert get_origin(args_hints) is Union
    assert itk.ImageBase in get_args(args_hints)

    assert "radius" in type_hints
    radius_hints = type_hints["radius"]
    assert get_origin(radius_hints) is Union
    assert int in get_args(radius_hints)
    assert Sequence[int] in get_args(radius_hints)

    assert "return" in type_hints
    result_hints = type_hints["return"]
    assert itk.ImageBase in get_args(args_hints)

    # Check for process_object attribute pointing to the associated class
    assert itk.median_image_filter.process_object is itk.MedianImageFilter


# Test that `__call__()` inside itkTemplate is deprecated. Replaced
# by snake_case functions
with warnings.catch_warnings(record=True) as w:
    # Cause all warnings to always be triggered.
    warnings.simplefilter("always")
    # Trigger a warning.
    result = itk.MedianImageFilter(reader, radius=radius)
    # Verify some things
    assert len(w) == 1
    print(w[-1])
    assert issubclass(w[-1].category, DeprecationWarning)
    assert "deprecated" in str(w[-1].message)

    compare_filter = itk.ComparisonImageFilter.New(filt_result, TestInput=result)
    compare_filter.Update()
    assert compare_filter.GetMaximumDifference() < 0.000000001

# Test that `call__()` on object is deprecated. Replaced
# by snake_case functions
with warnings.catch_warnings(record=True) as w:
    # Cause all warnings to always be triggered.
    warnings.simplefilter("always")
    # Trigger a warning.
    median_filter = itk.MedianImageFilter.New(reader)
    result = median_filter(reader, radius=radius)
    # Verify some things
    assert len(w) == 1
    assert issubclass(w[-1].category, DeprecationWarning)
    assert "deprecated" in str(w[-1].message)

    compare_filter = itk.ComparisonImageFilter.New(filt_result, TestInput=result)
    compare_filter.Update()
    assert compare_filter.GetMaximumDifference() < 0.000000001

# test the write method
itk.imwrite(filt_result, output_filename)
