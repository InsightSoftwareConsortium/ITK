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
import functools

input_image = itk.image_from_array(
    np.random.randint(0, 255, size=(6, 6), dtype=np.uint8)
)


def constant_output(py_image_filter, constant=42):
    output = py_image_filter.GetOutput()
    output.SetBufferedRegion(output.GetRequestedRegion())
    output.Allocate()
    output.FillBuffer(constant)


def constant_wrapper(f, constant=42):
    @functools.wraps(f)
    def wrapper(*args):
        return f(*args, constant=constant)

    return wrapper


py_filter = itk.PyImageFilter.New(input_image)
py_filter.SetPyGenerateData(constant_output)
py_filter.Update()
output_image = py_filter.GetOutput()
assert np.all(np.asarray(output_image) == 42)

# Filter calls Modified because a new PyGenerateData was passed
py_filter.SetPyGenerateData(constant_wrapper(constant_output, 10))
py_filter.Update()
output_image = py_filter.GetOutput()
assert np.all(np.asarray(output_image) == 10)

# Functional interface
output_image = itk.py_image_filter(
    input_image, py_generate_data=constant_wrapper(constant_output, 7)
)
assert np.all(np.asarray(output_image) == 7)

# PyGenerateInputRequestedRegion
class CheckCalled:
    called = False

    def __call__(self, py_image_filter):
        self.called = True


check_called = CheckCalled()
assert check_called.called == False

py_filter.SetPyGenerateInputRequestedRegion(check_called)
py_filter.Update()
assert check_called.called == True


# PyGenerateOutputInformation
check_called = CheckCalled()
assert check_called.called == False

py_filter.SetPyGenerateOutputInformation(check_called)
py_filter.UpdateOutputInformation()
assert check_called.called == True


# PyEnlargeOutputRequestedRegion
def enlarge_output_requested_region(py_image_filter, data):
    data.SetRequestedRegionToLargestPossibleRegion()


py_filter = itk.PyImageFilter.New(input_image)
py_filter.SetPyGenerateData(constant_output)
requested_region = itk.ImageRegion[2]([2, 2], [3, 3])
py_filter.GetOutput().SetRequestedRegion(requested_region)
py_filter.Update()
region = py_filter.GetOutput().GetRequestedRegion()
assert region.GetIndex()[0] == 2
assert region.GetIndex()[1] == 2
assert region.GetSize()[0] == 3
assert region.GetSize()[1] == 3

py_filter.SetPyEnlargeOutputRequestedRegion(enlarge_output_requested_region)
py_filter.Update()
region = py_filter.GetOutput().GetRequestedRegion()
assert region.GetIndex()[0] == 0
assert region.GetIndex()[1] == 0
assert region.GetSize()[0] == 6
assert region.GetSize()[1] == 6
