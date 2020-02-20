#==========================================================================
#
#   Copyright NumFOCUS
#
#   Licensed under the Apache License, Version 2.0 (the "License");
#   you may not use this file except in compliance with the License.
#   You may obtain a copy of the License at
#
#          http://www.apache.org/licenses/LICENSE-2.0.txt
#
#   Unless required by applicable law or agreed to in writing, software
#   distributed under the License is distributed on an "AS IS" BASIS,
#   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#   See the License for the specific language governing permissions and
#   limitations under the License.
#
#==========================================================================*/

import itk
import numpy as np
itk.auto_progress(2)

array1 = np.ones((4,4), dtype=np.uint8)
array2 = 2*np.ones((4,4), dtype=np.uint8)

added = itk.add_image_filter(array1, array2)
assert(isinstance(added, np.ndarray))
assert(np.all(added == 3))

added = itk.add_image_filter(Input1=array1, Input2=array2)
assert(isinstance(added, np.ndarray))
assert(np.all(added == 3))

# support kwargs with "image" in the name
masked = itk.mask_image_filter(array1, mask_image=array2)

try:
    import xarray as xr

    image1 = itk.image_from_array(array1)
    data_array1 = itk.xarray_from_image(image1)
    image2 = itk.image_from_array(array2)
    data_array2 = itk.xarray_from_image(image2)

    added = itk.add_image_filter(data_array1, data_array2)
    assert(isinstance(added, xr.DataArray))
    assert(np.all(added == 3))

    added = itk.add_image_filter(Input1=data_array1, Input2=data_array2)
    assert(isinstance(added, xr.DataArray))
    assert(np.all(added == 3))

    # support kwargs with "image" in the name
    masked = itk.mask_image_filter(data_array1, mask_image=data_array2)
except ImportError:
    # Could not import xarray
    pass
