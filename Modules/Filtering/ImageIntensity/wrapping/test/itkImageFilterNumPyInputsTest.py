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
import itkConfig

itkConfig.LazyLoading = False
import itk
import numpy as np

itk.auto_progress(2)

array1 = np.ones((4, 4), dtype=np.uint8)
array2 = 2 * np.ones((4, 4), dtype=np.uint8)

added = itk.add_image_filter(array1, array2)
assert isinstance(added, np.ndarray)
assert np.all(added == 3)

added = itk.add_image_filter(Input1=array1, Input2=array2)
assert isinstance(added, np.ndarray)
assert np.all(added == 3)

# support kwargs with "image" in the name
masked = itk.mask_image_filter(array1, mask_image=array2)

try:
    import xarray as xr

    image1 = itk.image_from_array(array1)
    data_array1 = itk.xarray_from_image(image1)
    image2 = itk.image_from_array(array2)
    data_array2 = itk.xarray_from_image(image2)

    added = itk.add_image_filter(data_array1, data_array2)
    assert isinstance(added, xr.DataArray)
    assert np.all(added == 3)

    added = itk.add_image_filter(Input1=data_array1, Input2=data_array2)
    assert isinstance(added, xr.DataArray)
    assert np.all(added == 3)

    # support kwargs with "image" in the name
    masked = itk.mask_image_filter(data_array1, mask_image=data_array2)
except ImportError:
    # Could not import xarray
    pass

try:
    import torch

    # construct normal, interleaved (RGBRGB) ITK image
    arrayMultiChannelInterleaved = np.arange(0, 4 * 2 * 3, dtype=np.uint8).reshape(
        (4, 2, 3)
    )
    print("arrayMultiChannelInterleaved:\n", arrayMultiChannelInterleaved)
    image0 = itk.image_from_array(
        np.zeros(arrayMultiChannelInterleaved.shape, dtype=np.uint8), is_vector=True
    )
    imageMCI = itk.image_from_array(arrayMultiChannelInterleaved, ttype=type(image0))

    # construct contiguous (RRBBGG) torch tensor
    arrayMultiChannelContiguous = np.copy(arrayMultiChannelInterleaved)
    dest = list(range(arrayMultiChannelInterleaved.ndim))
    source = dest.copy()
    end = source.pop()
    source.insert(0, end)
    arrayMultiChannelContiguous = np.moveaxis(
        arrayMultiChannelContiguous, source, dest
    ).copy()
    print("arrayMultiChannelContiguous:\n", arrayMultiChannelContiguous)
    tensorMCC = torch.from_numpy(arrayMultiChannelContiguous)
    tensor0 = torch.from_numpy(
        np.zeros(arrayMultiChannelContiguous.shape, dtype=np.uint8)
    )

    # sanity check: ITK image works with unary filter
    luminanceITK = itk.rgb_to_luminance_image_filter(imageMCI)
    assert isinstance(luminanceITK, itk.Image)
    array = itk.array_view_from_image(luminanceITK)

    # check that torch tensor works with unary filter
    luminanceTensor = itk.rgb_to_luminance_image_filter(tensorMCC)
    assert isinstance(luminanceTensor, torch.Tensor)
    print("luminanceTensor:\n", luminanceTensor)
    assert np.array_equal(luminanceTensor, array)

    # sanity check: ITK images work with binary filter
    image1 = itk.add_image_filter(image0, imageMCI)
    assert isinstance(image1, itk.Image)
    array = itk.array_view_from_image(image1)
    assert np.array_equal(array, arrayMultiChannelInterleaved)

    # check that ITK image and torch tensor work with binary filter
    itkTensor = itk.add_image_filter(image0, tensorMCC)
    assert isinstance(itkTensor, torch.Tensor)
    print("itkTensor:\n", itkTensor)
    assert np.array_equal(itkTensor, arrayMultiChannelContiguous)

    # check that two torch tensors work with binary filter
    tensor1 = itk.add_image_filter(Input1=tensorMCC, Input2=tensor0)
    assert isinstance(tensor1, torch.Tensor)
    assert np.array_equal(tensor1, itkTensor)

    # check that torch tensor and ITK image work with binary filter
    tensorITK = itk.add_image_filter(tensorMCC, image0)
    assert isinstance(tensorITK, torch.Tensor)
    assert np.array_equal(tensorITK, tensor1)

except ImportError:
    # Could not import torch
    pass
