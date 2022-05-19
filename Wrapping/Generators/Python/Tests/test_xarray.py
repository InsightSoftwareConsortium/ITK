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

# also test the import callback feature

import sys
import os
import numpy as np
import pathlib

import itk

filename = sys.argv[1]

# xarray conversion
try:
    import xarray as xr

    print("Testing xarray conversion")

    image = itk.imread(filename)
    image.SetSpacing((0.1, 0.2))
    image.SetOrigin((30.0, 44.0))
    theta = np.radians(30)
    cosine = np.cos(theta)
    sine = np.sin(theta)
    rotation = np.array(((cosine, -sine), (sine, cosine)))
    image.SetDirection(rotation)
    image["MyMeta"] = 4.0

    data_array = itk.xarray_from_image(image)
    # Default name
    assert data_array.name == "image"
    image.SetObjectName("test_image")
    data_array = itk.xarray_from_image(image)
    assert data_array.name == "test_image"
    assert data_array.dims[0] == "y"
    assert data_array.dims[1] == "x"
    assert data_array.dims[2] == "c"
    assert np.array_equal(data_array.values, itk.array_from_image(image))
    assert len(data_array.coords["x"]) == 256
    assert len(data_array.coords["y"]) == 256
    assert len(data_array.coords["c"]) == 3
    assert data_array.coords["x"][0] == 30.0
    assert data_array.coords["x"][1] == 30.1
    assert data_array.coords["y"][0] == 44.0
    assert data_array.coords["y"][1] == 44.2
    assert data_array.coords["c"][0] == 0
    assert data_array.coords["c"][1] == 1
    assert data_array.attrs["direction"][0, 0] == cosine
    assert data_array.attrs["direction"][0, 1] == sine
    assert data_array.attrs["direction"][1, 0] == -sine
    assert data_array.attrs["direction"][1, 1] == cosine
    assert data_array.attrs["MyMeta"] == 4.0

    round_trip = itk.image_from_xarray(data_array)
    assert round_trip.GetObjectName() == "test_image"
    assert np.array_equal(itk.array_from_image(round_trip), itk.array_from_image(image))
    spacing = round_trip.GetSpacing()
    assert np.isclose(spacing[0], 0.1)
    assert np.isclose(spacing[1], 0.2)
    origin = round_trip.GetOrigin()
    assert np.isclose(origin[0], 30.0)
    assert np.isclose(origin[1], 44.0)
    direction = round_trip.GetDirection()
    assert np.isclose(direction(0, 0), cosine)
    assert np.isclose(direction(0, 1), -sine)
    assert np.isclose(direction(1, 0), sine)
    assert np.isclose(direction(1, 1), cosine)
    assert round_trip["MyMeta"] == 4.0

    wrong_order = data_array.swap_dims({"y": "z"})
    try:
        round_trip = itk.image_from_xarray(wrong_order)
        assert False
    except ValueError:
        pass

    # Check empty array
    empty_array = np.array([], dtype=np.uint8)
    empty_array.shape = (0, 0, 0)
    empty_image = itk.image_from_array(empty_array)
    empty_da = itk.xarray_from_image(empty_image)
    empty_image_round = itk.image_from_xarray(empty_da)

    # Check order
    arr = np.random.randint(0, 255, size=(4, 5, 6), dtype=np.uint8)
    data_array = xr.DataArray(arr, dims=["z", "y", "x"])
    image = itk.image_from_xarray(data_array)
    assert np.allclose(arr, itk.array_view_from_image(image))
    assert np.allclose(arr.shape, itk.array_view_from_image(image).shape)

    data_array = xr.DataArray(arr, dims=["x", "y", "z"])
    image = itk.image_from_xarray(data_array)
    assert np.allclose(arr.transpose(), itk.array_view_from_image(image))
    assert np.allclose(arr.shape[::-1], itk.array_view_from_image(image).shape)

    data_array = xr.DataArray(arr, dims=["y", "x", "c"])
    image = itk.image_from_xarray(data_array)
    assert np.allclose(arr, itk.array_view_from_image(image))
    assert np.allclose(arr.shape, itk.array_view_from_image(image).shape)

    data_array = xr.DataArray(arr, dims=["c", "x", "y"])
    image = itk.image_from_xarray(data_array)
    assert np.allclose(arr.transpose(), itk.array_view_from_image(image))
    assert np.allclose(arr.shape[::-1], itk.array_view_from_image(image).shape)

    # Test in-place "view" where xarray data is valid only as long as ITK image
    data_array = itk.xarray_from_image(image, view=True)
    assert data_array.name == image.GetObjectName()
    # verify we can run a computation on xarray data
    assert float(data_array.min().compute()) == 1.0

    data_array = xr.DataArray(arr, dims=["q", "x", "y"])
    try:
        image = itk.image_from_xarray(data_array)
        assert False
    except ValueError:
        pass

    if "(<itkCType unsigned char>, 4)" in itk.Image.GetTypesAsList():
        arr = np.random.randint(0, 255, size=(4, 5, 6, 3), dtype=np.uint8)
        data_array = xr.DataArray(arr, dims=["t", "z", "y", "x"])
        image = itk.image_from_xarray(data_array)
        assert np.allclose(arr, itk.array_view_from_image(image))
        assert np.allclose(arr.shape, itk.array_view_from_image(image).shape)

except ImportError:
    print("xarray not imported. Skipping xarray conversion tests")
    pass
