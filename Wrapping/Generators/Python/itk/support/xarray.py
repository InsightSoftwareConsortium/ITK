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

import builtins
from typing import Tuple, TYPE_CHECKING

import itk.support.types as itkt

if TYPE_CHECKING:
    try:
        import xarray as xr
    except ImportError:
        pass


__all__ = [
    "xarray_from_image",
    "image_from_xarray",
]


def xarray_from_image(l_image: "itkt.ImageOrImageSource", view:bool=False) -> "xr.DataArray":
    """Convert an itk.Image to an xarray.DataArray.

    Origin and spacing metadata is preserved in the xarray's coords. The
    Direction is set in the `direction` attribute.
    Dims are labeled as `x`, `y`, `z`, `t`, and `c`.

    view may be set to True to get an xarray referencing the ITK image data container
    rather than an entirely new copy. This is best used in the narrow case where
      1. poor copy performance impacts larger operations, such as for a large image, and
      2. the underlying ITK image will not release its data during the xarray lifetime.
    In most cases view should be set to False so that the lifetime of the xarray data
    is independent of the ITK pipeline.

    This interface is and behavior is experimental and is subject to possible
    future changes."""
    import xarray as xr
    import itk
    import numpy as np

    if view:
        array = itk.array_view_from_image(l_image)
    else:
        array = itk.array_from_image(l_image)
    l_spacing = itk.spacing(l_image)
    l_origin = itk.origin(l_image)
    l_size = itk.size(l_image)
    direction = np.flip(itk.array_from_matrix(l_image.GetDirection()))
    image_dimension = l_image.GetImageDimension()

    image_dims: Tuple[str, str, str, str] = ("x", "y", "z", "t")
    coords = {}
    for l_index, dim in enumerate(image_dims[:image_dimension]):
        coords[dim] = np.linspace(
            l_origin[l_index],
            l_origin[l_index] + (l_size[l_index] - 1) * l_spacing[l_index],
            l_size[l_index],
            dtype=np.float64,
        )

    dims = list(reversed(image_dims[:image_dimension]))
    components = l_image.GetNumberOfComponentsPerPixel()
    if components > 1:
        dims.append("c")
        coords["c"] = np.arange(components, dtype=np.uint32)

    direction = np.flip(itk.array_from_matrix(l_image.GetDirection()))
    attrs = {"direction": direction}
    metadata = dict(l_image)
    ignore_keys = {"direction", "origin", "spacing"}
    for key in metadata:
        if not key in ignore_keys:
            attrs[key] = metadata[key]
    name = "image"
    if l_image.GetObjectName():
        name = l_image.GetObjectName()
    data_array = xr.DataArray(
        array, name=name, dims=dims, coords=coords, attrs=attrs
    )
    return data_array


def image_from_xarray(data_array: "xr.DataArray") -> "itkt.ImageBase":
    """Convert an xarray.DataArray to an itk.Image.

    Metadata encoded with xarray_from_image is applied to the itk.Image.

    This interface is and behavior is experimental and is subject to possible
    future changes."""
    import numpy as np
    import itk

    if not {"t", "z", "y", "x", "c"}.issuperset(data_array.dims):
        raise ValueError('Unsupported dims, supported dims: "t", "z", "y", "x", "c".')

    image_dims = list({"t", "z", "y", "x"}.intersection(set(data_array.dims)))
    image_dims.sort(reverse=True)
    image_dimension = len(image_dims)
    ordered_dims = ("t", "z", "y", "x")[-image_dimension:]
    is_vector = "c" in data_array.dims
    if is_vector:
        ordered_dims = ordered_dims + ("c",)

    values = data_array.values
    if ordered_dims != data_array.dims:
        dest = list(builtins.range(len(ordered_dims)))
        source = dest.copy()
        for ii in builtins.range(len(ordered_dims)):
            source[ii] = data_array.dims.index(ordered_dims[ii])
        values = np.moveaxis(values, source, dest).copy()
    itk_image = itk.image_view_from_array(values, is_vector=is_vector)

    l_origin = [0.0] * image_dimension
    l_spacing = [1.0] * image_dimension
    for l_index, dim in enumerate(image_dims):
        coords = data_array.coords[dim]
        if coords.shape[0] > 1:
            l_origin[l_index] = float(coords[0])
            l_spacing[l_index] = float(coords[1]) - float(coords[0])
    l_spacing.reverse()
    itk_image.SetSpacing(l_spacing)
    l_origin.reverse()
    itk_image.SetOrigin(l_origin)
    if "direction" in data_array.attrs:
        direction = data_array.attrs["direction"]
        itk_image.SetDirection(np.flip(direction))
    ignore_keys = {"direction", "origin", "spacing"}
    for key in data_array.attrs:
        if not key in ignore_keys:
            itk_image[key] = data_array.attrs[key]

    itk_image.SetObjectName(str(data_array.name))

    return itk_image
