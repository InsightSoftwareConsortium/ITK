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

# Installed but unimportable xarray, torch or SimpleITK must be treated as absent.

import builtins

import numpy as np

import itk
from itk.support import helpers

_OPTIONAL_PACKAGES = ("xarray", "torch", "SimpleITK")
_real_import = builtins.__import__


def passthrough(image):
    return image


for error_type in (ValueError, RuntimeError):

    def broken_import(name, *args, **kwargs):
        if name.split(".")[0] in _OPTIONAL_PACKAGES:
            raise error_type(f"simulated broken install of {name}")
        return _real_import(name, *args, **kwargs)

    saved_flags = (helpers._HAVE_XARRAY, helpers._HAVE_TORCH, helpers._HAVE_SIMPLEITK)
    helpers._HAVE_XARRAY = helpers._HAVE_TORCH = helpers._HAVE_SIMPLEITK = True
    builtins.__import__ = broken_import
    try:
        wrapped = helpers.accept_array_like_xarray_torch(passthrough)
    finally:
        builtins.__import__ = _real_import
        (
            helpers._HAVE_XARRAY,
            helpers._HAVE_TORCH,
            helpers._HAVE_SIMPLEITK,
        ) = saved_flags

    array = np.arange(12, dtype=np.uint8).reshape(3, 4)
    array_output = wrapped(array)
    assert isinstance(array_output, np.ndarray), type(array_output)
    assert np.array_equal(array_output, array)

    image = itk.image_from_array(array)
    image_output = wrapped(image)
    assert isinstance(image_output, itk.Image), type(image_output)
    assert np.array_equal(itk.array_view_from_image(image_output), array)

    keyword_output = wrapped(image=array)
    assert isinstance(keyword_output, np.ndarray), type(keyword_output)

    print(f"{error_type.__name__} on import: OK")
