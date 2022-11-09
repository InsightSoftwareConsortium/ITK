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

import enum
import re
from typing import Optional, Union, Dict, Any, List, Tuple, Sequence, TYPE_CHECKING
from sys import stderr as system_error_stream

import numpy as np

try:
    from numpy.typing import ArrayLike
except ImportError:
    from numpy import ndarray as ArrayLike
import warnings
from sys import stderr as system_error_stream
import os
import builtins

fileiotype = Union[str, bytes, os.PathLike]

import itk.support.types as itkt

from .helpers import wasm_type_from_image_type, image_type_from_wasm_type
from .helpers import wasm_type_from_mesh_type, mesh_type_from_wasm_type, python_to_js
from .helpers import wasm_type_from_pointset_type, pointset_type_from_wasm_type

from .xarray import xarray_from_image, image_from_xarray

if TYPE_CHECKING:
    try:
        import vtk
    except ImportError:
        pass


__all__ = [
    "output",
    "image",
    "set_nthreads",
    "get_nthreads",
    "echo",
    "size",
    "physical_size",
    "spacing",
    "origin",
    "index",
    "region",
    "GetArrayFromImage",
    "array_from_image",
    "GetArrayViewFromImage",
    "array_view_from_image",
    "GetImageFromArray",
    "image_from_array",
    "GetImageViewFromArray",
    "image_view_from_array",
    "array_from_vector_container",
    "array_view_from_vector_container",
    "vector_container_from_array",
    "GetArrayFromVnlVector",
    "array_from_vnl_vector",
    "GetVnlVectorFromArray",
    "vnl_vector_from_array",
    "GetArrayViewFromVnlVector",
    "array_view_from_vnl_vector",
    "GetVnlMatrixFromArray",
    "vnl_matrix_from_array",
    "GetArrayFromVnlMatrix",
    "array_from_vnl_matrix",
    "GetArrayViewFromVnlMatrix",
    "array_view_from_vnl_matrix",
    "GetArrayFromMatrix",
    "array_from_matrix",
    "GetMatrixFromArray",
    "matrix_from_array",
    "xarray_from_image",
    "image_from_xarray",
    "vtk_image_from_image",
    "image_from_vtk_image",
    "dict_from_image",
    "image_from_dict",
    "image_intensity_min_max",
    "imwrite",
    "imread",
    "meshwrite",
    "meshread",
    "mesh_from_dict",
    "dict_from_mesh",
    "pointset_from_dict",
    "dict_from_pointset",
    "transform_from_dict",
    "dict_from_transform",
    "transformwrite",
    "transformread",
    "search",
    "set_inputs",
    "templated_class",
    "pipeline",
    "auto_pipeline",
    "down_cast",
    "template",
    "class_",
    "ctype",
    "python_type",
    "range",
    "TemplateTypeError",
]


def output(input):
    """
    If input object has attribute "GetOutput()" then return an itk image,
    otherwise this function simply returns the input value
    """
    if hasattr(input, "GetOutput"):
        return input.GetOutput()
    return input


def image(input):
    warnings.warn(
        "WrapITK warning: itk.image() is deprecated. " "Use itk.output() instead."
    )
    return output(input)


def set_nthreads(number_of_threads: int) -> None:
    """
    Support convenient set of the number of threads.
    Use example (in python):
        import itk
        itk.set_nthreads(4)  ## use 4 threads
    """
    assert number_of_threads > 0, (
        "Please set a positive number of threads instead of %d" % number_of_threads
    )

    import itk

    threader = itk.MultiThreaderBase.New()
    threader.SetGlobalDefaultNumberOfThreads(number_of_threads)


def get_nthreads() -> int:
    """
    Get the number of threads
    """
    import itk

    threader = itk.MultiThreaderBase.New()
    return threader.GetGlobalDefaultNumberOfThreads()


def echo(obj, f=system_error_stream) -> None:
    """Print an object to stream

    If the object has a method Print(), this method is used.
    repr(obj) is used otherwise
    """
    print(f, obj)


def size(image_or_filter: "itkt.ImageOrImageSource") -> Sequence[int]:
    """Return the size of an image, or of the output image of a filter

    This method take care of updating the needed information
    """
    # we don't need the entire output, only its size

    import itk

    image_or_filter.UpdateOutputInformation()
    img = itk.output(image_or_filter)
    return img.GetLargestPossibleRegion().GetSize()


def physical_size(image_or_filter: "itkt.ImageOrImageSource") -> Sequence[float]:
    """Return the physical size of an image, or of the output image of a filter

    This method take care of updating the needed information
    """
    # required because range is overloaded in this module
    from builtins import range

    spacing_ = spacing(image_or_filter)
    size_ = size(image_or_filter)
    result = []
    for i in range(0, spacing_.Size()):
        result.append(spacing_.GetElement(i) * size_.GetElement(i))
    return result


def spacing(image_or_filter: "itkt.ImageOrImageSource") -> Sequence[float]:
    """Return the spacing of an image, or of the output image of a filter

    This method take care of updating the needed information
    """
    import itk

    # we don't need the entire output, only its size
    image_or_filter.UpdateOutputInformation()
    img = itk.output(image_or_filter)
    return img.GetSpacing()


def origin(image_or_filter: "itkt.ImageOrImageSource") -> Sequence[float]:
    """Return the origin of an image, or of the output image of a filter

    This method take care of updating the needed information
    """
    import itk

    # we don't need the entire output, only its size
    image_or_filter.UpdateOutputInformation()
    img = itk.output(image_or_filter)
    return img.GetOrigin()


def index(image_or_filter: "itkt.ImageOrImageSource") -> Sequence[int]:
    """Return the index of an image, or of the output image of a filter

    This method take care of updating the needed information
    """
    import itk

    # we don't need the entire output, only its size
    image_or_filter.UpdateOutputInformation()
    img = itk.output(image_or_filter)
    return img.GetLargestPossibleRegion().GetIndex()


def region(image_or_filter: "itkt.ImageOrImageSource") -> "itkt.ImageRegion":
    """Return the region of an image, or of the output image of a filter

    This method take care of updating the needed information
    """
    import itk

    # we don't need the entire output, only its size
    image_or_filter.UpdateOutputInformation()
    img = itk.output(image_or_filter)
    return img.GetLargestPossibleRegion()


def _get_itk_pixelid(numpy_array_type):
    """Returns a ITK PixelID given a numpy array."""

    import itk

    def _long_type():
        if os.name == "nt":
            return itk.ULL
        else:
            return itk.UL

    # This is a Mapping from numpy array types to itk pixel types.
    _np_itk = {
        np.uint8: itk.UC,
        np.uint16: itk.US,
        np.uint32: itk.UI,
        np.uint64: _long_type(),
        np.int8: itk.SC,
        np.int16: itk.SS,
        np.int32: itk.SI,
        np.int64: itk.SL,
        np.float32: itk.F,
        np.float64: itk.D,
        np.complex64: itk.complex[itk.F],
        np.complex128: itk.complex[itk.D],
    }
    try:
        return _np_itk[numpy_array_type.dtype.type]
    except KeyError as e:
        for key in _np_itk:
            if np.issubdtype(numpy_array_type.dtype.type, key):
                return _np_itk[key]
            raise e


def _GetArrayFromImage(
    image_or_filter, function_name: str, keep_axes: bool, update: bool, ttype
) -> np.ndarray:
    """Get an Array with the content of the image buffer"""
    # Finds the image type
    import itk

    img = itk.output(image_or_filter)
    if ttype is not None:
        if isinstance(ttype, (tuple, list)):
            if len(ttype) != 1:
                raise RuntimeError("Expected 1 component in ttype tuple.")
            ImageType = ttype[0]
        else:
            ImageType = ttype
    else:
        ImageType = img.__class__
    keys = [k for k in itk.PyBuffer.keys() if k[0] == ImageType]
    if len(keys) == 0:
        raise RuntimeError("No suitable template parameter can be found.")
    # Create a numpy array of the type of the input image
    templatedFunction = getattr(itk.PyBuffer[keys[0]], function_name)
    return templatedFunction(img, keep_axes, update)


def GetArrayFromImage(
    image_or_filter: "itkt.ImageOrImageSource",
    keep_axes: bool = False,
    update: bool = True,
    ttype=None,
) -> np.ndarray:
    """Get an array with the content of the image buffer.

    When *keep_axes* is *False*, the NumPy array will have C-order
    indexing. This is the reverse of how indices are specified in ITK,
    i.e. k,j,i versus i,j,k. However C-order indexing is expected by most
    algorithms in NumPy / SciPy.

    This is a deep copy of the image buffer and is completely safe and without potential side effects.
    """
    return _GetArrayFromImage(
        image_or_filter, "GetArrayFromImage", keep_axes, update, ttype
    )


array_from_image = GetArrayFromImage


def GetArrayViewFromImage(
    image_or_filter: "itkt.ImageOrImageSource",
    keep_axes: bool = False,
    update: bool = True,
    ttype=None,
) -> np.ndarray:
    """Get an array view with the content of the image buffer.

    When *keep_axes* is *False*, the NumPy array will have C-order
    indexing. This is the reverse of how indices are specified in ITK,
    i.e. k,j,i versus i,j,k. However C-order indexing is expected by most
    algorithms in NumPy / SciPy.
    """
    return _GetArrayFromImage(
        image_or_filter, "GetArrayViewFromImage", keep_axes, update, ttype
    )


array_view_from_image = GetArrayViewFromImage


def _GetImageFromArray(arr: ArrayLike, function_name: str, is_vector: bool,
        ttype, need_contiguous:bool = True):
    """Get an ITK image from a Python array."""
    import itk

    # Verify inputs
    if not isinstance(arr, np.ndarray):
        arr = np.asarray(arr)

    if ttype is not None:
        if is_vector:
            raise RuntimeError("Cannot specify both `is_vector` and `ttype`.")
        if isinstance(ttype, (tuple, list)):
            if len(ttype) != 1:
                raise RuntimeError("Expected 1 component in ttype tuple.")
            ImageType = ttype[0]
        else:
            ImageType = ttype
        if type(itk.template(ImageType)) != tuple or len(itk.template(ImageType)) < 2:
            raise RuntimeError("Cannot determine pixel type from supplied ttype.")
        is_vector = (
            type(itk.template(ImageType)[1][0]) != itk.support.types.itkCType
            or itk.template(ImageType)[0] == itk.VectorImage
        )
    else:
        PixelType = _get_itk_pixelid(arr)
        Dimension = arr.ndim
        if is_vector:
            Dimension = arr.ndim - 1
            if arr.flags["C_CONTIGUOUS"]:
                VectorDimension = arr.shape[-1]
            else:
                VectorDimension = arr.shape[0]
            if PixelType == itk.UC:
                if VectorDimension == 3:
                    ImageType = itk.Image[itk.RGBPixel[itk.UC], Dimension]
                elif VectorDimension == 4:
                    ImageType = itk.Image[itk.RGBAPixel[itk.UC], Dimension]
                else:
                    ImageType = itk.VectorImage[PixelType, Dimension]
            else:
                ImageType = itk.VectorImage[PixelType, Dimension]
        else:
            ImageType = itk.Image[PixelType, Dimension]
    keys = [k for k in itk.PyBuffer.keys() if k[0] == ImageType]
    if len(keys) == 0:
        raise RuntimeError(
            """No suitable template parameter can be found.

Please specify an output type via the 'ttype' keyword parameter."""
        )
    templatedFunction = getattr(itk.PyBuffer[keys[0]], function_name)
    if function_name == "GetImageViewFromArray":
        return templatedFunction(arr, is_vector, need_contiguous)
    else:
        return templatedFunction(arr, is_vector)



def GetImageFromArray(
    arr: ArrayLike, is_vector: bool = False, ttype=None
) -> "itkt.ImageBase":
    """Get an ITK image from a Python array.

    This is a deep copy of the NumPy array buffer and is completely safe without potential
    side effects.

    If is_vector is True, then a 3D array will be treated as a 2D vector image,
    otherwise it will be treated as a 3D image.

    If the array uses Fortran-order indexing, i.e. i,j,k, the Image Size
    will have the same dimensions as the array shape. If the array uses
    C-order indexing, i.e. k,j,i, the image Size will have the dimensions
    reversed from the array shape.

    Therefore, since the *np.transpose* operator on a 2D array simply
    inverts the indexing scheme, the Image representation will be the
    same for an array and its transpose. If flipping is desired, see
    *np.reshape*.

    ttype can be used te specify a specific itk.Image type.
    """
    return _GetImageFromArray(arr, "GetImageFromArray", is_vector, ttype)


image_from_array = GetImageFromArray


def GetImageViewFromArray(
    arr: ArrayLike, is_vector: bool = False, ttype=None, need_contiguous=True
) -> "itkt.ImageBase":
    """Get an ITK image view (shared pixel buffer memory) from a Python array.

    If is_vector is True, then a 3D array will be treated as a 2D vector image,
    otherwise it will be treated as a 3D image.

    If the array uses Fortran-order indexing, i.e. i,j,k, the Image Size
    will have the same dimensions as the array shape. If the array uses
    C-order indexing, i.e. k,j,i, the image Size will have the dimensions
    reversed from the array shape.

    Therefore, since the *np.transpose* operator on a 2D array simply
    inverts the indexing scheme, the Image representation will be the
    same for an array and its transpose. If flipping is desired, see
    *np.reshape*.

    By default, a warning is issued if this function is called on a non-contiguous
    array, since a copy is performed and care must be taken to keep a reference
    to the copied array. This warning can be suppressed with need_contiguous=False
    """
    return _GetImageFromArray(arr, "GetImageViewFromArray", is_vector, ttype,
            need_contiguous=need_contiguous)


image_view_from_array = GetImageViewFromArray


def array_from_vector_container(
    container: "itkt.VectorContainer", ttype=None
) -> np.ndarray:
    """Get an Array with the content of the vector container"""
    import itk

    container_template = itk.template(container)
    IndexType = container_template[1][0]

    # Find container data type
    if ttype is not None:
        if isinstance(ttype, (tuple, list)):
            if len(ttype) != 1:
                raise RuntimeError("Expected 1 component in ttype tuple.")
            DataType = ttype[0]
        else:
            DataType = ttype
    else:
        DataType = container_template[1][1]
    keys = [k for k in itk.PyVectorContainer.keys() if k == (IndexType, DataType)]
    if len(keys) == 0:
        raise RuntimeError("No suitable template parameter can be found.")
    # Create numpy array of the type of the input container
    return itk.PyVectorContainer[keys[0]].array_from_vector_container(container)


def array_view_from_vector_container(
    container: "itkt.VectorContainer", ttype=None
) -> np.ndarray:
    """Get an Array view with the content of the vector container"""
    import itk

    container_template = itk.template(container)
    IndexType = container_template[1][0]

    # Find container type
    if ttype is not None:
        if isinstance(ttype, (tuple, list)):
            if len(ttype) != 1:
                raise RuntimeError("Expected 1 component in ttype tuple.")
            DataType = ttype[0]
        else:
            DataType = ttype
    else:
        DataType = container_template[1][1]
    keys = [k for k in itk.PyVectorContainer.keys() if k == (IndexType, DataType)]
    if len(keys) == 0:
        raise RuntimeError("No suitable template parameter can be found.")
    # Create numpy array of the type of the input container
    return itk.PyVectorContainer[keys[0]].array_view_from_vector_container(container)


def vector_container_from_array(arr: ArrayLike, ttype=None) -> "itkt.VectorContainer":
    """Get a vector container from a Python array"""
    import itk

    # Verify inputs
    if not isinstance(arr, np.ndarray):
        arr = np.asarray(arr)

    # Return VectorContainer with 64-bit index type
    if os.name == "nt":
        IndexType = itk.ULL
    else:
        IndexType = itk.UL

    # Find container type
    if ttype is not None:
        if isinstance(ttype, (tuple, list)):
            if len(ttype) != 1:
                raise RuntimeError("Expected 1 component in ttype tuple.")
            DataType = ttype[0]
        else:
            DataType = ttype
    else:
        DataType = _get_itk_pixelid(arr)
    keys = [k for k in itk.PyVectorContainer.keys() if k == (IndexType, DataType)]
    if len(keys) == 0:
        raise RuntimeError("No suitable template parameter can be found.")
    # Create numpy array of the type of the input container
    return itk.PyVectorContainer[keys[0]].vector_container_from_array(arr)


def _GetArrayFromVnlObject(vnl_object, function_name: str, ttype) -> np.ndarray:
    """Get an array with the content of vnl_object"""
    # Finds the vnl object type
    import itk

    if ttype is not None:
        if isinstance(ttype, (tuple, list)):
            if len(ttype) != 1:
                raise RuntimeError("Expected 1 component in ttype tuple.")
            PixelType = ttype[0]
        else:
            PixelType = ttype
    else:
        PixelType = itk.template(vnl_object)[1][0]
    keys = [k for k in itk.PyVnl.keys() if k[0] == PixelType]
    if len(keys) == 0:
        raise RuntimeError("No suitable template parameter can be found.")
    # Create a numpy array of the type of the vnl object
    templatedFunction = getattr(itk.PyVnl[keys[0]], function_name)
    return templatedFunction(vnl_object)


def GetArrayFromVnlVector(vnl_vector, ttype=None) -> np.ndarray:
    """Get an array with the content of vnl_vector"""
    return _GetArrayFromVnlObject(vnl_vector, "GetArrayFromVnlVector", ttype)


array_from_vnl_vector = GetArrayFromVnlVector


def GetArrayViewFromVnlVector(vnl_vector, ttype=None) -> np.ndarray:
    """Get an array view of vnl_vector"""
    return _GetArrayFromVnlObject(vnl_vector, "GetArrayViewFromVnlVector", ttype)


array_view_from_vnl_vector = GetArrayFromVnlVector


def GetArrayFromVnlMatrix(vnl_matrix, ttype=None) -> np.ndarray:
    """Get an array with the content of vnl_matrix"""
    return _GetArrayFromVnlObject(vnl_matrix, "GetArrayFromVnlMatrix", ttype)


array_from_vnl_matrix = GetArrayFromVnlMatrix


def GetArrayViewFromVnlMatrix(vnl_matrix, ttype=None) -> np.ndarray:
    """Get an array view of vnl_matrix"""
    return _GetArrayFromVnlObject(vnl_matrix, "GetArrayViewFromVnlMatrix", ttype)


array_view_from_vnl_matrix = GetArrayViewFromVnlMatrix


def _GetVnlObjectFromArray(arr: ArrayLike, function_name: str, ttype):
    """Get a vnl object from a Python array."""
    import itk

    # Verify inputs
    if not isinstance(arr, np.ndarray):
        arr = np.asarray(arr)

    if ttype is not None:
        if isinstance(ttype, (tuple, list)):
            if len(ttype) != 1:
                raise RuntimeError("Expected 1 component in ttype tuple.")
            PixelType = ttype[0]
        else:
            PixelType = ttype
    else:
        PixelType = _get_itk_pixelid(arr)
    keys = [k for k in itk.PyVnl.keys() if k[0] == PixelType]
    if len(keys) == 0:
        raise RuntimeError("No suitable template parameter can be found.")
    templatedFunction = getattr(itk.PyVnl[keys[0]], function_name)
    return templatedFunction(arr)


def GetVnlVectorFromArray(arr: ArrayLike, ttype=None):
    """Get a vnl vector from a Python array."""
    return _GetVnlObjectFromArray(arr, "GetVnlVectorFromArray", ttype)


vnl_vector_from_array = GetVnlVectorFromArray


def GetVnlMatrixFromArray(arr: ArrayLike, ttype=None):
    """Get a vnl matrix from a Python array."""
    return _GetVnlObjectFromArray(arr, "GetVnlMatrixFromArray", ttype)


vnl_matrix_from_array = GetVnlMatrixFromArray


def GetArrayFromMatrix(itk_matrix) -> np.ndarray:
    return GetArrayFromVnlMatrix(itk_matrix.GetVnlMatrix().as_matrix())


array_from_matrix = GetArrayFromMatrix


def GetMatrixFromArray(arr: ArrayLike) -> "itkt.Matrix":
    import itk

    # Verify inputs
    if not isinstance(arr, np.ndarray):
        arr = np.asarray(arr)

    vnl_matrix = GetVnlMatrixFromArray(arr)

    dims = arr.shape
    PixelType = _get_itk_pixelid(arr)
    m = itk.Matrix[PixelType, dims[0], dims[1]](vnl_matrix)
    return m


matrix_from_array = GetMatrixFromArray


def vtk_image_from_image(l_image: "itkt.ImageOrImageSource") -> "vtk.vtkImageData":
    """Convert an itk.Image to a vtk.vtkImageData."""
    import itk
    import vtk
    from vtk.util.numpy_support import numpy_to_vtk

    array = itk.array_view_from_image(l_image)

    vtk_image = vtk.vtkImageData()
    data_array = numpy_to_vtk(array.reshape(-1))
    data_array.SetNumberOfComponents(l_image.GetNumberOfComponentsPerPixel())
    data_array.SetName("Scalars")
    # Always set Scalars for (future?) multi-component volume rendering
    vtk_image.GetPointData().SetScalars(data_array)
    dim = l_image.GetImageDimension()
    l_spacing = [1.0] * 3
    l_spacing[:dim] = l_image.GetSpacing()
    vtk_image.SetSpacing(l_spacing)
    l_origin = [0.0] * 3
    l_origin[:dim] = l_image.GetOrigin()
    vtk_image.SetOrigin(l_origin)
    dims = [1] * 3
    dims[:dim] = itk.size(l_image)
    vtk_image.SetDimensions(dims)
    # Copy direction matrix for VTK>=9
    import vtk

    if vtk.vtkVersion.GetVTKMajorVersion() >= 9:
        l_direction = l_image.GetDirection()
        direction = itk.array_from_matrix(l_direction).flatten().tolist()
        if len(direction) == 4:
            # Change 2d matrix to 3d
            direction = [
                direction[0],
                direction[1],
                0.0,
                direction[2],
                direction[3],
                0.0,
                0.0,
                0.0,
                1.0,
            ]
        vtk_image.SetDirectionMatrix(direction)
    if l_image.GetImageDimension() == 3:
        PixelType = itk.template(l_image)[1][0]
        if PixelType == itk.Vector:
            vtk_image.GetPointData().SetVectors(data_array)
        elif PixelType == itk.CovariantVector:
            vtk_image.GetPointData().SetVectors(data_array)
        elif PixelType == itk.SymmetricSecondRankTensor:
            vtk_image.GetPointData().SetTensors(data_array)
        elif PixelType == itk.DiffusionTensor3D:
            vtk_image.GetPointData().SetTensors(data_array)
    return vtk_image


def image_from_vtk_image(vtk_image: "vtk.vtkImageData") -> "itkt.ImageBase":
    """Convert a vtk.vtkImageData to an itk.Image."""
    import itk
    from vtk.util.numpy_support import vtk_to_numpy

    point_data = vtk_image.GetPointData()
    array = vtk_to_numpy(point_data.GetScalars())
    array = array.reshape(-1)
    is_vector = point_data.GetScalars().GetNumberOfComponents() != 1
    dims = list(vtk_image.GetDimensions())
    if is_vector and dims[-1] == 1:
        # 2D
        dims = dims[:2]
        dims.reverse()
        dims.append(point_data.GetScalars().GetNumberOfComponents())
    else:
        dims.reverse()
    array.shape = tuple(dims)
    l_image = itk.image_view_from_array(array, is_vector)

    dim = l_image.GetImageDimension()
    l_spacing = [1.0] * dim
    l_spacing[:dim] = vtk_image.GetSpacing()[:dim]
    l_image.SetSpacing(l_spacing)
    l_origin = [0.0] * dim
    l_origin[:dim] = vtk_image.GetOrigin()[:dim]
    l_image.SetOrigin(l_origin)
    # Direction support with VTK 9
    import vtk

    if vtk.vtkVersion.GetVTKMajorVersion() >= 9:
        direction = vtk_image.GetDirectionMatrix()
        if dim == 3:
            direction_array = np.identity(3)
            for y in (0, 1, 2):
                for x in (0, 1, 2):
                    direction_array[x, y] = direction.GetElement(x, y)
        elif dim == 2:
            direction_array = np.identity(2)
            for y in (0, 1):
                for x in (0, 1):
                    direction_array[x, y] = direction.GetElement(x, y)
        l_direction = itk.matrix_from_array(direction_array)
        l_image.SetDirection(l_direction)
    return l_image


def dict_from_image(image: "itkt.Image") -> Dict:
    """Serialize a Python itk.Image object to a pickable Python dictionary."""
    import itk

    pixel_arr = itk.array_from_image(image)
    imageType = wasm_type_from_image_type(image)
    return dict(
        imageType=imageType,
        name=image.GetObjectName(),
        origin=tuple(image.GetOrigin()),
        spacing=tuple(image.GetSpacing()),
        size=tuple(image.GetBufferedRegion().GetSize()),
        direction=np.asarray(image.GetDirection()),
        data=pixel_arr,
    )


def image_from_dict(image_dict: Dict) -> "itkt.Image":
    """Deserialize an dictionary representing an itk.Image object."""
    import itk

    ImageType = image_type_from_wasm_type(image_dict["imageType"])
    image = itk.PyBuffer[ImageType].GetImageViewFromArray(image_dict["data"])
    image.SetOrigin(image_dict["origin"])
    image.SetSpacing(image_dict["spacing"])
    image.SetDirection(image_dict["direction"])
    image.SetObjectName(image_dict["name"])
    return image


def mesh_from_dict(mesh_dict: Dict) -> "itkt.Mesh":
    """Deserialize an dictionary representing an itk.Mesh object."""
    import itk

    MeshType = mesh_type_from_wasm_type(mesh_dict["meshType"])
    mesh = MeshType.New()

    mesh.SetObjectName(mesh_dict["name"])

    points = mesh_dict["points"]
    points = itk.vector_container_from_array(points)
    mesh.SetPoints(points)

    point_data = mesh_dict["pointData"]
    point_data = itk.vector_container_from_array(point_data)
    mesh.SetPointData(point_data)

    cells = mesh_dict["cells"]
    cells = itk.vector_container_from_array(cells)
    mesh.SetCellsArray(cells)

    cell_data = mesh_dict["cellData"]
    cell_data = itk.vector_container_from_array(cell_data)
    mesh.SetCellData(cell_data)

    return mesh


def dict_from_mesh(mesh: "itkt.Mesh") -> Dict:
    """Serialize a Python itk.Mesh object to a pickable Python dictionary."""
    import itk

    mesh_template = itk.template(mesh)
    pixel_type, mangle, pixel_type_components = wasm_type_from_mesh_type(mesh)

    number_of_points = mesh.GetNumberOfPoints()
    number_of_cells = mesh.GetNumberOfCells()

    if number_of_cells == 0:
        cells_array = np.array([], np.uint)
    else:
        cells_array = itk.array_from_vector_container(mesh.GetCellsArray())

    if number_of_points == 0:
        points_array = np.array([], np.float32)
    else:
        points_array = itk.array_from_vector_container(mesh.GetPoints()).flatten()

    point_data = mesh.GetPointData()
    if point_data.Size() == 0:
        point_data_numpy = np.array([], mangle)
    else:
        point_data_numpy = itk.array_from_vector_container(point_data)

    cell_data = mesh.GetCellData()
    if cell_data.Size() == 0:
        cell_data_numpy = np.array([], mangle)
    else:
        cell_data_numpy = itk.array_from_vector_container(cell_data)

    if os.name == "nt":
        cell_component_type = python_to_js(itk.ULL)
    else:
        cell_component_type = python_to_js(itk.UL)

    point_component_type = python_to_js(itk.F)

    # Currently use the same data type for point and cell data
    mesh_type = dict()
    mesh_type["dimension"] = mesh_template[1][1]
    mesh_type["pointComponentType"] = point_component_type
    mesh_type["pointPixelComponentType"] = mangle
    mesh_type["pointPixelType"] = pixel_type
    mesh_type["pointPixelComponents"] = pixel_type_components
    mesh_type["cellComponentType"] = cell_component_type
    mesh_type["cellPixelComponentType"] = mangle
    mesh_type["cellPixelType"] = pixel_type
    mesh_type["cellPixelComponents"] = pixel_type_components

    cell_buffer_size = cells_array.size

    return dict(
        meshType=mesh_type,
        name=mesh.GetObjectName(),
        numberOfPoints=number_of_points,
        points=points_array,
        numberOfPointPixels=point_data.Size(),
        pointData=point_data_numpy,
        numberOfCells=number_of_cells,
        cells=cells_array,
        numberOfCellPixels=cell_data.Size(),
        cellData=cell_data_numpy,
        cellBufferSize=cell_buffer_size,
    )


def pointset_from_dict(pointset_dict: Dict) -> "itkt.PointSet":
    """Deserialize an dictionary representing an itk.PointSet object."""
    import itk

    MeshType = pointset_type_from_wasm_type(pointset_dict["pointSetType"])
    mesh = MeshType.New()

    mesh.SetObjectName(pointset_dict["name"])

    points = pointset_dict["points"]
    points = itk.vector_container_from_array(points)
    mesh.SetPoints(points)

    point_data = pointset_dict["pointData"]
    point_data = itk.vector_container_from_array(point_data)
    mesh.SetPointData(point_data)
    return mesh


def dict_from_pointset(pointset: "itkt.PointSet") -> Dict:
    """Serialize a Python itk.PointSet object to a pickable Python dictionary."""
    import itk

    pointset_template = itk.template(pointset)
    pixel_type, mangle, pixel_type_components = wasm_type_from_pointset_type(pointset)

    number_of_points = pointset.GetNumberOfPoints()

    if number_of_points == 0:
        points_array = np.array([], np.float32)
    else:
        points_array = itk.array_from_vector_container(
            pointset.GetPoints()
        ).flatten()

    point_data = pointset.GetPointData()
    if point_data.Size() == 0:
        point_data_numpy = np.array([], mangle)
    else:
        point_data_numpy = itk.array_from_vector_container(point_data)

    if os.name == "nt":
        cell_component_type = python_to_js(itk.ULL)
    else:
        cell_component_type = python_to_js(itk.UL)

    point_component_type = python_to_js(itk.F)

    # Currently use the same data type for point and cell data
    pointset_type = dict()
    pointset_type["dimension"] = pointset_template[1][1]
    pointset_type["pointComponentType"] = point_component_type
    pointset_type["pointPixelComponentType"] = mangle
    pointset_type["pointPixelType"] = pixel_type
    pointset_type["pointPixelComponents"] = pixel_type_components

    return dict(
        pointSetType=pointset_type,
        name=pointset.GetObjectName(),
        numberOfPoints=number_of_points,
        points=points_array,
        numberOfPointPixels=point_data.Size(),
        pointData=point_data_numpy,
    )


def dict_from_transform(transform: "itkt.TransformBase") -> Dict:
    import itk

    def update_transform_dict(current_transform):
        current_transform_type = current_transform.GetTransformTypeAsString()
        current_transform_type_split = current_transform_type.split("_")
        component = itk.template(current_transform)

        in_transform_dict = dict()
        in_transform_dict["name"] = current_transform.GetObjectName()

        datatype_dict = {"double": itk.D, "float": itk.F}
        in_transform_dict["parametersValueType"] = python_to_js(
            datatype_dict[current_transform_type_split[1]]
        )
        in_transform_dict["inputDimension"] = int(current_transform_type_split[2])
        in_transform_dict["outputDimension"] = int(current_transform_type_split[3])
        in_transform_dict["transformName"] = current_transform_type_split[0]

        in_transform_dict["inputSpaceName"] = current_transform.GetInputSpaceName()
        in_transform_dict["outputSpaceName"] = current_transform.GetOutputSpaceName()

        # To avoid copying the parameters for the Composite Transform
        # as it is a copy of child transforms.
        if "Composite" not in current_transform_type_split[0]:
            p = np.array(current_transform.GetParameters())
            in_transform_dict["parameters"] = p

            fp = np.array(current_transform.GetFixedParameters())
            in_transform_dict["fixedParameters"] = fp

            in_transform_dict["numberOfParameters"] = p.shape[0]
            in_transform_dict["numberOfFixedParameters"] = fp.shape[0]

        return in_transform_dict

    dict_array = []
    transform_type = transform.GetTransformTypeAsString()
    if "CompositeTransform" in transform_type:
        # Add the transforms inside the composite transform
        # range is over-ridden so using this hack to create a list
        for i, _ in enumerate([0] * transform.GetNumberOfTransforms()):
            current_transform = transform.GetNthTransform(i)
            dict_array.append(update_transform_dict(current_transform))
    else:
        dict_array.append(update_transform_dict(transform))

    return dict_array


def transform_from_dict(transform_dict: Dict) -> "itkt.TransformBase":
    import itk

    def set_parameters(transform, transform_parameters, transform_fixed_parameters, data_type):
        # First set fixed parameters then parameters
        o1 = itk.OptimizerParameters[data_type](list(transform_fixed_parameters))
        transform.SetFixedParameters(o1)

        o2 = itk.OptimizerParameters[data_type](list(transform_parameters))
        transform.SetParameters(o2)

    # For checking transforms which don't take additional parameters while instantiation
    def special_transform_check(transform_name):
        if "2D" in transform_name or "3D" in transform_name:
            return True

        check_list = ["VersorTransform", "QuaternionRigidTransform"]
        for t in check_list:
            if transform_name == t:
                return True
        return False

    parametersValueType_dict = {"float32": itk.F, "float64": itk.D}

    # Loop over all the transforms in the dictionary
    transforms_list = []
    for i, _ in enumerate(transform_dict):
        data_type = parametersValueType_dict[transform_dict[i]["parametersValueType"]]

        # No template parameter needed for transforms having 2D or 3D name
        # Also for some selected transforms
        if special_transform_check(transform_dict[i]["transformName"]):
            transform_template = getattr(itk, transform_dict[i]["transformName"])
            transform = transform_template[data_type].New()
        # Currently only BSpline Transform has 3 template parameters
        # For future extensions the information will have to be encoded in
        # the transformName variable. The transform object once added in a
        # composite transform lose the information for other template parameters ex. BSpline.
        # The Spline order is fixed as 3 here.
        elif transform_dict[i]["transformName"] == "BSplineTransform":
            transform_template = getattr(itk, transform_dict[i]["transformName"])
            transform = transform_template[
                data_type, transform_dict[i]["inputDimension"], 3
            ].New()
        else:
            transform_template = getattr(itk, transform_dict[i]["transformName"])
            if len(transform_template.items()[0][0]) > 2:
                transform = transform_template[
                    data_type, transform_dict[i]["inputDimension"], transform_dict[i]["outputDimension"]
                ].New()
            else:
                transform = transform_template[
                    data_type, transform_dict[i]["inputDimension"]
                ].New()

        transform.SetObjectName(transform_dict[i]["name"])
        transform.SetInputSpaceName(transform_dict[i]["inputSpaceName"])
        transform.SetOutputSpaceName(transform_dict[i]["outputSpaceName"])

        set_parameters(
            transform,
            transform_dict[i]["parameters"],
            transform_dict[i]["fixedParameters"],
            data_type
        )
        transforms_list.append(transform)

    # If array has length more than 1 then it's a composite transform
    if len(transforms_list) > 1:
        # Create a Composite Transform object
        # and add all the transforms in it.
        data_type = parametersValueType_dict[transform_dict[0]["parametersValueType"]]
        transform = itk.CompositeTransform[data_type, transforms_list[0]['inputDimension']].New()
        for current_transform in transforms_list:
            transform.AddTransform(current_transform)
    else:
        transform = transforms_list[0]

    return transform


def image_intensity_min_max(image_or_filter: "itkt.ImageOrImageSource"):
    """Return the minimum and maximum of values in a image of in the output image of a filter

    The minimum and maximum values are returned in a tuple: (min, max)
    image_intensity_min_max() take care of updating the pipeline
    """
    import itk

    img = itk.output(image_or_filter)
    img.UpdateOutputInformation()
    img.Update()
    # don't put that calculator in the automatic pipeline
    tmp_auto_pipeline = auto_pipeline.current
    auto_pipeline.current = None
    comp = itk.MinimumMaximumImageCalculator[img].New(Image=img)
    auto_pipeline.current = tmp_auto_pipeline
    comp.Compute()
    return comp.GetMinimum(), comp.GetMaximum()


# range is a python function, and should not be overridden
# the current use of the function name "range" is for backward
# compatibility, but should be considered for removal in the future
def range(image_or_filter):
    return image_intensity_min_max(image_or_filter)


def imwrite(
    image_or_filter: "itkt.ImageOrImageSource",
    filename: fileiotype,
    compression: bool = False,
    imageio: Optional["itkt.ImageIOBase"] = None,
) -> None:
    """Write a image or the output image of a filter to a file.

    Parameters
    ----------

    image_or_filter :
        Image or filter that produces an image to write to the file.

    filename :
        Target output file path.

    compression :
        Use compression when writing if the format supports it.

    imageio :
        Use the provided itk.ImageIOBase derived instance to write the file.

    The writer is instantiated with the image type of the image in
    parameter (or, again, with the output image of the filter in parameter).
    """
    import itk

    img = itk.output(image_or_filter)
    img.UpdateOutputInformation()
    # don't put that writer in the automatic pipeline
    tmp_auto_pipeline = auto_pipeline.current
    auto_pipeline.current = None
    writer = itk.ImageFileWriter[type(img)].New(
        Input=img, FileName=f"{filename}", UseCompression=compression
    )
    auto_pipeline.current = tmp_auto_pipeline
    if imageio:
        writer.SetImageIO(imageio)
    writer.Update()


def imread(
    filename: Union[fileiotype, Sequence[Union[str, os.PathLike]]],
    pixel_type: Optional["itkt.PixelTypes"] = None,
    fallback_only: bool = False,
    imageio: Optional["itkt.ImageIOBase"] = None,
) -> "itkt.ImageBase":
    """Read an image from a file or series of files and return an itk.Image.

    Parameters
    ----------

    filename :
        File path for a single file, a list of files for an image series, or a
        directory for a DICOM image series.

    pixel_type :
        Image pixel type to cast to when loading.

    fallback_only :
        If true, first try to automatically deduce the image pixel type, and
        only use the given `pixel_type` if automatic deduction fails.

    imageio :
        Use the provided itk.ImageIOBase derived instance to read the file.

    Returns
    -------

    image :
        The resulting itk.Image.

    The reader is instantiated with the image type of the image file if
    `pixel_type` is not provided (default). The dimension of the image is
    automatically deduced from the dimension stored on disk.

    If the filename provided is a directory then the directory is assumed to
    be for a DICOM series volume.  If there is exactly one DICOM series
    volume in that directory, the reader will use an itk.ImageSeriesReader
    object to read the the DICOM filenames within that directory.

    If the given filename is a list or a tuple of file names, the reader
    will use an itk.ImageSeriesReader object to read the files.

    If `fallback_only` is set to `True`, `imread()` will first try to
    automatically deduce the image pixel_type, and only use the given
    `pixel_type` if automatic deduction fails. Failures typically happen if
    the pixel type is not supported (e.g. it is not currently wrapped).

    """
    import itk
    from itk.support.extras import TemplateTypeError

    if fallback_only:
        if pixel_type is None:
            raise Exception(
                "pixel_type must be set when using the fallback_only option"
            )
        try:
            return imread(filename)
        except (KeyError, TemplateTypeError):
            pass
    if type(filename) not in [list, tuple]:
        import os

        if os.path.isdir(filename):
            # read DICOM series of 1 image in a folder, refer to: https://github.com/RSIP-Vision/medio
            names_generator = itk.GDCMSeriesFileNames.New()
            names_generator.SetUseSeriesDetails(True)
            names_generator.AddSeriesRestriction("0008|0021")  # Series Date
            names_generator.SetDirectory(f"{filename}")
            series_uid = names_generator.GetSeriesUIDs()
            if len(series_uid) == 0:
                raise FileNotFoundError(f"no DICOMs in: {filename}.")
            if len(series_uid) > 1:
                raise OSError(
                    f"the directory: {filename} contains more than one DICOM series."
                )
            series_identifier = series_uid[0]
            filename = names_generator.GetFileNames(series_identifier)
    if type(filename) in [list, tuple]:
        template_reader_type = itk.ImageSeriesReader
        io_filename = f"{filename[0]}"
        increase_dimension = True
        kwargs = {"FileNames": [f"{str(f)}" for f in filename]}
    else:
        template_reader_type = itk.ImageFileReader
        io_filename = f"{filename}"
        increase_dimension = False
        kwargs = {"FileName": f"{filename}"}
    if imageio:
        kwargs["ImageIO"] = imageio
    if pixel_type:
        image_IO = itk.ImageIOFactory.CreateImageIO(
            io_filename, itk.CommonEnums.IOFileMode_ReadMode
        )
        if not image_IO:
            raise RuntimeError("No ImageIO is registered to handle the given file.")
        image_IO.SetFileName(io_filename)
        image_IO.ReadImageInformation()
        dimension = image_IO.GetNumberOfDimensions()
        # Increase dimension if last dimension is not of size one.
        if increase_dimension and image_IO.GetDimensions(dimension - 1) != 1:
            dimension += 1
        is_vlv = False
        try:
            is_vlv = itk.template(pixel_type)[0] is itk.VariableLengthVector
        except KeyError:
            pass
        if is_vlv:
            ImageType = itk.VectorImage[itk.template(pixel_type)[1][0], dimension]
        else:
            ImageType = itk.Image[pixel_type, dimension]
        reader = template_reader_type[ImageType].New(**kwargs)
    else:
        reader = template_reader_type.New(**kwargs)
    reader.Update()
    return reader.GetOutput()


def meshwrite(
    mesh: "itkt.Mesh", filename: fileiotype, compression: bool = False
) -> None:
    """Write a mesh to a file.

    The writer is instantiated according to the type of the input mesh.
    """
    import itk

    mesh.UpdateOutputInformation()
    # don't put that writer in the automatic pipeline
    tmp_auto_pipeline = auto_pipeline.current
    auto_pipeline.current = None
    writer = itk.MeshFileWriter[type(mesh)].New(
        Input=mesh, FileName=f"{filename}", UseCompression=compression
    )
    auto_pipeline.current = tmp_auto_pipeline
    writer.Update()


def meshread(
    filename: fileiotype,
    pixel_type: Optional["itkt.PixelTypes"] = None,
    fallback_only: bool = False,
) -> "itkt.Mesh":
    """Read a mesh from a file and return an itk.Mesh.

    The reader is instantiated with the mesh type of the mesh file if
    `pixel_type` is not provided (default). The dimension of the mesh is
    automatically found.

    If `fallback_only` is set to `True`, `meshread()` will first try to
    automatically deduce the image pixel_type, and only use the given
    `pixel_type` if automatic deduction fails. Failures typically
    happen if the pixel type is not supported (e.g. it is not currently
    wrapped).
    """
    import itk

    if fallback_only:
        if pixel_type is None:
            raise Exception(
                "pixel_type must be set when using the fallback_only option"
            )
        try:
            return meshread(filename)
        except (KeyError, itk.TemplateTypeError):
            pass
    TemplateReaderType = itk.MeshFileReader
    io_filename = f"{filename}"
    increase_dimension = False
    kwargs = {"FileName": f"{filename}"}
    if pixel_type:
        meshIO = itk.MeshIOFactory.CreateMeshIO(
            io_filename, itk.CommonEnums.IOFileMode_ReadMode
        )
        if not meshIO:
            raise RuntimeError("No MeshIO is registered to handle the given file.")
        meshIO.SetFileName(io_filename)
        meshIO.ReadMeshInformation()
        dimension = meshIO.GetPointDimension()
        # Increase dimension if last dimension is not of size one.
        if increase_dimension and meshIO.GetDimensions(dimension - 1) != 1:
            dimension += 1
        MeshType = itk.Mesh[pixel_type, dimension]
        reader = TemplateReaderType[MeshType].New(**kwargs)
    else:
        reader = TemplateReaderType.New(**kwargs)
    reader.Update()
    return reader.GetOutput()


def transformread(filename: fileiotype) -> List["itkt.TransformBase"]:
    """Read an itk Transform file.

    Parameters
    ----------

    filename:
        Path to the transform file (typically a .h5 file).

    Returns
    -------

    A Python list containing the transforms in the file.
    """
    import itk

    reader = itk.TransformFileReaderTemplate[itk.D].New()
    reader.SetFileName(f"{filename}")
    reader.Update()

    transforms = []
    transform_list = reader.GetModifiableTransformList()
    while not transform_list.empty():
        transform = transform_list.pop()
        transforms.append(itk.down_cast(transform))
    transforms.reverse()

    return transforms


def transformwrite(
    transforms: List["itkt.TransformBase"],
    filename: fileiotype,
    compression: bool = False,
) -> None:
    """Write an itk Transform file.

    Parameters
    ----------

    transforms: list of itk.TransformBaseTemplate[itk.D]
        Python list of the transforms to write.

    filename:
        Path to the transform file (typically a .h5 file).

    compression:
        Use compression, if the file format supports it.
    """
    import itk

    writer = itk.TransformFileWriterTemplate[itk.D].New()
    writer.SetFileName(f"{filename}")
    writer.SetUseCompression(compression)
    for transform in transforms:
        writer.AddTransform(transform)
    writer.Update()


def search(s: str, case_sensitive: bool = False) -> List[str]:  # , fuzzy=True):
    """Search for a class name in the itk module."""
    s = s.replace(" ", "")
    if not case_sensitive:
        s = s.lower()
    import itk

    names = sorted(dir(itk))
    # exact match first
    if case_sensitive:
        res = [n for n in names if s == n]
    else:
        res = [n for n in names if s == n.lower()]
    # then exact match inside the name
    if case_sensitive:
        res += [n for n in names if s in n and s != n]
    else:
        res += [n for n in names if s in n.lower() and s != n.lower()]
    #     if fuzzy:
    #         try:
    # everything now requires editdist
    #             import editdist
    #             if case_sensitive:
    #                 res.sort(key=lambda x: editdist.distance(x, s))
    #             else:
    #                 res.sort(key=lambda x: (editdist.distance(x.lower(), s), x))
    #         except:
    #             pass
    return res


def _snake_to_camel(keyword: str):
    # Helpers for set_inputs snake case to CamelCase keyword argument conversion
    _snake_underscore_re = re.compile("(_)([a-z0-9A-Z])")

    def _underscore_upper(match_obj):
        return match_obj.group(2).upper()

    camel = keyword[0].upper()
    if _snake_underscore_re.search(keyword[1:]):
        return camel + _snake_underscore_re.sub(_underscore_upper, keyword[1:])
    return camel + keyword[1:]


def set_inputs(
    new_itk_object,
    inargs: Optional[Sequence[Any]] = None,
    inkargs: Optional[Dict[str, Any]] = None,
):
    """Set the inputs of the given objects, according to the non named or the
    named parameters in args and kargs

    This function tries to assign all the non named parameters in the input of
    the new_itk_object
    - the first non named parameter in the first input, etc.

    The named parameters are used by calling the method with the same name
    prefixed by 'Set'.
    set_inputs( obj, kargs={'Threshold': 10} ) calls obj.SetThreshold(10)

    This is the function use in the enhanced New() method to manage the inputs.
    It can be used to produce a similar behavior:

    def SetInputs(self, *args, **kargs):
        import itk
        itk.set_inputs(self, *args, **kargs)
    """

    # Fix bug with Mutable Default Arguments
    # https://docs.python-guide.org/writing/gotchas/
    args: List[Any] = inargs if inargs else []
    kargs: Dict[str, Any] = inkargs if inkargs else {}

    # try to get the images from the filters in args
    args = [output(arg) for arg in args]

    # args without name are filter used to set input image
    #
    # count SetInput calls to call SetInput, SetInput2, SetInput3, ...
    # useful with filter which take 2 input (or more) like SubtractImageFiler
    # Ex: subtract image2.png to image1.png and save the result in result.png
    # r1 = itk.ImageFileReader.US2.New(FileName='image1.png')
    # r2 = itk.ImageFileReader.US2.New(FileName='image2.png')
    # s = itk.SubtractImageFilter.US2US2US2.New(r1, r2)
    # itk.ImageFileWriter.US2.New(s, FileName='result.png').Update()
    setInputNb: int = -1
    try:
        for setInputNb, arg in enumerate(args):
            methodName = "SetInput%i" % (setInputNb + 1)
            if methodName in dir(new_itk_object):
                # first try to use methods called SetInput1, SetInput2, ...
                # those method should have more chances to work in case of
                # multiple input types
                getattr(new_itk_object, methodName)(arg)
            else:
                # no method called SetInput?
                # try with the standard SetInput(nb, input)
                new_itk_object.SetInput(setInputNb, arg)
    except TypeError as e:
        # the exception have (at least) to possible reasons:
        # + the filter don't take the input number as first argument
        # + arg is an object of wrong type
        #
        # if it's not the first input, re-raise the exception
        if setInputNb != 0:
            raise e
        # it's the first input, try to use the SetInput() method without input
        # number
        new_itk_object.SetInput(args[0])
        # but raise an exception if there is more than 1 argument
        if len(args) > 1:
            raise TypeError("Object accepts only 1 input.")
    except AttributeError:
        # There is no SetInput() method, try SetImage
        # but before, check the number of inputs
        if len(args) > 1:
            raise TypeError("Object accepts only 1 input.")
        methodList = ["SetImage", "SetInputImage"]
        methodName = None
        for m in methodList:
            if m in dir(new_itk_object):
                methodName = m
        if methodName:
            getattr(new_itk_object, methodName)(args[0])
        else:
            raise AttributeError("No method found to set the input.")

    # named args : name is the function name, value is argument(s)
    for attribName, value in kargs.items():
        # use Set as prefix. It allow to use a shorter and more intuitive
        # call (Ex: itk.ImageFileReader.UC2.New(FileName='image.png')) than
        # with the full name
        # (Ex: itk.ImageFileReader.UC2.New(SetFileName='image.png'))
        if attribName not in ["auto_progress", "template_parameters"]:
            if attribName.islower():
                attribName = _snake_to_camel(attribName)
            attrib = getattr(new_itk_object, "Set" + attribName)

            # Do not use try-except mechanism as this leads to
            # segfaults. Instead limit the number of types that are
            # tested. The list of tested type could maybe be replaced by
            # a test that would check for iterables.
            import itk

            if type(value) in [list, tuple]:
                try:
                    output_value = [itk.output(x) for x in value]
                    attrib(*output_value)
                except Exception:
                    attrib(itk.output(value))
            else:
                attrib(itk.output(value))


class templated_class:

    """This class is used to mimic the behavior of the templated C++ classes.

    It is used this way:

    class CustomClass:
        # class definition here
    CustomClass = templated_class(CustomClass)

    customObject = CustomClass[template, parameters].New()

    The template parameters are passed to the custom class constructor as a
    named parameter 'template_parameters' in a tuple.

    The custom class may implement a static method
    check_template_parameters(parameters) which should raise an exception if
    the template parameters provided are not suitable to instantiate the custom
    class.
    """

    def __init__(self, cls) -> None:
        """cls is the custom class"""
        self.__cls__ = cls
        self.__templates__ = {}

    def New(self, *args, **kargs):
        """Use the parameters to infer the types of the template parameters."""
        # extract the types from the arguments to instantiate the class
        import itk

        types = tuple(class_(o) for o in args)
        return self[types].New(*args, **kargs)

    def __getitem__(self, template_parameters):
        """Return a pair class-template parameters ready to be instantiated.

        The template parameters may be validated if the custom class provide
        the static method check_template_parameters(parameters).
        """
        if not isinstance(template_parameters, tuple):
            template_parameters = (template_parameters,)
        return templated_class.__templated_class_and_parameters__(
            self, template_parameters
        )

    def check_template_parameters(self, template_parameters) -> None:
        """Check the template parameters passed in parameter."""
        # this method is there mainly to make possible to reuse it in the
        # custom class constructor after having used templated_class().
        # Without that, the following example doesn't work:
        #
        # class CustomClass:
        #     def __init__(self, *args, **kargs):
        #         template_parameters = kargs["template_parameters"]
        #         CustomClass.check_template_parameters(template_parameters)
        # other init stuff
        #     def check_template_parameters(template_parameters):
        # check, really
        #         pass
        #    CustomClass = templated_class(CustomClass)
        #
        self.__cls__.check_template_parameters(template_parameters)

    def add_template(self, name: str, params):
        if not isinstance(params, list) and not isinstance(params, tuple):
            params = (params,)
        params = tuple(params)
        val = self[params]
        self.__templates__[params] = val
        setattr(self, name, val)

    def add_image_templates(self, *args) -> None:
        import itk

        if not args:
            return
        combinations = [[t] for t in args[0]]
        for types in args[1:]:
            temp = []
            for t in types:
                for c in combinations:
                    temp.append(c + [t])
            combinations = temp
        for d in itk.DIMS:
            for c in combinations:
                parameters = []
                name = ""
                for t in c:
                    parameters.append(itk.Image[t, d])
                    name += "I" + t.short_name + str(d)
                self.add_template(name, tuple(parameters))

    class __templated_class_and_parameters__:

        """Inner class used to store the pair class-template parameters ready
        to instantiate.
        """

        def __init__(self, l_templated_class, l_template_parameters) -> None:
            self.__templated_class__ = l_templated_class
            self.__template_parameters__ = l_template_parameters
            if "check_template_parameters" in dir(l_templated_class.__cls__):
                l_templated_class.__cls__.check_template_parameters(
                    l_template_parameters
                )

        def New(self, *args, **kargs):
            """A New() method to mimic the ITK default behavior, even if the
            class doesn't provide any New() method.
            """
            kargs["template_parameters"] = self.__template_parameters__
            if "New" in dir(self.__templated_class__.__cls__):
                obj = self.__templated_class__.__cls__.New(*args, **kargs)
            else:
                obj = self.__templated_class__.__cls__(*args, **kargs)
            setattr(obj, "__template_parameters__", self.__template_parameters__)
            setattr(obj, "__templated_class__", self.__templated_class__)
            return obj

        def __call__(self, *args, **kargs):
            return self.New(*args, **kargs)

    def keys(self):
        return self.__templates__.keys()

    def values(self):
        return list(self.__templates__.values())

    def items(self):
        return list(self.__templates__.items())

    # everything after this comment is for dict interface
    # and is a copy/paste from DictMixin
    # only methods to edit dictionary are not there
    def __iter__(self) -> str:
        yield from self.keys()

    def has_key(self, key: str):
        return key in self.__templates__

    def __contains__(self, key: str):
        return key in self

    def get(self, key: str, default: Optional[str] = None) -> Optional[str]:
        return self.get(key, default)

    def __len__(self):
        return len(self.keys())


class pipeline:

    """A convenient class to store the reference to the filters of a pipeline

    With this class, a method can create a pipeline of several filters and
    return it without losing the references to the filters in this pipeline.
    The pipeline object act almost like a filter (it has a GetOutput() method)
    and thus can be simply integrated in another pipeline.
    """

    def __init__(self, *args, **kargs) -> None:
        self.clear()
        self.input = None
        self.filters: List[Any] = []
        set_inputs(self, args, kargs)

    def connect(self, l_filter) -> None:
        """Connect a new l_filter to the pipeline

        The output of the first l_filter will be used as the input of this
        one and the l_filter passed as parameter will be added to the list
        """
        if self.GetOutput() is not None:
            set_inputs(l_filter, [self.GetOutput()])
        self.append(l_filter)

    def append(self, l_filter) -> None:
        """Add a new l_filter to the pipeline

        The new l_filter will not be connected. The user must connect it.
        """
        self.filters.append(l_filter)

    def clear(self) -> None:
        """Clear the filter list"""
        self.filters = []

    def GetOutput(self, l_index: int = 0):
        """Return the output of the pipeline

        If another output is needed, use
        pipeline.filters[-1].GetAnotherOutput() instead of this method,
        subclass pipeline to implement another GetOutput() method, or use
        expose()
        """
        if len(self.filters) == 0:
            return self.GetInput()
        else:
            l_filter = self.filters[-1]
            if hasattr(l_filter, "__getitem__"):
                return l_filter[l_index]
            try:
                return l_filter.GetOutput(l_index)
            except Exception:
                if l_index == 0:
                    return l_filter.GetOutput()
                else:
                    raise ValueError("Index can only be 0 on that object")

    def GetNumberOfOutputs(self) -> int:
        """Return the number of outputs"""
        if len(self.filters) == 0:
            return 1
        else:
            return self.filters[-1].GetNumberOfOutputs()

    def SetInput(self, l_input) -> None:
        """Set the l_input of the pipeline"""
        if len(self.filters) != 0:
            set_inputs(self.filters[0], [l_input])
        self.l_input = l_input

    def GetInput(self):
        """Get the input of the pipeline"""
        return self.input

    def Update(self):
        """Update the pipeline"""
        if len(self.filters) > 0:
            return self.filters[-1].Update()

    def UpdateLargestPossibleRegion(self):
        """Update the pipeline"""
        if len(self.filters) > 0:
            return self.filters[-1].UpdateLargestPossibleRegion()

    def UpdateOutputInformation(self) -> None:
        if "UpdateOutputInformation" in dir(self.filters[-1]):
            self.filters[-1].UpdateOutputInformation()
        else:
            self.Update()

    def __len__(self):
        return self.GetNumberOfOutputs()

    def __getitem__(self, item):
        return self.GetOutput(item)

    def __call__(self, *args, **kargs):
        set_inputs(self, args, kargs)
        self.UpdateLargestPossibleRegion()
        return self

    def expose(self, name: str, new_name: Optional[str] = None, position: int = -1):
        """Expose an attribute from a filter of the mini-pipeline.

        Once called, the pipeline instance has a new Set/Get set of methods to
        access directly the corresponding method of one of the filter of the
        pipeline.
        Ex: p.expose( "Radius" )
                p.SetRadius( 5 )
                p.GetRadius( 5 )
        By default, the attribute usable on the pipeline instance has the same
        name than the one of the filter, but it can be changed by providing a
        value to new_name.
        The last filter of the pipeline is used by default, but another one may
        be used by giving its position.
        Ex: p.expose("Radius", "SmoothingNeighborhood", 2)
            p.GetSmoothingNeighborhood()
        """
        if new_name is None:
            new_name = name
        src = self.filters[position]
        ok: bool = False
        set_name: str = "Set" + name
        if set_name in dir(src):
            setattr(self, "Set" + new_name, getattr(src, set_name))
            ok = True
        get_name = "Get" + name
        if get_name in dir(src):
            setattr(self, "Get" + new_name, getattr(src, get_name))
            ok = True
        if not ok:
            raise RuntimeError(f"No attribute {name} at position {position}.")


class auto_pipeline(pipeline):
    current = None

    def __init__(self, *args, **kargs) -> None:
        pipeline.__init__(self, *args, **kargs)
        self.Start()

    def Start(self) -> None:
        auto_pipeline.current = self

    @staticmethod
    def Stop() -> None:
        auto_pipeline.current = None


def down_cast(obj: "itkt.LightObject"):
    """Down cast an itk.LightObject (or a object of a subclass) to its most
    specialized type.
    """
    import itk
    from itk.support.template_class import itkTemplate

    class_name: str = obj.GetNameOfClass()
    t = getattr(itk, class_name)
    if isinstance(t, itkTemplate):
        for c in t.values():
            try:
                return c.cast(obj)
            except Exception:
                # fail silently for now
                pass
        raise RuntimeError(f"Can't downcast to a specialization of {class_name}")
    else:
        return t.cast(obj)


def attribute_list(inputobject, name: str):
    """Returns a list of the specified attributes for the objects in the image.

    i: the input LabelImage
    name: the attribute name
    """
    import itk

    img = itk.output(inputobject)
    relabel = itk.StatisticsRelabelLabelMapFilter[img].New(
        img, Attribute=name, ReverseOrdering=True, InPlace=False
    )
    relabel.UpdateLargestPossibleRegion()
    r = relabel.GetOutput()
    l_list: List[Any] = []
    # required because range is overloaded in this module
    import sys
    from builtins import range

    for i in range(1, r.GetNumberOfLabelObjects() + 1):
        l_list.append(r.GetLabelObject(i).__getattribute__("Get" + name)())
    return l_list


def attributes_list(inputObject, names: List[str]):
    """Returns a list of the specified attributes for the objects in the image.

    i: the input LabelImage
    name: the attribute name
    """
    import itk

    img = itk.output(inputObject)
    relabel = itk.StatisticsRelabelLabelMapFilter[img].New(
        img, Attribute=names[0], ReverseOrdering=True, InPlace=False
    )
    relabel.UpdateLargestPossibleRegion()
    r = relabel.GetOutput()
    l_list: List[Any] = []
    # required because range is overloaded in this module
    from builtins import range

    for i in range(1, r.GetNumberOfLabelObjects() + 1):
        attrs = []
        for name in names:
            attrs.append(r.GetLabelObject(i).__getattribute__("Get" + name)())
        l_list.append(tuple(attrs))
    return l_list


def attribute_dict(inputobject, name: str):
    """Returns a dict with the attribute values in keys and a list of the
    corresponding objects in value

    i: the input LabelImage
    name: the name of the attribute
    """
    import itk

    img = itk.output(inputobject)
    relabel = itk.StatisticsRelabelLabelMapFilter[img].New(
        img, Attribute=name, ReverseOrdering=True, InPlace=False
    )
    relabel.UpdateLargestPossibleRegion()
    r = relabel.GetOutput()
    d = {}
    # required because range is overloaded in this module
    from builtins import range

    for i in range(1, r.GetNumberOfLabelObjects() + 1):
        lo = r.GetLabelObject(i)
        v = lo.__getattribute__("Get" + name)()
        l_list = d.get(v, [])
        l_list.append(lo)
        d[v] = l_list
    return d


def number_of_objects(image_or_filter) -> int:
    """Returns the number of objects in the image.

    img: the input LabelImage
    """
    import itk

    image_or_filter.UpdateLargestPossibleRegion()
    img = itk.output(image_or_filter)
    return img.GetNumberOfLabelObjects()


def ipython_kw_matches(text: str):
    """Match named ITK object's named parameters"""
    import IPython
    import itk
    import re
    import inspect
    from itk.support import template_class

    regexp = re.compile(
        r"""
                    '.*?' |  # single quoted strings or
                    ".*?" |  # double quoted strings or
                    \w+     |  # identifier
                    \S  # other characters
                    """,
        re.VERBOSE | re.DOTALL,
    )
    ip = IPython.get_ipython()
    if "." in text:  # a parameter cannot be dotted
        return []
    # 1. Find the nearest identifier that comes before an unclosed
    # parenthesis e.g. for "foo (1+bar(x), pa", the candidate is "foo".
    if ip.Completer.readline:
        text_until_cursor = ip.Completer.readline.get_line_buffer()[
            : ip.Completer.readline.get_endidx()
        ]
    else:
        # IPython >= 5.0.0, which is based on the Python Prompt Toolkit
        text_until_cursor = ip.Completer.text_until_cursor

    tokens = regexp.findall(text_until_cursor)
    tokens.reverse()
    iter_tokens = iter(tokens)
    open_par = 0
    for token in iter_tokens:
        if token == ")":
            open_par -= 1
        elif token == "(":
            open_par += 1
            if open_par > 0:
                # found the last unclosed parenthesis
                break
    else:
        return []
    # 2. Concatenate dotted names ("foo.bar" for "foo.bar(x, pa" )
    ids = []
    is_id = re.compile(r"\w+$").match
    while True:
        try:
            ids.append(iter_tokens.next())
            if not is_id(ids[-1]):
                ids.pop()
                break
            if not iter_tokens.next() == ".":
                break
        except StopIteration:
            break
    # lookup the candidate callable matches either using global_matches
    # or attr_matches for dotted names
    if len(ids) == 1:
        callable_matches = ip.Completer.global_matches(ids[0])
    else:
        callable_matches = ip.Completer.attr_matches(".".join(ids[::-1]))
    arg_matches = []
    for callable_match in callable_matches:
        # drop the .New at this end, so we can search in the class members
        if callable_match.endswith(".New"):
            callable_match = callable_match[:-4]
        elif not re.findall("([A-Z])", callable_match):  # True if snake case
            # Split at the last '.' occurrence
            split_name_parts = callable_match.split(".")
            namespace = split_name_parts[:-1]
            function_name = split_name_parts[-1]
            # Find corresponding object name
            object_name = _snake_to_camel(function_name)
            # Check that this object actually exists
            try:
                object_callable_match = ".".join(namespace + [object_name])
                eval(object_callable_match, ip.Completer.namespace)
                # Reconstruct full object name
                callable_match = object_callable_match
            except AttributeError:
                # callable_match is not a snake case function with a
                # corresponding object.
                pass
        try:
            l_object = eval(callable_match, ip.Completer.namespace)
            if isinstance(l_object, template_class.itkTemplate):
                # this is a template - lets grab the first entry to search for
                # the methods
                l_object = l_object.values()[0]
            named_args = []
            is_in: bool = isinstance(l_object, itk.LightObject)
            if is_in or (
                inspect.isclass(l_object) and issubclass(l_object, itk.LightObject)
            ):
                named_args = [n[3:] for n in dir(l_object) if n.startswith("Set")]
        except Exception as e:
            print(e)
            continue
        for namedArg in named_args:
            if namedArg.startswith(text):
                arg_matches.append(f"{namedArg}=")
    return arg_matches


def template(cl):
    """Return the template of a class (or of the class of an object) and
    its parameters

    template() returns a tuple with 2 elements:
        - the first one is the itkTemplate object
        - the second is a tuple containing the template parameters
    """
    from itk.support.template_class import itkTemplateBase

    return itkTemplateBase.__template_instantiations_object_to_name__[class_(cl)]


def ctype(s: str) -> "itkt.itkCType":
    """Return the c type corresponding to the string passed in parameter

    The string can contain some extra spaces.
    see also itkCType
    """
    from itk.support.types import itkCType

    ret = itkCType.GetCType(" ".join(s.split()))
    if ret is None:
        raise KeyError(f"Unrecognized C type '{s}'")
    return ret


def class_(obj):
    """Return a class from an object

    Often in itk, the __class__ is not what the user is expecting.
    class_() should do a better job
    """
    import inspect

    if inspect.isclass(obj):
        # obj is already a class !
        return obj
    else:
        return obj.__class__


def python_type(object_ref) -> str:
    """Returns the Python type name of an object

    The Python name corresponding to the given instantiated object is printed.
    This includes both the Python name and the parameters of the object. A user
    can copy and paste the printed value to instantiate a new object of the
    same type."""
    from itk.support.template_class import itkTemplate
    from itk.support.types import itkCType

    def in_itk(name):
        import itk

        # Remove "itk::" and "std::" from template name.
        # Only happens for ITK objects.
        shortname: str = name.split("::")[-1]
        shortname = shortname.split("itk")[-1]

        namespace = itk
        # A type cannot be part of ITK if its name was not modified above. This
        # check avoids having an input of type `list` and return `itk.list` that
        # also exists.
        likely_itk: bool = shortname != name or name[:3] == "vnl"
        if likely_itk and hasattr(namespace, shortname):
            return namespace.__name__ + "." + shortname  # Prepend name with 'itk.'
        else:
            return name

    def recursive(l_obj, level: int):

        try:
            type_name, param_list = template(l_obj)
            name = in_itk(type_name.__name__)
            parameters = []
            for t in param_list:
                parameters.append(recursive(t, level + 1))
            return name + "[" + ",".join(parameters) + "]"
        except KeyError:
            if isinstance(l_obj, itkCType):  # Handles CTypes differently
                return "itk." + l_obj.short_name
            elif hasattr(l_obj, "__name__"):
                # This should be where most ITK types end up.
                return in_itk(l_obj.__name__)
            elif (
                not isinstance(l_obj, type)
                and type(l_obj) != itkTemplate
                and level != 0
            ):
                # l_obj should actually be considered a value, not a type,
                # or it is already an itkTemplate type.
                # A value can be an integer that is a template parameter.
                # This does not happen at the first level of the recursion
                # as it is not possible that this object would be a template
                # parameter. Checking the level `0` allows e.g. to find the
                # type of an object that is a `list` or an `int`.
                return str(l_obj)
            else:
                return in_itk(type(l_obj).__name__)

    return recursive(object_ref, 0)


class TemplateTypeError(TypeError):
    def __init__(self, template_type, input_type):
        def tuple_to_string_type(t):
            if type(t) == tuple:
                return ", ".join(python_type(x) for x in t)
            else:
                python_type(t)

        import itk

        # Special case for ITK readers: Add extra information.
        extra_eg: str = ""
        if template_type in [
            itk.ImageFileReader,
            itk.ImageSeriesReader,
            itk.MeshFileReader,
        ]:
            extra_eg = """

or

    e.g.: image = itk.imread(my_input_filename, itk.F)
"""

        python_template_type = python_type(template_type)
        python_input_type = tuple_to_string_type(input_type)
        type_list = "\n".join([python_type(x[0]) for x in template_type.keys()])
        eg_type = ", ".join([python_type(x) for x in list(template_type.keys())[0]])
        msg: str = """{template_type} is not wrapped for input type `{input_type}`.

To limit the size of the package, only a limited number of
types are available in ITK Python. To print the supported
types, run the following command in your python environment:

    {template_type}.GetTypes()

Possible solutions:
* If you are an application user:
** Convert your input image into a supported format (see below).
** Contact developer to report the issue.
* If you are an application developer, force input images to be
loaded in a supported pixel type.

    e.g.: instance = {template_type}[{eg_type}].New(my_input){extra_eg}

* (Advanced) If you are an application developer, build ITK Python yourself and
turned to `ON` the corresponding CMake option to wrap the pixel type or image
dimension you need. When configuring ITK with CMake, you can set
`ITK_WRAP_${{type}}` (replace ${{type}} with appropriate pixel type such as
`double`). If you need to support images with 4 or 5 dimensions, you can add
these dimensions to the list of dimensions in the CMake variable
`ITK_WRAP_IMAGE_DIMS`.

Supported input types:

{type_list}
""".format(
            template_type=python_template_type,
            input_type=python_input_type,
            type_list=type_list,
            eg_type=eg_type,
            extra_eg=extra_eg,
        )
        TypeError.__init__(self, msg)


# install progress callback and custom completer if we are in ipython
# interpreter
try:
    import itkConfig
    import IPython

    if IPython.get_ipython():
        IPython.get_ipython().Completer.matchers.insert(0, ipython_kw_matches)
    # some cleanup
    del itkConfig, IPython
except (ImportError, AttributeError):
    # fail silently
    pass
