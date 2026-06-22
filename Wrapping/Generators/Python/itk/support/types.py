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

import importlib
from importlib.metadata import metadata
from typing import TypeAlias, Union, TYPE_CHECKING
import os

try:
    from numpy.typing import ArrayLike
except ImportError:
    from numpy import ndarray as ArrayLike

_HAVE_XARRAY = False
try:
    metadata("xarray")

    _HAVE_XARRAY = True
except importlib.metadata.PackageNotFoundError:
    pass
_HAVE_TORCH = False
try:
    metadata("torch")

    _HAVE_TORCH = True
except importlib.metadata.PackageNotFoundError:
    pass


# noinspection PyPep8Naming
class itkCType:
    # import locally to facilitate dynamic loading in itk/__init__.py
    import numpy as np

    __c_types__: dict[str, "itkCType"] = {}
    __c_types_for_dtype__: dict[str, np.dtype] = {}

    def __init__(
        self, name: str, short_name: str, np_dtype: np.dtype | None = None
    ) -> None:
        # Remove potential white space around type names
        self.name = name.strip()
        self.short_name = short_name.strip()
        self.dtype = np_dtype

        itkCType.__c_types__[self.name] = self
        if np_dtype:
            itkCType.__c_types_for_dtype__[np_dtype] = self

    def __del__(self) -> None:
        try:
            del itkCType.__c_types__[self.name]
        except KeyError:
            pass

    def __repr__(self) -> str:
        return f"<itkCType {self.name}>"

    @staticmethod
    def GetCType(name: str) -> "itkCType | None":
        # import locally to facilitate dynamic loading in itk/__init__.py

        """Get the type corresponding to the provided C primitive type name."""
        aliases: dict[str, str] = {
            "short": "signed short",
            "int": "signed int",
            "long": "signed long",
            "long long": "signed long long",
        }
        # Remove potential white space around types
        name = name.strip()
        desired_name: str = aliases.get(name, name)
        try:
            return itkCType.__c_types__[desired_name]
        except KeyError:
            return None

    @staticmethod
    def GetCTypeForDType(np_dtype: np.dtype) -> "itkCType | None":
        """Get the type corresponding to the provided numpy.dtype."""
        try:
            return itkCType.__c_types_for_dtype__[np_dtype]
        except KeyError:
            return None

    @staticmethod
    def initialize_c_types_once() -> "tuple[itkCType, ...]":
        """
        This function is intended to be run only one time
        """
        import numpy as np

        _F: "itkCType" = itkCType("float", "F", np.dtype(np.float32))
        _D: "itkCType" = itkCType("double", "D", np.dtype(np.float64))
        _UC: "itkCType" = itkCType("unsigned char", "UC", np.dtype(np.uint8))
        _US: "itkCType" = itkCType("unsigned short", "US", np.dtype(np.uint16))
        _UI: "itkCType" = itkCType("unsigned int", "UI", np.dtype(np.uint32))
        if os.name == "nt":
            _UL: "itkCType" = itkCType("unsigned long", "UL", np.dtype(np.uint32))
            _SL: "itkCType" = itkCType("signed long", "SL", np.dtype(np.int32))
        else:
            _UL: "itkCType" = itkCType("unsigned long", "UL", np.dtype(np.uint64))
            _SL: "itkCType" = itkCType("signed long", "SL", np.dtype(np.int64))
        _ULL: "itkCType" = itkCType("unsigned long long", "ULL", np.dtype(np.uint64))
        _SC: "itkCType" = itkCType("signed char", "SC", np.dtype(np.int8))
        _SS: "itkCType" = itkCType("signed short", "SS", np.dtype(np.int16))
        _SI: "itkCType" = itkCType("signed int", "SI", np.dtype(np.int32))
        _SLL: "itkCType" = itkCType("signed long long", "SLL", np.dtype(np.int64))
        _B: "itkCType" = itkCType("bool", "B", np.dtype(np.bool_))
        return _F, _D, _UC, _US, _UI, _UL, _SL, _ULL, _SC, _SS, _SI, _SLL, _B


# Define typing hints
F: itkCType
D: itkCType
UC: itkCType
US: itkCType
UI: itkCType
UL: itkCType
SL: itkCType
ULL: itkCType
SC: itkCType
SS: itkCType
SI: itkCType
SLL: itkCType
B: itkCType

(
    F,
    D,
    UC,
    US,
    UI,
    UL,
    SL,
    ULL,
    SC,
    SS,
    SI,
    SLL,
    B,
) = itkCType.initialize_c_types_once()

# Aliases for SizeValueType, IdentifierType, OffsetType
ST = UL
IT = UL
OT = SL
if os.name == "nt":
    ST = ULL
    IT = ULL
    OT = SLL

# Type aliases to avoid expensive import, circular references. Use with forward references.
if TYPE_CHECKING:
    import itk

# Marked as TypeAlias per PEP 613 so type checkers resolve the string
# forward references instead of rejecting them as plain variables.
ImageBase: TypeAlias = "itk.ImageBase"
Image: TypeAlias = "itk.Image"
VectorImage: TypeAlias = "itk.VectorImage"

RGBPixel: TypeAlias = "itk.RGBPixel"
Vector: TypeAlias = "itk.Vector"
CovariantVector: TypeAlias = "itk.CovariantVector"
SymmetricSecondRankTensor: TypeAlias = "itk.SymmetricSecondRankTensor"
Offset: TypeAlias = "itk.Offset"
FixedArray: TypeAlias = "itk.FixedArray"
VariableLengthVector: TypeAlias = "itk.VariableLengthVector"
complex_: TypeAlias = "itk.complex"

PixelTypes = Union[
    itkCType,
    RGBPixel,
    Vector,
    CovariantVector,
    SymmetricSecondRankTensor,
    Offset,
    FixedArray,
    complex_,
]

ImageSource: TypeAlias = "itk.ImageSource"

ImageOrImageSource = Union[ImageBase, ImageSource]
# Can be coerced into an itk.ImageBase
if _HAVE_XARRAY and _HAVE_TORCH:
    ImageLike = Union[ImageBase, ArrayLike, "xr.DataArray", "torch.Tensor"]
elif _HAVE_XARRAY:
    ImageLike = Union[ImageBase, ArrayLike, "xr.DataArray"]
elif _HAVE_TORCH:
    ImageLike = Union[ImageBase, ArrayLike, "torch.Tensor"]
else:
    ImageLike = Union[ImageBase, ArrayLike]

ImageIOBase: TypeAlias = "itk.ImageIOBase"

LightObject: TypeAlias = "itk.LightObject"
Object: TypeAlias = "itk.Object"
DataObject: TypeAlias = "itk.DataObject"

PointSet: TypeAlias = "itk.PointSet"
Mesh: TypeAlias = "itk.Mesh"
QuadEdgeMesh: TypeAlias = "itk.QuadEdgeMesh"

Path: TypeAlias = "itk.Path"
ParametricPath: TypeAlias = "itk.ParametricPath"
PolyLineParametricPath: TypeAlias = "itk.PolyLineParametricPath"
SpatialObject: TypeAlias = "itk.SpatialObject"

TransformBase: TypeAlias = "itk.TransformBaseTemplate"
Transform: TypeAlias = "itk.Transform"

ImageRegion: TypeAlias = "itk.ImageRegion"

VectorContainer: TypeAlias = "itk.VectorContainer"
Matrix: TypeAlias = "itk.Matrix"

# Return types for the functional interfaces to ProcessObject's.
# When there is a single indexed output, it is returned directly.
# When there are multiple indexed outputs, a tuple of the indexed outputs is
# returned.
ImageSourceReturn = Union[ImageLike, tuple[ImageLike, ...]]
MeshSourceReturn = Union[Mesh, tuple[Mesh, ...]]
PathSourceReturn = Union[Path, tuple[Path, ...]]

InterpolateImageFunction: TypeAlias = "itk.InterpolateImageFunction"
ExtrapolateImageFunction: TypeAlias = "itk.ExtrapolateImageFunction"
ImageBoundaryCondition: TypeAlias = "itk.ImageBoundaryCondition"
FlatStructuringElement: TypeAlias = "itk.FlatStructuringElement"
