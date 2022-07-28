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

from typing import Union, Optional, Tuple, TYPE_CHECKING
import os

try:
    from numpy.typing import ArrayLike
except ImportError:
    from numpy import ndarray as ArrayLike

_HAVE_XARRAY = False
try:
    import xarray as xr

    _HAVE_XARRAY = True
except ImportError:
    pass
_HAVE_TORCH = False
try:
    import torch

    _HAVE_TORCH = True
except ImportError:
    pass

# noinspection PyPep8Naming
class itkCType:
    # import locally to facilitate dynamic loading in itk/__init__.py
    import numpy as np
    from typing import Any, Dict, Optional, Tuple

    __c_types__: Dict[str, "itkCType"] = {}
    __c_types_for_dtype__: Dict[str, np.dtype] = {}

    def __init__(self, name: str, short_name: str, np_dtype: np.dtype = None) -> None:
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
    def GetCType(name: str) -> Optional["itkCType"]:
        # import locally to facilitate dynamic loading in itk/__init__.py
        from typing import Dict

        """Get the type corresponding to the provided C primitive type name."""
        aliases: Dict[str, str] = {
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
    def GetCTypeForDType(np_dtype: np.dtype) -> Optional["itkCType"]:
        """Get the type corresponding to the provided numpy.dtype."""
        try:
            return itkCType.__c_types_for_dtype__[np_dtype]
        except KeyError:
            return None

    @staticmethod
    def initialize_c_types_once() -> (
        "itkCType",
        "itkCType",
        "itkCType",
        "itkCType",
        "itkCType",
        "itkCType",
        "itkCType",
        "itkCType",
        "itkCType",
        "itkCType",
        "itkCType",
        "itkCType",
        "itkCType",
        "itkCType",
    ):
        """
        This function is intended to be run only one time
        """
        import numpy as np

        _F: "itkCType" = itkCType("float", "F", np.float32)
        _D: "itkCType" = itkCType("double", "D", np.float64)
        _UC: "itkCType" = itkCType("unsigned char", "UC", np.uint8)
        _US: "itkCType" = itkCType("unsigned short", "US", np.uint16)
        _UI: "itkCType" = itkCType("unsigned int", "UI", np.uint32)
        if os.name == "nt":
            _UL: "itkCType" = itkCType("unsigned long", "UL", np.uint32)
            _SL: "itkCType" = itkCType("signed long", "SL", np.int32)
            _LD: "itkCType" = itkCType("long double", "LD")
        else:
            _UL: "itkCType" = itkCType("unsigned long", "UL", np.uint64)
            _SL: "itkCType" = itkCType("signed long", "SL", np.int64)
            if hasattr(np, "float128"):
                _LD: "itkCType" = itkCType("long double", "LD", np.float128)
            else:
                _LD: "itkCType" = itkCType("long double", "LD")
        _ULL: "itkCType" = itkCType("unsigned long long", "ULL", np.uint64)
        _SC: "itkCType" = itkCType("signed char", "SC", np.int8)
        _SS: "itkCType" = itkCType("signed short", "SS", np.int16)
        _SI: "itkCType" = itkCType("signed int", "SI", np.int32)
        _SLL: "itkCType" = itkCType("signed long long", "SLL", np.int64)
        _B: "itkCType" = itkCType("bool", "B", np.bool8)
        return _F, _D, _UC, _US, _UI, _UL, _SL, _LD, _ULL, _SC, _SS, _SI, _SLL, _B


# Define typing hints
F: itkCType
D: itkCType
UC: itkCType
US: itkCType
UI: itkCType
UL: itkCType
SL: itkCType
LD: itkCType
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
    LD,
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

# These should eventually explicitly be marked as TypeAlias's per PEP 613, available in CPython 3.10
ImageBase = "itk.ImageBase"
Image = "itk.Image"
VectorImage = "itk.VectorImage"

RGBPixel = "itk.RGBPixel"
Vector = "itk.Vector"
CovariantVector = "itk.CovariantVector"
SymmetricSecondRankTensor = "itk.SymmetricSecondRankTensor"
Offset = "itk.Offset"
FixedArray = "itk.FixedArray"
VariableLengthVector = "itk.VariableLengthVector"
complex_ = "itk.complex"

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

ImageSource = "itk.ImageSource"

ImageOrImageSource = Union[ImageBase, ImageSource]
# Can be coerced into an itk.ImageBase
if _HAVE_XARRAY and _HAVE_TORCH:
    ImageLike = Union[ImageBase, ArrayLike, xr.DataArray, torch.Tensor]
elif _HAVE_XARRAY:
    ImageLike = Union[ImageBase, ArrayLike, xr.DataArray]
elif _HAVE_TORCH:
    ImageLike = Union[ImageBase, ArrayLike, torch.Tensor]
else:
    ImageLike = Union[ImageBase, ArrayLike]

ImageIOBase = "itk.ImageIOBase"

LightObject = "itk.LightObject"
Object = "itk.Object"
DataObject = "itk.DataObject"

PointSet = "itk.PointSet"
Mesh = "itk.Mesh"
QuadEdgeMesh = "itk.QuadEdgeMesh"

Path = "itk.Path"
ParametricPath = "itk.ParametricPath"
PolyLineParametricPath = "itk.PolyLineParametricPath"
SpatialObject = "itk.SpatialObject"

TransformBase = "itk.TransformBaseTemplate"
Transform = "itk.Transform"

ImageRegion = "itk.ImageRegion"

VectorContainer = "itk.VectorContainer"
Matrix = "itk.Matrix"

# Return types for the functional interfaces to ProcessObject's.
# When there is a single indexed output, it is returned directly.
# When there are multiple indexed outputs, a tuple of the indexed outputs is
# returned.
ImageSourceReturn = Union[ImageLike, Tuple[ImageLike, ...]]
MeshSourceReturn = Union[Mesh, Tuple[Mesh, ...]]
PathSourceReturn = Union[Path, Tuple[Path, ...]]

InterpolateImageFunction = "itk.InterpolateImageFunction"
ExtrapolateImageFunction = "itk.ExtrapolateImageFunction"
ImageBoundaryCondition = "itk.ImageBoundaryCondition"
FlatStructuringElement = "itk.FlatStructuringElement"
