import itk

# Finding the named elements in the templates
# requires that they are force loaded before
# before searching
itk.force_load()

from itk.support import types
from itk.support.template_class import itkTemplate, itkTemplateBase

from typing import List, Union
from itkConfig import ITK_GLOBAL_WRAPPING_BUILD_OPTIONS as _itkwrapbo

DIMS: List[int] = [int(s) for s in _itkwrapbo["ITK_WRAP_IMAGE_DIMS"] if s]
USIGN_INTS: List[types.itkCType] = [
    getattr(types, s) for s in _itkwrapbo["WRAP_ITK_USIGN_INT"] if s
]
SIGN_INTS: List[types.itkCType] = [
    getattr(types, s) for s in _itkwrapbo["WRAP_ITK_SIGN_INT"] if s
]
REALS: List[types.itkCType] = [
    getattr(types, s) for s in _itkwrapbo["WRAP_ITK_REAL"] if s
]

VECTOR_REALS: List[itkTemplate] = [
    itkTemplateBase.__template_instantiations_name_to_object__[
        itkTemplate.normalizeName(s)
    ]
    for s in _itkwrapbo["ITK_WRAP_PYTHON_VECTOR_REAL"]
    if s
]
COV_VECTOR_REALS: List[itkTemplate] = [
    itkTemplateBase.__template_instantiations_name_to_object__[
        itkTemplate.normalizeName(s)
    ]
    for s in _itkwrapbo["ITK_WRAP_PYTHON_COV_VECTOR_REAL"]
    if s
]
RGBS: List[itkTemplate] = [
    itkTemplateBase.__template_instantiations_name_to_object__[
        itkTemplate.normalizeName(s)
    ]
    for s in _itkwrapbo["ITK_WRAP_PYTHON_RGB"]
    if s
]
RGBAS: List[itkTemplate] = [
    itkTemplateBase.__template_instantiations_name_to_object__[
        itkTemplate.normalizeName(s)
    ]
    for s in _itkwrapbo["ITK_WRAP_PYTHON_RGBA"]
    if s
]
COMPLEX_REALS: List[itkTemplate] = [
    itkTemplateBase.__template_instantiations_name_to_object__[
        itkTemplate.normalizeName(s)
    ]
    for s in _itkwrapbo["ITK_WRAP_PYTHON_COMPLEX_REAL"]
    if s
]

INTS: List[types.itkCType] = SIGN_INTS + USIGN_INTS
SCALARS: List[types.itkCType] = INTS + REALS
VECTORS: List[itkTemplate] = VECTOR_REALS + COV_VECTOR_REALS
COLORS: List[itkTemplate] = RGBS + RGBAS
ALL_TYPES: List[Union[types.itkCType, itkTemplate]] = (
    COLORS + VECTORS + SCALARS + COMPLEX_REALS
)

del itkTemplate
del types
