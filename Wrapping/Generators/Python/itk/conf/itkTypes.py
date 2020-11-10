# ==========================================================================
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
# ==========================================================================*/

# noinspection PyPep8Naming
class itkCType:
    # import locally to facilitate dynamic loading in itk/__init__.py
    from typing import Dict, Any

    __c_types__: Dict[str, Any] = {}
    __c_types_for_dtype__: Dict[str, Any] = {}

    def __init__(self, name: str, short_name: str, np_dtype=None):
        # Remove potential white space around type names
        self.name = name.strip()
        self.short_name = short_name.strip()
        self.dtype = np_dtype

        itkCType.__c_types__[self.name] = self
        if np_dtype:
            itkCType.__c_types_for_dtype__[np_dtype] = self

    def __del__(self):
        try:
            del itkCType.__c_types__[self.name]
        except KeyError:
            pass

    def __repr__(self):
        return "<itkCType %s>" % self.name

    @staticmethod
    def GetCType(name):
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
    def GetCTypeForDType(np_dtype):
        """Get the type corresponding to the provided numpy.dtype."""
        try:
            return itkCType.__c_types_for_dtype__[np_dtype]
        except KeyError:
            return None

    @staticmethod
    def initialize_c_types_once():
        """
        This function is intended to be run only one time
        """
        import os
        import numpy as np

        _F = itkCType("float", "F", np.float32)
        _D = itkCType("double", "D", np.float64)
        _UC = itkCType("unsigned char", "UC", np.uint8)
        _US = itkCType("unsigned short", "US", np.uint16)
        _UI = itkCType("unsigned int", "UI", np.uint32)
        if os.name == "nt":
            _UL = itkCType("unsigned long", "UL", np.uint32)
            _SL = itkCType("signed long", "SL", np.int32)
            _LD = itkCType("long double", "LD")
        else:
            _UL = itkCType("unsigned long", "UL", np.uint64)
            _SL = itkCType("signed long", "SL", np.int64)
            _LD = itkCType("long double", "LD", np.float128)
        _ULL = itkCType("unsigned long long", "ULL", np.uint64)
        _SC = itkCType("signed char", "SC", np.int8)
        _SS = itkCType("signed short", "SS", np.int16)
        _SI = itkCType("signed int", "SI", np.int32)
        _SLL = itkCType("signed long long", "SLL", np.int64)
        _B = itkCType("bool", "B", np.bool)
        return _F, _D, _UC, _US, _UI, _UL, _SL, _LD, _ULL, _SC, _SS, _SI, _SLL, _B


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
