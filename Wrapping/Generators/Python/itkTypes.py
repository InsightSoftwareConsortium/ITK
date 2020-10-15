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

import os
import numpy as np
from typing import Dict


class itkCType:
    __c_types__ = {}
    __c_types_for_dtype__ = {}

    def __init__(self, name, short_name, np_dtype=None):
        self.name = name
        self.short_name = short_name
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
        """Get the type corresponding to the provided C primitive type name."""
        aliases: Dict[str, str] = {
            "short": "signed short",
            "int": "signed int",
            "long": "signed long",
            "long long": "signed long long",
        }
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


F = itkCType("float", "F", np.float32)
D = itkCType("double", "D", np.float64)
UC = itkCType("unsigned char", "UC", np.uint8)
US = itkCType("unsigned short", "US", np.uint16)
UI = itkCType("unsigned int", "UI", np.uint32)
if os.name == "nt":
    UL = itkCType("unsigned long", "UL", np.uint32)
    SL = itkCType("signed long", "SL", np.int32)
    LD = itkCType("long double", "LD")
else:
    UL = itkCType("unsigned long", "UL", np.uint64)
    SL = itkCType("signed long", "SL", np.int64)
    LD = itkCType("long double", "LD", np.float128)
ULL = itkCType("unsigned long long", "ULL", np.uint64)
SC = itkCType("signed char", "SC", np.int8)
SS = itkCType("signed short", "SS", np.int16)
SI = itkCType("signed int", "SI", np.int32)
SLL = itkCType("signed long long", "SLL", np.int64)
B = itkCType("bool", "B", np.bool)
