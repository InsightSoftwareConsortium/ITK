#==========================================================================
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
#==========================================================================*/

import os
HAVE_NUMPY = True
try:
  import numpy as np
except ImportError:
  HAVE_NUMPY = False

class itkCType:
    __cTypes__ = {}

    def __init__(self, name, short_name, np_dtype=None):
        self.name = name
        self.short_name = short_name
        self.dtype = np_dtype

        itkCType.__cTypes__[self.name] = self

    def __del__(self):
        try:
            del itkCType.__cTypes__[self.name]
        except:
            pass

    def __repr__(self):
        return "<itkCType %s>" % self.name

    def GetCType(name):
        aliases = {'short': 'signed short',
            'int': 'signed int',
            'long': 'signed long',
            'long long': 'signed long long'}
        if name in aliases:
            name = aliases[name]
        try:
            return(itkCType.__cTypes__[name])
        except KeyError:
            return(None)
    GetCType = staticmethod(GetCType)


if HAVE_NUMPY:
    F = itkCType("float", "F", np.float32)
    D = itkCType("double", "D", np.float64)
    UC = itkCType("unsigned char", "UC", np.uint8)
    US = itkCType("unsigned short", "US", np.uint16)
    UI = itkCType("unsigned int", "UI", np.uint32)
    if os.name == 'nt':
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
else:
    F = itkCType("float", "F")
    D = itkCType("double", "D")
    LD = itkCType("long double", "LD")
    UC = itkCType("unsigned char", "UC")
    US = itkCType("unsigned short", "US")
    UI = itkCType("unsigned int", "UI")
    UL = itkCType("unsigned long", "UL")
    ULL = itkCType("unsigned long long", "ULL")
    SC = itkCType("signed char", "SC")
    SS = itkCType("signed short", "SS")
    SI = itkCType("signed int", "SI")
    SL = itkCType("signed long", "SL")
    SLL = itkCType("signed long long", "SLL")
    B = itkCType("bool", "B")
