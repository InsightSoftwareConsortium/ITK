#==========================================================================
#
#   Copyright Insight Software Consortium
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


class itkCType:
    __cTypes__ = {}

    def __init__(self, name, shortName):
        self.name = name
        self.short_name = shortName

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
