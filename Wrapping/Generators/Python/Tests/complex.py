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

import itk


def TestComplex(selectedType):
    cType = itk.complex[itk.ctype(selectedType)]
    c = cType(2.0, 3.0)
    assert c.real() == 2.0
    assert c.imag() == 3.0
    assert complex(c) == 2.0 + 3.0j


TestComplex("float")
TestComplex("double")
