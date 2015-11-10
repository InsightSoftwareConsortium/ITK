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

import itk

Dimension = 2

index = itk.Index[Dimension]()
index[0] = 1
index[1] = 2

offset = itk.Offset[Dimension]()
offset.Fill(1)

a = index + offset
b = offset + index
c = index + index
d = offset + offset
e = index - offset

assert a[0] == 2
assert a[1] == 3

assert b[0] == 2
assert b[1] == 3

assert c[0] == 2
assert c[1] == 4

assert d[0] == 2
assert d[1] == 2

assert e[0] == 0
assert e[1] == 1
