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

"""
itk.TemplateTypeError lists every wrapped template instantiation in full,
reports integer template parameters by value, and names a non-tuple
input type.
"""

import itk

msg = str(itk.TemplateTypeError(itk.Image, (itk.UC, 7)))

for pixel_type, dimension in itk.Image.keys():
    expected = f"itk.Image[{itk.python_type(pixel_type)}, {dimension}]"
    assert f"\n{expected}\n" in msg, f"missing {expected!r} in:\n{msg}"

assert "input type `itk.UC, 7`" in msg, msg

first_pixel_type, first_dimension = next(iter(itk.Image.keys()))
example = f"itk.Image[{itk.python_type(first_pixel_type)}, {first_dimension}].New("
assert example in msg, msg

msg = str(itk.TemplateTypeError(itk.Image, itk.UC))
assert "input type `itk.UC`" in msg, msg
