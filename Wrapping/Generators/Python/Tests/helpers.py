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
from itk.support import helpers
import itk

name = "MedianImageFilter"

snake = helpers.camel_to_snake_case(name)

assert snake == "median_image_filter"

image = itk.Image[itk.F, 2].New()
image.SetRegions([10,10])
image.Allocate()

wasm_type = helpers.wasm_type_from_image_type(image)
assert wasm_type['dimension'] == 2
assert wasm_type['componentType'] == 'float32'
assert wasm_type['pixelType'] == 'Scalar'
assert wasm_type['components'] == 1

image_type = helpers.image_type_from_wasm_type(wasm_type)
assert image_type == type(image)

image = itk.VectorImage[itk.F, 2].New()
image.SetNumberOfComponentsPerPixel(4)
image.SetRegions([10,10])
image.Allocate()

wasm_type = helpers.wasm_type_from_image_type(image)
assert wasm_type['dimension'] == 2
assert wasm_type['componentType'] == 'float32'
assert wasm_type['pixelType'] == 'VariableLengthVector'
assert wasm_type['components'] == 4

image_type = helpers.image_type_from_wasm_type(wasm_type)
assert image_type == type(image)
