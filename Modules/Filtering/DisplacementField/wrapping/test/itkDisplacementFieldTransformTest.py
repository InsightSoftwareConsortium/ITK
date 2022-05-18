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
import itk
itk.auto_progress(2)

ScalarType = itk.F
VectorDimension = 2
VectorType = itk.Vector[ScalarType,VectorDimension]

ImageDimension = 2
ImageType = itk.Image[VectorType, ImageDimension]

image_size = [10, 10]

transform = itk.DisplacementFieldTransform[ScalarType, ImageDimension].New()

# Test setting image of vectors
pixel_value = 5
image = ImageType.New()
image.SetRegions(image_size)
image.Allocate()
image.FillBuffer([pixel_value] * VectorDimension)

transform.SetDisplacementField(image)

# Verify all parameters match expected value
for value in list(transform.GetParameters()):
    assert value == pixel_value

# Test setting vector image
pixel_value = 20
vector_image = itk.VectorImage[ScalarType, ImageDimension].New()
vector_image.SetRegions(image_size)
vector_image.SetVectorLength(VectorDimension)
vector_image.Allocate()
pixel_default = itk.VariableLengthVector[ScalarType]()
pixel_default.SetSize(VectorDimension)
pixel_default.Fill(pixel_value)
vector_image.FillBuffer(pixel_default)

transform.SetDisplacementField(vector_image)

# Verify all parameters match expected value
for value in list(transform.GetParameters()):
    assert value == pixel_value
