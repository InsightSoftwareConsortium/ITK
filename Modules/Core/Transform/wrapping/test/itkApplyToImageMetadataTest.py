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

import itk
import numpy as np

Dimension = 3
PixelType = itk.F

# Create a simple image with known origin, spacing and direction.
ImageType = itk.Image[PixelType, Dimension]
image = ImageType.New()
size = itk.Size[Dimension]()
size.Fill(4)
region = itk.ImageRegion[Dimension]()
region.SetSize(size)
image.SetRegions(region)
image.SetOrigin([1.0, 2.0, 3.0])
image.SetSpacing([1.0, 1.0, 1.0])
image.Allocate()

# Build a translation transform, which is linear and invertible.
TransformType = itk.TranslationTransform[itk.D, Dimension]
transform = TransformType.New()
translation = itk.Vector[itk.D, Dimension]()
translation[0] = 10.0
translation[1] = 20.0
translation[2] = 30.0
transform.Translate(translation)

# Apply the transform to the image metadata, updating origin/spacing/direction
# in place, without resampling the pixel data.
transform.ApplyToImageMetadata(image)

expected_origin = transform.GetInverseTransform().TransformPoint([1.0, 2.0, 3.0])

for i in range(Dimension):
    assert np.isclose(image.GetOrigin()[i], expected_origin[i])

print("ApplyToImageMetadata Test Done")
