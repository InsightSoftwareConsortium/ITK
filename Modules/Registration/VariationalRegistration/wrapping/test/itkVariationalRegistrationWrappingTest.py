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

Dimension = 2
ImageType = itk.Image[itk.F, Dimension]
FieldType = itk.Image[itk.Vector[itk.F, Dimension], Dimension]

registration = itk.VariationalDiffeomorphicRegistrationFilter[
    ImageType, ImageType, FieldType
].New()
assert isinstance(
    registration, itk.VariationalRegistrationFilter[ImageType, ImageType, FieldType]
)

regularizer = itk.VariationalRegistrationGaussianRegularizer[FieldType].New()
assert isinstance(regularizer, itk.VariationalRegistrationRegularizer[FieldType])
registration.SetRegularizer(regularizer)

function = itk.VariationalRegistrationSSDFunction[ImageType, ImageType, FieldType].New()
assert isinstance(
    function, itk.VariationalRegistrationFunction[ImageType, ImageType, FieldType]
)
registration.SetDifferenceFunction(function)
