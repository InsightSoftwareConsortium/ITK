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
TransformType = itk.DisplacementFieldTransform[itk.D, Dimension]
FieldType = itk.Image[itk.Vector[itk.D, Dimension], Dimension]


def constant_field(size, spacing, value):
    field = FieldType.New()
    field.SetRegions([size] * Dimension)
    field.SetSpacing([spacing] * Dimension)
    field.Allocate()
    vector = itk.Vector[itk.D, Dimension]()
    for i, v in enumerate(value):
        vector[i] = v
    field.FillBuffer(vector)
    return field


def adapt(adaptor, transform, target):
    """Adapt ``transform`` onto ``target``'s grid and return the field array."""
    adaptor.SetRequiredSize(target.GetLargestPossibleRegion().GetSize())
    adaptor.SetRequiredSpacing(target.GetSpacing())
    adaptor.SetRequiredOrigin(target.GetOrigin())
    adaptor.SetRequiredDirection(target.GetDirection())
    adaptor.SetTransform(transform)
    adaptor.AdaptTransformParameters()
    adapted = transform.GetDisplacementField()
    assert list(adapted.GetLargestPossibleRegion().GetSize()) == list(
        target.GetLargestPossibleRegion().GetSize()
    )
    assert list(adapted.GetSpacing()) == list(target.GetSpacing())
    return itk.array_from_image(adapted)


value = (1.0, -2.0, 0.5)
field = constant_field(8, 2.0, value)
fine = constant_field(16, 1.0, (0.0, 0.0, 0.0))
transform = TransformType.New()
transform.SetDisplacementField(field)
adaptor = itk.DisplacementFieldTransformParametersAdaptor[TransformType].New()
# The resampler pads with zero outside the coarse field; check the interior.
interior = adapt(adaptor, transform, fine)[2:-2, 2:-2, 2:-2]
assert np.allclose(interior, value), interior.reshape(-1, Dimension)[:3]

GaussianTransformType = itk.GaussianSmoothingOnUpdateDisplacementFieldTransform[
    itk.D, Dimension
]
gaussian_transform = GaussianTransformType.New()
gaussian_transform.SetDisplacementField(constant_field(8, 2.0, value))
gaussian_adaptor = (
    itk.GaussianSmoothingOnUpdateDisplacementFieldTransformParametersAdaptor[
        GaussianTransformType
    ].New()
)
gaussian_adaptor.SetGaussianSmoothingVarianceForTheUpdateField(2.0)
gaussian_adaptor.SetGaussianSmoothingVarianceForTheTotalField(0.0)
assert isinstance(
    gaussian_adaptor,
    itk.TransformParametersAdaptorBase[itk.Transform[itk.D, Dimension, Dimension]],
)
assert isinstance(
    gaussian_adaptor,
    itk.DisplacementFieldTransformParametersAdaptor[GaussianTransformType],
)
interior = adapt(gaussian_adaptor, gaussian_transform, fine)[2:-2, 2:-2, 2:-2]
assert np.allclose(interior, value)
assert gaussian_transform.GetGaussianSmoothingVarianceForTheUpdateField() == 2.0
assert gaussian_transform.GetGaussianSmoothingVarianceForTheTotalField() == 0.0
