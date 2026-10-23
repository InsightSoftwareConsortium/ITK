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

import sys

import itk
import numpy as np


dimension = int(sys.argv[1])
image = itk.image_from_array(
    np.random.default_rng(0).random((16,) * dimension).astype(np.float32)
)
Image = type(image)
Transform = itk.DisplacementFieldTransform[itk.D, dimension]
Adaptor = itk.DisplacementFieldTransformParametersAdaptor[Transform]
AdaptorBase = itk.TransformParametersAdaptorBase[
    itk.Transform[itk.D, dimension, dimension]
]

for shrink_factors in ([2, 1], [1, 1]):
    registration = itk.SyNImageRegistrationMethod[Image, Image].New(
        FixedImage=image,
        MovingImage=image,
        Metric=itk.MeanSquaresImageToImageMetricv4[Image, Image].New(),
    )
    Field = itk.Image[itk.Vector[itk.D, dimension], dimension]
    fields = []
    for _ in range(2):
        field = Field.New()
        field.CopyInformation(image)
        field.SetRegions(image.GetBufferedRegion())
        field.AllocateInitialized()
        fields.append(field)
    initial_transform = Transform.New(
        DisplacementField=fields[0], InverseDisplacementField=fields[1]
    )
    registration.SetInitialTransform(initial_transform)
    registration.SetNumberOfLevels(2)
    registration.SetShrinkFactorsPerLevel(shrink_factors)
    registration.SetSmoothingSigmasPerLevel([1, 0])
    registration.SetNumberOfIterationsPerLevel([1, 1])

    adaptors = itk.vector[AdaptorBase]()
    for factor in shrink_factors:
        shrink = itk.ShrinkImageFilter[Image, Image].New(
            Input=image, ShrinkFactors=factor
        )
        shrink.Update()
        level_image = shrink.GetOutput()
        adaptor = Adaptor.New()
        adaptor.SetRequiredSpacing(level_image.GetSpacing())
        adaptor.SetRequiredSize(level_image.GetBufferedRegion().GetSize())
        adaptor.SetRequiredDirection(level_image.GetDirection())
        adaptor.SetRequiredOrigin(level_image.GetOrigin())
        adaptors.push_back(adaptor)

    try:
        registration.SetTransformParametersAdaptorsPerLevel(itk.vector[AdaptorBase]())
    except RuntimeError:
        pass
    else:
        raise AssertionError("An adaptor is required for each level")

    registration.SetTransformParametersAdaptorsPerLevel(adaptors)
    assert len(registration.GetTransformParametersAdaptorsPerLevel()) == 2
    registration.Update()

    transform = registration.GetModifiableTransform()
    for field in (
        transform.GetDisplacementField(),
        transform.GetInverseDisplacementField(),
    ):
        assert tuple(field.GetLargestPossibleRegion().GetSize()) == tuple(
            image.GetLargestPossibleRegion().GetSize()
        )
        np.testing.assert_allclose(field.GetOrigin(), image.GetOrigin())
        np.testing.assert_allclose(field.GetSpacing(), image.GetSpacing())
        np.testing.assert_allclose(
            itk.array_from_matrix(field.GetDirection()),
            itk.array_from_matrix(image.GetDirection()),
        )
        np.testing.assert_allclose(itk.array_from_image(field), 0, atol=1e-6)
