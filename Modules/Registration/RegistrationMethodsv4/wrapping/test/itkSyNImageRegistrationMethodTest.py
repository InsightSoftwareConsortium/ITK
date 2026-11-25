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
image_size = 32
blob_sigma = 4.0
shift = 2.0
smoothing_sigmas = [1, 0]
iterations_per_level = [20, 20]
geometry_tolerance = 1e-6
center = np.full(dimension, (image_size - 1) / 2)
moving_center = center.copy()
moving_center[0] += shift
coordinates = np.indices((image_size,) * dimension, dtype=np.float32)
fixed_squared_radius = sum(
    (coordinates[dimension - 1 - axis] - center[axis]) ** 2 for axis in range(dimension)
)
moving_squared_radius = sum(
    (coordinates[dimension - 1 - axis] - moving_center[axis]) ** 2
    for axis in range(dimension)
)
image = itk.image_from_array(
    np.exp(-fixed_squared_radius / (2 * blob_sigma**2)).astype(np.float32)
)
moving_image = itk.image_from_array(
    np.exp(-moving_squared_radius / (2 * blob_sigma**2)).astype(np.float32)
)
Image = type(image)
Transform = itk.DisplacementFieldTransform[itk.D, dimension]
Adaptor = itk.DisplacementFieldTransformParametersAdaptor[Transform]
AdaptorBase = itk.TransformParametersAdaptorBase[
    itk.Transform[itk.D, dimension, dimension]
]

for shrink_factors in ([2, 1], [4, 2], [1, 1]):
    registration = itk.SyNImageRegistrationMethod[Image, Image].New(
        FixedImage=image,
        MovingImage=moving_image,
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
    registration.SetNumberOfLevels(len(shrink_factors))
    registration.SetShrinkFactorsPerLevel(shrink_factors)
    registration.SetSmoothingSigmasPerLevel(smoothing_sigmas)
    registration.SetNumberOfIterationsPerLevel(iterations_per_level)

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
    assert len(registration.GetTransformParametersAdaptorsPerLevel()) == len(
        shrink_factors
    )
    registration.Update()

    transform = registration.GetModifiableTransform()
    mapped_center = np.array(transform.TransformPoint(center))
    assert np.linalg.norm(mapped_center - moving_center) < shift / 2
    inverse = transform.GetInverseTransform()
    mapped_moving_center = np.array(inverse.TransformPoint(moving_center))
    assert np.linalg.norm(mapped_moving_center - center) < shift / 2

    for field in (
        transform.GetDisplacementField(),
        transform.GetInverseDisplacementField(),
    ):
        assert tuple(field.GetLargestPossibleRegion().GetSize()) == tuple(
            level_image.GetLargestPossibleRegion().GetSize()
        )
        np.testing.assert_allclose(
            field.GetOrigin(), level_image.GetOrigin(), atol=geometry_tolerance
        )
        np.testing.assert_allclose(
            field.GetSpacing(), level_image.GetSpacing(), atol=geometry_tolerance
        )
        np.testing.assert_allclose(
            itk.array_from_matrix(field.GetDirection()),
            itk.array_from_matrix(level_image.GetDirection()),
            atol=geometry_tolerance,
        )
