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
shrink_factors = [2, 1]
smoothing_sigmas = [1, 0]
iterations = 20
update_variance = 2.0
center = np.full(dimension, (image_size - 1) / 2)
moving_center = center.copy()
moving_center[0] += shift
coordinates = np.indices((image_size,) * dimension, dtype=np.float32)


def blob(blob_center):
    squared_radius = sum(
        (coordinates[dimension - 1 - axis] - blob_center[axis]) ** 2
        for axis in range(dimension)
    )
    return itk.image_from_array(
        np.exp(-squared_radius / (2 * blob_sigma**2)).astype(np.float32)
    )


image = blob(center)
moving_image = blob(moving_center)
Image = type(image)
Transform = itk.DisplacementFieldTransform[itk.D, dimension]
GaussianTransform = itk.GaussianSmoothingOnUpdateDisplacementFieldTransform[
    itk.D, dimension
]
GaussianAdaptor = (
    itk.GaussianSmoothingOnUpdateDisplacementFieldTransformParametersAdaptor[
        GaussianTransform
    ]
)
AdaptorBase = itk.TransformParametersAdaptorBase[
    itk.Transform[itk.D, dimension, dimension]
]

field = itk.Image[itk.Vector[itk.D, dimension], dimension].New()
field.CopyInformation(image)
field.SetRegions(image.GetBufferedRegion())
field.AllocateInitialized()
initial_transform = GaussianTransform.New(DisplacementField=field)

metric = itk.MeanSquaresImageToImageMetricv4[Image, Image].New()
# The method's default optimizer estimates scales from its default metric, not this one.
scales_estimator = itk.RegistrationParameterScalesFromPhysicalShift[type(metric)].New(
    Metric=metric, TransformForward=True
)
optimizer = itk.GradientDescentOptimizerv4Template[itk.D].New(
    LearningRate=1.0,
    NumberOfIterations=iterations,
    ScalesEstimator=scales_estimator,
)
registration = itk.ImageRegistrationMethodv4[Image, Image, Transform].New(
    FixedImage=image,
    MovingImage=moving_image,
    Metric=metric,
    Optimizer=optimizer,
)
registration.SetInitialTransform(initial_transform)
registration.SetInPlace(True)
registration.SetNumberOfLevels(len(shrink_factors))
registration.SetShrinkFactorsPerLevel(shrink_factors)
registration.SetSmoothingSigmasPerLevel(smoothing_sigmas)

adaptors = itk.vector[AdaptorBase]()
for factor in shrink_factors:
    shrink = itk.ShrinkImageFilter[Image, Image].New(Input=image, ShrinkFactors=factor)
    shrink.Update()
    level_image = shrink.GetOutput()
    adaptor = GaussianAdaptor.New()
    adaptor.SetRequiredSpacing(level_image.GetSpacing())
    adaptor.SetRequiredSize(level_image.GetBufferedRegion().GetSize())
    adaptor.SetRequiredDirection(level_image.GetDirection())
    adaptor.SetRequiredOrigin(level_image.GetOrigin())
    adaptor.SetGaussianSmoothingVarianceForTheUpdateField(update_variance)
    adaptors.push_back(adaptor)
registration.SetTransformParametersAdaptorsPerLevel(adaptors)
registration.Update()

# The registration runs in place, so the adaptors act on initial_transform.
assert (
    initial_transform.GetGaussianSmoothingVarianceForTheUpdateField() == update_variance
)
final_field = initial_transform.GetDisplacementField()
assert tuple(final_field.GetLargestPossibleRegion().GetSize()) == tuple(
    image.GetLargestPossibleRegion().GetSize()
)
mapped_center = np.array(initial_transform.TransformPoint(center))
assert np.linalg.norm(mapped_center - moving_center) < shift / 2, mapped_center
