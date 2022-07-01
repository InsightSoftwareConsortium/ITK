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
import numpy as np
import pickle
import sys

Dimension = 3
PixelType = itk.D

# List of Transforms to test
transforms_to_test = [
    itk.AffineTransform[PixelType, Dimension],
    itk.DisplacementFieldTransform[PixelType, Dimension],
    itk.Rigid3DTransform[PixelType],
    itk.BSplineTransform[PixelType, Dimension, 3],
    itk.QuaternionRigidTransform[PixelType],
]

keys_to_test1 = [
    "name",
    "parametersValueType",
    "transformName",
    "inputDimension",
    "outputDimension",
    "inputSpaceName",
    "outputSpaceName",
    "numberOfParameters",
    "numberOfFixedParameters",
]
keys_to_test2 = ["parameters", "fixedParameters"]

transform_object_list = []
for i, transform_type in enumerate(transforms_to_test):
    transform = transform_type.New()
    transform.SetObjectName("transform" + str(i))
    transform.SetInputSpaceName("fixedSpace" + str(i))
    transform.SetOutputSpaceName("movingSpace" + str(i))
    # Check the serialization
    serialize_deserialize = pickle.loads(pickle.dumps(transform))
    # Test all the attributes
    for k in keys_to_test1:
        assert serialize_deserialize[k] == transform[k]
    # Test all the parameters
    for k in keys_to_test2:
        assert np.array_equal(serialize_deserialize[k], transform[k])
    transform_object_list.append(transform)

print("Individual Transforms Test Done")

# Test Composite Transform
transformType = itk.CompositeTransform[PixelType, Dimension]
composite_transform = transformType.New()

# Add the above created transforms in the composite transform
for transform in transform_object_list:
    composite_transform.AddTransform(transform)

# Check the serialization of composite transform
serialize_deserialize = pickle.loads(pickle.dumps(composite_transform))
assert serialize_deserialize.GetNumberOfTransforms() == 5

# Not testing for name attributes for the composite transform as
# they are lost in current approach

deserialized_object_list = []

# Get the individual transform objects from the composite transform for testing
for i in range(len(transforms_to_test)):
    transform_obj = serialize_deserialize.GetNthTransform(i)

    # Test all the attributes
    for k in keys_to_test1:
        assert transform_obj[k] == transform_object_list[i][k]

    # Test all the parameter arrays
    for k in keys_to_test2:
        assert np.array_equal(transform_obj[k], transform_object_list[i][k])


# Test for transformation using de-serialized BSpline Transform
ImageDimension = 2
SplineOrder = 3
SpaceDimension = ImageDimension
PixelType = itk.D

TransformType = itk.BSplineTransform[PixelType, ImageDimension, SplineOrder]
bspline_transform = TransformType.New()

parameters_values = open(sys.argv[1]).read().split()
parameters_values = [int(x) for x in parameters_values]

fixed_image = itk.imread(sys.argv[2])

InitializerType = itk.BSplineTransformInitializer[TransformType, type(fixed_image)]
transformInitializer = InitializerType.New()
transformInitializer.SetTransform(bspline_transform)
transformInitializer.SetImage(fixed_image)
transformInitializer.SetTransformDomainMeshSize(3)
transformInitializer.InitializeTransform()

# Set parameters by reading them from input file
numberOfParameters = bspline_transform.GetNumberOfParameters()
parameters = [0] * numberOfParameters
numberOfNodes = int(numberOfParameters / SpaceDimension)
for n in range(numberOfNodes):
    parameters[n] = parameters_values[n]
    parameters[n + numberOfNodes] = parameters_values[n + numberOfNodes]

# Set the parameters in the transform object
o2 = bspline_transform.GetParameters()
o2.SetSize(len(parameters))
for j, v in enumerate(parameters):
    o2.SetElement(j, v)
bspline_transform.SetParameters(o2)


# Test serialization of transform object
serialize_deserialize = pickle.loads(pickle.dumps(bspline_transform))

interpolator = itk.LinearInterpolateImageFunction.New(fixed_image)

resampled_image1 = itk.resample_image_filter(
    fixed_image,
    interpolator=interpolator,
    transform=bspline_transform,
    size=itk.size(fixed_image),
    output_origin=fixed_image.GetOrigin(),
    output_spacing=fixed_image.GetSpacing(),
)

resampled_image2 = itk.resample_image_filter(
    fixed_image,
    interpolator=interpolator,
    transform=serialize_deserialize,
    size=itk.size(fixed_image),
    output_origin=fixed_image.GetOrigin(),
    output_spacing=fixed_image.GetSpacing(),
)

# Check if transformed images are same
assert np.array_equal(np.array(resampled_image1), np.array(resampled_image2))


# Check if the displacement fields are same
convert_filter = itk.TransformToDisplacementFieldFilter.IVF22D.New()
convert_filter.SetTransform(bspline_transform)
convert_filter.UseReferenceImageOn()
convert_filter.SetReferenceImage(fixed_image)
convert_filter.Update()
field1 = convert_filter.GetOutput()
field1 = np.array(field1)

convert_filter = itk.TransformToDisplacementFieldFilter.IVF22D.New()
convert_filter.SetTransform(serialize_deserialize)
convert_filter.UseReferenceImageOn()
convert_filter.SetReferenceImage(fixed_image)
convert_filter.Update()
field2 = convert_filter.GetOutput()
field2 = np.array(field2)

assert np.array_equal(np.array(field1), np.array(field2))
