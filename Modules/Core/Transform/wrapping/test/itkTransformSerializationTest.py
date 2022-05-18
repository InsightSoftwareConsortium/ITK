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

Dimension = 3
PixelType = itk.D

# List of Transforms to test
transforms_to_test = [itk.AffineTransform[PixelType, Dimension], itk.DisplacementFieldTransform[PixelType, Dimension], itk.Rigid3DTransform[PixelType], itk.BSplineTransform[PixelType, Dimension, 3], itk.QuaternionRigidTransform[PixelType]]

keys_to_test1 = ["name", "parametersValueType", "transformName", "transformType", "inDimension", "outDimension", "numberOfParameters", "numberOfFixedParameters"]
keys_to_test2 = ["parameters", "fixedParameters"]

transform_object_list = []
for i, transform_type in enumerate(transforms_to_test):
    transform = transform_type.New()
    transform.SetObjectName("transform"+str(i))

    # Check the serialization
    serialize_deserialize = pickle.loads(pickle.dumps(transform))

    # Test all the attributes
    for k in keys_to_test1:
        assert serialize_deserialize[k] == transform[k]

    # Test all the parameters
    for k in keys_to_test2:
        assert np.array_equal(serialize_deserialize[k], transform[k])

    transform_object_list.append(transform)

print('Individual Transforms Test Done')

# Test Composite Transform
transformType = itk.CompositeTransform[PixelType, Dimension]
composite_transform = transformType.New()
composite_transform.SetObjectName('composite_transform')

# Add the above created transforms in the composite transform
for transform in transform_object_list:
    composite_transform.AddTransform(transform)

# Check the serialization of composite transform
serialize_deserialize = pickle.loads(pickle.dumps(composite_transform))

assert serialize_deserialize.GetObjectName() == composite_transform.GetObjectName()
assert serialize_deserialize.GetNumberOfTransforms() == 5
assert serialize_deserialize["name"] == composite_transform["name"]

deserialized_object_list = []

keys_to_test1 = ["name", "parametersValueType", "transformName", "inDimension", "outDimension", "numberOfParameters", "numberOfFixedParameters"]

# Get the individual transform objects from the composite transform for testing
for i in range(len(transforms_to_test)):
    transform_obj = serialize_deserialize.GetNthTransform(i)

    # Test all the attributes
    for k in keys_to_test1:
        assert transform_obj[k] == transform_object_list[i][k]

    # Test all the parameter arrays
    for k in keys_to_test2:
        assert np.array_equal(transform_obj[k], transform_object_list[i][k])

    # Testing for loss of transformType in Composite transform
    if i == 3:
        # BSpline has same type here D33
        assert transform_obj["transformType"] == transform_object_list[i]["transformType"]
    else:
        assert transform_obj["transformType"] != transform_object_list[i]["transformType"]
