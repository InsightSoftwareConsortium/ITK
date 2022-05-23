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
import os


Dimension = 3
PixelType = itk.D
NumberOfPoints = 5

PointSetType = itk.PointSet[PixelType, Dimension]
point_set = PointSetType.New()

# Set the name of mesh
point_set.SetObjectName("testpointset")

# Set Points in the PointSet
PointType = itk.Point[itk.F, 3]
if os.name == 'nt':
    v_point = itk.VectorContainer[itk.ULL, PointType].New()
else:
    v_point = itk.VectorContainer[itk.UL, PointType].New()
v_point.Reserve(NumberOfPoints)

point = PointType()
for i in range(NumberOfPoints):
    point[0] = i + 1.0
    point[1] = i + 2.0
    point[2] = i + 3.0
    v_point.SetElement(i, point)

arr = itk.array_view_from_vector_container(v_point)
points_vc = itk.vector_container_from_array(arr.flatten())

point_set.SetPoints(points_vc)


# Check the serialization of mesh
serialize_deserialize = pickle.loads(pickle.dumps(point_set))

assert serialize_deserialize.GetNumberOfPoints() == point_set.GetNumberOfPoints()
assert serialize_deserialize.GetObjectName() == point_set.GetObjectName()

# Check if points are same
p1 = itk.array_from_vector_container(serialize_deserialize.GetPoints())
p2 = itk.array_from_vector_container(point_set.GetPoints())
assert np.array_equal(p1, p2)


# Check dictionary set/get for ITK point_set
point_set["name"] = "testpoint_set1"
assert point_set["name"] == "testpoint_set1"

points_array = np.array([1, 2, 3, 4, 5, 6, 7, 8, 9], dtype="float32")
point_set["points"] = points_array
assert np.array_equal(point_set["points"], points_array)

points_data_array = np.array([10, 11], dtype="float64")
point_set["pointData"] = points_data_array
assert np.array_equal(point_set["pointData"], points_data_array)
