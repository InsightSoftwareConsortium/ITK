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
import sys
import numpy as np
import copy

print("Running RansacMeshTest Python")

inlier_value = float(sys.argv[1])
number_of_ransac_points = int(sys.argv[2])
number_of_iterations = int(sys.argv[3])
movingFeatureMeshPath = sys.argv[4]
fixedFeatureMeshPath = sys.argv[5]
movingMeshPath = sys.argv[6]
fixedMeshPath = sys.argv[7]

print(inlier_value)
print(number_of_ransac_points)
print(number_of_iterations)
print(movingFeatureMeshPath)
print(fixedFeatureMeshPath)
print(movingMeshPath)
print(fixedMeshPath)

movingFeatureMesh = itk.meshread(movingFeatureMeshPath, itk.F)
fixedFeatureMesh = itk.meshread(fixedFeatureMeshPath, itk.F)
movingMesh = itk.meshread(movingMeshPath, itk.F)
fixedMesh = itk.meshread(fixedMeshPath, itk.F)

movingMeshFeaturePoints = itk.array_from_vector_container(movingFeatureMesh.GetPoints())
fixedMeshFeaturePoints = itk.array_from_vector_container(fixedFeatureMesh.GetPoints())
movingMeshPoints = itk.array_from_vector_container(movingMesh.GetPoints())
fixedMeshPoints = itk.array_from_vector_container(fixedMesh.GetPoints())


def GenerateData(data, agreeData):
    """
    In current implmentation the agreedata contains two corressponding
    points from moving and fixed mesh. However, after the subsampling step the
    number of points need not be equal in those meshes. So we randomly sample
    the points from larger mesh.
    """
    data.reserve(movingMeshFeaturePoints.shape[0])
    for i in range(movingMeshFeaturePoints.shape[0]):
        point1 = movingMeshFeaturePoints[i]
        point2 = fixedMeshFeaturePoints[i]
        input_data = [point1[0], point1[1], point1[2], point2[0], point2[1], point2[2]]
        input_data = [float(x) for x in input_data]
        data.push_back(input_data)

    count_min = int(np.min([movingMeshPoints.shape[0], fixedMeshPoints.shape[0]]))

    mesh1_points = copy.deepcopy(movingMeshPoints)
    mesh2_points = copy.deepcopy(fixedMeshPoints)

    agreeData.reserve(count_min)
    for i in range(count_min):
        point1 = mesh1_points[i]
        point2 = mesh2_points[i]
        input_data = [point1[0], point1[1], point1[2], point2[0], point2[1], point2[2]]
        input_data = [float(x) for x in input_data]
        agreeData.push_back(input_data)
    return


maximumDistance = inlier_value

data = itk.vector[itk.Point[itk.D, 6]]()
agreeData = itk.vector[itk.Point[itk.D, 6]]()
GenerateData(data, agreeData)

transformParameters = itk.vector.D()
bestTransformParameters = itk.vector.D()

TransformType = itk.Similarity3DTransform.D
RegistrationEstimatorType = itk.Ransac.LandmarkRegistrationEstimator[6, TransformType]
registrationEstimator = RegistrationEstimatorType.New()
registrationEstimator.SetMinimalForEstimate(number_of_ransac_points)
registrationEstimator.SetAgreeData(agreeData)
registrationEstimator.SetDelta(maximumDistance)
registrationEstimator.LeastSquaresEstimate(data, transformParameters)

desiredProbabilityForNoOutliers = 0.99
RANSACType = itk.RANSAC[itk.Point[itk.D, 6], itk.D, TransformType]
ransacEstimator = RANSACType.New()
ransacEstimator.SetData(data)
ransacEstimator.SetAgreeData(agreeData)
ransacEstimator.SetMaxIteration(number_of_iterations)
ransacEstimator.SetNumberOfThreads(16)
ransacEstimator.SetParametersEstimator(registrationEstimator)

percentageOfDataUsed = ransacEstimator.Compute(
    transformParameters, desiredProbabilityForNoOutliers
)
print("Percentage of data used is ", percentageOfDataUsed)
