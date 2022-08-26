/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/

#include <fstream>
#include "itkRANSAC.h"
#include "itkLandmarkRegistrationEstimator.h"
#include "RandomNumberGenerator.h"
#include "itkMesh.h"
#include "itkMeshFileReader.h"
#include <algorithm>


template <unsigned int Dimension>
void
GenerateData(std::vector<itk::Point<double, Dimension>> & data, std::vector<itk::Point<double, Dimension>> & agreeData);


int
itkRansacTest_LandmarkRegistration(int argc, char * argv[])
{
  if (argc < 5)
  {
    std::cerr << "Missing arguments." << std::endl;
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << " movingFeatureMesh, "
              << "fixedFeatureMesh, "
              << "movingMesh, "
              << "fixedMesh " << std::endl;
    return EXIT_FAILURE;
  }

  const unsigned int                                              DimensionPoint = 6;
  typedef itk::RANSAC<itk::Point<double, DimensionPoint>, double> RANSACType;

  std::vector<itk::Point<double, DimensionPoint>> data;
  std::vector<itk::Point<double, DimensionPoint>> agreeData;
  std::vector<double>                             transformParameters;

  return EXIT_SUCCESS;
  GenerateData<DimensionPoint>(data, agreeData);


  // create and initialize the parameter estimator
  double maximalDistanceFromPlane = 6;
  auto   registrationEstimator = itk::LandmarkRegistrationEstimator<6>::New();
  registrationEstimator->SetMinimalForEstimate(3);
  registrationEstimator->SetDelta(maximalDistanceFromPlane);
  registrationEstimator->LeastSquaresEstimate(data, transformParameters);

  std::cout << "Least Squares Estimate is " << std::endl;
  unsigned int i = 0;
  for (i = 0; i < transformParameters.size(); i++)
  {
    std::cout << transformParameters[i] << ", ";
  }


  // create and initialize the RANSAC algorithm
  double              desiredProbabilityForNoOutliers = 0.999;
  double              percentageOfDataUsed;
  RANSACType::Pointer ransacEstimator = RANSACType::New();
  ransacEstimator->SetData(data);
  ransacEstimator->SetAgreeData(agreeData);
  ransacEstimator->SetParametersEstimator(registrationEstimator);
  percentageOfDataUsed = ransacEstimator->Compute(transformParameters, desiredProbabilityForNoOutliers);

  if (transformParameters.empty())
  {
    std::cout << "RANSAC estimate failed, degenerate configuration?\n";
  }
  else
  {
    std::cout << "RANSAC hyper(plane) parameters: [n,a]\n\t [ ";
    for (i = 0; i < transformParameters.size(); i++)
    {
      std::cout << transformParameters[i] << ", ";
    }
  }

  std::cout << "percentageOfDataUsed " << percentageOfDataUsed << "\n\n";
  return EXIT_SUCCESS;
}


template <unsigned int Dimension>
void
GenerateData(std::vector<itk::Point<double, Dimension>> & data, std::vector<itk::Point<double, Dimension>> & agreeData)
{
  // Read the two point sets that are the putative matches
  using CoordinateType = double;
  using MeshType = itk::Mesh<CoordinateType, 3>;
  using ReaderType = itk::MeshFileReader<MeshType>;

  auto reader1 = ReaderType::New();
  reader1->SetFileName("/data/Apedata/Slicer-cli-outputs/train/EJEJHH_fixed_corr.vtk");
  reader1->Update();
  auto mesh1 = reader1->GetOutput();

  auto reader2 = ReaderType::New();
  reader2->SetFileName("/data/Apedata/Slicer-cli-outputs/train/EJEJHH_moving_corr.vtk");
  reader2->Update();
  auto mesh2 = reader2->GetOutput();


  auto reader1_all = ReaderType::New();
  reader1_all->SetFileName("/data/Apedata/Slicer-cli-outputs/EJEJHH_fixedMeshPoints.vtk");
  reader1_all->Update();
  auto mesh1_all = reader1_all->GetOutput();

  auto reader2_all = ReaderType::New();
  reader2_all->SetFileName("/data/Apedata/Slicer-cli-outputs/EJEJHH_movingMeshPointsBefore.vtk");
  reader2_all->Update();
  auto mesh2_all = reader2_all->GetOutput();


  data.reserve(mesh1->GetNumberOfPoints());

  // Concatenate corressponding points from two meshes and insert in the data vector
  using PointType = itk::Point<CoordinateType, 6>;
  PointType p0;
  for (unsigned int i = 0; i < mesh1->GetNumberOfPoints(); ++i)
  {
    auto point1 = mesh1->GetPoint(i);
    auto point2 = mesh2->GetPoint(i);

    p0[0] = point1[0];
    p0[1] = point1[1];
    p0[2] = point1[2];

    p0[3] = point2[0];
    p0[4] = point2[1];
    p0[5] = point2[2];

    data.push_back(p0);
  }

  std::vector<unsigned int> indexArray;
  unsigned int              minCount = std::min(mesh1->GetNumberOfPoints(), mesh2->GetNumberOfPoints());
  unsigned int              maxCount = std::max(mesh1->GetNumberOfPoints(), mesh2->GetNumberOfPoints());
  indexArray.reserve(maxCount);

  // shuffle the larger pointset to sample points uniformly from pointset
  for (unsigned int i = 0; i < maxCount; ++i)
  {
    indexArray.push_back(i);
  }
  std::random_shuffle(indexArray.begin(), indexArray.end());

  for (unsigned int i = 0; i < minCount; ++i)
  {
    auto point1 = mesh1_all->GetPoint(i);
    auto point2 = mesh2_all->GetPoint(i);

    p0[0] = point1[0];
    p0[1] = point1[1];
    p0[2] = point1[2];

    p0[3] = point2[0];
    p0[4] = point2[1];
    p0[5] = point2[2];

    agreeData.push_back(p0);
  }
  return;
}
