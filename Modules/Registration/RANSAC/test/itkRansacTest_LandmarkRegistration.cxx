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
#include <random>

template <unsigned int Dimension>
void
GenerateData(std::vector<itk::Point<double, Dimension>> & data,
             std::vector<itk::Point<double, Dimension>> & agreeData,
             char *                                       movingFeatureMesh,
             char *                                       fixedFeatureMesh,
             char *                                       movingMesh,
             char *                                       fixedMesh);


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

  using TTransform = itk::Similarity3DTransform<double>;
  const unsigned int                                                          DimensionPoint = 6;
  typedef itk::RANSAC<itk::Point<double, DimensionPoint>, double, TTransform> RANSACType;

  std::vector<itk::Point<double, DimensionPoint>> data;
  std::vector<itk::Point<double, DimensionPoint>> agreeData;
  std::vector<double>                             transformParameters;

  GenerateData<DimensionPoint>(data, agreeData, argv[1], argv[2], argv[3], argv[4]);

  // create and initialize the parameter estimator
  double inlierValue = 3;
  int    ransacPoints = 3;
  int    maxIteration = 10000;
  auto   registrationEstimator = itk::LandmarkRegistrationEstimator<6, TTransform>::New();
  registrationEstimator->SetMinimalForEstimate(ransacPoints);
  registrationEstimator->SetDelta(inlierValue);
  registrationEstimator->SetAgreeData(agreeData);
  registrationEstimator->LeastSquaresEstimate(data, transformParameters);

  unsigned int i = 0;
  for (i = 0; i < transformParameters.size(); i++)
  {
    std::cout << transformParameters[i] << ", ";
  }

  // create and initialize the RANSAC algorithm
  double              desiredProbabilityForNoOutliers = 0.99;
  RANSACType::Pointer ransacEstimator = RANSACType::New();
  ransacEstimator->SetData(data);
  ransacEstimator->SetAgreeData(agreeData);
  ransacEstimator->SetParametersEstimator(registrationEstimator);
  ransacEstimator->SetCheckCorresspondenceDistance(true);
  ransacEstimator->SetCheckCorrespondenceEdgeLength(0.9);
  ransacEstimator->SetMaxIteration(maxIteration);

  auto percentageOfDataUsed = ransacEstimator->Compute(transformParameters, desiredProbabilityForNoOutliers);

  if (transformParameters.empty())
  {
    std::cout << "RANSAC estimate failed, degenerate configuration?\n";
  }
  else
  {
    std::cout << "RANSAC parameters: [n,a]\n\t [ ";
    for (i = 0; i < transformParameters.size(); i++)
    {
      std::cout << transformParameters[i] << ", ";
    }
  }

  std::cout << "\n\n" << std::endl;
  std::cout << "percentageOfDataUsed " << percentageOfDataUsed[0] << "\n\n";
  std::cout << "Inlier RMSE is  " << percentageOfDataUsed[1] << "\n\n";
  return EXIT_SUCCESS;
}


template <unsigned int Dimension>
void
GenerateData(std::vector<itk::Point<double, Dimension>> & data,
             std::vector<itk::Point<double, Dimension>> & agreeData,
             char *                                       movingFeatureMesh,
             char *                                       fixedFeatureMesh,
             char *                                       movingMesh,
             char *                                       fixedMesh)
{
  // Read the two point sets that are the putative matches
  using CoordinateType = double;
  using MeshType = itk::Mesh<CoordinateType, 3>;
  using ReaderType = itk::MeshFileReader<MeshType>;

  auto reader1 = ReaderType::New();
  reader1->SetFileName(fixedFeatureMesh);
  reader1->Update();
  auto mesh1 = reader1->GetOutput();

  auto reader2 = ReaderType::New();
  reader2->SetFileName(movingFeatureMesh);
  reader2->Update();
  auto mesh2 = reader2->GetOutput();

  auto reader1_all = ReaderType::New();
  reader1_all->SetFileName(fixedMesh);
  reader1_all->Update();
  auto mesh1_all = reader1_all->GetOutput();

  auto reader2_all = ReaderType::New();
  reader2_all->SetFileName(movingMesh);
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

  std::shuffle(indexArray.begin(), indexArray.end(), std::default_random_engine(0));
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
