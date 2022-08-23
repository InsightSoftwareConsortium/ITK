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


template <unsigned int Dimension>
void
GenerateData(unsigned int                                 numInliers,
             unsigned int                                 numOutliers,
             double                                       outlierDistance,
             std::vector<itk::Point<double, Dimension>> & data,
             std::vector<double> &                        planeParameters);


int
itkRansacTest_LandmarkRegistration(int argc, char * argv[])
{
  const unsigned int DimensionPoint = 6;
  const unsigned int INLIERS = 10;
  const unsigned int OUTLIERS = 0; // 10;

  typedef itk::RANSAC<itk::Point<double, DimensionPoint>, double> RANSACType;

  std::vector<itk::Point<double, 6>> data;
  std::vector<double>                truePlaneParameters, transformParameters;
  double                             outlierDistance = 20.0;

  GenerateData<Dimension>(INLIERS, OUTLIERS, outlierDistance, data, truePlaneParameters);

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
GenerateData(unsigned int                                 numInliers,
             unsigned int                                 numOutliers,
             double                                       outlierDistance,
             std::vector<itk::Point<double, Dimension>> & data,
             std::vector<double> &                        planeParameters)
{

  // Read the two point sets that are the putative matches
  using CoordinateType = double;
  using MeshType = itk::Mesh<CoordinateType, 3>;
  using ReaderType = itk::MeshFileReader<MeshType>;

  auto reader1 = ReaderType::New();
  reader1->SetFileName("/home/pranjal.sahu/ethicon_aws/deep_learning/deep_learning/train/fixed.vtk");
  reader1->Update();
  auto mesh1 = reader1->GetOutput();

  auto reader2 = ReaderType::New();
  reader2->SetFileName("/home/pranjal.sahu/ethicon_aws/deep_learning/deep_learning/train/moving.vtk");
  reader2->Update();
  auto mesh2 = reader2->GetOutput();

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

  return;
}
