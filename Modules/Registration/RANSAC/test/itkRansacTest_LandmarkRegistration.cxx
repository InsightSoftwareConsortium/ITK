#include <fstream>
#include "itkRANSAC.h"
#include "itkLandmarkRegistrationEstimator.h"
#include "RandomNumberGenerator.h"
#include "itkMesh.h"
#include "itkMeshFileReader.h"


template <unsigned int dimension>
void
GenerateData(unsigned int          numInliers,
             unsigned int          numOutliers,
             double                outlierDistance,
             std::vector<double> & data,
             std::vector<double> & planeParameters);


int
itkRansacTest_LandmarkRegistration(int argc, char * argv[])
{
  const unsigned int DIMENSION = 3;
  const unsigned int INLIERS = 10;
  const unsigned int OUTLIERS = 0; // 10;

  typedef itk::RANSAC<itk::Point<double, 6>, double> RANSACType;

  std::vector<double> data;
  std::vector<double> truePlaneParameters, planeParameters;
  double              outlierDistance = 20.0;
  unsigned int        i;
  double              dotProduct;

  GenerateData<DIMENSION>(INLIERS, OUTLIERS, outlierDistance, data, truePlaneParameters);

  std::cout << "Known (hyper)plane parameters [n,a]\n\t [ ";
  for (i = 0; i < (2 * DIMENSION - 1); i++)
  {
    std::cout << truePlaneParameters[i] << ", ";
  }
  std::cout << truePlaneParameters[i] << "]\n\n";

  // create and initialize the parameter estimator
  double maximalDistanceFromPlane = 0.5;
  auto   registrationEstimator = itk::LandmarkRegistrationEstimator<6>::New();
  registrationEstimator->SetDelta(maximalDistanceFromPlane);
  registrationEstimator->LeastSquaresEstimate(data, planeParameters);

  // if (planeParameters.empty())
  // {
  //   std::cout << "Least squares estimate failed, degenerate configuration?\n";
  // }
  // else
  // {
  //   std::cout << "Least squares hyper(plane) parameters: [n,a]\n\t [ ";
  //   for (i = 0; i < (2 * DIMENSION - 1); i++)
  //   {
  //     std::cout << planeParameters[i] << ", ";
  //   }
  //   std::cout << planeParameters[i] << "]\n\n";

  //   // cos(theta), theta is the angle between the two unit normals
  //   dotProduct = 0.0;
  //   for (i = 0; i < DIMENSION; i++)
  //   {
  //     dotProduct += planeParameters[i] * truePlaneParameters[i];
  //   }
  //   std::cout << "\tDot product of real and computed normals[+-1=correct]: ";
  //   std::cout << dotProduct << "\n";

  //   dotProduct = 0.0;
  //   for (i = 0; i < DIMENSION; i++)
  //   {
  //     dotProduct += (planeParameters[DIMENSION + i] - truePlaneParameters[DIMENSION + i]) * truePlaneParameters[i];
  //   }

  //   std::cout << dotProduct << "\n\n";
  // }

  // // create and initialize the RANSAC algorithm
  // double              desiredProbabilityForNoOutliers = 0.999;
  // double              percentageOfDataUsed;
  // RANSACType::Pointer ransacEstimator = RANSACType::New();
  // ransacEstimator->SetData(data);
  // ransacEstimator->SetParametersEstimator(registrationEstimator);
  // percentageOfDataUsed = ransacEstimator->Compute(planeParameters, desiredProbabilityForNoOutliers);

  // if (planeParameters.empty())
  // {
  //   std::cout << "RANSAC estimate failed, degenerate configuration?\n";
  // }
  // else
  // {
  //   std::cout << "RANSAC hyper(plane) parameters: [n,a]\n\t [ ";
  //   for (i = 0; i < (2 * DIMENSION - 1); i++)
  //   {
  //     std::cout << planeParameters[i] << ", ";
  //   }
  //   std::cout << planeParameters[i] << "]\n\n";

  //   dotProduct = 0.0;
  //   for (i = 0; i < DIMENSION; i++)
  //   {
  //     dotProduct += planeParameters[i] * truePlaneParameters[i];
  //   }
  //   std::cout << "\tDot product of real and computed normals[+-1=correct]: ";
  //   std::cout << dotProduct << "\n";

  //   dotProduct = 0.0;
  //   for (i = 0; i < DIMENSION; i++)
  //   {
  //     dotProduct += (planeParameters[DIMENSION + i] - truePlaneParameters[DIMENSION + i]) * truePlaneParameters[i];
  //   }

  //   std::cout << dotProduct << "\n\n";

  //   std::cout << percentageOfDataUsed << "\n\n";
  // }
  return EXIT_SUCCESS;
}


template <unsigned int dimension>
void
GenerateData(unsigned int                                 numInliers,
             unsigned int                                 numOutliers,
             double                                       outlierDistance,
             std::vector<itk::Point<double, dimension>> & data,
             std::vector<double> &                        planeParameters)
{
  using CoordinateType = double;
  using MeshType = itk::Mesh<CoordinateType, 3>;

  using ReaderType = itk::MeshFileReader<MeshType>;
  auto reader = ReaderType::New();
  reader->SetFileName("");
  reader->Update();
  auto mesh1 = reader->GetOutput();

  reader->SetFileName("");
  reader->Update();
  auto mesh2 = reader->GetOutput();

  data.reserve(mesh1->GetNumberOfPoints());
  using PointType = itk::Point<CoordinateType, 6>;

  // Concatenate corressponding points from two meshes and insert in the data vector
  PointType p0;
  for (int i = 0; i < mesh1->GetNumberOfPoints(); ++i)
  {
    auto point1 = mesh1->GetPoint(i);
    auto point2 = mesh2->GetPoint(i);

    p0[0] = point1[0];
    p0[1] = point1[1];
    p0[2] = point1[2];
    p0[3] = point2[0];
    p0[4] = point2[1];
    p0[5] = point2[2];

    data.insert(i, p0);
  }

  return;
}
