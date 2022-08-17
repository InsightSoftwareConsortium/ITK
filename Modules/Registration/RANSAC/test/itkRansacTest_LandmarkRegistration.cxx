#include <fstream>
#include "itkRANSAC.h"
#include "itkLandmarkRegistrationEstimator.h"
#include "RandomNumberGenerator.h"


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

  typedef itk::RANSAC<double, double> RANSACType;

  std::vector<double> data;
  std::vector<double> truePlaneParameters, planeParameters;
  double              outlierDistance = 20.0;
  unsigned int        i;
  double              dotProduct;

  // GenerateData<DIMENSION>(INLIERS, OUTLIERS, outlierDistance, data, truePlaneParameters);

  std::cout << "Known (hyper)plane parameters [n,a]\n\t [ ";
  for (i = 0; i < (2 * DIMENSION - 1); i++)
  {
    std::cout << truePlaneParameters[i] << ", ";
  }
  std::cout << truePlaneParameters[i] << "]\n\n";

  // create and initialize the parameter estimator
  double maximalDistanceFromPlane = 0.5;
  auto   registrationEstimator = itk::LandmarkRegistrationEstimator<33>::New();
  // registrationEstimator->SetDelta(maximalDistanceFromPlane);
  // registrationEstimator->LeastSquaresEstimate(data, planeParameters);

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
GenerateData(unsigned int          numInliers,
             unsigned int          numOutliers,
             double                outlierDistance,
             std::vector<double> & data,
             std::vector<double> & planeParameters)
{
  itk::Vector<double, dimension> normal, noise, tmp;

  double       noiseStandardDeviation = 0.4; // noise standard deviation
  double       coordinateMax = 1000.0;
  unsigned int i, j;

  RandomNumberGenerator random;

  planeParameters.clear();

  for (i = 0; i < dimension; i++)
  {
    normal[i] = random.uniform();
  }
  normal.Normalize();
  for (i = 0; i < dimension; i++)
    planeParameters.push_back(normal[i]);

  // generate inliers
  for (i = 0; i < numInliers; i++)
  {
    for (j = 0; j < dimension; j++)
    {
      noise[j] = random.normal(noiseStandardDeviation);
    }
  }
}
