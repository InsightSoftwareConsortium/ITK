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
#include "itkPlaneParametersEstimator.h"
#include "RandomNumberGenerator.h"


/**
 * Generate points on a (hyper)plane with additive zero mean Gaussian noise and
 * outliers.
 * @param numInliers How many points are inliers.
 * @param numOutliers How many points are outliers.
 * @param outlierDistance Threshold defining outliers, points that are further
 *                        than this distance from the plane.
 * @param data The points are added to the end of this vector.
 * @param planeParameters [n,a], plane normal and point on plane. Plane is
 *                        defined as the set of points p such that n^T(p-a)=0.
 */
template <unsigned int dimension>
void
GenerateData(unsigned int                                 numInliers,
             unsigned int                                 numOutliers,
             double                                       outlierDistance,
             std::vector<itk::Point<double, dimension>> & data,
             std::vector<double> &                        planeParameters);

/**
 * This function only works in 3D.
 *
 * Save an open inventor ASCII scene file showing the points identified as
 * inliers/outliers and the estimated plane. The output file is
 * "planeEstimation.iv". The format is compatible with the viewers available in
 * the open source coin3D toolkit (www.coin3d.org).
 *
 * @param outputFileName Write the scene to this file.
 * @param data The 3D points.
 * @param estimatedPlaneParameters [n,a], plane normal and point on plane. Plane
 *                                 is the set of points p such that n^T(p-a)=0.
 * @param parameterEstimator The plane parameter estimator whoes Agree() method
 *                           is used to identify inliers.
 */
// template<unsigned int dimension>
// void SaveOIVFile( std::string &outputFileName,
//                   std::vector< itk::Point<double,dimension> > &data,
//                   std::vector<double> &estimatedPlaneParameters,
//                   typename itk::PlaneParametersEstimator<dimension>::Pointer
//                   parameterEstimator );
/**
 * Given the hard coded dimension, and number of outliers and inliers generate
 * a random dataset accordingly. Then estimate the (hyper)plane parameter values
 * using a least squares estimate and the RANSAC algorithm. Compare the results
 * to the known (hyper)plane. Code is written for nD data except the
 * visualization which is limited to 3D. If DIMENSION is set to three, two
 * open inventor scene files are written, showing the least squares and RANSAC
 * estimates. Data points are colored spheres, those that agree with the
 * estimated model are green, otherwise they are red.
 *
 * @author Ziv Yaniv (zivy@isis.georgetown.edu)
 */
int
itkRansacTest_PlaneEstimation(int argc, char * argv[])
{
  const unsigned int DIMENSION = 3;
  const unsigned int INLIERS = 10;
  const unsigned int OUTLIERS = 0; // 10;
  std::string        leastSquaresOutputFileName = "leastSquaresPlaneEstimation.iv";
  std::string        ransacOutputFileName = "RANSACPlaneEstimation.iv";

  typedef itk::PlaneParametersEstimator<DIMENSION>           PlaneEstimatorType;
  typedef itk::RANSAC<itk::Point<double, DIMENSION>, double> RANSACType;

  std::vector<itk::Point<double, DIMENSION>> data;
  std::vector<double>                        truePlaneParameters, planeParameters;
  double                                     outlierDistance = 20.0;
  unsigned int                               i;
  double                                     dotProduct;

  GenerateData<DIMENSION>(INLIERS, OUTLIERS, outlierDistance, data, truePlaneParameters);

  std::cout << "Known (hyper)plane parameters [n,a]\n\t [ ";
  for (i = 0; i < (2 * DIMENSION - 1); i++)
    std::cout << truePlaneParameters[i] << ", ";
  std::cout << truePlaneParameters[i] << "]\n\n";

  // create and initialize the parameter estimator
  double                      maximalDistanceFromPlane = 0.5;
  PlaneEstimatorType::Pointer planeEstimator = PlaneEstimatorType::New();
  planeEstimator->SetDelta(maximalDistanceFromPlane);
  planeEstimator->LeastSquaresEstimate(data, planeParameters);
  if (planeParameters.empty())
    std::cout << "Least squares estimate failed, degenerate configuration?\n";
  else
  {
    std::cout << "Least squares hyper(plane) parameters: [n,a]\n\t [ ";
    for (i = 0; i < (2 * DIMENSION - 1); i++)
      std::cout << planeParameters[i] << ", ";
    std::cout << planeParameters[i] << "]\n\n";
    // cos(theta), theta is the angle between the two unit normals
    dotProduct = 0.0;
    for (i = 0; i < DIMENSION; i++)
      dotProduct += planeParameters[i] * truePlaneParameters[i];
    std::cout << "\tDot product of real and computed normals[+-1=correct]: ";
    std::cout << dotProduct << "\n";
    // distance between known hyper(plane) and estimated point on plane
    dotProduct = 0.0;
    for (i = 0; i < DIMENSION; i++)
      dotProduct += (planeParameters[DIMENSION + i] - truePlaneParameters[DIMENSION + i]) * truePlaneParameters[i];
    std::cout << "\tCheck if computed point is on known plane [0=correct]: ";
    std::cout << dotProduct << "\n\n";
    // save scene file (works only in 3D)
    // SaveOIVFile( leastSquaresOutputFileName, data, planeParameters, planeEstimator );
  }

  // create and initialize the RANSAC algorithm
  double              desiredProbabilityForNoOutliers = 0.999;
  double              percentageOfDataUsed;
  RANSACType::Pointer ransacEstimator = RANSACType::New();
  ransacEstimator->SetData(data);
  ransacEstimator->SetParametersEstimator(planeEstimator);
  percentageOfDataUsed = ransacEstimator->Compute(planeParameters, desiredProbabilityForNoOutliers);
  if (planeParameters.empty())
    std::cout << "RANSAC estimate failed, degenerate configuration?\n";
  else
  {
    std::cout << "RANSAC hyper(plane) parameters: [n,a]\n\t [ ";
    for (i = 0; i < (2 * DIMENSION - 1); i++)
      std::cout << planeParameters[i] << ", ";
    std::cout << planeParameters[i] << "]\n\n";
    // cos(theta), theta is the angle between the two unit normals
    dotProduct = 0.0;
    for (i = 0; i < DIMENSION; i++)
      dotProduct += planeParameters[i] * truePlaneParameters[i];
    std::cout << "\tDot product of real and computed normals[+-1=correct]: ";
    std::cout << dotProduct << "\n";
    // distance between known hyper(plane) and estimated point on plane
    dotProduct = 0.0;
    for (i = 0; i < DIMENSION; i++)
      dotProduct += (planeParameters[DIMENSION + i] - truePlaneParameters[DIMENSION + i]) * truePlaneParameters[i];
    std::cout << "\tCheck if computed point is on known plane [0=correct]: ";
    std::cout << dotProduct << "\n\n";
    std::cout << "\tPercentage of points which were used for final estimate: ";
    std::cout << percentageOfDataUsed << "\n\n";

    // save scene file (works only in 3D)
    // SaveOIVFile( ransacOutputFileName, data, planeParameters, planeEstimator );
  }
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
  itk::Vector<double, dimension> normal, noise, tmp;
  itk::Point<double, dimension>  pointOnPlane, randomPoint;
  double                         noiseStandardDeviation = 0.4; // noise standard deviation
  double                         coordinateMax = 1000.0;
  unsigned int                   i, j;

  RandomNumberGenerator random;

  planeParameters.clear();
  // generate points on random (hyper) plane
  for (i = 0; i < dimension; i++)
  {
    normal[i] = random.uniform();
    pointOnPlane[i] = random.uniform(-coordinateMax, coordinateMax);
  }
  normal.Normalize();
  for (i = 0; i < dimension; i++)
    planeParameters.push_back(normal[i]);
  for (i = 0; i < dimension; i++)
    planeParameters.push_back(pointOnPlane[i]);

  // generate inliers
  for (i = 0; i < numInliers; i++)
  {
    for (j = 0; j < dimension; j++)
    {
      randomPoint[j] = random.uniform(-coordinateMax, coordinateMax);
      noise[j] = random.normal(noiseStandardDeviation);
    }
    // project random point onto the plane and add noise
    tmp = randomPoint - pointOnPlane;
    randomPoint = pointOnPlane + noise + (tmp - (tmp * normal) * normal);
    // randomPoint = pointOnPlane +  (tmp - (tmp * normal) * normal);
    data.push_back(randomPoint);
  }
  // generate outliers (via rejection)
  for (i = 0; i < numOutliers; i++)
  {
    for (j = 0; j < dimension; j++)
    {
      randomPoint[j] = random.uniform(-coordinateMax, coordinateMax);
    }
    tmp = randomPoint - pointOnPlane;
    if (fabs(tmp * normal) >= outlierDistance)
      data.push_back(randomPoint);
    else
      i--;
  }
}

// // typename itk::PlaneParametersEstimator<dimension>::Pointer

// template<unsigned int dimension>
// void SaveOIVFile( std::string &outputFileName,
//                   std::vector< itk::Point<double,dimension> > &data,
//                   std::vector<double> &estimatedPlaneParameters,
//                   parameterEstimatorPointer parameterEstimator )
// {
//   if( dimension != 3 )
//     return;

//   double sphereRadius = 50.0;
//                //outliers are metalic red
//   std::string outlierMaterial = "Material {\n";
//   outlierMaterial+="\tambientColor 1.0 0.0 0.0\n";
//   outlierMaterial+="\tdiffuseColor 0.27 0.15 0.0\n";
//   outlierMaterial+="\tspecularColor 1.0 0.0 0.0\n";
//   outlierMaterial+="}\n";

//                //inliers are metalic green
//   std::string inlierMaterial = "\tMaterial {\n";
//   inlierMaterial+="\t\tambientColor 0.0 1.0 0.0\n";
//   inlierMaterial+="\t\tdiffuseColor 0.0 0.27 0.15\n";
//   inlierMaterial+="\t\tspecularColor 0.0 1.0 0.0\n";
//   inlierMaterial+="\t}\n";

//              //plane is gray
//   std::string planeMaterial = "\tMaterial {\n";
//   planeMaterial+="\t\tambientColor 0.5 0.5 0.5\n";
//   planeMaterial+="\t\tdiffuseColor 0.2 0.2 0.2\n";
//   planeMaterial+="\t\tspecularColor 0.8 0.8 0.8\n";
//   planeMaterial+="\t\ttransparency 0.4\n";
//   planeMaterial+="\t}\n";

//   std::ofstream out( outputFileName.c_str() );

//   out<<"#Inventor V2.1 ascii\n\n";
//                //go over all the points, find the coordinate ranges and write
//                //the outliers and inliers to the scene graph
//   double minX, maxX, minY, maxY, minZ, maxZ;
//   minX = maxX = data[0][0];
//   minY = maxY = data[0][1];
//   minZ = maxZ = data[0][2];
//   unsigned int inlierIndex = 0;
//   for( unsigned int i=0; i<data.size(); i++ ) {
//     out<<"Separator {\n";
//     if( parameterEstimator->Agree( estimatedPlaneParameters, data[i] ) ) {
//       out<<inlierMaterial;
//       inlierIndex = i;
//     }
//     else
//       out<<outlierMaterial;
//     out<<"\tTransform {\n";
//     out<<"\t\ttranslation "<<(data[i])[0]<<" "<<(data[i])[1]<<" "<<(data[i])[2]<<"\n";
//     out<<"\t}\n";
//     out<<"\tSphere {\n";
//     out<<"\t\tradius  "<<sphereRadius<<"\n";
//     out<<"\t}\n";
//     out<<"}\n";
//     if( data[i][0]<minX )
//       minX = data[i][0];
//     else if( data[i][0]>maxX )
//       maxX = data[i][0];
//     if( data[i][1]<minY )
//       minY = data[i][1];
//     else if( data[i][1]>maxY )
//       maxY = data[i][1];
//     if( data[i][2]<minZ )
//       minZ = data[i][2];
//     else if( data[i][2]>maxZ )
//       maxZ = data[i][2];
//   }
//        //create polygon representing estimated plane and write to file
//   double rangeX = maxX - minX;
//   double rangeY = maxY - minY;
//   double rangeZ = maxZ - minZ;
//   double maxRange = rangeX;
//   if( rangeY>maxRange )
//     maxRange = rangeY;
//   if( rangeZ>maxRange )
//     maxRange = rangeZ;
//   double halfRange = sqrt(3.0)*maxRange/2.0;

//   itk::Vector<double, 3> tmp, e1, e2, normal;
//   normal[0] = estimatedPlaneParameters[0];
//   normal[1] = estimatedPlaneParameters[1];
//   normal[2] = estimatedPlaneParameters[2];
//   tmp[0] = data[inlierIndex][0] - estimatedPlaneParameters[3];
//   tmp[1] = data[inlierIndex][1] - estimatedPlaneParameters[4];
//   tmp[2] = data[inlierIndex][2] - estimatedPlaneParameters[5];
//   e1 = tmp - tmp*normal;
//   e1.Normalize();
//   e2 = CrossProduct( normal, e1 );

//   itk::Point<double, 3> p1, p2, p3, p4;
//   p1[0] = p2[0] = p3[0] = p4[0] = estimatedPlaneParameters[3];
//   p1[1] = p2[1] = p3[1] = p4[1] = estimatedPlaneParameters[4];
//   p1[2] = p2[2] = p3[2] = p4[2] = estimatedPlaneParameters[5];

//   p1 = p1 + halfRange*e1;
//   p2 = p2 + halfRange*e2;
//   p3 = p3 - halfRange*e1;
//   p4 = p4 - halfRange*e2;
//   out<<"Separator {\n";
//   out<<planeMaterial;
//   out<<"\tIndexedFaceSet {\n";
//   out<<"\t\tvertexProperty\n";
//   out<<"\t\tVertexProperty {\n";
//   out<<"\t\tvertex [ "<<p1[0]<<" "<<p1[1]<<" "<<p1[2]<<",\n";
//   out<<"\t\t         "<<p2[0]<<" "<<p2[1]<<" "<<p2[2]<<",\n";
//   out<<"\t\t         "<<p3[0]<<" "<<p3[1]<<" "<<p3[2]<<",\n";
//   out<<"\t\t         "<<p4[0]<<" "<<p4[1]<<" "<<p4[2]<<" ]\n";
//   out<<"\t\t}\n";
//   out<<"\t\tcoordIndex [ 0, 1, 2,3 -1 ]\n";
//   out<<"\t}\n";
//   out<<"}\n";

//   out.close();
// }
