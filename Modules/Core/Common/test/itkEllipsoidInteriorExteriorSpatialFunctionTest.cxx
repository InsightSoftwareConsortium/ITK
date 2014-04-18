/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/

#include "itkEllipsoidInteriorExteriorSpatialFunction.h"

int itkEllipsoidInteriorExteriorSpatialFunctionTest(int, char* [] )
{
  std::cout << "itkEllipsoidInteriorExteriorSpatialFunction test start" << std::endl;

  // Test will create an ellipsoid (3-dimensional)
  const unsigned int dimension = 3;

  // Ellipsoid spatial function typedef
  typedef itk::EllipsoidInteriorExteriorSpatialFunction<3> TEllipsoidFunctionType;

  // Point position typedef
  typedef TEllipsoidFunctionType::InputType TEllipsoidFunctionVectorType;

  // Create an ellipsoid spatial function for the source image
  TEllipsoidFunctionType::Pointer spatialFunc = TEllipsoidFunctionType::New();
  // Define and set the axes lengths for the ellipsoid
  TEllipsoidFunctionVectorType axes;
  axes[0] = 40;
  axes[1] = 30;
  axes[2] = 20;
  spatialFunc->SetAxes(axes);

  // Define function doitkEllipsoidInteriorExteriorSpatialFunctionTest, which encapsulates ellipsoid.
  int xExtent = 50;
  int yExtent = 50;
  int zExtent = 50;

  // Define and set the center of the ellipsoid in the center of
  // the function doitkEllipsoidInteriorExteriorSpatialFunctionTest
  TEllipsoidFunctionVectorType center;
  center[0] = xExtent/2;
  center[1] = yExtent/2;
  center[2] = zExtent/2;
  spatialFunc->SetCenter(center);

  // Define the orientations of the ellipsoid axes
  // (0,1,0) corresponds to the axes of length axes[0]
  // (1,0,0) corresponds to the axes of length axes[1]
  // (0,0,1) corresponds to the axes of length axes[2]
  double data[] = {0, 1, 0, 1, 0, 0, 0, 0, 1};
  vnl_matrix<double> orientations (data, 3, 3);

  // Set the orientations of the ellipsoids
  spatialFunc->SetOrientations(orientations);

  // Evaluate all points in the spatial function and count the number of
  // pixels that are inside the sphere.
  double testPosition[dimension];  // position of a pixel in the function doitkEllipsoidInteriorExteriorSpatialFunctionTest

  bool functionValue;  // Value of pixel at a given position
  int interiorPixelCounter = 0;  // Count pixels inside ellipsoid

  for(int x = 0; x < xExtent; x++)
    {
    for(int y = 0; y < yExtent; y++)
      {
      for(int z =0; z < zExtent; z++)
        {
        testPosition[0] = x;
        testPosition[1] = y;
        testPosition[2] = z;
        functionValue = spatialFunc->Evaluate(testPosition);
        if(functionValue == 1)
          interiorPixelCounter ++;
        }
      }
    }

  // Evaluate the center of the ellipsoid, which is inside the ellipsoid and
  // should equal 1.
  testPosition[0] = center[0];
  testPosition[1] = center[1];
  testPosition[2] = center[2];
  functionValue = spatialFunc->Evaluate(testPosition);

  // Volume of ellipsoid using V=(4/3)*pi*(a/2)*(b/2)*(c/2)
  double volume = 4.18879013333*(axes[0]/2)*(axes[1]/2)*(axes[2]/2);

  // Percent difference in volume measurement and calculation
  double volumeError = (std::fabs(volume - interiorPixelCounter)/volume)*100;

  std::cout << spatialFunc;

  // 5% error was randomly chosen as a successful ellipsoid fill.
  // This should actually be some function of the image/ellipsoid size.
  if(volumeError <= 5 || functionValue == 1)
    {

    // With testing settings, results should yield:
    // calculated ellipsoid volume = 12566.4 pixels
    // measured ellipsoid volume = 12428 pixels
    // volume error = 1.10907%
    // function value = 1
    std::cout << "calculated ellipsoid volume = " << volume << std::endl
              << "measured ellipsoid volume = " << interiorPixelCounter << std::endl
              << "volume error = " << volumeError << "%" << std::endl
              << "function value = " << functionValue << std::endl
              << "center location = (" << spatialFunc->GetCenter()[0] << ", " << spatialFunc->GetCenter()[0]
              << ", " << spatialFunc->GetCenter()[2] << ")" << std::endl
              << "major axis length = " << spatialFunc->GetAxes()[0] << " minor axis 1 length = "
              << spatialFunc->GetAxes()[1] << " minor axis 2 length = " << spatialFunc->GetAxes()[2] << std::endl
              << "itkEllipsoidSpatialFunction ended successfully!" << std::endl;
    return EXIT_SUCCESS;
    }
  //Default behavior is to fail
  std::cerr << "calculated ellipsoid volume = " << volume << std::endl
              << "measured ellipsoid volume = " << interiorPixelCounter << std::endl
              << "volume error = " << volumeError << "%" << std::endl
              << "function value = " << functionValue << std::endl
              << "center location = (" << spatialFunc->GetCenter()[0] << ", " << spatialFunc->GetCenter()[0]
              << ", " << spatialFunc->GetCenter()[2] << ")" << std::endl
              << "major axis length = " << spatialFunc->GetAxes()[0] << " minor axis 1 length = "
              << spatialFunc->GetAxes()[1] << " minor axis 2 length = " << spatialFunc->GetAxes()[2] << std::endl
              << "itkEllipsoidSpatialFunction failed :(" << std::endl;
    return EXIT_FAILURE;
}
