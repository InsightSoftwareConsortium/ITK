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

#include "itkFiniteCylinderSpatialFunction.h"
#include "itkTestingMacros.h"

int
itkFiniteCylinderSpatialFunctionTest(int, char *[])
{
  std::cout << "itkFiniteCylinderSpatialFunction test start" << std::endl;

  // Test will create a cylinder (3 - dimensional)
  constexpr unsigned int dimension = 3;

  // Cylinder spatial function type alias.
  using TCylinderFunctionType = itk::FiniteCylinderSpatialFunction<dimension>;
  using TCylinderFunctionVectorType = TCylinderFunctionType::InputType;

  // cylinder
  auto spatialFunc = TCylinderFunctionType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(spatialFunc, FiniteCylinderSpatialFunction, InteriorExteriorSpatialFunction);


  // Test exceptions
  TCylinderFunctionVectorType orientation;
  orientation[0] = 0.0;
  orientation[1] = 0.0;
  orientation[2] = 0.0;
  ITK_TRY_EXPECT_EXCEPTION(spatialFunc->SetOrientation(orientation));

  double axis = 40.0;
  spatialFunc->SetAxisLength(axis);
  ITK_TEST_SET_GET_VALUE(axis, spatialFunc->GetAxisLength());

  // Define function, which encapsulates cylinder.
  int xExtent = 50;
  int yExtent = 50;
  int zExtent = 50;

  TCylinderFunctionVectorType center;
  center[0] = xExtent / 2;
  center[1] = yExtent / 2;
  center[2] = zExtent / 2;
  spatialFunc->SetCenter(center);
  ITK_TEST_SET_GET_VALUE(center, spatialFunc->GetCenter());

  orientation[0] = .35;
  orientation[1] = .35;
  orientation[2] = .30;
  spatialFunc->SetOrientation(orientation);
  ITK_TEST_SET_GET_VALUE(orientation, spatialFunc->GetOrientation());

  double radius = 5.0;
  spatialFunc->SetRadius(radius);
  ITK_TEST_SET_GET_VALUE(radius, spatialFunc->GetRadius());

  // Evaluate all points in the spatial function and count the number of
  // pixels that are inside the cylinder.
  double testPosition[dimension]; // position of a pixel

  bool functionValue;            // Value of pixel at a given position
  int  interiorPixelCounter = 0; // Count pixels inside cylinder

  for (int x = 0; x < xExtent; ++x)
  {
    for (int y = 0; y < yExtent; ++y)
    {
      for (int z = 0; z < zExtent; ++z)
      {
        testPosition[0] = x;
        testPosition[1] = y;
        testPosition[2] = z;
        functionValue = spatialFunc->Evaluate(testPosition);
        if (functionValue == 1)
        {
          interiorPixelCounter++;
        }
      }
    }
  }

  // Evaluate the center of the ellipsoid, which is inside the ellipsoid and
  // should equal 1.
  testPosition[0] = center[0];
  testPosition[1] = center[1];
  testPosition[2] = center[2];
  functionValue = spatialFunc->Evaluate(testPosition);

  // Volume of cylinder using V=pi*r^2*h
  double volume = 3.14159 * pow(radius, 2) * axis;

  // Percent difference in volume measurement and calculation
  double volumeError = (itk::Math::abs(volume - interiorPixelCounter) / volume) * 100;

  std::cout << spatialFunc;

  // 5% error was randomly chosen as a successful ellipsoid fill.
  // This should actually be some function of the image/ellipsoid size.
  if (volumeError <= 7.0 && functionValue == 1)
  {

    // With testing settings, results should yield:
    // calculated ellipsoid volume = 12566.4 pixels
    // measured ellipsoid volume = 12428 pixels
    // volume error = 1.10907%
    // function value = 1
    std::cout << "calculated cylinder volume = " << volume << std::endl
              << "measured cylinder volume = " << interiorPixelCounter << std::endl
              << "volume error = " << volumeError << "%" << std::endl
              << "function value = " << functionValue << std::endl
              << "center location = (" << spatialFunc->GetCenter()[0] << ", " << spatialFunc->GetCenter()[0] << ", "
              << spatialFunc->GetCenter()[2] << ")" << std::endl
              << "axis length = " << axis << std::endl
              << "itkFiniteCylinderSpatialFunction test ended successfully!" << std::endl;
    return EXIT_SUCCESS;
  }
  // Default is to produce error code
  std::cerr << "calculated ellipsoid volume = " << volume << std::endl
            << "measured ellipsoid volume = " << interiorPixelCounter << std::endl
            << "volume error = " << volumeError << "%" << std::endl
            << "function value = " << functionValue << std::endl
            << "center location = (" << spatialFunc->GetCenter()[0] << ", " << spatialFunc->GetCenter()[0] << ", "
            << spatialFunc->GetCenter()[2] << ")" << std::endl
            << "axis length = " << axis << std::endl
            << "itkFiniteCylinderSpatialFunction test failed :(" << std::endl;
  return EXIT_FAILURE;
}
