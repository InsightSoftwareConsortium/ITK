/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSymmetricEllipsoidInteriorExteriorSpatialFunctionTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkSymmetricEllipsoidInteriorExteriorSpatialFunction.h"

int itkSymmetricEllipsoidInteriorExteriorSpatialFunctionTest(int, char* [] )
{
  std::cout << "itkSymmetricEllipsoidInteriorExteriorSpatialFunction test start" << std::endl;

  // Test will create an ellipsoid (3-dimensional)
  const unsigned int dimension = 3;

  // Symmetric Ellipsoid spatial function typedef
  typedef itk::SymmetricEllipsoidInteriorExteriorSpatialFunction<3> TSymEllipsoidFunctionType;

  // Point position typedef
  typedef TSymEllipsoidFunctionType::InputType TSymEllipsoidFunctionVectorType;

  // Create an ellipsoid spatial function for the source image
  TSymEllipsoidFunctionType::Pointer spatialFunc = TSymEllipsoidFunctionType::New();

  // Define function doitkSymmetricEllipsoidInteriorExteriorSpatialFunctionTest, which encapsulates ellipsoid.
  int xExtent = 50;
  int yExtent = 50;
  int zExtent = 50;

  // Define and set the center of the ellipsoid in the center of
  // the function doitkSymmetricEllipsoidInteriorExteriorSpatialFunctionTest
  TSymEllipsoidFunctionVectorType center;
  center[0] = xExtent/2;
  center[1] = yExtent/2;
  center[2] = zExtent/2;
  spatialFunc->SetCenter(center);

  // Define and set the orientation and axes lengths of the ellipsoid
  // NOTE: Orienation vector must be normalized!!!!
  itk::Vector<double, 3> orientation;
  orientation[0] = 1/sqrt(2.0);
  orientation[1] = 1/sqrt(2.0);
  orientation[2] = 0;

  double uniqueAxisLength = 45;
  double symmetricAxesLength = 30;
 
  spatialFunc->SetOrientation(orientation, uniqueAxisLength, symmetricAxesLength);
 
  // Evaluate all points in the spatial function and count the number of
  // pixels that are inside the sphere.
  double testPosition[dimension];  // position of a pixel in the function doitkSymmetricEllipsoidInteriorExteriorSpatialFunctionTest

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
          interiorPixelCounter++;
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
  double volume = 4.18879013333*(uniqueAxisLength/2)*(symmetricAxesLength/2)*(symmetricAxesLength/2);
  
  // Percent difference in volume measurement and calculation
  double volumeError = (fabs(volume - interiorPixelCounter)/volume)*100; 

  // 5% error was randomly chosen as a successful ellipsoid fill.
  // This should actually be some function of the image/ellipsoid size.
  if(volumeError <= 5 || functionValue == 1)
    {
    // With testing settings, results should yield:
    // calculated ellipsoid volume = 21205.8 pixels
    // measured ellipsoid volume = 21197 pixels
    // volume error = 0.04126%
    // function value = 1
    std::cout << "calculated ellipsoid volume = " << volume << std::endl
              << "measured ellipsoid volume = " << interiorPixelCounter << std::endl
              << "volume error = " << volumeError << "%" << std::endl
              << "function value = " << functionValue << std::endl
              << "center location = (" << spatialFunc->GetCenter()[0] << ", " << spatialFunc->GetCenter()[0]
              << ", " << spatialFunc->GetCenter()[2] << ")" << std::endl
              << "itkSymmetricEllipsoidSpatialFunction ended successfully!" << std::endl;            
    return EXIT_SUCCESS;
    }
  else
    {
    std::cerr << "calculated ellipsoid volume = " << volume << std::endl
              << "measured ellipsoid volume = " << interiorPixelCounter << std::endl
              << "volume error = " << volumeError << "%" << std::endl
              << "function value = " << functionValue << std::endl
              << "center location = (" << spatialFunc->GetCenter()[0] << ", " << spatialFunc->GetCenter()[0]
              << ", " << spatialFunc->GetCenter()[2] << ")" << std::endl
              << "itkSymmetricEllipsoidSpatialFunction failed :(" << std::endl;
    return EXIT_FAILURE;
    }
}
