/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSymmetricEllipsoidInteriorExteriorSpatialFunctionTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

=========================================================================*/
#include "itkSymmetricEllipsoidInteriorExteriorSpatialFunction.h"

int main()
{
  std::cout << "itkSymmetricEllipsoidInteriorExteriorSpatialFunction test start" << std::endl;

  // Test will create an ellipsoid (3-dimensional)
  const unsigned int dimension = 3;

  // Symmetric Ellipsoid spatial function typedef
  typedef itk::SymmetricEllipsoidInteriorExteriorSpatialFunction<double, 3> TSymEllipsoidFunctionType;

  // Point position typedef
  typedef TSymEllipsoidFunctionType::TPositionType TSymEllipsoidFunctionVectorType;

  // Create an ellipsoid spatial function for the source image
  TSymEllipsoidFunctionType::Pointer spatialFunc = TSymEllipsoidFunctionType::New();

  // Define function domain, which encapsulates ellipsoid.
  int xExtent = 50;
  int yExtent = 50;
  int zExtent = 50;

  // Define and set the center of the ellipsoid in the center of
  // the function domain
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
  double testPosition[dimension];  // position of a pixel in the function domain

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
              << "itkSymmetricEllipsoidSpatialFunction ended successfully!" << std::endl;            
    return EXIT_SUCCESS;
    }
  else
    {
    std::cerr << "calculated ellipsoid volume = " << volume << std::endl
              << "measured ellipsoid volume = " << interiorPixelCounter << std::endl
              << "volume error = " << volumeError << "%" << std::endl
              << "function value = " << functionValue << std::endl
              << "itkSymmetricEllipsoidSpatialFunction failed :(" << std::endl;
    return EXIT_FAILURE;
    }
}
