/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkEllipsoidInteriorExteriorSpatialFunctionTest.cxx
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
#include "itkEllipsoidInteriorExteriorSpatialFunction.h"

int main()
{
  std::cerr << "itkEllipsoidInteriorExteriorSpatialFunction test start" << std::endl;

  // Test will create an ellipsoid (3-dimensional)
  const unsigned int dimension = 3;

  // Ellipsoid spatial function typedef
  typedef itk::EllipsoidInteriorExteriorSpatialFunction<double, 3> TEllipsoidFunctionType;

  // Point position typedef
  typedef TEllipsoidFunctionType::TPositionType TEllipsoidFunctionVectorType;

  // Create an ellipsoid spatial function for the source image
  TEllipsoidFunctionType::Pointer spatialFunc = TEllipsoidFunctionType::New();
  // Define and set the axes lengths for the ellipsoid
  TEllipsoidFunctionVectorType axes;
  axes[0] = 40;
  axes[1] = 30;
  axes[2] = 20;
  spatialFunc->SetAxes(axes);

  // Define function domain, which encapsulates ellipsoid.
  int xExtent = 50;
  int yExtent = 50;
  int zExtent = 50;

  // Define and set the center of the ellipsoid in the center of
  // the function domain
  TEllipsoidFunctionVectorType center;
  center[0] = xExtent/2;
  center[1] = yExtent/2;
  center[2] = zExtent/2;
  spatialFunc->SetCenter(center);

  // Define the orientations of the ellipsoid axes
  // (0,1,0) corresponds to the axes of length axes[0]
  // (1,0,0) corresponds to the axes of length axes[1]
  // (0,0,1) corresponds to the axes of lenght axes[2]
  double data[] = {0, 1, 0, 1, 0, 0, 0, 0, 1};
  vnl_matrix<double> orientations (data, 3, 3);  
  
  // Set the orientations of the ellipsoids
  spatialFunc->SetOrientations(orientations);
 
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
  std::cerr << "funVal = " << functionValue << std::endl;

  
  // Volume of ellipsoid using V=(4/3)*pi*a*b*c
  double volume = 4.18879013333*(axes[0]/2)*(axes[1]/2)*(axes[2]/2);  
  
  // Percent difference in volume measurement and calculation
  double volumeError = (fabs(volume - interiorPixelCounter)/volume)*100; 

  // 5% error was randomly chosen as a successful ellipsoid fill.
  // This should actually be some function of the image/ellipsoid size.
  if(volumeError <= 5 && functionValue == 1)
    {
    std::cerr << "calculated ellipsoid volume = " << volume << std::endl
              << "measured ellipsoid volume = " << interiorPixelCounter << std::endl
              << "volume error = " << volumeError << "%" << std::endl
              << "function value = " << functionValue << std::endl
              << "itkEllipsoidSpatialFunction ended successfully!" << std::endl;            
    return EXIT_SUCCESS;
    }
  else
    {
    std::cerr << "calculated ellipsoid volume = " << volume << std::endl
              << "measured ellipsoid volume = " << interiorPixelCounter << std::endl
              << "volume error = " << volumeError << "%" << std::endl
              << "function value = " << functionValue << std::endl
              << "itkEllipsoidSpatialFunction failed :(" << std::endl;
    return EXIT_FAILURE;
    }
}
