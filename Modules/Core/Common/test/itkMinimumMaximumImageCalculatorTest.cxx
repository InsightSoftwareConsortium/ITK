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

#include "itkMinimumMaximumImageCalculator.h"
#include "itkObject.h"
#include "itkTestingMacros.h"

int itkMinimumMaximumImageCalculatorTest( int, char *[] )
{

  typedef itk::Size< 3 >                                  SizeType;
  typedef itk::Image< short, 3 >                          ImageType;

  typedef itk::MinimumMaximumImageCalculator<ImageType>   MinMaxCalculatorType;

  /* Define the image size and physical coordinates */
  SizeType size = {{20, 20, 20}};
  double origin [3] = {0.0, 0.0, 0.0};
  double spacing[3] = {1, 1, 1};

  std::cout << "Testing Minimum and Maximum Image Calulator:\n";

  // Allocate a simple test image
  ImageType::Pointer image = ImageType::New();
  ImageType::RegionType region;
  region.SetSize(size);
  image->SetLargestPossibleRegion(region);
  image->SetRequestedRegion(region);
  image->SetBufferedRegion(region);
  image->Allocate();

  // Set origin and spacing of physical coordinates
  image->SetOrigin(origin);
  image->SetSpacing(spacing);

  short minimum = -52;
  short maximum = 103;

  // Initialize the image contents with the minimum value
  itk::Index<3> index;
  for (int slice = 0; slice < 20; ++slice) {
      index[2] = slice;
      for (int row = 0; row < 20; ++row) {
          index[1] = row;
          for (int col = 0; col < 20; ++col) {
              index[0] = col;
              image->SetPixel(index, minimum);
          }
      }
  }

  // The minimum intensity index position will contain a single value:
  // since all pixels have equal value, it will be ther first pixel
  itk::Index<3> minIntensityValueIndex;
  minIntensityValueIndex[0] = 0;
  minIntensityValueIndex[1] = 0;
  minIntensityValueIndex[2] = 0;

  // Set voxel (10,10,10) to maximum value
  itk::Index<3> maxIntensityValueIndex;
  maxIntensityValueIndex[0] = 10;
  maxIntensityValueIndex[1] = 10;
  maxIntensityValueIndex[2] = 10;
  image->SetPixel(maxIntensityValueIndex, maximum);

  // Create and initialize the calculator
  MinMaxCalculatorType::Pointer calculator = MinMaxCalculatorType::New();

  EXERCISE_BASIC_OBJECT_METHODS( calculator, MinimumMaximumImageCalculator, Object );

  calculator->SetImage( image );
  calculator->Compute();

  // Test minimum of intensity
  TEST_SET_GET_VALUE( minimum, calculator->GetMinimum() );
  TEST_SET_GET_VALUE( minIntensityValueIndex, calculator->GetIndexOfMinimum() );

  // Test maximum of intensity
  TEST_SET_GET_VALUE( maximum, calculator->GetMaximum() );
  TEST_SET_GET_VALUE( maxIntensityValueIndex, calculator->GetIndexOfMaximum() );

  // Set the region over which perform the computations
  itk::Size<3>  regionSize = {{4, 4, 4}};
  itk::Index<3> idx = {{0, 0, 0}};
  MinMaxCalculatorType::RegionType computationRegion;
  computationRegion.SetSize( regionSize );
  computationRegion.SetIndex( idx );

  calculator->SetRegion( computationRegion );

  minimum = -102;
  maximum = 800;

  // Set an index inside the region set
  maxIntensityValueIndex[0] = 2;
  maxIntensityValueIndex[1] = 2;
  maxIntensityValueIndex[2] = 2;

  image->SetPixel( minIntensityValueIndex, minimum );
  image->SetPixel( maxIntensityValueIndex, maximum );

  calculator->SetImage( image );

  calculator->ComputeMinimum();

  // Test minimum of intensity
  TEST_SET_GET_VALUE( minimum, calculator->GetMinimum() );
  TEST_SET_GET_VALUE( minIntensityValueIndex, calculator->GetIndexOfMinimum() );

  calculator->ComputeMaximum();

  // Test maximum of intensity
  TEST_SET_GET_VALUE( maximum, calculator->GetMaximum() );
  TEST_SET_GET_VALUE( maxIntensityValueIndex, calculator->GetIndexOfMaximum() );


  return EXIT_SUCCESS;
}
