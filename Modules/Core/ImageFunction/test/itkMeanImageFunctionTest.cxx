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


#include "itkMeanImageFunction.h"

int itkMeanImageFunctionTest(int, char* [] )
{

  const unsigned int Dimension = 3;
  typedef uint8_t   PixelType;

  typedef itk::Image< PixelType, Dimension > ImageType;
  typedef itk::MeanImageFunction< ImageType > FunctionType;

  // Create and allocate the image
  ImageType::Pointer      image = ImageType::New();
  ImageType::SizeType     size;
  ImageType::IndexType    start;
  ImageType::RegionType   region;

  size[0] = 50;
  size[1] = 50;
  size[2] = 50;

  start.Fill( 0 );

  region.SetIndex( start );
  region.SetSize( size );

  image->SetRegions( region );
  image->Allocate();

  ImageType::PixelType initialValue = 27;

  image->FillBuffer( initialValue );

  FunctionType::Pointer function = FunctionType::New();

  function->SetInputImage( image );

  function->SetNeighborhoodRadius( 5 );

  ImageType::IndexType    index;

  index[0] = 25;
  index[1] = 25;
  index[2] = 25;

  FunctionType::OutputType  mean;

  mean = function->EvaluateAtIndex( index );
  std::cout << "function->EvaluateAtIndex( index ): " << mean << std::endl;

  // Test Evaluate
  FunctionType::PointType point;
  point[0] = 25;
  point[1] = 25;
  point[2] = 25;
  FunctionType::OutputType mean2;
  mean2 = function->Evaluate(point);
  std::cout << "function->Evaluate(point): " << mean2 << std::endl;

  // Test EvaluateAtContinuousIndex
  FunctionType::ContinuousIndexType cindex;
  cindex[0] = 25;
  cindex[1] = 25;
  cindex[2] = 25;
  FunctionType::OutputType mean3;
  mean3 = function->EvaluateAtContinuousIndex(cindex);
  std::cout << "function->EvaluateAtContinuousIndex(cindex): " << mean3 << std::endl;

  // Test GetConstReferenceMacro
  const unsigned int & neighborhoodRadius = function->GetNeighborhoodRadius();
  std::cout << "function->GetNeighborhoodRadius(): " << neighborhoodRadius << std::endl;


  // since the input image is constant
  // the should be equal to the initial value
  if( vnl_math_abs( initialValue - mean ) > 10e-7 )
    {
    std::cerr << "Error in mean computation" << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "Test PASSED ! " << std::endl;
  return EXIT_SUCCESS;

}

