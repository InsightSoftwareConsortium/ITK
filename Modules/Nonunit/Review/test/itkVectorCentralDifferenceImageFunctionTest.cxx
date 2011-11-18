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

#include "itkVectorCentralDifferenceImageFunction.h"
#include "itkImageRegionIterator.h"

int itkVectorCentralDifferenceImageFunctionTest(int, char* [] )
{
  const unsigned int ImageDimension = 2;
  const unsigned int VectorDimension = 3;
  typedef itk::Vector<short,VectorDimension>   PixelType;
  typedef itk::Image<PixelType,ImageDimension> ImageType;

  ImageType::Pointer image = ImageType::New();
  ImageType::SizeType size;
  size.Fill( 16 );
  ImageType::RegionType region( size );

  image->SetRegions( region );
  image->Allocate();

  // make a test image
  typedef itk::ImageRegionIterator<ImageType> Iterator;
  Iterator iter( image, region );
  iter.GoToBegin();
  unsigned int counter = 0;

  while ( !iter.IsAtEnd() )
    {
    iter.Set( counter++ );
    ++iter;
    }

  // set up central difference calculator
  typedef float CoordRepType;
  typedef itk::VectorCentralDifferenceImageFunction<ImageType,CoordRepType> FunctionType;
  FunctionType::Pointer function = FunctionType::New();

  function->SetInputImage( image );

  ImageType::IndexType index;

  // pick an index inside the image
  index.Fill( 8 );
  std::cout << "Index: " << index << " Derivative:" << std::endl;
  std::cout << function->EvaluateAtIndex( index ) << std::endl;

  if ( function->IsInsideBuffer( index ) )
    {
    std::cout << "Index: " << index << " is inside the BufferedRegion." << std::endl;
    }

  FunctionType::ContinuousIndexType cindex;
  cindex.Fill( 8.0 );
  std::cout << "ContinuousIndex: " << cindex << " Derivative:" << std::endl;
  std::cout << function->EvaluateAtContinuousIndex( cindex ) << std::endl;

  FunctionType::PointType point;
  point.Fill( 8.0 );
  std::cout << "Point: " << cindex << " Derivative:" << std::endl;
  std::cout << function->Evaluate( point ) << std::endl;

  // pick an index on the image edge
  index.Fill( 8 );
  index[0] = 15;
  std::cout << "Index: " << index << " Derivative:" << std::endl;
  std::cout << function->EvaluateAtIndex( index ) << std::endl;

  if ( function->IsInsideBuffer( index ) )
    {
    std::cout << "Index: " << index << " is inside the BufferedRegion." << std::endl;
    }

  cindex.Fill( 8.0 );
  cindex[0] = 15.0;
  std::cout << "ContinuousIndex: " << cindex << " Derivative:" << std::endl;
  std::cout << function->EvaluateAtContinuousIndex( cindex ) << std::endl;

  point.Fill( 8.0 );
  point[0] = 15.0;
  std::cout << "Point: " << cindex << " Derivative:" << std::endl;
  std::cout << function->Evaluate( point ) << std::endl;


  std::cout << "Test passed." << std::endl;
  return EXIT_SUCCESS;

}
