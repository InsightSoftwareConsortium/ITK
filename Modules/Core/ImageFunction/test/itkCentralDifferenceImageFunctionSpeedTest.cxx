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

#include "itkCentralDifferenceImageFunction.h"
#include "itkImageRegionIteratorWithIndex.h"

int itkCentralDifferenceImageFunctionSpeedTest(int argc, char* argv[] )
{
  if( argc != 6 )
    {
    std::cerr << "usage: size reps doEAI doEACI doE" << std::endl;
    return EXIT_FAILURE;
    }

  int imageSize = atoi( argv[1] );
  int reps = atoi( argv[2] );
  bool doEAI = atoi( argv[3] );
  bool doEACI = atoi( argv[4] );
  bool doE = atoi( argv[5] );

  std::cout << "imageSize: " << imageSize << " reps: " << reps << " doEAI, doEACI, doE: " << doEAI << ", " << doEACI << ", " << doE << std::endl;

  const unsigned int                            ImageDimension = 2;
  typedef unsigned int                          PixelType;
  typedef itk::Image<PixelType,ImageDimension>  ImageType;

  ImageType::Pointer image = ImageType::New();
  ImageType::SizeType size;
  size.Fill( imageSize );
  ImageType::RegionType region( size );

  image->SetRegions( region );
  image->Allocate();

  // make a test image
  typedef itk::ImageRegionIteratorWithIndex<ImageType> Iterator;
  Iterator iter( image, region );
  iter.GoToBegin();
  unsigned int counter = 0;

  while ( !iter.IsAtEnd() )
    {
    iter.Set( counter );
    ++counter;
    ++iter;
    }

  // set up central difference calculator
  typedef float CoordRepType;
  typedef itk::CentralDifferenceImageFunction<ImageType,CoordRepType> FunctionType;
  typedef FunctionType::OutputType  OutputType;

  FunctionType::Pointer function = FunctionType::New();

  function->SetInputImage( image );

  ImageType::IndexType index;

  OutputType total;
  total.Fill( 0 );

  std::cout << "UseImageDirection: " << function->GetUseImageDirection() << std::endl;

  ///loop
  for( int l=0; l < reps; l++ )
    {
    iter.GoToBegin();
    while( !iter.IsAtEnd() )
      {
      index = iter.GetIndex();
      if( doEAI )
        {
        OutputType indexOutput = function->EvaluateAtIndex( index );
        total += indexOutput;
        }

      if( doEACI )
        {
        FunctionType::ContinuousIndexType cindex;
        cindex[0] = index[0] + 0.1;
        cindex[1] = index[1] + 0.1;
        OutputType continuousIndexOutput = function->EvaluateAtContinuousIndex( cindex );
        total += continuousIndexOutput;
        }

      if( doE )
        {
        FunctionType::PointType point;
        image->TransformIndexToPhysicalPoint( index, point );
        OutputType pointOutput = function->Evaluate( point );
        total += pointOutput;
        }

      ++iter;
      }
    }
  std::cout << "total: " << total << std::endl;

  return EXIT_SUCCESS;
}
