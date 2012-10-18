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

template<unsigned int vecLength>
int itkCentralDifferenceImageFunctionOnVectorSpeedTestRun(char* argv[] )
{
  int imageSize = atoi( argv[1] );
  int reps = atoi( argv[2] );
  bool doEAI = atoi( argv[3] );
  bool doEACI = atoi( argv[4] );
  bool doE = atoi( argv[5] );

  std::cout << "imageSize: " << imageSize << " reps: " << reps << " doEAI, doEACI, doE: " << doEAI << ", " << doEACI << ", " << doE << std::endl;
  std::cout << "vecLength: " << vecLength << std::endl;

  const unsigned int                            ImageDimension = 2;
  typedef itk::Vector<float,vecLength>          PixelType;
  typedef itk::Image<PixelType,ImageDimension>  ImageType;

  typename ImageType::Pointer image = ImageType::New();
  typename ImageType::SizeType size;
  size.Fill( imageSize );
  typename ImageType::RegionType region( size );

  image->SetRegions( region );
  image->Allocate();

  // make a test image
  typedef itk::ImageRegionIteratorWithIndex<ImageType> Iterator;
  Iterator iter( image, region );
  iter.GoToBegin();
  unsigned int counter = 0;

  while ( !iter.IsAtEnd() )
    {
    PixelType pix;
    for( unsigned int n=0; n < vecLength; n++ )
      {
      pix[n] = counter; //(n+1) + counter;
      }
    iter.Set( pix );
    ++counter;
    ++iter;
    }

  // set up central difference calculator
  typedef float                             CoordRepType;
  typedef itk::Matrix<double,vecLength,2>   DerivativeType;

  typedef itk::CentralDifferenceImageFunction<ImageType,CoordRepType,DerivativeType>  FunctionType;
  typedef typename FunctionType::OutputType                                           OutputType;

  typename FunctionType::Pointer function = FunctionType::New();

  function->SetInputImage( image );

  typename ImageType::IndexType index;

  OutputType indexOutput;
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
        indexOutput = function->EvaluateAtIndex( index );
        total += indexOutput;
        }

      if( doEACI )
        {
        typename FunctionType::ContinuousIndexType cindex;
        cindex[0] = index[0] + 0.1;
        cindex[1] = index[1] + 0.1;
        OutputType continuousIndexOutput = function->EvaluateAtContinuousIndex( cindex );
        total += continuousIndexOutput;
        }

      if( doE )
        {
        typename FunctionType::PointType point;
        image->TransformIndexToPhysicalPoint( index, point );
        OutputType pointOutput = function->Evaluate( point );
        total += pointOutput;
        }

      ++iter;
      }
    }

  return EXIT_SUCCESS;
}


int itkCentralDifferenceImageFunctionOnVectorSpeedTest(int argc, char* argv[] )
{
  if( argc != 7 )
    {
    std::cerr << "usage: size reps doEAI doEACI doE vecLength" << std::endl;
    return EXIT_FAILURE;
    }
  int vecLength = atoi( argv[6] );

  switch( vecLength )
    {
    case 1:
      itkCentralDifferenceImageFunctionOnVectorSpeedTestRun<1>(argv);
      break;
    case 2:
      itkCentralDifferenceImageFunctionOnVectorSpeedTestRun<2>(argv);
      break;
    case 3:
      itkCentralDifferenceImageFunctionOnVectorSpeedTestRun<3>(argv);
      break;
    case 4:
      itkCentralDifferenceImageFunctionOnVectorSpeedTestRun<4>(argv);
      break;
    case 5:
      itkCentralDifferenceImageFunctionOnVectorSpeedTestRun<5>(argv);
      break;
    case 6:
      itkCentralDifferenceImageFunctionOnVectorSpeedTestRun<6>(argv);
      break;
    case 7:
      itkCentralDifferenceImageFunctionOnVectorSpeedTestRun<7>(argv);
      break;
    case 8:
      itkCentralDifferenceImageFunctionOnVectorSpeedTestRun<8>(argv);
      break;
    case 9:
      itkCentralDifferenceImageFunctionOnVectorSpeedTestRun<9>(argv);
      break;
    case 10:
      itkCentralDifferenceImageFunctionOnVectorSpeedTestRun<10>(argv);
      break;
    default:
      std::cout << "Invalid vecLength" << std::endl;
      break;
    }
  return EXIT_SUCCESS;
}
