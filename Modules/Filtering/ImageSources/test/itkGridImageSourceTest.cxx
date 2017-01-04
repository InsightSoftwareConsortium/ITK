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
#include "itkGridImageSource.h"
#include "itkImageFileWriter.h"
#include "itkBSplineKernelFunction.h"
#include "itkSimpleFilterWatcher.h"
#include "itkTestingMacros.h"


int itkGridImageSourceTest( int argc, char *argv[] )
{
  if ( argc != 12 )
    {
    std::cout << "Usage: " << argv[0]
      << " outputImage"
      << " imageSize"
      << " sigma"
      << " variableSigma"
      << " gridSpacing"
      << " variableGridSpacing"
      << " gridOffset"
      << " gridAllDimensions"
      << " toggleLastGridDimension"
      << " useBSplineKernel"
      << " bSplineOrder" << std::endl;
    return EXIT_FAILURE;
    }


  const unsigned int  ImageDimension = 3;
  typedef float       PixelType;

  typedef itk::Image< PixelType, ImageDimension > ImageType;

  // Instantiate the filter
  typedef itk::GridImageSource< ImageType > GridSourceType;
  GridSourceType::Pointer gridImage = GridSourceType::New();

  EXERCISE_BASIC_OBJECT_METHODS( gridImage, GridImageSource, GenerateImageSource );


  // Specify image parameters
  ImageType::SizeValueType size =
    static_cast< ImageType::SizeValueType >( atof( argv[2] ) );
  ImageType::SizeType imageSize;
  imageSize.Fill( size );

  ImageType::PointType origin;
  origin.Fill( 0.0 );

  ImageType::SpacingType imageSpacing;
  imageSpacing.Fill( 1.0 );

  ImageType::DirectionType direction;
  direction.SetIdentity();

  gridImage->SetSize( imageSize );
  gridImage->SetSpacing( imageSpacing );
  gridImage->SetOrigin( origin );
  gridImage->SetDirection( direction );


  // Specify grid parameters
  double scale = 255.0;
  gridImage->SetScale( scale );
  TEST_SET_GET_VALUE( scale, gridImage->GetScale() );


  GridSourceType::ArrayType::ValueType sigmaValue =
    static_cast< GridSourceType::ArrayType::ValueType >( atof( argv[3] ) );
  GridSourceType::ArrayType sigma;
  sigma.Fill( sigmaValue );
  bool variableSigma = static_cast< bool >( atoi( argv[4] ) );

  if( variableSigma )
    {
    if( sigma.Size() > 2 )
      {
      sigma[1] = sigma[0] + 4.0;
      sigma[2] = sigma[0] + 5.0;
      }
    }
  gridImage->SetSigma( sigma );
  TEST_SET_GET_VALUE( sigma, gridImage->GetSigma() );


  GridSourceType::ArrayType::ValueType spacing =
    static_cast< GridSourceType::ArrayType::ValueType >( atof( argv[5] ) );
  GridSourceType::ArrayType gridSpacing;
  gridSpacing.Fill( spacing );


  bool variableGridSpacing = static_cast< bool >( atoi( argv[6] ) );
  if( variableGridSpacing )
    {
    for( unsigned int i = 0; i < gridSpacing.Size(); ++i )
      {
      gridSpacing[i] = gridSpacing[0] / 2.0;
      }
    }
  gridImage->SetGridSpacing( gridSpacing );
  TEST_SET_GET_VALUE( gridSpacing, gridImage->GetGridSpacing() );


  GridSourceType::ArrayType::ValueType offset =
    static_cast< GridSourceType::ArrayType::ValueType >( atof( argv[7] ) );
  GridSourceType::ArrayType gridOffset;
  gridOffset.Fill( offset );
  gridImage->SetGridOffset( gridOffset );
  TEST_SET_GET_VALUE( gridOffset, gridImage->GetGridOffset() );


  bool gridAllDimensions = static_cast< bool >( atoi( argv[8] ) );
  GridSourceType::BoolArrayType whichDimension;
  whichDimension.Fill( gridAllDimensions );

  bool toggleLastGridDimension = atof( argv[9] );
  if( toggleLastGridDimension )
    {
    whichDimension[ImageDimension - 1] = !gridAllDimensions;
    }
  gridImage->SetWhichDimensions( whichDimension );
  TEST_SET_GET_VALUE( whichDimension, gridImage->GetWhichDimensions() );


  bool useBSplineKernel = static_cast< bool >( atoi( argv[10] ) );
  if( useBSplineKernel )
    {
    unsigned int bSplineOrder = atoi( argv[11] );
    // Specify B-Spline function
    if( bSplineOrder == 3 )
      {
      typedef itk::BSplineKernelFunction< 3 > KernelType;
      KernelType::Pointer kernel = KernelType::New();
      gridImage->SetKernelFunction( kernel );
      }
    else
      {
      typedef itk::BSplineKernelFunction< 0 > KernelType;
      KernelType::Pointer kernel = KernelType::New();
      gridImage->SetKernelFunction( kernel );
      }
    }


  itk::SimpleFilterWatcher watcher( gridImage, "GridImageSource" );

  TRY_EXPECT_NO_EXCEPTION( gridImage->Update() );

  typedef itk::ImageFileWriter<ImageType> WriterType;
  WriterType::Pointer writer = WriterType::New();
  writer->SetFileName( argv[1] );
  writer->SetInput( gridImage->GetOutput() );

  TRY_EXPECT_NO_EXCEPTION( writer->Update() );

  std::cout << "Test finished" << std::endl;
  return EXIT_SUCCESS;
}
