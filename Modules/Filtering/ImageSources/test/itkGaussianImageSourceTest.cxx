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

#include "itkFilterWatcher.h"
#include "itkImageFileWriter.h"
#include "itkGaussianImageSource.h"
#include "itkTestingMacros.h"
#include "itkMath.h"

int itkGaussianImageSourceTest( int argc, char* argv[] )
{
  if ( argc < 3 )
    {
    std::cout << "Usage: " << argv[0]
      << " outputImage normalized" << std::endl;
    return EXIT_FAILURE;
    }

  const unsigned int    Dimension = 3;
  typedef unsigned char PixelType;

  typedef itk::Image< PixelType, Dimension > ImageType;

  // Create a Gaussian image source
  typedef itk::GaussianImageSource< ImageType > GaussianSourceType;
  GaussianSourceType::Pointer gaussianImage = GaussianSourceType::New();

  ImageType::SpacingValueType spacing[] = { 1.2f, 1.3f, 1.4f };
  ImageType::PointValueType origin[] = { 1.0f, 4.0f, 2.0f };
  ImageType::SizeValueType size[] = { 130, 150, 120 };
  ImageType::DirectionType direction;
  direction.SetIdentity();

  GaussianSourceType::ArrayType mean;
  mean[0] = size[0]/2.0f + origin[0];
  mean[1] = size[1]/2.0f + origin[1];
  mean[2] = size[2]/2.0f + origin[2];

  GaussianSourceType::ArrayType sigma;
  sigma[0] = 25.0f;
  sigma[1] = 35.0f;
  sigma[2] = 55.0f;

  gaussianImage->SetSize( size );
  for( unsigned int i = 0; i < Dimension; ++i)
    {
    TEST_SET_GET_VALUE( size[i], gaussianImage->GetSize()[i] );
    }

  gaussianImage->SetOrigin( origin );
  for( unsigned int i = 0; i < Dimension; ++i)
    {
    TEST_SET_GET_VALUE( origin[i], gaussianImage->GetOrigin()[i] );
    }

  gaussianImage->SetSpacing( spacing );
  for( unsigned int i = 0; i < Dimension; ++i)
    {
    TEST_SET_GET_VALUE( spacing[i], gaussianImage->GetSpacing()[i] );
    }

  gaussianImage->SetDirection( direction );
  TEST_SET_GET_VALUE( direction, gaussianImage->GetDirection() );

  // Test SetReferenceImage from GenerateImageSource base class.
  ImageType::Pointer referenceImage = ImageType::New();
  ImageType::IndexType startIndex;
  startIndex.Fill(0);
  ImageType::SizeType referenceSize;
  referenceSize.SetSize(size);
  ImageType::RegionType region(startIndex, referenceSize);
  referenceImage->SetRegions(region);
  referenceImage->Allocate();
  referenceImage->FillBuffer(0);

  referenceImage->SetOrigin( origin );
  referenceImage->SetSpacing( spacing );
  referenceImage->SetDirection( direction );
  gaussianImage->SetReferenceImage( referenceImage );
  bool useReferenceImage = true;
  TEST_SET_GET_BOOLEAN( gaussianImage, UseReferenceImage, useReferenceImage );
  gaussianImage->SetReferenceImage( referenceImage );
  TEST_SET_GET_VALUE( referenceImage, gaussianImage->GetReferenceImage() );

  gaussianImage->SetOutputParametersFromImage( referenceImage );

  bool normalized = atoi( argv[2] ) != 0;
  TEST_SET_GET_BOOLEAN( gaussianImage, Normalized, normalized );

  gaussianImage->SetMean( mean );
  TEST_SET_GET_VALUE( mean, gaussianImage->GetMean() );

  gaussianImage->SetSigma( sigma );
  TEST_SET_GET_VALUE( sigma, gaussianImage->GetSigma() );


  // Check the parameters
  GaussianSourceType::ParametersType params = gaussianImage->GetParameters();
  if ( params.GetSize() != 7 )
    {
    std::cerr << "Incorrect number of parameters. Expected 7, got "
              << params.GetSize() << "." << std::endl;
    return EXIT_FAILURE;
    }

  if ( itk::Math::NotAlmostEquals( params[0], sigma[0] ) ||
       itk::Math::NotAlmostEquals( params[1], sigma[1] ) ||
       itk::Math::NotAlmostEquals( params[2], sigma[2] ) )
    {
    std::cerr << "Parameters have incorrect sigma value." << std::endl;
    return EXIT_FAILURE;
    }

  if ( itk::Math::NotAlmostEquals( params[3], mean[0] ) ||
       itk::Math::NotAlmostEquals( params[4], mean[1] ) ||
       itk::Math::NotAlmostEquals( params[5], mean[2] ) )
    {
    std::cerr << "Parameters have incorrect mean value." << std::endl;
    return EXIT_FAILURE;
    }

  if ( itk::Math::NotAlmostEquals( params[6], gaussianImage->GetScale() ) )
    {
    std::cerr << "Parameters have incorrect scale value." << std::endl;
    return EXIT_FAILURE;
    }

  params[0] = 12.0;
  params[1] = 13.0;
  params[2] = 14.0;
  params[3] = 22.0;
  params[4] = 32.0;
  params[5] = 42.0;
  params[6] = 55.5;
  gaussianImage->SetParameters( params );

  if ( itk::Math::NotAlmostEquals( gaussianImage->GetSigma()[0], params[0] ) ||
       itk::Math::NotAlmostEquals( gaussianImage->GetSigma()[1], params[1] ) ||
       itk::Math::NotAlmostEquals( gaussianImage->GetSigma()[2], params[2] ) )
    {
    std::cerr << "Sigma disagrees with parameters array." << std::endl;
    std::cerr << "Sigma: " << gaussianImage->GetSigma() << ", parameters: ["
              << params[0] << ", " << params[1] << ", " << params[2]
              << "]" << std::endl;
    return EXIT_FAILURE;
    }

  if ( itk::Math::NotAlmostEquals( gaussianImage->GetMean()[0], params[3] ) ||
       itk::Math::NotAlmostEquals( gaussianImage->GetMean()[1], params[4] ) ||
       itk::Math::NotAlmostEquals( gaussianImage->GetMean()[2], params[5] ) )
    {
    std::cerr << "Mean disagrees with parameters array." << std::endl;
    std::cerr << "Mean: " << gaussianImage->GetMean() << ", parameters: ["
             << params[3] << ", " << params[4] << ", " << params[5]
             << "]" << std::endl;
    return EXIT_FAILURE;
    }

  if ( itk::Math::NotAlmostEquals( gaussianImage->GetScale(), params[6] ) )
    {
    std::cerr << "Scale disagrees with parameters array." << std::endl;
    std::cerr << "Scale: " << gaussianImage->GetScale() << ", parameters: "
              << params[6] << std::endl;
    return EXIT_FAILURE;
    }


  FilterWatcher watcher( gaussianImage, "GaussianImageSource" );

  // Run the pipeline
  TRY_EXPECT_NO_EXCEPTION( gaussianImage->Update() );


  // Get the output of the image source
  ImageType::Pointer outputImage = gaussianImage->GetOutput();

  // Write the result image
  typedef itk::ImageFileWriter< ImageType > WriterType;

  WriterType::Pointer writer = WriterType::New();

  writer->SetFileName( argv[1] );

  writer->SetInput( outputImage );

  TRY_EXPECT_NO_EXCEPTION( writer->Update() );

  std::cout << "Test finished" << std::endl;
  return EXIT_SUCCESS;
}
