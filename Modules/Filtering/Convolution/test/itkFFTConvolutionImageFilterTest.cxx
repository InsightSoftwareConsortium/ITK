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

#include "itkChangeInformationImageFilter.h"
#include "itkConstantBoundaryCondition.h"
#include "itkFFTConvolutionImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkPeriodicBoundaryCondition.h"
#include "itkSimpleFilterWatcher.h"
#include "itkTestingMacros.h"
#include "itkZeroFluxNeumannBoundaryCondition.h"

int itkFFTConvolutionImageFilterTest( int argc, char * argv[] )
{

  if ( argc < 4 )
    {
    std::cout << "Usage: " << argv[0]
      << " inputImage "
      << "kernelImage "
      << "outputImage "
      << "[sizeGreatestPrimeFactor] "
      << "[normalizeImage] "
      << "[outputRegionMode] "
      << "[boundaryCondition] " << std::endl;
    return EXIT_FAILURE;
    }

  const int ImageDimension = 2;

  typedef float                                    PixelType;
  typedef itk::Image< PixelType, ImageDimension >  ImageType;
  typedef itk::ImageFileReader< ImageType >        ReaderType;

  ReaderType::Pointer reader1 = ReaderType::New();
  reader1->SetFileName( argv[1] );

  TRY_EXPECT_NO_EXCEPTION( reader1->Update() );

  ReaderType::Pointer reader2 = ReaderType::New();
  reader2->SetFileName( argv[2] );

  TRY_EXPECT_NO_EXCEPTION( reader2->Update() );

  typedef itk::FFTConvolutionImageFilter< ImageType > ConvolutionFilterType;
  ConvolutionFilterType::Pointer convoluter = ConvolutionFilterType::New();

  EXERCISE_BASIC_OBJECT_METHODS( convoluter, FFTConvolutionImageFilter,
    ConvolutionImageFilterBase );

  // Test empty image exception
  ImageType::Pointer emptyImage = ImageType::New();
  convoluter->SetInput( emptyImage );
  try
    {
    convoluter->Update();
    std::cerr << "Failed to throw expected exception" << std::endl;
    return EXIT_FAILURE;
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cout << excp << std::endl;
    std::cout << "Caught EXPECTED exception for empty image as input" << std::endl;
    }

  // Test generality of filter by changing the image index
  typedef itk::ChangeInformationImageFilter< ImageType > ChangeInformationFilterType;
  ChangeInformationFilterType::Pointer inputChanger = ChangeInformationFilterType::New();
  inputChanger->ChangeRegionOn();
  ImageType::OffsetType inputOffset = {{-2, 3}};
  inputChanger->SetOutputOffset(inputOffset);
  inputChanger->SetInput( reader1->GetOutput() );

  convoluter->SetInput( inputChanger->GetOutput() );

  // Test generality of filter by changing the kernel index
  ChangeInformationFilterType::Pointer kernelChanger = ChangeInformationFilterType::New();
  kernelChanger->ChangeRegionOn();
  ImageType::OffsetType kernelOffset = {{3, -5}};
  kernelChanger->SetOutputOffset( kernelOffset );
  kernelChanger->SetInput( reader2->GetOutput() );

  convoluter->SetKernelImage( kernelChanger->GetOutput() );

  if( argc >= 5 )
    {
    ConvolutionFilterType::SizeValueType sizeGreatestPrimeFactor = atoi( argv[4] );
    if( !itk::Math::IsPrime(sizeGreatestPrimeFactor) )
      {
      std::cerr << "A prime number is expected for the greatest prime factor size!" << std::endl;
      return EXIT_FAILURE;
      }
    convoluter->SetSizeGreatestPrimeFactor( sizeGreatestPrimeFactor );
    TEST_SET_GET_VALUE( sizeGreatestPrimeFactor, convoluter->GetSizeGreatestPrimeFactor() );
    }

  if( argc >= 6 )
    {
    bool normalize = static_cast<bool>( atoi( argv[5] ) );
    convoluter->SetNormalize( normalize );
    TEST_SET_GET_VALUE( normalize, convoluter->GetNormalize() );

    if( normalize )
      {
      convoluter->NormalizeOn();
      TEST_EXPECT_TRUE( convoluter->GetNormalize() );
      }
    else
      {
      convoluter->NormalizeOff();
      TEST_EXPECT_TRUE( !convoluter->GetNormalize() );
      }
    }

  if( argc >= 7 )
    {
    std::string outputRegionMode( argv[6] );
    if ( outputRegionMode == "SAME" )
      {
      convoluter->SetOutputRegionMode( ConvolutionFilterType::SAME );
      TEST_SET_GET_VALUE( ConvolutionFilterType::SAME, convoluter->GetOutputRegionMode() );
      }
    else if ( outputRegionMode == "VALID" )
      {
      convoluter->SetOutputRegionMode( ConvolutionFilterType::VALID );
      TEST_SET_GET_VALUE( ConvolutionFilterType::VALID, convoluter->GetOutputRegionMode() );
      }
    else
      {
      std::cerr << "Invalid OutputRegionMode '" << outputRegionMode << "'." << std::endl;
      std::cerr << "Valid values are SAME or VALID." << std::endl;
      return EXIT_FAILURE;
      }

    if( outputRegionMode == "SAME" )
      {
      convoluter->SetOutputRegionModeToSame();
      TEST_SET_GET_VALUE( ConvolutionFilterType::SAME, convoluter->GetOutputRegionMode() );
      }
    else
      {
      convoluter->SetOutputRegionModeToValid();
      TEST_SET_GET_VALUE( ConvolutionFilterType::VALID, convoluter->GetOutputRegionMode() );
      }
    }

  itk::ConstantBoundaryCondition< ImageType > constantBoundaryCondition;
  convoluter->SetBoundaryCondition( &constantBoundaryCondition );
  itk::PeriodicBoundaryCondition< ImageType > periodicBoundaryCondition;
  itk::ZeroFluxNeumannBoundaryCondition< ImageType > zeroFluxNeumannBoundaryCondition;
  if( argc >= 7 )
  {
    std::string boundaryCondition( argv[7] );
    if ( boundaryCondition == "CONSTANT" )
      {
      convoluter->SetBoundaryCondition( &constantBoundaryCondition );
      TEST_SET_GET_VALUE( &constantBoundaryCondition, convoluter->GetBoundaryCondition() );
      }
    else if ( boundaryCondition == "PERIODIC" )
      {
      convoluter->SetBoundaryCondition( &periodicBoundaryCondition );
      TEST_SET_GET_VALUE( &periodicBoundaryCondition, convoluter->GetBoundaryCondition() );
      }
    else if ( boundaryCondition == "ZEROFLUXNEUMANN" )
      {
      convoluter->SetBoundaryCondition( &zeroFluxNeumannBoundaryCondition );
      TEST_SET_GET_VALUE( &zeroFluxNeumannBoundaryCondition, convoluter->GetBoundaryCondition() );
      }
    else
      {
      std::cerr << "Invalid BoundaryCondition '" << boundaryCondition << "'." << std::endl;
      std::cerr << "Valid values are CONSTANT, PERIODIC or ZEROFLUXNEUMANN." << std::endl;
      return EXIT_FAILURE;
      }
    }

  itk::SimpleFilterWatcher watcher( convoluter, "filter" );

  TRY_EXPECT_NO_EXCEPTION( convoluter->Update() );

  typedef itk::ImageFileWriter<ImageType> WriterType;
  WriterType::Pointer writer = WriterType::New();
  writer->SetFileName( argv[3] );
  writer->SetInput( convoluter->GetOutput() );

  TRY_EXPECT_NO_EXCEPTION( writer->Update() );


  // Test VALID output region mode with kernel that is larger than
  // the input image. Should result in a zero-size valid region.
  ImageType::Pointer largeKernel = ImageType::New();
  ImageType::RegionType kernelRegion( reader1->GetOutput()->GetLargestPossibleRegion().GetSize() );
  kernelRegion.PadByRadius( 5 );

  largeKernel->SetRegions( kernelRegion );
  largeKernel->Allocate();
  convoluter->SetOutputRegionModeToValid();
  convoluter->SetInput( reader1->GetOutput() );
  convoluter->SetKernelImage( largeKernel );

  TRY_EXPECT_EXCEPTION( convoluter->Update() );

  // Test for invalid request region.
  ImageType::IndexType invalidIndex;
  invalidIndex.Fill( 1000 );
  ImageType::SizeType invalidSize;
  invalidSize.Fill( 1000 );
  ImageType::RegionType invalidRequestRegion( invalidIndex, invalidSize );
  convoluter->GetOutput()->SetRequestedRegion( invalidRequestRegion );

  TRY_EXPECT_EXCEPTION( convoluter->Update() );

  return EXIT_SUCCESS;
}
