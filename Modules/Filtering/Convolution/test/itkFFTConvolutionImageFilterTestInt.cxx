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

#include "itkFFTConvolutionImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkSimpleFilterWatcher.h"
#include "itkTestingMacros.h"

int itkFFTConvolutionImageFilterTestInt(int argc, char * argv[])
{

  if ( argc < 4 )
    {
    std::cout << "Usage: " << argv[0]
      << " inputImage kernelImage outputImage [normalizeImage] [outputRegionMode]" << std::endl;
    return EXIT_FAILURE;
    }

  const int ImageDimension = 2;

  typedef unsigned char                          PixelType;
  typedef itk::Image<PixelType, ImageDimension>  ImageType;
  typedef itk::ImageFileReader<ImageType>        ReaderType;

  ReaderType::Pointer reader1 = ReaderType::New();
  reader1->SetFileName( argv[1] );

  TRY_EXPECT_NO_EXCEPTION( reader1->Update() );

  ReaderType::Pointer reader2 = ReaderType::New();
  reader2->SetFileName( argv[2] );

  TRY_EXPECT_NO_EXCEPTION( reader2->Update() );

  typedef itk::FFTConvolutionImageFilter<ImageType> ConvolutionFilterType;
  ConvolutionFilterType::Pointer convolver = ConvolutionFilterType::New();

  EXERCISE_BASIC_OBJECT_METHODS( convolver, FFTConvolutionImageFilter, ConvolutionImageFilterBase );

  convolver->SetInput( reader1->GetOutput() );
  convolver->SetKernelImage( reader2->GetOutput() );

  ConvolutionFilterType::SizeValueType sizeGreatestPrimeFactor = 2;
  convolver->SetSizeGreatestPrimeFactor( sizeGreatestPrimeFactor );
  TEST_SET_GET_VALUE( sizeGreatestPrimeFactor, convolver->GetSizeGreatestPrimeFactor() );

  itk::SimpleFilterWatcher watcher(convolver, "filter");

  if ( argc >= 5 )
    {
    convolver->SetNormalize( static_cast<bool>( atoi( argv[4] ) ) );
    }

  if ( argc >= 6 )
    {
    std::string outputRegionMode( argv[5] );
    if ( outputRegionMode == "SAME" )
      {
      convolver->SetOutputRegionModeToSame();
      TEST_SET_GET_VALUE( ConvolutionFilterType::SAME, convolver->GetOutputRegionMode() );
      std::cout << "OutputRegionMode set to SAME." << std::endl;
      }
    else if ( outputRegionMode == "VALID" )
      {
      convolver->SetOutputRegionModeToValid();
      TEST_SET_GET_VALUE( ConvolutionFilterType::VALID, convolver->GetOutputRegionMode() );
      std::cout << "OutputRegionMode set to VALID." << std::endl;
      }
    else
      {
      std::cerr << "Invalid OutputRegionMode '" << outputRegionMode << "'." << std::endl;
      std::cerr << "Valid values are SAME or VALID." << std::endl;
      return EXIT_FAILURE;
      }
    }

  TRY_EXPECT_NO_EXCEPTION( convolver->Update() );

  typedef itk::ImageFileWriter< ImageType > WriterType;
  WriterType::Pointer writer = WriterType::New();
  writer->SetFileName( argv[3] );
  writer->SetInput( convolver->GetOutput() );

  TRY_EXPECT_NO_EXCEPTION( writer->Update() );

  return EXIT_SUCCESS;
}
