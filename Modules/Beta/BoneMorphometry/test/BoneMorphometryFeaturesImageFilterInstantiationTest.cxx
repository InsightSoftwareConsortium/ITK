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
#include "itkBoneMorphometryFeaturesImageFilter.h"

#include "itkMath.h"
#include "itkImage.h"
#include "itkVector.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkTestingMacros.h"

int BoneMorphometryFeaturesImageFilterInstantiationTest( int argc, char *argv[] )
{
    if( argc < 3 )
      {
      std::cerr << "Missing parameters." << std::endl;
      std::cerr << "Usage: " << argv[0]
        << " inputImageFile"
        << " maskImageFile"
        << " outputImageFile"<< std::endl;
      return EXIT_FAILURE;
      }

  const unsigned int ImageDimension = 3;
  const unsigned int VectorComponentDimension = 5;

  // Declare types
  typedef float                                         InputPixelType;
  typedef itk::Image< InputPixelType, ImageDimension >  InputImageType;
  typedef itk::ImageFileReader< InputImageType >        ReaderType;

  typedef float                                         OutputPixelComponentType;
  typedef itk::Vector< OutputPixelComponentType, VectorComponentDimension >
                                                        OutputPixelType;
  typedef itk::Image< OutputPixelType, ImageDimension > OutputImageType;

  // Create and set up a reader
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );

  // Create and set up a maskReader
  ReaderType::Pointer maskReader = ReaderType::New();
  maskReader->SetFileName( argv[2] );

  // Create the filter
  typedef itk::BoneMorphometryFeaturesImageFilter<InputImageType, OutputImageType, InputImageType> FilterType;
  FilterType::Pointer filter = FilterType::New();

  EXERCISE_BASIC_OBJECT_METHODS( filter,
   BoneMorphometryFeaturesImageFilter, ImageToImageFilter );

  filter->SetInput( reader->GetOutput() );

  filter->SetMaskImage( maskReader->GetOutput() );
  TEST_SET_GET_VALUE( maskReader->GetOutput(), filter->GetMaskImage() );

  filter->SetThreshold( 1300 );
  TEST_SET_GET_VALUE( 1300, filter->GetThreshold() );

  TRY_EXPECT_NO_EXCEPTION( filter->Update() );

  // Create and set up a writer
  typedef itk::ImageFileWriter< OutputImageType > WriterType;
  WriterType::Pointer writer = WriterType::New();
  writer->SetFileName( argv[3] );
  writer->SetInput( filter->GetOutput() );

  TRY_EXPECT_NO_EXCEPTION( writer->Update() );

  std::cout << "Test finished." << std::endl;
  return EXIT_SUCCESS;
}
