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
#include "itkReplaceFeatureMapNanInfImageFilter.h"

#include "itkMath.h"
#include "itkImage.h"
#include "itkVector.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkTestingMacros.h"
#include "itkVectorIndexSelectionCastImageFilter.h"

int ReplaceFeatureMapNanInfImageFilterInstantiationTest( int argc, char *argv[] )
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

  constexpr unsigned int ImageDimension = 3;
  constexpr unsigned int VectorComponentDimension = 5;

  // Declare types
  using InputPixelType = float;
  using InputImageType = itk::Image< InputPixelType, ImageDimension >;
  using ReaderType = itk::ImageFileReader< InputImageType >;

  using OutputPixelComponentType = float;
  using OutputPixelType = itk::Vector< OutputPixelComponentType, VectorComponentDimension >;
  using OutputImageType = itk::Image< OutputPixelType, ImageDimension >;

  // Create and set up a reader
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );

  // Create and set up a maskReader
  ReaderType::Pointer maskReader = ReaderType::New();
  maskReader->SetFileName( argv[2] );

  // Create the filter
  using FilterType = itk::BoneMorphometryFeaturesImageFilter<InputImageType, OutputImageType, InputImageType>;
  FilterType::Pointer filter = FilterType::New();


  filter->SetInput( reader->GetOutput() );
  filter->SetMaskImage( maskReader->GetOutput() );
  filter->SetThreshold( 1300 );

  TRY_EXPECT_NO_EXCEPTION( filter->Update() );

  using PostProcessingFilterType = itk::ReplaceFeatureMapNanInfImageFilter<OutputImageType>;
  PostProcessingFilterType::Pointer postProcessingFilter = PostProcessingFilterType::New();

  postProcessingFilter->SetInput( filter->GetOutput() );

  // Create and set up a writer
  using WriterType = itk::ImageFileWriter< OutputImageType >;
  WriterType::Pointer writer = WriterType::New();
  writer->SetFileName( argv[3] );
  writer->SetInput( postProcessingFilter->GetOutput() );


  TRY_EXPECT_NO_EXCEPTION( writer->Update() );

  std::cout << "Test finished." << std::endl;
  return EXIT_SUCCESS;
}
