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

#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkSimpleFilterWatcher.h"
#include "itkIntensityWindowingImageFilter.h"
#include "itkMorphologicalWatershedImageFilter.h"
#include "itkLabelOverlayImageFilter.h"
#include "itkTestingMacros.h"


int itkMorphologicalWatershedImageFilterTest( int argc, char * argv[] )
{
  if( argc < 6 )
    {
    std::cerr << "Missing parameters" << std::endl;
    std::cerr << "Usage: " << argv[0]
      << " inputImageFile"
      << " outputImageFile"
      << " markWatershedLine"
      << " fullyConnected"
      << " level"
      << " [ovelayOutput [alpha]]";
    std::cerr << std::endl;
    return EXIT_FAILURE;
    }

  const unsigned int Dimension = 2;

  typedef unsigned char PixelType;

  typedef itk::Image< PixelType, Dimension > ImageType;

  typedef itk::ImageFileReader< ImageType > ReaderType;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );

  typedef itk::MorphologicalWatershedImageFilter< ImageType, ImageType >
    FilterType;
  FilterType::Pointer filter = FilterType::New();

  EXERCISE_BASIC_OBJECT_METHODS( filter, MorphologicalWatershedImageFilter,
    ImageToImageFilter );

  bool markWatershedLine = atoi( argv[3] );
  TEST_SET_GET_BOOLEAN( filter, MarkWatershedLine, markWatershedLine );

  bool fullyConnected = atoi( argv[4] );
  TEST_SET_GET_BOOLEAN( filter, FullyConnected, fullyConnected );

  FilterType::InputImagePixelType level =
    static_cast< FilterType::InputImagePixelType >( atof( argv[5] ) );
  filter->SetLevel( level );
  TEST_SET_GET_VALUE( level, filter->GetLevel() );


  filter->SetInput( reader->GetOutput() );

  itk::SimpleFilterWatcher watcher( filter, "MorphologicalWatershedImageFilter" );

  TRY_EXPECT_NO_EXCEPTION( filter->Update() );


  // Rescale the output to have a better display
  typedef itk::MinimumMaximumImageCalculator< ImageType > MaxCalculatorType;
  MaxCalculatorType::Pointer minMaxCalculator = MaxCalculatorType::New();
  minMaxCalculator->SetImage( filter->GetOutput() );
  minMaxCalculator->Compute();

  typedef itk::IntensityWindowingImageFilter<
    ImageType, ImageType > RescaleType;
  RescaleType::Pointer rescaler = RescaleType::New();
  rescaler->SetInput( filter->GetOutput() );
  rescaler->SetWindowMinimum( itk::NumericTraits< PixelType >::ZeroValue() );
  rescaler->SetWindowMaximum( minMaxCalculator->GetMaximum() );
  rescaler->SetOutputMaximum( itk::NumericTraits< PixelType >::max() );
  rescaler->SetOutputMinimum( itk::NumericTraits< PixelType >::ZeroValue() );

  // Write output image
  typedef itk::ImageFileWriter< ImageType > WriterType;
  WriterType::Pointer writer = WriterType::New();
  writer->SetInput( rescaler->GetOutput() );
  writer->SetFileName( argv[2] );

  TRY_EXPECT_NO_EXCEPTION( writer->Update() );

  if( argc > 6 )
    {
    typedef itk::RGBPixel< PixelType >            RGBPixelType;
    typedef itk::Image< RGBPixelType, Dimension > RGBImageType;

    typedef itk::LabelOverlayImageFilter<
      ImageType, ImageType, RGBImageType> OverlayType;

    OverlayType::Pointer overlay = OverlayType::New();
    overlay->SetInput( reader->GetOutput() );
    overlay->SetLabelImage( filter->GetOutput() );

    typedef itk::ImageFileWriter< RGBImageType > RGBWriterType;
    RGBWriterType::Pointer rgbwriter = RGBWriterType::New();
    rgbwriter->SetInput( overlay->GetOutput() );
    rgbwriter->SetFileName( argv[6] );

    if( argc > 7 )
      {
      overlay->SetOpacity( atof( argv[7] ) );
      }

    TRY_EXPECT_NO_EXCEPTION( rgbwriter->Update() );
    }

  std::cerr << "Test finished" << std::endl;
  return EXIT_SUCCESS;
}
