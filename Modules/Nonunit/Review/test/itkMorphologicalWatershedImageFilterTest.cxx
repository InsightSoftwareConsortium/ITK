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

int itkMorphologicalWatershedImageFilterTest(int argc, char * argv[])
{
  if( argc < 6 )
    {
    std::cerr << "Missing Parameters " << std::endl;
    std::cerr << "Usage: " << argv[0];
    std::cerr << " InputImage OutputImage MarkWatershedLine";
    std::cerr << " FullyConnected Level [OvelayOutput [Alpha]]" << std::endl;
    return EXIT_FAILURE;
    }
  const int dim = 2;

  typedef unsigned char PixelType;

  typedef itk::Image< PixelType, dim > ImageType;

  typedef itk::ImageFileReader< ImageType > ReaderType;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );

  typedef itk::MorphologicalWatershedImageFilter<
    ImageType, ImageType > FilterType;

  FilterType::Pointer filter = FilterType::New();
  filter->SetInput( reader->GetOutput() );

  // test default values
  if ( filter->GetMarkWatershedLine( ) != true )
    {
    std::cerr << "Wrong default MarkWatershedLine." << std::endl;
    return EXIT_FAILURE;
    }
  if ( filter->GetFullyConnected( ) != false )
    {
    std::cerr << "Wrong default FullyConnected." << std::endl;
    return EXIT_FAILURE;
    }
  if ( filter->GetLevel( ) != 0 )
    {
    std::cerr << "Wrong default Level." << std::endl;
    return EXIT_FAILURE;
    }


  filter->SetMarkWatershedLine( atoi( argv[3] ) );
  if ( filter->GetMarkWatershedLine( ) != (bool)atoi(argv[3]) )
    {
    std::cerr << "Set/Get MarkWatershedLine problem." << std::endl;
    return EXIT_FAILURE;
    }
  filter->SetFullyConnected( atoi( argv[4] ) );
  if ( filter->GetFullyConnected( ) != (bool)atoi(argv[4]) )
    {
    std::cerr << "Set/Get FullyConnected problem." << std::endl;
    return EXIT_FAILURE;
    }

  filter->SetLevel( atoi( argv[5] ) );

  if ( filter->GetLevel( ) != atoi(argv[5]) )
    {
    std::cerr << "Set/Get Level problem." << std::endl;
    return EXIT_FAILURE;
    }

  itk::SimpleFilterWatcher watcher(filter, "filter");

  try
    {
    filter->Update();
    }
  catch ( itk::ExceptionObject & excp )
    {
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }

  // rescale the output to have a better display
  typedef itk::MinimumMaximumImageCalculator< ImageType > MaxCalculatorType;
  MaxCalculatorType::Pointer max = MaxCalculatorType::New();
  max->SetImage( filter->GetOutput() );
  max->Compute();

  typedef itk::IntensityWindowingImageFilter<
    ImageType, ImageType > RescaleType;

  RescaleType::Pointer rescale = RescaleType::New();
  rescale->SetInput( filter->GetOutput() );
  rescale->SetWindowMinimum( itk::NumericTraits< PixelType >::ZeroValue() );
  rescale->SetWindowMaximum( max->GetMaximum() );
  rescale->SetOutputMaximum( itk::NumericTraits< PixelType >::max() );
  rescale->SetOutputMinimum( itk::NumericTraits< PixelType >::ZeroValue() );

  typedef itk::ImageFileWriter< ImageType > WriterType;
  WriterType::Pointer writer = WriterType::New();
  writer->SetInput( rescale->GetOutput() );
  writer->SetFileName( argv[2] );

  try
    {
    writer->Update();
    }
  catch ( itk::ExceptionObject & excp )
    {
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }

  if( argc > 6 )
    {
    typedef itk::RGBPixel< unsigned char >     RGBPixelType;
    typedef itk::Image< RGBPixelType, dim >    RGBImageType;

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

    try
      {
      rgbwriter->Update();
      }
    catch ( itk::ExceptionObject & excp )
      {
      std::cerr << excp << std::endl;
      return EXIT_FAILURE;
      }

    }

  return EXIT_SUCCESS;

}
