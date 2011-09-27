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
#include "itkLabelToRGBImageFilter.h"


int itkLabelToRGBImageFilterTest(int argc, char * argv[])
{
  const int Dimension = 2;

  if( argc < 3 )
    {
    std::cerr << "Missing Parameters " << std::endl;
    std::cerr << "Usage: " << argv[0];
    std::cerr << " LabelImage OutputImage" << std::endl;
    return 1;
    }

  typedef unsigned char                               PixelType;
  typedef itk::Image< PixelType, Dimension >          ImageType;
  typedef itk::RGBPixel<unsigned char>                ColorPixelType;
  typedef itk::Image< ColorPixelType, Dimension >     ColorImageType;
  typedef itk::ImageFileReader< ImageType >           ReaderType;

  //Read in the input image
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );

  //Instantiate the filter
  typedef itk::LabelToRGBImageFilter<
    ImageType, ColorImageType> FilterType;
  FilterType::Pointer filter = FilterType::New();

  // Exercising Background Value methods
  filter->SetBackgroundValue( 10 );
  if( filter->GetBackgroundValue() != 10 )
    {
    std::cerr << "Background value Set/Get Problem" << std::endl;
    return EXIT_FAILURE;
    }


  //Set the filter input and label images
  filter->SetInput( reader->GetOutput() );
  filter->SetBackgroundValue( 0 );

  itk::SimpleFilterWatcher watcher(filter, "filter");

  //Instantiate output image
  typedef itk::ImageFileWriter< ColorImageType > WriterType;
  WriterType::Pointer writer = WriterType::New();

  writer->SetInput( filter->GetOutput() );
  writer->SetFileName( argv[2] );

  try
    {
    writer->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }

  // exercise the methods to change the colors
  unsigned int numberOfColors1 = filter->GetNumberOfColors();
  filter->AddColor( 1, 255, 255 );

  unsigned int numberOfColors2 = filter->GetNumberOfColors();

  if( numberOfColors2 != numberOfColors1 + 1 )
    {
    std::cerr << "Error in GetNumberOfColors() or AddColor() " << std::endl;
    return EXIT_FAILURE;
    }

  filter->ResetColors();
  filter->AddColor( 255, 255, 255 );

  unsigned int numberOfColors3 = filter->GetNumberOfColors();

  if( numberOfColors3 != 1 )
    {
    std::cerr << "Error in GetNumberOfColors() or ResetColors() or AddColor() " << std::endl;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}
