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
#include "itkMorphologicalWatershedFromMarkersImageFilter.h"
#include "itkLabelOverlayImageFilter.h"

int itkMorphologicalWatershedFromMarkersImageFilterTest(int argc, char * argv[])
{
  if( argc < 6 )
    {
    std::cerr << "Missing Parameters " << std::endl;
    std::cerr << "Usage: " << argv[0];
    std::cerr << " InputImage MarkerImage OutputImage MarkWatershedLine FullyConnected [OvelayOutput [Alpha]]" << std::endl;
    return EXIT_FAILURE;
    }
  const int dim = 2;

  typedef unsigned char PType;

  typedef itk::Image< PType, dim > IType;

  typedef itk::ImageFileReader< IType > ReaderType;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );

  ReaderType::Pointer reader2 = ReaderType::New();
  reader2->SetFileName( argv[2] );

  typedef itk::MorphologicalWatershedFromMarkersImageFilter< IType, IType > FilterType;
  FilterType::Pointer filter = FilterType::New();
  filter->SetInput( reader->GetOutput() );
  filter->SetMarkerImage( reader2->GetOutput() );

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

  filter->SetMarkWatershedLine( atoi( argv[4] ) );
  if ( filter->GetMarkWatershedLine( ) != (bool)atoi(argv[4]) )
    {
    std::cerr << "Set/Get MarkWatershedLine problem." << std::endl;
    return EXIT_FAILURE;
    }
  filter->SetFullyConnected( atoi( argv[5] ) );
  if ( filter->GetFullyConnected( ) != (bool)atoi(argv[5]) )
    {
    std::cerr << "Set/Get FullyConnected problem." << std::endl;
    return EXIT_FAILURE;
    }

  itk::SimpleFilterWatcher watcher(filter, "filter");

  typedef itk::ImageFileWriter< IType > WriterType;
  WriterType::Pointer writer = WriterType::New();
  writer->SetInput( filter->GetOutput() );
  writer->SetFileName( argv[3] );
  writer->Update();

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
    typedef itk::RGBPixel<unsigned char>   RGBPixelType;
    typedef itk::Image<RGBPixelType, dim>  RGBImageType;

    typedef itk::LabelOverlayImageFilter<IType, IType, RGBImageType> OverlayType;
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
