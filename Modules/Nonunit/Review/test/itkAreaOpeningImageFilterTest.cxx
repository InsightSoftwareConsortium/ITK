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
#include "itkAreaOpeningImageFilter.h"

int itkAreaOpeningImageFilterTest(int argc, char * argv[])
{

  if( argc != 6 )
    {
    std::cerr << "usage: " << argv[0] << " inputImage outputImage lambda conn use_spacing" << std::endl;
    std::cerr << "  inputImage: The input image." << std::endl;
    std::cerr << "  outputImage: The output image." << std::endl;
    return EXIT_FAILURE;
    }

  const int dim = 3;

  typedef unsigned char                  PType;
  typedef itk::Image< PType, dim >       IType;

  typedef itk::ImageFileReader< IType > ReaderType;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );

  typedef itk::AreaOpeningImageFilter< IType, IType > FilterType;
  FilterType::Pointer filter = FilterType::New();

  //
  // Tests for raising code coverage
  //
  filter->FullyConnectedOn();
  if( !filter->GetFullyConnected() )
    {
    std::cerr << "Set/GetFullyConnected() error" << std::endl;
    return EXIT_FAILURE;
    }

  filter->FullyConnectedOff();
  if( filter->GetFullyConnected() )
    {
    std::cerr << "Set/GetFullyConnected() error" << std::endl;
    return EXIT_FAILURE;
    }

  filter->UseImageSpacingOn();
  if( !filter->GetUseImageSpacing() )
    {
    std::cerr << "Set/GetUseImageSpacing() error" << std::endl;
    return EXIT_FAILURE;
    }

  filter->UseImageSpacingOff();
  if( filter->GetUseImageSpacing() )
    {
    std::cerr << "Set/GetUseImageSpacing() error" << std::endl;
    return EXIT_FAILURE;
    }


  filter->SetInput( reader->GetOutput() );

  filter->SetLambda( atoi(argv[3]) );
  if ( filter->GetLambda( ) != atoi(argv[3]) )
    {
    std::cerr << "Set/Get Lambda problem." << std::endl;
    return EXIT_FAILURE;
    }

  filter->SetFullyConnected( atoi(argv[4]) );
  if ( filter->GetFullyConnected( ) != (bool)atoi(argv[4]) )
    {
    std::cerr << "Set/Get FullyConnected problem." << std::endl;
    return EXIT_FAILURE;
    }

  filter->SetUseImageSpacing( atoi(argv[5]) );
  if ( filter->GetUseImageSpacing( ) != (bool)atoi(argv[5]) )
    {
    std::cerr << "Set/Get UseImageSpacing problem." << std::endl;
    return EXIT_FAILURE;
    }

  itk::SimpleFilterWatcher watcher(filter, "filter");

  typedef itk::ImageFileWriter< IType > WriterType;
  WriterType::Pointer writer = WriterType::New();
  writer->SetInput( filter->GetOutput() );
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

  return EXIT_SUCCESS;

}
