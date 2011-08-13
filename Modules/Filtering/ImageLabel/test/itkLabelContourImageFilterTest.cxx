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
#include "itkLabelContourImageFilter.h"

int itkLabelContourImageFilterTest(int argc, char * argv[])
{

  if( argc != 5 )
    {
    std::cerr << "usage: " << argv[0] << " intput output fullyConnected bg" << std::endl;
    std::cerr << " input: the input image" << std::endl;
    std::cerr << " output: the output image" << std::endl;
    std::cerr << " fullyConnected: 0 or 1" << std::endl;
    return EXIT_FAILURE;
    }

  const int dim = 3;

  typedef unsigned char            PType;
  typedef itk::Image< PType, dim > IType;

  typedef itk::ImageFileReader< IType > ReaderType;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );

  typedef itk::LabelContourImageFilter< IType, IType > FilterType;
  FilterType::Pointer filter = FilterType::New();

  // test default values
  if ( filter->GetFullyConnected( ) != false )
    {
    std::cerr << "Wrong default FullyConnected." << std::endl;
    return EXIT_FAILURE;
    }
  if ( filter->GetBackgroundValue( ) != 0 )
    {
    std::cerr << "Wrong default background value." << std::endl;
    return EXIT_FAILURE;
    }

  //
  // Tests for raising code coverage
  //
  try
    {
    filter->Update();
    std::cerr << "Failed to throw expected exception" << std::endl;
    return EXIT_FAILURE;
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cout << excp << std::endl;
    std::cout << "caught EXPECTED exception for empty image as input" << std::endl;
    }

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


  // set the inputs

  filter->SetInput( reader->GetOutput() );

  filter->SetFullyConnected( atoi(argv[3]) );
  if ( filter->GetFullyConnected( ) != (bool)atoi(argv[3]) )
    {
    std::cerr << "Set/Get FullyConnected problem." << std::endl;
    return EXIT_FAILURE;
    }

  filter->SetBackgroundValue( atoi(argv[4]) );
  if ( filter->GetBackgroundValue( ) != atoi(argv[4]) )
    {
    std::cerr << "Set/Get BackgroundValue problem." << std::endl;
    return EXIT_FAILURE;
    }

  itk::SimpleFilterWatcher watcher(filter, "filter");

  typedef itk::ImageFileWriter< IType > WriterType;
  WriterType::Pointer writer = WriterType::New();
  writer->SetInput( filter->GetOutput() );
  writer->SetFileName( argv[2] );
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

  return EXIT_SUCCESS;

}
