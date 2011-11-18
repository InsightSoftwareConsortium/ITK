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
#include "itkFFTShiftImageFilter.h"

int itkFFTShiftImageFilterTest(int argc, char * argv[])
{

  if( argc != 4 )
    {
    std::cerr << "usage: " << argv[0] << " inputImage outputImage inverse" << std::endl;
    std::cerr << "  inputImage: The input image." << std::endl;
    std::cerr << "  outputImage: The output image." << std::endl;
    std::cerr << "  inverse: 0, to perform a forward transform, or 1 to perform" << std::endl;
    std::cerr << "           an inverse transform." << std::endl;
    return EXIT_FAILURE;
    }

  const int dim = 3;

  typedef itk::RGBPixel< unsigned char > PType;
  typedef itk::Image< PType, dim >       IType;

  typedef itk::ImageFileReader< IType > ReaderType;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );

  typedef itk::FFTShiftImageFilter< IType, IType > FilterType;
  FilterType::Pointer filter = FilterType::New();
  // test default values
  if ( filter->GetInverse( ) != false )
    {
    std::cerr << "Wrong default Inverse." << std::endl;
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
    std::cout << "catched EXPECTED exception for emtpy image as input" << std::endl;
    }

  filter->InverseOn();
  if( !filter->GetInverse() )
    {
    std::cerr << "Set/GetInverse() error" << std::endl;
    return EXIT_FAILURE;
    }

  filter->InverseOff();
  if( filter->GetInverse() )
    {
    std::cerr << "Set/GetInverse() error" << std::endl;
    return EXIT_FAILURE;
    }


  filter->SetInput( reader->GetOutput() );

  filter->SetInverse( static_cast<bool>( atoi( argv[3] ) ) );
  if ( filter->GetInverse( ) != static_cast<bool>(atoi(argv[3])) )
    {
    std::cerr << "Set/Get Inverse problem." << std::endl;
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
