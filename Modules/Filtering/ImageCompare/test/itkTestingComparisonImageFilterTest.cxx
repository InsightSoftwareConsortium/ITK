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

#include <iostream>
#include "itkTestingComparisonImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkSimpleFilterWatcher.h"

#ifdef ITKV3_COMPATIBILITY
#include "itkDifferenceImageFilter.h"
#endif

int itkTestingComparisonImageFilterTest(int argc, char *argv [] )
{
  if( argc < 6 )
    {
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0];
    std::cerr << "  inputImageFile1 inputImageFile2 outputImage threshold radius" << std::endl;
    return EXIT_FAILURE;
    }


  // Test using an unsigned integral pixel type and generate a signed
  // integral pixel type
  typedef   signed   short  InputPixelType;
  typedef   unsigned short  OutputPixelType;

  const unsigned int Dimension = 2;

  typedef itk::Image< InputPixelType,  Dimension >   InputImageType;
  typedef itk::Image< OutputPixelType, Dimension >   OutputImageType;


  typedef itk::ImageFileReader< InputImageType  >  ReaderType;

  ReaderType::Pointer reader1 = ReaderType::New();
  ReaderType::Pointer reader2 = ReaderType::New();

  reader1->SetFileName( argv[1] );
  reader2->SetFileName( argv[2] );

#ifdef ITKV3_COMPATIBILITY
  typedef itk::DifferenceImageFilter<
                             InputImageType,
                             OutputImageType >  DiffFilterType;
  DiffFilterType::Pointer testInit=DiffFilterType::New();
  // setup the testInit
  testInit->SetDifferenceThreshold( atoi( argv[4] ) );
  testInit->SetToleranceRadius(     atoi( argv[5] ) );

  // wire the pipeline
  testInit->SetValidInput( reader1->GetOutput() );
  testInit->SetTestInput(  reader2->GetOutput() );
  testInit->Update();
#endif


  // Define the filter
  typedef itk::Testing::ComparisonImageFilter<
                             InputImageType,
                             OutputImageType >  FilterType;

  FilterType::Pointer filter = FilterType::New();

  // setup the filter
  filter->SetDifferenceThreshold( atoi( argv[4] ) );
  filter->SetToleranceRadius(     atoi( argv[5] ) );

  itk::SimpleFilterWatcher watcher( filter, "Difference");

  // wire the pipeline
  filter->SetValidInput( reader1->GetOutput() );
  filter->SetTestInput(  reader2->GetOutput() );

  // Write the output
  typedef itk::ImageFileWriter< OutputImageType >       WriterType;

  WriterType::Pointer writer = WriterType::New();

  writer->SetInput( filter->GetOutput() );

  writer->SetFileName( argv[3] );

  try
    {
    writer->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << "Exception : " << excp << std::endl;
    return EXIT_FAILURE;
    }

  unsigned long numberOfPixelsWithDifferences =
    filter->GetNumberOfPixelsWithDifferences();

  std::cout << "Number of pixels with differences = ";
  std::cout << numberOfPixelsWithDifferences << std::endl;

  return EXIT_SUCCESS;
}
