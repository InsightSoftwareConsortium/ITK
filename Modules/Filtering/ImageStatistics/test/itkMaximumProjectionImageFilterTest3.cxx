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

#include "itkMaximumProjectionImageFilter.h"


int itkMaximumProjectionImageFilterTest3(int argc, char * argv[])
{
  if( argc < 4 )
    {
    std::cerr << "Missing parameters " << std::endl;
    std::cerr << "Usage: " << argv[0];
    std::cerr << "Dimension Inputimage Outputimage " << std::endl;
    return EXIT_FAILURE;
    }

  int dim = atoi(argv[1]);

  typedef unsigned char PixelType;

  typedef itk::Image< PixelType, 3 > ImageType;
  typedef itk::Image< PixelType, 2 > Image2DType;

  typedef itk::ImageFileReader< ImageType > ReaderType;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[2] );

  typedef itk::MaximumProjectionImageFilter<
    ImageType, Image2DType > FilterType;

  FilterType::Pointer filter = FilterType::New();
  filter->SetInput( reader->GetOutput() );
  filter->SetProjectionDimension( dim );

  itk::SimpleFilterWatcher watcher(filter, "filter");

  typedef itk::ImageFileWriter< Image2DType > WriterType;
  WriterType::Pointer writer = WriterType::New();
  writer->SetInput( filter->GetOutput() );
  writer->SetFileName( argv[3] );

  try
    {
    writer->Update();
    }
  catch ( itk::ExceptionObject & excp )
    {
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }

  // Set ProjectionDimension to a bad value
  bool caught = false;
  try
    {
    filter->SetProjectionDimension(100);
    writer->Update();
    }
  catch ( itk::ExceptionObject & excp )
    {
    std::cerr << std::endl << "Caught expected exception!";
    std::cerr << excp << std::endl;
    caught = true;
    }
  if (!caught)
    {
    std::cerr << "Failed to catch expected exception!" << std::endl;
    return EXIT_FAILURE;
    }
  return EXIT_SUCCESS;
}
