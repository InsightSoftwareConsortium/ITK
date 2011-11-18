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

#include "itkComposeImageFilter.h"

#include "itkImageFileReader.h"
#include "itkImageSeriesWriter.h"

int itkImageToVectorImageFilterTest(int argc, char *argv[] )
{

  typedef unsigned char PixelType;

  typedef itk::Image<PixelType,2>        ScalarImageType;
  typedef itk::VectorImage<PixelType,2>  VectorImageType;

  typedef itk::ImageFileReader<ScalarImageType>                ReaderType;
  typedef itk::ImageFileWriter<VectorImageType>                WriterType;

  typedef itk::ComposeImageFilter<ScalarImageType> FilterType;

  if (argc < 3)
    {
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << "  input1 input2 ... inputn output" << std::endl;
    return EXIT_FAILURE;
    }

  FilterType::Pointer filter = FilterType::New();
  int f = 0;
  for (int i=1; i < argc - 1; i++)
    {
    ReaderType::Pointer reader = ReaderType::New();
    reader->SetFileName (argv[i]);
    reader->Update();
    filter->SetInput(f++,reader->GetOutput());
    }

  WriterType::Pointer writer = WriterType::New();
  writer->SetFileName (  argv[argc-1] );

  try
    {
    writer->SetInput(filter->GetOutput());
    writer->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << "Error while writing the file" << std::endl;
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;

    }
  std::cout << "Test passed." << std::endl;
  return EXIT_SUCCESS;

}
