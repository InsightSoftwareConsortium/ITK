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
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkDCMTKImageIO.h"
#include "itkSubtractImageFilter.h"
#include "itkStatisticsImageFilter.h"

int
itkDCMTKMultiFrame4DTest(int argc, char *argv[])
{
  if(argc != 3)
    {
    std::cerr << "Missing filenames" << std::endl
              << "itkDCMTKMultiFram4DTest"
              << " <inputDicomFile> <outputFile>"
              << std::endl;
    return EXIT_FAILURE;
    }
  typedef itk::Image<unsigned short, 4>   ImageType;
  typedef itk::ImageFileReader<ImageType> ReaderType;
  typedef itk::ImageFileWriter<ImageType> WriterType;

  ReaderType::Pointer reader = ReaderType::New();
  WriterType::Pointer writer = WriterType::New();
  reader->SetImageIO(itk::DCMTKImageIO::New());
  reader->SetFileName(argv[1]);
  writer->SetFileName(argv[2]);


  try
    {
    reader->Update();
    }
  catch (itk::ExceptionObject & e)
    {
    std::cerr << "exception in file reader" << std::endl;
    std::cerr << e << std::endl;
    return EXIT_FAILURE;
    }

  ImageType::Pointer im = reader->GetOutput();
  std::cout << im;
  writer->SetInput(im);

  try
    {
    writer->Update();
    }
  catch (itk::ExceptionObject & e)
    {
    std::cerr << "exception in file writer" << std::endl;
    std::cerr << e << std::endl;
    return EXIT_FAILURE;
    }
  catch(...)
    {
    return EXIT_FAILURE;
    }

  // don't want to set imageIO so re-instantiate reader
  reader = ReaderType::New();
  reader->SetFileName(argv[2]);

  typedef itk::SubtractImageFilter<ImageType,ImageType,ImageType>
    SubtractFilterType;

  SubtractFilterType::Pointer subtractFilter =
    SubtractFilterType::New();
  subtractFilter->SetInput1(im);
  subtractFilter->SetInput2(reader->GetOutput());

  typedef itk::StatisticsImageFilter<ImageType> StatisticsFilterType;
  StatisticsFilterType::Pointer statisticsFilter =
    StatisticsFilterType::New();

  statisticsFilter->SetInput(subtractFilter->GetOutput());
  try
    {
    statisticsFilter->Update();
    }
  catch (itk::ExceptionObject & e)
    {
    std::cerr << "exception checking files " << std::endl;
    std::cerr << e << std::endl;
    return EXIT_FAILURE;
    }
  if(statisticsFilter->GetMinimum() != 0.0 || statisticsFilter->GetMaximum() != 0.0)
    {
    std::cerr << "file written doesn't match file read." << std::endl
              << "min(" << statisticsFilter->GetMinimum()
              << ") max(" << statisticsFilter->GetMaximum()
              << std::endl;
    return EXIT_FAILURE;
    }
  return EXIT_SUCCESS;
}
