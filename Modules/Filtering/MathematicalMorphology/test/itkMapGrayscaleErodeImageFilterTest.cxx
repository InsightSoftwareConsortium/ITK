/*=========================================================================
 *
 *  Copyright NumFOCUS
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

#include <fstream>
#include "itkGrayscaleErodeImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkTextOutput.h"
#include "itkSimpleFilterWatcher.h"
#include "itkFlatStructuringElement.h"

int
itkMapGrayscaleErodeImageFilterTest(int ac, char * av[])
{
  // Comment the following if you want to use the itk text output window
  itk::OutputWindow::SetInstance(itk::TextOutput::New());

  if (ac < 6)
  {
    std::cerr << "Usage: " << av[0] << " InputImage BASIC HISTO ANCHOR VHGW" << std::endl;
    return -1;
  }

  unsigned int const dim = 2;
  using ImageType = itk::Image<unsigned short, dim>;

  using ReaderType = itk::ImageFileReader<ImageType>;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName(av[1]);

  // Create a filter
  using SRType = itk::FlatStructuringElement<dim>;
  using FilterType = itk::GrayscaleErodeImageFilter<ImageType, ImageType, SRType>;
  FilterType::Pointer filter = FilterType::New();
  filter->SetInput(reader->GetOutput());

  itk::SimpleFilterWatcher watcher(filter, "filter");

  using RadiusType = FilterType::RadiusType;

  // test default values
  RadiusType r1;
  r1.Fill(1);
  if (filter->GetRadius() != r1)
  {
    std::cerr << "Wrong default Radius: " << filter->GetRadius() << std::endl;
    return EXIT_FAILURE;
  }

  if (filter->GetAlgorithm() != FilterType::HISTO)
  {
    std::cerr << "Wrong default algorithm." << std::endl;
    return EXIT_FAILURE;
  }

  try
  {
    filter->SetRadius(4);

    using WriterType = itk::ImageFileWriter<ImageType>;
    WriterType::Pointer writer = WriterType::New();
    writer->SetInput(filter->GetOutput());

    filter->SetAlgorithm(FilterType::BASIC);
    writer->SetFileName(av[2]);
    writer->Update();

    filter->SetAlgorithm(FilterType::HISTO);
    writer->SetFileName(av[3]);
    writer->Update();

    filter->SetAlgorithm(FilterType::ANCHOR);
    writer->SetFileName(av[4]);
    writer->Update();

    filter->SetAlgorithm(FilterType::VHGW);
    writer->SetFileName(av[5]);
    writer->Update();
  }
  catch (const itk::ExceptionObject & e)
  {
    std::cerr << "Exception detected: " << e.GetDescription();
    return EXIT_FAILURE;
  }

  // Generate test image
  using WriterType = itk::ImageFileWriter<ImageType>;
  WriterType::Pointer writer = WriterType::New();
  writer->SetInput(filter->GetOutput());
  writer->SetFileName(av[2]);
  writer->Update();

  return EXIT_SUCCESS;
}
