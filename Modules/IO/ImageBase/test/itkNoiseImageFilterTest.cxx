/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/

#include <fstream>
#include "itkNoiseImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkTextOutput.h"
#include "itkRescaleIntensityImageFilter.h"
#include "itkSimpleFilterWatcher.h"
#include "itkTestingMacros.h"

int
itkNoiseImageFilterTest(int argc, char * argv[])
{
  // Comment the following if you want to use the itk text output window
  itk::OutputWindow::SetInstance(itk::TextOutput::New());

  if (argc < 3)
  {
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv) << " InputImage BaselineImage\n";
    return EXIT_FAILURE;
  }

  itk::Size<2> radius;
  using myImageIn = itk::Image<unsigned short, 2>;
  using myImageOut = itk::Image<float, 2>;
  using myImageChar = itk::Image<unsigned char, 2>;
  itk::ImageFileReader<myImageIn>::Pointer input = itk::ImageFileReader<myImageIn>::New();
  input->SetFileName(argv[1]);

  // Create a filter
  using FilterType = itk::NoiseImageFilter<myImageIn, myImageOut>;

  auto                     filter = FilterType::New();
  itk::SimpleFilterWatcher filterWatch(filter);

  using RescaleFilterType = itk::RescaleIntensityImageFilter<myImageOut, myImageChar>;

  auto rescale = RescaleFilterType::New();
  rescale->SetOutputMinimum(0);
  rescale->SetOutputMaximum(255);
  rescale->SetInput(filter->GetOutput());

  try
  {
    radius.Fill(5);
    filter->SetInput(input->GetOutput());
    filter->SetRadius(radius);
    filter->Update();
  }
  catch (const itk::ExceptionObject & e)
  {
    std::cerr << "Exception detected: " << e.GetDescription();
    return -1;
  }

  // Generate test image
  itk::ImageFileWriter<myImageChar>::Pointer writer;
  writer = itk::ImageFileWriter<myImageChar>::New();
  writer->SetInput(rescale->GetOutput());
  writer->SetFileName(argv[2]);
  writer->Update();

  return EXIT_SUCCESS;
}
