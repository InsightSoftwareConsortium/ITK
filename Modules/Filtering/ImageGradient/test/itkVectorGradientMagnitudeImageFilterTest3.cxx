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

#include "itkVectorGradientMagnitudeImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkPipelineMonitorImageFilter.h"
#include "itkRGBToVectorImageAdaptor.h"
#include "itkTestingMacros.h"

int
itkVectorGradientMagnitudeImageFilterTest3(int argc, char * argv[])
{
  using RGBPixelType = itk::RGBPixel<unsigned char>;
  using RGBImageType = itk::Image<RGBPixelType, 3>;
  using Monitor1Filter = itk::PipelineMonitorImageFilter<RGBImageType>;
  using AdaptorType = itk::RGBToVectorImageAdaptor<RGBImageType>;
  using FilterType = itk::VectorGradientMagnitudeImageFilter<AdaptorType>;
  using ReaderType = itk::ImageFileReader<RGBImageType>;
  using Monitor2Filter = itk::PipelineMonitorImageFilter<FilterType::OutputImageType>;
  using WriterType = itk::ImageFileWriter<FilterType::OutputImageType>;

  if (argc < 4)
  {
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv) << " InputImage OutputImage Mode\n";
    return EXIT_FAILURE;
  }

  // Create a reader and filter
  auto reader = ReaderType::New();
  reader->SetFileName(argv[1]);

  auto monitor1 = Monitor1Filter::New();
  monitor1->SetInput(reader->GetOutput());
  monitor1->ClearPipelineOnGenerateOutputInformationOff();

  auto adaptor = AdaptorType::New();
  adaptor->SetImage(monitor1->GetOutput());

  auto filter = FilterType::New();
  filter->SetInput(adaptor);

  auto mode = static_cast<bool>(std::stoi(argv[3]));
#if !defined(ITK_FUTURE_LEGACY_REMOVE)
  if (mode)
  {
    filter->SetUsePrincipleComponentsOn();
  }
  else
  {
    filter->SetUsePrincipleComponentsOff();
  }
#endif
  ITK_TEST_SET_GET_BOOLEAN(filter, UsePrincipleComponents, mode);

  auto monitor2 = Monitor2Filter::New();
  monitor2->SetInput(filter->GetOutput());

  auto writer = WriterType::New();
  writer->SetFileName(argv[2]);
  writer->SetInput(monitor2->GetOutput());
  writer->SetNumberOfStreamDivisions(4);

  try
  {
    writer->Update();
  }
  catch (const itk::ExceptionObject & e)
  {
    std::cerr << "Exception detected: " << e.GetDescription();
    return EXIT_FAILURE;
  }
  catch (...)
  {
    std::cerr << "Some other exception occurred" << std::endl;
    return EXIT_FAILURE;
  }

  if (!monitor1->VerifyAllInputCanStream(4 - 1))
  {
    std::cout << "Input of filter failed to execute as expected!" << std::endl;
    std::cout << monitor1;
    return EXIT_FAILURE;
  }

  // this verifies that the pipeline was executed as expected along
  // with correct region propagation and output information
  if (!monitor2->VerifyAllInputCanStream(4))
  {
    std::cout << "Filter failed to execute as expected!" << std::endl;
    std::cout << monitor2;
    return EXIT_FAILURE;
  }

  return EXIT_SUCCESS;
}
