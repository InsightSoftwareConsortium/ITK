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

#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkAddImageFilter.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkPipelineMonitorImageFilter.h"
#include "itkTestingMacros.h"

int
itkAddImageFilterTest2(int argc, char * argv[])
{
  if (argc != 3)
  {
    std::cerr << "Usage: " << argv[0] << " <InputImage>"
              << " <OutputImage>" << std::endl;
    return EXIT_FAILURE;
  }
  const char * inputImage = argv[1];
  const char * outputImage = argv[2];

  // Define the dimension of the images
  constexpr unsigned int Dimension = 2;

  using FloatPixelType = float;
  using IntegerPixelType = unsigned short;

  using FloatImageType = itk::Image<FloatPixelType, Dimension>;
  using IntegerImageType = itk::Image<IntegerPixelType, Dimension>;

  using FloatReaderType = itk::ImageFileReader<FloatImageType>;
  FloatReaderType::Pointer floatReader = FloatReaderType::New();
  floatReader->SetFileName(inputImage);
  floatReader->SetUseStreaming(true);

  using IntegerReaderType = itk::ImageFileReader<IntegerImageType>;
  IntegerReaderType::Pointer integerReader = IntegerReaderType::New();
  integerReader->SetFileName(inputImage);
  integerReader->SetUseStreaming(true);

  constexpr unsigned int streams = 4;

  using IntegerMonitorFilterType = itk::PipelineMonitorImageFilter<IntegerImageType>;
  IntegerMonitorFilterType::Pointer integerMonitorFilter = IntegerMonitorFilterType::New();
  integerMonitorFilter->SetInput(integerReader->GetOutput());

  // Test with different input types.
  using AddFilterType = itk::AddImageFilter<FloatImageType, IntegerImageType, FloatImageType>;
  AddFilterType::Pointer addFilter = AddFilterType::New();
  addFilter->SetInput1(floatReader->GetOutput());
  addFilter->SetInput2(integerMonitorFilter->GetOutput());

  using MonitorFilterType = itk::PipelineMonitorImageFilter<FloatImageType>;
  MonitorFilterType::Pointer monitorFilter = MonitorFilterType::New();
  monitorFilter->SetInput(addFilter->GetOutput());

  using WriterType = itk::ImageFileWriter<FloatImageType>;
  WriterType::Pointer writer = WriterType::New();
  writer->SetInput(monitorFilter->GetOutput());
  writer->SetNumberOfStreamDivisions(streams);
  writer->SetFileName(outputImage);

  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());

  ITK_TEST_EXPECT_TRUE(monitorFilter->VerifyInputFilterExecutedStreaming(streams));
  ITK_TEST_EXPECT_TRUE(integerMonitorFilter->VerifyInputFilterExecutedStreaming(streams));

  return EXIT_SUCCESS;
}
