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
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkSimpleFilterWatcher.h"
#include "itkThresholdLabelerImageFilter.h"
#include "itkChangeLabelImageFilter.h"

#include "itkProjectionImageFilter.h"
#include "itkTestingMacros.h"

namespace itk
{
namespace ProjectionImageFilterNamespace
{
namespace Function
{

template <typename TInputPixel, typename TOutputPixel>
class BinaryAccumulator
{
public:
  BinaryAccumulator(unsigned long)
    : m_IsForeground(false)
  {}
  ~BinaryAccumulator() = default;

  inline void
  Initialize()
  {
    m_IsForeground = false;
  }

  inline void
  operator()(const TInputPixel & input)
  {
    if (input == 100)
    {
      m_IsForeground = true;
    }
  }

  inline TOutputPixel
  GetValue()
  {
    if (m_IsForeground)
    {
      return 100;
    }
    else
    {
      return 0;
    }
  }

  bool m_IsForeground;
};
} // end namespace Function

} // end namespace ProjectionImageFilterNamespace
} // end namespace itk

int
itkProjectionImageFilterTest(int argc, char * argv[])
{
  if (argc < 5)
  {
    std::cerr << "Missing Parameters " << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv);
    std::cerr << " InputImage OutputImage Foreground Background" << std::endl;
    return EXIT_FAILURE;
  }

  constexpr unsigned int Dimension = 3;

  using PixelType = unsigned char;
  using ImageType = itk::Image<PixelType, Dimension>;

  using ReaderType = itk::ImageFileReader<ImageType>;
  auto reader = ReaderType::New();
  reader->SetFileName(argv[1]);

  // produce an image with 3 labels: 0 (background), 100 and 200

  using LabelerType = itk::ThresholdLabelerImageFilter<ImageType, ImageType>;
  auto labeler = LabelerType::New();
  labeler->SetInput(reader->GetOutput());
  LabelerType::RealThresholdVector thresholds;
  thresholds.push_back(100);
  thresholds.push_back(200);
  labeler->SetRealThresholds(thresholds);

  using ChangeType = itk::ChangeLabelImageFilter<ImageType, ImageType>;
  auto change = ChangeType::New();
  change->SetInput(labeler->GetOutput());
  change->SetChange(1, 100);
  change->SetChange(2, 200);

  using FunctionType = itk::ProjectionImageFilterNamespace::Function::BinaryAccumulator<PixelType, PixelType>;

  using FilterType = itk::ProjectionImageFilter<ImageType, ImageType, FunctionType>;

  auto filter = FilterType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(filter, ProjectionImageFilter, ImageToImageFilter);


  filter->SetInput(change->GetOutput());

  // Test exceptions
  unsigned int projectionDimension = ImageType::ImageDimension;
  filter->SetProjectionDimension(projectionDimension);
  ITK_TRY_EXPECT_EXCEPTION(filter->Update());


  projectionDimension = ImageType::ImageDimension - 1;
  filter->SetProjectionDimension(projectionDimension);
  ITK_TEST_SET_GET_VALUE(projectionDimension, filter->GetProjectionDimension());


  itk::SimpleFilterWatcher watcher(filter, "filter");

  using WriterType = itk::ImageFileWriter<ImageType>;
  auto writer = WriterType::New();
  writer->SetInput(filter->GetOutput());
  writer->SetFileName(argv[2]);

  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());


  std::cout << "Test finished" << std::endl;
  return EXIT_SUCCESS;
}
