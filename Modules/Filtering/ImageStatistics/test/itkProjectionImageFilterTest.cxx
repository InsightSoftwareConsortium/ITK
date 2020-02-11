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

  constexpr int dim = 3;

  using PixelType = unsigned char;
  using ImageType = itk::Image<PixelType, dim>;

  using ReaderType = itk::ImageFileReader<ImageType>;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName(argv[1]);

  // produce an image with 3 labels: 0 (background), 100 and 200

  using LabelerType = itk::ThresholdLabelerImageFilter<ImageType, ImageType>;
  LabelerType::Pointer labeler = LabelerType::New();
  labeler->SetInput(reader->GetOutput());
  LabelerType::RealThresholdVector thresholds;
  thresholds.push_back(100);
  thresholds.push_back(200);
  labeler->SetRealThresholds(thresholds);

  using ChangeType = itk::ChangeLabelImageFilter<ImageType, ImageType>;
  ChangeType::Pointer change = ChangeType::New();
  change->SetInput(labeler->GetOutput());
  change->SetChange(1, 100);
  change->SetChange(2, 200);

  using FunctionType = itk::ProjectionImageFilterNamespace::Function::BinaryAccumulator<PixelType, PixelType>;

  using FilterType = itk::ProjectionImageFilter<ImageType, ImageType, FunctionType>;

  FilterType::Pointer filter = FilterType::New();
  filter->SetInput(change->GetOutput());

  itk::SimpleFilterWatcher watcher(filter, "filter");

  using WriterType = itk::ImageFileWriter<ImageType>;
  WriterType::Pointer writer = WriterType::New();
  writer->SetInput(filter->GetOutput());
  writer->SetFileName(argv[2]);

  try
  {
    writer->Update();
  }
  catch (const itk::ExceptionObject & excp)
  {
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
  }

  return EXIT_SUCCESS;
}
