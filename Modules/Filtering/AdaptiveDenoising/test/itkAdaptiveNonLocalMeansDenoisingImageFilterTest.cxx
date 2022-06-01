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

#include <set>
#include "itkAdaptiveNonLocalMeansDenoisingImageFilter.h"

#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkTestingMacros.h"
#include "itkMath.h"

template <typename TFilter>
class CommandProgressUpdate : public itk::Command
{
public:
  using Self = CommandProgressUpdate<TFilter>;
  using Superclass = itk::Command;
  using Pointer = itk::SmartPointer<CommandProgressUpdate<TFilter>>;
  itkNewMacro(CommandProgressUpdate);

protected:
  CommandProgressUpdate() = default;

  using FilterType = TFilter;

  unsigned int m_CurrentProgress{ 0 };

public:
  void
  Execute(itk::Object * caller, const itk::EventObject & event) override
  {
    auto * po = dynamic_cast<itk::ProcessObject *>(caller);
    if (!po)
      return;
    if (typeid(event) == typeid(itk::ProgressEvent))
    {
      if (this->m_CurrentProgress < 99)
      {
        this->m_CurrentProgress++;
        if (this->m_CurrentProgress % 10 == 0)
        {
          std::cout << this->m_CurrentProgress << std::flush;
        }
        else
        {
          std::cout << "*" << std::flush;
        }
      }
    }
  }

  void
  Execute(const itk::Object * object, const itk::EventObject & event) override
  {
    auto * po = dynamic_cast<itk::ProcessObject *>(const_cast<itk::Object *>(object));
    if (!po)
      return;

    if (typeid(event) == typeid(itk::ProgressEvent))
    {
      if (this->m_CurrentProgress < 99)
      {
        this->m_CurrentProgress++;
        if (this->m_CurrentProgress % 10 == 0)
        {
          std::cout << this->m_CurrentProgress << std::flush;
        }
        else
        {
          std::cout << "*" << std::flush;
        }
      }
    }
  }
};

int
itkAdaptiveNonLocalMeansDenoisingImageFilterTest(int argc, char * argv[])
{
  if (argc < 4)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv) << " inputImage"
              << " outputImage"
              << " similarityMetric (0: PEARSON_CORRELATION; 1: MEAN_SQUARES)" << std::endl;
    return EXIT_FAILURE;
  }

  constexpr unsigned int Dimension = 2;
  using PixelType = float;
  using ImageType = itk::Image<PixelType, Dimension>;

  using ReaderType = itk::ImageFileReader<ImageType>;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName(argv[1]);

  using DenoiserType = itk::AdaptiveNonLocalMeansDenoisingImageFilter<ImageType, ImageType>;
  DenoiserType::Pointer filter = DenoiserType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(filter, AdaptiveNonLocalMeansDenoisingImageFilter, NonLocalPatchBasedImageFilter);

  filter->SetInput(reader->GetOutput());
  filter->SetUseRicianNoiseModel(false);
  ITK_TEST_SET_GET_VALUE(false, filter->GetUseRicianNoiseModel());

  DenoiserType::NeighborhoodRadiusType neighborhoodPatchRadius;
  DenoiserType::NeighborhoodRadiusType neighborhoodSearchRadius;

  neighborhoodPatchRadius.Fill(1);
  neighborhoodSearchRadius.Fill(2);

  filter->SetNeighborhoodSearchRadius(neighborhoodSearchRadius);
  filter->SetNeighborhoodPatchRadius(neighborhoodPatchRadius);

  DenoiserType::NeighborhoodRadiusType neighborhoodRadiusForLocalMeanAndVariance;
  neighborhoodRadiusForLocalMeanAndVariance.Fill(1);

  filter->SetNeighborhoodRadiusForLocalMeanAndVariance(neighborhoodRadiusForLocalMeanAndVariance);

  filter->SetEpsilon(0.00001f);
  ITK_TEST_EXPECT_TRUE(itk::Math::FloatAlmostEqual(0.00001f, filter->GetEpsilon()));

  filter->SetMeanThreshold(0.95f);
  ITK_TEST_EXPECT_TRUE(itk::Math::FloatAlmostEqual(0.95f, filter->GetMeanThreshold()));

  filter->SetVarianceThreshold(0.5f);
  ITK_TEST_EXPECT_TRUE(itk::Math::FloatAlmostEqual(0.5f, filter->GetVarianceThreshold()));

  filter->SetSmoothingFactor(1.0f);
  ITK_TEST_EXPECT_TRUE(itk::Math::FloatAlmostEqual(1.0f, filter->GetSmoothingFactor()));

  filter->SetSmoothingVariance(2.0f);
  ITK_TEST_EXPECT_TRUE(itk::Math::FloatAlmostEqual(2.0f, filter->GetSmoothingVariance()));

  auto similarityMetric = static_cast<DenoiserType::SimilarityMetricEnum>(std::atoi(argv[3]));
  filter->SetSimilarityMetric(similarityMetric);
  ITK_TEST_SET_GET_VALUE(similarityMetric, filter->GetSimilarityMetric());

  using CommandType = CommandProgressUpdate<DenoiserType>;
  CommandType::Pointer observer = CommandType::New();
  filter->AddObserver(itk::ProgressEvent(), observer);

  ITK_TRY_EXPECT_NO_EXCEPTION(filter->Update());

  using WriterType = itk::ImageFileWriter<ImageType>;
  WriterType::Pointer writer = WriterType::New();
  writer->SetFileName(argv[2]);
  writer->SetInput(filter->GetOutput());

  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());


  // Test streaming enumeration for NonLocalPatchBasedImageFilterEnums::SimilarityMetric elements
  const std::set<itk::NonLocalPatchBasedImageFilterEnums::SimilarityMetric> allSimilarityMetric{
    itk::NonLocalPatchBasedImageFilterEnums::SimilarityMetric::PEARSON_CORRELATION,
    itk::NonLocalPatchBasedImageFilterEnums::SimilarityMetric::MEAN_SQUARES
  };
  for (const auto & ee : allSimilarityMetric)
  {
    std::cout << "STREAMED ENUM VALUE NonLocalPatchBasedImageFilterEnums::SimilarityMetric: " << ee << std::endl;
  }


  std::cout << "Test finished" << std::endl;
  return EXIT_SUCCESS;
}
