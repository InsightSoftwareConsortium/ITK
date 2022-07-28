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
#include "itkLabelOverlapMeasuresImageFilter.h"
#include "itkTestingMacros.h"

#include <iomanip>

template <unsigned int VImageDimension>
int
LabelOverlapMeasures(int, char * argv[])
{
  using PixelType = unsigned int;
  using ImageType = itk::Image<PixelType, VImageDimension>;

  using ReaderType = itk::ImageFileReader<ImageType>;
  auto reader1 = ReaderType::New();
  reader1->SetFileName(argv[2]);
  auto reader2 = ReaderType::New();
  reader2->SetFileName(argv[3]);

  using FilterType = itk::LabelOverlapMeasuresImageFilter<ImageType>;
  auto filter = FilterType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(filter, LabelOverlapMeasuresImageFilter, ImageToImageFilter);


  filter->SetSourceImage(reader1->GetOutput());
  filter->SetTargetImage(reader2->GetOutput());
  filter->Update();

  std::cout << "All Labels" << std::endl;
  std::cout << std::setw(10) << "   " << std::setw(17) << "Total" << std::setw(17) << "Union (jaccard)" << std::setw(17)
            << "Mean (dice)" << std::setw(17) << "Volume sim." << std::setw(17) << "False negative" << std::setw(17)
            << "False positive" << std::setw(17) << "False discovery" << std::endl;
  std::cout << std::setw(10) << "   ";
  std::cout << std::setw(17) << filter->GetTotalOverlap();
  std::cout << std::setw(17) << filter->GetUnionOverlap();
  std::cout << std::setw(17) << filter->GetMeanOverlap();
  std::cout << std::setw(17) << filter->GetVolumeSimilarity();
  std::cout << std::setw(17) << filter->GetFalseNegativeError();
  std::cout << std::setw(17) << filter->GetFalsePositiveError();
  std::cout << std::setw(17) << filter->GetFalseDiscoveryRate();
  std::cout << std::endl;

  std::cout << "Individual Labels" << std::endl;
  std::cout << std::setw(10) << "Label" << std::setw(17) << "Target" << std::setw(17) << "Union (jaccard)"
            << std::setw(17) << "Mean (dice)" << std::setw(17) << "Volume sim." << std::setw(17) << "False negative"
            << std::setw(17) << "False positive" << std::setw(17) << "False discovery" << std::endl;

  typename FilterType::MapType                 labelMap = filter->GetLabelSetMeasures();
  typename FilterType::MapType::const_iterator it;
  int                                          label = 0;
  for (it = labelMap.begin(); it != labelMap.end(); ++it)
  {
    if (it->first == 0)
    {
      continue;
    }

    label = it->first;

    std::cout << std::setw(10) << label;
    std::cout << std::setw(17) << filter->GetTargetOverlap(label);
    std::cout << std::setw(17) << filter->GetUnionOverlap(label);
    std::cout << std::setw(17) << filter->GetMeanOverlap(label);
    std::cout << std::setw(17) << filter->GetVolumeSimilarity(label);
    std::cout << std::setw(17) << filter->GetFalseNegativeError(label);
    std::cout << std::setw(17) << filter->GetFalsePositiveError(label);
    std::cout << std::setw(17) << filter->GetFalseDiscoveryRate(label);
    std::cout << std::endl;
  }


  // Check results when a non-existing label's metrics are queried
  //

  // Assume that no such label exists
  label = itk::NumericTraits<PixelType>::max();

  typename FilterType::RealType expectedValue = 0.0;
  typename FilterType::RealType result = filter->GetTargetOverlap(label);
  if (itk::Math::NotAlmostEquals(expectedValue, result))
  {
    std::cout << "Error in label " << static_cast<itk::NumericTraits<PixelType>::PrintType>(label) << ": ";
    std::cout << "Expected target overlap: " << expectedValue << ", but got " << result << std::endl;
    std::cout << "Test failed" << std::endl;
    return EXIT_FAILURE;
  }

  result = filter->GetUnionOverlap(label);
  if (itk::Math::NotAlmostEquals(expectedValue, result))
  {
    std::cout << "Error in label " << static_cast<itk::NumericTraits<PixelType>::PrintType>(label) << ": ";
    std::cout << "Expected union overlap: " << expectedValue << ", but got " << result << std::endl;
    std::cout << "Test failed" << std::endl;
    return EXIT_FAILURE;
  }

  result = filter->GetVolumeSimilarity(label);
  if (itk::Math::NotAlmostEquals(expectedValue, result))
  {
    std::cout << "Error in label " << static_cast<itk::NumericTraits<PixelType>::PrintType>(label) << ": ";
    std::cout << "Expected volume similarity: " << expectedValue << ", but got " << result << std::endl;
    std::cout << "Test failed" << std::endl;
    return EXIT_FAILURE;
  }

  result = filter->GetFalseNegativeError(label);
  if (itk::Math::NotAlmostEquals(expectedValue, result))
  {
    std::cout << "Error in label " << static_cast<itk::NumericTraits<PixelType>::PrintType>(label) << ": ";
    std::cout << "Expected false negative error: " << expectedValue << ", but got " << result << std::endl;
    std::cout << "Test failed" << std::endl;
    return EXIT_FAILURE;
  }

  result = filter->GetFalsePositiveError(label);
  if (itk::Math::NotAlmostEquals(expectedValue, result))
  {
    std::cout << "Error in label " << static_cast<itk::NumericTraits<PixelType>::PrintType>(label) << ": ";
    std::cout << "Expected false positive error: " << expectedValue << ", but got " << result << std::endl;
    std::cout << "Test failed" << std::endl;
    return EXIT_FAILURE;
  }

  result = filter->GetFalseDiscoveryRate(label);
  if (itk::Math::NotAlmostEquals(expectedValue, result))
  {
    std::cout << "Error in label " << static_cast<itk::NumericTraits<PixelType>::PrintType>(label) << ": ";
    std::cout << "Expected false discovery rate: " << expectedValue << ", but got " << result << std::endl;
    std::cout << "Test failed" << std::endl;
    return EXIT_FAILURE;
  }

  return EXIT_SUCCESS;
}

int
itkLabelOverlapMeasuresImageFilterTest(int argc, char * argv[])
{
  if (argc < 4)
  {
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv) << " imageDimension sourceImage "
              << "targetImage" << std::endl;
    return EXIT_FAILURE;
  }

  // Instantiate the filter
  constexpr unsigned int ImageDimension = 3;
  using PixelType = unsigned int;
  using ImageType = itk::Image<PixelType, ImageDimension>;

  using LabelOverlapMeasuresImageFilterType = itk::LabelOverlapMeasuresImageFilter<ImageType>;

  LabelOverlapMeasuresImageFilterType::Pointer labelOverlapMeasuresImageFilter =
    LabelOverlapMeasuresImageFilterType::New();

  // Exercise basic object methods
  // Done outside the helper function in the test because GCC is limited
  // when calling overloaded base class functions.
  ITK_EXERCISE_BASIC_OBJECT_METHODS(labelOverlapMeasuresImageFilter, LabelOverlapMeasuresImageFilter, ImageSink);


  switch (std::stoi(argv[1]))
  {
    case 2:
      LabelOverlapMeasures<2>(argc, argv);
      break;
    case 3:
      LabelOverlapMeasures<3>(argc, argv);
      break;
    default:
      std::cerr << "Unsupported dimension" << std::endl;
      return EXIT_FAILURE;
  }
  return EXIT_SUCCESS;
}
