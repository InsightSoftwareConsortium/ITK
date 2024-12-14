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

#include <iostream>

#include "itkLabelStatisticsImageFilter.h"
#include "itkImageFileReader.h"

#include "itkSimpleFilterWatcher.h"
#include "itkTestingMacros.h"


int
itkLabelStatisticsImageFilterTest(int argc, char * argv[])
{
  std::cout << "itkLabelStatisticsImageFilterTest Start" << '\n';

  if (argc < 4)
  {
    std::cerr << "Missing Arguments" << '\n';
    std::cerr << "Usage: " << '\n';
    std::cerr << itkNameOfTestExecutableMacro(argv) << " inputImage labeledImage useHistograms [numberOfStreamDivision]"
              << '\n';
    return EXIT_FAILURE;
  }
  using ImageType = itk::Image<unsigned char, 2>;

  using ReaderType = itk::ImageFileReader<ImageType>;

  auto reader1 = ReaderType::New();
  auto reader2 = ReaderType::New();

  reader1->SetFileName(argv[1]);
  reader2->SetFileName(argv[2]);


  unsigned int numberOfStreamDivisions = 1;

  if (argc > 4)
  {
    numberOfStreamDivisions = std::max(std::stoi(argv[4]), 1);
  }

  using FilterType = itk::LabelStatisticsImageFilter<ImageType, ImageType>;

  auto filter = FilterType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(filter, LabelStatisticsImageFilter, ImageSink);


  const itk::SimpleFilterWatcher filterWatch(filter);

  auto useHistograms = static_cast<bool>(std::stoi(argv[3]));
  ITK_TEST_SET_GET_BOOLEAN(filter, UseHistograms, useHistograms);

  filter->SetNumberOfStreamDivisions(numberOfStreamDivisions);
  ITK_TEST_SET_GET_VALUE(numberOfStreamDivisions, filter->GetNumberOfStreamDivisions());

  filter->SetInput(reader1->GetOutput());
  filter->SetLabelInput(reader2->GetOutput());

  try
  {
    filter->Update();
  }
  catch (const itk::ExceptionObject & excp)
  {
    std::cerr << "Exception caught ! " << '\n';
    std::cerr << excp << '\n';
    return EXIT_FAILURE;
  }

  const unsigned int numberOfObjects = filter->GetNumberOfObjects();
  const unsigned int numberOfLabels = filter->GetNumberOfLabels();

  using RealType = FilterType::RealType;
  using BoundingBoxType = FilterType::BoundingBoxType;
  using RegionType = FilterType::RegionType;
  using LabelPixelType = FilterType::LabelPixelType;

  LabelPixelType labelValue = 0.0;

  std::cout << "There are " << numberOfLabels << " labels" << '\n';
  std::cout << "There are " << numberOfObjects << " objects" << '\n';

  unsigned int labelCount = 0;
  // Try to validate that the numberOfLabels in the ValidLabelList is
  // equal to the number of labels reported
  for (auto vIt = filter->GetValidLabelValues().begin(); vIt != filter->GetValidLabelValues().end(); ++vIt)
  {
    if (filter->HasLabel(*vIt))
    {
      ++labelCount;
    }
  }
  if (labelCount != numberOfLabels)
  {
    std::cerr << "Valid Labels Mismatch found!" << '\n';
    std::cerr << labelCount << " != " << numberOfLabels << '\n';
    return EXIT_FAILURE;
  }

  // Try two labels: one that exists and one that does not
  for (int i = 0; i < 2; ++i)
  {
    // Find an existing label
    if (i == 0)
    {
      labelValue = 0;
      while (!filter->HasLabel(labelValue))
      {
        labelValue++;
      }
      std::cout << "Label Statistics for label "
                << static_cast<itk::NumericTraits<LabelPixelType>::PrintType>(labelValue) << " which exists" << '\n';
    }
    // Find a non existent label
    if (i != 0)
    {
      labelValue = 0;
      while (filter->HasLabel(labelValue))
      {
        labelValue++;
      }
      std::cout << "Label Statistics for label "
                << static_cast<itk::NumericTraits<LabelPixelType>::PrintType>(labelValue) << " which does not exist"
                << '\n';
    }


    const RealType        min = filter->GetMinimum(labelValue);
    const RealType        max = filter->GetMaximum(labelValue);
    const RealType        median = filter->GetMedian(labelValue);
    const RealType        mean = filter->GetMean(labelValue);
    const RealType        sigma = filter->GetSigma(labelValue);
    const RealType        variance = filter->GetVariance(labelValue);
    const RealType        sum = filter->GetSum(labelValue);
    const BoundingBoxType box = filter->GetBoundingBox(labelValue);
    const RegionType      region = filter->GetRegion(labelValue);

    std::cout << "Minimum   = " << min << '\n';
    std::cout << "Maximum   = " << max << '\n';
    std::cout << "Median    = " << median << '\n';
    std::cout << "Mean      = " << mean << '\n';
    std::cout << "Sigma     = " << sigma << '\n';
    std::cout << "Variance  = " << variance << '\n';
    std::cout << "Sum       = " << sum << '\n';
    std::cout << "Region    = " << region << '\n';

    auto itr = box.begin();
    while (itr != box.end())
    {
      std::cout << "Index = " << *itr << '\n';
      ++itr;
    }
  }
  return EXIT_SUCCESS;
}
