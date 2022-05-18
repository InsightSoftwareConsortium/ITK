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
#include "itkMaskNeighborhoodOperatorImageFilter.h"
#include "itkRescaleIntensityImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkSobelOperator.h"
#include "itkTestingMacros.h"

int
itkMaskNeighborhoodOperatorImageFilterTest(int argc, char * argv[])
{
  if (argc < 3)
  {
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv) << " InputImage OutputImage\n";
    return EXIT_FAILURE;
  }

  constexpr unsigned int Dimension = 2;
  using PixelType = float;
  using OutputPixelType = unsigned char;

  using InputImageType = itk::Image<PixelType, Dimension>;
  using OutputImageType = itk::Image<OutputPixelType, Dimension>;

  itk::ImageFileReader<InputImageType>::Pointer input = itk::ImageFileReader<InputImageType>::New();
  input->SetFileName(argv[1]);
  input->Update();

  // create a mask the size of the input file
  using MaskImageType = itk::Image<unsigned char, Dimension>;
  auto                      mask1 = MaskImageType::New();
  auto                      mask2 = MaskImageType::New();
  MaskImageType::RegionType region;
  MaskImageType::SizeType   size;
  MaskImageType::IndexType  index;

  region = input->GetOutput()->GetBufferedRegion();
  mask1->SetRegions(region);
  mask1->Allocate(true); // initialize buffer to zero

  mask2->SetRegions(region);
  mask2->Allocate(true); // initialize buffer to zero


  size = region.GetSize();
  index = region.GetIndex();
  unsigned int width = size[0];
  size[0] = width / 2 - static_cast<unsigned int>(.25 * static_cast<float>(width));
  index[0] = size[0] + static_cast<unsigned int>(.25 * static_cast<float>(width));
  region.SetSize(size);
  region.SetIndex(index);
  {
    itk::ImageRegionIterator<MaskImageType> it(mask1, region);
    it.GoToBegin();
    while (!it.IsAtEnd())
    {
      it.Set(1);
      ++it;
    }
  }
  index[0] = 0;
  region.SetIndex(index);
  {
    itk::ImageRegionIterator<MaskImageType> it(mask2, region);
    it.GoToBegin();
    while (!it.IsAtEnd())
    {
      it.Set(1);
      ++it;
    }
  }

  // Create a filter
  using FilterType = itk::MaskNeighborhoodOperatorImageFilter<InputImageType, MaskImageType, InputImageType>;

  itk::SobelOperator<float, 2> sobelHorizontal;
  sobelHorizontal.SetDirection(0);
  sobelHorizontal.CreateDirectional();

  itk::SobelOperator<float, 2> sobelVertical;
  sobelVertical.SetDirection(1);
  sobelVertical.CreateDirectional();

  auto filter1 = FilterType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(filter1, MaskNeighborhoodOperatorImageFilter, NeighborhoodOperatorImageFilter);

  filter1->SetInput(input->GetOutput());
  filter1->SetMaskImage(mask1);
  filter1->SetOperator(sobelHorizontal);

  auto defaultValue = itk::NumericTraits<typename FilterType::OutputPixelType>::ZeroValue();
  filter1->SetDefaultValue(defaultValue);
  ITK_TEST_SET_GET_VALUE(defaultValue, filter1->GetDefaultValue());

  bool useDefaultValue = false;
  ITK_TEST_SET_GET_BOOLEAN(filter1, UseDefaultValue, useDefaultValue);

  auto filter2 = FilterType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(filter2, MaskNeighborhoodOperatorImageFilter, NeighborhoodOperatorImageFilter);

  filter2->SetInput(filter1->GetOutput());
  filter2->SetMaskImage(mask2);
  filter2->SetOperator(sobelVertical);

  filter2->SetDefaultValue(defaultValue);
  ITK_TEST_SET_GET_VALUE(defaultValue, filter2->GetDefaultValue());

  ITK_TEST_SET_GET_BOOLEAN(filter2, UseDefaultValue, useDefaultValue);

  using RescaleFilterType = itk::RescaleIntensityImageFilter<InputImageType, OutputImageType>;
  auto rescaler = RescaleFilterType::New();
  rescaler->SetOutputMinimum(0);
  rescaler->SetOutputMaximum(255);
  rescaler->SetInput(filter2->GetOutput());

  // Generate test image
  itk::ImageFileWriter<OutputImageType>::Pointer writer = itk::ImageFileWriter<OutputImageType>::New();
  writer->SetInput(rescaler->GetOutput());
  writer->SetFileName(argv[2]);

  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());

  return EXIT_SUCCESS;
}
