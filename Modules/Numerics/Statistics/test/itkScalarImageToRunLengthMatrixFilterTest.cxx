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

// Insight classes
#include "itkImageRegionIterator.h"


#include "itkScalarImageToRunLengthMatrixFilter.h"
#include "itkMath.h"

int
itkScalarImageToRunLengthMatrixFilterTest(int, char *[])
{

  // Data definitions
  constexpr unsigned int IMGWIDTH = 5;
  constexpr unsigned int IMGHEIGHT = 5;
  constexpr unsigned int NDIMENSION = 2;


  //------------------------------------------------------
  // Create a simple test images
  //------------------------------------------------------
  using InputImageType = itk::Image<unsigned char, NDIMENSION>;

  using InputImageIterator = itk::ImageRegionIterator<InputImageType>;


  auto image = InputImageType::New();
  auto mask = InputImageType::New();


  const InputImageType::SizeType inputImageSize = { { IMGWIDTH, IMGHEIGHT } };

  InputImageType::RegionType region;

  region.SetSize(inputImageSize);
  {
    const InputImageType::IndexType index{};
    region.SetIndex(index);
  }

  //--------------------------------------------------------------------------
  // Set up the image first. It looks like:
  //  1 2 1 2 1
  //  1 2 1 2 1
  //  1 2 1 2 1
  //  1 2 1 2 1
  //  1 2 1 2 1
  //--------------------------------------------------------------------------

  image->SetRegions(region);
  image->Allocate();

  // setup the iterator
  InputImageIterator imageIt(image, image->GetBufferedRegion());

  imageIt.GoToBegin();

  for (unsigned int i = 0; i < 5; ++i)
  {
    for (unsigned int j = 0; j < 5; j++, ++imageIt)
    {
      imageIt.Set(j % 2 + 1);
    }
  }

  //--------------------------------------------------------------------------
  // Set up the mask next. It looks like:
  //  0 0 0 0 0
  //  0 0 1 0 0
  //  0 0 1 0 0
  //  0 0 1 0 0
  //  0 0 0 0 0
  //--------------------------------------------------------------------------

  mask->SetRegions(region);
  mask->Allocate();

  // setup the iterator
  InputImageIterator maskIt(mask, mask->GetBufferedRegion());
  maskIt.GoToBegin();
  for (int i = 0; i < 5; ++i)
    for (int j = 0; j < 5; j++, ++maskIt)
    {
      if (j == 2 && i > 0 && i < 4)
      {
        maskIt.Set(1);
      }
      else
      {
        maskIt.Set(0);
      }
    }

  try
  {

    using FilterType = itk::Statistics::ScalarImageToRunLengthMatrixFilter<InputImageType>;

    auto filter = FilterType::New();

    filter->SetInput(image);

    const InputImageType::OffsetType      offset1 = { { 0, -1 } };
    const InputImageType::OffsetType      offset2 = { { -1, 0 } };
    const FilterType::OffsetVectorPointer offsetV = FilterType::OffsetVector::New();
    offsetV->push_back(offset1);
    offsetV->push_back(offset2);

    filter->SetOffsets(offsetV);
    filter->SetMaskImage(mask);
    // purposely setting the max value to max(Image)+1
    filter->SetPixelValueMinMax(0, 3);
    filter->SetDistanceValueMinMax(0, 8);
    filter->SetNumberOfBinsPerAxis(5);
    filter->Update();
    const FilterType::HistogramType * hist = filter->GetOutput();


    //--------------------------------------------------------------------------
    // Test the histogram.
    //--------------------------------------------------------------------------
    bool passed = true;

    const unsigned int frequencies[5][5] = {
      { 0, 3, 0, 0, 0 }, { 0, 1, 0, 0, 0 }, { 0, 0, 0, 0, 0 }, { 0, 0, 0, 0, 0 }, { 0, 0, 0, 0, 0 }
    };

    for (unsigned int i = 0; i < 5; ++i)
    {
      for (unsigned int j = 0; j < 5; ++j)
      {
        using IndexType = FilterType::HistogramType::IndexType;
        IndexType index(hist->GetMeasurementVectorSize());
        index[0] = i;
        index[1] = j;
        if (hist->GetFrequency(index) != frequencies[j][i])
        {
          std::cerr << "Expected frequency  (i,j)= " << '(' << i << ',' << j << ')' << frequencies[j][i]
                    << ", calculated = " << hist->GetFrequency(index) << '\n';
          passed = false;
        }
      }
    }
    const unsigned int totalF = hist->GetTotalFrequency();
    if (totalF != 4)
    {
      std::cerr << "Expected total frequency = 4, calculated = " << totalF << '\n';
      passed = false;
    }

    filter = FilterType::New();

    filter->SetInput(image);
    filter->SetOffsets(offsetV);
    filter->SetMaskImage(mask);
    filter->SetInsidePixelValue(0);
    // purposely setting the max value to max(Image)+1
    filter->SetPixelValueMinMax(0, 3);
    filter->SetDistanceValueMinMax(0, 8);
    filter->SetNumberOfBinsPerAxis(5);

    if (filter->GetInsidePixelValue() != 0)
    {
      std::cerr << "Error: " << '\n';
      std::cerr << "GetInsidePixelValue() is not returning the expected value" << '\n';
      passed = false;
    }
    if (filter->GetMaskImage() == nullptr)
    {
      std::cerr << "Error: " << '\n';
      std::cerr << "Mask should not be null." << '\n';
      passed = false;
    }
    if (filter->GetMin() != 0)
    {
      std::cerr << "Error: " << '\n';
      std::cerr << "GetMin() is not returning the expected value" << '\n';
      passed = false;
    }
    if (filter->GetMax() != 3)
    {
      std::cerr << "Error: " << '\n';
      std::cerr << "GetMax() is not returning the expected value" << '\n';
      passed = false;
    }
    if (itk::Math::NotExactlyEquals(filter->GetMinDistance(), 0))
    {
      std::cerr << "Error: " << '\n';
      std::cerr << "GetMinDistance() is not returning the expected value" << '\n';
      passed = false;
    }
    if (itk::Math::NotExactlyEquals(filter->GetMaxDistance(), 8))
    {
      std::cerr << "Error: " << '\n';
      std::cerr << "GetMaxDistance() is not returning the expected value" << '\n';
      passed = false;
    }

    const FilterType::OffsetVector * offsetVector = filter->GetOffsets();
    if (offsetVector->size() != 2 || (*offsetVector)[0][0] != 0 || (*offsetVector)[0][1] != -1 ||
        (*offsetVector)[1][0] != -1 || (*offsetVector)[1][1] != 0)
    {
      std::cerr << "Error: " << '\n';
      std::cerr << "GetOffsets() is not returning the correct offsets" << '\n';
      passed = false;
    }
    if (filter->GetNumberOfBinsPerAxis() != 5)
    {
      std::cerr << "Error: " << '\n';
      std::cerr << "GetNumberOfBinsPerAxis() is not returning the expected value" << '\n';
      passed = false;
    }

    filter->Update();
    hist = filter->GetOutput();

    const unsigned int frequencies2[5][5] = {
      { 0, 12, 0, 10, 0 }, { 0, 0, 0, 0, 0 }, { 0, 3, 0, 2, 0 }, { 0, 0, 0, 0, 0 }, { 0, 0, 0, 0, 0 }
    };

    for (unsigned int i = 0; i < 5; ++i)
    {
      for (unsigned int j = 0; j < 5; ++j)
      {
        using IndexType = FilterType::HistogramType::IndexType;
        IndexType index(hist->GetMeasurementVectorSize());
        index[0] = i;
        index[1] = j;
        if (hist->GetFrequency(index) != frequencies2[j][i])
        {
          std::cerr << "Expected frequency2  (i,j)= " << '(' << i << ',' << j << ')' << frequencies2[j][i]
                    << ", calculated = " << hist->GetFrequency(index) << '\n';
          passed = false;
        }
      }
    }

    filter->Print(std::cout, 3);

    if (!passed)
    {
      std::cerr << "Test failed" << '\n';
      return EXIT_FAILURE;
    }
    else
    {
      std::cerr << "Test succeeded" << '\n';
      return EXIT_SUCCESS;
    }
  }
  catch (const itk::ExceptionObject & err)
  {
    std::cerr << "ExceptionObject caught !" << '\n';
    std::cerr << err << '\n';
    std::cerr << "Test failed" << '\n';
    return EXIT_FAILURE;
  }
}
