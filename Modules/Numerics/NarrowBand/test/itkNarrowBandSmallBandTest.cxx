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

#include "itkFiniteDifferenceFunction.h"
#include "itkImage.h"
#include "itkNarrowBand.h"
#include "itkNarrowBandImageFilterBase.h"
#include "itkTestingMacros.h"
#include <iostream>

namespace
{
using ImageType = itk::Image<float, 2>;

class UnitUpdateFunction : public itk::FiniteDifferenceFunction<ImageType>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(UnitUpdateFunction);
  using Self = UnitUpdateFunction;
  using Superclass = itk::FiniteDifferenceFunction<ImageType>;
  using Pointer = itk::SmartPointer<Self>;
  itkNewMacro(Self);
  itkOverrideGetNameOfClassMacro(UnitUpdateFunction);

  PixelType
  ComputeUpdate(const NeighborhoodType &, void * globalData, const FloatOffsetType &) override
  {
    *static_cast<bool *>(globalData) = true;
    return 1.0f;
  }

  TimeStepType
  ComputeGlobalTimeStep(void * globalData) const override
  {
    // Empty workers must not freeze the nonempty workers' updates.
    return *static_cast<bool *>(globalData) ? 1.0 : 0.0;
  }

  void *
  GetGlobalDataPointer() const override
  {
    return new bool(false);
  }

  void
  ReleaseGlobalDataPointer(void * globalData) const override
  {
    delete static_cast<bool *>(globalData);
  }

protected:
  UnitUpdateFunction()
  {
    const auto radius = RadiusType::Filled(0);
    this->SetRadius(radius);
  }
  ~UnitUpdateFunction() override = default;
};

class SmallBandFilter : public itk::NarrowBandImageFilterBase<ImageType, ImageType>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(SmallBandFilter);
  using Self = SmallBandFilter;
  using Superclass = itk::NarrowBandImageFilterBase<ImageType, ImageType>;
  using Pointer = itk::SmartPointer<Self>;
  itkNewMacro(Self);
  itkOverrideGetNameOfClassMacro(SmallBandFilter);

  void
  Configure(unsigned int count, bool shrink)
  {
    m_Count = count;
    m_Shrink = shrink;
    this->Modified();
  }

  void
  RequestSplitRegion(size_t index)
  {
    ThreadRegionType region;
    this->GetSplitRegion(index, region);
  }

  void
  CreateNarrowBand() override
  {
    this->ClearNarrowBand();
    if (m_Shrink && this->GetElapsedIterations() > 0)
    {
      return;
    }
    for (unsigned int i = 0; i < m_Count; ++i)
    {
      const IndexType index{ static_cast<itk::IndexValueType>(i + 1), 1 };
      this->InsertNarrowBandNode(index, 0.0f, 2);
    }
  }

protected:
  SmallBandFilter()
  {
    this->SetDifferenceFunction(UnitUpdateFunction::New());
    this->m_ReinitializationFrequency = 1;
  }
  ~SmallBandFilter() override = default;

  bool
  Halt() override
  {
    return this->GetElapsedIterations() >= 2;
  }

private:
  unsigned int m_Count{};
  bool         m_Shrink{};
};
} // namespace

int
itkNarrowBandSmallBandTest(int, char *[])
{
  for (const unsigned int count : { 0u, 1u, 2u, 5u, 17u })
  {
    const auto band = itk::NarrowBand<int>::New();
    for (unsigned int i = 0; i < count; ++i)
    {
      band->PushBack(static_cast<int>(i));
    }
    for (const unsigned int workUnits : { 0u, 1u, 2u, 8u, 25u })
    {
      const auto regions = band->SplitBand(workUnits);
      ITK_TEST_EXPECT_EQUAL(regions.size(), workUnits);
      auto cursor = band->Begin();
      for (const auto & region : regions)
      {
        ITK_TEST_EXPECT_TRUE(region.Begin == cursor);
        cursor = region.End;
      }
      if (workUnits > 0)
      {
        ITK_TEST_EXPECT_TRUE(cursor == band->End());
      }
    }
  }

  for (const unsigned int workUnits : { 1u, 8u })
  {
    for (const unsigned int count : { 0u, 1u, 3u })
    {
      for (const bool shrink : { false, true })
      {
        const auto                input = ImageType::New();
        const ImageType::SizeType size{ 8, 4 };
        input->SetRegions(ImageType::RegionType{ size });
        input->Allocate();
        input->FillBuffer(10.0f);
        const auto filter = SmallBandFilter::New();
        filter->SetInput(input);
        filter->SetNumberOfWorkUnits(workUnits);
        filter->Configure(count, shrink);
        ITK_TRY_EXPECT_NO_EXCEPTION(filter->Update());
        const unsigned int iterations = count == 0 ? 0 : (shrink ? 1 : 2);
        ITK_TEST_EXPECT_EQUAL(filter->GetElapsedIterations(), iterations);
        for (unsigned int y = 0; y < 4; ++y)
        {
          for (unsigned int x = 0; x < 8; ++x)
          {
            const ImageType::IndexType index{ static_cast<itk::IndexValueType>(x),
                                              static_cast<itk::IndexValueType>(y) };
            const float                expected = 10.0f + ((y == 1 && x > 0 && x <= count) ? iterations : 0);
            ITK_TEST_EXPECT_EQUAL(filter->GetOutput()->GetPixel(index), expected);
            ITK_TEST_EXPECT_EQUAL(input->GetPixel(index), 10.0f);
          }
        }
        ITK_TRY_EXPECT_EXCEPTION(filter->RequestSplitRegion(workUnits));
      }
    }
  }
  return EXIT_SUCCESS;
}
