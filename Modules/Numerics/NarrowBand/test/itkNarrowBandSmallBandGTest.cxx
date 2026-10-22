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

// First include the header files to be tested:
#include "itkNarrowBand.h"
#include "itkNarrowBandImageFilterBase.h"

#include "itkFiniteDifferenceFunction.h"
#include "itkGTest.h"
#include "itkImage.h"
#include "itkImageRegionConstIteratorWithIndex.h"
#include <cstddef>
#include <string>

namespace
{
using ImageType = itk::Image<float, 2>;

constexpr ImageType::SizeType imageSize{ { 8, 4 } };
constexpr float               initialPixelValue{ 10.0f };
constexpr itk::IndexValueType bandRow{ 1 };
constexpr itk::IndexValueType firstBandColumn{ 1 };
constexpr unsigned int        maximumIterations{ 2 };

// Adds 1 to every band node per iteration, so a node's value counts the iterations that updated it.
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

// Builds a band of consecutive nodes along bandRow, starting at firstBandColumn.
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
  SetBand(unsigned int numberOfNodes, bool emptyBandAfterFirstIteration)
  {
    m_NumberOfNodes = numberOfNodes;
    m_EmptyBandAfterFirstIteration = emptyBandAfterFirstIteration;
    this->Modified();
  }

  void
  GetSplitRegionOfWorkUnit(size_t workUnit)
  {
    ThreadRegionType region;
    this->GetSplitRegion(workUnit, region);
  }

  void
  CreateNarrowBand() override
  {
    this->ClearNarrowBand();
    if (m_EmptyBandAfterFirstIteration && this->GetElapsedIterations() > 0)
    {
      return;
    }
    for (unsigned int i = 0; i < m_NumberOfNodes; ++i)
    {
      const IndexType index{ firstBandColumn + static_cast<itk::IndexValueType>(i), bandRow };
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
    return this->GetElapsedIterations() >= maximumIterations;
  }

private:
  unsigned int m_NumberOfNodes{};
  bool         m_EmptyBandAfterFirstIteration{};
};

unsigned int
ExpectedElapsedIterations(unsigned int numberOfNodes, bool emptyBandAfterFirstIteration)
{
  if (numberOfNodes == 0)
  {
    return 0;
  }
  if (emptyBandAfterFirstIteration)
  {
    return 1;
  }
  return maximumIterations;
}

bool
IsBandNode(const ImageType::IndexType & index, unsigned int numberOfNodes)
{
  return index[1] == bandRow && index[0] >= firstBandColumn &&
         index[0] < firstBandColumn + static_cast<itk::IndexValueType>(numberOfNodes);
}
} // namespace


TEST(NarrowBand, SplitBandReturnsOneContiguousRegionPerWorkUnit)
{
  for (const unsigned int numberOfNodes : { 0u, 1u, 2u, 5u, 17u })
  {
    const auto band = itk::NarrowBand<int>::New();
    for (unsigned int i = 0; i < numberOfNodes; ++i)
    {
      band->PushBack(static_cast<int>(i));
    }
    for (const unsigned int workUnits : { 0u, 1u, 2u, 8u, 25u })
    {
      SCOPED_TRACE("numberOfNodes " + std::to_string(numberOfNodes) + ", workUnits " + std::to_string(workUnits));
      const auto regions = band->SplitBand(workUnits);
      EXPECT_EQ(regions.size(), workUnits);
      std::ptrdiff_t expectedBegin = 0;
      for (const auto & region : regions)
      {
        EXPECT_EQ(region.Begin - band->Begin(), expectedBegin);
        expectedBegin = region.End - band->Begin();
      }
      if (workUnits > 0)
      {
        EXPECT_EQ(expectedBegin, static_cast<std::ptrdiff_t>(numberOfNodes));
      }
    }
  }
}


TEST(NarrowBandImageFilterBase, UpdatesBandWithFewerNodesThanWorkUnits)
{
  for (const unsigned int workUnits : { 1u, 8u })
  {
    for (const unsigned int numberOfNodes : { 0u, 1u, 3u })
    {
      for (const bool emptyBandAfterFirstIteration : { false, true })
      {
        SCOPED_TRACE("workUnits " + std::to_string(workUnits) + ", numberOfNodes " + std::to_string(numberOfNodes) +
                     ", emptyBandAfterFirstIteration " + std::to_string(emptyBandAfterFirstIteration));
        const auto input = ImageType::New();
        input->SetRegions(ImageType::RegionType{ imageSize });
        input->Allocate();
        input->FillBuffer(initialPixelValue);
        const auto filter = SmallBandFilter::New();
        filter->SetInput(input);
        filter->SetNumberOfWorkUnits(workUnits);
        filter->SetBand(numberOfNodes, emptyBandAfterFirstIteration);
        ASSERT_NO_THROW(filter->Update());

        const unsigned int iterations = ExpectedElapsedIterations(numberOfNodes, emptyBandAfterFirstIteration);
        EXPECT_EQ(filter->GetElapsedIterations(), iterations);
        for (itk::ImageRegionConstIteratorWithIndex<ImageType> it(filter->GetOutput(),
                                                                  filter->GetOutput()->GetLargestPossibleRegion());
             !it.IsAtEnd();
             ++it)
        {
          const ImageType::IndexType index = it.GetIndex();
          const float                expected = initialPixelValue + (IsBandNode(index, numberOfNodes) ? iterations : 0);
          EXPECT_EQ(it.Get(), expected) << "at " << index;
          EXPECT_EQ(input->GetPixel(index), initialPixelValue) << "at " << index;
        }
        EXPECT_THROW(filter->GetSplitRegionOfWorkUnit(workUnits), itk::ExceptionObject);
      }
    }
  }
}
