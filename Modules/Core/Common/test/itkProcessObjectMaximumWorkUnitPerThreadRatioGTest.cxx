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

#include "itkGTest.h"
#include "itkPlatformMultiThreader.h"
#include "itkProcessObject.h"
#include "itkSingleMultiThreader.h"
#if defined(ITK_USE_POOL_MULTI_THREADER)
#  include "itkPoolMultiThreader.h"
#endif
#if defined(ITK_USE_TBB)
#  include "itkTBBMultiThreader.h"
#endif

#include <algorithm>
#include <vector>

namespace
{
template <unsigned int VRatio>
class RatioObject : public itk::ProcessObject
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(RatioObject);
  using Self = RatioObject;
  using Superclass = itk::ProcessObject;
  using Pointer = itk::SmartPointer<Self>;
  itkNewMacro(Self);

  void
  SetNumberOfWorkUnits(itk::ThreadIdType numberOfWorkUnits) override
  {
    Superclass::SetNumberOfWorkUnits(numberOfWorkUnits);
    m_ForwardedNumberOfWorkUnits = numberOfWorkUnits;
  }

  itk::ThreadIdType m_ForwardedNumberOfWorkUnits{};

protected:
  RatioObject() { this->SetMaximumWorkUnitPerThreadRatio(VRatio); }
};

std::vector<itk::MultiThreaderBase::Pointer>
MakeThreaders()
{
  std::vector<itk::MultiThreaderBase::Pointer> threaders{ itk::PlatformMultiThreader::New().GetPointer(),
                                                          itk::SingleMultiThreader::New().GetPointer() };
#if defined(ITK_USE_POOL_MULTI_THREADER)
  threaders.emplace_back(itk::PoolMultiThreader::New().GetPointer());
#endif
#if defined(ITK_USE_TBB)
  threaders.emplace_back(itk::TBBMultiThreader::New().GetPointer());
#endif
  return threaders;
}
} // namespace

TEST(ProcessObjectMaximumWorkUnitPerThreadRatio, ZeroKeepsThreaderCount)
{
  for (const auto & threader : MakeThreaders())
  {
    const auto expected = threader->GetNumberOfWorkUnits();
    const auto object = RatioObject<0>::New();
    object->SetMultiThreader(threader);
    EXPECT_EQ(object->GetNumberOfWorkUnits(), expected) << threader->GetNameOfClass();
  }
}

TEST(ProcessObjectMaximumWorkUnitPerThreadRatio, RatioLimitsButNeverRaisesCount)
{
  for (const auto & threader : MakeThreaders())
  {
    const auto threads = threader->GetMaximumNumberOfThreads();
    const auto workUnits = threader->GetNumberOfWorkUnits();
    const auto object = RatioObject<2>::New();
    object->SetMultiThreader(threader);
    EXPECT_EQ(object->GetNumberOfWorkUnits(), std::min(workUnits, 2 * threads)) << threader->GetNameOfClass();
    EXPECT_EQ(threader->GetMaximumNumberOfThreads(), threads) << threader->GetNameOfClass();
  }
}

TEST(ProcessObjectMaximumWorkUnitPerThreadRatio, RatioIsForwardedThroughVirtualSetter)
{
  const auto object = RatioObject<0>::New();
  const auto threads = object->GetMultiThreader()->GetMaximumNumberOfThreads();
  object->SetNumberOfWorkUnits(3 * threads);
  object->m_ForwardedNumberOfWorkUnits = 0;
  const auto before = object->GetNumberOfWorkUnits();
  object->SetMaximumWorkUnitPerThreadRatio(1);
  EXPECT_EQ(object->GetMaximumWorkUnitPerThreadRatio(), 1u);
  const auto limit = object->GetMultiThreader()->GetMaximumNumberOfThreads();
  EXPECT_EQ(object->GetNumberOfWorkUnits(), std::min(before, limit));
  if (before > limit)
  {
    EXPECT_EQ(object->m_ForwardedNumberOfWorkUnits, limit);
  }
}

TEST(ProcessObjectMaximumWorkUnitPerThreadRatio, LaterExplicitCountWins)
{
  const auto object = RatioObject<1>::New();
  const auto threads = object->GetMultiThreader()->GetMaximumNumberOfThreads();
  object->SetNumberOfWorkUnits(3 * threads);
  EXPECT_EQ(object->GetNumberOfWorkUnits(), object->GetMultiThreader()->GetNumberOfWorkUnits());
  EXPECT_GE(object->GetNumberOfWorkUnits(), threads);
}
