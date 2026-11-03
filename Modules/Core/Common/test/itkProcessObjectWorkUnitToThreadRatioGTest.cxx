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
#include "itkPoolMultiThreader.h"
#include "itkProcessObject.h"
#include "itkSingleMultiThreader.h"
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

protected:
  RatioObject() { this->SetWorkUnitToThreadRatio(VRatio); }
};

std::vector<itk::MultiThreaderBase::Pointer>
MakeThreaders()
{
  std::vector<itk::MultiThreaderBase::Pointer> threaders{ itk::PlatformMultiThreader::New().GetPointer(),
                                                          itk::PoolMultiThreader::New().GetPointer(),
                                                          itk::SingleMultiThreader::New().GetPointer() };
#if defined(ITK_USE_TBB)
  threaders.emplace_back(itk::TBBMultiThreader::New().GetPointer());
#endif
  return threaders;
}

itk::ThreadIdType
ExpectedWorkUnits(const itk::MultiThreaderBase & threader, unsigned int ratio)
{
  return std::clamp<itk::ThreadIdType>(ratio * threader.GetMaximumNumberOfThreads(), 1, ITK_MAX_THREADS);
}
} // namespace

TEST(ProcessObjectWorkUnitToThreadRatio, ZeroKeepsThreaderDefault)
{
  for (const auto & threader : MakeThreaders())
  {
    const auto object = RatioObject<0>::New();
    object->SetMultiThreader(threader);
    EXPECT_EQ(object->GetWorkUnitToThreadRatio(), 0u);
    EXPECT_EQ(object->GetNumberOfWorkUnits(), threader->GetNumberOfWorkUnits()) << threader->GetNameOfClass();
  }
}

TEST(ProcessObjectWorkUnitToThreadRatio, RatioFromConstructorFollowsThreader)
{
  for (const auto & threader : MakeThreaders())
  {
    const auto oneObject = RatioObject<1>::New();
    oneObject->SetMultiThreader(threader);
    EXPECT_EQ(oneObject->GetNumberOfWorkUnits(), ExpectedWorkUnits(*threader, 1)) << threader->GetNameOfClass();

    const auto twoObject = RatioObject<2>::New();
    twoObject->SetMultiThreader(threader);
    EXPECT_EQ(twoObject->GetNumberOfWorkUnits(), ExpectedWorkUnits(*threader, 2)) << threader->GetNameOfClass();
  }
}

TEST(ProcessObjectWorkUnitToThreadRatio, ChangingRatioUpdatesDefault)
{
  const auto object = RatioObject<0>::New();
  const auto threaderDefault = object->GetMultiThreader()->GetNumberOfWorkUnits();

  object->SetWorkUnitToThreadRatio(1);
  EXPECT_EQ(object->GetNumberOfWorkUnits(), ExpectedWorkUnits(*object->GetMultiThreader(), 1));

  object->SetWorkUnitToThreadRatio(0);
  EXPECT_EQ(object->GetNumberOfWorkUnits(), threaderDefault);
}

TEST(ProcessObjectWorkUnitToThreadRatio, ExplicitNumberOfWorkUnitsIsKept)
{
  const auto object = RatioObject<1>::New();
  const auto ratioDefault = object->GetNumberOfWorkUnits();
  const auto explicitWorkUnits = ratioDefault == 1 ? itk::ThreadIdType{ 2 } : ratioDefault - 1;

  object->SetNumberOfWorkUnits(explicitWorkUnits);
  object->SetWorkUnitToThreadRatio(3);
  EXPECT_EQ(object->GetNumberOfWorkUnits(), explicitWorkUnits);
}
