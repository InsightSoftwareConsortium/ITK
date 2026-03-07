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
#include "itkDomainThreader.h"
#include "itkThreadedIndexedContainerPartitioner.h"
#include "itkGTest.h"

#include <string>
#include <vector>
#include <iostream>

namespace
{

class DomainThreaderAssociate
{
public:
  using Self = DomainThreaderAssociate;

  class TestDomainThreader : public itk::DomainThreader<itk::ThreadedIndexedContainerPartitioner, Self>
  {
  public:
    ITK_DISALLOW_COPY_AND_MOVE(TestDomainThreader);

    using Self = TestDomainThreader;
    using Superclass = itk::DomainThreader<itk::ThreadedIndexedContainerPartitioner, Self>;
    using Pointer = itk::SmartPointer<Self>;
    using ConstPointer = itk::SmartPointer<const Self>;

    using DomainPartitionerType = Superclass::DomainPartitionerType;
    using DomainType = Superclass::DomainType;

    itkNewMacro(Self);

    const std::vector<DomainType> &
    GetDomainInThreadedExecution() const
    {
      return m_DomainInThreadedExecution;
    }

  protected:
    TestDomainThreader() = default;

  private:
    void
    BeforeThreadedExecution() override
    {
      this->m_DomainInThreadedExecution.resize(this->GetNumberOfWorkUnitsUsed());
      auto unsetDomain = itk::MakeFilled<DomainType>(-1);
      for (auto & i : m_DomainInThreadedExecution)
      {
        i = unsetDomain;
      }
    }

    void
    ThreadedExecution(const DomainType & subdomain, const itk::ThreadIdType threadId) override
    {
      if (threadId == 0)
      {
        std::cout << "This is the : " << this->m_Associate->m_ClassDescriptor << std::endl;
      }
      this->m_DomainInThreadedExecution[threadId] = subdomain;
    }

    void
    AfterThreadedExecution() override
    {
      std::cout << "\nDomain partition per thread:" << std::endl;
      for (itk::ThreadIdType i = 0; i < m_DomainInThreadedExecution.size(); ++i)
      {
        std::cout << "ThreadId: " << i << '\t' << m_DomainInThreadedExecution[i] << std::endl;
      }
      std::cout << std::endl;
    }

    std::vector<DomainType> m_DomainInThreadedExecution;
  }; // end TestDomainThreader class

  DomainThreaderAssociate()
    : m_ClassDescriptor("enclosing class")
  {
    m_TestDomainThreader = TestDomainThreader::New();
  }

  TestDomainThreader *
  GetDomainThreader()
  {
    return m_TestDomainThreader.GetPointer();
  }

  void
  Execute(const TestDomainThreader::DomainType & completeDomain)
  {
    m_TestDomainThreader->Execute(this, completeDomain);
  }

private:
  TestDomainThreader::Pointer m_TestDomainThreader;

  std::string m_ClassDescriptor;
};


void
RunPartitionerTest(DomainThreaderAssociate &                                       enclosingClass,
                   itk::ThreadIdType                                               numberOfThreads,
                   const DomainThreaderAssociate::TestDomainThreader::DomainType & fullRange)
{
  std::cout << "Testing with " << numberOfThreads << " threads and complete domain " << fullRange << " ..."
            << std::endl;

  const DomainThreaderAssociate::TestDomainThreader::Pointer domainThreader = enclosingClass.GetDomainThreader();

  domainThreader->GetMultiThreader();
  domainThreader->SetMaximumNumberOfThreads(numberOfThreads);
  EXPECT_GE(domainThreader->GetMaximumNumberOfThreads(), numberOfThreads);

  domainThreader->SetNumberOfWorkUnits(numberOfThreads);
  EXPECT_EQ(domainThreader->GetNumberOfWorkUnits(), numberOfThreads);

  enclosingClass.Execute(fullRange);

  std::cout << "Requested numberOfThreads: " << numberOfThreads << std::endl
            << "actual: threader->GetNumberOfWorkUnitsUsed(): " << domainThreader->GetNumberOfWorkUnitsUsed() << "\n\n"
            << std::endl;

  // Check the results
  using DomainType = DomainThreaderAssociate::TestDomainThreader::DomainType;
  DomainType::IndexValueType    previousEndIndex = -1;
  const std::vector<DomainType> domainInThreadedExecution = domainThreader->GetDomainInThreadedExecution();
  for (itk::ThreadIdType i = 0; i < domainThreader->GetNumberOfWorkUnitsUsed(); ++i)
  {
    DomainType subRange = domainInThreadedExecution[i];
    EXPECT_NE(subRange[0], -1) << "subRange " << i << " was not set";
    EXPECT_NE(subRange[1], -1) << "subRange " << i << " was not set";
    if (i == 0)
    {
      EXPECT_EQ(subRange[0], fullRange[0]) << "First subRange should start at fullRange[0]";
    }
    if (i == static_cast<itk::ThreadIdType>(numberOfThreads - 1))
    {
      EXPECT_EQ(subRange[1], fullRange[1]) << "Last subRange should end at fullRange[1]";
    }
    if (i > 0)
    {
      EXPECT_EQ(previousEndIndex + 1, subRange[0]) << "subRange " << i << " is not continuous with previous subRange";
    }
    previousEndIndex = subRange[1];
  }
}

} // namespace


TEST(ThreadedIndexedContainerPartitioner, Partition)
{
  DomainThreaderAssociate                                         enclosingClass;
  const DomainThreaderAssociate::TestDomainThreader::ConstPointer domainThreader = enclosingClass.GetDomainThreader();

  std::cout << "GetGlobalMaximumNumberOfThreads: "
            << domainThreader->GetMultiThreader()->GetGlobalMaximumNumberOfThreads() << std::endl;
  std::cout << "GetGlobalDefaultNumberOfThreads: "
            << domainThreader->GetMultiThreader()->GetGlobalDefaultNumberOfThreads() << std::endl;

  using DomainType = DomainThreaderAssociate::TestDomainThreader::DomainType;
  DomainType fullRange;

  // Test with single thread
  fullRange[0] = 0;
  fullRange[1] = 103;
  RunPartitionerTest(enclosingClass, 1, fullRange);

  // Test with range that doesn't start at 0
  fullRange[0] = 2;
  fullRange[1] = 105;
  RunPartitionerTest(enclosingClass, 1, fullRange);

  // Test with multiple threads
  if (domainThreader->GetMultiThreader()->GetGlobalMaximumNumberOfThreads() > 1)
  {
    fullRange[0] = 6;
    fullRange[1] = 109;
    const itk::ThreadIdType defaultThreads = domainThreader->GetMultiThreader()->GetGlobalDefaultNumberOfThreads();
    RunPartitionerTest(enclosingClass, defaultThreads, fullRange);

    const itk::ThreadIdType maxThreads = domainThreader->GetMultiThreader()->GetGlobalMaximumNumberOfThreads();
    fullRange[0] = 6;
    fullRange[1] = fullRange[0] + maxThreads - 2;
    RunPartitionerTest(enclosingClass, maxThreads, fullRange);

    EXPECT_EQ(domainThreader->GetNumberOfWorkUnitsUsed(), maxThreads - 1);
  }
  else
  {
    std::cout << "No multi-threading available." << std::endl;
  }
}
