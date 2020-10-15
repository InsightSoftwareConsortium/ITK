/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
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
      DomainType unsetDomain;
      unsetDomain.Fill(-1);
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
        std::cout << "ThreadId: " << i << "\t" << m_DomainInThreadedExecution[i] << std::endl;
      }
      std::cout << std::endl;
    }

    std::vector<DomainType> m_DomainInThreadedExecution;
  }; // end TestDomainThreader class

  DomainThreaderAssociate()
  {
    m_TestDomainThreader = TestDomainThreader::New();
    m_ClassDescriptor = "enclosing class";
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


int
ThreadedIndexedContainerPartitionerRunTest(DomainThreaderAssociate & enclosingClass,
                                           itk::ThreadIdType         numberOfThreads,
                                           const DomainThreaderAssociate::TestDomainThreader::DomainType & fullRange)
{
  std::cout << "Testing with " << numberOfThreads << " threads and complete domain " << fullRange << " ..."
            << std::endl;

  DomainThreaderAssociate::TestDomainThreader::Pointer domainThreader = enclosingClass.GetDomainThreader();

  // Exercise GetMultiThreader().
  domainThreader->GetMultiThreader();
  domainThreader->SetMaximumNumberOfThreads(numberOfThreads);
  // Possible if numberOfThreads > GlobalMaximumNumberOfThreads
  if (domainThreader->GetMaximumNumberOfThreads() < numberOfThreads)
  {
    std::cerr << "Failed setting requested number of threads: " << numberOfThreads << std::endl
              << "domainThreader->GetMaximumNumberOfThreads(): " << domainThreader->GetMaximumNumberOfThreads()
              << std::endl;
    return EXIT_FAILURE;
  }

  domainThreader->SetNumberOfWorkUnits(numberOfThreads);
  // Possible if numberOfThreads > GlobalMaximumNumberOfThreads
  if (domainThreader->GetNumberOfWorkUnits() != numberOfThreads)
  {
    std::cerr << "Failed setting requested number of work units: " << numberOfThreads << std::endl
              << "domainThreader->GetNumberOfWorkUnits(): " << domainThreader->GetNumberOfWorkUnits() << std::endl;
    return EXIT_FAILURE;
  }

  enclosingClass.Execute(fullRange);

  /* Did we use as many threads as requested? */
  std::cout << "Requested numberOfThreads: " << numberOfThreads << std::endl
            << "actual: threader->GetNumberOfWorkUnitsUsed(): " << domainThreader->GetNumberOfWorkUnitsUsed() << "\n\n"
            << std::endl;

  /* Check the results. */
  using DomainType = DomainThreaderAssociate::TestDomainThreader::DomainType;
  DomainType::IndexValueType    previousEndIndex = -1;
  const std::vector<DomainType> domainInThreadedExecution = domainThreader->GetDomainInThreadedExecution();
  for (itk::ThreadIdType i = 0; i < domainThreader->GetNumberOfWorkUnitsUsed(); ++i)
  {
    DomainType subRange = domainInThreadedExecution[i];
    /* Check that the sub range was assigned something at all */
    if (subRange[0] == -1 || subRange[1] == -1)
    {
      std::cerr << "Error: subRange " << i << " is was not set: " << subRange[i];
      return EXIT_FAILURE;
    }
    /* Check that we got the begin of the range */
    if (i == 0 && subRange[0] != fullRange[0])
    {
      std::cerr << "Error: subRange[0][0] should be " << fullRange[0] << ", but it's " << subRange[0] << ".";
      return EXIT_FAILURE;
    }
    /* Check that we got the end of the range */
    if (i == numberOfThreads - 1 && subRange[1] != fullRange[1])
    {
      std::cerr << "Error: subRange[N-1][1] should be " << fullRange[1] << ", but it's " << subRange[1] << ".";
      return EXIT_FAILURE;
    }
    /* Check that the sub-range endings and beginnings are continuous */
    if (i > 0)
    {
      if (previousEndIndex + 1 != subRange[0])
      {
        std::cerr << "Error: subRange " << i << " is not continuous with "
                  << "previous subRange." << std::endl
                  << "previousEndIndex: " << previousEndIndex << std::endl
                  << "subRange[0]: " << subRange[0] << std::endl;
        return EXIT_FAILURE;
      }
    }
    previousEndIndex = subRange[1];
  }

  return EXIT_SUCCESS;
}

int
itkThreadedIndexedContainerPartitionerTest(int, char *[])
{
  DomainThreaderAssociate                                   enclosingClass;
  DomainThreaderAssociate::TestDomainThreader::ConstPointer domainThreader = enclosingClass.GetDomainThreader();

  /* Check # of threads */
  std::cout << "GetGlobalMaximumNumberOfThreads: "
            << domainThreader->GetMultiThreader()->GetGlobalMaximumNumberOfThreads() << std::endl;
  std::cout << "GetGlobalDefaultNumberOfThreads: "
            << domainThreader->GetMultiThreader()->GetGlobalDefaultNumberOfThreads() << std::endl;
  std::cout << "domainThreader->GetMultiThreader()->NumberOfWorkUnits(): "
            << domainThreader->GetMultiThreader()->GetNumberOfWorkUnits() << std::endl;

  using DomainType = DomainThreaderAssociate::TestDomainThreader::DomainType;
  DomainType fullRange;

  /* Test with single thread */
  fullRange[0] = 0;
  fullRange[1] = 103; // set total range to prime to test uneven division
  itk::ThreadIdType numberOfThreads = 1;
  if (ThreadedIndexedContainerPartitionerRunTest(enclosingClass, numberOfThreads, fullRange) != EXIT_SUCCESS)
  {
    return EXIT_FAILURE;
  }

  /* Test with range that doesn't start at 0 */
  fullRange[0] = 2;
  fullRange[1] = 105; // set total range to prime to test uneven division
  numberOfThreads = 1;
  if (ThreadedIndexedContainerPartitionerRunTest(enclosingClass, numberOfThreads, fullRange) != EXIT_SUCCESS)
  {
    return EXIT_FAILURE;
  }

  /* Test with multiple threads */
  if (domainThreader->GetMultiThreader()->GetGlobalMaximumNumberOfThreads() > 1)
  {
    /* Test with default number of threads. */
    fullRange[0] = 6;
    fullRange[1] = 109; // set total range to prime to test uneven division
    numberOfThreads = domainThreader->GetMultiThreader()->GetGlobalDefaultNumberOfThreads();
    if (ThreadedIndexedContainerPartitionerRunTest(enclosingClass, numberOfThreads, fullRange) != EXIT_SUCCESS)
    {
      return EXIT_FAILURE;
    }

    /* Test with max number of threads and check that we only used as
     * many as is reasonable. */
    itk::ThreadIdType maxNumberOfThreads = domainThreader->GetMultiThreader()->GetGlobalMaximumNumberOfThreads();
    fullRange[0] = 6;
    fullRange[1] = fullRange[0] + maxNumberOfThreads - 2;
    if (ThreadedIndexedContainerPartitionerRunTest(enclosingClass, maxNumberOfThreads, fullRange) != EXIT_SUCCESS)
    {
      return EXIT_FAILURE;
    }
    if (domainThreader->GetNumberOfWorkUnitsUsed() != maxNumberOfThreads - 1)
    {
      std::cerr << "Error: Expected to use only " << maxNumberOfThreads - 1 << "threads, but used "
                << domainThreader->GetNumberOfWorkUnitsUsed() << "." << std::endl;
    }
  }
  else
  {
    std::cout << "No multi-threading available. " << std::endl;
  }

  return EXIT_SUCCESS;
}
