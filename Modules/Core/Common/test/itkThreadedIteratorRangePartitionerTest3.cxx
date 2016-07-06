/*=========================================================================
 *
 *  Copyright Insight Software Consortium
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
#include <iterator>
#include "itkDomainThreader.h"
#include "itkThreadedIteratorRangePartitioner.h"
#include "itkMapContainer.h"

namespace
{
  class IteratorRangeDomainThreaderAssociate
  {
  public:
    typedef IteratorRangeDomainThreaderAssociate Self;

    typedef itk::MapContainer< int, unsigned int > DomainContainerType;
    typedef DomainContainerType::Pointer           DomainContainerPointer;
    typedef itk::ThreadedIteratorRangePartitioner< DomainContainerType::ConstIterator > ThreadedPartitionerType;

    class TestDomainThreader: public itk::DomainThreader< ThreadedPartitionerType, Self >
    {
    public:
      typedef TestDomainThreader                                   Self;
      typedef itk::DomainThreader< ThreadedPartitionerType, Self > Superclass;
      typedef itk::SmartPointer< Self >                            Pointer;
      typedef itk::SmartPointer< const Self >                      ConstPointer;

      typedef Superclass::DomainPartitionerType     DomainPartitionerType;
      typedef Superclass::DomainType                DomainType;

      itkSimpleNewMacro( Self );

      typedef std::vector< DomainContainerType::ElementIdentifier > BorderValuesType;
      typedef std::vector< BorderValuesType >                       DomainBorderValuesInThreadedExecutionType;

      const DomainBorderValuesInThreadedExecutionType & GetDomainInThreadedExecution() const
        {
        return m_DomainInThreadedExecution;
        }

    protected:
      TestDomainThreader() {};

    private:
      virtual void BeforeThreadedExecution() ITK_OVERRIDE
        {
        this->m_DomainInThreadedExecution.resize( this->GetNumberOfThreadsUsed() );
        BorderValuesType unsetBorderValues( 2, -1 );
        for( itk::ThreadIdType i = 0; i < m_DomainInThreadedExecution.size(); ++i )
          {
          m_DomainInThreadedExecution[i] = unsetBorderValues;
          }
        }

      virtual void ThreadedExecution( const DomainType& subdomain,
                                      const itk::ThreadIdType threadId ) ITK_OVERRIDE
        {
        if( threadId == 0 )
          {
          std::cout << "This is the : " << this->m_Associate->m_ClassDescriptor;
          }
        this->m_DomainInThreadedExecution[threadId].resize( 2 );
        this->m_DomainInThreadedExecution[threadId][0] = (subdomain.Begin()).Index();
        DomainType::IteratorType it = subdomain.End();
        --it;
        this->m_DomainInThreadedExecution[threadId][1] = it.Index();
        }

      virtual void AfterThreadedExecution() ITK_OVERRIDE
        {
        std::cout << "\nDomain partition per thread:" << std::endl;
        for( itk::ThreadIdType i = 0; i < m_DomainInThreadedExecution.size(); ++i )
          {
          std::cout << "ThreadId: " << i << "\t" << m_DomainInThreadedExecution[i][0] << " " << m_DomainInThreadedExecution[i][1] << std::endl;
          }
        std::cout << std::endl;
        }

      DomainBorderValuesInThreadedExecutionType m_DomainInThreadedExecution;
      ITK_DISALLOW_COPY_AND_ASSIGN(TestDomainThreader);
    }; // end TestDomainThreader class

    IteratorRangeDomainThreaderAssociate()
      {
      m_TestDomainThreader = TestDomainThreader::New();
      m_ClassDescriptor    = "enclosing class";
      }

    TestDomainThreader * GetDomainThreader()
      {
      return m_TestDomainThreader.GetPointer();
      }

    void Execute( const TestDomainThreader::DomainType & completeDomain )
      {
      m_TestDomainThreader->Execute(this, completeDomain);
      }

    private:
      TestDomainThreader::Pointer m_TestDomainThreader;

      std::string m_ClassDescriptor;
  };


  int ThreadedIteratorRangePartitionerRunTest(
    IteratorRangeDomainThreaderAssociate & enclosingClass,
    itk::ThreadIdType numberOfThreads,
    const IteratorRangeDomainThreaderAssociate::TestDomainThreader::DomainType & fullDomain )
  {
    std::cout << "Testing with " << numberOfThreads << " threads." << std::endl;

    typedef IteratorRangeDomainThreaderAssociate::TestDomainThreader TestDomainThreaderType;
    TestDomainThreaderType::Pointer domainThreader = enclosingClass.GetDomainThreader();

    // Exercise GetMultiThreader().
    domainThreader->GetMultiThreader();
    domainThreader->SetMaximumNumberOfThreads( numberOfThreads );
    // Possible if numberOfThreads < GlobalMaximumNumberOfThreads
    if( domainThreader->GetMaximumNumberOfThreads() != numberOfThreads )
      {
      std::cerr << "Failed setting requested number of threads: "
                << numberOfThreads << std::endl
                << "domainThreader->GetMaximumNumberOfThreads(): "
                << domainThreader->GetMaximumNumberOfThreads() << std::endl;
      return EXIT_FAILURE;
      }

    enclosingClass.Execute( fullDomain );

    /* Did we use as many threads as requested? */
    std::cout << "Requested numberOfThreads: " << numberOfThreads << std::endl
              << "actual: threader->GetNumberOfThreadsUsed(): "
              << domainThreader->GetNumberOfThreadsUsed() << "\n\n" << std::endl;

    /* Check the results. */
    typedef TestDomainThreaderType::BorderValuesType BorderValuesType;
    int previousEndIndex = -1;
    const TestDomainThreaderType::DomainBorderValuesInThreadedExecutionType domainInThreadedExecution = domainThreader->GetDomainInThreadedExecution();
    for( itk::ThreadIdType i = 0; i < domainThreader->GetNumberOfThreadsUsed(); ++i )
      {
      BorderValuesType subRange = domainInThreadedExecution[i];
      /* Check that the sub range was assigned something at all */
      if( subRange[0] == -1 ||
          subRange[1] == -1 )
          {
          std::cerr << "Error: subRange " << i << " is was not set: "
                    << subRange[i];
          return EXIT_FAILURE;
          }
      /* Check that we got the begin of the range */
      if( i == 0 && subRange[0] != (fullDomain.Begin()).Index() )
          {
          std::cerr << "Error: subRange[0][0] should be " << (fullDomain.Begin()).Index()
                    << ", but it's " << subRange[0] << ".";
          return EXIT_FAILURE;
          }

      /* Check that we got the end of the range */
      typedef IteratorRangeDomainThreaderAssociate::TestDomainThreader::DomainType DomainType;
      DomainType::IteratorType fullIt = fullDomain.End();
      --fullIt;
      if( i == numberOfThreads-1 && subRange[1] != fullIt.Index() )
          {
          std::cerr << "Error: subRange[N-1][1] should be " << fullIt.Index()
                    << ", but it's " << subRange[1] << ".";
          return EXIT_FAILURE;
          }
      /* Check that the sub-range endings and beginnings are continuous */
      if( i > 0 )
        {
        if( previousEndIndex + 2 != subRange[0] )
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

  // Helper function.
  void getIteratorFromIndex( const unsigned int index,
    IteratorRangeDomainThreaderAssociate::DomainContainerPointer container,
    IteratorRangeDomainThreaderAssociate::DomainContainerType::ConstIterator & it )
  {
    it = container->Begin();
    for( unsigned int ii = 0; ii < index; ++ii )
      {
      ++it;
      }
  }

  void setStartEnd( const unsigned int start,
    const unsigned int end,
    IteratorRangeDomainThreaderAssociate::DomainContainerPointer container,
    IteratorRangeDomainThreaderAssociate::TestDomainThreader::DomainType & fullDomain )
  {
    std::cout << std::endl << "From starting iterator index = " << start << " ending iterator index " << end << std::endl;
    IteratorRangeDomainThreaderAssociate::DomainContainerType::ConstIterator beginIt;
    IteratorRangeDomainThreaderAssociate::DomainContainerType::ConstIterator endIt;
    getIteratorFromIndex( start, container, beginIt );
    getIteratorFromIndex( end, container, endIt );

    IteratorRangeDomainThreaderAssociate::TestDomainThreader::DomainType newDomain( beginIt, endIt );
    fullDomain = newDomain;
  }
}

int itkThreadedIteratorRangePartitionerTest3(int, char* [])
{
  IteratorRangeDomainThreaderAssociate enclosingClass;
  IteratorRangeDomainThreaderAssociate::TestDomainThreader::ConstPointer domainThreader = enclosingClass.GetDomainThreader();

  if( domainThreader->GetMultiThreader() )
    {
    /* Check # of threads */
    std::cout << "GetGlobalMaximumNumberOfThreads: "
              << domainThreader->GetMultiThreader()->GetGlobalMaximumNumberOfThreads()
              << std::endl;
    std::cout << "GetGlobalDefaultNumberOfThreads: "
              << domainThreader->GetMultiThreader()->GetGlobalDefaultNumberOfThreads()
              << std::endl;
    std::cout << "domainThreader->GetMultiThreader()->NumberOfThreads(): " << domainThreader->GetMultiThreader()->GetNumberOfThreads()
              << std::endl;
    }
  else
    {
    std::cerr << "domainThreader->GetMultiThreader() is NULL" << std::endl;
    return EXIT_FAILURE;
    }

  typedef IteratorRangeDomainThreaderAssociate::TestDomainThreader::DomainType  DomainType;
  typedef IteratorRangeDomainThreaderAssociate::DomainContainerType             DomainContainerType;
  DomainContainerType::Pointer container = DomainContainerType::New();

  for( unsigned int i = 0; i < 256; ++i )
    {
    container->SetElement( static_cast< int >( i * 2 ), 2 * i + 1 );
    }
  DomainType fullDomain( container->Begin(), container->End() );

  /* Test with single thread */
  setStartEnd( 0, 103, container, fullDomain );
  itk::ThreadIdType numberOfThreads = 1;
  if( ThreadedIteratorRangePartitionerRunTest( enclosingClass, numberOfThreads, fullDomain )
        != EXIT_SUCCESS )
    {
    return EXIT_FAILURE;
    }

  /* Test with range that doesn't start at 0 */
  setStartEnd( 2, 105, container, fullDomain );
  numberOfThreads = 1;
  if( ThreadedIteratorRangePartitionerRunTest( enclosingClass, numberOfThreads, fullDomain )
        != EXIT_SUCCESS )
    {
    return EXIT_FAILURE;
    }

  /* Test with multiple threads */
  if( domainThreader->GetMultiThreader()->GetGlobalMaximumNumberOfThreads() > 1 )
    {
    /* Test with default number of threads. */
    setStartEnd( 6, 109, container, fullDomain );
    numberOfThreads =
      domainThreader->GetMultiThreader()->GetGlobalDefaultNumberOfThreads();
    if( ThreadedIteratorRangePartitionerRunTest( enclosingClass, numberOfThreads, fullDomain )
          != EXIT_SUCCESS )
      {
      return EXIT_FAILURE;
      }

    /* Test with max number of threads and check that we only used as
     * many as is reasonable. */
    itk::ThreadIdType maxNumberOfThreads =
      domainThreader->GetMultiThreader()->GetGlobalMaximumNumberOfThreads();
    setStartEnd( 6, 6+maxNumberOfThreads/2, container, fullDomain );
    if( ThreadedIteratorRangePartitionerRunTest( enclosingClass, maxNumberOfThreads, fullDomain )
          != EXIT_SUCCESS )
      {
      return EXIT_FAILURE;
      }
    if( domainThreader->GetNumberOfThreadsUsed() != maxNumberOfThreads-1 )
      {
      std::cerr << "Error: Expected to use only " << maxNumberOfThreads-1
                << "threads, but used " << domainThreader->GetNumberOfThreadsUsed()
                << "." << std::endl;
      }
    }
  else
    {
    std::cout << "No multi-threading available. " << std::endl;
    }

  return EXIT_SUCCESS;
}
