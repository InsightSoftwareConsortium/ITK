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

#include "itkMath.h"
#include "itkDomainThreader.h"
#include "itkThreadedIndexedContainerPartitioner.h"
#include "itkCompensatedSummation.h"
#include <iostream>
#include <iomanip>

/*
 * This test demonstrates the variance in output when the same operation is
 * performed with different numbers of threads, and the utility of the
 * CompensatedSummation class for summing the per-thread output of
 * multi-threaded operation to reduce the variance. The variance is a
 * result of different floating-point rounding that occurs when
 * different numbers of threads are used.
 */

class CompensatedSummationTest2Associate
{
public:
  typedef CompensatedSummationTest2Associate Self;

  // Nested class holds the domain threader
  class TestDomainThreader
    : public itk::DomainThreader< itk::ThreadedIndexedContainerPartitioner, Self >
  {
  public:
    typedef TestDomainThreader                                                    Self;
    typedef itk::DomainThreader< itk::ThreadedIndexedContainerPartitioner, Self > Superclass;
    typedef itk::SmartPointer< Self >                                             Pointer;
    typedef itk::SmartPointer< const Self >                                       ConstPointer;

    typedef Superclass::DomainPartitionerType     DomainPartitionerType;
    typedef Superclass::DomainType                DomainType;

    itkNewMacro( Self );

  protected:
    TestDomainThreader() {};

  private:
    virtual void BeforeThreadedExecution() ITK_OVERRIDE
      {
      const itk::ThreadIdType numThreadsUsed = this->GetNumberOfThreadsUsed();
      this->m_PerThreadCompensatedSum.resize( numThreadsUsed );
      for( itk::ThreadIdType i = 0; i < numThreadsUsed; ++i )
        {
        this->m_PerThreadCompensatedSum[i].ResetToZero();
        }
      }

    virtual void ThreadedExecution( const DomainType& subdomain,
                                    const itk::ThreadIdType threadId ) ITK_OVERRIDE
      {
      itk::CompensatedSummation<double> compensatedSum;
      for( DomainType::IndexValueType i=subdomain[0]; i <= subdomain[1]; i++ )
        {
        double value = itk::NumericTraits<double>::OneValue() / 7;
        this->m_PerThreadCompensatedSum[threadId].AddElement( value );
        }
      }

    virtual void AfterThreadedExecution() ITK_OVERRIDE
      {
      this->m_Associate->m_UncompensatedSumOfThreads = itk::NumericTraits<double>::ZeroValue();
      this->m_Associate->m_CompensatedSumOfThreads.ResetToZero();

      for( itk::ThreadIdType i = 0, numThreadsUsed = this->GetNumberOfThreadsUsed(); i < numThreadsUsed; ++i )
        {
        double sum = this->m_PerThreadCompensatedSum[i].GetSum();
        std::cout << i << ": " << sum << std::endl;
        this->m_Associate->m_CompensatedSumOfThreads.AddElement( sum );
        this->m_Associate->m_UncompensatedSumOfThreads += sum;
        }
      }

    std::vector< itk::CompensatedSummation<double> > m_PerThreadCompensatedSum;

    ITK_DISALLOW_COPY_AND_ASSIGN(TestDomainThreader);
  }; // end TestDomainThreader class

  CompensatedSummationTest2Associate()
    {
    m_TestDomainThreader = TestDomainThreader::New();
    m_ClassDescriptor    = "enclosing class";
    m_UncompensatedSumOfThreads = 0.0;
    }

  double GetCompensatedSumOfThreads()
    {
    return this->m_CompensatedSumOfThreads.GetSum();
    }

  double GetUncompensatedSumOfThreads()
    {
    return this->m_UncompensatedSumOfThreads;
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

    std::string                       m_ClassDescriptor;
    itk::CompensatedSummation<double> m_CompensatedSumOfThreads;
    double                            m_UncompensatedSumOfThreads;
};

int itkCompensatedSummationTest2(int, char* [])
{
  CompensatedSummationTest2Associate enclosingClass;
  CompensatedSummationTest2Associate::TestDomainThreader::Pointer domainThreader = enclosingClass.GetDomainThreader();

  /* Check # of threads */
  std::cout << "GetGlobalMaximumNumberOfThreads: "
            << domainThreader->GetMultiThreader()->GetGlobalMaximumNumberOfThreads()
            << std::endl;
  std::cout << "GetGlobalDefaultNumberOfThreads: "
            << domainThreader->GetMultiThreader()->GetGlobalDefaultNumberOfThreads()
            << std::endl;

  typedef CompensatedSummationTest2Associate::TestDomainThreader::DomainType DomainType;
  DomainType domain;

  itk::ThreadIdType maxNumberOfThreads =
    domainThreader->GetMultiThreader()->GetGlobalMaximumNumberOfThreads();
  domain[0] = 0;
  domain[1] = maxNumberOfThreads * 10000;

  /* Test with single thread. We should get the same result. */
  itk::ThreadIdType numberOfThreads = 1;
  domainThreader->SetMaximumNumberOfThreads( numberOfThreads );
  std::cout << "Testing with " << numberOfThreads
            << " threads and domain " << domain << " ..." << std::endl;

  /* Execute */
  enclosingClass.Execute( domain );

  /* Did we use as many threads as requested? */
  std::cout << "Requested numberOfThreads: " << numberOfThreads << std::endl
            << "actual: threader->GetNumberOfThreadsUsed(): "
            << domainThreader->GetNumberOfThreadsUsed() << "\n\n" << std::endl;

  /* Check results */
  if( itk::Math::NotAlmostEquals( enclosingClass.GetCompensatedSumOfThreads(), enclosingClass.GetUncompensatedSumOfThreads() ) )
    {
    std::cerr << std::setprecision(20)
              << "Error. Expected the sum to be the same for compensated and uncompensated."
              << " Instead, got " << enclosingClass.GetCompensatedSumOfThreads() << " and "
              << enclosingClass.GetUncompensatedSumOfThreads() << std::endl
              << "Difference: " << enclosingClass.GetCompensatedSumOfThreads() - enclosingClass.GetUncompensatedSumOfThreads()
              << std::endl;
    return EXIT_FAILURE;
    }

  /* Store result as reference */
  double referenceSum = enclosingClass.GetCompensatedSumOfThreads();

  /* Test with maximum threads. We need at least three threads to see a difference. */
  if( domainThreader->GetMultiThreader()->GetGlobalMaximumNumberOfThreads() > 2 )
    {
    domainThreader->SetMaximumNumberOfThreads( maxNumberOfThreads );
    std::cout << "Testing with " << maxNumberOfThreads
              << " threads and domain " << domain << " ..." << std::endl;

    /* Execute */
    enclosingClass.Execute( domain );

    /* Check number of threads used */
    if( domainThreader->GetNumberOfThreadsUsed() != maxNumberOfThreads )
      {
      std::cerr << "Error: Expected to use " << maxNumberOfThreads
                << "threads, but used " << domainThreader->GetNumberOfThreadsUsed()
                << "." << std::endl;
      return EXIT_FAILURE;
      }
    std::cout << "# of digits precision in double: " << std::numeric_limits< double >::digits10
              << std::endl;
    std::cout << std::setprecision(100)
              << "Reference:     " << referenceSum << std::endl
              << "Compensated:   " << enclosingClass.GetCompensatedSumOfThreads() << std::endl
              << "Uncompensated: " << enclosingClass.GetUncompensatedSumOfThreads()
              << std::endl
              << "Difference: " << enclosingClass.GetCompensatedSumOfThreads() - enclosingClass.GetUncompensatedSumOfThreads()
              << std::endl;

    /* Check that the compensated result is not further from reference than
     * uncompensated.
     * Generally we see the compensated sum closer, but on a few platforms the
     * sums are equal. This could be because of differences in compiler
     * optimizations that were not handled by the CompensatedSummation class
     * pragmas, or perhaps because of differences in math coprocessors, or
     * something else. It's not clear. */
    if( std::fabs( referenceSum - enclosingClass.GetCompensatedSumOfThreads() ) >
        std::fabs( referenceSum - enclosingClass.GetUncompensatedSumOfThreads() ) )
      {
      std::cerr << "Error. Expected the compensated sum of threads to be closer "
                << "to reference than the uncompensated sum, or the same value. "
                << std::endl;
      return EXIT_FAILURE;
      }
    }
  else
    {
    std::cout << "No multi-threading available, or too few threads available. "
              << std::endl;
    }

  return EXIT_SUCCESS;
}
