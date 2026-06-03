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
#include "itkCompensatedSummation.h"
#include "itkGTest.h"

#include "itkMath.h"
#include "itkMersenneTwisterRandomVariateGenerator.h"
#include "itkDomainThreader.h"
#include "itkThreadedIndexedContainerPartitioner.h"
#include <iostream>
#include <iomanip>

TEST(CompensatedSummation, ConvertedLegacyTest)
{
  using FloatType = float;
  constexpr long seedValue{ 17 };

  constexpr FloatType expectedMean{ 0.5 };

  constexpr itk::SizeValueType accumSize{ 50000000 };

  using GeneratorType = itk::Statistics::MersenneTwisterRandomVariateGenerator;
  auto generator = GeneratorType::New();
  generator->SetSeed(seedValue);

  FloatType vanillaSum = 0.0;
  using CompensatedSummationType = itk::CompensatedSummation<FloatType>;
  CompensatedSummationType floatAccumulator;
  FloatType                randomNumber = NAN;
  for (itk::SizeValueType ii = 0; ii < accumSize; ++ii)
  {
    randomNumber = generator->GetVariate();
    vanillaSum += randomNumber;
    floatAccumulator.AddElement(randomNumber);
  }
  const FloatType vanillaMean = vanillaSum / static_cast<FloatType>(accumSize);
  const FloatType vanillaError = itk::Math::Absolute(vanillaMean - expectedMean);
  const FloatType accumulatorSum = floatAccumulator.GetSum();
  const FloatType accumulatorMean = accumulatorSum / static_cast<FloatType>(accumSize);
  const FloatType accumulatorError = itk::Math::Absolute(accumulatorMean - expectedMean);

  EXPECT_LT(accumulatorError, vanillaError);
  EXPECT_LE(accumulatorError, 1.0e-4);

  // exercise other methods
  const CompensatedSummationType floatAccumulatorCopy = floatAccumulator;
  EXPECT_EQ(floatAccumulatorCopy.GetSum(), floatAccumulator.GetSum());

  const CompensatedSummationType floatAccumulatorCopy2 = floatAccumulator;
  EXPECT_EQ(floatAccumulatorCopy2.GetSum(), floatAccumulator.GetSum());

  floatAccumulator += randomNumber;
  floatAccumulator -= randomNumber;
  EXPECT_FLOAT_EQ(floatAccumulatorCopy2.GetSum(), floatAccumulator.GetSum());

  floatAccumulator *= randomNumber;
  floatAccumulator /= randomNumber;
  EXPECT_FLOAT_EQ(floatAccumulatorCopy2.GetSum(), floatAccumulator.GetSum());

  floatAccumulator.ResetToZero();
  EXPECT_FLOAT_EQ(floatAccumulator.GetSum(), FloatType{});

  floatAccumulator = 2.0;
  EXPECT_EQ(floatAccumulator.GetSum(), 2.0);
}


/*
 * This test demonstrates the variance in output when the same operation is
 * performed with different numbers of threads, and the utility of the
 * CompensatedSummation class for summing the per-thread output of
 * multi-threaded operation to reduce the variance. The variance is a
 * result of different floating-point rounding that occurs when
 * different numbers of threads are used.
 */
namespace
{

class CompensatedSummationTest2Associate
{
public:
  using Self = CompensatedSummationTest2Associate;

  // Nested class holds the domain threader
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

  protected:
    TestDomainThreader() = default;

  private:
    void
    BeforeThreadedExecution() override
    {
      const itk::ThreadIdType numWorkUnitsUsed = this->GetNumberOfWorkUnitsUsed();
      this->m_PerThreadCompensatedSum.resize(numWorkUnitsUsed);
      for (itk::ThreadIdType i = 0; i < numWorkUnitsUsed; ++i)
      {
        this->m_PerThreadCompensatedSum[i].ResetToZero();
      }
    }

    void
    ThreadedExecution(const DomainType & subdomain, const itk::ThreadIdType threadId) override
    {
      constexpr double value{ 1.0 / 7 };
      for (DomainType::IndexValueType i = subdomain[0]; i <= subdomain[1]; ++i)
      {
        this->m_PerThreadCompensatedSum[threadId].AddElement(value);
      }
    }

    void
    AfterThreadedExecution() override
    {
      this->m_Associate->m_UncompensatedSumOfThreads = 0.0;
      this->m_Associate->m_CompensatedSumOfThreads.ResetToZero();

      for (itk::ThreadIdType i = 0, numWorkUnitsUsed = this->GetNumberOfWorkUnitsUsed(); i < numWorkUnitsUsed; ++i)
      {
        const double sum = this->m_PerThreadCompensatedSum[i].GetSum();
        this->m_Associate->m_CompensatedSumOfThreads.AddElement(sum);
        this->m_Associate->m_UncompensatedSumOfThreads += sum;
      }
    }

    std::vector<itk::CompensatedSummation<double>> m_PerThreadCompensatedSum;

  }; // end TestDomainThreader class

  CompensatedSummationTest2Associate()
    : m_ClassDescriptor("enclosing class")
  {
    m_TestDomainThreader = TestDomainThreader::New();
  }

  double
  GetCompensatedSumOfThreads()
  {
    return this->m_CompensatedSumOfThreads.GetSum();
  }

  [[nodiscard]] double
  GetUncompensatedSumOfThreads() const
  {
    return this->m_UncompensatedSumOfThreads;
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

  std::string                       m_ClassDescriptor;
  itk::CompensatedSummation<double> m_CompensatedSumOfThreads;
  double                            m_UncompensatedSumOfThreads{ 0.0 };
};

} // namespace

TEST(CompensatedSummation, ConvertedLegacyTest2)
{
  CompensatedSummationTest2Associate                                    enclosingClass;
  const CompensatedSummationTest2Associate::TestDomainThreader::Pointer domainThreader =
    enclosingClass.GetDomainThreader();

  using DomainType = CompensatedSummationTest2Associate::TestDomainThreader::DomainType;
  DomainType domain;

  const itk::ThreadIdType maxNumberOfThreads = domainThreader->GetMultiThreader()->GetGlobalMaximumNumberOfThreads();
  domain[0] = 0;
  domain[1] = maxNumberOfThreads * 10000;

  /* Test with single thread. We should get the same result. */
  constexpr itk::ThreadIdType numberOfThreads{ 1 };
  domainThreader->SetMaximumNumberOfThreads(numberOfThreads);
  domainThreader->SetNumberOfWorkUnits(numberOfThreads);

  /* Execute */
  enclosingClass.Execute(domain);

  /* Check results */
  EXPECT_DOUBLE_EQ(enclosingClass.GetCompensatedSumOfThreads(), enclosingClass.GetUncompensatedSumOfThreads());

  /* Store result as reference */
  const double referenceSum = enclosingClass.GetCompensatedSumOfThreads();

  /* Test with maximum threads. We need at least three threads to see a difference. */
  if (domainThreader->GetMultiThreader()->GetGlobalMaximumNumberOfThreads() > 2)
  {
    domainThreader->SetMaximumNumberOfThreads(maxNumberOfThreads);
    domainThreader->SetNumberOfWorkUnits(maxNumberOfThreads);

    /* Execute */
    enclosingClass.Execute(domain);

    /* Check number of threads used */
    EXPECT_EQ(domainThreader->GetNumberOfWorkUnitsUsed(), maxNumberOfThreads);

    /* Check that the compensated result is not further from reference than
     * uncompensated.
     * Generally we see the compensated sum closer, but on a few platforms the
     * sums are equal. This could be because of differences in compiler
     * optimizations that were not handled by the CompensatedSummation class
     * pragmas, or perhaps because of differences in math coprocessors, or
     * something else. It's not clear. */
    EXPECT_LE(itk::Math::Absolute(referenceSum - enclosingClass.GetCompensatedSumOfThreads()),
              itk::Math::Absolute(referenceSum - enclosingClass.GetUncompensatedSumOfThreads()));
  }
}
