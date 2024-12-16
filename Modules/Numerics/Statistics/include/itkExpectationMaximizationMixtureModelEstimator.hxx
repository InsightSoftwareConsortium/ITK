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
#ifndef itkExpectationMaximizationMixtureModelEstimator_hxx
#define itkExpectationMaximizationMixtureModelEstimator_hxx

#include "itkNumericTraits.h"
#include "itkMath.h"

namespace itk
{
namespace Statistics
{
template <typename TSample>
ExpectationMaximizationMixtureModelEstimator<TSample>::ExpectationMaximizationMixtureModelEstimator()
  : m_Sample(nullptr)
  , m_MembershipFunctionsObject(MembershipFunctionVectorObjectType::New())
  , m_MembershipFunctionsWeightArrayObject(MembershipFunctionsWeightsArrayObjectType::New())
{}

template <typename TSample>
void
ExpectationMaximizationMixtureModelEstimator<TSample>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Maximum Iteration: " << this->GetMaximumIteration() << std::endl;
  os << indent << "Sample: " << this->GetSample() << std::endl;
  os << indent << "Number Of Components: " << this->GetNumberOfComponents() << std::endl;
  for (unsigned int i = 0; i < this->GetNumberOfComponents(); ++i)
  {
    os << indent << "Component Membership Function[" << i << "]: " << this->GetComponentMembershipFunction(i)
       << std::endl;
  }
  os << indent << "Termination Code: " << this->GetTerminationCode() << std::endl;
  os << indent << "Initial Proportions: " << this->GetInitialProportions() << std::endl;
  os << indent << "Proportions: " << this->GetProportions() << std::endl;
  os << indent << "Calculated Expectation: " << this->CalculateExpectation() << std::endl;
}

template <typename TSample>
void
ExpectationMaximizationMixtureModelEstimator<TSample>::SetMaximumIteration(int numberOfIterations)
{
  m_MaxIteration = numberOfIterations;
}

template <typename TSample>
int
ExpectationMaximizationMixtureModelEstimator<TSample>::GetMaximumIteration() const
{
  return m_MaxIteration;
}

template <typename TSample>
void
ExpectationMaximizationMixtureModelEstimator<TSample>::SetInitialProportions(ProportionVectorType & proportions)
{
  m_InitialProportions = proportions;
}

template <typename TSample>
auto
ExpectationMaximizationMixtureModelEstimator<TSample>::GetInitialProportions() const -> const ProportionVectorType &
{
  return m_InitialProportions;
}

template <typename TSample>
auto
ExpectationMaximizationMixtureModelEstimator<TSample>::GetProportions() const -> const ProportionVectorType &
{
  return m_Proportions;
}

template <typename TSample>
void
ExpectationMaximizationMixtureModelEstimator<TSample>::SetSample(const TSample * sample)
{
  m_Sample = sample;
}

template <typename TSample>
const TSample *
ExpectationMaximizationMixtureModelEstimator<TSample>::GetSample() const
{
  return m_Sample;
}

template <typename TSample>
int
ExpectationMaximizationMixtureModelEstimator<TSample>::AddComponent(ComponentType * component)
{
  m_ComponentVector.push_back(component);
  return static_cast<int>(m_ComponentVector.size());
}

template <typename TSample>
unsigned int
ExpectationMaximizationMixtureModelEstimator<TSample>::GetNumberOfComponents() const
{
  return static_cast<unsigned int>(m_ComponentVector.size());
}

template <typename TSample>
auto
ExpectationMaximizationMixtureModelEstimator<TSample>::GetTerminationCode() const -> TERMINATION_CODE_ENUM
{
  return m_TerminationCode;
}

template <typename TSample>
auto
ExpectationMaximizationMixtureModelEstimator<TSample>::GetComponentMembershipFunction(int componentIndex) const
  -> ComponentMembershipFunctionType *
{
  return (m_ComponentVector[componentIndex])->GetMembershipFunction();
}

template <typename TSample>
bool
ExpectationMaximizationMixtureModelEstimator<TSample>::CalculateDensities()
{
  bool componentModified = false;

  for (size_t i = 0; i < m_ComponentVector.size(); ++i)
  {
    if ((m_ComponentVector[i])->AreParametersModified())
    {
      componentModified = true;
      break;
    }
  }

  if (!componentModified)
  {
    return false;
  }

  const size_t        numberOfComponents = m_ComponentVector.size();
  std::vector<double> tempWeights(numberOfComponents, 0.);

  typename TSample::ConstIterator       iter = m_Sample->Begin();
  const typename TSample::ConstIterator last = m_Sample->End();
  // Note: The data type of componentIndex should be unsigned int
  //       because itk::Array only supports 'unsigned int' number of elements.

  using FrequencyType = typename TSample::AbsoluteFrequencyType;

  constexpr FrequencyType zeroFrequency{};
  constexpr double        minDouble = NumericTraits<double>::epsilon();
  SizeValueType           measurementVectorIndex = 0;
  while (iter != last)
  {
    typename TSample::MeasurementVectorType mvector = iter.GetMeasurementVector();
    const FrequencyType                     frequency = iter.GetFrequency();
    double                                  densitySum = 0.0;
    if (frequency > zeroFrequency)
    {
      for (unsigned int componentIndex = 0; componentIndex < numberOfComponents; ++componentIndex)
      {
        const double t_prop = m_Proportions[componentIndex];
        const double t_value = m_ComponentVector[componentIndex]->Evaluate(mvector);
        const double density = t_prop * t_value;
        tempWeights[componentIndex] = density;
        densitySum += density;
      }

      for (unsigned int componentIndex = 0; componentIndex < numberOfComponents; ++componentIndex)
      {
        double temp = tempWeights[static_cast<unsigned int>(componentIndex)];

        // just to make sure temp does not blow up!
        if (densitySum > NumericTraits<double>::epsilon())
        {
          temp /= densitySum;
        }
        m_ComponentVector[static_cast<unsigned int>(componentIndex)]->SetWeight(measurementVectorIndex, temp);
      }
    }
    else
    {
      for (unsigned int componentIndex = 0; componentIndex < numberOfComponents; ++componentIndex)
      {
        m_ComponentVector[componentIndex]->SetWeight(measurementVectorIndex, minDouble);
      }
    }

    ++iter;
    ++measurementVectorIndex;
  }

  return true;
}

template <typename TSample>
double
ExpectationMaximizationMixtureModelEstimator<TSample>::CalculateExpectation() const
{
  double sum = 0.0;

  if (m_Sample)
  {

    const SizeValueType size = m_Sample->Size();


    for (size_t componentIndex = 0; componentIndex < m_ComponentVector.size(); ++componentIndex)
    {
      double temp = m_Proportions[static_cast<unsigned int>(componentIndex)];

      // if temp is below the smallest positive double number
      // the log may blow up
      const double logProportion =
        (temp > NumericTraits<double>::epsilon()) ? std::log(temp) : NumericTraits<double>::NonpositiveMin();
      for (unsigned int measurementVectorIndex = 0; measurementVectorIndex < size; ++measurementVectorIndex)
      {
        temp = m_ComponentVector[componentIndex]->GetWeight(measurementVectorIndex);
        if (temp > NumericTraits<double>::epsilon())
        {
          sum += temp * (logProportion + std::log(temp));
        }
        else
        {
          // let's throw an exception
          itkExceptionMacro("temp is null");
        }
        // m_ComponentVector[componentIndex]->GetWeight(measurementVectorIndex) ) );
      }
    }
  }
  return sum;
}

template <typename TSample>
bool
ExpectationMaximizationMixtureModelEstimator<TSample>::UpdateComponentParameters()
{
  bool updated = false;
  for (size_t componentIndex = 0; componentIndex < m_ComponentVector.size(); ++componentIndex)
  {
    ComponentType * component = m_ComponentVector[componentIndex];
    component->Update();
    if (component->AreParametersModified())
    {
      updated = true;
    }
  }

  return updated;
}

template <typename TSample>
bool
ExpectationMaximizationMixtureModelEstimator<TSample>::UpdateProportions()
{
  const size_t numberOfComponents = m_ComponentVector.size();
  const size_t sampleSize = m_Sample->Size();
  auto         totalFrequency = static_cast<double>(m_Sample->GetTotalFrequency());

  bool updated = false;

  for (size_t i = 0; i < numberOfComponents; ++i)
  {
    double tempSum = 0.;

    if (totalFrequency > NumericTraits<double>::epsilon())
    {
      for (size_t j = 0; j < sampleSize; ++j)
      {
        tempSum += (m_ComponentVector[static_cast<unsigned int>(i)]->GetWeight(static_cast<unsigned int>(j)) *
                    m_Sample->GetFrequency(static_cast<unsigned int>(j)));
      }

      tempSum /= totalFrequency;
    }

    if (Math::NotAlmostEquals(tempSum, m_Proportions[static_cast<unsigned int>(i)]))
    {
      m_Proportions[static_cast<unsigned int>(i)] = tempSum;
      updated = true;
    }
  }

  return updated;
}

template <typename TSample>
void
ExpectationMaximizationMixtureModelEstimator<TSample>::GenerateData()
{
  m_Proportions = m_InitialProportions;

  int iteration = 0;
  m_CurrentIteration = 0;
  while (iteration < m_MaxIteration)
  {
    m_CurrentIteration = iteration;
    if (this->CalculateDensities())
    {
      this->UpdateComponentParameters();
      this->UpdateProportions();
    }
    else
    {
      m_TerminationCode = TERMINATION_CODE_ENUM::CONVERGED;
      break;
    }
    ++iteration;
  }

  m_TerminationCode = TERMINATION_CODE_ENUM::NOT_CONVERGED;
}

template <typename TSample>
auto
ExpectationMaximizationMixtureModelEstimator<TSample>::GetOutput() const -> const MembershipFunctionVectorObjectType *
{
  const size_t                   numberOfComponents = m_ComponentVector.size();
  MembershipFunctionVectorType & membershipFunctionsVector = m_MembershipFunctionsObject->Get();

  const typename SampleType::MeasurementVectorSizeType measurementVectorSize = m_Sample->GetMeasurementVectorSize();

  typename GaussianMembershipFunctionType::MeanVectorType mean;
  NumericTraits<typename GaussianMembershipFunctionType::MeanVectorType>::SetLength(mean, measurementVectorSize);

  typename GaussianMembershipFunctionType::CovarianceMatrixType covariance;
  covariance.SetSize(measurementVectorSize, measurementVectorSize);

  typename ComponentType::ParametersType parameters;

  for (size_t i = 0; i < numberOfComponents; ++i)
  {
    parameters = m_ComponentVector[i]->GetFullParameters();
    auto membershipFunction = GaussianMembershipFunctionType::New();
    membershipFunction->SetMeasurementVectorSize(measurementVectorSize);
    unsigned int parameterIndex = 0;
    for (unsigned int j = 0; j < measurementVectorSize; ++j)
    {
      mean[j] = parameters[j];
      ++parameterIndex;
    }

    for (unsigned int ii = 0; ii < measurementVectorSize; ++ii)
    {
      for (unsigned int jj = 0; jj < measurementVectorSize; ++jj)
      {
        covariance.GetVnlMatrix().put(ii, jj, parameters[parameterIndex]);
        ++parameterIndex;
      }
    }

    membershipFunction->SetMean(mean);
    membershipFunction->SetCovariance(covariance);
    membershipFunctionsVector.push_back(membershipFunction);
  }

  return static_cast<const MembershipFunctionVectorObjectType *>(m_MembershipFunctionsObject);
}

template <typename TSample>
auto
ExpectationMaximizationMixtureModelEstimator<TSample>::GetMembershipFunctionsWeightsArray() const
  -> const MembershipFunctionsWeightsArrayObjectType *
{
  const size_t           numberOfComponents = m_ComponentVector.size();
  ProportionVectorType & membershipFunctionsWeightVector = m_MembershipFunctionsWeightArrayObject->Get();

  membershipFunctionsWeightVector.SetSize(static_cast<SizeValueType>(numberOfComponents));
  for (size_t i = 0; i < numberOfComponents; ++i)
  {
    membershipFunctionsWeightVector[static_cast<unsigned int>(i)] = m_Proportions[static_cast<unsigned int>(i)];
  }

  return static_cast<const MembershipFunctionsWeightsArrayObjectType *>(m_MembershipFunctionsWeightArrayObject);
}

template <typename TSample>
void
ExpectationMaximizationMixtureModelEstimator<TSample>::Update()
{
  this->GenerateData();
}
} // end of namespace Statistics
} // end of namespace itk

#endif
