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
#ifndef itkMultiStartOptimizerv4_hxx
#define itkMultiStartOptimizerv4_hxx

#include "itkPrintHelper.h"


namespace itk
{

template <typename TInternalComputationValueType>
MultiStartOptimizerv4Template<TInternalComputationValueType>::MultiStartOptimizerv4Template()

{
  this->m_NumberOfIterations = static_cast<SizeValueType>(0);
  this->m_StopCondition = StopConditionObjectToObjectOptimizerEnum::MAXIMUM_NUMBER_OF_ITERATIONS;
  this->m_StopConditionDescription << this->GetNameOfClass() << ": ";

  this->m_BestParametersIndex = static_cast<ParameterListSizeType>(0);
  this->m_MaximumMetricValue = NumericTraits<MeasureType>::max();
  this->m_MinimumMetricValue = this->m_MaximumMetricValue;
  m_LocalOptimizer = nullptr;
}

template <typename TInternalComputationValueType>
void
MultiStartOptimizerv4Template<TInternalComputationValueType>::PrintSelf(std::ostream & os, Indent indent) const
{
  using namespace print_helper;

  Superclass::PrintSelf(os, indent);

  os << indent << "StopCondition: " << m_StopCondition << std::endl;
  os << indent << "StopConditionDescription: " << m_StopConditionDescription.str() << std::endl;

  os << indent << "ParametersList: " << m_ParametersList << std::endl;
  os << indent << "MetricValuesList: " << m_MetricValuesList << std::endl;

  os << indent
     << "MinimumMetricValue: " << static_cast<typename NumericTraits<MeasureType>::PrintType>(m_MinimumMetricValue)
     << std::endl;
  os << indent
     << "MaximumMetricValue: " << static_cast<typename NumericTraits<MeasureType>::PrintType>(m_MaximumMetricValue)
     << std::endl;
  os << indent << "BestParametersIndex: "
     << static_cast<typename NumericTraits<ParameterListSizeType>::PrintType>(m_BestParametersIndex) << std::endl;

  itkPrintSelfObjectMacro(LocalOptimizer);
}

template <typename TInternalComputationValueType>
auto
MultiStartOptimizerv4Template<TInternalComputationValueType>::GetParametersList() -> ParametersListType &
{
  return this->m_ParametersList;
}

template <typename TInternalComputationValueType>
void
MultiStartOptimizerv4Template<TInternalComputationValueType>::SetParametersList(
  typename MultiStartOptimizerv4Template::ParametersListType & p)
{
  if (p != this->m_ParametersList)
  {
    this->m_ParametersList = p;
    this->Modified();
  }
}

template <typename TInternalComputationValueType>
auto
MultiStartOptimizerv4Template<TInternalComputationValueType>::GetMetricValuesList() const
  -> const MetricValuesListType &
{
  return this->m_MetricValuesList;
}

template <typename TInternalComputationValueType>
auto
MultiStartOptimizerv4Template<TInternalComputationValueType>::GetBestParameters() -> ParametersType
{
  return this->m_ParametersList[m_BestParametersIndex];
}

template <typename TInternalComputationValueType>
void
MultiStartOptimizerv4Template<TInternalComputationValueType>::InstantiateLocalOptimizer()
{
  LocalOptimizerPointer optimizer = LocalOptimizerType::New();
  optimizer->SetLearningRate(static_cast<TInternalComputationValueType>(1.e-1));
  optimizer->SetNumberOfIterations(25);
  this->m_LocalOptimizer = optimizer;
}

template <typename TInternalComputationValueType>
auto
MultiStartOptimizerv4Template<TInternalComputationValueType>::GetStopConditionDescription() const
  -> const StopConditionReturnStringType
{
  return this->m_StopConditionDescription.str();
}

template <typename TInternalComputationValueType>
void
MultiStartOptimizerv4Template<TInternalComputationValueType>::StopOptimization()
{
  itkDebugMacro("StopOptimization called with a description - " << this->GetStopConditionDescription());
  this->m_Stop = true;

  this->m_Metric->SetParameters(this->m_ParametersList[this->m_BestParametersIndex]);

  this->InvokeEvent(EndEvent());
}

template <typename TInternalComputationValueType>
void
MultiStartOptimizerv4Template<TInternalComputationValueType>::StartOptimization(bool doOnlyInitialization)
{
  itkDebugMacro("StartOptimization");

  this->m_NumberOfIterations = static_cast<SizeValueType>(this->m_ParametersList.size());
  this->m_MetricValuesList.clear();
  this->m_BestParametersIndex = static_cast<ParameterListSizeType>(0);
  this->m_MinimumMetricValue = this->m_MaximumMetricValue;

  // Must call the superclass version for basic validation and setup.
  if (this->m_NumberOfIterations > static_cast<SizeValueType>(0))
  {
    Superclass::StartOptimization(doOnlyInitialization);
  }

  this->m_CurrentIteration = static_cast<SizeValueType>(0);

  if (!doOnlyInitialization)
  {
    if (this->m_NumberOfIterations > static_cast<SizeValueType>(0))
    {
      this->ResumeOptimization();
    }
  }
}

template <typename TInternalComputationValueType>
void
MultiStartOptimizerv4Template<TInternalComputationValueType>::ResumeOptimization()
{
  this->m_StopConditionDescription.str("");
  this->m_StopConditionDescription << this->GetNameOfClass() << ": ";
  this->InvokeEvent(StartEvent());

  this->m_Stop = false;
  while (!this->m_Stop)
  {
    // Compute metric value
    try
    {
      this->m_Metric->SetParameters(this->m_ParametersList[this->m_CurrentIteration]);
      if (this->m_LocalOptimizer)
      {
        this->m_LocalOptimizer->SetMetric(this->m_Metric);
        this->m_LocalOptimizer->StartOptimization();
        this->m_ParametersList[this->m_CurrentIteration] = this->m_Metric->GetParameters();
      }
      this->m_CurrentMetricValue = this->m_Metric->GetValue();
      this->m_MetricValuesList.push_back(this->m_CurrentMetricValue);
    }
    catch (const ExceptionObject &)
    {
      // We simply ignore this exception because it may just be a bad starting point.
      // We hope that other start points are better.
      itkWarningMacro("An exception occurred in sub-optimization number "
                      << this->m_CurrentIteration
                      << ".  If too many of these occur, you may need to set a different set of initial parameters.");
    }

    if (this->m_CurrentMetricValue < this->m_MinimumMetricValue)
    {
      this->m_MinimumMetricValue = this->m_CurrentMetricValue;
      this->m_BestParametersIndex = this->m_CurrentIteration;
    }
    // Check if optimization has been stopped externally.
    // (Presumably this could happen from a multi-threaded client app?)
    if (this->m_Stop)
    {
      this->m_StopConditionDescription << "StopOptimization() called";
      break;
    }

    this->InvokeEvent(IterationEvent());

    // Update and check iteration count
    this->m_CurrentIteration++;
    if (this->m_CurrentIteration >= this->m_NumberOfIterations)
    {
      this->m_StopConditionDescription << "Maximum number of iterations (" << this->m_NumberOfIterations
                                       << ") exceeded.";
      this->m_StopCondition = StopConditionObjectToObjectOptimizerEnum::MAXIMUM_NUMBER_OF_ITERATIONS;
      this->StopOptimization();
      break;
    }
  }
}

} // namespace itk

#endif
