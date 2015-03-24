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
#ifndef itkTrainingFunctionBase_hxx
#define itkTrainingFunctionBase_hxx

#include "itkTrainingFunctionBase.h"

namespace itk
{
namespace Statistics
{

template<typename TSample, typename TTargetVector, typename ScalarType>
TrainingFunctionBase<TSample,TTargetVector,ScalarType>
::TrainingFunctionBase()
{
  m_PerformanceFunction = DefaultPerformanceType::New();
  m_Iterations = 0;
  m_TrainingSamples = ITK_NULLPTR;
  m_SampleTargets = ITK_NULLPTR;
  m_LearningRate = 1.0;
}

template<typename TSample, typename TTargetVector, typename ScalarType>
void TrainingFunctionBase<TSample,TTargetVector,ScalarType>
::SetTrainingSamples(TSample* samples)
{
  m_TrainingSamples = samples;
  std::cout << "Training functionSample Size=" << samples->Size() << std::endl;
  typename TSample::ConstIterator iter = samples->Begin();
  while (iter != samples->End())
    {
    //m_InputSamples.push_back(defaultconverter(iter.GetMeasurementVector()));
    m_InputSamples.push_back(iter.GetMeasurementVector());

    ++iter;
    }
}

template<typename TSample, typename TTargetVector, typename ScalarType>
void TrainingFunctionBase<TSample,TTargetVector,ScalarType>
::SetTargetValues(TTargetVector* targets)
{
  typename TTargetVector::ConstIterator iter = targets->Begin();
  while (iter != targets->End())
    {
    //m_Targets.push_back(targetconverter(iter.GetMeasurementVector()));
    m_Targets.push_back(iter.GetMeasurementVector());
    ++iter;
    }
  std::cout << "Num of Sample Targets converted= " << m_Targets.size()
            << std::endl;
  this->Modified();
}
template<typename TSample, typename TTargetVector, typename ScalarType>
void
TrainingFunctionBase<TSample,TTargetVector,ScalarType>
::SetLearningRate(ValueType lr)
{
  m_LearningRate = lr;
  this->Modified();
}

template<typename TSample, typename TTargetVector, typename ScalarType>
typename TrainingFunctionBase<TSample,TTargetVector,ScalarType>::ValueType
TrainingFunctionBase<TSample,TTargetVector,ScalarType>
::GetLearningRate()
{
  return m_LearningRate;
}

template<typename TSample, typename TTargetVector, typename ScalarType>
void TrainingFunctionBase<TSample,TTargetVector,ScalarType>
::SetPerformanceFunction(PerformanceFunctionType* f)
{
  m_PerformanceFunction=f;
  this->Modified();
}


/** Print the object */
template<typename TSample, typename TTargetVector, typename ScalarType>
void
TrainingFunctionBase<TSample,TTargetVector,ScalarType>
::PrintSelf( std::ostream& os, Indent indent ) const
{
  os << indent << "TrainingFunctionBase(" << this << ")" << std::endl;

  os << indent << "m_PerformanceFunction = " << m_PerformanceFunction << std::endl;
  os << indent << "m_Iterations = " << m_Iterations << std::endl;
  if(m_TrainingSamples)
    {
    os << indent << "m_TrainingSamples = " << m_TrainingSamples << std::endl;
    }

  if(m_SampleTargets)
    {
    os << indent << "m_SampleTargets = " << m_SampleTargets << std::endl;
    }
  //os << indent << "m_InputSamples = " << m_InputSamples << std::endl;
  //os << indent << "m_Targets = " << m_Targets << std::endl;
  os << indent << "m_LearningRate = " << m_LearningRate << std::endl;
  Superclass::PrintSelf( os, indent );
}

} // end namespace Statistics
} // end namespace itk

#endif
