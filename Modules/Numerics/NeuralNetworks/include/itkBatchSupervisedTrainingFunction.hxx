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
#ifndef itkBatchSupervisedTrainingFunction_hxx
#define itkBatchSupervisedTrainingFunction_hxx

#include "itkBatchSupervisedTrainingFunction.h"
#include <fstream>
#include <algorithm>

namespace itk
{
namespace Statistics
{

template<typename TSample, typename TTargetVector, typename ScalarType>
BatchSupervisedTrainingFunction<TSample,TTargetVector,ScalarType>//,f>
::BatchSupervisedTrainingFunction()
{
  this->m_LearningRate = 0.1;  //0.5 multilayer test 0.1 perceptron
  m_Threshold = 0;
  m_Stop = false; //stop condition
}

template<typename TSample, typename TTargetVector, typename ScalarType>
void BatchSupervisedTrainingFunction<TSample,TTargetVector,ScalarType>
::SetNumOfIterations(SizeValueType i)
{
  this->SetIterations(i);
}

template<typename TSample, typename TTargetVector, typename ScalarType>
void BatchSupervisedTrainingFunction<TSample,TTargetVector,ScalarType>
::Train(typename BatchSupervisedTrainingFunction<TSample, TTargetVector, ScalarType>::NetworkType* net,
        TSample* samples, TTargetVector* targets)
{
  this->SetTrainingSamples(samples);
  this->SetTargetValues(targets);

  InternalVectorType outputvector;
  InternalVectorType errorvector;
  outputvector.SetSize(targets->GetMeasurementVectorSize());
  errorvector.SetSize(targets->GetMeasurementVectorSize());
  //std::cout<<"Target dim ="<<targets->GetMeasurementVectorSize()<<std::endl;
  //typename Superclass::OutputVectorType outputvector;
  typename Superclass::VectorType inputvector;
  typename Superclass::OutputVectorType targetvector;
  //typename Superclass::OutputVectorType errorvector;

  SizeValueType num_iterations = this->GetIterations();
  m_Stop = false;
  SizeValueType count = 0;

  while (!m_Stop)
    {
    for (SizeValueType i = 0; i < this->m_InputSamples.size(); i++)
      {
      inputvector = this->m_InputSamples[i];
      targetvector = this->m_Targets[i];

      outputvector=net->GenerateOutput(inputvector);
      for(unsigned int k=0; k<targetvector.Size(); k++)
        {
        errorvector[k] = targetvector[k] - outputvector[k];
        }

      net->BackwardPropagate(this->m_PerformanceFunction
        ->EvaluateDerivative(errorvector));
      }
    net->UpdateWeights(this->m_LearningRate);
    count++;
    if (count > num_iterations)
      {
      m_Stop = true;
      }
    }
#ifdef __OLD_CODE__
  if (this->m_PerformanceFunction->Evaluate(errorvector) < m_Threshold
   && count < num_iterations)
    {
    std::cout << "Goal Met " << std::endl;
    }
  else
    {
    std::cout << "Goal Not Met Max Iterations Reached " << std::endl;
    }
  std::cout << net << std::endl;
#endif
}

/** Print the object */
template<typename TSample, typename TTargetVector, typename ScalarType>
void
BatchSupervisedTrainingFunction<TSample,TTargetVector,ScalarType>
::PrintSelf( std::ostream& os, Indent indent ) const
{
  os << indent << "BatchSupervisedTrainingFunction(" << this << ")" << std::endl;
  os << indent << "m_Threshold = " << m_Threshold << std::endl;
  os << indent << "m_Stop = " << m_Stop << std::endl;
  Superclass::PrintSelf( os, indent );
}

} // end namespace Statistics
} // end namespace itk

#endif
