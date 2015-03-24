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
#ifndef itkIterativeSupervisedTrainingFunction_hxx
#define itkIterativeSupervisedTrainingFunction_hxx

#include "itkIterativeSupervisedTrainingFunction.h"
#include <fstream>

namespace itk
{
namespace Statistics
{

template<typename TSample, typename TTargetVector, typename ScalarType>
IterativeSupervisedTrainingFunction<TSample,TTargetVector,ScalarType>
::IterativeSupervisedTrainingFunction()
{
  this->m_LearningRate = 0.5;
  m_Threshold = 0;
  m_Stop = false;
}

template<typename TSample, typename TTargetVector, typename ScalarType>
void IterativeSupervisedTrainingFunction<TSample,TTargetVector,ScalarType>
::SetNumOfIterations(SizeValueType i)
{
  this->SetIterations(i);
  this->Modified();
}

template<typename TSample, typename TTargetVector, typename ScalarType>
void IterativeSupervisedTrainingFunction<TSample,TTargetVector,ScalarType>
::Train(typename IterativeSupervisedTrainingFunction<TSample, TTargetVector, ScalarType>::NetworkType* Net,
        TSample* samples, TTargetVector* targets)
{
  this->SetTrainingSamples(samples);
  this->SetTargetValues(targets);

  InternalVectorType outputvector;
  InternalVectorType errorvector;
  outputvector.SetSize(targets->GetMeasurementVectorSize());
  errorvector.SetSize(targets->GetMeasurementVectorSize());

  //typename Superclass::OutputVectorType outputvector;
  typename Superclass::VectorType inputvector;
  typename Superclass::OutputVectorType targetvector;
  //typename Superclass::OutputVectorType errorvector;

#ifdef __OLD_CODE__
  std::ofstream outfile;
  outfile.open("output.txt");
#endif

  const SizeValueType num_iterations = this->GetIterations();
  m_Stop = false;
  SizeValueType i = 0;
  while (!m_Stop)
    {
    int temp = rand() % (this->m_InputSamples.size());
    inputvector = this->m_InputSamples[temp];
    targetvector = this->m_Targets[temp];
    outputvector = Net->GenerateOutput(inputvector);
    for(unsigned int k=0; k<targetvector.Size(); k++)
      {
      errorvector[k] = targetvector[k] - outputvector[k];
      }
#ifdef __OLD_CODE__
    outfile <<errorvector[0] << std::endl;
#endif
    Net->BackwardPropagate(this->m_PerformanceFunction->EvaluateDerivative(errorvector));
    Net->UpdateWeights(this->m_LearningRate);
    i++;
    if (i > num_iterations)
      {
      m_Stop = true;
      }
    }
#ifdef __OLD_CODE__
  if (this->m_PerformanceFunction->Evaluate(errorvector) < m_Threshold
   && i < num_iterations)
    {
    std::cout << "Goal Met " << std::endl;
    }
  else
    {
    std::cout << "Goal Not Met Max Iterations Reached " << std::endl;
    }
#endif
}

/** Print the object */
template<typename TSample, typename TTargetVector, typename ScalarType>
void
IterativeSupervisedTrainingFunction<TSample,TTargetVector,ScalarType>
::PrintSelf( std::ostream& os, Indent indent ) const
{
  os << indent << "IterativeSupervisedTrainingFunction(" << this << ")" << std::endl;
  os << indent << "m_Threshold = " << m_Threshold << std::endl;
  os << indent << "m_Stop = " << m_Stop << std::endl;
  Superclass::PrintSelf( os, indent );
}

} // end namespace Statistics
} // end namespace itk

#endif
