/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkIterativeSupervisedTrainingFunction.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkIterativeSupervisedTrainingFunction_txx
#define __itkIterativeSupervisedTrainingFunction_txx

#include "itkIterativeSupervisedTrainingFunction.h"
#include <fstream>

namespace itk
{
namespace Statistics
{

template<class TSample, class TOutput, class ScalarType> 
IterativeSupervisedTrainingFunction<TSample,TOutput,ScalarType>
::IterativeSupervisedTrainingFunction()
{
  this->m_LearningRate = 0.5; 
  m_Threshold = 0;
  m_Stop = false;
}

template<class TSample, class TOutput, class ScalarType>
void IterativeSupervisedTrainingFunction<TSample,TOutput,ScalarType>
::SetNumOfIterations(long i)
{
  this->SetIterations(i);
  this->Modified();
}

template<class TSample, class TOutput, class ScalarType>
void IterativeSupervisedTrainingFunction<TSample,TOutput,ScalarType>
::Train(typename IterativeSupervisedTrainingFunction<TSample, TOutput, ScalarType>::NetworkType* Net,
        TSample* samples, TOutput* targets)
{
  this->SetTrainingSamples(samples); 
  this->SetTargetValues(targets);
  typename Superclass::OutputVectorType outputvector;
  typename Superclass::VectorType inputvector;
  typename Superclass::OutputVectorType targetvector;
  typename Superclass::OutputVectorType errorvector;

  std::ofstream outfile;
  outfile.open("output.txt");

  long num_iterations = this->GetIterations();
  m_Stop = false;
  long i = 0;
  while (!m_Stop)
    {
    int temp = rand() % (this->m_InputSamples.size()); 
    inputvector = this->m_InputSamples[temp];
    targetvector = this->m_Targets[temp];
    outputvector = Net->GenerateOutput(inputvector);
    errorvector = targetvector - outputvector; 
    outfile <<errorvector[0] << std::endl;
    Net->BackwardPropagate(this->m_PerformanceFunction->EvaluateDerivative(errorvector));  
    Net->UpdateWeights(this->m_LearningRate);
    i++;
    if (i > num_iterations)
      {
      m_Stop = true;
      }
    }
  if (this->m_PerformanceFunction->Evaluate(errorvector) < m_Threshold
   && i < num_iterations)
    {
    std::cout << "Goal Met " << std::endl;
    }
  else
    {
    std::cout << "Goal Not Met Max Iterations Reached " << std::endl;
    }
}

/** Print the object */
template<class TSample, class TOutput, class ScalarType> 
void  
IterativeSupervisedTrainingFunction<TSample,TOutput,ScalarType>
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
