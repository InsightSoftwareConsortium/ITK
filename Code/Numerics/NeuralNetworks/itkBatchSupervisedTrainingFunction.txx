/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBatchSupervisedTrainingFunction.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkBatchSupervisedTrainingFunction_txx
#define __itkBatchSupervisedTrainingFunction_txx

#include "itkBatchSupervisedTrainingFunction.h"
#include <fstream>
#include <algorithm>

namespace itk
{
namespace Statistics
{

template<class TSample, class TOutput, class ScalarType> 
BatchSupervisedTrainingFunction<TSample,TOutput,ScalarType>//,f>
::BatchSupervisedTrainingFunction()
{
  this->m_LearningRate = 0.1;  //0.5 multilayer test 0.1 perceptron
  m_Threshold = 0;
  m_Stop = false; //stop condition
}

template<class TSample, class TOutput, class ScalarType> 
void BatchSupervisedTrainingFunction<TSample,TOutput,ScalarType>
::SetNumOfIterations(long i)
{
  this->SetIterations(i);
}

template<class TSample, class TOutput, class ScalarType>
void BatchSupervisedTrainingFunction<TSample,TOutput,ScalarType>
::Train(typename BatchSupervisedTrainingFunction<TSample, TOutput, ScalarType>::NetworkType* net,
        TSample* samples, TOutput* targets)
{
  this->SetTrainingSamples(samples); 
  this->SetTargetValues(targets);
  typename Superclass::OutputVectorType outputvector;
  typename Superclass::VectorType inputvector;
  typename Superclass::OutputVectorType targetvector;
  typename Superclass::OutputVectorType errorvector;
 
  long num_iterations = this->GetIterations();
  m_Stop = false;
  long count = 0;
  std::ofstream outf;
  outf.open("output.txt");

  while (!m_Stop)
    {
    for (long i = 0; i < this->m_InputSamples.size(); i++)
      {
      inputvector = this->m_InputSamples[i];
      targetvector = this->m_Targets[i];
      outputvector = net->GenerateOutput(inputvector);
      errorvector = targetvector - outputvector; 
        
      outf << this->m_PerformanceFunction->Evaluate(errorvector) <<
      " "<<errorvector[0]<<std::endl;
      net->BackwardPropagate(this->m_PerformanceFunction->EvaluateDerivative
                                                            (errorvector));     
      }
    net->UpdateWeights(this->m_LearningRate);
    count++;
    if (count > num_iterations)
      {
      m_Stop = true;
      }
    }
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
}

/** Print the object */
template<class TSample, class TOutput, class ScalarType> 
void  
BatchSupervisedTrainingFunction<TSample,TOutput,ScalarType>
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
