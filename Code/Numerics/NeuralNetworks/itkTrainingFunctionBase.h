/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkTrainingFunctionBase.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkTrainingFunctionBase_h
#define __itkTrainingFunctionBase_h

#include <iostream>
#include "itkLightProcessObject.h"
#include "itkNeuralNetworkObject.h"
#include "itkSquaredDifferenceErrorFunction.h"
#include "itkMeanSquaredErrorFunction.h"
namespace itk
{
namespace Statistics
{

template<class TSample, class TOutput, class ScalarType>
class TrainingFunctionBase : public LightProcessObject
{
public:
  typedef TrainingFunctionBase Self;
  typedef LightProcessObject Superclass;
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self> ConstPointer;

  /** Method for creation through the object factory. */
  itkTypeMacro(TrainingFunctionBase, LightProcessObject);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  typedef typename TSample::MeasurementVectorType VectorType;
  typedef typename TOutput::MeasurementVectorType OutputVectorType;

  typedef std::vector<VectorType> InputSampleVectorType;
  typedef std::vector<OutputVectorType> OutputSampleVectorType;
  typedef ScalarType ValueType;
  typedef NeuralNetworkObject<VectorType, OutputVectorType> NetworkType;
  typedef ErrorFunctionBase<OutputVectorType, ScalarType> PerformanceFunctionType;
  typedef SquaredDifferenceErrorFunction<OutputVectorType, ScalarType> DefaultPerformanceType;

  void SetTrainingSamples(TSample* samples);
  void SetTargetValues(TOutput* targets);
  void SetLearningRate(ValueType);

  ValueType GetLearningRate();

  itkSetMacro(Iterations, long);
  itkGetConstReferenceMacro(Iterations, long);

  void SetPerformanceFunction(PerformanceFunctionType* f);

  virtual void
  Train(NetworkType* net, TSample* samples, TOutput* targets){};

  inline VectorType
  defaultconverter(typename TSample::MeasurementVectorType v)
    {
    VectorType temp;
    for (unsigned int i = 0; i < v.Size(); i++)
      {
      temp[i] = static_cast<ScalarType>(v[i]) ;
      }
    return temp;
    }

  inline OutputVectorType
  targetconverter(typename TOutput::MeasurementVectorType v)
    {
    OutputVectorType temp;
    for (int i = 0; i < v.Size(); i++)
      {
      temp[i] = static_cast<ScalarType>(v[i]) ;
      }
    return temp;
    }

protected:

  TrainingFunctionBase();
  ~TrainingFunctionBase(){};
   
  /** Method to print the object. */
  virtual void PrintSelf( std::ostream& os, Indent indent ) const;

  TSample*                m_TrainingSamples;// original samples
  TOutput*                m_SampleTargets;  // original samples
  InputSampleVectorType   m_InputSamples;   // itk::vectors
  OutputSampleVectorType  m_Targets;        // itk::vectors
  long                    m_Iterations;    
  ValueType               m_LearningRate;
  typename PerformanceFunctionType::Pointer m_PerformanceFunction;
};

} // end namespace Statistics
} // end namespace itk
#ifndef ITK_MANUAL_INSTANTIATION
#include "itkTrainingFunctionBase.txx"
#endif

#endif
