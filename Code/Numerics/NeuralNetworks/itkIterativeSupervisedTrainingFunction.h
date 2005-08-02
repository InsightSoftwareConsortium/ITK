/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkIterativeSupervisedTrainingFunction.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkIterativeSupervisedTrainingFunction_h
#define __itkIterativeSupervisedTrainingFunction_h

#include "itkTrainingFunctionBase.h"


namespace itk
{
namespace Statistics
{

template<class TSample, class TOutput, class ScalarType>
class IterativeSupervisedTrainingFunction : public TrainingFunctionBase<TSample, TOutput, ScalarType>
{
public:

  typedef IterativeSupervisedTrainingFunction Self;
  typedef TrainingFunctionBase<TSample, TOutput, ScalarType> Superclass;
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self> ConstPointer;

  /** Method for creation through the object factory. */
  itkTypeMacro(IterativeSupervisedTrainingFunction, TrainingFunctionBase);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  typedef typename Superclass::NetworkType NetworkType;

  void SetNumOfIterations(long i);

  void Train(NetworkType* net, TSample* samples, TOutput* targets);

  itkSetMacro(Threshold, ScalarType);

protected:

  IterativeSupervisedTrainingFunction();
  ~IterativeSupervisedTrainingFunction(){};
  
  /** Method to print the object. */
  virtual void PrintSelf( std::ostream& os, Indent indent ) const;

  ScalarType m_Threshold;
  bool       m_Stop; //stop condition
};

} // end namespace Statistics
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
  #include "itkIterativeSupervisedTrainingFunction.txx"
#endif

#endif
