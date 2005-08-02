/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRBFBackPropagationLearningFunction.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/


#ifndef __itkRBFBackPropagationLearningFunction_h
#define __itkRBFBackPropagationLearningFunction_h

#include <iostream>
#include "itkLightProcessObject.h"
#include "itkLearningFunctionBase.h"
#include "itkRBFLayer.h"

namespace itk
{
namespace Statistics
{

template<class LayerType, class TOutput>
class RBFBackPropagationLearningFunction : public LearningFunctionBase<LayerType, TOutput>
{
public:

  typedef RBFBackPropagationLearningFunction Self;
  typedef LearningFunctionBase<LayerType, TOutput> Superclass;
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self> ConstPointer;

  /** Method for creation through the object factory. */
  itkTypeMacro(RBFBackPropagationLearningFunction, LearningFunctionBase);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  typedef typename Superclass::ValueType ValueType;

  void Learn(LayerType* layer,ValueType learningrate);

  void Learn(LayerType* layer, TOutput error, ValueType learningrate);

  itkSetMacro(LearningRate1, ValueType);
  itkGetMacro(LearningRate1, ValueType);
  itkSetMacro(LearningRate2, ValueType);
  itkGetMacro(LearningRate2, ValueType);
  itkSetMacro(LearningRate3, ValueType);
  itkGetMacro(LearningRate3, ValueType);

protected:

  RBFBackPropagationLearningFunction();
  ~RBFBackPropagationLearningFunction() {};
 
  /** Method to print the object. */
  virtual void PrintSelf( std::ostream& os, Indent indent ) const;

private:

  ValueType             m_LearningRate1;   // output weights
  ValueType             m_LearningRate2;   // centers
  ValueType             m_LearningRate3;   // widths
  vnl_vector<ValueType> m_OutputErrors;

};

} // end namespace Statistics
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
  #include "itkRBFBackPropagationLearningFunction.txx"
#endif

#endif
