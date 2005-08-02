/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkQuickPropLearningRule.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

/*
The QuickPropLearningRule is the base class for all the ErrorBackPropagationLearning strategies. These
include error back propagation, bp+momentum, conjugte gradient descent, quick prop. 
This class specifies how the errors are backpropagated for a layer. They take a LayerBase 
object as input and compute the input for the layers input weightset
*/

#ifndef __itkQuickPropLearningRule_h
#define __itkQuickPropLearningRule_h

#include <iostream>
#include "itkLightProcessObject.h"
#include "itkLearningFunctionBase.h"

namespace itk
{
namespace Statistics
{

template<class LayerType, class TOutput>
class QuickPropLearningRule : public LearningFunctionBase<LayerType, TOutput>
{
public:

  typedef QuickPropLearningRule Self;
  typedef LearningFunctionBase<LayerType, TOutput> Superclass;
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self> ConstPointer;

  /** Method for creation through the object factory. */
  itkTypeMacro(QuickPropLearningRule, LearningFunctionBase);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  typedef typename Superclass::ValueType ValueType;
  void Learn(LayerType* layer, ValueType learningrate);

  void Learn(LayerType* layer, TOutput errors, ValueType learningrate);

  itkSetMacro(Max_Growth_Factor, ValueType);
  itkGetConstReferenceMacro(Max_Growth_Factor,ValueType);

  itkSetMacro(Decay, ValueType);
  itkGetConstReferenceMacro(Decay,ValueType);

protected:
  QuickPropLearningRule();
  ~QuickPropLearningRule(){};

  ValueType m_Momentum;
  ValueType m_Max_Growth_Factor;
  ValueType m_Decay;
  ValueType m_Threshold;
  ValueType m_Epsilon;
  ValueType m_SigmoidPrimeOffset;
  ValueType m_SplitEpsilon;

  /** Method to print the object. */
  virtual void PrintSelf( std::ostream& os, Indent indent ) const;
};

} // end namespace Statistics
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
  #include "itkQuickPropLearningRule.txx"
#endif

#endif
