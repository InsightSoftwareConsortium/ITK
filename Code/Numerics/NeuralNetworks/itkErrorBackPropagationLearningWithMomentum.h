/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkErrorBackPropagationLearningWithMomentum.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

/** \class ErrorBackPropagationLearningWithMomentum
 *  \brief The ErrorBackPropagationLearningWithMomentum is the base 
 *  class for all the ErrorBackPropagationLearning strategies. These
 *  include error back propagation, bp+momentum, conjugte gradient descent, quick prop. 
 *  This class specifies how the errors are backpropagated for a layer. They take a LayerBase 
 *  object as input and compute the input for the layers input weightset
 */

#ifndef __itkErrorBackPropagationLearningWithMomentum_h
#define __itkErrorBackPropagationLearningWithMomentum_h

#include <iostream>
#include "itkLightProcessObject.h"
#include "itkLearningFunctionBase.h"

namespace itk
{
namespace Statistics
{

template<class LayerType, class TTargetVector>
class ErrorBackPropagationLearningWithMomentum : public LearningFunctionBase<typename LayerType::LayerInterfaceType, TTargetVector>
{
public:

  typedef ErrorBackPropagationLearningWithMomentum Self;
  typedef LearningFunctionBase<typename LayerType::LayerInterfaceType, TTargetVector>
                                                   Superclass;
  typedef SmartPointer<Self>                       Pointer;
  typedef SmartPointer<const Self>                 ConstPointer;

  /** Method for creation through the object factory. */
  itkTypeMacro(ErrorBackPropagationLearningWithMomentum, LearningFunctionBase);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  typedef typename Superclass::ValueType           ValueType;
  typedef typename LayerType::LayerInterfaceType   LayerInterfaceType;

  virtual void Learn( LayerInterfaceType * layer, ValueType learningrate );
  virtual void Learn( LayerInterfaceType * layer, TTargetVector errors, ValueType learningrate );


protected:
  ErrorBackPropagationLearningWithMomentum();
  virtual ~ErrorBackPropagationLearningWithMomentum() {};

  virtual void PrintSelf( std::ostream& os, Indent indent ) const;

  ValueType m_Momentum;
};

} // end namespace Statistics
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
  #include "itkErrorBackPropagationLearningWithMomentum.txx"
#endif

#endif
