/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkErrorBackPropagationLearningFunctionBase.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

/** \class ErrorBackPropagationLearningFunctionBase
 *  \brief The ErrorBackPropagationLearningFunctionBase is the base 
 *  class for all the ErrorBackPropagationLearning strategies. These
 *  include error back propagation, bp+momentum, conjugte gradient descent, quick prop. 
 *  This class specifies how the errors are backpropagated for a layer. They take a LayerBase 
 *  object as input and compute the input for the layers input weightset 
 */

#ifndef __itkErrorBackPropagationLearningFunctionBase_h
#define __itkErrorBackPropagationLearningFunctionBase_h

#include <iostream>
#include "itkLightProcessObject.h"
#include "itkLearningFunctionBase.h"

namespace itk
{
namespace Statistics
{
template<class LayerType, class TTargetVector>
class ErrorBackPropagationLearningFunctionBase : public LearningFunctionBase<typename LayerType::LayerInterfaceType, TTargetVector>
{
public:
  typedef ErrorBackPropagationLearningFunctionBase Self;
  typedef LearningFunctionBase<typename LayerType::LayerInterfaceType, TTargetVector>
                                                   Superclass;
  typedef SmartPointer<Self>                       Pointer;
  typedef SmartPointer<const Self>                 ConstPointer;

  typedef typename Superclass::ValueType         ValueType;
  typedef typename LayerType::LayerInterfaceType LayerInterfaceType;

  /** Method for creation through the object factory. */
  itkTypeMacro(ErrorBackPropagationLearningFunctionBase, LearningFunctionBase);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  virtual void Learn( LayerInterfaceType * layer, ValueType learningrate);
  virtual void Learn( LayerInterfaceType * layer, TTargetVector error, ValueType learningrate);

protected:

  ErrorBackPropagationLearningFunctionBase() {};
  ~ErrorBackPropagationLearningFunctionBase() {};

  virtual void PrintSelf( std::ostream& os, Indent indent ) const;

};

} // end namespace Statistics
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
  #include "itkErrorBackPropagationLearningFunctionBase.txx"
#endif

#endif
