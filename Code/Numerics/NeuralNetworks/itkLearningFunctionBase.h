/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkLearningFunctionBase.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
/** \class LearningFunctionBase
 *  \brief The LearningFunctionBase is the base class for all the learning strategies.
 *  These include error back propagation, bp+momentum, conjugte gradient descent, quick prop. 
 *  This class specifies how the errors are backpropagated for a layer. They take a LayerBase 
 *  object as input and compute the input for the layers input weightset */

#ifndef __itkLearningFunctionBase_h
#define __itkLearningFunctionBase_h

#include "itkLightProcessObject.h"

namespace itk
{
namespace Statistics
{

template<class LayerType, class TOutput>
class LearningFunctionBase : public LightProcessObject
{
public:

  typedef LearningFunctionBase Self;
  typedef LightProcessObject Superclass;
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self> ConstPointer;

  /** Method for creation through the object factory. */
  itkTypeMacro(LearningFunctionBase, LightProcessObject);

  typedef typename LayerType::ValueType ValueType;
  
  virtual void Learn(LayerType* layer, ValueType) = 0;
  virtual void Learn(LayerType* layer, TOutput error,ValueType) = 0;

protected:

  LearningFunctionBase() {};
  ~LearningFunctionBase() {};

  /** Method to print the object. */
  virtual void PrintSelf( std::ostream& os, Indent indent ) const
    {
    os << indent << "LearningFunctionBase(" << this << ")" << std::endl; 
    Superclass::PrintSelf( os, indent );
    }
};

} // end namespace Statistics
} // end namespace itk

#endif
