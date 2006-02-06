/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFunctionBase.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkFunctionBase_h
#define __itkFunctionBase_h

#include "itkObject.h"
#include "itkObjectFactory.h"

namespace itk
{

/**
 * \class FunctionBase
 * \brief Base class for all ITK function objects
 *
 * FunctionBase is the base class for ITK function objects. Specifically,
 * the abstract method Evaluate() maps a point from the input space to a point
 * in the output space.
 *
 * Subclasses must override Evaluate(). 
 * 
 * This class is template over the input (domain) type and 
 * the output (range) type.
 *
 * \ingroup Functions
 * 
 */
template < class TInput, class TOutput >
class ITK_EXPORT FunctionBase : public Object
{
public:
  /** Standard class typedefs. */
  typedef FunctionBase Self;
  typedef Object Superclass;
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self> ConstPointer;
  
  /** Run-time type information (and related methods). */
  itkTypeMacro(FunctionBase, Object);

  /** Input type */
  typedef TInput InputType;

  /** Output type */
  typedef TOutput OutputType;

  /** Evaluate at the specified input position */
  virtual OutputType Evaluate( const InputType& input ) const = 0;

protected:
  FunctionBase(){};
  ~FunctionBase(){};

private:
  FunctionBase(const Self& ); //purposely not implemented
  void operator=(const Self& ); //purposely not implemented

};

} // end namespace itk

#endif
