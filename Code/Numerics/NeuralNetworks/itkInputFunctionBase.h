/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkInputFunctionBase.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef _itkInputFunctionBase_h
#define _itkInputFunctionBase_h

#include "itkFunctionBase.h"

namespace itk
{
namespace Statistics
{

template<class TVector, class TOutput>
class InputFunctionBase : public FunctionBase<TVector, TOutput>
{
public:

  /** Standard class typedefs. */
  typedef InputFunctionBase Self;
  typedef FunctionBase<TVector, TOutput> Superclass;
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self> ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(InputFunctionBase, FunctionBase);

  /** Input type */
  typedef TVector InputVectorType;

  /** Output type */
  typedef TOutput OutputType;

  /** Evaluate at the specified input position */
  virtual OutputType Evaluate(const InputVectorType& input) const = 0;

  virtual void SetSize(long) = 0;

protected:

  InputFunctionBase() {};
  virtual~InputFunctionBase() {};
  
  /** Method to print the object. */
  virtual void PrintSelf( std::ostream& os, Indent indent ) const
    {
    os << indent << "InputFunctionBase(" << this << ")" << std::endl; 
    Superclass::PrintSelf( os, indent ); 
    }

private:

  InputFunctionBase(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

};//class

} // end namespace Statistics
} // end namespace itk

#endif

