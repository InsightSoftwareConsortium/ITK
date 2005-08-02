/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSymmetricSigmoidTransferFunction.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkSymmetricSigmoidTransferFunction_h
#define __itkSymmetricSigmoidTransferFunction_h

#include "itkTransferFunctionBase.h"

namespace itk
{
namespace Statistics
{

template<class ScalarType>
class SymmetricSigmoidTransferFunction : public TransferFunctionBase<ScalarType>
{
public:

  /** Standard class typedefs. */
  typedef SymmetricSigmoidTransferFunction Self;
  typedef TransferFunctionBase<ScalarType> Superclass;
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self> ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(SymmetricSigmoidTransferFunction, TransferFunctionBase);

  /** Method for creation through the object factory. */
  itkNewMacro(Self) ;

  /** Evaluate at the specified input position */
  ScalarType Evaluate(const ScalarType& input) const;

  /** Evaluate the derivative at the specified input position */
  ScalarType EvaluateDerivative(const ScalarType& input) const;

protected:

  SymmetricSigmoidTransferFunction();
  ~SymmetricSigmoidTransferFunction();
  
  ScalarType m_Range;
  ScalarType m_Offset;
  
  /** Method to print the object. */
  virtual void PrintSelf( std::ostream& os, Indent indent ) const;
};

} // end namespace Statistics
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
  #include "itkSymmetricSigmoidTransferFunction.txx"
#endif


#endif
