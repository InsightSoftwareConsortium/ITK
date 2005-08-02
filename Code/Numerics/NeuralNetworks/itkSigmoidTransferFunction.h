/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSigmoidTransferFunction.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkSigmoidTransferFunction_h
#define __itkSigmoidTransferFunction_h

#include "itkTransferFunctionBase.h"

namespace itk
{
namespace Statistics
{

template<class ScalarType>
class SigmoidTransferFunction : public TransferFunctionBase<ScalarType>
{

public:
  
  /** Standard class typedefs. */
  typedef SigmoidTransferFunction Self;
  typedef TransferFunctionBase<ScalarType> Superclass;
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self> ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(SigmoidTransferFunction, TransferFunctionBase);

  /** Method for creation through the object factory. */
  itkNewMacro(Self) ;

  /** Set/Get macros*/
  itkSetMacro(Alpha,ScalarType);
  itkGetMacro(Alpha,ScalarType);
  itkSetMacro(Beta,ScalarType);
  itkGetMacro(Beta,ScalarType);
  itkSetMacro(OutputMinimum,ScalarType);
  itkGetMacro(OutputMinimum,ScalarType);
  itkSetMacro(OutputMaximum,ScalarType);
  itkGetMacro(OutputMaximum,ScalarType);

  /** Evaluate at the specified input position */
  ScalarType Evaluate(const ScalarType& input) const;

  /** Evaluate the derivative at the specified input position */
  ScalarType EvaluateDerivative(const ScalarType& input) const;

protected:

  SigmoidTransferFunction();
  ~SigmoidTransferFunction();

  /** Method to print the object. */
  virtual void PrintSelf( std::ostream& os, Indent indent ) const;

private:

  ScalarType m_Alpha;
  ScalarType m_Beta;
  ScalarType m_OutputMinimum;
  ScalarType m_OutputMaximum;
};

} // end namespace Statistics
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
  #include "itkSigmoidTransferFunction.txx"
#endif

#endif
