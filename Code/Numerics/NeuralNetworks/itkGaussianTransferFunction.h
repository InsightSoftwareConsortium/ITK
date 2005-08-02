/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGaussianTransferFunction.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkGaussianTransferFunction_h
#define __itkGaussianTransferFunction_h

#include "itkTransferFunctionBase.h"


namespace itk
{
namespace Statistics
{

template<class ScalarType>
class GaussianTransferFunction : public TransferFunctionBase<ScalarType>
{
public:
  /** Standard class typedefs. */
  typedef GaussianTransferFunction Self;
  typedef TransferFunctionBase<ScalarType> Superclass;
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self> ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(GaussianTransferFunction, TransferFunctionBase);

  /** Method for creation through the object factory. */
  itkNewMacro(Self) ;
  /** Evaluate at the specified input position */
  ScalarType Evaluate(const ScalarType& input) const;

  /** Evaluate the derivative at the specified input position */
  ScalarType EvaluateDerivative(const ScalarType& input) const;

protected:

  GaussianTransferFunction();
  ~GaussianTransferFunction();
  
  /** Method to print the object. */
  virtual void PrintSelf( std::ostream& os, Indent indent ) const;

};//class

} // end namespace Statistics
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
  #include "itkGaussianTransferFunction.txx"
#endif

#endif
