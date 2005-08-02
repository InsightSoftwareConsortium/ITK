/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSumInputFunction.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkSumInputFunction_h
#define __itkSumInputFunction_h

#include "itkInputFunctionBase.h"
#include "vnl/vnl_matrix.h"
#include "vnl/vnl_vector.h"

namespace itk
{
namespace Statistics
{

template<class TVector, class ScalarType>
class SumInputFunction : public InputFunctionBase<TVector, ScalarType>
{
public:

  /** Standard class typedefs. */
  typedef SumInputFunction Self;
  typedef InputFunctionBase<TVector, ScalarType> Superclass;
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self> ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(SumInputFunction, FunctionBase);

  /** Method for creation through the object factory. */
  itkNewMacro(Self) ;

  /** Evaluate at the specified input position */
  ScalarType Evaluate(const TVector& input) const;

  void SetSize(long n);

protected:

  SumInputFunction();
  ~SumInputFunction();

  long m_Size;

  /** Method to print the object. */
  virtual void PrintSelf( std::ostream& os, Indent indent ) const;
};

} // end namespace Statistics
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
  #include "itkSumInputFunction.txx"
#endif

#endif
