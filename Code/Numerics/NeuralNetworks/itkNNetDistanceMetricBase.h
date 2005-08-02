/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNNetDistanceMetricBase.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef _itkNNetDistanceMetricBase_h
#define _itkNNetDistanceMetricBase_h

#include "itkFunctionBase.h"

namespace itk
{
namespace Statistics
{

template<class TVector>
class NNetDistanceMetricBase : public FunctionBase<TVector, double>
{
public:

  /** Standard class typedefs. */
  typedef NNetDistanceMetricBase Self;
  typedef FunctionBase<TVector,double> Superclass;
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self> ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(NNetDistanceMetricBase, FunctionBase);

  /** Input type */
  typedef TVector InputVectorType;
 
  /** Evaluate at the specified input position */
  virtual double Evaluate(const InputVectorType& x1, const InputVectorType& x2) const = 0;

  /** Evaluate */
  virtual double Evaluate(const InputVectorType& x) const {return 0;}

protected:
  NNetDistanceMetricBase() {};
  virtual ~NNetDistanceMetricBase() {};
 
  /** Method to print the object. */
  virtual void PrintSelf( std::ostream& os, Indent indent ) const
    {
    os << indent << "NNetDistanceMetricBase(" << this << ")" << std::endl; 
    Superclass::PrintSelf( os, indent ); 
    } 

private:

  NNetDistanceMetricBase (const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
 
};

} // end namespace Statistics
} // end namespace itk

#endif
