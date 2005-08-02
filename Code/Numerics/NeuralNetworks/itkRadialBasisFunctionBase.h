/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRadialBasisFunctionBase.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef _itkRadialBasisFunctionBase_h
#define _itkRadialBasisFunctionBase_h

#include "itkFunctionBase.h"
#include "itkArray.h"

namespace itk
{
namespace Statistics
{

template<class ScalarType>
class RadialBasisFunctionBase : public FunctionBase<ScalarType,ScalarType>
{
public:

  /** Standard class typedefs. */
  typedef RadialBasisFunctionBase Self;
  typedef FunctionBase<ScalarType,ScalarType> Superclass;
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self> ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(RadialBasisFunctionBase, FunctionBase);

  /** Input/Output types*/ 
  typedef Array<ScalarType> ArrayType;

  ///** Evaluate at the specified input position */
  virtual ScalarType Evaluate(const ScalarType& input) const=0;

  /** Evaluate the derivative at the specified input position */
  virtual ScalarType EvaluateDerivative(const ScalarType& dist, const ArrayType& input,
                                                    char mode,int element_id=0) const=0;
  
  itkSetMacro(Radius,ScalarType);
  itkGetConstMacro( Radius, ScalarType );

  itkSetMacro(Center,ArrayType);
  itkGetConstMacro(Center, ArrayType );

protected:
  
  RadialBasisFunctionBase() 
    {
    m_Radius = 0;
    }
  virtual ~RadialBasisFunctionBase() {};
   
  /** Method to print the object. */
  virtual void PrintSelf( std::ostream& os, Indent indent ) const
    {
    os << indent << "RadialBasisFunctionBase(" << this << ")" << std::endl; 
    Superclass::PrintSelf( os, indent );
    }

private:
  
  ArrayType  m_Center;
  ScalarType m_Radius;

  RadialBasisFunctionBase(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
};

} // end namespace Statistics
} // end namespace itk

#endif
