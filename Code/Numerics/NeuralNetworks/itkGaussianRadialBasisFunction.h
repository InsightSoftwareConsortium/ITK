/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGaussianRadialBasisFunction.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkGaussianRadialBasisFunction_h
#define __itkGaussianRadialBasisFunction_h

#include "itkRadialBasisFunctionBase.h"
#include "itkNNetDistanceMetricBase.h"

namespace itk
{
namespace Statistics
{
template<class ScalarType>
class GaussianRadialBasisFunction : public RadialBasisFunctionBase<ScalarType>
{
public:

  /** Standard class typedefs. */
  typedef GaussianRadialBasisFunction Self;
  typedef RadialBasisFunctionBase<ScalarType> Superclass;
  typedef typename Superclass::ArrayType ArrayType;
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self> ConstPointer;
 
  /** Run-time type information (and related methods). */
  itkTypeMacro(GaussianRadialBasisFunction,RadialBasisFunctionBase);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);
  
  /** Evaluate at the specified input position */
  ScalarType Evaluate(const ScalarType& input) const;
 
  ScalarType EvaluateDerivative(const ScalarType& dist,const ArrayType& input,
                                      char mode,int element_id=0) const;

protected:
 
  GaussianRadialBasisFunction();
  ~GaussianRadialBasisFunction();
  
  /** Method to print the object. */
  virtual void PrintSelf( std::ostream& os, Indent indent ) const;

};

} // end namespace Statistics
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
  #include "itkGaussianRadialBasisFunction.txx"
#endif

#endif
