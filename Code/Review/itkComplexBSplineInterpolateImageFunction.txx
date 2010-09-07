/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkComplexBSplineInterpolateImageFunction.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright ( c ) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkComplexBSplineInterpolateImageFunction_txx
#define __itkComplexBSplineInterpolateImageFunction_txx

#include "itkComplexBSplineInterpolateImageFunction.h"

namespace itk
{
/**
 * Constructor
 */
template< class TImageType, class TCoordRep, class TCoefficientType >
ComplexBSplineInterpolateImageFunction< TImageType, TCoordRep, TCoefficientType >
::ComplexBSplineInterpolateImageFunction()
{
  m_RealInterpolator = InterpolatorType::New();
  m_ImaginaryInterpolator = InterpolatorType::New();

  m_RealFilter = RealFilterType::New();
  m_ImaginaryFilter = ImaginaryFilterType::New();

  this->SetSplineOrder(3);
}

/**
 * Standard "PrintSelf" method
 */
template< class TImageType, class TCoordRep, class TCoefficientType >
void ComplexBSplineInterpolateImageFunction< TImageType, TCoordRep, TCoefficientType >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Spline Order: " << m_SplineOrder << std::endl;
  os << indent << "Real Interpolator: " << m_RealInterpolator << std::endl;
  os << indent << "Imaginary Interpolator: " << m_ImaginaryInterpolator << std::endl;
  os << indent << "Complex to Real Filter: " << m_RealFilter << std::endl;
  os << indent << "Complex to Imaginary Filter: " << m_ImaginaryFilter << std::endl;
}

template< class TImageType, class TCoordRep, class TCoefficientType >
void ComplexBSplineInterpolateImageFunction< TImageType, TCoordRep, TCoefficientType >
::SetInputImage(const TImageType *inputData)
{
  if ( inputData )
    {
    m_RealFilter->SetInput(inputData);
    m_ImaginaryFilter->SetInput(inputData);

    m_RealInterpolator->SetInputImage( m_RealFilter->GetOutput() );
    m_ImaginaryInterpolator->SetInputImage( m_ImaginaryFilter->GetOutput() );

    Superclass::SetInputImage(inputData);
    }
}

template< class TImageType, class TCoordRep, class TCoefficientType >
void ComplexBSplineInterpolateImageFunction< TImageType, TCoordRep, TCoefficientType >
::SetSplineOrder(unsigned int SplineOrder)
{
  m_SplineOrder = SplineOrder;
  m_RealInterpolator->SetSplineOrder(SplineOrder);
  m_ImaginaryInterpolator->SetSplineOrder(SplineOrder);
}

template< class TImageType, class TCoordRep, class TCoefficientType >
typename ComplexBSplineInterpolateImageFunction< TImageType, TCoordRep,
                                                 TCoefficientType >::OutputType ComplexBSplineInterpolateImageFunction<
  TImageType, TCoordRep, TCoefficientType >
::EvaluateAtContinuousIndex(const ContinuousIndexType & x) const
{
  typename InterpolatorType::OutputType realPart =
    m_RealInterpolator->EvaluateAtContinuousIndex(x);
  typename InterpolatorType::OutputType imagPart =
    m_ImaginaryInterpolator->EvaluateAtContinuousIndex(x);
  typedef typename ComplexBSplineInterpolateImageFunction< TImageType, TCoordRep, TCoefficientType >::OutputType
  ReturnType;

  return ReturnType(realPart, imagPart);
}
} // namespace itk

#endif
