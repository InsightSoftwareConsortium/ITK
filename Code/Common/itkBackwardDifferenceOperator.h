/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBackwardDifferenceOperator.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkBackwardDifferenceOperator_h
#define __itkBackwardDifferenceOperator_h

#include "itkNeighborhoodOperator.h"
#include "itkExceptionObject.h"

namespace itk {

/**
 * \class BackwardDifferenceOperator
 * \brief Operator whose inner product with a neighborhood returns
 * a "half" derivative at the center of the neighborhood.
 *
 * BackwardDifferenceOperator uses backward differences
 * i.e. F(x) - F(x-1) to calculate a "half" derivative useful, among
 * other things, in solving differential equations. It is a directional
 * NeighborhoodOperator that should be applied to a Neighborhood using the
 * inner product. 
 *
 * \ingroup Operators
 *
 */
template<class TPixel, unsigned int TDimension=2,
  class TAllocator = NeighborhoodAllocator<TPixel> >
class ITK_EXPORT BackwardDifferenceOperator
  : public NeighborhoodOperator<TPixel, TDimension, TAllocator>
{
public:
  /** Standard class typedefs. */
  typedef BackwardDifferenceOperator                           Self;
  typedef NeighborhoodOperator<TPixel, TDimension, TAllocator> Superclass;
    
  /** From Superclass */
  typedef typename Superclass::PixelType PixelType;

  /** Constructor. */
  BackwardDifferenceOperator() {}

protected:
  /** Necessary to work around a compiler bug in VC++. */
  typedef typename Superclass::CoefficientVector CoefficientVector;

  /** Calculates operator coefficients. */
  CoefficientVector GenerateCoefficients();

  /** Arranges coefficients spatially in the memory buffer. */
  void Fill(const CoefficientVector &coeff)
    { this->FillCenteredDirectional(coeff); }

private:
  BackwardDifferenceOperator(const Self& other); //purposely not implemented
  Self &operator=(const Self& other); //purposely not implemented
  
};

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkBackwardDifferenceOperator.txx"
#endif

#endif
