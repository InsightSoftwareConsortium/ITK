/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVarianceImageFunction.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkVarianceImageFunction_txx
#define _itkVarianceImageFunction_txx

#include "itkNumericTraits.h"
#include "itkConstSmartNeighborhoodIterator.h"

namespace itk
{

/**
 * Constructor
 */
template <class TInputImage>
VarianceImageFunction<TInputImage>
::VarianceImageFunction()
{
}


/**
 *
 */
template<class TInputImage>
void
VarianceImageFunction<TInputImage>
::PrintSelf(std::ostream& os, Indent indent) const
{
  this->Superclass::PrintSelf(os,indent);
}


/**
 *
 */
template <class TInputImage>
VarianceImageFunction<TInputImage>::RealType
VarianceImageFunction<TInputImage>
::EvaluateAtIndex(const IndexType& index) const
{
  int i;
  double num;
  RealType sum;
  RealType sumOfSquares;
  RealType var;

  var = NumericTraits<RealType>::Zero;
  sum = NumericTraits<RealType>::Zero;
  sumOfSquares = NumericTraits<RealType>::Zero;
  
  if( !m_Image )
    {
    return ( NumericTraits<RealType>::max() );
    }
  
  if ( !this->IsInsideBuffer( index ) )
    {
    return ( NumericTraits<RealType>::max() );
    }

  // Create an N-d neighborhood kernel, using a zeroflux boundary condition
  InputImageType::SizeType kernelSize;
  kernelSize.Fill( 1 );
  
  ConstSmartNeighborhoodIterator<InputImageType>
    it(kernelSize, m_Image, m_Image->GetBufferedRegion());

  // Set the iterator at the desired location
  it.SetLocation(index);

  // Walk the neighborhood
  for (i = 0; i < it.Size(); ++i)
    {
    sum += static_cast<RealType>(it.GetPixel(i));
    sumOfSquares
      += (static_cast<RealType>(it.GetPixel(i))
          * static_cast<RealType>(it.GetPixel(i)));
    }

  num = (double) it.Size();
  var = (sumOfSquares - (sum*sum / num)) / (num - 1.0);
  
  return ( var );
}


} // namespace itk

#endif
