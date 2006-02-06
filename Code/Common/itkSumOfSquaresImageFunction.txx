/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSumOfSquaresImageFunction.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkSumOfSquaresImageFunction_txx
#define __itkSumOfSquaresImageFunction_txx

#include "itkSumOfSquaresImageFunction.h"

#include "itkNumericTraits.h"
#include "itkConstNeighborhoodIterator.h"

namespace itk
{

/**
 * Constructor
 */
template <class TInputImage, class TCoordRep>
SumOfSquaresImageFunction<TInputImage,TCoordRep>
::SumOfSquaresImageFunction()
{
  m_NeighborhoodRadius = 1;
  m_NeighborhoodSize = 1;
}


/**
 *
 */
template <class TInputImage, class TCoordRep>
void
SumOfSquaresImageFunction<TInputImage,TCoordRep>
::PrintSelf(std::ostream& os, Indent indent) const
{
  this->Superclass::PrintSelf(os,indent);
  os << indent << "NeighborhoodRadius: "  << m_NeighborhoodRadius << std::endl;
  os << indent << "NeighborhoodSize: "  << m_NeighborhoodSize << std::endl;
}


/**
 *
 */
template <class TInputImage, class TCoordRep>
typename SumOfSquaresImageFunction<TInputImage,TCoordRep>
::RealType
SumOfSquaresImageFunction<TInputImage,TCoordRep>
::EvaluateAtIndex(const IndexType& index) const
{
  RealType sumOfSquares;

  sumOfSquares = NumericTraits<RealType>::Zero;
  
  if( !this->GetInputImage() )
    {
    return ( NumericTraits<RealType>::max() );
    }
  
  if ( !this->IsInsideBuffer( index ) )
    {
    return ( NumericTraits<RealType>::max() );
    }

  // Create an N-d neighborhood kernel, using a zeroflux boundary condition
  typename InputImageType::SizeType kernelSize;
  kernelSize.Fill( m_NeighborhoodRadius );
  
  ConstNeighborhoodIterator<InputImageType>
    it(kernelSize, this->GetInputImage(), this->GetInputImage()->GetBufferedRegion());

  // Set the iterator at the desired location
  it.SetLocation(index);

  // Walk the neighborhood
  const unsigned int size = it.Size();
  for (unsigned int i = 0; i < size; ++i)
    {
    const RealType value = static_cast<RealType>( it.GetPixel(i) );
    sumOfSquares  += value * value;
    }
  
  return ( sumOfSquares );
}


} // end namespace itk

#endif
