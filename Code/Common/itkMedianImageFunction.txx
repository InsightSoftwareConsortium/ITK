/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMedianImageFunction.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkMedianImageFunction_txx
#define _itkMedianImageFunction_txx
#include "itkMedianImageFunction.h"

#include "itkNumericTraits.h"
#include "itkConstNeighborhoodIterator.h"
#include <vector>
#include <algorithm>


namespace itk
{

/**
 * Constructor
 */
template <class TInputImage, class TCoordRep>
MedianImageFunction<TInputImage,TCoordRep>
::MedianImageFunction()
{
}


/**
 *
 */
template <class TInputImage, class TCoordRep>
void
MedianImageFunction<TInputImage,TCoordRep>
::PrintSelf(std::ostream& os, Indent indent) const
{
  this->Superclass::PrintSelf(os,indent);
}


/**
 *
 */
template <class TInputImage, class TCoordRep>
typename MedianImageFunction<TInputImage,TCoordRep>
::OutputType
MedianImageFunction<TInputImage,TCoordRep>
::EvaluateAtIndex(const IndexType& index) const
{
  int i;
  
  if( !m_Image )
    {
    return ( NumericTraits<OutputType>::max() );
    }
  
  if ( !this->IsInsideBuffer( index ) )
    {
    return ( NumericTraits<OutputType>::max() );
    }

  // Create an N-d neighborhood kernel, using a zeroflux boundary condition
  typename InputImageType::SizeType kernelSize;
  kernelSize.Fill( 1 );
  
  ConstNeighborhoodIterator<InputImageType>
    it(kernelSize, m_Image, m_Image->GetBufferedRegion());

  // Set the iterator at the desired location
  it.SetLocation(index);

  // We have to copy the pixels so we can run std::nth_element.
  std::vector<InputPixelType> pixels;
  typename std::vector<InputPixelType>::iterator medianIterator;
  
  // Walk the neighborhood
  for (i = 0; i < it.Size(); ++i)
    {
    pixels.push_back( it.GetPixel(i) );
    }

  // Get the median value
  unsigned int medianPosition = it.Size() / 2;
  medianIterator = pixels.begin() + medianPosition;
  std::nth_element(pixels.begin(), medianIterator, pixels.end());
             
  return ( *medianIterator );
}


} // namespace itk

#endif
