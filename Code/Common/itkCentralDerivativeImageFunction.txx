/*==========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkCentralDerivativeImageFunction.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.
  
==========================================================================*/
#ifndef _itkCentralDerivativeImageFunction_txx
#define _itkCentralDerivativeImageFunction_txx

namespace itk
{

/**
 *
 */
template <class TInputImage>
void
CentralDerivativeImageFunction<TInputImage>
::SetInputImage( InputImageType * ptr )
{
  this->Superclass::SetInputImage( ptr );

  m_ImageSize = 
    this->GetInputImage()->GetLargestPossibleRegion().GetSize();
  
}


/**
 *
 */
template<class TInputImage>
void
CentralDerivativeImageFunction<TInputImage>
::PrintSelf(std::ostream& os, Indent indent)
{
  this->Superclass::PrintSelf(os,indent);
  os << indent << "calculate central difference derivative:" << std::endl;
}


/**
 *
 */
template <class TInputImage>
double
CentralDerivativeImageFunction<TInputImage>
::Evaluate(
const IndexType& index,
unsigned int dim ) const
{
  
  if( !m_Image || dim > ImageDimension - 1 )
    {
    return 0.0;
    }
  
  for( unsigned int j = 0; j < ImageDimension; j++ )
    {
    if( index[j] > m_ImageSize[j] - 1 )
      return 0.0;
    }


  double derivative = 0.0;
  IndexType neighIndex = index ;

  if( index[dim] < 1 || index[dim] > m_ImageSize[dim] - 2 )
    {
    // index out of range; return immediately
    return( derivative );
    }

  neighIndex[dim] += 1;
  derivative = m_Image->GetPixel( neighIndex );

  neighIndex[dim] -= 2;
  derivative -= m_Image->GetPixel( neighIndex );

  derivative *= 0.5;

  return ( derivative );

}


} // namespace itk

#endif
