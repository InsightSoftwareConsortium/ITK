/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkCentralDifferenceImageFunction.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkCentralDifferenceImageFunction_txx
#define _itkCentralDifferenceImageFunction_txx

namespace itk
{


/**
 * Constructor
 */
template <class TInputImage>
CentralDifferenceImageFunction<TInputImage>
::CentralDifferenceImageFunction()
{
}


/**
 *
 */
template<class TInputImage>
void
CentralDifferenceImageFunction<TInputImage>
::PrintSelf(std::ostream& os, Indent indent) const
{
  this->Superclass::PrintSelf(os,indent);
}


/**
 *
 */
template <class TInputImage>
double
CentralDifferenceImageFunction<TInputImage>
::EvaluateAtIndex(
const IndexType& index,
unsigned int dim ) const
{
  
  double derivative = 0.0;

  if( !m_Image || dim > ImageDimension - 1 )
    {
    return ( derivative );
    }
  
  if ( !this->IsInsideBuffer( index ) )
    {
    return ( derivative );
    }

  IndexType neighIndex = index;

  const typename InputImageType::SizeType& size =
    m_Image->GetBufferedRegion().GetSize();
  const typename InputImageType::IndexType& start =
    m_Image->GetBufferedRegion().GetIndex();

  // bounds checking
  if( index[dim] < static_cast<long>(start[dim]) + 1 ||
      index[dim] >= (start[dim] + static_cast<long>(size[dim]) - 2 ) )
    {
      return ( derivative );
    }
  
  // compute derivative
  neighIndex[dim] += 1;
  derivative = m_Image->GetPixel( neighIndex );

  neighIndex[dim] -= 2;
  derivative -= m_Image->GetPixel( neighIndex );

  derivative *= 0.5 / m_Spacing[dim];


  return ( derivative );

}


} // namespace itk

#endif
