/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageFunction.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkImageFunction_txx
#define _itkImageFunction_txx

#include "itkImageFunction.h"

namespace itk
{

/**
 * Constructor
 */
template <class TInputImage, class TOutput, class TCoordRep>
ImageFunction<TInputImage, TOutput, TCoordRep>
::ImageFunction()
{
  m_Image = NULL;
}


/**
 * Standard "PrintSelf" method
 */
template <class TInputImage, class TOutput, class TCoordRep>
void
ImageFunction<TInputImage, TOutput, TCoordRep>
::PrintSelf(
std::ostream& os, 
Indent indent) const
{
  Superclass::PrintSelf( os, indent );
  os << indent << "InputImage: " << m_Image.GetPointer() << std::endl;

}


/**
 * Initialize by setting the input image
 */
template <class TInputImage, class TOutput, class TCoordRep>
void
ImageFunction<TInputImage, TOutput, TCoordRep>
::SetInputImage(
const InputImageType * ptr )
{
  // set the input image
  m_Image = ptr;

  if ( ptr )
    {
    typedef typename IndexType::IndexValueType IndexValueType;
    CoordRepType epsilon = 5.0e-15;
    typename InputImageType::SizeType size = ptr->GetBufferedRegion().GetSize();
    m_StartIndex = ptr->GetBufferedRegion().GetIndex();

    for ( unsigned int j = 0; j < ImageDimension; j++ )
      {
      m_EndIndex[j] = m_StartIndex[j] + 
        static_cast<IndexValueType>( size[j] ) - 1;
      m_StartContinuousIndex[j] = static_cast<CoordRepType>( m_StartIndex[j] ) - epsilon;
      m_EndContinuousIndex[j]   = static_cast<CoordRepType>( m_EndIndex[j] ) + epsilon;
      }

    }
}



} // end namespace itk

#endif

