/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBinaryThresholdImageFunction.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkBinaryThresholdImageFunction_txx
#define __itkBinaryThresholdImageFunction_txx

#include "itkBinaryThresholdImageFunction.h"

namespace itk
{

template <class TInputImage, class TCoordRep>
BinaryThresholdImageFunction<TInputImage,TCoordRep>
::BinaryThresholdImageFunction()
{
  m_Lower = NumericTraits<PixelType>::NonpositiveMin();
  m_Upper = NumericTraits<PixelType>::max();
}

/**
 * Values greater than or equal to the value are inside
 */
template <class TInputImage, class TCoordRep>
void 
BinaryThresholdImageFunction<TInputImage,TCoordRep>
::ThresholdAbove(PixelType thresh)
{
  if (m_Lower != thresh
      || m_Upper != NumericTraits<PixelType>::max())
    {
    m_Lower = thresh;
    m_Upper = NumericTraits<PixelType>::max();
    this->Modified();
    }
}

/**
 * The values less than or equal to the value are inside
 */
template <class TInputImage, class TCoordRep>
void 
BinaryThresholdImageFunction<TInputImage,TCoordRep>
::ThresholdBelow(PixelType thresh)
{
  if (m_Lower != NumericTraits<PixelType>::NonpositiveMin()
      || m_Upper != thresh)
    {
    m_Lower = NumericTraits<PixelType>::NonpositiveMin();
    m_Upper = thresh;
    this->Modified();
    }
}

/**
 * The values less than or equal to the value are inside
 */
template <class TInputImage, class TCoordRep>
void 
BinaryThresholdImageFunction<TInputImage,TCoordRep>
::ThresholdBetween(PixelType lower, PixelType upper)
{
  if (m_Lower != lower
      || m_Upper != upper)
    {
    m_Lower = lower;
    m_Upper = upper;
    this->Modified();
    }
}

template <class TInputImage, class TCoordRep>
void 
BinaryThresholdImageFunction<TInputImage,TCoordRep>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf( os, indent );

  os << indent << "Lower: " << m_Lower << std::endl;
  os << indent << "Upper: " << m_Upper << std::endl;
}

} // end namespace itk

#endif
