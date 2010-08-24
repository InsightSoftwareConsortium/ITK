/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkLabelToRGBImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkLabelToRGBImageFilter_txx
#define __itkLabelToRGBImageFilter_txx

#include "itkLabelToRGBImageFilter.h"

namespace itk
{
/**
 *
 */
template< class TLabelImage, class TOutputImage >
LabelToRGBImageFilter< TLabelImage, TOutputImage >
::LabelToRGBImageFilter()
{
  m_BackgroundValue = NumericTraits< LabelPixelType >::Zero;
  m_BackgroundColor.Fill(NumericTraits< OutputPixelValueType >::Zero);
}

template< class TLabelImage, class TOutputImage >
unsigned int
LabelToRGBImageFilter< TLabelImage, TOutputImage >
::GetNumberOfColors() const
{
  return this->GetFunctor().GetNumberOfColors();
}

template< class TLabelImage, class TOutputImage >
void
LabelToRGBImageFilter< TLabelImage, TOutputImage >
::ResetColors()
{
  this->GetFunctor().ResetColors();
}

template< class TLabelImage, class TOutputImage >
void
LabelToRGBImageFilter< TLabelImage, TOutputImage >
::AddColor(ComponentType r, ComponentType g, ComponentType b)
{
  this->GetFunctor().AddColor(r, g, b);
}

template< class TLabelImage, class TOutputImage >
void
LabelToRGBImageFilter< TLabelImage, TOutputImage >
::BeforeThreadedGenerateData()
{
  this->GetFunctor().SetBackgroundValue(m_BackgroundValue);
  this->GetFunctor().SetBackgroundColor(m_BackgroundColor);
}

/**
 *
 */
template< class TLabelImage, class TOutputImage >
void
LabelToRGBImageFilter< TLabelImage, TOutputImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "BackgroundValue: "
     << static_cast< typename NumericTraits< LabelPixelType >::PrintType >( m_BackgroundValue )
     << std::endl
     << indent << "ColorBackground: "
     << static_cast< typename NumericTraits< OutputPixelType >::PrintType >( m_BackgroundColor )
     << std::endl;
}
} // end namespace itk

#endif
