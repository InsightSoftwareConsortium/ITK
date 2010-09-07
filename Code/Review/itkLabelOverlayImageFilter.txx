/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkLabelOverlayImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkLabelOverlayImageFilter_txx
#define __itkLabelOverlayImageFilter_txx

#include "itkLabelOverlayImageFilter.h"

namespace itk
{
/**
 * Constructor method
 */
template< class TInputImage, class TLabelImage, class TOutputImage >
LabelOverlayImageFilter< TInputImage, TLabelImage, TOutputImage >
::LabelOverlayImageFilter()
{
  m_Opacity = 0.5;
  m_BackgroundValue = NumericTraits< LabelPixelType >::Zero;
}

/**
 * Destructor method
 */
template< class TInputImage, class TLabelImage, class TOutputImage >
void
LabelOverlayImageFilter< TInputImage, TLabelImage, TOutputImage >
::BeforeThreadedGenerateData()
{
  this->GetFunctor().SetOpacity(m_Opacity);
  this->GetFunctor().SetBackgroundValue(m_BackgroundValue);
}

/**
 * Set Label Image
 */
template< class TInputImage, class TLabelImage, class TOutputImage >
void
LabelOverlayImageFilter< TInputImage, TLabelImage, TOutputImage >
::SetLabelImage(const TLabelImage *input)
{
  this->SetInput2(input);
}

/**
 * Get Label Image
 */
template< class TInputImage, class TLabelImage, class TOutputImage >
const
typename LabelOverlayImageFilter<
  TInputImage, TLabelImage, TOutputImage >::LabelImageType *
LabelOverlayImageFilter< TInputImage, TLabelImage, TOutputImage >
::GetLabelImage() const
{
  return static_cast< LabelImageType * >(
           const_cast< DataObject * >( this->ProcessObject::GetInput(1) ) );
}

/**
 * Get number of colors in the LUT container
 */
template< class TInputImage, class TLabelImage, class TOutputImage >
unsigned int
LabelOverlayImageFilter< TInputImage, TLabelImage, TOutputImage >
::GetNumberOfColors() const
{
  return this->GetFunctor().GetNumberOfColors();
}

/**
 * Empty the color LUT container
 */
template< class TInputImage, class TLabelImage, class TOutputImage >
void
LabelOverlayImageFilter< TInputImage, TLabelImage, TOutputImage >
::ResetColors()
{
  this->GetFunctor().ResetColors();
}

/**
 * Add a color to the LUT container
 */
template< class TInputImage, class TLabelImage, class TOutputImage >
void
LabelOverlayImageFilter< TInputImage, TLabelImage, TOutputImage >
::AddColor(ComponentType r, ComponentType g, ComponentType b)
{
  this->GetFunctor().AddColor(r, g, b);
}

/**
 * Standard PrintSelf method
 */
template< class TInputImage, class TLabelImage, class TOutputImage >
void
LabelOverlayImageFilter< TInputImage, TLabelImage, TOutputImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Opacity: "
     << static_cast< typename NumericTraits< double >::PrintType >( m_Opacity )
     << std::endl
     << indent << "BackgroundValue: "
     << static_cast<
    typename NumericTraits< LabelPixelType >::PrintType >( m_BackgroundValue )
     << std::endl;
}
} // end namespace itk

#endif
