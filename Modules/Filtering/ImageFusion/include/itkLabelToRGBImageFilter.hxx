/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef __itkLabelToRGBImageFilter_hxx
#define __itkLabelToRGBImageFilter_hxx

#include "itkLabelToRGBImageFilter.h"

/*
 *
 * This code was contributed in the Insight Journal paper:
 * "The watershed transform in ITK - discussion and new developments"
 * by Beare R., Lehmann G.
 * http://hdl.handle.net/1926/202
 * http://www.insight-journal.org/browse/publication/92
 *
 */

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
