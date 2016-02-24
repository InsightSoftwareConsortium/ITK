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
#ifndef itkLabelToRGBImageFilter_hxx
#define itkLabelToRGBImageFilter_hxx

#include "itkLabelToRGBImageFilter.h"

/*
 *
 * This code was contributed in the Insight Journal paper:
 * "The watershed transform in ITK - discussion and new developments"
 * by Beare R., Lehmann G.
 * https://hdl.handle.net/1926/202
 * http://www.insight-journal.org/browse/publication/92
 *
 */

namespace itk
{
/**
 *
 */
template< typename TLabelImage, typename TOutputImage >
LabelToRGBImageFilter< TLabelImage, TOutputImage >
::LabelToRGBImageFilter()
{
  m_BackgroundValue = NumericTraits< LabelPixelType >::ZeroValue();
  NumericTraits< OutputPixelType>::SetLength( m_BackgroundColor, 3);
  m_BackgroundColor.Fill(NumericTraits< OutputPixelValueType >::ZeroValue());
}

template< typename TLabelImage, typename TOutputImage >
unsigned int
LabelToRGBImageFilter< TLabelImage, TOutputImage >
::GetNumberOfColors() const
{
  return this->GetFunctor().GetNumberOfColors();
}

template< typename TLabelImage, typename TOutputImage >
void
LabelToRGBImageFilter< TLabelImage, TOutputImage >
::ResetColors()
{
  this->GetFunctor().ResetColors();
}

template< typename TLabelImage, typename TOutputImage >
void
LabelToRGBImageFilter< TLabelImage, TOutputImage >
::AddColor(ComponentType r, ComponentType g, ComponentType b)
{
  this->GetFunctor().AddColor(r, g, b);
}

template< typename TLabelImage, typename TOutputImage >
void
LabelToRGBImageFilter< TLabelImage, TOutputImage >
::GenerateOutputInformation()
{
  // this methods is overloaded so that if the output image is a
  // VectorImage then the correct number of components are set.

  Superclass::GenerateOutputInformation();
  OutputImageType* output = this->GetOutput();

  if ( !output )
    {
    return;
    }
  if ( output->GetNumberOfComponentsPerPixel() != 3 )
    {
    output->SetNumberOfComponentsPerPixel( 3 );
    }
}

template< typename TLabelImage, typename TOutputImage >
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
template< typename TLabelImage, typename TOutputImage >
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
