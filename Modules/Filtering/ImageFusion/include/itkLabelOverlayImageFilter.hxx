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
#ifndef itkLabelOverlayImageFilter_hxx
#define itkLabelOverlayImageFilter_hxx

#include "itkLabelOverlayImageFilter.h"

namespace itk
{
/**
 * Constructor method
 */
template< typename TInputImage, typename TLabelImage, typename TOutputImage >
LabelOverlayImageFilter< TInputImage, TLabelImage, TOutputImage >
::LabelOverlayImageFilter()
{
  m_Opacity = 0.5;
  m_BackgroundValue = NumericTraits< LabelPixelType >::ZeroValue();
}

template< typename TInputImage, typename TLabelImage, typename TOutputImage >
void
LabelOverlayImageFilter< TInputImage, TLabelImage, TOutputImage >
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


/**
 * Destructor method
 */
template< typename TInputImage, typename TLabelImage, typename TOutputImage >
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
template< typename TInputImage, typename TLabelImage, typename TOutputImage >
void
LabelOverlayImageFilter< TInputImage, TLabelImage, TOutputImage >
::SetLabelImage(const TLabelImage *input)
{
  this->SetInput2(input);
}

/**
 * Get Label Image
 */
template< typename TInputImage, typename TLabelImage, typename TOutputImage >
const
typename LabelOverlayImageFilter<
  TInputImage, TLabelImage, TOutputImage >::LabelImageType *
LabelOverlayImageFilter< TInputImage, TLabelImage, TOutputImage >
::GetLabelImage() const
{
  return itkDynamicCastInDebugMode< LabelImageType * >(
           const_cast< DataObject * >( this->ProcessObject::GetInput(1) ) );
}

/**
 * Get number of colors in the LUT container
 */
template< typename TInputImage, typename TLabelImage, typename TOutputImage >
unsigned int
LabelOverlayImageFilter< TInputImage, TLabelImage, TOutputImage >
::GetNumberOfColors() const
{
  return this->GetFunctor().GetNumberOfColors();
}

/**
 * Empty the color LUT container
 */
template< typename TInputImage, typename TLabelImage, typename TOutputImage >
void
LabelOverlayImageFilter< TInputImage, TLabelImage, TOutputImage >
::ResetColors()
{
  this->GetFunctor().ResetColors();
}

/**
 * Add a color to the LUT container
 */
template< typename TInputImage, typename TLabelImage, typename TOutputImage >
void
LabelOverlayImageFilter< TInputImage, TLabelImage, TOutputImage >
::AddColor(ComponentType r, ComponentType g, ComponentType b)
{
  this->GetFunctor().AddColor(r, g, b);
}

/**
 * Standard PrintSelf method
 */
template< typename TInputImage, typename TLabelImage, typename TOutputImage >
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
