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
#ifndef itkRobustAutomaticThresholdCalculator_hxx
#define itkRobustAutomaticThresholdCalculator_hxx
#include "itkRobustAutomaticThresholdCalculator.h"

#include "itkImageRegionConstIteratorWithIndex.h"
#include "itkImageRegionConstIterator.h"


namespace itk
{
template< typename TInputImage, typename TGradientImage >
RobustAutomaticThresholdCalculator< TInputImage, TGradientImage >
::RobustAutomaticThresholdCalculator() :
  m_Valid( false ),
  m_Pow( 1 ),
  m_Output( NumericTraits< InputPixelType >::ZeroValue() )
{
}

template< typename TInputImage, typename TGradientImage >
void
RobustAutomaticThresholdCalculator< TInputImage, TGradientImage >
::Compute()
{
  if ( !m_Input || !m_Gradient )
    {
    itkExceptionMacro(<< "Input or gradient image(s) not set.");
    }

  ImageRegionConstIteratorWithIndex< InputImageType > iIt( m_Input,
                                                           m_Input->GetRequestedRegion() );
  iIt.GoToBegin();
  ImageRegionConstIteratorWithIndex< GradientImageType > gIt( m_Gradient,
                                                              m_Gradient->GetRequestedRegion() );
  gIt.GoToBegin();

  // Init the values
  double n = 0;
  double d = 0;

  while ( !iIt.IsAtEnd() )
    {
    double g = std::pow(static_cast< double >( gIt.Get() ), m_Pow);
    n += iIt.Get() * g;
    d += g;
    ++iIt;
    ++gIt;
    }

  m_Output = static_cast< InputPixelType >( n / d );
  m_Valid = true;
}

template< typename TInputImage, typename TGradientImage >
const typename RobustAutomaticThresholdCalculator< TInputImage, TGradientImage >::InputPixelType &
RobustAutomaticThresholdCalculator< TInputImage, TGradientImage >
::GetOutput() const
{
  if ( !m_Valid )
    {
    itkExceptionMacro(<< "GetOutput() invoked, but the output has not been computed. Call Compute() first.");
    }
  return m_Output;
}

template< typename TInputImage, typename TGradientImage >
void
RobustAutomaticThresholdCalculator< TInputImage, TGradientImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Valid: " << m_Valid << std::endl;
  os << indent << "Pow: " << m_Pow << std::endl;
  os << indent << "Output: "
    << static_cast< typename NumericTraits< InputPixelType >::PrintType >( m_Output ) << std::endl;

  itkPrintSelfObjectMacro( Input );
  itkPrintSelfObjectMacro( Gradient );
}
} // end namespace itk

#endif
