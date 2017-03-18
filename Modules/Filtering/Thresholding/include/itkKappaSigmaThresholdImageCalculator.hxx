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
#ifndef itkKappaSigmaThresholdImageCalculator_hxx
#define itkKappaSigmaThresholdImageCalculator_hxx
#include "itkKappaSigmaThresholdImageCalculator.h"

#include "itkImageRegionConstIteratorWithIndex.h"
#include "itkImageRegionConstIterator.h"

namespace itk
{
template< typename TInputImage, typename TMaskImage >
KappaSigmaThresholdImageCalculator< TInputImage, TMaskImage >
::KappaSigmaThresholdImageCalculator() :
  m_Valid( false ),
  m_MaskValue( NumericTraits<MaskPixelType>::max() ),
  m_SigmaFactor( 2 ),
  m_NumberOfIterations( 2 ),
  m_Output( NumericTraits<InputPixelType>::ZeroValue() )
{
}

template< typename TInputImage, typename TMaskImage >
void
KappaSigmaThresholdImageCalculator< TInputImage, TMaskImage >
::Compute()
{
  if ( !this->m_Image )
    {
    return;
    }

  // Initialize the values
  InputPixelType threshold = NumericTraits< InputPixelType >::max(); // use all
                                                                     // the
                                                                     // pixels
                                                                     // to begin

  for ( unsigned int iteration = 0; iteration < this->m_NumberOfIterations; iteration++ )
    {
    ImageRegionConstIteratorWithIndex< InputImageType > iIt( this->m_Image,
                                                             this->m_Image->GetRequestedRegion() );

    // Compute the mean
    iIt.GoToBegin();
    SizeValueType count = 0;
    double        mean = 0;
    while ( !iIt.IsAtEnd() )
      {
      if ( !this->m_Mask || this->m_Mask->GetPixel( iIt.GetIndex() ) == this->m_MaskValue )
        {
        const InputPixelType & v = iIt.Get();
        if ( v <= threshold )
          {
          mean += v;
          count++;
          }
        }
      ++iIt;
      }
    mean = mean / count;

    // Compute sigma
    iIt.GoToBegin();
    double sigma = 0;
    while ( !iIt.IsAtEnd() )
      {
      if ( !this->m_Mask || this->m_Mask->GetPixel( iIt.GetIndex() ) == this->m_MaskValue )
        {
        const InputPixelType & v = iIt.Get();
        if ( v <= threshold )
          {
          sigma += itk::Math::sqr(v - mean);
          }
        }
      ++iIt;
      }
    sigma = std::sqrt( sigma / ( count - 1 ) );

    // Compute the threshold for the next iteration
    InputPixelType newThreshold = static_cast< InputPixelType >( mean + this->m_SigmaFactor * sigma );
    if ( newThreshold == threshold )
      {
      // No need to continue - the threshold is the same and will produce the
      // same result
      break;
      }
    threshold = newThreshold;
    }

  this->m_Output = threshold;
  this->m_Valid = true;
}

template< typename TInputImage, typename TMaskImage >
const typename KappaSigmaThresholdImageCalculator< TInputImage, TMaskImage >::InputPixelType &
KappaSigmaThresholdImageCalculator< TInputImage, TMaskImage >
::GetOutput() const
{
  if ( !this->m_Valid )
    {
    itkExceptionMacro(<< "GetOutput() invoked, but the output have not been computed. Call Compute() first.");
    }
  return this->m_Output;
}

template< typename TInputImage, typename TMaskImage >
void
KappaSigmaThresholdImageCalculator< TInputImage, TMaskImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Valid: " << this->m_Valid << std::endl;
  os << indent << "MaskValue: " << this->m_MaskValue << std::endl;
  os << indent << "SigmaFactor: " << this->m_SigmaFactor << std::endl;
  os << indent << "NumberOfIterations: " << this->m_NumberOfIterations << std::endl;

  os << indent << "Output: " << static_cast< typename itk::NumericTraits<
    InputPixelType >::PrintType >( m_Output ) << std::endl;

  itkPrintSelfObjectMacro( Image );
  itkPrintSelfObjectMacro( Mask );
}
} // end namespace itk

#endif
