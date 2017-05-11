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
#ifndef itkApproximateSignedDistanceMapImageFilter_hxx
#define itkApproximateSignedDistanceMapImageFilter_hxx

#include "itkApproximateSignedDistanceMapImageFilter.h"

#include "itkNumericTraits.h"
#include "itkImageScanlineIterator.h"
#include "itkProgressAccumulator.h"

namespace itk
{

template< typename TInputImage, typename TOutputImage >
ApproximateSignedDistanceMapImageFilter< TInputImage, TOutputImage >
::ApproximateSignedDistanceMapImageFilter():
  m_IsoContourFilter( IsoContourType::New() ),
  m_ChamferFilter( ChamferType::New() ),
  m_InsideValue( NumericTraits< InputPixelType >::min() ),
  m_OutsideValue( NumericTraits< InputPixelType >::max() )
{
}

template< typename TInputImage, typename TOutputImage >
void
ApproximateSignedDistanceMapImageFilter< TInputImage, TOutputImage >
::GenerateData()
{
  ThreadIdType numberOfThreads = this->GetNumberOfThreads();

  OutputImagePointer output = this->GetOutput();

  typedef typename OutputImageType::RegionType OutputRegionType;
  OutputRegionType oRegion = output->GetRequestedRegion();

  // Calculate the largest possible distance in the output image.
  // this maximum is the distance from one corner of the image to the other.
  OutputSizeType      outputSize = oRegion.GetSize();
  OutputSizeValueType maximumDistance = 0;

  for( unsigned int i = 0; i < InputImageDimension; i++ )
    {
    maximumDistance += outputSize[i] * outputSize[i];
    }

  // Cast to double and back because there's no sqrt defined for unsigned long,
  // which is the general SizeValueType.
  maximumDistance =
    static_cast< OutputSizeValueType >( std::sqrt( static_cast< double >( maximumDistance ) ) );

  // Allocate the output
  this->AllocateOutputs();

  // Create a process accumulator for tracking the progress of this minipipeline
  ProgressAccumulator::Pointer progress = ProgressAccumulator::New();
  progress->SetMiniPipelineFilter(this);
  progress->RegisterInternalFilter(m_IsoContourFilter, 0.5f);
  progress->RegisterInternalFilter(m_ChamferFilter, 0.5f);

  // Set up the isocontour filter
  m_IsoContourFilter->SetInput( this->GetInput() );
  m_IsoContourFilter->SetFarValue(maximumDistance + 1);
  m_IsoContourFilter->SetNumberOfThreads( numberOfThreads );
  typename IsoContourType::PixelRealType levelSetValue =
    (static_cast<typename IsoContourType::PixelRealType>( m_InsideValue )
    + static_cast<typename IsoContourType::PixelRealType>(m_OutsideValue) ) / 2.0;
  m_IsoContourFilter->SetLevelSetValue(levelSetValue);

  // Set up the chamfer filter
  m_ChamferFilter->SetInput( m_IsoContourFilter->GetOutput() );
  m_ChamferFilter->SetMaximumDistance(maximumDistance);
  m_ChamferFilter->SetNumberOfThreads( numberOfThreads );

  // Graft our output to the chamfer filter to force the proper regions
  // to be generated
  m_ChamferFilter->GraftOutput( output );

  // Create the distance map
  m_ChamferFilter->Update();

  // Graft the output of the chamfer filter back onto this filter's
  // output. this is needed to get the appropriate regions passed
  // back.
  this->GraftOutput( m_ChamferFilter->GetOutput() );

  // Recall that we set the isocontour value to halfway between the inside and
  // outside value. The above filters assume that regions "inside" objects are
  // those with values *less* than the isocontour. This assumption is violated
  // if the "inside" intensity value is greater than the "outside" value.
  // (E.g. in the case that we're computing the distance from a mask where the
  // background is zero and the objects are colored 255.)
  // In this case, the distance will be calculated negative, so we need to
  // flip the sign of the output image.
  if( m_InsideValue > m_OutsideValue )
    {
    ImageScanlineIterator< OutputImageType > ot( output, oRegion );
    while( !ot.IsAtEnd() )
      {
      while( !ot.IsAtEndOfLine() )
        {
        ot.Set(ot.Get() * -1);
        ++ot;
        }
      ot.NextLine();
      }
    }
}

template< typename TInputImage, typename TOutputImage >
void
ApproximateSignedDistanceMapImageFilter< TInputImage, TOutputImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Inside intensity value: " << m_InsideValue << std::endl;
  os << indent << "Outside intensity value: " << m_OutsideValue << std::endl;
  os << indent << "IsoContourDistanceImageFilter (used internally): "
     << m_IsoContourFilter << std::endl;
  os << indent << "FastChamferDistanceImageFilter (used internally): "
     << m_ChamferFilter << std::endl;
}
} // end of namespace itk

#endif
