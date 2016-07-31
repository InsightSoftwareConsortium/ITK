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
#ifndef itkSimpleContourExtractorImageFilter_hxx
#define itkSimpleContourExtractorImageFilter_hxx

#include "itkConstNeighborhoodIterator.h"
#include "itkNeighborhoodInnerProduct.h"
#include "itkImageRegionIterator.h"
#include "itkNeighborhoodAlgorithm.h"
#include "itkOffset.h"
#include "itkProgressReporter.h"
#include "itkSimpleContourExtractorImageFilter.h"

namespace itk
{
template< typename TInputImage, typename TOutputImage >
SimpleContourExtractorImageFilter< TInputImage, TOutputImage >
::SimpleContourExtractorImageFilter() :
  m_InputForegroundValue( NumericTraits< InputPixelType >::max() ),
  m_InputBackgroundValue( NumericTraits< InputPixelType >::ZeroValue() ),
  m_OutputForegroundValue( NumericTraits< OutputPixelType >::max() ),
  m_OutputBackgroundValue( NumericTraits< OutputPixelType >::ZeroValue() )
{
}

template< typename TInputImage, typename TOutputImage >
void
SimpleContourExtractorImageFilter< TInputImage, TOutputImage >
::ThreadedGenerateData(const OutputImageRegionType & outputRegionForThread,
                       ThreadIdType threadId)
{
  unsigned int i;

  ZeroFluxNeumannBoundaryCondition< InputImageType > nbc;

  ConstNeighborhoodIterator< InputImageType > bit;
  ImageRegionIterator< OutputImageType >      it;

  // Allocate output
  typename OutputImageType::Pointer output = this->GetOutput();
  typename  InputImageType::ConstPointer input  = this->GetInput();

  // Find the data-set boundary "faces"
  typename NeighborhoodAlgorithm::ImageBoundaryFacesCalculator< InputImageType >::FaceListType faceList;
  NeighborhoodAlgorithm::ImageBoundaryFacesCalculator< InputImageType > bC;
  faceList = bC( input, outputRegionForThread, this->GetRadius() );

  typename NeighborhoodAlgorithm::ImageBoundaryFacesCalculator< InputImageType >::FaceListType::iterator fit;

  // support progress methods/callbacks
  ProgressReporter progress( this, threadId, outputRegionForThread.GetNumberOfPixels() );

  // Process each of the boundary faces.  These are N-d regions which border
  // the edge of the buffer.
  for ( fit = faceList.begin(); fit != faceList.end(); ++fit )
    {
    bit = ConstNeighborhoodIterator< InputImageType >(this->GetRadius(),
                                                      input, *fit);
    unsigned int neighborhoodSize = bit.Size();
    it = ImageRegionIterator< OutputImageType >(output, *fit);

    bit.OverrideBoundaryCondition(&nbc);
    bit.GoToBegin();

    bool bIsOnContour;

    while ( !bit.IsAtEnd() )
      {
      // first test
      // if current pixel is not on, let's continue
      if ( bit.GetCenterPixel() == m_InputForegroundValue )
        {
        bIsOnContour = false;

        for ( i = 0; i < neighborhoodSize; ++i )
          {
          // second test if at least one neighbour pixel is off
          // the center pixel belongs to contour
          if ( bit.GetPixel(i) == m_InputBackgroundValue )
            {
            bIsOnContour = true;
            break;
            }
          }

        // set pixel center pixel value weither it is or not on contour
        if ( bIsOnContour )
          {
          it.Set(m_OutputForegroundValue);
          }
        else
          {
          it.Set(m_OutputBackgroundValue);
          }
        }
      else
        {
        it.Set(m_OutputBackgroundValue);
        }

      ++bit;
      ++it;
      progress.CompletedPixel();
      }
    }
}

template< typename TInputImage, typename TOutput >
void
SimpleContourExtractorImageFilter< TInputImage, TOutput >
::PrintSelf(
  std::ostream & os,
  Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Input Foreground Value: "
     << static_cast< typename NumericTraits< InputPixelType >::PrintType >( m_InputForegroundValue ) << std::endl;
  os << indent << "Input Background Value: "
     << static_cast< typename NumericTraits< InputPixelType >::PrintType >( m_InputBackgroundValue ) << std::endl;
  os << indent << "Output Foreground Value: "
     << static_cast< typename NumericTraits< OutputPixelType >::PrintType >( m_OutputForegroundValue ) << std::endl;
  os << indent << "Output Background Value: "
     << static_cast< typename NumericTraits< OutputPixelType >::PrintType >( m_OutputBackgroundValue ) << std::endl;
}
} // end namespace itk

#endif
