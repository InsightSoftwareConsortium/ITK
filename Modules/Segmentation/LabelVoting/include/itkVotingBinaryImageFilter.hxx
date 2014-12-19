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
#ifndef itkVotingBinaryImageFilter_hxx
#define itkVotingBinaryImageFilter_hxx
#include "itkVotingBinaryImageFilter.h"

#include "itkConstNeighborhoodIterator.h"
#include "itkNeighborhoodInnerProduct.h"
#include "itkImageRegionIterator.h"
#include "itkNeighborhoodAlgorithm.h"
#include "itkProgressReporter.h"

#include <vector>
#include <algorithm>

namespace itk
{
template< typename TInputImage, typename TOutputImage >
VotingBinaryImageFilter< TInputImage, TOutputImage >
::VotingBinaryImageFilter()
{
  m_Radius.Fill(1);
  m_ForegroundValue = NumericTraits< InputPixelType >::max();
  m_BackgroundValue = NumericTraits< InputPixelType >::ZeroValue();
  m_BirthThreshold = 1;
  m_SurvivalThreshold = 1;
}

template< typename TInputImage, typename TOutputImage >
void
VotingBinaryImageFilter< TInputImage, TOutputImage >
::GenerateInputRequestedRegion()
{
  // call the superclass' implementation of this method
  Superclass::GenerateInputRequestedRegion();

  // get pointers to the input and output
  typename Superclass::InputImagePointer inputPtr =
    const_cast< TInputImage * >( this->GetInput() );
  typename Superclass::OutputImagePointer outputPtr = this->GetOutput();

  if ( !inputPtr || !outputPtr )
    {
    return;
    }

  // get a copy of the input requested region (should equal the output
  // requested region)
  typename TInputImage::RegionType inputRequestedRegion;
  inputRequestedRegion = inputPtr->GetRequestedRegion();

  // pad the input requested region by the operator radius
  inputRequestedRegion.PadByRadius(m_Radius);

  // crop the input requested region at the input's largest possible region
  if ( inputRequestedRegion.Crop( inputPtr->GetLargestPossibleRegion() ) )
    {
    inputPtr->SetRequestedRegion(inputRequestedRegion);
    return;
    }
  else
    {
    // Couldn't crop the region (requested region is outside the largest
    // possible region).  Throw an exception.

    // store what we tried to request (prior to trying to crop)
    inputPtr->SetRequestedRegion(inputRequestedRegion);

    // build an exception
    InvalidRequestedRegionError e(__FILE__, __LINE__);
    e.SetLocation(ITK_LOCATION);
    e.SetDescription("Requested region is (at least partially) outside the largest possible region.");
    e.SetDataObject(inputPtr);
    throw e;
    }
}

template< typename TInputImage, typename TOutputImage >
void
VotingBinaryImageFilter< TInputImage, TOutputImage >
::ThreadedGenerateData(const OutputImageRegionType & outputRegionForThread,
                       ThreadIdType threadId)
{
  ZeroFluxNeumannBoundaryCondition< InputImageType > nbc;

  ConstNeighborhoodIterator< InputImageType > bit;
  ImageRegionIterator< OutputImageType >      it;

  // Allocate output
  typename OutputImageType::Pointer output = this->GetOutput();
  typename InputImageType::ConstPointer input  = this->GetInput();

  // Find the data-set boundary "faces"
  typename NeighborhoodAlgorithm::ImageBoundaryFacesCalculator< InputImageType >::FaceListType faceList;
  NeighborhoodAlgorithm::ImageBoundaryFacesCalculator< InputImageType > bC;
  faceList = bC(input, outputRegionForThread, m_Radius);

  typename NeighborhoodAlgorithm::ImageBoundaryFacesCalculator< InputImageType >::FaceListType::iterator fit;

  ProgressReporter progress( this, threadId, outputRegionForThread.GetNumberOfPixels() );

  // Process each of the boundary faces.  These are N-d regions which border
  // the edge of the buffer.
  for ( fit = faceList.begin(); fit != faceList.end(); ++fit )
    {
    bit = ConstNeighborhoodIterator< InputImageType >(m_Radius, input, *fit);
    it  = ImageRegionIterator< OutputImageType >(output, *fit);
    bit.OverrideBoundaryCondition(&nbc);
    bit.GoToBegin();

    unsigned int neighborhoodSize = bit.Size();

    while ( !bit.IsAtEnd() )
      {
      const InputPixelType inpixel = bit.GetCenterPixel();

      // count the pixels ON in the neighborhood
      unsigned int count = 0;
      for ( unsigned int i = 0; i < neighborhoodSize; ++i )
        {
        InputPixelType value = bit.GetPixel(i);
        if ( value == m_ForegroundValue )
          {
          count++;
          }
        }

      // Unless the birth or survival rate is meet the pixel will be
      // the same value
      it.Set( static_cast< OutputPixelType >( inpixel ) );

      if ( inpixel == m_BackgroundValue && count >= m_BirthThreshold )
        {
        it.Set( static_cast< OutputPixelType >( m_ForegroundValue ) );
        }
      else if ( inpixel == m_ForegroundValue && count < m_SurvivalThreshold )
        {
        it.Set( static_cast< OutputPixelType >( m_BackgroundValue ) );
        }


      ++bit;
      ++it;
      progress.CompletedPixel();
      }
    }
}

/**
 * Standard "PrintSelf" method
 */
template< typename TInputImage, typename TOutput >
void
VotingBinaryImageFilter< TInputImage, TOutput >
::PrintSelf(
  std::ostream & os,
  Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Radius: " << m_Radius << std::endl;
  os << indent << "Foreground value : "
     << static_cast< typename NumericTraits< InputPixelType >::PrintType >( m_ForegroundValue ) << std::endl;
  os << indent << "Background value : "
     << static_cast< typename NumericTraits< InputPixelType >::PrintType >( m_BackgroundValue ) << std::endl;
  os << indent << "Birth Threshold   : " << m_BirthThreshold << std::endl;
  os << indent << "Survival Threshold   : " << m_SurvivalThreshold << std::endl;
}
} // end namespace itk

#endif
