/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVotingBinaryImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkVotingBinaryImageFilter_txx
#define _itkVotingBinaryImageFilter_txx
#include "itkVotingBinaryImageFilter.h"

#include "itkConstNeighborhoodIterator.h"
#include "itkNeighborhoodInnerProduct.h"
#include "itkImageRegionIterator.h"
#include "itkNeighborhoodAlgorithm.h"
#include "itkZeroFluxNeumannBoundaryCondition.h"
#include "itkOffset.h"
#include "itkProgressReporter.h"

#include <vector>
#include <algorithm>

namespace itk
{

template <class TInputImage, class TOutputImage>
VotingBinaryImageFilter<TInputImage, TOutputImage>
::VotingBinaryImageFilter()
{
  m_Radius.Fill(1);
  m_ForegroundValue = NumericTraits<InputPixelType>::max();
  m_BackgroundValue = NumericTraits<InputPixelType>::Zero;
  m_BirthThreshold = 1;
  m_SurvivalThreshold = 1;
}

template <class TInputImage, class TOutputImage>
void 
VotingBinaryImageFilter<TInputImage, TOutputImage>
::GenerateInputRequestedRegion() throw (InvalidRequestedRegionError)
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
  inputRequestedRegion.PadByRadius( m_Radius );

  // crop the input requested region at the input's largest possible region
  if ( inputRequestedRegion.Crop(inputPtr->GetLargestPossibleRegion()) )
    {
    inputPtr->SetRequestedRegion( inputRequestedRegion );
    return;
    }
  else
    {
    // Couldn't crop the region (requested region is outside the largest
    // possible region).  Throw an exception.

    // store what we tried to request (prior to trying to crop)
    inputPtr->SetRequestedRegion( inputRequestedRegion );
    
    // build an exception
    InvalidRequestedRegionError e(__FILE__, __LINE__);
    OStringStream msg;
    msg << static_cast<const char *>(this->GetNameOfClass())
        << "::GenerateInputRequestedRegion()";
    e.SetLocation(msg.str().c_str());
    e.SetDescription("Requested region is (at least partially) outside the largest possible region.");
    e.SetDataObject(inputPtr);
    throw e;
    }
}


template< class TInputImage, class TOutputImage>
void
VotingBinaryImageFilter< TInputImage, TOutputImage>
::ThreadedGenerateData(const OutputImageRegionType& outputRegionForThread,
                       int threadId)
{
  
  ZeroFluxNeumannBoundaryCondition<InputImageType> nbc;

  ConstNeighborhoodIterator<InputImageType> bit;
  ImageRegionIterator<OutputImageType> it;
  
  // Allocate output
  typename OutputImageType::Pointer output = this->GetOutput();
  typename InputImageType::ConstPointer input  = this->GetInput();
  
  // Find the data-set boundary "faces"
  typename NeighborhoodAlgorithm::ImageBoundaryFacesCalculator<InputImageType>::FaceListType faceList;
  NeighborhoodAlgorithm::ImageBoundaryFacesCalculator<InputImageType> bC;
  faceList = bC(input, outputRegionForThread, m_Radius);

  typename NeighborhoodAlgorithm::ImageBoundaryFacesCalculator<InputImageType>::FaceListType::iterator fit;

  ProgressReporter progress(this, threadId, outputRegionForThread.GetNumberOfPixels());
  
  // Process each of the boundary faces.  These are N-d regions which border
  // the edge of the buffer.
  for (fit = faceList.begin(); fit != faceList.end(); ++fit)
    { 
    bit = ConstNeighborhoodIterator<InputImageType>(m_Radius, input, *fit);
    it  = ImageRegionIterator<OutputImageType>(output, *fit);
    bit.OverrideBoundaryCondition(&nbc);
    bit.GoToBegin();
    
    unsigned int neighborhoodSize = bit.Size();

    while ( ! bit.IsAtEnd() )
      {
      const InputPixelType inpixel = bit.GetCenterPixel();

      // count the pixels ON in the neighborhood
      unsigned int count = 0;
      for (unsigned int i = 0; i < neighborhoodSize; ++i)
        {
        InputPixelType value = bit.GetPixel(i);
        if( value == m_ForegroundValue )
          {
          count++;
          }
        }

      if( inpixel == m_BackgroundValue )
        {
        if( count >= m_BirthThreshold )
          {
          it.Set( static_cast<OutputPixelType>( m_ForegroundValue ) );
          }
        else 
          {
          it.Set( static_cast<OutputPixelType>( m_BackgroundValue ) );
          }
        } 
      else
        {
        if( inpixel == m_ForegroundValue )
          {
          if( count >= m_SurvivalThreshold )
            {
            it.Set( static_cast<OutputPixelType>( m_ForegroundValue ) );
            }
          else 
            {
            it.Set( static_cast<OutputPixelType>( m_BackgroundValue ) );
            }
          }
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
template <class TInputImage, class TOutput>
void
VotingBinaryImageFilter<TInputImage, TOutput>
::PrintSelf(
  std::ostream& os, 
  Indent indent) const
{
  Superclass::PrintSelf( os, indent );
  os << indent << "Radius: " << m_Radius << std::endl;
  os << indent << "Foreground value : "
     << static_cast<typename NumericTraits<InputPixelType>::PrintType>( m_ForegroundValue )<< std::endl;
  os << indent << "Background value : " 
     << static_cast<typename NumericTraits<InputPixelType>::PrintType>( m_BackgroundValue ) << std::endl;
  os << indent << "Birth Threshold   : " << m_BirthThreshold << std::endl;
  os << indent << "Survival Threshold   : " << m_SurvivalThreshold << std::endl;

}

} // end namespace itk

#endif
