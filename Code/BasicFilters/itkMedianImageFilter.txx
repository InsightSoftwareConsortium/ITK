/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMedianImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkMedianImageFilter_txx
#define _itkMedianImageFilter_txx
#include "itkMedianImageFilter.h"

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
MedianImageFilter<TInputImage, TOutputImage>
::MedianImageFilter()
{
  m_Radius.Fill(1);
}

template <class TInputImage, class TOutputImage>
void 
MedianImageFilter<TInputImage, TOutputImage>
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
MedianImageFilter< TInputImage, TOutputImage>
::ThreadedGenerateData(const OutputImageRegionType& outputRegionForThread,
                       int threadId)
{
  unsigned int i;
  ZeroFluxNeumannBoundaryCondition<InputImageType> nbc;

  ConstNeighborhoodIterator<InputImageType> bit;
  ImageRegionIterator<OutputImageType> it;
  
  std::vector<InputPixelType> pixels;
  typename std::vector<InputPixelType>::iterator medianIterator;
  
  // Allocate output
  typename OutputImageType::Pointer output = this->GetOutput();
  typename  InputImageType::ConstPointer input  = this->GetInput();
  
  // Find the data-set boundary "faces"
  typename NeighborhoodAlgorithm::ImageBoundaryFacesCalculator<InputImageType>::FaceListType faceList;
  NeighborhoodAlgorithm::ImageBoundaryFacesCalculator<InputImageType> bC;
  faceList = bC(input, outputRegionForThread, m_Radius);

  typename NeighborhoodAlgorithm::ImageBoundaryFacesCalculator<InputImageType>::FaceListType::iterator fit;

  // support progress methods/callbacks
  ProgressReporter progress(this, threadId, outputRegionForThread.GetNumberOfPixels());
  
  try
    {
    // All of our neighborhoods have an odd number of pixels, so there is
    // always a median index (if there where an even number of pixels
    // in the neighborhood we have to average the middle two values).
    
    // Process each of the boundary faces.  These are N-d regions which border
    // the edge of the buffer.
    for (fit=faceList.begin(); fit != faceList.end(); ++fit)
      { 
      bit = ConstNeighborhoodIterator<InputImageType>(m_Radius,
                                                           input, *fit);

      unsigned int medianPosition = bit.Size() / 2;
      unsigned int neighborhoodSize = bit.Size();

      it = ImageRegionIterator<OutputImageType>(output, *fit);
      bit.OverrideBoundaryCondition(&nbc);
      bit.GoToBegin();
      
      while ( ! bit.IsAtEnd() )
        {
        // collect all the pixels in the neighborhood, note that we use
        // GetPixel on the NeighborhoodIterator to honor the boundary conditions
        pixels.clear();
        for (i = 0; i < neighborhoodSize; ++i)
          {
          pixels.push_back( bit.GetPixel(i) );
          }
        
        // get the median value
        medianIterator = pixels.begin() + medianPosition;
        std::nth_element(pixels.begin(), medianIterator, pixels.end());
        it.Set( *medianIterator );
        
        ++bit;
        ++it;
        progress.CompletedPixel();
        }
      }

    }
  catch( ProcessAborted  & except )
    {
    // User aborted filter excecution Here we catch an exception thrown by the
    // progress reporter and rethrow it with the correct line number and file
    // name. We also invoke AbortEvent in case some observer was interested on
    // it.

    this->InvokeEvent( AbortEvent() );
    this->ResetPipeline();
    throw ProcessAborted(__FILE__,__LINE__);
    }

}


/**
 * Standard "PrintSelf" method
 */
template <class TInputImage, class TOutput>
void
MedianImageFilter<TInputImage, TOutput>
::PrintSelf(
std::ostream& os, 
Indent indent) const
{
  Superclass::PrintSelf( os, indent );
  os << indent << "Radius: " << m_Radius << std::endl;

}

} // end namespace itk

#endif
