/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBinaryMedianImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkBinaryMedianImageFilter_txx
#define _itkBinaryMedianImageFilter_txx
#include "itkBinaryMedianImageFilter.h"

#include "itkConstNeighborhoodIterator.h"
#include "itkConstSmartNeighborhoodIterator.h"
#include "itkNeighborhoodInnerProduct.h"
#include "itkImageRegionIterator.h"
#include "itkNeighborhoodAlgorithm.h"
#include "itkZeroFluxNeumannBoundaryCondition.h"
#include "itkOffset.h"

#include <vector>
#include <algorithm>

namespace itk
{

template <class TInputImage, class TOutputImage>
BinaryMedianImageFilter<TInputImage, TOutputImage>
::BinaryMedianImageFilter()
{
  m_Radius.Fill(1);
  m_ForegroundValue = NumericTraits<InputPixelType>::max();
  m_BackgroundValue = NumericTraits<InputPixelType>::Zero;
}

template <class TInputImage, class TOutputImage>
void 
BinaryMedianImageFilter<TInputImage, TOutputImage>
::GenerateInputRequestedRegion() throw (InvalidRequestedRegionError)
{
  // call the superclass' implementation of this method
  Superclass::GenerateInputRequestedRegion();
  
  // get pointers to the input and output
  InputImagePointer inputPtr = 
      const_cast< TInputImage * >( this->GetInput() );
  OutputImagePointer outputPtr = this->GetOutput();
  
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
    msg << (char *)this->GetNameOfClass()
        << "::GenerateInputRequestedRegion()";
    e.SetLocation(msg.str().c_str());
    e.SetDescription("Requested region is (at least partially) outside the largest possible region.");
    e.SetDataObject(inputPtr);
    throw e;
    }
}


template< class TInputImage, class TOutputImage>
void
BinaryMedianImageFilter< TInputImage, TOutputImage>
::ThreadedGenerateData(const OutputImageRegionType& outputRegionForThread,
                       int threadId)
{
  
  ZeroFluxNeumannBoundaryCondition<InputImageType> nbc;

  ConstNeighborhoodIterator<InputImageType> nit;
  ConstSmartNeighborhoodIterator<InputImageType> bit;
  ImageRegionIterator<OutputImageType> it;
  ConstNeighborhoodIterator<InputImageType>::ConstIterator inner_it;
  ConstNeighborhoodIterator<InputImageType>::ConstIterator innerEnd_it;
  
  // Allocate output
  typename OutputImageType::Pointer output = this->GetOutput();
  typename  InputImageType::ConstPointer input  = this->GetInput();
  
  // Find the data-set boundary "faces"
  typename NeighborhoodAlgorithm::ImageBoundaryFacesCalculator<InputImageType>::FaceListType faceList;
  NeighborhoodAlgorithm::ImageBoundaryFacesCalculator<InputImageType> bC;
  faceList = bC(input, outputRegionForThread, m_Radius);

  typename NeighborhoodAlgorithm::ImageBoundaryFacesCalculator<InputImageType>::FaceListType::iterator fit;
  fit = faceList.begin();

  // support progress methods/callbacks
  unsigned long ii = 0;
  unsigned long updateVisits = 0;
  unsigned long totalPixels = 0;
  if ( threadId == 0 )
    {
    totalPixels = outputRegionForThread.GetNumberOfPixels();
    updateVisits = totalPixels / 10;
    if( updateVisits < 1 ) updateVisits = 1;
    }

  // Process non-boundary face
  nit = ConstNeighborhoodIterator<InputImageType>(m_Radius, input, *fit);
  it  = ImageRegionIterator<OutputImageType>(output, *fit);

  nit.GoToBegin();
  it.GoToBegin();

  // All of our neighborhoods have an odd number of pixels, so there is
  // always a median index (if there where an even number of pixels
  // in the neighborhood we have to average the middle two values).
  unsigned int medianPosition = nit.Size() / 2;

  while( ! nit.IsAtEnd() )
    {
    if ( threadId == 0 && !(++ii % updateVisits ) )
      {
      this->UpdateProgress((float)ii / (float)totalPixels);
      }

    // count the pixels in the neighborhood
    unsigned int count = 0;
    innerEnd_it = nit.End();
    for (inner_it = nit.Begin(); inner_it != innerEnd_it; ++inner_it)
      {
      if( **inner_it == m_ForegroundValue )
        {
        count++;
        }
      }

    if( count > medianPosition )
      {
      it.Set( static_cast<OutputPixelType>( m_ForegroundValue ) );
      }
    else 
      {
      it.Set( static_cast<OutputPixelType>( m_BackgroundValue ) );
      }

    ++nit;
    ++it;
    }
  
  // Process each of the boundary faces.  These are N-d regions which border
  // the edge of the buffer.
  for (++fit; fit != faceList.end(); ++fit)
    { 
    bit = ConstSmartNeighborhoodIterator<InputImageType>(m_Radius, input, *fit);
    it  = ImageRegionIterator<OutputImageType>(output, *fit);
    bit.OverrideBoundaryCondition(&nbc);
    bit.GoToBegin();
    
    unsigned int neighborhoodSize = bit.Size();

    while ( ! bit.IsAtEnd() )
      {
      if ( threadId == 0 && !(++ii % updateVisits ) )
        {
        this->UpdateProgress((float)ii / (float)totalPixels);
        }

       // count the pixels in the neighborhood
      unsigned int count = 0;
      for (unsigned int i = 0; i < neighborhoodSize; ++i)
        {
        InputPixelType value = bit.GetPixel(i);
        if( value == m_ForegroundValue )
          {
          count++;
          }
        }

      if( count > medianPosition )
        {
        it.Set( static_cast<OutputPixelType>( m_ForegroundValue ) );
        }
      else 
        {
        it.Set( static_cast<OutputPixelType>( m_BackgroundValue ) );
        }
       
      ++bit;
      ++it;
      }
    }
}

/**
 * Standard "PrintSelf" method
 */
template <class TInputImage, class TOutput>
void
BinaryMedianImageFilter<TInputImage, TOutput>
::PrintSelf(
std::ostream& os, 
Indent indent) const
{
  Superclass::PrintSelf( os, indent );
  os << indent << "Radius: " << m_Radius << std::endl;
  os << indent << "Foreground value : " << m_ForegroundValue << std::endl;
  os << indent << "Background value : " << m_BackgroundValue << std::endl;

}

} // end namespace itk

#endif
