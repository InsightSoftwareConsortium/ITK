/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSimpleContourExtractorImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$
  
  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.
  
    This software is distributed WITHOUT ANY WARRANTY; without even 
    the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
    PURPOSE.  See the above copyright notices for more information.
    
=========================================================================*/
#ifndef _itkSimpleContourExtractorImageFilter_txx
#define _itkSimpleContourExtractorImageFilter_txx

#include "itkConstNeighborhoodIterator.h"
#include "itkNeighborhoodInnerProduct.h"
#include "itkImageRegionIterator.h"
#include "itkNeighborhoodAlgorithm.h"
#include "itkZeroFluxNeumannBoundaryCondition.h"
#include "itkOffset.h"
#include "itkProgressReporter.h"
#include "itkSimpleContourExtractorImageFilter.h"

namespace itk
{
  
template <class TInputImage, class TOutputImage>
SimpleContourExtractorImageFilter<TInputImage, TOutputImage>
::SimpleContourExtractorImageFilter()
{
  m_Radius.Fill(1);
  m_InputBackgroundValue  = NumericTraits<InputPixelType>::Zero;
  m_InputForegroundValue  = NumericTraits<InputPixelType>::max();
  m_OutputBackgroundValue  = NumericTraits<OutputPixelType>::Zero;
  m_OutputForegroundValue  = NumericTraits<OutputPixelType>::max();
}
  
template <class TInputImage, class TOutputImage>
void 
SimpleContourExtractorImageFilter<TInputImage, TOutputImage>
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
    e.SetLocation(ITK_LOCATION);
    e.SetDescription("Requested region is (at least partially) outside the largest possible region.");
    e.SetDataObject(inputPtr);
    throw e;
    }
}
  
template< class TInputImage, class TOutputImage>
void
SimpleContourExtractorImageFilter< TInputImage, TOutputImage>
::ThreadedGenerateData(const OutputImageRegionType& outputRegionForThread,
                       int threadId)
{
  unsigned int i;
  ZeroFluxNeumannBoundaryCondition<InputImageType> nbc;
    
  ConstNeighborhoodIterator<InputImageType> bit;
  ImageRegionIterator<OutputImageType> it;
    
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
    
        
  // Process each of the boundary faces.  These are N-d regions which border
  // the edge of the buffer.
  for (fit=faceList.begin(); fit != faceList.end(); ++fit)
    { 
    bit = ConstNeighborhoodIterator<InputImageType>(m_Radius,
                                                    input, *fit);
    unsigned int neighborhoodSize = bit.Size();
    it = ImageRegionIterator<OutputImageType>(output, *fit);

    bit.OverrideBoundaryCondition(&nbc);
    bit.GoToBegin();

    bool bIsOnContour;
      
    while ( ! bit.IsAtEnd() )
      {
        
      // first test
      // if current pixel is not on, let's continue
      if( bit.GetCenterPixel() == m_InputForegroundValue )
        {
          
        bIsOnContour = false;
          
        for (i = 0; i < neighborhoodSize; ++i)
          {
          // second test if at least one neighbour pixel is off
          // the center pixel belongs to contour
          if( bit.GetPixel(i) == m_InputBackgroundValue )
            {
            bIsOnContour = true;
            break;
            }
          }
          
        // set pixel center pixel value weither it is or not on contour
        if( bIsOnContour )
          {
          it.Set( m_OutputForegroundValue );  
          }
        else
          {
          it.Set( m_OutputBackgroundValue );  
          }          
        }
      else
        {
        it.Set( m_OutputBackgroundValue );
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
SimpleContourExtractorImageFilter<TInputImage, TOutput>
::PrintSelf(
  std::ostream& os, 
  Indent indent) const
{
  Superclass::PrintSelf( os, indent );
  os << indent << "Radius: " << m_Radius << std::endl;
  os << indent << "Input Foreground Value: " 
     << static_cast<typename NumericTraits<InputPixelType>::PrintType>(m_InputForegroundValue) << std::endl;
  os << indent << "Input Background Value: "
     << static_cast<typename NumericTraits<InputPixelType>::PrintType>(m_InputBackgroundValue) << std::endl;    
  os << indent << "Output Foreground Value: "
     << static_cast<typename NumericTraits<OutputPixelType>::PrintType>(m_OutputForegroundValue) << std::endl;
  os << indent << "Output Background Value: "
     << static_cast<typename NumericTraits<OutputPixelType>::PrintType>(m_OutputBackgroundValue) << std::endl;    
}
  
} // end namespace itk

#endif
