/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkZeroCrossingImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkZeroCrossingImageFilter_txx
#define _itkZeroCrossingImageFilter_txx

#include "itkConstNeighborhoodIterator.h"
#include "itkConstSmartNeighborhoodIterator.h"
#include "itkZeroCrossingImageFilter.h"
#include "itkImageRegionIterator.h"
#include "itkNeighborhoodAlgorithm.h"
#include "itkZeroFluxNeumannBoundaryCondition.h"
#include "itkFixedArray.h"

namespace itk
{

template <class TInputImage, class TOutputImage>
void 
ZeroCrossingImageFilter<TInputImage,TOutputImage>
::GenerateInputRequestedRegion() throw(InvalidRequestedRegionError)
{
  // call the superclass' implementation of this method
  Superclass::GenerateInputRequestedRegion();
  
  // get pointers to the input and output
  InputImagePointer  inputPtr = 
    const_cast<TInputImage *>( this->GetInput() );
  OutputImagePointer outputPtr = this->GetOutput();
  
  if ( !inputPtr || !outputPtr )
    {
    return;
    }

  // Build an operator so that we can determine the kernel size
  unsigned long radius = 1;
  
  // get a copy of the input requested region (should equal the output
  // requested region)
  typename TInputImage::RegionType inputRequestedRegion;
  inputRequestedRegion = inputPtr->GetRequestedRegion();

  // pad the input requested region by the operator radius
  inputRequestedRegion.PadByRadius( radius );

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
    std::ostrstream msg;
    msg << (char *)this->GetNameOfClass()
        << "::GenerateInputRequestedRegion()" << std::ends;
    e.SetLocation(msg.str());
    e.SetDescription("Requested region is (at least partially) outside the largest possible region.");
    e.SetDataObject(inputPtr);
    throw e;
    }
}

template< class TInputImage, class TOutputImage >
void
ZeroCrossingImageFilter< TInputImage, TOutputImage >
::ThreadedGenerateData(const OutputImageRegionType& outputRegionForThread,
                       int threadId)
{
  unsigned int i;
  ZeroFluxNeumannBoundaryCondition<TInputImage> nbc;

  ConstNeighborhoodIterator<TInputImage> nit;
  ConstSmartNeighborhoodIterator<TInputImage> bit;
  ImageRegionIterator<TOutputImage> it;
  
  typename OutputImageType::Pointer      output = this->GetOutput();
  typename  InputImageType::ConstPointer input  = this->GetInput();
  
  // Calculate iterator radius
  Size<ImageDimension> radius;
  for (i = 0; i < ImageDimension; ++i)
    { radius[i]  = 1; }
  
  // Find the data-set boundary "faces"
  typename NeighborhoodAlgorithm::ImageBoundaryFacesCalculator<TInputImage>::
    FaceListType faceList;
  NeighborhoodAlgorithm::ImageBoundaryFacesCalculator<TInputImage> bC;
  faceList = bC(input, outputRegionForThread, radius);
  
  typename NeighborhoodAlgorithm::ImageBoundaryFacesCalculator<TInputImage>::
    FaceListType::iterator fit;
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
  nit = ConstNeighborhoodIterator<TInputImage>(radius, input, *fit);
  it  = ImageRegionIterator<TOutputImage>(output, *fit);
  
  nit.GoToBegin();
  it.GoToBegin();
  
  InputImagePixelType this_one, that, abs_this_one, abs_that;
  InputImagePixelType zero = NumericTraits<InputImagePixelType>::Zero;

  unsigned long center;
  FixedArray<long, 2 * ImageDimension> offset;

  //Set the offset of the neighbors to the center pixel.
  for ( i = 0 ; i < ImageDimension; i++)
    {
      offset[i] = - nit.GetStride(i);
      offset[i+ImageDimension] =  nit.GetStride(i);
    }

  // Now Process the non-boundary region.
  center = nit.Size()/2;
  while( ! nit.IsAtEnd() )
    {
      if ( threadId == 0 && !(ii % updateVisits ) )
        {
        this->UpdateProgress((float)ii++ / (float)totalPixels);
        }
      
      this_one = nit.GetPixel(center);
      
      for( i = 0; i< ImageDimension * 2; i++)
        {
          that = nit.GetPixel(center + offset[i]);
          it.Set(m_BackgroundValue);
          if( ((this_one < zero) && (that > zero))
              || ((this_one > zero) && (that < zero)) 
              || ((this_one == zero) && (that != zero))
              || ((this_one != zero) && (that == zero))  )
            {
              abs_this_one = vnl_math_abs(this_one);
              abs_that = vnl_math_abs(that);
              if(abs_this_one < abs_that)
                {
                  it.Set(m_ForegroundValue);
                  break;
                }
              else if(abs_this_one == abs_that && i >= ImageDimension)
                {
                  it.Set(m_ForegroundValue);
                  break;
                }

            }
        }
      ++nit;
      ++it;
    }
  
  // Process each of the boundary faces.  These are N-d regions which border
  // the edge of the buffer.
  for (++fit; fit != faceList.end(); ++fit)
    { 
      if ( threadId == 0 && !(ii % updateVisits ) )
        {
          this->UpdateProgress((float)ii++ / (float)totalPixels);
        }
      
      bit = ConstSmartNeighborhoodIterator<InputImageType>(radius,
                                                           input, *fit);
      it = ImageRegionIterator<OutputImageType>(output, *fit);
      bit.OverrideBoundaryCondition(&nbc);
      bit.GoToBegin();
      
      center = bit.Size()/2;
      while ( ! bit.IsAtEnd() )
        {
          this_one = bit.GetPixel(center);
          it.Set(m_BackgroundValue);
          for( i = 0; i< ImageDimension * 2; i++)
            {
              that = bit.GetPixel(center + offset[i]);
              if( ((this_one < zero) && (that > zero))
                  || ((this_one > zero) && (that < zero)) 
                  || ((this_one == zero) && (that != zero))
                  || ((this_one != zero) && (that == zero))  )
                {
                  abs_this_one =  vnl_math_abs(this_one);
                  abs_that = vnl_math_abs(that);
                  if(abs_this_one < abs_that)
                    {
                      it.Set(m_ForegroundValue);
                      break;
                    }
                  else if(abs_this_one == abs_that && i >= ImageDimension)
                    {
                      it.Set(m_ForegroundValue);
                      break;
                    }
                }
            }
          ++bit;
          ++it;
        }    
    }
}

template< class TInputImage, class TOutputImage >
void
ZeroCrossingImageFilter< TInputImage, TOutputImage >
::PrintSelf( std::ostream& os, Indent indent ) const
{
  Superclass::PrintSelf(os,indent);

  os << indent << "BackgroundValue: " << m_BackgroundValue << std::endl;
  os << indent << "ForegroundValue: " << m_ForegroundValue << std::endl;

}

}//end of itk namespace



#endif
