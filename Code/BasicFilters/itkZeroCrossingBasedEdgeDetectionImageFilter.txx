/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkZeroCrossingBasedEdgeDetectionImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkZeroCrossingBasedEdgeDetectionImageFilter_txx
#define _itkZeroCrossingBasedEdgeDetectionImageFilter_txx

#include "itkZeroCrossingBasedEdgeDetectionImageFilter.h"
#include "itkDiscreteGaussianImageFilter.h"
#include "itkLaplacianImageFilter.h"
#include "itkZeroCrossingImageFilter.h"

namespace itk
{
/*
template <class TInputImage, class TOutputImage>
void 
ZeroCrossingBasedEdgeDetectionImageFilter<TInputImage,TOutputImage>
::GenerateInputRequestedRegion() throw(InvalidRequestedRegionError)
{
  // call the superclass' implementation of this method
  Superclass::GenerateInputRequestedRegion();
  
  // get pointers to the input and output
  InputImagePointer  inputPtr = this->GetInput();
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
    OStringStream msg;
    msg << static_cast<const char *>(this->GetNameOfClass())
        << "::GenerateInputRequestedRegion()";
    e.SetLocation(msg.str().c_str());
    e.SetDescription("Requested region is (at least partially) outside the largest possible region.");
    e.SetDataObject(inputPtr);
    throw e;
    }
}

*/

template< class TInputImage, class TOutputImage >
void
ZeroCrossingBasedEdgeDetectionImageFilter< TInputImage, TOutputImage >
::GenerateData( )
{
  typename  InputImageType::ConstPointer input  = this->GetInput();
  
  // Create the filters that are needed
  typename DiscreteGaussianImageFilter<TInputImage, TOutputImage>::Pointer
    gaussianFilter
    = DiscreteGaussianImageFilter<TInputImage, TOutputImage>::New();
  typename LaplacianImageFilter<TInputImage, TOutputImage>::Pointer  laplacianFilter 
    =  LaplacianImageFilter<TInputImage, TOutputImage>::New();
  typename ZeroCrossingImageFilter<TInputImage, TOutputImage>:: Pointer
    zerocrossingFilter =
    ZeroCrossingImageFilter<TInputImage,TOutputImage>::New(); 
  
  //Construct the mini-pipeline
  
  // Apply the Gaussian filter
  gaussianFilter->SetVariance(m_Variance);
  gaussianFilter->SetMaximumError(m_MaximumError);
  gaussianFilter->SetInput(input);

  // Calculate the laplacian of the smoothed image
  laplacianFilter->SetInput(gaussianFilter->GetOutput());

  // Find the zero-crossings of the laplacian
  zerocrossingFilter->SetInput(laplacianFilter->GetOutput());
  zerocrossingFilter->SetBackgroundValue( m_BackgroundValue );
  zerocrossingFilter->SetForegroundValue( m_ForegroundValue );
  zerocrossingFilter->GraftOutput( this->GetOutput() );
  zerocrossingFilter->Update();

  // Graft the output of the mini-pipeline back onto the filter's output.
  // This action copies back the region ivars and meta-data 
  this->GraftOutput(zerocrossingFilter->GetOutput());
}

template< class TInputImage, class TOutputImage >
void
ZeroCrossingBasedEdgeDetectionImageFilter< TInputImage, TOutputImage >
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
  os << indent << "Variance: [";
  for (unsigned i = 0; i < ImageDimension; ++i)
    {
    os << m_Variance[i] << " ";
    }
  os << "]" << std::endl;

  os << indent << "MaximumError: [";
  for (unsigned i = 0; i < ImageDimension; ++i)
    {
    os << m_MaximumError[i] << " ";
    }
  os << "]" << std::endl;

  os << indent << "ForegroundValue: "
     << static_cast<typename NumericTraits<OutputImagePixelType>::PrintType>(m_ForegroundValue)
     << std::endl;
  os << indent << "BackgroundValue: "
     << static_cast<typename NumericTraits<OutputImagePixelType>::PrintType>(m_BackgroundValue)
     << std::endl;
}

}//end of itk namespace



#endif
