/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGradientMagnitudeImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkGradientMagnitudeImageFilter_txx
#define _itkGradientMagnitudeImageFilter_txx
#include "itkGradientMagnitudeImageFilter.h"

#include "itkConstNeighborhoodIterator.h"
#include "itkNeighborhoodInnerProduct.h"
#include "itkImageRegionIterator.h"
#include "itkDerivativeOperator.h"
#include "itkNeighborhoodAlgorithm.h"
#include "itkZeroFluxNeumannBoundaryCondition.h"
#include "itkOffset.h"
#include "itkProgressReporter.h"

namespace itk
{
 
template <typename TInputImage, typename TOutputImage>
void 
GradientMagnitudeImageFilter<TInputImage,TOutputImage>
::GenerateInputRequestedRegion() throw(InvalidRequestedRegionError)
{
  // call the superclass' implementation of this method
  Superclass::GenerateInputRequestedRegion();
  
  // get pointers to the input and output
  InputImagePointer  inputPtr = 
      const_cast< InputImageType * >( this->GetInput());
  OutputImagePointer outputPtr = this->GetOutput();
  
  if ( !inputPtr || !outputPtr )
    {
    return;
    }

  // Build an operator so that we can determine the kernel size
  DerivativeOperator<OutputPixelType, ImageDimension> oper;
   oper.SetDirection(0);
   oper.SetOrder(1);
   oper.CreateDirectional();
  unsigned long radius = oper.GetRadius()[0];
  
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


template< typename TInputImage, typename TOutputImage >
void
GradientMagnitudeImageFilter< TInputImage, TOutputImage >
::ThreadedGenerateData(const OutputImageRegionType& outputRegionForThread,
                       int threadId)
{
  unsigned int i;
  ZeroFluxNeumannBoundaryCondition<TInputImage> nbc;

  ConstNeighborhoodIterator<TInputImage> nit;
  ConstNeighborhoodIterator<TInputImage> bit;
  ImageRegionIterator<TOutputImage> it;

 
  NeighborhoodInnerProduct<TInputImage> SIP;

  // Allocate output
  typename OutputImageType::Pointer       output = this->GetOutput();
  typename  InputImageType::ConstPointer  input  = this->GetInput();

  // Set up operators
  DerivativeOperator<OutputPixelType, ImageDimension> op;
   op.SetDirection(0);
   op.SetOrder(1);
   op.CreateDirectional();

  // Calculate iterator radius
  Size<ImageDimension> radius;
  for (i = 0; i < ImageDimension; ++i) radius[i]  = op.GetRadius()[0];

  // Find the data-set boundary "faces"
  typename NeighborhoodAlgorithm::ImageBoundaryFacesCalculator<TInputImage>::
    FaceListType faceList;
  NeighborhoodAlgorithm::ImageBoundaryFacesCalculator<TInputImage> bC;
  faceList = bC(input, outputRegionForThread, radius);

  typename NeighborhoodAlgorithm::ImageBoundaryFacesCalculator<TInputImage>::
    FaceListType::iterator fit;
  fit = faceList.begin();

  // support progress methods/callbacks
  ProgressReporter progress(this, threadId, outputRegionForThread.GetNumberOfPixels());

  // Process non-boundary face
  nit = ConstNeighborhoodIterator<TInputImage>(radius, input, *fit);

  std::slice x_slice[ImageDimension];
  const unsigned long center = nit.Size() / 2;
  for (i = 0; i < ImageDimension; ++i)
    {
      x_slice[i] = std::slice( center - nit.GetStride(i) * radius[i],
                               op.GetSize()[0], nit.GetStride(i));
    }


  // Process each of the boundary faces.  These are N-d regions which border
  // the edge of the buffer.
  for (fit=faceList.begin(); fit != faceList.end(); ++fit)
    { 
    bit = ConstNeighborhoodIterator<InputImageType>(radius,
                                                         input, *fit);
    it = ImageRegionIterator<OutputImageType>(output, *fit);
    bit.OverrideBoundaryCondition(&nbc);
    bit.GoToBegin();

    while ( ! bit.IsAtEnd() )
      {
      RealType a = NumericTraits<RealType>::Zero;
      for (i = 0; i < ImageDimension; ++i)
        {
        const RealType g = SIP(x_slice[i], bit, op);
        a += g * g;
        }
      it.Value() = static_cast<OutputPixelType>(::sqrt(a));
      ++bit;
      ++it;
      progress.CompletedPixel();
      }

    }

}

} // end namespace itk

#endif
