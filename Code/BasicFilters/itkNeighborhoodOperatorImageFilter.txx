/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNeighborhoodOperatorImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkNeighborhoodOperatorImageFilter_txx
#define _itkNeighborhoodOperatorImageFilter_txx

#include "itkNeighborhoodAlgorithm.h"
#include "itkNeighborhoodInnerProduct.h"
#include "itkImageRegionIterator.h"
#include "itkConstNeighborhoodIterator.h"
#include "itkProgressReporter.h"

namespace itk
{

template <class TInputImage, class TOutputImage, class TOperatorValueType>
void 
NeighborhoodOperatorImageFilter<TInputImage,TOutputImage, TOperatorValueType>
::GenerateInputRequestedRegion() throw (InvalidRequestedRegionError)
{
  // call the superclass' implementation of this method. this should
  // copy the output requested region to the input requested region
  Superclass::GenerateInputRequestedRegion();
  
  // get pointers to the input and output
  InputImagePointer  inputPtr = 
    const_cast< TInputImage * >( this->GetInput() );
  
  if ( !inputPtr )
    {
    return;
    }

  // get a copy of the input requested region (should equal the output
  // requested region)
  typename TInputImage::RegionType inputRequestedRegion;
  inputRequestedRegion = inputPtr->GetRequestedRegion();

  // pad the input requested region by the operator radius
  inputRequestedRegion.PadByRadius( m_Operator.GetRadius() );

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


template< class TInputImage, class TOutputImage, class TOperatorValueType>
void
NeighborhoodOperatorImageFilter<TInputImage, TOutputImage, TOperatorValueType>
::ThreadedGenerateData(const OutputImageRegionType& outputRegionForThread,
                       int threadId)
{
  typedef NeighborhoodAlgorithm::ImageBoundaryFacesCalculator<InputImageType>
    BFC;
  typedef typename BFC::FaceListType FaceListType;

  NeighborhoodInnerProduct<InputImageType, OperatorValueType> smartInnerProduct;
  BFC faceCalculator;
  FaceListType faceList;

  // This filter can only operate on data types that are signed.
  if (!NumericTraits<OutputPixelType>::is_signed)
    {
    itkExceptionMacro( << "This filter can only create images of signed data type.");
    }
  // Allocate output
  OutputImageType *output = this->GetOutput();
  
  const InputImageType *input   = this->GetInput();
 
  // Break the input into a series of regions.  The first region is free
  // of boundary conditions, the rest with boundary conditions. Note,
  // we pass in the input image and the OUTPUT requested region. We are
  // only concerned with centering the neighborhood operator at the
  // pixels that correspond to output pixels.
  faceList = faceCalculator(input, outputRegionForThread,
                            m_Operator.GetRadius());

  typename FaceListType::iterator fit;
  ImageRegionIterator<OutputImageType> it;

  // support progress methods/callbacks
  ProgressReporter progress(this, threadId, outputRegionForThread.GetNumberOfPixels());
    
  // Process non-boundary region and each of the boundary faces.
  // These are N-d regions which border the edge of the buffer.
  ConstNeighborhoodIterator<InputImageType> bit;
  for (fit=faceList.begin(); fit != faceList.end(); ++fit)
    {
    bit =
      ConstNeighborhoodIterator<InputImageType>(m_Operator.GetRadius(),
                                                input, *fit);
    bit.OverrideBoundaryCondition( m_BoundsCondition );
    it = ImageRegionIterator<OutputImageType>(output, *fit);
    bit.GoToBegin();
    while ( ! bit.IsAtEnd() )
      {
      it.Value() = static_cast<typename OutputImageType::PixelType>(smartInnerProduct(bit, m_Operator));
      ++bit;
      ++it;
      progress.CompletedPixel();
      }
    }
}

} // end namespace itk

#endif
