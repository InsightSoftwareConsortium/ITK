/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMaskNeighborhoodOperatorImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkMaskNeighborhoodOperatorImageFilter_txx
#define _itkMaskNeighborhoodOperatorImageFilter_txx

#include "itkMaskNeighborhoodOperatorImageFilter.h"
#include "itkNeighborhoodAlgorithm.h"
#include "itkImageRegionIterator.h"
#include "itkImageRegionConstIterator.h"
#include "itkConstNeighborhoodIterator.h"
#include "itkProgressReporter.h"

namespace itk
{


template< class TInputImage, class TMaskImage, class TOutputImage, class TOperatorValueType>
void
MaskNeighborhoodOperatorImageFilter<TInputImage, TMaskImage, TOutputImage, TOperatorValueType>
::SetMaskImage( const TMaskImage *mask )
{
  this->ProcessObject::SetNthInput( 1, const_cast<TMaskImage *>(mask) );
}

template< class TInputImage, class TMaskImage, class TOutputImage, class TOperatorValueType>
const TMaskImage *
MaskNeighborhoodOperatorImageFilter<TInputImage, TMaskImage, TOutputImage, TOperatorValueType>
::GetMaskImage() const
{
  return static_cast<MaskImageType*>(const_cast<DataObject *>(this->ProcessObject::GetInput(1)));
}
    

template <class TInputImage, class TMaskImage, class TOutputImage, class TOperatorValueType>
void 
MaskNeighborhoodOperatorImageFilter<TInputImage, TMaskImage, TOutputImage, TOperatorValueType>
::GenerateInputRequestedRegion() throw (InvalidRequestedRegionError)
{
  // call the superclass' implementation of this method
  Superclass::GenerateInputRequestedRegion();

  // Superclass handled the input image, now we just need to handle
  // the mask image is any.
  InputImagePointer  inputPtr = 
    const_cast< TInputImage * >( this->GetInput() );
  MaskImagePointer  maskPtr = 
    const_cast< TMaskImage * >( this->GetMaskImage() );
  
  if ( !inputPtr || !maskPtr )
    {
    return;
    }

  // get a copy of the input requested region which was set up by the
  // superclass (NeighborhoodOperatorImageFilter)
  typename TInputImage::RegionType inputRequestedRegion;
  inputRequestedRegion = inputPtr->GetRequestedRegion();

  // set the mask requested region to match the input requested region
  if ( maskPtr->GetLargestPossibleRegion().IsInside( inputRequestedRegion ) )
    {
    maskPtr->SetRequestedRegion( inputRequestedRegion );
    }
  else
    {
    // Couldn't make the masked region match the input region.
    // Throw an exception.

    // store what we tried to request
    maskPtr->SetRequestedRegion( inputRequestedRegion );
    
    // build an exception
    InvalidRequestedRegionError e(__FILE__, __LINE__);
    OStringStream msg;
    msg << static_cast<const char *>(this->GetNameOfClass())
        << "::GenerateInputRequestedRegion()";
    e.SetLocation(msg.str().c_str());
    e.SetDescription("Requested region is (at least partially) outside the largest possible region of the mask image.");
    e.SetDataObject(maskPtr);
    throw e;
    }
}


template< class TInputImage, class TMaskImage, class TOutputImage, class TOperatorValueType>
void
MaskNeighborhoodOperatorImageFilter<TInputImage, TMaskImage, TOutputImage, TOperatorValueType>
::ThreadedGenerateData(const OutputImageRegionType& outputRegionForThread,
                       int threadId)
{
  // get output/inputs
  OutputImageType *output = this->GetOutput();
  const InputImageType *input = this->GetInput();
  const MaskImageType *mask = this->GetMaskImage();

  // If mask is not specified, called the superclass...
  if (!mask)
    {
    Superclass::ThreadedGenerateData(outputRegionForThread, threadId);
    return;
    }
  
  // Define the inner product algorithm
  NeighborhoodInnerProduct<InputImageType, OperatorValueType> smartInnerProduct;
  // Break the input into a series of regions.  The first region is free
  // of boundary conditions, the rest with boundary conditions. Note,
  // we pass in the input image and the OUTPUT requested region. We are
  // only concerned with centering the neighborhood operator at the
  // pixels that correspond to output pixels.
  typedef NeighborhoodAlgorithm::ImageBoundaryFacesCalculator<InputImageType>
    BFC;
  typedef typename BFC::FaceListType FaceListType;
  BFC faceCalculator;
  FaceListType faceList;
 
  faceList = faceCalculator(input, outputRegionForThread,
                            this->GetOperator().GetRadius());

  // support progress methods/callbacks
  ProgressReporter progress(this, threadId,
                            outputRegionForThread.GetNumberOfPixels());
    
  // Get the operator
  OutputNeighborhoodType noperator = this->GetOperator();
  
  // Process non-boundary region and each of the boundary faces.
  // These are N-d regions which border the edge of the buffer.
  ConstNeighborhoodIterator<InputImageType> bit;
  typename FaceListType::iterator fit;
  ImageRegionIterator<OutputImageType> it;
  ImageRegionConstIterator<MaskImageType> mit;

  for (fit=faceList.begin(); fit != faceList.end(); ++fit)
    {
    bit =
      ConstNeighborhoodIterator<InputImageType>(noperator.GetRadius(),
                                                input, *fit);
    bit.OverrideBoundaryCondition(this->GetBoundaryCondition());
    bit.GoToBegin();

    it = ImageRegionIterator<OutputImageType>(output, *fit);
    mit = ImageRegionConstIterator<MaskImageType>(mask, *fit);
    while ( ! bit.IsAtEnd() )
      {
      if (mit.Get())
        {
        // Compute the inner product at this pixel
        it.Value() = static_cast<typename OutputImageType::PixelType>(smartInnerProduct(bit, noperator));
        }
      else
        {
        // Use the default value or the input value
        it.Value() = m_UseDefaultValue ? m_DefaultValue : bit.GetCenterPixel();
        }
      ++bit;
      ++it;
      ++mit;
      progress.CompletedPixel();
      }
    }
}

template< class TInputImage, class TMaskImage, class TOutputImage, class TOperatorValueType>
void
MaskNeighborhoodOperatorImageFilter<TInputImage, TMaskImage, TOutputImage, TOperatorValueType>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
  os << indent << "Default value : "
     << static_cast<typename NumericTraits<OutputPixelType>::PrintType>( m_DefaultValue )<< std::endl;
  os << indent << "UseDefaultValue : " << m_UseDefaultValue << std::endl;
}

} // end namespace itk

#endif


