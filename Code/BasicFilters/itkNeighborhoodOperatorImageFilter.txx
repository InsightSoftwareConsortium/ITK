/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNeighborhoodOperatorImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
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
#include "itkConstSmartNeighborhoodIterator.h"

namespace itk
{

template <class TInputImage, class TOutputImage>
void 
NeighborhoodOperatorImageFilter<TInputImage,TOutputImage>
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
    std::ostrstream msg;
    msg << (char *)this->GetNameOfClass()
        << "::GenerateInputRequestedRegion()" << std::ends;
    e.SetLocation(msg.str());
    e.SetDescription("Requested region is (at least partially) outside the largest possible region.");
    e.SetDataObject(inputPtr);
    throw e;
    }
}


template< class TInputImage, class TOutputImage>
void
NeighborhoodOperatorImageFilter<TInputImage, TOutputImage>
::ThreadedGenerateData(const OutputImageRegionType& outputRegionForThread,
                       int threadId)
{
  typedef NeighborhoodAlgorithm::ImageBoundaryFacesCalculator<InputImageType>
    BFC;
  typedef typename BFC::FaceListType FaceListType;

  NeighborhoodInnerProduct<InputImageType>      innerProduct;
  SmartNeighborhoodInnerProduct<InputImageType> smartInnerProduct;
  BFC faceCalculator;
  FaceListType faceList;

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
  typename FaceListType::iterator fit = faceList.begin();

  // support progress methods/callbacks
  unsigned long i = 0;
  unsigned long updateVisits = 0;
  unsigned long totalPixels = 0;
  if ( threadId == 0 )
    {
    totalPixels = outputRegionForThread.GetNumberOfPixels();
    updateVisits = totalPixels / 10;
    if( updateVisits < 1 ) updateVisits = 1;
    }
  
  // Process non-boundary region
  ConstNeighborhoodIterator<InputImageType>
    nit(m_Operator.GetRadius(), input, *fit);
  ImageRegionIterator<OutputImageType> it(output, *fit);
  nit.GoToBegin();
  it.GoToBegin();
  
  while( ! nit.IsAtEnd() )
    {
    if ( threadId == 0 && !(i % updateVisits ) )
      {
      this->UpdateProgress((float)i++ / (float)totalPixels);
      }

    it.Value() = innerProduct(nit, m_Operator);
    ++nit;
    ++it;
    }

  // Process each of the boundary faces.  These are N-d regions which border
  // the edge of the buffer.
  ConstSmartNeighborhoodIterator<InputImageType> bit;
  for (++fit; fit != faceList.end(); ++fit)
    { 
    bit =
      ConstSmartNeighborhoodIterator<InputImageType>(m_Operator.GetRadius(),
                                                     input, *fit);
    it = ImageRegionIterator<OutputImageType>(output, *fit);
    bit.GoToBegin();
    while ( ! bit.IsAtEnd() )
      {
      if ( threadId == 0 && !(i % updateVisits ) )
        {
        this->UpdateProgress((float)i++ / (float)totalPixels);
        }
      
      it.Value() = smartInnerProduct(bit, m_Operator);
      ++bit;
      ++it;
      }
   }
}

} // end namespace itk

#endif
