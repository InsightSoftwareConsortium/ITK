/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNeighborhoodOperatorImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

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
  InputImagePointer  inputPtr = this->GetInput();
  
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
  InputImageType *input   = this->GetInput();
 
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
    //  bit.Print(std::cout);
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
