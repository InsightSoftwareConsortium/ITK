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
::GenerateInputRequestedRegion()
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
  
  // we need to compute the input requested region (size and start index)
  int i;
  const typename TOutputImage::SizeType& outputRequestedRegionSize
    = outputPtr->GetRequestedRegion().GetSize();
  const typename TOutputImage::IndexType& outputRequestedRegionStartIndex
    = outputPtr->GetRequestedRegion().GetIndex();
  
  typename TInputImage::SizeType  inputRequestedRegionSize;
  typename TInputImage::IndexType inputRequestedRegionStartIndex;

  const typename TInputImage::SizeType  inputLargestPossibleRegionSize
    = inputPtr->GetLargestPossibleRegion().GetSize();
  const typename TInputImage::IndexType inputLargestPossibleRegionStartIndex
    = inputPtr->GetLargestPossibleRegion().GetIndex();

  long crop=0;
  for (i = 0; i < TInputImage::ImageDimension; i++)
    {
    inputRequestedRegionSize[i]
      = outputRequestedRegionSize[i] + 2 * m_Operator.GetRadius(i);
    inputRequestedRegionStartIndex[i]
      = outputRequestedRegionStartIndex[i] - m_Operator.GetRadius(i);

    // crop the requested region to the largest possible region
    //

    // first check the start index
    if (inputRequestedRegionStartIndex[i]
        < inputLargestPossibleRegionStartIndex[i])
      {
      // how much do we need to adjust
      crop = inputLargestPossibleRegionStartIndex[i]
        - inputRequestedRegionStartIndex[i];

      // adjust the start index and the size of the requested region
      inputRequestedRegionStartIndex[i] += crop;
      inputRequestedRegionSize[i] -= crop;
      }
    // now check the final size
    if (inputRequestedRegionStartIndex[i] + inputRequestedRegionSize[i] 
        > inputLargestPossibleRegionStartIndex[i]
        + inputLargestPossibleRegionSize[i])
      {
      // how much do we need to adjust
      crop = inputRequestedRegionStartIndex[i] + inputRequestedRegionSize[i] 
        - inputLargestPossibleRegionStartIndex[i]
        - inputLargestPossibleRegionSize[i];

      // adjust the size
      inputRequestedRegionSize[i] -= crop;
      }
    }
  
  typename TInputImage::RegionType inputRequestedRegion;
  inputRequestedRegion.SetSize( inputRequestedRegionSize );
  inputRequestedRegion.SetIndex( inputRequestedRegionStartIndex );
  
  inputPtr->SetRequestedRegion( inputRequestedRegion );
}


template< class TInputImage, class TOutputImage>
void
NeighborhoodOperatorImageFilter<TInputImage, TOutputImage>
::GenerateData()
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
 
  // Need to allocate output buffer memory.
  output->SetBufferedRegion(output->GetRequestedRegion());
  output->Allocate();
 
  // Break the input into a series of regions.  The first region is free
  // of boundary conditions, the rest with boundary conditions. Note,
  // we pass in the input image and the OUTPUT requested region. We are
  // only concerned with centering the neighborhood operator at the
  // pixels that correspond to output pixels.
  faceList = faceCalculator(input, output->GetRequestedRegion(),
                            m_Operator.GetRadius());
  typename FaceListType::iterator fit = faceList.begin();

  // Process non-boundary region
  ConstNeighborhoodIterator<InputImageType>
    nit(m_Operator.GetRadius(), input, *fit);
  ImageRegionIterator<OutputImageType> it(output, *fit);
  nit.GoToBegin();
  it.GoToBegin();
  //  nit.Print(std::cout);
  while( ! nit.IsAtEnd() )
    {
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
      it.Value() = smartInnerProduct(bit, m_Operator);
      ++bit;
      ++it;
      }
   }
}

} // end namespace itk

#endif
