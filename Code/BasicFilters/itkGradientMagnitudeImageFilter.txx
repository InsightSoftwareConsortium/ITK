/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGradientMagnitudeImageFilter.txx
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
#ifndef _itkGradientMagnitudeImageFilter_txx
#define _itkGradientMagnitudeImageFilter_txx

#include "itkConstNeighborhoodIterator.h"
#include "itkConstSmartNeighborhoodIterator.h"
#include "itkNeighborhoodInnerProduct.h"
#include "itkImageRegionIterator.h"
#include "itkDerivativeOperator.h"
#include "itkNeighborhoodAlgorithm.h"
#include "itkZeroFluxNeumannBoundaryCondition.h"
#include "itkOffset.h"

namespace itk
{
 
template <class TInputImage, class TOutputImage>
void 
GradientMagnitudeImageFilter<TInputImage,TOutputImage>
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

  // Build an operator so that we can determine the kernel size
  DerivativeOperator<OutputPixelType, ImageDimension> oper;
   oper.SetDirection(0);
   oper.SetOrder(1);
   oper.CreateDirectional();
  long radius = oper.GetRadius()[0];
  
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
      = outputRequestedRegionSize[i] + 2 * radius;
    inputRequestedRegionStartIndex[i]
      = outputRequestedRegionStartIndex[i] - radius;

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


template< class TInputImage, class TOutputImage >
void
GradientMagnitudeImageFilter< TInputImage, TOutputImage >
::ThreadedGenerateData(const OutputImageRegionType& outputRegionForThread,
                       int threadId)
{
  unsigned int i;
  OutputPixelType a, g;
  ZeroFluxNeumannBoundaryCondition<TInputImage> nbc;

  ConstNeighborhoodIterator<TInputImage> nit;
  ConstSmartNeighborhoodIterator<TInputImage> bit;
  ImageRegionIterator<TOutputImage> it;

  NeighborhoodInnerProduct<TInputImage> IP;
  SmartNeighborhoodInnerProduct<TInputImage> SIP;

  // Allocate output
  typename OutputImageType::Pointer output = this->GetOutput();
  typename  InputImageType::Pointer input  = this->GetInput();
  
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

  std::slice x_slice[ImageDimension];
  const unsigned long center = nit.Size() / 2;
  for (i = 0; i < ImageDimension; ++i)
    {
      x_slice[i] = std::slice( center - nit.GetStride(i) * radius[i],
                               op.GetSize()[0], nit.GetStride(i));
    }

  nit.GoToBegin();
  it.GoToBegin();

  while( ! nit.IsAtEnd() )
    {
    if ( threadId == 0 && !(ii % updateVisits ) )
      {
      this->UpdateProgress((float)ii++ / (float)totalPixels);
      }

    a = NumericTraits<OutputPixelType>::Zero;
    for (i = 0; i < ImageDimension; ++i)
      {
      g = IP(x_slice[i], nit, op);
      a += g * g;
      }
    it.Value() = ::sqrt(a);
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
    
    while ( ! bit.IsAtEnd() )
      {
      a = NumericTraits<OutputPixelType>::Zero;
      for (i = 0; i < ImageDimension; ++i)
        {
        g = SIP(x_slice[i], bit, op);
        a += g * g;
        }
      it.Value() = ::sqrt(a);          
      ++bit;
      ++it;
      }
    }
}

} // end namespace itk

#endif
