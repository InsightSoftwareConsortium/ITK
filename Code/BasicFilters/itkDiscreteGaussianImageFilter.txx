/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkDiscreteGaussianImageFilter.txx
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
#ifndef _itkDiscreteGaussianImageFilter_txx
#define _itkDiscreteGaussianImageFilter_txx

#include "itkNeighborhoodOperatorImageFilter.h"
#include "itkGaussianOperator.h"
#include "itkImageRegionIterator.h"

namespace itk
{

template< class TInputImage, class TOutputImage >
void
DiscreteGaussianImageFilter< TInputImage, TOutputImage >
::ImageRegionCopy(TOutputImage *imgT, TInputImage *input)
{
  ImageRegionIterator<TInputImage> in_it(input,
                                         imgT->GetRequestedRegion());
  ImageRegionIterator<TOutputImage> out_it(imgT,
                                           imgT->GetRequestedRegion());
  for (; !in_it.IsAtEnd(); ++in_it, ++out_it)
    {
    out_it.Set( in_it.Get() );
    }
}

template <class TInputImage, class TOutputImage>
void 
DiscreteGaussianImageFilter<TInputImage,TOutputImage>
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
  GaussianOperator<OutputPixelType, ImageDimension> oper;
  
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
  long ithRadius;
  for (i = 0; i < TInputImage::ImageDimension; i++)
    {
    // Determine the size of the operator in this dimension.  Note that the
    // Gaussian is built as a 1D operator in each of the specified directions.
    oper.SetDirection(i);
    oper.SetVariance(m_Variance[i]);
    oper.SetMaximumError(m_MaximumError[i]);
    oper.CreateDirectional();

    ithRadius = oper.GetRadius(i);

    // pad the input requested region along this dimension by the ithRadius
    inputRequestedRegionSize[i]
      = outputRequestedRegionSize[i] + 2 * ithRadius;
    inputRequestedRegionStartIndex[i]
      = outputRequestedRegionStartIndex[i] - ithRadius;

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
DiscreteGaussianImageFilter<TInputImage, TOutputImage>
::GenerateData()
{
  typename TInputImage::Pointer input = this->GetInput();
  typename TOutputImage::Pointer output = this->GetOutput();
  typename TOutputImage::Pointer swapPtrA, swapPtrB, swapPtrC;
  
  output->SetBufferedRegion(output->GetRequestedRegion());
  output->Allocate();

  typename TOutputImage::Pointer imgT = TOutputImage::New();
  imgT->SetLargestPossibleRegion(output->GetLargestPossibleRegion());
  imgT->SetRequestedRegion(output->GetRequestedRegion());
  imgT->SetBufferedRegion(output->GetBufferedRegion());
  imgT->Allocate();

  Self::ImageRegionCopy(imgT, input);

  GaussianOperator<OutputPixelType, ImageDimension> *oper;
  NeighborhoodOperatorImageFilter<InputImageType, OutputImageType>::Pointer filter;

  swapPtrA = imgT;
  swapPtrB = output;
  for (unsigned int i = 0; i < ImageDimension; ++i)
    {
    // Filter
    oper = new GaussianOperator<OutputPixelType,ImageDimension>;
    oper->SetDirection(i);
    oper->SetVariance(m_Variance[i]);
    oper->SetMaximumError(m_MaximumError[i]);
    oper->CreateDirectional();

    filter = NeighborhoodOperatorImageFilter<InputImageType, OutputImageType>::New();
    filter->SetOperator(*oper);
    filter->SetInput(swapPtrA);
    filter->SetOutput(swapPtrB);
    filter->Update();

    //      delete oper;       pipeline problems cause seg fault? --3/13/01
    swapPtrC = swapPtrB;
    swapPtrB = swapPtrA;
    swapPtrA = swapPtrC;
    }

  if ((ImageDimension % 2) == 0)
    { 
    Self::ImageRegionCopy(output, imgT);
    }
}

} // end namespace itk

#endif
