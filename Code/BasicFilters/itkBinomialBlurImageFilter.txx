/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBinomialBlurImageFilter.txx
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
#ifndef __itkBinomialBlurImageFilter_txx
#define __itkBinomialBlurImageFilter_txx

#include <iostream>
#include "vnl/vnl_vector_fixed.h"
#include "itkSize.h"
#include "itkImageRegion.h"
#include "itkBinomialBlurImageFilter.h"
#include "itkImageRegionIterator.h"
#include "itkImageRegionReverseIterator.h"

namespace itk
{

template< class TInputImage, class TOutputImage >
BinomialBlurImageFilter< TInputImage, TOutputImage >
::BinomialBlurImageFilter()
{
  itkDebugMacro(<< "BinomialBlurImageFilter::BinomialBlurImageFilter() called");

  // The default is to just do one repetition
  m_Repetitions = 1;
}

template< class TInputImage, class TOutputImage >
void
BinomialBlurImageFilter< TInputImage, TOutputImage >
::GenerateInputRequestedRegion()
{
  itkDebugMacro(<< "BinomialBlurImageFilter::GenerateInputRequestedRegion() called");
  
  Superclass::GenerateInputRequestedRegion();

  InputImagePointer inputPtr = this->GetInput(0);
  OutputImagePointer outputPtr = this->GetOutput(0);

  typename TOutputImage::RegionType outputRegion;
  typename TInputImage::RegionType inputRegion;
  typename TInputImage::RegionType::SizeType inputSize;
  typename TInputImage::RegionType::IndexType inputIndex;
  
  outputRegion = outputPtr->GetRequestedRegion();

  // This filter needs a m_Repetitions pixel border about the output
  // (clamped of course at the true boundaries of the input image)
  inputRegion = outputRegion;

  inputSize = inputRegion.GetSize();
  inputIndex = inputRegion.GetIndex();

  typename TInputImage::RegionType::IndexType inputLargestPossibleRegionIndex
    = inputPtr->GetLargestPossibleRegion().GetIndex();
  typename TInputImage::RegionType::SizeType inputLargestPossibleRegionSize
    = inputPtr->GetLargestPossibleRegion().GetSize();

  for (unsigned int i=0; i < inputPtr->GetImageDimension(); ++i)
    {
    inputIndex[i] -= m_Repetitions;
    if (inputIndex[i] < inputLargestPossibleRegionIndex[i])
      {
      inputIndex[i] = inputLargestPossibleRegionIndex[i];
      }

    inputSize[i] += m_Repetitions;
    if (inputSize[i] > inputLargestPossibleRegionSize[i])
      {
      inputSize[i] = inputLargestPossibleRegionSize[i];
      }
    }

  inputRegion.SetIndex( inputIndex );
  inputRegion.SetSize( inputSize );
  
  inputPtr->SetRequestedRegion( inputRegion );
}


template< class TInputImage, class TOutputImage >
void
BinomialBlurImageFilter< TInputImage, TOutputImage >
::GenerateData()
{
  itkDebugMacro(<< "BinomialBlurImageFilter::GenerateData() called");

  // Get the input and output pointers
  InputImagePointer  inputPtr = this->GetInput(0);
  OutputImagePointer outputPtr = this->GetOutput(0);

  // Allocate the output
  outputPtr->SetBufferedRegion( outputPtr->GetRequestedRegion() );
  outputPtr->Allocate();

  // Create a temporary image used while processing the image
  // Processing with doubles eliminates possible rounding artifacts which may
  // accumulate over repeated integer division
  typedef Image<double, NDimensions> TTempImage;
  TTempImage::Pointer tempPtr = TTempImage::New();

  TTempImage::RegionType tempRegion;
  tempRegion = inputPtr->GetRequestedRegion();
  
  tempPtr->SetLargestPossibleRegion( tempRegion );
  tempPtr->SetBufferedRegion( tempRegion );
  tempPtr->SetRequestedRegion( tempRegion );
  tempPtr->Allocate();

  // How big is the input image?
  typename TInputImage::SizeType size=inputPtr->GetRequestedRegion().GetSize();
  typename TInputImage::IndexType startIndex=inputPtr->GetRequestedRegion().GetIndex();
  
  // Iterator Typedefs for this routine
  typedef ImageRegionIterator<TTempImage> TempIterator;
  typedef ImageRegionReverseIterator<TTempImage> TempReverseIterator;
  typedef ImageRegionIterator<TInputImage> InputIterator;
  typedef ImageRegionIterator<TOutputImage> OutputIterator;
  
  // Copy the input image to the temporary image
  TempIterator tempIt = TempIterator(tempPtr,
                                     tempPtr->GetRequestedRegion());
  InputIterator inputIt = InputIterator(inputPtr, inputPtr->GetRequestedRegion());
  
  for ( inputIt.GoToBegin(), tempIt.GoToBegin(); !tempIt.IsAtEnd();++tempIt, ++inputIt)
    {
    tempIt.Set( (double) inputIt.Get() );
    }

  // Define a few indices that will be used to translate from an input pixel
  // to an output pixel
  typename TTempImage::IndexType index;
  typename TTempImage::IndexType indexShift;

  // How many times has the algorithm executed? (for debug)
  int num_reps = 0;

  // Temporary pixel storage
  double pixelA, pixelB;

  // walk the output image forwards and compute blur
  for (unsigned int rep = 0; rep < m_Repetitions; rep++)
    {
    num_reps++;

    itkDebugMacro(<< "Repetition # " << rep);

    // blur each dimension
    for (unsigned int dim = 0; dim < NDimensions; dim ++)
      {
      TempIterator tempIt = TempIterator(tempPtr,
                                         tempPtr->GetRequestedRegion());
      tempIt.GoToBegin();
      while( !tempIt.IsAtEnd() )
        {
        // determine the index of the output pixel
        index = tempIt.GetIndex();

        if ( index[dim] < 
           ( startIndex[dim] + static_cast<long>(size[dim]) - 1))
          {
          // Figure out the location of the "neighbor" pixel
          for (unsigned int i = 0; i < NDimensions; i++)
            {
            if ( i == dim ) 
              {
              indexShift.m_Index[i] = index.m_Index[i] + 1;
              }
            else
              {
              indexShift.m_Index[i] = index.m_Index[i];
              }
            }

          // Average the pixel of interest and shifted pixel
          pixelA = tempPtr->GetPixel(index);
          pixelB = tempPtr->GetPixel(indexShift);

          pixelA += pixelB;
          pixelA = pixelA / 2.0;

          tempPtr->SetPixel(index, pixelA);
          }

        ++tempIt;
    
        } // end walk the image forwards
      
      itkDebugMacro(<< "End processing forward dimension " << dim);

      //----------------------Reverse pass----------------------
      TempReverseIterator tempReverseIt
        = TempReverseIterator(tempPtr, tempPtr->GetRequestedRegion());
      
      tempReverseIt.GoToBegin();
      
      while ( !tempReverseIt.IsAtEnd() )
        {
        // determine the index of the output pixel
        index = tempReverseIt.GetIndex();

        if (index[dim] > startIndex[dim])
          {    
          // Figure out the location of the "neighbor" pixel
          for (unsigned int i = 0; i < NDimensions; i++)
            {
            if ( i == dim ) 
              {
              indexShift.m_Index[i] = index.m_Index[i] - 1;
              }
            else
              {
              indexShift.m_Index[i] = index.m_Index[i];
              }
            }

          /*
          itkDebugMacro(<< "Dimension = " << dim << ", reverse pass");
          itkDebugMacro(<< "Index = (" << index[0] << "," << index[1] << ","
                        << index[2] << ")");
          itkDebugMacro(<< "shiftIndex = (" << indexShift[0] << "," << indexShift[1] << ","
                        << indexShift[2] << ")");
          */

          // Average the pixel of interest and shifted pixel
          pixelA = tempPtr->GetPixel(index);
          pixelB = tempPtr->GetPixel(indexShift);

          pixelA += pixelB;
          pixelA = pixelA / 2;

          tempPtr->SetPixel(index, pixelA);
          }

        ++tempReverseIt;
    
        } // end walk the image backwards
      
      itkDebugMacro(<< "End processing reverse dimension " << dim);
      } // end dimension loop

    } // end number of repetitions loop

  // Now, copy the temporary image to the output image. Note that the temp
  // buffer iterator walks a region defined by the output
  typedef ImageRegionIterator<TOutputImage> OutputIterator;

  OutputIterator outIt = OutputIterator(outputPtr,
                                        outputPtr->GetRequestedRegion());
  TempIterator tempIt2 = TempIterator(tempPtr,
                                     outputPtr->GetRequestedRegion());
  
  for ( outIt.GoToBegin(), tempIt2.GoToBegin(); !outIt.IsAtEnd();
        ++outIt, ++tempIt2)
    {
    outIt.Set( tempIt2.Get() );
    }

  itkDebugMacro(<< "Binomial blur filter executed " << num_reps << " times");
}

template< class TInputImage, class TOutputImage >
void
BinomialBlurImageFilter< TInputImage, TOutputImage >
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
  
  os << indent << "Number of repetitions: " << m_Repetitions << std::endl;
  
}

} // end namespace

#endif
