/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkConstantPadImageFilter.txx
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
#ifndef _itkConstantPadImageFilter_txx
#define _itkConstantPadImageFilter_txx

#include "itkConstantPadImageFilter.h"
#include "itkImageRegionIterator.h"
#include "itkObjectFactory.h"

namespace itk
{

/**
 *
 */
template <class TInputImage, class TOutputImage>
ConstantPadImageFilter<TInputImage,TOutputImage>
::ConstantPadImageFilter()
{
  m_Constant = NumericTraits<PixelType>::Zero;
}


/**
 *
 */
template <class TInputImage, class TOutputImage>
void 
ConstantPadImageFilter<TInputImage,TOutputImage>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);

  os << indent << "Constant: ";
  os << m_Constant << " ";
  os << std::endl;
}

/**
 * Given the list of region breaks, determine the breaks for this 
 * particular combination. 
 */
template <class TInputImage, class TOutputImage>
int
ConstantPadImageFilter<TInputImage,TOutputImage>
::GenerateNextRegion(int *regIndices, int *regLimit, 
		     OutputImageIndexType *indices, 
		     OutputImageSizeType *sizes, 
		     OutputImageRegionType& outputRegion)
{
  int ctr;
  int done = 0;
  OutputImageIndexType nextIndex = outputRegion.GetIndex();
  OutputImageSizeType nextSize = outputRegion.GetSize();

  for (ctr=0; (ctr<ImageDimension) && !done; ctr++) {
    regIndices[ctr]++;
    done = 1;
    if (regIndices[ctr] >= regLimit[ctr]) 
      {
	regIndices[ctr] = 0;
	done = 0;
      }
    nextIndex[ctr] = indices[regIndices[ctr]][ctr];
    nextSize[ctr] = sizes[regIndices[ctr]][ctr];
  }

  outputRegion.SetIndex(nextIndex);
  outputRegion.SetSize(nextSize);

  for (ctr=0; ctr<ImageDimension; ctr++) {
    if (nextSize[ctr] == 0) {
      return 0;
    }
  }

  return 1;
}

/**
 *
 */
template <class TInputImage, class TOutputImage>
void 
ConstantPadImageFilter<TInputImage,TOutputImage> // support progress methods/callbacks

::ThreadedGenerateData(const OutputImageRegionType& outputRegionForThread,
                       int threadId)
{
  int dimCtr, regCtr, i;
  int numRegions=1; // Actual number of regions in our decomposed space.
  long sizeTemp;    // We need to calculate negative sizes.  This allows us to do so.

  itkDebugMacro(<<"Actually executing");

  // Get the input and output pointers
  InputImagePointer  inputPtr = this->GetInput();
  OutputImagePointer outputPtr = this->GetOutput();

  // Define a few indices that will be used to translate from an input pixel
  // to an output pixel
  OutputImageIndexType outputIndex = outputRegionForThread.GetIndex();
  InputImageIndexType inputIndex 
    = inputPtr->GetLargestPossibleRegion().GetIndex();
  OutputImageSizeType outputSize = outputRegionForThread.GetSize();
  InputImageSizeType inputSize 
    = inputPtr->GetLargestPossibleRegion().GetSize();

  OutputImageRegionType outputRegion; 
  InputImageRegionType inputRegion;

  // For n dimensions, there are 3^n combinations of before, between, and
  // after on these regions.  We are keeping this flexible so that we 
  // can handle other blockings imposed by the mirror and wrap algorithms.
  OutputImageIndexType indices[3];
  OutputImageSizeType sizes[3];
  int regIndices[ImageDimension];
  int regLimit[ImageDimension];

  for (dimCtr=0; dimCtr<ImageDimension; dimCtr++) 
    {
      regIndices[dimCtr] = 2;
      regLimit[dimCtr] = 3;
      numRegions *= 3;

      // Region 0 is between, which has a starting index equal to 
      // the input region starting index, unless that would be
      // outside the bounds of the output image.
      if (inputIndex[dimCtr] > outputIndex[dimCtr]) 
	{
	  indices[0][dimCtr] = inputIndex[dimCtr];
	}
      else
	{
	  indices[0][dimCtr] = outputIndex[dimCtr];
	}
      // Region 1 is before, which is always the output starting index,
      // and Region 2 is after, which is either the end of the input 
      // image, or the start of the output image.
      indices[1][dimCtr] = outputIndex[dimCtr];

      if ((inputIndex[dimCtr]+ (int)inputSize[dimCtr]) > outputIndex[dimCtr])
	{
	  indices[2][dimCtr] = inputIndex[dimCtr]+ (int) inputSize[dimCtr];
	} 
      else
	{
	  indices[2][dimCtr] = outputIndex[dimCtr];
	}

      // Size 0 is the area from index 0 to the end of the input or the 
      // output, whichever comes first.
      if ((inputIndex[dimCtr]+(int)inputSize[dimCtr]) 
	  < (outputIndex[dimCtr]+(int)outputSize[dimCtr])) 
	{
	  sizeTemp = inputIndex[dimCtr] + (int) inputSize[dimCtr] 
	    - indices[0][dimCtr];
	}
      else
	{
	  sizeTemp = outputIndex[dimCtr] + (int) outputSize[dimCtr] 
	    - indices[0][dimCtr];
	}
	sizes[0][dimCtr] = ((sizeTemp > 0) ? sizeTemp:0);
      // Size 1 is all the output that preceeds the input, and Size 2 is
      // all the output that succeeds the input.
      if ((outputIndex[dimCtr]+(int) outputSize[dimCtr]) > indices[0][dimCtr])
	{
	  sizeTemp = indices[0][dimCtr] - outputIndex[dimCtr];
	}
      else
	{
	  sizeTemp = outputSize[dimCtr];
	}
	sizes[1][dimCtr] = ((sizeTemp > 0) ? sizeTemp:0);
    sizeTemp = outputIndex[dimCtr] + (int) outputSize[dimCtr]
	- indices[2][dimCtr];
	sizes[2][dimCtr] = ((sizeTemp > 0) ? sizeTemp:0);
	
    }


  // support progress methods/callbacks
  unsigned long updateVisits = 0;
  unsigned long totalPixels = 0;
  if ( threadId == 0 )
    {
    totalPixels = 
      outputPtr->GetRequestedRegion().GetNumberOfPixels();
    updateVisits = totalPixels / 10;
    if( updateVisits < 1 ) updateVisits = 1;
    }

  // Define/declare iterators that will walk the input and output regions
  // for this thread.
  outputRegion.SetSize(sizes[0]);
  outputRegion.SetIndex(indices[0]);
  inputRegion.SetSize(sizes[0]);
  inputRegion.SetIndex(indices[0]);

  typedef
    ImageRegionIterator<TOutputImage> OutputIterator;
  typedef 
    ImageRegionIterator<TInputImage> InputIterator;

  OutputIterator outIt;
  InputIterator inIt;

  // Walk the first region which is defined as the between for everyone.
  if (GenerateNextRegion(regIndices, regLimit, indices, sizes, outputRegion))
    {
      inputRegion.SetIndex(outputRegion.GetIndex());
      inputRegion.SetSize(outputRegion.GetSize());
      outIt = OutputIterator(outputPtr, outputRegion);
      inIt = OutputIterator(inputPtr, inputRegion);

      // walk the output region, and sample the input image
      for (i=0; !outIt.IsAtEnd(); ++outIt, ++inIt, i++ )
	{
	  if ( threadId == 0 && !(i % updateVisits ) )
	    {
	      this->UpdateProgress((float)i / (float)totalPixels);
	    }
	  
	  // copy the input pixel to the output
	  outIt.Set( inIt.Get());
	}
    } 

  // Now walk the remaining regions.
  for (regCtr=1; regCtr<numRegions; regCtr++)
    {
      if (GenerateNextRegion
	  (regIndices, regLimit, indices, sizes, outputRegion))
	{
	  outIt = OutputIterator(outputPtr, outputRegion);
	  
	  // walk the output region, and sample the input image
	  for (; !outIt.IsAtEnd(); ++outIt, i++ )
	    {
	      if ( threadId == 0 && !(i % updateVisits ) )
		{
		  this->UpdateProgress((float)i / (float)totalPixels);
		}
	      
	      // copy the input pixel to the output
	      outIt.Set( m_Constant );
	    }
	} 
    }
}


} // end namespace itk

#endif
