/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkWrapPadImageFilter.txx
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
#ifndef _itkWrapPadImageFilter_txx
#define _itkWrapPadImageFilter_txx

#include "itkWrapPadImageFilter.h"
#include "itkImageRegionIterator.h"
#include "itkObjectFactory.h"
#include <vector>

namespace itk
{
  
/**
 *
 */
template <class TInputImage, class TOutputImage>
void 
WrapPadImageFilter<TInputImage,TOutputImage>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
}
  
/**
 * Given an n dimensional list of output region breakpoints in indices
 * and size (where the current region and maximum region for each dimension
 * is encoded in regIndices and regLimit), choose the next output region.
 */ 
template <class TInputImage, class TOutputImage>
int WrapPadImageFilter<TInputImage,TOutputImage>
::GenerateNextOutputRegion(int *regIndices, int *regLimit, 
			   std::vector<int> indices[ImageDimension], 
			   std::vector<int> sizes[ImageDimension],
			   OutputImageRegionType& outputRegion)
{
    int ctr;
    int done = 0;
    OutputImageIndexType nextIndex = outputRegion.GetIndex();
    OutputImageSizeType nextSize = outputRegion.GetSize();
    
  //
  // Starting at the first dimension, increment the counter and set a new 
  // value for the region parameters.  If we wrap on a region, then we 
  // also increment to the next region for the next higher dimension.
  //
    for (ctr=0; (ctr<ImageDimension) && !done; ctr++) {
      regIndices[ctr]++;
      done = 1;
      if (regIndices[ctr] >= regLimit[ctr]) 
	{
	  regIndices[ctr] = 0;
	  done = 0;
	}
      nextIndex[ctr] = indices[ctr][regIndices[ctr]];
      nextSize[ctr] = sizes[ctr][regIndices[ctr]];
    }
    
  //
  // Set what we have learned into the image region.
  //
    outputRegion.SetIndex(nextIndex);
    outputRegion.SetSize(nextSize);
    
  // 
  // If any dimension has zero size, then we do not need to process this
  // region.  Report this back to the calling routine.
  //
    for (ctr=0; ctr<ImageDimension; ctr++) {
      if (nextSize[ctr] == 0) {
	return 0;
      }
    }
    
    return 1;
}

/**
 * Given an n dimensional list of input region breakpoints in indices
 * and size (where the current region and maximum region for each dimension
 * is encoded in regIndices and regLimit), choose the next input region.
 */ 
template <class TInputImage, class TOutputImage>
int WrapPadImageFilter<TInputImage,TOutputImage>
::GenerateNextInputRegion(int *regIndices, int *regLimit, 
			  std::vector<int> indices[ImageDimension], 
			  std::vector<int> sizes[ImageDimension],
			  InputImageRegionType& inputRegion)
{
  int ctr;
  int done = 0;
  InputImageIndexType nextIndex = inputRegion.GetIndex();
  InputImageSizeType nextSize = inputRegion.GetSize();
  
  //
  // Starting at the first dimension, increment the counter and set a new 
  // value for the region parameters.  If we wrap on a region, then we 
  // also increment to the next region for the next higher dimension.
  //
  for (ctr=0; (ctr<ImageDimension) && !done; ctr++) {
    regIndices[ctr]++;
    done = 1;
    if (regIndices[ctr] >= regLimit[ctr]) 
      {
	regIndices[ctr] = 0;
	done = 0;
      }
    nextIndex[ctr] = indices[ctr][regIndices[ctr]];
    nextSize[ctr] = sizes[ctr][regIndices[ctr]];
  }
  
  //
  // Set what we have learned into the image region.
  //
  inputRegion.SetIndex(nextIndex);
  inputRegion.SetSize(nextSize);
  
  // 
  // If any dimension has zero size, then we do not need to process this
  // region.  Report this back to the calling routine.
  //
  for (ctr=0; ctr<ImageDimension; ctr++) {
    if (nextSize[ctr] == 0) {
      return 0;
    }
  }
  
  return 1;
}

/**
 * Given the start and end indices of a region, determine how many
 * instances of size fit within the region.  The variable offset provides
 * a way to adjust width of the area while forcing alignment to the
 * start or end location.
 */
template <class TInputImage, class TOutputImage>
int
WrapPadImageFilter<TInputImage,TOutputImage>
::FindRegionsInArea(int start, int end, int size, int offset)
{
  int result = 1;
  int regionsize;
  
  regionsize = end - start;
  if (regionsize > 0)  // Find out home many regions we have,
    {
      result = regionsize / size;
      if ((regionsize % size) != 0)
	{
	  result++;
	}
      if (offset > 0)
	{
	  result = result - (offset/size);
	}
    }
  
  return result;
}

/**
 * Generate region 0 (inter-region) information.  Based on the indices
 * of the input and the output for this dimension, decide what are the 
 * starting points and the lengths of the output region directly 
 * corresponding to the input region.  Padding will be on either 
 * side of this region.  The algorithmic complications are necessary
 * to support the streaming interface and multithreading.
 */
template <class TInputImage, class TOutputImage>
int 
WrapPadImageFilter<TInputImage,TOutputImage>
::BuildInterRegions(std::vector<int>& inputRegionStart, 
                    std::vector<int>& outputRegionStart,
		    std::vector<int>& inputRegionSizes, 
                    std::vector<int>& outputRegionSizes,
		    int inputIndex, int outputIndex,
		    int inputSize, int outputSize, 
                    int numRegs, int & regCtr)
{
  int sizeTemp;  // Holder for current size calculation.

  // Region 0 is between, which has a starting index equal to 
  // the input region starting index, unless that would be
  // outside the bounds of the output image.
  if (inputIndex > outputIndex) 
    {
      outputRegionStart[0] = inputIndex;
      inputRegionStart[0] = inputIndex;
    }
  else
    {
      outputRegionStart[0] = outputIndex;
      inputRegionStart[0] = outputIndex;
    }
  
  // Size of the in region is the area from index 0 to the end of the 
  // input or the output, whichever comes first.
  if ((inputIndex+inputSize) 
      < (outputIndex+outputSize)) 
    {
      sizeTemp = inputIndex + inputSize - outputRegionStart[0];
    }
  else
    {
      sizeTemp = outputIndex + outputSize - outputRegionStart[0];
    }
  outputRegionSizes[0] = ((sizeTemp > 0) ? sizeTemp:0);
  inputRegionSizes[0] = ((sizeTemp > 0) ? sizeTemp:0);
  
  regCtr = numRegs;
  return 1;
}

/**
 * Generate region 1 (pre-region) information.  Based on the indices
 * of the input and the output for this dimension, decide what are the 
 * starting points and the lengths of the output region directly 
 * preceding the input region in this dimension.  This may require
 * more than one region be defined if the padding is larger than the
 * size of the input image in this dimension.  Other algorithmic 
 * complications are necessary to support the streaming interface 
 * and multithreading.
 */
template <class TInputImage, class TOutputImage>
int 
WrapPadImageFilter<TInputImage,TOutputImage>
::BuildPreRegions(std::vector<int>& inputRegionStart, 
                    std::vector<int>& outputRegionStart,
		    std::vector<int>& inputRegionSizes, 
                    std::vector<int>& outputRegionSizes,
		    int inputIndex, int outputIndex,
		    int inputSize, int outputSize, 
                    int numRegs, int & regCtr)
{
  int sizeTemp;  // Holder for current size calculation.
  int ctr;       // Generic loop counter.
  
  // Handle the pre-region.  Within the pre-region, the first and last
  // groups may be truncated and only contain the back part of the input
  // data.  All other regions will be complete copies of the input.
  outputRegionStart[regCtr] = outputIndex;
  
  // Size of the pre-region is all the output that preceeds the input, 
  // all except the first (and possibly the last) will be the size of 
  // the input.
  sizeTemp = outputRegionStart[0] - outputIndex;
  sizeTemp = ((sizeTemp > 0) ? (sizeTemp % inputSize) : 0);
  outputRegionSizes[regCtr] = sizeTemp;
  inputRegionSizes[regCtr] = sizeTemp;
  inputRegionStart[regCtr] = inputIndex + inputSize - sizeTemp;
  // Handle the rest of the pre-region by stepping through in blocks of
  // the size of the input image.
  for (ctr=1; ctr<numRegs; ctr++) 
    {
      regCtr++;
      outputRegionStart[regCtr] = outputRegionStart[regCtr-1]
        + outputRegionSizes[regCtr-1];
      inputRegionStart[regCtr] = inputIndex;
      outputRegionSizes[regCtr] = inputSize;
      inputRegionSizes[regCtr] = inputSize;
    }
  // Fix size on last region, if necessary.
  if ((outputRegionStart[regCtr]+outputRegionSizes[regCtr]) 
      > (outputIndex+outputSize)) 
    {
      outputRegionSizes[regCtr] = outputIndex + outputSize 
        - outputRegionStart[regCtr];
      inputRegionSizes[regCtr] = outputRegionSizes[regCtr];
    }

  return regCtr;
}

/**
 * Generate region 2 (post-region) information.  Based on the indices
 * of the input and the output for this dimension, decide what are the 
 * starting points and the lengths of the output region directly 
 * succeeding the input region in this dimension.  This may require
 * more than one region be defined if the padding is larger than the
 * size of the input image in this dimension.  Other algorithmic 
 * complications are necessary to support the streaming interface 
 * and multithreading.
 */
template <class TInputImage, class TOutputImage>
int 
WrapPadImageFilter<TInputImage,TOutputImage>
::BuildPostRegions(std::vector<int>& inputRegionStart, 
                    std::vector<int>& outputRegionStart,
		    std::vector<int>& inputRegionSizes, 
                    std::vector<int>& outputRegionSizes,
		    int inputIndex, int outputIndex,
		    int inputSize, int outputSize, 
                    int numRegs, int & regCtr)
{
  int sizeTemp;  // Holder for current size calculation.
  int ctr;       // Generic loop counter.

  // Handle the post region.  The post region has a number of
  // areas of size equal to the input region, followed by one
  // region of possibly smaller size.
  regCtr++;
  sizeTemp = outputIndex + outputSize - inputIndex - inputSize;
  sizeTemp = ((sizeTemp>0) ? (sizeTemp % (int)inputSize):0);
  outputRegionSizes[regCtr] = sizeTemp;
  inputRegionSizes[regCtr] = sizeTemp;
  outputRegionStart[regCtr] = outputIndex + outputSize - sizeTemp;
  inputRegionStart[regCtr] = inputIndex;
  
  for (ctr=numRegs-1; ctr>=1; ctr--) 
    {
      regCtr++;
      outputRegionStart[regCtr] = outputRegionStart[regCtr-1] - inputSize;
      inputRegionStart[regCtr] = inputIndex;
      outputRegionSizes[regCtr] = inputSize;
      inputRegionSizes[regCtr] = inputSize;
    }
  // Fix size on last region, if necessary.
  if (outputRegionStart[regCtr] < outputIndex)
    {
      sizeTemp = outputIndex - outputRegionStart[regCtr];
      outputRegionStart[regCtr] += sizeTemp;
      inputRegionStart[regCtr] += sizeTemp;
      outputRegionSizes[regCtr] -= sizeTemp;
      inputRegionSizes[regCtr] = outputRegionSizes[regCtr];
    }
}


/**
 *
 */
template <class TInputImage, class TOutputImage>
void 
WrapPadImageFilter<TInputImage,TOutputImage>
::ThreadedGenerateData(const OutputImageRegionType& outputRegionForThread,
                       int threadId)
{
  int dimCtr, regCtr, i;
  int numRegions=1; // Actual number of regions in our decomposed space.
  
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
  
  // For n dimensions, there are k^n combinations of before, between, and
  // after on these regions.  We are keeping this flexible so that we 
  // can handle other blockings imposed by the mirror and wrap algorithms.
  int inRegIndices[ImageDimension];
  int inRegLimit[ImageDimension];
  int outRegIndices[ImageDimension];
  int outRegLimit[ImageDimension];
  int numPre[ImageDimension], numPost[ImageDimension], numIn[ImageDimension];
  std::vector<int> outputRegionStart[ImageDimension];
  std::vector<int> outputRegionSizes[ImageDimension];  
  std::vector<int> inputRegionStart[ImageDimension];
  std::vector<int> inputRegionSizes[ImageDimension];
  
  // Calculate the actual number of regions for each dimension,
  // and set up the required variables here.
  for (dimCtr=0; dimCtr<ImageDimension; dimCtr++) 
    {
      numIn[dimCtr] = 1;  // Always assume exactly one inter region.
      numPre[dimCtr] =    // Count how many versions of input fit in pre-pad
	this->FindRegionsInArea(outputIndex[dimCtr], inputIndex[dimCtr],
				(int) inputSize[dimCtr], 
				inputIndex[dimCtr]-outputIndex[dimCtr]
				- (int)outputSize[dimCtr]);
      numPost[dimCtr] =   // Count how many versions of input fit in post-pad
	this->FindRegionsInArea(inputIndex[dimCtr]+(int)inputSize[dimCtr],
				outputIndex[dimCtr]+(int)outputSize[dimCtr],
				(int) inputSize[dimCtr],
				outputIndex[dimCtr]-inputIndex[dimCtr]
				- (int) inputSize[dimCtr]);
      inRegLimit[dimCtr] = numPre[dimCtr] + numIn[dimCtr] + numPost[dimCtr];
      inRegIndices[dimCtr] = inRegLimit[dimCtr] - 1;
      outRegLimit[dimCtr] = numPre[dimCtr] + numIn[dimCtr] + numPost[dimCtr];
      outRegIndices[dimCtr] = outRegLimit[dimCtr] - 1;
      numRegions *= outRegLimit[dimCtr];
      outputRegionStart[dimCtr].resize(outRegLimit[dimCtr]);
      outputRegionSizes[dimCtr].resize(outRegLimit[dimCtr]);
      inputRegionStart[dimCtr].resize(inRegLimit[dimCtr]);
      inputRegionSizes[dimCtr].resize(inRegLimit[dimCtr]);
    }
  
  //
  // Generate the break points for the image regions we counted in the 
  // previous loop.
  //
  for (dimCtr=0; dimCtr<ImageDimension; dimCtr++) 
    {
      //
      // Generate region 0 (inter-region) information.  Based on the indices
      // of the input and the output for this dimension, decide what are the 
      // starting points and the lengths of the output region directly 
      // corresponding to the input region.  Padding will be on either 
      // side of this region.
      //
      regCtr = BuildInterRegions(inputRegionStart[dimCtr], 
                                 outputRegionStart[dimCtr],
                                 inputRegionSizes[dimCtr], 
                                 outputRegionSizes[dimCtr],
                                 inputIndex[dimCtr], 
                                 outputIndex[dimCtr],
                                 (int) inputSize[dimCtr], 
                                 (int) outputSize[dimCtr], 
                                 numIn[dimCtr], regCtr);

      //
      // Generate region 1 (pre-region) information for that part of the
      // output image which precedes the input image in this dimension.
      //
      regCtr = BuildPreRegions(inputRegionStart[dimCtr], 
                               outputRegionStart[dimCtr],
                               inputRegionSizes[dimCtr], 
                               outputRegionSizes[dimCtr],
                               inputIndex[dimCtr], 
                               outputIndex[dimCtr],
                               (int) inputSize[dimCtr], 
                               (int) outputSize[dimCtr], 
                               numPre[dimCtr], regCtr);

      //
      // Generate region 2 (post-region) information for that part of the
      // output image which succeeds the input image in this dimension.
      //
      regCtr = BuildPostRegions(inputRegionStart[dimCtr], 
                               outputRegionStart[dimCtr],
                               inputRegionSizes[dimCtr], 
                               outputRegionSizes[dimCtr],
                               inputIndex[dimCtr], 
                               outputIndex[dimCtr],
                               (int) inputSize[dimCtr], 
                               (int) outputSize[dimCtr], 
                               numPost[dimCtr], regCtr);
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
  typedef ImageRegionIterator<TOutputImage> OutputIterator;
  typedef ImageRegionIterator<TInputImage> InputIterator;
  
  OutputIterator outIt;
  InputIterator inIt;
  
  i = 0;
  
  // Now walk the regions.
  for (regCtr=0; regCtr<numRegions; regCtr++)
    {
      // If both a valid output and input region are defined for the particular
      // defined region, then copy the input values to the output values.
      if (this->GenerateNextOutputRegion
	  (outRegIndices,outRegLimit,outputRegionStart,outputRegionSizes,outputRegion))
	{
	  if (this->GenerateNextInputRegion
	      (inRegIndices,inRegLimit,inputRegionStart,inputRegionSizes,inputRegion))
	    {
	      outIt = OutputIterator(outputPtr, outputRegion);
	      inIt = OutputIterator(inputPtr, inputRegion);
	      
	      // Do the actual copy of the input pixels to the output
              // pixels here.
	      for (; !outIt.IsAtEnd(); ++outIt, i++, ++inIt )
		{
		  if ( threadId == 0 && !(i % updateVisits ) )
		    {
		      this->UpdateProgress((float)i / (float)totalPixels);
		    }
		  
		  // copy the input pixel to the output
		  outIt.Set( inIt.Get() );
		}
	      
	    } 
	}
    }
}

} // end namespace itk

#endif
