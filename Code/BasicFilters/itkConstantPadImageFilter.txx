/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkConstantPadImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkConstantPadImageFilter_txx
#define _itkConstantPadImageFilter_txx

#include "itkConstantPadImageFilter.h"
#include "itkImageRegionIterator.h"
#include "itkImageRegionConstIterator.h"
#include "itkObjectFactory.h"
#include "itkProgressReporter.h"

namespace itk
{

/**
 *
 */
template <class TInputImage, class TOutputImage>
ConstantPadImageFilter<TInputImage,TOutputImage>
::ConstantPadImageFilter()
{
  m_Constant = NumericTraits<OutputImagePixelType>::Zero;
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

  os << indent << "Constant: "
     << static_cast<typename NumericTraits<OutputImagePixelType>::PrintType>(m_Constant)
     << std::endl;
  os << std::endl;
}

/**
 * Given an n dimensional list of output region breakpoints in indices
 * and size (where the current region and maximum region for each dimension
 * is encoded in regIndices and regLimit), choose the next output region.
 */ 
template <class TInputImage, class TOutputImage>
int
ConstantPadImageFilter<TInputImage,TOutputImage>
::GenerateNextRegion(long *regIndices, long *regLimit, 
                     OutputImageIndexType *indices, 
                     OutputImageSizeType *sizes, 
                     OutputImageRegionType& outputRegion)
{
  unsigned int ctr;
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
  unsigned int dimCtr, regCtr, ctr=0;
  unsigned int numRegions=1; // number of regions in our decomposed space.
  long sizeTemp;    // We need to calculate negative sizes.  This allows us to do so.

  itkDebugMacro(<<"Actually executing");

  // Get the input and output pointers
  typename Superclass::InputImageConstPointer  inputPtr = this->GetInput();
  typename Superclass::OutputImagePointer outputPtr = this->GetOutput();

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
  long regIndices[ImageDimension];
  long regLimit[ImageDimension];

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

    if ((inputIndex[dimCtr]+ static_cast<long>(inputSize[dimCtr])) > outputIndex[dimCtr])
      {
      indices[2][dimCtr] = inputIndex[dimCtr]+ static_cast<long>(inputSize[dimCtr]);
      } 
    else
      {
      indices[2][dimCtr] = outputIndex[dimCtr];
      }

    // Size 0 is the area from index 0 to the end of the input or the 
    // output, whichever comes first.
    if ((inputIndex[dimCtr]+static_cast<long>(inputSize[dimCtr])) 
        < (outputIndex[dimCtr]+static_cast<long>(outputSize[dimCtr]))) 
      {
      sizeTemp = inputIndex[dimCtr] + static_cast<long>(inputSize[dimCtr]) 
        - indices[0][dimCtr];
      }
    else
      {
      sizeTemp = outputIndex[dimCtr] + static_cast<long>(outputSize[dimCtr]) 
        - indices[0][dimCtr];
      }
    sizes[0][dimCtr] = ((sizeTemp > 0) ? sizeTemp:0);
    // Size 1 is all the output that preceeds the input, and Size 2 is
    // all the output that succeeds the input.
    if ((outputIndex[dimCtr]+static_cast<long>(outputSize[dimCtr])) > indices[0][dimCtr])
      {
      sizeTemp = indices[0][dimCtr] - outputIndex[dimCtr];
      }
    else
      {
      sizeTemp = static_cast<long>(outputSize[dimCtr]);
      }
    sizes[1][dimCtr] = ((sizeTemp > 0) ? sizeTemp:0);
    sizeTemp = outputIndex[dimCtr] + static_cast<long>(outputSize[dimCtr])
      - indices[2][dimCtr];
    sizes[2][dimCtr] = ((sizeTemp > 0) ? sizeTemp:0);

    }


  ProgressReporter progress(this, threadId, outputRegionForThread.GetNumberOfPixels());
  
  // Define/declare iterators that will walk the input and output regions
  // for this thread.
  outputRegion.SetSize(sizes[0]);
  outputRegion.SetIndex(indices[0]);
  inputRegion.SetSize(sizes[0]);
  inputRegion.SetIndex(indices[0]);

  typedef
    ImageRegionIterator<TOutputImage> OutputIterator;
  typedef 
    ImageRegionConstIterator<TInputImage> InputIterator;

  // Walk the first region which is defined as the between for everyone.
  if (GenerateNextRegion(regIndices, regLimit, indices, sizes, outputRegion))
    {
    inputRegion.SetIndex(outputRegion.GetIndex());
    inputRegion.SetSize(outputRegion.GetSize());
    OutputIterator outIt = OutputIterator(outputPtr, outputRegion);
    InputIterator  inIt  = InputIterator(inputPtr, inputRegion);

    // walk the output region, and sample the input image
    for (ctr=0; !outIt.IsAtEnd(); ++outIt, ++inIt, ctr++ )
      {
      // copy the input pixel to the output
      outIt.Set( inIt.Get());
      progress.CompletedPixel();
      }
    } 

  // Now walk the remaining regions.
  for (regCtr=1; regCtr<numRegions; regCtr++)
    {
    if (GenerateNextRegion(regIndices, regLimit, indices, sizes, outputRegion))
      {
      OutputIterator outIt = OutputIterator(outputPtr, outputRegion);
        
      // walk the output region, and sample the input image
      for (; !outIt.IsAtEnd(); ++outIt, ctr++ )
        {
        // copy the input pixel to the output
        outIt.Set( m_Constant );
        progress.CompletedPixel();
        }
      } 
    }
}


} // end namespace itk

#endif
