/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkMirrorPadImageFilter_hxx
#define itkMirrorPadImageFilter_hxx

#include "itkMirrorPadImageFilter.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkImageRegionConstIterator.h"
#include "itkObjectFactory.h"
#include "itkProgressReporter.h"
#include <vector>

namespace itk
{
/**
 * Given an n dimensional list of output region breakpoints in indices
 * and size (where the current region and maximum region for each dimension
 * is encoded in regIndices and regLimit), choose the next output region.
 */
template< typename TInputImage, typename TOutputImage >
int MirrorPadImageFilter< TInputImage, TOutputImage >
::GenerateNextOutputRegion(long *regIndices, long *regLimit,
                           std::vector< long > *indices,
                           std::vector< long > *sizes,
                           OutputImageRegionType & outputRegion)
{
  unsigned int         ctr;
  int                  done = 0;
  OutputImageIndexType nextIndex = outputRegion.GetIndex();
  OutputImageSizeType  nextSize = outputRegion.GetSize();

  //
  // Starting at the first dimension, increment the counter and set a new
  // value for the region parameters.  If we wrap on a region, then we
  // also increment to the next region for the next higher dimension.
  //
  for ( ctr = 0; ( ctr < ImageDimension ) && !done; ctr++ )
    {
    regIndices[ctr]++;
    done = 1;
    if ( regIndices[ctr] >= regLimit[ctr] )
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
  for ( ctr = 0; ctr < ImageDimension; ctr++ )
    {
    if ( nextSize[ctr] == 0 )
      {
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
template< typename TInputImage, typename TOutputImage >
int MirrorPadImageFilter< TInputImage, TOutputImage >
::GenerateNextInputRegion(long *regIndices, long *regLimit,
                          std::vector< long > *indices,
                          std::vector< long > *sizes,
                          InputImageRegionType & inputRegion)
{
  unsigned int        ctr;
  int                 done = 0;
  InputImageIndexType nextIndex = inputRegion.GetIndex();
  InputImageSizeType  nextSize = inputRegion.GetSize();

  //
  // Starting at the first dimension, increment the counter and set a new
  // value for the region parameters.  If we wrap on a region, then we
  // also increment to the next region for the next higher dimension.
  //
  for ( ctr = 0; ( ctr < ImageDimension ) && !done; ctr++ )
    {
    regIndices[ctr]++;
    done = 1;
    if ( regIndices[ctr] >= regLimit[ctr] )
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
  for ( ctr = 0; ctr < ImageDimension; ctr++ )
    {
    if ( nextSize[ctr] == 0 )
      {
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
template< typename TInputImage, typename TOutputImage >
int
MirrorPadImageFilter< TInputImage, TOutputImage >
::FindRegionsInArea(long start, long end, long size, long offset)
{
  int  result = 1;
  long regionsize;

  regionsize = end - start;
  if ( regionsize > 0 )  // Find out home many regions we have,
    {
    result = regionsize / size;
//      if ((regionsize % size) != 0)
//  {
    result++;
//  }
    if ( offset > 0 )
      {
      result = result - ( offset / size );
      }
    }

  return result;
}

/**
 * Convert from the output index to the input index taking
 * into consideration mirrored and normal regions.
 */
template< typename TInputImage, typename TOutputImage >
void
MirrorPadImageFilter< TInputImage, TOutputImage >
::ConvertOutputIndexToInputIndex(OutputImageIndexType & outputIndex,
                                 InputImageIndexType & inputIndex,
                                 OutputImageRegionType & outputRegion,
                                 InputImageRegionType & inputRegion,
                                 int *oddRegionArray)
{
  unsigned int dimCtr;
  long         a, b, c; // Output region goes from a to a+b-1
                        // Input region goes from c to c+b-1
  OutputImageIndexType outputRegionStart = outputRegion.GetIndex();
  InputImageIndexType  inputRegionStart = inputRegion.GetIndex();
  InputImageSizeType   inputSizes = inputRegion.GetSize();

  for ( dimCtr = 0; dimCtr < ImageDimension; dimCtr++ )
    {
    a = outputRegionStart[dimCtr];
    c = inputRegionStart[dimCtr];

    if ( oddRegionArray[dimCtr] )
      {
      b = inputSizes[dimCtr];
      inputIndex[dimCtr] = a + c + b - 1 - outputIndex[dimCtr];
      }
    else
      {
      inputIndex[dimCtr] =  outputIndex[dimCtr] - a + c;
      }
    }
}

/**
 * Decide whether test falls within an odd or even number
 * of size regions from base.
 */
template< typename TInputImage, typename TOutputImage >
int
MirrorPadImageFilter< TInputImage, TOutputImage >
::RegionIsOdd(long base, long test, long size)
{
  long oddness;

  // Within first region is even.
  if ( ( test >= base ) && ( test < ( base + size ) ) )
    {
    return 0;
    }

  if ( test < base )
    {
    oddness = ( base - test - 1 ) / size;
    return !( oddness & 1 );
    }

  oddness = ( test - base ) / size;
  return ( oddness & 1 );
}

/**
 * Generate region 0 (inter-region) information.  Based on the indices
 * of the input and the output for this dimension, decide what are the
 * starting points and the lengths of the output region directly
 * corresponding to the input region.  Padding will be on either
 * side of this region.  The algorithmic complications are necessary
 * to support the streaming interface and multithreading.
 */
template< typename TInputImage, typename TOutputImage >
int
MirrorPadImageFilter< TInputImage, TOutputImage >
::BuildInterRegions(std::vector< long > & inputRegionStart,
                    std::vector< long > & outputRegionStart,
                    std::vector< long > & inputRegionSizes,
                    std::vector< long > & outputRegionSizes,
                    long inputIndex, long outputIndex,
                    long inputSize, long outputSize,
                    int numRegs, int & regCtr)
{
  long sizeTemp;  // Holder for current size calculation.

  // Region 0 is between, which has a starting index equal to
  // the input region starting index, unless that would be
  // outside the bounds of the output image.
  if ( inputIndex > outputIndex )
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
  if ( ( inputIndex + inputSize )
       < ( outputIndex + outputSize ) )
    {
    sizeTemp = inputIndex + inputSize - outputRegionStart[0];
    }
  else
    {
    sizeTemp = outputIndex + outputSize - outputRegionStart[0];
    }
  outputRegionSizes[0] = ( ( sizeTemp > 0 ) ? sizeTemp : 0 );
  inputRegionSizes[0] = ( ( sizeTemp > 0 ) ? sizeTemp : 0 );

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
template< typename TInputImage, typename TOutputImage >
int
MirrorPadImageFilter< TInputImage, TOutputImage >
::BuildPreRegions(std::vector< long > & inputRegionStart,
                  std::vector< long > & outputRegionStart,
                  std::vector< long > & inputRegionSizes,
                  std::vector< long > & outputRegionSizes,
                  long inputIndex, long outputIndex,
                  long inputSize, long outputSize,
                  int numRegs, int & regCtr)
{
  long sizeTemp;  // Holder for current size calculation.
  int  ctr;       // Generic loop counter.
  long offset;    // Offset for times when we need to shorten both ends.

  // Handle the pre-region.  Within the pre-region, the first and last
  // groups may be truncated and only contain the back part of the input
  // data.  All other regions will be complete copies of the input.
  outputRegionStart[regCtr] = outputIndex;

  // Size of the pre-region is all the output that precedes the input,
  // all except the first (and possibly the last) will be the size of
  // the input.
  sizeTemp = outputRegionStart[0] - outputIndex;
  sizeTemp = ( ( sizeTemp > 0 ) ? ( sizeTemp % inputSize ) : 0 );
  outputRegionSizes[regCtr] = sizeTemp;
  inputRegionSizes[regCtr] = sizeTemp;
  offset = inputSize - sizeTemp;
  if ( ( sizeTemp == 0 ) || this->RegionIsOdd(inputIndex, outputIndex, inputSize) )
    {
    inputRegionStart[regCtr] = inputIndex;
    }
  else
    {
    inputRegionStart[regCtr] = inputIndex + offset;
    }
  // Handle the rest of the pre-region by stepping through in blocks of
  // the size of the input image.
  for ( ctr = 1; ctr < numRegs; ctr++ )
    {
    regCtr++;
    offset = 0;
    outputRegionStart[regCtr] = outputRegionStart[regCtr - 1]
                                + static_cast< long >( outputRegionSizes[regCtr - 1] );
    inputRegionStart[regCtr] = inputIndex;
    outputRegionSizes[regCtr] = inputSize;
    inputRegionSizes[regCtr] = inputSize;
    }
  // Fix size on last region, if necessary.
  if ( ( outputRegionStart[regCtr] + static_cast< long >( outputRegionSizes[regCtr] ) )
       > ( outputIndex + outputSize ) )
    {
    outputRegionSizes[regCtr] = outputIndex + outputSize
                                - outputRegionStart[regCtr];
    inputRegionSizes[regCtr] = outputRegionSizes[regCtr];
    if ( ( inputRegionSizes[regCtr] < inputSize )
         && this->RegionIsOdd(inputIndex, outputRegionStart[regCtr], inputSize) )
      {
      inputRegionStart[regCtr] = inputIndex + inputSize - inputRegionSizes[regCtr] - offset;
      }
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
template< typename TInputImage, typename TOutputImage >
int
MirrorPadImageFilter< TInputImage, TOutputImage >
::BuildPostRegions(std::vector< long > & inputRegionStart,
                   std::vector< long > & outputRegionStart,
                   std::vector< long > & inputRegionSizes,
                   std::vector< long > & outputRegionSizes,
                   long inputIndex, long outputIndex,
                   long inputSize, long outputSize,
                   int numRegs, int & regCtr)
{
  long sizeTemp;  // Holder for current size calculation.
  int  ctr;       // Generic loop counter.
  int  offset;    // Offset for when we have to shorten both ends.

  // Handle the post region.  The post region has a number of
  // areas of size equal to the input region, followed by one
  // region of possibly smaller size.
  regCtr++;
  sizeTemp = outputIndex + outputSize - inputIndex - inputSize;
  sizeTemp = ( ( sizeTemp > 0 ) ? ( sizeTemp % inputSize ) : 0 );
  outputRegionSizes[regCtr] = sizeTemp;
  inputRegionSizes[regCtr] = sizeTemp;
  outputRegionStart[regCtr] = outputIndex + outputSize - sizeTemp;
  offset = inputSize - sizeTemp;
  if ( ( sizeTemp > 0 ) && this->RegionIsOdd(inputIndex, outputRegionStart[regCtr], inputSize) )
    {
    inputRegionStart[regCtr] = inputIndex + offset;
    }
  else
    {
    inputRegionStart[regCtr] = inputIndex;
    }

  for ( ctr = numRegs - 1; ctr >= 1; ctr-- )
    {
    offset = 0;
    regCtr++;
    outputRegionStart[regCtr] = outputRegionStart[regCtr - 1] - inputSize;
    inputRegionStart[regCtr] = inputIndex;
    outputRegionSizes[regCtr] = inputSize;
    inputRegionSizes[regCtr] = inputSize;
    }
  // Fix size on last region, if necessary.
  if ( outputRegionStart[regCtr] < outputIndex )
    {
    sizeTemp = outputIndex - outputRegionStart[regCtr];
    outputRegionStart[regCtr] += sizeTemp;
    if ( this->RegionIsOdd(inputIndex, outputRegionStart[regCtr], inputSize)
         && ( outputIndex > ( inputIndex + inputSize ) ) )
      {
      inputRegionStart[regCtr] = inputIndex + offset;
      }
    else
      {
      inputRegionStart[regCtr] += sizeTemp;
      }
    outputRegionSizes[regCtr] -= sizeTemp;
    inputRegionSizes[regCtr] = outputRegionSizes[regCtr];
    }

  return regCtr;
}

/**
 * MirrorPadImageFilter needs a different input requested region than
 * output requested region.  As such, MirrorPadImageFilter needs to
 * provide an implementation for GenerateInputRequestedRegion() in
 * order to inform the pipeline execution model.
 *
 * \sa PadImageFilter::GenerateInputRequestedRegion()
 * \sa ProcessObject::GenerateInputRequestedRegion()
 */
template< typename TInputImage, typename TOutputImage >
void
MirrorPadImageFilter< TInputImage, TOutputImage >
::GenerateInputRequestedRegion()
{
  unsigned int dimCtr;
  int          regCtr;

  // call the superclass' implementation of this method
  // Superclass::GenerateInputRequestedRegion();

  // get pointers to the input and output
  typename Superclass::InputImagePointer inputPtr =
    const_cast< InputImageType * >( this->GetInput() );
  typename Superclass::OutputImagePointer outputPtr = this->GetOutput();

  if ( !inputPtr || !outputPtr )
    {
    return;
    }

  // Define a few indices that will be used to translate from an input pixel
  // to an output pixel
  OutputImageIndexType outputIndex = outputPtr->GetRequestedRegion().GetIndex();
  InputImageIndexType  inputIndex = inputPtr->GetLargestPossibleRegion().GetIndex();
  OutputImageSizeType  outputSize = outputPtr->GetRequestedRegion().GetSize();
  InputImageSizeType   inputSize = inputPtr->GetLargestPossibleRegion().GetSize();

  OutputImageRegionType outputRegion;
  InputImageRegionType  inputRegion;

  // For n dimensions, there are k^n combinations of before, between, and
  // after on these regions.  We are keeping this flexible so that we
  // can handle other blockings imposed by the mirror and wrap algorithms.
  long                inRegLimit[ImageDimension];
  long                outRegLimit[ImageDimension];
  long                minIndex[ImageDimension], maxIndex[ImageDimension];
  int                 numPre[ImageDimension], numPost[ImageDimension], numIn[ImageDimension];
  std::vector< long > outputRegionStart[ImageDimension];
  std::vector< long > outputRegionSizes[ImageDimension];
  std::vector< long > inputRegionStart[ImageDimension];
  std::vector< long > inputRegionSizes[ImageDimension];

  // Calculate the actual number of regions for each dimension,
  // and set up the required variables here.
  for ( dimCtr = 0; dimCtr < ImageDimension; dimCtr++ )
    {
    numIn[dimCtr] = 1;  // Always assume exactly one inter region.
    numPre[dimCtr] =    // Count how many versions of input fit in pre-pad
                     this->FindRegionsInArea( outputIndex[dimCtr], inputIndex[dimCtr],
                                              static_cast< long >( inputSize[dimCtr] ),
                                              inputIndex[dimCtr] - outputIndex[dimCtr]
                                              - static_cast< long >( outputSize[dimCtr] ) );
    numPost[dimCtr] =   // Count how many versions of input fit in post-pad
                      this->FindRegionsInArea( inputIndex[dimCtr] + static_cast< long >( inputSize[dimCtr] ),
                                               outputIndex[dimCtr] + static_cast< long >( outputSize[dimCtr] ),
                                               static_cast< long >( inputSize[dimCtr] ),
                                               outputIndex[dimCtr] - inputIndex[dimCtr]
                                               - static_cast< long >( inputSize[dimCtr] ) );
    inRegLimit[dimCtr] = numPre[dimCtr] + numIn[dimCtr] + numPost[dimCtr];
    outRegLimit[dimCtr] = numPre[dimCtr] + numIn[dimCtr] + numPost[dimCtr];
    outputRegionStart[dimCtr].resize(outRegLimit[dimCtr]);
    outputRegionSizes[dimCtr].resize(outRegLimit[dimCtr]);
    inputRegionStart[dimCtr].resize(inRegLimit[dimCtr]);
    inputRegionSizes[dimCtr].resize(inRegLimit[dimCtr]);
    }

  //
  // Generate the break points for the image regions we counted in the
  // previous loop.
  //
  for ( dimCtr = 0; dimCtr < ImageDimension; dimCtr++ )
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
                               static_cast< long >( inputSize[dimCtr] ),
                               static_cast< long >( outputSize[dimCtr] ),
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
                             static_cast< long >( inputSize[dimCtr] ),
                             static_cast< long >( outputSize[dimCtr] ),
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
                              static_cast< long >( inputSize[dimCtr] ),
                              static_cast< long >( outputSize[dimCtr] ),
                              numPost[dimCtr], regCtr);
    }

  //
  // Pick the indices which span the largest input region we need for this
  // output region.
  //
  for ( dimCtr = 0; dimCtr < ImageDimension; dimCtr++ )
    {
    minIndex[dimCtr] = inputRegionStart[dimCtr][0];
    maxIndex[dimCtr] = minIndex[dimCtr] + static_cast< long >( inputRegionSizes[dimCtr][0] );

    for ( regCtr = 1;
          regCtr < ( numIn[dimCtr] + numPre[dimCtr] + numPost[dimCtr] );
          regCtr++ )
      {
      if ( minIndex[dimCtr] == maxIndex[dimCtr] )
        {
        minIndex[dimCtr] = inputRegionStart[dimCtr][regCtr];
        maxIndex[dimCtr] = minIndex[dimCtr]
                           + static_cast< long >( inputRegionSizes[dimCtr][regCtr] );
        }
      else
        {
        if ( inputRegionStart[dimCtr][regCtr] < minIndex[dimCtr] )
          {
          minIndex[dimCtr] = inputRegionStart[dimCtr][regCtr];
          }
        if ( ( inputRegionStart[dimCtr][regCtr]
               + static_cast< long >( inputRegionSizes[dimCtr][regCtr] ) )
             > maxIndex[dimCtr] )
          {
          maxIndex[dimCtr] = inputRegionStart[dimCtr][regCtr]
                             + static_cast< long >( inputRegionSizes[dimCtr][regCtr] );
          }
        }
      }
    }

  typename TInputImage::SizeType inputRequestedRegionSize;
  typename TInputImage::IndexType inputRequestedRegionStartIndex;
  for ( dimCtr = 0; dimCtr < ImageDimension; dimCtr++ )
    {
    inputRequestedRegionStartIndex[dimCtr] = minIndex[dimCtr];
    inputRequestedRegionSize[dimCtr] = maxIndex[dimCtr] - minIndex[dimCtr];
    }

  typename TInputImage::RegionType inputRequestedRegion;
  inputRequestedRegion.SetSize(inputRequestedRegionSize);
  inputRequestedRegion.SetIndex(inputRequestedRegionStartIndex);

  inputPtr->SetRequestedRegion(inputRequestedRegion);
}

/**
 *
 */
template< typename TInputImage, typename TOutputImage >
void
MirrorPadImageFilter< TInputImage, TOutputImage >
::ThreadedGenerateData(const OutputImageRegionType & outputRegionForThread,
                       ThreadIdType threadId)
{
  unsigned int dimCtr, i;
  int          regCtr;
  int          numRegions = 1; // number of regions in our decomposed space.
  int          goodInput, goodOutput;

  // Are the regions non-empty?

  itkDebugMacro(<< "Actually executing");

  // Get the input and output pointers
  typename Superclass::InputImageConstPointer inputPtr = this->GetInput();
  typename Superclass::OutputImagePointer outputPtr = this->GetOutput();

  // Define a few indices that will be used to translate from an input pixel
  // to an output pixel
  OutputImageIndexType outputIndex = outputRegionForThread.GetIndex();
  InputImageIndexType  inputIndex =
    inputPtr->GetLargestPossibleRegion().GetIndex();
  OutputImageSizeType outputSize = outputRegionForThread.GetSize();
  InputImageSizeType  inputSize =
    inputPtr->GetLargestPossibleRegion().GetSize();

  OutputImageRegionType outputRegion;
  InputImageRegionType  inputRegion;

  // For n dimensions, there are k^n combinations of before, between, and
  // after on these regions.  We are keeping this flexible so that we
  // can handle other blockings imposed by the mirror and wrap algorithms.
  long                inRegIndices[ImageDimension];
  long                inRegLimit[ImageDimension];
  long                outRegIndices[ImageDimension];
  long                outRegLimit[ImageDimension];
  int                 numPre[ImageDimension], numPost[ImageDimension], numIn[ImageDimension];
  std::vector< long > outputRegionStart[ImageDimension];
  std::vector< long > outputRegionSizes[ImageDimension];
  std::vector< long > inputRegionStart[ImageDimension];
  std::vector< long > inputRegionSizes[ImageDimension];

  // Calculate the actual number of regions for each dimension,
  // and set up the required variables here.
  for ( dimCtr = 0; dimCtr < ImageDimension; dimCtr++ )
    {
    numIn[dimCtr] = 1;  // Always assume exactly one inter region.
    numPre[dimCtr] =    // Count how many versions of input fit in pre-pad
                     this->FindRegionsInArea( outputIndex[dimCtr], inputIndex[dimCtr],
                                              static_cast< long >( inputSize[dimCtr] ),
                                              inputIndex[dimCtr] - outputIndex[dimCtr]
                                              - static_cast< long >( outputSize[dimCtr] ) );
    numPost[dimCtr] =   // Count how many versions of input fit in post-pad
                      this->FindRegionsInArea( inputIndex[dimCtr] + static_cast< long >( inputSize[dimCtr] ),
                                               outputIndex[dimCtr] + static_cast< long >( outputSize[dimCtr] ),
                                               static_cast< long >( inputSize[dimCtr] ),
                                               outputIndex[dimCtr] - inputIndex[dimCtr]
                                               - static_cast< long >( inputSize[dimCtr] ) );
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
  for ( dimCtr = 0; dimCtr < ImageDimension; dimCtr++ )
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
                               static_cast< long >( inputSize[dimCtr] ),
                               static_cast< long >( outputSize[dimCtr] ),
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
                             static_cast< long >( inputSize[dimCtr] ),
                             static_cast< long >( outputSize[dimCtr] ),
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
                              static_cast< long >( inputSize[dimCtr] ),
                              static_cast< long >( outputSize[dimCtr] ),
                              numPost[dimCtr], regCtr);
    }

  // support progress methods/callbacks
  ProgressReporter progress( this, threadId, outputRegionForThread.GetNumberOfPixels() );

  // Define/declare iterators that will walk the input and output regions
  // for this thread.
  typedef ImageRegionIterator< TOutputImage >     OutputIterator;
  typedef ImageRegionConstIterator< TInputImage > InputIterator;

  int                  oddRegionArray[ImageDimension];
  OutputImageIndexType currentOutputIndex;
  InputImageIndexType  currentInputIndex;

  i = 0;

  // Now walk the regions.
  for ( regCtr = 0; regCtr < numRegions; regCtr++ )
    {
    // If both a valid output and input region are defined for the particular
    // defined region, then copy the input values to the output values.
    goodOutput = this->GenerateNextOutputRegion
                   (outRegIndices, outRegLimit, outputRegionStart,
                   outputRegionSizes, outputRegion);
    goodInput = this->GenerateNextInputRegion
                  (inRegIndices, inRegLimit, inputRegionStart,
                  inputRegionSizes, inputRegion);
    if ( goodInput && goodOutput )
      {
      for ( dimCtr = 0; dimCtr < ImageDimension; dimCtr++ )
        {
        oddRegionArray[dimCtr] =
          RegionIsOdd( inputIndex[dimCtr],
                       outputRegion.GetIndex()[dimCtr],
                       static_cast< long >( inputSize[dimCtr] ) );
        }

      OutputIterator outIt = OutputIterator(outputPtr, outputRegion);
      InputIterator  inIt  = InputIterator(inputPtr, inputRegion);

      // Do the actual copy of the input pixels to the output
      // pixels here.
      for (; !outIt.IsAtEnd(); ++outIt, i++, ++inIt )
        {
        currentOutputIndex = outIt.GetIndex();
        this->ConvertOutputIndexToInputIndex(currentOutputIndex,
                                             currentInputIndex,
                                             outputRegion,
                                             inputRegion,
                                             oddRegionArray);
        inIt.SetIndex(currentInputIndex);
        // copy the input pixel to the output
        outIt.Set( inIt.Get() );
        progress.CompletedPixel();
        }
      }
    }
}
} // end namespace itk

#endif
