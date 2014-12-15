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
#ifndef itkImageRegionMultidimensionalSplitter_hxx
#define itkImageRegionMultidimensionalSplitter_hxx
#include "itkImageRegionMultidimensionalSplitter.h"
#include <cmath>

namespace itk
{
/**
 *
 */
template< unsigned int VImageDimension >
unsigned int
ImageRegionMultidimensionalSplitter< VImageDimension >
::GetNumberOfSplits(const RegionType & region, unsigned int requestedNumber)
{
  unsigned int splits[VImageDimension]; // number of splits in each dimension

  unsigned int numPieces = this->ComputeSplits(requestedNumber, region, splits);

  return numPieces;
}

/**
 *
 */
template< unsigned int VImageDimension >
ImageRegion< VImageDimension >
ImageRegionMultidimensionalSplitter< VImageDimension >
::GetSplit(unsigned int splitI, unsigned int numberOfPieces,
           const RegionType & region)
{
  RegionType splitRegion;
  IndexType  splitIndex;
  SizeType   splitSize, regionSize;

  // Initialize the splitRegion to the requested region
  splitRegion = region;
  splitIndex = splitRegion.GetIndex();
  splitSize = splitRegion.GetSize();
  regionSize = region.GetSize();

  unsigned int splits[VImageDimension]; // number of splits in each
                                        // dimension

  if ( numberOfPieces != this->ComputeSplits(numberOfPieces, region, splits) )
    {
    itkExceptionMacro( "numberOfPieces did not match GetNumberOfSplits. Expected: "
                       << numberOfPieces << " but got"
                       << this->ComputeSplits(numberOfPieces, region, splits) );
    }

  unsigned int splittedRegionIndex[VImageDimension]; // index into splitted
                                                     // regions
  unsigned int i;

  // determine which splitted region we are in
  unsigned int offset = splitI;
  for ( i = VImageDimension - 1; i > 0; i-- )
    {
    unsigned int dimensionOffset = 1;
    for ( unsigned int j = 0; j < i; ++j )
      {
      dimensionOffset *= splits[j];
      }

    splittedRegionIndex[i] = offset / dimensionOffset;
    offset -= ( splittedRegionIndex[i] * dimensionOffset );
    }
  splittedRegionIndex[0] = offset;

  // compute the region size and index
  for ( i = 0; i < VImageDimension; i++ )
    {
    splitIndex[i] += Math::Floor<IndexValueType>( ( splittedRegionIndex[i] ) * ( regionSize[i] / double(splits[i]) ) );
    if ( splittedRegionIndex[i] < splits[i] - 1 )
      {
      splitSize[i] = Math::Floor<SizeValueType>( ( splittedRegionIndex[i] + 1 ) * ( regionSize[i] / double(splits[i]) ) )
                     - splitIndex[i];
      }
    else
      {
      // this dimension is falling off the edge of the image
      splitSize[i] = regionSize[i] - splitIndex[i];
      }
    }

  // set the split region ivars
  splitRegion.SetIndex(splitIndex);
  splitRegion.SetSize(splitSize);

  itkDebugMacro("  Split Piece: " << std::endl << splitRegion);

  return splitRegion;
}

/**
 *
 */
template< unsigned int VImageDimension >
void
ImageRegionMultidimensionalSplitter< VImageDimension >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}

/**
 * given the requestedNumber of regions to split the "region" argument
 * into, it retures the number of splitted regions in each dimension
 * as "splits" and returns the total number of splitted regions
 */
template< unsigned int VImageDimension >
unsigned int
ImageRegionMultidimensionalSplitter< VImageDimension >
::ComputeSplits(unsigned int requestedNumber,
                const RegionType & region,
                unsigned int splits[])
{
  const SizeType & regionSize = region.GetSize();
  double           splitRegionSize[VImageDimension]; // size of each splited
                                                     // region
  unsigned int i;
  unsigned int numPieces = 1;

  // initialize arrays
  for ( i = 0; i < VImageDimension; ++i )
    {
    splits[i] = 1;
    splitRegionSize[i] = regionSize[i];
    }

  while ( true )
    {
    // find the dimension with the largest size
    unsigned int maxSplitDim = 0;
    for ( i = 1; i < VImageDimension; ++i )
      {
      if ( splitRegionSize[maxSplitDim] < splitRegionSize[i] )
        {
        maxSplitDim = i;
        }
      }

    // calculate the number of additional pieces this split would add
    unsigned int additionalNumPieces = 1;
    for ( i = 0; i < VImageDimension; ++i )
      {
      if ( i != maxSplitDim )
        {
        additionalNumPieces *= splits[i];
        }
      }

    // check if this would give us too many pieces or
    // if this would require the subpixel splits
    if ( numPieces + additionalNumPieces > requestedNumber
         || splits[maxSplitDim] == regionSize[maxSplitDim] )
      {
      return numPieces;
      }

    // update the variable with the new split
    numPieces += additionalNumPieces;
    ++splits[maxSplitDim];
    splitRegionSize[maxSplitDim] = regionSize[maxSplitDim] / double(splits[maxSplitDim]);
    }
}
} // end namespace itk

#endif
