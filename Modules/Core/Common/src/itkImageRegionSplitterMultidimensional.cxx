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

#include "itkImageRegionSplitterMultidimensional.h"

namespace itk
{

/**
 *
 */
ImageRegionSplitterMultidimensional
::ImageRegionSplitterMultidimensional()
{
}

void
ImageRegionSplitterMultidimensional
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}

unsigned int
ImageRegionSplitterMultidimensional
::GetNumberOfSplitsInternal(unsigned int dim,
                            const IndexValueType regionIndex[],
                            const SizeValueType regionSize[],
                            unsigned int requestedNumber) const
{
  // number of splits in each dimension
  std::vector<unsigned int> splits(dim); // Note: stack allocation preferred

  unsigned int numberOfPieces = this->ComputeSplits(dim, requestedNumber, regionIndex, regionSize, &splits[0]);

  return numberOfPieces;
}

unsigned int
ImageRegionSplitterMultidimensional
::GetSplitInternal(unsigned int dim,
                   unsigned int splitI,
                   unsigned int numberOfPieces,
                   IndexValueType regionIndex[],
                   SizeValueType regionSize[]) const
{
  // number of splits in each dimension
  std::vector<unsigned int> splits(dim); // Note: stack allocation preferred

  // index into splitted regions
  std::vector<unsigned int> splittedRegionIndex(dim);  // Note: stack allocation preferred

  numberOfPieces = this->ComputeSplits(dim, numberOfPieces, regionIndex, regionSize, &splits[0]);

  // determine which splitted region we are in
  unsigned int offset = splitI;
  for ( unsigned i = dim - 1; i > 0; --i )
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


  // Assign the output split region to the input region in-place
  for ( unsigned int i = 0; i < dim; i++ )
    {
    const SizeValueType inputRegionSize = regionSize[i];
    const IndexValueType indexOffset = Math::Floor<IndexValueType>( ( splittedRegionIndex[i] ) * ( inputRegionSize / double(splits[i]) ) );

    regionIndex[i] += indexOffset;
    if ( splittedRegionIndex[i] < splits[i] - 1 )
      {
      regionSize[i] = Math::Floor<SizeValueType>( ( splittedRegionIndex[i] + 1 ) * ( inputRegionSize / double(splits[i]) ) ) - indexOffset;
      }
    else
      {
      // this dimension is falling off the edge of the image
      regionSize[i] = regionSize[i] - indexOffset;
      }
    }

  return numberOfPieces;
}


/**
 * given the requestedNumber of regions to split the "region" argument
 * into, it retures the number of splitted regions in each dimension
 * as "splits" and returns the total number of splitted regions
 */
unsigned int
ImageRegionSplitterMultidimensional
::ComputeSplits(unsigned int dim,
                unsigned int requestedNumber,
                const IndexValueType itkNotUsed(regionIndex)[],
                const SizeValueType regionSize[],
                unsigned int splits[])
{
  // size of each splited region
  std::vector<double> splitRegionSize(dim); // Note: stack allocation preferred
  unsigned int numberOfPieces = 1;

  // initialize arrays
  for ( unsigned int i = 0; i < dim; ++i )
    {
    splits[i] = 1;
    splitRegionSize[i] = regionSize[i];
    }

  while ( true )
    {
    // find the dimension with the largest size
    unsigned int maxSplitDim = 0;
    for ( unsigned int i = 1; i < dim; ++i )
      {
      if ( splitRegionSize[maxSplitDim] < splitRegionSize[i] )
        {
        maxSplitDim = i;
        }
      }

    // calculate the number of additional pieces this split would add
    unsigned int additionalNumPieces = 1;
    for ( unsigned int i = 0; i < dim; ++i )
      {
      if ( i != maxSplitDim )
        {
        additionalNumPieces *= splits[i];
        }
      }

    // check if this would give us too many pieces or
    // if this would require the subpixel splits
    if ( numberOfPieces + additionalNumPieces > requestedNumber
         || splits[maxSplitDim] == regionSize[maxSplitDim] )
      {
      return numberOfPieces;
      }

    // update the variable with the new split
    numberOfPieces += additionalNumPieces;
    ++splits[maxSplitDim];
    splitRegionSize[maxSplitDim] = regionSize[maxSplitDim] / double(splits[maxSplitDim]);
    }
}

} // end namespace itk
