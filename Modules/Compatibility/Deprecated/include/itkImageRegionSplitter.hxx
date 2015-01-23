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
#ifndef itkImageRegionSplitter_hxx
#define itkImageRegionSplitter_hxx

#include "itkImageRegionSplitter.h"
#include <cmath>

namespace itk
{
/**
 *
 */
template< unsigned int VImageDimension >
unsigned int
ImageRegionSplitter< VImageDimension >
::GetNumberOfSplits(const RegionType & region, unsigned int requestedNumber)
{
  const SizeType & regionSize = region.GetSize();

  // split on the outermost dimension available
  int splitAxis = VImageDimension - 1;
  while ( regionSize[splitAxis] == 1 )
    {
    --splitAxis;
    if ( splitAxis < 0 )
      { // cannot split
      itkDebugMacro("  Cannot Split");
      return 1;
      }
    }

  // determine the actual number of pieces that will be generated
  const SizeValueType range = regionSize[splitAxis];
  const unsigned int valuesPerPiece = Math::Ceil< unsigned int >(range / (double)requestedNumber);
  const unsigned int maxPieceUsed = Math::Ceil< unsigned int >(range / (double)valuesPerPiece) - 1;

  return maxPieceUsed + 1;
}

/**
 *
 */
template< unsigned int VImageDimension >
ImageRegion< VImageDimension >
ImageRegionSplitter< VImageDimension >
::GetSplit(unsigned int i, unsigned int numberOfPieces,
           const RegionType & region)
{
  int        splitAxis;
  RegionType splitRegion;
  IndexType  splitIndex;
  SizeType   splitSize, regionSize;

  // Initialize the splitRegion to the requested region
  splitRegion = region;
  splitIndex = splitRegion.GetIndex();
  splitSize = splitRegion.GetSize();

  regionSize = region.GetSize();

  // split on the outermost dimension available
  splitAxis = VImageDimension - 1;
  while ( regionSize[splitAxis] == 1 )
    {
    --splitAxis;
    if ( splitAxis < 0 )
      { // cannot split
      itkDebugMacro("  Cannot Split");
      return splitRegion;
      }
    }

  // determine the actual number of pieces that will be generated
  SizeValueType range = regionSize[splitAxis];
  int           valuesPerPiece = Math::Ceil< int >(range / (double)numberOfPieces);
  int           maxPieceUsed = Math::Ceil< int >(range / (double)valuesPerPiece) - 1;

  // Split the region
  if ( (int)i < maxPieceUsed )
    {
    splitIndex[splitAxis] += i * valuesPerPiece;
    splitSize[splitAxis] = valuesPerPiece;
    }
  if ( (int)i == maxPieceUsed )
    {
    splitIndex[splitAxis] += i * valuesPerPiece;
    // last piece needs to process the "rest" dimension being split
    splitSize[splitAxis] = splitSize[splitAxis] - i * valuesPerPiece;
    }

  // set the split region ivars
  splitRegion.SetIndex(splitIndex);
  splitRegion.SetSize(splitSize);

  itkDebugMacro("  Split Piece: " << splitRegion);

  return splitRegion;
}

/**
 *
 */
template< unsigned int VImageDimension >
void
ImageRegionSplitter< VImageDimension >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}
} // end namespace itk

#endif
