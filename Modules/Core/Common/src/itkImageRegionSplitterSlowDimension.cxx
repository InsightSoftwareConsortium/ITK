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

#include "itkImageRegionSplitterSlowDimension.h"


namespace itk
{

ImageRegionSplitterSlowDimension
::ImageRegionSplitterSlowDimension()
{
}

unsigned int
ImageRegionSplitterSlowDimension
::GetNumberOfSplitsInternal(unsigned int dim,
                            const IndexValueType itkNotUsed(regionIndex)[],
                            const SizeValueType regionSize[],
                            unsigned int requestedNumber) const
{
  // split on the outermost dimension available
  int splitAxis = dim - 1;
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
  const double range = regionSize[splitAxis];
  const unsigned int valuesPerPiece = Math::Ceil< unsigned int >(range / (double)requestedNumber);
  const unsigned int maxPieceUsed = Math::Ceil< unsigned int >(range / (double)valuesPerPiece) - 1;

  return maxPieceUsed + 1;
}

unsigned int
ImageRegionSplitterSlowDimension
::GetSplitInternal(unsigned int dim,
                   unsigned int i,
                   unsigned int numberOfPieces,
                   IndexValueType regionIndex[],
                   SizeValueType regionSize[]) const
{

  // split on the outermost dimension available
  unsigned int splitAxis = dim - 1;
  while ( regionSize[splitAxis] == 1 )
    {
    if ( splitAxis == 0 )
      { // cannot split
      itkDebugMacro("  Cannot Split");
      return 1;
      }
    --splitAxis;
    }

  // determine the actual number of pieces that will be generated
  const double range=static_cast<double>(regionSize[splitAxis]);
  const unsigned int valuesPerPiece = Math::Ceil< unsigned int >(range / static_cast<double>(numberOfPieces));
  const unsigned int maxPieceIdUsed = Math::Ceil< unsigned int >(range / static_cast<double>(valuesPerPiece)) - 1;

  // Split the region
  if ( i < maxPieceIdUsed )
    {
    regionIndex[splitAxis] += i * valuesPerPiece;
    regionSize[splitAxis] = valuesPerPiece;
    }
  if ( i == maxPieceIdUsed )
    {
    regionIndex[splitAxis] += i * valuesPerPiece;
    // last piece needs to process the "rest" dimension being split
    regionSize[splitAxis] = regionSize[splitAxis] - i * valuesPerPiece;
    }

  return maxPieceIdUsed + 1;

}

}
