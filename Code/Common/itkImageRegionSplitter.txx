/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageRegionSplitter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkImageRegionSplitter_txx
#define __itkImageRegionSplitter_txx

#include "itkImageRegionSplitter.h"
#include <math.h>

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
  int              splitAxis;
  const SizeType & regionSize = region.GetSize();

  // split on the outermost dimension available
  splitAxis = VImageDimension - 1;
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
  SizeValueType range = regionSize[splitAxis];
  int           valuesPerPiece = Math::Ceil< int >(range / (double)requestedNumber);
  int           maxPieceUsed = Math::Ceil< int >(range / (double)valuesPerPiece) - 1;

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
