/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageRegionMultidimensionalSplitter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkImageRegionMultidimensionalSplitter_txx
#define _itkImageRegionMultidimensionalSplitter_txx
#include "itkImageRegionMultidimensionalSplitter.h"
#include <math.h>

namespace itk
{

/**
 *
 */
template <unsigned int VImageDimension>
unsigned int 
ImageRegionMultidimensionalSplitter<VImageDimension>
::GetNumberOfSplits(const RegionType &region, unsigned int requestedNumber)
{
  const SizeType &regionSize = region.GetSize();

  // requested number of splits per dimension
  double splitsPerDimension =
    ceil( pow( (double) requestedNumber, 1.0/(double) VImageDimension));

  // if a given dimension has fewer pixels that splitsPerDimension, then
  // only split number of pixels times
  unsigned int i, numPieces;
  numPieces = 1;
  for (i=0; i < VImageDimension; ++i)
    {
    if (regionSize[i] < splitsPerDimension)
      {
      numPieces *= regionSize[i];
      }
    else
      {
      numPieces *= (unsigned int) splitsPerDimension;
      }
    }

  return numPieces;
}

  
/**
 *
 */
template <unsigned int VImageDimension>
ImageRegion<VImageDimension>
ImageRegionMultidimensionalSplitter<VImageDimension>
::GetSplit(unsigned int i, unsigned int numberOfPieces,
           const RegionType &region)
{
  RegionType splitRegion;
  IndexType splitIndex;
  SizeType splitSize, regionSize;
  
  // Initialize the splitRegion to the requested region
  splitRegion = region;
  splitIndex = splitRegion.GetIndex();
  splitSize = splitRegion.GetSize();

  regionSize = region.GetSize();
  
  // requested number of splits per dimension
  double splitsPerDimension =
    ceil( pow( (double) numberOfPieces, 1.0/(double) VImageDimension));

  // if a given dimension has fewer pixels that splitsPerDimension, then
  // only split number of pixels times
  unsigned int splits[VImageDimension], pixelsPerSplit[VImageDimension];
  unsigned int j, numPieces;
  unsigned int ijk[VImageDimension];
  unsigned int offsetTable[VImageDimension];
  numPieces = 1;
  for (j=0; j < VImageDimension; ++j)
    {
    offsetTable[j] = numPieces;
    if (regionSize[j] < splitsPerDimension)
      {
      splits[j] = regionSize[j];
      pixelsPerSplit[j] = 1;
      numPieces *= regionSize[j];
      }
    else
      {
      splits[j] = (unsigned int) splitsPerDimension;
      pixelsPerSplit[j] = (unsigned int) ceil(regionSize[j]
                                              / (double) splits[j]);
      numPieces *= (unsigned int) splitsPerDimension;
      }
    }

  // determine which split we are in
  unsigned int offset = i;
  for (int j=VImageDimension-1; j > 0; j--)
    {
    ijk[j] = offset / offsetTable[j];
    offset -= (ijk[j] * offsetTable[j]);
    }
  ijk[0] = offset;

  // compute the split
  for (j=0; j < VImageDimension; j++)
    {
    splitIndex[j] += ijk[j]*pixelsPerSplit[j];
    if (ijk[j] < splits[j] - 1)
      {
      splitSize[j] = pixelsPerSplit[j];
      }
    else
      {
      // this dimension is falling off the edge of the image
      splitSize[j] = splitSize[j] - ijk[j]*pixelsPerSplit[j];
      }
    }
  
  // set the split region ivars
  splitRegion.SetIndex( splitIndex );
  splitRegion.SetSize( splitSize );

  itkDebugMacro("  Split Piece: " << std::endl << splitRegion );

  return splitRegion;
}
  
  

/**
 *
 */
template <unsigned int VImageDimension>
void 
ImageRegionMultidimensionalSplitter<VImageDimension>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
}


} // end namespace itk

#endif
