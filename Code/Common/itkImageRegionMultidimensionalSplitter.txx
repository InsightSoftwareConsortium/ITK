/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageRegionMultidimensionalSplitter.txx
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
#ifndef _itkImageRegionMultidimensionalSplitter_txx
#define _itkImageRegionMultidimensionalSplitter_txx
#include "itkImageRegionMultidimensionalSplitter.h"

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
