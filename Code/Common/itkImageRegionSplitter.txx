/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageRegionSplitter.txx
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
#ifndef _itkImageRegionSplitter_txx
#define _itkImageRegionSplitter_txx
#include "itkImageRegionSplitter.h"

namespace itk
{

/**
 *
 */
template <unsigned int VImageDimension>
unsigned int 
ImageRegionSplitter<VImageDimension>
::GetNumberOfSplits(const RegionType &region, unsigned int requestedNumber)
{
  int splitAxis;
  const SizeType &regionSize = region.GetSize();

  // split on the outermost dimension available
  splitAxis = VImageDimension - 1;
  while (regionSize[splitAxis] == 1)
    {
    --splitAxis;
    if (splitAxis < 0)
      { // cannot split
      itkDebugMacro("  Cannot Split");
      return 1;
      }
    }

  // determine the actual number of pieces that will be generated
  int range = regionSize[splitAxis];
  int valuesPerPiece = (int)ceil(range/(double)requestedNumber);
  int maxPieceUsed = (int)ceil(range/(double)valuesPerPiece) - 1;

  return maxPieceUsed + 1;
}

  
/**
 *
 */
template <unsigned int VImageDimension>
ImageRegion<VImageDimension>
ImageRegionSplitter<VImageDimension>
::GetSplit(unsigned int i, unsigned int numberOfPieces,
           const RegionType &region)
{
  int splitAxis;
  RegionType splitRegion;
  IndexType splitIndex;
  SizeType splitSize, regionSize;
  
  // Initialize the splitRegion to the requested region
  splitRegion = region;
  splitIndex = splitRegion.GetIndex();
  splitSize = splitRegion.GetSize();

  regionSize = region.GetSize();
  
  // split on the outermost dimension available
  splitAxis = VImageDimension - 1;
  while (regionSize[splitAxis] == 1)
    {
    --splitAxis;
    if (splitAxis < 0)
      { // cannot split
      itkDebugMacro("  Cannot Split");
      return splitRegion;
      }
    }

  // determine the actual number of pieces that will be generated
  int range = regionSize[splitAxis];
  int valuesPerPiece = (int)ceil(range/(double)numberOfPieces);
  int maxPieceUsed = (int)ceil(range/(double)valuesPerPiece) - 1;

  // Split the region
  if (i < maxPieceUsed)
    {
    splitIndex[splitAxis] += i*valuesPerPiece;
    splitSize[splitAxis] = valuesPerPiece;
    }
  if (i == maxPieceUsed)
    {
    splitIndex[splitAxis] += i*valuesPerPiece;
    // last piece needs to process the "rest" dimension being split
    splitSize[splitAxis] = splitSize[splitAxis] - i*valuesPerPiece;
    }
  
  // set the split region ivars
  splitRegion.SetIndex( splitIndex );
  splitRegion.SetSize( splitSize );

  itkDebugMacro("  Split Piece: " << splitRegion );

  return splitRegion;
}
  
  

/**
 *
 */
template <unsigned int VImageDimension>
void 
ImageRegionSplitter<VImageDimension>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
}


} // end namespace itk

#endif
