/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNeighborhoodAlgorithm.txx
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
#ifndef _itkNeighborhoodAlgorithm_txx
#define _itkNeighborhoodAlgorithm_txx
#include "itkImageRegionIterator.h"
#include "itkImageRegion.h"
#include "itkConstSliceIterator.h"

namespace itk
{

namespace NeighborhoodAlgorithm {

template<class TImage>
typename ImageBoundaryFacesCalculator<TImage>::FaceListType
ImageBoundaryFacesCalculator<TImage>
::operator()(const TImage *img, RegionType regionToProcess, RadiusType radius)
{
  unsigned int j, i;
  // Analyze the regionToProcess to determine if any of its faces are
  // along a buffer boundary (we have no data in the buffer for pixels
  // that are outside the boundary, but within the neighborhood radius and will
  // have to treat them differently).  We also determine the size of the non-
  // boundary region that will be processed.

  const IndexType bStart = img->GetBufferedRegion().GetIndex();
  const SizeType  bSize  = img->GetBufferedRegion().GetSize();
  const IndexType rStart = regionToProcess.GetIndex();
  const SizeType  rSize  = regionToProcess.GetSize();

  int  overlapLow, overlapHigh;
  FaceListType faceList;
  IndexType  fStart;         // Boundary, "face"
  SizeType   fSize;          // region data.
  RegionType fRegion;
  SizeType   nbSize  = regionToProcess.GetSize();   // Non-boundary region
  IndexType  nbStart = regionToProcess.GetIndex();  // data.
  RegionType nbRegion;

  for (i = 0; i < ImageDimension; ++i)
    {
      overlapLow = (rStart[i] - radius[i]) - bStart[i];
      overlapHigh= (bStart[i] + bSize[i]) - (rStart[i] + rSize[i] + radius[i]);

      if (overlapLow < 0) // out of bounds condition, define a region of 
        {                 // iteration along this face
          for (j = 0; j < ImageDimension; ++j) // define the starting index
            {                                  // and size of the face region
              fStart[j] = rStart[j];
              if ( j == i ) fSize[j] = -overlapLow;// NOTE: this algorithm
              else          fSize[j] = rSize[j];   // results in a single
            }                                      // duplicate
          nbSize[i]  -= fSize[i];                  // pixel at corners between
          nbStart[i] += -overlapLow;               // adjacent faces.  
          fRegion.SetIndex(fStart);
          fRegion.SetSize(fSize);
          faceList.push_back(fRegion);
        }
      if (overlapHigh < 0)
        {
          for (j = 0; j < ImageDimension; ++j)
            {
              if ( j == i )
                {
                  fStart[j] = rStart[j] + rSize[j] + overlapHigh;
                  fSize[j] = -overlapHigh;
                }
              else
                {
                  fStart[j] = rStart[j];
                  fSize[j] = rSize[j];
                }
            }
          nbSize[i] -= fSize[i];
          fRegion.SetIndex(fStart);
          fRegion.SetSize(fSize);
          faceList.push_back(fRegion);
        }
    }
  nbRegion.SetSize(nbSize);
  nbRegion.SetIndex(nbStart);

  faceList.push_front(nbRegion);
  return faceList;
}

template<class TImage>
typename CalculateOutputWrapOffsetModifiers<TImage>::OffsetType
CalculateOutputWrapOffsetModifiers<TImage>
::operator()(TImage *input, TImage *output) const
{
  OffsetType ans;
  const Size<TImage::ImageDimension> isz =  input->GetBufferedRegion().GetSize();
  const Size<TImage::ImageDimension> osz = output->GetBufferedRegion().GetSize();

  for (int i=0; i<TImage::ImageDimension; ++i) ans[i] = osz[i] - isz[i];
  return ans;
}





} // end namespace NeighborhoodAlgorithm
} // end namespace itk

#endif
