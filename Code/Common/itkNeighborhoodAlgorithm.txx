/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNeighborhoodAlgorithm.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

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

  long  overlapLow, overlapHigh;
  FaceListType faceList;
  IndexType  fStart;         // Boundary, "face"
  SizeType   fSize;          // region data.
  RegionType fRegion;
  SizeType   nbSize  = regionToProcess.GetSize();   // Non-boundary region
  IndexType  nbStart = regionToProcess.GetIndex();  // data.
  RegionType nbRegion;

  for (i = 0; i < ImageDimension; ++i)
    {
    overlapLow = static_cast<long>((rStart[i] - radius[i]) - bStart[i]);
    overlapHigh= static_cast<long>((bStart[i] + bSize[i]) - (rStart[i] + rSize[i] + radius[i]));

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
          fStart[j] = rStart[j] + static_cast<IndexValueType>(rSize[j]) + overlapHigh;
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

  for (int i=0; i<TImage::ImageDimension; ++i)
    {
    ans[i] = osz[i] - isz[i];
    }
  return ans;
}





} // end namespace NeighborhoodAlgorithm
} // end namespace itk

#endif
