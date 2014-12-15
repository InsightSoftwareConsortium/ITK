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
#ifndef itkNeighborhoodAlgorithm_hxx
#define itkNeighborhoodAlgorithm_hxx
#include "itkNeighborhoodAlgorithm.h"
#include "itkImageRegionIterator.h"
#include "itkImageRegion.h"
#include "itkConstSliceIterator.h"

namespace itk
{
namespace NeighborhoodAlgorithm
{
template< typename TImage >
typename ImageBoundaryFacesCalculator< TImage >::FaceListType
ImageBoundaryFacesCalculator< TImage >
::operator()(const TImage *img, RegionType regionToProcess, RadiusType radius)
{
  unsigned int j, i;
  // Analyze the regionToProcess to determine if any of its faces are
  // along a buffer boundary (we have no data in the buffer for pixels
  // that are outside the boundary, but within the neighborhood radius and will
  // have to treat them differently).  We also determine the size of the non-
  // boundary region that will be processed. For instance, given a 2D image
  // and regionTOProcess (size = 5x5),

  FaceListType faceList;
  // The portion of the regionToProcess that is outside of the image bufferedRegion
  // doesn't make sense. If the regionToProcess is completely outside of
  // the image bufferedRegion, return a empty region list.
  if( !regionToProcess.Crop( img->GetBufferedRegion() ) )
    {
    return faceList;
    }

  const IndexType bStart = img->GetBufferedRegion().GetIndex();
  const SizeType  bSize  = img->GetBufferedRegion().GetSize();
  const IndexType rStart = regionToProcess.GetIndex();
  const SizeType  rSize  = regionToProcess.GetSize();

  IndexValueType overlapLow, overlapHigh;
  IndexType    fStart;       // Boundary, "face"
  SizeType     fSize;        // region data.
  RegionType   fRegion;
  SizeType     nbSize  = regionToProcess.GetSize();  // Non-boundary region
  IndexType    nbStart = regionToProcess.GetIndex(); // data.
  RegionType   nbRegion;

  IndexType    vrStart = rStart; //start index of variable processed region which has considered the boundary region in last direction
  SizeType     vrSize = rSize; //size of variable processed region which has considered the boundary region in last direction

  for ( i = 0; i < ImageDimension; ++i )
    {
    overlapLow = static_cast< IndexValueType >( ( rStart[i] - radius[i] ) - bStart[i] );
    // image buffered region should be more than twice of the radius,
    // otherwise there would be overlap between two boundary regions.
    // in the case, we reduce upper boundary size to remove overlap.
    if ( bSize[i] > 2 * radius[i] )
      {
      overlapHigh = static_cast< IndexValueType >( ( bStart[i] + bSize[i] ) - ( rStart[i] + rSize[i] + radius[i] ) );
      }
    else
      {
      overlapHigh = static_cast< IndexValueType >( ( bStart[i] + radius[i] ) - ( rStart[i] + rSize[i] ) );
      }

    if ( overlapLow < 0 )                    // out of bounds condition, define
                                             // a region of
      {                                      // iteration along this face
      for ( j = 0; j < ImageDimension; ++j ) // define the starting index
        {                                    // and size of the face region
        fStart[j] = vrStart[j];
        if ( j == i )
          {
          // Boundary region cannot be outside the region to process
          if( -overlapLow > static_cast< IndexValueType >( rSize[i] ) )
            {
            overlapLow = - static_cast< IndexValueType >( rSize[i] );
            }
          fSize[j] = -overlapLow;
          vrSize[j] += overlapLow; //change start and size in this direction
          vrStart[j] -= overlapLow;//to ensure no duplicate pixels at corners
          }
        else
          {
          fSize[j] = vrSize[j];
          }

        // Boundary region cannot be outside the region to process
        if ( fSize[j] > rSize[j] )
          {
          fSize[j] = rSize[j];
          }
        }
      // avoid unsigned overflow if the non-boundary region is too small to
      // process
      if ( fSize[i] > nbSize[i] )
        {
        nbSize[i] = 0;
        }
      else
        {
        nbSize[i] -= fSize[i];
        }
      nbStart[i] += -overlapLow;
      fRegion.SetIndex(fStart);
      fRegion.SetSize(fSize);
      faceList.push_back(fRegion);
      }
    if ( overlapHigh < 0 )
      {
      for ( j = 0; j < ImageDimension; ++j )
        {
        if ( j == i )
          {
          // Boundary region cannot be outside the region to process
          if( -overlapHigh > static_cast< IndexValueType >( rSize[i] ) )
            {
            overlapHigh = - static_cast< IndexValueType >( rSize[i] );
            }
          fStart[j] = rStart[j] + static_cast< IndexValueType >( rSize[j] ) + overlapHigh;
          fSize[j] = -overlapHigh;
          vrSize[j] += overlapHigh; //change size in this direction
          }
        else
          {
          fStart[j] = vrStart[j];
          fSize[j] = vrSize[j];
          }
        }
      // avoid unsigned overflow if the non-boundary region is too small to
      // process
      if ( fSize[i] > nbSize[i] )
        {
        nbSize[i] = 0;
        }
      else
        {
        nbSize[i] -= fSize[i];
        }

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

template< typename TImage >
typename CalculateOutputWrapOffsetModifiers< TImage >::OffsetType
CalculateOutputWrapOffsetModifiers< TImage >
::operator()(TImage *input, TImage *output) const
{
  OffsetType ans;

  const Size< TImage::ImageDimension > isz =  input->GetBufferedRegion().GetSize();
  const Size< TImage::ImageDimension > osz = output->GetBufferedRegion().GetSize();

  for ( int i = 0; i < TImage::ImageDimension; ++i )
    {
    ans[i] = osz[i] - isz[i];
    }
  return ans;
}
} // end namespace NeighborhoodAlgorithm
} // end namespace itk

#endif
