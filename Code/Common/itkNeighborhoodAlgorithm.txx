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
#include "itkNeighborhoodAlgorithm.h"
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
  unsigned int i, k;

  // Analyze the regionToProcess to determine if any of its faces are
  // along a buffer boundary (we have no data in the buffer for pixels
  // that are outside the boundary, and within the neighborhood radius and will
  // have to treat them differently).  We also determine the size of the non-
  // boundary region that will be processed.

  const IndexType bStart = img->GetBufferedRegion().GetIndex();
  const SizeType  bSize  = img->GetBufferedRegion().GetSize();
  const IndexType rStart = regionToProcess.GetIndex();
  const SizeType  rSize  = regionToProcess.GetSize();

  FaceListType faceList;

  // Divide each dimension into 1, 2, or 3 subregions depending on
  // whether:
  //
  //   2*radius[1] > bufferedRegion.GetSize()[i]   --> 1 region
  //     else                                      --> 3 regions
  //
  // In the worst case, there will be 3^d regions.
  //
  //
  std::vector<std::vector<IndexValueType> > fStarts;
  std::vector<std::vector<SizeValueType> >  fSizes;
  std::vector<std::vector<bool> >           fBoundary;
  fStarts.resize( ImageDimension );
  fSizes.resize( ImageDimension );
  fBoundary.resize( ImageDimension );
  for (i=0; i < ImageDimension; ++i)
    {
    if ( 2*radius[i]+1 > bSize[i] )
      {
      // Kernel does not fit in the buffer. So every pixel along this
      // dimension is a boundary condition.  Use only one region along
      // this dimension.
      fStarts[i].push_back( rStart[i] );
      fSizes[i].push_back( rSize[i] );
      fBoundary[i].push_back( true );
      }
    else
      {
      // Now, there could be 1, 2, or 3 regions along this dimension
      // depending on the size of radius[i].
      //

      
      // If the start and end of the region to process is more than
      // radius away from the boundary of buffer, then 1 region
      // (no boundary conditions).
      //
      if ((rStart[i] >= bStart[i] + static_cast<IndexValueType>(radius[i]))
          && (rStart[i] + static_cast<IndexValueType>(rSize[i])
              <= bStart[i] + static_cast<IndexValueType>(bSize[i])
              - static_cast<IndexValueType>(radius[i])))
        {
        fStarts[i].push_back( rStart[i] );
        fSizes[i].push_back( rSize[i] );
        fBoundary[i].push_back( false );
        }
      
      // If the start of the region to process is less than radius
      // away from the boundary of the buffer but the end of the
      // region to process is more than radius away from the boundary
      // of the buffer, then 2 regions.
      //
      else if ((rStart[i] < bStart[i] + static_cast<IndexValueType>(radius[i]))
               && (rStart[i] + static_cast<IndexValueType>(rSize[i])
                   <= bStart[i] +
                   static_cast<IndexValueType>(bSize[i])
                   - static_cast<IndexValueType>(radius[i])))
        {
        fStarts[i].push_back( rStart[i] );
        fSizes[i].push_back( radius[i] - (bStart[i] - rStart[i]) );
        fBoundary[i].push_back( true );
        fStarts[i].push_back( rStart[i] + radius[i] - (bStart[i] - rStart[i]));
        fSizes[i].push_back( rSize[i] - (radius[i] - (bStart[i] - rStart[i])));
        fBoundary[i].push_back( false );
        }

             
      // If the start of the region to process is more than radius
      // away from the boundary of the buffer but the end of the
      // region to process is less than radius away from the boundary
      // of buffer, then 2 regions.
      //
      else if ((rStart[i] >= bStart[i] + static_cast<IndexValueType>(radius[i]))
               && (rStart[i] + static_cast<IndexValueType>(rSize[i])
                   > bStart[i] + static_cast<IndexValueType>(bSize[i])
                   - static_cast<IndexValueType>(radius[i])))
        {
        fStarts[i].push_back( rStart[i] );
        fSizes[i].push_back( bStart[i] + bSize[i] - rStart[i] - radius[i] );
        fBoundary[i].push_back( false );
        fStarts[i].push_back( bStart[i] + bSize[i] - radius[i] );
        fSizes[i].push_back( rStart[i] + rSize[i] - bStart[i] - bSize[i]
                             + radius[i] );
        fBoundary[i].push_back( true );
        }
      
      // If the start and the end of the region to process is less
      // than radius away from the boundary of the buffer, then 3
      // regions. If the region to process is not large enough to hold
      // the kernels, then this case becomes 1 region.
      //
      else
        {
        if ( 2*radius[i] + 1 > rSize[i] )
          {
          fStarts[i].push_back( rStart[i] );
          fSizes[i].push_back( rSize[i] );
          fBoundary[i].push_back( true );
          }
        else
          {
          // define 3 regions
          fStarts[i].push_back( rStart[i] );
          fSizes[i].push_back( radius[i] - (bStart[i] - rStart[i]) );
          fBoundary[i].push_back( true );
          
          fStarts[i].push_back( rStart[i] + radius[i]
                                - (bStart[i] - rStart[i]));
          fSizes[i].push_back( rSize[i] - (radius[i] - (bStart[i] - rStart[i]))
                               - (rStart[i] + rSize[i] - bStart[i] - bSize[i]
                                  + radius[i]) );
          fBoundary[i].push_back( false );
          
          fStarts[i].push_back( bStart[i] + bSize[i] - radius[i] );
          fSizes[i].push_back( rStart[i] + rSize[i] - bStart[i] - bSize[i]
                               + radius[i] );
          fBoundary[i].push_back( true );
          }
        }
      }
    }

  // How many faces are we going to have
  unsigned int numberOfRegions = 1;
  for (i=0; i < ImageDimension; i++)
    {
    // product of the number of regions along each dimension
    numberOfRegions *= fStarts[i].size();
    }

  // Now build the faces by combining these various segments together.
  //
  //
  
  // Build a dummy image whose size is defined how many faces we have
  // along each of the dimensions.  
  typedef Image<unsigned char, ImageDimension> FaceImageType;
  typename FaceImageType::Pointer faceImage = FaceImageType::New();
  typename FaceImageType::IndexType faceIndex;
  typename FaceImageType::SizeType faceImageSize;
  for (i = 0; i < ImageDimension; i++)  // for each dimension
    {
    faceImageSize[i] = fStarts[i].size();
    }
  faceImage->SetRegions( faceImageSize );
  faceImage->Allocate();

  // Visit each "pixel" in this dummy image determine its "index".
  // This index is used to index into the datastructures defined above
  // that hold the start and end indices along each dimension.
  RegionType face;
  bool interior;
  bool hasInterior = false;
  for (k=0; k < numberOfRegions; k++)   // for each region
    {
    interior = true;
    
    // compute the "index" of the kth region so that we know what
    // indices in fStarts to fSizes to use for the kth region
    faceIndex = faceImage->ComputeIndex( k );

    for (i = 0; i < ImageDimension; i++)
      {
      face.SetIndex(i, fStarts[i][faceIndex[i]]);
      face.SetSize(i, fSizes[i][faceIndex[i]]);

      // Determine if face in an interior face, i.e. no boundary
      // conditions.  This requires fBoundary is false along all
      // dimensions for the face.
      interior = interior && !fBoundary[i][faceIndex[i]];
      }

    // Add this region to the face list, if it is the interior face
    // (no boundary conditions) then add it to the front of the list
    if (interior)
      {
      faceList.push_front( face );
      hasInterior = true;
      }
    else
      {
      faceList.push_back( face );
      }
    }

  // If we did not define an "interior" face (one without boundary
  // conditions), then we need to construct a dummy region that has a
  // size of 0.  This is to satisfy the original design of the face
  // calculator where the first face is a nonboundary face.
  if ( !hasInterior )
    {
    // Set index to something reasonable (beginning of region to process)
    face.SetIndex( regionToProcess.GetIndex() );

    // Set the size to all zeros
    SizeType tSize;
    tSize.Fill( 0 );
    face.SetSize(  tSize );

    // Put this face at the start of the list
    faceList.push_front( face );
    }
  
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
