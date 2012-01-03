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
#ifndef __itkMaskFeaturePointSelectionFilter_hxx
#define __itkMaskFeaturePointSelectionFilter_hxx

#include <map>
#include "vnl/vnl_trace.h"
#include "itkMaskFeaturePointSelectionFilter.h"
#include "itkNeighborhood.h"
#include "itkNumericTraits.h"
#include "itkImageRegionIterator.h"
#include "itkCompensatedSummation.h"


namespace itk
{
template<
  class TImagePixel,
  class TMaskPixel,
  class TTensorValueType,
  class TFeaturesTraits,
  unsigned VImageDimension >
MaskFeaturePointSelectionFilter<
  Image< TImagePixel, VImageDimension >,
  Image< TMaskPixel, VImageDimension >,
  PointSet< Matrix< TTensorValueType, VImageDimension, VImageDimension>, VImageDimension, TFeaturesTraits > >
::MaskFeaturePointSelectionFilter()
{
  // default parameters
  m_NonConnectivity = Self::VERTEX_CONNECTIVITY;
  m_SelectFraction = 0.1;
  m_BlockRadius.Fill( 2 );
  m_ComputeStructureTensors = true;
}

template<
  class TImagePixel,
  class TMaskPixel,
  class TTensorValueType,
  class TFeaturesTraits,
  unsigned VImageDimension >
MaskFeaturePointSelectionFilter<
  Image< TImagePixel, VImageDimension >,
  Image< TMaskPixel, VImageDimension >,
  PointSet< Matrix< TTensorValueType, VImageDimension, VImageDimension>, VImageDimension, TFeaturesTraits > >
::~MaskFeaturePointSelectionFilter()
{
}

template<
  class TImagePixel,
  class TMaskPixel,
  class TTensorValueType,
  class TFeaturesTraits,
  unsigned VImageDimension >
void
MaskFeaturePointSelectionFilter<
  Image< TImagePixel, VImageDimension >,
  Image< TMaskPixel, VImageDimension >,
  PointSet< Matrix< TTensorValueType, VImageDimension, VImageDimension>, VImageDimension, TFeaturesTraits > >
::PrintSelf( std::ostream & os, Indent indent ) const
{
  Superclass::PrintSelf( os, indent );
  os << indent << "m_NonConnectivity: ";
  switch ( m_NonConnectivity )
    {
      case 0:
        os << "VERTEX_CONNECTIVITY";
        break;
      case 1:
        os << "EDGE_CONNECTIVITY";
        break;
      case 2:
        os << "FACE_CONNECTIVITY";
        break;
      default:
        os << static_cast< unsigned >( m_NonConnectivity );
    }
  os << std::endl
     << indent << "m_BlockRadius: " << m_BlockRadius << std::endl
     << indent << "m_ComputeStructureTensors: " << ( m_ComputeStructureTensors ? "yes" : "no" ) << std::endl
     << indent << "m_SelectFraction: " << m_SelectFraction << std::endl;
}

template<
  class TImagePixel,
  class TMaskPixel,
  class TTensorValueType,
  class TFeaturesTraits,
  unsigned VImageDimension >
void
MaskFeaturePointSelectionFilter<
  Image< TImagePixel, VImageDimension >,
  Image< TMaskPixel, VImageDimension >,
  PointSet< Matrix< TTensorValueType, VImageDimension, VImageDimension>, VImageDimension, TFeaturesTraits > >
::SetNonConnectivity( unsigned connect ) throw ( ExceptionObject )
{
  if ( connect < VImageDimension )
    {
      m_NonConnectivityOffsets.clear();
      // use Neighbourhood to compute all offsets in radius 1
      Neighborhood< unsigned, VImageDimension> neighborhood;
      neighborhood.SetRadius( NumericTraits< SizeValueType >::One );
      for ( unsigned i = 0, n = neighborhood.Size(); i < n; i++ )
        {
          OffsetType off = neighborhood.GetOffset( i );

          // count 0s offsets in each dimension
          unsigned zeros = 0;
          for ( unsigned j = 0; j < VImageDimension; j++ )
            {
              if ( off[ j ] == 0 ) zeros++;
            }

          if ( connect <= zeros && zeros < VImageDimension )
            {
              m_NonConnectivityOffsets.push_back( off );
            }
        }
    }
  else
    {
      itkExceptionMacro( "Cannot set non-connectivity to " << connect << ", allowed 0.." << VImageDimension - 1 << "." );
    }

  m_NonConnectivity = connect;
}

template<
  class TImagePixel,
  class TMaskPixel,
  class TTensorValueType,
  class TFeaturesTraits,
  unsigned VImageDimension >
void
MaskFeaturePointSelectionFilter<
  Image< TImagePixel, VImageDimension >,
  Image< TMaskPixel, VImageDimension >,
  PointSet< Matrix< TTensorValueType, VImageDimension, VImageDimension>, VImageDimension, TFeaturesTraits > >
::GenerateData()
{
  // generate non-connectivity offsets
  if ( m_NonConnectivityOffsets.empty() )
    {
      SetNonConnectivity( m_NonConnectivity );
    }

  // fill inputs / outputs / misc
  ImageConstPointer image = this->GetInput();
  RegionType region = image->GetRequestedRegion();
  typename ImageType::SpacingType voxelSpacing = image->GetSpacing();

  MaskConstPointer mask = this->GetMaskImage();

  FeaturePointsPointer pointSet = this->GetOutput();
  typename FeaturePointsType::PointsContainer::Pointer points
    = FeaturePointsType::PointsContainer::New();
  typename FeaturePointsType::PointDataContainer::Pointer pointData
    = FeaturePointsType::PointDataContainer::New();

  // initialize selectionMap
  typedef Image<unsigned char, 3>  SelectionMapType;
  SelectionMapType::Pointer selectionMap = SelectionMapType::New();
  selectionMap->SetRegions( region );
  selectionMap->Allocate();

  if ( mask.IsNull() )
    {
      // create all 1s selectionMap
      selectionMap->FillBuffer( NumericTraits< SelectionMapType::PixelType >::One );
    }
  else
    {
      // copy mask into selectionMap
      ImageRegionConstIterator< MaskType > itMask( mask, region );
      ImageRegionIterator< SelectionMapType > itMap( selectionMap, region );
      for ( itMask.GoToBegin(), itMap.GoToBegin(); !itMask.IsAtEnd(); ++itMask, ++itMap )
        {
          itMap.Set( static_cast< SelectionMapType::PixelType >( itMask.Get() ) );
        }
    }

  // set safe region for picking feature points depending on whether tensors are computed
  IndexType safeIndex = region.GetIndex();
  SizeType safeSize = region.GetSize();

  if ( m_ComputeStructureTensors )
    {
      // tensor calculations access points in 2 X m_BlockRadius + 1 radius
      SizeType onesSize;
      onesSize.Fill( 1 );
      safeIndex += m_BlockRadius + m_BlockRadius + onesSize;
      safeSize -= m_BlockRadius + m_BlockRadius + onesSize + m_BlockRadius + m_BlockRadius + onesSize;
    } else
    {
      // variance calculations access points in m_BlockRadius radius
      safeIndex += m_BlockRadius;
      safeSize -= m_BlockRadius + m_BlockRadius;
    }
  region.SetIndex( safeIndex );
  region.SetSize( safeSize );

  // iterators for variance computing loop
  ImageRegionIterator< SelectionMapType > itMap( selectionMap, region );
  ConstNeighborhoodIterator< ImageType > itImage( m_BlockRadius, image, region );
  typedef typename ConstNeighborhoodIterator< ImageType >::NeighborIndexType NeighborIndexType;
  NeighborIndexType numPixelsInNeighborhood = itImage.Size();

  // sorted container for feature points, stores pair(variance, index)
  typedef std::multimap< double, IndexType > MultiMapType;
  MultiMapType pointMap;

  // compute variance for eligible points
  for ( itImage.GoToBegin(), itMap.GoToBegin(); !itImage.IsAtEnd(); ++itImage, ++itMap )
    {
      if ( itMap.Get() )
        {
          CompensatedSummation< double > mean;
          CompensatedSummation< double > variance;
          for ( NeighborIndexType i = 0; i < numPixelsInNeighborhood; i++ )
            {
              ImagePixelType pixel = itImage.GetPixel( i );
              mean += pixel;
              variance += pixel * pixel;
            }
          double variance1 = variance.GetSum() / ( static_cast< double >( numPixelsInNeighborhood ) )
                           - mean.GetSum() / numPixelsInNeighborhood * mean.GetSum() / numPixelsInNeighborhood;

          pointMap.insert( typename MultiMapType::value_type( variance1, itImage.GetIndex() ) );
        }
    }

  // number of points to select
  SizeValueType pointsLeft = floor( 0.5 + pointMap.size() * m_SelectFraction );

  // pick points with highest variance first
  for ( typename MultiMapType::reverse_iterator rit = pointMap.rbegin(); rit != pointMap.rend(); ++rit )
    {
      // if point is not marked off in selection map and there are still points to be picked
      if ( selectionMap->GetPixel( ( *rit ).second ) && pointsLeft )
        {
          pointsLeft--;

          // add point to points container
          PointType point;
          image->TransformIndexToPhysicalPoint( ( *rit ).second, point );
          points->InsertElement( pointsLeft, point );

          // compute and add structure tensor into pointData
          if ( m_ComputeStructureTensors )
            {
              StructureTensorType product;
              StructureTensorType tensor;
              tensor.Fill( 0 );

              Matrix < double, VImageDimension, 1 > gradI; // vector declared as column matrix

              SizeType radius;
              radius.Fill( 1 ); // iterate over neighbourhood of a voxel

              RegionType center;
              center.SetSize( radius );
              center.SetIndex( ( *rit ).second );

              radius = m_BlockRadius + m_BlockRadius; // neighbourhood radius for tensor
              ConstNeighborhoodIterator< ImageType > itGrad( radius, image, center );

              itGrad.GoToBegin();
              for ( unsigned i = 0; i < itGrad.Size(); i++ ) // iterate over voxels in the neighbourhood
                {
                  OffsetType off = itGrad.GetOffset( i );
                  for ( unsigned j = 0; j < VImageDimension; j++ )
                  {
                    OffsetType left = off;
                    left[ j ] -= 1;

                    OffsetType right = off;
                    right[ j ] += 1;

                    // using image GetPixel instead of iterator GetPixel since offsets might be outside of neighbourhood
                    gradI( j, 0 ) = ( image->GetPixel( itGrad.GetIndex( left ) ) - image->GetPixel( itGrad.GetIndex( right ) ) )
                                  / voxelSpacing[ j ] * 0.5;
                  }
                  product = gradI * gradI.GetTranspose(); // equivalent to tensor product of gradI with itself
                  tensor += product;
                }
              tensor /= vnl_trace( tensor.GetVnlMatrix() );

              pointData->InsertElement( pointsLeft, tensor );
            }

          // mark off connected points
          for ( int j = 0, n = m_NonConnectivityOffsets.size(); j < n; j++ )
          {
            IndexType idx = ( *rit ).second;
            idx += m_NonConnectivityOffsets[ j ];
            selectionMap->SetPixel( idx, 0 );
            //if ( region.IsInside( idx ) ) // is this check necessary?
            //  {
            //    selectionMap->SetPixel( idx, 0 );
            //  }
          }
        }
    }

  // set points
  pointSet->SetPoints( points );
  pointSet->SetPointData( pointData );
}
} // end namespace itk

#endif
