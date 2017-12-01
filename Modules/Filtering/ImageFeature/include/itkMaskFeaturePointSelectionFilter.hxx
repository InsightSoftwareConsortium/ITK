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
#ifndef itkMaskFeaturePointSelectionFilter_hxx
#define itkMaskFeaturePointSelectionFilter_hxx


#include <map>
#include "vnl/vnl_trace.h"
#include "itkMaskFeaturePointSelectionFilter.h"
#include "itkNeighborhood.h"
#include "itkNumericTraits.h"
#include "itkImageRegionIterator.h"
#include "itkCompensatedSummation.h"


namespace itk
{
template< typename TImage, typename TMask, typename TFeatures >
MaskFeaturePointSelectionFilter< TImage, TMask, TFeatures >
::MaskFeaturePointSelectionFilter()
{
  // default parameters
  m_NonConnectivity = Self::VERTEX_CONNECTIVITY;
  m_SelectFraction = 0.1;
  m_BlockRadius.Fill( 2 );
  m_ComputeStructureTensors = true;
}

template< typename TImage, typename TMask, typename TFeatures >
MaskFeaturePointSelectionFilter< TImage, TMask, TFeatures >
::~MaskFeaturePointSelectionFilter()
{
}

template< typename TImage, typename TMask, typename TFeatures >
void
MaskFeaturePointSelectionFilter< TImage, TMask, TFeatures >
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

template< typename TImage, typename TMask, typename TFeatures >
void
MaskFeaturePointSelectionFilter< TImage, TMask, TFeatures >
::ComputeConnectivityOffsets( void )
{
  if ( m_NonConnectivity < ImageDimension )
    {
    m_NonConnectivityOffsets.clear();
    // use Neighbourhood to compute all offsets in radius 1
    Neighborhood< unsigned, ImageDimension> neighborhood;
    neighborhood.SetRadius( NumericTraits< SizeValueType >::OneValue() );
    for ( SizeValueType i = 0, n = neighborhood.Size(); i < n; i++ )
      {
      OffsetType off = neighborhood.GetOffset( i );

      // count 0s offsets in each dimension
      unsigned numberOfZeros = 0;
      for ( unsigned j = 0; j < ImageDimension; j++ )
        {
        if ( off[ j ] == 0 )
          {
          numberOfZeros++;
          }
        }

      if ( m_NonConnectivity <= numberOfZeros && numberOfZeros < ImageDimension )
        {
        m_NonConnectivityOffsets.push_back( off );
        }
      }
    }
  else
    {
    itkExceptionMacro( "Cannot use non-connectivity of value " << m_NonConnectivity
      << ", expected a value in the range 0.." << ImageDimension - 1 << "." );
    }
}

template< typename TImage, typename TMask, typename TFeatures >
void
MaskFeaturePointSelectionFilter< TImage, TMask, TFeatures >
::GenerateData()
{
  // generate non-connectivity offsets
  this->ComputeConnectivityOffsets();

  // fill inputs / outputs / misc
  const TImage * image = this->GetInput();
  RegionType region = image->GetLargestPossibleRegion();
  typename ImageType::SpacingType voxelSpacing = image->GetSpacing();

  FeaturePointsPointer pointSet = this->GetOutput();

  typedef typename FeaturePointsType::PointsContainer PointsContainer;
  typedef typename PointsContainer::Pointer           PointsContainerPointer;

  PointsContainerPointer points = PointsContainer::New();

  typedef typename FeaturePointsType::PointDataContainer  PointDataContainer;
  typedef typename PointDataContainer::Pointer            PointDataContainerPointer;

  PointDataContainerPointer pointData = PointDataContainer::New();

  // initialize selectionMap
  typedef unsigned char                         MapPixelType;
  typedef Image< MapPixelType, ImageDimension>  SelectionMapType;
  typename SelectionMapType::Pointer selectionMap = SelectionMapType::New();

  // The selectionMap only needs to have the same pixel grid of the input image,
  // but do not have to care about origin, spacing or orientation.
  selectionMap->SetRegions( region );
  selectionMap->Allocate();

  const TMask * mask = this->GetMaskImage();

  if ( mask == ITK_NULLPTR )
    {
    // create all 1s selectionMap
    selectionMap->FillBuffer( NumericTraits< MapPixelType >::OneValue() );
    }
  else
    {
    // copy mask into selectionMap
    ImageRegionConstIterator< MaskType > maskItr( mask, region );
    ImageRegionIterator< SelectionMapType > mapItr( selectionMap, region );
    for ( maskItr.GoToBegin(), mapItr.GoToBegin(); !maskItr.IsAtEnd(); ++maskItr, ++mapItr )
      {
      mapItr.Set( static_cast< MapPixelType >( maskItr.Get() ) );
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
    // Define the area in which tensors are going to be computed.
    const SizeType blockSize =  m_BlockRadius + m_BlockRadius + onesSize;
    safeIndex += blockSize;
    safeSize -= blockSize + blockSize;
    }
  else
    {
    // variance calculations access points in m_BlockRadius radius
    safeIndex += m_BlockRadius;
    safeSize -= m_BlockRadius + m_BlockRadius;
    }

  region.SetIndex( safeIndex );
  region.SetSize( safeSize );

  // iterators for variance computing loop
  ImageRegionIterator< SelectionMapType > mapItr( selectionMap, region );
  ConstNeighborhoodIterator< ImageType > imageItr( m_BlockRadius, image, region );
  typedef typename ConstNeighborhoodIterator< ImageType >::NeighborIndexType NeighborSizeType;
  NeighborSizeType numPixelsInNeighborhood = imageItr.Size();

  // sorted container for feature points, stores pair(variance, index)
  typedef std::multimap< double, IndexType > MultiMapType;
  MultiMapType pointMap;

  // compute variance for eligible points
  for ( imageItr.GoToBegin(), mapItr.GoToBegin(); !imageItr.IsAtEnd(); ++imageItr, ++mapItr )
    {
    if ( mapItr.Get() )
      {
      CompensatedSummation< double > sum;
      CompensatedSummation< double > sumOfSquares;
      for ( NeighborSizeType i = 0; i < numPixelsInNeighborhood; i++ )
        {
        const ImagePixelType pixel = imageItr.GetPixel( i );
        sum += pixel;
        sumOfSquares += pixel * pixel;
        }

      const double mean = sum.GetSum() / numPixelsInNeighborhood;
      const double squaredMean = mean * mean;
      const double meanOfSquares = sumOfSquares.GetSum() / numPixelsInNeighborhood;

      const double variance = meanOfSquares - squaredMean;
      typedef typename MultiMapType::value_type PairType;

      // we only insert blocks with variance > 0
      if(itk::NumericTraits<double>::IsPositive(variance))
        {
        pointMap.insert( PairType( variance, imageItr.GetIndex() ) );
        }
      }
    }

  // number of points to select
  IndexValueType numberOfPointsInserted = -1; // initialize to -1
  IndexValueType maxNumberPointsToInserted = Math::Floor<SizeValueType>( 0.5 + pointMap.size() * m_SelectFraction );
  const double TRACE_EPSILON = 1e-8;

  // pick points with highest variance first (inverse iteration)
  typedef typename MultiMapType::reverse_iterator MapReverseIterator;
  MapReverseIterator rit = pointMap.rbegin();
  while ( rit != pointMap.rend() && numberOfPointsInserted < maxNumberPointsToInserted)
    {
    // if point is not marked off in selection map and there are still points to be picked
    const IndexType & indexOfPointToPick = rit->second;

    // index should be inside the mask image (GetPixel = 1)
    if ( selectionMap->GetPixel( indexOfPointToPick ) && region.IsInside(indexOfPointToPick) )
      {
      numberOfPointsInserted++;
      // compute and add structure tensor into pointData
      if ( m_ComputeStructureTensors )
        {
        StructureTensorType tensor;
        tensor.Fill( 0 );

        Matrix < SpacePrecisionType, ImageDimension, 1 > gradI; // vector declared as column matrix

        SizeType radius;
        radius.Fill( 1 ); // iterate over neighbourhood of a voxel

        RegionType center;
        center.SetSize( radius );
        center.SetIndex( indexOfPointToPick );

        SizeType neighborRadiusForTensor = m_BlockRadius + m_BlockRadius;
        ConstNeighborhoodIterator< ImageType > gradientItr( neighborRadiusForTensor, image, center );

        gradientItr.GoToBegin();

        // iterate over voxels in the neighbourhood
        for ( SizeValueType i = 0; i < gradientItr.Size(); i++ )
          {
          OffsetType off = gradientItr.GetOffset( i );

          for ( unsigned j = 0; j < ImageDimension; j++ )
            {
            OffsetType left = off;
            left[ j ] -= 1;

            OffsetType right = off;
            right[ j ] += 1;

            const ImagePixelType leftPixelValue  = image->GetPixel( gradientItr.GetIndex( left  ) );
            const ImagePixelType rightPixelValue = image->GetPixel( gradientItr.GetIndex( right ) );
            const SpacePrecisionType doubleSpacing = voxelSpacing[ j ] * 2.0;

            // using image GetPixel instead of iterator GetPixel since offsets might be outside of neighbourhood
            gradI( j, 0 ) = ( leftPixelValue - rightPixelValue ) / doubleSpacing;
            }

          // Compute tensor product of gradI with itself
          const vnl_matrix< SpacePrecisionType > tnspose = gradI.GetTranspose();
          StructureTensorType product ( gradI * tnspose );
          tensor += product;
          }

        const double trace = vnl_trace( tensor.GetVnlMatrix() );

        // trace should be non-zero
        if ( itk::Math::abs(trace) < TRACE_EPSILON )
          {
            rit++;
            numberOfPointsInserted--;
            continue;
          }

        tensor /= trace;
        pointData->InsertElement( numberOfPointsInserted , tensor );

        } // end of compute structure tensor

      // add the point to the container
      PointType point;
      image->TransformIndexToPhysicalPoint( indexOfPointToPick, point );
      points->InsertElement( numberOfPointsInserted, point );

      // mark off connected points
      const MapPixelType ineligeblePointCode = 0;
      for ( size_t j = 0, n = m_NonConnectivityOffsets.size(); j < n; j++ )
        {
        IndexType idx = rit->second;
        idx += m_NonConnectivityOffsets[ j ];
        selectionMap->SetPixel( idx, ineligeblePointCode );
        }
      }
    rit++;
    }
  // set points
  pointSet->SetPoints( points );
  pointSet->SetPointData( pointData );
}
} // end namespace itk

#endif
