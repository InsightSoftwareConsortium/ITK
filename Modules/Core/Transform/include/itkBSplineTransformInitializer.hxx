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
#ifndef itkBSplineTransformInitializer_hxx
#define itkBSplineTransformInitializer_hxx

#include "itkBSplineTransformInitializer.h"

#include "itkContinuousIndex.h"
#include "itkPointSet.h"
#include "itkBoundingBox.h"

namespace itk
{

template<typename TTransform, typename TImage>
BSplineTransformInitializer<TTransform, TImage>
::BSplineTransformInitializer() :
  m_Transform( ITK_NULLPTR ),
  m_SetTransformDomainMeshSizeViaInitializer( false )
{
  this->m_TransformDomainMeshSize.Fill( 1 );
}

template<typename TTransform, typename TImage>
BSplineTransformInitializer<TTransform, TImage>
::~BSplineTransformInitializer()
{
}

template<typename TTransform, typename TImage>
void
BSplineTransformInitializer<TTransform, TImage>
::SetTransformDomainMeshSize( const MeshSizeType meshSize )
{
  itkDebugMacro( "setting m_TransformDomainMeshSize to " << meshSize );
  if( this->m_SetTransformDomainMeshSizeViaInitializer == false ||
    this->m_TransformDomainMeshSize != meshSize )
    {
    this->m_SetTransformDomainMeshSizeViaInitializer = true;
    this->m_TransformDomainMeshSize = meshSize;
    this->Modified();
    }
}

template<typename TTransform, typename TImage>
void
BSplineTransformInitializer<TTransform, TImage>
::InitializeTransform() const
{
  if( !this->m_Transform )
    {
    itkExceptionMacro( << "Transform has not been set." );
    return;
    }
  if( !this->m_Image )
    {
    itkExceptionMacro( << "Image has not been set." );
    return;
    }
  if( TImage::GetImageDimension() != SpaceDimension )
    {
    itkExceptionMacro( << "Image dimensionality does not match the transform." );
    return;
    }

  OriginType                        transformDomainOrigin;
  PhysicalDimensionsType            transformDomainPhysicalDimensions;
  DirectionType                     transformDomainDirection;

  // Determine the image corners. We keep track of the relative location of
  // the corners using a binary labeling system. For example, in a 3-D
  // coordinate system aligned with the x,y,z axes, we have 8 points labeled as
  // follows:
  //
  //  1. 000  min_x, min_y, min_z
  //  2. 001  max_x, min_y, min_z
  //  3. 010  min_x, max_y, min_z
  //  4. 011  max_x, max_y, min_z
  //  5. 100  min_x, min_y, min_z
  //  6. 101  max_x, min_y, max_z
  //  7. 110  min_x, max_y, max_z
  //  8. 111  max_x, max_y, max_z
  //
  // We use this binary description of the corners in n-dimensions because it
  // allows us to know the adjacent neighbors of an arbitrary image corner. For
  // example, suppose we locate the transform domain origin at the corner 011
  // the adjacent neighbors which form the rotated coordinate system are
  // 111, 001, and 010. Notice that we just change 1 bit at a time from the
  // origin to determine these axes. Thus bitwise operators are used
  // throughout the code so that the initializer is generalized to n-dimensions.

  typedef typename ImagePointType::CoordRepType      CoordRepType;

  typedef PointSet<CoordRepType, SpaceDimension>     PointSetType;
  typename PointSetType::Pointer cornerPoints = PointSetType::New();
  cornerPoints->Initialize();

  typedef typename PointSetType::PointType           PointType;
  typedef typename PointSetType::PointIdentifier     PointIdentifier;
  typedef typename PointType::RealType               RealType;
  typedef typename PointType::VectorType             VectorType;

  typedef ContinuousIndex<CoordRepType, SpaceDimension> ContinuousIndexType;

  // We first convert the image corners into points which reside in physical
  // space and label them as indicated above.  Note that the corners reside
  // at the extreme corners of the image and not just at the voxel centers.
  // We also store the corners using the point set class which gives us easy
  // access to the bounding box.

  const CoordRepType BSplineTransformDomainEpsilon = std::pow( 2.0, -3 );

  ContinuousIndexType startIndex;
  for( unsigned int i = 0; i < SpaceDimension; i++ )
    {
    startIndex[i] = this->m_Image->GetLargestPossibleRegion().GetIndex()[i] - 0.5 -
      BSplineTransformDomainEpsilon;
    }

  for( unsigned int d = 0, N = 1 << SpaceDimension; d < N; d++ )
    {
    ContinuousIndexType whichIndex;
    for( unsigned int i = 0; i < SpaceDimension; i++ )
      {
      whichIndex[i] = startIndex[i] + static_cast<CoordRepType>( ( ( d >> i ) &
        1 ) * ( this->m_Image->GetLargestPossibleRegion().GetSize()[i] + 2.0 *
        BSplineTransformDomainEpsilon ) );
      }
    ImagePointType point;
    this->m_Image->TransformContinuousIndexToPhysicalPoint( whichIndex, point );
    PointType corner;
    corner.CastFrom( point );
    cornerPoints->SetPoint( d, corner );
    }

  // We next determine which corner is the transform domain origin by which
  // point is closest to the minimum of the bounding box.

  typedef BoundingBox<unsigned int, SpaceDimension,
    typename PointSetType::CoordRepType,
    typename PointSetType::PointsContainer> BoundingBoxType;
  typename BoundingBoxType::Pointer bbox = BoundingBoxType::New();
  bbox->SetPoints( cornerPoints->GetPoints() );
  bbox->ComputeBoundingBox();

  transformDomainOrigin.Fill( 0 );
  PointIdentifier transformDomainOriginId = 0;
  RealType minDistance = NumericTraits<RealType>::max();

  for( unsigned int d = 0; d < cornerPoints->GetNumberOfPoints(); d++ )
    {
    PointType corner;
    corner.Fill( 0.0 );
    cornerPoints->GetPoint( d, &corner );

    RealType distance = corner.SquaredEuclideanDistanceTo(
      bbox->GetMinimum() );
    if( distance < minDistance )
      {
      transformDomainOrigin.CastFrom( corner );
      minDistance = distance;
      transformDomainOriginId = static_cast<PointIdentifier>( d );
      }
    }

  // Now we need to find the transform direction matrix. This is done
  // by using the domain origin and its adjacent neighbors to determine a new
  // rotated coordinate system.

  transformDomainDirection.SetIdentity();

  // We first determine which image axis is the most aligned with each physical
  // axis.

  PointIdentifier minCornerId[SpaceDimension];
  double minAngle[SpaceDimension];

  for( unsigned int d = 0; d < SpaceDimension; d++ )
    {
    minAngle[d] = NumericTraits<double>::max();

    VectorType vectorAxis( 0.0 );
    vectorAxis[d] = 1.0;

    for( unsigned int i = 0; i < SpaceDimension; i++ )
      {
      PointIdentifier oppositeCornerId = ( static_cast< PointIdentifier >(
        1 ) << static_cast< PointIdentifier >( i ) )^ transformDomainOriginId;

      PointType corner;
      corner.Fill( 0.0 );
      cornerPoints->GetPoint( oppositeCornerId, &corner );

      VectorType vector = corner - transformDomainOrigin;
      vector.Normalize();

      double theta = angle( vectorAxis.GetVnlVector(), vector.GetVnlVector() );

      if( theta < minAngle[d] )
        {
        bool alreadyFound = false;
        for( unsigned int j = 0; j < d; j++ )
          {
          if( minCornerId[j] == oppositeCornerId )
            {
            alreadyFound = true;
            break;
            }
          }
        if( !alreadyFound )
          {
          minCornerId[d] = oppositeCornerId;
          minAngle[d] = theta;
          }
        }
      }
    }

  // Now that we know which image axes corresponds to the unrotated coordinate
  // axes in physical space, we can easily construct the rotation matrix which
  // rotates a point from the unrotated coordinate system to the rotated
  // coordinate system. This is done by placing the rotated axis vectors as
  // columns in the rotation matrix.

  for( unsigned int d = 0; d < SpaceDimension; d++ )
    {
    PointType corner;
    corner.Fill( 0.0 );
    cornerPoints->GetPoint( minCornerId[d], &corner );

    VectorType vector = corner - transformDomainOrigin;

    // Note that specifying the size and spacing separately doesn't matter in
    // the case of the B-spline transform since the B-spline transform is a
    // continuous object over its finite domain.

    transformDomainPhysicalDimensions[d] = vector.GetNorm();
    vector.Normalize();

    for( unsigned int i = 0; i < SpaceDimension; i++ )
      {
      transformDomainDirection[i][d] = vector[i];
      }
    }

  this->m_Transform->SetTransformDomainOrigin( transformDomainOrigin );
  this->m_Transform->SetTransformDomainPhysicalDimensions(
    transformDomainPhysicalDimensions );
  this->m_Transform->SetTransformDomainDirection( transformDomainDirection );
  if( this->m_SetTransformDomainMeshSizeViaInitializer == true )
    {
    this->m_Transform->SetTransformDomainMeshSize(
      this->m_TransformDomainMeshSize );
    }
}

template<typename TTransform, typename TImage>
void
BSplineTransformInitializer<TTransform, TImage>
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf( os, indent );

  itkPrintSelfObjectMacro( Image );

  itkPrintSelfObjectMacro( Transform );

  if( this->m_SetTransformDomainMeshSizeViaInitializer == true )
    {
    os << indent << "Transform domain mesh size: " <<
      this->m_TransformDomainMeshSize << std::endl;
    }
}

}  // namespace itk

#endif
