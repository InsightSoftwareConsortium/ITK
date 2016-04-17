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
#ifndef itkTriangleHelper_hxx
#define itkTriangleHelper_hxx

#include "itkTriangleHelper.h"
#include "itkMath.h"

namespace itk
{
template< typename TPoint >
bool TriangleHelper< TPoint >::IsObtuse(const PointType & iA, const PointType & iB, const PointType & iC)
{
  VectorType v01 = iB - iA;
  VectorType v02 = iC - iA;
  VectorType v12 = iC - iB;

  if ( v01 * v02 < 0.0 )
    {
    return true;
    }
  else
    {
    if ( v02 * v12 < 0.0 )
      {
      return true;
      }
    else
      {
      if ( v01 * -v12 < 0.0 )
        {
        return true;
        }
      else
        {
        return false;
        }
      }
    }
}

template< typename TPoint >
typename TriangleHelper< TPoint >::VectorType
TriangleHelper< TPoint >::ComputeNormal(const PointType & iA,
                                        const PointType & iB,
                                        const PointType & iC)
{
  CrossVectorType cross;
  VectorType      w = cross (iB - iA, iC - iA);
  CoordRepType    l2 = w.GetSquaredNorm();

  if ( l2 != 0.0 )
    {
    w /= std::sqrt(l2);
    }

  return w;
}

template< typename TPoint >
typename TriangleHelper< TPoint >::CoordRepType
TriangleHelper< TPoint >::Cotangent(const PointType & iA,
                                    const PointType & iB,
                                    const PointType & iC)
{
  VectorType   v21 = iA - iB;

  CoordRepType v21_l2 = v21.GetSquaredNorm();

  if ( Math::NotAlmostEquals( v21_l2, NumericTraits< CoordRepType >::ZeroValue() ) )
    {
    v21 /= std::sqrt(v21_l2);
    }

  VectorType   v23 = iC - iB;
  CoordRepType v23_l2 = v23.GetSquaredNorm();
  if ( Math::NotAlmostEquals( v23_l2, NumericTraits< CoordRepType >::ZeroValue() ) )
    {
    v23 /= std::sqrt(v23_l2);
    }

  CoordRepType bound(0.999999);

  CoordRepType cos_theta = std::max( -bound,
                                         std::min(bound, v21 * v23) );

  return 1.0 / std::tan( std::acos(cos_theta) );
}

template< typename TPoint >
typename TriangleHelper< TPoint >::PointType
TriangleHelper< TPoint >::ComputeBarycenter(
  const CoordRepType & iA1, const PointType & iP1,
  const CoordRepType & iA2, const PointType & iP2,
  const CoordRepType & iA3, const PointType & iP3)
{
  PointType oPt;

  CoordRepType total = iA1 + iA2 + iA3;

  if ( Math::AlmostEquals( total, NumericTraits< CoordRepType >::ZeroValue() ) )
    {
    //in such case there is no barycenter;
    oPt.Fill(0.);
    return oPt;
    }

  CoordRepType inv_total = 1. / total;
  CoordRepType a1 = iA1 * inv_total;
  CoordRepType a2 = iA2 * inv_total;
  CoordRepType a3 = iA3 * inv_total;

  for ( unsigned int dim = 0; dim < PointDimension; ++dim )
    {
    oPt[dim] = a1 * iP1[dim] + a2 * iP2[dim] + a3 * iP3[dim];
    }

  return oPt;
}

template< typename TPoint >
typename TriangleHelper< TPoint >::CoordRepType
TriangleHelper< TPoint >::ComputeAngle(const PointType & iP1,
                                       const PointType & iP2,
                                       const PointType & iP3)
{
  VectorType v21 = iP1 - iP2;
  VectorType v23 = iP3 - iP2;

  CoordRepType v21_l2 = v21.GetSquaredNorm();
  CoordRepType v23_l2 = v23.GetSquaredNorm();

  if ( v21_l2 != 0.0 )
    {
    v21 /= std::sqrt(v21_l2);
    }
  if ( v23_l2 != 0.0 )
    {
    v23 /= std::sqrt(v23_l2);
    }

  CoordRepType bound(0.999999);

  CoordRepType cos_theta = std::max( -bound,
                                         std::min(bound, v21 * v23) );

  return std::acos(cos_theta);
}

template< typename TPoint >
typename TriangleHelper< TPoint >::PointType
TriangleHelper< TPoint >::ComputeGravityCenter(
  const PointType & iP1,
  const PointType & iP2,
  const PointType & iP3)
{
  return ComputeBarycenter(1., iP1, 1., iP2, 1., iP3);
}

template< typename TPoint >
typename TriangleHelper< TPoint >::PointType
TriangleHelper< TPoint >::ComputeCircumCenter(
  const PointType & iP1,
  const PointType & iP2,
  const PointType & iP3)
{
  PointType oPt;

  oPt.Fill (0.0);

  CoordRepType a = iP2.SquaredEuclideanDistanceTo (iP3);
  CoordRepType b = iP1.SquaredEuclideanDistanceTo (iP3);
  CoordRepType c = iP2.SquaredEuclideanDistanceTo (iP1);

  CoordRepType Weight[3];
  Weight[0] = a * ( b + c - a );
  Weight[1] = b * ( c + a - b );
  Weight[2] = c * ( a + b - c );

  return ComputeBarycenter(Weight[0], iP1, Weight[1], iP2, Weight[2], iP3);
}

template< typename TPoint >
typename TriangleHelper< TPoint >::PointType
TriangleHelper< TPoint >::ComputeConstrainedCircumCenter(const PointType & iP1,
                                                         const PointType & iP2, const PointType & iP3)
{
  PointType    oPt;
  CoordRepType a = iP2.SquaredEuclideanDistanceTo (iP3);
  CoordRepType b = iP1.SquaredEuclideanDistanceTo (iP3);
  CoordRepType c = iP2.SquaredEuclideanDistanceTo (iP1);

  CoordRepType Weight[3];

  Weight[0] = a * ( b + c - a );
  Weight[1] = b * ( c + a - b );
  Weight[2] = c * ( a + b - c );

  for ( unsigned int i = 0; i < 3; i++ )
    {
    if ( Weight[i] < 0.0 )
      {
      Weight[i] = 0.;
      }
    }

  return ComputeBarycenter(Weight[0], iP1, Weight[1], iP2, Weight[2], iP3);
}

template< typename TPoint >
typename TriangleHelper< TPoint >::CoordRepType
TriangleHelper< TPoint >::ComputeArea(const PointType & iP1,
                                      const PointType & iP2,
                                      const PointType & iP3)
{
  CoordRepType a = iP2.EuclideanDistanceTo (iP3);
  CoordRepType b = iP1.EuclideanDistanceTo (iP3);
  CoordRepType c = iP2.EuclideanDistanceTo (iP1);

  CoordRepType s = 0.5 * ( a + b + c );

  return static_cast< CoordRepType >( std::sqrt ( s * ( s - a ) * ( s - b ) * ( s - c ) ) );
}

template< typename TPoint >
typename TriangleHelper< TPoint >::CoordRepType
TriangleHelper< TPoint >::ComputeMixedArea(const PointType & iP1,
                                      const PointType & iP2,
                                      const PointType & iP3)
{
  typedef TriangleHelper< TPoint > TriangleType;

  if ( !TriangleType::IsObtuse(iP1, iP2, iP3) )
    {
    CoordRepType sq_d01 =
        static_cast< CoordRepType >( iP1.SquaredEuclideanDistanceTo(iP2) );
    CoordRepType sq_d02 =
      static_cast< CoordRepType >( iP1.SquaredEuclideanDistanceTo(iP3) );

    CoordRepType cot_theta_210 = TriangleType::Cotangent(iP3, iP2, iP1);
    CoordRepType cot_theta_021 = TriangleType::Cotangent(iP1, iP3, iP2);

    return 0.125 * ( sq_d02 * cot_theta_210 + sq_d01 * cot_theta_021 );
    }
  else
    {
    CoordRepType area =
      static_cast< CoordRepType >( TriangleType::ComputeArea(iP1, iP2, iP3) );

    if ( ( iP2 - iP1 ) * ( iP3 - iP1 ) < NumericTraits< CoordRepType >::ZeroValue() )
      {
      return 0.5 * area;
      }
    else
      {
      return 0.25 * area;
      }
    }
}
}

#endif
