/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
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

#include "itkMath.h"

namespace itk
{
template <typename TPoint>
bool
TriangleHelper<TPoint>::IsObtuse(const PointType & iA, const PointType & iB, const PointType & iC)
{
  const VectorType v01 = iB - iA;
  const VectorType v02 = iC - iA;
  const VectorType v12 = iC - iB;

  if (v01 * v02 < 0.0)
  {
    return true;
  }

  if (v02 * v12 < 0.0)
  {
    return true;
  }
  else
  {
    if (v01 * -v12 < 0.0)
    {
      return true;
    }
    else
    {
      return false;
    }
  }
}

template <typename TPoint>
auto
TriangleHelper<TPoint>::ComputeNormal(const PointType & iA, const PointType & iB, const PointType & iC) -> VectorType
{
  const CrossVectorType cross;
  VectorType            w = cross(iB - iA, iC - iA);
  const CoordinateType  l2 = w.GetSquaredNorm();

  if (l2 != 0.0)
  {
    w /= std::sqrt(l2);
  }

  return w;
}

template <typename TPoint>
auto
TriangleHelper<TPoint>::Cotangent(const PointType & iA, const PointType & iB, const PointType & iC) -> CoordinateType
{
  VectorType v21 = iA - iB;

  const CoordinateType v21_l2 = v21.GetSquaredNorm();

  if (Math::NotAlmostEquals(v21_l2, CoordinateType{}))
  {
    v21 /= std::sqrt(v21_l2);
  }

  VectorType           v23 = iC - iB;
  const CoordinateType v23_l2 = v23.GetSquaredNorm();
  if (Math::NotAlmostEquals(v23_l2, CoordinateType{}))
  {
    v23 /= std::sqrt(v23_l2);
  }

  const CoordinateType bound(0.999999);

  const CoordinateType cos_theta = std::clamp(v21 * v23, -bound, bound);

  return 1.0 / std::tan(std::acos(cos_theta));
}

template <typename TPoint>
auto
TriangleHelper<TPoint>::ComputeBarycenter(const CoordinateType & iA1,
                                          const PointType &      iP1,
                                          const CoordinateType & iA2,
                                          const PointType &      iP2,
                                          const CoordinateType & iA3,
                                          const PointType &      iP3) -> PointType
{
  const CoordinateType total = iA1 + iA2 + iA3;
  PointType            oPt{};
  if (Math::AlmostEquals(total, CoordinateType{}))
  {
    // in such case there is no barycenter;
    return oPt;
  }

  const CoordinateType inv_total = 1. / total;
  const CoordinateType a1 = iA1 * inv_total;
  const CoordinateType a2 = iA2 * inv_total;
  const CoordinateType a3 = iA3 * inv_total;

  for (unsigned int dim = 0; dim < PointDimension; ++dim)
  {
    oPt[dim] = a1 * iP1[dim] + a2 * iP2[dim] + a3 * iP3[dim];
  }

  return oPt;
}

template <typename TPoint>
auto
TriangleHelper<TPoint>::ComputeAngle(const PointType & iP1, const PointType & iP2, const PointType & iP3)
  -> CoordinateType
{
  VectorType v21 = iP1 - iP2;
  VectorType v23 = iP3 - iP2;

  const CoordinateType v21_l2 = v21.GetSquaredNorm();
  const CoordinateType v23_l2 = v23.GetSquaredNorm();

  if (v21_l2 != 0.0)
  {
    v21 /= std::sqrt(v21_l2);
  }
  if (v23_l2 != 0.0)
  {
    v23 /= std::sqrt(v23_l2);
  }

  const CoordinateType bound(0.999999);

  const CoordinateType cos_theta = std::clamp(v21 * v23, -bound, bound);

  return std::acos(cos_theta);
}

template <typename TPoint>
auto
TriangleHelper<TPoint>::ComputeGravityCenter(const PointType & iP1, const PointType & iP2, const PointType & iP3)
  -> PointType
{
  return ComputeBarycenter(1., iP1, 1., iP2, 1., iP3);
}

template <typename TPoint>
auto
TriangleHelper<TPoint>::ComputeCircumCenter(const PointType & iP1, const PointType & iP2, const PointType & iP3)
  -> PointType
{
  const PointType oPt{};

  const CoordinateType a = iP2.SquaredEuclideanDistanceTo(iP3);
  const CoordinateType b = iP1.SquaredEuclideanDistanceTo(iP3);
  const CoordinateType c = iP2.SquaredEuclideanDistanceTo(iP1);

  CoordinateType Weight[3];
  Weight[0] = a * (b + c - a);
  Weight[1] = b * (c + a - b);
  Weight[2] = c * (a + b - c);

  return ComputeBarycenter(Weight[0], iP1, Weight[1], iP2, Weight[2], iP3);
}

template <typename TPoint>
auto
TriangleHelper<TPoint>::ComputeConstrainedCircumCenter(const PointType & iP1,
                                                       const PointType & iP2,
                                                       const PointType & iP3) -> PointType
{
  const CoordinateType a = iP2.SquaredEuclideanDistanceTo(iP3);
  const CoordinateType b = iP1.SquaredEuclideanDistanceTo(iP3);
  const CoordinateType c = iP2.SquaredEuclideanDistanceTo(iP1);

  CoordinateType Weight[3] = { a * (b + c - a), b * (c + a - b), c * (a + b - c) };

  for (auto & i : Weight)
  {
    if (i < 0.0)
    {
      i = 0.;
    }
  }

  return ComputeBarycenter(Weight[0], iP1, Weight[1], iP2, Weight[2], iP3);
}

template <typename TPoint>
auto
TriangleHelper<TPoint>::ComputeArea(const PointType & iP1, const PointType & iP2, const PointType & iP3)
  -> CoordinateType
{
  const CoordinateType a = iP2.EuclideanDistanceTo(iP3);
  const CoordinateType b = iP1.EuclideanDistanceTo(iP3);
  const CoordinateType c = iP2.EuclideanDistanceTo(iP1);

  const CoordinateType s = 0.5 * (a + b + c);

  return static_cast<CoordinateType>(std::sqrt(s * (s - a) * (s - b) * (s - c)));
}

template <typename TPoint>
auto
TriangleHelper<TPoint>::ComputeMixedArea(const PointType & iP1, const PointType & iP2, const PointType & iP3)
  -> CoordinateType
{
  using TriangleType = TriangleHelper<TPoint>;

  if (!TriangleType::IsObtuse(iP1, iP2, iP3))
  {
    auto sq_d01 = static_cast<CoordinateType>(iP1.SquaredEuclideanDistanceTo(iP2));
    auto sq_d02 = static_cast<CoordinateType>(iP1.SquaredEuclideanDistanceTo(iP3));

    const CoordinateType cot_theta_210 = TriangleType::Cotangent(iP3, iP2, iP1);
    const CoordinateType cot_theta_021 = TriangleType::Cotangent(iP1, iP3, iP2);

    return 0.125 * (sq_d02 * cot_theta_210 + sq_d01 * cot_theta_021);
  }

  auto area = static_cast<CoordinateType>(TriangleType::ComputeArea(iP1, iP2, iP3));

  if ((iP2 - iP1) * (iP3 - iP1) < CoordinateType{})
  {
    return 0.5 * area;
  }
  else
  {
    return 0.25 * area;
  }
}
} // namespace itk

#endif
