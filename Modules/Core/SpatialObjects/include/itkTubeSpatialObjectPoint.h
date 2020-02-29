/*=========================================================================
 *
 *  Copyright NumFOCUS
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
#ifndef itkTubeSpatialObjectPoint_h
#define itkTubeSpatialObjectPoint_h

#include "itkSpatialObjectPoint.h"
#include "itkCovariantVector.h"
#include "vnl/vnl_vector_fixed.h"

namespace itk
{
/**
 *\class TubeSpatialObjectPoint
 * \brief Point used for a tube definition
 *
 * This class contains all the functions necessary to define a point
 * that can be used to build tubes.
 *
 * \sa TubeSpatialObject
 * \ingroup ITKSpatialObjects
 */

template <unsigned int TPointDimension = 3>
class ITK_TEMPLATE_EXPORT TubeSpatialObjectPoint : public SpatialObjectPoint<TPointDimension>
{
public:
  using Self = TubeSpatialObjectPoint;
  using Superclass = SpatialObjectPoint<TPointDimension>;
  using PointType = Point<double, TPointDimension>;
  using VectorType = Vector<double, TPointDimension>;
  using CovariantVectorType = CovariantVector<double, TPointDimension>;

  /** Constructor. This one defines the number of dimensions in the
   * TubeSpatialObjectPoint */
  TubeSpatialObjectPoint();

  /** Default destructor. */
  ~TubeSpatialObjectPoint() override = default;

  /** Get R */
  double
  GetRadiusInObjectSpace() const
  {
    return m_RadiusInObjectSpace;
  }

  /** Get R */
  double
  GetRadiusInWorldSpace() const;

  /** Set R */
  void
  SetRadiusInObjectSpace(double newR)
  {
    m_RadiusInObjectSpace = newR;
  }

  /** Set R */
  void
  SetRadiusInWorldSpace(double newR);

  /** Get the tangent in Object Space */
  const VectorType &
  GetTangentInObjectSpace() const
  {
    return m_TangentInObjectSpace;
  }

  /** Get the tangent in World Space */
  const VectorType
  GetTangentInWorldSpace() const;

  /** Set the tangent in object space. */
  void
  SetTangentInObjectSpace(const VectorType & newT)
  {
    m_TangentInObjectSpace = newT;
  }

  /** Set the tangent in World Space. */
  void
  SetTangentInWorldSpace(const VectorType & newT);

  /** Get V1 in Object space */
  const CovariantVectorType &
  GetNormal1InObjectSpace() const
  {
    return m_Normal1InObjectSpace;
  }

  /** Get V1 in World space */
  const CovariantVectorType
  GetNormal1InWorldSpace() const;

  /** Set V1 */
  void
  SetNormal1InObjectSpace(const CovariantVectorType & newV1)
  {
    m_Normal1InObjectSpace = newV1;
  }

  /** Set V1 */
  void
  SetNormal1InWorldSpace(const CovariantVectorType & newV1);

  /** Get V2 */
  const CovariantVectorType &
  GetNormal2InObjectSpace() const
  {
    return m_Normal2InObjectSpace;
  }

  /** Get V2 */
  const CovariantVectorType
  GetNormal2InWorldSpace() const;

  /** Set V2 */
  void
  SetNormal2InObjectSpace(const CovariantVectorType & newV2)
  {
    m_Normal2InObjectSpace = newV2;
  }

  /** Set V2 */
  void
  SetNormal2InWorldSpace(const CovariantVectorType & newV2);

  void
  SetRidgeness(double ridgeness)
  {
    m_Ridgeness = ridgeness;
  }

  double
  GetRidgeness() const
  {
    return m_Ridgeness;
  }

  void
  SetCurvature(double curvature)
  {
    m_Curvature = curvature;
  }

  double
  GetCurvature() const
  {
    return m_Curvature;
  }

  void
  SetLevelness(double levelness)
  {
    m_Levelness = levelness;
  }

  double
  GetLevelness() const
  {
    return m_Levelness;
  }

  void
  SetRoundness(double roundness)
  {
    m_Roundness = roundness;
  }

  double
  GetRoundness() const
  {
    return m_Roundness;
  }

  void
  SetIntensity(double intensity)
  {
    m_Intensity = intensity;
  }

  double
  GetIntensity() const
  {
    return m_Intensity;
  }

  void
  SetMedialness(double medialness)
  {
    m_Medialness = medialness;
  }

  double
  GetMedialness() const
  {
    return m_Medialness;
  }

  void
  SetBranchness(double branchness)
  {
    m_Branchness = branchness;
  }

  double
  GetBranchness() const
  {
    return m_Branchness;
  }

  void
  SetAlpha1(double alpha1)
  {
    m_Alpha1 = alpha1;
  }

  double
  GetAlpha1() const
  {
    return m_Alpha1;
  }

  void
  SetAlpha2(double alpha2)
  {
    m_Alpha2 = alpha2;
  }

  double
  GetAlpha2() const
  {
    return m_Alpha2;
  }

  void
  SetAlpha3(double alpha3)
  {
    m_Alpha3 = alpha3;
  }

  double
  GetAlpha3() const
  {
    return m_Alpha3;
  }

  /** Copy one TubeSpatialObjectPoint to another */
  Self &
  operator=(const TubeSpatialObjectPoint & rhs);

protected:
  VectorType          m_TangentInObjectSpace;
  CovariantVectorType m_Normal1InObjectSpace;
  CovariantVectorType m_Normal2InObjectSpace;

  double m_Branchness;
  double m_Medialness;
  double m_Ridgeness;
  double m_Curvature;
  double m_Levelness;
  double m_Roundness;
  double m_Intensity;
  double m_Alpha1;
  double m_Alpha2;
  double m_Alpha3;

  /** The radius of the tube point */
  double m_RadiusInObjectSpace;

  /** Print the object */
  void
  PrintSelf(std::ostream & os, Indent indent) const override;
};

} // end of namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkTubeSpatialObjectPoint.hxx"
#endif

#endif // itkTubeSpatialObjectPoint_h
