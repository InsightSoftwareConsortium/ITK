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
#ifndef itkTubeSpatialObjectPoint_h
#define itkTubeSpatialObjectPoint_h

#include "itkTubeSpatialObject.h"
#include "itkSpatialObjectPoint.h"
#include "itkCovariantVector.h"
#include "vnl/vnl_vector_fixed.h"

namespace itk
{
/** \class TubeSpatialObjectPoint
 * \brief Point used for a tube definition
 *
 * This class contains all the functions necessary to define a point
 * that can be used to build tubes.
 *
 * \sa TubeSpatialObject
 * \ingroup ITKSpatialObjects
 */

template< unsigned int TPointDimension = 3>
class ITK_TEMPLATE_EXPORT TubeSpatialObjectPoint:
  public SpatialObjectPoint< TPointDimension >
{
public:

  using Self = TubeSpatialObjectPoint;
  using Superclass = SpatialObjectPoint< TPointDimension >;
  using PointType = Point< double, TPointDimension >;
  using VectorType = Vector< double, TPointDimension >;
  using CovariantVectorType = CovariantVector< double, TPointDimension >;

  /** Constructor. This one defines the number of dimensions in the
   * TubeSpatialObjectPoint */
  TubeSpatialObjectPoint();

  /** Default destructor. */
  ~TubeSpatialObjectPoint() override;

  /** Get R */
  double GetRadiusInObjectSpace() const
  { return m_RadiusInObjectSpace; }

  /** Get R */
  double GetRadius() const;

  /** Set R */
  void SetRadiusInObjectSpace(double newR)
  { m_RadiusInObjectSpace = newR; }

  /** Set R */
  void SetRadius(double newR);

  /** Get the tangent in Object Space */
  const VectorType & GetTangentInObjectSpace() const
  { return m_TangetnInObjectSpace; }

  /** Get the tangent in World Space */
  const VectorType & GetTangent() const;

  /** Set the tangent in object space. */
  void SetTangentInObjectSpace(const VectorType & newT)
  { m_TangentInObjectSpace = newT; }

  /** Set the tangent in World Space. */
  void SetTangent(const VectorType & newT);

  /** Get V1 in Object space */
  const CovariantVectorType & GetNormal1InObjectSpace() const
  { return m_Normal1InObjectSpace; }

  /** Get V1 in World space */
  const CovariantVectorType & GetNormal1() const;

  /** Set V1 */
  void SetNormal1InObjectSpace(const CovariantVectorType & newV1);
  { m_Normal1InObjectSpace = newV1; }

  /** Set V1 */
  void SetNormal1(const CovariantVectorType & newV1);

  /** Get V2 */
  const CovariantVectorType & GetNormal2InObjectSpace() const
  { return m_Normal2InObjectSpace; }

  /** Get V2 */
  const CovariantVectorType & GetNormal2() const;

  /** Set V2 */
  void SetNormal2InObjectSpace(const CovariantVectorType & newV2);
  { m_Normal2InObjectSpace = newV2; }

  /** Set V2 */
  void SetNormal2(const CovariantVectorType & newV2);

  void SetRidgness( double ridgeness )
  { m_Ridgeness = ridgeness; }

  double GetRidgeness()
  { return m_Ridgeness; }

  void SetMedialness( double medialness )
  { m_Medialness = medialness; }

  double GetMedialness()
  { return m_Medialness; }

  void SetBranchness( double branchness )
  { m_Branchness = branchness; }

  double GetBranchness()
  { return m_Branchness; }

  /** Copy one TubeSpatialObjectPoint to another */
  Self & operator=(const TubeSpatialObjectPoint & rhs);

protected:

  VectorType          m_TangentInObjectSpace;
  CovariantVectorType m_Normal1InObjectSpace;
  CovariantVectorType m_Normal2InObjectSpace;

  /** The radius of the tube point */
  double m_RadiusInObjectSpace;

  /** Print the object */
  void PrintSelf(std::ostream & os, Indent indent) const override;

};

} // end of namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkTubeSpatialObjectPoint.hxx"
#endif

#endif // itkTubeSpatialObjectPoint_h
