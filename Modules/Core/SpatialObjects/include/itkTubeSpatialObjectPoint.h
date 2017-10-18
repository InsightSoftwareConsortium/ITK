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

template< unsigned int TPointDimension = 3 >
class ITK_TEMPLATE_EXPORT TubeSpatialObjectPoint:
  public SpatialObjectPoint< TPointDimension >
{
public:

  typedef TubeSpatialObjectPoint                     Self;
  typedef SpatialObjectPoint< TPointDimension >      Superclass;
  typedef Point< double, TPointDimension >           PointType;
  typedef Vector< double, TPointDimension >          VectorType;
  typedef CovariantVector< double, TPointDimension > CovariantVectorType;

  /** Constructor. This one defines the number of dimensions in the
   * TubeSpatialObjectPoint */
  TubeSpatialObjectPoint();

  /** Default destructor. */
  virtual ~TubeSpatialObjectPoint() ITK_OVERRIDE;

  /** Get the tangent */
  const VectorType & GetTangent() const;

  /** Set T. Couldn't use macros for these methods */
  void SetTangent(const VectorType & newT);

  void SetTangent(const double t0, const double t1);

  void SetTangent(const double t0, const double t1, const double t2);

  /** Get V1 */
  const CovariantVectorType & GetNormal1() const;

  /** Set V1 */
  void SetNormal1(const CovariantVectorType & newV1);

  void SetNormal1(const double v10, const double v11);

  void SetNormal1(const double v10, const double v11, const double v12);

  /** Get V2 */
  const CovariantVectorType & GetNormal2() const;

  /** Set V2 */
  void SetNormal2(const CovariantVectorType & newV2);

  void SetNormal2(const double v20, const double v21);

  void SetNormal2(const double v20, const double v21, const double v22);

  /** Get R */
  float GetRadius() const;

  /** Set R */
  void SetRadius(const float newR);

  /** Get number of dimensions */
  unsigned short int GetNumDimensions() const;

  /** Copy one TubeSpatialObjectPoint to another */
  Self & operator=(const TubeSpatialObjectPoint & rhs);

protected:

  VectorType          m_T;
  CovariantVectorType m_Normal1;
  CovariantVectorType m_Normal2;

  /** The radius of the tube point */
  float m_R;

  /** number of dimensions */
  unsigned short int m_NumDimensions;

  /** Print the object */
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;
};
} // end of namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkTubeSpatialObjectPoint.hxx"
#endif

#endif // itkTubeSpatialObjectPoint_h
