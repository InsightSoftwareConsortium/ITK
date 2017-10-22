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
#ifndef itkVesselTubeSpatialObjectPoint_h
#define itkVesselTubeSpatialObjectPoint_h

#include "itkTubeSpatialObjectPoint.h"
#include "vnl/vnl_vector_fixed.h"

namespace itk
{
/** \class VesselTubeSpatialObjectPoint
 * \brief Point used for a tube definition
 *
 * This class contains all the functions necessary to define a point
 * that can be used to build tubes.
 *
 * \sa VesselTubeSpatialObject
 * \ingroup ITKSpatialObjects
 */

template< unsigned int TPointDimension = 3 >
class ITK_TEMPLATE_EXPORT VesselTubeSpatialObjectPoint:
  public TubeSpatialObjectPoint< TPointDimension >
{
public:

  typedef VesselTubeSpatialObjectPoint               Self;
  typedef TubeSpatialObjectPoint< TPointDimension >  Superclass;
  typedef Point< double, TPointDimension >           PointType;
  typedef Vector< double, TPointDimension >          VectorType;
  typedef CovariantVector< double, TPointDimension > CovariantVectorType;

  /** Constructor. This one defines the number of dimensions in the
   * VesselTubeSpatialObjectPoint */
  VesselTubeSpatialObjectPoint();

  /** Default destructor. */
  virtual ~VesselTubeSpatialObjectPoint() ITK_OVERRIDE;

  /** Get Medialness */
  float GetMedialness() const;

  /** Set Medialness */
  void SetMedialness(const float newMedialness);

  /** Get Ridgeness */
  float GetRidgeness() const;

  /** Set Ridgeness */
  void SetRidgeness(const float newRidgeness);

  /** Get Branchness */
  float GetBranchness() const;

  /** Set Branchness */
  void SetBranchness(const float newBranchness);

  /** Get Mark */
  bool GetMark() const;

  /** Set Mark */
  void SetMark(const bool newMark);

  /** Get Alpha1 */
  float GetAlpha1() const;

  /** Set Alpha1 */
  void SetAlpha1(const float newAlpha);

  /** Get Alpha2 */
  float GetAlpha2() const;

  /** Set Alpha2 */
  void SetAlpha2(const float newAlpha);

  /** Get Alpha3 */
  float GetAlpha3() const;

  /** Set Alpha3 */
  void SetAlpha3(const float newAlpha);

  /** Copy one VesselTubeSpatialObjectPoint to another */
  Self & operator=(const VesselTubeSpatialObjectPoint & rhs);

protected:

  /** First of 3 alpha values */
  float m_Alpha1;

  /** Second of 3 alpha values */
  float m_Alpha2;

  /** Third of 3 alpha values */
  float m_Alpha3;

  /** The medialness of the tube point */
  float m_Medialness;

  /** The ridgeness of the tube point */
  float m_Ridgeness;

  /** The branchness of the tube point */
  float m_Branchness;

  /** Is the tube point marked (selected) ? */
  bool m_Mark;

  /** Print the object */
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;
};
} // end of namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkVesselTubeSpatialObjectPoint.hxx"
#endif

#endif // itkVesselTubeSpatialObjectPoint_h
