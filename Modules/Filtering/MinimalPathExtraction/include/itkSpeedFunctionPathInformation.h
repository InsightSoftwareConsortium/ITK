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
#ifndef itkSpeedFunctionPathInformation_h
#define itkSpeedFunctionPathInformation_h

#include "itkLightObject.h"
#include "itkObjectFactory.h"

#include <vector>

namespace itk
{

/** \class SpeedFunctionPathInformation
 * \brief  PathInfo class for encapsulating information about a path
 * for a SpeedFunctionToPathFilter Object.
 * The points are stored as follows: end, start, way0, way1, ..., wayN.
 * Each element of a path can be a set of points - e.g. "end" may be
 * derived from a segmentation mask. Alternatively start and end may
 * be the left and right sides of a 2d image, producing a minimal
 * path across the image.
 * Fronts are propagated in reverse order: wayN, ..., way1, way0, start.
 * (NOTE: a front is never propagated from end).
 *
 * The user must provide at least one PathInfo object using
 * SpeedFunctionToPathFilter::AddPathInfo().
 * If multiple PathInfo objects are added,
 * multiple paths are extracted and saved to separate filter outputs.
 *
 * \author Dan Mueller,
 * Queensland University of Technology, dan.muel[at]gmail.com
 *
 * \sa LightObject
 *
 * \ingroup MinimalPathExtraction
 */
template <typename TPoint>
class ITK_TEMPLATE_EXPORT SpeedFunctionPathInformation : public LightObject
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(SpeedFunctionPathInformation);

  /** Standard class type alias. */
  using Self = SpeedFunctionPathInformation;
  using Superclass = LightObject;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;


  /** Run-time type information (and related methods). */
  itkOverrideGetNameOfClassMacro(SpeedFunctionPathInformation);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Some point type alias. */
  using PointType = TPoint;
  using PointsContainerType = std::vector<PointType>;

  void
  ClearInfo();

  void
  SetStartPoint(const PointType & start);

  void
  SetEndPoint(const PointType & end);

  void
  AddWayPoint(const PointType & way);


  /** Methods for adding extended path components **/
  void
  SetStartPoint(const PointsContainerType & start);

  void
  SetEndPoint(const PointsContainerType & end);

  void
  AddWayPoint(const PointsContainerType & way);

  // methods for modifying path seeds - needed when using
  // an extended seed.
  void
  SetCurrent(const PointsContainerType & newcurrent);
  void
  SetPrevious(const PointsContainerType & newprevious);
  void
  SetNext(const PointsContainerType & newnext);

  void
  SetCurrent(const PointType & current);
  void
  SetPrevious(const PointType & newprevious);
  void
  SetNext(const PointType & newnext);

  void
  Advance();

  unsigned int
  GetNumberOfPoints() const;

  const PointsContainerType &
  GetStartPoint() const;

  const PointsContainerType &
  GetEndPoint() const;

  const PointsContainerType &
  GetWayPoint(SizeValueType i) const;

  bool
  HasNextFront() const;

  const PointsContainerType &
  GetCurrentFrontAndAdvance();

  const PointsContainerType &
  PeekCurrentFront() const;

  const PointsContainerType &
  PeekNextFront() const;

  const PointsContainerType &
  PeekPreviousFront() const;


protected:
  SpeedFunctionPathInformation();
  ~SpeedFunctionPathInformation() override;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  std::vector<PointsContainerType> m_Information;
  SizeValueType                    m_Front;


  PointsContainerType
  PtoPVec(const PointType & P)
  {
    PointsContainerType V(1);
    V[0] = P;
    return (V);
  }
};


} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkSpeedFunctionPathInformation.hxx"
#endif

#endif
