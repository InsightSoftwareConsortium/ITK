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
#ifndef itkPolygonSpatialObject_h
#define itkPolygonSpatialObject_h

#include "itkPointBasedSpatialObject.h"

namespace itk
{
/**
 *\class PolygonSpatialObject
 *
 * \brief TODO
 * \ingroup ITKSpatialObjects
 */
template <unsigned int TDimension = 3>
class ITK_TEMPLATE_EXPORT PolygonSpatialObject
  : public PointBasedSpatialObject<TDimension, SpatialObjectPoint<TDimension>>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(PolygonSpatialObject);

  using Self = PolygonSpatialObject<TDimension>;
  using Superclass = PointBasedSpatialObject<TDimension>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  using PolygonPointType = SpatialObjectPoint<TDimension>;
  using PolygonPointListType = std::vector<PolygonPointType>;

  using PointType = typename Superclass::PointType;
  using TransformType = typename Superclass::TransformType;
  using SpatialObjectPointType = typename Superclass::SpatialObjectPointType;

  using ObjectDimensionType = unsigned int;
  static constexpr ObjectDimensionType ObjectDimension = TDimension;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Method for creation through the object factory. */
  itkTypeMacro(PolygonSpatialObject, PointBasedSpatialObject);

  /** Reset the spatial object to its initial condition, yet preserves
   *   Id, Parent, and Child information */
  void
  Clear() override;

  /** Method returning plane alignment of strand */
  int
  GetOrientationInObjectSpace() const;

  /** Method sets the thickness of the current strand */
  itkSetMacro(ThicknessInObjectSpace, double);

  /** Method gets the thickness of the current strand */
  itkGetConstMacro(ThicknessInObjectSpace, double);

  /** Set if the contour is closed */
  itkSetMacro(IsClosed, bool);

  /** Get if the contour is closed */
  itkGetConstMacro(IsClosed, bool);

  itkBooleanMacro(IsClosed);

  /** Method returns area of polygon described by points */
  double
  MeasureAreaInObjectSpace() const;

  /** Method returns the volume of the strand */
  double
  MeasureVolumeInObjectSpace() const;

  /** Method returns the length of the perimeter */
  double
  MeasurePerimeterInObjectSpace() const;

  /** Test whether a point is inside or outside the object. */
  bool
  IsInsideInObjectSpace(const PointType & point) const override;

  /* Avoid hiding the overload that supports depth and name arguments */
  using Superclass::IsInsideInObjectSpace;

protected:
  PolygonSpatialObject();
  ~PolygonSpatialObject() override = default;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  typename LightObject::Pointer
  InternalClone() const override;

private:
  mutable bool             m_IsClosed;
  mutable int              m_OrientationInObjectSpace;
  mutable ModifiedTimeType m_OrientationInObjectSpaceMTime;
  double                   m_ThicknessInObjectSpace;
};

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkPolygonSpatialObject.hxx"
#endif

#endif // itkPolygonSpatialObject_h
