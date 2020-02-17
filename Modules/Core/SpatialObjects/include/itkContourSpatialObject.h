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
#ifndef itkContourSpatialObject_h
#define itkContourSpatialObject_h

#include <list>

#include "itkPointBasedSpatialObject.h"
#include "itkContourSpatialObjectPoint.h"

namespace itk
{
/***\class ContourSpatialObjectEnums
 *
 * \brief Enum classes for the ControuSpatialObject class.
 *
 * \ingroup ITKSpatialObjects
 */
class ContourSpatialObjectEnums
{
public:
  /***\class InterpolationMethodEnum
   * \ingroup ITKSpatialObjects
   * Hold interpolation method type
   */
  enum class InterpolationMethod : uint8_t
  {
    NO_INTERPOLATION = 0,
    EXPLICIT_INTERPOLATION,
    BEZIER_INTERPOLATION,
    LINEAR_INTERPOLATION
  };
};
// Define how to print enumeration
extern ITKSpatialObjects_EXPORT std::ostream &
                                operator<<(std::ostream & out, ContourSpatialObjectEnums::InterpolationMethod value);

/**
 * \class ContourSpatialObject
 * \brief Representation of a Contour based on the spatial object classes.
 *
 * The Contour is basically defined by a set of points which are inside this
 * blob
 *
 * \sa SpatialObjectPoint
 * \ingroup ITKSpatialObjects
 *
 * \sphinx
 * \sphinxexample{Core/SpatialObjects/{{ContourSpatialObject,Contour Spacial Object}
 * \endsphinx
 */

template <unsigned int TDimension = 3>
class ITK_TEMPLATE_EXPORT ContourSpatialObject
  : public PointBasedSpatialObject<TDimension, ContourSpatialObjectPoint<TDimension>>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(ContourSpatialObject);

  using Self = ContourSpatialObject;
  using Superclass = PointBasedSpatialObject<TDimension, ContourSpatialObjectPoint<TDimension>>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  using ScalarType = double;

  using ContourPointType = ContourSpatialObjectPoint<TDimension>;
  using ContourPointListType = std::vector<ContourPointType>;
  using ControlPointType = ContourSpatialObjectPoint<TDimension>;
  using ControlPointListType = std::vector<ControlPointType>;

  using PointType = typename Superclass::PointType;
  using TransformType = typename Superclass::TransformType;
  using BoundingBoxType = typename Superclass::BoundingBoxType;
  using PointContainerType = VectorContainer<IdentifierType, PointType>;
  using PointContainerPointer = SmartPointer<PointContainerType>;

  using InterpolationMethodEnum = ContourSpatialObjectEnums::InterpolationMethod;
#if !defined(ITK_LEGACY_REMOVE)
  /**Exposes enums values for backwards compatibility*/
  // We need to expose the enum values at the class level
  // for backwards compatibility
  static constexpr InterpolationMethodEnum NO_INTERPOLATION = InterpolationMethodEnum::NO_INTERPOLATION;
  static constexpr InterpolationMethodEnum EXPLICIT_INTERPOLATION = InterpolationMethodEnum::EXPLICIT_INTERPOLATION;
  static constexpr InterpolationMethodEnum BEZIER_INTERPOLATION = InterpolationMethodEnum::BEZIER_INTERPOLATION;
  static constexpr InterpolationMethodEnum LINEAR_INTERPOLATION = InterpolationMethodEnum::LINEAR_INTERPOLATION;
#endif

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Method for creation through the object factory. */
  itkTypeMacro(ContourSpatialObject, PointBasedSpatialObject);

  /** Reset the spatial object to its initial condition, yet preserves
   *   Id, Parent, and Child information */
  void
  Clear() override;

  /** Returns a reference to the list of the control points. */
  ControlPointListType &
  GetControlPoints()
  {
    return m_ControlPoints;
  }

  /** Returns a reference to the list of the control points. */
  const ControlPointListType &
  GetControlPoints() const
  {
    return m_ControlPoints;
  }

  /** Set the list of control points. */
  void
  SetControlPoints(const ControlPointListType & newPoints);

  /** Set the list of control points. */
  void
  AddControlPoint(const ControlPointType & point);

  /** Return a control point in the list given the index */
  const ControlPointType *
  GetControlPoint(IdentifierType id) const
  {
    return &(m_ControlPoints[id]);
  }

  /** Return a control point in the list given the index */
  ControlPointType *
  GetControlPoint(IdentifierType id)
  {
    return &(m_ControlPoints[id]);
  }

  /** Return the number of control points in the list */
  SizeValueType
  GetNumberOfControlPoints() const
  {
    return static_cast<SizeValueType>(m_ControlPoints.size());
  }

  /** Set the interpolation type */
  itkSetEnumMacro(InterpolationMethod, InterpolationMethodEnum)

    /** Get the interpolation type */
    itkGetConstMacro(InterpolationMethod, InterpolationMethodEnum)

    /** Set the interpolation factor, e.g., factor of 2 means 2 interpolated
     *    points created for every control point. */
    itkSetMacro(InterpolationFactor, unsigned int)

    /** Get the interpolation factor */
    itkGetConstMacro(InterpolationFactor, unsigned int)

    /** Set if the contour is closed */
    itkSetMacro(IsClosed, bool);

  /** Get if the contour is closed */
  itkGetConstMacro(IsClosed, bool);

  /** Get the axis-normal orientation of the contour */
  int
  GetOrientationInObjectSpace() const;

  /** Set the slice attached to the contour.
   *   Set -1 to indicate no attachment */
  itkSetMacro(AttachedToSlice, int);

  /** Get the slice attached to the contour.
   *   Return -1 if not attached. */
  itkGetConstMacro(AttachedToSlice, int);

  /** Apply the interpolator to generate points from the control points */
  void
  Update() override;

protected:
  ContourSpatialObject();
  ~ContourSpatialObject() override = default;

  /** Method to print the object. */
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  typename LightObject::Pointer
  InternalClone() const override;

private:
  ContourPointListType m_ControlPoints;

  InterpolationMethodEnum m_InterpolationMethod;
  unsigned int            m_InterpolationFactor;

  mutable bool             m_IsClosed;
  mutable int              m_OrientationInObjectSpace;
  mutable ModifiedTimeType m_OrientationInObjectSpaceMTime;
  int                      m_AttachedToSlice;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkContourSpatialObject.hxx"
#endif

#endif // itkContourSpatialObject_h
