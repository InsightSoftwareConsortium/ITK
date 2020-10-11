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
#ifndef itkBoxSpatialObject_h
#define itkBoxSpatialObject_h

#include "itkSpatialObject.h"
#include "itkAffineTransform.h"
#include "itkFixedArray.h"

namespace itk
{
/**
 *\class BoxSpatialObject
 *
 * \brief
 * The class may be used to represent N-dimensional boxes.
 * In two dimensions it is a rectangle, In three dimensions it is a cuboid...
 *
 * \ingroup ITKSpatialObjects
 */
template <unsigned int TDimension = 3>
class ITK_TEMPLATE_EXPORT BoxSpatialObject : public SpatialObject<TDimension>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(BoxSpatialObject);

  using Self = BoxSpatialObject;
  using ScalarType = double;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;
  using Superclass = SpatialObject<TDimension>;
  using SuperclassPointer = SmartPointer<Superclass>;
  using PointType = typename Superclass::PointType;
  using TransformType = typename Superclass::TransformType;
  using BoundingBoxType = typename Superclass::BoundingBoxType;
  using SizeType = FixedArray<double, TDimension>;
  using PointsContainerType = typename BoundingBoxType::PointsContainer;

  itkNewMacro(Self);
  itkTypeMacro(BoxSpatialObject, SpatialObject);

  /** Reset the spatial object to its initial condition, yet preserves
   *   Id, Parent, and Child information */
  void
  Clear() override;

  /** Set the size of the box spatial object in object space. */
  itkSetMacro(SizeInObjectSpace, SizeType);

  /** Get the size of the box spatial object in object space. */
  itkGetConstReferenceMacro(SizeInObjectSpace, SizeType);

  /** Set the position of the box spatial object in object space. */
  itkSetMacro(PositionInObjectSpace, PointType);

  /** Get the position of the box spatial object in object space. */
  itkGetConstReferenceMacro(PositionInObjectSpace, PointType);

  /** Test whether a point is inside or outside the object */
  bool
  IsInsideInObjectSpace(const PointType & point) const override;

  /* Avoid hiding the overload that supports depth and name arguments */
  using Superclass::IsInsideInObjectSpace;

protected:
  /** Get the boundaries of a specific object.  This function needs to
   *  be called every time one of the object's components is
   *  changed. */
  void
  ComputeMyBoundingBox() override;

  BoxSpatialObject();
  ~BoxSpatialObject() override = default;

  /** Print the object informations in a stream. */
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  typename LightObject::Pointer
  InternalClone() const override;

private:
  /** object space */
  SizeType  m_SizeInObjectSpace;
  PointType m_PositionInObjectSpace;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkBoxSpatialObject.hxx"
#endif

#endif // itkBoxSpatialObject_h
