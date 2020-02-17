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
#ifndef itkEllipseSpatialObject_h
#define itkEllipseSpatialObject_h

#include "itkSpatialObject.h"
#include "itkAffineTransform.h"
#include "itkFixedArray.h"

namespace itk
{
/**
 *\class EllipseSpatialObject
 *
 * \ingroup ITKSpatialObjects
 *
 * \sphinx
 * \sphinxexample{Core/SpatialObjects/Ellipse,Ellipse}
 * \endsphinx
 */

template <unsigned int TDimension = 3>
class ITK_TEMPLATE_EXPORT EllipseSpatialObject : public SpatialObject<TDimension>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(EllipseSpatialObject);

  using Self = EllipseSpatialObject;
  using ScalarType = double;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;
  using Superclass = SpatialObject<TDimension>;
  using SuperclassPointer = SmartPointer<Superclass>;
  using PointType = typename Superclass::PointType;
  using TransformType = typename Superclass::TransformType;
  using BoundingBoxType = typename Superclass::BoundingBoxType;
  using PointContainerType = VectorContainer<IdentifierType, PointType>;
  using PointContainerPointer = SmartPointer<PointContainerType>;

  using ArrayType = FixedArray<double, TDimension>;

  static constexpr unsigned int ObjectDimension = TDimension;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(EllipseSpatialObject, SpatialObject);

  /** Reset the spatial object to its initial condition, yet preserves
   *   Id, Parent, and Child information */
  void
  Clear() override;

  /** Set all radii to the same radius value.  Each radius is
   *  half the length of one axis of the ellipse.  */
  void
  SetRadiusInObjectSpace(double radius);

  /** Set radii via an array of radius values */
  itkSetMacro(RadiusInObjectSpace, ArrayType);

  /** Get radii via an array of radius values */
  itkGetConstReferenceMacro(RadiusInObjectSpace, ArrayType);

  /** Set center point in object space. */
  itkSetMacro(CenterInObjectSpace, PointType);

  /** Get center in object space */
  itkGetConstReferenceMacro(CenterInObjectSpace, PointType);

  /** Test whether a point is inside or outside the object */
  bool
  IsInsideInObjectSpace(const PointType & point) const override;

  /* Avoid hiding the overload that supports depth and name arguments */
  using Superclass::IsInsideInObjectSpace;

#if !defined(ITK_LEGACY_REMOVE)
  itkLegacyMacro(void SetRadius(double radius)) { this->SetRadiusInObjectSpace(radius); }

  itkLegacyMacro(void SetRadius(ArrayType radii)) { this->SetRadiusInObjectSpace(radii); }

  itkLegacyMacro(ArrayType GetRadius() const) { return this->GetRadiusInObjectSpace(); }

  itkLegacyMacro(void SetRadiiInObjectSpace(ArrayType radii)) { this->SetRadiusInObjectSpace(radii); }
#endif
protected:
  /** Get the boundaries of a specific object.  This function needs to
   *  be called every time one of the object's components is
   *  changed. */
  void
  ComputeMyBoundingBox() override;

  EllipseSpatialObject();
  ~EllipseSpatialObject() override = default;

  /** Print the object informations in a stream. */
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  typename LightObject::Pointer
  InternalClone() const override;

private:
  /* object space */
  ArrayType m_RadiusInObjectSpace;
  PointType m_CenterInObjectSpace;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkEllipseSpatialObject.hxx"
#endif

#endif // itkEllipseSpatialObject_h
