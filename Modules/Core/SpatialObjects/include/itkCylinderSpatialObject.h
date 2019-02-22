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
#ifndef itkCylinderSpatialObject_h
#define itkCylinderSpatialObject_h

#include "itkSpatialObject.h"
#include "itkAffineTransform.h"
#include "itkFixedArray.h"

namespace itk
{
/** \class CylinderSpatialObject
 *
 * \brief This class describe a cylinder in 3D only.
 * \ingroup ITKSpatialObjects
 */
class CylinderSpatialObject:
  public SpatialObject< 3 >
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(CylinderSpatialObject);

  using Self = CylinderSpatialObject;
  using ScalarType = double;
  using Pointer = SmartPointer< Self >;
  using ConstPointer = SmartPointer< const Self >;
  using Superclass = SpatialObject< 3 >;
  using SuperclassPointer = SmartPointer< Superclass >;
  using PointType = Superclass::PointType;
  using TransformType = Superclass::TransformType;
  using BoundingBoxType = Superclass::BoundingBoxType;
  using PointContainerType = VectorContainer< IdentifierType, PointType >;
  using PointContainerPointer = SmartPointer< PointContainerType >;

  static constexpr unsigned int NumberOfDimension = 3;

  itkNewMacro(Self);
  itkTypeMacro(CylinderSpatialObject, SpatialObject);

  /** Set/Get the radius */
  itkSetMacro(Radius, double);
  itkGetConstReferenceMacro(Radius, double);

  /** Set/Get the height */
  itkSetMacro(Height, double);
  itkGetConstReferenceMacro(Height, double);

  /** Returns a degree of membership to the object.
   *  That's useful for fuzzy objects. */
  bool ValueAt(const PointType & point, double & value,
                       unsigned int depth = 0,
                       char *name = nullptr) const override;

  /** Return true if the object provides a method to evaluate the value
   * at the specified point, false otherwise. */
  bool IsEvaluableAt(const PointType & point,
                             unsigned int depth = 0,
                             char *name = nullptr) const override;

  /** Test whether a point is inside or outside the object */
  bool IsInside(const PointType & point,
                        unsigned int depth,
                        char *) const override;

  /** Test whether a point is inside or outside the object
   *  For computational speed purposes, it is faster if the method does not
   *  check the name of the class and the current depth */
  virtual bool IsInside(const PointType & point) const;

  /** Get the boundaries of a specific object.  This function needs to
   *  be called every time one of the object's components is
   *  changed. */
  bool ComputeLocalBoundingBox() const override;

protected:
  CylinderSpatialObject();
  ~CylinderSpatialObject() override = default;

  double m_Radius;
  double m_Height;

  /** Print the object informations in a stream. */
  void PrintSelf(std::ostream & os, Indent indent) const override;
};
} // end namespace itk

#endif // itkCylinderSpatialObject_h
