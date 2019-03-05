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
#ifndef itkBoxSpatialObject_h
#define itkBoxSpatialObject_h

#include "itkSpatialObject.h"
#include "itkAffineTransform.h"
#include "itkFixedArray.h"

namespace itk
{
/** \class BoxSpatialObject
 *
 * \brief
 * The class may be used to represent N-dimensional boxes.
 * In two dimensions it is a rectangle, In three dimensions it is a cuboid...
 *
 * \ingroup ITKSpatialObjects
 */
template< unsigned int TDimension = 3 >
class ITK_TEMPLATE_EXPORT BoxSpatialObject:
  public SpatialObject< TDimension >
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(BoxSpatialObject);

  using Self = BoxSpatialObject;
  using ScalarType = double;
  using Pointer = SmartPointer< Self >;
  using ConstPointer = SmartPointer< const Self >;
  using Superclass = SpatialObject< TDimension >;
  using SuperclassPointer = SmartPointer< Superclass >;
  using PointType = typename Superclass::PointType;
  using TransformType = typename Superclass::TransformType;
  using BoundingBoxType = typename Superclass::BoundingBoxType;
  using SizeType = FixedArray< double, TDimension >;
  using PointContainerType = VectorContainer< IdentifierType, PointType >;

  itkNewMacro(Self);
  itkTypeMacro(BoxSpatialObject, SpatialObject);

  /** Set/Get the size of the box spatial object. */
  itkSetMacro(Size, SizeType);
  itkGetConstReferenceMacro(Size, SizeType);

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
  BoxSpatialObject();
  ~BoxSpatialObject() override = default;

  SizeType m_Size;

  /** Print the object informations in a stream. */
  void PrintSelf(std::ostream & os, Indent indent) const override;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkBoxSpatialObject.hxx"
#endif

#endif // itkBoxSpatialObject_h
