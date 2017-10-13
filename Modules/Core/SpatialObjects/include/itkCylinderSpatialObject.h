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

  typedef CylinderSpatialObject                        Self;
  typedef double                                       ScalarType;
  typedef SmartPointer< Self >                         Pointer;
  typedef SmartPointer< const Self >                   ConstPointer;
  typedef SpatialObject< 3 >                           Superclass;
  typedef SmartPointer< Superclass >                   SuperclassPointer;
  typedef Superclass::PointType                        PointType;
  typedef Superclass::TransformType                    TransformType;
  typedef Superclass::BoundingBoxType                  BoundingBoxType;
  typedef VectorContainer< IdentifierType, PointType > PointContainerType;
  typedef SmartPointer< PointContainerType >           PointContainerPointer;

  itkStaticConstMacro(NumberOfDimension, unsigned int,
                      3);

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
  virtual bool ValueAt(const PointType & point, double & value,
                       unsigned int depth = 0,
                       char *name = ITK_NULLPTR) const ITK_OVERRIDE;

  /** Return true if the object provides a method to evaluate the value
   * at the specified point, false otherwise. */
  virtual bool IsEvaluableAt(const PointType & point,
                             unsigned int depth = 0,
                             char *name = ITK_NULLPTR) const ITK_OVERRIDE;

  /** Test whether a point is inside or outside the object */
  virtual bool IsInside(const PointType & point,
                        unsigned int depth,
                        char *) const ITK_OVERRIDE;

  /** Test whether a point is inside or outside the object
   *  For computational speed purposes, it is faster if the method does not
   *  check the name of the class and the current depth */
  virtual bool IsInside(const PointType & point) const;

  /** Get the boundaries of a specific object.  This function needs to
   *  be called every time one of the object's components is
   *  changed. */
  virtual bool ComputeLocalBoundingBox() const ITK_OVERRIDE;

protected:
  ITK_DISALLOW_COPY_AND_ASSIGN(CylinderSpatialObject);

  CylinderSpatialObject();
  ~CylinderSpatialObject() ITK_OVERRIDE;

  double m_Radius;
  double m_Height;

  /** Print the object informations in a stream. */
  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;
};
} // end namespace itk

#endif // itkCylinderSpatialObject_h
