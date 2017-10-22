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
#ifndef itkPlaneSpatialObject_h
#define itkPlaneSpatialObject_h

#include "itkSpatialObject.h"
#include "itkAffineTransform.h"

namespace itk
{
/** \class PlaneSpatialObject
 * A plane spatial object is defined by two points
 * \brief
 * \ingroup ITKSpatialObjects
 *
 * \wiki
 * \wikiexample{SpatialObjects/PlaneSpatialObject,Plane spatial object}
 * \endwiki
 */

template< unsigned int TDimension = 3  >
class ITK_TEMPLATE_EXPORT PlaneSpatialObject:
  public SpatialObject< TDimension >
{
public:

  /** Standard typedefs */
  typedef PlaneSpatialObject                           Self;
  typedef double                                       ScalarType;
  typedef SmartPointer< Self >                         Pointer;
  typedef SmartPointer< const Self >                   ConstPointer;
  typedef SpatialObject< TDimension >                  Superclass;
  typedef SmartPointer< Superclass >                   SuperclassPointer;
  typedef typename Superclass::PointType               PointType;
  typedef typename Superclass::TransformType           TransformType;
  typedef VectorContainer< IdentifierType, PointType > PointContainerType;
  typedef SmartPointer< PointContainerType >           PointContainerPointer;
  typedef typename Superclass::BoundingBoxType         BoundingBoxType;

  itkStaticConstMacro(NumberOfDimension, unsigned int,
                      TDimension);

  itkNewMacro(Self);
  itkTypeMacro(PlaneSpatialObject, SpatialObject);

  /** Returns a degree of membership to the object.
   *  That's useful for fuzzy objects. */
  virtual bool ValueAt(const PointType & point, double & value,
                       unsigned int depth = 0, char *name = ITK_NULLPTR) const ITK_OVERRIDE;

  /** return ture if the object provides a method to evaluate the value
   * at the specified point, else otherwise. */
  virtual bool IsEvaluableAt(const PointType & point,
                             unsigned int depth = 0, char *name = ITK_NULLPTR) const ITK_OVERRIDE;

  /** Test whether a point is inside or outside the object */
  virtual bool IsInside(const PointType & point,
                        unsigned int depth, char *name) const ITK_OVERRIDE;

  /** Test whether a point is inside or outside the object
   *  For computational speed purposes, it is faster if the method does not
   *  check the name of the class and the current depth */
  virtual bool IsInside(const PointType & point) const;

  /** provide a method to get the boundaries of
  *  a specific object. Basically, this function need to be called
  *  every time one of the object component is changed. */
  virtual bool ComputeLocalBoundingBox() const ITK_OVERRIDE;

  itkSetMacro(LowerPoint, PointType);
  itkSetMacro(UpperPoint, PointType);
  itkGetMacro(LowerPoint, PointType);
  itkGetMacro(UpperPoint, PointType);

protected:
  ITK_DISALLOW_COPY_AND_ASSIGN(PlaneSpatialObject);

  PlaneSpatialObject();
  ~PlaneSpatialObject() ITK_OVERRIDE;

  PointType m_LowerPoint;
  PointType m_UpperPoint;

  /** Print the object informations in a stream. */
  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkPlaneSpatialObject.hxx"
#endif

#endif // itkPlaneSpatialObject_h
