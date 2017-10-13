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
#ifndef itkEllipseSpatialObject_h
#define itkEllipseSpatialObject_h

#include "itkSpatialObject.h"
#include "itkAffineTransform.h"
#include "itkFixedArray.h"

namespace itk
{
/** \class EllipseSpatialObject
 *
 * \brief TODO
 * \ingroup ITKSpatialObjects
 *
 * \wiki
 * \wikiexample{SpatialObjects/EllipseSpatialObject,Ellipse}
 * \endwiki
 */

template< unsigned int TDimension = 3 >
class ITK_TEMPLATE_EXPORT EllipseSpatialObject:
  public SpatialObject< TDimension >
{
public:

  typedef EllipseSpatialObject                         Self;
  typedef double                                       ScalarType;
  typedef SmartPointer< Self >                         Pointer;
  typedef SmartPointer< const Self >                   ConstPointer;
  typedef SpatialObject< TDimension >                  Superclass;
  typedef SmartPointer< Superclass >                   SuperclassPointer;
  typedef typename Superclass::PointType               PointType;
  typedef typename Superclass::TransformType           TransformType;
  typedef typename Superclass::BoundingBoxType         BoundingBoxType;
  typedef VectorContainer< IdentifierType, PointType > PointContainerType;
  typedef SmartPointer< PointContainerType >           PointContainerPointer;

  typedef FixedArray< double, TDimension > ArrayType;
  itkStaticConstMacro(NumberOfDimension, unsigned int,
                      TDimension);

  itkNewMacro(Self);
  itkTypeMacro(EllipseSpatialObject, SpatialObject);

  /** Set all radii to the same radius value.  Each radius is
   *  half the length of one axis of the ellipse.  */
  void SetRadius(double radius);

  /** Set radii via an array of radius values */
  itkSetMacro(Radius, ArrayType);

  /** Get radii via an array of radius values */
  itkGetConstReferenceMacro(Radius, ArrayType);

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

  /** Copy the information from another SpatialObject */
  void CopyInformation(const DataObject *data) ITK_OVERRIDE;

protected:
  ITK_DISALLOW_COPY_AND_ASSIGN(EllipseSpatialObject);

  EllipseSpatialObject();
  ~EllipseSpatialObject() ITK_OVERRIDE;

  ArrayType m_Radius;

  /** Print the object informations in a stream. */
  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkEllipseSpatialObject.hxx"
#endif

#endif // itkEllipseSpatialObject_h
