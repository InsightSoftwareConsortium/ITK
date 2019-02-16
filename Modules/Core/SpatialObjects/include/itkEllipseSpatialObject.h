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
  ITK_DISALLOW_COPY_AND_ASSIGN(EllipseSpatialObject);

  using Self = EllipseSpatialObject;
  using ScalarType = double;
  using Pointer = SmartPointer< Self >;
  using ConstPointer = SmartPointer< const Self >;
  using Superclass = SpatialObject< TDimension >;
  using SuperclassPointer = SmartPointer< Superclass >;
  using PointType = typename Superclass::PointType;
  using TransformType = typename Superclass::TransformType;
  using BoundingBoxType = typename Superclass::BoundingBoxType;
  using PointContainerType = VectorContainer< IdentifierType, PointType >;
  using PointContainerPointer = SmartPointer< PointContainerType >;

  using ArrayType = FixedArray< double, TDimension >;
  static constexpr unsigned int ObjectDimension = TDimension;

  itkNewMacro(Self);
  itkTypeMacro(EllipseSpatialObject, SpatialObject);

  /** Set all radii to the same radius value.  Each radius is
   *  half the length of one axis of the ellipse.  */
  void SetRadiusInObjectSpace(double radius)
  {
    for ( unsigned int i = 0; i < ObjectDimension; i++ )
      {
      m_RadiusInObjectSpace[i] = radius;
      }
    this->Modified();
  }

  /** Set radii via an array of radius values */
  itkSetMacro(RadiusInObjectSpace, ArrayType);

  /** Get radii via an array of radius values */
  itkGetConstReferenceMacro(RadiusInObjectSpace, ArrayType);

  /** Set center point in object space. */
  void SetCenterInObjectSpace(const PointType & center)
  {
    m_CenterInObjectSpace = center;
    this->Modified();
  }

  /** Get center in object space */
  itkGetConstReferenceMacro(CenterInObjectSpace, PointType);

  /** Test whether a point is inside or outside the object */
  bool IsInsideInWorldSpace(const PointType & point, unsigned int depth=0,
    const std::string & name="" ) const override;

  /** Get the boundaries of a specific object.  This function needs to
   *  be called every time one of the object's components is
   *  changed. */
  bool ComputeMyBoundingBoxInWorldSpace() const override;

  /** world-space property getters */
  itkGetConstReferenceMacro( Center, PointType );

  void Update() override;

protected:
  EllipseSpatialObject();
  ~EllipseSpatialObject() override = default;

  /* object space */
  ArrayType m_RadiusInObjectSpace;
  PointType m_CenterInObjectSpace;
  /* world space */
  PointType m_Center;

  /** Print the object informations in a stream. */
  void PrintSelf(std::ostream & os, Indent indent) const override;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkEllipseSpatialObject.hxx"
#endif

#endif // itkEllipseSpatialObject_h
