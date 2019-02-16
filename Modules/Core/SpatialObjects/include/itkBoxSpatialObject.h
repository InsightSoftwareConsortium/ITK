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
  using PointsContainerType = typename BoundingBoxType::PointsContainer;

  itkNewMacro(Self);
  itkTypeMacro(BoxSpatialObject, SpatialObject);

  /** Set the size of the box spatial object in object space. */
  void SetSizeInObjectSpace( const SizeType & size )
  {
    this->m_SizeInObjectSpace = size;
    this->Modified();
  }

  /** Get the size of the box spatial object in object space. */
  itkGetConstReferenceMacro(SizeInObjectSpace, SizeType);

  /** Set the position of the box spatial object in object space. */
  void SetPositionInObjectSpace( const PointType & pos )
  {
    this->m_PositionInObjectSpace = pos;
    this->Modified();
  }

  /** Get the position of the box spatial object in object space. */
  itkGetConstReferenceMacro(PositionInObjectSpace, PointType);

  /** Update position of corners in world space */
  void Update() override;

  const PointType & GetCorner( unsigned int cornerNumber ) const;
  itkGetConstObjectMacro(Corners, PointsContainerType);

  /** Test whether a point is inside or outside the object */
  bool IsInsideInWorldSpace(const PointType & point, unsigned int depth = 0,
    const std::string & name = "" ) const override;

  /** Get the boundaries of a specific object.  This function needs to
   *  be called every time one of the object's components is
   *  changed. */
  bool ComputeMyBoundingBoxInWorldSpace() const override;

  /** world-space getters */
  itkGetConstReferenceMacro( Position, PointType );

protected:
  BoxSpatialObject();
  ~BoxSpatialObject() override = default;

  /** object space */
  SizeType  m_SizeInObjectSpace;
  PointType m_PositionInObjectSpace;

  /** world space */
  PointType m_Position;
  typename PointsContainerType::Pointer m_Corners;

  /** Print the object informations in a stream. */
  void PrintSelf(std::ostream & os, Indent indent) const override;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkBoxSpatialObject.hxx"
#endif

#endif // itkBoxSpatialObject_h
