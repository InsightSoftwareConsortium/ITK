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
#ifndef itkArrowSpatialObject_h
#define itkArrowSpatialObject_h

#include "itkSpatialObject.h"

namespace itk
{
/**
 * \class ArrowSpatialObject
 * \brief Representation of a Arrow based on the spatial object classes.
 *
 * A ArrowSpatialObject represents a Arrow by serving as the parent of
 * the elements of the Arrow.  Since any itk::SpatialObject can have
 * children (see SpatialObject::GetChildren()), this class needs no
 * additional methods.
 * \ingroup ITKSpatialObjects
 */

template< unsigned int TDimension = 3 >
class ITK_TEMPLATE_EXPORT ArrowSpatialObject:
  public SpatialObject< TDimension >
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(ArrowSpatialObject);

  using Self = ArrowSpatialObject;
  using Superclass = SpatialObject< TDimension >;
  using Pointer = SmartPointer< Self >;
  using ConstPointer = SmartPointer< const Self >;
  using ScalarType = double;
  using VectorType = Vector< double, TDimension >;
  using PointType = Point< double, TDimension >;
  using TransformType = typename Superclass::TransformType;
  using MatrixType = typename TransformType::MatrixType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Method for creation through the object factory. */
  itkTypeMacro(ArrowSpatialObject, SpatialObject);

  /** Set the position of the arrow */
  void SetPosition(const PointType & p)
  {
    m_Position = p;
  }

  itkGetConstMacro(Position, PointType);

  void SetPosition(float x, float y)
  {
    m_Position[0] = x;
    m_Position[1] = y;
  }

  void SetPosition(float x, float y, float z)
  {
    m_Position[0] = x;
    m_Position[1] = y;
    m_Position[2] = z;
  }

  /** Set the direction of the arrow */
  void SetDirection(const VectorType & d)
  {
    m_Direction = d;
  }

  itkGetConstMacro(Direction, VectorType);

  void SetDirection(float x, float y)
  {
    m_Direction[0] = x;
    m_Direction[1] = y;
  }

  void SetDirection(float x, float y, float z)
  {
    m_Direction[0] = x;
    m_Direction[1] = y;
    m_Direction[2] = z;
  }

  /** Set the length of the arrow */
  void SetLength(double length);

  /** Get the length of the arrow */
  itkGetConstReferenceMacro(Length, double);

  /** Compute the local bounding box */
  bool ComputeLocalBoundingBox() const override;

  /** Returns true if the point is inside the line, false otherwise. */
  bool IsInside(const PointType & point,
                unsigned int depth, char *name) const override;

  /** Test whether a point is inside or outside the object
   *  For computational speed purposes, it is faster if the method does not
   *  check the name of the class and the current depth */
  virtual bool IsInside(const PointType & point) const;

protected:

  ArrowSpatialObject();
  ~ArrowSpatialObject() override = default;

  /** Method to print the object.*/
  void PrintSelf(std::ostream & os, Indent indent) const override;

private:
  VectorType m_Direction;
  PointType  m_Position;
  double     m_Length;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkArrowSpatialObject.hxx"
#endif

#endif // itkArrowSpatialObject_h
