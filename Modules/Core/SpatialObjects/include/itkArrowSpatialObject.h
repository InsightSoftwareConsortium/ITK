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
  void SetObjectPosition(const PointType & p)
  {
    m_ObjectPosition = p;
    this->Modified();
  }

  itkGetConstMacro(ObjectPosition, PointType);

  void SetObjectPosition(float x, float y)
  {
    m_ObjectPosition[0] = x;
    m_ObjectPosition[1] = y;
    this->Modified();
  }

  void SetObjectPosition(float x, float y, float z)
  {
    m_ObjectPosition[0] = x;
    m_ObjectPosition[1] = y;
    m_ObjectPosition[2] = z;
    this->Modified();
  }

  /** Set the direction of the arrow */
  void SetObjectDirection(const VectorType & d)
  {
    m_ObjectDirection = d;
    this->Modified();
  }

  itkGetConstMacro(ObjectDirection, VectorType);

  void SetObjectDirection(float x, float y)
  {
    m_ObjectDirection[0] = x;
    m_ObjectDirection[1] = y;
    this->Modified();
  }

  void SetObjectDirection(float x, float y, float z)
  {
    m_ObjectDirection[0] = x;
    m_ObjectDirection[1] = y;
    m_ObjectDirection[2] = z;
    this->Modified();
  }

  /** Set the length of the arrow */
  void SetObjectLength(double length);

  /** Get the length of the arrow */
  itkGetConstReferenceMacro(ObjectLength, double);

  /** Compute the Object bounding box */
  bool ComputeObjectWorldBoundingBox() const override;

  /** Returns true if the point is inside the line, false otherwise. */
  bool IsInside(const PointType & point, unsigned int depth=0,
    const std::string & name="") const override;

  void Update() override;

  itkGetConstReferenceMacro( WorldPosition, PointType );
  itkGetConstReferenceMacro( WorldDirection, VectorType );
  itkGetConstReferenceMacro( WorldLength, double );

protected:

  ArrowSpatialObject();
  ~ArrowSpatialObject() override = default;

  /** Method to print the object.*/
  void PrintSelf(std::ostream & os, Indent indent) const override;

private:
  VectorType m_ObjectDirection;
  PointType  m_ObjectPosition;
  double     m_ObjectLength;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkArrowSpatialObject.hxx"
#endif

#endif // itkArrowSpatialObject_h
