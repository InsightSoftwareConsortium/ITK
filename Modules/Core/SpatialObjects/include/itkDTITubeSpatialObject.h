/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkDTITubeSpatialObject_h
#define itkDTITubeSpatialObject_h

#include <list>

#include "itkTubeSpatialObject.h"
#include "itkDTITubeSpatialObjectPoint.h"

namespace itk
{
/**
 * \class DTITubeSpatialObject
 * \brief Representation of a tube based on the spatial object classes.
 *
 * The tube is basically defined by a set of points. Each tube can
 * be connected to a tube network, by using the AddChild() methods
 * of a DTITubeSpatialObject Object.
 * A tube is also identified by an id number when connected to a network.
 *
 * \sa DTITubeSpatialObjectPoint
 * \ingroup ITKSpatialObjects
 */

template <unsigned int TDimension = 3>
class ITK_TEMPLATE_EXPORT DTITubeSpatialObject
  : public TubeSpatialObject<TDimension, DTITubeSpatialObjectPoint<TDimension>>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(DTITubeSpatialObject);

  using Self = DTITubeSpatialObject;
  using Superclass = TubeSpatialObject<TDimension, DTITubeSpatialObjectPoint<TDimension>>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  using DTITubePointType = DTITubeSpatialObjectPoint<TDimension>;
  using DTITubePointListType = std::vector<DTITubePointType>;

  using typename Superclass::PointType;
  using typename Superclass::TransformType;
  using typename Superclass::SpatialObjectPointType;
  using PointContainerType = VectorContainer<IdentifierType, PointType>;
  using PointContainerPointer = SmartPointer<PointContainerType>;
  using typename Superclass::VectorType;
  using typename Superclass::CovariantVectorType;
  using typename Superclass::BoundingBoxType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Method for creation through the object factory. */
  itkTypeMacro(DTITubeSpatialObject, TubeSpatialObject);

protected:
  DTITubeSpatialObject();
  ~DTITubeSpatialObject() override = default;

  typename LightObject::Pointer
  InternalClone() const override;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkDTITubeSpatialObject.hxx"
#endif

#endif // itkDTITubeSpatialObject_h
