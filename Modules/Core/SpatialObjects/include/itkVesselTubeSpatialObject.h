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
#ifndef itkVesselTubeSpatialObject_h
#define itkVesselTubeSpatialObject_h

#include <list>

#include "itkTubeSpatialObject.h"
#include "itkVesselTubeSpatialObjectPoint.h"

namespace itk
{
/**
 * \class VesselTubeSpatialObject
 * \brief Representation of a tube based on the spatial object classes.
 *
 * The tube is basically defined by a set of points. Each tube can
 * be connected to a tube network, by using the AddSpatialObject() methods
 * of a VesselTubeSpatialObject Object.
 * A tube is also identified by an id number when connected to a network.
 *
 * \sa VesselTubeSpatialObjectPoint
 * \ingroup ITKSpatialObjects
 */

template< unsigned int TDimension = 3 >
class ITK_TEMPLATE_EXPORT VesselTubeSpatialObject:
  public TubeSpatialObject< TDimension,
                            VesselTubeSpatialObjectPoint< TDimension >  >
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(VesselTubeSpatialObject);

  using Self = VesselTubeSpatialObject;
  using Superclass = TubeSpatialObject< TDimension,
                             VesselTubeSpatialObjectPoint< TDimension > >;
  using Pointer = SmartPointer< Self >;
  using ConstPointer = SmartPointer< const Self >;
  using TubePointType = VesselTubeSpatialObjectPoint< TDimension >;
  using PointListType = typename Superclass::PointListType;
  using PointType = typename Superclass::PointType;
  using TransformType = typename Superclass::TransformType;
  using SpatialObjectPointType = typename Superclass::SpatialObjectPointType;
  using PointContainerType = VectorContainer< IdentifierType, PointType >;
  using PointContainerPointer = SmartPointer< PointContainerType >;
  using VectorType = typename Superclass::VectorType;
  using CovariantVectorType = typename Superclass::CovariantVectorType;
  using BoundingBoxType = typename Superclass::BoundingBoxType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Method for creation through the object factory. */
  itkTypeMacro(VesselTubeSpatialObject, TubeSpatialObject);

protected:

  VesselTubeSpatialObject();
  ~VesselTubeSpatialObject() override = default;

  /** Method to print the object.*/
  void PrintSelf(std::ostream & os, Indent indent) const override;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkVesselTubeSpatialObject.hxx"
#endif

#endif // itkVesselTubeSpatialObject_h
