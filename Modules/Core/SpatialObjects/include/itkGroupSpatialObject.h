/*=========================================================================
 *
 *  Copyright NumFOCUS
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
#ifndef itkGroupSpatialObject_h
#define itkGroupSpatialObject_h

#include <list>

#include "itkSpatialObject.h"

namespace itk
{
/**
 * \class GroupSpatialObject
 * \brief Representation of a group based on the spatial object classes.
 *
 * A GroupSpatialObject represents a group by serving as the parent of
 * the elements of the group.  Since any itk::SpatialObject can have
 * children (see SpatialObject::GetChildren()), this class needs no
 * additional methods.
 * \ingroup ITKSpatialObjects
 */

template <unsigned int TDimension = 3>
class ITK_TEMPLATE_EXPORT GroupSpatialObject : public SpatialObject<TDimension>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(GroupSpatialObject);

  using Self = GroupSpatialObject;
  using Superclass = SpatialObject<TDimension>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;
  using ScalarType = double;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Method for creation through the object factory. */
  itkTypeMacro(GroupSpatialObject, SpatialObject);

protected:
  GroupSpatialObject();
  ~GroupSpatialObject() override = default;

  /** Method to print the object.*/
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  typename LightObject::Pointer
  InternalClone() const override;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkGroupSpatialObject.hxx"
#endif

#endif // itkGroupSpatialObject_h
