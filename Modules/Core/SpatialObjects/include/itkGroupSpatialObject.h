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

template< unsigned int TDimension = 3 >
class ITK_TEMPLATE_EXPORT GroupSpatialObject:
  public SpatialObject< TDimension >
{
public:

  typedef GroupSpatialObject                      Self;
  typedef SpatialObject< TDimension >             Superclass;
  typedef SmartPointer< Self >                    Pointer;
  typedef SmartPointer< const Self >              ConstPointer;
  typedef double                                  ScalarType;
  typedef typename Superclass::TreeNodeType       TreeNodeType;
  typedef typename TreeNodeType::ChildrenListType TreeNodeChildrenListType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Method for creation through the object factory. */
  itkTypeMacro(GroupSpatialObject, SpatialObject);

  /**  */
  bool ComputeLocalBoundingBox() const ITK_OVERRIDE { return false; }

protected:
  ITK_DISALLOW_COPY_AND_ASSIGN(GroupSpatialObject);

  GroupSpatialObject();
  virtual ~GroupSpatialObject() ITK_OVERRIDE;

  /** Method to print the object.*/
  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkGroupSpatialObject.hxx"
#endif

#endif // itkGroupSpatialObject_h
