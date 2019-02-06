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
#ifndef itkSceneSpatialObject_h
#define itkSceneSpatialObject_h

#include "itkSpatialObject.h"

#include <list>

namespace itk
{
/** \class SceneSpatialObject
 * \brief a SceneSpatialObject has a list of SpatialObjects
 *
 * This class represent a SceneSpatialObject object into which one can
 * plug any kind of spatial object.
 *
 * \sa SpatialObject
 * \ingroup ITKSpatialObjects
 */

template< unsigned int TDimension = 3 >
class ITK_TEMPLATE_EXPORT SceneSpatialObject:
  public GroupSpatialObject< TDimension >
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(SceneSpatialObject);

  using Self = SceneSpatialObject;
  using Superclass = GroupSpatialObject< TDimension >;
  using SuperclassPointer = Superclass::Pointer;
  using Pointer = SmartPointer< Self >;
  using ConstPointer = SmartPointer< const Self >;

  using SpatialObjectType = SpatialObject< TDimension >;
  using SpatialObjectPointer = typename SpatialObjectType::Pointer;

  using ChildrenListType = typename Superclass::ChildrenListType;

  /** Method for creation through the object factory */
  itkNewMacro(Self);
  itkTypeMacro(SceneSpatialObject, Object);

protected:

  /** constructor */
  SceneSpatialObject();

  /** destructor */
  ~SceneSpatialObject() override;

  /** Print the object informations in a stream. */
  void PrintSelf(std::ostream & os, Indent indent) const override;

};

} // end of namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkSceneSpatialObject.hxx"
#endif

#endif // __SceneSpatialObject_h
