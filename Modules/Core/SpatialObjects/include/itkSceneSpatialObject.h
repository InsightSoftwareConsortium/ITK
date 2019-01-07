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

template< unsigned int TSpaceDimension = 3 >
class ITK_TEMPLATE_EXPORT SceneSpatialObject:
  public Object
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(SceneSpatialObject);

  using Self = SceneSpatialObject< TSpaceDimension >;
  using Superclass = Object;
  using SuperclassPointer = Superclass::Pointer;
  using Pointer = SmartPointer< Self >;
  using ConstPointer = SmartPointer< const Self >;

  using ObjectType = SpatialObject< TSpaceDimension >;
  using SpatialObjectType = SpatialObject< TSpaceDimension >;
  using SpatialObjectPointer = typename SpatialObjectType::Pointer;

  using ObjectListType = std::list< SpatialObjectPointer >;

  static constexpr unsigned int MaximumDepth = 9999999;

  /** Method for creation through the object factory */
  itkNewMacro(Self);
  itkTypeMacro(SceneSpatialObject, Object);

  /** Add an object to the list of children. */
  void AddSpatialObject(SpatialObject< TSpaceDimension > *pointer);

  /** Remove the object passed as arguments from the list of
   *  children. May this function
   *  should return a false value if the object to remove is
   *  not found in the list. */
  void RemoveSpatialObject(SpatialObject< TSpaceDimension > *object);

  /** Returns a list of pointer to the objects in the scene.
   *  \warning This function creates a new list therefore
   *  the user is responsible for the deletion of the list. */
  ObjectListType * GetObjects(unsigned int depth = MaximumDepth,
                              char *name = nullptr);

  /** Returns the number of children currently assigned to
   *  the SceneSpatialObject object. */
  unsigned int GetNumberOfObjects(unsigned int depth = MaximumDepth,
                                  char *name = nullptr);

  /** Set the list of pointers to children to the list passed as
* argument. */
  void SetObjects(ObjectListType & children);

  /** Returns the latest modified time of all the objects contained
   *  in this SceneSpatialObject object. */
  ModifiedTimeType GetMTime() const override;

  /** Get/Set the ParentID */
  void SetParentId(int parentid) { m_ParentId = parentid; }
  int  GetParentId() { return m_ParentId; }

  /** Return a SpatialObject in the SceneSpatialObject given its ID */
  SpatialObject< TSpaceDimension > * GetObjectById(int Id);

  /** In practice, this is used to transform an imported MetaIO scene hierarchy
   * specified only by Ids into the SceneSpatialObject hierarchy specified by
   * Ids and Child/Parent lists. */
  bool FixHierarchy();

  bool CheckIdValidity();

  void FixIdValidity();

  int GetNextAvailableId();

  /** Clear function : Remove all the objects in the scene */
  void Clear();

protected:
  /** List of the children object plug to the SceneSpatialObject
   *  spatial object. */
  ObjectListType m_Objects;

  /** constructor */
  SceneSpatialObject();

  /** destructor */
  ~SceneSpatialObject() override;

  /** Print the object informations in a stream. */
  void PrintSelf(std::ostream & os, Indent indent) const override;

  /** Parent ID : default = -1 */
  int m_ParentId{0};
};
} // end of namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkSceneSpatialObject.hxx"
#endif

#endif // __SceneSpatialObject_h
