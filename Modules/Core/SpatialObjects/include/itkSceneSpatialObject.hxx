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
#ifndef itkSceneSpatialObject_hxx
#define itkSceneSpatialObject_hxx

#include "itkSceneSpatialObject.h"
#include <algorithm>

namespace itk
{
/** Constructor */
template< unsigned int TSpaceDimension >
SceneSpatialObject< TSpaceDimension >
::SceneSpatialObject() : m_ParentId(0)
{}

/** Destructor */
template< unsigned int TSpaceDimension >
SceneSpatialObject< TSpaceDimension >
::~SceneSpatialObject()
{}

/** Add a spatial object to the SceneSpatialObject */
template< unsigned int TSpaceDimension >
void
SceneSpatialObject< TSpaceDimension >
::AddSpatialObject(SpatialObject< TSpaceDimension > *pointer)
{
  m_Objects.push_back(pointer);
  this->Modified();
}

/** Remove a spatial object from the SceneSpatialObject */
template< unsigned int TSpaceDimension >
void
SceneSpatialObject< TSpaceDimension >
::RemoveSpatialObject(SpatialObject< TSpaceDimension > *pointer)
{
  typename ObjectListType::iterator it;
  it = std::find(m_Objects.begin(), m_Objects.end(), pointer);

  if ( it != m_Objects.end() )
    {
    if ( *it == pointer )
      {
      m_Objects.erase(it);
      this->Modified();
      }
    }
  else
    {
    //throw an exception object to let user know that
    // he tried to remove an object
    // which is not in the list of the children.
    }
}

/** Return the modification time of the SceneSpatialObject */
template< unsigned int TSpaceDimension >
ModifiedTimeType
SceneSpatialObject< TSpaceDimension >
::GetMTime(void) const
{
  typename ObjectListType::const_iterator it = m_Objects.begin();
  typename ObjectListType::const_iterator itEnd = m_Objects.end();

  ModifiedTimeType latestTime = Superclass::GetMTime();
  ModifiedTimeType localTime;
  while ( it != itEnd )
    {
    localTime = ( *it )->GetMTime();
    if ( localTime > latestTime )
      {
      latestTime = localTime;
      }
    it++;
    }
  return latestTime;
}

/** Returns a new list of objects in the scene */
template< unsigned int TSpaceDimension >
typename SceneSpatialObject< TSpaceDimension >::ObjectListType *
SceneSpatialObject< TSpaceDimension >
::GetObjects(unsigned int depth, char *name)
{
  ObjectListType *newList = new ObjectListType;

  typename ObjectListType::const_iterator it = m_Objects.begin();
  typename ObjectListType::const_iterator itEnd = m_Objects.end();

  while ( it != itEnd )
    {
    if ( name == ITK_NULLPTR || strstr(typeid( **it ).name(), name) )
      {
      newList->push_back(*it);
      }
    if ( depth > 0 )
      {
      typedef typename SpatialObject< TSpaceDimension >::ChildrenListType
      ChildListType;
      ChildListType *childList =
        dynamic_cast< SpatialObject< TSpaceDimension > * >( ( *it ).GetPointer() )->
        GetChildren(depth - 1, name);
      typename ChildListType::const_iterator cIt = childList->begin();
      typename ChildListType::const_iterator cItEnd = childList->end();

      while ( cIt != cItEnd )
        {
        newList->push_back( dynamic_cast< ObjectType * >( ( *cIt ).GetPointer() ) );
        cIt++;
        }

      delete childList;
      }
    it++;
    }

  return newList;
}

/** Set the children list */
template< unsigned int TSpaceDimension >
void
SceneSpatialObject< TSpaceDimension >
::SetObjects(ObjectListType & children)
{
  m_Objects = children;
}

/** Return the number of objects in the SceneSpatialObject */
template< unsigned int TSpaceDimension >
unsigned int
SceneSpatialObject< TSpaceDimension >
::GetNumberOfObjects(unsigned int depth, char *name)
{
  typename ObjectListType::const_iterator it = m_Objects.begin();
  typename ObjectListType::const_iterator itEnd = m_Objects.end();

  unsigned int cnt = 0;
  while ( it != itEnd )
    {
    if ( name == ITK_NULLPTR || strstr(typeid( **it ).name(), name) )
      {
      cnt++;
      }
    it++;
    }

  it = m_Objects.begin();
  itEnd = m_Objects.end();
  if ( depth > 0 )
    {
    while ( it != itEnd )
      {
      cnt +=
        ( dynamic_cast< SpatialObject< TSpaceDimension > * >( ( *it ).GetPointer() ) )->
        GetNumberOfChildren(depth - 1, name);
      it++;
      }
    }

  return cnt;
}

/** Print the object */
template< unsigned int TSpaceDimension >
void
SceneSpatialObject< TSpaceDimension >
::PrintSelf(std::ostream & os, Indent indent) const
{
  os << indent << "Number of objects: "
     << m_Objects.size() << std::endl;
  os << indent << "List of objects: ";

  typename ObjectListType::const_iterator it = m_Objects.begin();
  typename ObjectListType::const_iterator itEnd = m_Objects.end();

  while ( it != itEnd )
    {
    os << "[" << ( *it ) << "] ";
    it++;
    }
  os << std::endl;

  Superclass::PrintSelf(os, indent);
}

/** Return a SpatialObject in the SceneSpatialObject
 *  given a parent ID */
template< unsigned int TSpaceDimension >
SpatialObject< TSpaceDimension > *
SceneSpatialObject< TSpaceDimension >
::GetObjectById(int Id)
{
  typename ObjectListType::iterator it = m_Objects.begin();
  typename ObjectListType::iterator itEnd = m_Objects.end();

  typedef typename SpatialObjectType::ChildrenListType ChildListType;
  ChildListType *cList;
  typename ChildListType::iterator cIt;
  typename ChildListType::iterator cItEnd;

  while ( it != itEnd )
    {
    if ( ( *it )->GetId() == Id )
      {
      return *it;
      }
    else
      {
      //cList = (dynamic_cast<SpatialObject<TSpaceDimension> *>(*it))->
      //  GetChildren(SpatialObjectType::MaximumDepth);
      cList = ( *it )->GetChildren(SpatialObjectType::MaximumDepth);
      cIt = cList->begin();
      cItEnd = cList->end();
      while ( cIt != cItEnd )
        {
        if ( ( *cIt )->GetId() == Id )
          {
          SpatialObject< TSpaceDimension > *tmp;
          tmp = *cIt;
          delete cList;
          return tmp;
          }
        cIt++;
        }

      delete cList;
      }

    it++;
    }

  return ITK_NULLPTR;
}

template< unsigned int TSpaceDimension >
bool
SceneSpatialObject< TSpaceDimension >
::FixHierarchy(void)
{
  typename ObjectListType::iterator it = m_Objects.begin();
  typename ObjectListType::iterator oldIt;
  typename ObjectListType::iterator itEnd = m_Objects.end();

  bool ret = true;
  while ( it != itEnd )
    {
    const int parentId = ( *it )->GetParentId();
    if ( parentId >= 0 )
      {
      SpatialObject< TSpaceDimension > *parentObject =
        static_cast< SpatialObject< TSpaceDimension > * >
        ( this->GetObjectById(parentId) );
      if ( parentObject == ITK_NULLPTR )
        {
        ret = false;
        ++it;
        }
      else
        {
        parentObject->AddSpatialObject( dynamic_cast< SpatialObject< TSpaceDimension > * >
                                ( ( *it ).GetPointer() ) );
        oldIt = it;
        ++it;
        m_Objects.erase(oldIt);
        }
      }
    else
      {
      ++it;
      }
    }

  return ret;
}

/** Check if the parent objects have a defined ID */
template< unsigned int TSpaceDimension >
bool
SceneSpatialObject< TSpaceDimension >
::CheckIdValidity(void)
{
  typename ObjectListType::iterator it = m_Objects.begin();
  typename ObjectListType::iterator itEnd = m_Objects.end();

  bool ret = true;
  while ( it != itEnd )
    {
    // For every object in the scene we check the ID validity
    typename ObjectType::ChildrenListType * children = ( *it )->GetChildren();
    typename ObjectType::ChildrenListType::const_iterator
    itChild = children->begin();

    while ( itChild != children->end() )
      {
      if ( ( *itChild )->HasParent() )
        {
        if ( ( *itChild )->GetParent()->GetId() < 0 )
          {
          delete children;
          return false;
          }
        }
      itChild++;
      }
    delete children;
    it++;
    }
  return ret;
}

template< unsigned int TSpaceDimension >
void
SceneSpatialObject< TSpaceDimension >
::FixIdValidity(void)
{
  typename ObjectListType::iterator it = m_Objects.begin();
  typename ObjectListType::iterator itEnd = m_Objects.end();

  while ( it != itEnd )
    {
    // For every object in the scene we check the ID validity
    typename ObjectType::ChildrenListType * children = ( *it )->GetChildren();
    typename ObjectType::ChildrenListType::iterator itChild = children->begin();

    while ( itChild != children->end() )
      {
      if ( ( *itChild )->HasParent() )
        {
        if ( ( *itChild )->GetParent()->GetId() < 0 )
          {
          ( *itChild )->GetParent()->SetId( this->GetNextAvailableId() );
          }
        }
      itChild++;
      }
    delete children;
    it++;
    }
}

/** Return the next available Id. For speed reason the MaxID+1 is returned */
template< unsigned int TSpaceDimension >
int
SceneSpatialObject< TSpaceDimension >
::GetNextAvailableId()
{
  int Id = 0;

  typename ObjectListType::iterator it = m_Objects.begin();
  typename ObjectListType::iterator itEnd = m_Objects.end();

  while ( it != itEnd )
    {
    typename ObjectType::ChildrenListType * children = ( *it )->GetChildren();
    typename ObjectType::ChildrenListType::iterator
    itChild = children->begin();

    while ( itChild != children->end() )
      {
      if ( ( *itChild )->GetId() >= Id )
        {
        Id = ( *itChild )->GetId() + 1;
        }
      itChild++;
      }
    delete children;
    it++;
    }
  return Id;
}

/** Clear function : Remove all the objects in the scene */
template< unsigned int TSpaceDimension >
void
SceneSpatialObject< TSpaceDimension >
::Clear()
{
  m_Objects.clear();
  this->Modified();
}
} // end of namespace itk

#endif
