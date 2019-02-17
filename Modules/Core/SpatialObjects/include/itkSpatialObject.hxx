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
#ifndef itkSpatialObject_hxx
#define itkSpatialObject_hxx

#include "itkSpatialObject.h"
#include "itkNumericTraits.h"
#include <algorithm>
#include <string>
#include "itkMath.h"
#include "itkImageBase.h"

namespace itk
{
/** Constructor */
template< unsigned int TDimension >
SpatialObject< TDimension >
::SpatialObject()
{
  m_TypeName = "SpatialObject";

  m_FamilyBoundingBoxInWorldSpace = BoundingBoxType::New();
  typename BoundingBoxType::PointType pnt;
  pnt.Fill( NumericTraits< typename BoundingBoxType::PointType::ValueType >::
    ZeroValue() );
  m_FamilyBoundingBoxInWorldSpace->SetMinimum(pnt);
  m_FamilyBoundingBoxInWorldSpace->SetMaximum(pnt);
  m_FamilyBoundingBoxInWorldSpaceMTime = 0;

  m_MyBoundingBoxInWorldSpace = BoundingBoxType::New();
  m_MyBoundingBoxInWorldSpace->SetMinimum(pnt);
  m_MyBoundingBoxInWorldSpace->SetMaximum(pnt);

  m_ObjectToWorldTransform = TransformType::New();
  m_ObjectToWorldTransform->SetIdentity();
  m_ObjectToParentTransform = TransformType::New();
  m_ObjectToParentTransform->SetIdentity();

  m_Id = -1;
  m_ParentId = -1;
  m_DefaultInsideValue = 1.0;
  m_DefaultOutsideValue  = 0.0;
}

/** Destructor */
template< unsigned int TDimension >
SpatialObject< TDimension >
::~SpatialObject()
{
  this->RemoveAllChildren(0);
}

/** Return the Derivative at a point given the order of the derivative */
template< unsigned int TDimension >
void
SpatialObject< TDimension >
::DerivativeAtInWorldSpace(const PointType & point, short unsigned int order,
               DerivativeVectorType & value, unsigned int depth,
               const std::string & name,
               const DerivativeOffsetType & offset)
{
  if ( !IsEvaluableAtInWorldSpace(point, depth, name) )
    {
    ExceptionObject e(__FILE__);
    e.SetLocation( "SpatialObject::DerivateAt()" );
    e.SetDescription("This spatial object is not evaluable at the point");
    throw e;
    }

  if ( order == 0 )
    {
    double r;

    ValueAtInWorldSpace(point, r, depth, name);
    value.Fill(r);
    }
  else
    {
    PointType                  p1, p2;
    DerivativeVectorType       v1, v2;
    typename DerivativeVectorType::Iterator it = value.Begin();
    typename DerivativeVectorType::Iterator it_v1 = v1.Begin();
    typename DerivativeVectorType::Iterator it_v2 = v2.Begin();

    DerivativeOffsetType offsetDiv2;
    for ( unsigned short i = 0; i < TDimension; i++ )
      {
      offsetDiv2[i] = offset[i] / 2.0;
      }
    for ( unsigned short i = 0; i < TDimension; i++, it++, it_v1++, it_v2++ )
      {
      p1 = point;
      p2 = point;

      p1[i] -= offset[i];
      p2[i] += offset[i];

      // note DerivativeAtInWorldSpace might throw.
      DerivativeAtInWorldSpace(p1, order - 1, v1, depth, name, offsetDiv2);
      DerivativeAtInWorldSpace(p2, order - 1, v2, depth, name, offsetDiv2);

      ( *it ) = ( ( *it_v2 ) - ( *it_v1 ) ) / 2;
      }
    }
}

/** Return if a point is inside the object or its children */
template< unsigned int TDimension >
bool
SpatialObject< TDimension >
::IsInsideInWorldSpace(const PointType &  point, unsigned int depth,
  const std::string & name) const
{
  if( depth > 0 )
    {
    return IsInsideChildrenInWorldSpace( point, depth-1, name );
    }
  else
    {
    return false;
    }
}

/** Return if a point is inside the object or its children */
template< unsigned int TDimension >
bool
SpatialObject< TDimension >
::IsInsideChildrenInWorldSpace(const PointType &  point, unsigned int depth,
  const std::string & name) const
{
  typename ChildrenListType::const_iterator it = m_ChildrenList.begin();

  while ( it != m_ChildrenList.end() )
    {
    if ( (*it)->IsInsideInWorldSpace(point, depth, name) )
      {
      return true;
      }
    it++;
    }

  return false;
}

/** Return if the object is evaluable at a point */
template< unsigned int TDimension >
bool
SpatialObject< TDimension >
::IsEvaluableAtInWorldSpace(const PointType & point, unsigned int depth,
  const std::string & name) const
{
  if( IsInsideInWorldSpace( point, 0, name ) )
    {
    return true;
    }
  else
    {
    if( depth > 0 )
      {
      return IsEvaluableAtChildrenInWorldSpace( point, depth-1, name );
      }
    else
      {
      return false;
      }
    }
}

/** Return if the object is evaluable at a point */
template< unsigned int TDimension >
bool
SpatialObject< TDimension >
::IsEvaluableAtChildrenInWorldSpace(const PointType & point, unsigned int depth,
  const std::string & name) const
{
  typename ChildrenListType::const_iterator it = m_ChildrenList.begin();

  while ( it != m_ChildrenList.end() )
    {
    if ( ( *it )->IsEvaluableAtInWorldSpace(point, depth, name) )
      {
      return true;
      }
    it++;
    }

  return false;
}

/** Return the value of the object at a point */
template< unsigned int TDimension >
bool
SpatialObject< TDimension >
::ValueAtInWorldSpace(const PointType & point, double & value, unsigned int depth,
          const std::string & name) const
{
  if( IsEvaluableAtInWorldSpace( point, 0, name ) )
    {
    if( IsInsideInWorldSpace( point, 0, name ) )
      {
      value = m_DefaultInsideValue;
      return true;
      }
    else
      {
      value = m_DefaultOutsideValue;
      return true;
      }
    }
  else
    {
    if( depth > 0 )
      {
      if( ValueAtChildrenInWorldSpace( point, value, depth-1, name ) )
        {
        return true;
        }
      }
    }

  value = m_DefaultOutsideValue;
  return false;
}

/** Return the value of the object at a point */
template< unsigned int TDimension >
bool
SpatialObject< TDimension >
::ValueAtChildrenInWorldSpace(const PointType & point, double & value, unsigned int depth,
          const std::string & name) const
{
  typename ChildrenListType::const_iterator it = m_ChildrenList.begin();

  while ( it != m_ChildrenList.end() )
    {
    if ( ( *it )->IsEvaluableAtInWorldSpace(point, depth, name) )
      {
      ( *it )->ValueAtInWorldSpace(point, value, depth, name);
      return true;
      }
    it++;
    }

  value = m_DefaultOutsideValue;
  return false;
}

/** Print self */
template< unsigned int TDimension >
void
SpatialObject< TDimension >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << "Object Bounding Box:" << std::endl;
  os << indent << m_MyBoundingBoxInWorldSpace << std::endl;
  os << "Bounding Box:" << std::endl;
  os << indent << m_FamilyBoundingBoxInWorldSpace << std::endl;
  os << "Geometric properties:" << std::endl;
  os << indent << "Object to World Transform: " << m_ObjectToWorldTransform
     << std::endl;
  os << indent << "Object to Parent Transform: " << m_ObjectToParentTransform
     << std::endl;
  os << std::endl << std::endl;
  os << "Object properties: " << std::endl;
  m_Property.Print( std::cout );
}

/** Get the bounds of the object */
template< unsigned int TDimension >
typename SpatialObject< TDimension >::BoundingBoxType *
SpatialObject< TDimension >
::GetFamilyBoundingBoxInWorldSpace() const
{
  return m_FamilyBoundingBoxInWorldSpace.GetPointer();
}

/** Add a child to the object */
template< unsigned int TDimension >
void
SpatialObject< TDimension >
::AddChild(Self *pointer)
{
  typename ChildrenListType::iterator pos;
  pos = std::find(m_ChildrenList.begin(), m_ChildrenList.end(), pointer);
  if( pos == m_ChildrenList.end() )
    {
    m_ChildrenList.push_back(pointer);

    pointer->SetParent( this );

    if( pointer->GetId() == -1 )
      {
      pointer->SetId( this->GetNextAvailableId() );
      }

    this->Modified();
    }
}

/** Remove a child to the object */
template< unsigned int TDimension >
bool
SpatialObject< TDimension >
::RemoveChild(Self *pointer)
{
  typename ChildrenListType::iterator pos;
  pos = std::find(m_ChildrenList.begin(), m_ChildrenList.end(), pointer);
  if ( pos != m_ChildrenList.end() )
    {
    pointer->SetParent( nullptr );

    m_ChildrenList.erase(pos);

    this->Modified();

    return true;
    }
  else
    {
    return false;
    }
}

/** Remove a child to the object */
template< unsigned int TDimension >
void
SpatialObject< TDimension >
::RemoveAllChildren( unsigned int depth )
{
  typename ChildrenListType::iterator it = m_ChildrenList.begin();
  while( it != m_ChildrenList.end() )
    {
    Self * oldChild = (*it);
    it = m_ChildrenList.erase( it );
    oldChild->SetParent( nullptr );
    if( depth > 0 )
      {
      oldChild->RemoveAllChildren( depth-1 );
      }
    }

  this->Modified();
}

/** Set the local to global transformation */
template< unsigned int TDimension >
void
SpatialObject< TDimension >
::SetObjectToParentTransform( const TransformType *transform)
{
  typename TransformType::Pointer inverse = TransformType::New();
  if ( !transform->GetInverse( inverse ) )
    {
    ExceptionObject e(__FILE__);
    e.SetLocation( "SpatialObject::SetObjectToParentTransform()" );
    e.SetDescription( "Transform must be invertible." );
    throw e;
    }

  m_ObjectToParentTransform->SetFixedParameters(
    transform->GetFixedParameters() );
  m_ObjectToParentTransform->SetParameters( transform->GetParameters() );

  ComputeObjectToWorldTransform();

  auto it = m_ChildrenList.begin();
  while( it != m_ChildrenList.end() )
    {
    (*it)->ComputeObjectToWorldTransform();
    ++it;
    }
}

/** Compute the Global Transform */
template< unsigned int TDimension >
void
SpatialObject< TDimension >
::ComputeObjectToWorldTransform()
{
  m_ObjectToWorldTransform->SetFixedParameters(
    this->GetObjectToParentTransform()->GetFixedParameters() );
  m_ObjectToWorldTransform->SetParameters(
    this->GetObjectToParentTransform()->GetFixedParameters() );
  if( this->HasParent() )
    {
    m_ObjectToWorldTransform->Compose( this->GetParent()->
      GetObjectToWorldTransform(), false );
    }

  // Propagate the changes to the children
  typename ChildrenListType::iterator it = m_ChildrenList.begin();
  while ( it != m_ChildrenList.end() )
    {
    ( *it )->ComputeObjectToWorldTransform();
    it++;
    }

  this->ComputeMyBoundingBoxInWorldSpace();

  this->Modified();
}

/** Get the local transformation */
template< unsigned int TDimension >
typename SpatialObject< TDimension >::TransformType *
SpatialObject< TDimension >
::GetObjectToParentTransform()
{
  return m_ObjectToParentTransform.GetPointer();
}

/** Get the local transformation (const) */
template< unsigned int TDimension >
const typename SpatialObject< TDimension >::TransformType *
SpatialObject< TDimension >
::GetObjectToParentTransform() const
{
  return m_ObjectToParentTransform.GetPointer();
}


/** Set the global to local transformation */
template< unsigned int TDimension >
void
SpatialObject< TDimension >
::SetObjectToWorldTransform( const TransformType *transform )
{
  typename TransformType::Pointer inverse = TransformType::New();
  if ( !transform->GetInverse( inverse ) )
    {
    ExceptionObject e(__FILE__);
    e.SetLocation( "SpatialObject::SetObjectToWorldTransform()" );
    e.SetDescription( "Transform must be invertible." );
    throw e;
    }

  m_ObjectToWorldTransform->SetFixedParameters(
    transform->GetFixedParameters() );
  m_ObjectToWorldTransform->SetParameters( transform->GetParameters() );

  ComputeObjectToParentTransform();
}

/** Compute the Transform when the global transform as been set
 *  This does not change the IndexToObjectMatrix */
template< unsigned int TDimension >
void
SpatialObject< TDimension >
::ComputeObjectToParentTransform()
{
  m_ObjectToParentTransform->SetFixedParameters( m_ObjectToWorldTransform->GetFixedParameters() );
  m_ObjectToParentTransform->SetParameters( m_ObjectToWorldTransform->GetParameters() );

  if( this->HasParent() )
    {
    typename TransformType::Pointer inverse = TransformType::New();
    if( this->GetParent()->GetObjectToWorldTransform()->GetInverse(inverse) )
      {
      m_ObjectToParentTransform->Compose(inverse, true);
      }
    }
  // Propagate the changes to the children
  typename ChildrenListType::iterator it = m_ChildrenList.begin();
  while ( it != m_ChildrenList.end() )
    {
    ( *it )->ComputeObjectToWorldTransform();
    it++;
    }

  this->ComputeMyBoundingBoxInWorldSpace();

  this->Modified();
}

/** Get the modification time  */
template< unsigned int TDimension >
ModifiedTimeType
SpatialObject< TDimension >
::GetMTime() const
{
  ModifiedTimeType latestTime = Object::GetMTime();

  if( latestTime < m_FamilyBoundingBoxInWorldSpaceMTime )
    {
    latestTime = m_FamilyBoundingBoxInWorldSpaceMTime;
    }

  typename ChildrenListType::const_iterator it = m_ChildrenList.begin();
  ModifiedTimeType localTime;

  while( it != m_ChildrenList.end() )
    {
    localTime = (*it)->GetMTime();

    if( localTime > latestTime )
      {
      latestTime = localTime;
      }
    it++;
    }

  return latestTime;
}

template< unsigned int TDimension >
bool
SpatialObject< TDimension >
::ComputeMyBoundingBoxInWorldSpace() const
{
  typename BoundingBoxType::PointType pnt;
  pnt.Fill( NumericTraits< typename BoundingBoxType::PointType::ValueType >::
    ZeroValue() );
  m_MyBoundingBoxInWorldSpace->SetMinimum(pnt);
  m_MyBoundingBoxInWorldSpace->SetMaximum(pnt);

  return false;
}


/** Get the bounds of the object */
template< unsigned int TDimension >
typename SpatialObject< TDimension >::BoundingBoxType *
SpatialObject< TDimension >
::GetMyBoundingBoxInWorldSpace() const
{
  return m_MyBoundingBoxInWorldSpace.GetPointer();
}

/**
 * Compute an axis-aligned bounding box for an object and its selected
 * children, down to a specified depth.  After computation, the
 * resulting bounding box is stored in this->m_FamilyBoundingBoxInWorldSpace.  */
template< unsigned int TDimension >
bool
SpatialObject< TDimension >
::ComputeFamilyBoundingBoxInWorldSpace( unsigned int depth, const std::string & name ) const
{
  itkDebugMacro("Computing Bounding Box");

  typename BoundingBoxType::PointType pnt;
  pnt.Fill( NumericTraits< typename BoundingBoxType::PointType::ValueType >::
    ZeroValue() );
  m_FamilyBoundingBoxInWorldSpace->SetMinimum(pnt);
  m_FamilyBoundingBoxInWorldSpace->SetMaximum(pnt);
  m_FamilyBoundingBoxInWorldSpaceMTime = this->GetMTime();
  bool bbDefined = false;

  if( this->GetTypeName().find( name ) != std::string::npos )
    {
    PointType pointMin = this->GetMyBoundingBoxInWorldSpace()->GetMinimum();
    PointType pointMax = this->GetMyBoundingBoxInWorldSpace()->GetMaximum();
    for ( unsigned int i = 0; i < ObjectDimension; i++ )
      {
      if ( Math::NotExactlyEquals(pointMin[i], 0)
           || Math::NotExactlyEquals(pointMax[i], 0) )
        {
        bbDefined = true;
        m_FamilyBoundingBoxInWorldSpace->SetMinimum( pointMin );
        m_FamilyBoundingBoxInWorldSpace->SetMaximum( pointMax );
        break;
        }
      }
    }

  if( depth > 0 )
    {
    typename ChildrenListType::const_iterator it = m_ChildrenList.begin();
    while( it != m_ChildrenList.end() )
      {
      ( *it )->ComputeFamilyBoundingBoxInWorldSpace( depth-1, name );

      if( !bbDefined )
        {
        m_FamilyBoundingBoxInWorldSpace->SetMinimum(
          (*it)->GetFamilyBoundingBoxInWorldSpace()->GetMinimum() );
        m_FamilyBoundingBoxInWorldSpace->SetMaximum(
          (*it)->GetFamilyBoundingBoxInWorldSpace()->GetMaximum() );
        bbDefined = true;
        }
      else
        {
        m_FamilyBoundingBoxInWorldSpace->ConsiderPoint(
          (*it)->GetFamilyBoundingBoxInWorldSpace()->GetMinimum() );
        m_FamilyBoundingBoxInWorldSpace->ConsiderPoint(
          (*it)->GetFamilyBoundingBoxInWorldSpace()->GetMaximum() );
        }
      it++;
      }
    }

  return bbDefined;
}

/** Get the children list.
 * User is responsible for freeing the list, but not the elements of
 * the list. */
template< unsigned int TDimension >
typename SpatialObject< TDimension >::ChildrenListType *
SpatialObject< TDimension >
::GetChildren(unsigned int depth, const std::string & name) const
{
  auto * childrenSO = new ChildrenListType;

  auto it = m_ChildrenList.begin();
  while ( it != m_ChildrenList.end() )
    {
    if( (*it)->GetTypeName().find( name ) != std::string::npos )
      {
      childrenSO->push_back( (*it) );
      }
    it++;
    }

  if( depth > 0 )
    {
    it = m_ChildrenList.begin();
    while ( it != m_ChildrenList.end() )
      {
      (*it)->AddChildrenToList( childrenSO, depth-1, name );
      it++;
      }
    }

  return childrenSO;
}

template< unsigned int TDimension >
void
SpatialObject< TDimension >
::AddChildrenToList( ChildrenListType * childrenList , unsigned int depth,
 const std::string & name) const
{
  auto it = m_ChildrenList.begin();
  while ( it != m_ChildrenList.end() )
    {
    if( (*it)->GetTypeName().find( name ) != std::string::npos )
      {
      childrenList->push_back( (*it) );
      }
    it++;
    }

  if( depth > 0 )
    {
    it = m_ChildrenList.begin();
    while ( it != m_ChildrenList.end() )
      {
      (*it)->AddChildrenToList( childrenList, depth-1, name  );
      ++it;
      }
    }
}

/** Set children list */
template< unsigned int TDimension >
void
SpatialObject< TDimension >
::SetChildren(ChildrenListType & children)
{
  this->RemoveAllChildren(0);

  // Add children
  auto it = children.begin();
  while ( it != children.end() )
    {
    this->AddChild( (*it) );
    ++it;
    }
}

/** Get the number of children */
template< unsigned int TDimension >
unsigned int
SpatialObject< TDimension >
::GetNumberOfChildren(unsigned int depth, const std::string & name) const
{
  unsigned int ccount = 0;
  auto it = m_ChildrenList.begin();
  while ( it != m_ChildrenList.end() )
    {
    if( (*it)->GetTypeName().find( name ) != std::string::npos )
      {
      ++ccount;
      }
    it++;
    }

  if( depth > 0 )
    {
    it = m_ChildrenList.begin();
    while ( it != m_ChildrenList.end() )
      {
      ccount += (*it)->GetNumberOfChildren( depth-1, name );
      ++it;
      }
    }

  return ccount;
}

/** Return a SpatialObject in the SpatialObject
 *  given a parent ID */
template< unsigned int TDimension >
SpatialObject< TDimension > *
SpatialObject< TDimension >
::GetObjectById(int Id)
{
  if( Id == this->GetId() )
    {
    return this;
    }

  auto it = m_ChildrenList.begin();
  while ( it != m_ChildrenList.end() )
    {
    SpatialObject< TDimension > * tmp = (*it)->GetObjectById( Id );
    if( tmp != nullptr )
      {
      return tmp;
      }
    ++it;
    }

  return nullptr;
}

template< unsigned int TDimension >
bool
SpatialObject< TDimension >
::FixParentChildHierarchyUsingParentIds()
{
  ChildrenListType * children = this->GetChildren( MaximumDepth );

  auto it = children->begin();
  auto itEnd = children->end();
  typename ChildrenListType::iterator oldIt;

  bool ret = true;
  while ( it != itEnd )
    {
    const int parentId = ( *it )->GetParentId();
    if ( parentId >= 0 )
      {
      auto * parentObject = static_cast< SpatialObject< TDimension > * >(
        this->GetObjectById(parentId) );
      if ( parentObject == nullptr )
        {
        ret = false;
        ++it;
        }
      else
        {
        parentObject->AddChild( dynamic_cast< SpatialObject< TDimension > * >(
          ( *it ).GetPointer() ) );
        oldIt = it;
        ++it;
        }
      }
    else
      {
      ++it;
      }
    }

  delete children;

  return ret;
}

/** Check if the parent objects have a defined ID */
template< unsigned int TDimension >
bool
SpatialObject< TDimension >
::CheckIdValidity() const
{
  if( this->GetId() == -1 )
    {
    return false;
    }

  ChildrenListType * children = this->GetChildren();

  typename ObjectListType::iterator it = children->begin();
  typename ObjectListType::iterator itEnd = children->end();
  typename ObjectListType::iterator it2;
  int id;
  int id2;

  while ( it != itEnd )
    {
    id = (*it)->GetId();
    it2 = ++it;
    while( it2 != itEnd )
      {
      id2 = (*it2)->GetId();
      if( id == id2 || id2 == -1 )
        {
        delete children;
        return false;
        }
      ++it2;
      }
    }

  delete children;
  return true;
}

template< unsigned int TDimension >
void
SpatialObject< TDimension >
::FixIdValidity()
{
  if( this->GetId() == -1 )
    {
    this->SetId( this->GetNextAvailableId() );
    }

  ChildrenListType * children = this->GetChildren();

  typename ObjectListType::iterator it = children->begin();
  typename ObjectListType::iterator itEnd = children->end();
  typename ObjectListType::iterator it2;
  int id;
  int id2;

  while ( it != itEnd )
    {
    id = (*it)->GetId();
    it2 = ++it;
    while( it2 != itEnd )
      {
      id2 = (*it2)->GetId();
      if( id == id2 || id2 == -1 )
        {
        ( *it2 )->SetId( this->GetNextAvailableId() );
        // TODO: Go thru children and fix ParentId;
        //   - This should be done in the SetId function
        }
      ++it2;
      }
    }

  delete children;
}

/** Return the next available Id. For speed reason the MaxID+1 is returned */
template< unsigned int TDimension >
int
SpatialObject< TDimension >
::GetNextAvailableId() const
{
  int maxId = 0;

  ChildrenListType * children = this->GetChildren();

  typename ObjectListType::iterator it = children->begin();
  typename ObjectListType::iterator itEnd = children->end();
  int id;

  while ( it != itEnd )
    {
    id = (*it)->GetId();
    if( id > maxId )
      {
      maxId = id;
      }
    ++it;
    }

  delete children;

  return maxId + 1;
}

/** Return the Modified time of the LocalToGlobalTransform */
template< unsigned int TDimension >
unsigned long
SpatialObject< TDimension >
::GetObjectToParentTransformMTime()
{
  return m_ObjectToParentTransform->GetMTime();
}

/** Return the Modified time of the GlobalToLocalTransform */
template< unsigned int TDimension >
unsigned long
SpatialObject< TDimension >
::GetObjectToWorldTransformMTime()
{
  return m_ObjectToWorldTransform->GetMTime();
}

/** Get the parent of the spatial object */
template< unsigned int TDimension >
SpatialObject< TDimension > *
SpatialObject< TDimension >
::GetParent()
{
  return m_Parent;
}

/** Get the parent of the spatial object */
template< unsigned int TDimension >
const SpatialObject< TDimension > *
SpatialObject< TDimension >
::GetParent() const
{
  return m_Parent;
}

/** Set the parent of the spatial object */
template< unsigned int TDimension >
void
SpatialObject< TDimension >
::SetParent(Self *parent)
{
  if( parent != m_Parent )
    {
    Self * oldParent = m_Parent;
    TransformType * oldObjectWorldTransform = this->GetObjectToWorldTransform();

    m_Parent = parent;
    if( parent != nullptr )
      {
      m_ParentId = parent->GetId();
      m_Parent->AddChild( this );
      this->SetObjectToWorldTransform( oldObjectWorldTransform );
      }
    else
      {
      m_ParentId = -1;
      this->SetObjectToParentTransform( oldObjectWorldTransform );
      }

    if( oldParent != nullptr )
      {
      oldParent->RemoveChild( this );
      }
    }
}

/** Return true if the spatial object has a parent */
template< unsigned int TDimension >
bool
SpatialObject< TDimension >
::HasParent() const
{
  if( m_Parent == nullptr )
    {
    return false;
    }
  return true;
}

/** Set the largest possible region */
template< unsigned int TDimension >
void
SpatialObject< TDimension >
::SetLargestPossibleRegion(const RegionType & region)
{
  if ( m_LargestPossibleRegion != region )
    {
    m_LargestPossibleRegion = region;
    this->Modified();
    }
}

/** Update the Output information */
template< unsigned int TDimension >
void SpatialObject< TDimension >
::UpdateOutputInformation()
{
  if ( this->GetSource() )
    {
    this->GetSource()->UpdateOutputInformation();
    }
  // If we don't have a source, then let's make our Image
  // span our buffer
  else
    {
    m_LargestPossibleRegion = m_BufferedRegion;
    }

  // Now we should know what our largest possible region is. If our
  // requested region was not set yet, (or has been set to something
  // invalid - with no data in it ) then set it to the largest possible
  // region.
  if ( m_RequestedRegion.GetNumberOfPixels() == 0 )
    {
    this->SetRequestedRegionToLargestPossibleRegion();
    }
}

template< unsigned int TDimension >
void
SpatialObject< TDimension >
::SetRequestedRegionToLargestPossibleRegion()
{
  m_RequestedRegion = m_LargestPossibleRegion;
}

template< unsigned int TDimension >
bool
SpatialObject< TDimension >
::RequestedRegionIsOutsideOfTheBufferedRegion()
{
  unsigned int      i;
  const IndexType & requestedRegionIndex = m_RequestedRegion.GetIndex();
  const IndexType & bufferedRegionIndex = m_BufferedRegion.GetIndex();

  const SizeType & requestedRegionSize = m_RequestedRegion.GetSize();
  const SizeType & bufferedRegionSize = m_BufferedRegion.GetSize();

  for ( i = 0; i < ObjectDimension; i++ )
    {
    if ( ( requestedRegionIndex[i] < bufferedRegionIndex[i] )
         || ( ( requestedRegionIndex[i]
             + static_cast< OffsetValueType >( requestedRegionSize[i] ) )
              > ( bufferedRegionIndex[i]
                 + static_cast< OffsetValueType >( bufferedRegionSize[i] ) ) ) )
      {
      return true;
      }
    }
  return false;
}

template< unsigned int TDimension >
void
SpatialObject< TDimension >
::SetBufferedRegion(const RegionType & region)
{
  if ( m_BufferedRegion != region )
    {
    m_BufferedRegion = region;
    this->Modified();
    }
}

template< unsigned int TDimension >
bool
SpatialObject< TDimension >
::VerifyRequestedRegion()
{
  bool         retval = true;
  unsigned int i;

  // Is the requested region within the LargestPossibleRegion?
  // Note that the test is indeed against the largest possible region
  // rather than the buffered region; see DataObject::VerifyRequestedRegion.
  const IndexType & requestedRegionIndex = m_RequestedRegion.GetIndex();
  const IndexType & largestPossibleRegionIndex =
    m_LargestPossibleRegion.GetIndex();

  const SizeType & requestedRegionSize = m_RequestedRegion.GetSize();
  const SizeType & largestPossibleRegionSize =
    m_LargestPossibleRegion.GetSize();

  for ( i = 0; i < ObjectDimension; i++ )
    {
    if ( ( requestedRegionIndex[i] < largestPossibleRegionIndex[i] )
         || ( ( requestedRegionIndex[i]
             + static_cast< OffsetValueType >( requestedRegionSize[i] ) )
              > ( largestPossibleRegionIndex[i]
                 + static_cast< OffsetValueType >(
                   largestPossibleRegionSize[i] ) ) ) )
      {
      retval = false;
      }
    }

  return retval;
}

template< unsigned int TDimension >
void
SpatialObject< TDimension >
::SetRequestedRegion(const RegionType & region)
{
  if ( m_RequestedRegion != region )
    {
    m_RequestedRegion = region;
    this->Modified();
    }
}

template< unsigned int TDimension >
void
SpatialObject< TDimension >
::SetRequestedRegion(const DataObject *data)
{
  const auto * soData = dynamic_cast< const SpatialObject * >( data );
  const auto * imgData = dynamic_cast< const ImageBase<TDimension> * >( data );

  if( soData != nullptr )
    {
    m_RequestedRegion = soData->GetRequestedRegion();
    }
  else if( imgData != nullptr )
    {
    m_RequestedRegion = imgData->GetRequestedRegion();
    }
  else
    {
    itkExceptionMacro(
      << "SpatialObject::SetRequestedRegion(const DataObject *) cannot cast "
      << typeid( data ).name() << " to " << typeid( SpatialObject * ).name() );
    }
}

template< unsigned int TDimension >
void
SpatialObject< TDimension >
::Update()
{
  Superclass::Update();
}

/** Return the type of the spatial object as a string
 *  This is used by the SpatialObjectFactory */
template< unsigned int TDimension >
std::string
SpatialObject< TDimension >::GetClassNameAndDimension() const
{
  std::ostringstream n;

  n << GetNameOfClass();
  n << "_";
  n << TDimension;

  return n.str();
}

/** Copy the information from another spatial object */
template< unsigned int TDimension >
void SpatialObject< TDimension >
::CopyInformation(const DataObject *data)
{
  // Standard call to the superclass' method
  Superclass::CopyInformation(data);

  // Attempt to cast data to an ImageBase
  const SpatialObject< TDimension > * soData;
  soData = dynamic_cast< const SpatialObject< TDimension > * >( data );

  if ( soData == nullptr )
    {
    // pointer could not be cast back down
    itkExceptionMacro( << "itk::SpatialObject::CopyInformation() cannot cast "
                       << typeid( data ).name() << " to "
                       << typeid( SpatialObject< TDimension > * ).name() );
    }

  // Copy the meta data for this data type
  m_LargestPossibleRegion = soData->GetLargestPossibleRegion();

  // check if we are the same type
  const auto * source = dynamic_cast< const Self * >( data );
  if ( !source )
    {
    std::cout << "CopyInformation: objects are not of the same type"
              << std::endl;
    return;
    }

  // copy the properties
  this->SetProperty( source->GetProperty() );

  // copy the ivars
  this->SetObjectToWorldTransform( source->GetObjectToWorldTransform() );
  this->SetDefaultInsideValue( source->GetDefaultInsideValue() );
  this->SetDefaultOutsideValue( source->GetDefaultOutsideValue() );

  // Do not copy id, parent, or child info
  // this->SetParent( source->GetParent() );
}

} // end of namespace itk

#endif // __SpatialObject_hxx
