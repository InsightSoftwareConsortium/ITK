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

namespace itk
{
/** Constructor */
template< unsigned int TDimension >
SpatialObject< TDimension >
::SpatialObject()
{
  m_TypeName = "SpatialObject";
  m_Bounds = BoundingBoxType::New();
  m_BoundsMTime = 0;
  m_Property = PropertyType::New();

  m_ObjectToWorldTransform = TransformType::New();
  m_ObjectToWorldTransform->SetIdentity();
  m_ObjectToParentTransform = TransformType::New();
  m_ObjectToParentTransform->SetIdentity();

  m_BoundingBoxChildrenDepth = MaximumDepth;
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
  this->RemoveAllChildren();
}

/** Return the Derivative at a point given the order of the derivative */
template< unsigned int TDimension >
void
SpatialObject< TDimension >
::DerivativeAt(const PointType & point, short unsigned int order,
               DerivativeVectorType & value, unsigned int depth, const std::string & name,
               const SpacingVectorType & spacing)
{
  if ( !IsEvaluableAt(point, depth, name) )
    {
    ExceptionObject e(__FILE__);
    e.SetLocation(
      "SpatialObject< TDimension >::DerivateAt(\
                   const PointType, unsigned short, DerivativeVectorType & )"                                                 );
    e.SetDescription("This spatial object is not evaluable at the point");
    throw e;
    }

  if ( order == 0 )
    {
    double r;

    ValueAt(point, r, depth, name);
    value.Fill(r);
    }
  else
    {
    PointType                  p1, p2;
    DerivativeVectorType       v1, v2;
    typename DerivativeVectorType::Iterator it = value.Begin();
    typename DerivativeVectorType::Iterator it_v1 = v1.Begin();
    typename DerivativeVectorType::Iterator it_v2 = v2.Begin();

    typename SpacingVectorType spacingDiv2;
    for ( unsigned short i = 0; i < TDimension; i++ )
      {
      spacingDiv2[i] = spacing[i] / 2.0;
      }
    for ( unsigned short i = 0; i < TDimension; i++, it++, it_v1++, it_v2++ )
      {
      p1 = point;
      p2 = point;

      p1[i] -= spacing[i];
      p2[i] += spacing[i];

      // note DerivativeAt might throw.
      DerivativeAt(p1, order - 1, v1, depth, name, spacingDiv2);
      DerivativeAt(p2, order - 1, v2, depth, name, spacingDiv2);

      ( *it ) = ( ( *it_v2 ) - ( *it_v1 ) ) / 2;
      }
    }
}

/** Return if a point is inside the object or its children */
template< unsigned int TDimension >
bool
SpatialObject< TDimension >
::IsInside(const PointType &  point, unsigned int depth, const std::string & name) const
{
  if ( depth > 0 )
    {
    using TreeChildrenListType = typename TreeNodeType::ChildrenListType;
    TreeChildrenListType *children = m_TreeNode->GetChildren();
    typename TreeChildrenListType::const_iterator it = children->begin();
    typename TreeChildrenListType::const_iterator itEnd = children->end();

    while ( it != itEnd )
      {
      if ( ( *it )->Get()->IsInside(point, depth - 1, name) )
        {
        delete children;
        return true;
        }
      it++;
      }
    delete children;
    }

  return false;
}

/** Return if the object is evaluable at a point */
template< unsigned int TDimension >
bool
SpatialObject< TDimension >
::IsEvaluableAt(const PointType & point, unsigned int depth,
                const std::string & name) const
{
  if ( depth > 0 )
    {
    ChildrenListType *children = this->GetChildren( depth, name );

    while ( it != itEnd )
      {
      if ( ( *it )->Get()->IsEvaluableAt(point, depth - 1, name) )
        {
        delete children;
        return true;
        }
      it++;
      }
    delete children;
    }

  return false;
}

/** Return the value of the object at a point */
template< unsigned int TDimension >
bool
SpatialObject< TDimension >
::ValueAt(const PointType & point, double & value, unsigned int depth,
          const std::string & name) const
{
  bool evaluable = false;

  value = 0;
  if ( depth > 0 )
    {
    ChildrenListType *children = this->GetChildren( depth, name );

    while ( it != itEnd )
      {
      if ( ( *it )->Get()->IsEvaluableAt(point, depth - 1, name) )
        {
        ( *it )->Get()->ValueAt(point, value, depth - 1, name);
        evaluable = true;
        break;
        }
      it++;
      }
    delete children;
    }

  if ( evaluable )
    {
    return true;
    }
  return false;
}

/** Print self */
template< unsigned int TDimension >
void
SpatialObject< TDimension >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << "Bounding Box:" << std::endl;
  os << indent << m_Bounds << std::endl;
  os << "Geometric properties:" << std::endl;
  os << indent << "Object to World Transform: " << m_ObjectToWorldTransform
     << std::endl;
  os << std::endl << std::endl;
  os << indent << "Bounding Box Children Depth: " << m_BoundingBoxChildrenDepth
     << std::endl;
  os << indent << "Bounding Box Children Name: " << m_BoundingBoxChildrenName
     << std::endl;
  os << "Object properties: " << std::endl;
  os << m_Property << std::endl;
}

/** Get the bounds of the object */
template< unsigned int TDimension >
typename SpatialObject< TDimension >::BoundingBoxType *
SpatialObject< TDimension >
::GetBoundingBox() const
{
  return m_Bounds.GetPointer();
}

/** Add a child to the object */
template< unsigned int TDimension >
void
SpatialObject< TDimension >
::AddChild(Self *pointer)
{
  typename ChildrenListType::iterator pos;
  pos = std::find(m_InternalChildrenList.begin(),
                  m_InternalChildrenList.end(), pointer);
  if( pos == m_InternalChildrenList.end() )
    {
    m_InternalChildrenList.push_back(pointer);

    pointer->SetParent( this );

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
  pos = std::find(m_InternalChildrenList.begin(),
                  m_InternalChildrenList.end(), pointer);
  if ( pos != m_InternalChildrenList.end() )
    {
    pointer->SetParent( nullptr );

    m_InternalChildrenList.erase(pos);

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
  typename ChildrenListType::iterator it = m_InternalChildrenList.begin();
  while( it != m_InternalChildrenList.end() )
    {
    Self * oldChild = (*it)->Get();
    it = m_InternalChildrenList.erase( it );
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
::SetObjectToParentTransform(TransformType *transform)
{
  m_ObjectToParentTransform.CopyInFixedParameters( transform );
  m_ObjectToParentTransform.CopyInParameters( transform );
  ComputeObjectToWorldTransform();
}

/** Compute the Global Transform */
template< unsigned int TDimension >
void
SpatialObject< TDimension >
::ComputeObjectToWorldTransform()
{
  m_ObjectToWorldTransform->CopyInFixedParameters(
    this->GetObjectToParentTransform() );
  m_ObjectToWorldTransform->CopyInParameters(
    this->GetObjectToParentTransform() );
  if( this->HasParent() )
    {
    m_ObjectToWorldTransform->Compose( this->GetParent()
      ->GetObjectToWorldTransform(), false );
    }

  // Propagate the changes to the children
  ChildrenListType *children = this->GetChildren();
  typename ChildrenListType::iterator it = children->begin();
  typename ChildrenListType::iterator itEnd = children->end();
  while ( it != itEnd )
    {
    ( *it )->Get()->ComputeObjectToWorldTransform();
    it++;
    }
  delete children;
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
::SetObjectToWorldTransform(TransformType *transform)
{
  m_ObjectToWorldTransform->CopyInFixedParameters( transform );
  m_ObjectToWorldTransform->CopyInParameters( transform );
  ComputeObjectToParentTransform();
}

/** Compute the Transform when the global transform as been set
 *  This does not change the IndexToObjectMatrix */
template< unsigned int TDimension >
void
SpatialObject< TDimension >
::ComputeObjectToParentTransform()
{
  m_ObjectToParentTransform->CopyInFixedParameters( m_ObjectToWorldTransform );
  m_ObjectToParentTransform->CopyInParameters( m_ObjectToWorldTransform );

  if( this->HasParent() )
    {
    typename TransformType::Pointer inverse = TransformType::New();
    if( this->GetParent()->GetObjectToWorldTransform()->GetInverse(inverse) )
      {
      m_ObjectToParentTransform->Compose(inverse, true);
      }
    }
}

/** Get the modification time  */
template< unsigned int TDimension >
ModifiedTimeType
SpatialObject< TDimension >
::GetMTime() const
{
  ModifiedTimeType latestTime = Object::GetMTime();

  if( latestTime < m_BoundsMTime )
    {
    latestTime = m_BoundsMTime;
    }

  ChildrenListType *children = this->GetChildren();
  typename ChildrenListType::const_iterator it = children->begin();
  typename ChildrenListType::const_iterator itEnd = children->end();
  ModifiedTimeType localTime;

  while( it != itEnd )
    {
    localTime = ( *it )->Get()->GetMTime();

    if( localTime > latestTime )
      {
      latestTime = localTime;
      }
    it++;
    }
  delete children;

  return latestTime;
}

/**
 * Compute an axis-aligned bounding box for an object and its selected
 * children, down to a specified depth.  After computation, the
 * resulting bounding box is stored in this->m_Bounds.
 *
 * By default, the bounding box children depth is maximum, meaning that
 * the bounding box for the object and all its recursive children is computed.
 * This depth can be set (before calling ComputeBoundingBox) using
 * SetBoundingBoxChildrenDepth().
 *
 * By calling SetBoundingBoxChildrenName(), it is possible to restrict
 * the bounding box computation to objects of a specified type or
 * family of types.  The spatial objects included in the computation
 * are those whose typenames share, as their initial substring, the
 * string specified via SetBoundingBoxChildrenName().  The root
 * spatial object (on which the method is called) is not treated
 * specially.  If its typename does not match the bounding box
 * children name, then it is not included in the bounding box
 * computation, but its descendents that match the string are
 * included.
 */
template< unsigned int TDimension >
bool
SpatialObject< TDimension >
::ComputeBoundingBox() const
{
  itkDebugMacro("Computing Bounding Box");
  this->ComputeLocalBoundingBox();

  if ( m_BoundingBoxChildrenDepth > 0 )
    {
    typename ChildrenListType::const_iterator it =
      m_InternalChildrenList->begin();
    while ( it != m_InternalChildrenList->end() )
      {
      ( *it )->Get()->SetBoundingBoxChildrenDepth(
        m_BoundingBoxChildrenDepth - 1 );
      ( *it )->Get()->SetBoundingBoxChildrenName( m_BoundingBoxChildrenName );
      ( *it )->Get()->ComputeBoundingBox();

      // If the bounding box is not defined we set the minimum and maximum
      bool bbDefined = false;
      for ( unsigned int i = 0; i < ObjectDimension; i++ )
        {
        if ( Math::NotExactlyEquals(m_Bounds->GetBounds()[2 * i], 0)
             || Math::NotExactlyEquals(m_Bounds->GetBounds()[2 * i + 1], 0) )
          {
          bbDefined = true;
          break;
          }
        }

      if ( !bbDefined )
        {
        m_Bounds->SetMinimum( ( *it )->Get()->GetBoundingBox()->GetMinimum() );
        m_Bounds->SetMaximum( ( *it )->Get()->GetBoundingBox()->GetMaximum() );
        }
      else
        {
        m_Bounds->ConsiderPoint( ( *it )->Get()->GetBoundingBox()->GetMinimum() );
        m_Bounds->ConsiderPoint( ( *it )->Get()->GetBoundingBox()->GetMaximum() );
        }
      it++;
      }
    return true;
    }

  typename BoundingBoxType::PointType pnt;
  pnt.Fill( NumericTraits< typename BoundingBoxType::PointType::ValueType >::
    ZeroValue() );
  m_Bounds->SetMinimum(pnt);
  m_Bounds->SetMaximum(pnt);
  m_BoundsMTime = this->GetMTime();

  return false;
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

  auto it = m_InternalChildrenList->begin();
  while ( it != m_InternChildrenList->end() )
    {
    if( name.compare( 0, name.size(), (*it)->Get()->GetTypeName() ) == 0 )
      {
      childrenSO->push_back( ( *it )->Get() );
      }
    it++;
    }

  if( depth > 0 )
    {
    it = m_InternChildrenList->begin();
    while ( it != m_InternChildrenList->end() )
      {
      (*it)->Get()->AddChildrenToList( depth-1, name, childrenSO );
      }
    }

  return childrenSO;
}

template< unsigned int TDimension >
void
SpatialObject< TDimension >
::AddChildrenToList( unsigned int depth, const std::string & name,
  ChildrenListType * childrenList ) const
{
  auto it = m_InternalChildrenList->begin();
  while ( it != m_InternalChildrenList->end() )
    {
    if( name.compare( 0, name.size(), (*it)->Get()->GetTypeName() ) == 0 )
      {
      childrenList->push_back( ( *it )->Get() );
      }
    it++;
    }

  if( depth > 0 )
    {
    it = m_InternalChildrenList->begin();
    while ( it != m_InternalChildrenList->end() )
      {
      (*it)->Get()->AddChildrenToList( depth-1, name, childrenList );
      }
    }
}

/** Set children list */
template< unsigned int TDimension >
void
SpatialObject< TDimension >
::SetChildren(ChildrenListType & children)
{
  this->RemoveAllChildren();

  // Add children
  auto it = children.begin();
  while ( it != children.end() )
    {
    this->AddChild( ( *it )->Get() );
    it++;
    }
}

/** Get the number of children */
template< unsigned int TDimension >
unsigned int
SpatialObject< TDimension >
::GetNumberOfChildren(unsigned int depth, const std::string & name) const
{
  unsigned int ccount = 0;
  auto it = m_InternalChildrenList->begin();
  while ( it != m_InternalChildrenList->end() )
    {
    if( name.compare( 0, name.size(), (*it)->Get()->GetTypeName() ) == 0 )
      {
      ++ccount;
      }
    it++;
    }

  if( depth > 0 )
    {
    it = m_InternalChildrenList->begin();
    while ( it != m_InternalChildrenList->end() )
      {
      ccount += (*it)->Get()->GetNumberOfChildren( depth-1, name );
      }
    }

  return ccount;
}

/** Return the Modified time of the LocalToGlobalTransform */
template< unsigned int TDimension >
unsigned long
SpatialObject< TDimension >
::GetTransformMTime()
{
  return m_ObjectToParentTransform->GetMTime();
}

/** Return the Modified time of the GlobalToLocalTransform */
template< unsigned int TDimension >
unsigned long
SpatialObject< TDimension >
::GetWorldTransformMTime()
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
    m_Parent = parent;
    if( parent != nullptr )
      {
      m_ParentId = parent->GetId();
      this->ComputeObjectToParentTransform();
      m_Parent->AddChild( this );
      }
    else
      {
      m_parentId = 0;
      this->SetObjectToParentTransform( this->GetObjectToWorldTransform() );
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
  const auto * imgData = dynamic_cast< const ImageBase * >( data );

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
typename SpatialObject< TDimension >::PropertyType *
SpatialObject< TDimension >
::GetProperty()
{
  return m_Property;
}

template< unsigned int TDimension >
void
SpatialObject< TDimension >
::SetProperty(PropertyType *property)
{
  m_Property = property;
}

template< unsigned int TDimension >
void
SpatialObject< TDimension >
::Update()
{
  Superclass::Update();

  /** This is probably not correct and should be removed */
  this->Modified();
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
  const SpatialObject *imgData;

  soData = dynamic_cast< const SpatialObject * >( data );

  if ( soData == nullptr )
    {
    // pointer could not be cast back down
    itkExceptionMacro( << "itk::SpatialObject::CopyInformation() cannot cast "
                       << typeid( data ).name() << " to "
                       << typeid( SpatialObject * ).name() );
    }

  // Copy the meta data for this data type
  m_LargestPossibleRegion = imgData->GetLargestPossibleRegion();

  // check if we are the same type
  const auto * source = dynamic_cast< const Self * >( data );
  if ( !source )
    {
    std::cout << "CopyInformation: objects are not of the same type"
              << std::endl;
    return;
    }

  // copy the properties
  this->GetProperty()->CopyInformation( source->GetProperty() );

  // copy the ivars
  this->SetObjectToWorldTransform( source-GetObjectToWorldTransform() );
  this->SetParent( source->GetParent() );
}

} // end of namespace itk

#endif // __SpatialObject_hxx
