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
::SpatialObject(void)
{
  m_TypeName = "SpatialObject";
  m_Dimension = TDimension;
  m_Bounds = BoundingBoxType::New();
  m_BoundsMTime = 0;
  m_Property = PropertyType::New();
  m_TreeNode = ITK_NULLPTR;

  m_ObjectToWorldTransform = TransformType::New();
  m_ObjectToWorldTransform->SetIdentity();
  m_ObjectToParentTransform = TransformType::New();
  m_ObjectToParentTransform->SetIdentity();
  m_IndexToWorldTransform = TransformType::New();
  m_IndexToWorldTransform->SetIdentity();

  m_BoundingBoxChildrenDepth = MaximumDepth;
  m_Id = -1;
  m_ParentId = -1;
  m_AffineGeometryFrame = AffineGeometryFrameType::New();
  m_AffineGeometryFrame->SetIndexToWorldTransform(m_IndexToWorldTransform);
  m_TreeNode = SpatialObjectTreeNode< TDimension >::New();
  m_TreeNode->Set(this);
  m_InternalInverseTransform = TransformType::New();
  m_DefaultInsideValue = 1.0;
  m_DefaultOutsideValue  = 0.0;
}

/** Destructor */
template< unsigned int TDimension >
SpatialObject< TDimension >
::~SpatialObject(void)
{
  this->Clear();
}

/** Clear the spatial object by deleting all
 *  lists of children and subchildren */
template< unsigned int TDimension >
void
SpatialObject< TDimension >
::Clear(void)
{
  typename ChildrenListType::iterator pos = m_InternalChildrenList.begin();
  typename ChildrenListType::iterator it =  m_InternalChildrenList.begin();
  while ( it != m_InternalChildrenList.end() )
    {
    pos = it;
    it++;
    m_InternalChildrenList.erase(pos);
    }
  m_InternalChildrenList.clear();
}

/** Return the Derivative at a point given the order of the derivative */
template< unsigned int TDimension >
void
SpatialObject< TDimension >
::DerivativeAt(const PointType & point, short unsigned int order,
               OutputVectorType & value, unsigned int depth, char *name)
{
  if ( !IsEvaluableAt(point, depth, name) )
    {
    ExceptionObject e(__FILE__);
    e.SetLocation(
      "SpatialObject< TDimension >::DerivateAt(\
                   const PointType, unsigned short, OutputVectorType & )"                                                 );
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
    PointType        p1, p2;
    OutputVectorType v1, v2;
    typename OutputVectorType::Iterator it = value.Begin();
    typename OutputVectorType::Iterator it_v1 = v1.Begin();
    typename OutputVectorType::Iterator it_v2 = v2.Begin();

    for ( unsigned short i = 0; i < TDimension; i++, it++, it_v1++, it_v2++ )
      {
      p1 = point;
      p2 = point;

      // should get the spacing from the transform
      const double *spacing = this->GetModifiableIndexToObjectTransform()->GetScale();
      p1[i] -= spacing[i];
      p2[i] += spacing[i];

      try
        {
        DerivativeAt(p1, order - 1, v1, depth, name);
        DerivativeAt(p2, order - 1, v2, depth, name);
        }
      catch ( ExceptionObject e )
        {
        throw e;
        }

      ( *it ) = ( ( *it_v2 ) - ( *it_v1 ) ) / 2;
      }
    }
}

/** Return if a point is inside the object or its children */
template< unsigned int TDimension >
bool
SpatialObject< TDimension >
::IsInside(const PointType &  point, unsigned int depth, char *name) const
{
  if ( depth > 0 )
    {
    typedef typename TreeNodeType::ChildrenListType TreeChildrenListType;
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
                char *name) const
{
  if ( depth > 0 )
    {
    typedef typename TreeNodeType::ChildrenListType TreeChildrenListType;
    TreeChildrenListType *children = m_TreeNode->GetChildren();
    typename TreeChildrenListType::const_iterator it = children->begin();
    typename TreeChildrenListType::const_iterator itEnd = children->end();

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
          char *name) const
{
  bool evaluable = false;

  if ( depth > 0 )
    {
    typedef typename TreeNodeType::ChildrenListType TreeChildrenListType;
    TreeChildrenListType *children = m_TreeNode->GetChildren();
    typename TreeChildrenListType::const_iterator it = children->begin();
    typename TreeChildrenListType::const_iterator itEnd = children->end();

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
  os << indent << "Index to World Transform: " << m_IndexToWorldTransform
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
::AddSpatialObject(Self *pointer)
{
  m_TreeNode->AddChild( pointer->GetModifiableTreeNode() );
  m_InternalChildrenList.push_back(pointer);
  this->Modified();
}

/** Remove a child to the object */
template< unsigned int TDimension >
void
SpatialObject< TDimension >
::RemoveSpatialObject(Self *pointer)
{
  if ( m_TreeNode->Remove( pointer->GetModifiableTreeNode() ) )
    {
    typename ChildrenListType::iterator pos;
    pos = std::find(m_InternalChildrenList.begin(),
                    m_InternalChildrenList.end(), pointer);
    if ( pos != m_InternalChildrenList.end() )
      {
      m_InternalChildrenList.erase(pos);
      }
    this->Modified();
    }
  else
    {
    std::cerr << "Cannot RemoveSpatialObject" << std::endl;
    }
}

/** Set the local to global transformation */
template< unsigned int TDimension >
void
SpatialObject< TDimension >
::SetObjectToParentTransform(TransformType *transform)
{
  static_cast< TreeNodeType * >(
    m_TreeNode.GetPointer() )->SetNodeToParentNodeTransform(transform);
  ComputeObjectToWorldTransform();
}

/** Compute the Global Transform */
template< unsigned int TDimension >
void
SpatialObject< TDimension >
::ComputeObjectToWorldTransform()
{
  // The ObjectToParentTransform is the combination of the
  //    ObjectToNodeTransform and the NodeToParentNodeTransform
  m_ObjectToParentTransform->SetIdentity();
  m_ObjectToParentTransform->SetCenter(
    m_AffineGeometryFrame->GetObjectToNodeTransform()->GetCenter() );
  m_ObjectToParentTransform->Compose(
    m_AffineGeometryFrame->GetObjectToNodeTransform(), false);
  m_ObjectToParentTransform->Compose(
    static_cast< TreeNodeType * >(
      m_TreeNode.GetPointer() )->GetNodeToParentNodeTransform(), false);

  m_ObjectToWorldTransform->SetCenter(
    m_AffineGeometryFrame->GetObjectToNodeTransform()->GetCenter() );
  m_ObjectToWorldTransform->SetMatrix(
    m_AffineGeometryFrame->GetObjectToNodeTransform()->GetMatrix() );
  m_ObjectToWorldTransform->SetOffset(
    m_AffineGeometryFrame->GetObjectToNodeTransform()->GetOffset() );

  m_IndexToWorldTransform->SetCenter(
    m_AffineGeometryFrame->GetIndexToObjectTransform()->GetCenter() );
  m_IndexToWorldTransform->SetMatrix(
    m_AffineGeometryFrame->GetIndexToObjectTransform()->GetMatrix() );
  m_IndexToWorldTransform->SetOffset(
    m_AffineGeometryFrame->GetIndexToObjectTransform()->GetOffset() );

  static_cast< TreeNodeType * >( m_TreeNode.GetPointer() )
  ->ComputeNodeToWorldTransform();
  m_ObjectToWorldTransform->Compose(
    static_cast< TreeNodeType * >(
      m_TreeNode.GetPointer() )->GetNodeToWorldTransform(), false);

  m_IndexToWorldTransform->Compose(this->GetObjectToWorldTransform(), false);

  // Propagate the changes to the children
  typedef typename TreeNodeType::ChildrenListType TreeChildrenListType;
  TreeChildrenListType *children = m_TreeNode->GetChildren();
  typename TreeChildrenListType::const_iterator it = children->begin();
  typename TreeChildrenListType::const_iterator itEnd = children->end();

  while ( it != itEnd )
    {
    ( *it )->Get()->ComputeObjectToWorldTransform();
    it++;
    }
  // handle internal inverse
  if(!this->GetIndexToWorldTransform()->GetInverse( const_cast< TransformType *>( this->GetInternalInverseTransform() ) ))
    {
    this->m_InternalInverseTransform = ITK_NULLPTR;
    }
  delete children;
}
#if !defined(ITK_LEGACY_REMOVE)
/** Get the local transformation */
template< unsigned int TDimension >
typename SpatialObject< TDimension >::TransformType *
SpatialObject< TDimension >
::GetObjectToNodeTransform(void)
{
  return m_AffineGeometryFrame->GetModifiableObjectToNodeTransform();
}
#else
/** Get the local transformation */
template< unsigned int TDimension >
typename SpatialObject< TDimension >::TransformType *
SpatialObject< TDimension >
::GetModifiableObjectToNodeTransform(void)
{
  return m_AffineGeometryFrame->GetModifiableObjectToNodeTransform();
}
#endif
/** Get the local transformation (const) */
template< unsigned int TDimension >
const typename SpatialObject< TDimension >::TransformType *
SpatialObject< TDimension >
::GetObjectToNodeTransform(void) const
{
  return m_AffineGeometryFrame->GetObjectToNodeTransform();
}

/** Get the local transformation */
template< unsigned int TDimension >
typename SpatialObject< TDimension >::TransformType *
SpatialObject< TDimension >
::GetObjectToParentTransform(void)
{
  return static_cast< TreeNodeType * >(
           m_TreeNode.GetPointer() )->GetNodeToParentNodeTransform();
  //return m_ObjectToNodeTransform.GetPointer();
}

/** Get the local transformation (const) */
template< unsigned int TDimension >
const typename SpatialObject< TDimension >::TransformType *
SpatialObject< TDimension >
::GetObjectToParentTransform(void) const
{
  return static_cast< TreeNodeType * >(
           m_TreeNode.GetPointer() )->GetNodeToParentNodeTransform();
}


/** Get the local transformation (const) */
template< unsigned int TDimension >
const typename SpatialObject< TDimension >::TransformType *
SpatialObject< TDimension >
::GetIndexToObjectTransform(void) const
{
  return m_AffineGeometryFrame->GetIndexToObjectTransform();
}

/** Set the global to local transformation */
template< unsigned int TDimension >
void
SpatialObject< TDimension >
::SetObjectToWorldTransform(TransformType *transform)
{
  m_ObjectToWorldTransform = transform;
  ComputeObjectToParentTransform();
}

/** Compute the Transform when the global transform as been set
 *  This does not change the IndexToObjectMatrix */
template< unsigned int TDimension >
void
SpatialObject< TDimension >
::ComputeObjectToParentTransform()
{
  m_ObjectToParentTransform->SetScale( m_ObjectToWorldTransform->GetScale() );
  m_ObjectToParentTransform->SetCenter( m_ObjectToWorldTransform->GetCenter() );
  m_ObjectToParentTransform->SetMatrix( m_ObjectToWorldTransform->GetMatrix() );
  m_ObjectToParentTransform->SetOffset( m_ObjectToWorldTransform->GetOffset() );

  if ( m_TreeNode->HasParent() )
    {
    typename TransformType::Pointer inverse = TransformType::New();
    if ( static_cast< TreeNodeType * >( m_TreeNode->GetParent() )
         ->GetNodeToParentNodeTransform()->GetInverse(inverse) )
      {
      m_ObjectToParentTransform->Compose(inverse, true);
      }
    }

  m_AffineGeometryFrame->GetModifiableObjectToNodeTransform()->SetIdentity();
  static_cast< TreeNodeType * >( m_TreeNode.GetPointer() )
  ->GetNodeToParentNodeTransform()
  ->SetCenter( m_ObjectToParentTransform->GetCenter() );
  static_cast< TreeNodeType * >( m_TreeNode.GetPointer() )
  ->GetNodeToParentNodeTransform()
  ->SetMatrix( m_ObjectToParentTransform->GetMatrix() );
  static_cast< TreeNodeType * >( m_TreeNode.GetPointer() )
  ->GetNodeToParentNodeTransform()
  ->SetOffset( m_ObjectToParentTransform->GetOffset() );

  m_IndexToWorldTransform->SetCenter( m_AffineGeometryFrame
                                      ->GetIndexToObjectTransform()
                                      ->GetCenter() );
  m_IndexToWorldTransform->SetMatrix( m_AffineGeometryFrame
                                      ->GetIndexToObjectTransform()
                                      ->GetMatrix() );
  m_IndexToWorldTransform->SetOffset( m_AffineGeometryFrame
                                      ->GetIndexToObjectTransform()
                                      ->GetOffset() );
  m_IndexToWorldTransform->Compose(m_ObjectToWorldTransform, false);

  // handle internal inverse
  if(!this->GetIndexToWorldTransform()->GetInverse( const_cast< TransformType *>( this->GetInternalInverseTransform() ) ))
    {
    this->m_InternalInverseTransform = ITK_NULLPTR;
    }
}

/** Get the modification time  */
template< unsigned int TDimension >
ModifiedTimeType
SpatialObject< TDimension >
::GetMTime(void) const
{
  ModifiedTimeType latestTime = Object::GetMTime();

  if ( latestTime < m_BoundsMTime )
    {
    latestTime = m_BoundsMTime;
    }
  typedef typename TreeNodeType::ChildrenListType TreeChildrenListType;

  if ( !m_TreeNode )
    {
    return latestTime;
    }

  TreeChildrenListType *children = m_TreeNode->GetChildren();
  typename TreeChildrenListType::const_iterator it = children->begin();
  typename TreeChildrenListType::const_iterator itEnd = children->end();
  ModifiedTimeType localTime;

  while ( it != itEnd )
    {
    localTime = ( *it )->Get()->GetMTime();

    if ( localTime > latestTime )
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

  if ( m_BoundingBoxChildrenDepth > 0 && m_TreeNode )
    {
    typedef typename TreeNodeType::ChildrenListType TreeChildrenListType;
    TreeChildrenListType *children = m_TreeNode->GetChildren(0);
    typename TreeChildrenListType::const_iterator it = children->begin();
    typename TreeChildrenListType::const_iterator itEnd = children->end();

    while ( it != itEnd )
      {
      ( *it )->Get()->SetBoundingBoxChildrenDepth(m_BoundingBoxChildrenDepth - 1);
      ( *it )->Get()->SetBoundingBoxChildrenName(m_BoundingBoxChildrenName);
      ( *it )->Get()->ComputeBoundingBox();

      // If the bounding box is not defined we set the minimum and maximum
      bool bbDefined = false;
      for ( unsigned int i = 0; i < m_Dimension; i++ )
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
    delete children;
    return true;
    }

  typename BoundingBoxType::PointType pnt;
  pnt.Fill(NumericTraits< typename
                               BoundingBoxType::PointType::ValueType >::ZeroValue());
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
::GetChildren(unsigned int depth, char *name) const
{
  if ( !m_TreeNode )
    {
    return ITK_NULLPTR;
    }

  typename TreeNodeType::ChildrenListType * children =
    m_TreeNode->GetChildren(depth, name);
  typename TreeNodeType::ChildrenListType::const_iterator it =
    children->begin();

  ChildrenListType *childrenSO = new ChildrenListType;

  while ( it != children->end() )
    {
    childrenSO->push_back( ( *it )->Get() );
    it++;
    }

  delete children;
  return childrenSO;
}

/** Set children list */
template< unsigned int TDimension >
void
SpatialObject< TDimension >
::SetChildren(ChildrenListType & children)
{
  // Add children
  typename ChildrenListType::iterator it = children.begin();
  typename ChildrenListType::iterator itEnd = children.end();

  while ( it != itEnd )
    {
    static_cast< TreeNodeType * >(
      m_TreeNode.GetPointer() )->AddChild( ( *it )->GetModifiableTreeNode() );
    it++;
    }
}

/** Get the number of children */
template< unsigned int TDimension >
unsigned int
SpatialObject< TDimension >
::GetNumberOfChildren(unsigned int depth, char *name) const
{
  return m_TreeNode->GetNumberOfChildren(depth, name);
}

/** Return the Modified time of the LocalToGlobalTransform */
template< unsigned int TDimension >
unsigned long
SpatialObject< TDimension >
::GetTransformMTime(void)
{
  return m_ObjectToParentTransform->GetMTime();
}

/** Return the Modified time of the GlobalToLocalTransform */
template< unsigned int TDimension >
unsigned long
SpatialObject< TDimension >
::GetWorldTransformMTime(void)
{
  return m_IndexToWorldTransform->GetMTime();
}

/** Get the parent of the spatial object */
template< unsigned int TDimension >
SpatialObject< TDimension > *
SpatialObject< TDimension >
::GetParent(void)
{
  if ( m_TreeNode->HasParent() )
    {
    return m_TreeNode->GetParent()->Get();
    }
  return ITK_NULLPTR;
}

/** Get the parent of the spatial object */
template< unsigned int TDimension >
const SpatialObject< TDimension > *
SpatialObject< TDimension >
::GetParent(void) const
{
  if ( m_TreeNode->HasParent() )
    {
    return m_TreeNode->GetParent()->Get();
    }
  return ITK_NULLPTR;
}

/** Set the parent of the spatial object */
template< unsigned int TDimension >
void
SpatialObject< TDimension >
::SetParent(Self *parent)
{
  if ( !parent )
    {
    m_TreeNode->SetParent(ITK_NULLPTR);
    }
  else
    {
    m_TreeNode->SetParent( parent->GetModifiableTreeNode() );
    }
}

/** Return true if the spatial object has a parent */
template< unsigned int TDimension >
bool
SpatialObject< TDimension >
::HasParent(void) const
{
  return m_TreeNode->HasParent();
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

  for ( i = 0; i < m_Dimension; i++ )
    {
    if ( ( requestedRegionIndex[i] < bufferedRegionIndex[i] )
         || ( ( requestedRegionIndex[i] + static_cast< OffsetValueType >( requestedRegionSize[i] ) )
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
    this->ComputeOffsetTable();
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
  const SizeType & largestPossibleRegionSize = m_LargestPossibleRegion.GetSize();

  for ( i = 0; i < m_Dimension; i++ )
    {
    if ( ( requestedRegionIndex[i] < largestPossibleRegionIndex[i] )
         || ( ( requestedRegionIndex[i] + static_cast< OffsetValueType >( requestedRegionSize[i] ) )
              > ( largestPossibleRegionIndex[i]
                  + static_cast< OffsetValueType >( largestPossibleRegionSize[i] ) ) ) )
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
  const SpatialObject *imgData = dynamic_cast< const SpatialObject * >( data );

  if ( imgData == ITK_NULLPTR)
    {
    // pointer could not be cast back down
    itkExceptionMacro(
      << "itk::ImageBase::SetRequestedRegion(const DataObject *) cannot cast "
      << typeid( data ).name() << " to " << typeid( SpatialObject * ).name() );
    }

  m_RequestedRegion = imgData->GetRequestedRegion();
}

template< unsigned int TDimension >
void
SpatialObject< TDimension >
::ComputeOffsetTable()
{
  OffsetValueType  num = 1;
  const SizeType & bufferSize = m_BufferedRegion.GetSize();

  m_OffsetTable[0] = static_cast< OffsetValueType >( num );
  for ( unsigned int i = 0; i < m_Dimension; i++ )
    {
    num *= bufferSize[i];
    m_OffsetTable[i + 1] = static_cast< OffsetValueType >( num );
    }
}

template< unsigned int TDimension >
typename SpatialObject< TDimension >::PropertyType *
SpatialObject< TDimension >
::GetProperty(void)
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
::Update(void)
{
  Superclass::Update();

  /** This is probably not correct and should be removed */
  this->Modified();
}

template< unsigned int TDimension >
bool
SpatialObject< TDimension >
::SetInternalInverseTransformToWorldToIndexTransform() const
{
  if( this->m_InternalInverseTransform.IsNull() )
    {
    return false;
    }
  return true;
}

template< unsigned int TDimension >
void
SpatialObject< TDimension >
::SetNodeToParentNodeTransform(TransformType *transform)
{
  if ( !m_TreeNode )
    {
    static_cast< TreeNodeType * >(
      m_TreeNode.GetPointer() )->SetNodeToParentNodeTransform(transform);
    }
}

template< unsigned int TDimension >
typename SpatialObject< TDimension >::TransformType *
SpatialObject< TDimension >
::GetNodeToParentNodeTransform(void)
{
  if ( m_TreeNode )
    {
    return static_cast< TreeNodeType * >(
             m_TreeNode.GetPointer() )->GetNodeToParentNodeTransform();
    }
  return ITK_NULLPTR;
}

template< unsigned int TDimension >
const typename SpatialObject< TDimension >::TransformType *
SpatialObject< TDimension >
::GetNodeToParentNodeTransform(void) const
{
  if ( m_TreeNode )
    {
    return static_cast< TreeNodeType * >(
             m_TreeNode.GetPointer() )->GetNodeToParentNodeTransform();
    }
  return ITK_NULLPTR;
}

/** Return the type of the spatial object as a string
 *  This is used by the SpatialObjectFactory */
template< unsigned int TDimension >
std::string
SpatialObject< TDimension >::GetSpatialObjectTypeAsString() const
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

  imgData = dynamic_cast< const SpatialObject * >( data );

  if ( imgData == ITK_NULLPTR )
    {
    // pointer could not be cast back down
    itkExceptionMacro( << "itk::SpatialObject::CopyInformation() cannot cast "
                       << typeid( data ).name() << " to "
                       << typeid( SpatialObject * ).name() );
    }

  // Copy the meta data for this data type
  m_LargestPossibleRegion = imgData->GetLargestPossibleRegion();

  // check if we are the same type
  const Self *source = dynamic_cast< const Self * >( data );
  if ( !source )
    {
    std::cout << "CopyInformation: objects are not of the same type"
              << std::endl;
    return;
    }

  // copy the properties
  this->GetProperty()->SetRed( source->GetProperty()->GetRed() );
  this->GetProperty()->SetGreen( source->GetProperty()->GetGreen() );
  this->GetProperty()->SetBlue( source->GetProperty()->GetBlue() );
  this->GetProperty()->SetAlpha( source->GetProperty()->GetAlpha() );
  this->GetProperty()->SetName( source->GetProperty()->GetName().c_str() );

  // copy the ivars
  this->SetId( source->GetId() );
  this->SetParentId( source->GetParentId() );
}
} // end of namespace itk

#endif // __SpatialObject_hxx
