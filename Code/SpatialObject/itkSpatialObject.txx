/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSpatialObject.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __SpatialObject_txx
#define __SpatialObject_txx

#include "itkSpatialObject.h"
#include <itkNumericTraits.h>
#include <algorithm>
#include <string>

namespace itk 
{

/** Constructor */
template< unsigned int TDimension >
SpatialObject< TDimension >
::SpatialObject( void )
{
  m_TypeName = "SpatialObject";
  m_Dimension = TDimension;
  m_Bounds = BoundingBoxType::New();
  m_BoundsMTime = 0;
  m_Property = PropertyType::New();
  m_TreeNode = NULL;

  m_ObjectToWorldTransform = TransformType::New();
  m_ObjectToWorldTransform->SetIdentity();
  m_ObjectToParentTransform = TransformType::New();
  m_ObjectToParentTransform->SetIdentity();
  m_IndexToWorldTransform = TransformType::New();
  m_IndexToWorldTransform->SetIdentity();

  m_BoundingBoxChildrenDepth=MaximumDepth;
  m_Id = -1;
  m_ParentId = -1;
  m_AffineGeometryFrame = AffineGeometryFrameType::New();
  m_AffineGeometryFrame->SetIndexToWorldTransform(m_IndexToWorldTransform);
  m_TreeNode = SpatialObjectTreeNode<TDimension>::New();
  m_TreeNode->Set(this);
  m_InternalInverseTransform = TransformType::New();
  m_DefaultInsideValue = 1.0;
  m_DefaultOutsideValue  = 0.0;
}

/** Destructor */
template< unsigned int TDimension >
SpatialObject< TDimension >
::~SpatialObject( void )
{
  this->Clear();
}

/** Clear the spatial object by deleting all lists of children and subchildren */
template< unsigned int TDimension >
void
SpatialObject< TDimension >
::Clear(void)
{
  typename ChildrenListType::iterator pos = m_InternalChildrenList.begin();
  typename ChildrenListType::iterator it =  m_InternalChildrenList.begin();
  while( it != m_InternalChildrenList.end() ) 
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
::DerivativeAt( const PointType & point, short unsigned int order,
                OutputVectorType & value, unsigned int depth, char * name )
{
  if( !IsEvaluableAt(point, depth, name) )
    {
    itk::ExceptionObject e("SpatialObject.txx");
    e.SetLocation("SpatialObject< TDimension >::DerivateAt(\
                   const PointType, unsigned short, OutputVectorType & )");
    e.SetDescription("This spatial object is not evaluable at the point");
    throw e;
    }

  if( order == 0 )
    {
    double r;

    ValueAt(point, r, depth, name);
    value.Fill(r);
    }
  else
    {
    PointType p1,p2;
    OutputVectorType v1,v2;
    typename OutputVectorType::Iterator it = value.Begin();
    typename OutputVectorType::Iterator it_v1 = v1.Begin();
    typename OutputVectorType::Iterator it_v2 = v2.Begin();

    for( unsigned short i=0; i<TDimension; i++, it++, it_v1++, it_v2++ )
      {
      p1=point;
      p2=point;
      
      // should get the spacing from the transform
      const double* spacing = this->GetIndexToObjectTransform()
                                  ->GetScaleComponent();
      p1[i]-=spacing[i];
      p2[i]+=spacing[i];

      try
        {
        DerivativeAt(p1,order-1,v1, depth, name);
        DerivativeAt(p2,order-1,v2, depth, name);
        } 
      catch( itk::ExceptionObject e )
        {
        throw e;
        }

      (*it) = ((*it_v2)-(*it_v1))/2;
      }
    }
}

/** Return if a point is inside the object or its children */
template< unsigned int TDimension >
bool
SpatialObject< TDimension >
::IsInside( const PointType &  point, unsigned int depth, char * name) const
{
  if( depth > 0 )
    {
    typedef typename TreeNodeType::ChildrenListType ChildrenListType; 
    ChildrenListType* children = m_TreeNode->GetChildren();
    typename ChildrenListType::const_iterator it = children->begin();
    typename ChildrenListType::const_iterator itEnd = children->end();
    
    while(it!=itEnd)
      {
      if( (*it)->Get()->IsInside(point, depth-1, name) ) 
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
::IsEvaluableAt( const PointType & point, unsigned int depth,
                 char * name ) const
{
  if( depth > 0 )
    {
    typedef typename TreeNodeType::ChildrenListType ChildrenListType; 
    ChildrenListType* children = m_TreeNode->GetChildren();
    typename ChildrenListType::const_iterator it = children->begin();
    typename ChildrenListType::const_iterator itEnd = children->end();
    
    while(it!=itEnd)
      {
      if( (*it)->Get()->IsEvaluableAt(point, depth-1, name) ) 
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
::ValueAt( const PointType & point, double & value, unsigned int depth,
           char * name ) const
{  
  bool evaluable = false;
  if( depth > 0 )
    {
    typedef typename TreeNodeType::ChildrenListType ChildrenListType; 
    ChildrenListType* children = m_TreeNode->GetChildren();
    typename ChildrenListType::const_iterator it = children->begin();
    typename ChildrenListType::const_iterator itEnd = children->end();
  
    while(it!=itEnd)
      {
      if( (*it)->Get()->IsEvaluableAt(point, depth-1, name) )
        {
        (*it)->Get()->ValueAt(point,value, depth-1, name); 
        evaluable = true;
        break;
        }
      it++;
      } 
    delete children;
    }

  if(evaluable)
    {
    return true;
    }
  return false;
}

/** Print self */
template< unsigned int TDimension >
void 
SpatialObject< TDimension >
::PrintSelf( std::ostream& os, Indent indent ) const
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
::AddSpatialObject( Self * pointer )
{
  m_TreeNode->AddChild(pointer->GetTreeNode());
  m_InternalChildrenList.push_back(pointer);
  this->Modified();
}

/** Remove a child to the object */
template< unsigned int TDimension >
void
SpatialObject< TDimension >
::RemoveSpatialObject( Self * pointer )
{
  if(m_TreeNode->Remove(pointer->GetTreeNode()))
    {
    typename ChildrenListType::iterator pos;
    pos = std::find(m_InternalChildrenList.begin(),
                    m_InternalChildrenList.end(), pointer );
    if ( pos != m_InternalChildrenList.end() ) 
      {
      m_InternalChildrenList.erase(pos);
      }
    this->Modified();
    }
  else
    {
    std::cout << "Cannot RemoveSpatialObject" << std::endl;
    }
}


/** Set the local to global transformation */
template< unsigned int TDimension >
void 
SpatialObject< TDimension >
::SetObjectToParentTransform(TransformType * transform )
{
  static_cast<TreeNodeType*>(
       m_TreeNode.GetPointer())->SetNodeToParentNodeTransform(transform);
  ComputeObjectToWorldTransform();
}

/** Compute the Global Transform */
template< unsigned int TDimension >
void 
SpatialObject< TDimension >
::ComputeObjectToWorldTransform( )
{
  // The ObjectToParentTransform is the combination of the
  //    ObjectToNodeTransform and the NodeToParentNodeTransform
  m_ObjectToParentTransform->SetIdentity(); 
  m_ObjectToParentTransform->SetCenter(
       m_AffineGeometryFrame->GetObjectToNodeTransform()->GetCenter());
  m_ObjectToParentTransform->Compose(
       m_AffineGeometryFrame->GetObjectToNodeTransform(),false);
  m_ObjectToParentTransform->Compose(
       static_cast<TreeNodeType*>(
            m_TreeNode.GetPointer())->GetNodeToParentNodeTransform(),false);

  m_ObjectToWorldTransform->SetCenter(
       m_AffineGeometryFrame->GetObjectToNodeTransform()->GetCenter());
  m_ObjectToWorldTransform->SetMatrix(
       m_AffineGeometryFrame->GetObjectToNodeTransform()->GetMatrix());
  m_ObjectToWorldTransform->SetOffset(
       m_AffineGeometryFrame->GetObjectToNodeTransform()->GetOffset());

  m_IndexToWorldTransform->SetCenter(
       m_AffineGeometryFrame->GetIndexToObjectTransform()->GetCenter());
  m_IndexToWorldTransform->SetMatrix(
       m_AffineGeometryFrame->GetIndexToObjectTransform()->GetMatrix());
  m_IndexToWorldTransform->SetOffset(
       m_AffineGeometryFrame->GetIndexToObjectTransform()->GetOffset());

  static_cast<TreeNodeType*>(m_TreeNode.GetPointer())
       ->ComputeNodeToWorldTransform();
  m_ObjectToWorldTransform->Compose(
       static_cast<TreeNodeType*>(
            m_TreeNode.GetPointer())->GetNodeToWorldTransform(),false);

  m_IndexToWorldTransform->Compose(this->GetObjectToWorldTransform(),false);
  
  // Propagate the changes to the children
  typedef typename TreeNodeType::ChildrenListType ChildrenListType; 
  ChildrenListType* children = m_TreeNode->GetChildren();
  typename ChildrenListType::const_iterator it = children->begin();
  typename ChildrenListType::const_iterator itEnd = children->end();
    
  while(it!=itEnd)
    {
    (*it)->Get()->ComputeObjectToWorldTransform();
    it++;
    }
  delete children;
}

/** Get the local transformation */
template< unsigned int TDimension >
typename SpatialObject< TDimension >::TransformType *
SpatialObject< TDimension >
::GetObjectToNodeTransform( void )
{
  return m_AffineGeometryFrame->GetObjectToNodeTransform();
}

/** Get the local transformation (const)*/
template< unsigned int TDimension >
const typename SpatialObject< TDimension >::TransformType *
SpatialObject< TDimension >
::GetObjectToNodeTransform( void ) const
{
  return m_AffineGeometryFrame->GetObjectToNodeTransform();
}

/** Get the local transformation */
template< unsigned int TDimension >
typename SpatialObject< TDimension >::TransformType *
SpatialObject< TDimension >
::GetObjectToParentTransform( void )
{
  return static_cast<TreeNodeType*>(
       m_TreeNode.GetPointer())->GetNodeToParentNodeTransform();
  //return m_ObjectToNodeTransform.GetPointer();
}

/** Get the local transformation (const)*/
template< unsigned int TDimension >
const typename SpatialObject< TDimension >::TransformType *
SpatialObject< TDimension >
::GetObjectToParentTransform( void ) const
{
  return static_cast<TreeNodeType*>(
       m_TreeNode.GetPointer())->GetNodeToParentNodeTransform();
}

/** Get the local transformation */
template< unsigned int TDimension >
typename SpatialObject< TDimension >::TransformType *
SpatialObject< TDimension >
::GetIndexToObjectTransform( void )
{
  return m_AffineGeometryFrame->GetIndexToObjectTransform();
}

/** Get the local transformation (const)*/
template< unsigned int TDimension >
const typename SpatialObject< TDimension >::TransformType *
SpatialObject< TDimension >
::GetIndexToObjectTransform( void ) const
{
  return m_AffineGeometryFrame->GetIndexToObjectTransform();
}

/** Set the global to local transformation */
template< unsigned int TDimension >
void 
SpatialObject< TDimension >
::SetObjectToWorldTransform(TransformType * transform )
{
  m_ObjectToWorldTransform = transform;
  ComputeObjectToParentTransform();
}

/** Compute the Transform when the global tranform as been set
 *  This does not change the IndexToObjectMatrix */
template< unsigned int TDimension >
void 
SpatialObject< TDimension >
::ComputeObjectToParentTransform()
{

  m_ObjectToParentTransform->SetMatrixComponent(m_ObjectToWorldTransform->GetMatrixComponent());
  m_ObjectToParentTransform->SetOffsetComponent(m_ObjectToWorldTransform->GetOffsetComponent());
  m_ObjectToParentTransform->SetCenterOfRotationComponent(m_ObjectToWorldTransform->GetCenterOfRotationComponent());
  m_ObjectToParentTransform->SetScaleComponent(m_ObjectToWorldTransform->GetScaleComponent());

  if(m_TreeNode->HasParent())
    {
    typename TransformType::Pointer inverse = TransformType::New();
    if(static_cast<TreeNodeType*>(m_TreeNode->GetParent())->GetNodeToParentNodeTransform()->GetInverse(inverse))
      {
      m_ObjectToParentTransform->Compose(inverse,true);
      }
    
    }

  m_AffineGeometryFrame->GetObjectToNodeTransform()->SetIdentity();
  static_cast<TreeNodeType*>(m_TreeNode.GetPointer())->GetNodeToParentNodeTransform()->SetMatrix(m_ObjectToParentTransform->GetMatrix());
  static_cast<TreeNodeType*>(m_TreeNode.GetPointer())->GetNodeToParentNodeTransform()->SetOffset(m_ObjectToParentTransform->GetOffset());

  m_IndexToWorldTransform->SetMatrix(m_AffineGeometryFrame->GetIndexToObjectTransform()->GetMatrix());
  m_IndexToWorldTransform->SetOffset(m_AffineGeometryFrame->GetIndexToObjectTransform()->GetOffset());
  m_IndexToWorldTransform->Compose(m_ObjectToWorldTransform,false);
}

/** Get the modification time  */
template< unsigned int TDimension >
unsigned long 
SpatialObject< TDimension >
::GetMTime( void ) const
{
  unsigned long latestTime = Object::GetMTime();

  if( latestTime < m_BoundsMTime )
    {
    latestTime = m_BoundsMTime;
    }
  typedef typename TreeNodeType::ChildrenListType ChildrenListType; 
 
  if(!m_TreeNode)
    {
    return latestTime; 
    }
  
  ChildrenListType* children = m_TreeNode->GetChildren();
  typename ChildrenListType::const_iterator it = children->begin();
  typename ChildrenListType::const_iterator itEnd = children->end();
  unsigned long localTime;

  while(it!=itEnd)
    {
    localTime = (*it)->Get()->GetMTime();

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
  itkDebugMacro( "Computing Bounding Box" );
  this->ComputeLocalBoundingBox();

  if( m_BoundingBoxChildrenDepth > 0 && m_TreeNode)
    {
    typedef typename TreeNodeType::ChildrenListType ChildrenListType; 
    ChildrenListType* children = m_TreeNode->GetChildren(0);
    typename ChildrenListType::const_iterator it = children->begin();
    typename ChildrenListType::const_iterator itEnd = children->end();
    
    while(it!=itEnd)
      {
      (*it)->Get()->SetBoundingBoxChildrenDepth(m_BoundingBoxChildrenDepth-1);
      (*it)->Get()->SetBoundingBoxChildrenName(m_BoundingBoxChildrenName);
      (*it)->Get()->ComputeBoundingBox();

      m_Bounds->ConsiderPoint((*it)->Get()->GetBoundingBox()->GetMinimum());
      m_Bounds->ConsiderPoint((*it)->Get()->GetBoundingBox()->GetMaximum());
      it++;
      }
      delete children;
      return true;
    }

    typename BoundingBoxType::PointType pnt;
    pnt.Fill( itk::NumericTraits< ITK_TYPENAME 
              BoundingBoxType::PointType::ValueType>::Zero );
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
::GetChildren( unsigned int depth, char * name) const
{
  if(!m_TreeNode)
    {
    return 0;
    }
  
  typename TreeNodeType::ChildrenListType* children =
                                           m_TreeNode->GetChildren(depth,name);
  typename TreeNodeType::ChildrenListType::const_iterator it = 
                                                          children->begin();

  ChildrenListType * childrenSO = new ChildrenListType; 

  while(it != children->end())
    {
    childrenSO->push_back((*it)->Get());
    it++;
    }

  delete children;
  return childrenSO;
}

/** Set children list*/
template< unsigned int TDimension >
void
SpatialObject< TDimension >
::SetChildren( ChildrenListType & children )
{ 
  // Add children
  typename ChildrenListType::iterator it = children.begin();
  typename ChildrenListType::iterator itEnd = children.end();
  
  while(it != itEnd)
    {
    static_cast<TreeNodeType*>(m_TreeNode.GetPointer())->AddChild((*it)->GetTreeNode());  
    it++;
    }
}


/** Get the number of children */
template< unsigned int TDimension >
unsigned int
SpatialObject< TDimension >
::GetNumberOfChildren( unsigned int depth, char * name ) const
{
  return m_TreeNode->GetNumberOfChildren(depth,name);
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
::GetParent( void )
{
  if(m_TreeNode->HasParent())
    {
    return m_TreeNode->GetParent()->Get();
    }
  return NULL;
}

/** Get the parent of the spatial object */
template< unsigned int TDimension >
const SpatialObject< TDimension > *
SpatialObject< TDimension >
::GetParent( void ) const
{
  if(m_TreeNode->HasParent())
    {
    return m_TreeNode->GetParent()->Get();
    }
  return NULL;
}

/** Set the parent of the spatial object */
template< unsigned int TDimension >
void
SpatialObject< TDimension >
::SetParent( Self * parent )
{
  if(!parent)
    {
    m_TreeNode->SetParent(NULL);
    }
  else
    {
    m_TreeNode->SetParent(parent->GetTreeNode());
    }
}

/** Return true if the spatial object has a parent */
template< unsigned int TDimension >
bool
SpatialObject< TDimension >
::HasParent( void ) const
{
  return  m_TreeNode->HasParent();
}

/** Set the largest possible region */
template< unsigned int TDimension >
void
SpatialObject< TDimension >
::SetLargestPossibleRegion(const RegionType &region)
{
  if (m_LargestPossibleRegion != region)
    {
    m_LargestPossibleRegion = region;
    this->Modified();
    }
}

/** Update the Output information */
template< unsigned int TDimension >
void
SpatialObject< TDimension >
::UpdateOutputInformation()
{
  if (this->GetSource())
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
  if ( m_RequestedRegion.GetNumberOfPixels() == 0)
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
void
SpatialObject< TDimension >
::CopyInformation(const DataObject *data)
{
  // Standard call to the superclass' method
  Superclass::CopyInformation(data);

  // Attempt to cast data to an ImageBase
  const SpatialObject *imgData;
  
  imgData = dynamic_cast<const SpatialObject*>(data);

  if (imgData)
    {
    // Copy the meta data for this data type
    m_LargestPossibleRegion = imgData->GetLargestPossibleRegion();
    }
  else
    {
    // pointer could not be cast back down
    itkExceptionMacro( << "itk::SpatialObject::CopyInformation() cannot cast "
                       << typeid(data).name() << " to "
                       << typeid(SpatialObject*).name() );
    }
}


template< unsigned int TDimension >
bool
SpatialObject< TDimension >
::RequestedRegionIsOutsideOfTheBufferedRegion()
{
  unsigned int i;
  const IndexType &requestedRegionIndex = m_RequestedRegion.GetIndex();
  const IndexType &bufferedRegionIndex = m_BufferedRegion.GetIndex();

  const SizeType& requestedRegionSize = m_RequestedRegion.GetSize();
  const SizeType& bufferedRegionSize = m_BufferedRegion.GetSize();
  
  for (i=0; i< m_Dimension; i++)
    {
    if ( (requestedRegionIndex[i] < bufferedRegionIndex[i]) ||
         ((requestedRegionIndex[i] + static_cast<long>(requestedRegionSize[i]))
          > (bufferedRegionIndex[i] + static_cast<long>(bufferedRegionSize[i]))) )
      {
      return true;
      }
    }
  return false;
}


template< unsigned int TDimension >
void
SpatialObject< TDimension >
::SetBufferedRegion(const RegionType &region)
{
  if (m_BufferedRegion != region)
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
  bool retval = true;
  unsigned int i;

  // Is the requested region within the LargestPossibleRegion?
  // Note that the test is indeed against the largest possible region
  // rather than the buffered region; see DataObject::VerifyRequestedRegion.
  const IndexType &requestedRegionIndex = m_RequestedRegion.GetIndex();
  const IndexType &largestPossibleRegionIndex
    = m_LargestPossibleRegion.GetIndex();

  const SizeType& requestedRegionSize = m_RequestedRegion.GetSize();
  const SizeType& largestPossibleRegionSize
    = m_LargestPossibleRegion.GetSize();
  
  for (i=0; i< m_Dimension; i++)
    {
    if ( (requestedRegionIndex[i] < largestPossibleRegionIndex[i]) ||
         ((requestedRegionIndex[i] + static_cast<long>(requestedRegionSize[i]))
          > (largestPossibleRegionIndex[i]+static_cast<long>(largestPossibleRegionSize[i]))))
      {
      retval = false;
      }
    }

  return retval;
}

template< unsigned int TDimension >
void
SpatialObject< TDimension >
::SetRequestedRegion(const RegionType &region)
{
  if (m_RequestedRegion != region)
    {
    m_RequestedRegion = region;
    this->Modified();
    }
}


template< unsigned int TDimension >
void
SpatialObject< TDimension >
::SetRequestedRegion(DataObject *data)
{
  SpatialObject *imgData;
  
  imgData = dynamic_cast<SpatialObject*>(data);

  if (imgData)
    {
    m_RequestedRegion = imgData->GetRequestedRegion();
    }
  else
    {
    // pointer could not be cast back down
    itkExceptionMacro( << "itk::ImageBase::SetRequestedRegion(DataObject*) cannot cast " << typeid(data).name() << " to " << typeid(SpatialObject*).name() );
    }
}


template< unsigned int TDimension >
void
SpatialObject< TDimension >
::ComputeOffsetTable()
{
  double num=1;
  const SizeType& bufferSize = m_BufferedRegion.GetSize();
  
  m_OffsetTable[0] = num;
  for (unsigned int i=0; i < m_Dimension; i++)
    {
    num *= bufferSize[i];
    m_OffsetTable[i+1] = num;
    }
}


template< unsigned int TDimension >
typename SpatialObject< TDimension >::PropertyType * 
SpatialObject< TDimension >
::GetProperty( void )
{ 
  return m_Property; 
}


template< unsigned int TDimension >
void
SpatialObject< TDimension >
::SetProperty( const PropertyType * property)
{ 
  m_Property = property; 
}

template< unsigned int TDimension >
void
SpatialObject< TDimension >
::Update(void)
{
  this->Modified();
}

template< unsigned int TDimension >
void
SpatialObject< TDimension >
::SetSpacing( const SpacingType & spacing )
{
   double spacingValues[ TDimension ];
   for(unsigned int i=0; i<TDimension; i++)
      {
      spacingValues[i] = spacing[i];
      }  
   this->SetSpacing( spacingValues );
}

template< unsigned int TDimension >
void
SpatialObject< TDimension >
::SetNodeToParentNodeTransform( TransformType * transform )
{
  if(!m_TreeNode)
    {
    static_cast<TreeNodeType*>(m_TreeNode.GetPointer())->SetNodeToParentNodeTransform(transform);
    }
}
  
template< unsigned int TDimension >
typename SpatialObject< TDimension >::TransformType * 
SpatialObject< TDimension >
::GetNodeToParentNodeTransform( void )
{
  if(m_TreeNode)
    {
    return static_cast<TreeNodeType*>(m_TreeNode.GetPointer())->GetNodeToParentNodeTransform();
    }
  return NULL;
}
  
template< unsigned int TDimension >
const typename SpatialObject< TDimension >::TransformType * 
SpatialObject< TDimension >
::GetNodeToParentNodeTransform( void ) const
{
  if(m_TreeNode)
    {
    return static_cast<TreeNodeType*>(m_TreeNode.GetPointer())->GetNodeToParentNodeTransform();
    }
  return NULL;
}
  


} // end of namespace itk

#endif // __SpatialObject_txx
