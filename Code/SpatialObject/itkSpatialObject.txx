/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSpatialObject.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __SpatialObject_txx
#define __SpatialObject_txx

#include "itkSpatialObject.h"

namespace itk 
{

/** Constructor */
template< unsigned int NDimensions, typename TTransform, unsigned int PipelineDimension >
SpatialObject< NDimensions, TTransform, PipelineDimension>
::SpatialObject( void )
{
  m_ParentId=-1;
  m_Dimension = NDimensions;
  m_Bounds = BoundingBoxType::New();
  m_Property = PropertyType::New();
  m_LocalToGlobalTransform = TransformType::New();
  m_GlobalToLocalTransform = TransformType::New();
  m_Spacing.resize(NDimensions);
  m_Spacing.fill(1);
  SetParent(NULL);
  BuildLocalToGlobalTransformList(m_LocalToGlobalTransformList,false);
  BuildGlobalToLocalTransformList(m_GlobalToLocalTransformList,false);
}

/** Destructor */
template< unsigned int NDimensions, typename TTransform, unsigned int PipelineDimension >
SpatialObject< NDimensions, TTransform, PipelineDimension>
::~SpatialObject( void )
{
}


/** Return the Derivative at a point given the order of the derivative */
template< unsigned int NDimensions, typename TTransform, unsigned int PipelineDimension >
void
SpatialObject< NDimensions, TTransform, PipelineDimension>
::DerivativeAt( const PointType & point, short unsigned int order, OutputVectorType & value )
{
  if( !IsEvaluableAt(point) )
  {
    itk::ExceptionObject e("SpatialObject.txx");
    e.SetLocation("SpatialObject< NDimensions, TTransform, PipelineDimension>::DerivateAt(\
    const PointType, unsigned short, OutputVectorType & )");
    e.SetDescription("This spatial object is not derivable at the requested point");
    throw e;
  }

  double r;

  if( order == 0 )
  {
    ValueAt(point,r);
    value.Fill(r);
  }
  else
  {
    PointType p1,p2;
    OutputVectorType v1,v2;
    typename OutputVectorType::Iterator it = value.Begin();
    typename OutputVectorType::Iterator it_v1 = v1.Begin();
    typename OutputVectorType::Iterator it_v2 = v2.Begin();

    for( unsigned short i=0; i<NDimensions; i++, it++, it_v1++, it_v2++ )
    {
      p1=point;
      p2=point;
      p1[i]-=m_Spacing[i];
      p2[i]+=m_Spacing[i];

      try
      {
        DerivativeAt(p1,order-1,v1);
        DerivativeAt(p2,order-1,v2);
      } 
      catch( itk::ExceptionObject e )
      {
      
        throw;
      }

      (*it) = ((*it_v2)-(*it_v1))/2;
      }
    }

    /** Compute parent derivatives */
    typename ChildrenListType::iterator it = m_Children.begin();
    typename ChildrenListType::iterator end = m_Children.end();
  
    for(; it!=end; it++)
    {
      if( (*it)->IsInside(point) )
      {
      try
      {
        (*it)->DerivativeAt(point,order,value);
      }
      catch(...)
      {
        throw;
      }
    }
  } 
}

/** Return if a point is inside the object or its children */
template< unsigned int NDimensions, typename TTransform, unsigned int PipelineDimension >
bool
SpatialObject< NDimensions, TTransform, PipelineDimension>
::IsInside( const PointType &  point )
{
  typename ChildrenListType::iterator it = m_Children.begin();
  typename ChildrenListType::iterator end = m_Children.end();
  
  for(; it!=end; it++)
  {
    if( (*it)->IsInside(point) ) 
    {
    return true;
    }
  }  
  return false;
}

/** Return if the object is evaluable at a point */
template< unsigned int NDimensions, typename TTransform, unsigned int PipelineDimension >
bool
SpatialObject< NDimensions, TTransform, PipelineDimension>
::IsEvaluableAt( const PointType & point )
{
  typename ChildrenListType::iterator it = m_Children.begin();
  typename ChildrenListType::iterator end = m_Children.end();
  
  for(; it!=end; it++)
  {
    if( (*it)->IsEvaluableAt(point) ) 
    {
    return true;
    }
  }  
  return false;
}

/** Return the value of the object at a point */
template< unsigned int NDimensions, typename TTransform, unsigned int PipelineDimension >
void
SpatialObject< NDimensions, TTransform, PipelineDimension>
::ValueAt( const PointType & point, double & value )
{
  bool evaluable = false;
  typename ChildrenListType::iterator it = m_Children.begin();
  typename ChildrenListType::iterator end = m_Children.end();

  for(; it!=end; it++)
  {
    if( (*it)->IsEvaluableAt(point) )
    {
    (*it)->ValueAt(point,value); 
    evaluable = true;
    }
  } 

  if(!evaluable)
  {
    itk::ExceptionObject e("CompositeSpatialObject.txx");
    e.SetLocation("CompositeSpatialObject< NDimensions, TransformType, OutputType, PipelineDimension >::ValueAt( const PointType & )");
    e.SetDescription("This composite spatial object is not evaluable at the requested point");
    throw e;
  }
}

/** Set the parent of the object */
template< unsigned int NDimensions, typename TTransform, unsigned int PipelineDimension >  
void 
SpatialObject< NDimensions, TTransform, PipelineDimension>
::SetParent( const Superclass * parent )
{
  m_Parent = dynamic_cast<const Self*>(parent);
  RebuildAllTransformLists();
}

/** Print self */
template< unsigned int NDimensions, typename TTransform, unsigned int PipelineDimension >
void 
SpatialObject< NDimensions, TTransform, PipelineDimension>
::PrintSelf( std::ostream& os, Indent indent ) const
{
  typename TransformListType::const_iterator it;
  typename TransformListType::const_iterator end;

  Superclass::PrintSelf(os, indent);
  os << indent << "Parent: " << m_Parent.GetPointer() << std::endl << std::endl;
  os << "Bounding Box:" << std::endl;
  os << indent << m_Bounds << std::endl;
  os << "Geometric properties:" << std::endl;
  os << indent << "(local to global ) " << m_LocalToGlobalTransform << std::endl;
  os << indent << "(global to local ) " << m_GlobalToLocalTransform << std::endl;
  os << indent << "LocalToGlobalTransformList: ";
  it = m_LocalToGlobalTransformList.begin();  
  end = m_LocalToGlobalTransformList.end();
  for( ; it != end; it++ )
  {
    os<<"["<<(*it)<<"] ";
  }
  os << std::endl;
  os << indent << "GlobalToLocalTransformList size: ";
  it = m_GlobalToLocalTransformList.begin();  
  end = m_GlobalToLocalTransformList.end();
  for( ; it != end; it++ )
  {
    os<<"["<<(*it)<<"] ";
  }
  os << std::endl << std::endl;
  os << "Object properties: " << std::endl;
  os << m_Property << std::endl;
  os << indent << "Number of children: " 
     << m_Children.size() << std::endl;
  os << indent << "List of children: ";

  typename ChildrenListType::const_iterator it_children = m_Children.begin();
  typename ChildrenListType::const_iterator children_end = m_Children.end();

  for(; it_children != children_end; it_children++ )
    {
    os << "[" << (*it_children) << "] ";
    }
  os << std::endl;
}
  
/** Set the bounds of the object */
template< unsigned int NDimensions, typename TTransform, unsigned int PipelineDimension >
void
SpatialObject< NDimensions, TTransform, PipelineDimension>
::SetBounds( BoundingBoxPointer bounds )
{ 
  m_Bounds = bounds; 
}

/** Get the bounds of the object */
template< unsigned int NDimensions, typename TTransform, unsigned int PipelineDimension >
typename SpatialObject< NDimensions, TTransform, PipelineDimension>::BoundingBoxType *
SpatialObject< NDimensions, TTransform, PipelineDimension>
::GetBounds( void )
{ 
  return m_Bounds.GetPointer();
}

/** Add a child to the object */
template< unsigned int NDimensions, typename TTransform, unsigned int PipelineDimension >
void
SpatialObject< NDimensions, TTransform, PipelineDimension> 
::AddSpatialObject( Self * pointer )
{
  typename ChildrenListType::iterator it;

  it = std::find(m_Children.begin(),m_Children.end(),pointer);

  if( it == m_Children.end() )
  {
    m_Children.push_back( pointer );
    m_NDimensionalChildrenList.push_back( pointer );
    pointer->SetParent( this );
  }
  else
  { 
    //throw an exception object to let user know that he tried to add an object
    // which is already in the list of the children.
  }
}

/** Remove a child to the object */
template< unsigned int NDimensions, typename TTransform, unsigned int PipelineDimension >
void
SpatialObject< NDimensions, TTransform, PipelineDimension>
::RemoveSpatialObject( Self * pointer )
{
  typename ChildrenListType::iterator it;
  it = std::find(m_Children.begin(),m_Children.end(),pointer);

  if( it != m_Children.end() )
  {
    if( *it == pointer )
    {
    (*it)->SetParent(NULL);
    m_Children.erase( it );
    }
  }
  else
  { 
    //throw an exception object to let user know that he tried to remove an object
    // which is not in the list of the children.
  }

  // remove the child from the NDimensional list also
  typename NDimensionalChildrenListType::iterator it_NDim;
  it_NDim = std::find(m_NDimensionalChildrenList.begin(),m_NDimensionalChildrenList.end(),pointer);

  if( it_NDim != m_NDimensionalChildrenList.end() )
  {
    if( *it_NDim == pointer )
    {
    (*it_NDim)->SetParent(NULL);
    m_NDimensionalChildrenList.erase( it_NDim );
    }
  }
  else
  { 
    //throw an exception object to let user know that he tried to remove an object
    // which is not in the list of the children.
  }
}

/** Build the local to global transformation list */
template< unsigned int NDimensions, typename TTransform, unsigned int PipelineDimension >
void
SpatialObject< NDimensions, TTransform, PipelineDimension>
::BuildLocalToGlobalTransformList( TransformListType & list, bool init ) const
{
  list.push_back(m_LocalToGlobalTransform);

  if( HasParent() )
  {
    dynamic_cast<const Self*>(m_Parent.GetPointer())->BuildLocalToGlobalTransformList(list,true);
  }
}

/** Build the global to local transformation list */
template< unsigned int NDimensions, typename TTransform, unsigned int PipelineDimension >
void
SpatialObject< NDimensions, TTransform, PipelineDimension>
::BuildGlobalToLocalTransformList( TransformListType & list, bool init ) const
{
  list.push_back(m_GlobalToLocalTransform);

  if( HasParent() )
  {
    dynamic_cast<const Self*>(m_Parent.GetPointer())->BuildGlobalToLocalTransformList(list,true);
  }
}

/** Rebuild the global to local transformation list */
template< unsigned int NDimensions, typename TTransform, unsigned int PipelineDimension >
void 
SpatialObject< NDimensions, TTransform, PipelineDimension>
::RebuildLocalToGlobalTransformList( void )
{
  m_LocalToGlobalTransformList.clear();
  BuildLocalToGlobalTransformList(m_LocalToGlobalTransformList,false);

  typename ChildrenListType::const_iterator it = m_Children.begin();
  typename ChildrenListType::const_iterator end = m_Children.end();

  for(; it != end; it++ )
  {
    (*it)->RebuildLocalToGlobalTransformList();
  }
}

/** Rebuild the global to local transformation list */
template< unsigned int NDimensions, typename TTransform, unsigned int PipelineDimension >
void 
SpatialObject< NDimensions, TTransform, PipelineDimension>
::RebuildGlobalToLocalTransformList( void )
{
  m_GlobalToLocalTransformList.clear();
  BuildGlobalToLocalTransformList(m_GlobalToLocalTransformList,false);

  typename ChildrenListType::const_iterator it = m_Children.begin();
  typename ChildrenListType::const_iterator end = m_Children.end();

  for(; it != end; it++ )
  {
    (*it)->RebuildGlobalToLocalTransformList();
  }
}

/** Rebuild both Local to Global and Global to Local transformation lists */
template< unsigned int NDimensions, typename TTransform, unsigned int PipelineDimension >
void 
SpatialObject< NDimensions, TTransform, PipelineDimension>
::RebuildAllTransformLists( void )
{
  RebuildLocalToGlobalTransformList();
  RebuildGlobalToLocalTransformList();
}

/** Set the local to gloabal transformation */
template< unsigned int NDimensions, typename TTransform, unsigned int PipelineDimension >
void 
SpatialObject< NDimensions, TTransform, PipelineDimension>
::SetLocalToGlobalTransform(TransformType * transform )
{
  m_LocalToGlobalTransform = transform;
  RebuildLocalToGlobalTransformList();
}

/** Get the local to gloabal transformation */
template< unsigned int NDimensions, typename TTransform, unsigned int PipelineDimension >
const typename SpatialObject< NDimensions, TTransform, PipelineDimension>::TransformType *
SpatialObject< NDimensions, TTransform, PipelineDimension>
::GetLocalToGlobalTransform( void )
{
  return m_LocalToGlobalTransform.GetPointer();
}

/** Set the global to local transformation */
template< unsigned int NDimensions, typename TTransform, unsigned int PipelineDimension >
void 
SpatialObject< NDimensions, TTransform, PipelineDimension>
::SetGlobalToLocalTransform(TransformType * transform )
{
  m_GlobalToLocalTransform = transform;
  RebuildGlobalToLocalTransformList();
}

/** Get the global to local transformation */
template< unsigned int NDimensions, typename TTransform, unsigned int PipelineDimension >
const typename SpatialObject< NDimensions, TTransform, PipelineDimension>::TransformType *
SpatialObject< NDimensions, TTransform, PipelineDimension>
::GetGlobalToLocalTransform( void )
{
  return m_GlobalToLocalTransform.GetPointer();
}

/** Get the Local to Global transformation list */
template< unsigned int NDimensions, typename TTransform, unsigned int PipelineDimension >
typename SpatialObject< NDimensions, TTransform, PipelineDimension>::TransformListType &
SpatialObject< NDimensions, TTransform, PipelineDimension>
::GetLocalToGlobalTransformList( void )
{
  return m_LocalToGlobalTransformList;
}

/** Get the Global to Local transformation list */
template< unsigned int NDimensions, typename TTransform, unsigned int PipelineDimension >
typename SpatialObject< NDimensions, TTransform, PipelineDimension>::TransformListType &
SpatialObject< NDimensions, TTransform, PipelineDimension>
::GetGlobalToLocalTransformList( void )
{
  return m_GlobalToLocalTransformList;
}

/** Transform a point to the local coordinate frame */
template< unsigned int NDimensions, typename TTransform, unsigned int PipelineDimension >
void
SpatialObject< NDimensions, TTransform, PipelineDimension>
::TransformPointToLocalCoordinate( PointType & p ) const
{
  typename TransformListType::const_reverse_iterator it = m_GlobalToLocalTransformList.rbegin();
  typename TransformListType::const_reverse_iterator end = m_GlobalToLocalTransformList.rend();
  PointType p1,p2;
  p1 = p;
 
  for(; it!=end; it++ )
  { 
    p2 = (*it)->TransformPoint(p1);
    p1 = p2;
  }

  p = p2;
}

/** Transform a point to the global coordinate frame */
template< unsigned int NDimensions, typename TTransform, unsigned int PipelineDimension >
void
SpatialObject< NDimensions, TTransform, PipelineDimension>
::TransformPointToGlobalCoordinate( PointType & p ) const
{
  typename TransformListType::reverse_iterator it = m_LocalToGlobalTransformList->rbegin();
  typename TransformListType::reverse_iterator end = m_LocalToGlobalTransformList->rend();
  PointType p1,p2;
  p1 = p;

  for(; it!=end; it++ )
  { 
    p2 = (*it)->TransformPoint(p1);
    p1 = p2;
  }
  p = p2;
}

/** Get the modification time  */
template< unsigned int NDimensions, typename TTransform, unsigned int PipelineDimension >
unsigned long 
SpatialObject< NDimensions, TTransform, PipelineDimension>
::GetMTime( void ) const
{
  unsigned long latestTime = Object::GetMTime();
  unsigned long boundingBoxMTime = m_Bounds->GetMTime();

  if( latestTime < boundingBoxMTime )
  {
    latestTime = boundingBoxMTime;
  }

  typename ChildrenListType::const_iterator it = m_Children.begin();
  typename ChildrenListType::const_iterator end = m_Children.end();
 
  unsigned long localTime;

  for(; it!=end; it++ )
  {
    localTime = (*it)->GetMTime();

    if( localTime > latestTime )
    {
    latestTime = localTime;
    }
  } 
  return latestTime;  
}

/** Compute boundary of the object */
template< unsigned int NDimensions, typename TTransform, unsigned int PipelineDimension >
void
SpatialObject< NDimensions, TTransform, PipelineDimension>
::ComputeBounds( void )
{
  typename ChildrenListType::iterator it = m_Children.begin();
  typename ChildrenListType::iterator end = m_Children.end();
  PointType pointLow,pointHigh;
  typename BoundingBoxType::PointsContainerPointer points = BoundingBoxType::PointsContainer::New() ;

  if( this->GetMTime() > m_BoundsMTime )
  {
    unsigned int i = 0;
    for(; it!=end; it++)
    {
      typename BoundingBoxType::PointsContainerConstPointer  childrenPoints  = (*it)->GetBounds()->GetPoints();
      typename BoundingBoxType::PointsContainerConstIterator childrenPointsIt  = childrenPoints->Begin();
      typename BoundingBoxType::PointsContainerConstIterator childrenPointsEnd = childrenPoints->End();

      for(; childrenPointsIt != childrenPointsEnd; childrenPointsIt++,i++ )
      {
        points->InsertElement( i, childrenPointsIt.Value() );
      }
    }
   
    points->Modified();
    m_Bounds->SetPoints(points);
    m_Bounds->ComputeBoundingBox();
    m_BoundsMTime.Modified();
  }
}
  
/** Get the children list*/
template< unsigned int NDimensions, typename TTransform, unsigned int PipelineDimension >
typename SpatialObject< NDimensions, TTransform, PipelineDimension>::ChildrenListType &
SpatialObject< NDimensions, TTransform, PipelineDimension>
::GetChildren( void )
{
  return m_Children;
}

/** Set children list*/
template< unsigned int NDimensions, typename TTransform, unsigned int PipelineDimension >
void
SpatialObject< NDimensions, TTransform, PipelineDimension>
::SetChildren( ChildrenListType & children )
{ 
  m_Children = children;

  typename ChildrenListType::const_iterator it = m_Children.begin();
  typename ChildrenListType::const_iterator end = m_Children.end();
  
  for(; it != end; it++ )
  {
    (*it)->SetParent( this );  
  }
}

/** Get the number of children */
template< unsigned int NDimensions, typename TTransform, unsigned int PipelineDimension >
unsigned int
SpatialObject< NDimensions, TTransform, PipelineDimension>
::GetNumberOfChildren( void )
{
  return m_Children.size();
} 


} // end of namespace itk

#endif // __SpatialObject_txx

