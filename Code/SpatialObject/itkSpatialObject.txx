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
#include <algorithm>

namespace itk 
{

/** Constructor */
template< unsigned int NDimensions, unsigned int PipelineDimension >
SpatialObject< NDimensions, PipelineDimension>
::SpatialObject( void )
{
  m_ParentId=-1;
  m_Dimension = NDimensions;
  m_Bounds = BoundingBoxType::New();
  m_Property = PropertyType::New();
  m_Transform = TransformType::New();
  m_GlobalTransform = TransformType::New();
  m_TransformWithCoR = TransformType::New();
  // Initialize the spacing to 1 by default
  for (unsigned int i=0; i<ObjectDimension; i++)
  {
    m_Spacing[i] = 1;
    m_Scale[i] = 1;
    m_GlobalScale[i] = 1;
    m_CenterOfRotation[i] = 0;
  }
  
  SetParent(NULL);
}

/** Destructor */
template< unsigned int NDimensions, unsigned int PipelineDimension >
SpatialObject< NDimensions, PipelineDimension>
::~SpatialObject( void )
{
  this->Clear();
}

/** Clear the spatial object by deleting all lists of children and subchildren */
template< unsigned int NDimensions, unsigned int PipelineDimension >
void
SpatialObject< NDimensions, PipelineDimension>
::Clear(void)
{
  // Call the Clear function of every child
  typename ChildrenListType::iterator it = m_Children.begin();
  typename ChildrenListType::iterator end = m_Children.end();

  for(; it!=end; it++)
  {
    if((*it))
    {
      (*it)->Delete();
    }
  }
}


/** Set the spacing of the object */
template< unsigned int NDimensions, unsigned int PipelineDimension >
void
SpatialObject< NDimensions, PipelineDimension>
::SetSpacing(const double spacing[ObjectDimension] )
{
  unsigned int i; 
  for (i=0; i<ObjectDimension; i++)
  {
    if ( spacing[i] != m_Spacing[i] )
    {
      break;
    }
  } 
  if ( i < ObjectDimension ) 
  { 
    for (i=0; i<ObjectDimension; i++)
    {
      m_Spacing[i] = spacing[i];
    }
  }
}


/** Set the Scale of the spatial object */
template< unsigned int NDimensions, unsigned int PipelineDimension >
void
SpatialObject< NDimensions, PipelineDimension>
::SetScale(const double scale[ObjectDimension] )
{
  unsigned int i; 
  for (i=0; i<ObjectDimension; i++)
  {
    if ( scale[i] != m_Scale[i] )
    {
      break;
    }
  } 
  if ( i < ObjectDimension ) 
  { 
    for (i=0; i<ObjectDimension; i++)
    {
      m_Scale[i] = scale[i];
    }
  }
}


/** Return the Derivative at a point given the order of the derivative */
template< unsigned int NDimensions, unsigned int PipelineDimension >
void
SpatialObject< NDimensions, PipelineDimension>
::DerivativeAt( const PointType & point, short unsigned int order, OutputVectorType & value )
{
  if( !IsEvaluableAt(point) )
  {
    itk::ExceptionObject e("SpatialObject.txx");
    e.SetLocation("SpatialObject< NDimensions, PipelineDimension>::DerivateAt(\
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
template< unsigned int NDimensions, unsigned int PipelineDimension >
bool
SpatialObject< NDimensions, PipelineDimension>
::IsInside( const PointType &  point ) const
{
  typename ChildrenListType::const_iterator it = m_Children.begin();
  typename ChildrenListType::const_iterator end = m_Children.end();
  
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
template< unsigned int NDimensions, unsigned int PipelineDimension >
bool
SpatialObject< NDimensions, PipelineDimension>
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
template< unsigned int NDimensions, unsigned int PipelineDimension >
void
SpatialObject< NDimensions, PipelineDimension>
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
template< unsigned int NDimensions, unsigned int PipelineDimension >  
void 
SpatialObject< NDimensions, PipelineDimension>
::SetParent( const Superclass * parent )
{
  m_Parent = parent;
}


/** Print self */
template< unsigned int NDimensions, unsigned int PipelineDimension >
void 
SpatialObject< NDimensions, PipelineDimension>
::PrintSelf( std::ostream& os, Indent indent ) const
{
  typename TransformListType::const_iterator it;
  typename TransformListType::const_iterator end;

  Superclass::PrintSelf(os, indent);
  os << indent << "Parent: " << m_Parent << std::endl << std::endl;
  os << "Bounding Box:" << std::endl;
  os << indent << m_Bounds << std::endl;
  os << "Geometric properties:" << std::endl;
  os << indent << "(local to global ) " << m_Transform << std::endl;
  os << indent << "(global to local ) " << m_GlobalTransform << std::endl;
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
template< unsigned int NDimensions, unsigned int PipelineDimension >
void
SpatialObject< NDimensions, PipelineDimension>
::SetBounds( BoundingBoxPointer bounds )
{ 
  m_Bounds = bounds; 
}

/** Get the bounds of the object */
template< unsigned int NDimensions, unsigned int PipelineDimension >
typename SpatialObject< NDimensions, PipelineDimension>::BoundingBoxType *
SpatialObject< NDimensions, PipelineDimension>
::GetBounds( void ) const
{ 
  return m_Bounds.GetPointer();
}

/** Add a child to the object */
template< unsigned int NDimensions, unsigned int PipelineDimension >
void
SpatialObject< NDimensions, PipelineDimension> 
::AddSpatialObject( Self * pointer )
{
  pointer->Register(); // increase the reference count.

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

  this->Modified();
}

/** Remove a child to the object */
template< unsigned int NDimensions, unsigned int PipelineDimension >
void
SpatialObject< NDimensions, PipelineDimension>
::RemoveSpatialObject( Self * pointer )
{
  bool found = false;
  typename ChildrenListType::iterator it;
  it = std::find(m_Children.begin(),m_Children.end(),pointer);

  if( it != m_Children.end() )
  {
    if( *it == pointer )
    {
      (*it)->SetParent(NULL);
      m_Children.erase( it );
      found =true;
    }
    this->Modified();
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
      found = true;
    }
    this->Modified();
  }
  else
  { 
    //throw an exception object to let user know that he tried to remove an object
    // which is not in the list of the children.
  }

  if(found)
  {
    (pointer)->UnRegister();
  }

}

/** Set the local to global transformation */
template< unsigned int NDimensions, unsigned int PipelineDimension >
void 
SpatialObject< NDimensions, PipelineDimension>
::SetTransform(TransformType * transform )
{
  m_Transform = transform;
  ComputeGlobalTransform();
}

/** Compute the Global Transform */
template< unsigned int NDimensions, unsigned int PipelineDimension >
void 
SpatialObject< NDimensions, PipelineDimension>
::ComputeGlobalTransform( )
{

  TransformType::MatrixType matrix = m_Transform->GetMatrix();
  TransformType::OffsetType offset = m_Transform->GetOffset();

  // matrix is changed to include the scaling
  for(unsigned int i=0;i<3;i++)
  {
    for(unsigned int j=0;j<3;j++)
    {
      matrix.GetVnlMatrix().put(i,j,matrix.GetVnlMatrix().get(i,j)*m_Scale[i]);
    }
    m_GlobalScale[i] = m_Scale[i];
  }

  PointType point;
  point = matrix*m_CenterOfRotation;

  for(unsigned i=0;i<NDimensions;i++)
  {
    offset[i] += m_Scale[i]*m_CenterOfRotation[i]-point[i];
  }

  m_TransformWithCoR->SetMatrix(matrix);
  m_TransformWithCoR->SetOffset(offset);

  m_GlobalTransform->SetMatrix(matrix);
  m_GlobalTransform->SetOffset(offset);


  if(m_Parent)
  {
    for(unsigned int i=0;i<NDimensions;i++)
    {
      m_GlobalScale[i] *= dynamic_cast<const SpatialObject<NDimensions, PipelineDimension>*>(m_Parent)->GetGlobalScale()[i];
    }
    m_GlobalTransform->Compose(dynamic_cast<const SpatialObject<NDimensions, PipelineDimension>*>(m_Parent)->GetGlobalTransform(),false);
  }

  
  // Propagate the changes to the children
  typename ChildrenListType::iterator it = m_Children.begin();
  for(; it!=m_Children.end(); it++)
  {
    (*it)->ComputeGlobalTransform();
  }
}



/** Get the local transformation */
template< unsigned int NDimensions, unsigned int PipelineDimension >
typename SpatialObject< NDimensions, PipelineDimension>::TransformType *
SpatialObject< NDimensions, PipelineDimension>
::GetTransform( void )
{
  return m_Transform.GetPointer();
}

/** Get the local transformation (const)*/
template< unsigned int NDimensions, unsigned int PipelineDimension >
const typename SpatialObject< NDimensions, PipelineDimension>::TransformType *
SpatialObject< NDimensions, PipelineDimension>
::GetTransform( void ) const
{
  return m_Transform.GetPointer();
}


/** Set the global to local transformation */
template< unsigned int NDimensions, unsigned int PipelineDimension >
void 
SpatialObject< NDimensions, PipelineDimension>
::SetGlobalTransform(TransformType * transform )
{
  m_GlobalTransform = transform;
  ComputeTransform();
}


/** Compute the Transform when the global tranform as been set*/
template< unsigned int NDimensions, unsigned int PipelineDimension >
void 
SpatialObject< NDimensions, PipelineDimension>
::ComputeTransform( )
{
  m_Transform = m_GlobalTransform;

  if(m_Parent)
  {
    m_Transform->Compose(dynamic_cast<const SpatialObject<NDimensions, PipelineDimension>*>(m_Parent)->GetGlobalTransform()->Inverse(),true);
  }
}


/** Get the global transformation */
template< unsigned int NDimensions, unsigned int PipelineDimension >
typename SpatialObject< NDimensions, PipelineDimension>::TransformType *
SpatialObject< NDimensions, PipelineDimension>
::GetGlobalTransform( void )
{
  return m_GlobalTransform.GetPointer();
}

/** Get the global transformation (const)*/
template< unsigned int NDimensions, unsigned int PipelineDimension >
const typename SpatialObject< NDimensions, PipelineDimension>::TransformType *
SpatialObject< NDimensions, PipelineDimension>
::GetGlobalTransform( void ) const
{
  return m_GlobalTransform.GetPointer();
}

/** Get the Global to Local transformation list */
template< unsigned int NDimensions, unsigned int PipelineDimension >
typename SpatialObject< NDimensions, PipelineDimension>::TransformListType &
SpatialObject< NDimensions, PipelineDimension>
::GetGlobalTransformList( void )
{
  return m_GlobalTransformList;
}

/** Transform a point to the local coordinate frame */
template< unsigned int NDimensions, unsigned int PipelineDimension >
void
SpatialObject< NDimensions, PipelineDimension>
::TransformPointToLocalCoordinate( PointType & p ) const
{
  p = m_GlobalTransform->Inverse()->TransformPoint(p);
}

/** Transform a point to the global coordinate frame */
template< unsigned int NDimensions, unsigned int PipelineDimension >
void
SpatialObject< NDimensions, PipelineDimension>
::TransformPointToGlobalCoordinate( PointType & p ) const
{
  p = m_GlobalTransform->TransformPoint(p);
}

/** Get the modification time  */
template< unsigned int NDimensions, unsigned int PipelineDimension >
unsigned long 
SpatialObject< NDimensions, PipelineDimension>
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
template< unsigned int NDimensions, unsigned int PipelineDimension >
void
SpatialObject< NDimensions, PipelineDimension>
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
template< unsigned int NDimensions, unsigned int PipelineDimension >
typename SpatialObject< NDimensions, PipelineDimension>::ChildrenListType &
SpatialObject< NDimensions, PipelineDimension>
::GetChildren( void )
{
  return m_Children;
}

/** Set children list*/
template< unsigned int NDimensions, unsigned int PipelineDimension >
void
SpatialObject< NDimensions, PipelineDimension>
::SetChildren( ChildrenListType & children )
{ 
  m_Children = children;

  typename ChildrenListType::const_iterator it = m_Children.begin();
  typename ChildrenListType::const_iterator end = m_Children.end();
  
  for(; it != end; it++ )
  {
    (*it)->Register(); // increase the reference count
    m_NDimensionalChildrenList.push_back(*it);
    (*it)->SetParent( this );  
  }
}

/** Get the number of children */
template< unsigned int NDimensions, unsigned int PipelineDimension >
unsigned int
SpatialObject< NDimensions, PipelineDimension>
::GetNumberOfChildren( void )
{
  return m_Children.size();
} 

/** Return the Modified time of the LocalToGlobalTransform */
template< unsigned int NDimensions, unsigned int PipelineDimension >
unsigned long
SpatialObject< NDimensions, PipelineDimension>
::GetTransformMTime(void)
{
  return m_Transform->GetMTime();
}

/** Return the Modified time of the GlobalToLocalTransform */
template< unsigned int NDimensions, unsigned int PipelineDimension >
unsigned long
SpatialObject< NDimensions, PipelineDimension>
::GetGlobalTransformMTime(void)
{
  return m_GlobalTransform->GetMTime();
}

} // end of namespace itk

#endif // __SpatialObject_txx

