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
#include <itkNumericTraits.h>
#include <algorithm>
#include <string>

namespace itk 
{

/** Constructor */
template< unsigned int NDimensions, unsigned int SpaceDimension >
SpatialObject< NDimensions, SpaceDimension>
::SpatialObject( void )
{
  m_ParentId=-1;
  m_Dimension = NDimensions;
  m_Bounds = BoundingBoxType::New();
  m_BoundsMTime = 0;
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
template< unsigned int NDimensions, unsigned int SpaceDimension >
SpatialObject< NDimensions, SpaceDimension>
::~SpatialObject( void )
{
  this->Clear();
}

/** Clear the spatial object by deleting all lists of children and subchildren */
template< unsigned int NDimensions, unsigned int SpaceDimension >
void
SpatialObject< NDimensions, SpaceDimension>
::Clear(void)
{
  // Call the Clear function of every child
  typename ChildrenListType::iterator it = m_Children.begin();
  typename ChildrenListType::iterator end = m_Children.end();

  while(it!=end)
  {
    if((*it))
    {
      (*it)->Delete();
    }
    it++;
  }
}


/** Set the spacing of the object */
template< unsigned int NDimensions, unsigned int SpaceDimension >
void
SpatialObject< NDimensions, SpaceDimension>
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
template< unsigned int NDimensions, unsigned int SpaceDimension >
void
SpatialObject< NDimensions, SpaceDimension>
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
template< unsigned int NDimensions, unsigned int SpaceDimension >
void
SpatialObject< NDimensions, SpaceDimension>
::DerivativeAt( const PointType & point, short unsigned int order,
                OutputVectorType & value, unsigned int depth, char * name )
  {
  if( !IsEvaluableAt(point, depth, name) )
    {
    itk::ExceptionObject e("SpatialObject.txx");
    e.SetLocation("SpatialObject< NDimensions, SpaceDimension>::DerivateAt(\
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

    for( unsigned short i=0; i<NDimensions; i++, it++, it_v1++, it_v2++ )
      {
      p1=point;
      p2=point;
      p1[i]-=m_Spacing[i];
      p2[i]+=m_Spacing[i];

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
template< unsigned int NDimensions, unsigned int SpaceDimension >
bool
SpatialObject< NDimensions, SpaceDimension>
::IsInside( const PointType &  point, unsigned int depth, char * name) const
{
  if( depth > 0 )
    {
    typename ChildrenListType::const_iterator it = m_Children.begin();
    typename ChildrenListType::const_iterator end = m_Children.end();
    
    while(it!=end)
      {
      if( (*it)->IsInside(point, depth-1, name) ) 
        {
        return true;
        }
      it++;
      }  
    }

  return false;
}

/** Return if the object is evaluable at a point */
template< unsigned int NDimensions, unsigned int SpaceDimension >
bool
SpatialObject< NDimensions, SpaceDimension>
::IsEvaluableAt( const PointType & point, unsigned int depth, char * name )
{
  if( depth > 0 )
    {
    typename ChildrenListType::iterator it = m_Children.begin();
    typename ChildrenListType::iterator end = m_Children.end();
    
    while(it!=end)
      {
      if( (*it)->IsEvaluableAt(point, depth-1, name) ) 
        {
        return true;
        }
      it++;
      }  
    }

  return false;
}

/** Return the value of the object at a point */
template< unsigned int NDimensions, unsigned int SpaceDimension >
void
SpatialObject< NDimensions, SpaceDimension>
::ValueAt( const PointType & point, double & value, unsigned int depth,
           char * name )
{
  bool evaluable = false;
  if( depth > 0 )
    {
    typename ChildrenListType::iterator it = m_Children.begin();
    typename ChildrenListType::iterator end = m_Children.end();
  
    while(it!=end)
      {
      if( (*it)->IsEvaluableAt(point, depth-1, name) )
        {
        (*it)->ValueAt(point,value, depth-1, name); 
        evaluable = true;
        break;
        }
      it++;
      } 
    }

  if(!evaluable)
    {
    itk::ExceptionObject e("SpatialObject.txx");
    e.SetLocation("SpatialObject<>::ValueAt( const PointType & )");
    e.SetDescription("This spatial object is not evaluable at the point");
    throw e;
    }
}

/** Set the parent of the object */
template< unsigned int NDimensions, unsigned int SpaceDimension >  
void 
SpatialObject< NDimensions, SpaceDimension>
::SetParent( const Superclass * parent )
{
  m_Parent = parent;
}


/** Print self */
template< unsigned int NDimensions, unsigned int SpaceDimension >
void 
SpatialObject< NDimensions, SpaceDimension>
::PrintSelf( std::ostream& os, Indent indent ) const
{
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

  while(it_children != children_end)
    {
    os << "[" << (*it_children) << "] ";
    it_children++;
    }
  os << std::endl;
}
  
/** Set the bounds of the object */
template< unsigned int NDimensions, unsigned int SpaceDimension >
void
SpatialObject< NDimensions, SpaceDimension>
::SetBoundingBox( BoundingBoxPointer bounds )
{ 
  m_Bounds = bounds; 
}

/** Get the bounds of the object */
template< unsigned int NDimensions, unsigned int SpaceDimension >
typename SpatialObject< NDimensions, SpaceDimension>::BoundingBoxType *
SpatialObject< NDimensions, SpaceDimension>
::GetBoundingBox( void ) const
{ 
  return m_Bounds.GetPointer();
}

/** Add a child to the object */
template< unsigned int NDimensions, unsigned int SpaceDimension >
void
SpatialObject< NDimensions, SpaceDimension> 
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
template< unsigned int NDimensions, unsigned int SpaceDimension >
void
SpatialObject< NDimensions, SpaceDimension>
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
  it_NDim = std::find(m_NDimensionalChildrenList.begin(),
                      m_NDimensionalChildrenList.end(), pointer);

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
template< unsigned int NDimensions, unsigned int SpaceDimension >
void 
SpatialObject< NDimensions, SpaceDimension>
::SetTransform(TransformType * transform )
{
  m_Transform = transform;
  ComputeGlobalTransform();
}

/** Compute the Global Transform */
template< unsigned int NDimensions, unsigned int SpaceDimension >
void 
SpatialObject< NDimensions, SpaceDimension>
::ComputeGlobalTransform( )
{

  typename TransformType::MatrixType matrix = m_Transform->GetMatrix();
  typename TransformType::OffsetType offset = m_Transform->GetOffset();

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
      m_GlobalScale[i] *= dynamic_cast<const SpatialObject<NDimensions, SpaceDimension>*>(m_Parent)->GetGlobalScale()[i];
    }
    m_GlobalTransform->Compose(dynamic_cast<const SpatialObject<NDimensions, SpaceDimension>*>(m_Parent)->GetGlobalTransform(),false);
  }

  
  // Propagate the changes to the children
  typename ChildrenListType::iterator it = m_Children.begin();
  while(it!=m_Children.end())
  {
    (*it)->ComputeGlobalTransform();
    it++;
  }
}



/** Get the local transformation */
template< unsigned int NDimensions, unsigned int SpaceDimension >
typename SpatialObject< NDimensions, SpaceDimension>::TransformType *
SpatialObject< NDimensions, SpaceDimension>
::GetTransform( void )
{
  return m_Transform.GetPointer();
}

/** Get the local transformation (const)*/
template< unsigned int NDimensions, unsigned int SpaceDimension >
const typename SpatialObject< NDimensions, SpaceDimension>::TransformType *
SpatialObject< NDimensions, SpaceDimension>
::GetTransform( void ) const
{
  return m_Transform.GetPointer();
}


/** Set the global to local transformation */
template< unsigned int NDimensions, unsigned int SpaceDimension >
void 
SpatialObject< NDimensions, SpaceDimension>
::SetGlobalTransform(TransformType * transform )
{
  m_GlobalTransform = transform;
  ComputeTransform();
}


/** Compute the Transform when the global tranform as been set*/
template< unsigned int NDimensions, unsigned int SpaceDimension >
void 
SpatialObject< NDimensions, SpaceDimension>
::ComputeTransform( )
{
  m_Transform = m_GlobalTransform;

  if(m_Parent)
  {
    m_Transform->Compose(dynamic_cast<const SpatialObject<NDimensions, SpaceDimension>*>(m_Parent)->GetGlobalTransform()->Inverse(),true);
  }
}


/** Get the global transformation */
template< unsigned int NDimensions, unsigned int SpaceDimension >
typename SpatialObject< NDimensions, SpaceDimension>::TransformType *
SpatialObject< NDimensions, SpaceDimension>
::GetGlobalTransform( void )
{
  return m_GlobalTransform.GetPointer();
}

/** Get the global transformation (const)*/
template< unsigned int NDimensions, unsigned int SpaceDimension >
const typename SpatialObject< NDimensions, SpaceDimension>::TransformType *
SpatialObject< NDimensions, SpaceDimension>
::GetGlobalTransform( void ) const
{
  return m_GlobalTransform.GetPointer();
}

/** Get the Global to Local transformation list */
template< unsigned int NDimensions, unsigned int SpaceDimension >
typename SpatialObject< NDimensions, SpaceDimension>::TransformListType &
SpatialObject< NDimensions, SpaceDimension>
::GetGlobalTransformList( void )
{
  return m_GlobalTransformList;
}

/** Transform a point to the local coordinate frame */
template< unsigned int NDimensions, unsigned int SpaceDimension >
void
SpatialObject< NDimensions, SpaceDimension>
::TransformPointToLocalCoordinate( PointType & p ) const
{
  p = m_GlobalTransform->Inverse()->TransformPoint(p);
}

/** Transform a point to the global coordinate frame */
template< unsigned int NDimensions, unsigned int SpaceDimension >
void
SpatialObject< NDimensions, SpaceDimension>
::TransformPointToGlobalCoordinate( PointType & p ) const
{
  p = m_GlobalTransform->TransformPoint(p);
}

/** Get the modification time  */
template< unsigned int NDimensions, unsigned int SpaceDimension >
unsigned long 
SpatialObject< NDimensions, SpaceDimension>
::GetMTime( void ) const
{
  unsigned long latestTime = Object::GetMTime();

  if( latestTime < m_BoundsMTime )
  {
    latestTime = m_BoundsMTime;
  }

  typename ChildrenListType::const_iterator it = m_Children.begin();
  typename ChildrenListType::const_iterator end = m_Children.end();
 
  unsigned long localTime;

  while(it!=end)
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

/** Compute boundary of the object */
template< unsigned int NDimensions, unsigned int SpaceDimension >
bool
SpatialObject< NDimensions, SpaceDimension>
::ComputeBoundingBox( unsigned int depth, char * name )
  {
  itkDebugMacro( "Computing Bounding Box" );

  if( this->GetMTime() > m_BoundsMTime )
    {
    if( depth > 0 )
      {
      typename ChildrenListType::iterator it = m_Children.begin();
      typename ChildrenListType::iterator end = m_Children.end();
      if(it != end)
        {
        (*it)->ComputeBoundingBox(depth-1, name);
        m_Bounds->SetMinimum((*it)->GetBoundingBox()->GetMinimum());
        m_Bounds->SetMaximum((*it)->GetBoundingBox()->GetMaximum());
        it++;

        while(it!=end)
          {
          (*it)->ComputeBoundingBox(depth-1, name);
          m_Bounds->ConsiderPoint((*it)->GetBoundingBox()->GetMinimum());
          m_Bounds->ConsiderPoint((*it)->GetBoundingBox()->GetMaximum());
          it++;
          }
        m_BoundsMTime = this->GetMTime();
        return true;
        }
      }

    typename BoundingBoxType::PointType pnt;
    pnt.Fill( itk::NumericTraits< ITK_TYPENAME 
              BoundingBoxType::PointType::ValueType>::Zero );
    m_Bounds->SetMinimum(pnt);
    m_Bounds->SetMaximum(pnt);
    m_BoundsMTime = this->GetMTime();
    return false;
    }
  else
    {
    typename BoundingBoxType::PointType pnt;
    pnt.Fill( itk::NumericTraits< ITK_TYPENAME 
              BoundingBoxType::PointType::ValueType>::Zero );
    if(m_Bounds->GetMinimum() == pnt &&
       m_Bounds->GetMaximum() == pnt)
      {
      return false;
      }
    else
      {
      return true;
      }
    }
  }
  
/** Get the children list.
 * User is responsible for freeing the list, but not the elements of
 * the list. */
template< unsigned int NDimensions, unsigned int SpaceDimension >
typename SpatialObject< NDimensions, SpaceDimension>::ChildrenListType *
SpatialObject< NDimensions, SpaceDimension>
::GetChildren( unsigned int depth, 
               char * name)
{
  ChildrenListType * children = new ChildrenListType;

  typename ChildrenListType::const_iterator childrenListIt = 
           m_Children.begin();
  typename ChildrenListType::const_iterator childrenListEnd = 
           m_Children.end();

  while( childrenListIt != childrenListEnd )
    {
    if( name == NULL || strstr(typeid(**childrenListIt).name(), name) )
      {
      children->push_back(*childrenListIt);
      }
    if( depth > 0 )
      {
      children->merge(*((**childrenListIt).GetChildren(depth-1, name)));
      }
    childrenListIt++;
    }

  return children;
}

/** Set children list*/
template< unsigned int NDimensions, unsigned int SpaceDimension >
void
SpatialObject< NDimensions, SpaceDimension>
::SetChildren( ChildrenListType & children )
{ 
  m_Children = children;

  typename ChildrenListType::const_iterator it = m_Children.begin();
  typename ChildrenListType::const_iterator end = m_Children.end();
  
  while(it != end)
  {
    (*it)->Register(); // increase the reference count
    m_NDimensionalChildrenList.push_back(*it);
    (*it)->SetParent( this );  
    it++;
  }
}

/** Get the number of children */
template< unsigned int NDimensions, unsigned int SpaceDimension >
unsigned int
SpatialObject< NDimensions, SpaceDimension>
::GetNumberOfChildren( unsigned int depth, char * name )
{
  unsigned int cnt = m_Children.size();

  if( depth > 0 )
    {
    typename ChildrenListType::const_iterator it = m_Children.begin();
    typename ChildrenListType::const_iterator end = m_Children.end();
    
    while(it != end)
      {
      cnt += (*it)->GetNumberOfChildren( depth-1, name );
      it++;
      }
    }

  return cnt;
} 

/** Return the Modified time of the LocalToGlobalTransform */
template< unsigned int NDimensions, unsigned int SpaceDimension >
unsigned long
SpatialObject< NDimensions, SpaceDimension>
::GetTransformMTime(void)
{
  return m_Transform->GetMTime();
}

/** Return the Modified time of the GlobalToLocalTransform */
template< unsigned int NDimensions, unsigned int SpaceDimension >
unsigned long
SpatialObject< NDimensions, SpaceDimension>
::GetGlobalTransformMTime(void)
{
  return m_GlobalTransform->GetMTime();
}

} // end of namespace itk

#endif // __SpatialObject_txx

