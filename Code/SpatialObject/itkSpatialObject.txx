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
template< unsigned int TDimension >
SpatialObject< TDimension >
::SpatialObject( void )
{
  strcpy(m_TypeName,"SpatialObject");
  m_Dimension = TDimension;
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
  m_Id = 0;
  m_ParentId=-1; // by default
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
  // Call the Clear function of every child
  typename ChildrenListType::iterator it = m_Children.begin();
  typename ChildrenListType::iterator itEnd = m_Children.end();

  while(it!=itEnd)
  {
    if((*it))
    {
      (*it)->Delete();
    }
    it++;
  }
  m_Children.clear();
}


/** Set the spacing of the object */
template< unsigned int TDimension >
void
SpatialObject< TDimension >
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
template< unsigned int TDimension >
void
SpatialObject< TDimension >
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
template< unsigned int TDimension >
bool
SpatialObject< TDimension >
::IsInside( const PointType &  point, unsigned int depth, char * name) const
{
  if( depth > 0 )
    {
    typename ChildrenListType::const_iterator it = m_Children.begin();
    typename ChildrenListType::const_iterator itEnd = m_Children.end();
    
    while(it!=itEnd)
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
template< unsigned int TDimension >
bool
SpatialObject< TDimension >
::IsEvaluableAt( const PointType & point, unsigned int depth, char * name ) const
{
  if( depth > 0 )
    {
    typename ChildrenListType::const_iterator it = m_Children.begin();
    typename ChildrenListType::const_iterator itEnd = m_Children.end();
    
    while(it!=itEnd)
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
template< unsigned int TDimension >
bool
SpatialObject< TDimension >
::ValueAt( const PointType & point, double & value, unsigned int depth,
           char * name ) const
{  
  bool evaluable = false;
  if( depth > 0 )
    {
    typename ChildrenListType::const_iterator it = m_Children.begin();
    typename ChildrenListType::const_iterator itEnd = m_Children.end();
  
    while(it!=itEnd)
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
     << static_cast<unsigned long>( m_Children.size() )<< std::endl;
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
template< unsigned int TDimension >
void
SpatialObject< TDimension >
::SetBoundingBox( BoundingBoxPointer bounds )
{ 
  m_Bounds = bounds; 
}

/** Get the bounds of the object */
template< unsigned int TDimension >
typename SpatialObject< TDimension >::BoundingBoxType *
SpatialObject< TDimension >
::GetBoundingBox( void ) const
{ 
  return m_Bounds.GetPointer();
}

/** Add a child to the object */
template< unsigned int TDimension >
void
SpatialObject< TDimension > 
::AddSpatialObject( Self * pointer )
{
  pointer->Register(); // increase the reference count.

  typename ChildrenListType::iterator it;

  it = std::find(m_Children.begin(),m_Children.end(),pointer);

  if( it == m_Children.end() )
  {
    pointer->SetParent( this );
    pointer->SetParentId( this->GetId() );
    m_Children.push_back( pointer );
  }
  else
  { 
    //throw an exception object to let user know that he tried to add an object
    // which is already in the list of the children.
  }

  this->Modified();
}

/** Remove a child to the object */
template< unsigned int TDimension >
void
SpatialObject< TDimension >
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
      (*it)->SetParentId(-1);
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

  if(found)
  {
    (pointer)->UnRegister();
  }

}

/** Set the local to global transformation */
template< unsigned int TDimension >
void 
SpatialObject< TDimension >
::SetTransform(TransformType * transform )
{
  m_Transform = transform;
  ComputeGlobalTransform();
}

/** Compute the Global Transform */
template< unsigned int TDimension >
void 
SpatialObject< TDimension >
::ComputeGlobalTransform( )
{

  typename TransformType::MatrixType matrix = m_Transform->GetMatrix();
  typename TransformType::OffsetType offset = m_Transform->GetOffset();

  // matrix is changed to include the scaling
  for(unsigned int i=0;i<TDimension;i++)
  {
    for(unsigned int j=0;j<TDimension;j++)
    {
      matrix.GetVnlMatrix().put(i,j,matrix.GetVnlMatrix().get(i,j)*m_Scale[i]);
    }
    m_GlobalScale[i] = m_Scale[i];
  }

  PointType point;
  point = matrix*m_CenterOfRotation;

  for(unsigned i=0;i<TDimension;i++)
  {
    offset[i] += m_Scale[i]*m_CenterOfRotation[i]-point[i];
  }

  m_TransformWithCoR->SetMatrix(matrix);
  m_TransformWithCoR->SetOffset(offset);

  m_GlobalTransform->SetMatrix(matrix);
  m_GlobalTransform->SetOffset(offset);


  if(m_Parent)
  {
    for(unsigned int i=0;i<TDimension;i++)
    {
      m_GlobalScale[i] *= dynamic_cast<const SpatialObject<TDimension>*>(m_Parent)->GetGlobalScale()[i];
    }
    m_GlobalTransform->Compose(dynamic_cast<const SpatialObject<TDimension>*>(m_Parent)->GetGlobalTransform(),false);
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
template< unsigned int TDimension >
typename SpatialObject< TDimension >::TransformType *
SpatialObject< TDimension >
::GetTransform( void )
{
  return m_Transform.GetPointer();
}

/** Get the local transformation (const)*/
template< unsigned int TDimension >
const typename SpatialObject< TDimension >::TransformType *
SpatialObject< TDimension >
::GetTransform( void ) const
{
  return m_Transform.GetPointer();
}


/** Set the global to local transformation */
template< unsigned int TDimension >
void 
SpatialObject< TDimension >
::SetGlobalTransform(TransformType * transform )
{
  m_GlobalTransform = transform;
  ComputeTransform();
}


/** Compute the Transform when the global tranform as been set*/
template< unsigned int TDimension >
void 
SpatialObject< TDimension >
::ComputeTransform( )
{
  m_Transform = m_GlobalTransform;

  if(m_Parent)
  {
    m_Transform->Compose(dynamic_cast<const SpatialObject<TDimension>*>(m_Parent)->GetGlobalTransform()->Inverse(),true);
  }
}


/** Get the global transformation */
template< unsigned int TDimension >
typename SpatialObject< TDimension >::TransformType *
SpatialObject< TDimension >
::GetGlobalTransform( void )
{
  return m_GlobalTransform.GetPointer();
}

/** Get the global transformation (const)*/
template< unsigned int TDimension >
const typename SpatialObject< TDimension >::TransformType *
SpatialObject< TDimension >
::GetGlobalTransform( void ) const
{
  return m_GlobalTransform.GetPointer();
}

/** Get the Global to Local transformation list */
template< unsigned int TDimension >
typename SpatialObject< TDimension >::TransformListType &
SpatialObject< TDimension >
::GetGlobalTransformList( void )
{
  return m_GlobalTransformList;
}

/** Transform a point to the local coordinate frame */
template< unsigned int TDimension >
void
SpatialObject< TDimension >
::TransformPointToLocalCoordinate( PointType & p ) const
{
  p = m_GlobalTransform->Inverse()->TransformPoint(p);
}

/** Transform a point to the global coordinate frame */
template< unsigned int TDimension >
void
SpatialObject< TDimension >
::TransformPointToGlobalCoordinate( PointType & p ) const
{
  p = m_GlobalTransform->TransformPoint(p);
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

  typename ChildrenListType::const_iterator it = m_Children.begin();
  typename ChildrenListType::const_iterator itEnd = m_Children.end();
 
  unsigned long localTime;

  while(it!=itEnd)
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

/** 
 * Compute an axis-aligned bounding box for an object and its selected
 * children, down to a specified depth.  After computation, the
 * resulting bounding box is stored in this->m_Bounds.  Once this
 * function is called with a specific value of \p depth and \p name,
 * future calls, irrespective of the parameters, will leave the
 * bounding box unchanged until the spatial object is modified and the
 * modification time updated.
 *
 * \param depth Include children down to this depth.  If \p depth = 0,
 * include only the object itself.
 * \param name Include only objects whose type string contains \p
 * name.  
 * \return \c true if, after the function completes, the bounding box
 * reflects object information, and \c false if the bounding box is
 * still in an initial state.  The return value is mainly used by recursive
 * calls of this function.
 */
template< unsigned int TDimension >
bool
SpatialObject< TDimension >
::ComputeBoundingBox( unsigned int depth, char * name )
  {
  itkDebugMacro( "Computing Bounding Box" );

  if( this->GetMTime() > m_BoundsMTime )
    {
    if( depth > 0 )
      {
      typename ChildrenListType::iterator it = m_Children.begin();
      typename ChildrenListType::iterator itEnd = m_Children.end();
      if(it != itEnd)
        {
        (*it)->ComputeBoundingBox(depth-1, name);
        m_Bounds->SetMinimum((*it)->GetBoundingBox()->GetMinimum());
        m_Bounds->SetMaximum((*it)->GetBoundingBox()->GetMaximum());
        it++;

        while(it!=itEnd)
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
template< unsigned int TDimension >
typename SpatialObject< TDimension >::ChildrenListType *
SpatialObject< TDimension >
::GetChildren( unsigned int depth, 
               char * name) const
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
template< unsigned int TDimension >
void
SpatialObject< TDimension >
::SetChildren( ChildrenListType & children )
{ 
  m_Children = children;

  typename ChildrenListType::const_iterator it = m_Children.begin();
  typename ChildrenListType::const_iterator itEnd = m_Children.end();
  
  while(it != itEnd)
  {
    (*it)->Register(); // increase the reference count
    (*it)->SetParent( this );  
    it++;
  }
}

/** Get the number of children */
template< unsigned int TDimension >
unsigned int
SpatialObject< TDimension >
::GetNumberOfChildren( unsigned int depth, char * name )
{
  typename ChildrenListType::const_iterator it = m_Children.begin();
  typename ChildrenListType::const_iterator itEnd = m_Children.end();

  unsigned int cnt = 0;
  while(it != itEnd)
    {
    if(name == NULL || strstr(typeid(**it).name(), name))
      {
      cnt++;
      }
    it++;
    }

  it = m_Children.begin();
  itEnd = m_Children.end();
  if( depth > 0 )
    {
    while(it != itEnd)
      {
      cnt += (*it)->GetNumberOfChildren( depth-1, name );
      it++;
      }
    }

  return cnt;
} 

/** Return the Modified time of the LocalToGlobalTransform */
template< unsigned int TDimension >
unsigned long
SpatialObject< TDimension >
::GetTransformMTime(void)
{
  return m_Transform->GetMTime();
}

/** Return the Modified time of the GlobalToLocalTransform */
template< unsigned int TDimension >
unsigned long
SpatialObject< TDimension >
::GetGlobalTransformMTime(void)
{
  return m_GlobalTransform->GetMTime();
}




/** Get the parent of the spatial object */
template< unsigned int TDimension >
const SpatialObject< TDimension > *
SpatialObject< TDimension >
::GetParent( void ) const
{
  return m_Parent;
}

/** Set the parent of the spatial object */
template< unsigned int TDimension >
void
SpatialObject< TDimension >
::SetParent( const Self * parent )
{
  m_Parent = parent;
}

/** Return true if the spatial object has a parent */
template< unsigned int TDimension >
bool
SpatialObject< TDimension >
::HasParent( void ) const
{
  if( m_Parent )
  {
    return true;
  }
  else
  {
    return false;
  }
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
  if ( ! m_RequestedRegionInitialized)
  {
    this->SetRequestedRegionToLargestPossibleRegion();
    m_RequestedRegionInitialized = true;
  }
  
  m_LastRequestedRegionWasOutsideOfTheBufferedRegion = 0;
}

template< unsigned int TDimension >
void
SpatialObject< TDimension >
::SetRequestedRegionToLargestPossibleRegion()
{
  m_RequestedRegion = m_LargestPossibleRegion;
  m_RequestedRegionInitialized = true;
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
    m_RequestedRegionInitialized = true;
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
    m_RequestedRegionInitialized = true;
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











} // end of namespace itk

#endif // __SpatialObject_txx

