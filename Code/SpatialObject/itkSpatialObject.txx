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
  m_TypeName = "SpatialObject";
  m_Dimension = TDimension;
  m_Bounds = BoundingBoxType::New();
  m_BoundsMTime = 0;
  m_Property = PropertyType::New();

  m_IndexToObjectTransform = TransformType::New();
  m_IndexToObjectTransform->SetIdentity();
  m_ObjectToParentTransform = TransformType::New();
  m_ObjectToParentTransform->SetIdentity();
  m_ObjectToWorldTransform = TransformType::New();
  m_ObjectToWorldTransform->SetIdentity();
  m_IndexToWorldTransform = TransformType::New();
  m_IndexToWorldTransform->SetIdentity();
  m_WorldToIndexTransform = TransformType::New();
  m_WorldToIndexTransform->SetIdentity();

  m_BoundingBoxChildrenDepth=0;
  SetParent(NULL);
  m_Id = -1;
  m_ParentId = -1;
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
      const double* spacing = m_IndexToObjectTransform->GetScaleComponent();
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
  os << indent << "Index to Object Transform: " << m_IndexToObjectTransform << std::endl;
  os << indent << "Object to Parent Transform: " << m_ObjectToParentTransform << std::endl;
  os << indent << "Object to World Transform: " << m_ObjectToWorldTransform << std::endl;
  os << indent << "Index to World Transform: " << m_IndexToWorldTransform << std::endl;
  os << indent << "World to Index Transform: " << m_WorldToIndexTransform << std::endl;
  os << std::endl << std::endl;
  os << indent << "Bounding Box Children Depth: " << m_BoundingBoxChildrenDepth << std::endl;
  os << indent << "Bounding Box Children Name: " << m_BoundingBoxChildrenName << std::endl;
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
  
/** Get the bounds of the object */
template< unsigned int TDimension >
typename SpatialObject< TDimension >::BoundingBoxType *
SpatialObject< TDimension >
::GetBoundingBox() const
{ 
  this->ComputeBoundingBox();
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
::SetObjectToParentTransform(TransformType * transform )
{
  m_ObjectToParentTransform = transform;
  ComputeObjectToWorldTransform();
}

/** Compute the Global Transform */
template< unsigned int TDimension >
void 
SpatialObject< TDimension >
::ComputeObjectToWorldTransform( )
{
  typename TransformType::MatrixType matrix  = m_ObjectToParentTransform->GetMatrix();
  typename TransformType::OffsetType offset  = m_ObjectToParentTransform->GetOffset();

  m_ObjectToWorldTransform->SetMatrix(matrix);
  m_ObjectToWorldTransform->SetOffset(offset);

  m_IndexToWorldTransform->SetMatrix(m_IndexToObjectTransform->GetMatrix());
  m_IndexToWorldTransform->SetOffset(m_IndexToObjectTransform->GetOffset());


  if(m_Parent)
    {
    m_ObjectToWorldTransform->Compose(dynamic_cast<const SpatialObject<TDimension>*>
                               (m_Parent)->GetObjectToWorldTransform(),false);
    }

  m_IndexToWorldTransform->Compose(this->GetObjectToWorldTransform(),false);

  m_WorldToIndexTransform->SetMatrix(
                              m_IndexToWorldTransform->Inverse()->GetMatrix());
  m_WorldToIndexTransform->SetOffset(
                              m_IndexToWorldTransform->Inverse()->GetOffset());
  
  // Propagate the changes to the children
  typename ChildrenListType::iterator it = m_Children.begin();
  while(it!=m_Children.end())
  {
    (*it)->ComputeObjectToWorldTransform();
    it++;
  }
}

/** Get the local transformation */
template< unsigned int TDimension >
typename SpatialObject< TDimension >::TransformType *
SpatialObject< TDimension >
::GetObjectToParentTransform( void )
{
  return m_ObjectToParentTransform.GetPointer();
}

/** Get the local transformation (const)*/
template< unsigned int TDimension >
const typename SpatialObject< TDimension >::TransformType *
SpatialObject< TDimension >
::GetObjectToParentTransform( void ) const
{
  return m_ObjectToParentTransform.GetPointer();
}

/** Get the local transformation */
template< unsigned int TDimension >
typename SpatialObject< TDimension >::TransformType *
SpatialObject< TDimension >
::GetIndexToObjectTransform( void )
{
  return m_IndexToObjectTransform.GetPointer();
}

/** Get the local transformation (const)*/
template< unsigned int TDimension >
const typename SpatialObject< TDimension >::TransformType *
SpatialObject< TDimension >
::GetIndexToObjectTransform( void ) const
{
  return m_IndexToObjectTransform.GetPointer();
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

  if(m_Parent)
    {
    m_ObjectToParentTransform->Compose(dynamic_cast<const SpatialObject<TDimension>*>
                         (m_Parent)->GetObjectToWorldTransform()->Inverse(),true);
    }


  m_IndexToWorldTransform->SetMatrix(m_IndexToObjectTransform->GetMatrix());
  m_IndexToWorldTransform->SetOffset(m_IndexToObjectTransform->GetOffset());
  m_IndexToWorldTransform->Compose(m_ObjectToWorldTransform,false);

  m_WorldToIndexTransform->SetMatrix(
                              m_IndexToWorldTransform->Inverse()->GetMatrix());
  m_WorldToIndexTransform->SetOffset(
                              m_IndexToWorldTransform->Inverse()->GetOffset());

}


/** Get the global transformation */
template< unsigned int TDimension >
typename SpatialObject< TDimension >::TransformType *
SpatialObject< TDimension >
::GetObjectToWorldTransform( void )
{
  return m_ObjectToWorldTransform.GetPointer();
}

/** Get the global transformation (const)*/
template< unsigned int TDimension >
const typename SpatialObject< TDimension >::TransformType *
SpatialObject< TDimension >
::GetObjectToWorldTransform( void ) const
{
  return m_ObjectToWorldTransform.GetPointer();
}

/** Get the global transformation */
template< unsigned int TDimension >
typename SpatialObject< TDimension >::TransformType *
SpatialObject< TDimension >
::GetIndexToWorldTransform( void )
{
  return m_IndexToWorldTransform.GetPointer();
}

/** Get the global transformation (const)*/
template< unsigned int TDimension >
const typename SpatialObject< TDimension >::TransformType *
SpatialObject< TDimension >
::GetIndexToWorldTransform( void ) const
{
  return m_IndexToWorldTransform.GetPointer();
}

/** Get the global transformation */
template< unsigned int TDimension >
typename SpatialObject< TDimension >::TransformType *
SpatialObject< TDimension >
::GetWorldToIndexTransform( void )
{
  return m_WorldToIndexTransform.GetPointer();
}

/** Get the global transformation (const)*/
template< unsigned int TDimension >
const typename SpatialObject< TDimension >::TransformType *
SpatialObject< TDimension >
::GetWorldToIndexTransform( void ) const
{
  return m_WorldToIndexTransform.GetPointer();
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
::ComputeBoundingBox() const
  {
  itkDebugMacro( "Computing Bounding Box" );

  if( this->GetMTime() > m_BoundsMTime )
    {
    if( m_BoundingBoxChildrenDepth > 0 )
      {
      typename ChildrenListType::const_iterator it = m_Children.begin();
      typename ChildrenListType::const_iterator itEnd = m_Children.end();
      if(it != itEnd)
        {
        (*it)->SetBoundingBoxChildrenDepth(m_BoundingBoxChildrenDepth-1);
        (*it)->SetBoundingBoxChildrenName(m_BoundingBoxChildrenName);
        (*it)->ComputeBoundingBox();
        m_Bounds->SetMinimum((*it)->GetBoundingBox()->GetMinimum());
        m_Bounds->SetMaximum((*it)->GetBoundingBox()->GetMaximum());
        it++;

        while(it!=itEnd)
          {
          (*it)->SetBoundingBoxChildrenDepth(m_BoundingBoxChildrenDepth-1);
          (*it)->SetBoundingBoxChildrenName(m_BoundingBoxChildrenName);
          (*it)->ComputeBoundingBox();
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
::GetNumberOfChildren( unsigned int depth, char * name ) const
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

