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
  template< unsigned int NDimensions, class TransformType, class OutputType >
  unsigned int 
  SpatialObject< NDimensions, TransformType, OutputType >
  ::GetDimension( void )
  { 
    return NDimensions; 
  }

  template< unsigned int NDimensions, class TransformType, class OutputType >
  void
  SpatialObject< NDimensions, TransformType, OutputType >
  ::DerivativeAt( const PointType & point, short unsigned int order, OutputVectorType & value )
  {
    if( !IsEvaluableAt(point) )
      {
      itk::ExceptionObject e("SpatialObject.txx");
      e.SetLocation("SpatialObject< NDimensions, TransformType, OutputType >::DerivateAt(\
      const PointType, unsigned short, OutputVectorType & )");
      e.SetDescription("This spatial object is not derivable at the requested point");
      throw e;
      }

    OutputType r;

    if( order == 0 )
      {
      ValueAt(point,r);
      value.Fill(r);
      }
    else
      {
      PointType p1,p2;
      OutputVectorType v1,v2;
      OutputVectorType::Iterator it = value.Begin();
      OutputVectorType::Iterator it_v1 = v1.Begin();
      OutputVectorType::Iterator it_v2 = v2.Begin();

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
  }

  template< unsigned int NDimensions, class TransformType, class OutputType >
  SpatialObject< NDimensions, TransformType, OutputType >::PropertyPointer
  SpatialObject< NDimensions, TransformType, OutputType >
  ::GetProperty( void )
  { 
    return m_Property; 
  }

  template< unsigned int NDimensions, class TransformType, class OutputType >
  void 
  SpatialObject< NDimensions, TransformType, OutputType >
  ::SetProperty( PropertyPointer property)
  { 
    m_Property = property; 
  }

  template< unsigned int NDimensions, class TransformType, class OutputType >
  const SpatialObject< NDimensions, TransformType, OutputType >::Self & 
  SpatialObject< NDimensions, TransformType, OutputType >
  ::GetParent( void )
  {
    return m_Parent;
  }

  template< unsigned int NDimensions, class TransformType, class OutputType >  
  void 
  SpatialObject< NDimensions, TransformType, OutputType >
  ::SetParent( Pointer parent )
  {
    m_Parent = parent;
    RebuildAllTransformLists();
  }

  template< unsigned int NDimensions, class TransformType, class OutputType >
  bool
  SpatialObject< NDimensions, TransformType, OutputType >
  ::HasParent( void )
  {
    if( m_Parent.GetPointer() != NULL )
      return true;
    else
      return false;
  }

  template< unsigned int NDimensions, class TransformType, class OutputType >
  SpatialObject< NDimensions, TransformType, OutputType >
  ::SpatialObject( void )
  {
    m_Bounds = BoundingBoxType::New();
    m_Property = PropertyType::New();
    m_LocalToGlobalTransform = TransformType::New();
    m_GlobalToLocalTransform = TransformType::New();
    m_LocalToGlobalTransformList = new TransformListType();
    m_GlobalToLocalTransformList = new TransformListType();
    m_Spacing.Fill(1);
    SetParent(NULL);
    BuildLocalToGlobalTransformList(m_LocalToGlobalTransformList,false);
    BuildGlobalToLocalTransformList(m_GlobalToLocalTransformList,false);
  }

  template< unsigned int NDimensions, class TransformType, class OutputType >
  SpatialObject< NDimensions, TransformType, OutputType >
  ::~SpatialObject( void )
  {
    delete m_LocalToGlobalTransformList;
    delete m_GlobalToLocalTransformList;
  }


  template< unsigned int NDimensions, class TransformType, class OutputType >
  void 
  SpatialObject< NDimensions, TransformType, OutputType >
  ::PrintSelf( std::ostream& os, Indent indent ) const
  {
    TransformListType::iterator it;
    TransformListType::iterator end;

    Superclass::PrintSelf(os, indent);
    os << indent << "Parent: " << m_Parent.GetPointer() << std::endl << std::endl;
    os << "Bounding Box:" << std::endl;
    os << indent << m_Bounds << std::endl;
    os << "Geometric properties:" << std::endl;
    os << indent << "(local to global ) " << m_LocalToGlobalTransform << std::endl;
    os << indent << "(global to local ) " << m_GlobalToLocalTransform << std::endl;
    os << indent << "LocalToGlobalTransformList: ";
    it = m_LocalToGlobalTransformList->begin();    
    end = m_LocalToGlobalTransformList->end();
    for( ; it != end; it++ )
      {
      os<<"["<<(*it).GetPointer()<<"] ";
      }
    os << std::endl;
    os << indent << "GlobalToLocalTransformList size: ";
    it = m_GlobalToLocalTransformList->begin();    
    end = m_GlobalToLocalTransformList->end();
    for( ; it != end; it++ )
      {
      os<<"["<<(*it).GetPointer()<<"] ";
      }
    os << std::endl << std::endl;
    os << "Object properties: " << std::endl;
    os << m_Property << std::endl;
  }
  
  template< unsigned int NDimensions, class TransformType, class OutputType >
  void
  SpatialObject< NDimensions, TransformType, OutputType >
  ::SetBounds( BoundingBoxPointer bounds )
  { 
    m_Bounds = bounds; 
  }

  template< unsigned int NDimensions, class TransformType, class OutputType >
  SpatialObject< NDimensions, TransformType, OutputType >::BoundingBoxPointer
  SpatialObject< NDimensions, TransformType, OutputType >
  ::GetBounds( void )
  { 
    return m_Bounds;
  }

  template< unsigned int NDimensions, class TransformType, class OutputType >
  void
  SpatialObject< NDimensions, TransformType, OutputType >
  ::BuildLocalToGlobalTransformList( TransformListPointer list, bool init )
  {
    if( !init )
      {
      list->clear();
      }

    list->push_back(m_LocalToGlobalTransform);
    if( HasParent() )
      {
      m_Parent->BuildLocalToGlobalTransformList(list,true);
      }
  }

  template< unsigned int NDimensions, class TransformType, class OutputType >
  void
  SpatialObject< NDimensions, TransformType, OutputType >
  ::BuildGlobalToLocalTransformList( TransformListPointer list, bool init )
  {
    if( !init )
      {
      list->clear();
      }

    list->push_back(m_GlobalToLocalTransform);
    if( HasParent() )
      {
      m_Parent->BuildGlobalToLocalTransformList(list,true);
      }
  }

  template< unsigned int NDimensions, class TransformType, class OutputType >
  void 
  SpatialObject< NDimensions, TransformType, OutputType >
  ::SetLocalToGlobalTransform( TransformPointer transform )
  {
    m_LocalToGlobalTransform = transform;
    RebuildLocalToGlobalTransformList();
  }

  template< unsigned int NDimensions, class TransformType, class OutputType >
  const SpatialObject< NDimensions, TransformType, OutputType >::TransformPointer
  SpatialObject< NDimensions, TransformType, OutputType >
  ::GetLocalToGlobalTransform( void )
  {
    return m_LocalToGlobalTransform;
  }

  template< unsigned int NDimensions, class TransformType, class OutputType >
  void 
  SpatialObject< NDimensions, TransformType, OutputType >
  ::SetGlobalToLocalTransform( TransformPointer transform )
  {
    m_GlobalToLocalTransform = transform;
    RebuildGlobalToLocalTransformList();
  }

  template< unsigned int NDimensions, class TransformType, class OutputType >
  const SpatialObject< NDimensions, TransformType, OutputType >::TransformPointer
  SpatialObject< NDimensions, TransformType, OutputType >
  ::GetGlobalToLocalTransform( void )
  {
    return m_GlobalToLocalTransform;
  }

  template< unsigned int NDimensions, class TransformType, class OutputType >
  SpatialObject< NDimensions, TransformType, OutputType >::TransformListPointer
  SpatialObject< NDimensions, TransformType, OutputType >
  ::GetLocalToGlobalTransformList( void )
  {
    return m_LocalToGlobalTransformList;
  }

  template< unsigned int NDimensions, class TransformType, class OutputType >
  SpatialObject< NDimensions, TransformType, OutputType >::TransformListPointer
  SpatialObject< NDimensions, TransformType, OutputType >
  ::GetGlobalToLocalTransformList( void )
  {
    return m_GlobalToLocalTransformList;
  }

  template< unsigned int NDimensions, class TransformType, class OutputType >
  SpatialObject< NDimensions, TransformType, OutputType >::PointType
  SpatialObject< NDimensions, TransformType, OutputType >
  ::TransformPointToLocalCoordinate( PointType p )
  {
    TransformListType::reverse_iterator it = m_GlobalToLocalTransformList->rbegin();
    TransformListType::reverse_iterator end = m_GlobalToLocalTransformList->rend();
    PointType p1,p2;

    p1 = p;

    for(; it!=end; it++ )
      { 
      p2 = (*it)->TransformPoint(p1);
      p1 = p2;
      }

    return p2;
  }

  template< unsigned int NDimensions, class TransformType, class OutputType >
  SpatialObject< NDimensions, TransformType, OutputType >::PointType
  SpatialObject< NDimensions, TransformType, OutputType >
  ::TransformPointToGlobalCoordinate( PointType p )
  {
    TransformListType::reverse_iterator it = m_LocalToGlobalTransformList->rbegin();
    TransformListType::reverse_iterator end = m_LocalToGlobalTransformList->rend();
    PointType p1,p2;

    p1 = p;

    for(; it!=end; it++ )
      { 
      p2 = (*it)->TransformPoint(p1);
      p1 = p2;
      }

    return p2;
  }

  template< unsigned int NDimensions, class TransformType, class OutputType >
  unsigned long 
  SpatialObject< NDimensions, TransformType, OutputType >
  ::GetMTime( void ) const
  {
    unsigned long latestTime = Object::GetMTime();
    unsigned long boundingBoxMTime = m_Bounds->GetMTime();

    if( latestTime < boundingBoxMTime )
      {
      latestTime = boundingBoxMTime;
      }
    
    return latestTime;
  }

  template< unsigned int NDimensions, class TransformType, class OutputType >
  void 
  SpatialObject< NDimensions, TransformType, OutputType >
  ::RebuildLocalToGlobalTransformList( void )
  {
    BuildLocalToGlobalTransformList(m_LocalToGlobalTransformList,false);
  }

  template< unsigned int NDimensions, class TransformType, class OutputType >
  void 
  SpatialObject< NDimensions, TransformType, OutputType >
  ::RebuildGlobalToLocalTransformList( void )
  {
    BuildGlobalToLocalTransformList(m_GlobalToLocalTransformList,false);
  }

  template< unsigned int NDimensions, class TransformType, class OutputType >
  void 
  SpatialObject< NDimensions, TransformType, OutputType >
  ::RebuildAllTransformLists( void )
  {
    RebuildLocalToGlobalTransformList();
    RebuildGlobalToLocalTransformList();
  }

} // end of namespace itk

#endif // __SpatialObject_txx

