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

  template< unsigned int NDimensions, typename TTransform, unsigned int PipelineDimension >
  SpatialObject< NDimensions, TTransform, PipelineDimension>::PropertyType *
  SpatialObject< NDimensions, TTransform, PipelineDimension>
  ::GetProperty( void )
  { 
    return m_Property; 
  }

  template< unsigned int NDimensions, typename TTransform, unsigned int PipelineDimension >
  void 
  SpatialObject< NDimensions, TTransform, PipelineDimension>
  ::SetProperty( const PropertyType * property)
  { 
    m_Property = property; 
  }

  template< unsigned int NDimensions, typename TTransform, unsigned int PipelineDimension >  
  void 
  SpatialObject< NDimensions, TTransform, PipelineDimension>
  ::SetParent( const Self * parent )
  {
    m_Parent = parent;
    RebuildAllTransformLists();
  }

  template< unsigned int NDimensions, typename TTransform, unsigned int PipelineDimension >
  bool
  SpatialObject< NDimensions, TTransform, PipelineDimension>
  ::HasParent( void ) const
  {
    if( m_Parent )
      return true;
    else
      return false;
  }

  template< unsigned int NDimensions, typename TTransform, unsigned int PipelineDimension >
  SpatialObject< NDimensions, TTransform, PipelineDimension>
  ::SpatialObject( void )
  {
    m_Dimension = NDimensions;
    m_Bounds = BoundingBoxType::New();
    m_Property = PropertyType::New();
    m_LocalToGlobalTransform = TransformType::New();
    m_GlobalToLocalTransform = TransformType::New();
    m_LocalToGlobalTransformList = new TransformListType();
    m_GlobalToLocalTransformList = new TransformListType();
    m_Spacing.resize(NDimensions);
    m_Spacing.fill(1);
    SetParent(NULL);
    BuildLocalToGlobalTransformList(m_LocalToGlobalTransformList,false);
    BuildGlobalToLocalTransformList(m_GlobalToLocalTransformList,false);
  }

  template< unsigned int NDimensions, typename TTransform, unsigned int PipelineDimension >
  SpatialObject< NDimensions, TTransform, PipelineDimension>
  ::~SpatialObject( void )
  {
  }


  template< unsigned int NDimensions, typename TTransform, unsigned int PipelineDimension >
  void 
  SpatialObject< NDimensions, TTransform, PipelineDimension>
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
      os<<"["<<(*it)<<"] ";
      }
    os << std::endl;
    os << indent << "GlobalToLocalTransformList size: ";
    it = m_GlobalToLocalTransformList->begin();    
    end = m_GlobalToLocalTransformList->end();
    for( ; it != end; it++ )
      {
      os<<"["<<(*it)<<"] ";
      }
    os << std::endl << std::endl;
    os << "Object properties: " << std::endl;
    os << m_Property << std::endl;
  }
  
  template< unsigned int NDimensions, typename TTransform, unsigned int PipelineDimension >
  void
  SpatialObject< NDimensions, TTransform, PipelineDimension>
  ::SetBounds( BoundingBoxPointer bounds )
  { 
    m_Bounds = bounds; 
  }

  template< unsigned int NDimensions, typename TTransform, unsigned int PipelineDimension >
  SpatialObject< NDimensions, TTransform, PipelineDimension>::BoundingBoxType *
  SpatialObject< NDimensions, TTransform, PipelineDimension>
  ::GetBounds( void )
  { 
    return m_Bounds.GetPointer();
  }

  template< unsigned int NDimensions, typename TTransform, unsigned int PipelineDimension >
  void
  SpatialObject< NDimensions, TTransform, PipelineDimension>
  ::BuildLocalToGlobalTransformList( TransformListPointer list, bool init ) const
  {
    list->push_back(m_LocalToGlobalTransform);

    if( HasParent() )
      {
      m_Parent->BuildLocalToGlobalTransformList(list,true);
      }
  }

  template< unsigned int NDimensions, typename TTransform, unsigned int PipelineDimension >
  void
  SpatialObject< NDimensions, TTransform, PipelineDimension>
  ::BuildGlobalToLocalTransformList( TransformListPointer list, bool init ) const
  {
    list->push_back(m_GlobalToLocalTransform);

    if( HasParent() )
      {
      m_Parent->BuildGlobalToLocalTransformList(list,true);
      }
  }

  template< unsigned int NDimensions, typename TTransform, unsigned int PipelineDimension >
  void 
  SpatialObject< NDimensions, TTransform, PipelineDimension>
  ::RebuildLocalToGlobalTransformList( void ) const
  {
    m_LocalToGlobalTransformList->clear();
    BuildLocalToGlobalTransformList(m_LocalToGlobalTransformList,false);
  }

  template< unsigned int NDimensions, typename TTransform, unsigned int PipelineDimension >
  void 
  SpatialObject< NDimensions, TTransform, PipelineDimension>
  ::RebuildGlobalToLocalTransformList( void ) const
  {
    m_GlobalToLocalTransformList->clear();
    BuildGlobalToLocalTransformList(m_GlobalToLocalTransformList,false);
  }

  template< unsigned int NDimensions, typename TTransform, unsigned int PipelineDimension >
  void 
  SpatialObject< NDimensions, TTransform, PipelineDimension>
  ::RebuildAllTransformLists( void ) const
  {
    RebuildLocalToGlobalTransformList();
    RebuildGlobalToLocalTransformList();
  }

  template< unsigned int NDimensions, typename TTransform, unsigned int PipelineDimension >
  void 
  SpatialObject< NDimensions, TTransform, PipelineDimension>
  ::SetLocalToGlobalTransform( const TransformType * transform )
  {
    m_LocalToGlobalTransform = transform;
    RebuildLocalToGlobalTransformList();
  }

  template< unsigned int NDimensions, typename TTransform, unsigned int PipelineDimension >
  const SpatialObject< NDimensions, TTransform, PipelineDimension>::TransformType *
  SpatialObject< NDimensions, TTransform, PipelineDimension>
  ::GetLocalToGlobalTransform( void )
  {
    return m_LocalToGlobalTransform.GetPointer();
  }

  template< unsigned int NDimensions, typename TTransform, unsigned int PipelineDimension >
  void 
  SpatialObject< NDimensions, TTransform, PipelineDimension>
  ::SetGlobalToLocalTransform( const TransformType * transform )
  {
    m_GlobalToLocalTransform = transform;
    RebuildGlobalToLocalTransformList();
  }

  template< unsigned int NDimensions, typename TTransform, unsigned int PipelineDimension >
  const SpatialObject< NDimensions, TTransform, PipelineDimension>::TransformType *
  SpatialObject< NDimensions, TTransform, PipelineDimension>
  ::GetGlobalToLocalTransform( void )
  {
    return m_GlobalToLocalTransform.GetPointer();
  }

  template< unsigned int NDimensions, typename TTransform, unsigned int PipelineDimension >
  SpatialObject< NDimensions, TTransform, PipelineDimension>::TransformListPointer
  SpatialObject< NDimensions, TTransform, PipelineDimension>
  ::GetLocalToGlobalTransformList( void )
  {
    return m_LocalToGlobalTransformList;
  }

  template< unsigned int NDimensions, typename TTransform, unsigned int PipelineDimension >
  SpatialObject< NDimensions, TTransform, PipelineDimension>::TransformListPointer
  SpatialObject< NDimensions, TTransform, PipelineDimension>
  ::GetGlobalToLocalTransformList( void )
  {
    return m_GlobalToLocalTransformList;
  }

  template< unsigned int NDimensions, typename TTransform, unsigned int PipelineDimension >
  void
  SpatialObject< NDimensions, TTransform, PipelineDimension>
  ::TransformPointToLocalCoordinate( PointType & p ) const
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

    p = p2;
  }

  template< unsigned int NDimensions, typename TTransform, unsigned int PipelineDimension >
  void
  SpatialObject< NDimensions, TTransform, PipelineDimension>
  ::TransformPointToGlobalCoordinate( PointType & p ) const
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

    p = p2;
  }

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
    
    return latestTime;
  }



} // end of namespace itk

#endif // __SpatialObject_txx

