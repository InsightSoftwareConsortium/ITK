/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkCompositeSpatialObject.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __CompositeSpatialObject_txx
#define __CompositeSpatialObject_txx

#include "itkCompositeSpatialObject.h"

namespace itk
{
  template < unsigned int NDimensions, class TransformType, class OutputType, unsigned int PipelineDimension >
  CompositeSpatialObject< NDimensions, TransformType, OutputType, PipelineDimension >
  ::CompositeSpatialObject()
  {
    m_ParentId=-1;
  }

  template < unsigned int NDimensions, class TransformType, class OutputType, unsigned int PipelineDimension >
  CompositeSpatialObject< NDimensions, TransformType, OutputType, PipelineDimension >
  ::~CompositeSpatialObject()
  {
  }


  template < unsigned int NDimensions, class TransformType, class OutputType, unsigned int PipelineDimension >
  void 
  CompositeSpatialObject< NDimensions, TransformType, OutputType, PipelineDimension >
  ::AddSpatialObject( Superclass * pointer )
  {
    ChildrenListType::iterator it;

    it = std::find(m_Children.begin(),m_Children.end(),pointer);

    if( it == m_Children.end() )
      {
      m_Children.push_back( pointer );
      pointer->SetParent( this );
      }
    else
      { 
      //throw an exception object to let user know that he tried to add an object
      // which is already in the list of the children.
      }
  }

  template < unsigned int NDimensions, class TransformType, class OutputType, unsigned int PipelineDimension >
  void
  CompositeSpatialObject< NDimensions, TransformType, OutputType, PipelineDimension >
  ::RemoveSpatialObject( Superclass * pointer )
  {
    ChildrenListType::iterator it;
    
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
  }
  
  template < unsigned int NDimensions, class TransformType, class OutputType, unsigned int PipelineDimension >
  bool 
  CompositeSpatialObject< NDimensions, TransformType, OutputType, PipelineDimension >
  ::IsInside( const PointType &  point )
  {
    ChildrenListType::iterator it = m_Children.begin();
    ChildrenListType::iterator end = m_Children.end();
    
    for(; it!=end; it++)
      {
      if( (*it)->IsInside(point) ) 
        {
        return true;
        }
      }  
    return false;
  }

  template < unsigned int NDimensions, class TransformType, class OutputType, unsigned int PipelineDimension >
  bool 
  CompositeSpatialObject< NDimensions, TransformType, OutputType, PipelineDimension >
  ::IsEvaluableAt( const PointType & point )
  {
    ChildrenListType::iterator it = m_Children.begin();
    ChildrenListType::iterator end = m_Children.end();
    
    for(; it!=end; it++)
      {
      if( (*it)->IsEvaluableAt(point) ) 
        {
        return true;
        }
      }  
    return false;
  }

  template < unsigned int NDimensions, class TransformType, class OutputType, unsigned int PipelineDimension >
  void 
  CompositeSpatialObject< NDimensions, TransformType, OutputType, PipelineDimension >
  ::ValueAt( const PointType & point, OutputType & value )
  {
    ChildrenListType::iterator it = m_Children.begin();
    ChildrenListType::iterator end = m_Children.end();

    for(; it!=end; it++)
      {
      if( (*it)->IsEvaluableAt(point) )
        {
        (*it)->ValueAt(point,value); 
        }
      } 

    itk::ExceptionObject e("CompositeSpatialObject.txx");
    e.SetLocation("CompositeSpatialObject< NDimensions, TransformType, OutputType, PipelineDimension >::ValueAt( const PointType & )");
    e.SetDescription("This composite spatial object is not evaluable at the requested point");
    throw e;
  }

  template < unsigned int NDimensions, class TransformType, class OutputType, unsigned int PipelineDimension >
  void 
  CompositeSpatialObject< NDimensions, TransformType, OutputType, PipelineDimension >
  ::DerivativeAt( const PointType & point, short unsigned int order, OutputVectorType & value )
  {
    ChildrenListType::iterator it = m_Children.begin();
    ChildrenListType::iterator end = m_Children.end();
    
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

  template < unsigned int NDimensions, class TransformType, class OutputType, unsigned int PipelineDimension >
  void 
  CompositeSpatialObject< NDimensions, TransformType, OutputType, PipelineDimension >
  ::ComputeBounds( void )
  {
    ChildrenListType::iterator it = m_Children.begin();
    ChildrenListType::iterator end = m_Children.end();
    PointType pointLow,pointHigh;
    BoundingBoxType::PointsContainerPointer points = BoundingBoxType::PointsContainer::New() ;

    if( this->GetMTime() > m_BoundsMTime )
      {

      unsigned int i = 0;

      for(; it!=end; it++)
        {
        BoundingBoxType::PointsContainerConstPointer  childrenPoints    = (*it)->GetBounds()->GetPoints();
        BoundingBoxType::PointsContainerConstIterator childrenPointsIt  = childrenPoints->Begin();
        BoundingBoxType::PointsContainerConstIterator childrenPointsEnd = childrenPoints->End();

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
  
  template < unsigned int NDimensions, class TransformType, class OutputType, unsigned int PipelineDimension >
  unsigned long
  CompositeSpatialObject< NDimensions, TransformType, OutputType, PipelineDimension >
  ::GetMTime( void ) const
  {
    ChildrenListType::const_iterator it = m_Children.begin();
    ChildrenListType::const_iterator end = m_Children.end();
 
    unsigned long latestTime = Superclass::GetMTime();
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

  template < unsigned int NDimensions, class TransformType, class OutputType, unsigned int PipelineDimension >
  CompositeSpatialObject< NDimensions, TransformType, OutputType, PipelineDimension >::ChildrenListType &
  CompositeSpatialObject< NDimensions, TransformType, OutputType, PipelineDimension >
  ::GetChildren( void )
  {
    return m_Children;
  }

  template < unsigned int NDimensions, class TransformType, class OutputType, unsigned int PipelineDimension >
  void 
  CompositeSpatialObject< NDimensions, TransformType, OutputType, PipelineDimension >
  ::SetChildren( ChildrenListType & children )
  { 
    m_Children = children;

    ChildrenListType::const_iterator it = m_Children.begin();
    ChildrenListType::const_iterator end = m_Children.end();
    
    for(; it != end; it++ )
      {
      (*it)->SetParent( this );    
      }
  }

  template < unsigned int NDimensions, class TransformType, class OutputType, unsigned int PipelineDimension >
  unsigned int
  CompositeSpatialObject< NDimensions, TransformType, OutputType, PipelineDimension >
  ::GetNumberOfChildren( void )
  {
    return m_Children.size();
  } 

  template < unsigned int NDimensions, class TransformType, class OutputType, unsigned int PipelineDimension >
  void
  CompositeSpatialObject< NDimensions, TransformType, OutputType, PipelineDimension >
  ::PrintSelf( std::ostream& os, Indent indent ) const
  {
    os << indent << "Number of children: " 
       << m_Children.size() << std::endl;
    os << indent << "List of children: ";

    ChildrenListType::const_iterator it = m_Children.begin();
    ChildrenListType::const_iterator end = m_Children.end();

    for(; it != end; it++ )
      {
      os << "[" << (*it) << "] ";
      }
    os << std::endl;

    Superclass::PrintSelf(os, indent);
  }

  template < unsigned int NDimensions, class TransformType, class OutputType, unsigned int PipelineDimension >
  void 
  CompositeSpatialObject< NDimensions, TransformType, OutputType, PipelineDimension >
  ::RebuildGlobalToLocalTransformList( void ) const
  {
    BuildGlobalToLocalTransformList(m_GlobalToLocalTransformList,false);

    ChildrenListType::const_iterator it = m_Children.begin();
    ChildrenListType::const_iterator end = m_Children.end();

    for(; it != end; it++ )
      {
      (*it)->RebuildGlobalToLocalTransformList();
      }
  }

  template < unsigned int NDimensions, class TransformType, class OutputType, unsigned int PipelineDimension >
  void 
  CompositeSpatialObject< NDimensions, TransformType, OutputType, PipelineDimension >
  ::RebuildLocalToGlobalTransformList( void ) const
  {
    BuildLocalToGlobalTransformList(m_LocalToGlobalTransformList,false);

    ChildrenListType::const_iterator it = m_Children.begin();
    ChildrenListType::const_iterator end = m_Children.end();

    for(; it != end; it++ )
      {
      (*it)->RebuildLocalToGlobalTransformList();
      }
  }

} // end of namespace itk 

#endif // __SpatialCompositeObject_txx



