/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkTubeSpatialObject.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

#ifndef __itkTubeSpatialObject_txx
#define __itkTubeSpatialObject_txx

#include "itkTubeSpatialObject.h" 

/* 
* one of the things to do to the tube class is to use a common
* container type for the TubeSpatialObjectPoints, and the points of the bounding
* box 
*/

namespace itk  
{ 

  template< unsigned int TDimension , unsigned int PipelineDimension >
  TubeSpatialObject< TDimension, PipelineDimension > 
  ::TubeSpatialObject()  
  { 
    m_Dimension = TDimension;
    strcpy(m_TypeName,"TubeSpatialObject");
    m_Id = 0; 
    m_Property->SetRed(1); 
    m_Property->SetGreen(0); 
    m_Property->SetBlue(0); 
    m_Property->SetAlpha(1); 
    m_Points = new PointListType();
    ComputeBounds();
  } 
 
  template< unsigned int TDimension , unsigned int PipelineDimension >
  TubeSpatialObject< TDimension, PipelineDimension >  
  ::~TubeSpatialObject()
  { 
    delete m_Points;
  } 
 
  template< unsigned int TDimension , unsigned int PipelineDimension >
  TubeSpatialObject< TDimension, PipelineDimension > ::PointListPointer  
  TubeSpatialObject< TDimension, PipelineDimension > 
  ::GetPoints() const 
  { 
    itkDebugMacro( "Getting TubePoint list" );
    
    return m_Points;
  } 
 
  template< unsigned int TDimension , unsigned int PipelineDimension >
  void  
  TubeSpatialObject< TDimension, PipelineDimension >  
  ::SetPoints( PointListPointer points )  
  {
    // in this function, passing a null pointer as argument will
    // just clear the list...

    itkDebugMacro( "Setting TubePoint list to " << points );

    m_Points->clear();
         
    if( points )
    {
      PointListType::iterator it,end;
      it = points->begin();    
      end = points->end();

      for(; it != end; it++ )
      {
        m_Points->push_back(*it);
      }
    }  
      
    this->Modified();
  } 
 
  template< unsigned int TDimension , unsigned int PipelineDimension >
  unsigned int  
  TubeSpatialObject< TDimension, PipelineDimension >  
  ::GetId( void ) const  
  { 
    itkDebugMacro( "Getting tube ID" );
    return m_Id; 
  } 
 
  template< unsigned int TDimension , unsigned int PipelineDimension >
  void  
  TubeSpatialObject< TDimension, PipelineDimension >  
  ::SetId( const unsigned int id )  
  { 
    itkDebugMacro( "Setting tube ID to " << id );
    m_Id = id; 
  } 
 
  template< unsigned int TDimension , unsigned int PipelineDimension >
  void  
  TubeSpatialObject< TDimension, PipelineDimension >  
  ::PrintSelf( std::ostream& os, Indent indent ) const 
  { 
    os << indent << "TubeSpatialObject(" << this << ")" << std::endl; 
    os << indent << "ID: " << m_Id << std::endl; 
    os << indent << "nb of points: "<< m_Points->size() << std::endl;
    //os << indent << "Bounds: " << m_Bounds <<std::endl;
    Superclass::PrintSelf( os, indent ); 
  } 
   
  template< unsigned int TDimension , unsigned int PipelineDimension >
  void 
  TubeSpatialObject< TDimension, PipelineDimension >  
  ::ComputeBounds( void ) 
  { 
    itkDebugMacro( "Computing tube bounding box" );

    if( this->GetMTime() > m_BoundsMTime )
    {
      PointType pointLow, pointHigh; 
      PointType tempPointLow, tempPointHigh;
      PointListType::iterator it  = m_Points->begin();
      PointListType::iterator end = m_Points->end();

      PointContainerPointer points = PointContainerType::New();
      points->Initialize();

      for(unsigned int i=0; it!= end; it++, i++ ) 
        {     
        points->InsertElement(i,(*it)->GetCenterLinePoint());
        } 

      m_Bounds->SetPoints(points);
      m_Bounds->ComputeBoundingBox();
      m_BoundsMTime.Modified();
    }
  } 

  template< unsigned int TDimension , unsigned int PipelineDimension >
  bool 
  TubeSpatialObject< TDimension, PipelineDimension >  
  ::IsInside( const PointType & point )  
  {
    itkDebugMacro( "Checking the point [" << point << "is inside the tube" );

    // find the closest point, and get the radius at that point...
    // if the distance is shorter than the radius, then the point is
    // inside, else it is outside :)))
    // think about using an interpolation between the closest point and
    // its next, and previous neighbor, in order to be more accurate during 
    // the selection process.

    double minSquareDist=999999.0;
    double tempSquareDist;
    PointListType::iterator it = m_Points->begin();
    PointListType::iterator end = m_Points->end(); 
    PointListType::iterator min;
    
    PointType transformedPoint = point;
    TransformPointToLocalCoordinate(transformedPoint);

    if( !m_Bounds->IsInside(transformedPoint) )
      {
      return false;
      }

    for(unsigned int i=0; it!= end; it++,i++)
      {  
      if( (tempSquareDist=transformedPoint.SquaredEuclideanDistanceTo((*it)->GetCenterLinePoint())) < minSquareDist)
        {
        minSquareDist = tempSquareDist;
        min = it; 
        }
      }

    double dist = sqrt(minSquareDist);

    if( dist <= ((*min)->GetRadius()) )
      {
      return true;
      }
    else
      {
      return false;
      }
  } 
 
  template< unsigned int TDimension , unsigned int PipelineDimension >
  bool  
  TubeSpatialObject< TDimension, PipelineDimension >  
  ::CalcTangent( void ) 
  { 
    itkDebugMacro( "Computing the tangent vectors of the tube" );

    if( m_Points->size() == 0 ) 
    return true; 
     
    PointType point; 
    vnl_vector< double > t;
    double l; 

    t = point.Get_vnl_vector();
    t.fill(0.0);
 
    if( m_Points->size() == 1 ) 
    { 
      ( * m_Points->begin() )->SetTangent(t); 
      return true; 
    } 
     
    PointListType::iterator i, j, k, e; 
    i = m_Points->begin(); 
    i++; 
    e = m_Points->end(); 
    e--; 
     
    for(;i!=e; i++) 
    { 
      j = i; 
      j++; 
      k = i; 
      k--; 
      t = ((*j)->GetCenterLinePoint().Get_vnl_vector()) - ((*k)->GetCenterLinePoint().Get_vnl_vector()); 
      t(1) /= 2; 
      t(2) /= 2; 
      t(3) /= 2; 
      l = sqrt(t(1)*t(1) + t(2)*t(2) + t(3)*t(3)); 
      t(1) /= l; 
      t(2) /= l; 
      t(3) /= l; 
      (*i)->SetTangent(t); 
    } 
     
    (*e)->SetTangent(t); 
    i = m_Points->begin(); 
    j = i; 
    j++; 
    (*i)->SetTangent( *((*j)->GetTangent()) ); 
    return true; 
  } 

  template< unsigned int TDimension , unsigned int PipelineDimension >
  bool
  TubeSpatialObject< TDimension, PipelineDimension > 
  ::IsEvaluableAt( const PointType & point )
  {
    itkDebugMacro( "Checking if the tube is evaluable at " << point );

    return IsInside(point);
  }

  template< unsigned int TDimension , unsigned int PipelineDimension >
  void
  TubeSpatialObject< TDimension, PipelineDimension > 
  ::ValueAt( const PointType & point, double & value )
  {
    itkDebugMacro( "Getting the value of the tube at " << point );

    if( !IsEvaluableAt(point) )
      {
      value = 0;
      itk::ExceptionObject e("TubeSpatialObject.txx");
      e.SetLocation("TubeSpatialObject::ValueAt( const PointType & )");
      e.SetDescription("this object cannot provide a value at the requested point");
      throw e;
      }

    value = 1;
  }

  template< unsigned int TDimension , unsigned int PipelineDimension >
  unsigned long
  TubeSpatialObject< TDimension, PipelineDimension > 
  ::GetMTime( void ) const
  {
    unsigned long latestMTime = Object::GetMTime();
    unsigned long boundsMTime;

    if( (boundsMTime = m_Bounds->GetMTime()) > latestMTime )
      {
      latestMTime = boundsMTime;
      }
    return latestMTime;
  }

} // end namespace itk 

#endif
