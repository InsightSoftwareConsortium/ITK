/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkLineSpatialObject.txx
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

#ifndef __itkLineSpatialObject_txx
#define __itkLineSpatialObject_txx

#include "itkLineSpatialObject.h" 

namespace itk  
{ 

/** Constructor */
template< unsigned int TDimension , unsigned int PipelineDimension >
LineSpatialObject< TDimension, PipelineDimension > 
::LineSpatialObject()  
{ 
  m_Dimension = TDimension;
  strcpy(m_TypeName,"LineSpatialObject");
  m_Property->SetRed(1); 
  m_Property->SetGreen(0); 
  m_Property->SetBlue(0); 
  m_Property->SetAlpha(1); 
  ComputeBounds();
} 
 
/** Destructor */
template< unsigned int TDimension , unsigned int PipelineDimension >
LineSpatialObject< TDimension, PipelineDimension >  
::~LineSpatialObject()
{ 
} 
 
/** Returns a reference to the list of the Line points.*/ 
template< unsigned int TDimension , unsigned int PipelineDimension >
typename LineSpatialObject< TDimension, PipelineDimension > ::PointListType &  
LineSpatialObject< TDimension, PipelineDimension > 
::GetPoints()
{ 
  itkDebugMacro( "Getting LinePoint list" );
  return m_Points;
} 

/** Set the list of Line points. */
template< unsigned int TDimension , unsigned int PipelineDimension >
void  
LineSpatialObject< TDimension, PipelineDimension >  
::SetPoints( PointListType & points )  
{
  // in this function, passing a null pointer as argument will
  // just clear the list...
  m_Points.clear();
       
  typename PointListType::iterator it,end;
  it = points.begin();    
  end = points.end();
  for(; it != end; it++ )
  {
    m_Points.push_back(*it);
  }
   
  this->Modified();
}
 
/** Print the object. */
template< unsigned int TDimension , unsigned int PipelineDimension >
void  
LineSpatialObject< TDimension, PipelineDimension >  
::PrintSelf( std::ostream& os, Indent indent ) const 
{ 
  os << indent << "LineSpatialObject(" << this << ")" << std::endl; 
  os << indent << "ID: " << m_Id << std::endl; 
  os << indent << "nb of points: "<< m_Points.size() << std::endl;
  Superclass::PrintSelf( os, indent ); 
} 
   
/** Compute the boundaries of the line.*/
template< unsigned int TDimension , unsigned int PipelineDimension >
void 
LineSpatialObject< TDimension, PipelineDimension >  
::ComputeBounds( void ) 
{ 
  itkDebugMacro( "Computing tube bounding box" );
  if( this->GetMTime() > m_BoundsMTime )
  {
    PointType pointLow, pointHigh; 
    PointType tempPointLow, tempPointHigh;
    typename PointListType::iterator it  = m_Points.begin();
    typename PointListType::iterator end = m_Points.end();
    PointContainerPointer points = PointContainerType::New();
    points->Initialize();

    for(unsigned int i=0; it!= end; it++, i++ ) 
    {     
      points->InsertElement(i,(*it)->GetPosition());
    } 

    m_Bounds->SetPoints(points);
    m_Bounds->ComputeBoundingBox();
    m_BoundsMTime.Modified();
  }
} 

/** Check if a given point is inside a line
 *  return True only if the point is in the point list */
template< unsigned int TDimension , unsigned int PipelineDimension >
bool 
LineSpatialObject< TDimension, PipelineDimension >  
::IsInside( const PointType & point )  
{
  itkDebugMacro( "Checking the point [" << point << "is on the Line" );
  typename PointListType::iterator it = m_Points.begin();
  
  PointType transformedPoint = point;
  TransformPointToLocalCoordinate(transformedPoint);

  if( m_Bounds->IsInside(transformedPoint) )
  {
    while(it != m_Points.end())
    {
      if((*it)->GetPosition() == transformedPoint)
      {
        return true;
      }
      it++;
    }
  }
  return Superclass::IsInside(transformedPoint);
} 

/** Returns true if the line is evaluable at the requested point, 
 *  false otherwise. */
template< unsigned int TDimension , unsigned int PipelineDimension >
bool
LineSpatialObject< TDimension, PipelineDimension > 
::IsEvaluableAt( const PointType & point )
{
  itkDebugMacro( "Checking if the tube is evaluable at " << point );
  return IsInside(point);
}

/** Returns the value of the line at that point.
 * Currently this function returns a binary value,
 * but it might want to return a degree of membership
 * in case of fuzzy Lines. */
template< unsigned int TDimension , unsigned int PipelineDimension >
void
LineSpatialObject< TDimension, PipelineDimension > 
::ValueAt( const PointType & point, double & value )
{
  itkDebugMacro( "Getting the value of the tube at " << point );

  if( !IsEvaluableAt(point) )
  {
    value = 0;
    itk::ExceptionObject e("LineSpatialObject.txx");
    e.SetLocation("LineSpatialObject::ValueAt( const PointType & )");
    e.SetDescription("this object cannot provide a value at the requested point");
    throw e;
  }
  value = 1;
}

/** Return the last modified time of the object, and all of its components */
template< unsigned int TDimension , unsigned int PipelineDimension >
unsigned long
LineSpatialObject< TDimension, PipelineDimension > 
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
