/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkEllipseSpatialObject.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __EllipseSpatialObject_txx
#define __EllipseSpatialObject_txx

#include "itkEllipseSpatialObject.h" 

namespace itk 
{ 

/** Constructor */
template< unsigned int NDimensions , unsigned int PipelineDimension >
EllipseSpatialObject<NDimensions, PipelineDimension >
::EllipseSpatialObject()
{
  strcpy(m_TypeName,"EllipseSpatialObject");
  m_Radius.Fill(1.0);
  m_Dimension = NDimensions;
} 

/** Destructor */
template< unsigned int NDimensions , unsigned int PipelineDimension >
EllipseSpatialObject<NDimensions, PipelineDimension >
::~EllipseSpatialObject()  
{
  
}

/** Set all radii to the same radius value */
template< unsigned int NDimensions , unsigned int PipelineDimension >
void
EllipseSpatialObject<NDimensions, PipelineDimension >
::SetRadius(double radius)
{
  for(unsigned int i=0;i<NumberOfDimension;i++)
  {
    m_Radius[i]=radius;
  }
}

/** Test if the given point is inside the blob */
template< unsigned int NDimensions , unsigned int PipelineDimension >
bool 
EllipseSpatialObject< NDimensions, PipelineDimension > 
::IsInside( const PointType & point )  
{
  itkDebugMacro( "Checking the point [" << point << "is inside the tube" );
    
  PointType transformedPoint = point;
  TransformPointToLocalCoordinate(transformedPoint);

  bool inside;
  for(unsigned int i=0;i<NDimensions;i++)
  {
    if((transformedPoint[i] <= m_Radius[i] ) 
       && (transformedPoint[i] >= -m_Radius[i] )
      )
    {
      inside = true;
    }
    else
    {
      inside = false;
      break;
    }
  }

  if(inside)
  {
    return true;
  }
  else
  {
    return Superclass::IsInside(transformedPoint);
  }
} 

/** Compute the bounds of the ellipse */
template< unsigned int NDimensions , unsigned int PipelineDimension >
void
EllipseSpatialObject<NDimensions, PipelineDimension >
::ComputeBounds( void ) 
{ 
  itkDebugMacro( "Computing tube bounding box" );

  if( this->GetMTime() > m_BoundsMTime )
  { 
    PointContainerPointer points = PointContainerType::New();
    points->Initialize();

    PointType pnt;
    pnt.Fill(0);
    PointType pnt2;
    pnt2.Fill(0);
    unsigned int j=0;
    for(unsigned int i=0; i<NDimensions;i++) 
    {   
      pnt[i]=m_Radius[i];
      pnt2[i]=-m_Radius[i];
    } 

    points->InsertElement(j++,pnt); 
    points->InsertElement(j++,pnt2);
    m_Bounds->SetPoints(points);
    m_Bounds->ComputeBoundingBox();
    m_BoundsMTime.Modified();
  }
} 


/** Returns if the ellipse os evaluable at one point */
template< unsigned int NDimensions , unsigned int PipelineDimension >
bool
EllipseSpatialObject<NDimensions, PipelineDimension >
::IsEvaluableAt( const PointType & point )
{
  itkDebugMacro( "Checking if the ellipse is evaluable at " << point );
  return IsInside(point);
}

/** Returns the value at one point */
template< unsigned int NDimensions , unsigned int PipelineDimension >
void
EllipseSpatialObject<NDimensions, PipelineDimension >
::ValueAt( const PointType & point, double & value )
{
  itkDebugMacro( "Getting the value of the tube at " << point );

  if( !IsEvaluableAt(point) )
  {
    value = 0;
    itk::ExceptionObject e("BlobSpatialObject.txx");
    e.SetLocation("BlobSpatialObject::ValueAt( const PointType & )");
    e.SetDescription("this object cannot provide a value at the requested point");
    throw e;
  }

  value = 1;
}

/** Print Self function */
template< unsigned int NDimensions , unsigned int PipelineDimension >
void 
EllipseSpatialObject< NDimensions, PipelineDimension >
::PrintSelf( std::ostream& os, Indent indent ) const
{

  Superclass::PrintSelf(os, indent);
  os << "Radius: " << m_Radius << std::endl;

}

/** Return the modification time */
template< unsigned int NDimensions , unsigned int PipelineDimension >
unsigned long
EllipseSpatialObject< NDimensions, PipelineDimension >
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
