/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkPlaneSpatialObject.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __PlaneSpatialObject_txx
#define __PlaneSpatialObject_txx

#include "itkPlaneSpatialObject.h" 

namespace itk 
{ 

/** Constructor */
template< unsigned int NDimensions , unsigned int PipelineDimension >
PlaneSpatialObject<NDimensions, PipelineDimension >
::PlaneSpatialObject()
{
  strcpy(m_TypeName,"PlaneSpatialObject");
  m_Dimension = NDimensions;
} 

/** Destructor */
template< unsigned int NDimensions , unsigned int PipelineDimension >
PlaneSpatialObject<NDimensions, PipelineDimension >
::~PlaneSpatialObject()  
{
  
}

/** Test if the given point is inside the blob */
template< unsigned int NDimensions , unsigned int PipelineDimension >
bool 
PlaneSpatialObject< NDimensions, PipelineDimension > 
::IsInside( const PointType & point ) const
{
  itkDebugMacro( "Checking the point [" << point << "is inside the plane" );
    
  PointType transformedPoint = point;
  TransformPointToLocalCoordinate(transformedPoint);

  bool inside;
  for(unsigned int i=0;i<NDimensions;i++)
  {
    if((transformedPoint[i] <= m_UpperPoint[i] ) 
       && (transformedPoint[i] >= m_LowerPoint[i] )
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

/** Compute the bounds of the Plane */
template< unsigned int NDimensions , unsigned int PipelineDimension >
void
PlaneSpatialObject<NDimensions, PipelineDimension >
::ComputeBounds( void ) 
{ 
  itkDebugMacro( "Computing tube bounding box" );

  if( this->GetMTime() > m_BoundsMTime )
  { 
    PointContainerPointer points = PointContainerType::New();
    points->Initialize();

    unsigned int j=0;
    PointType pnt;
    PointType pnt2;
    pnt.Fill(0);
    pnt2.Fill(0);  
    for(unsigned int i=0; i<NDimensions;i++) 
    {   
      pnt[i]=m_LowerPoint[i];
      pnt2[i]=m_UpperPoint[i];
    }
    points->InsertElement(j++,pnt);
    points->InsertElement(j++,pnt2);
    m_Bounds->SetPoints(points);
    m_Bounds->ComputeBoundingBox();
    m_BoundsMTime.Modified();
  }
} 


/** Returns if the Plane os evaluable at one point */
template< unsigned int NDimensions , unsigned int PipelineDimension >
bool
PlaneSpatialObject<NDimensions, PipelineDimension >
::IsEvaluableAt( const PointType & point )
{
  itkDebugMacro( "Checking if the Plane is evaluable at " << point );
  return IsInside(point);
}

/** Returns the value at one point */
template< unsigned int NDimensions , unsigned int PipelineDimension >
void
PlaneSpatialObject<NDimensions, PipelineDimension >
::ValueAt( const PointType & point, double & value )
{
  itkDebugMacro( "Getting the value of the tube at " << point );

  if( !IsEvaluableAt(point) )
  {
    value = 0;
    itk::ExceptionObject e("PlaneSpatialObject.txx");
    e.SetLocation("BlobSpatialObject::ValueAt( const PointType & )");
    e.SetDescription("this object cannot provide a value at the requested point");
    throw e;
  }

  value = 1;
}

/** Print Self function */
template< unsigned int NDimensions , unsigned int PipelineDimension >
void 
PlaneSpatialObject< NDimensions, PipelineDimension >
::PrintSelf( std::ostream& os, Indent indent ) const
{
  Superclass::PrintSelf(os, indent);
}

/** Return the modification time */
template< unsigned int NDimensions , unsigned int PipelineDimension >
unsigned long
PlaneSpatialObject< NDimensions, PipelineDimension >
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
