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
::IsInside( const PointType & point, bool includeChildren ) const
{
  itkDebugMacro( "Checking the point [" << point << "is inside the plane" );
    
  PointType transformedPoint = point;
  TransformPointToLocalCoordinate(transformedPoint);

  bool inside = true;
  for(unsigned int i=0;i<NDimensions;i++)
    {
    if((transformedPoint[i] > m_UpperPoint[i] ) 
       || (transformedPoint[i] < m_LowerPoint[i] ))
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
    return Superclass::IsInside(point, includeChildren);
    }
} 

/** Compute the bounds of the Plane */
template< unsigned int NDimensions , unsigned int PipelineDimension >
bool
PlaneSpatialObject<NDimensions, PipelineDimension >
::ComputeBoundingBox( bool includeChildren ) 
{ 
  itkDebugMacro( "Computing tube bounding box" );
  bool ret = false;

  if( this->GetMTime() > m_BoundsMTime )
    { 
    ret = Superclass::ComputeBoundingBox(includeChildren);

    PointType pnt;
    PointType pnt2;
    pnt.Fill(0);
    pnt2.Fill(0);  
    for(unsigned int i=0; i<NDimensions;i++) 
      {   
      pnt[i]=m_LowerPoint[i];
      pnt2[i]=m_UpperPoint[i];
      }

    if(!ret)
      {
      m_Bounds->SetMinimum(pnt);
      m_Bounds->SetMaximum(pnt2);
      }
    else
      {
      m_Bounds->ConsiderPoint(pnt);
      m_Bounds->ConsiderPoint(pnt2);
      }

    m_BoundsMTime = this->GetMTime();
    }
  return ret;
} 


/** Returns if the Plane os evaluable at one point */
template< unsigned int NDimensions , unsigned int PipelineDimension >
bool
PlaneSpatialObject<NDimensions, PipelineDimension >
::IsEvaluableAt( const PointType & point, bool includeChildren )
{
  itkDebugMacro( "Checking if the Plane is evaluable at " << point );
  return IsInside(point, includeChildren);
}

/** Returns the value at one point */
template< unsigned int NDimensions , unsigned int PipelineDimension >
void
PlaneSpatialObject<NDimensions, PipelineDimension >
::ValueAt( const PointType & point, double & value, bool includeChildren )
{
  itkDebugMacro( "Getting the value of the tube at " << point );
  if( IsInside(point, false) )
    {
    value = 1;
    return;
    }
  else
    {
    if( Superclass::IsEvaluableAt(point, includeChildren) )
      {
      Superclass::ValueAt(point, value, includeChildren);
      return;
      }
    else
      {
      value = 0;
      itk::ExceptionObject e("PlaneSpatialObject.txx");
      e.SetLocation("BlobSpatialObject::ValueAt( const PointType & )");
      e.SetDescription("this object cannot provide a value at the point");
      throw e;
      }
    }
}

/** Print Self function */
template< unsigned int NDimensions , unsigned int PipelineDimension >
void 
PlaneSpatialObject< NDimensions, PipelineDimension >
::PrintSelf( std::ostream& os, Indent indent ) const
{
  Superclass::PrintSelf(os, indent);
}

} // end namespace itk

#endif
