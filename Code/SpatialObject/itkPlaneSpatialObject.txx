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
template< unsigned int NDimensions , unsigned int SpaceDimension >
PlaneSpatialObject<NDimensions, SpaceDimension >
::PlaneSpatialObject()
{
  strcpy(m_TypeName,"PlaneSpatialObject");
  m_Dimension = NDimensions;
} 

/** Destructor */
template< unsigned int NDimensions , unsigned int SpaceDimension >
PlaneSpatialObject<NDimensions, SpaceDimension >
::~PlaneSpatialObject()  
{
  
}

/** Test if the given point is inside the blob */
template< unsigned int NDimensions , unsigned int SpaceDimension >
bool 
PlaneSpatialObject< NDimensions, SpaceDimension > 
::IsInside( const PointType & point, unsigned int depth, char * name ) const
{
  itkDebugMacro( "Checking the point [" << point << "is inside the plane" );
    
  if(name == NULL || strstr(typeid(Self).name(), name) )
    {
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
    }

  return Superclass::IsInside(point, depth, name);
} 

/** Compute the bounds of the Plane */
template< unsigned int NDimensions , unsigned int SpaceDimension >
bool
PlaneSpatialObject<NDimensions, SpaceDimension >
::ComputeBoundingBox( unsigned int depth, char * name ) 
{ 
  itkDebugMacro( "Computing tube bounding box" );
  bool ret = false;

  if( this->GetMTime() > m_BoundsMTime )
    { 
    ret = Superclass::ComputeBoundingBox(depth, name);

    if(name == NULL || strstr(typeid(Self).name(), name) )
      {
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
      }

    m_BoundsMTime = this->GetMTime();
    }

  return ret;
} 


/** Returns if the Plane os evaluable at one point */
template< unsigned int NDimensions , unsigned int SpaceDimension >
bool
PlaneSpatialObject<NDimensions, SpaceDimension >
::IsEvaluableAt( const PointType & point, unsigned int depth, char * name )
{
  itkDebugMacro( "Checking if the Plane is evaluable at " << point );
  return IsInside(point, depth, name );
}

/** Returns the value at one point */
template< unsigned int NDimensions , unsigned int SpaceDimension >
void
PlaneSpatialObject<NDimensions, SpaceDimension >
::ValueAt( const PointType & point, double & value, unsigned int depth, 
           char * name )
{
  itkDebugMacro( "Getting the value of the tube at " << point );
  if( IsInside(point, 0, name) )
    {
    value = 1;
    return;
    }
  else
    {
    if( Superclass::IsEvaluableAt(point, depth, name) )
      {
      Superclass::ValueAt(point, value, depth, name);
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
template< unsigned int NDimensions , unsigned int SpaceDimension >
void 
PlaneSpatialObject< NDimensions, SpaceDimension >
::PrintSelf( std::ostream& os, Indent indent ) const
{
  Superclass::PrintSelf(os, indent);
}

} // end namespace itk

#endif
