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
template< unsigned int TDimension >
PlaneSpatialObject<TDimension >
::PlaneSpatialObject()
{
  strcpy(m_TypeName,"PlaneSpatialObject");
  m_Dimension = TDimension;
} 

/** Destructor */
template< unsigned int TDimension >
PlaneSpatialObject<TDimension >
::~PlaneSpatialObject()  
{
  
}

/** Test if the given point is inside the blob */
template< unsigned int TDimension >
bool 
PlaneSpatialObject< TDimension > 
::IsInside( const PointType & point, unsigned int depth, char * name ) const
{
  itkDebugMacro( "Checking the point [" << point << "is inside the plane" );
    
  if(name == NULL || strstr(typeid(Self).name(), name) )
    {
    PointType transformedPoint = point;
    TransformPointToLocalCoordinate(transformedPoint);
  
    bool inside = true;
    for(unsigned int i=0;i<TDimension;i++)
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
template< unsigned int TDimension >
bool
PlaneSpatialObject<TDimension >
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
      for(unsigned int i=0; i<TDimension;i++) 
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
template< unsigned int TDimension >
bool
PlaneSpatialObject<TDimension >
::IsEvaluableAt( const PointType & point, unsigned int depth, char * name ) const
{
  itkDebugMacro( "Checking if the Plane is evaluable at " << point );
  return IsInside(point, depth, name );
}

/** Returns the value at one point */
template< unsigned int TDimension >
bool
PlaneSpatialObject<TDimension >
::ValueAt( const PointType & point, double & value, unsigned int depth, 
           char * name ) const
{
  itkDebugMacro( "Getting the value of the tube at " << point );
  if( IsInside(point, 0, name) )
    {
    value = 1;
    return true;
    }
  else
    {
    if( Superclass::IsEvaluableAt(point, depth, name) )
      {
      Superclass::ValueAt(point, value, depth, name);
      return true;
      }
    else
      {
      value = 0;
      return false;
      }
    }
  return false;
}

/** Print Self function */
template< unsigned int TDimension >
void 
PlaneSpatialObject< TDimension >
::PrintSelf( std::ostream& os, Indent indent ) const
{
  Superclass::PrintSelf(os, indent);
}

} // end namespace itk

#endif
