/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkPlaneSpatialObject.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
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
  m_TypeName = "PlaneSpatialObject";
  m_Dimension = TDimension;
  m_LowerPoint.Fill(0);
  m_UpperPoint.Fill(0);
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
    TransformType::Pointer inverse = TransformType::New();
    if(!GetIndexToWorldTransform()->GetInverse(inverse))
      {
      return false;
      }

    PointType transformedPoint = inverse->TransformPoint(point);
    
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
::ComputeLocalBoundingBox(void) const
{ 
  itkDebugMacro( "Computing tube bounding box" );

  if( m_BoundingBoxChildrenName.empty() 
      || strstr(typeid(Self).name(), m_BoundingBoxChildrenName.c_str()) )
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
    
      pnt = this->GetIndexToWorldTransform()->TransformPoint(pnt);
      pnt2 = this->GetIndexToWorldTransform()->TransformPoint(pnt2);
         
      m_Bounds->SetMinimum(pnt);
      m_Bounds->SetMaximum(pnt2);
    }
  return true;
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
